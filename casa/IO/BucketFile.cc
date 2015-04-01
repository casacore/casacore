//# BucketFile.cc: Tiled Hypercube Storage Manager for tables
//# Copyright (C) 1995,1996,1999,2001,2002
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$


//# Includes
#include <casacore/casa/IO/LargeIOFuncDef.h>
#include <casacore/casa/IO/BucketFile.h>
#include <casacore/casa/IO/MMapfdIO.h>
#include <casacore/casa/IO/FilebufIO.h>
#include <casacore/casa/IO/MFFileIO.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/OS/DOos.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>                // needed for errno
#include <casacore/casa/string.h>          // needed for strerror

#if defined(AIPS_DARWIN) || defined(AIPS_BSD)
#undef trace3OPEN
#define trace3OPEN open
#undef trace2OPEN
#define trace2OPEN open
#undef traceLSEEK
#define traceLSEEK lseek
#endif

namespace casacore { //# NAMESPACE CASACORE - BEGIN

BucketFile::BucketFile (const String& fileName,
                        uInt bufSizeFile, Bool mappedFile,
                        MultiFileBase* mfile)
: name_p         (Path(fileName).expandedName()),
  isWritable_p   (True),
  isMapped_p     (mappedFile),
  bufSize_p      (bufSizeFile),
  fd_p           (-1),
  file_p         (),
  mappedFile_p   (0),
  bufferedFile_p (0),
  mfile_p        (mfile)
{
    // Create the file.
    if (mfile_p) {
      file_p = new MFFileIO (*mfile_p, name_p, ByteIO::New);
      isMapped_p = False;
      bufSize_p  = 0;
    } else {
      fd_p   = FiledesIO::create (name_p.chars());
      file_p = new FiledesIO (fd_p, name_p);
    }
    createMapBuf();
}

BucketFile::BucketFile (const String& fileName, Bool isWritable,
                        uInt bufSizeFile, Bool mappedFile,
                        MultiFileBase* mfile)
: name_p         (Path(fileName).expandedName()),
  isWritable_p   (isWritable),
  isMapped_p     (mappedFile),
  bufSize_p      (bufSizeFile),
  fd_p           (-1),
  file_p         (),
  mappedFile_p   (0),
  bufferedFile_p (0),
  mfile_p        (mfile)
{
  if (mfile_p) {
    isMapped_p = False;
    bufSize_p  = 0;
  }
}

BucketFile::~BucketFile()
{
    close();
}

CountedPtr<ByteIO> BucketFile::makeFilebufIO (uInt bufferSize)
{
  if (mfile_p) {
    return file_p;
  }
  return new FilebufIO (fd_p, bufferSize);
}


void BucketFile::close()
{
    if (file_p) {
        deleteMapBuf();
	file_p = CountedPtr<ByteIO>();
        FiledesIO::close (fd_p);
	fd_p   = -1;
    }
}


void BucketFile::open()
{
    if (! file_p) {
      if (mfile_p) {
        file_p = new MFFileIO (*mfile_p, name_p,
                               isWritable_p ? ByteIO::Update : ByteIO::Old);
      } else {
        fd_p   = FiledesIO::open (name_p.chars(), isWritable_p);
        file_p = new FiledesIO (fd_p, name_p);
      }
      createMapBuf();
    }
}

void BucketFile::createMapBuf()
{
    deleteMapBuf();
    if (isMapped_p) {
        AlwaysAssert (fd_p >= 0, AipsError);
        mappedFile_p = new MMapfdIO (fd_p, name_p);
    }
    if (bufSize_p > 0) {
        AlwaysAssert (fd_p >= 0, AipsError);
        bufferedFile_p = new FilebufIO (fd_p, bufSize_p);
    }
}

void BucketFile::deleteMapBuf()
{
    delete mappedFile_p;
    mappedFile_p = 0;
    delete bufferedFile_p;
    bufferedFile_p = 0;
}

void BucketFile::remove()
{
    close();
    if (mfile_p) {
      // Remove the file from the MultiFileBase. Note it might not exist yet.
      Int id = mfile_p->fileId (name_p, False);
      if (id >= 0) {
        mfile_p->deleteFile (id);
      }
    } else {
      DOos::remove (name_p, False, False);
    }
    file_p = CountedPtr<ByteIO>();
}


void BucketFile::fsync()
{
    file_p->fsync();
}


void BucketFile::setRW()
{
    // Exit if already writable.
    if (isWritable_p) {
	return;
    }
    isWritable_p = True;
    // Try to reopen the file as read/write.
    // Throw an exception if it fails.
    if (file_p) {
      if (mfile_p) {
        file_p->reopenRW();
      } else {
        close();
        open();
      }
    }
}


uInt BucketFile::read (void* buffer, uInt length)
{
  return file_p->read (length, buffer);
}

uInt BucketFile::write (const void* buffer, uInt length)
{
  file_p->write (length, buffer);
    return length;
}

void BucketFile::seek (Int64 offset)
{
    AlwaysAssert (bufferedFile_p == 0, AipsError);
    file_p->seek (offset, ByteIO::Begin);
}

Int64 BucketFile::fileSize () const
{
    // If a buffered file is used, seek in there. Otherwise its internal
    // offset is wrong.
    Int64 size;
    if (bufferedFile_p) {
        size = bufferedFile_p->seek (0, ByteIO::End);
    } else {
      size = file_p->length();
    }
    if (size < 0){
        LogIO logIo (LogOrigin ("BucketFile", "fileSize"));
        logIo << LogIO::WARN;
        logIo << "lseek failed for " << name() << ": errno=" << errno
              << "'" << strerror(errno) << "'\n";
        logIo << LogIO::POST;
    }
    return size;
}

} //# NAMESPACE CASACORE - END

