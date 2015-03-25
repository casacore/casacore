//# MultiFile.cc: Class to combine multiple files in a single one
//# Copyright (C) 2014
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
//# $Id: RegularFileIO.h 20551 2009-03-25 00:11:33Z Malte.Marquarding $

//# Includes
#include <casacore/casa/IO/MultiFile.h>
#include <casacore/casa/IO/RegularFileIO.h>
#include <casacore/casa/IO/MemoryIO.h>
#include <casacore/casa/IO/CanonicalIO.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/OS/CanonicalConversion.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <unistd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  MultiFile::MultiFile (const String& name, ByteIO::OpenOption option,
                        Int blockSize)
    : MultiFileBase (name, blockSize)
  {
    itsFD = RegularFileIO::openCreate (itsName, option);
    itsIO.attach (itsFD, itsName);
    if (option == ByteIO::New  ||  option == ByteIO::NewNoReplace) {
      // New file; first block is for administration.
      setNewFile();
      itsNrBlock = 1;
    } else {
      readHeader();
    }
    itsWritable = itsIO.isWritable();
  }

  MultiFile::~MultiFile()
  {
    close();
  }

  void MultiFile::flushFile()
  {
    itsIO.flush();
  }

  void MultiFile::close()
  {
    flush();
    FiledesIO::close (itsFD);
  }

  void MultiFile::reopenRW()
  {
    if (isWritable()) {
      return;
    }
    // First try if the file can be opened as read/write.
    int fd = RegularFileIO::openCreate (itsName, ByteIO::Update);
    // Now close the readonly file and reset fd.
    FiledesIO::close (itsFD);
    itsIO.detach();
    itsFD = fd;
    itsIO.attach (itsFD, itsName);
    itsIO.setWritable();
    itsWritable = True;
  }

  void MultiFile::fsync()
  {
    itsIO.fsync();
  }

  void MultiFile::writeHeader()
  {
    // Write all header info into a memory buffer.
    // This buffer will be written into the file.
    // If needed, spread over multiple blocks.
    MemoryIO mio(itsBlockSize, itsBlockSize);
    CanonicalIO cio(&mio);
    AipsIO aio(&cio);
    itsHdrCounter++;
    cio.write (1, &itsBlockSize);         // reserve space for header size
    cio.write (1, &itsBlockSize);
    cio.write (1, &itsHdrCounter);
    aio.putstart ("MultiFile", 1);
    aio << itsNrBlock << itsInfo << itsFreeBlocks;
    aio.putend();
    Int64 todo = mio.length();
    uChar* buf = const_cast<uChar*>(mio.getBuffer());
    CanonicalConversion::fromLocal (buf, todo);       // header size
    // Write the first part of the buffer at the beginning of the file.
    itsIO.seek (0);
    itsIO.write (itsBlockSize, buf);
    todo -= itsBlockSize;
    if (todo > 0) {
      // The rest is written in another file. If the header info was written
      // at the end of the file, it would be overwritten when extending with
      // possible file corruption if the program or system crashes.
      // By using a separate file, corruption chances are much lower.
      // Even better would be using another name and doing a rename at the end.
      int fd = RegularFileIO::openCreate (itsName + "_hdrext", ByteIO::New);
      FiledesIO iohdr (fd, itsName + "_hdrext");
      iohdr.write (todo, buf+itsBlockSize);
      FiledesIO::close (fd);
    }
  }

  void MultiFile::readHeader (Bool always)
  {
    // Read the first part of the header.
    vector<char> buf(3*sizeof(Int64));
    itsIO.seek (0);
    itsIO.read (buf.size(), &(buf[0]));
    // Extract the required info.
    Int64 headerSize, hdrCounter;
    CanonicalConversion::toLocal (headerSize, &(buf[0]));
    CanonicalConversion::toLocal (itsBlockSize, &(buf[1*sizeof(Int64)]));
    CanonicalConversion::toLocal (hdrCounter, &(buf[2*sizeof(Int64)]));
    // Only if needed, read the rest of the header.
    if (hdrCounter == itsHdrCounter  &&  !always) {
      return;
    }
    itsHdrCounter = hdrCounter;
    Int64 leadSize = 3*sizeof(Int64);
    buf.resize (headerSize);
    if (headerSize > itsBlockSize) {
      itsIO.read (itsBlockSize - leadSize, &(buf[leadSize]));
      int fd = RegularFileIO::openCreate (itsName + "_hdrext", ByteIO::Old);
      FiledesIO iohdr (fd, itsName + "_hdrext");
      iohdr.read (headerSize - itsBlockSize, &(buf[itsBlockSize]));
      FiledesIO::close (fd);
    } else {
      itsIO.read (headerSize - leadSize, &(buf[leadSize]));
    }
    // Read all header info from the memory buffer.
    MemoryIO mio(&(buf[leadSize]), headerSize - leadSize);
    CanonicalIO cio(&mio);
    AipsIO aio(&cio);
    Int version = aio.getstart ("MultiFile");
    AlwaysAssert (version==1, AipsError);
    aio >> itsNrBlock >> itsInfo >> itsFreeBlocks;
    aio.getend();
    // Initialize remaining info fields.
    for (vector<MultiFileInfo>::iterator iter=itsInfo.begin();
         iter!=itsInfo.end(); ++iter) {
      iter->curBlock = -1;
      iter->dirty    = False;
      iter->buffer.resize (itsBlockSize);
    }
  }

  void MultiFile::doAddFile (MultiFileInfo&)
  {}

  void MultiFile::doDeleteFile (MultiFileInfo& info)
  {
    // Add the blocknrs to the free list.
    // Later we can merge them in order and leave out blocks past last block used.
    itsFreeBlocks.reserve (itsFreeBlocks.size() + info.blockNrs.size());
    for (size_t i=0; i<info.blockNrs.size(); ++i) {
      itsFreeBlocks.push_back (info.blockNrs[i]);
    }
    // Sort them in descending order, so free blocks can be taken from the tail.
    genSort (&(itsFreeBlocks[0]), itsFreeBlocks.size(),
             Sort::Descending, Sort::QuickSort);
  }

  void MultiFile::extend (MultiFileInfo& info, Int64 lastblk)
  {
    Int64 curnrb = info.blockNrs.size();
    info.blockNrs.resize (lastblk);
    for (Int64 i=curnrb; i<lastblk; ++i) {
      if (itsFreeBlocks.empty()) {
        info.blockNrs[i] = itsNrBlock;
        itsNrBlock++;
      } else {
        info.blockNrs[i] = itsFreeBlocks[itsFreeBlocks.size() - 1];
        itsFreeBlocks.resize (itsFreeBlocks.size() - 1);
      }
    }
  }

  void MultiFile::readBlock (MultiFileInfo& info, Int64 blknr,
                             void* buffer)
  {
    itsIO.seek (info.blockNrs[blknr] * itsBlockSize);
    itsIO.read (itsBlockSize, buffer);
  }

  void MultiFile::writeBlock (MultiFileInfo& info, Int64 blknr,
                              const void* buffer)
  {
    itsIO.seek  (info.blockNrs[blknr] * itsBlockSize);
    itsIO.write (itsBlockSize, buffer);
  }


} //# NAMESPACE CASACORE - END
