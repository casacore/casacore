//# MFFileIO.cc: A single file in a MultiFileBase
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
#include <casacore/casa/IO/MFFileIO.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  MFFileIO::MFFileIO (MultiFileBase& file, const String& name,
                      ByteIO::OpenOption opt)
    : itsFile     (file),
      itsPosition (0),
      itsName     (name)
  {
    if (opt == ByteIO::New  ||  opt == ByteIO::NewNoReplace) {
      itsId = itsFile.fileId (name, False);
      if (itsId >= 0) {
        if (opt == ByteIO::NewNoReplace) {
          throw AipsError ("MFFileIO: file " + name + " already exists");
        }
        itsFile.deleteFile (itsId);
      }
      itsId = itsFile.addFile (name);
    } else {
      itsId = itsFile.fileId (name);
    }
  }

  MFFileIO::~MFFileIO()
  {
    itsFile.flush();
  }

  void MFFileIO::remove()
  {
    itsFile.deleteFile (itsId);
    itsId = -1;
  }

  Int64 MFFileIO::read (Int64 size, void* buffer, Bool throwException)
  {
    Int64 n = itsFile.read (itsId, buffer, size, itsPosition);
    itsPosition += n;
    if (throwException  &&  n < size) {
      throw AipsError ("MFFileIO::read - incorrect number of bytes ("
		       + String::toString(n) + " out of "
                       + String::toString(size) + ") read for file "
                       + itsName + " in MultiFileBase " + itsFile.fileName());
    }
    return n;
  }

  void MFFileIO::write (Int64 size, const void* buffer)
  {
    Int64 n = itsFile.write (itsId, buffer, size, itsPosition);
    itsPosition += n;
    if (n != size) {
      throw AipsError ("MFFileIO: write error in " + itsName);
    }
  }

  void MFFileIO::reopenRW()
  {
    itsFile.reopenRW();
  }

  void MFFileIO::flush()
  {
    itsFile.flush();
  }

  void MFFileIO::fsync()
  {}

  String MFFileIO::fileName() const
  {
    return itsName;
  }

  Int64 MFFileIO::length()
  {
    return itsFile.info()[itsId].fsize;
  }
       
  Bool MFFileIO::isReadable() const
  {
    return True;
  }

  Bool MFFileIO::isWritable() const
  {
    return itsFile.isWritable();
  }

  Bool MFFileIO::isSeekable() const
  {
    return True;
  }

  Int64 MFFileIO::doSeek (Int64 offset, ByteIO::SeekOption dir)
  {
    // Determine the new position.
    // Exit with error status if negative.
    Int64 newPos;
    switch (dir) {
    case ByteIO::Begin:
      newPos = offset;
      break;
    case ByteIO::End:
      newPos = length() + offset;
      break;
    default:
      newPos = itsPosition + offset;
      break;
    }
    if (newPos < 0) {
      throw (AipsError("MFFileIO::seek - cannot seek before start of file"));
    }
    itsPosition = newPos;
    return newPos;
  }


} //# NAMESPACE CASACORE - END
