//# TapeIO.cc: Class for IO on a tape device
//# Copyright (C) 1999
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

#include <trial/IO/TapeIO.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <unistd.h>               // needed for ::close
#include <fcntl.h>                // needed for ::open
#include <errno.h>                // needed for errno
#include <string.h>               // needed for strerror
#include <sys/mtio.h>             // needed for ioctl

TapeIO::TapeIO()
  :itsSeekable(False),
   itsReadable(False),
   itsWritable(False),
   itsDevice(-1)
{
}

TapeIO::TapeIO (int fd)
  :itsDevice(-1)
{
  attach (fd);
}

TapeIO::~TapeIO() {
  detach();
}


void TapeIO::attach(int fd) {
  AlwaysAssert(itsDevice == -1, AipsError);
  itsDevice = fd;
  fillRWFlags (fd);
  fillSeekable();
}

void TapeIO::detach() {
  itsDevice = -1;
}

void TapeIO::fillRWFlags(int fd) {
  itsReadable = itsWritable = False;
  int flags = fcntl (fd, F_GETFL);
  if ((flags & O_RDWR)  ==  O_RDWR) {
    itsReadable = True;
    itsWritable = True;
  } else if ((flags & O_WRONLY)  ==  O_WRONLY) {
    itsWritable = True;
  } else {
    itsReadable = True;
  }
}

void TapeIO::fillSeekable() {
  //  cout << "seek output " << seek (0, ByteIO::Current) << endl;
  itsSeekable = ToBool (seek (0, ByteIO::Current)  >= 0);
}


String TapeIO::fileName() const {
  return "";
}


void TapeIO::write (uInt size, const void* buf) {
  // Throw an exception if not writable.
  if (!itsWritable) {
    throw (AipsError ("TapeIO object is not writable"));
  }
  if (::write (itsDevice, buf, size) != Int(size)) {
    throw (AipsError (String("TapeIO: write error: ")
		      + strerror(errno)));
  }
}

void TapeIO::read (uInt size, void* buf) {
  if (!itsReadable) {
    throw (AipsError ("TapeIO object is not readable"));
  }
  Int bytesRead = ::read (itsDevice, buf, size);
  if (bytesRead < Int(size)) {// Throw an exception if not readable.
    //    cout << "Bytes read: " << bytesRead << "\tExpected:" << size << endl;
    if (bytesRead < 0) {
      throw(AipsError(String("TapeIO: read error ") + strerror(errno)));
    }
  }
}

void TapeIO::rewind() {
  struct mtop tapeCommand;
  tapeCommand.mt_op = MTREW;
  tapeCommand.mt_count = 1;
  Int error = ioctl(itsDevice, MTIOCTOP, &tapeCommand);
  if (error != 0) {
    //    cout << "Rewind returned " << error << endl;
    throw(AipsError(String("TapeIO: rewind error") + strerror(errno)));
  }
}

Long TapeIO::seek (Long offset, ByteIO::SeekOption dir) {
  switch (dir) {
  case ByteIO::Begin:
    return ::lseek (itsDevice, offset, SEEK_SET);
  case ByteIO::End:
    return ::lseek (itsDevice, offset, SEEK_END);
  default:
    break;
  }
  return ::lseek (itsDevice, offset, SEEK_CUR);
}

Long TapeIO::length() {
  if (!itsSeekable) {
    return -1;
  }
  // Get current position to be able to reposition.
  Long pos = seek (0, ByteIO::Current);
  //  cout << "pos output: " << pos << endl;
  // Seek to the end of the stream.
  // If it fails, we cannot seek and the current position is the length.
  Long len = seek (0, ByteIO::End);
  //  cout << "len output: " << len << endl;

  // Reposition
  seek (pos, ByteIO::Begin);
  // Return the length.
  if (len > pos) {
    return len;
  } else {
    return pos;
  }
//   cout << "final output: " <<   seek (pos, ByteIO::Begin) << endl;
//   return len;
//   return -1;
  
}

   
Bool TapeIO::isReadable() const {
  return itsReadable;
}

Bool TapeIO::isWritable() const {
  return itsWritable;
}

Bool TapeIO::isSeekable() const {
  return itsSeekable;
}

int TapeIO::open(const String& device, Bool writable) {
  int fd;
  if (writable) {
    fd = ::open (device, O_RDWR);
  } else {
    fd = ::open (device, O_RDONLY);
  }
  if (fd == -1) {
    throw (AipsError ("TapeIO: device " + device +
		      " could not be opened: " + String(strerror(errno))));
  }
  return fd;
}

void TapeIO::close(int fd) {
  if (::close (fd)  == -1) {
    throw (AipsError (String("TapeIO: file could not be closed: ")
		      + strerror(errno)));
  }
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 TapeIO; cd test; gmake OPTLIB=1 tTapeIO"
// End: 
