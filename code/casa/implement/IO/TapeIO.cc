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
#include <aips/OS/Path.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <unistd.h>               // needed for ::close
#include <fcntl.h>                // needed for ::open
#include <errno.h>                // needed for errno
#include <string.h>               // needed for strerror
#include <sys/ioctl.h>            // needed for ioctl
#include <sys/mtio.h>             // needed for ioctl

TapeIO::TapeIO()
  :itsDevice(-1),
   itsOwner(False),
   itsReadable(False),
   itsWritable(False),
   itsSeekable(False),
   itsDeviceName("")
{
}

TapeIO::TapeIO (int fd)
  :itsDevice(-1)
{
  attach(fd);
}

TapeIO::TapeIO(const Path& device, Bool writable) 
  :itsDevice(-1)
{
  attach(device, writable);
}

TapeIO::~TapeIO() {
  detach();
}

void TapeIO::attach(int fd) {
  if (itsDevice >= 0) detach();
  DebugAssert(itsDevice == -1, AipsError);
  itsOwner = False;
  itsDevice = fd;
  fillRWFlags();
  fillSeekable();
  itsDeviceName = String("");
}

void TapeIO::attach(const Path& device, Bool writable) {
  if (itsDevice >= 0) detach();
  DebugAssert(itsDevice == -1, AipsError);
  itsOwner = True;
  itsDevice = TapeIO::open(device, writable);
  fillRWFlags();
  fillSeekable();
  itsDeviceName = device.absoluteName();
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

Int TapeIO::read(uInt size, void* buf, Bool throwException) {
  if (!itsReadable) {
    throw (AipsError ("TapeIO::read - tape is not readable"));
  }
  Int bytesRead = ::read (itsDevice, buf, size);
  if (bytesRead < 0) {
    throw (AipsError (String("TapeIO::read - "
			     " error returned by system call: ") + 
		      strerror(errno)));
  }
  if (bytesRead > Int(size)) { // Should never be executed
    throw (AipsError ("TapeIO::read - read more bytes than requested"));
  }
  if (bytesRead != Int(size) && throwException == True) {
    throw (AipsError ("TapeIO::read - incorrect number of bytes read"));
  }
  return bytesRead;
}

void TapeIO::rewind() {
  struct mtop tapeCommand;
  tapeCommand.mt_op = MTREW;
  tapeCommand.mt_count = 1;
  Int error = ::ioctl(itsDevice, MTIOCTOP, &tapeCommand);
  if (error != 0) {
    throw(AipsError(String("TapeIO::rewind - error returned by ioctl: ") 
		    + strerror(errno)));
  }
}

void TapeIO::skip(uInt howMany) {
  if (howMany > 0) {
    struct mtop tapeCommand;
    tapeCommand.mt_op = MTFSF;
    tapeCommand.mt_count = howMany;
    Int error = ::ioctl(itsDevice, MTIOCTOP, &tapeCommand);
    if (error != 0) {
      throw(AipsError(String("TapeIO::skip - error returned by ioctl: ") 
		      + strerror(errno)));
    }
  }
}

void TapeIO::mark(uInt howMany) {
  DebugAssert(isWritable(), AipsError);
  if (howMany > 0) {
    struct mtop tapeCommand;
    tapeCommand.mt_op = MTWEOF;
    tapeCommand.mt_count = howMany;
    Int error = ::ioctl(itsDevice, MTIOCTOP, &tapeCommand);
    if (error != 0) {
      throw(AipsError(String("TapeIO::mark - error returned by ioctl: ") 
		      + strerror(errno)));
    }
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
  return -1;
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

String TapeIO::fileName() const {
  return itsDeviceName;
}

int TapeIO::open(const Path& device, Bool writable) {
  int fd;
  const String& deviceString = device.absoluteName();
  char* devicePtr = (char*) deviceString.chars();
  if (writable) {
    fd = ::open (devicePtr, O_RDWR);
  } else {
    fd = ::open (devicePtr, O_RDONLY);
  }
  if (fd == -1) {
    throw (AipsError ("TapeIO: device " + deviceString +
		      " could not be opened: " + String(strerror(errno))));
  }
  return fd;
}

void TapeIO::close(int fd) {
  DebugAssert(fd >= 0, AipsError);
  if (::close (fd)  == -1) {
    throw (AipsError (String("TapeIO: file could not be closed: ")
		      + strerror(errno)));
  }
}

void TapeIO::detach() {
  if (itsOwner) {
    if (isWritable()) mark(1);
    TapeIO::close(itsDevice);
    itsOwner = False;
    itsDeviceName = String("");
  }
  itsDevice = -1;
  itsSeekable = itsReadable = itsWritable = False;
}

void TapeIO::fillRWFlags() {
  if (itsDevice < 0) {
    itsReadable = False;
    itsWritable = False;
    return;
  }
  int flags = fcntl (itsDevice, F_GETFL);
  if ((flags & O_RDWR)  ==  O_RDWR) {
    itsReadable = True;
    itsWritable = True;
  } else if ((flags & O_RDONLY)  ==  O_RDONLY) {
    itsReadable = True;
    itsWritable = False;
  } else if ((flags & O_WRONLY)  ==  O_WRONLY) {
    itsReadable = False;
    itsWritable = True;
  } else {
    itsReadable = False;
    itsWritable = False;
  }
}

void TapeIO::fillSeekable() {
  if (itsDevice < 0) {
    itsSeekable = False;
    return;
  }
  itsSeekable = ToBool (seek (0, ByteIO::Current)  >= 0);
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 TapeIO; cd test; gmake OPTLIB=1 tTapeIO"
// End: 
