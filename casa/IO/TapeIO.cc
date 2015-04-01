//# TapeIO.cc: Class for IO on a tape device
//# Copyright (C) 1999,2000,2001
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

#include <casacore/casa/IO/TapeIO.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <unistd.h>               // needed for ::close
#include <fcntl.h>                // needed for ::open
#include <errno.h>                // needed for errno
#include <casacore/casa/string.h>          // needed for strerror

#ifndef CASA_NOTAPE
// No tape support on Cray XT3 and OS-X 10.6
#  if defined(AIPS_CRAY_PGI)
#    define CASA_NOTAPE 1
#  endif
#  if defined(__APPLE__)
// MAC_OS_X_VERSION_MAX_ALLOWED reflects the version of the SDK being used
#    include <AvailabilityMacros.h>
#    if defined(MAC_OS_X_VERSION_10_6)
#      if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6
#        define CASA_NOTAPE 1
#      endif
#    endif
#  endif
#endif
#ifndef CASA_NOTAPE
#  include <sys/mtio.h>             // needed for ioctl
#  if defined(AIPS_SOLARIS) || defined(AIPS_DARWIN)
#    include <sys/ioctl.h>            // needed for ioctl
#    include <sys/types.h>            // needed for ioctl
#  endif
#endif

namespace casacore { //# NAMESPACE CASACORE - BEGIN

TapeIO::TapeIO()
  :ByteIO(),
   itsDevice(-1),
   itsOwner(False),
   itsReadable(False),
   itsWritable(False),
   itsSeekable(False),
   itsDeviceName("")
{
}

TapeIO::TapeIO (int fd)
  :ByteIO(),
   itsDevice(-1)
{
  attach(fd);
}

TapeIO::TapeIO(const Path& device, Bool writable) 
  :ByteIO(),
   itsDevice(-1)
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

void TapeIO::write (Int64 size, const void* buf) {
  // Throw an exception if not writable.
  if (!itsWritable) {
    throw (AipsError ("TapeIO object is not writable"));
  }
  if (::write (itsDevice, buf, size) != static_cast<Int>(size)) {
    throw (AipsError (String("TapeIO: write error: ")
		      + strerror(errno)));
  }
}

Int64 TapeIO::read(Int64 size, void* buf, Bool throwException) {
  if (!itsReadable) {
    throw (AipsError ("TapeIO::read - tape is not readable"));
  }
  Int64 bytesRead = ::read (itsDevice, buf, size);
  if (bytesRead < 0) {
    throw (AipsError (String("TapeIO::read - "
			     " error returned by system call: ") + 
		      strerror(errno)));
  }
  if (bytesRead > size) { // Should never be executed
    throw (AipsError ("TapeIO::read - read more bytes than requested"));
  }
  if (bytesRead != size  &&  throwException) {
    throw (AipsError ("TapeIO::read - incorrect number of bytes read"));
  }
#if defined(AIPS_SOLARIS)
  // I assume a read of zero means we have hit the end of file. Solaris
  // requires an explicit skip to the next file UNLESS the bsd compatible
  // devices are used. The bsd compatible devices have a 'b' in the device
  // name. This test may fail if symlinks to the tape device are used.
  if (bytesRead == 0 && size != 0 && 
      !Path(itsDeviceName).baseName().contains('b')) {
    skip(1);
  }
#endif
  return bytesRead;
}

void TapeIO::rewind() {
#ifndef CASA_NOTAPE
  struct mtop tapeCommand;
  tapeCommand.mt_op = MTREW;
  tapeCommand.mt_count = 1;
  Int error = ::ioctl(itsDevice, MTIOCTOP, &tapeCommand);
  if (error != 0) {
    throw(AipsError(String("TapeIO::rewind - error returned by ioctl: ") 
		    + strerror(errno)));
  }
#endif
}

#ifndef CASA_NOTAPE
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
#else
void TapeIO::skip(uInt) {
}
#endif

#ifndef CASA_NOTAPE
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
#else
void TapeIO::mark(uInt) {
}
#endif

Bool TapeIO::fixedBlocks() const {
  return  (getBlockSize() != 0) ? True : False;
}

uInt TapeIO::fixedBlockSize() const {
  return getBlockSize();
}

void TapeIO::setFixedBlockSize(uInt sizeInBytes) {
  DebugAssert(sizeInBytes > 0, AipsError);
  setBlockSize(sizeInBytes);
}

void TapeIO::setVariableBlockSize() {
#if defined(AIPS_LINUX)
  setBlockSize(0);
#endif
}

#if (defined(AIPS_SOLARIS) || defined(AIPS_LINUX)) && !defined(CASA_NOTAPE)
void TapeIO::setBlockSize(uInt sizeInBytes) {
  struct mtop tapeCommand;
#if defined(AIPS_LINUX) 
  tapeCommand.mt_op = MTSETBLK;
#else 
  tapeCommand.mt_op = MTSRSZ;
#endif
  tapeCommand.mt_count = sizeInBytes;
  Int error = ::ioctl(itsDevice, MTIOCTOP, &tapeCommand);
  if (error != 0) {
    throw(AipsError(String("TapeIO::setVariableBlockSize - ") + 
		    String("error returned by ioctl: ") 
		    + strerror(errno)));
  }
#else
void TapeIO::setBlockSize(uInt) {
#endif
}

uInt TapeIO::getBlockSize() const {
#if (defined(AIPS_SOLARIS) || defined(AIPS_LINUX)) && !defined(CASA_NOTAPE)
#if defined(AIPS_LINUX) 
  struct mtget tapeInquiry;
  Int error = ::ioctl(itsDevice, MTIOCGET, &tapeInquiry);
  if (error != 0) {
    throw(AipsError(String("TapeIO::setVariableBlockSize - ") + 
		    String("error returned by ioctl: ") 
		    + strerror(errno)));
  }
  return tapeInquiry.mt_dsreg & MT_ST_BLKSIZE_MASK;
#else 
  struct mtdrivetype tapeInfo;
  struct mtdrivetype_request tapeInquiry;
  tapeInquiry.size = sizeof(struct mtdrivetype);
  tapeInquiry.mtdtp = &tapeInfo;
  Int error = ::ioctl(itsDevice, MTIOCGETDRIVETYPE, &tapeInquiry);
  if (error != 0) {
    throw(AipsError(String("TapeIO::setVariableBlockSize - ") + 
		    String("error returned by ioctl: ") 
		    + strerror(errno)));
  }
  return tapeInfo.bsize;
#endif
#else
  return 0;
#endif
}

Int64 TapeIO::doSeek (Int64 offset, ByteIO::SeekOption dir) {
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

Int64 TapeIO::length() {
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
  itsSeekable = (seek (0, ByteIO::Current)  >= 0);
}

} //# NAMESPACE CASACORE - END
