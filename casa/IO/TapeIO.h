//# TapeIO.h: Class for IO on a tape device.
//# Copyright (C) 1999,2001
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

#ifndef CASA_TAPEIO_H
#define CASA_TAPEIO_H

#include <casacore/casa/aips.h>
#include <casacore/casa/IO/ByteIO.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class Path;

// <summary>Class for IO on a tape device</summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite> 
//    <li> <linkto class=ByteIO>ByteIO</linkto> class
//    <li> Tape descriptors
// </prerequisite>

// <synopsis> 
// This class is a specialization of class
// <linkto class=ByteIO>ByteIO</linkto>. It uses a file descriptor
// to read/write data.
// <p>
// The file associated with the file descriptor has to be opened
// before hand.
// The constructor will determine automatically if the file is
// readable, writable and seekable.
// Note that on destruction the file descriptor is NOT closed.
// </synopsis>

// <example>
// This example shows how FiledesIO can be used with an fd.
// It uses the fd for a regular file, which could be done in an easier
// way using class <linkto class=RegularFileIO>RegularFileIO</linkto>.
// However, when using pipes or sockets, this would be the only way.
// <srcblock>
//    // Get a file descriptor for the file.
//    int fd = open ("file.name");
//    // Use that as the source of AipsIO (which will also use CanonicalIO).
//    FiledesIO fio (fd);
//    AipsIO stream (&fio);
//    // Read the data.
//    Int vali;
//    Bool valb;
//    stream >> vali >> valb;
// </srcblock>
// </example>

// <motivation> 
// Make it possible to use the Casacore IO functionality on any file.
// In this way any device can be hooked to the IO framework.
// </motivation>


class TapeIO: public ByteIO
{
public: 
  // Default constructor.
  // A stream can be attached using the attach function.
  TapeIO();

  // Construct from the given file descriptor. The file descriptor must have
  // been obtained using the TapeIO::open static function. When constructed
  // this way the class will not take over the file descriptor and hence not
  // close the Tape device when this class is destroyed.
  explicit TapeIO(int fd);

  // Construct from the given device. The device must point to a tape device
  // and if requested it is checked if the device is writeable. Throws an
  // exception if the device could not be opened correctly. When constructed
  // this way the class will close the Tape device when this class is destroyed
  // or the TapeIO object is attached to a new file descriptor.
  TapeIO(const Path& device, Bool writable = False);

  // The destructor will only close the file if the appropriate constructor, or
  // attach function, was used.
  virtual ~TapeIO();
    
  // Attach to the given file descriptor. The file descriptor will not be
  // closed when this class is destroyed.
  void attach(int fd);

  // Attach to the given tape device. The tape will be closed when this class
  // is destroyed or the TapeIO object is attached to a new descriptor.
  void attach(const Path& device, Bool writable = False);

  // Write the specified number of bytes.
  virtual void write(Int64 size, const void* buf);

  // Read <src>size</src> bytes from the tape. Returns the number of bytes
  // actually read or a negative number if an error occured. Will throw an
  // exception (AipsError) if the requested number of bytes could not be read,
  // or an error occured, unless throwException is set to False. Will always
  // throw an exception if the tape is not readable or the system call returns
  // an undocumented value. Returns zero if the tape is at the end of the
  // current file (and size is non-zero and throwException is False).
  virtual Int64 read(Int64 size, void* buf, Bool throwException=True);    

  // Rewind the tape device to the beginning.
  virtual void rewind();
  
  // skip the specified number of files (ie tape marks) on the tape. Throws an
  // exception if you try to skip past the last filemark.
  virtual void skip(uInt howMany=1);
  
  // write the specified number of filemarks.
  virtual void mark(uInt howMany=1);
  
  // returns True if the tape device is configured to use a fixed block size
  Bool fixedBlocks() const;

  // returns the block size in bytes. Returns zero if the device is configured
  // to use variable length blocks.
  uInt fixedBlockSize() const;

  // Configure the tape device to use fixed length blocks of the specified
  // size. The size must be bigger than zero (dugh!). Values bigger than 64k
  // may cause problems on some systems. Currently this function only does
  // anything under Solaris and Linux systems.
  void setFixedBlockSize(uInt sizeInBytes);

  // Configure the tape device to use variable length blocks. Currently this
  // function only does anything under Solaris and Linux systems.
  void setVariableBlockSize();

  // Get the length of the tape device.  Not a meaningful function for this
  // class and this function always returns -1.
  virtual Int64 length();
  
  // Is the tape device readable?
  virtual Bool isReadable() const;
  
  // Is the tape device writable?
  virtual Bool isWritable() const;
  
  // Is the tape device seekable?
  virtual Bool isSeekable() const;
  
  // Get the name of the attached device or return a zero length string if it
  // cannot be determined.
  virtual String fileName() const;

  // Some static convenience functions for file descriptor opening &
  // closing. The open function returns a file descriptor and the close
  // function requires a file descriptor as an argument.
  // <group>
  static int open(const Path& device, Bool writable = False);
  static void close(int fd);
  // </group>

protected:
  // Detach the FILE. Close it when it is owned.
  void detach();

  // Determine if the file is readable and/or writable.
  void fillRWFlags();

  // Determine if the file is seekable.
  void fillSeekable();

  // Reset the position pointer to the given value. It returns the new
  // position. May not work on all Tape devices use the isSeekable(0 member
  // function to see if this function is usuable. Otherwise an Exception
  // (AipsError) is thrown. 
  virtual Int64 doSeek(Int64 offset, ByteIO::SeekOption);
  
private:
  // The following functions are made private so that the compiler does not
  // generate default ones. They cannot be used and are not defined.
  TapeIO (const TapeIO& that);
  TapeIO& operator= (const TapeIO& that);

  void setBlockSize(uInt sizeInBytes);
  uInt getBlockSize() const;

  int         itsDevice;
  Bool        itsOwner;
  Bool        itsReadable;
  Bool        itsWritable;
  Bool        itsSeekable;
  String      itsDeviceName;
};




} //# NAMESPACE CASACORE - END

#endif
