//# StreamIO.h: Class for connection oriented IO to/from a socket
//# Copyright (C) 2001
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

#ifndef CASA_STREAMIO_H
#define CASA_STREAMIO_H

#include <casacore/casa/aips.h>
#include <casacore/casa/IO/ByteIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class String;

// <summary>Class for IO on connection oriented socket</summary>

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
// to read/write data to a Internet (AF_INET) stream.
// <p>
// </synopsis>

// <example>
// </example>

// <motivation> 
// This class was needed for the online version of the VLA filler.
// </motivation>


class StreamIO: public ByteIO
{
public: 
  // Construct a stream that is attached to the specified host on the specified
  // portnumber. Name lookup is not currently done so that the dotted quad
  // notation must be used.
  StreamIO(const String& hostname, uShort portNumber);

  // The destructor closes the file.
  virtual ~StreamIO();
    
  // Write the specified number of bytes.
  virtual void write(Int64 size, const void* buf);

  // Read <src>size</src> bytes from the tape. Returns the number of bytes
  // actually read or a negative number if an error occured. Will throw an
  // exception (AipsError) if the requested number of bytes could not be read,
  // or an error occured, unless throwException is set to False. 
  virtual Int64 read(Int64 size, void* buf, Bool throwException=True);    

  // Get the length of the stream.  Not a meaningful function for this
  // class and this function always returns -1.
  virtual Int64 length();
  
  // Is the stream readable? This function always returns True.
  virtual Bool isReadable() const;
  
  // Is the stream writable? This function always returns True.
  virtual Bool isWritable() const;
  
  // Is the stream seekable? This function always returns False.
  virtual Bool isSeekable() const;
  
protected:
  // Reset the position pointer to the given value. It returns the new
  // position. As stream devices are not seekable calling this function will
  // always throw an AipsError exception.
  virtual Int64 doSeek(Int64 offset, ByteIO::SeekOption);
  
private:
  // The following functions are made private so that the compiler does not
  // generate default ones. They cannot be used and are not defined.
  StreamIO (const StreamIO& other);
  StreamIO& operator= (const StreamIO& other);

  int itsSockDesc;
};

} //# NAMESPACE CASACORE - END

#endif
