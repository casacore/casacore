//# StreamIO.cc: Class for IO to a connection oriented socket
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

#include <casacore/casa/IO/StreamIO.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/Utilities/Copy.h>
#include <casacore/casa/Exceptions/Error.h>

//# No socket support on Cray XT3 Catamount (yet)
#ifndef AIPS_CRAY_PGI
#include <netinet/in.h>
#include <arpa/inet.h>            // Definition of sockaddr_in
#include <sys/socket.h>           // Definition of sockaddr & socket function
#include <netdb.h>
#endif
#include <unistd.h>               // needed for ::close
#include <cstring>                //# for memcpy with gcc-4.3

namespace casacore { //# NAMESPACE CASACORE - BEGIN

StreamIO::StreamIO(const String& hostname, uShort portNumber) 
  :ByteIO(),
   itsSockDesc(-1)
{
#ifdef AIPS_CRAY_PGI
  throw AipsError("StreamIO is not supported on Cray XT3");
#else
  // Do hostname lookup!
  struct sockaddr_in serverInfo;
  objset(reinterpret_cast<char*>(&serverInfo), static_cast<char>(0), 
	 sizeof(serverInfo)); // Isn't C a wonderful language!
  serverInfo.sin_family = AF_INET;

  Regex anyLetters("[A-Za-z]");
  if(hostname.contains(anyLetters)){
    struct hostent *hp = gethostbyname(hostname.chars());
    memcpy ((char *) &serverInfo.sin_addr, (char *) hp->h_addr,  hp->h_length);
    serverInfo.sin_family = hp->h_addrtype;
  } else {
     serverInfo.sin_addr.s_addr = inet_addr(hostname.chars());;
  }
  serverInfo.sin_port = htons(portNumber);
  
  itsSockDesc = socket(AF_INET, SOCK_STREAM, 0);
  if (itsSockDesc < 0) {
    throw(AipsError(String("StreamIO::StreamIO - cannot attach a socket to") +
		    String(" host ") + hostname + String(" on port ") +
		    String::toString(portNumber)));
  }
  if (connect(itsSockDesc, reinterpret_cast<sockaddr*>(&serverInfo),
	      sizeof(serverInfo)) < 0) {
    throw(AipsError(String("StreamIO::StreamIO - cannot connect to") +
		    String(" host ") + hostname + String(" on port ") +
		    String::toString(portNumber)));
  }
#endif
}

StreamIO::~StreamIO() {
  if (itsSockDesc > 0) {
    ::close(itsSockDesc);
  }
}

void StreamIO::write(Int64 size, const void* buf) {
  Int64 bytesToWrite = size;
  const Char* bytePtr = static_cast<const Char*>(buf);
  while (bytesToWrite > 0) {
    const Int64 bytesWritten = ::write(itsSockDesc, bytePtr, bytesToWrite);
    if (bytesWritten <= 0) {
      const String fString = "StreamIO::write - cannot write ";
      if (bytesToWrite == size) {
	throw(AipsError(fString + String("any data to the socket")));
      } else {
	throw(AipsError(fString + String("all the data to the socket")));
      }
    }
    bytePtr += bytesWritten;
    bytesToWrite -= bytesWritten;
  }    
}

Int64 StreamIO::read(Int64 size, void* buf, Bool throwException) {
  Int64 bytesToRead = size;
  Char* bytePtr = static_cast<Char*>(buf);
  while (bytesToRead > 0) {
    const Int64 bytesRead = ::read(itsSockDesc, bytePtr, bytesToRead);
    if (bytesRead < 0) {
      throw (AipsError(String("StreamIO::read - ") +
		       String("Error returned by system call")));
    }
    if (bytesRead > bytesToRead) {
      throw (AipsError ("StreamIO::read - read more bytes than requested"));
    }
    if (bytesRead == 0) {
      break;
    }
    bytesToRead -= bytesRead;
    bytePtr += bytesRead;
  }
  if (bytesToRead != 0  &&  throwException) {
    throw (AipsError ("StreamIO::read - incorrect number of bytes read"));
  }
  return size - bytesToRead;
}

Int64 StreamIO::doSeek (Int64, ByteIO::SeekOption) {
  throw(AipsError("StreamIO::doSeek - streams are not seekable."));
  return -1;
}

Int64 StreamIO::length() {
  return -1;
}
   
Bool StreamIO::isReadable() const {
  return True;
}

Bool StreamIO::isWritable() const {
  return True;
}

Bool StreamIO::isSeekable() const {
  return False;
}

} //# NAMESPACE CASACORE - END
