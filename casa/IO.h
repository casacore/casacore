//# IO.h: Basic classes and global functions for IO and object persistency
//# Copyright (C) 1995,1996,1999,2001
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

#ifndef CASA_IO_H
#define CASA_IO_H

#include <casacore/casa/aips.h>

//# Includes for object persistency.
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/IO/AipsIOCarray.h>

//# Includes for general IO.
#include <casacore/casa/IO/ByteSinkSource.h>

//# Includes for underlying IO classes.
#include <casacore/casa/IO/CanonicalIO.h>
#include <casacore/casa/IO/RawIO.h>
#include <casacore/casa/IO/RegularFileIO.h>
#include <casacore/casa/IO/FilebufIO.h>
#include <casacore/casa/IO/FiledesIO.h>
#include <casacore/casa/IO/MemoryIO.h>
#include <casacore/casa/IO/MFFileIO.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>

// <summary>
// Basic classes and global functions for IO and object persistency
// </summary>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" demos="">
// </reviewed>

// <synopsis>
// This module provides the basic IO functionality for the Casacore classes.
// There are two IO mechanisms:
// <ol>
// <li> Class <linkto class=AipsIO:description>AipsIO</linkto>
//      provides the object persistency mechanism.
//      The templated global functions in
//      <linkto file="AipsIOCarray.h#AipsIOCarray">AipsIOCarray.h</linkto>
//      form a little layer upon AipsIO. They provide the means to put or
//      get a C-style array of any type.
// <li> Class <linkto class=ByteSinkSource:description>ByteSinkSource</linkto>
//      and its ancestors provide a general IO mechanism.
// </ol>
//
// Both use the underlying IO framework which define where and how
// the data are written. The how-part is defined by classes derived from
// <linkto class=TypeIO:description>TypeIO</linkto> as shown
// in the <a href=IO/IO_1.html>OMT diagram</a>.
// There are three such classes:
// <ol>
// <li> <linkto class=CanonicalIO:description>CanonicalIO</linkto> reads/writes
//      data in canonical (machine-independent) format. This should be
//      used when data are meant to be exportable.
//      It uses the conversion functions in class
//      <linkto class=CanonicalConversion:description>CanonicalConversion
//      </linkto>.
// <li> <linkto class=RawIO:description>RawIO</linkto> reads/writes
//      data in native (machine-dependent) format. This can be used when
//      data are not exported.
// <li> <linkto class=ConversionIO:description>ConversionIO</linkto>
//      reads/writes in an external format as defined at construction time.
//      This can be used when the external format can be one of several
//      (e.g. VAX or IBM for a WSRT archive tape). In this way the
//      format has to be defined only once and thereafter is it handled
//      correctly by the polymorphism mechanism.
// </ol>
// The where-part is defined by classes derived from
// <linkto class=ByteIO:description>ByteIO</linkto> as shown
// in the <a href=IO/IO_2.html>OMT diagram</a>.
// There are a few such classes:
// <ol>
// <li> <linkto class=RegularFileIO:description>RegularFileIO</linkto> uses a
//      regular file to hold the data. Internally it uses FilebufIO (see below).
//      It can handle files > 2 GB.
// <li> <linkto class=FilebufIO:description>FilebufIO</linkto> does the IO
//      in a buffered way similar to the <src>stdio</src> system. However, it
//      does not use stdio because that gave problems when doing concurrent
//      access from multiple processes.
//      It can handle files > 2 GB.
// <li> <linkto class=FiledesIO:description>FiledesIO</linkto> uses the
//      UNIX IO-functions like <src>open, read</src> to do IO directly.
//      It does not use an internal buffer. Instead it always does
//      physical IO. It is meant for IO operations where large chunks of
//      a file are accessed and for IO on sockets, pipes, etc..
//      It can handle files > 2 GB.
// <li> <linkto class=StreamIO:description>StreamIO</linkto> for IO
//      on a socket.
// <li> <linkto class=TapeIO:description>TapeIO</linkto> for IO on a tape
//      device.
// <li> <linkto class=MemoryIO:description>MemoryIO</linkto> uses a
//      (possibly expandable) buffer in memory to hold the data.
// <li> <linkto class=MMapIO:description>MMapIO</linkto> uses memory-mapped IO.
//      Be careful to use this on 32-bit machines, because its address space is
//      too small to handle a file of a few GBytes.
// <li> <linkto class=MFFileIO:description>MFFileIO</linkto> uses a virtual
//      file in a <linkto class=MultiFile:description>MultiFile</linkto>
//      container file. MultiFile is meant to combine multiple files in
//      a single physical file to reduce the number of files used by
//      the Casacore table system.
// </ol>
//
// The IO framework is easily expandable. One can for instance think of a
// class <src>AsciiIO</src> derived from <src>TypeIO</src>
// to hold data in ASCII format.
// A class <src>RemoteTapeIO</src> could be developed for IO on a tape
// device on another node.
// </synopsis>

// </module>



} //# NAMESPACE CASACORE - END

#endif

