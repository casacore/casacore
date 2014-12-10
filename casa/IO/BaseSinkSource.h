//# BaseSinkSource.h: Shared base class for ByteSink and ByteSource
//# Copyright (C) 1996,1999,2001
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

#ifndef CASA_BASESINKSOURCE_H
#define CASA_BASESINKSOURCE_H

#include <casacore/casa/aips.h>
#include <casacore/casa/IO/TypeIO.h>
#include <casacore/casa/IO/ByteIO.h>
#include <casacore/casa/Utilities/CountedPtr.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>Shared base class for ByteSink and ByteSource.</summary>

// <use visibility=export>

// <prerequisite> 
//    <li> <linkto class=TypeIO>TypeIO</linkto> class and derived classes
// </prerequisite>

// <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tByteSink" demos="">
// </reviewed>

// <synopsis> 
// This class provides the common functionality for the classes
// <linkto class=ByteSink>ByteSink</linkto> and
// <linkto class=ByteSource>ByteSource</linkto>.
// <p>
// The object is constructed using a typed byte stream. This stream
// is an instance of a class derived from class
// <linkto class=TypeIO>TypeIO</linkto>. This makes it possible to
// store the data in any format (e.g. CanonicalIO or RawIO).
// Class <linkto class=CanonicalIO>CanonicalIO</linkto> makes it
// possible to store the data in a canonical (machine-independent) format,
// so it can be read on any machine and operating system. The canonical
// format is big-endian IEEE, where a (unsigned) long is stored as 8 bytes.
// This means that on common 32-bit big-endian machines like SUN and HP
// only longs have to be converted and that CanonicalIO is as fast as RawIO.
// Class <linkto class=RawIO>RawIO</linkto> stores the data in native
// format, so the IO-process is faster on especially little-endian
// machines (PC, DEC-alpha). Note that RawIO can also be used to read
// bytes and interprete or convert them thereafter (e.g. using the
// conversion functions in the <linkto class=Conversion>Conversion</linkto>
// Conversion framework.
// <p>
// In its turn TypeIO uses an instance of a class derived from class
// <linkto class=ByteIO>ByteIO</linkto>. This makes it possible to
// use any output stream (e.g. file, memory).
// </synopsis>

// <motivation> 
// The design of the ByteSink and ByteSource classes resembles the design of 
// the iostream classes in the standard library. A shared base class is needed
// to allow multiple inheritance needed for class ByteSinkSource.
// </motivation>


class BaseSinkSource
{
public: 
    // This functions returns a reference to itsTypeIO.
    // <group>
    TypeIO& typeIO();
    const TypeIO& typeIO() const;
    // </group>

    // This function sets the position on the given offset.
    // The seek option defines from which position the seek is done.
    // <group>
    Int64 seek (Int64 offset, ByteIO::SeekOption = ByteIO::Begin);
    Int64 seek (Int offset, ByteIO::SeekOption = ByteIO::Begin);
    // </group>

    // Is the SinkSource readable?
    Bool isReadable() const;

    // Is the SinkSource writable?
    Bool isWritable() const;

    // Is the SinkSource seekable?
    Bool isSeekable() const;

    // Is the BaseSinkSource unusable? 
    Bool isNull() const;

protected:
    BaseSinkSource();

    // Construct using the given TypeIO. If takeOver is true the this class
    // will delete the supplied pointer. Otherwise the caller is responsible
    // for this.
    BaseSinkSource (TypeIO* typeIO, Bool takeOver=False);

    // The copy constructor uses reference semantics
    BaseSinkSource (const BaseSinkSource& BaseSinkSource);

    // The assignment operator uses reference semantics
    BaseSinkSource& operator= (const BaseSinkSource& BaseSinkSource);

    virtual ~BaseSinkSource();


    // This variable keeps a pointer to a TypeIO.
    CountedPtr<TypeIO> itsTypeIO;
};



} //# NAMESPACE CASACORE - END

#endif
