//# CanonicalIO.h: Class for IO in canonical format
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

#ifndef CASA_CANONICALIO_H
#define CASA_CANONICALIO_H

#include <casa/aips.h>
#include <casa/IO/TypeIO.h>
//# The following should be a forward declaration. But our Complex & DComplex
//# classes are a typedef hence this does not work. Replace the following with
//# forward declarations when Complex and DComplex are no longer typedefs.
#include <casa/BasicSL/Complex.h>

namespace casa { //# NAMESPACE CASA - BEGIN

class ByteIO;
class String;

// <summary>Class for IO in canonical format.</summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tTypeIO" demos="">
// </reviewed>

// <prerequisite> 
//    <li> <linkto class=ByteIO>ByteIO</linkto> class and derived classes
//    <li> <linkto class=TypeIO>TypeIO</linkto>class
//    <li> <linkto class=CanonicalConversion>CanonicalConversion</linkto>
// </prerequisite>

// <synopsis> 
// CanonicalIO is a specialization of class TypeIO to store
// data in canonical format.
// <p>
// The class converts the data to/from canonical data and reads/writes
// them from/into the ByteIO object given at construction time.
// Conversion is only done when really needed. If not needed, the
// data is directly read or written.
// <p>
// Canonical format is big-endian IEEE format, where longs are 8 bytes.
// Bools are stored as bits to be as space-efficient as possible.
// This means that on a 32-bit SUN or HP conversions only have to be done
// for Bools and longs. For a DEC-alpha, however, the data will always
// be converted because it is a little-endian machine.
// </synopsis>


class CanonicalIO: public TypeIO
{
public: 
    // Constructor.
    // The read/write functions will use the given ByteIO object
    // as the data store.
    // <p>
    // The read and write functions use an intermediate buffer to hold the data
    // in canonical format.  For small arrays it uses a fixed buffer with
    // length <src>bufferLength</src>. For arrays not fitting in this buffer,
    // it uses a temporary buffer allocated on the heap.
    // <p>
    // If takeOver is True the this class will delete the supplied
    // pointer. Otherwise the caller is responsible for this.
    explicit CanonicalIO (ByteIO* byteIO, uInt bufferLength=4096,
			  Bool takeOver=False);

    // The copy constructor uses reference semantics
    CanonicalIO (const CanonicalIO& canonicalIO);

    // The assignment operator uses reference semantics
    CanonicalIO& operator= (const CanonicalIO& canonicalIO);

    // Destructor, deletes allocated memory.
    ~CanonicalIO();

    // Convert the values and write them to the ByteIO object.
    // Bool, complex and String values are handled by the base class.
    // <group>
    virtual uInt write (uInt nvalues, const Bool* value);
    virtual uInt write (uInt nvalues, const Char* data);
    virtual uInt write (uInt nvalues, const uChar* data);
    virtual uInt write (uInt nvalues, const Short* data);
    virtual uInt write (uInt nvalues, const uShort* data);
    virtual uInt write (uInt nvalues, const Int* data);
    virtual uInt write (uInt nvalues, const uInt* data);
    virtual uInt write (uInt nvalues, const Int64* data);
    virtual uInt write (uInt nvalues, const uInt64* data);
    virtual uInt write (uInt nvalues, const Float* data);
    virtual uInt write (uInt nvalues, const Double* data);
    virtual uInt write (uInt nvalues, const Complex* value);
    virtual uInt write (uInt nvalues, const DComplex* value);
    virtual uInt write (uInt nvalues, const String* value);
    // </group>

    // Read the values from the ByteIO object and convert them.
    // Bool, complex and String values are handled by the base class.
    // <group>
    virtual uInt read (uInt nvalues, Bool* value);
    virtual uInt read (uInt nvalues, Char* data);
    virtual uInt read (uInt nvalues, uChar* data);
    virtual uInt read (uInt nvalues, Short* data);
    virtual uInt read (uInt nvalues, uShort* data);
    virtual uInt read (uInt nvalues, Int* data);
    virtual uInt read (uInt nvalues, uInt* data);
    virtual uInt read (uInt nvalues, Int64* data);
    virtual uInt read (uInt nvalues, uInt64* data);
    virtual uInt read (uInt nvalues, Float* data);
    virtual uInt read (uInt nvalues, Double* data);
    virtual uInt read (uInt nvalues, Complex* value);
    virtual uInt read (uInt nvalues, DComplex* value);
    virtual uInt read (uInt nvalues, String* value);
    // </group>

private:
    //# The buffer
    char* itsBuffer;
    uInt itsBufferLength;
};



} //# NAMESPACE CASA - END

#endif
