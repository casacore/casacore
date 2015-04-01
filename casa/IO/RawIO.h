//# RawIO.h: Class for IO in local format
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

#ifndef CASA_RAWIO_H
#define CASA_RAWIO_H

#include <casacore/casa/aips.h>
#include <casacore/casa/IO/TypeIO.h>
//# The following should be a forward declaration. But our Complex & DComplex
//# classes are a typedef hence this does not work. Replace the following with
//# forward declarations when Complex and DComplex are no longer typedefs.
#include <casacore/casa/BasicSL/Complex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class ByteIO;
class String;

// <summary>Class for IO in local format.</summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tTypeIO" demos="">
// </reviewed>

// <prerequisite> 
//    <li> <linkto class=ByteIO>ByteIO</linkto> class and derived classes
//    <li> <linkto class=TypeIO>TypeIO</linkto>class
// </prerequisite>

// <synopsis> 
// RawIO is a specialization of class TypeIO to store
// data in local format.
// <p>
// This class is intended for data that will only be used internally
// and will not be exported to machines with a possible different
// data format.
// <p>
// To save storage Bools will be written as bits (using the
// static functions in class <linkto class=Conversion>Conversion</linkto>.
// </synopsis>

// <motivation> 
// Storing data in local format can improve performance on
// little-endian machines like DEC-alpha and PC's.
// </motivation>


class RawIO: public TypeIO
{
public: 
    // Constructor.  The read/write functions will use the given ByteIO object
    // as the data store.  If takeOver is True the this class will delete the
    // supplied pointer. Otherwise the caller is responsible for this.
    explicit RawIO (ByteIO* byteIO, Bool takeOver=False);

    // The copy constructor uses reference semantics
    RawIO (const RawIO& rawIO);

    // The assignment operator uses reference semantics
    RawIO& operator= (const RawIO& rawIO);

    // Destructor.
    ~RawIO();

    // Write the values to the ByteIO object.
    // Bool, complex and String values are handled by the base class.
    // <group>
    virtual size_t write (size_t nvalues, const Bool* value);
    virtual size_t write (size_t nvalues, const Char* data);
    virtual size_t write (size_t nvalues, const uChar* data);
    virtual size_t write (size_t nvalues, const Short* data);
    virtual size_t write (size_t nvalues, const uShort* data);
    virtual size_t write (size_t nvalues, const Int* data);
    virtual size_t write (size_t nvalues, const uInt* data);
    virtual size_t write (size_t nvalues, const Int64* data);
    virtual size_t write (size_t nvalues, const uInt64* data);
    virtual size_t write (size_t nvalues, const Float* data);
    virtual size_t write (size_t nvalues, const Double* data);
    virtual size_t write (size_t nvalues, const Complex* value);
    virtual size_t write (size_t nvalues, const DComplex* value);
    virtual size_t write (size_t nvalues, const String* value);
    // </group>

    // Read the values from the ByteIO object.
    // Bool, complex and String values are handled by the base class.
    // <group>
    virtual size_t read (size_t nvalues, Bool* value);
    virtual size_t read (size_t nvalues, Char* data);
    virtual size_t read (size_t nvalues, uChar* data);
    virtual size_t read (size_t nvalues, Short* data);
    virtual size_t read (size_t nvalues, uShort* data);
    virtual size_t read (size_t nvalues, Int* data);
    virtual size_t read (size_t nvalues, uInt* data);
    virtual size_t read (size_t nvalues, Int64* data);
    virtual size_t read (size_t nvalues, uInt64* data);
    virtual size_t read (size_t nvalues, Float* data);
    virtual size_t read (size_t nvalues, Double* data);
    virtual size_t read (size_t nvalues, Complex* value);
    virtual size_t read (size_t nvalues, DComplex* value);
    virtual size_t read (size_t nvalues, String* value);
    // </group>
};



} //# NAMESPACE CASACORE - END

#endif
