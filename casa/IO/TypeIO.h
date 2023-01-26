//# TypeIO.h: Abstract base class for IO of data in a type-dependent format
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

#ifndef CASA_TYPEIO_H
#define CASA_TYPEIO_H

#include <casacore/casa/aips.h>
#include <casacore/casa/IO/ByteIO.h>
#include <casacore/casa/Utilities/CountedPtr.h>
//# The following should be a forward declaration. But our Complex & DComplex
//# classes are a typedef hence this does not work. Replace the following with
//# forward declarations when Complex and DComplex are no longer typedefs.
#include <casacore/casa/BasicSL/Complex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class String;

// <summary>Abstract base class for IO of data in a type-dependent format</summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tTypeIO" demos="">
// </reviewed>

// <prerequisite> 
//    <li> <linkto class=ByteIO>ByteIO</linkto> class and derived classes
// </prerequisite>

// <synopsis> 
// This class is the abstract base class for doing IO in a type-dependent
// way. Derived from it are classes like <linkto class=CanonicalIO>
// CanonicalIO</linkto> doing the actual formatting of the data.
// <p>
// The TypeIO classes convert the data to/from the given format
// using the static conversion functions in the classes like
// <linkto class=CanonicalConversion>CanonicalConversion</linkto>.
// The data is written to or read from the <linkto class=ByteIO>ByteIO</linkto>
// object given when constructing the TypeIO object.
// <p>
// TypeIO declares the virtual functions read and write to read/write
// one or more values of a given data type. Usually the derived classes
// have to implement these functions. An exception are the functions
// handling bool, complex and String values. These functions have a
// default implementation in this base class. However, if needed
// they can be overwritten in derived classes.
// </synopsis>

// <motivation> 
// The base class is needed for polymorphic type-dependent IO.
// Furthermore the common functionality can be implemented here.
// </motivation>


class TypeIO
{
public: 
    // Constructor.
    // The read/write functions will use the given ByteIO object
    // as the data store.
    explicit TypeIO (ByteIO* byteIO, bool takeOver=false);

    virtual ~TypeIO();
    
    // Functions to return a reference to the ByteIO class.
    // <group>
    const ByteIO& byteIO() const;
    ByteIO& byteIO();
    // </group>

    // Convert the values and write them to the ByteIO object.
    // By default Bools are stored as bits, Complex as 2 floats,
    // DComplex as 2 doubles and String as a length (uint32_t) and chars.
    // If it does not succeed an exception will be thrown.
    // <group>
    virtual size_t write (size_t nvalues, const bool* value);
    virtual size_t write (size_t nvalues, const char* value) = 0;
    virtual size_t write (size_t nvalues, const unsigned char* value) = 0;
    virtual size_t write (size_t nvalues, const int16_t* value) = 0;
    virtual size_t write (size_t nvalues, const uint16_t* value) = 0;
    virtual size_t write (size_t nvalues, const int32_t* value) = 0;
    virtual size_t write (size_t nvalues, const uint32_t* value) = 0;
    virtual size_t write (size_t nvalues, const int64_t* value) = 0;
    virtual size_t write (size_t nvalues, const uint64_t* value) = 0;
    virtual size_t write (size_t nvalues, const float* value) = 0;
    virtual size_t write (size_t nvalues, const double* value) = 0;
    virtual size_t write (size_t nvalues, const Complex* value);
    virtual size_t write (size_t nvalues, const DComplex* value);
    virtual size_t write (size_t nvalues, const String* value);
    // </group>
   
    // Read the values from the ByteIO object and convert them.
    // By default Bools are stored as bits, Complex as 2 floats,
    // DComplex as 2 doubles and String as a length (uint32_t) and chars.
    // If it does not succeed an exception will be thrown.
    // <group>
    virtual size_t read (size_t nvalues, bool* value);
    virtual size_t read (size_t nvalues, char* value) = 0;
    virtual size_t read (size_t nvalues, unsigned char* value) = 0;
    virtual size_t read (size_t nvalues, int16_t* value) = 0;
    virtual size_t read (size_t nvalues, uint16_t* value) = 0;
    virtual size_t read (size_t nvalues, int32_t* value) = 0;
    virtual size_t read (size_t nvalues, uint32_t* value) = 0;
    virtual size_t read (size_t nvalues, int64_t* value) = 0;
    virtual size_t read (size_t nvalues, uint64_t* value) = 0;
    virtual size_t read (size_t nvalues, float* value) = 0;
    virtual size_t read (size_t nvalues, double* value) = 0;
    virtual size_t read (size_t nvalues, Complex* value);
    virtual size_t read (size_t nvalues, DComplex* value);
    virtual size_t read (size_t nvalues, String* value);
    // </group>

    // This function sets the position on the given offset.
    // The seek option defines from which file position the seek is done.
    // -1 is returned if not seekable.
    // <group>
    int64_t seek (int64_t offset, ByteIO::SeekOption = ByteIO::Begin);
    int64_t seek (int32_t offset, ByteIO::SeekOption = ByteIO::Begin);
    // </group>
    
    // Is the TypeIO stream readable?
    bool isReadable() const;

    // Is the TypeIO stream writable?
    bool isWritable() const;

    // Is the TypeIO stream seekable?
    bool isSeekable() const;

protected:    
    // This variable keeps a pointer to a ByteIO.
    CountedPtr<ByteIO> itsByteIO;

    // The copy constructor uses reference semantics.
    TypeIO (const TypeIO& TypeIO);

    // The assignment operator uses reference semantics.
    TypeIO& operator= (const TypeIO& typeIO);
};




} //# NAMESPACE CASACORE - END

#endif
