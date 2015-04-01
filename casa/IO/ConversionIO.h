//# ConversionIO.h: Class for IO in a converted format
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

#ifndef CASA_CONVERSIONIO_H
#define CASA_CONVERSIONIO_H

#include <casacore/casa/aips.h>
#include <casacore/casa/IO/TypeIO.h>
#include <casacore/casa/Utilities/CountedPtr.h>
//# The following should be a declaration. But our Complex & DComplex classes
//# are a typedef hence this does not work. Replace the following with
//# forward declarations when Complex and DComplex are no longer typedefs.
#include <casacore/casa/BasicSL/Complex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class DataConversion;
class ByteIO;
class String;

// <summary>Class for IO in a converted format.</summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tTypeIO" demos="">
// </reviewed>

// <prerequisite> 
//    <li> <linkto class=ByteIO>ByteIO</linkto> class and derived classes
//    <li> <linkto class=TypeIO>TypeIO</linkto>class
//    <li> <linkto class=DataConversion>DataConversion</linkto>
// </prerequisite>

// <synopsis> 
// ConversionIO is a specialization of class TypeIO to store
// data in a converted format.
// <p>
// The class converts the data to/from external data and reads/writes
// them from/into the ByteIO object given at construction time.
// Conversion is only done when really needed. If not needed, the
// data is directly read or written.
// <p>
// This class is useful when data can be stored in one of multiple formats.
// Only at construction time the correct <linkto class=DataConversion>
// DataConversion</linkto> class has to be given. Thereafter polymorphism
// ensures that the correct conversion is done when reading or writing.
// </synopsis>


class ConversionIO: public TypeIO
{
public: 
    // Constructor.
    // The read/write functions will use the given <src>ByteIO</src> object
    // as the data store and the given <src>DataConversion</src> object
    // as the conversion engine. 
    // <p>
    // The read and write functions use an intermediate buffer to hold the data
    // in canonical format.  For small arrays it uses a fixed buffer with
    // length <src>bufferLength</src>. For arrays not fitting in this buffer,
    // it uses a temporary buffer allocated on the heap.
    // <p>
    // If takeOver is True this this class will be responsible for deleting the
    // DataConversion and ByteIO pointers.  Otherwise it is the callers
    // responsibility.
    ConversionIO (DataConversion* dataConversion, ByteIO* byteIO,
		  uInt bufferLength=4096, Bool takeOver=False);

    // The copy constructor uses reference semantics
    ConversionIO (const ConversionIO& conversionIO);

    // The assignment operator uses reference semantics
    ConversionIO& operator= (const ConversionIO& conversionIO);

    // Destructor, deletes allocated memory.
    ~ConversionIO();

    // Convert the values and write them to the ByteIO object.
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

    // Read the values from the ByteIO object and convert them.
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

private:
    // Initialize the <src>itsSize</src> and <src>itsCopy</src> variables.
    void init();


    //# The data.
    CountedPtr<DataConversion> itsConversion;
    uInt itsSizeChar;
    uInt itsSizeuChar;
    uInt itsSizeShort;
    uInt itsSizeuShort;
    uInt itsSizeInt;
    uInt itsSizeuInt;
    uInt itsSizeInt64;
    uInt itsSizeuInt64;
    uInt itsSizeFloat;
    uInt itsSizeDouble;
    Bool itsCopyChar;
    Bool itsCopyuChar;
    Bool itsCopyShort;
    Bool itsCopyuShort;
    Bool itsCopyInt;
    Bool itsCopyuInt;
    Bool itsCopyInt64;
    Bool itsCopyuInt64;
    Bool itsCopyFloat;
    Bool itsCopyDouble;
    //# The buffer
    char* itsBuffer;
    uInt itsBufferLength;
};



} //# NAMESPACE CASACORE - END

#endif
