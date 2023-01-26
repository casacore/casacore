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
    // If takeOver is true this this class will be responsible for deleting the
    // DataConversion and ByteIO pointers.  Otherwise it is the callers
    // responsibility.
    ConversionIO (DataConversion* dataConversion, ByteIO* byteIO,
		  uint32_t bufferLength=4096, bool takeOver=false);

    // The copy constructor uses reference semantics
    ConversionIO (const ConversionIO& conversionIO);

    // The assignment operator uses reference semantics
    ConversionIO& operator= (const ConversionIO& conversionIO);

    // Destructor, deletes allocated memory.
    ~ConversionIO();

    // Convert the values and write them to the ByteIO object.
    // bool, complex and String values are handled by the base class.
    // <group>
    virtual size_t write (size_t nvalues, const bool* value);
    virtual size_t write (size_t nvalues, const char* data);
    virtual size_t write (size_t nvalues, const unsigned char* data);
    virtual size_t write (size_t nvalues, const int16_t* data);
    virtual size_t write (size_t nvalues, const uint16_t* data);
    virtual size_t write (size_t nvalues, const int32_t* data);
    virtual size_t write (size_t nvalues, const uint32_t* data);
    virtual size_t write (size_t nvalues, const int64_t* data);
    virtual size_t write (size_t nvalues, const uint64_t* data);
    virtual size_t write (size_t nvalues, const float* data);
    virtual size_t write (size_t nvalues, const double* data);
    virtual size_t write (size_t nvalues, const Complex* value);
    virtual size_t write (size_t nvalues, const DComplex* value);
    virtual size_t write (size_t nvalues, const String* value);
    // </group>

    // Read the values from the ByteIO object and convert them.
    // bool, complex and String values are handled by the base class.
    // <group>
    virtual size_t read (size_t nvalues, bool* value);
    virtual size_t read (size_t nvalues, char* data);
    virtual size_t read (size_t nvalues, unsigned char* data);
    virtual size_t read (size_t nvalues, int16_t* data);
    virtual size_t read (size_t nvalues, uint16_t* data);
    virtual size_t read (size_t nvalues, int32_t* data);
    virtual size_t read (size_t nvalues, uint32_t* data);
    virtual size_t read (size_t nvalues, int64_t* data);
    virtual size_t read (size_t nvalues, uint64_t* data);
    virtual size_t read (size_t nvalues, float* data);
    virtual size_t read (size_t nvalues, double* data);
    virtual size_t read (size_t nvalues, Complex* value);
    virtual size_t read (size_t nvalues, DComplex* value);
    virtual size_t read (size_t nvalues, String* value);
    // </group>

private:
    // Initialize the <src>itsSize</src> and <src>itsCopy</src> variables.
    void init();


    //# The data.
    CountedPtr<DataConversion> itsConversion;
    uint32_t itsSizeChar;
    uint32_t itsSizeuChar;
    uint32_t itsSizeShort;
    uint32_t itsSizeuShort;
    uint32_t itsSizeInt;
    uint32_t itsSizeuInt;
    uint32_t itsSizeInt64;
    uint32_t itsSizeuInt64;
    uint32_t itsSizeFloat;
    uint32_t itsSizeDouble;
    bool itsCopyChar;
    bool itsCopyuChar;
    bool itsCopyShort;
    bool itsCopyuShort;
    bool itsCopyInt;
    bool itsCopyuInt;
    bool itsCopyInt64;
    bool itsCopyuInt64;
    bool itsCopyFloat;
    bool itsCopyDouble;
    //# The buffer
    char* itsBuffer;
    uint32_t itsBufferLength;
};



} //# NAMESPACE CASACORE - END

#endif
