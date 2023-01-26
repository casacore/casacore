//# ByteSource.h: Class for read-only access to data in a given format
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

#ifndef CASA_BYTESOURCE_H
#define CASA_BYTESOURCE_H

#include <casacore/casa/aips.h>
#include <casacore/casa/IO/BaseSinkSource.h>
//# The following should be a forward declaration. But our Complex & DComplex
//# classes are a typedef hence this does not work. Replace the following with
//# forward declarations when Complex and DComplex are no longer typedefs.
#include <casacore/casa/BasicSL/Complex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class TypeIO;
class String;

// <summary>Class for read-only access to data in a given format.</summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tByteSink" demos="">
// </reviewed>

// <prerequisite> 
//    <li> <linkto class=BaseSinkSource>BaseSinkSource</linkto> class
//    <li> <linkto class=TypeIO>TypeIO</linkto> class and derived classes
// </prerequisite> 

// <etymology> 
// A source is the place where bytes are read from.
// </etymology>

// <synopsis> 
// ByteSource provides read-only access to a typed byte stream in the
// Casacore IO framework. The base class <src>BaseSinkSource</src>
// contains common functions like <src>seek</src>.
// <p>
// The object is constructed using a typed byte stream. This stream
// is an instance of a class derived from class
// <linkto class=TypeIO>TypeIO</linkto>. This makes it possible to
// read the data in any format (e.g. CanonicalIO or RawIO).
// <br> In its turn TypeIO uses an instance of a class derived from class
// <linkto class=ByteIO>ByteIO</linkto>. This makes it possible to
// use any input stream (e.g. file, memory).
// </synopsis>

// <example>
// <srcblock>
//    // Construct the correct input stream.
//    RegularFileIO filio ("file.name");
//    CanonicalIO canio (&filio);
//    ByteSource source (&canio);
//    // Read data.
//    int32_t vali;
//    bool flag;
//    source >> vali >> flag;
// </srcblock>
// </example>

// <motivation> 
// This class makes it possible to deny write-access to an IO stream.
// </motivation>


class ByteSource: virtual public BaseSinkSource
{
public: 
    // Default constructor.
    // This creates an invalid object, but is present for convenience.
    ByteSource();

    // Construct from given TypeIO object.  The constructor does not copy the
    // object, but only keeps a pointer to it. If takeOver is true the this
    // class will delete the supplied pointer. Otherwise the caller is
    // responsible for this.
    ByteSource (TypeIO* typeIO, bool takeOver=false);

    // The copy constructor uses reference semantics
    ByteSource (const ByteSource& source);

    // The assignment operator uses reference semantics
    ByteSource& operator= (const ByteSource& source);

    // destructor
    ~ByteSource();
  
    // These functions read one value of the given type.
    // If this function does not succeed, an exception will be thrown.
    // <group>
    ByteSource& operator>> (bool& value);
    ByteSource& operator>> (char& value);
    ByteSource& operator>> (unsigned char& value);
    ByteSource& operator>> (int16_t& value);
    ByteSource& operator>> (uint16_t& value);
    ByteSource& operator>> (int32_t& value);
    ByteSource& operator>> (uint32_t& value);
    ByteSource& operator>> (int64_t& value);
    ByteSource& operator>> (uint64_t& value);
    ByteSource& operator>> (float& value);
    ByteSource& operator>> (double& value);
    ByteSource& operator>> (Complex& value);
    ByteSource& operator>> (DComplex& value);
    ByteSource& operator>> (String& value);
    // </group>

    // These functions read multiple values of the given type.
    // If this function does not succeed, an exception will be thrown.
    // <group>
    void read (size_t nvalues, bool* value);
    void read (size_t nvalues, char* value);
    void read (size_t nvalues, unsigned char* value);
    void read (size_t nvalues, int16_t* value);
    void read (size_t nvalues, uint16_t* value);
    void read (size_t nvalues, int32_t* value);
    void read (size_t nvalues, uint32_t* value);
    void read (size_t nvalues, int64_t* value);
    void read (size_t nvalues, uint64_t* value);
    void read (size_t nvalues, float* value);
    void read (size_t nvalues, double* value);
    void read (size_t nvalues, Complex* value);
    void read (size_t nvalues, DComplex* value);
    void read (size_t nvalues, String* value);
    // </group>

protected:
};



} //# NAMESPACE CASACORE - END

#endif
