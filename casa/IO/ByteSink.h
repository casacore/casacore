//# ByteSink.h: Class for write-only access to data in a given format
//# Copyright (C) 1996,1998,1999,2001
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

#ifndef CASA_BYTESINK_H
#define CASA_BYTESINK_H

#include <casacore/casa/aips.h>
#include <casacore/casa/IO/BaseSinkSource.h>
//# The following should be a forward declaration. But our Complex & DComplex
//# classes are a typedef hence this does not work. Replace the following with
//# forward declarations when Complex and DComplex are no longer typedefs.
#include <casacore/casa/BasicSL/Complex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class TypeIO;
class String;

// <summary>Class for write-only access to data in a given format.</summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tByteSink" demos="">
// </reviewed>

// <prerequisite> 
//    <li> <linkto class=BaseSinkSource>BaseSinkSource</linkto> class
//    <li> <linkto class=TypeIO>TypeIO</linkto> class and derived classes
// </prerequisite> 

// <etymology> 
// A sink is the place where bytes are written to.
// </etymology>

// <synopsis> 
// ByteSink provides write-only access to a typed byte stream in the
// Casacore IO framework. The base class <src>BaseSinkSource</src>
// contains common functions like <src>seek</src>.
// <p>
// The object is constructed using a typed byte stream. This stream
// is an instance of a class derived from class
// <linkto class=TypeIO>TypeIO</linkto>. This makes it possible to
// store the data in any format (e.g. CanonicalIO or RawIO).
// <br> In its turn TypeIO uses an instance of a class derived from class
// <linkto class=ByteIO>ByteIO</linkto>. This makes it possible to
// use any output stream (e.g. file, memory).
// <p>
// Note that in general <src>ByteSink</src> will only be used
// for write-only streams like sockets or pipes.
// Class <linkto class=ByteSinkSource>ByteSinkSource</linkto>
// is the obvious choice for read/write streams.
// </synopsis>

// <example>
// <srcblock>
//    // Construct the correct output stream.
//    MemoryIO memio;
//    CanonicalIO canio (&memio);
//    ByteSink sink (&canio);
//    // Write data.
//    Int vali;
//    sink << vali << True;
// </srcblock>
// </example>

// <motivation> 
// This class makes it possible to deny read-access to an IO stream.
// </motivation>


class ByteSink: virtual public BaseSinkSource
{
public: 
    // Default constructor.
    // This creates an invalid object, but is present for convenience.
    ByteSink();

    // Construct from given TypeIO object.  The constructor does not copy the
    // object, but only keeps a pointer to it. If takeOver is true the this
    // class will delete the supplied pointer. Otherwise the caller is
    // responsible for this.
    ByteSink (TypeIO* typeIO, Bool takeOver=False);
 
    // The copy constructor uses reference semantics
    ByteSink (const ByteSink& sink);

    // The assignment operator uses reference semantics
    ByteSink& operator= (const ByteSink& sink);

    // destructor
    ~ByteSink();

    // These functions write one value of the given type.
    // If this function does not succeed, an exception will be thrown.
    // <group>
    ByteSink& operator<< (Bool value);
    ByteSink& operator<< (Char value);
    ByteSink& operator<< (uChar value);
    ByteSink& operator<< (Short value);
    ByteSink& operator<< (uShort value);
    ByteSink& operator<< (Int value);
    ByteSink& operator<< (uInt value);
    ByteSink& operator<< (Int64 value);
    ByteSink& operator<< (uInt64 value);
    ByteSink& operator<< (Float value);
    ByteSink& operator<< (Double value);
    ByteSink& operator<< (const Complex& value);
    ByteSink& operator<< (const DComplex& value);
    ByteSink& operator<< (const String& value);
    ByteSink& operator<< (const Char* value);
    // </group>

    // These functions write multiple values of the given type.
    // If this function does not succeed, an exception will be thrown.
    // <group>
    void write (size_t nvalues, const Bool* value);
    void write (size_t nvalues, const Char* value);
    void write (size_t nvalues, const uChar* value);
    void write (size_t nvalues, const Short* value);
    void write (size_t nvalues, const uShort* value);
    void write (size_t nvalues, const Int* value);
    void write (size_t nvalues, const uInt* value);
    void write (size_t nvalues, const Int64* value);
    void write (size_t nvalues, const uInt64* value);
    void write (size_t nvalues, const Float* value);
    void write (size_t nvalues, const Double* value);
    void write (size_t nvalues, const Complex* value);
    void write (size_t nvalues, const DComplex* value);
    void write (size_t nvalues, const String* value);
     // </group>
};



} //# NAMESPACE CASACORE - END

#endif
