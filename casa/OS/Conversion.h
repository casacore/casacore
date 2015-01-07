//# Conversion.h: A class with general conversion definitions
//# Copyright (C) 1996,1999,2001,2002,2003
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

#ifndef CASA_CONVERSION_H
#define CASA_CONVERSION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/string.h>                       // needed for memcpy


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// A class with general conversion definitions
// </summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tConversion" demos="">
// </reviewed>

// <synopsis>
// This class contains the general definitions for the Conversion classes.
// <ul>
// <li>
// It defines the signature for the functions converting from input
// to output format (e.g. from local to canonical format). There is
// a version where the number of values is given and a version where
// the number of bytes is given. The latter is there to be able to use
// memcpy as a conversion function (which will often be the case).
// Note that the signatures only differ in return value.
// <li>
// It defines functions to convert Bools to bits and vice-versa.
// These are used elsewhere to store Bools as space efficient as possible.
// Note that these functions are machine independent (they work on little
// and big endian machines).
// <li>
// It defines a private version of memcpy for compilers having a
// different signature for memcpy (e.g. ObjectCenter and DEC-Alpha).
// </ul>
// Static functions in the classes
// <linkto class=CanonicalConversion>CanonicalConversion</linkto>,
// <linkto class=VAXConversion>VAXConversion</linkto>, and
// <linkto class=IBMConversion>IBMConversion</linkto> convert data
// from/to canonical, VAX, and IBM/360 format, resp..
// <br>Classes derived from
// <linkto class=DataConversion>DataConversion</linkto>
// provide the same functionality in a polymorphic way.
// </synopsis>

// <motivation>
// This provides a common place for definitions used elsewhere.
// It also provides a uniform interface to memcpy.
// </motivation>

//# <todo asof="$DATE$">
//# </todo>


class Conversion
{
public:
    // Define the signature of a function converting <src>nvalues</src>
    // values from internal to external format or vice-versa.
    // These functions are used in the <linkto class=TypeIO>IO framework
    // </linkto>, but are also used in the table system.
    // Examples of such conversions are:
    // <br>- local <-> canonical    (when storing in canonical format)
    // <br>- local <-> local        (when storing in local format)
    // <br>- binary <-> ASCII
    // <br>It returns the number of bytes in <em>external</em> format.
    // (For example the ToLocal/FromLocal functions in class
    // <linkto class=CanonicalConversion>CanonicalConversion</linkto>
    // return the number of bytes in canonical format).
    typedef size_t ValueFunction (void* to, const void* from,
                                  size_t nvalues);

    // Define the signature of a function converting from one
    // format to another providing the number of bytes.
    // It returns the <src>to</src> pointer (similar to memcpy).
    // (For example the byteTo/FromLocalXXX functions in class
    // <linkto class=CanonicalConversion>CanonicalConversion</linkto>.
    typedef void* ByteFunction (void* to, const void* from,
				size_t nbytes);

    // Convert a stream of Bools to output format (as bits).
    // The variable <src>startBit</src> (0-relative) indicates
    // where to start in the <src>to</src> buffer.
    // <group>
    static size_t boolToBit (void* to, const void* from,
                             size_t nvalues);
    static void boolToBit (void* to, const void* from,
			   size_t startBit,
			   size_t nvalues);
    // </group>

    // Convert a stream of Bools to output format (as bits).
    // The variable <src>startBit</src> (0-relative) indicates
    // where to start in the <src>from</src> buffer.
    // <group>
    static size_t bitToBool (void* to, const void* from,
                             size_t nvalues);
    static void bitToBool (void* to, const void* from,
			   size_t startBit,
			   size_t nvalues);
    // </group>

    // Copy a value using memcpy.
    // It differs from memcpy in the return value.
    // <note> This version has the <src>ValueFunction</src> signature,
    // but it expects as input the number of bytes.
    // </note>
    static size_t valueCopy (void* to, const void* from,
                             size_t nbytes);

    // Get a pointer to the memcpy function.
    static ByteFunction* getmemcpy();

private:
    // Copy bits to Bool in an unoptimized way needed when 'to' is not
    // aligned properly.
    static size_t bitToBool_ (void* to, const void* from,
                              size_t nvalues);
};


inline Conversion::ByteFunction* Conversion::getmemcpy()
{
    return memcpy;
}



} //# NAMESPACE CASACORE - END

#endif
