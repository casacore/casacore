//# MaskArrIO.h: Write out an ascii representation of a MaskedArray.
//# Copyright (C) 1993,1994,1995,1999,2000,2001
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

#ifndef CASA_MASKARRIO_H
#define CASA_MASKARRIO_H

#include <casacore/casa/aips.h>

//# Forward declarations
#include <casacore/casa/iosfwd.h>
namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> class MaskedArray;
template<class T> class MaskedArray;


// <summary>
//    Ascii input/output operations for MaskedArrays.
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMaskArrIO">
//
// <prerequisite>
//   <li> <linkto class=Array>Array</linkto>
//   <li> <linkto class=MaskedArray>MaskedArray</linkto>
//   <li> <linkto group="ArrayIO.h#Array IO">ArrayIO</linkto>
// </prerequisite>
//
// <etymology>
// MaskArrIO is short for MaskedArrayIO, which is too long by the old
// AIPS++ file naming conventions.  This file contains global functions
// for writing out ascii representations of masked arrays.
// </etymology>
//
// <synopsis>
// These functions write out masked arrays in ascii representation.
// They simply write out the Array and the LogicalArray which is the mask
// using the ascii output functions for these objects.
// </synopsis>
//
// <example>
// <srcblock>
//   Vector<Int> a(10);
//   LogicalVector b(10);
//   MaskedArray m (a,b);
//      . . .
//   cout << m;
// </srcblock>
// This example writes out m in ascii.  It writes first a and then
// the mask constructed from b.
// </example>
//
// <motivation>
// These are primarily for debugging, so that one can examine the MaskedArray.
// Since MaskedArrays are manipulators of Arrays, it was not thought to
// be necessary, or even a good idea, to have other kinds of IO defined
// for them.
// </motivation>
//
// <linkfrom anchor="MaskedArray IO" classes="MaskedArray">
//    <here>MaskedArray IO</here> -- Ascii input/output operations
//    for MaskedArrays.
// </linkfrom>
//
// <group name="MaskedArray IO">


// 
// Write out an ascii representation of a MaskedArray.
// The component Array and LogicalArray are written out sequentially.
template<class T> ostream & operator<< (ostream &, const MaskedArray<T> &);


// </group>


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Arrays/MaskArrIO.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
