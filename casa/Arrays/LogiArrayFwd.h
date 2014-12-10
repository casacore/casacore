//# LogiArrayFwd.h: Forwards for LogicalArrays.
//# Copyright (C) 1994,1995,1999
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

#ifndef CASA_LOGIARRAYFWD_H
#define CASA_LOGIARRAYFWD_H


#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
//    Forward declarations for LogicalArrays.
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
//
// <prerequisite>
//   <li> <linkto class=Array>Array</linkto>
//   <li> <linkto group="LogiArray.h#LogicalArray">LogicalArray</linkto>
// </prerequisite>
//
// <etymology>
// LogiArrayFwd is short for LogicalArrayForwards, which was too long by
// the old AIPS++ file naming conventions.
// It contains forwards for LogicalArrays.
// </etymology>
//
// <synopsis>
// This file contains forward definitions for LogicalArrays.
// </synopsis>
//
// <motivation>
// There are places where LogicalArrays cannot be defined, i.e. where
// LogiArray.h cannot be included.  In those places, one must provide
// forward declarations for those objects which one is using.
// This is particularly tricky for LogicalArrays, since they are not
// classes, but are rather typedefs.  In order to make these forwards
// easier to get correct, and easier to maintain, this file was created.
// If LogicalArrays should ever be change to a class, only this file would
// need to be changed, instead of every file where the current typedefs
// would have to be changed to class forwards.
// </motivation>
//
// <todo asof="$DATE:$>
//   <li> Change LogicalArrayElem char.
//   <li> Consider making these into classes.
//   <li> Consider replacing with builtin boolean class when that
//          makes it into the C++ compiler.
// </todo>
//
// <linkfrom anchor="LogicalArray forwards" classes="Array MaskedArray">
//    <here>LogicalArray forwards</here> --  Forward declarations for LogicalArrays.
// </linkfrom>
//
// <group name="LogicalArray forwards">


//# Typedefs

// The element type for LogicalArray.
// This should stay a typedef and not a class, so that it will work inside
// of logical expressions without defining a cast to int, which would
// lose its typesafeness.
//
typedef Bool LogicalArrayElem;


//# Forwards

template<class T> class Array;


// Define LogicalArray.
//
typedef Array<LogicalArrayElem> LogicalArray;


// </group>


} //# NAMESPACE CASACORE - END

#endif
