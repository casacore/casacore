//# LogiArray.h:  Logical valued Arrays.
//# Copyright (C) 1994,1995
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

#if !defined (AIPS_LOGIARRAY_H)
#define AIPS_LOGIARRAY_H

//# There is no source file, so this pragma is not needed.
#if 0
#if defined(_AIX)
#pragma implementation ("LogiArray.cc")
#endif
#endif


#include <aips/aips.h>
#include <aips/Arrays/LogiArrayFwd.h>
#include <aips/Arrays/Array.h>


// <summary>
//    Logical valued Arrays.
// </summary>
// <reviewed reviewer="" date="" tests="">
//
// <prerequisite>
//   <li> <linkto class=Array>Array</linkto>
//   <li> <linkto group="LogiArrayFwd.h#LogicalArray forwards">LogicalArrayFwd</linkto>
// </prerequisite>
//
// <etymology>
// LogicalArray declares logical valued Arrays.
// </etymology>
//
// <synopsis>
// This file contains the declarations for LogicalArrays.
// </synopsis>
//
// <motivation>
// One needs to have logical valued Arrays.  They are the result of
// logical operations on Arrays.  They can also be created in other ways.
// They are used as masks for MaskedArrays.
//
// Array<Bool> would have served the purpose.  However, it is very space
// inefficient.  Instead, the concept has been abstracted.  Currently,
// the implementation of LogicalArray is Array<LogicalArrayElem>, done
// with typedefs.  The type of LogicalArrayElem can be changed at any time.
// Later, if desired, LogicalArray can be made to be a true class, without
// requiring more than a recompile of code which uses it.
// </motivation>
//
// <todo asof="$DATE:$>
//   <li> Consider making these into classes.
//   <li> Consider replacing with builtin boolean class when that
//          makes it into the C++ compiler.
// </todo>
//
// <linkfrom anchor="LogicalArray" classes="Array Vector Matrix Cube MaskedArray">
//    <here>LogicalArray</here> -- Logical valued Arrays.
// </linkfrom>
//
// <group name="LogicalArray">

//# This is empty.  Everything is done by the include files.

// </group>

#endif
