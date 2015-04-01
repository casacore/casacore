//# LogiCube.h:  Logical valued Cubes.
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

#ifndef CASA_LOGICUBE_H
#define CASA_LOGICUBE_H


#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/LogiArray.h>
#include <casacore/casa/Arrays/Cube.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
//    Logical valued Cubes.
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
//
// <prerequisite>
//   <li> <linkto class=Array>Array</linkto>
//   <li> <linkto class=Cube>Cube</linkto>
//   <li> <linkto group="LogiArrayFwd.h#LogicalArray forwards">LogicalArrayFwd</linkto>
//   <li> <linkto group="LogiArray.h#LogicalArray">LogicalArray</linkto>
// </prerequisite>
//
// <etymology>
// LogicalCube declares logical valued Cubes.
// </etymology>
//
// <synopsis>
// This file contains the declarations for LogicalCubes.
// </synopsis>
//
// <motivation>
// These are the Cube specialization of LogicalArrays.
// </motivation>
//
// <todo asof="$DATE:$>
//   <li> Consider making these into classes.
//   <li> Consider replacing with builtin boolean class when that
//          makes it into the C++ compiler.
// </todo>
//
// <linkfrom anchor="LogicalCube" classes="Array Cube MaskedArray">
//    <here>LogicalCube</here> --  Logical valued Cubes.
// </linkfrom>
//
// <group name="LogicalCube">


// Define LogicalCube.
//
typedef Cube<LogicalArrayElem> LogicalCube;


// </group>


} //# NAMESPACE CASACORE - END

#endif
