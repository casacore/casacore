//# LogiMatrix.h:  Logical valued Matrices.
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

#ifndef CASA_LOGIMATRIX_H
#define CASA_LOGIMATRIX_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/LogiArray.h>
#include <casacore/casa/Arrays/Matrix.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
//    Logical valued Matrices.
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
//
// <prerequisite>
//   <li> <linkto class=Array>Array</linkto>
//   <li> <linkto class=Matrix>Matrix</linkto>
//   <li> <linkto group="LogiArrayFwd.h#LogicalArray forwards">LogicalArrayFwd</linkto>
//   <li> <linkto group="LogiArray.h#LogicalArray">LogicalArray</linkto>
// </prerequisite>
//
// <etymology>
// LogicalMatrix declares logical valued Matrices.
// </etymology>
//
// <synopsis>
// This file contains the declarations for LogicalMatrixs.
// </synopsis>
//
// <motivation>
// These are the Matrix specialization of LogicalArrays.
// </motivation>
//
// <todo asof="$DATE:$>
//   <li> Consider making these into classes.
//   <li> Consider replacing with builtin boolean class when that
//          makes it into the C++ compiler.
// </todo>
//
// <linkfrom anchor="LogicalMatrix" classes="Array Matrix MaskedArray">
//    <here>LogicalMatrix</here> --  Logical valued Matrices.
// </linkfrom>
//
// <group name="LogicalMatrix">


// Define LogicalMatrix.
//
typedef Matrix<LogicalArrayElem> LogicalMatrix;


// </group>


} //# NAMESPACE CASACORE - END

#endif
