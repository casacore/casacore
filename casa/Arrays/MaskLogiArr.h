//# MaskLogiArr.h: Masked logical arrays.
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

#ifndef CASA_MASKLOGIARR_H
#define CASA_MASKLOGIARR_H

//# There is no source file, so this pragma is not needed.
#if 0
#endif


#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/MaskLogiArrFwd.h>
#include <casacore/casa/Arrays/LogiArray.h>
#include <casacore/casa/Arrays/MaskedArray.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
//    Masked LogicalArrays.
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMaskedArray">
//
// <prerequisite>
//   <li> <linkto class=Array>Array</linkto>
//   <li> <linkto group="LogiArray.h#LogicalArray">LogicalArray</linkto>
//   <li> <linkto class=MaskedArray>MaskedArray</linkto>
//   <li> <linkto groupt="MaskLogiArrFwd.h#MaskedLogicalArray forwards">MaskLogiArrFwd</linkto>
// </prerequisite>
//
// <etymology>
// MaskLogiArr is short for MaskedLogicalArray, which is too long by
// the old AIPS++ file naming conventions.  This file contains typedefs
// for MaskedLogicalArrays.
// </etymology>
//
// <synopsis>
// These classes are analogous to MaskedArrays, where the type of the
// Array is a LogicalArray.
// </synopsis>
//
// <motivation>
// One wants to be able to mask and use LogicalArrays the same way one
// can mask and use general Arrays.
// </motivation>
//
// <linkfrom anchor=MaskedLogicalArray classes="Array Vector Matrix Cube MaskedArray">
//    <here>MaskedLogicalArray</here> -- Masked LogicalArrays.
// </linkfrom>
//
// <group name=MaskedLogicalArray>

//# This is empty.  Everything is done by the include files.

// </group>

} //# NAMESPACE CASACORE - END

#endif
