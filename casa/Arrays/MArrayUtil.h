//# MArrayUtil.h: Utility functions for MArrays
//# Copyright (C) 2012
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

#ifndef CASA_MARRAYUTIL_H
#define CASA_MARRAYUTIL_H


//# Includes
#include <casa/Arrays/ArrayUtil.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary>
// Reorder the axes of the data in an MArray object
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tArrayUtil2.cc">

// <synopsis>
// This function makes it possible to reorder the axes of an MArray.
// Both the data and the optional mask are reordered.
// The resulting array is a copy of the input array with its data
// moved around according to the new array order.
// If the order does not change, a copy is returned if the
// <src>alwaysCopy</src> is true. Otherwise a reference of the
// input array is returned.
// <p>
// The <src>newAxisOrder</src> defines the new axes order.
// Its length can be less than the dimensionality of the input array.
// It is appended with the non-specified axes in their natural order.
// <src>newAxisOrder(i)</src> gives the axis in the original array
// which will now get axis <src>i</src>.
// </synopsis>

// <example>
// <srcblock>
//   MArray<Int> result = reorderArray (someArray, IPosition(2,1,3));
// </srcblock>
// Say that someArray is a 4D array with shape [3,4,5,6].
// The non-specified axes get appended to the axis order
// specification [1,3] resulting in [1,3,0,2].
// <br> This means that axis 1 gets axis 0, axis 3 gets axis 1, axis 0 gets
// axis 2, and axis 2 gets axis 3.
// Thus the resulting shape is [4,6,3,5] and the data are moved accordingly.
// </example>

// <group name=reorderMArray>
template<class T>
MArray<T> reorderArray (const MArray<T>& array,
                        const IPosition& newAxisOrder,
                        Bool alwaysCopy = True)
{
  return (array.hasMask()  ?
    MArray<T> (reorderArray(array.array(), newAxisOrder, alwaysCopy),
               reorderArray(array.mask(),  newAxisOrder, alwaysCopy)) :
    MArray<T> (reorderArray(array.array(), newAxisOrder, alwaysCopy)));
}
// </group>

} //# NAMESPACE CASA - END

#endif
