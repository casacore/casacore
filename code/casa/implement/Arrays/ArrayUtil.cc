//# ArrayUtil.cc: Utility functions for arrays (templated)
//# Copyright (C) 1995,2001
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

#include <aips/Arrays/ArrayUtil.h>
#include <aips/Arrays/ArrayError.h>


template<class T>
Array<T> concatenateArray (const Array<T>& left, const Array<T>& right)
{
    if (left.nelements() == 0) {
	return right.copy();
    }
    if (right.nelements() == 0) {
	return left.copy();
    }
    IPosition shape = right.shape();
    IPosition leftShape = left.shape();
    uInt ndim = shape.nelements();
    if (! shape.isEqual (leftShape, ndim-1)) {
	throw (ArrayConformanceError ("concatenateArray(left,right)"));
    }
    shape(ndim-1) += leftShape(ndim-1);
    Array<T> result (shape);
    IPosition start(ndim, 0);
    result (start, leftShape-1) = left;
    start(ndim-1) = leftShape(ndim-1);
    result (start, shape-1) = right;
    return result;
}
