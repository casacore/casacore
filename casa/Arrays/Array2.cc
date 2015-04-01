//# Array2.cc: Template Arrays with slices, logical operations, and arithmetic
//# Copyright (C) 1993,1994,1995,1999
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

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/ArrayError.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// This probably isn't of interest to normal users. It returns the "volume" of
// an array (i.e. "nelements").
size_t ArrayVolume (uInt Ndim, const ssize_t *Shape)
{
    uInt i;
    if (aips_debug) {
	for (i=0; i < Ndim; i++)
	    if (Shape[i] < 0)
		throw(ArrayError("::ArrayVolume - negative shape"));
    }
    if (Ndim == 0)
	return 0;
    size_t total=1;
    for(i=0; i < Ndim; i++)
	total *= Shape[i];

    return total;
}

// This probably isn't of interest to normal users. Given a decimated
// array with a non-zero origin, what is the linear index into storage.
// Here we assume that the Shape is the original length, i.e. has INC
// in it.
size_t ArrayIndexOffset (uInt Ndim, const ssize_t *Shape,
                         const ssize_t *Origin, const ssize_t *Inc,
                         const IPosition &Index)
{
    uInt i;
    if (aips_debug) {
	for (i=0; i < Ndim; i++)
	    if (Index(i) < Origin[i] || Index(i) > (Origin[i] + Shape[i] - 1) ||
		Shape[i] < 0 || Inc[i] < 1)
		throw(ArrayError("::ArrayIndexOffset - negative shape or inc"
				 "<1 or out-of-bounds index"));
    }
    size_t offset = (Index(0) - Origin[0])*Inc[0];
    for (i=1; i < Ndim; i++)
	offset += (Index(i) - Origin[i])*Inc[i]*ArrayVolume(i, Shape);

    return offset;
}

size_t ArrayIndexOffset (uInt Ndim, const ssize_t *Shape,
                         const ssize_t *Inc, const IPosition &Index)
{
    uInt i;
    if (aips_debug) {
	for (i=0; i < Ndim; i++)
	    if (Index(i) < 0 || Index(i) >= Shape[i] ||
		Shape[i] < 0 || Inc[i] < 1)
		throw(ArrayError("::ArrayIndexOffset - negative shape or inc"
				 "<1 or out-of-bounds index"));
    }
    size_t offset = Index(0)*Inc[0];
    for (i=1; i < Ndim; i++)
	offset += Index(i)*Inc[i]*ArrayVolume(i, Shape);

    return offset;
}

} //# NAMESPACE CASACORE - END

