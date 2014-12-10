//# TSMShape.cc: A vector of integers, used to index into arrays.
//# Copyright (C) 1994,1995,1996,1997
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

#include <casacore/tables/DataMan/TSMShape.h>
#include <casacore/casa/Arrays/ArrayError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TSMShape::TSMShape()
: data_p (),
  size_p (0)
{}

TSMShape::TSMShape (const IPosition& shape)
: data_p (shape.nelements()),
  size_p (shape.nelements())
{
    if (size_p > 0) {
	data_p(0) = 1;
	for (uInt i=1; i<size_p; i++) {
	    data_p(i) = data_p(i-1) * shape(i-1);
	}
    }
}

TSMShape::TSMShape (const TSMShape& other)
: data_p (other.data_p),
  size_p (other.size_p)
{}
    
TSMShape& TSMShape::operator= (const TSMShape& other)
{
    data_p = other.data_p;
    size_p = other.size_p;
    return *this;
}

TSMShape::~TSMShape()
{}

// Calculate the offset for a given position.
size_t TSMShape::offset (const IPosition& position) const
{
    if (size_p != position.nelements()) {
	throw (ArrayConformanceError(
                               "TSMShape::offset - shapes do not conform"));
    }
    size_t off = 0;
    for (uInt i=0; i<size_p; i++) {
	off += position(i) * data_p(i);
    }
    return off;
}

size_t TSMShape::offset (const IPosition& position,
                         const IPosition& origin) const
{
    if (size_p != position.nelements()  ||  size_p != origin.nelements()) {
	throw (ArrayConformanceError(
                               "TSMShape::offset - shapes do not conform"));
    }
    size_t off = 0;
    for (uInt i=0; i<size_p; i++) {
	off += (position(i) - origin(i)) * data_p(i);
    }
    return off;
}

// Calculate the position for a given offset.
IPosition TSMShape::position (size_t offset) const
{
    IPosition pos(size_p);
    if (size_p > 0) {
	for (uInt i=size_p-1; i>0; i--) {
	    pos(i) = offset / data_p(i);
	    offset -= pos(i) * data_p(i);
	}
	pos(0) = offset;
    }
    return pos;
}

IPosition TSMShape::position (size_t offset, const IPosition& origin) const
{
    if (size_p != origin.nelements()) {
	throw (ArrayConformanceError(
                               "TSMShape::position - shapes do not conform"));
    }
    IPosition pos(size_p);
    if (size_p > 0) {
	for (uInt i=size_p-1; i>0; i--) {
	    pos(i) = offset / data_p(i);
	    offset -= pos(i) * data_p(i);
	    pos(i) += origin(i);
	}
	pos(0) = offset + origin(0);
    }
    return pos;
}

IPosition TSMShape::offsetIncrement (const IPosition& subShape) const
{
    if (size_p != subShape.nelements()) {
	throw (ArrayConformanceError(
                        "TSMShape::offsetIncrement - shapes do not conform"));
    }
    IPosition incr(size_p,1);
    for (uInt i=1; i<size_p; i++) {
	incr(i) = data_p(i) - subShape(i-1) * data_p(i-1);
    }
    return incr;
}

IPosition TSMShape::offsetIncrement (const IPosition& subShape,
				     const IPosition& stride) const
{
    if (size_p != subShape.nelements()  ||  size_p != stride.nelements()) {
	throw (ArrayConformanceError(
                        "TSMShape::offsetIncrement - shapes do not conform"));
    }
    IPosition incr(size_p,1);
    for (uInt i=1; i<size_p; i++) {
	incr(i) = stride(i) * data_p(i) -
	          subShape(i-1) * stride(i-1) * data_p(i-1);
    }
    return incr;
}

} //# NAMESPACE CASACORE - END

