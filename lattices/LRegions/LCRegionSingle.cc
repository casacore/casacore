//# LCRegionSingle.cc: Abstract base class to define a single region
//# Copyright (C) 1998,2000,2003
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

#include <casacore/lattices/LRegions/LCRegionSingle.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/COWPtr.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

LCRegionSingle::LCRegionSingle()
{}

LCRegionSingle::LCRegionSingle (const IPosition& latticeShape)
: LCRegion   (latticeShape),
  itsHasMask (False),
  itsMaskPtr (0)
{}

LCRegionSingle::LCRegionSingle (const LCRegionSingle& other)
: LCRegion   (other),
  itsHasMask (False),
  itsMaskPtr (0)
{}

LCRegionSingle::~LCRegionSingle()
{}

LCRegionSingle& LCRegionSingle::operator= (const LCRegionSingle& other)
{
    LCRegion::operator= (other);
    return *this;
}

void LCRegionSingle::setMaskPtr (Lattice<Bool>& mask)
{
    itsMaskPtr = &mask;
    if (mask.nelements() != 0) {
        itsHasMask = True;
    }
}


Bool LCRegionSingle::masksEqual (const LCRegion& other) const
{
    // Object type check.
    if (type() != other.type()) {
	return False;
    }
    // Not equal if one has a mask and the other has not.
    if (hasMask() != other.hasMask()) {
	return False;
    }
    // True if both do not have a mask.
    if (!hasMask() && !other.hasMask()) {
	return True;
    }
    // Cast (is safe because object types are equal).
    const LCRegionSingle& that = (const LCRegionSingle&)other;
    // See if masks are the same shape.
    if (itsMaskPtr->shape() != that.itsMaskPtr->shape()) {
	return False;
    }
    // Now we must check the values.  
    RO_LatticeIterator<Bool> iter1(*itsMaskPtr, 
				   itsMaskPtr->niceCursorShape());
    RO_LatticeIterator<Bool> iter2(*(that.itsMaskPtr), 
				   itsMaskPtr->niceCursorShape());
    while (!iter1.atEnd()) {   
	if (anyNE (iter1.cursor(), iter2.cursor())) {
	    return False;
	}
	iter1++; 
	iter2++;
    }
    return True;
}

const Array<Bool> LCRegionSingle::maskArray() const
{
    // Return a [] shaped array if there is no mask
    IPosition shape;
    if (hasMask()) {
	shape = itsMaskPtr->shape();
    }
    COWPtr<Array<Bool> > pMask(new Array<Bool>(shape));
    if (hasMask()) {
	itsMaskPtr->get (pMask);
    }
    return *pMask;
}

Bool LCRegionSingle::hasMask() const
{
    return itsHasMask;
}

Bool LCRegionSingle::doGetSlice (Array<Bool>& buffer,
				 const Slicer& section)
{
    if (itsHasMask != 0) {
        return itsMaskPtr->getSlice (buffer, section);
    }
    buffer.resize (section.length());
    buffer = True;
    return False;
}

IPosition LCRegionSingle::doNiceCursorShape (uInt maxPixels) const
{
    if (itsHasMask != 0) {
        return itsMaskPtr->niceCursorShape (maxPixels);
    }
    return Lattice<Bool>::doNiceCursorShape (maxPixels);
}

LatticeIterInterface<Bool>* LCRegionSingle::makeIter
                                (const LatticeNavigator& navigator,
				 Bool useRef) const
{
    if (itsHasMask != 0) {
        return itsMaskPtr->makeIter (navigator, useRef);
    }
    return Lattice<Bool>::makeIter (navigator, useRef);
}


void LCRegionSingle::doPutSlice (const Array<Bool>& sourceBuffer,
				 const IPosition& where,
				 const IPosition& stride)
{
    AlwaysAssert (hasMask() && isWritable(), AipsError);
    itsMaskPtr->putSlice (sourceBuffer, where, stride);
}
void LCRegionSingle::set (const Bool& value)
{
    AlwaysAssert (hasMask() && isWritable(), AipsError);
    itsMaskPtr->set (value);
}
void LCRegionSingle::apply (Bool (*function)(Bool))
{
    AlwaysAssert (hasMask() && isWritable(), AipsError);
    itsMaskPtr->apply (function);
}
void LCRegionSingle::apply (Bool (*function)(const Bool&))
{
    AlwaysAssert (hasMask() && isWritable(), AipsError);
    itsMaskPtr->apply (function);
}
void LCRegionSingle::apply (const Functional<Bool,Bool>& function)
{
    AlwaysAssert (hasMask() && isWritable(), AipsError);
    itsMaskPtr->apply (function);
}
void LCRegionSingle::putAt (const Bool& value, const IPosition& where)
{
    AlwaysAssert (hasMask() && isWritable(), AipsError);
    itsMaskPtr->putAt (value, where);
}
void LCRegionSingle::copyData (const Lattice<Bool>& from)
{
    AlwaysAssert (hasMask() && isWritable(), AipsError);
    itsMaskPtr->copyData (from);
}

} //# NAMESPACE CASACORE - END

