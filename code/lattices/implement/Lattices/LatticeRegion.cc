//# LatticeRegion.cc: An optionally strided region in a lattice
//# Copyright (C) 1998
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
//#
//# $Id$


#include <trial/Lattices/LatticeRegion.h>
#include <trial/Lattices/LCBox.h>
#include <aips/Arrays/Array.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>


LatticeRegion::LatticeRegion()
: itsRegion (0),
  itsParent (0),
  itsHasRegionMask (False),
  itsHasParentMask (False)
{}

LatticeRegion::LatticeRegion (const LCRegion& region,
			      const LatticeRegion* parent)
: itsRegion (region.cloneRegion()),
  itsSlicer (region.boundingBox()),
  itsParent (parent),
  itsHasRegionMask (region.hasMask()),
  itsHasParentMask (False)
{
    if (itsParent != 0) {
        itsHasParentMask = parent->hasMask();
    }
}

LatticeRegion::LatticeRegion (LCRegion* region)
: itsRegion (region),
  itsSlicer (region->boundingBox()),
  itsParent (0),
  itsHasRegionMask (region->hasMask()),
  itsHasParentMask (False)
{}

LatticeRegion::LatticeRegion (const Slicer& slicer,
			      const IPosition& latticeShape)
: itsRegion (0),
  itsParent (0),
  itsHasRegionMask (False),
  itsHasParentMask (False)
{
  // Make sure that the slicer has blc,trc filled in.
  IPosition blc, trc, inc;
  slicer.inferShapeFromSource (latticeShape, blc, trc, inc);
  itsSlicer = Slicer (blc, trc, inc, Slicer::endIsLast);
  itsRegion = new LCBox (Slicer (slicer.start(), slicer.length()),
			 latticeShape);
}
  
LatticeRegion::LatticeRegion (const Slicer& slicer,
			      const LatticeRegion* parent)
: itsRegion (0),
  itsParent (parent),
  itsHasRegionMask (False),
  itsHasParentMask (parent->hasMask())
{
  // Make sure that the slicer has blc,trc filled in.
  IPosition blc, trc, inc;
  slicer.inferShapeFromSource (parent->shape(), blc, trc, inc);
  itsSlicer = Slicer (blc, trc, inc, Slicer::endIsLast);
  itsRegion = new LCBox (Slicer (slicer.start(), slicer.length()),
			 parent->shape());
}
  
LatticeRegion::LatticeRegion (const LatticeRegion& other)
: itsRegion (other.itsRegion->cloneRegion()),
  itsSlicer (other.itsSlicer),
  itsParent (other.itsParent),
  itsHasRegionMask (other.itsHasRegionMask),
  itsHasParentMask (other.itsHasParentMask)
{}
    
LatticeRegion::~LatticeRegion()
{
  delete itsRegion;
}

LatticeRegion& LatticeRegion::operator= (const LatticeRegion& other)
{
    if (this != &other) {
        delete itsRegion;
	itsRegion = other.itsRegion;
	if (itsRegion != 0) {
	    itsRegion = itsRegion->cloneRegion();
	}
	itsSlicer = other.itsSlicer;
	itsParent = other.itsParent;
	itsHasRegionMask = other.itsHasRegionMask;
	itsHasParentMask = other.itsHasParentMask;
    }
    return *this;
}

Lattice<Bool>* LatticeRegion::clone() const
{
    return new LatticeRegion (*this);
}


Bool LatticeRegion::isWritable() const
{
    return itsRegion->isWritable();
}

IPosition LatticeRegion::shape() const
{
    return itsSlicer.length();
}
  
uInt LatticeRegion::ndim() const
{
    return itsSlicer.ndim();
}
  
uInt LatticeRegion::nelements() const
{
    return itsRegion->nelements();
}
  
LatticeIterInterface<Bool>* LatticeRegion::makeIter
                        (const LatticeNavigator& navigator) const
{
    return itsRegion->makeIter (navigator);
}


Bool LatticeRegion::doGetSlice (Array<Bool>& buffer,
				const Slicer& section)
{
    // When no mask at all, simply return all true.
    if (! hasMask()) {
        buffer.resize (section.length());
        buffer = True;
	return False;
    }
    // If parent has no mask, return the required section.
    if (!itsHasParentMask) {
        LCRegion* reg = (LCRegion*)itsRegion;
        return reg->doGetSlice (buffer, section);
    }
    // If we have no mask, return the parent mask.
    LCRegion* par = (LCRegion*)itsParent;
    if (!itsHasRegionMask) {
        return par->doGetSlice (buffer, convert(section));
    }
    // We have to combine this mask and the parent mask.
    Array<Bool> tmpbuf = itsRegion->getSlice (section);
    par->doGetSlice (buffer, convert (section));
    Bool deleteBuf, deleteTmp;
    Bool* tmp = tmpbuf.getStorage (deleteTmp);
    Bool* tmpptr = tmp;
    const Bool* buf = buffer.getStorage (deleteBuf);
    const Bool* bufptr = buf;
    const Bool* bufend = buf + buffer.nelements();
    while (bufptr < bufend) {
        if (*tmpptr) {
	    *tmpptr = *bufptr;
	}
	bufptr++;
	tmpptr++;
    }
    buffer.freeStorage (buf, deleteBuf);
    tmpbuf.putStorage (tmp, deleteTmp);
    buffer.reference (tmpbuf);
    return False;
}

void LatticeRegion::doPutSlice (const Array<Bool>& sourceBuffer,
				const IPosition& where,
				const IPosition& stride)
{
    AlwaysAssert (hasMask() && isWritable(), AipsError);
    itsRegion->putSlice (sourceBuffer, where, stride);
}

IPosition LatticeRegion::doNiceCursorShape (uInt maxPixels) const
{
    if (itsHasRegionMask) {
        return itsRegion->niceCursorShape (maxPixels);
    }
    return Lattice<Bool>::doNiceCursorShape (maxPixels);
}

void LatticeRegion::set (const Bool& value)
{
    AlwaysAssert (hasMask() && isWritable(), AipsError);
    itsRegion->set (value);
}
void LatticeRegion::apply (Bool (*function)(Bool))
{
    AlwaysAssert (hasMask() && isWritable(), AipsError);
    itsRegion->apply (function);
}
void LatticeRegion::apply (Bool (*function)(const Bool&))
{
    AlwaysAssert (hasMask() && isWritable(), AipsError);
    itsRegion->apply (function);
}
void LatticeRegion::apply (const Functional<Bool,Bool>& function)
{
    AlwaysAssert (hasMask() && isWritable(), AipsError);
    itsRegion->apply (function);
}
void LatticeRegion::putAt (const Bool& value, const IPosition& where)
{
    AlwaysAssert (hasMask() && isWritable(), AipsError);
    itsRegion->putAt (value, where);
}
void LatticeRegion::copyData (const Lattice<Bool>& from)
{
    AlwaysAssert (hasMask() && isWritable(), AipsError);
    itsRegion->copyData (from);
}

Bool LatticeRegion::ok() const
{
    return itsRegion->ok();
}


Slicer LatticeRegion::convert (const Slicer& slicer) const
{
    IPosition blc, trc, inc;
    IPosition shape = slicer.inferShapeFromSource (itsSlicer.length(),
						   blc, trc, inc);
    const IPosition& start = itsSlicer.start();
    const IPosition& incr  = itsSlicer.stride();
    uInt ndim = shape.nelements();
    for (uInt i=0; i<ndim; i++) {
	blc(i) = start(i) + blc(i) * incr(i);
	inc(i) *= incr(i);
    }
    return Slicer(blc, shape, inc);
}
IPosition LatticeRegion::convert (const IPosition& position) const
{
    uInt ndim = itsSlicer.ndim();
    DebugAssert (position.nelements() == ndim, AipsError);
    IPosition result (ndim);
    const IPosition& start = itsSlicer.start();
    const IPosition& incr  = itsSlicer.stride();
    for (uInt i=0; i<ndim; i++) {
	DebugAssert (position(i) < itsSlicer.length()(i), AipsError);
	result(i) = start(i) + position(i) * incr(i);
    }
    return result;
}
