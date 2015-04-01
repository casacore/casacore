//# LatticeRegion.cc: An optionally strided region in a lattice
//# Copyright (C) 1998,1999,2000,2003
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


#include <casacore/lattices/LRegions/LatticeRegion.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LatticeRegion::LatticeRegion()
: itsRegion (0),
  itsHasRegionMask (False)
{}

LatticeRegion::LatticeRegion (const LCRegion& region)
: itsRegion (region.cloneRegion()),
  itsSlicer (region.boundingBox()),
  itsHasRegionMask (region.hasMask())
{}

LatticeRegion::LatticeRegion (LCRegion* region)
: itsRegion (region),
  itsSlicer (region->boundingBox()),
  itsHasRegionMask (region->hasMask())
{}

LatticeRegion::LatticeRegion (const Slicer& slicer,
			      const IPosition& latticeShape)
: itsRegion (0),
  itsHasRegionMask (False)
{
  // Make sure that the slicer has blc,trc filled in.
  IPosition blc, trc, inc;
  slicer.inferShapeFromSource (latticeShape, blc, trc, inc);
  itsSlicer = Slicer (blc, trc, inc, Slicer::endIsLast);
  itsRegion = new LCBox (Slicer (slicer.start(), slicer.length()),
			 latticeShape);
}
  
LatticeRegion::LatticeRegion (const LatticeRegion& other)
: Lattice<Bool>(),
  itsRegion (other.itsRegion->cloneRegion()),
  itsSlicer (other.itsSlicer),
  itsHasRegionMask (other.itsHasRegionMask)
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
	itsHasRegionMask = other.itsHasRegionMask;
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


uInt LatticeRegion::advisedMaxPixels() const
{
  return itsRegion->advisedMaxPixels();
}

IPosition LatticeRegion::doNiceCursorShape (uInt maxPixels) const
{
    if (itsHasRegionMask) {
        return itsRegion->niceCursorShape (maxPixels);
    }
    return LatticeBase::doNiceCursorShape (maxPixels);
}

uInt LatticeRegion::maximumCacheSize() const
{
  return itsRegion->maximumCacheSize();
}

void LatticeRegion::setMaximumCacheSize (uInt howManyPixels)
{
  itsRegion->setMaximumCacheSize (howManyPixels);
}

void LatticeRegion::setCacheSizeFromPath (const IPosition& sliceShape,
					  const IPosition& windowStart,
					  const IPosition& windowLength,
					  const IPosition& axisPath)
{
  itsRegion->setCacheSizeFromPath (sliceShape, windowStart, windowLength,
				   axisPath);
}

void LatticeRegion::setCacheSizeInTiles (uInt howManyTiles)
{
  itsRegion->setCacheSizeInTiles (howManyTiles);
}

void LatticeRegion::clearCache()
{
  itsRegion->clearCache();
}

void LatticeRegion::showCacheStatistics (ostream& os) const
{
  itsRegion->showCacheStatistics (os);
}


Bool LatticeRegion::lock (FileLocker::LockType type, uInt nattempts)
{
    // Llock the PagedArray containing the mask.
    return itsRegion->lock (type, nattempts);
}
void LatticeRegion::unlock()
{
    // Unlock the PagedArray containing the mask.
    itsRegion->unlock();
}
Bool LatticeRegion::hasLock (FileLocker::LockType type) const
{
    return itsRegion->hasLock (type);
}
void LatticeRegion::resync()
{
    itsRegion->resync();
}

void LatticeRegion::flush()
{
    itsRegion->flush();
}

void LatticeRegion::tempClose()
{
    itsRegion->tempClose();
}

void LatticeRegion::reopen()
{
    itsRegion->reopen();
}


IPosition LatticeRegion::shape() const
{
    return itsSlicer.length();
}
  
uInt LatticeRegion::ndim() const
{
    return itsSlicer.ndim();
}
  
size_t LatticeRegion::nelements() const
{
    return itsRegion->nelements();
}
  
LatticeIterInterface<Bool>* LatticeRegion::makeIter
                        (const LatticeNavigator& navigator,
			 Bool useRef) const
{
    return itsRegion->makeIter (navigator, useRef);
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
    // Return the required section.
    ///    LCRegion* reg = (LCRegion*)itsRegion;
    return itsRegion->doGetSlice (buffer, section);
}

void LatticeRegion::doPutSlice (const Array<Bool>& sourceBuffer,
				const IPosition& where,
				const IPosition& stride)
{
    AlwaysAssert (hasMask() && isWritable(), AipsError);
    itsRegion->putSlice (sourceBuffer, where, stride);
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

} //# NAMESPACE CASACORE - END

