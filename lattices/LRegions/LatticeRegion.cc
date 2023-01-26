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


#include <casacore/lattices/LRegions/LatticeRegion.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LatticeRegion::LatticeRegion()
: itsRegion (0),
  itsHasRegionMask (false)
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
  itsHasRegionMask (false)
{
  // Make sure that the slicer has blc,trc filled in.
  IPosition blc, trc, inc;
  slicer.inferShapeFromSource (latticeShape, blc, trc, inc);
  itsSlicer = Slicer (blc, trc, inc, Slicer::endIsLast);
  itsRegion = new LCBox (Slicer (slicer.start(), slicer.length()),
			 latticeShape);
}
  
LatticeRegion::LatticeRegion (const LatticeRegion& other)
: Lattice<bool>(),
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

Lattice<bool>* LatticeRegion::clone() const
{
    return new LatticeRegion (*this);
}


bool LatticeRegion::isWritable() const
{
    return itsRegion->isWritable();
}


uint32_t LatticeRegion::advisedMaxPixels() const
{
  return itsRegion->advisedMaxPixels();
}

IPosition LatticeRegion::doNiceCursorShape (uint32_t maxPixels) const
{
    if (itsHasRegionMask) {
        return itsRegion->niceCursorShape (maxPixels);
    }
    return LatticeBase::doNiceCursorShape (maxPixels);
}

uint32_t LatticeRegion::maximumCacheSize() const
{
  return itsRegion->maximumCacheSize();
}

void LatticeRegion::setMaximumCacheSize (uint32_t howManyPixels)
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

void LatticeRegion::setCacheSizeInTiles (uint32_t howManyTiles)
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


bool LatticeRegion::lock (FileLocker::LockType type, uint32_t nattempts)
{
    // Llock the PagedArray containing the mask.
    return itsRegion->lock (type, nattempts);
}
void LatticeRegion::unlock()
{
    // Unlock the PagedArray containing the mask.
    itsRegion->unlock();
}
bool LatticeRegion::hasLock (FileLocker::LockType type) const
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
  
uint32_t LatticeRegion::ndim() const
{
    return itsSlicer.ndim();
}
  
size_t LatticeRegion::nelements() const
{
    return itsRegion->nelements();
}
  
LatticeIterInterface<bool>* LatticeRegion::makeIter
                        (const LatticeNavigator& navigator,
			 bool useRef) const
{
    return itsRegion->makeIter (navigator, useRef);
}


bool LatticeRegion::doGetSlice (Array<bool>& buffer,
				const Slicer& section)
{
    // When no mask at all, simply return all true.
    if (! hasMask()) {
        buffer.resize (section.length());
        buffer = true;
	return false;
    }
    // Return the required section.
    ///    LCRegion* reg = (LCRegion*)itsRegion;
    return itsRegion->doGetSlice (buffer, section);
}

void LatticeRegion::doPutSlice (const Array<bool>& sourceBuffer,
				const IPosition& where,
				const IPosition& stride)
{
    AlwaysAssert (hasMask() && isWritable(), AipsError);
    itsRegion->putSlice (sourceBuffer, where, stride);
}

void LatticeRegion::set (const bool& value)
{
    AlwaysAssert (hasMask() && isWritable(), AipsError);
    itsRegion->set (value);
}
void LatticeRegion::apply (bool (*function)(bool))
{
    AlwaysAssert (hasMask() && isWritable(), AipsError);
    itsRegion->apply (function);
}
void LatticeRegion::apply (bool (*function)(const bool&))
{
    AlwaysAssert (hasMask() && isWritable(), AipsError);
    itsRegion->apply (function);
}
void LatticeRegion::apply (const Functional<bool,bool>& function)
{
    AlwaysAssert (hasMask() && isWritable(), AipsError);
    itsRegion->apply (function);
}
void LatticeRegion::putAt (const bool& value, const IPosition& where)
{
    AlwaysAssert (hasMask() && isWritable(), AipsError);
    itsRegion->putAt (value, where);
}
void LatticeRegion::copyData (const Lattice<bool>& from)
{
    AlwaysAssert (hasMask() && isWritable(), AipsError);
    itsRegion->copyData (from);
}

bool LatticeRegion::ok() const
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
    uint32_t ndim = shape.nelements();
    for (uint32_t i=0; i<ndim; i++) {
	blc(i) = start(i) + blc(i) * incr(i);
	inc(i) *= incr(i);
    }
    return Slicer(blc, shape, inc);
}
IPosition LatticeRegion::convert (const IPosition& position) const
{
    uint32_t ndim = itsSlicer.ndim();
    DebugAssert (position.nelements() == ndim, AipsError);
    IPosition result (ndim);
    const IPosition& start = itsSlicer.start();
    const IPosition& incr  = itsSlicer.stride();
    for (uint32_t i=0; i<ndim; i++) {
	DebugAssert (position(i) < itsSlicer.length()(i), AipsError);
	result(i) = start(i) + position(i) * incr(i);
    }
    return result;
}

} //# NAMESPACE CASACORE - END

