//# LCMask.cc: Class to define a rectangular mask of interest
//# Copyright (C) 2000,2001,2003
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

#include <casacore/lattices/LRegions/LCMask.h>
#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LCMask::LCMask()
{}

LCMask::LCMask (const IPosition& lattShape)
: LCRegionSingle (lattShape),
  itsBox  (IPosition(lattShape.nelements(), 0),
	   lattShape-1, lattShape),
  itsMask (0)
{
  setBoundingBox (itsBox.boundingBox());
  itsMask = new TempLattice<Bool> (lattShape);
  setMaskPtr (*itsMask);
}

LCMask::LCMask (const IPosition& maskShape, const LCBox& box)
: LCRegionSingle (box.latticeShape()),
  itsBox  (box),
  itsMask (0)
{
  // Check if box shape and mask shape are equal.
  if (itsBox.shape() != maskShape) {
    throw (AipsError ("LCMask::LCMask- "
		      "shape of mask and box differ"));
  }
  setBoundingBox (itsBox.boundingBox());
  itsMask = new TempLattice<Bool> (maskShape);
  setMaskPtr (*itsMask);
}

LCMask::LCMask (Lattice<Bool>& mask)
: LCRegionSingle (mask.shape()),
  itsBox  (IPosition(mask.shape().nelements(), 0),
	   mask.shape()-1, mask.shape()),
  itsMask (0)
{
  setBoundingBox (itsBox.boundingBox());
  itsMask = mask.clone();
  setMaskPtr (*itsMask);
}

LCMask::LCMask (Lattice<Bool>& mask, const LCBox& box)
: LCRegionSingle (box.latticeShape()),
  itsBox (box)
{
  // Check if box shape and mask shape are equal.
  if (itsBox.shape() != mask.shape()) {
    throw (AipsError ("LCMask::LCMask- "
		      "shape of mask and box differ"));
  }
  setBoundingBox (itsBox.boundingBox());
  itsMask = mask.clone();
  setMaskPtr (*itsMask);
}

LCMask::LCMask (const LCMask& other)
: LCRegionSingle (other),
  itsBox  (other.itsBox),
  itsMask (0)
{
  itsMask = other.itsMask->clone();
  setMaskPtr (*itsMask);
}

LCMask::~LCMask()
{
  delete itsMask;
}

LCMask& LCMask::operator= (const LCMask& that)
{
  if (this != &that) {
    LCRegionSingle::operator= (that);
    itsBox  = that.itsBox;
    delete itsMask;
    itsMask = 0;
    itsMask = that.itsMask->clone();
    setMaskPtr (*itsMask);
  }
  return *this;
}

Bool LCMask::operator== (const LCRegion& other) const
{
  // Check if parent class matches.
  // If so, we can safely cast.
  if (! LCRegionSingle::operator== (other)) {
    return False;
  }
  const LCMask& that = (const LCMask&)other;
  // Check the box and mask.
  return  (itsBox == that.itsBox  &&  masksEqual (that));
}


LCRegion* LCMask::cloneRegion() const
{
  return new LCMask(*this);
}


uInt LCMask::advisedMaxPixels() const
{
  return itsMask->advisedMaxPixels();
}

IPosition LCMask::doNiceCursorShape (uInt maxPixels) const
{
  return itsMask->niceCursorShape (maxPixels);
}

uInt LCMask::maximumCacheSize() const
{
  return itsMask->maximumCacheSize();
}

void LCMask::setMaximumCacheSize (uInt howManyPixels)
{
  itsMask->setMaximumCacheSize (howManyPixels);
}

void LCMask::setCacheSizeFromPath (const IPosition& sliceShape,
				   const IPosition& windowStart,
				   const IPosition& windowLength,
				   const IPosition& axisPath)
{
  itsMask->setCacheSizeFromPath (sliceShape, windowStart, windowLength,
				 axisPath);
}

void LCMask::setCacheSizeInTiles (uInt howManyTiles)
{
  itsMask->setCacheSizeInTiles (howManyTiles);
}

void LCMask::clearCache()
{
  itsMask->clearCache();
}

void LCMask::showCacheStatistics (ostream& os) const
{
  itsMask->showCacheStatistics (os);
}

LatticeIterInterface<Bool>* LCMask::makeIter
                                   (const LatticeNavigator& navigator,
				    Bool useRef) const
{
  return itsMask->makeIter (navigator, useRef);
}


Bool LCMask::lock (FileLocker::LockType type, uInt nattempts)
{
  // Lock the PagedArray containing the mask.
  return itsMask->lock (type, nattempts);
}
void LCMask::unlock()
{
  // Unlock the PagedArray containing the mask.
  itsMask->unlock();
}
Bool LCMask::hasLock (FileLocker::LockType type) const
{
  return itsMask->hasLock (type);
}
void LCMask::resync()
{
  itsMask->resync();
}

void LCMask::flush()
{
  itsMask->flush();
}

void LCMask::tempClose()
{
  itsMask->tempClose();
}

void LCMask::reopen()
{
  itsMask->reopen();
}


LCRegion* LCMask::doTranslate (const Vector<Float>&,
			       const IPosition&) const
{
  // An LCMask cannot be translated.
  throw (AipsError ("LCMask::translate is not supported"));
  return 0;
}

String LCMask::className()
{
  return "LCMask";
}

String LCMask::type() const
{
  return className();
}

TableRecord LCMask::toRecord (const String&) const
{
  throw AipsError ("LCMask::toRecord is not supported");
  return TableRecord();;
}

Bool LCMask::isWritable() const
{
  return itsMask->isWritable();
}

} //# NAMESPACE CASACORE - END

