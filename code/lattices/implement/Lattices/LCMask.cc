//# LCMask.cc: Class to define a rectangular mask of interest
//# Copyright (C) 2000
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

#include <trial/Lattices/LCMask.h>
#include <aips/Lattices/TempLattice.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>


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
  return ToBool (itsBox == that.itsBox  &&  masksEqual (that));
}


LCRegion* LCMask::cloneRegion() const
{
  return new LCMask(*this);
}


Bool LCMask::lock (FileLocker::LockType type, uInt nattempts)
{
  // Llock the PagedArray containing the mask.
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

TableRecord LCMask::toRecord (const String& tableName) const
{
  throw AipsError ("LCMask::toRecord is not supported");
  return TableRecord();;
}

Bool LCMask::isWritable() const
{
  return itsMask->isWritable();
}
