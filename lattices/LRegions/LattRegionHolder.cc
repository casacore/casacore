//# LattRegionHolder.cc: Class to hold a region of interest in a lattice
//# Copyright (C) 1999,2001
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

#include <casacore/lattices/LRegions/LattRegionHolder.h>
#include <casacore/lattices/LRegions/LCRegion.h>
#include <casacore/lattices/LRegions/LCSlicer.h>
#include <casacore/lattices/LRegions/LCExtension.h>
#include <casacore/lattices/LRegions/LCUnion.h>
#include <casacore/lattices/LRegions/LCIntersection.h>
#include <casacore/lattices/LRegions/LCDifference.h>
#include <casacore/lattices/LRegions/LCComplement.h>
#include <casacore/lattices/LRegions/RegionType.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

LattRegionHolder::LattRegionHolder (uInt ndim)
: itsLC     (0),
  itsSlicer (0),
  itsNdim   (ndim)
{}

LattRegionHolder::LattRegionHolder (const LCRegion& region)
: itsLC     (region.cloneRegion()),
  itsSlicer (0),
  itsNdim   (region.ndim())
{}

LattRegionHolder::LattRegionHolder (const LCSlicer& slicer)
: itsLC     (0),
  itsSlicer (new LCSlicer(slicer)),
  itsNdim   (slicer.ndim())
{}

LattRegionHolder::LattRegionHolder (LCRegion* region)
: itsLC     (region),
  itsSlicer (0),
  itsNdim   (region->ndim())
{}

LattRegionHolder::LattRegionHolder (LCSlicer* slicer)
: itsLC     (0),
  itsSlicer (slicer),
  itsNdim   (slicer->ndim())
{}

LattRegionHolder::LattRegionHolder (const LattRegionHolder& other)
: itsLC     (0),
  itsSlicer (0)
{
    operator= (other);
}

LattRegionHolder::~LattRegionHolder()
{
    delete itsLC;
    delete itsSlicer;
}

LattRegionHolder& LattRegionHolder::operator= (const LattRegionHolder& other)
{
    if (this != &other) {
	delete itsLC;
	delete itsSlicer;
	itsLC = other.itsLC;
	itsSlicer = other.itsSlicer;
	itsNdim = other.itsNdim;
	if (itsLC != 0) {
	    itsLC = itsLC->cloneRegion();
	}
	if (itsSlicer != 0) {
	    itsSlicer = new LCSlicer(*itsSlicer);
	}
    }
    return *this;
}

LattRegionHolder* LattRegionHolder::clone() const
{
    return new LattRegionHolder (*this);
}

Bool LattRegionHolder::operator== (const LattRegionHolder& other) const
{
    if (isWCRegion()   != other.isWCRegion()
    ||  isLCRegion()   != other.isLCRegion()
    ||  isLCSlicer()   != other.isLCSlicer()) {
	return False;
    }
    Bool match = True;
    if (isLCRegion()) {
	match = (*itsLC == *other.asLCRegionPtr());
    } else if (isLCSlicer()) {
	match = (*itsSlicer == *other.asLCSlicerPtr());
    }
    return match;
}

Bool LattRegionHolder::isWCRegion() const
{
    return False;
}

const LCRegion* LattRegionHolder::asLCRegionPtr() const
{
    AlwaysAssert (isLCRegion(), AipsError);
    return itsLC;
}

const LCSlicer* LattRegionHolder::asLCSlicerPtr() const
{
    AlwaysAssert (isLCSlicer(), AipsError);
    return itsSlicer;
}

const WCRegion* LattRegionHolder::asWCRegionPtr() const
{
    return 0;
}

LatticeRegion LattRegionHolder::toLatticeRegion (const CoordinateSystem&,
						 const IPosition& shape) const
{
    return toLatticeRegion (shape);
}

LatticeRegion LattRegionHolder::toLatticeRegion (const IPosition& shape) const
{
    if (isLCRegion()) {
	return LatticeRegion (*itsLC);
    }
    if (isWCRegion()) {
        throw (AipsError ("LattRegionHolder::toLatticeRegion - "
			  "using a region in world coordinates requires "
			  "image coordinates"));
    }
    if (! itsSlicer->isAbsolute()) {
	throw (AipsError ("LattRegionHolder::toLatticeRegion - "
			  "cannot convert a relative LCSlicer"));
    }
    Vector<Float> refpix(shape.nelements());
    refpix = 0;
    return LatticeRegion (itsSlicer->toSlicer (refpix, shape), shape);
}


LattRegionHolder* LattRegionHolder::makeUnion
                                (const LattRegionHolder& other) const
{
    return new LattRegionHolder
           (new LCUnion (*asLCRegionPtr(), *other.asLCRegionPtr()));
}
LattRegionHolder* LattRegionHolder::makeIntersection
                                (const LattRegionHolder& other) const
{
    return new LattRegionHolder
           (new LCIntersection (*asLCRegionPtr(), *other.asLCRegionPtr()));
}
LattRegionHolder* LattRegionHolder::makeDifference
                                (const LattRegionHolder& other) const
{
    return new LattRegionHolder
           (new LCDifference (*asLCRegionPtr(), *other.asLCRegionPtr()));
}
LattRegionHolder* LattRegionHolder::makeComplement() const
{
    return new LattRegionHolder
           (new LCComplement (*asLCRegionPtr()));
}

} //# NAMESPACE CASACORE - END

