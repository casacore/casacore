//# WCCompound.cc: Base class for compound WCRegion objects
//# Copyright (C) 1998,2000
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


#include <casacore/images/Regions/WCCompound.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/lattices/LRegions/RegionType.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

WCCompound::WCCompound (const ImageRegion& region1,
			const ImageRegion& region2)
{
    PtrBlock<const ImageRegion*> regions(2);
    regions[0] = &region1;
    regions[1] = &region2;
    makeWCRegion (regions);
    init (false);
}

WCCompound::WCCompound (const ImageRegion* region1,
			const ImageRegion* region2,
			const ImageRegion* region3,
			const ImageRegion* region4,
			const ImageRegion* region5,
			const ImageRegion* region6,
			const ImageRegion* region7,
			const ImageRegion* region8,
			const ImageRegion* region9,
			const ImageRegion* region10)
{
    PtrBlock<const ImageRegion*> regions(10);
    uint32_t n=0;
    regions[n++] = region1;
    if (region2 != 0) regions[n++] = region2;
    if (region3 != 0) regions[n++] = region3;
    if (region4 != 0) regions[n++] = region4;
    if (region5 != 0) regions[n++] = region5;
    if (region6 != 0) regions[n++] = region6;
    if (region7 != 0) regions[n++] = region7;
    if (region8 != 0) regions[n++] = region8;
    if (region9 != 0) regions[n++] = region9;
    if (region10 != 0) regions[n++] = region10;
    regions.resize (n, true, true);
    makeWCRegion (regions);
    init (false);
}

WCCompound::WCCompound (const PtrBlock<const ImageRegion*>& regions)
{
    makeWCRegion (regions);
    init (false);
}

WCCompound::WCCompound (bool takeOver,
			const PtrBlock<const WCRegion*>& regions)
: itsRegions (regions)
{
    init (takeOver);
}

WCCompound::WCCompound (const WCCompound& other)
: WCRegion    (other),
  itsRegions  (other.itsRegions.nelements()),
  itsAxesUsed (other.itsAxesUsed)
{
    uint32_t nr = itsRegions.nelements();
    for (uint32_t i=0; i<nr; i++) {
	itsRegions[i] = other.itsRegions[i]->cloneRegion();
    }
}

WCCompound::~WCCompound()
{
    uint32_t nr = itsRegions.nelements();
    for (uint32_t i=0; i<nr; i++) {
	delete itsRegions[i];
    }
}

WCCompound& WCCompound::operator= (const WCCompound& other)
{
    uint32_t i;
    if (this != &other) {
	uint32_t nr = itsRegions.nelements();
	for (i=0; i<nr; i++) {
	    delete itsRegions[i];
	}
	WCRegion::operator= (other);
	itsRegions.resize (other.itsRegions.nelements(), true);
	nr = itsRegions.nelements();
	for (i=0; i<nr; i++) {
	    itsRegions[i] = other.itsRegions[i]->cloneRegion();
	}
	itsAxesUsed = other.itsAxesUsed;
    }
    return *this;
}

void WCCompound::multiToLCRegion (PtrBlock<const LCRegion*>& regions,
				  const CoordinateSystem& cSys,
				  const IPosition& shape,
				  const IPosition& pixelAxesMap,
				  const IPosition& outOrder) const
{
    uint32_t nr = itsRegions.nelements();
    regions.resize (nr, true);
    uint32_t nd = pixelAxesMap.nelements();
    IPosition pixAxesMap(pixelAxesMap);
    IPosition outOrd(outOrder);
    IPosition axisUsed(nd);
    for (uint32_t i=0; i<nr; i++) {
        const IPosition& axes = itsAxesUsed[i];
	axisUsed = 0;
        // The used axes of the region are the first axes.
        // The latter axes are the auto-extension axes.
	uint32_t na = axes.nelements();
	uint32_t j;
        for (j=0; j<na; j++) {
	    pixAxesMap(j) = pixelAxesMap(axes(j));
	    outOrd(j) = outOrder(axes(j));
	    axisUsed(axes(j)) = 1;
	}
        for (uint32_t k=0; k<nd; k++) {
	    if (axisUsed(k) == 0) {
	        pixAxesMap(j) = pixelAxesMap(k);
		outOrd(j++) = outOrder(k);
	    }
	}
        regions[i] = itsRegions[i]->toLCRegionAxes (cSys, shape, pixAxesMap,
						    outOrd);
    }
}

bool WCCompound::operator== (const WCRegion& other) const
{
    // Type check.
    if (! WCRegion::operator== (other)) {
        return false;
    }
    // Cast is safe since types match.
    const WCCompound& that = (const WCCompound&)other;
    // Check the regions.
    if (itsRegions.nelements() != that.itsRegions.nelements()) {
	return false;
    }
    // The regions do not have to be in the same order.
    // It makes it a bit slower.
    uint32_t nr = itsRegions.nelements();
    Vector<bool> used(nr, false);
    for (uint32_t i=0; i<nr; i++) {
	bool found = false;
	for (uint32_t j=0; j<nr; j++) {
	    if (!used(j)) {      
		if (*itsRegions[i] == *(that.itsRegions[j])) {
		    used(j) = true;
		    found = true;
		    break;
		}
	    }
	}
	if (!found) {
	    return false;          // no matching region
	}
    }
    return true;
}

void WCCompound::makeWCRegion (const PtrBlock<const ImageRegion*>& regions)
{
    uint32_t nr = regions.nelements();
    itsRegions.resize (nr);
    for (uint32_t i=0; i<nr; i++) {
        if (regions[i]->isLCSlicer()) {
	    throw (AipsError ("WCCompound::WCCompound - "
			      "an LCSlicer object cannot be part of "
			      "an WCCompound"));
	}
	itsRegions[i] = &(regions[i]->asWCRegion());
    }
}

void WCCompound::init (bool takeOver)
{
    // Copy the region object if takeOver=false.
    // Compose the axes description of the entire compound.
    // Find out which compound axes are used in each region.
    uint32_t nr = itsRegions.nelements();
    itsAxesUsed.resize (nr);
    for (uint32_t i=0; i<nr; i++) {
        AlwaysAssert (itsRegions[i] != 0, AipsError);
	// Clone the object if needed.
        if (!takeOver) {
	    itsRegions[i] = itsRegions[i]->cloneRegion();
	}
	// Add axes to description if not already defined.
	// Fill in the axes used.
	uint32_t nd = itsRegions[i]->ndim();
	IPosition& axesUsed = itsAxesUsed[i];
	axesUsed.resize (nd);
	for (uint32_t j=0; j<nd; j++) {
	    const Record& desc = itsRegions[i]->getAxisDesc(j);
	    // If the axis is already defined, it has that axis number.
	    // Otherwise add its description and use that as axis number.
	    axesUsed(j) = axisNr (desc, getAxesDesc());
	    if (axesUsed(j) < 0) {
	        axesUsed(j) = getAxesDesc().nfields();
	        addAxisDesc (desc);
	    }
	}
    }
}

TableRecord WCCompound::makeRecord (const String& tableName) const
{
    TableRecord rec;
    int32_t nr = itsRegions.nelements();
    for (int32_t i=0; i<nr; i++) {
	rec.defineRecord (i, itsRegions[i]->toRecord (tableName));
    }
    rec.define ("nr", nr);
    return rec;
}

void WCCompound::unmakeRecord (PtrBlock<const WCRegion*>& regions,
			       const TableRecord& rec,
			       const String& tableName)
{
    int32_t nr = rec.asInt ("nr");
    regions.resize (nr, true);
    for (int32_t i=0; i<nr; i++) {
	regions[i] = WCRegion::fromRecord (rec.asRecord (i), tableName);
    }
}

} //# NAMESPACE CASACORE - END

