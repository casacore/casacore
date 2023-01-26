//# LCRegionMulti.cc: Abstract base class for regions composed of other regions
//# Copyright (C) 1998,2001
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


#include <casacore/lattices/LRegions/LCRegionMulti.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LCRegionMulti::LCRegionMulti()
{}

LCRegionMulti::LCRegionMulti (const LCRegion& region1,
			      const LCRegion& region2)
: LCRegion   (region1.latticeShape()),
  itsRegions (2)
{
    itsRegions[0] = &region1;
    itsRegions[1] = &region2;
    init (false);
}

LCRegionMulti::LCRegionMulti (bool takeOver, const LCRegion* region1,
			      const LCRegion* region2,
			      const LCRegion* region3,
			      const LCRegion* region4,
			      const LCRegion* region5,
			      const LCRegion* region6,
			      const LCRegion* region7,
			      const LCRegion* region8,
			      const LCRegion* region9,
			      const LCRegion* region10)
: LCRegion   (region1->latticeShape()),
  itsRegions (10)
{
    uint32_t n=0;
    itsRegions[n++] = region1;
    if (region2 != 0) itsRegions[n++] = region2;
    if (region3 != 0) itsRegions[n++] = region3;
    if (region4 != 0) itsRegions[n++] = region4;
    if (region5 != 0) itsRegions[n++] = region5;
    if (region6 != 0) itsRegions[n++] = region6;
    if (region7 != 0) itsRegions[n++] = region7;
    if (region8 != 0) itsRegions[n++] = region8;
    if (region9 != 0) itsRegions[n++] = region9;
    if (region10 != 0) itsRegions[n++] = region10;
    itsRegions.resize (n, true, true);
    init (takeOver);
}

LCRegionMulti::LCRegionMulti (bool takeOver,
			      const PtrBlock<const LCRegion*>& regions)
: LCRegion   (regions[0]->latticeShape()),
  itsRegions (regions)
{
    init (takeOver);
}

LCRegionMulti::LCRegionMulti (const LCRegion* regionPtr,
			      const IPosition& latticeShape)
: LCRegion   (latticeShape),
  itsRegions (1)
{
    itsRegions[0] = regionPtr;
    itsHasMask = (regionPtr->hasMask()  ?  0 : -1);
}

LCRegionMulti::LCRegionMulti (const LCRegionMulti& other)
: LCRegion   (other),
  itsHasMask (other.itsHasMask),
  itsRegions (other.itsRegions.nelements())
{
    uint32_t nr = itsRegions.nelements();
    for (uint32_t i=0; i<nr; i++) {
	itsRegions[i] = other.itsRegions[i]->cloneRegion();
    }
}

LCRegionMulti::~LCRegionMulti()
{
    uint32_t nr = itsRegions.nelements();
    for (uint32_t i=0; i<nr; i++) {
	delete itsRegions[i];
    }
}

LCRegionMulti& LCRegionMulti::operator= (const LCRegionMulti& other)
{
    if (this != &other) {
	LCRegion::operator= (other);
	itsHasMask = other.itsHasMask;
	uint32_t nr = itsRegions.nelements();
	for (uint32_t j=0; j<nr; j++) {
	    delete itsRegions[j];
	    itsRegions[j] = 0;
	}
	itsRegions.resize (other.itsRegions.nelements(), true);
	nr = itsRegions.nelements();
	for (uint32_t i=0; i<nr; i++) {
	    itsRegions[i] = other.itsRegions[i]->cloneRegion();
	}
    }
    return *this;
}

bool LCRegionMulti::hasMask() const
{
    return (itsHasMask >= 0);
}

void LCRegionMulti::multiTranslate (PtrBlock<const LCRegion*>& regions,
				    const Vector<float>& translateVector,
				    const IPosition& newLatticeShape) const
{
    regions.resize (itsRegions.nelements(), true);
    for (uint32_t i=0; i<itsRegions.nelements(); i++) {
        regions[i] = itsRegions[i]->translate (translateVector,
					       newLatticeShape);
    }
}


bool LCRegionMulti::operator== (const LCRegion& other) const
{
    // Check if parent class matches.
    // If so, we can safely cast.
    if (! LCRegion::operator== (other)) {
	return false;
    }
    const LCRegionMulti& that = (const LCRegionMulti&)other;
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


void LCRegionMulti::init (bool takeOver)
{
    itsHasMask = 0;
    for (uint32_t i=0; i<itsRegions.nelements(); i++) {
        AlwaysAssert (itsRegions[i] != 0, AipsError);
	if (itsRegions[i]->latticeShape() != latticeShape()) {
	    throw (AipsError ("LCRegionMulti::init - "
			      "all regions must have same lattice shape"));
	}
        if (!takeOver) {
	    itsRegions[i] = itsRegions[i]->cloneRegion();
	}
    }
}

void LCRegionMulti::fillHasMask()
{
    itsHasMask = -1;
    uint32_t maxNelem = 0;
    for (uint32_t i=0; i<itsRegions.nelements(); i++) {
	if (itsRegions[i]->hasMask()
        &&  itsRegions[i]->nelements() > maxNelem) {
	    itsHasMask = i;
	}
    }
}

bool LCRegionMulti::findAreas (IPosition& bufStart, IPosition& bufEnd,
			       IPosition& regStart, IPosition& regEnd,
			       const Slicer& section, uint32_t regNr) const
{
    DebugAssert (regNr < itsRegions.nelements(), AipsError);
    uint32_t nrdim = section.ndim();
    bufStart.resize (nrdim);
    bufEnd.resize (nrdim);
    regStart.resize (nrdim);
    regEnd.resize (nrdim);
    const IPosition& bboxstart = boundingBox().start();
    const IPosition& rstart = itsRegions[regNr]->boundingBox().start();
    const IPosition& rend = itsRegions[regNr]->boundingBox().end();
    bool overlap = true;
    for (uint32_t j=0; j<nrdim; j++) {
        int32_t bstart = bboxstart(j);
        int32_t secst  = section.start()(j);      // section start in bounding box
	int32_t secend = section.end()(j);        // section end in bounding box
	int32_t secinc = section.stride()(j);
	int32_t regst  = rstart(j) - bstart;      // region start in bounding box
	int32_t regend = rend(j) - bstart;        // region end in bounding box
	// Exit if there is no overlap between this region and the
	// requested section in the entire bounding box.
        if (regst > secend  ||  regend < secst) {
	    overlap = false;
	    break;
	}
	// Fine, there is overlap.
	// Now find out where the section starts in the region and in
	// the receiving buffer. There are several cases:
	// - section starts before or after region
	// - section ends before or after region
	// Life is complicated by the fact that the section can have a
	// stride. This less common case is handled separately to
	// not to affect performance of common case stride==1.
	// At the end we get:
	// - bufStart gives the offset of the first pixel in the buffer
	// - bufEnd   gives the offset of the last pixel in the buffer
	// - regStart gives the offset of the first pixel in the region
	// - regEnd   gives the offset of the last pixel in the region
	// They tell the caller where to get the required pixels from
	// this region and where to store them in the buffer.
	if (secinc == 1) {
	    int32_t diff = secst - regst;
	    if (diff >= 0) {
		regStart(j) = diff;        // section starts at or after region
		bufStart(j) = 0;
	    } else {
		regStart(j) = 0;           // section starts before region
		bufStart(j) = -diff;
	    }
	    if (regend < secend) {
		regEnd(j) = regend-regst;  // section ends after region
		bufEnd(j) = regend-secst;
	    } else {
		regEnd(j) = secend-regst;  // section ends at or before region
		bufEnd(j) = secend-secst;
	    }
	} else {
	    // The case with stride>1 is a bit more complicated.
	    // The buffer has no stride (it receives the required pixels only),
	    // so when determining its starts, we must take the increment
	    // into account. When the section starts before the boundary,
	    // the actual start in the region must be on a stride alignment.
	    // It is also possible that the increment is such that the
	    // entire region is skipped.
	    int32_t diff = secst - regst;
	    if (diff >= 0) {
		regStart(j) = diff;
		bufStart(j) = 0;
	    } else {
		int32_t diffalign = 1 + (-1-diff)/secinc;
		regStart(j) = diffalign*secinc + diff;
		bufStart(j) = diffalign;
	    }
	    if (regend < secend) {
		regEnd(j) = regend-regst;
	    } else {
		regEnd(j) = secend-regst;
	    }
	    if (regEnd(j) < regStart(j)) {
		overlap = false;
		break;
	    }
	    bufEnd(j) = bufStart(j) + (regEnd(j)-regStart(j))/secinc;
	}
	DebugAssert (bufEnd(j)-bufStart(j) == (regEnd(j)-regStart(j))/secinc,
		     AipsError);
    }
    return overlap;
}


TableRecord LCRegionMulti::makeRecord (const String& tableName) const
{
    TableRecord rec;
    int32_t nr = itsRegions.nelements();
    for (int32_t i=0; i<nr; i++) {
	rec.defineRecord (i, itsRegions[i]->toRecord (tableName));
    }
    rec.define ("nr", nr);
    return rec;
}

void LCRegionMulti::unmakeRecord (PtrBlock<const LCRegion*>& regions,
				  const TableRecord& rec,
				  const String& tableName)
{
    int32_t nr = rec.asInt ("nr");
    regions.resize (nr, true);
    for (int32_t i=0; i<nr; i++) {
	regions[i] = LCRegion::fromRecord (rec.asRecord (i), tableName);
    }
}

bool LCRegionMulti::doGetSlice (Array<bool>& buffer,
				const Slicer& section)
{
    if (itsHasMask >= 0) {
        multiGetSlice (buffer, section);
    } else {
        buffer.resize (section.length());
        buffer = true;
    }
    return false;
}

IPosition LCRegionMulti::doNiceCursorShape (uint32_t maxPixels) const
{
    if (itsHasMask >= 0) {
        return itsRegions[itsHasMask]->niceCursorShape (maxPixels);
    }
    return Lattice<bool>::doNiceCursorShape (maxPixels);
}

} //# NAMESPACE CASACORE - END

