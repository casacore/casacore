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
//#
//# $Id$


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
    init (False);
}

LCRegionMulti::LCRegionMulti (Bool takeOver, const LCRegion* region1,
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
    uInt n=0;
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
    itsRegions.resize (n, True, True);
    init (takeOver);
}

LCRegionMulti::LCRegionMulti (Bool takeOver,
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
    uInt nr = itsRegions.nelements();
    for (uInt i=0; i<nr; i++) {
	itsRegions[i] = other.itsRegions[i]->cloneRegion();
    }
}

LCRegionMulti::~LCRegionMulti()
{
    uInt nr = itsRegions.nelements();
    for (uInt i=0; i<nr; i++) {
	delete itsRegions[i];
    }
}

LCRegionMulti& LCRegionMulti::operator= (const LCRegionMulti& other)
{
    if (this != &other) {
	LCRegion::operator= (other);
	itsHasMask = other.itsHasMask;
	uInt nr = itsRegions.nelements();
	for (uInt j=0; j<nr; j++) {
	    delete itsRegions[j];
	    itsRegions[j] = 0;
	}
	itsRegions.resize (other.itsRegions.nelements(), True);
	nr = itsRegions.nelements();
	for (uInt i=0; i<nr; i++) {
	    itsRegions[i] = other.itsRegions[i]->cloneRegion();
	}
    }
    return *this;
}

Bool LCRegionMulti::hasMask() const
{
    return (itsHasMask >= 0);
}

void LCRegionMulti::multiTranslate (PtrBlock<const LCRegion*>& regions,
				    const Vector<Float>& translateVector,
				    const IPosition& newLatticeShape) const
{
    regions.resize (itsRegions.nelements(), True);
    for (uInt i=0; i<itsRegions.nelements(); i++) {
        regions[i] = itsRegions[i]->translate (translateVector,
					       newLatticeShape);
    }
}


Bool LCRegionMulti::operator== (const LCRegion& other) const
{
    // Check if parent class matches.
    // If so, we can safely cast.
    if (! LCRegion::operator== (other)) {
	return False;
    }
    const LCRegionMulti& that = (const LCRegionMulti&)other;
    // Check the regions.
    if (itsRegions.nelements() != that.itsRegions.nelements()) {
	return False;
    }
    // The regions do not have to be in the same order.
    // It makes it a bit slower.
    uInt nr = itsRegions.nelements();
    Vector<Bool> used(nr, False);
    for (uInt i=0; i<nr; i++) {
	Bool found = False;
	for (uInt j=0; j<nr; j++) {
	    if (!used(j)) {      
		if (*itsRegions[i] == *(that.itsRegions[j])) {
		    used(j) = True;
		    found = True;
		    break;
		}
	    }
	}
	if (!found) {
	    return False;          // no matching region
	}
    }
    return True;
}


void LCRegionMulti::init (Bool takeOver)
{
    itsHasMask = 0;
    for (uInt i=0; i<itsRegions.nelements(); i++) {
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
    uInt maxNelem = 0;
    for (uInt i=0; i<itsRegions.nelements(); i++) {
	if (itsRegions[i]->hasMask()
        &&  itsRegions[i]->nelements() > maxNelem) {
	    itsHasMask = i;
	}
    }
}

Bool LCRegionMulti::findAreas (IPosition& bufStart, IPosition& bufEnd,
			       IPosition& regStart, IPosition& regEnd,
			       const Slicer& section, uInt regNr) const
{
    DebugAssert (regNr < itsRegions.nelements(), AipsError);
    uInt nrdim = section.ndim();
    bufStart.resize (nrdim);
    bufEnd.resize (nrdim);
    regStart.resize (nrdim);
    regEnd.resize (nrdim);
    const IPosition& bboxstart = boundingBox().start();
    const IPosition& rstart = itsRegions[regNr]->boundingBox().start();
    const IPosition& rend = itsRegions[regNr]->boundingBox().end();
    Bool overlap = True;
    for (uInt j=0; j<nrdim; j++) {
        Int bstart = bboxstart(j);
        Int secst  = section.start()(j);      // section start in bounding box
	Int secend = section.end()(j);        // section end in bounding box
	Int secinc = section.stride()(j);
	Int regst  = rstart(j) - bstart;      // region start in bounding box
	Int regend = rend(j) - bstart;        // region end in bounding box
	// Exit if there is no overlap between this region and the
	// requested section in the entire bounding box.
        if (regst > secend  ||  regend < secst) {
	    overlap = False;
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
	    Int diff = secst - regst;
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
	    Int diff = secst - regst;
	    if (diff >= 0) {
		regStart(j) = diff;
		bufStart(j) = 0;
	    } else {
		Int diffalign = 1 + (-1-diff)/secinc;
		regStart(j) = diffalign*secinc + diff;
		bufStart(j) = diffalign;
	    }
	    if (regend < secend) {
		regEnd(j) = regend-regst;
	    } else {
		regEnd(j) = secend-regst;
	    }
	    if (regEnd(j) < regStart(j)) {
		overlap = False;
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
    Int nr = itsRegions.nelements();
    for (Int i=0; i<nr; i++) {
	rec.defineRecord (i, itsRegions[i]->toRecord (tableName));
    }
    rec.define ("nr", nr);
    return rec;
}

void LCRegionMulti::unmakeRecord (PtrBlock<const LCRegion*>& regions,
				  const TableRecord& rec,
				  const String& tableName)
{
    Int nr = rec.asInt ("nr");
    regions.resize (nr, True);
    for (Int i=0; i<nr; i++) {
	regions[i] = LCRegion::fromRecord (rec.asRecord (i), tableName);
    }
}

Bool LCRegionMulti::doGetSlice (Array<Bool>& buffer,
				const Slicer& section)
{
    if (itsHasMask >= 0) {
        multiGetSlice (buffer, section);
    } else {
        buffer.resize (section.length());
        buffer = True;
    }
    return False;
}

IPosition LCRegionMulti::doNiceCursorShape (uInt maxPixels) const
{
    if (itsHasMask >= 0) {
        return itsRegions[itsHasMask]->niceCursorShape (maxPixels);
    }
    return Lattice<Bool>::doNiceCursorShape (maxPixels);
}

} //# NAMESPACE CASACORE - END

