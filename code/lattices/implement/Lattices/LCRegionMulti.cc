//# LCRegionMulti.cc: Abstract base class for regions composed of other regions
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
//# $Id$


#include <trial/Lattices/LCRegionMulti.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <stdio.h>                          // for sprintf


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
	itsRegions.resize (other.itsRegions.nelements(), True);
	uInt nr = itsRegions.nelements();
	for (uInt i=0; i<nr; i++) {
	    itsRegions[i] = other.itsRegions[i]->cloneRegion();
	}
    }
    return *this;
}

Bool LCRegionMulti::hasMask() const
{
    return ToBool(itsHasMask >= 0);
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
    uInt nrdim = section.ndim();
    bufStart.resize (nrdim);
    bufEnd.resize (nrdim);
    regStart.resize (nrdim);
    regEnd.resize (nrdim);
    const IPosition& bboxstart = box().start();
    const IPosition& rstart = itsRegions[regNr]->box().start();
    const IPosition& rend = itsRegions[regNr]->box().end();
    Bool overlap = True;
    for (uInt j=0; j<nrdim; j++) {
        Int bstart = bboxstart(j);
        Int secst  = section.start()(j);
	Int secend = section.end()(j);
	Int regst  = rstart(j) - bstart;
	Int regend = rend(j) - bstart;
        if (regst > secend  ||  regend < secst) {
	    overlap = False;
	    break;
	}
	if (secst < regst) {
	    regStart(j) = 0;
	    bufStart(j) = regst-secst;
	} else {
	    regStart(j) = secst-regst;
	    bufStart(j) = 0;
	}
	if (regend < secend) {
	    regEnd(j) = regend-regst;
	    bufEnd(j) = regend-secst;
	} else {
	    regEnd(j) = secend-regst;
	    bufEnd(j) = secend-secst;
	}
    }
    return overlap;
}

TableRecord LCRegionMulti::makeRecord (const String& tableName) const
{
    TableRecord rec;
    Int nr = itsRegions.nelements();
    rec.define ("nr", nr);
    char str[8];
    for (Int i=0; i<nr; i++) {
        sprintf (str, "r%i", i);
	rec.defineRecord (str, itsRegions[i]->toRecord (tableName));
    }
    return rec;
}

void LCRegionMulti::unmakeRecord (PtrBlock<const LCRegion*>& regions,
				  const TableRecord& rec,
				  const String& tableName)
{
    Int nr = rec.asInt ("nr");
    regions.resize (nr, True);
    char str[8];
    for (Int i=0; i<nr; i++) {
        sprintf (str, "r%i", i);
	regions[i] = LCRegion::fromRecord (rec.asRecord (str), tableName);
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
