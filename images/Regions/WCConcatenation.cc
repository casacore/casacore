//# WCConcatenation.cc: Combine multiple ImageRegion's into a new dimension
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
//#
//# $Id$


#include <casacore/images/Regions/WCConcatenation.h>
#include <casacore/images/Regions/WCBox.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/lattices/LRegions/LCConcatenation.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

WCConcatenation::WCConcatenation (const PtrBlock<const ImageRegion*>& regions,
				  const WCBox& extendBox)
: WCCompound   (regions),
  itsExtendBox (extendBox)
{
    fill();
}

WCConcatenation::WCConcatenation (Bool takeOver,
				  const PtrBlock<const WCRegion*>& regions,
				  const WCBox& extendBox)
: WCCompound   (takeOver, regions),
  itsExtendBox (extendBox)
{
    fill();
}

WCConcatenation::WCConcatenation (const WCConcatenation& other)
: WCCompound   (other),
  itsExtendBox (other.itsExtendBox)
{}

WCConcatenation::~WCConcatenation()
{}

WCConcatenation& WCConcatenation::operator= (const WCConcatenation& other)
{
    if (this != &other) {
	WCCompound::operator= (other);
	itsExtendBox = other.itsExtendBox;
    }
    return *this;
}

Bool WCConcatenation::operator== (const WCRegion& other) const
{
    if (! WCCompound::operator== (other)) {
        return False;
    }
    const WCConcatenation& that = (const WCConcatenation&)other;
    return (itsExtendBox == that.itsExtendBox);
}

WCRegion* WCConcatenation::cloneRegion() const
{
    return new WCConcatenation (*this);
}

void WCConcatenation::fill()
{
    // Check if all regions have the same axes which is true if they
    // have the same dimensionality as the compound.
    uInt nr = regions().nelements();
    for (uInt i=0; i<nr; i++) {
        if (regions()[i]->ndim() != ndim()) {
	    throw (AipsError ("WCConcatenation::WCConcatenation - "
			      "all its regions should have the same axes"));
	}
    }
    // Check if the box is 1-dimensional.
    if (itsExtendBox.ndim() != 1) {
        throw (AipsError ("WCConcatenation::WCConcatenation - "
			  "the extendBox should be 1-dim"));
    }
    const Record& desc = itsExtendBox.getAxisDesc(0);
    // If the axis is already defined, an exception is thrown.
    // Otherwise add its description.
    if (axisNr (desc, getAxesDesc()) >= 0) {
        throw (AipsError ("WCConcatenation::WCConcatenation - "
			  "one or more axes of region to be extended "
			  "is used in the extendBox"));
    }
    addAxisDesc (desc);
}

LCRegion* WCConcatenation::doToLCRegion (const CoordinateSystem& cSys,
					 const IPosition& shape,
					 const IPosition& pixelAxesMap,
					 const IPosition& outOrder) const
{
    uInt i;
    // Split the pixelAxesMap and outOrder into the parts for the
    // region and the box (which can be more than the box itself
    // because it might be extended).
    uInt ndreg = ndim() - 1;
    DebugAssert (outOrder.nelements() == ndim(), AipsError);
    IPosition regPixMap(ndreg);
    IPosition regOutOrd(ndreg);
    IPosition boxPixMap(1, pixelAxesMap(ndreg));
    IPosition boxOutOrd(1, 0);
    // In our axesDesc the first axes are used for the region and the
    // rest for the box.
    for (i=0; i<ndreg; i++) {
        regPixMap(i) = pixelAxesMap(i);
	regOutOrd(i) = outOrder(i);
    }
    // The new outOrd objects have to have numbers in the range 0..n,
    // where n is the length.
    // We use the same trick as in WCRegion by sorting them and using
    // the resulting index vector.
    Vector<uInt> reginx(ndreg);
    std::vector<Int> tmp(regOutOrd.begin(), regOutOrd.end());
    GenSortIndirect<Int>::sort (reginx, &(tmp[0]), ndreg);
    for (i=0; i<ndreg; i++) {
	regOutOrd(reginx(i)) = i;
    }
    // Great, we're almost there.
    // Convert the regions and the box and combine them into an LCConcatenation.
    // The extend axis is the last axis of outOrder.
    PtrBlock<const LCRegion*> regions;
    multiToLCRegion (regions, cSys, shape, regPixMap, regOutOrd);
    LCRegion* boxptr = itsExtendBox.toLCRegionAxes (cSys, shape, boxPixMap,
						    boxOutOrd);
    DebugAssert (boxptr->type() == LCBox::className(), AipsError);
    LCConcatenation* extptr = new LCConcatenation (True, regions,
						   outOrder(ndreg),
						   *(LCBox*)boxptr);
    delete boxptr;
    return extptr;
}

String WCConcatenation::className()
{
    return "WCConcatenation";
}

String WCConcatenation::type() const
{
   return className();
}

TableRecord WCConcatenation::toRecord (const String& tableName) const
{
    TableRecord rec;
    defineRecordFields (rec, className());
    rec.defineRecord ("regions", makeRecord(tableName));
    rec.defineRecord ("box", itsExtendBox.toRecord(tableName));
    return rec;
}

WCConcatenation* WCConcatenation::fromRecord (const TableRecord& rec,
					      const String& tableName)
{
    PtrBlock<const WCRegion*> regions;
    unmakeRecord (regions, rec.asRecord("regions"), tableName);
    WCRegion* boxptr = WCRegion::fromRecord (rec.asRecord("box"), tableName);
    DebugAssert (boxptr->type() == WCBox::className(), AipsError);
    return new WCConcatenation (True, regions, *(const WCBox*)boxptr);
}

} //# NAMESPACE CASACORE - END

