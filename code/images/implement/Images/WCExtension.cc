//# WCExtension.cc: Make the extension of an image region
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


#include <trial/Images/WCExtension.h>
#include <trial/Images/WCBox.h>
#include <trial/Images/ImageRegion.h>
#include <trial/Lattices/LCExtension.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/GenSort.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>


WCExtension::WCExtension (const ImageRegion& region,
			  const WCBox& extendBox)
: WCCompound (region, ImageRegion(extendBox))
{
    // Check if the axes do not overlap.
    // They do not if the dimensionality is the sum of the
    // other dimensionalities.
    if (ndim() != region.ndim() + extendBox.ndim()) {
        throw (AipsError ("WCExtension::WCExtension - "
			  "one or more axes of region to be extended "
			  "is used in the extendBox too"));
    }
}

WCExtension::WCExtension (Bool takeOver, const PtrBlock<const WCRegion*>& regions)
: WCCompound (takeOver, regions)
{}

WCExtension::WCExtension (const WCExtension& other)
: WCCompound (other)
{}

WCExtension::~WCExtension()
{}

WCExtension& WCExtension::operator= (const WCExtension& other)
{
    if (this != &other) {
	WCCompound::operator= (other);
    }
    return *this;
}

Bool WCExtension::operator== (const WCRegion& other) const
{
   return WCCompound::operator== (other);
}

WCRegion* WCExtension::cloneRegion() const
{
    return new WCExtension (*this);
}

Bool WCExtension::canExtend() const
{
    // It extend itself if the box can do so.
    DebugAssert (regions().nelements() == 2, AipsError);
    return regions()[1]->canExtend();
}

LCRegion* WCExtension::doToLCRegion (const CoordinateSystem& cSys,
				     const IPosition& shape,
				     const IPosition& pixelAxesMap,
				     const IPosition& outOrder) const
{
    uInt i;
    // There should be 2 regions. The latter one should be a WCBox.
    DebugAssert (regions().nelements() == 2, AipsError);
    DebugAssert (regions()[1]->type() == WCBox::className(), AipsError);
    // Split the pixelAxesMap and outOrder into the parts for the
    // region and the box (which can be more than the box itself
    // because it might be extended).
    uInt ndout = outOrder.nelements();
    uInt ndreg = regions()[0]->ndim();
    uInt ndbox = ndout - ndreg;
    IPosition regPixMap(ndreg);
    IPosition regOutOrd(ndreg);
    IPosition boxPixMap(ndbox);
    IPosition boxOutOrd(ndbox);
    // In our axesDesc the first axes are used for the region and the
    // rest for the box.
    for (i=0; i<ndreg; i++) {
        regPixMap(i) = pixelAxesMap(i);
	regOutOrd(i) = outOrder(i);
    }
    for (i=0; i<ndbox; i++) {
        boxPixMap(i) = pixelAxesMap(i+ndreg);
	boxOutOrd(i) = outOrder(i+ndreg);
    }
    // Now boxOutOrd gives us the axes over which to extend.
    IPosition extendAxes (boxOutOrd);
    // The new outOrd objects have to have numbers in the range 0..n,
    // where n is the length.
    // We use the same trick as in WCRegion by sorting them and using
    // the resuting index vector.
    Vector<uInt> reginx(ndreg);
    GenSortIndirect<Int>::sort (reginx, regOutOrd.storage(), ndreg);
    for (i=0; i<ndreg; i++) {
	regOutOrd(reginx(i)) = i;
    }
    Vector<uInt> boxinx(ndbox);
    GenSortIndirect<Int>::sort (boxinx, boxOutOrd.storage(), ndbox);
    for (i=0; i<ndbox; i++) {
	boxOutOrd(boxinx(i)) = i;
    }
    // Great, we're almost there.
    // Convert the region and the box and combine them into an LCExtension.
    LCRegion* regptr = regions()[0]->toLCRegion (cSys, shape, regPixMap,
						 regOutOrd);
    LCRegion* boxptr = regions()[1]->toLCRegion (cSys, shape, boxPixMap,
						 boxOutOrd);
    DebugAssert (boxptr->type() == LCBox::className(), AipsError);
    LCExtension* extptr = new LCExtension (True, regptr, extendAxes,
					   *(LCBox*)boxptr);
    delete boxptr;
    return extptr;
}

String WCExtension::className()
{
    return "WCExtension";
}

String WCExtension::type() const
{
   return className();
}

TableRecord WCExtension::toRecord (const String& tableName) const
{
    TableRecord rec;
    defineRecordFields (rec, className());
    rec.defineRecord ("regions", makeRecord(tableName));
    return rec;
}

WCExtension* WCExtension::fromRecord (const TableRecord& rec,
				      const String& tableName)
{
    PtrBlock<const WCRegion*> regions;
    unmakeRecord (regions, rec.asRecord("regions"), tableName);
    return new WCExtension (True, regions);
}
