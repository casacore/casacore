//# WCExtension.cc: Make the extension of an image region
//# Copyright (C) 1998,2000,2001
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


#include <casacore/images/Regions/WCExtension.h>
#include <casacore/images/Regions/WCBox.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/lattices/LRegions/LCExtension.h>
#include <casacore/lattices/LRegions/LCStretch.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

WCExtension::WCExtension (const ImageRegion& region,
			  const WCBox& extendBox)
: WCCompound (region, ImageRegion(extendBox))
{}

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
    // It can extend itself if the box can do so.
    DebugAssert (regions().nelements() == 2, AipsError);
    return regions()[1]->canExtend();
}

void WCExtension::findAxes (IPosition& extendBoxAxes,
			    IPosition& stretchBoxAxes,
			    IPosition& stretchRegionAxes) const
{
    const WCRegion& box = *(regions()[1]);
    uInt nstretch = regions()[0]->ndim() + box.ndim() - ndim();
    uInt nextend = box.ndim() - nstretch;
    extendBoxAxes.resize (nextend);
    stretchBoxAxes.resize (nstretch);
    stretchRegionAxes.resize (nstretch);
    const Record& desc = regions()[0]->getAxesDesc();
    uInt nre = 0;
    uInt nrs = 0;
    for (uInt i=0; i<box.ndim(); i++) {
        Int axis = axisNr (box.getAxisDesc(i), desc);
	if (axis < 0) {
	    AlwaysAssert (nre < nextend, AipsError);
	    extendBoxAxes(nre++) = i;
	} else {
	    AlwaysAssert (nrs < nstretch, AipsError);
	    stretchBoxAxes(nrs) = i;
	    stretchRegionAxes(nrs++) = axis;
	}
    }
    AlwaysAssert (nre == nextend, AipsError);
    AlwaysAssert (nrs == nstretch, AipsError);
}

LCRegion* WCExtension::doToLCRegion (const CoordinateSystem& cSys,
				     const IPosition& shape,
				     const IPosition& pixelAxesMap,
				     const IPosition& outOrder) const
{
    // There should be 2 regions. The latter one should be a WCBox.
    DebugAssert (regions().nelements() == 2, AipsError);
    DebugAssert (regions()[1]->type() == WCBox::className(), AipsError);
    uInt ndout = outOrder.nelements();
    uInt ndreg = regions()[0]->ndim();
    AlwaysAssert (ndreg <= ndout, AipsError);
    // Split the box into the extend and the stretch part.
    // The IPositions give the axis numbers in the extend box.
    const WCBox* bptr = dynamic_cast<const WCBox*>(regions()[1]);
    AlwaysAssert (bptr != 0, AipsError);
    IPosition extendBoxAxes;
    IPosition stretchBoxAxes;
    IPosition stretchRegAxes;
    findAxes (extendBoxAxes, stretchBoxAxes, stretchRegAxes);
    WCBox extbox = bptr->splitBox (extendBoxAxes);
    WCBox strbox = bptr->splitBox (stretchBoxAxes);
    // Split the pixelAxesMap and outOrder into the parts for the
    // region, the stretch box and the extend box (which can be more than
    // the box itself because there can be extra extend axes).
    uInt ndstr = stretchBoxAxes.nelements();
    uInt ndext = ndout - ndreg;
    DebugAssert (ndext >= extendBoxAxes.nelements(), AipsError);
    IPosition regPixMap(ndreg);
    IPosition regOutOrd(ndreg);
    IPosition strPixMap(ndstr);
    IPosition strOutOrd(ndstr);
    IPosition extPixMap(ndext);
    IPosition extOutOrd(ndext);
    // In our axesDesc the first axes are used for the region.
    for (uInt i=0; i<ndreg; i++) {
        regPixMap(i) = pixelAxesMap(i);
	regOutOrd(i) = outOrder(i);
    }
    // The rest of the pixel/outOrder are for the extend box.
    for (uInt i=0; i<ndext; i++) {
        extPixMap(i) = pixelAxesMap(i+ndreg);
	extOutOrd(i) = outOrder(i+ndreg);
    }
    // The stretch box uses some axes in the region.
    for (uInt i=0; i<ndstr; i++) {
        uInt axis = stretchRegAxes(i);
	strPixMap(i) = pixelAxesMap(axis);
	strOutOrd(i) = outOrder(axis);
    }
    // Determine the axes to be passed to LCStretch and LCExtension.
    // Note that outOrd already reorders the axes as needed, so we
    // need the axes for the LC objects in normal order.
    IPosition extendAxes (ndext);
    IPosition stretchAxes (ndstr);
    // The new outOrd objects have to have numbers in the range 0..n,
    // where n is the length.
    // We use the same trick as in WCRegion by sorting them and using
    // the resulting index vector.
    Vector<uInt> reginx(ndreg);
    std::vector<Int> tmpreg(regOutOrd.begin(), regOutOrd.end());
    GenSortIndirect<Int>::sort (reginx, &(tmpreg[0]), ndreg);
    for (uInt i=0; i<ndreg; i++) {
	regOutOrd(reginx(i)) = i;
    }
    Vector<uInt> extinx(ndext);
    std::vector<Int> tmpext(extOutOrd.begin(), extOutOrd.end());
    GenSortIndirect<Int>::sort (extinx, &(tmpext[0]), ndext);
    for (uInt i=0; i<ndext; i++) {
        extendAxes(i) = extOutOrd(extinx(i));
	extOutOrd(extinx(i)) = i;
    }
    Vector<uInt> strinx(ndstr);
    std::vector<Int> tmpstr(strOutOrd.begin(), strOutOrd.end());
    GenSortIndirect<Int>::sort (strinx, &(tmpstr[0]), ndstr);
    for (uInt i=0; i<ndstr; i++) {
        stretchAxes(i) = regOutOrd(stretchRegAxes(i));
	strOutOrd(strinx(i)) = i;
    }
    // The box axes get already reordered by its toLCRegion.
    // So the stretched axis must be region axis in the new order.
    std::vector<Int> tmpstretch(stretchAxes.begin(), stretchAxes.end());
    GenSortIndirect<Int>::sort (strinx, &(tmpstretch[0]), ndstr);
    for (uInt i=0; i<ndstr; i++) {
        stretchRegAxes(i) = stretchAxes(strinx(i));
    }
    // Great, we're almost there.
    // Convert the region and the box and combine them into an
    // LCStretch and/or LCExtension.
    LCRegion* regptr = regions()[0]->toLCRegionAxes (cSys, shape, regPixMap,
						     regOutOrd);
    if (ndstr > 0) {
        LCRegion* boxptr = strbox.toLCRegionAxes (cSys, shape, strPixMap,
						  strOutOrd);
	LCBox* dboxptr = dynamic_cast<LCBox*>(boxptr);
	AlwaysAssert (dboxptr != 0, AipsError);
	LCStretch* extptr = new LCStretch (True, regptr, stretchRegAxes,
					   *dboxptr);
	delete boxptr;
	regptr = extptr;
    }
    if (ndext > 0) {
        LCRegion* boxptr = extbox.toLCRegionAxes (cSys, shape, extPixMap,
						  extOutOrd);
	LCBox* dboxptr = dynamic_cast<LCBox*>(boxptr);
	AlwaysAssert (dboxptr != 0, AipsError);
	LCExtension* extptr = new LCExtension (True, regptr, extendAxes,
					       *dboxptr);
	delete boxptr;
	regptr = extptr;
    }
    return regptr;
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

} //# NAMESPACE CASACORE - END

