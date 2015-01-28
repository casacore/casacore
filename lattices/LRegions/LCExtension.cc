//# LCExtension.cc: Extend an LCRegion along straight lines to other dimensions
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


#include <casacore/lattices/LRegions/LCExtension.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LCExtension::LCExtension()
{}

LCExtension::LCExtension (const LCRegion& region,
			  const IPosition& extendAxes,
			  const LCBox& extendBox)
: LCRegionMulti (True, region.cloneRegion())
{
    // Fill the other members variables and determine the bounding box.
    fill (extendAxes, extendBox);
}

LCExtension::LCExtension (Bool takeOver,
			  const LCRegion* region,
			  const IPosition& extendAxes,
			  const LCBox& extendBox)
: LCRegionMulti (takeOver, region)
{
    // Fill the other members variables and determine the bounding box.
    fill (extendAxes, extendBox);
}

LCExtension::LCExtension (const LCExtension& other)
: LCRegionMulti (other),
  itsExtendAxes (other.itsExtendAxes),
  itsRegionAxes (other.itsRegionAxes),
  itsExtendBox  (other.itsExtendBox)
{}

LCExtension::~LCExtension()
{}

LCExtension& LCExtension::operator= (const LCExtension& other)
{
    if (this != &other) {
	LCRegionMulti::operator= (other);
	itsExtendAxes.resize (other.itsExtendAxes.nelements());
	itsRegionAxes.resize (other.itsRegionAxes.nelements());
	itsExtendAxes = other.itsExtendAxes;
	itsRegionAxes = other.itsRegionAxes;
	itsExtendBox  = other.itsExtendBox;
    }
    return *this;
}

Bool LCExtension::operator== (const LCRegion& other) const
{
    // Check if parent class matches.
    // If so, we can safely cast.
    if (! LCRegionMulti::operator== (other)) {
	return False;
    }
    const LCExtension& that = (const LCExtension&)other;
    // Check the private data
    if (! itsExtendAxes.isEqual (that.itsExtendAxes)
    ||  ! itsRegionAxes.isEqual (that.itsRegionAxes)
    ||  !(itsExtendBox == that.itsExtendBox)) {
	return False;
    }
    return True;
}
 

LCRegion* LCExtension::cloneRegion() const
{
    return new LCExtension (*this);
}

LCRegion* LCExtension::doTranslate (const Vector<Float>& translateVector,
				    const IPosition& newLatticeShape) const
{
    uInt i;
    // First translate the extendBox.
    // Take appropriate elements from the vectors.
    uInt nre = itsExtendAxes.nelements();
    Vector<Float> boxTransVec (nre);
    IPosition boxLatShape (nre);
    for (i=0; i<nre; i++) {
	uInt axis = itsExtendAxes(i);
	boxTransVec(i) = translateVector(axis);
	boxLatShape(i) = newLatticeShape(axis);
    }
    LCBox* boxPtr = (LCBox*)(itsExtendBox.translate (boxTransVec, boxLatShape));
    // Now translate the region.
    uInt nrr = itsRegionAxes.nelements();
    Vector<Float> regTransVec (nrr);
    IPosition regLatShape (nrr);
    for (i=0; i<nrr; i++) {
	uInt axis = itsRegionAxes(i);
	regTransVec(i) = translateVector(axis);
	regLatShape(i) = newLatticeShape(axis);
    }
    LCRegion* regPtr = region().translate (regTransVec, regLatShape);
    // Create the new LCExtension object.
    LCExtension* extPtr = new LCExtension (*regPtr, itsExtendAxes, *boxPtr);
    delete boxPtr;
    delete regPtr;
    return extPtr;
}

String LCExtension::className()
{
    return "LCExtension";
}

String LCExtension::type() const
{
   return className();
}

TableRecord LCExtension::toRecord (const String& tableName) const
{
    TableRecord rec;
    defineRecordFields (rec, className());
    rec.defineRecord ("region", region().toRecord (tableName));
    rec.define ("axes", itsExtendAxes.asVector());
    rec.defineRecord ("box", itsExtendBox.toRecord (tableName));
    return rec;
}

LCExtension* LCExtension::fromRecord (const TableRecord& rec,
				      const String& tableName)
{
    // Initialize pointers to 0 to get rid of gcc-2.95 warnings.
    LCRegion* regPtr = 0;
    regPtr = LCRegion::fromRecord (rec.asRecord("region"), tableName);
    LCBox* boxPtr = 0;
    boxPtr = (LCBox*)(LCRegion::fromRecord (rec.asRecord("box"), tableName));
    LCExtension* extPtr = new LCExtension (True, regPtr,
					  Vector<Int>(rec.toArrayInt ("axes")),
					  *boxPtr);
    delete boxPtr;
    return extPtr;
}

void LCExtension::fillRegionAxes()
{
    uInt nre = itsExtendAxes.nelements();
    uInt nrr = region().ndim();
    uInt nrdim = nre+nrr;
    // allAxes will get the remaining (thus region) axes at the end.
    IPosition allAxes = IPosition::makeAxisPath (nrdim, itsExtendAxes);
    itsRegionAxes.resize (nrr);
    for (uInt i=nre; i<nrdim; i++) {
        uInt axis = allAxes(i);
	itsRegionAxes(i-nre) = axis;
    }
}

void LCExtension::fill (const IPosition& extendAxes, const LCBox& extendBox)
{
    // Check if extend axes are specified correctly.
    // They do not need to be in ascending order, but duplicates are
    // not allowed. 
    IPosition regionShape = region().shape();
    uInt nre = extendAxes.nelements();
    if (nre == 0) {
	throw (AipsError ("LCExtension::LCExtension - "
			  "no extend axes have been specified"));
    }
    if (nre != extendBox.blc().nelements()) {
	throw (AipsError ("LCExtension::LCExtension - "
			  "number of axes in extend box mismatches "
			  "number of extend axes"));
    }
    // The axes can be specified in any order. We want them ordered.
    // So sort them and fill itsExtendAxes and itsExtendBox.
    itsExtendAxes.resize (nre);
    IPosition boxLatShape(nre);
    Vector<Float> boxLatBlc(nre);
    Vector<Float> boxLatTrc(nre);
    Vector<uInt> reginx(nre);
    GenSortIndirect<ssize_t>::sort (reginx, extendAxes.storage(), nre);
    Int first = -1;
    for (uInt i=0; i<nre; i++) {
        uInt axis = reginx(i);
	itsExtendAxes(i) = extendAxes(axis);
	boxLatShape(i) = extendBox.latticeShape()(axis);
	boxLatBlc(i) = extendBox.blc()(axis);
	boxLatTrc(i) = extendBox.trc()(axis);
	if (itsExtendAxes(i) <= first) {
	    throw (AipsError ("LCExtension::LCExtension - "
			      "extend axes multiply specified"));
	}
	first = itsExtendAxes(i);
    }
    itsExtendBox = LCBox (boxLatBlc, boxLatTrc, boxLatShape);
    // Fill itsRegionAxes, i.e. the mapping of the axis of the contributing
    // region into the extended region.
    fillRegionAxes();
    // Make up the lattice shape from the region and box latticeshape.
    // Fill the bounding box from blc/trc in region and box.
    uInt nrr = itsRegionAxes.nelements();
    uInt nrdim = nre+nrr;
    IPosition latShape(nrdim);
    IPosition blc (nrdim);
    IPosition trc (nrdim);
    const IPosition& regionShp = region().latticeShape();
    const IPosition& regionBlc = region().boundingBox().start();
    const IPosition& regionTrc = region().boundingBox().end();
    for (uInt i=0; i<nrr; i++) {
        uInt axis = itsRegionAxes(i);
	latShape(axis) = regionShp(i);
	blc(axis) = regionBlc(i);
	trc(axis) = regionTrc(i);
    }
    const IPosition& boxShp = itsExtendBox.latticeShape();
    const IPosition& boxBlc = itsExtendBox.boundingBox().start();
    const IPosition& boxTrc = itsExtendBox.boundingBox().end();
    for (uInt i=0; i<nre; i++) {
        uInt axis = itsExtendAxes(i);
	latShape(axis) = boxShp(i);
	blc(axis) = boxBlc(i);
	trc(axis) = boxTrc(i);
    }
    setShapeAndBoundingBox (latShape, Slicer(blc, trc, Slicer::endIsLast));
    fillHasMask();
}


void LCExtension::multiGetSlice (Array<Bool>& buffer,
				 const Slicer& section)
{
    buffer.resize (section.length());
    uInt i;
    uInt nre = itsExtendAxes.nelements();
    uInt nrr = itsRegionAxes.nelements();
    // Read the required region section.
    // This means we have to create a Slicer for those axes only.
    IPosition blc(nrr);
    IPosition len(nrr);
    IPosition inc(nrr);
    IPosition shape(buffer.ndim(), 1);
    for (i=0; i<nrr; i++) {
        uInt axis = itsRegionAxes(i);
	blc(i) = section.start()(axis);
	len(i) = section.length()(axis);
	inc(i) = section.stride()(axis);
	shape(axis) = len(i);
    }
    Array<Bool> tmpbuf(len);
    LCRegion* reg = (LCRegion*)(regions()[0]);
    reg->doGetSlice (tmpbuf, Slicer(blc, len, inc));
    // Reform tmpbuf, so it has the same dimensionality as buffer.
    Array<Bool> mask = tmpbuf.reform (shape);
    // Now we have to extend tmpbuf along all extend axes.
    const IPosition& length = section.length();
    IPosition pos (buffer.ndim(), 0);
    IPosition end (buffer.shape() - 1);
    //# Iterate along itsExtendAxes (the new axes) through the new mask.
    for (;;) {
	for (i=0; i<nre; i++) {
	    end(itsExtendAxes(i)) = pos(itsExtendAxes(i));
	}
	//# Set each section of the mask to the mask of the region.
	buffer(pos,end) = mask;
	//# Go to the next section.
	for (i=0; i<nre; i++) {
	    if (++pos(itsExtendAxes(i)) < length(itsExtendAxes(i))) {
		break;
	    }
	    // This dimension is done. Reset it and continue with the next.
	    pos(itsExtendAxes(i)) = 0;
        }
	//# End the iteration when all dimensions are done.
	if (i == nre) {
	    break;
	}
    }
}

IPosition LCExtension::doNiceCursorShape (uInt maxPixels) const
{
    return Lattice<Bool>::doNiceCursorShape (maxPixels);
}

} //# NAMESPACE CASACORE - END

