//# LCConcatenation.cc: Combine multiple LCRegion's into a new dimension
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


#include <casacore/lattices/LRegions/LCConcatenation.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

LCConcatenation::LCConcatenation()
{}

LCConcatenation::LCConcatenation (Bool takeOver,
				  const PtrBlock<const LCRegion*>& regions,
				  Int extendAxis)
: LCRegionMulti (takeOver, regions),
  itsExtendAxis (extendAxis)
{
    // Define a box for the entire shape (is length of regions vector)..
    itsExtendBox = LCBox(IPosition(1,0), IPosition(1,regions.nelements()-1),
			 IPosition(1,regions.nelements()));
    // Fill the other members variables and determine the bounding box.
    fill();
}

LCConcatenation::LCConcatenation (Bool takeOver,
				  const PtrBlock<const LCRegion*>& regions,
				  Int extendAxis,
				  const LCBox& extendBox)
: LCRegionMulti (takeOver, regions),
  itsExtendAxis (extendAxis),
  itsExtendBox  (extendBox)
{
    // Fill the other members variables and determine the bounding box.
    fill();
}

LCConcatenation::LCConcatenation (const LCConcatenation& other)
: LCRegionMulti (other),
  itsExtendAxis (other.itsExtendAxis),
  itsRegionAxes (other.itsRegionAxes),
  itsExtendBox  (other.itsExtendBox)
{}

LCConcatenation::~LCConcatenation()
{}

LCConcatenation& LCConcatenation::operator= (const LCConcatenation& other)
{
    if (this != &other) {
	LCRegionMulti::operator= (other);
	itsRegionAxes.resize (other.itsRegionAxes.nelements());
	itsExtendAxis = other.itsExtendAxis;
	itsRegionAxes = other.itsRegionAxes;
	itsExtendBox  = other.itsExtendBox;
    }
    return *this;
}

Bool LCConcatenation::operator== (const LCRegion& other) const
{
    // Check if parent class matches.
    // If so, we can safely cast.
    if (! LCRegionMulti::operator== (other)) {
	return False;
    }
    const LCConcatenation& that = (const LCConcatenation&)other;
    // Check the private data
    if (! (itsExtendAxis == that.itsExtendAxis)
    ||  ! itsRegionAxes.isEqual (that.itsRegionAxes)
    ||  !(itsExtendBox == that.itsExtendBox)) {
	return False;
    }
    return True;
}
 

LCRegion* LCConcatenation::cloneRegion() const
{
    return new LCConcatenation (*this);
}

LCRegion* LCConcatenation::doTranslate (const Vector<Float>& translateVector,
					const IPosition& newLatticeShape) const
{
    uInt i;
    // First translate extendBox.
    // Take appropriate elements from the vectors.
    Vector<Float> boxTransVec (1);
    IPosition boxLatShape (1);
    boxTransVec(0) = translateVector(itsExtendAxis);
    boxLatShape(0) = newLatticeShape(itsExtendAxis);
    LCBox* boxPtr = (LCBox*)(itsExtendBox.translate (boxTransVec, boxLatShape));
    // Now translate regions.
    uInt nrr = itsRegionAxes.nelements();
    Vector<Float> regTransVec (nrr);
    IPosition regLatShape (nrr);
    for (i=0; i<nrr; i++) {
	uInt axis = itsRegionAxes(i);
	regTransVec(i) = translateVector(axis);
	regLatShape(i) = newLatticeShape(axis);
    }
    PtrBlock<const LCRegion*> regions;
    multiTranslate (regions, regTransVec, regLatShape);
    // Create the new LCConcatenation object.
    LCConcatenation* extPtr = new LCConcatenation (True, regions,
						   itsExtendAxis, *boxPtr);
    delete boxPtr;
    return extPtr;
}

String LCConcatenation::className()
{
    return "LCConcatenation";
}

String LCConcatenation::type() const
{
   return className();
}

TableRecord LCConcatenation::toRecord (const String& tableName) const
{
    TableRecord rec;
    defineRecordFields (rec, className());
    rec.defineRecord ("regions", makeRecord(tableName));
    rec.define ("axis", itsExtendAxis);
    rec.defineRecord ("box", itsExtendBox.toRecord (tableName));
    return rec;
}

LCConcatenation* LCConcatenation::fromRecord (const TableRecord& rec,
					      const String& tableName)
{
    PtrBlock<const LCRegion*> regions;
    unmakeRecord (regions, rec.asRecord("regions"), tableName);
    LCBox* boxPtr = (LCBox*)(LCRegion::fromRecord (rec.asRecord("box"),
						   tableName));
    LCConcatenation* extPtr = new LCConcatenation (True, regions,
						   rec.asInt ("axis"),
						   *boxPtr);
    delete boxPtr;
    return extPtr;
}

void LCConcatenation::fillRegionAxes()
{
    // Extend the axes to all of them.
    // The specified axis is the first one, thereafter the remaining axes.
    uInt nrdim = 1 + regions()[0]->ndim();
    IPosition allAxes = IPosition::makeAxisPath (nrdim,
						 IPosition(1, itsExtendAxis));
    itsRegionAxes.resize (nrdim-1);
    for (uInt i=1; i<nrdim; i++) {
        uInt axis = allAxes(i);
	itsRegionAxes(i-1) = axis;
    }
}

void LCConcatenation::fill()
{
    uInt i;
    // Check if the basic things are right.
    if (itsExtendBox.ndim() != 1) {
	throw (AipsError ("LCConcatenation::LCConcatenation - "
			  "extendBox has to be 1-dimensional"));
    }
    fillRegionAxes();
    // Check if number of regions matches.
    if (Int(regions().nelements()) != itsExtendBox.shape()(0)) {
	throw (AipsError ("LCConcatenation::LCConcatenation - "
			  "number of regions has to match the range "
			  "specified in extendBox"));
    }
    // Find the minimum/maximum box of the regions.
    uInt nrr = itsRegionAxes.nelements();
    IPosition regionBlc(regions()[0]->boundingBox().start());
    IPosition regionTrc(regions()[0]->boundingBox().end());
    uInt nr = regions().nelements();
    for (i=1; i<nr; i++) {
	const IPosition& regblc = regions()[i]->boundingBox().start();
	const IPosition& regtrc = regions()[i]->boundingBox().end();
	for (uInt j=0; j<nrr; j++) {
	    if (regblc(j) < regionBlc(j)) {
	        regionBlc(j) = regblc(j);
	    }
	    if (regtrc(j) > regionTrc(j)) {
	        regionTrc(j) = regtrc(j);
	    }
	}
    }
    // Make up the lattice shape from the first region and box latticeshape.
    // Fill the bounding box from blc/trc in regions and box.
    uInt nrdim = nrr+1;
    IPosition latShape(nrdim);
    IPosition blc (nrdim);
    IPosition trc (nrdim);
    const IPosition& regionShp = regions()[0]->latticeShape();
    for (i=0; i<nrr; i++) {
        uInt axis = itsRegionAxes(i);
	latShape(axis) = regionShp(i);
	blc(axis) = regionBlc(i);
	trc(axis) = regionTrc(i);
    }
    const IPosition& boxShp = itsExtendBox.latticeShape();
    const IPosition& boxBlc = itsExtendBox.boundingBox().start();
    const IPosition& boxTrc = itsExtendBox.boundingBox().end();
    latShape(itsExtendAxis) = boxShp(0);
    blc(itsExtendAxis) = boxBlc(0);
    trc(itsExtendAxis) = boxTrc(0);
    setShapeAndBoundingBox (latShape, Slicer(blc, trc, Slicer::endIsLast));
}


void LCConcatenation::multiGetSlice (Array<Bool>& buffer,
				     const Slicer& section)
{
    buffer.resize (section.length());
    buffer = False;
    uInt i;
    // Construct a slicer for the regions axes only, since the concatenation
    // has one more axis.
    uInt nrr = itsRegionAxes.nelements();
    IPosition blc(nrr);
    IPosition len(nrr);
    IPosition inc(nrr);
    for (i=0; i<nrr; i++) {
        uInt axis = itsRegionAxes(i);
	blc(i) = section.start()(axis);
	len(i) = section.length()(axis);
	inc(i) = section.stride()(axis);
    }
    Slicer regSection(blc, len, inc);
    // Find the start, end and stride of the extendAxis to access.
    uInt extStart = section.start()(itsExtendAxis);
    uInt extEnd = section.end()(itsExtendAxis);
    uInt extInc = section.stride()(itsExtendAxis);
    IPosition stbuf(nrr);
    IPosition endbuf(nrr);
    IPosition streg(nrr);
    IPosition endreg(nrr);
    IPosition bufStart(nrr+1);
    IPosition bufEnd(nrr+1);
    IPosition tmpShape(nrr+1);
    uInt bufInx = 0;
    for (i=extStart; i<=extEnd; i+=extInc, bufInx++) {
        if (findAreas (stbuf, endbuf, streg, endreg, regSection, i)) {
	    Array<Bool> tmpbuf;
	    LCRegion* reg = (LCRegion*)(regions()[i]);
	    reg->doGetSlice (tmpbuf, Slicer(streg, endreg, inc,
					    Slicer::endIsLast));
	    // The buffer dimensionality is 1 more than the region's.
	    // So the extendAxis needs to be inserted into the IPositions.
	    for (uInt j=0; j<nrr; j++) {
		uInt axis = itsRegionAxes(j);
		bufStart(axis) = stbuf(j);
		bufEnd(axis) = endbuf(j);
		tmpShape(axis) = tmpbuf.shape()(j);
	    }
	    bufStart(itsExtendAxis) = bufInx;
	    bufEnd(itsExtendAxis) = bufInx;
	    tmpShape(itsExtendAxis) = 1;
	    Array<Bool> reformBuf (tmpbuf.reform (tmpShape));
	    Array<Bool> bufsect (buffer(bufStart,bufEnd));
	    DebugAssert (bufsect.shape() == reformBuf.shape(), AipsError);
	    bufsect = reformBuf;
	}
    }
}

IPosition LCConcatenation::doNiceCursorShape (uInt maxPixels) const
{
    return Lattice<Bool>::doNiceCursorShape (maxPixels);
}

} //# NAMESPACE CASACORE - END

