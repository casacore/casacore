//# LCStretch.cc: Stretch length 1 axes in an LCRegion along straight lines
//# Copyright (C) 2001
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


#include <casacore/lattices/LRegions/LCStretch.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LCStretch::LCStretch()
{}

LCStretch::LCStretch (const LCRegion& region,
		      const IPosition& stretchAxes,
		      const LCBox& stretchBox)
: LCRegionMulti (true, region.cloneRegion())
{
    // Fill the other members variables and determine the bounding box.
    fill (stretchAxes, stretchBox);
}

LCStretch::LCStretch (bool takeOver,
		      const LCRegion* region,
		      const IPosition& stretchAxes,
		      const LCBox& stretchBox)
: LCRegionMulti (takeOver, region)
{
    // Fill the other members variables and determine the bounding box.
    fill (stretchAxes, stretchBox);
}

LCStretch::LCStretch (const LCStretch& other)
: LCRegionMulti (other),
  itsStretchAxes (other.itsStretchAxes),
  itsStretchBox  (other.itsStretchBox)
{}

LCStretch::~LCStretch()
{}

LCStretch& LCStretch::operator= (const LCStretch& other)
{
    if (this != &other) {
	LCRegionMulti::operator= (other);
	itsStretchAxes.resize (other.itsStretchAxes.nelements());
	itsStretchAxes = other.itsStretchAxes;
	itsStretchBox  = other.itsStretchBox;
    }
    return *this;
}

bool LCStretch::operator== (const LCRegion& other) const
{
    // Check if parent class matches.
    // If so, we can safely cast.
    if (! LCRegionMulti::operator== (other)) {
	return false;
    }
    const LCStretch& that = (const LCStretch&)other;
    // Check the private data
    if (! itsStretchAxes.isEqual (that.itsStretchAxes)
    ||  !(itsStretchBox == that.itsStretchBox)) {
	return false;
    }
    return true;
}
 

LCRegion* LCStretch::cloneRegion() const
{
    return new LCStretch (*this);
}

LCRegion* LCStretch::doTranslate (const Vector<float>& translateVector,
				  const IPosition& newLatticeShape) const
{
    uint32_t i;
    // First translate the stretchBox.
    // Take appropriate elements from the vectors.
    uint32_t nre = itsStretchAxes.nelements();
    Vector<float> boxTransVec (nre);
    IPosition boxLatShape (nre);
    for (i=0; i<nre; i++) {
	uint32_t axis = itsStretchAxes(i);
	boxTransVec(i) = translateVector(axis);
	boxLatShape(i) = newLatticeShape(axis);
    }
    LCBox* boxPtr = dynamic_cast<LCBox*>(itsStretchBox.translate
                                               (boxTransVec, boxLatShape));
    // Now translate the region.
    LCRegion* regPtr = region().translate (translateVector, newLatticeShape);
    // Create the new LCStretch object.
    LCStretch* extPtr = new LCStretch (*regPtr, itsStretchAxes, *boxPtr);
    delete boxPtr;
    delete regPtr;
    return extPtr;
}

String LCStretch::className()
{
    return "LCStretch";
}

String LCStretch::type() const
{
   return className();
}

TableRecord LCStretch::toRecord (const String& tableName) const
{
    TableRecord rec;
    defineRecordFields (rec, className());
    rec.defineRecord ("region", region().toRecord (tableName));
    rec.define ("axes", itsStretchAxes.asVector());
    rec.defineRecord ("box", itsStretchBox.toRecord (tableName));
    return rec;
}

LCStretch* LCStretch::fromRecord (const TableRecord& rec,
				  const String& tableName)
{
    // Initialize pointers to 0 to get rid of gcc-2.95 warnings.
    LCRegion* regPtr = 0;
    regPtr = LCRegion::fromRecord (rec.asRecord("region"), tableName);
    LCBox* boxPtr = 0;
    boxPtr = (LCBox*)(LCRegion::fromRecord (rec.asRecord("box"), tableName));
    LCStretch* extPtr = new LCStretch (true, regPtr,
				       Vector<int32_t>(rec.toArrayInt ("axes")),
				       *boxPtr);
    delete boxPtr;
    return extPtr;
}

void LCStretch::fill (const IPosition& stretchAxes, const LCBox& stretchBox)
{
    // Check if stretch axes are specified correctly.
    // They do not need to be in ascending order, but duplicates are
    // not allowed. 
    IPosition regionShape = region().shape();
    uint32_t nrdim = regionShape.nelements();
    uint32_t nrs = stretchAxes.nelements();
    if (nrs == 0) {
	throw (AipsError ("LCStretch::LCStretch - "
			  "no stretch axes have been specified"));
    }
    if (nrs != stretchBox.blc().nelements()) {
	throw (AipsError ("LCStretch::LCStretch - "
			  "number of axes in stretch box mismatches "
			  "number of stretch axes"));
    }
    itsStretchAxes.resize (nrs);
    IPosition boxLatShape(nrs);
    Vector<float> boxLatBlc(nrs);
    Vector<float> boxLatTrc(nrs);
    Vector<uint32_t> reginx(nrs);
    GenSortIndirect<ssize_t,uint32_t>::sort (reginx, stretchAxes.storage(), nrs);
    int32_t first = -1;
    for (uint32_t i=0; i<nrs; i++) {
        uint32_t axis = reginx(i);
	itsStretchAxes(i) = stretchAxes(axis);
	boxLatShape(i) = stretchBox.latticeShape()(axis);
	boxLatBlc(i) = stretchBox.blc()(axis);
	boxLatTrc(i) = stretchBox.trc()(axis);
	if (itsStretchAxes(i) <= first  ||  itsStretchAxes(i) >= int32_t(nrdim)) {
	    throw (AipsError ("LCStretch::LCStretch - "
			      "stretch axes multiply specified "
			      "or exceed nrdim"));
	}
	first = itsStretchAxes(i);
	if (regionShape(itsStretchAxes(i)) != 1) {
	    throw (AipsError ("LCStretch::LCStretch - "
			      "a stretch axis does not have length 1"));
	}
    }
    itsStretchBox = LCBox (boxLatBlc, boxLatTrc, boxLatShape);
    // Make up the lattice shape from the region and box latticeshape.
    // Fill the bounding box from blc/trc in region and box.
    IPosition latShape = region().latticeShape();
    IPosition blc = region().boundingBox().start();
    IPosition trc = region().boundingBox().end();
    const IPosition& boxShp = itsStretchBox.latticeShape();
    const IPosition& boxBlc = itsStretchBox.boundingBox().start();
    const IPosition& boxTrc = itsStretchBox.boundingBox().end();
    for (uint32_t i=0; i<nrs; i++) {
        uint32_t axis = itsStretchAxes(i);
	latShape(axis) = boxShp(i);
	blc(axis) = boxBlc(i);
	trc(axis) = boxTrc(i);
    }
    setShapeAndBoundingBox (latShape, Slicer(blc, trc, Slicer::endIsLast));
    fillHasMask();
}


void LCStretch::multiGetSlice (Array<bool>& buffer,
			       const Slicer& section)
{
    buffer.resize (section.length());
    // Read the required region section.
    // This means we have to create a Slicer with length 1 for stretched axes.
    IPosition blc(section.start());
    IPosition len(section.length());
    IPosition inc(section.stride());
    uint32_t nrs = itsStretchAxes.nelements();
    for (uint32_t i=0; i<nrs; i++) {
        uint32_t axis = itsStretchAxes(i);
	blc(axis) = 0;
	len(axis) = 1;
	inc(axis) = 1;
    }
    Array<bool> tmpbuf(len);
    LCRegion* reg = (LCRegion*)(regions()[0]);
    reg->doGetSlice (tmpbuf, Slicer(blc, len, inc));
    // Now we have to stretch tmpbuf along all stretch axes.
    const IPosition& length = section.length();
    IPosition pos (buffer.ndim(), 0);
    IPosition end (buffer.shape() - 1);
    //# Iterate along itsStretchAxes through the new mask.
    for (;;) {
	for (uint32_t i=0; i<nrs; i++) {
	    end(itsStretchAxes(i)) = pos(itsStretchAxes(i));
	}
	//# Set each section of the mask to the mask of the region.
	buffer(pos,end) = tmpbuf;
	//# Go to the next section.
	uint32_t dim;
	for (dim=0; dim<nrs; dim++) {
	    if (++pos(itsStretchAxes(dim)) < length(itsStretchAxes(dim))) {
		break;
	    }
	    // This dimension is done. Reset it and continue with the next.
	    pos(itsStretchAxes(dim)) = 0;
        }
	//# End the iteration when all dimensions are done.
	if (dim == nrs) {
	    break;
	}
    }
}

IPosition LCStretch::doNiceCursorShape (uint32_t maxPixels) const
{
    return Lattice<bool>::doNiceCursorShape (maxPixels);
}

} //# NAMESPACE CASACORE - END

