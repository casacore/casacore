//# LCPolygon.cc: Define a 2-dimensional region by a polygon
//# Copyright (C) 1998,1999,2000,2001
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


#include <casacore/lattices/LRegions/LCPolygon.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

LCPolygon::LCPolygon()
{}

LCPolygon::LCPolygon (const Vector<float>& x, const Vector<float>& y,
		      const IPosition& latticeShape)
: LCRegionFixed (latticeShape),
  itsX (x.copy()),
  itsY (y.copy())
{
    defineBox();
    defineMask();
}

LCPolygon::LCPolygon (const Vector<double>& x, const Vector<double>& y,
		      const IPosition& latticeShape)
: LCRegionFixed (latticeShape),
  itsX (x.nelements()),
  itsY (y.nelements())
{
    for (uint32_t i=0; i<x.nelements(); i++) {
	itsX[i] = x[i];
	if (i < y.nelements()) {
           // define box will catch unequal lengths x and y
           itsY[i] = y[i];  
        }
                                                
    }
    defineBox();
    defineMask();
}

LCPolygon::LCPolygon (const LCPolygon& other)
: LCRegionFixed (other),
  itsX (other.itsX),
  itsY (other.itsY)
{}

LCPolygon::~LCPolygon()
{}

LCPolygon& LCPolygon::operator= (const LCPolygon& other)
{
    if (this != &other) {
	LCRegionFixed::operator= (other);
	itsX.resize (other.itsX.nelements());
	itsY.resize (other.itsX.nelements());
	itsX = other.itsX;
	itsY = other.itsY;
    }
    return *this;
}


bool LCPolygon::operator== (const LCRegion& other) const
{
    // Check if parent class matches.
    // If so, we can safely cast.
    if (! LCRegionFixed::operator== (other)) {
	return false;
    }
    const LCPolygon& that = (const LCPolygon&)other;
    // Compare private data.
    if (itsX.nelements() != that.itsX.nelements()
    ||  itsY.nelements() != that.itsY.nelements()) {
	return false;
    }
    bool deleteX1, deleteY1;
    bool deleteX2, deleteY2;
    const float* pX1 = itsX.getStorage (deleteX1);
    const float* pY1 = itsY.getStorage (deleteY1);
    const float* pX2 = that.itsX.getStorage (deleteX2);
    const float* pY2 = that.itsY.getStorage (deleteY2);
    bool result = true;
    for (uint32_t i=0; i<itsX.nelements(); i++) {
	if (!_isNear (pX1[i], pX2[i])
	||  !_isNear (pY1[i], pY2[i])) {
	    result = false;
	    break;
	}
    }
    itsX.freeStorage (pX1, deleteX1);
    itsY.freeStorage (pY1, deleteY1);
    that.itsX.freeStorage (pX2, deleteX2);
    that.itsY.freeStorage (pY2, deleteY2);
    return result;
}


LCRegion* LCPolygon::cloneRegion() const
{
    return new LCPolygon(*this);
}

LCRegion* LCPolygon::doTranslate (const Vector<float>& translateVector,
				  const IPosition& newLatticeShape) const
{
    Vector<float> x, y;
    x = itsX;
    y = itsY;
    uint32_t n = x.nelements();
    for (uint32_t i=0; i<n; i++) {
        x[i] += translateVector[0];
        y[i] += translateVector[1];
    }
    return new LCPolygon (x, y, newLatticeShape);
}

String LCPolygon::className()
{
    return "LCPolygon";
}

String LCPolygon::type() const
{
   return className();
}


TableRecord LCPolygon::toRecord (const String&) const
{
    TableRecord rec;
    defineRecordFields (rec, className());
    // Write 1-relative.
    rec.define ("oneRel", true);
    rec.define ("x", itsX + float(1));
    rec.define ("y", itsY + float(1));
    rec.define ("shape", latticeShape().asVector());
    return rec;
}

LCPolygon* LCPolygon::fromRecord (const TableRecord& rec,
				  const String&)
{
    // If 1-relative, subtract 1 from x and y.
    bool oneRel = rec.asBool ("oneRel");
    float off = (oneRel ? 1:0);
    Array<float> x (rec.toArrayFloat ("x"));
    Array<float> y (rec.toArrayFloat ("y"));
    return new LCPolygon (x-off, y-off,
                          Vector<int32_t>(rec.toArrayInt ("shape")));
}

// Truncate start such that edge is taken if very close to a pixel point.
// Take care of fact that float is not always exact.
int32_t LCPolygon::truncateStart (float v)
{
  int32_t res;
  float vt = floor(v+0.1);
  if (_isNear(vt, v))  {
    res = static_cast<int32_t>(v+0.1);
  } else {
    res = static_cast<int32_t>(v+1);
  }
  return std::max (res, 0);
}

bool LCPolygon::_isNear(float val1, float val2) {
    return val1 == 0 || val2 == 0 ? nearAbs(val1, val2) : near(val1, val2);
}

// Truncate end such that edge is taken if very close to a pixel point.
int32_t LCPolygon::truncateEnd (float v, int32_t maxEnd)
{
  int32_t res;
  float vt = floor(v+0.1);
  if (_isNear(vt, v))  {
    res = static_cast<int32_t>(v+0.1);
  } else {
    res = static_cast<int32_t>(v);
  }
  return std::min (res, maxEnd);
}

void LCPolygon::defineBox()
{
    const IPosition& shape = latticeShape();
    uint32_t i;
    uint32_t nrp = itsX.nelements();
    // First make sure basic things are right.
    if (itsY.nelements() != nrp) {
	throw AipsError ("LCPolygon - x and y vectors must have equal length");
    }
    if (shape.nelements() != 2) {
	throw AipsError ("LCPolygon - can only be used as a 2-dim region");
    }
    // If the last point is not equal to the first one, add it.
    if (!_isNear (itsX[nrp-1], itsX[0])  ||  !_isNear(itsY[nrp-1], itsY[0])) {
	itsX.resize (nrp+1, true);
	itsY.resize (nrp+1, true);
	nrp++;
    }
    itsX[nrp-1] = itsX[0];        // Make sure they are always equal.
    itsY[nrp-1] = itsY[0];
    if (nrp < 3) {
	throw AipsError ("LCPolygon - "
			  "at least 3 different points have to be specified");
    }
    // Determine the maximum and minimum x,y.
    // They form the bounding box.
    // Check if at least one point is inside lattice.
    float minx = itsX[0];
    float maxx = itsX[0];
    float miny = itsY[0];
    float maxy = itsY[0];
    for (i=1; i<nrp; i++) {
	if (itsX[i] < minx) minx = itsX[i];
	if (itsX[i] > maxx) maxx = itsX[i];
	if (itsY[i] < miny) miny = itsY[i];
	if (itsY[i] > maxy) maxy = itsY[i];
    }
    // Get boundingbox; truncate values in the right way.
    IPosition blc(2, 0);
    IPosition trc(shape-1);
    blc[0] = truncateStart(minx);
    blc[1] = truncateStart(miny);
    trc[0] = truncateEnd(maxx, shape[0]-1);
    trc[1] = truncateEnd(maxy, shape[1]-1);
    if (trc[0] < blc[0]  ||  trc[1] < blc[1]) {
      throw AipsError ("LCPolygon - entire polygon is outside the lattice");
    }
    setBoundingBox (Slicer(blc, trc, Slicer::endIsLast));
}

void LCPolygon::defineMask()
{
    // Create and initialize the mask.
    IPosition blc = boundingBox().start();
    const IPosition& shape = boundingBox().length();
    Matrix<bool> mask(shape);
    mask = false;
    uint32_t nrline = itsX.nelements() - 1;
    bool delX, delY, delM;
    const float* ptrX = itsX.getStorage (delX);
    const float* ptrY = itsY.getStorage (delY);
    bool* ptrM = mask.getStorage (delM);
    // Fill the mask.
    // Note that fillMask is now called such that the outer loop is over y.
    // This is usually most efficient; however if ny>>nx it might be
    // more efficient to call fillMask with x and y swapped. That also
    // means that the mask should be transposed after fillMask.
    fillMask (ptrM, shape[1], shape[0], blc[1], blc[0], ptrY, ptrX, nrline);
    itsX.freeStorage (ptrX, delX);
    itsY.freeStorage (ptrY, delY);
    mask.putStorage (ptrM, delM);
    // Test if rows/columns at the edges are all false.
    // If so, remove them and adjust the bounding box.
    int32_t stx = 0;
    int32_t sty = 0;
    int32_t endx = shape[0];
    int32_t endy = shape[1];
    for (; stx<endx && allEQ(mask.row(stx), false); ++stx) {}
    for (; endx-1>stx && allEQ(mask.row(endx-1), false); --endx) {}
    for (; sty<endy && allEQ(mask.column(sty), false); ++sty) {}
    for (; endy-1>sty && allEQ(mask.column(endy-1), false); --endy) {}
    if (stx>0 || sty>0 || endx<shape[0] || endy<shape[1]) {
      if (stx >= endx  ||  sty >= endy) {
        throw AipsError ("LCPolygon - polygon does not contain any pixel");
      }
      Matrix<bool> mask2;
      mask2 = mask(Slice(stx,endx-stx), Slice(sty,endy-sty));
      mask.reference (mask2);
      blc[0] += stx;
      blc[1] += sty;
      setBoundingBox (Slicer(blc, mask.shape()));
    }
    setMask (mask);
}

void LCPolygon::fillMask (bool* mask, int32_t ny, int32_t nx,
			  int32_t blcy, int32_t blcx,
			  const float* ptrY, const float* ptrX, uint32_t nrline)
{
  uint32_t i;
  Block<float> a(nrline);
  Block<float> b(nrline);
  Block<int32_t> dir(nrline, -1);     // -1=same dir, 0=vertical, 1=change dir
  // Fill the mask for all vertical lines.
  // Also determine the index of the last non-vertical line, which
  // is used in the slope-calculation loop.
  int32_t prev = -1;
  for (i=0; i<nrline; i++) {
    if (_isNear (ptrY[i], ptrY[i+1])) {
      dir[i] = 0;                         // vertical line
      // Fill vertical line if on pixel.
      int32_t y = static_cast<int32_t>(ptrY[i]);
      if (y >= blcy  &&  y < ny+blcy  &&  _isNear(float(y), ptrY[i])) {
        int32_t xs, xe;
        if (ptrX[i] < ptrX[i+1]) {
          xs = truncateStart (ptrX[i] - blcx);
          xe = truncateEnd (ptrX[i+1] - blcx, nx-1);
        } else {
          xs = truncateStart (ptrX[i+1] - blcx);
          xe = truncateEnd (ptrX[i] - blcx, nx-1);
        }
	bool* maskPtr = mask + (y-blcy)*nx;
	while (xs <= xe) {
	    maskPtr[xs++] = true;
	}
      }
    } else {
      prev = i;
    }
  }
  // Calculate the slope (a) and offset (b) for all line segments.
  // Vertical line segments are ignored for this.
  // Determine if a line segment changes direction with respect to
  // the last non-vertical line segment.
  for (i=0; i<nrline; i++) {
    if (dir[i] != 0) {
      a[i] = (ptrX[i] - ptrX[i+1]) / (ptrY[i] - ptrY[i+1]);
      b[i] = ptrX[i] - a[i] * ptrY[i];
      if ((ptrY[i] > ptrY[i+1]  &&  ptrY[prev] < ptrY[prev+1])
      ||  (ptrY[i] < ptrY[i+1]  &&  ptrY[prev] > ptrY[prev+1])) {
        dir[i] = 1;
      }
      prev = i;
    }
  }
  // Loop through all y-es and mask the x-points inside or on the polygon.
  // This is done by determining the crossing point of all the y-lines
  // with all line segments (the in/out algorithm).
  Block<float> cross(nrline);
  bool* maskPtr = mask;
  for (int32_t y=0; y<ny; y++) {
    uint32_t nrcross = 0;
    float yf = y + blcy;
    for (i=0; i<nrline; i++) {
      // Ignore vertical lines.
      if (dir[i] != 0) {
        bool take = false;
        // Calculate the crossing point if yf is inside the line segment.
        if ((yf > ptrY[i]  &&  yf < ptrY[i+1])
        ||  (yf < ptrY[i]  &&  yf > ptrY[i+1])
        ||  _isNear(yf, ptrY[i])  ||  _isNear(yf, ptrY[i+1])) {
          float cr = a[i] * yf + b[i] - blcx;
          take = true;
          // If a polygon point is a pixel point (in y), always
          // count the ending point of the line segment.
          // Do not count the starting point if the direction has
          // not changed with respect to the previous non-vertical
          // line segment.
          if (_isNear (yf, ptrY[i])) {
            if (dir[i] != 1) {
              take = false;
            }
          }
          if (take) {
            cross[nrcross++] = cr;
          }
        }
      }
    }
    DebugAssert (nrcross >= 2, AipsError);
    DebugAssert (nrcross%2 == 0, AipsError);
    GenSort<float>::sort (cross, nrcross);
    for (i=0; i<nrcross; i+=2) {
      int32_t xs = truncateStart (cross[i]);
      int32_t xe = truncateEnd (cross[i+1], nx-1);
      while (xs <= xe) {
        maskPtr[xs++] = true;
      }
    }
    maskPtr += nx;
  }
}

} //# NAMESPACE CASACORE - END

