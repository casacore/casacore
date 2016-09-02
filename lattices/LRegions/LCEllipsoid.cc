//# LCEllipsoid.cc: Define an N-dimensional ellipsoidal region of interest
//# Copyright (C) 1997,1998,1999,2001,2003
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

#include <casacore/lattices/LRegions/LCEllipsoid.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

LCEllipsoid::LCEllipsoid() : _theta(0)
{}

LCEllipsoid::LCEllipsoid (const IPosition& center, Float radius,
        const IPosition& latticeShape)
: LCRegionFixed (latticeShape),
  itsRadii      (latticeShape.nelements(), radius),
  _theta        (0)
{
    fillCenter (center);
    setBoundingBox (makeBox(itsRadii, latticeShape));
    defineMask();
}

LCEllipsoid::LCEllipsoid (const Vector<Float>& center, Float radius,
        const IPosition& latticeShape)
: LCRegionFixed (latticeShape), itsCenter(center.copy()),
  itsRadii      (latticeShape.nelements(), radius),
  _theta        (0)
{
    setBoundingBox(makeBox(itsRadii, latticeShape));
    defineMask();
}

LCEllipsoid::LCEllipsoid(const Vector<Double>& center, Double radius,
        const IPosition& latticeShape)
: LCRegionFixed (latticeShape),
  itsCenter     (center.size()),
  itsRadii      (center.size(), radius),
  _theta        (0)
{
    for (uInt i=0; i<center.size(); ++i) {
        itsCenter[i] = center[i];
    }
    setBoundingBox(makeBox(itsRadii, latticeShape));
    defineMask();
}

LCEllipsoid::LCEllipsoid(const Vector<Float>& center, const Vector<Float>& radii,
        const IPosition& latticeShape)
: LCRegionFixed (latticeShape),
  itsCenter     (center.copy()),
  itsRadii      (radii.copy()),
  _theta(0)
{
    setBoundingBox(makeBox(itsRadii, latticeShape));
    defineMask();
}

LCEllipsoid::LCEllipsoid(const Vector<Double>& center,
        const Vector<Double>& radii,
        const IPosition& latticeShape)
: LCRegionFixed (latticeShape),
  itsCenter     (center.size()),
  itsRadii      (radii.size()),
  _theta        (0)
{
    for (uInt i=0; i<center.size(); ++i) {
        itsCenter[i] = center[i];
        if (i < radii.size()) {
            itsRadii[i] = radii[i];
        }
    }
    setBoundingBox(makeBox(itsRadii, latticeShape));
    defineMask();
}

LCEllipsoid::LCEllipsoid (
    const Float xcenter, const Float ycenter,
    const Float majorAxis, const Float minorAxis,
    const Float theta, const IPosition& latticeShape
) : LCRegionFixed (latticeShape), _theta(fmod(theta, Float(C::pi))) {
    itsCenter.resize(2);
    itsCenter[0] = xcenter;
    itsCenter[1] = ycenter;
    itsRadii.resize(2);
    if (_theta < 0) {
        _theta += C::pi;
    }
    if (near(_theta, C::pi/2)) {
        itsRadii[0] = minorAxis;
        itsRadii[1] = majorAxis;
        _theta = 0;
    }
    else {
        itsRadii[0] = majorAxis;
        itsRadii[1] = minorAxis;
    }
    if (near(_theta, Float(0))) {
        setBoundingBox(makeBox(itsRadii, latticeShape));
        defineMask();
    }
    else {
        // overkill but the general way to find the minimal
        // box eludes me atm.
        Vector<Float> proj(itsRadii.size(), max(itsRadii));
        setBoundingBox(makeBox(proj, latticeShape));
        _defineMask2D();
    }
}

LCEllipsoid::LCEllipsoid (const LCEllipsoid& other)
: LCRegionFixed(other), itsCenter(other.itsCenter),
  itsRadii(other.itsRadii), _epsilon(other._epsilon),
  _theta(other._theta), _centerIsInside(other._centerIsInside)
{}

LCEllipsoid::~LCEllipsoid()
{}

LCEllipsoid& LCEllipsoid::operator= (const LCEllipsoid& other)
{
    if (this != &other) {
        LCRegionFixed::operator= (other);
        itsCenter.resize (other.itsCenter.nelements());
        itsRadii.resize  (other.itsCenter.nelements());
        itsCenter = other.itsCenter;
        itsRadii  = other.itsRadii;
        _epsilon = other._epsilon;
        _theta = other._theta;
        _centerIsInside = other._centerIsInside;
    }
    return *this;
}

Bool LCEllipsoid::operator== (const LCRegion& other) const
{
    // Check if parent class matches.
    // If so, we can safely cast.
    if (! LCRegionFixed::operator== (other)) {
        return False;
    }
    const LCEllipsoid& that = (const LCEllipsoid&)other;
    // Compare private data.
    if (
        itsCenter.nelements() != that.itsCenter.nelements()
        ||  itsRadii.nelements()  != that.itsRadii.nelements()
    ) {
        return False;
    }
    for (uInt i=0; i<itsCenter.nelements(); ++i) {
        if (
            !near (itsCenter(i), that.itsCenter(i))
            ||  !near (itsRadii(i),  that.itsRadii(i))
        ) {
            return False;
        }
        if (itsRadii.size() == 2 && ! near(_theta, that._theta)) {
            return False;
        }
    }
    return True;
}

LCRegion* LCEllipsoid::cloneRegion() const
{
    return new LCEllipsoid(*this);
}

LCRegion* LCEllipsoid::doTranslate (const Vector<Float>& translateVector,
    const IPosition& newLatticeShape) const
{
    uInt ndim = latticeShape().nelements();
    Vector<Float> center;
    center = itsCenter;
    for (uInt i=0; i<ndim; ++i) {
        center[i] += translateVector[i];
    }
    if (itsCenter.size() != 2 || _theta == 0) {
        return new LCEllipsoid (center, itsRadii, newLatticeShape);
    }
    else {
        // 2-D ellipse with axis not coincident with x-axis
        return new LCEllipsoid (
            center[0], center[1], itsRadii[0], itsRadii[1],
            _theta, newLatticeShape
        );
    }
}

String LCEllipsoid::className()
{
    return "LCEllipsoid";
}

String LCEllipsoid::type() const
{
    return className();
}

TableRecord LCEllipsoid::toRecord (const String&) const
{
    TableRecord rec;
    defineRecordFields(rec, className());
    // Write 1-relative.
    rec.define ("oneRel", True);
    rec.define ("center", itsCenter + Float(1));
    rec.define ("radii", itsRadii);
    rec.define ("shape", latticeShape().asVector());
    if (itsRadii.size() == 2) {
        rec.define ("theta", _theta);
    }
    return rec;
}

LCEllipsoid* LCEllipsoid::fromRecord(const TableRecord& rec,
        const String&)
{
    // If 1-relative, subtract 1 from center.
    Bool oneRel = rec.asBool ("oneRel");
    Float off = (oneRel ? 1:0);
    Array<Float> center (rec.toArrayFloat ("center"));
    if (center.size() != 2 || ! rec.isDefined("theta")) {
        return new LCEllipsoid (center-off,
            Vector<Float>(rec.toArrayFloat ("radii")),
            Vector<Int>(rec.toArrayInt ("shape"))
        );
    }
    else {
        Vector<Float> radii (rec.toArrayFloat ("radii"));
        return new LCEllipsoid(
            center(IPosition(1,0))-off, center(IPosition(1,1))-off,
            radii[0], radii[1], rec.asFloat("theta"),
            Vector<Int>(rec.toArrayInt ("shape"))
        );
    }
}

void LCEllipsoid::fillCenter(const IPosition& center)
{
    itsCenter.resize (center.nelements());
    for (uInt i=0; i<center.nelements(); ++i) {
        itsCenter(i) = center(i);
    }
}

Slicer LCEllipsoid::makeBox(
    const Vector<Float>& radii, const IPosition& latticeShape
) {
    uInt nrdim = itsCenter.size();
    // First make sure dimensionalities conform.
    if (latticeShape.size() != nrdim  ||  radii.size() != nrdim) {
        ThrowCc("dimensionality of center,radii,lattice mismatch");
    }
    // Determine blc and trc.
    IPosition blc(nrdim);
    IPosition trc(nrdim);
    _epsilon.resize(nrdim);
    _centerIsInside = True;
    for (uInt i=0; i<nrdim; ++i) {
        if (itsCenter[i] > latticeShape[i]-1  ||  itsCenter[i] < 0) {
            _centerIsInside = False;
            ThrowIf(
                itsCenter[i] + radii[i] < 0
                || itsCenter[i] - radii[i] > latticeShape[i] - 1,
                "Ellipsoid lies completely outside the lattice"
            );
        }
        _epsilon[i] = powf(10.0, int(log10(2*radii[i]))-5);
        blc[i] = max(Int(itsCenter[i] - radii[i] + 1 - _epsilon[i]), 0);
        trc[i] = min(Int(itsCenter[i] + radii[i] + _epsilon[i]), latticeShape[i] - 1);
        if (blc[i] > trc[i]) {
            ostringstream rstr;
            rstr << radii;
            ThrowCc(
                "ellipsoid is empty (radii " + rstr.str()
                + " too small)"
            );
        }
    }
    return Slicer(blc, trc, Slicer::endIsLast);
}

const Float& LCEllipsoid::theta() const {
    ThrowIf(
        itsRadii.size() != 2,
        "Angle can only be gotten for 2-D ellipses"
    );
    return _theta;
}

void LCEllipsoid::defineMask() {
    if (! _centerIsInside) {
        _doOutside();
        return;
    }
    uInt i;
    // Create the mask with the shape of the bounding box.
    // Set the mask initially to False.
    const IPosition& length = boundingBox().length();
    uInt nrdim = length.nelements();
    Array<Bool> mask(length);
    mask = False;
    // Get access to the mask storage.
    Bool deleteIt;
    Bool* maskData = mask.getStorage (deleteIt);
    // Initialize some variables for the loop below.
    Float center0 = itsCenter[0] - boundingBox().start()[0];
    Float radsq0 = itsRadii[0] * itsRadii[0];
    Int np = length(0);
    IPosition pos (nrdim, 0);
    Vector<Float> center (nrdim);
    Vector<Float> radsq (nrdim);
    Vector<Float> dist (nrdim, 0.0);
    Float distsq = 0;
    for (i=1; i<nrdim; ++i) {
        center[i] = itsCenter[i] - boundingBox().start()[i];
        Float d = max (float(0), center[i]) / itsRadii[i];
        dist[i] = d * d;
        distsq += dist[i];
    }
    // Loop through all pixels in the ellipsoid.
    // The outer loop iterates over all lines.
    // The inner loop sets the pixel mask for a line by calculating
    // the start and end of the ellipsoid for that line.
    // The variable distsq contains the 'distance' of the line to the center.
    i = 1;
    for (;;) {
        // Ignore the line when the distance exceeds the radius.
        Float d = 1 - distsq;
        if (d >= 0) {
            d = sqrt(d * radsq0);
            d += _epsilon[0];
            Int start = max(Int(center0 - d + 1 - _epsilon[i]), 0);
            Int end = min(Int(center0 + d + _epsilon[i]), np-1);
            for (Int j=start; j<=end; j++) {
                maskData[j] = True;
            }
        }
        // Go to the next line and update the line distance.
        maskData += np;
        for (i=1; i<nrdim; ++i) {
            distsq -= dist[i];
            if (++pos[i] < length[i]) {
                Float d = abs(center[i] - pos[i]);
                d = max(float(0), d) / itsRadii[i];
                dist[i] = d*d;
                distsq += dist[i];
                break;
            }
            // This dimension is done. Reset it and continue with the next.
            pos[i] = 0;
            Float d = max (float(0), center[i]) / itsRadii[i];
            dist(i) = d*d;
            distsq += dist[i];
        }
        // End the iteration when all dimensions are done.
        if (i == nrdim) {
            break;
        }
        DebugAssert (maskData == &mask(pos), AipsError);
    }
    mask.putStorage (maskData, deleteIt);
    setMask (mask);
}

void LCEllipsoid::_defineMask2D() {
    // Create the mask with the shape of the bounding box.
    // Set the mask initially to False.
    const IPosition& length = boundingBox().length();
    uInt ndim = length.size();
    AlwaysAssert(ndim == 2, AipsError);
    Array<Bool> mask(length);
    mask = False;
    // Get access to the mask storage.
    Bool deleteIt;
    Bool* maskData = mask.getStorage (deleteIt);
    Vector<Float> center(ndim);
    Vector<Float> rad2(ndim);
    for (uInt i=0; i<ndim; ++i) {
        center[i] = itsCenter[i] - Float(boundingBox().start()[i]);
        rad2[i] = itsRadii[i]*itsRadii[i];
    }
    // Initialize some variables for the loop below.
    Float prevSum = 0;
    for (Int y=0; y<length[1]; ++y) {
        Float ydiff = Float(y-center[1]);
        for (Int x=0; x<length[0]; ++x) {
            Float xdiff = Float(x-center[0]);
            Float xp = xdiff*cos(-_theta) - ydiff*sin(-_theta);
            Float yp = xdiff*sin(-_theta) + ydiff*cos(-_theta);
            Float sum = xp*xp/rad2[0] + yp*yp/rad2[1];
            if (sum <= 1) {
                maskData[x] = True;
            }
            else if (x != 0 && sum > prevSum) {
                break;
            }
            prevSum = sum;
        }
        maskData += length[0];
    }
    mask.putStorage (maskData, deleteIt);
    ThrowIf(
        ! _centerIsInside && ! casacore::anyTrue(mask),
        "Ellipsoid lies entirely outside the lattice"
    );
    setMask (mask);
}

void LCEllipsoid::_doOutside() {
    // Create the mask with the shape of the bounding box.
    // Set the mask initially to False.
    const IPosition& length = boundingBox().length();
    Float center0 = itsCenter[0] - boundingBox().start()[0];
    uInt ndim = length.size();
    Array<Bool> mask(length);
    Int np = length[0];
    mask = False;
    // Get access to the mask storage.
    Bool deleteIt;
    Bool* maskData = mask.getStorage (deleteIt);
    Vector<Float> center(ndim);
    Vector<Float> rad2 = itsRadii * itsRadii;
    IPosition pos(ndim, 0);
    Vector<Float> d2(ndim);
    Float curD2 = 0;
    for (uInt i=1; i<ndim; ++i) {
        center[i] = itsCenter[i] - Float(boundingBox().start()[i]);
        d2[i] = center[i]*center[i]/rad2[i];
        // sumsq of all components except the first
        curD2 += d2[i];
    }
    uInt i = 1;
    while (True) {
        if (1 - curD2 >= 0) {
            // x**2/rad2[0] = 1 - curD2
            Float maxXDiff = itsRadii[0] * sqrt(1 - curD2);
            Int start = max(Int(center0 - maxXDiff + 1 - _epsilon[0]), 0);
            Int end = min(Int(center0 + maxXDiff + _epsilon[0]), np-1);
            for (Int j=start; j<=end; ++j) {
                maskData[j] = True;
            }
        }
        maskData += np;
        for (i=1; i<ndim; ++i) {
            curD2 -= d2[i];
            if (++pos[i] < length[i]) {
                Float d = center[i] - pos[i];
                d /= itsRadii[i];
                d2[i] = d*d;
                curD2 += d2[i];
                break;
            }
            // This dimension is done. Reset it and continue with the next.
            pos[i] = 0;
            Float d = center[i]/itsRadii[i];
            d2[i] = d*d;
            curD2 += d2[i];
        }
        // End the iteration when all dimensions are done.
        if (i == ndim) {
            break;
        }
    }
    mask.putStorage (maskData, deleteIt);
    ThrowIf(
        ! _centerIsInside && ! casacore::anyTrue(mask),
        "Ellipsoid lies entirely outside the lattice"
    );
    setMask(mask);
}

} //# NAMESPACE CASACORE - END

