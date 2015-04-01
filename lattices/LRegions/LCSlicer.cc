//# LCSlicer.cc: Class to define a rectangular box of interest with strides
//# Copyright (C) 1998,1999,2000,2001,2002,2003
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

#include <casacore/lattices/LRegions/LCSlicer.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LCSlicer::LCSlicer()
: itsIsFractional  (False),
  itsIsAbsolute    (False),
  itsIsUnspecified (True),
  itsIsStrided     (False)
{}

LCSlicer::LCSlicer (const Vector<Float>& blc, const Vector<Float>& trc,
		    Bool fractional,
		    RegionType::AbsRelType absRel)
: itsBlc (blc.copy()),
  itsTrc (trc.copy()),
  itsInc (blc.nelements())
{
    itsInc = 1;
    fillFlags (fractional, absRel, blc.nelements(), trc.nelements(),
	       itsInc.nelements());
    fill();
}

LCSlicer::LCSlicer (const Vector<Float>& blc, const Vector<Float>& trc,
		    const Vector<Float>& inc, Bool fractional,
		    RegionType::AbsRelType absRel)
: itsBlc (blc.copy()),
  itsTrc (trc.copy()),
  itsInc (inc.copy())
{
    fillFlags (fractional, absRel, blc.nelements(), trc.nelements(),
	       inc.nelements());
    fill();
}

LCSlicer::LCSlicer (const Vector<Float>& blc, const Vector<Float>& trc,
		    const Vector<Float>& inc,
		    const Vector<Bool>& fractionalBlc,
		    const Vector<Bool>& fractionalTrc,
		    const Vector<Bool>& fractionalInc,
		    const Vector<Int>& absRelBlc,
		    const Vector<Int>& absRelTrc)
: itsBlc (blc.copy()),
  itsTrc (trc.copy()),
  itsInc (inc.copy()),
  itsFracBlc   (fractionalBlc.copy()),
  itsFracTrc   (fractionalTrc.copy()),
  itsFracInc   (fractionalInc.copy()),
  itsAbsRelBlc (absRelBlc.copy()),
  itsAbsRelTrc (absRelTrc.copy())
{
    fill();
}


LCSlicer::LCSlicer (const Vector<Double>& blc, const Vector<Double>& trc,
		    Bool fractional,
		    RegionType::AbsRelType absRel)
{
    Vector<Double> inc(blc.nelements());
    inc = 1;
    fillFlags (fractional, absRel, blc.nelements(), trc.nelements(),
	       inc.nelements());
    fillFromDouble (blc, trc, inc);
}

LCSlicer::LCSlicer (const Vector<Double>& blc, const Vector<Double>& trc,
		    const Vector<Double>& inc, Bool fractional,
		    RegionType::AbsRelType absRel)
{
    fillFlags (fractional, absRel, blc.nelements(), trc.nelements(),
	       inc.nelements());
    fillFromDouble (blc, trc, inc);
}

LCSlicer::LCSlicer (const Vector<Double>& blc, const Vector<Double>& trc,
		    const Vector<Double>& inc,
		    const Vector<Bool>& fractionalBlc,
		    const Vector<Bool>& fractionalTrc,
		    const Vector<Bool>& fractionalInc,
		    const Vector<Int>& absRelBlc,
		    const Vector<Int>& absRelTrc)
: itsFracBlc   (fractionalBlc.copy()),
  itsFracTrc   (fractionalTrc.copy()),
  itsFracInc   (fractionalInc.copy()),
  itsAbsRelBlc (absRelBlc.copy()),
  itsAbsRelTrc (absRelTrc.copy())
{
    fillFromDouble (blc, trc, inc);
}


LCSlicer::LCSlicer (const Slicer& slicer)
{
    uInt ndim = slicer.ndim();
    fillFlags (False, False, ndim, ndim, ndim);
    fillFromIPosition (slicer.start(), slicer.end(), slicer.stride());
}

LCSlicer::LCSlicer (const IPosition& blc, const IPosition& trc,
		    RegionType::AbsRelType absRel)
{
    IPosition inc(blc.nelements());
    inc = 1;
    fillFlags (False, absRel, blc.nelements(), trc.nelements(),
	       inc.nelements());
    fillFromIPosition (blc, trc, inc);
}

LCSlicer::LCSlicer (const IPosition& blc, const IPosition& trc,
		    const IPosition& inc,
		    RegionType::AbsRelType absRel)
{
    fillFlags (False, absRel, blc.nelements(), trc.nelements(),
	       inc.nelements());
    fillFromIPosition (blc, trc, inc);
}

LCSlicer::LCSlicer (const IPosition& blc, const IPosition& trc,
		    const IPosition& inc,
		    const Vector<Int>& absRelBlc,
		    const Vector<Int>& absRelTrc)
: itsFracBlc   (blc.nelements()),
  itsFracTrc   (trc.nelements()),
  itsFracInc   (inc.nelements()),
  itsAbsRelBlc (absRelBlc.copy()),
  itsAbsRelTrc (absRelTrc.copy())
{
    itsFracBlc = False;
    itsFracTrc = False;
    itsFracInc = False;
    fillFromIPosition (blc, trc, inc);
}


LCSlicer::LCSlicer (const LCSlicer& other)
: itsBlc           (other.itsBlc),
  itsTrc           (other.itsTrc),
  itsInc           (other.itsInc),
  itsFracBlc       (other.itsFracBlc),
  itsFracTrc       (other.itsFracTrc),
  itsFracInc       (other.itsFracInc),
  itsAbsRelBlc     (other.itsAbsRelBlc),
  itsAbsRelTrc     (other.itsAbsRelTrc),
  itsIsFractional  (other.itsIsFractional),
  itsIsAbsolute    (other.itsIsAbsolute),
  itsIsUnspecified (other.itsIsUnspecified),
  itsIsStrided     (other.itsIsStrided),
  itsComment       (other.itsComment)
{}

LCSlicer::~LCSlicer()
{}

LCSlicer& LCSlicer::operator= (const LCSlicer& other)
{
    if (this != &other) {
        uInt nr = other.itsBlc.nelements();
	itsBlc.resize (nr);
	itsTrc.resize (nr);
	itsInc.resize (nr);
	itsBlc = other.itsBlc;
	itsTrc = other.itsTrc;
	itsInc = other.itsInc;
	itsFracBlc.resize (nr);
	itsFracTrc.resize (nr);
	itsFracInc.resize (nr);
	itsAbsRelBlc.resize (nr);
	itsAbsRelTrc.resize (nr);
	itsFracBlc   = other.itsFracBlc;
	itsFracTrc   = other.itsFracTrc;
	itsFracInc   = other.itsFracInc;
	itsAbsRelBlc = other.itsAbsRelBlc;
	itsAbsRelTrc = other.itsAbsRelTrc;
	itsIsFractional  = other.itsIsFractional;
	itsIsAbsolute    = other.itsIsAbsolute;
	itsIsUnspecified = other.itsIsUnspecified;
	itsIsStrided     = other.itsIsStrided;
	itsComment       = other.itsComment;
    }
    return *this;
}

Bool LCSlicer::operator== (const LCSlicer& other) const
{
    // Compare private data.
    if (itsBlc.nelements() != other.itsBlc.nelements()
    ||  itsIsFractional  != other.itsIsFractional
    ||  itsIsAbsolute    != other.itsIsAbsolute
    ||  itsIsUnspecified != other.itsIsUnspecified
    ||  itsIsStrided     != other.itsIsStrided) {
	return False;
    }
    for (uInt i=0; i<itsInc.nelements(); i++) {
	if (!near (itsBlc(i), other.itsBlc(i))
	||  !near (itsTrc(i), other.itsTrc(i))
	||  !near (itsInc(i), other.itsInc(i))) {
	    return False;
	}
	if (itsFracBlc(i) != other.itsFracBlc(i)
	||  itsFracTrc(i) != other.itsFracTrc(i)
	||  itsFracInc(i) != other.itsFracInc(i)
	||  itsAbsRelBlc(i) != other.itsAbsRelBlc(i)
	||  itsAbsRelTrc(i) != other.itsAbsRelTrc(i)) {
	    return False;
	}
    }
    return True;
}


void LCSlicer::fillFlags (Bool fractional, Int absRel, uInt nrblc,
			  uInt nrtrc, uInt nrinc)
{
    itsFracBlc.resize (nrblc);
    itsFracTrc.resize (nrtrc);
    itsFracInc.resize (nrinc);
    itsAbsRelBlc.resize (nrblc);
    itsAbsRelTrc.resize (nrtrc);
    itsFracBlc = fractional;
    itsFracTrc = fractional;
    itsFracInc = False;
    itsAbsRelBlc = absRel;
    itsAbsRelTrc = absRel;
}

void LCSlicer::fillFromDouble (const Vector<Double>& blc,
			       const Vector<Double>& trc,
			       const Vector<Double>& inc)
{
    uInt i;
    itsBlc.resize (blc.nelements());
    for (i=0; i<blc.nelements(); i++) {
        itsBlc(i) = blc(i);
    }
    itsTrc.resize (trc.nelements());
    for (i=0; i<trc.nelements(); i++) {
        itsTrc(i) = trc(i);
    }
    itsInc.resize (inc.nelements());
    for (i=0; i<inc.nelements(); i++) {
        itsInc(i) = inc(i);
    }
    fill();
}

void LCSlicer::fillFromIPosition (const IPosition& blc,
				  const IPosition& trc,
				  const IPosition& inc)
{
    uInt i;
    itsBlc.resize (blc.nelements());
    for (i=0; i<blc.nelements(); i++) {
        itsBlc(i) = blc(i);
    }
    itsTrc.resize (trc.nelements());
    for (i=0; i<trc.nelements(); i++) {
        itsTrc(i) = trc(i);
    }
    itsInc.resize (inc.nelements());
    for (i=0; i<inc.nelements(); i++) {
        itsInc(i) = inc(i);
    }
    fill();
}

void LCSlicer::fill()
{
    // Check if the corresponding vectors have equal lengths.
    uInt nrblc = itsBlc.nelements();
    uInt nrtrc = itsTrc.nelements();
    uInt nrinc = itsInc.nelements();
    if (itsFracBlc.nelements() != nrblc
    ||  itsAbsRelBlc.nelements() != nrblc) {
        throw (AipsError ("LCSlicer::LCSLicer - "
			  "blc, fracBlc, and absRelBlc have unequal lengths"));
    }
    if (itsFracTrc.nelements() != nrtrc
    ||  itsAbsRelTrc.nelements() != nrtrc) {
        throw (AipsError ("LCSlicer::LCSLicer - "
			  "trc, fracTrc, and absRelTrc have unequal lengths"));
    }
    if (itsFracInc.nelements() != nrinc) {
        throw (AipsError ("LCSlicer::LCSLicer - "
			  "inc and fracInc have unequal lengths"));
    }
    // Give all vectors the same length.
    // Pad with default values.
    uInt nr = max(max(nrblc, nrtrc), nrinc);
    if (nrblc < nr) {
        itsBlc.resize (nr, True);
        itsFracBlc.resize (nr, True);
        itsAbsRelBlc.resize (nr, True);
	for (uInt i=nrblc; i<nr; i++) {
	    itsBlc(i) = Slicer::MimicSource;
	    itsFracBlc(i) = False;
	    itsAbsRelBlc(i) = RegionType::Abs;
	}
    }
    if (nrtrc < nr) {
        itsTrc.resize (nr, True);
        itsFracTrc.resize (nr, True);
        itsAbsRelTrc.resize (nr, True);
	for (uInt i=nrtrc; i<nr; i++) {
	    itsTrc(i) = Slicer::MimicSource;
	    itsFracTrc(i) = False;
	    itsAbsRelTrc(i) = RegionType::Abs;
	}
    }
    if (nrinc < nr) {
        itsInc.resize (nr, True);
        itsFracInc.resize (nr, True);
	for (uInt i=nrinc; i<nr; i++) {
	    itsInc(i) = 1;
	    itsFracInc(i) = False;
	}
    }
    // Check if the region is absloute, fractional, unspecified, or strided.
    itsIsFractional  = False;
    itsIsAbsolute    = True;
    itsIsUnspecified = False;
    itsIsStrided     = False;
    for (uInt i=0; i<nr; i++) {
        if (itsBlc(i) == Slicer::MimicSource) {
	    itsIsUnspecified = True;
	    itsFracBlc(i) = False;
	    itsAbsRelBlc(i) = RegionType::Abs;
	}
	if (itsTrc(i) == Slicer::MimicSource) {
	    itsIsUnspecified = True;
	    itsFracTrc(i) = False;
	    itsAbsRelTrc(i) = RegionType::Abs;
	}
	if (itsFracBlc(i) || itsFracTrc(i) || itsFracInc(i)) {
	    itsIsFractional = True;
	}
	if (itsAbsRelBlc != RegionType::Abs
	||  itsAbsRelTrc != RegionType::Abs) {
	    itsIsAbsolute = False;
	}
        if (itsInc(i) != 1  ||  itsFracInc(i)) {
	    itsIsStrided = True;
	}
    }
}


Slicer LCSlicer::toSlicer (const IPosition& referencePixel,
			   const IPosition& newLatticeShape) const
{
    uInt nr = referencePixel.nelements();
    Vector<Float> vec (nr);
    for (uInt i=0; i<nr; i++) {
        vec(i) = referencePixel(i);
    }
    return toSlicer (vec, newLatticeShape);
}
Slicer LCSlicer::toSlicer (const Vector<Double>& referencePixel,
			   const IPosition& newLatticeShape) const
{
    uInt nr = referencePixel.nelements();
    Vector<Float> vec (nr);
    for (uInt i=0; i<nr; i++) {
        vec(i) = referencePixel(i);
    }
    return toSlicer (vec, newLatticeShape);
}
Slicer LCSlicer::toSlicer (const Vector<Float>& referencePixel,
			   const IPosition& newLatticeShape) const
{
    uInt i;
    if (referencePixel.nelements() != newLatticeShape.nelements()) {
        throw (AipsError ("LCSlicer::makeComplete - "
			  "referencePixel and newLatticeShape vectors "
			  "do not have same length"));
    }
    uInt ndreg = ndim();
    uInt ndout = newLatticeShape.nelements();
    if (ndout < ndreg) {
	throw (AipsError ("LCSlicer::makeComplete - "
			  "length of newLatticeShape vector less than "
			  "dimensionality of region"));
    }
    // The output result can have a higher dimensionality than the region
    // (resulting in auto-extension).
    // So create the results with default values which are suitable
    // for axes to be added.
    IPosition blc(ndout, 0);
    IPosition trc(newLatticeShape-1);
    IPosition inc(ndout, 1);
    // Now convert the region's blc,trc,inc to a normal blc,trc,inc.
    for (i=0; i<ndreg; i++) {
        Float v = itsBlc(i);
	if (v == Slicer::MimicSource) {
	    v = 0;
	} else {
	    if (itsFracBlc(i)) {
		v *= newLatticeShape(i);
	    }
	    if (itsAbsRelBlc(i) == RegionType::RelRef) {
		v += referencePixel(i);
	    } else if (itsAbsRelBlc(i) == RegionType::RelCen) {
		v += Float(newLatticeShape(i)) / 2;
	    }
	}
	blc(i) = Int(v + 0.5);
        v = itsTrc(i);
	if (v == Slicer::MimicSource) {
	    v = newLatticeShape(i) - 1;
	} else {
	    if (itsFracTrc(i)) {
		v *= newLatticeShape(i);
		v -= 1;
	    }
	    if (itsAbsRelTrc(i) == RegionType::RelRef) {
		v += referencePixel(i);
	    } else if (itsAbsRelTrc(i) == RegionType::RelCen) {
		v += Float(newLatticeShape(i)) / 2;
	    }
	}
	trc(i) = Int(v + 0.5);
        v = itsInc(i);
	if (v == Slicer::MimicSource) {
	    v = 1;
	} else {
	    if (itsFracInc(i)) {
		v *= newLatticeShape(i);
	    }
	}
	inc(i) = Int(v + 0.5);
    }
    for (i=0; i<ndout; i++) {
	// Make sure box does not exceed lattice.
	if (blc(i) < 0) {
	    blc(i) = 0;
	}
	if (trc(i) >= newLatticeShape(i)) {
	    trc(i) = newLatticeShape(i) - 1;
	}
	if (blc(i) > trc(i)) {
	    ostringstream bstr, tstr;
	    bstr << blc;
	    tstr << trc;
	    throw (AipsError ("LCSlicer::toSlicer - "
			      "blc " + String(bstr) + " must be <= trc "
			      + String(tstr)));
	}
    }
    // Return the completed LCSlicer as a Slicer.
    return Slicer (blc, trc, inc, Slicer::endIsLast);
}


Bool LCSlicer::isComplete() const
{
    return  (!itsIsFractional && itsIsAbsolute && !itsIsUnspecified);
}

String LCSlicer::className()
{
    return "LCSlicer";
}

String LCSlicer::type() const
{
    return className();
}

TableRecord LCSlicer::toRecord (const String&) const
{
    TableRecord rec;
    rec.define ("isRegion", Int(RegionType::ArrSlicer));
    rec.define ("name", className());
    rec.define ("comment", itsComment);
    // Write 1-relative.
    rec.define ("oneRel", True);
    Vector<Float> blc(itsBlc.copy());
    Vector<Float> trc(itsTrc.copy());
    for (uInt i=0; i<itsBlc.nelements(); i++) {
	if ((!itsFracBlc(i)  &&  itsAbsRelBlc(i) == RegionType::Abs)
        ||  blc(i) == Slicer::MimicSource) {
	    blc(i)++;
	}
	if ((!itsFracTrc(i)  &&  itsAbsRelTrc(i) == RegionType::Abs)
        ||  trc(i) == Slicer::MimicSource) {
	    trc(i)++;
	}
    }
    rec.define ("blc", blc);
    rec.define ("trc", trc);
    rec.define ("inc", itsInc);
    rec.define ("fracblc", itsFracBlc);
    rec.define ("fractrc", itsFracTrc);
    rec.define ("fracinc", itsFracInc);
    rec.define ("arblc", itsAbsRelBlc);
    rec.define ("artrc", itsAbsRelTrc);
    return rec;
}

LCSlicer* LCSlicer::fromRecord (const TableRecord& rec,
				const String&)
{
    uInt i;
    if (!rec.isDefined("isRegion")
    ||  rec.asInt("isRegion") != RegionType::ArrSlicer) {
	throw (AipsError ("LCSlicer::fromRecord - "
			  "record does not contain an LCSlicer"));
    }
    // If 1-relative, subtract 1 from blc and trc.
    Bool oneRel = rec.asBool ("oneRel");
    Vector<Float> blc (rec.toArrayFloat ("blc").copy());
    Vector<Float> trc (rec.toArrayFloat ("trc").copy());
    Vector<Bool> fracblc (rec.toArrayBool ("fracblc"));
    Vector<Bool> fractrc (rec.toArrayBool ("fractrc"));
    Vector<Int> arblc (rec.toArrayInt ("arblc"));
    Vector<Int> artrc (rec.toArrayInt ("artrc"));
    // If blc,trc is 1-relative, make it 0-relative by subtracting 1.
    // Do it only if absolute non-fractional or if MimicSource+1.
    if (oneRel) {
      for (i=0; i<std::min(blc.nelements(), fracblc.nelements()); i++) {
	    if ((! fracblc(i)  &&  arblc(i) == RegionType::Abs)
            ||  blc(i) == 1+Slicer::MimicSource) {
		if (blc(i) != Slicer::MimicSource) {
		    blc(i)--;
		}
	    }
	}
      for (i=0; i<std::min(trc.nelements(), fractrc.nelements()); i++) {
	    if ((! fractrc(i)  &&  artrc(i) == RegionType::Abs)
	    ||  trc(i) == 1+Slicer::MimicSource) {
		if (trc(i) != Slicer::MimicSource) {
		    trc(i)--;
		}
	    }
	}
    }
    LCSlicer* regPtr = new LCSlicer (blc, trc,
				     rec.toArrayFloat ("inc"),
				     fracblc, fractrc,
				     rec.toArrayBool ("fracinc"),
				     arblc, artrc);
    if (rec.isDefined ("comment")) {
	regPtr->setComment (rec.asString ("comment"));
    }
    return regPtr;
}

} //# NAMESPACE CASACORE - END

