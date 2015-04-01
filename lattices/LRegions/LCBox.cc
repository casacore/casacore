//# LCBox.cc: Class to define a rectangular box of interest
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

#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

LCBox::LCBox()
{}

LCBox::LCBox (const IPosition& latticeShape)
: LCRegionFixed (latticeShape)
{
    // Set the box to the full lattice.
    setBoundingBox (Slicer (IPosition(latticeShape.nelements(), 0),
			    latticeShape));
    // Fill the blc and trc vectors.
    fillBlcTrc();
}

LCBox::LCBox (const Slicer& box, const IPosition& latticeShape)
: LCRegionFixed (latticeShape)
{
    // Make sure no stride is given.
    if (box.stride() != 1) {
	throw (AipsError ("LCBox::LCBox - "
			  "stride in given Slicer has to be 1"));
    }
    // When the slicer is fixed (i.e. blc and trc explicitly given),
    // it is possible that it partly exceeds the lattice boundaries.
    if (box.isFixed()) {
        setSlicerBox (box.start(), box.end());
    } else {
	setBoundingBox (box);
    }
    // Fill the blc and trc vectors.
    fillBlcTrc();
}

// Construct from the IPosition's defining the bottom-left and
// top-right corner of the box.
LCBox::LCBox (const IPosition& blc, const IPosition& trc,
	      const IPosition& latticeShape)
: LCRegionFixed (latticeShape)
{
    setSlicerBox (blc, trc);
    fillBlcTrc();
}

LCBox::LCBox (const Vector<Float>& blc, const Vector<Float>& trc,
	      const IPosition& latticeShape)
: LCRegionFixed (latticeShape),
  itsBlc        (blc.copy()),
  itsTrc        (trc.copy())
{
    uInt i;
    IPosition bl(blc.nelements());
    for (i=0; i<blc.nelements(); i++) {
	bl(i) = Int(blc(i) + 0.5);
    }
    IPosition tr(trc.nelements());
    for (i=0; i<trc.nelements(); i++) {
	tr(i) = Int(trc(i) + 0.5);
    }
    setSlicerBox (bl, tr);
}

LCBox::LCBox (const Vector<Double>& blc, const Vector<Double>& trc,
	      const IPosition& latticeShape)
: LCRegionFixed (latticeShape),
  itsBlc        (blc.nelements()),
  itsTrc        (trc.nelements())
{
    uInt i;
    IPosition bl(blc.nelements());
    for (i=0; i<blc.nelements(); i++) {
	itsBlc(i) = blc(i);
	bl(i) = Int(blc(i) + 0.5);
    }
    IPosition tr(trc.nelements());
    for (i=0; i<trc.nelements(); i++) {
	itsTrc(i) = trc(i);
	tr(i) = Int(trc(i) + 0.5);
    }
    setSlicerBox (bl, tr);
}

LCBox::LCBox (const LCBox& other)
: LCRegionFixed (other),
  itsBlc(other.itsBlc),
  itsTrc(other.itsTrc)
{}
 

LCBox::~LCBox()
{}

LCBox& LCBox::operator= (const LCBox& other)
{
    if (this != &other) {
	LCRegionFixed::operator= (other);
	itsBlc.resize(other.itsBlc.nelements());
	itsTrc.resize(other.itsTrc.nelements());
	itsBlc = other.itsBlc;    
	itsTrc = other.itsTrc;
    }
    return *this;
}


Bool LCBox::operator== (const LCRegion& other) const
{
    // Check if parent class matches.
    // If so, we can safely cast.
    if (! LCRegionFixed::operator== (other)) {
	return False;
    }
    const LCBox& that = (const LCBox&)other;
    // Compare private data.
    if (itsBlc.nelements() != that.itsBlc.nelements()
    ||  itsTrc.nelements() != that.itsTrc.nelements()) {
	return False;
    }
    for (uInt i=0; i<itsBlc.nelements(); i++) {
	if (!near (itsBlc(i), that.itsBlc(i))
	||  !near (itsTrc(i), that.itsTrc(i))) {
	    return False;
	}
    }
    return True;
}

LCRegion* LCBox::cloneRegion() const
{
    return new LCBox(*this);
}

LCRegion* LCBox::doTranslate (const Vector<Float>& translateVector,
			      const IPosition& newLatticeShape) const
{
    uInt ndim = latticeShape().nelements();
    Vector<Float> blc (itsBlc.copy());
    Vector<Float> trc (itsTrc.copy());
    for (uInt i=0; i<ndim; i++) {
        blc(i) += translateVector(i);
        trc(i) += translateVector(i);
    }
    return new LCBox (blc, trc, newLatticeShape);
}

String LCBox::className()
{
    return "LCBox";
}

String LCBox::type() const
{
   return className();
}

TableRecord LCBox::toRecord (const String&) const
{
    TableRecord rec;
    defineRecordFields (rec, className());
    // Write 1-relative.
    rec.define ("oneRel", True);
    rec.define ("blc", itsBlc + Float(1));
    rec.define ("trc", itsTrc + Float(1));
    rec.define ("shape", latticeShape().asVector());
    return rec;
}

LCBox* LCBox::fromRecord (const TableRecord& rec, const String&)
{
    // If 1-relative, subtract 1 from blc and trc.
    Bool oneRel = rec.asBool ("oneRel");
    Float off = (oneRel ? 1:0);
    Array<Float> blc (rec.toArrayFloat ("blc"));
    Array<Float> trc (rec.toArrayFloat ("trc"));
    return new LCBox (blc-off, trc-off,
		      Vector<Int>(rec.toArrayInt ("shape")));
}

void LCBox::setSlicerBox (const IPosition& blc, const IPosition& trc)
{
    const IPosition& shape = latticeShape();
    uInt ndim = shape.nelements();
    if (blc.nelements() != ndim  ||  trc.nelements() != ndim) {
	throw (AipsError ("LCBox::LCBox - "
			  "length of blc and trc vectors have to match "
			  "dimensionality of lattice"));
    }
    IPosition bl(blc);
    IPosition tr(trc);
    for (uInt i=0; i<ndim; i++) {
        if (bl(i) < 0) {
	    bl(i) = 0;
	}
	if (tr(i) >= shape(i)) {
	    tr(i) = shape(i) - 1;
	}
	if (bl(i) > tr(i)) {
	    ostringstream bstr, tstr;
	    bstr << bl;
	    tstr << tr;
	    throw (AipsError ("LCBox::LCBox - "
			      "blc " + String(bstr) + " must be <= trc "
			      + String(tstr)));
	}
    }
    setBoundingBox (Slicer(bl, tr, Slicer::endIsLast));
}

void LCBox::fillBlcTrc()
{
    const Slicer& sl = boundingBox();
    uInt nd = sl.ndim();
    itsBlc.resize (nd);
    itsTrc.resize (nd);
    for (uInt i=0; i<nd; i++) {
	itsBlc(i) = sl.start()(i);
	itsTrc(i) = sl.end()(i);
    }
}

Bool LCBox::verify (IPosition& blc, IPosition& trc,
                    IPosition& inc, const IPosition& shape)
{
   IPosition inBlc(blc);
   IPosition inTrc(trc);
   IPosition inInc(inc);
   const Int nDim = shape.nelements();

// Check blc

   const Int blcDim = blc.nelements();
   blc.resize(nDim,True);
   if (blcDim == 0) {
      blc = 0;
   } else {
      for (Int i=0; i<nDim; i++) {
         if (i > blcDim-1) {
            blc(i) = 0;
         } else {
            if (blc(i) < 0 || blc(i) > shape(i)-1) blc(i) = 0;
         }
      }
   }

// Check trc

   const Int trcDim = trc.nelements();
   trc.resize(nDim,True);
   if (trcDim == 0) {
      trc = shape- 1;
   } else {
      for (Int i=0; i<nDim; i++) {
         if (i > trcDim-1) {
            trc(i) = shape(i) - 1;
         } else {
            if (trc(i) < 0 || trc(i) > shape(i)-1) {
               trc(i) = shape(i) - 1;
            }
         }
      }
   }

// Check increment

   const Int incDim = inc.nelements();
   inc.resize(nDim,True);
   if (incDim == 0) {
      inc = 1;
   } else {
      for (Int i=0; i<nDim; i++) {
         if (i > incDim-1) {
            inc(i) = 1;
         } else {
            if (inc(i) < 1 || inc(i) > trc(i)-blc(i)+1) inc(i) = 1;
         }
      }     
   }     

// Check blc<trc 

   for (Int i=0; i<nDim; i++) {
      if (blc (i) > trc(i)) {
         blc(i) = 0;
         trc(i) = shape(i) - 1;
      }
   }
//
   Bool changed = (blc.nelements()!=inBlc.nelements() ||
                         trc.nelements()!=inTrc.nelements() ||
                         inc.nelements()!=inInc.nelements());
   if (!changed) changed = (blc!=inBlc || trc!=inTrc || inc!=inInc);
//
   return changed;
}
  

             


} //# NAMESPACE CASACORE - END

