//# MPosition.cc:  A Measure: position on Earth
//# Copyright (C) 1995, 1996
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

//# Includes
#ifdef __GNUG__
#include <aips/Measures/Quantum.h>
typedef Quantum<Double> gpp_mposition_bug1;
#endif
#include <aips/Exceptions.h>
#include <aips/Arrays/Vector.h>
#include <aips/Mathematics/Math.h>
#include <aips/Utilities/Assert.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MeasData.h>

//# Constructors
MPosition::MPosition() :
  MeasBase<MVPosition,MPosition::Ref>() {}

MPosition::MPosition(const MVPosition &dt) : 
  MeasBase<MVPosition,MPosition::Ref>(dt,MPosition::DEFAULT) {}

MPosition::MPosition(const MVPosition &dt, const MPosition::Ref &rf) : 
  MeasBase<MVPosition,MPosition::Ref>(dt,rf) {}

MPosition::MPosition(const MVPosition &dt, uInt rf) : 
  MeasBase<MVPosition,MPosition::Ref>(dt,rf) {}

MPosition::MPosition(const Quantity &dt, const Quantity &dt1,
		     const Quantity &dt2) : 
  MeasBase<MVPosition,MPosition::Ref>(MVPosition(dt,dt1,dt2),
				      MPosition::DEFAULT) {}

MPosition::MPosition(const Quantity &dt, const Quantity &dt1,
		     const Quantity &dt2, const MPosition::Ref &rf) : 
  MeasBase<MVPosition,MPosition::Ref>(MVPosition(dt,dt1,dt2),rf) {}

MPosition::MPosition(const Quantity &dt, const Quantity &dt1,
		     const Quantity &dt2, uInt rf) : 
  MeasBase<MVPosition,MPosition::Ref>(MVPosition(dt,dt1,dt2),rf) {}

MPosition::MPosition(const Quantity &dt0, const Quantum<Vector<Double> > &dt) :
  MeasBase<MVPosition,MPosition::Ref>(MVPosition(dt0,dt),
				      MPosition::DEFAULT) {}

MPosition::MPosition(const Quantity &dt0, const Quantum<Vector<Double> > &dt,
		     const MPosition::Ref &rf) : 
  MeasBase<MVPosition,MPosition::Ref>(MVPosition(dt0,dt),rf) {}

MPosition::MPosition(const Quantity &dt0, const Quantum<Vector<Double> > &dt,
		     uInt rf) : 
  MeasBase<MVPosition,MPosition::Ref>(MVPosition(dt0,dt),rf) {}

//# Destructor
MPosition::~MPosition() {}

//# Operators

//# Member functions

const String &MPosition::tellMe() const {
    return MPosition::showMe();
}

const String &MPosition::showMe() {
    static const String name("Position");
    return name;
}

const String &MPosition::showType(uInt tp) {
    static const String tname[MPosition::N_Types] = {
	"ITRF",
	"WGS84"};
    DebugAssert(tp < MPosition::N_Types, AipsError);
    return tname[tp];
}

Bool MPosition::giveMe(const String &in, MPosition::Ref &mr) {
    static const Int N_name = 2;
    static const String tname[N_name] = {
	"ITRF",
	"WGS84"};

    static const uInt oname[N_name] = {
	MPosition::ITRF,
	MPosition::WGS84};

    uInt i = Measure::giveMe(in, N_name, tname);

    if (i>=N_name) {
	mr = MPosition::Ref();
	return False;
    } else {
	mr = MPosition::Ref(oname[i]);
    };
    return True;
}

Quantum<Vector<Double> > MPosition::get(const Unit &inunit) const {
    return Quantum<Vector<Double> >(data.getValue(),"m").get(inunit);
}

Quantum<Vector<Double> > MPosition::getAngle() const {
    return (data.getAngle());
}

Quantum<Vector<Double> > MPosition::getAngle(const Unit &inunit) const {
    return (data.getAngle(inunit));
}

void *MPosition::clone() const {
    return ((void *) new MPosition(*this));
}

void MPosition::getConvert(MPosition::Convert &mc,
			   const MPosition::Ref &inref,
			   const MPosition::Ref &outref) {
// Array of conversion routines to call
    static const MPosition::Routes 
	FromTo[MPosition::N_Types][MPosition::N_Types] = {
    { MPosition::N_Routes,    MPosition::ITRF_WGS84},
    { MPosition::WGS84_ITRF,  MPosition::N_Routes}
    };

// List of codes converted to
    static const MPosition::Types ToRef[MPosition::N_Routes] = {
	MPosition::WGS84, MPosition::ITRF
	};

    Int iin  = inref.getType();
    Int iout = outref.getType();
    Int tmp;
    while (iin != iout) {
	tmp = FromTo[iin][iout];
	iin = ToRef[tmp];
	mc.addMethod(tmp);
	initConvert(tmp, mc);
    };
}

void MPosition::clearConvert(MPosition::Convert &mc) {
  delete (Vector<Double> *) mc.getStruct(MPosition::DVEC1);
}

//# Conversion routines
void MPosition::initConvert(uInt which, MPosition::Convert &mc) {
    if (!(mc.getStruct(MPosition::DVEC1))) {
	mc.addStruct(MPosition::DVEC1,
		     (void *) new Vector<Double>(3));
    };

    switch (which) {

	case ITRF_WGS84:
	break;

	case WGS84_ITRF:
	break;

	default:
	break;
	
    }
}

void MPosition::doConvert(MVPosition &in, 
			  const MPosition::Ref &outref,
			  const MPosition::Ref &inref,
 			  const MPosition::Convert &mc) {
    
    Double g1, g2, g3;
    Vector<Double> *angsol;

    for (Int i=0; i<mc.nMethod(); i++) {

      switch (mc.getMethod(i)) {
	
      case ITRF_WGS84: {
	Double d1, d2;
// Get angles
	angsol = (Vector<Double> *) mc.getStruct(MPosition::DVEC1);
	*angsol = in.get();
// Get flattening
	g1 = MeasData::WGS84(1); g1 = 1.0/g1;
	g1 = 2*g1 - g1*g1;
// Iterate
	d2 = (*angsol)(0) * cos((*angsol)(2));
	do {
	  g2 = (*angsol)(2);
	  d1 = sin(g2);
	  g3 = 1.0/sqrt(1 - g1 * d1 * d1);
	  (*angsol)(2) = atan((in(2) + 
			       MeasData::WGS84(0) * g3 * g1 * d1)/d2);
	}
	while ( !nearAbs((*angsol)(2), g2, 1e-10));
	(*angsol)(0) = d2/cos((*angsol)(2)) - MeasData::WGS84(0) * g3;
	in = MVPosition(Quantity((*angsol)(0),"m"),
			(*angsol)(1), (*angsol)(2));
      };
      break;

      case WGS84_ITRF: {
// Equatorial radius
	g1 = MeasData::WGS84(0);
// Flattening
	g2 = MeasData::WGS84(1);
	g2 = 1.0 - 1.0/g2; g2 *= g2;
// C
	g3 = in(0) * in(0) +
	  in(1) * in(1) +
	  g2 * in(2) * in(2);
	g3 = g1 * sqrt(1.0/g3);
// S
	g2 *= g3;
// Apply
	in(0) *= (1.0 + g3);
	in(1) *= (1.0 + g3);
	in(2) *= (1.0 + g2);
      }	
      break;

      default:
	break;
	
      }
    }
}
