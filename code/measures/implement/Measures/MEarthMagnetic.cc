//# MEarthMagnetic.cc: A Measure: Magnetic field on Earth
//# Copyright (C) 1995,1996,1997,1998
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
typedef Quantum<Double> gpp_mEarthMagnetic_bug1;
#endif
#include <aips/Exceptions.h>
#include <aips/Utilities/Assert.h>
#include <aips/Arrays/Vector.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Constants.h>
#include <aips/RTTI/Register.h>
#include <trial/Measures/MEarthMagnetic.h>
#include <aips/Measures/RotMatrix.h>
#include <aips/Measures/Euler.h>
#include <aips/Measures/MVPosition.h>
#include <aips/Measures/MVDirection.h>
#include <aips/Measures/QMath.h>

//# Constructors
MEarthMagnetic::MEarthMagnetic() :
  MeasBase<MVEarthMagnetic,MEarthMagnetic::Ref>() {}

MEarthMagnetic::MEarthMagnetic(const MVEarthMagnetic &dt) : 
  MeasBase<MVEarthMagnetic,MEarthMagnetic::Ref>(dt,MEarthMagnetic::DEFAULT) {}

MEarthMagnetic::MEarthMagnetic(const MVEarthMagnetic &dt, const MEarthMagnetic::Ref &rf) : 
  MeasBase<MVEarthMagnetic,MEarthMagnetic::Ref>(dt,rf) {}

MEarthMagnetic::MEarthMagnetic(const MVEarthMagnetic &dt, uInt rf) : 
  MeasBase<MVEarthMagnetic,MEarthMagnetic::Ref>(dt,rf) {}

//# Destructor
MEarthMagnetic::~MEarthMagnetic() {}

//# Operators
MVEarthMagnetic MEarthMagnetic::operator()(const MVPosition &pos) const {
  static Quantity dpfield = Quantity(0.30953, "G");
  static Quantity dplong = Quantity(299.73,"deg");
  static Quantity dplat =  Quantity(78.7,"deg");
  static Quantity dprad = Quantity(6371.2, "km");
  MVDirection rz(dplong, dplat);
  MVDirection rx(dplong, dplat - Quantity(90.0, "deg"));
  MVDirection ry(rz.crossProduct(rx));
  RotMatrix dprot; dprot.set(rx.getValue(), ry.getValue(),
				    rz.getValue());
  RotMatrix idprot(dprot); idprot.transpose();

  MVPosition posrot = dprot * pos;
  Vector<Double> rotan = posrot.getAngle().getBaseValue();
  Quantity poslen = posrot.getLength();
  Double hor = (dpfield * pow(dprad/poslen, 3) * sin(rotan(1))).
    getBaseValue();
  Double ver = (2. * dpfield * pow(dprad/poslen, 3) * sin(rotan(1))).
    getBaseValue();
  MVEarthMagnetic res = MVEarthMagnetic(hor,0,ver);
  rz = MVDirection(rotan(0), rotan(1));
  rx = MVDirection(rotan(0), rotan(1) - C::pi_2);
  ry = rz.crossProduct(rx);
  dprot.set(rx.getValue(), ry.getValue(), rz.getValue());
  dprot.transpose();
  res = dprot * res;
  res = idprot * res;
  return res;
}

//# Member functions

const String &MEarthMagnetic::tellMe() const {
    return MEarthMagnetic::showMe();
}

const String &MEarthMagnetic::showMe() {
    static const String name("EarthMagnetic");
    return name;
}

uInt MEarthMagnetic::type() const {
  return Register((MEarthMagnetic *)0);
}

void MEarthMagnetic::assert(const Measure &in) {
  if (in.type() != Register((MEarthMagnetic *)0)) {
    throw(AipsError("Illegal Measure type argument: " +
		    MEarthMagnetic::showMe()));
  };
}

const String &MEarthMagnetic::showType(uInt tp) {
    static const String tname[MEarthMagnetic::N_Types] = {
	"DIPOLE",
	"MDIPOLE",
	"XDIPOLE",
	"MXDIPOLE",
	"IGRF"};
    DebugAssert(tp < MEarthMagnetic::N_Types, AipsError);
    return tname[tp];
}

Bool MEarthMagnetic::getType(MEarthMagnetic::Types &tp, const String &in) {
  static const Int N_name = 5;
  static const String tname[N_name] = {
    "DIPOLE",
    "MDIPOLE",
    "XDIPOLE",
    "MXDIPOLE",
    "IGRF"};

  static const MEarthMagnetic::Types oname[N_name] = {
    MEarthMagnetic::DIPOLE,
    MEarthMagnetic::MDIPOLE,
    MEarthMagnetic::XDIPOLE,
    MEarthMagnetic::MXDIPOLE,
    MEarthMagnetic::IGRF};

    uInt i = Measure::giveMe(in, N_name, tname);

  if (i>=N_name) {
    return False;
  } else {
    tp = oname[i];
  };
  return True;
}

Bool MEarthMagnetic::giveMe(MEarthMagnetic::Ref &mr, const String &in) {
  MEarthMagnetic::Types tp;
  if (MEarthMagnetic::getType(tp, in)) {
    mr = MEarthMagnetic::Ref(tp);
  } else {
    mr = MEarthMagnetic::Ref();
    return False;
  };
  return True;
};

Bool MEarthMagnetic::giveMe(const String &in, MEarthMagnetic::Ref &mr) {
  return MEarthMagnetic::giveMe(mr, in);
}

Bool MEarthMagnetic::setOffset(const Measure &in) {
  if (in.type() != Register((MEarthMagnetic *)0)) return False;
  ref.set(in);
  return True;
}

Bool MEarthMagnetic::setRefString(const String &in) {
  MEarthMagnetic::Types tp;
  if (MEarthMagnetic::getType(tp, in)) {
    ref.setType(tp);
    return True;
  };
  ref.setType(MEarthMagnetic::DEFAULT);
  return False;
}

const String &MEarthMagnetic::getDefaultType() const {
  return MEarthMagnetic::showType(MEarthMagnetic::DEFAULT);
}

String MEarthMagnetic::getRefString() const {
  return MEarthMagnetic::showType(ref.getType());
}

Quantum<Vector<Double> > MEarthMagnetic::get(const Unit &inunit) const {
    return Quantum<Vector<Double> >(data.getValue(),"m").get(inunit);
}

Quantum<Vector<Double> > MEarthMagnetic::getAngle() const {
    return (data.getAngle());
}

Quantum<Vector<Double> > MEarthMagnetic::getAngle(const Unit &inunit) const {
    return (data.getAngle(inunit));
}

Measure *MEarthMagnetic::clone() const {
    return (new MEarthMagnetic(*this));
}
