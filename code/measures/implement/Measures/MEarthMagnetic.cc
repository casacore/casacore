//# MEarthMagnetic.cc: A Measure: Magnetic field on Earth
//# Copyright (C) 1995,1996,1997
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
#include <aips/Arrays/Vector.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Utilities/Assert.h>
#include <trial/Measures/MEarthMagnetic.h>
#include <aips/Measures/MeasData.h>
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

Bool MEarthMagnetic::giveMe(const String &in, MEarthMagnetic::Ref &mr) {
    static const Int N_name = 5;
    static const String tname[N_name] = {
	"DIPOLE",
	"MDIPOLE",
	"XDIPOLE",
	"MXDIPOLE",
	"IGRF"};

    static const uInt oname[N_name] = {
	MEarthMagnetic::DIPOLE,
	MEarthMagnetic::MDIPOLE,
	MEarthMagnetic::XDIPOLE,
	MEarthMagnetic::MXDIPOLE,
	MEarthMagnetic::IGRF};

    uInt i = Measure::giveMe(in, N_name, tname);

    if (i>=N_name) {
	mr = MEarthMagnetic::Ref();
	return False;
    } else {
	mr = MEarthMagnetic::Ref(oname[i]);
    };
    return True;
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

void *MEarthMagnetic::clone() const {
    return ((void *) new MEarthMagnetic(*this));
}
