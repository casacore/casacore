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
#include <aips/Quanta/Quantum.h>
typedef Quantum<Double> gpp_mEarthMagnetic_bug1;
#endif
#include <aips/Measures/MEarthMagnetic.h>
#include <aips/Exceptions.h>
#include <aips/Utilities/Assert.h>
#include <aips/Arrays/Vector.h>
#include <aips/Mathematics/Math.h>
#include <aips/RTTI/Register.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Quanta/RotMatrix.h>
#include <aips/Quanta/Euler.h>
#include <aips/Measures/MVPosition.h>
#include <aips/Measures/MVDirection.h>
#include <aips/Quanta/QMath.h>

//# Constructors
MEarthMagnetic::MEarthMagnetic() :
  MeasBase<MVEarthMagnetic,MEarthMagnetic::Ref>() {}

MEarthMagnetic::MEarthMagnetic(const MVEarthMagnetic &dt) : 
  MeasBase<MVEarthMagnetic,MEarthMagnetic::Ref>(dt,MEarthMagnetic::DEFAULT) {}

MEarthMagnetic::MEarthMagnetic(const MVEarthMagnetic &dt, const MEarthMagnetic::Ref &rf) : 
  MeasBase<MVEarthMagnetic,MEarthMagnetic::Ref>(dt,rf) {}

MEarthMagnetic::MEarthMagnetic(const MVEarthMagnetic &dt, uInt rf) : 
  MeasBase<MVEarthMagnetic,MEarthMagnetic::Ref>(dt,rf) {}

MEarthMagnetic::MEarthMagnetic(const Measure *dt) :
  MeasBase<MVEarthMagnetic,MEarthMagnetic::Ref>(dt) {}

MEarthMagnetic::MEarthMagnetic(const MeasValue *dt) :
  MeasBase<MVEarthMagnetic,MEarthMagnetic::Ref>(*(MVEarthMagnetic*)dt,
						MEarthMagnetic::DEFAULT) {}

MEarthMagnetic::MEarthMagnetic(const MEarthMagnetic::Ref &rf) :
  MeasBase<MVEarthMagnetic,MEarthMagnetic::Ref>(rf) {}

MEarthMagnetic::MEarthMagnetic(const MEarthMagnetic &other)
  : MeasBase<MVEarthMagnetic, MeasRef<MEarthMagnetic> > (other) {}

MEarthMagnetic &MEarthMagnetic::operator=(const MEarthMagnetic &other) {
  if (this != &other) {
    MeasBase<MVEarthMagnetic, MeasRef<MEarthMagnetic> > &This = *this;
    const MeasBase<MVEarthMagnetic, MeasRef<MEarthMagnetic> > &Other = other;
    This = Other;
  }
  return *this;
}

//# Destructor
MEarthMagnetic::~MEarthMagnetic() {}

//# Operators

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
    "ITRF",
    "J2000",
    "JMEAN",
    "JTRUE",
    "APP",
    "B1950",
    "BMEAN",
    "BTRUE",
    "GALACTIC",
    "HADEC",
    "AZEL",
    "AZELSW",
    "JNAT",
    "ECLIPTIC",
    "MECLIPTIC",
    "TECLIPTIC",
    "SUPERGAL" };
  static const String pname[MEarthMagnetic::N_Models -
			   MEarthMagnetic::IGRF] = {
    "IGRF" };

    if ((tp & MEarthMagnetic::EXTRA) == 0) {
      DebugAssert(tp < MEarthMagnetic::N_Types, AipsError);
      return tname[tp];
    };
    DebugAssert((tp & ~MEarthMagnetic::EXTRA) < 
		(MEarthMagnetic::N_Models - MEarthMagnetic::IGRF), AipsError);
    return pname[tp & ~MEarthMagnetic::EXTRA];
}

Bool MEarthMagnetic::getType(MEarthMagnetic::Types &tp, const String &in) {
  static const Int N_name = 19;
  static const String tname[N_name] = {
    "ITRF",
    "J2000",
    "JMEAN",
    "JTRUE",
    "APP",
    "B1950",
    "BMEAN",
    "BTRUE",
    "GALACTIC",
    "HADEC",
    "AZEL",
    "AZELSW",
    "AZELNE",
    "JNAT",
    "ECLIPTIC",
    "MECLIPTIC",
    "TECLIPTIC",
    "SUPERGAL",
    "IGRF" };

  static const MEarthMagnetic::Types oname[N_name] = {
    MEarthMagnetic::ITRF,
    MEarthMagnetic::J2000,
    MEarthMagnetic::JMEAN,
    MEarthMagnetic::JTRUE,
    MEarthMagnetic::APP,
    MEarthMagnetic::B1950,
    MEarthMagnetic::BMEAN,
    MEarthMagnetic::BTRUE,
    MEarthMagnetic::GALACTIC,
    MEarthMagnetic::HADEC,
    MEarthMagnetic::AZEL,
    MEarthMagnetic::AZELSW,
    MEarthMagnetic::AZEL,
    MEarthMagnetic::JNAT,
    MEarthMagnetic::ECLIPTIC,
    MEarthMagnetic::MECLIPTIC,
    MEarthMagnetic::TECLIPTIC,
    MEarthMagnetic::SUPERGAL,
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

uInt MEarthMagnetic::myType() {
  return Register((MEarthMagnetic *)0);
}

Quantum<Vector<Double> > MEarthMagnetic::get(const Unit &inunit) const {
    return Quantum<Vector<Double> >(data.getValue(),"T").get(inunit);
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
