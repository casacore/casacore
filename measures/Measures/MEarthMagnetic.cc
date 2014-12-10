//# MEarthMagnetic.cc: A Measure: Magnetic field on Earth
//# Copyright (C) 1995-1999,2000,2001,2002,2004
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
#include <casacore/measures/Measures/MEarthMagnetic.h>
#include <casacore/casa/Exceptions.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Register.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Quanta/RotMatrix.h>
#include <casacore/casa/Quanta/Euler.h>
#include <casacore/casa/Quanta/MVPosition.h>
#include <casacore/casa/Quanta/MVDirection.h>
#include <casacore/casa/Quanta/QMath.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors
MEarthMagnetic::MEarthMagnetic() :
  MeasBase<MVEarthMagnetic, MEarthMagnetic::Ref>() {}

MEarthMagnetic::MEarthMagnetic(const MVEarthMagnetic &dt) : 
  MeasBase<MVEarthMagnetic, MEarthMagnetic::Ref>(dt,MEarthMagnetic::DEFAULT) {}

MEarthMagnetic::MEarthMagnetic(const MVEarthMagnetic &dt,
			       const MEarthMagnetic::Ref &rf) : 
  MeasBase<MVEarthMagnetic, MEarthMagnetic::Ref>(dt,rf) {}

MEarthMagnetic::MEarthMagnetic(const MVEarthMagnetic &dt,
			       MEarthMagnetic::Types rf) : 
  MeasBase<MVEarthMagnetic, MEarthMagnetic::Ref>(dt,rf) {}

MEarthMagnetic::MEarthMagnetic(const Measure *dt) :
  MeasBase<MVEarthMagnetic, MEarthMagnetic::Ref>(dt) {}

MEarthMagnetic::MEarthMagnetic(const MeasValue *dt) :
  MeasBase<MVEarthMagnetic, MEarthMagnetic::Ref>(*(MVEarthMagnetic*)dt,
						 MEarthMagnetic::DEFAULT) {}

MEarthMagnetic::MEarthMagnetic(const MEarthMagnetic::Ref &rf) :
  MeasBase<MVEarthMagnetic, MEarthMagnetic::Ref>(rf) {}

MEarthMagnetic::MEarthMagnetic(const MEarthMagnetic &other) :
  MeasBase<MVEarthMagnetic, MEarthMagnetic::Ref> (other) {}

MEarthMagnetic &MEarthMagnetic::operator=(const MEarthMagnetic &other) {
  if (this != &other) {
    MeasBase<MVEarthMagnetic, MEarthMagnetic::Ref> &This = *this;
    const MeasBase<MVEarthMagnetic, MEarthMagnetic::Ref> &Other = other;
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
  return Register(static_cast<MEarthMagnetic *>(0));
}

void MEarthMagnetic::assure(const Measure &in) {
  if (in.type() != Register(static_cast<MEarthMagnetic *>(0))) {
    throw(AipsError("Illegal Measure type argument: " +
		    MEarthMagnetic::showMe()));
  }
}

MEarthMagnetic::Types MEarthMagnetic::castType(uInt tp) {
  MEarthMagnetic::checkMyTypes();
  if ((tp & MEarthMagnetic::EXTRA) == 0) {
    AlwaysAssert(tp < MEarthMagnetic::N_Types, AipsError);
  } else {
    AlwaysAssert((tp & ~MEarthMagnetic::EXTRA) < 
		 (MEarthMagnetic::N_Models - MEarthMagnetic::IGRF),
		 AipsError);
  }
  return static_cast<MEarthMagnetic::Types>(tp);
}

const String &MEarthMagnetic::showType(MEarthMagnetic::Types tp) {
  static const String tname[MEarthMagnetic::N_Types] = {
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
    "AZELGEO",
    "AZELSWGEO",
    "JNAT",
    "ECLIPTIC",
    "MECLIPTIC",
    "TECLIPTIC",
    "SUPERGAL",
    "ITRF",
    "TOPO",
    "ICRS" };
  static const String pname[MEarthMagnetic::N_Models -
			   MEarthMagnetic::IGRF] = {
    "IGRF" };

  MEarthMagnetic::checkMyTypes();
  if ((tp & MEarthMagnetic::EXTRA) == 0) return tname[tp];
  return pname[tp & ~MEarthMagnetic::EXTRA];
}

const String &MEarthMagnetic::showType(uInt tp) {
  return MEarthMagnetic::showType(MEarthMagnetic::castType(tp));
}

const String* MEarthMagnetic::allMyTypes(Int &nall, Int &nextra,
                                         const uInt *&typ) {
  static const Int N_name  = 24;
  static const Int N_extra = 0;
  static const String tname[N_name] = {
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
    "AZELGEO",
    "AZELSWGEO",
    "AZELNEGEO",
    "JNAT",
    "ECLIPTIC",
    "MECLIPTIC",
    "TECLIPTIC",
    "SUPERGAL",
    "ITRF",
    "TOPO",
    "ICRS",
    "IGRF" };

  static const uInt oname[N_name] = {
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
    MEarthMagnetic::AZELGEO,
    MEarthMagnetic::AZELSWGEO,
    MEarthMagnetic::AZELGEO,
    MEarthMagnetic::JNAT,
    MEarthMagnetic::ECLIPTIC,
    MEarthMagnetic::MECLIPTIC,
    MEarthMagnetic::TECLIPTIC,
    MEarthMagnetic::SUPERGAL,
    MEarthMagnetic::ITRF,
    MEarthMagnetic::TOPO,
    MEarthMagnetic::ICRS,
    MEarthMagnetic::IGRF};

  MEarthMagnetic::checkMyTypes();
  nall   = N_name;
  nextra = N_extra;
  typ    = oname;
  return tname;
}

const String* MEarthMagnetic::allTypes(Int &nall, Int &nextra,
                                       const uInt *&typ) const {
  return MEarthMagnetic::allMyTypes(nall, nextra, typ);
}

Bool MEarthMagnetic::getType(MEarthMagnetic::Types &tp, const String &in) {
  const uInt *oname;
  Int nall, nex;
  const String *tname = MEarthMagnetic::allMyTypes(nall, nex, oname);
  
  Int i = Measure::giveMe(in, nall, tname);
  
  if (i>=nall) return False;
  else tp = static_cast<MEarthMagnetic::Types>(oname[i]);
  return True;
}

void MEarthMagnetic::checkTypes() const {
  MEarthMagnetic::checkMyTypes();
}

void MEarthMagnetic::checkMyTypes() {
  // Multiple threads could execute this, but that is harmless.
  static Bool first(True);
  if (first) {
    first = False;
    Int nall, nex;
    const uInt *typ;
    const String *const tps = MEarthMagnetic::allMyTypes(nall,nex, typ);
    MEarthMagnetic::Types tp;
    for (Int i=0; i<nall; i++) {
      AlwaysAssert(MEarthMagnetic::getType(tp, MEarthMagnetic::showType(typ[i])) &&
		   tp == Int(typ[i]) &&
		   MEarthMagnetic::getType(tp, tps[i]) &&
		   tp == Int(typ[i]), AipsError);
    }
    for (Int i=0; i<N_Types; i++) {
      AlwaysAssert(MEarthMagnetic::getType(tp, MEarthMagnetic::showType(i)) &&
		   tp == i, AipsError);
    }
    for (Int i=IGRF; i<N_Models; i++) {
      AlwaysAssert(MEarthMagnetic::getType(tp, MEarthMagnetic::showType(i)) &&
		   tp == i, AipsError);
    }
  }
}

Bool MEarthMagnetic::giveMe(MEarthMagnetic::Ref &mr, const String &in) {
  MEarthMagnetic::Types tp;
  if (MEarthMagnetic::getType(tp, in)) mr = MEarthMagnetic::Ref(tp);
  else {
    mr = MEarthMagnetic::Ref();
    return False;
  }
  return True;
}

Bool MEarthMagnetic::setOffset(const Measure &in) {
  if (in.type() != Register(static_cast<MEarthMagnetic *>(0))) return False;
  ref.set(in);
  return True;
}

Bool MEarthMagnetic::setRefString(const String &in) {
  MEarthMagnetic::Types tp;
  if (MEarthMagnetic::getType(tp, in)) {
    ref.setType(tp);
    return True;
  }
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
  return Register(static_cast<MEarthMagnetic *>(0));
}

Bool MEarthMagnetic::isModel() const {
  return ((ref.getType() & MEarthMagnetic::EXTRA) != 0);
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

} //# NAMESPACE CASACORE - END

