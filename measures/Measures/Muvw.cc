//# Muvw.cc:  A Measure: uvw on Earth
//# Copyright (C) 1998-2002,2004,2007
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
#include <casacore/measures/Measures/Muvw.h>
#include <casacore/casa/Exceptions.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Register.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors
Muvw::Muvw() :
  MeasBase<MVuvw, Muvw::Ref>() {}

Muvw::Muvw(const MVuvw &dt) : 
  MeasBase<MVuvw, Muvw::Ref>(dt,Muvw::DEFAULT) {}

Muvw::Muvw(const MVuvw &dt, const Muvw::Ref &rf) : 
  MeasBase<MVuvw, Muvw::Ref>(dt,rf) {}

Muvw::Muvw(const MVuvw &dt, Muvw::Types rf) : 
  MeasBase<MVuvw, Muvw::Ref>(dt,rf) {}

Muvw::Muvw(const Measure *dt) :
  MeasBase<MVuvw, Muvw::Ref>(dt) {}

Muvw::Muvw(const MeasValue *dt) :
  MeasBase<MVuvw, Muvw::Ref>(*(MVuvw*)dt, Muvw::DEFAULT) {}

Muvw::Muvw(const Muvw &other) :
  MeasBase<MVuvw, Muvw::Ref> (other) {}

Muvw &Muvw::operator=(const Muvw &other) {
  if (this != &other) {
    MeasBase<MVuvw, Muvw::Ref> &This = *this;
    const MeasBase<MVuvw, Muvw::Ref> &Other = other;
    This = Other;
  }
  return *this;
}

//# Destructor
Muvw::~Muvw() {}

//# Operators

//# Member functions

const String &Muvw::tellMe() const {
    return Muvw::showMe();
}

const String &Muvw::showMe() {
    static const String name("uvw");
    return name;
}

uInt Muvw::type() const {
  return Register(static_cast<Muvw *>(0));
}

void Muvw::assure(const Measure &in) {
  if (in.type() != Register(static_cast<Muvw *>(0))) {
    throw(AipsError("Illegal Measure type argument: " +
		    Muvw::showMe()));
  }
}

Muvw::Types Muvw::castType(uInt tp) {
  Muvw::checkMyTypes();
  AlwaysAssert(tp < Muvw::N_Types, AipsError);
  return static_cast<Muvw::Types>(tp);
}

const String &Muvw::showType(Muvw::Types tp) {
  static const String tname[Muvw::N_Types] = {
    "J2000",
    "JMEAN",
    "JTRUE",
    "APP",
    "B1950",
    "B1950_VLA",
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

  Muvw::checkMyTypes();
  return tname[tp];
}

const String &Muvw::showType(uInt tp) {
  return Muvw::showType(Muvw::castType(tp));
}

const String* Muvw::allMyTypes(Int &nall, Int &nextra,
                               const uInt *&typ) {
  static const Int N_name  = 24;
  static const Int N_extra = 0;
  static const String tname[N_name] = {
    "J2000",
    "JMEAN",
    "JTRUE",
    "APP",
    "B1950",
    "B1950_VLA",
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
    "ICRS" };
  
  static const uInt oname[N_name] = {
    Muvw::J2000,
    Muvw::JMEAN,
    Muvw::JTRUE,
    Muvw::APP,
    Muvw::B1950,
    Muvw::B1950_VLA,
    Muvw::BMEAN,
    Muvw::BTRUE,
    Muvw::GALACTIC,
    Muvw::HADEC,
    Muvw::AZEL,
    Muvw::AZELSW,
    Muvw::AZEL,
    Muvw::AZELGEO,
    Muvw::AZELSWGEO,
    Muvw::AZELGEO,
    Muvw::JNAT,
    Muvw::ECLIPTIC,
    Muvw::MECLIPTIC,
    Muvw::TECLIPTIC,
    Muvw::SUPERGAL,
    Muvw::ITRF,
    Muvw::TOPO,
    Muvw::ICRS };

  Muvw::checkMyTypes();
  nall   = N_name;
  nextra = N_extra;
  typ    = oname;
  return tname;
}

const String* Muvw::allTypes(Int &nall, Int &nextra,
                             const uInt *&typ) const {
  return Muvw::allMyTypes(nall, nextra, typ);
}

void Muvw::checkTypes() const {
  Muvw::checkMyTypes();
}

void Muvw::checkMyTypes() {
  // Multiple threads could execute this, but that is harmless.
  static Bool first(True);
  if (first) {
    first = False;
    Int nall, nex;
    const uInt *typ;
    const String *const tps = Muvw::allMyTypes(nall,nex, typ);
    Muvw::Types tp;
    for (Int i=0; i<nall; i++) {
      AlwaysAssert(Muvw::getType(tp, Muvw::showType(typ[i])) &&
		   tp == Int(typ[i]) &&
		   Muvw::getType(tp, tps[i]) &&
		   tp == Int(typ[i]), AipsError);
    }
    for (Int i=0; i<N_Types; i++) {
      AlwaysAssert(Muvw::getType(tp, Muvw::showType(i)) &&
		   tp == i, AipsError);
    }
    // Check if uvw types are identical to direction types
    AlwaysAssert(static_cast<Int>(Muvw::N_Types) == 
		 static_cast<Int>(MDirection::N_Types), AipsError);
    for (Int i=0; i<N_Types; i++) {
      AlwaysAssert(Muvw::showType(i) == MDirection::showType(i),
		   AipsError);
    }
  }
}

Muvw::Types Muvw::fromDirType(const MDirection::Types in) {
  Muvw::checkMyTypes();
  return static_cast<Muvw::Types>(static_cast<uInt>(in));
}

MDirection::Types Muvw::toDirType(const Muvw::Types in) {
  Muvw::checkMyTypes();
  return static_cast<MDirection::Types>(static_cast<uInt>(in));
}

Bool Muvw::getType(Muvw::Types &tp, const String &in) {
  const uInt *oname;
  Int nall, nex;
  const String *tname = Muvw::allMyTypes(nall, nex, oname);
  
  Int i = Measure::giveMe(in, nall, tname);
  
  if (i>=nall) return False;
  else tp = static_cast<Muvw::Types>(oname[i]);
  return True;
}

Bool Muvw::giveMe(Muvw::Ref &mr, const String &in) {
  Muvw::Types tp;
  if (Muvw::getType(tp, in)) mr = Muvw::Ref(tp);
  else {
    mr = Muvw::Ref();
    return False;
  }
  return True;
}

Bool Muvw::setOffset(const Measure &in) {
  if (in.type() != Register(static_cast<Muvw *>(0))) return False;
  ref.set(in);
  return True;
}

Bool Muvw::setRefString(const String &in) {
  Muvw::Types tp;
  if (Muvw::getType(tp, in)) {
    ref.setType(tp);
    return True;
  }
  ref.setType(Muvw::DEFAULT);
  return False;
}

const String &Muvw::getDefaultType() const {
  return Muvw::showType(Muvw::DEFAULT);
}

String Muvw::getRefString() const {
  return Muvw::showType(ref.getType());
}

uInt Muvw::myType() {
  return Register(static_cast<Muvw *>(0));
}

Quantum<Vector<Double> > Muvw::get(const Unit &inunit) const {
  Vector<Double> x;
  x = data.getValue();
  return Quantum<Vector<Double> >(x, "m").get(inunit);
}

Quantum<Vector<Double> > Muvw::getAngle() const {
    return (data.getAngle());
}

Quantum<Vector<Double> > Muvw::getAngle(const Unit &inunit) const {
    return (data.getAngle(inunit));
}

Measure *Muvw::clone() const {
    return (new Muvw(*this));
}

} //# NAMESPACE CASACORE - END

