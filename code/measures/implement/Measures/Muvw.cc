//# Muvw.cc:  A Measure: uvw on Earth
//# Copyright (C) 1998
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
typedef Quantum<Double> gpp_muvw_bug1;
#endif
#include <aips/Measures/Muvw.h>
#include <aips/Exceptions.h>
#include <aips/Arrays/Vector.h>
#include <aips/Mathematics/Math.h>
#include <aips/RTTI/Register.h>
#include <aips/Utilities/Assert.h>

//# Constructors
Muvw::Muvw() :
  MeasBase<MVuvw,Muvw::Ref>() {}

Muvw::Muvw(const MVuvw &dt) : 
  MeasBase<MVuvw,Muvw::Ref>(dt,Muvw::DEFAULT) {}

Muvw::Muvw(const MVuvw &dt, const Muvw::Ref &rf) : 
  MeasBase<MVuvw,Muvw::Ref>(dt,rf) {}

Muvw::Muvw(const MVuvw &dt, uInt rf) : 
  MeasBase<MVuvw,Muvw::Ref>(dt,rf) {}

Muvw::Muvw(const Measure *dt) :
  MeasBase<MVuvw,Muvw::Ref>(dt) {}

Muvw::Muvw(const MeasValue *dt) :
  MeasBase<MVuvw,Muvw::Ref>(*(MVuvw*)dt,
			    Muvw::DEFAULT) {}

Muvw::Muvw(const Muvw &other)
  : MeasBase<MVuvw, MeasRef<Muvw> > (other) {}

Muvw &Muvw::operator=(const Muvw &other) {
  if (this != &other) {
    MeasBase<MVuvw, MeasRef<Muvw> > &This = *this;
    const MeasBase<MVuvw, MeasRef<Muvw> > &Other = other;
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
  return Register((Muvw *)0);
}

void Muvw::assert(const Measure &in) {
  if (in.type() != Register((Muvw *)0)) {
    throw(AipsError("Illegal Measure type argument: " +
		    Muvw::showMe()));
  };
}

const String &Muvw::showType(uInt tp) {
  static const String tname[Muvw::N_Types] = {
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
  
  DebugAssert(tp < Muvw::N_Types, AipsError);
  return tname[tp];
}

const String *const Muvw::allMyTypes(Int &nall, Int &nextra,
					  const uInt *&typ) {
  static const Int N_name  = 18;
  static const Int N_extra = 0;
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
    "SUPERGAL" };
  
  static const uInt oname[N_name] = {
    Muvw::ITRF,
    Muvw::J2000,
    Muvw::JMEAN,
    Muvw::JTRUE,
    Muvw::APP,
    Muvw::B1950,
    Muvw::BMEAN,
    Muvw::BTRUE,
    Muvw::GALACTIC,
    Muvw::HADEC,
    Muvw::AZEL,
    Muvw::AZELSW,
    Muvw::AZEL,
    Muvw::JNAT,
    Muvw::ECLIPTIC,
    Muvw::MECLIPTIC,
    Muvw::TECLIPTIC,
    Muvw::SUPERGAL };

  nall   = N_name;
  nextra = N_extra;
  typ    = oname;
  return tname;
}

const String *const Muvw::allTypes(Int &nall, Int &nextra,
					const uInt *&typ) const {
  return Muvw::allMyTypes(nall, nextra, typ);
}

Bool Muvw::getType(Muvw::Types &tp, const String &in) {
  const uInt *oname;
  Int nall, nex;
  const String *tname = Muvw::allMyTypes(nall, nex, oname);
  
  Int i = Measure::giveMe(in, nall, tname);
  
  if (i>=nall) {
    return False;
  } else {
    tp = (Muvw::Types) oname[i];
  };
  return True;
}

Bool Muvw::giveMe(Muvw::Ref &mr, const String &in) {
  Muvw::Types tp;
  if (Muvw::getType(tp, in)) {
    mr = Muvw::Ref(tp);
  } else {
    mr = Muvw::Ref();
    return False;
  };
  return True;
};

Bool Muvw::setOffset(const Measure &in) {
  if (in.type() != Register((Muvw *)0)) return False;
  ref.set(in);
  return True;
}

Bool Muvw::setRefString(const String &in) {
  Muvw::Types tp;
  if (Muvw::getType(tp, in)) {
    ref.setType(tp);
    return True;
  };
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
  return Register((Muvw *)0);
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
