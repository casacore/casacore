//# MBaseline.cc:  A Measure: Baseline on Earth
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
typedef Quantum<Double> gpp_mBaseline_bug1;
#endif
#include <aips/Measures/MBaseline.h>
#include <aips/Exceptions.h>
#include <aips/Arrays/Vector.h>
#include <aips/Mathematics/Math.h>
#include <aips/RTTI/Register.h>
#include <aips/Utilities/Assert.h>

//# Constructors
MBaseline::MBaseline() :
  MeasBase<MVBaseline,MBaseline::Ref>() {}

MBaseline::MBaseline(const MVBaseline &dt) : 
  MeasBase<MVBaseline,MBaseline::Ref>(dt,MBaseline::DEFAULT) {}

MBaseline::MBaseline(const MVBaseline &dt, const MBaseline::Ref &rf) : 
  MeasBase<MVBaseline,MBaseline::Ref>(dt,rf) {}

MBaseline::MBaseline(const MVBaseline &dt, uInt rf) : 
  MeasBase<MVBaseline,MBaseline::Ref>(dt,rf) {}

MBaseline::MBaseline(const Measure *dt) :
  MeasBase<MVBaseline,MBaseline::Ref>(dt) {}

MBaseline::MBaseline(const MeasValue *dt) :
  MeasBase<MVBaseline,MBaseline::Ref>(*(MVBaseline*)dt,
				      MBaseline::DEFAULT) {}

MBaseline::MBaseline(const MBaseline &other)
  : MeasBase<MVBaseline, MeasRef<MBaseline> > (other) {}

MBaseline &MBaseline::operator=(const MBaseline &other) {
  if (this != &other) {
    MeasBase<MVBaseline, MeasRef<MBaseline> > &This = *this;
    const MeasBase<MVBaseline, MeasRef<MBaseline> > &Other = other;
    This = Other;
  }
  return *this;
}

//# Destructor
MBaseline::~MBaseline() {}

//# Operators

//# Member functions

const String &MBaseline::tellMe() const {
    return MBaseline::showMe();
}

const String &MBaseline::showMe() {
    static const String name("Baseline");
    return name;
}

uInt MBaseline::type() const {
  return Register((MBaseline *)0);
}

void MBaseline::assert(const Measure &in) {
  if (in.type() != Register((MBaseline *)0)) {
    throw(AipsError("Illegal Measure type argument: " +
		    MBaseline::showMe()));
  };
}

const String &MBaseline::showType(uInt tp) {
  static const String tname[MBaseline::N_Types] = {
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
  
  DebugAssert(tp < MBaseline::N_Types, AipsError);
  return tname[tp];
}

Bool MBaseline::getType(MBaseline::Types &tp, const String &in) {
  static const Int N_name = 18;
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
  
  static const MBaseline::Types oname[N_name] = {
    MBaseline::ITRF,
    MBaseline::J2000,
    MBaseline::JMEAN,
    MBaseline::JTRUE,
    MBaseline::APP,
    MBaseline::B1950,
    MBaseline::BMEAN,
    MBaseline::BTRUE,
    MBaseline::GALACTIC,
    MBaseline::HADEC,
    MBaseline::AZEL,
    MBaseline::AZELSW,
    MBaseline::AZEL,
    MBaseline::JNAT,
    MBaseline::ECLIPTIC,
    MBaseline::MECLIPTIC,
    MBaseline::TECLIPTIC,
    MBaseline::SUPERGAL };
  
  uInt i = Measure::giveMe(in, N_name, tname);
  
  if (i>=N_name) {
    return False;
  } else {
    tp = oname[i];
  };
  return True;
}

Bool MBaseline::giveMe(MBaseline::Ref &mr, const String &in) {
  MBaseline::Types tp;
  if (MBaseline::getType(tp, in)) {
    mr = MBaseline::Ref(tp);
  } else {
    mr = MBaseline::Ref();
    return False;
  };
  return True;
};

Bool MBaseline::setOffset(const Measure &in) {
  if (in.type() != Register((MBaseline *)0)) return False;
  ref.set(in);
  return True;
}

Bool MBaseline::setRefString(const String &in) {
  MBaseline::Types tp;
  if (MBaseline::getType(tp, in)) {
    ref.setType(tp);
    return True;
  };
  ref.setType(MBaseline::DEFAULT);
  return False;
}

const String &MBaseline::getDefaultType() const {
  return MBaseline::showType(MBaseline::DEFAULT);
}

String MBaseline::getRefString() const {
  return MBaseline::showType(ref.getType());
}

uInt MBaseline::myType() {
  return Register((MBaseline *)0);
}

Quantum<Vector<Double> > MBaseline::get(const Unit &inunit) const {
    return Quantum<Vector<Double> >(data.getValue(),"m").get(inunit);
}

Quantum<Vector<Double> > MBaseline::getAngle() const {
    return (data.getAngle());
}

Quantum<Vector<Double> > MBaseline::getAngle(const Unit &inunit) const {
    return (data.getAngle(inunit));
}

Measure *MBaseline::clone() const {
    return (new MBaseline(*this));
}
