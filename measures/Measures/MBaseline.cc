//# MBaseline.cc:  A Measure: Baseline on Earth
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

//# Includes
#include <casacore/measures/Measures/MBaseline.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/casa/Exceptions.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors
MBaseline::MBaseline() :
  MeasBase<MVBaseline, MBaseline::Ref>() {}

MBaseline::MBaseline(const MVBaseline &dt) : 
  MeasBase<MVBaseline, MBaseline::Ref>(dt,MBaseline::DEFAULT) {}

MBaseline::MBaseline(const MVBaseline &dt, const MBaseline::Ref &rf) : 
  MeasBase<MVBaseline, MBaseline::Ref>(dt,rf) {}

MBaseline::MBaseline(const MVBaseline &dt, MBaseline::Types  rf) : 
  MeasBase<MVBaseline, MBaseline::Ref>(dt,rf) {}

MBaseline::MBaseline(const Measure *dt) :
  MeasBase<MVBaseline, MBaseline::Ref>(dt) {}

MBaseline::MBaseline(const MeasValue *dt) :
  MeasBase<MVBaseline, MBaseline::Ref>(*(MVBaseline*)dt,
				       MBaseline::DEFAULT) {}

MBaseline::MBaseline(const MBaseline &other)
  : MeasBase<MVBaseline, MBaseline::Ref> (other) {}

MBaseline &MBaseline::operator=(const MBaseline &other) {
  if (this != &other) {
    MeasBase<MVBaseline, MBaseline::Ref> &This = *this;
    const MeasBase<MVBaseline, MBaseline::Ref> &Other = other;
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

void MBaseline::assure(const Measure& in) {
  if (!dynamic_cast<const MBaseline*>(&in)) {
    throw(AipsError("Illegal Measure type argument: " +
		    MBaseline::showMe()));
  }
}

MBaseline::Types MBaseline::castType(uint32_t tp) {
  MBaseline::checkMyTypes();
  AlwaysAssert(tp < MBaseline::N_Types, AipsError);
  return static_cast<MBaseline::Types>(tp);
}

const String &MBaseline::showType(MBaseline::Types tp) {
  static const String tname[MBaseline::N_Types] = {
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

  MBaseline::checkMyTypes();
  return tname[tp];
}

const String &MBaseline::showType(uint32_t tp) {
  return MBaseline::showType(MBaseline::castType(tp));
}

const String* MBaseline::allMyTypes(int32_t &nall, int32_t &nextra,
                                    const uint32_t *&typ) {
  static const int32_t N_name  = 24;
  static const int32_t N_extra = 0;
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
  
  static const uint32_t oname[N_name] = {
    MBaseline::J2000,
    MBaseline::JMEAN,
    MBaseline::JTRUE,
    MBaseline::APP,
    MBaseline::B1950,
    MBaseline::B1950_VLA,
    MBaseline::BMEAN,
    MBaseline::BTRUE,
    MBaseline::GALACTIC,
    MBaseline::HADEC,
    MBaseline::AZEL,
    MBaseline::AZELSW,
    MBaseline::AZEL,
    MBaseline::AZELGEO,
    MBaseline::AZELSWGEO,
    MBaseline::AZELGEO,
    MBaseline::JNAT,
    MBaseline::ECLIPTIC,
    MBaseline::MECLIPTIC,
    MBaseline::TECLIPTIC,
    MBaseline::SUPERGAL,
    MBaseline::ITRF,
    MBaseline::TOPO,
    MBaseline::ICRS };

  MBaseline::checkMyTypes();
  nall   = N_name;
  nextra = N_extra;
  typ    = oname;
  return tname;
}

const String* MBaseline::allTypes(int32_t &nall, int32_t &nextra,
                                  const uint32_t *&typ) const {
  return MBaseline::allMyTypes(nall, nextra, typ);
}

void MBaseline::checkTypes() const {
  MBaseline::checkMyTypes();
}

void MBaseline::checkMyTypes() {
  // Multiple threads could execute this, but that is harmless.
  static bool first(true);
  if (first) {
    first = false;
    int32_t nall, nex;
    const uint32_t *typ;
    const String *const tps = MBaseline::allMyTypes(nall,nex, typ);
    MBaseline::Types tp;
    for (int32_t i=0; i<nall; i++) {
      AlwaysAssert(MBaseline::getType(tp, MBaseline::showType(typ[i])) &&
		   tp == int32_t(typ[i]) &&
		   MBaseline::getType(tp, tps[i]) &&
		   tp == int32_t(typ[i]), AipsError);
    }
    for (int32_t i=0; i<N_Types; i++) {
      AlwaysAssert(MBaseline::getType(tp, MBaseline::showType(i)) &&
		   tp == i, AipsError);
    }
    // Check if baseline types are identical to direction types
    AlwaysAssert(static_cast<int32_t>(MBaseline::N_Types) == 
		 static_cast<int32_t>(MDirection::N_Types), AipsError);
    for (int32_t i=0; i<N_Types; i++) {
      AlwaysAssert(MBaseline::showType(i) == MDirection::showType(i),
		   AipsError);
    }
  }
}

MBaseline::Types MBaseline::fromDirType(const MDirection::Types in) {
  MBaseline::checkMyTypes();
  return static_cast<MBaseline::Types>(static_cast<uint32_t>(in));
}

MDirection::Types MBaseline::toDirType(const MBaseline::Types in) {
  MBaseline::checkMyTypes();
  return static_cast<MDirection::Types>(static_cast<uint32_t>(in));
}

bool MBaseline::getType(MBaseline::Types &tp, const String &in) {
  const uint32_t *oname;
  int32_t nall, nex;
  const String *tname = MBaseline::allMyTypes(nall, nex, oname);
  
  int32_t i = Measure::giveMe(in, nall, tname);
  
  if (i>=nall) return false;
  else tp = static_cast<MBaseline::Types>(oname[i]);
  return true;
}

bool MBaseline::giveMe(MBaseline::Ref &mr, const String &in) {
  MBaseline::Types tp;
  if (MBaseline::getType(tp, in)) mr = MBaseline::Ref(tp);
  else {
    mr = MBaseline::Ref();
    return false;
  }
  return true;
}

bool MBaseline::setOffset(const Measure &in) {
  if (!dynamic_cast<const MBaseline*>(&in)) return false;
  ref.set(in);
  return true;
}

bool MBaseline::setRefString(const String &in) {
  MBaseline::Types tp;
  if (MBaseline::getType(tp, in)) {
    ref.setType(tp);
    return true;
  }
  ref.setType(MBaseline::DEFAULT);
  return false;
}

const String &MBaseline::getDefaultType() const {
  return MBaseline::showType(MBaseline::DEFAULT);
}

String MBaseline::getRefString() const {
  return MBaseline::showType(ref.getType());
}

Quantum<Vector<double> > MBaseline::get(const Unit &inunit) const {
    return Quantum<Vector<double> >(data.getValue(),"m").get(inunit);
}

Quantum<Vector<double> > MBaseline::getAngle() const {
    return (data.getAngle());
}

Quantum<Vector<double> > MBaseline::getAngle(const Unit &inunit) const {
    return (data.getAngle(inunit));
}

Measure *MBaseline::clone() const {
    return (new MBaseline(*this));
}

} //# NAMESPACE CASACORE - END

