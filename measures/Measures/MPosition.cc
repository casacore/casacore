//# MPosition.cc:  A Measure: position on Earth
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001
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
#include <casacore/casa/Exceptions.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors
MPosition::MPosition() :
  MeasBase<MVPosition, MPosition::Ref>() {}

MPosition::MPosition(const MVPosition &dt) : 
  MeasBase<MVPosition, MPosition::Ref>(dt,MPosition::DEFAULT) {}

MPosition::MPosition(const MVPosition &dt, const MPosition::Ref &rf) : 
  MeasBase<MVPosition, MPosition::Ref>(dt,rf) {}

MPosition::MPosition(const MVPosition &dt, MPosition::Types rf) : 
  MeasBase<MVPosition, MPosition::Ref>(dt,rf) {}

MPosition::MPosition(const Quantity &dt, const Quantity &dt1,
		     const Quantity &dt2) : 
  MeasBase<MVPosition, MPosition::Ref>(MVPosition(dt,dt1,dt2),
				       MPosition::DEFAULT) {}

MPosition::MPosition(const Quantity &dt, const Quantity &dt1,
		     const Quantity &dt2, const MPosition::Ref &rf) : 
  MeasBase<MVPosition, MPosition::Ref>(MVPosition(dt,dt1,dt2),rf) {}

MPosition::MPosition(const Quantity &dt, const Quantity &dt1,
		     const Quantity &dt2, MPosition::Types rf) : 
  MeasBase<MVPosition, MPosition::Ref>(MVPosition(dt,dt1,dt2),rf) {}

MPosition::MPosition(const Quantity &dt0, const Quantum<Vector<double> > &dt) :
  MeasBase<MVPosition, MPosition::Ref>(MVPosition(dt0,dt),
				       MPosition::DEFAULT) {}

MPosition::MPosition(const Quantity &dt0, const Quantum<Vector<double> > &dt,
		     const MPosition::Ref &rf) : 
  MeasBase<MVPosition, MPosition::Ref>(MVPosition(dt0,dt),rf) {}

MPosition::MPosition(const Quantity &dt0, const Quantum<Vector<double> > &dt,
		     MPosition::Types rf) : 
  MeasBase<MVPosition, MPosition::Ref>(MVPosition(dt0,dt),rf) {}

MPosition::MPosition(const Measure *dt) :
  MeasBase<MVPosition, MPosition::Ref>(dt) {}

MPosition::MPosition(const MeasValue *dt) :
  MeasBase<MVPosition, MPosition::Ref>(*(MVPosition*)dt, MPosition::DEFAULT) {}

MPosition::MPosition(const MPosition &other) :
  MeasBase<MVPosition, MPosition::Ref> (other) {}

MPosition &MPosition::operator=(const MPosition &other) {
  if (this != &other) {
    MeasBase<MVPosition, MPosition::Ref> &This = *this;
    const MeasBase<MVPosition, MPosition::Ref> &Other = other;
    This = Other;
  }
  return *this;
}

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

void MPosition::assure(const Measure &in) {
  if (!dynamic_cast<const MPosition*>(&in)) {
    throw(AipsError("Illegal Measure type argument: " +
		    MPosition::showMe()));
  }
}

const String* MPosition::allMyTypes(int32_t &nall, int32_t &nextra,
                                    const uint32_t *&typ) {
  static const int32_t N_name  = 2;
  static const int32_t N_extra = 0;
  static const String tname[N_name] = {
    "ITRF",
    "WGS84" };
  
  static const uint32_t oname[N_name] = {
    MPosition::ITRF,
    MPosition::WGS84 };

  MPosition::checkMyTypes();
  nall   = N_name;
  nextra = N_extra;
  typ    = oname;
  return tname;
}

const String* MPosition::allTypes(int32_t &nall, int32_t &nextra,
                                  const uint32_t *&typ) const {
  return MPosition::allMyTypes(nall, nextra, typ);
}

void MPosition::checkTypes() const {
  MPosition::checkMyTypes();
}

void MPosition::checkMyTypes() {
  // Multiple threads could execute this, but that is harmless.
  static bool first(true);
  if (first) {
    first = false;
    int32_t nall, nex;
    const uint32_t *typ;
    const String *const tps = MPosition::allMyTypes(nall,nex, typ);
    MPosition::Types tp;
    for (int32_t i=0; i<nall; i++) {
      AlwaysAssert(MPosition::getType(tp, MPosition::showType(typ[i])) &&
		   tp == int32_t(typ[i]) &&
		   MPosition::getType(tp, tps[i]) &&
		   tp == int32_t(typ[i]), AipsError);
    }
    for (int32_t i=0; i<N_Types; i++) {
      AlwaysAssert(MPosition::getType(tp, MPosition::showType(i)) &&
		   tp == i, AipsError);
    }
  }
}

MPosition::Types MPosition::castType(uint32_t tp) {
  MPosition::checkMyTypes();
  AlwaysAssert(tp < MPosition::N_Types, AipsError);
  return static_cast<MPosition::Types>(tp);
}

const String &MPosition::showType(MPosition::Types tp) {
  static const String tname[MPosition::N_Types] = {
    "ITRF",
    "WGS84"};

  MPosition::checkMyTypes();
  return tname[tp];
}

const String &MPosition::showType(uint32_t tp) {
  return MPosition::showType(MPosition::castType(tp));
}

bool MPosition::getType(MPosition::Types &tp, const String &in) {
  const uint32_t *oname;
  int32_t nall, nex;
  const String *tname = MPosition::allMyTypes(nall, nex, oname);
  
  int32_t i = Measure::giveMe(in, nall, tname);
  
  if (i>=nall) return false;
  else tp = static_cast<MPosition::Types>(oname[i]);
  return true;
}

MPosition::Types MPosition::getType(const String& in) {
	Types myType;
	if (! getType(myType, in)) {
		throw AipsError("MPosition::Types: Unrecognized type string " + in);
	}
	return myType;
}

bool MPosition::giveMe(MPosition::Ref &mr, const String &in) {
  MPosition::Types tp;
  if (MPosition::getType(tp, in)) mr = MPosition::Ref(tp);
  else {
    mr = MPosition::Ref();
    return false;
  }
  return true;
}

bool MPosition::setOffset(const Measure &in) {
  if (!dynamic_cast<const MPosition*>(&in)) return false;
  ref.set(in);
  return true;
}

bool MPosition::setRefString(const String &in) {
  MPosition::Types tp;
  if (MPosition::getType(tp, in)) {
    ref.setType(tp);
    return true;
  }
  ref.setType(MPosition::DEFAULT);
  return false;
}

const String &MPosition::getDefaultType() const {
  return MPosition::showType(MPosition::DEFAULT);
}

String MPosition::getRefString() const {
  return MPosition::showType(ref.getType());
}

Quantum<Vector<double> > MPosition::get(const Unit &inunit) const {
  return Quantum<Vector<double> >(data.getValue(),"m").get(inunit);
}

Quantum<Vector<double> > MPosition::getAngle() const {
  return (data.getAngle());
}

Quantum<Vector<double> > MPosition::getAngle(const Unit &inunit) const {
  return (data.getAngle(inunit));
}

Measure *MPosition::clone() const {
  return (new MPosition(*this));
}

} //# NAMESPACE CASACORE - END

