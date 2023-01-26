//# MRadialVelocity.cc: A Measure: radial velocity
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2003
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
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Quanta/QMath.h>
#include <casacore/measures/Measures/MRadialVelocity.h>
#include <casacore/measures/Measures/MDoppler.h>
#include <casacore/measures/Measures/MCDoppler.h>
#include <casacore/measures/Measures/MeasConvert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors
MRadialVelocity::MRadialVelocity() :
  MeasBase<MVRadialVelocity, MRadialVelocity::Ref>() {}

MRadialVelocity::MRadialVelocity(const MVRadialVelocity &dt) : 
  MeasBase<MVRadialVelocity,
  MRadialVelocity::Ref>(dt, MRadialVelocity::DEFAULT) {}

MRadialVelocity::MRadialVelocity(const MVRadialVelocity &dt, 
				 const MRadialVelocity::Ref &rf) : 
  MeasBase<MVRadialVelocity, MRadialVelocity::Ref>(dt,rf) {}

MRadialVelocity::MRadialVelocity(const MVRadialVelocity &dt,
				 MRadialVelocity::Types rf) : 
  MeasBase<MVRadialVelocity, MRadialVelocity::Ref>(dt,rf) {}

MRadialVelocity::MRadialVelocity(const Quantity &dt) : 
  MeasBase<MVRadialVelocity, MRadialVelocity::Ref>(dt,
						   MRadialVelocity::DEFAULT) {}

MRadialVelocity::MRadialVelocity(const Quantity &dt,
				 const MRadialVelocity::Ref &rf) : 
  MeasBase<MVRadialVelocity, MRadialVelocity::Ref>(dt,rf) {}

MRadialVelocity::MRadialVelocity(const Quantity &dt,
				 MRadialVelocity::Types rf) : 
  MeasBase<MVRadialVelocity, MRadialVelocity::Ref>(dt,rf) {}

MRadialVelocity::MRadialVelocity(const Measure *dt) :
  MeasBase<MVRadialVelocity, MRadialVelocity::Ref>(dt) {}

MRadialVelocity::MRadialVelocity(const MeasValue *dt) :
  MeasBase<MVRadialVelocity,
  MRadialVelocity::Ref>(*(MVRadialVelocity*)dt, 
			MRadialVelocity::DEFAULT) {}

//# Destructor
MRadialVelocity::~MRadialVelocity() {}

//# Operators

//# Member functions

const String &MRadialVelocity::tellMe() const {
    return MRadialVelocity::showMe();
}

const String &MRadialVelocity::showMe() {
    static const String name("Radialvelocity");
    return name;
}

void MRadialVelocity::assure(const Measure &in) {
  if (!dynamic_cast<const MRadialVelocity*>(&in)) {
    throw(AipsError("Illegal Measure type argument: " +
		    MRadialVelocity::showMe()));
  }
}

MRadialVelocity::Types MRadialVelocity::castType(uint32_t tp) {
  MRadialVelocity::checkMyTypes();
  AlwaysAssert(tp < MRadialVelocity::N_Types, AipsError);
  return static_cast<MRadialVelocity::Types>(tp);
}

const String &MRadialVelocity::showType(MRadialVelocity::Types tp) {
  static const String tname[MRadialVelocity::N_Types] = {
    "LSRK",
    "LSRD",
    "BARY",
    "GEO",	    
    "TOPO",
    "GALACTO",
    "LGROUP",
    "CMB" }; 

  MRadialVelocity::checkMyTypes();
  return tname[tp];
}

const String &MRadialVelocity::showType(uint32_t tp) {
  return MRadialVelocity::showType(MRadialVelocity::castType(tp));
}

const String* MRadialVelocity::allMyTypes(int32_t &nall, int32_t &nextra,
					  const uint32_t *&typ) {
  static const int32_t N_name  = 8;
  static const int32_t N_extra = 0;
  static const String tname[N_name] = {
    "LSRK",
    "LSRD",
    "BARY",
    "GEO",	    
    "TOPO",
    "GALACTO",
    "LGROUP",
    "CMB" }; 

  static const uint32_t oname[N_name] = {
    MRadialVelocity::LSRK,
    MRadialVelocity::LSRD,
    MRadialVelocity::BARY,
    MRadialVelocity::GEO,
    MRadialVelocity::TOPO,
    MRadialVelocity::GALACTO,
    MRadialVelocity::LGROUP,
    MRadialVelocity::CMB };

  MRadialVelocity::checkMyTypes();
  nall   = N_name;
  nextra = N_extra;
  typ    = oname;
  return tname;
}

const String* MRadialVelocity::allTypes(int32_t &nall, int32_t &nextra,
					const uint32_t *&typ) const {
  return MRadialVelocity::allMyTypes(nall, nextra, typ);
}

void MRadialVelocity::checkTypes() const {
  MRadialVelocity::checkMyTypes();
}

void MRadialVelocity::checkMyTypes() {
  // Multiple threads could execute this, but that is harmless.
  static bool first(true);
  if (first) {
    first = false;
    int32_t nall, nex;
    const uint32_t *typ;
    const String *const tps = MRadialVelocity::allMyTypes(nall,nex, typ);
    MRadialVelocity::Types tp;
    for (int32_t i=0; i<nall; i++) {
      AlwaysAssert
	(MRadialVelocity::getType(tp, MRadialVelocity::showType(typ[i])) &&
	 tp == int32_t(typ[i]) &&
	 MRadialVelocity::getType(tp, tps[i]) &&
	 tp == int32_t(typ[i]), AipsError);
    }
    for (int32_t i=0; i<N_Types; i++) {
      AlwaysAssert(MRadialVelocity::getType(tp,
					    MRadialVelocity::showType(i)) &&
		   tp == i, AipsError);
    }
  }
}

bool MRadialVelocity::getType(MRadialVelocity::Types &tp, const String &in) {
  const uint32_t *oname;
  int32_t nall, nex;
  const String *tname = MRadialVelocity::allMyTypes(nall, nex, oname);
  
  int32_t i = Measure::giveMe(in, nall, tname);
  
  if (i>=nall) return false;
  else tp = static_cast<MRadialVelocity::Types>(oname[i]);
  return true;
}

bool MRadialVelocity::giveMe(MRadialVelocity::Ref &mr, const String &in) {
  MRadialVelocity::Types tp;
  if (MRadialVelocity::getType(tp, in)) mr = MRadialVelocity::Ref(tp);
  else {
    mr = MRadialVelocity::Ref();
    return false;
  }
  return true;
}

bool MRadialVelocity::setOffset(const Measure &in) {
  if (!dynamic_cast<const MRadialVelocity*>(&in)) return false;
  ref.set(in);
  return true;
}

bool MRadialVelocity::setRefString(const String &in) {
  MRadialVelocity::Types tp;
  if (MRadialVelocity::getType(tp, in)) {
    ref.setType(tp);
    return true;
  }
  ref.setType(MRadialVelocity::DEFAULT);
  return false;
}

const String &MRadialVelocity::getDefaultType() const {
  return MRadialVelocity::showType(MRadialVelocity::DEFAULT);
}

String MRadialVelocity::getRefString() const {
  return MRadialVelocity::showType(ref.getType());
}

Quantity MRadialVelocity::get(const Unit &un) const {
    return data.get(un);
}

MDoppler MRadialVelocity::toDoppler() {
    double t = data.getValue() / C::c;
    return MDoppler( MVDoppler(t), MDoppler::BETA);
}

MDoppler MRadialVelocity::toDoppler(const Measure &in) {
  MRadialVelocity::assure(in);
  double t = ((MVRadialVelocity *)(in.getData()))->getValue() / C::c;
  return MDoppler( MVDoppler(t), MDoppler::BETA);
}

MRadialVelocity MRadialVelocity::fromDoppler(const MDoppler &dop) {
    double t = C::c * MDoppler::Convert(dop, MDoppler::BETA)()
	.getValue().getValue();
    return MRadialVelocity(MVRadialVelocity(t),
		      MRadialVelocity::LSRK);
}

MRadialVelocity MRadialVelocity::fromDoppler(const MDoppler &dop,
					     MRadialVelocity::Types typ) {
    double t = C::c * MDoppler::Convert(dop, MDoppler::BETA)()
	.getValue().getValue();
    return MRadialVelocity(MVRadialVelocity(t),
			   typ);
}

MRadialVelocity MRadialVelocity::fromDoppler(const Measure &dop,
					     MRadialVelocity::Types typ) {
  MDoppler::assure(dop);
  double t = C::c * MDoppler::Convert(dop, MDoppler::BETA)()
    .getValue().getValue();
  return MRadialVelocity(MVRadialVelocity(t),
			 typ);
}
    
Measure *MRadialVelocity::clone() const {
    return (new MRadialVelocity(*this));
}

} //# NAMESPACE CASACORE - END

