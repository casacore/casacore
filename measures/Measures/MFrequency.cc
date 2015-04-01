//# MFrequency.cc: A Measure: wave characteristics
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
//#
//# $Id$

//# Includes
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/casa/Utilities/Register.h>
#include <casacore/measures/Measures/MDoppler.h>
#include <casacore/measures/Measures/MCDoppler.h>
#include <casacore/measures/Measures/MeasConvert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors
MFrequency::MFrequency() :
  MeasBase<MVFrequency, MFrequency::Ref>() {}

MFrequency::MFrequency(const MVFrequency &dt) : 
  MeasBase<MVFrequency, MFrequency::Ref>(dt,MFrequency::DEFAULT) {}

MFrequency::MFrequency(const MVFrequency &dt, const MFrequency::Ref &rf) : 
  MeasBase<MVFrequency, MFrequency::Ref>(dt,rf) {}

MFrequency::MFrequency(const MVFrequency &dt, MFrequency::Types rf) : 
  MeasBase<MVFrequency, MFrequency::Ref>(dt,rf) {}

MFrequency::MFrequency(const Quantity &dt) : 
  MeasBase<MVFrequency, MFrequency::Ref>(dt,MFrequency::DEFAULT) {}

MFrequency::MFrequency(const Quantity &dt, const MFrequency::Ref &rf) : 
  MeasBase<MVFrequency, MFrequency::Ref>(dt,rf) {}

MFrequency::MFrequency(const Quantity &dt, MFrequency::Types rf) : 
  MeasBase<MVFrequency, MFrequency::Ref>(dt,rf) {}

MFrequency::MFrequency(const Measure *dt) :
  MeasBase<MVFrequency, MFrequency::Ref>(dt) {}

MFrequency::MFrequency(const MeasValue *dt) :
  MeasBase<MVFrequency, MFrequency::Ref>(*(MVFrequency*)dt,
					 MFrequency::DEFAULT) {}

//# Destructor
MFrequency::~MFrequency() {}

//# Operators

//# Member functions

const String &MFrequency::tellMe() const {
    return MFrequency::showMe();
}

const String &MFrequency::showMe() {
    static const String name("Frequency");
    return name;
}

uInt MFrequency::type() const {
  return Register(static_cast<MFrequency *>(0));
}

void MFrequency::assure(const Measure &in) {
  if (in.type() != Register(static_cast<MFrequency *>(0))) {
    throw(AipsError("Illegal Measure type argument: " +
		    MFrequency::showMe()));
  }
}

MFrequency::Types MFrequency::castType(uInt tp) {
  MFrequency::checkMyTypes();

  if ((tp & MFrequency::EXTRA) == 0) {
    AlwaysAssert(tp < MFrequency::N_Types, AipsError);
  } else {
    AlwaysAssert((tp & ~MFrequency::EXTRA) < 
		 (MFrequency::N_Other - MFrequency::EXTRA), AipsError);
  }

  return static_cast<MFrequency::Types>(tp);
}

const String &MFrequency::showType(MFrequency::Types tp) {
  static const String tname[MFrequency::N_Types] = {
    "REST",
    "LSRK",
    "LSRD",
    "BARY",
    "GEO",	    
    "TOPO",
    "GALACTO",
    "LGROUP",
    "CMB"
  }; 
  static const String ename[MFrequency::N_Other - MFrequency::EXTRA] = {
    "Undefined" 
  };
  
  MFrequency::checkMyTypes();
  if ((tp & MFrequency::EXTRA) == 0) return tname[tp];
  return ename[tp & ~MFrequency::EXTRA];

}

const String &MFrequency::showType(uInt tp) {
  return MFrequency::showType(MFrequency::castType(tp));
}

const String* MFrequency::allMyTypes(Int &nall, Int &nextra,
                                     const uInt *&typ) {
  static const Int N_name  = 10;
  static const Int N_extra = 1;
  static const String tname[N_name] = {
    "REST",
    "LSRK",
    "LSRD",
    "BARY",
    "GEO",	    
    "TOPO",
    "GALACTO",
    "LGROUP",
    "CMB",
    "Undefined"
  }; 
  
  static const uInt oname[N_name] = {
    MFrequency::REST,
    MFrequency::LSRK,
    MFrequency::LSRD,
    MFrequency::BARY,
    MFrequency::GEO,
    MFrequency::TOPO,
    MFrequency::GALACTO,
    MFrequency::LGROUP,
    MFrequency::CMB,
    MFrequency::Undefined
 };

  MFrequency::checkMyTypes();
  nall   = N_name;
  nextra = N_extra;
  typ    = oname;
  return tname;
}

const String* MFrequency::allTypes(Int &nall, Int &nextra,
                                   const uInt *&typ) const {
  return MFrequency::allMyTypes(nall, nextra, typ);
}

Bool MFrequency::getType(MFrequency::Types &tp, const String &in) {
  const uInt *oname;
  Int nall, nex;
  const String *tname = MFrequency::allMyTypes(nall, nex, oname);
  
  Int i = Measure::giveMe(in, nall, tname);
  
  if (i>=nall) return False;
  else tp = static_cast<MFrequency::Types>(oname[i]);
  return True;
}

MFrequency::Types MFrequency::typeFromString(const String& in) {
	MFrequency::Types tp;
	ThrowIf(
		! getType(tp, in),
		in + " is not a recognized type identifier"
	);
	return tp;
}


void MFrequency::checkTypes() const {
  MFrequency::checkMyTypes();
}

void MFrequency::checkMyTypes() {
  static Bool first(True);
  if (first) {
    first = False;
    Int nall, nex;
    const uInt *typ;
    const String *const tps = MFrequency::allMyTypes(nall,nex, typ);
    MFrequency::Types tp;
    for (Int i=0; i<nall; i++) {
      AlwaysAssert(MFrequency::getType(tp, MFrequency::showType(typ[i])) &&
		   tp == Int(typ[i]) &&
		   MFrequency::getType(tp, tps[i]) &&
		   tp == Int(typ[i]), AipsError);
    }
    for (Int i=0; i<N_Types; i++) {
      AlwaysAssert(MFrequency::getType(tp, MFrequency::showType(i)) &&
		   tp == i, AipsError);
    }
  }
}

Bool MFrequency::giveMe(MFrequency::Ref &mr, const String &in) {
  MFrequency::Types tp;
  if (MFrequency::getType(tp, in)) mr = MFrequency::Ref(tp);
  else {
    mr = MFrequency::Ref();
    return False;
  }
  return True;
}

Bool MFrequency::setOffset(const Measure &in) {
  if (in.type() != Register(static_cast<MFrequency *>(0))) return False;
  ref.set(in);
  return True;
}

Bool MFrequency::setRefString(const String &in) {
  MFrequency::Types tp;
  if (MFrequency::getType(tp, in)) {
    ref.setType(tp);
    return True;
  }
  ref.setType(MFrequency::DEFAULT);
  return False;
}

const String &MFrequency::getDefaultType() const {
  return MFrequency::showType(MFrequency::DEFAULT);
}

String MFrequency::getRefString() const {
  return MFrequency::showType(ref.getType());
}

uInt MFrequency::myType() {
  return Register(static_cast<MFrequency *>(0));
}

Quantity MFrequency::get(const Unit &un) const {
    return data.get(un);
}

MDoppler MFrequency::toDoppler(const MVFrequency &rest) {
    Double t = data / rest;
    t *= t;
    return MDoppler( MVDoppler((1-t)/(1+t)), MDoppler::BETA);
}

MDoppler MFrequency::toDoppler(const MVFrequency &rest) const {
    Double t = data / rest;
    t *= t;
    return MDoppler( MVDoppler((1-t)/(1+t)), MDoppler::BETA);
}

MDoppler MFrequency::toDoppler(const Measure &in, const MVFrequency &rest) {
  MFrequency::assure(in);
  Double t = ((MVFrequency *)(in.getData()))->getValue()
    / rest.getValue();
  t *= t;
  return MDoppler( MVDoppler((1-t)/(1+t)), MDoppler::BETA);
}

MFrequency MFrequency::fromDoppler(const MDoppler &dop,
				   const MVFrequency &rest) {
  return MFrequency::fromDoppler(dop, rest, MFrequency::LSRK);
}

MFrequency MFrequency::fromDoppler(const MDoppler &dop,
				   const MVFrequency &rest,
				   MFrequency::Types type) {
    Double t = MDoppler::Convert(dop, MDoppler::BETA)().getValue();
    t = (1-t)/(1+t);
    return MFrequency(MVFrequency(sqrt(t) * rest.getValue()),
		      type);
}

MFrequency MFrequency::fromDoppler(const Measure &dop,
				   const MVFrequency &rest,
				   MFrequency::Types type) {
  MDoppler::assure(dop);
  Double t = MDoppler::Convert(dop, MDoppler::BETA)().getValue();
  t = (1-t)/(1+t);
  return MFrequency(MVFrequency(sqrt(t) * rest.getValue()),
		    type);
}

MFrequency MFrequency::toRest(const MDoppler &dop) {
    Double t = MDoppler::Convert(dop, MDoppler::BETA)().getValue();
    t = (1-t)/(1+t);
    return MFrequency(MVFrequency(data.getValue() / sqrt(t)),
		      MFrequency::REST);
}

MFrequency MFrequency::toRest(const Measure &in, const Measure &dop) {
  MDoppler::assure(dop);
  MFrequency::assure(in);
  Double t = MDoppler::Convert(dop, MDoppler::BETA)().getValue();
  t = (1-t)/(1+t);
  return MFrequency(MVFrequency(((MVFrequency *)(in.getData()))->getValue()
				/ sqrt(t)),
		    MFrequency::REST);
}

Measure *MFrequency::clone() const {
  return (new MFrequency(*this));
}

} //# NAMESPACE CASACORE - END

