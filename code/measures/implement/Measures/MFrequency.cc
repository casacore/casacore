//# MFrequency.cc: A Measure: wave characteristics
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
typedef Quantum<Double> gpp_mfrequency_bug1;
#endif
#include <aips/Utilities/Assert.h>
#include <aips/Measures/MFrequency.h>
#include <aips/RTTI/Register.h>
#include <aips/Measures/MDoppler.h>
#include <aips/Measures/MCDoppler.h>
#include <aips/Measures/MeasConvert.h>
#ifdef __GNUG__
typedef MeasConvert<MDoppler,MVDoppler,MCDoppler> gpp_mradvel_bug2;
#endif

//# Constructors
MFrequency::MFrequency() :
  MeasBase<MVFrequency,MFrequency::Ref>() {}

MFrequency::MFrequency(const MVFrequency &dt) : 
  MeasBase<MVFrequency,MFrequency::Ref>(dt,MFrequency::DEFAULT) {}

MFrequency::MFrequency(const MVFrequency &dt, const MFrequency::Ref &rf) : 
  MeasBase<MVFrequency,MFrequency::Ref>(dt,rf) {}

MFrequency::MFrequency(const MVFrequency &dt, uInt rf) : 
  MeasBase<MVFrequency,MFrequency::Ref>(dt,rf) {}

MFrequency::MFrequency(const Quantity &dt) : 
  MeasBase<MVFrequency,MFrequency::Ref>(dt,MFrequency::DEFAULT) {}

MFrequency::MFrequency(const Quantity &dt, const MFrequency::Ref &rf) : 
  MeasBase<MVFrequency,MFrequency::Ref>(dt,rf) {}

MFrequency::MFrequency(const Quantity &dt, uInt rf) : 
  MeasBase<MVFrequency,MFrequency::Ref>(dt,rf) {}

MFrequency::MFrequency(const Measure *dt) :
  MeasBase<MVFrequency,MFrequency::Ref>(dt) {}

MFrequency::MFrequency(const MeasValue *dt) :
  MeasBase<MVFrequency,MFrequency::Ref>(*(MVFrequency*)dt,
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
  return Register((MFrequency *)0);
}

void MFrequency::assert(const Measure &in) {
  if (in.type() != Register((MFrequency *)0)) {
    throw(AipsError("Illegal Measure type argument: " +
		    MFrequency::showMe()));
  };
}

const String &MFrequency::showType(uInt tp) {
    static const String tname[MFrequency::N_Types] = {
	"REST",
	"LSR",
	"LSRK",
	"BARY",
	"GEO",	    
	"TOPO",
	"GALACTO"}; 
    DebugAssert(tp < MFrequency::N_Types, AipsError);
    return tname[tp];
}

const String *const MFrequency::allMyTypes(Int &nall, Int &nextra,
					  const uInt *&typ) {
  static const Int N_name  = 7;
  static const Int N_extra = 0;
  static const String tname[N_name] = {
    "REST",
    "LSR",
    "LSRK",
    "BARY",
    "GEO",	    
    "TOPO",
    "GALACTO" }; 
  
  static const uInt oname[N_name] = {
    MFrequency::REST,
    MFrequency::LSR,
    MFrequency::LSRK,
    MFrequency::BARY,
    MFrequency::GEO,
    MFrequency::TOPO,
    MFrequency::GALACTO };

  nall   = N_name;
  nextra = N_extra;
  typ    = oname;
  return tname;
}

const String *const MFrequency::allTypes(Int &nall, Int &nextra,
					const uInt *&typ) const {
  return MFrequency::allMyTypes(nall, nextra, typ);
}

Bool MFrequency::getType(MFrequency::Types &tp, const String &in) {
  const uInt *oname;
  Int nall, nex;
  const String *tname = MFrequency::allMyTypes(nall, nex, oname);
  
  Int i = Measure::giveMe(in, nall, tname);
  
  if (i>=nall) {
    return False;
  } else {
    tp = (MFrequency::Types) oname[i];
  };
  return True;
}

Bool MFrequency::giveMe(MFrequency::Ref &mr, const String &in) {
  MFrequency::Types tp;
  if (MFrequency::getType(tp, in)) {
    mr = MFrequency::Ref(tp);
  } else {
    mr = MFrequency::Ref();
    return False;
  };
  return True;
};

Bool MFrequency::giveMe(const String &in, MFrequency::Ref &mr) {
  return MFrequency::giveMe(mr, in);
}

Bool MFrequency::setOffset(const Measure &in) {
  if (in.type() != Register((MFrequency *)0)) return False;
  ref.set(in);
  return True;
}

Bool MFrequency::setRefString(const String &in) {
  MFrequency::Types tp;
  if (MFrequency::getType(tp, in)) {
    ref.setType(tp);
    return True;
  };
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
  return Register((MFrequency *)0);
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
  MFrequency::assert(in);
  Double t = ((MVFrequency *)(in.getData()))->getValue()
    / rest.getValue();
  t *= t;
  return MDoppler( MVDoppler((1-t)/(1+t)), MDoppler::BETA);
}

MFrequency MFrequency::fromDoppler(const MDoppler &dop,
				   const MVFrequency &rest) {
  return MFrequency::fromDoppler(dop, rest, MFrequency::LSR);
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
  MDoppler::assert(dop);
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
  MDoppler::assert(dop);
  MFrequency::assert(in);
  Double t = MDoppler::Convert(dop, MDoppler::BETA)().getValue();
  t = (1-t)/(1+t);
  return MFrequency(MVFrequency(((MVFrequency *)(in.getData()))->getValue()
				/ sqrt(t)),
		    MFrequency::REST);
}

Measure *MFrequency::clone() const {
  return (new MFrequency(*this));
}
