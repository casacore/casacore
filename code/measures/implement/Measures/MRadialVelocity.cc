//# MRadialVelocity.cc: A Measure: radial velocity
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
#include <aips/Measures/Quantum.h>
typedef Quantum<Double> gpp_mradvel_bug1;
#endif
#include <aips/Utilities/Assert.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Measures/QMath.h>
#include <aips/Measures/MRadialVelocity.h>
#include <aips/RTTI/Register.h>
#include <aips/Measures/MDoppler.h>
#include <aips/Measures/MCDoppler.h>
#include <aips/Measures/MeasConvert.h>
#ifdef __GNUG__
typedef MeasConvert<MDoppler,MVDoppler,MCDoppler> gpp_mradvel_bug2;
#endif

//# Constructors
MRadialVelocity::MRadialVelocity() :
  MeasBase<MVRadialVelocity,MRadialVelocity::Ref>() {}

MRadialVelocity::MRadialVelocity(const MVRadialVelocity &dt) : 
  MeasBase<MVRadialVelocity,MRadialVelocity::Ref>(dt,
						  MRadialVelocity::DEFAULT) {}

MRadialVelocity::MRadialVelocity(const MVRadialVelocity &dt, 
				 const MRadialVelocity::Ref &rf) : 
  MeasBase<MVRadialVelocity,MRadialVelocity::Ref>(dt,rf) {}

MRadialVelocity::MRadialVelocity(const MVRadialVelocity &dt, uInt rf) : 
  MeasBase<MVRadialVelocity,MRadialVelocity::Ref>(dt,rf) {}

MRadialVelocity::MRadialVelocity(const Quantity &dt) : 
  MeasBase<MVRadialVelocity,MRadialVelocity::Ref>(dt,
						  MRadialVelocity::DEFAULT) {}

MRadialVelocity::MRadialVelocity(const Quantity &dt, const MRadialVelocity::Ref &rf) : 
  MeasBase<MVRadialVelocity,MRadialVelocity::Ref>(dt,rf) {}

MRadialVelocity::MRadialVelocity(const Quantity &dt, uInt rf) : 
  MeasBase<MVRadialVelocity,MRadialVelocity::Ref>(dt,rf) {}

MRadialVelocity::MRadialVelocity(const Measure *dt) :
  MeasBase<MVRadialVelocity,MRadialVelocity::Ref>(dt) {}

MRadialVelocity::MRadialVelocity(const MeasValue *dt) :
  MeasBase<MVRadialVelocity,MRadialVelocity::Ref>(*(MVRadialVelocity*)dt,
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

uInt MRadialVelocity::type() const {
  return Register((MRadialVelocity *)0);
}

void MRadialVelocity::assert(const Measure &in) {
  if (in.type() != Register((MRadialVelocity *)0)) {
    throw(AipsError("Illegal Measure type argument: " +
		    MRadialVelocity::showMe()));
  };
}

const String &MRadialVelocity::showType(uInt tp) {
    static const String tname[MRadialVelocity::N_Types] = {
	"LSR",
	"LSRK",
	"BARY",
	"GEO",	    
	"TOPO",
	"GALACTO"}; 
    DebugAssert(tp < MRadialVelocity::N_Types, AipsError);
    return tname[tp];
}

Bool MRadialVelocity::getType(MRadialVelocity::Types &tp, const String &in) {
  static const Int N_name = 6;
  static const String tname[N_name] = {
    "LSR",
    "LSRK",
    "BARY",
    "GEO",	    
    "TOPO",
    "GALACTO"}; 

  static const MRadialVelocity::Types oname[N_name] = {
    MRadialVelocity::LSR,
    MRadialVelocity::LSRK,
    MRadialVelocity::BARY,
    MRadialVelocity::GEO,
    MRadialVelocity::TOPO,
    MRadialVelocity::GALACTO};

  uInt i = Measure::giveMe(in, N_name, tname);
  
  if (i>=N_name) {
    return False;
  } else {
    tp = oname[i];
  };
  return True;
}

Bool MRadialVelocity::giveMe(MRadialVelocity::Ref &mr, const String &in) {
  MRadialVelocity::Types tp;
  if (MRadialVelocity::getType(tp, in)) {
    mr = MRadialVelocity::Ref(tp);
  } else {
    mr = MRadialVelocity::Ref();
    return False;
  };
  return True;
};

Bool MRadialVelocity::giveMe(const String &in, MRadialVelocity::Ref &mr) {
  return MRadialVelocity::giveMe(mr, in);
}

Bool MRadialVelocity::setOffset(const Measure &in) {
  if (in.type() != Register((MRadialVelocity *)0)) return False;
  ref.set(in);
  return True;
}

Bool MRadialVelocity::setRefString(const String &in) {
  MRadialVelocity::Types tp;
  if (MRadialVelocity::getType(tp, in)) {
    ref.setType(tp);
    return True;
  };
  ref.setType(MRadialVelocity::DEFAULT);
  return False;
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
    Double t = data.getValue() / C::c;
    return MDoppler( MVDoppler(t), MDoppler::BETA);
}

MDoppler MRadialVelocity::toDoppler(const Measure &in) {
  MRadialVelocity::assert(in);
  Double t = ((MVRadialVelocity *)(in.getData()))->getValue() / C::c;
  return MDoppler( MVDoppler(t), MDoppler::BETA);
}

MRadialVelocity MRadialVelocity::fromDoppler(const MDoppler &dop) {
    Double t = C::c * MDoppler::Convert(dop, MDoppler::BETA)()
	.getValue().getValue();
    return MRadialVelocity(MVRadialVelocity(t),
		      MRadialVelocity::LSR);
}

MRadialVelocity MRadialVelocity::fromDoppler(const MDoppler &dop,
					     MRadialVelocity::Types typ) {
    Double t = C::c * MDoppler::Convert(dop, MDoppler::BETA)()
	.getValue().getValue();
    return MRadialVelocity(MVRadialVelocity(t),
			   typ);
}

MRadialVelocity MRadialVelocity::fromDoppler(const Measure &dop,
					     MRadialVelocity::Types typ) {
  MDoppler::assert(dop);
  Double t = C::c * MDoppler::Convert(dop, MDoppler::BETA)()
    .getValue().getValue();
  return MRadialVelocity(MVRadialVelocity(t),
			 typ);
}
    
Measure *MRadialVelocity::clone() const {
    return (new MRadialVelocity(*this));
}
