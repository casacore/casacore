//# MFrequency.cc: A Measure: wave characteristics
//# Copyright (C) 1995,1996,1997
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
#include <aips/Utilities/Assert.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Measures/QC.h>
#include <aips/Measures/QMath.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MeasFrame.h>
#include <aips/Measures/MVPosition.h>
#include <aips/Measures/MVDirection.h>
#include <aips/Measures/Aberration.h>
#include <aips/Measures/MeasData.h>
#include <aips/Measures/MDoppler.h>

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

Bool MFrequency::giveMe(const String &in, MFrequency::Ref &mr) {
    static const Int N_name = 7;
    static const String tname[N_name] = {
	"REST",
	"LSR",
	"LSRK",
	"BARY",
	"GEO",	    
	"TOPO",
	"GALACTO"}; 

    static const uInt oname[N_name] = {
	MFrequency::REST,
	MFrequency::LSR,
	MFrequency::LSRK,
	MFrequency::BARY,
	MFrequency::GEO,
	MFrequency::TOPO,
	MFrequency::GALACTO};

    uInt i = Measure::giveMe(in, N_name, tname);

    if (i>=N_name) {
	mr = MFrequency::Ref();
	return False;
    } else {
	mr = MFrequency::Ref(oname[i]);
    };
    return True;
}

Quantity MFrequency::get(const Unit &un) const {
    return data.get(un);
}

MDoppler MFrequency::toDoppler(const MVFrequency &rest) {
    Double t = data / rest;
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

MFrequency MFrequency::toRest(const MDoppler &dop) {
    Double t = MDoppler::Convert(dop, MDoppler::BETA)().getValue();
    t = (1-t)/(1+t);
    return MFrequency(MVFrequency(data.getValue() / sqrt(t)),
		      MFrequency::REST);
}
    
void *MFrequency::clone() const {
    return ((void *) new MFrequency(*this));
}

void MFrequency::getConvert(MFrequency::Convert &mc,
			    const MFrequency::Ref &inref, 
			    const MFrequency::Ref &outref) {
// Array of conversion routines to call
    static const MFrequency::Routes 
	FromTo[MFrequency::N_Types][MFrequency::N_Types] = {
    { MFrequency::N_Routes,
      MFrequency::REST_LSR,
      MFrequency::REST_LSR,
      MFrequency::REST_LSR,
      MFrequency::REST_LSR,
      MFrequency::REST_LSR,
      MFrequency::REST_LSR},
    { MFrequency::LSR_REST,
      MFrequency::N_Routes,
      MFrequency::LSR_BARY,
      MFrequency::LSR_BARY,
      MFrequency::LSR_BARY,
      MFrequency::LSR_BARY,
      MFrequency::LSR_GALACTO},
    { MFrequency::LSRK_BARY,
      MFrequency::LSRK_BARY,
      MFrequency::N_Routes,
      MFrequency::LSRK_BARY,
      MFrequency::LSRK_BARY,
      MFrequency::LSRK_BARY,
      MFrequency::LSRK_BARY},
    { MFrequency::BARY_LSR,
      MFrequency::BARY_LSR,
      MFrequency::BARY_LSRK,
      MFrequency::N_Routes,
      MFrequency::BARY_GEO,
      MFrequency::BARY_GEO,
      MFrequency::BARY_LSR},
    { MFrequency::GEO_BARY,
      MFrequency::GEO_BARY,
      MFrequency::GEO_BARY,
      MFrequency::GEO_BARY,
      MFrequency::N_Routes,
      MFrequency::GEO_TOPO,
      MFrequency::GEO_BARY},
    { MFrequency::TOPO_GEO,
      MFrequency::TOPO_GEO,
      MFrequency::TOPO_GEO,
      MFrequency::TOPO_GEO,
      MFrequency::TOPO_GEO,
      MFrequency::N_Routes,
      MFrequency::TOPO_GEO},
    { MFrequency::GALACTO_LSR,
      MFrequency::GALACTO_LSR,
      MFrequency::GALACTO_LSR,
      MFrequency::GALACTO_LSR,
      MFrequency::GALACTO_LSR,
      MFrequency::GALACTO_LSR,
      MFrequency::N_Routes}
    };

// List of codes converted to
    static const MFrequency::Types ToRef[MFrequency::N_Routes] = {
        MFrequency::BARY,
        MFrequency::LSR,
        MFrequency::GEO,
        MFrequency::TOPO,
	MFrequency::BARY,
        MFrequency::GEO,
	MFrequency::GALACTO,
        MFrequency::LSR,
	MFrequency::BARY,
	MFrequency::LSRK,
        MFrequency::LSR,
	MFrequency::REST
	};

    Int iin  = inref.getType();
    Int iout = outref.getType();
    Int tmp;
    while (iin != iout) {
	tmp = FromTo[iin][iout];
	iin = ToRef[tmp];
	mc.addMethod(tmp);
	initConvert(tmp, mc);
    }
}

void MFrequency::clearConvert(MFrequency::Convert &mc) {
  delete (MVPosition *) mc.getStruct(MFrequency::MVPOS1);
  delete (MVDirection *) mc.getStruct(MFrequency::MVDIR1);
  delete (Aberration *) mc.getStruct(MFrequency::ABERFROM);
  delete (Aberration *) mc.getStruct(MFrequency::ABERTO);
}

//# Conversion routines
void MFrequency::initConvert(uInt which, MFrequency::Convert &mc) {
    if (!(mc.getStruct(MFrequency::MVPOS1))) {
	mc.addStruct(MFrequency::MVPOS1,
		     (void *) new MVPosition());
    };

    if (!(mc.getStruct(MFrequency::MVDIR1))) {
	mc.addStruct(MFrequency::MVDIR1,
		     (void *) new MVDirection());
    };

    switch (which) {
      
    case LSR_BARY:
      break;
      
    case BARY_LSR:
      break;
      
    case BARY_GEO:
      mc.addStruct(MFrequency::ABERFROM,
		   (void *) new Aberration(Aberration::STANDARD));
      break;
      
    case GEO_TOPO:
      break;
      
    case GEO_BARY:
      mc.addStruct(MFrequency::ABERTO,
		   (void *) new Aberration(Aberration::STANDARD));
      break;
      
    case TOPO_GEO:
      break;
      
    case LSR_GALACTO:
      break;
      
    case GALACTO_LSR:
      break;
      
    case LSRK_BARY:
      break;
      
    case BARY_LSRK:
      break;
      
    case REST_LSR:
      break;
      
    case LSR_REST:
      break;
      
    default:
      break;
    }
}

void MFrequency::doConvert(MVFrequency &in,
			   const MFrequency::Ref &inref,
			   const MFrequency::Ref &outref,
			   const MFrequency::Convert &mc) {
    Double g0, g1, g2, g3, lengthE, tdbTime;
    MVPosition *solpos;
    MVDirection *respos;

    for (Int i=0; i<mc.nMethod(); i++) {

      switch (mc.getMethod(i)) {

      case LSR_BARY: {
	solpos = (MVPosition *) mc.getStruct(MFrequency::MVPOS1);
	*solpos = MVPosition(MeasData::velocityLSR(0));
	respos = (MVDirection *) mc.getStruct(MFrequency::MVDIR1);
	MFrequency::Ref::frameDirection(outref, inref).
	  getJ2000(*respos);
	g0 = sqrt(1. - (*solpos * *solpos)/C::c/C::c);
	g1 = (*solpos * *respos) / C::c;
	g2 = in.getValue();
	in = g2 * g0/(1 + g1);
      }
      break;

      case BARY_LSR: {
	solpos = (MVPosition *) mc.getStruct(MFrequency::MVPOS1);
	*solpos = MVPosition(MeasData::velocityLSR(0));
	respos = (MVDirection *) mc.getStruct(MFrequency::MVDIR1);
	MFrequency::Ref::frameDirection(inref, outref).
	  getJ2000(*respos);
	g0 = sqrt(1. - (*solpos * *solpos)/C::c/C::c);
	g1 = (*solpos * *respos) / C::c;
	g2 = in.getValue();
	in = g2 * g0/(1 - g1);
      }
      break;

      case BARY_GEO: {
	MFrequency::Ref::frameEpoch(outref, inref).
	  getTDB(tdbTime);
	solpos = (MVPosition *) mc.getStruct(MFrequency::MVPOS1);
	*solpos = ((Aberration *)
		   mc.getStruct(MFrequency::ABERFROM))->
	  operator()(tdbTime);
	respos = (MVDirection *) mc.getStruct(MFrequency::MVDIR1);
	MFrequency::Ref::frameDirection(outref, inref).
	  getJ2000(*respos);
	g0 = sqrt(1. - (*solpos * *solpos));
	g1 = (*solpos * *respos);
	g2 = in.getValue();
	in = g2 * g0/(1 + g1);
      }	
      break;

      case GEO_TOPO: {
	MFrequency::Ref::frameEpoch(outref, inref).
	  getLASTr(g1);
	MFrequency::Ref::framePosition(outref, inref).
	  getRadius(lengthE);
	MFrequency::Ref::frameEpoch(outref, inref).
	  getTDB(tdbTime);
	MFrequency::Ref::framePosition(outref, inref).
	  getLat(g3);
	solpos = (MVPosition *) mc.getStruct(MFrequency::MVPOS1);
	g2 = MeasData::diurnalAber(lengthE, tdbTime);
	*solpos = MVDirection(g1, g3);
	solpos->readjust(g2);
	respos = (MVDirection *) mc.getStruct(MFrequency::MVDIR1);
	MFrequency::Ref::frameDirection(outref, inref).
	  getApp(*respos);
	g0 = sqrt(1. - (*solpos * *solpos));
	g1 = (*solpos * *respos);
	g2 = in.getValue();
	in = g2 * g0/(1 + g1);
      }
      break;

      case GEO_BARY: {
	MFrequency::Ref::frameEpoch(inref, outref).
	  getTDB(tdbTime);
	solpos = (MVPosition *) mc.getStruct(MFrequency::MVPOS1);
	*solpos = ((Aberration *)
		   mc.getStruct(MFrequency::ABERTO))->
	  operator()(tdbTime);
	respos = (MVDirection *) mc.getStruct(MFrequency::MVDIR1);
	MFrequency::Ref::frameDirection(outref, inref).
	  getApp(*respos);
	g0 = sqrt(1. - (*solpos * *solpos));
	g1 = (*solpos * *respos);
	g2 = in.getValue();
	in = g2 * g0/(1 - g1);
      }	
      break;

      case TOPO_GEO: {
	MFrequency::Ref::frameEpoch(inref, outref).
	  getLASTr(g1);
	MFrequency::Ref::framePosition(inref, outref).
	  getRadius(lengthE);
	MFrequency::Ref::frameEpoch(inref, outref).
	  getTDB(tdbTime);
	MFrequency::Ref::framePosition(inref, outref).
	  getLat(g3);
	solpos = (MVPosition *) mc.getStruct(MFrequency::MVPOS1);
	g2 = MeasData::diurnalAber(lengthE, tdbTime);
	*solpos = MVDirection(g1, g3);
	solpos->readjust(g2);
	respos = (MVDirection *) mc.getStruct(MFrequency::MVDIR1);
	MFrequency::Ref::frameDirection(outref, inref).
	  getApp(*respos);
	g0 = sqrt(1. - (*solpos * *solpos));
	g1 = (*solpos * *respos);
	g2 = in.getValue();
	in = g2 * g0/(1 - g1);
      }
      break;

      case LSR_GALACTO:
	solpos = (MVPosition *) mc.getStruct(MFrequency::MVPOS1);
	*solpos = MVPosition(MeasData::velocityLSRGal(0));
	respos = (MVDirection *) mc.getStruct(MFrequency::MVDIR1);
	MFrequency::Ref::frameDirection(inref, outref).
	  getJ2000(*respos);
	g0 = sqrt(1. - (*solpos * *solpos)/C::c/C::c);
	g1 = (*solpos * *respos) / C::c;
	g2 = in.getValue();
	in = g2 * g0/(1 - g1);
	break;

      case GALACTO_LSR:
	solpos = (MVPosition *) mc.getStruct(MFrequency::MVPOS1);
	*solpos = MVPosition(MeasData::velocityLSRGal(0));
	respos = (MVDirection *) mc.getStruct(MFrequency::MVDIR1);
	MFrequency::Ref::frameDirection(outref, inref).
	  getJ2000(*respos);
	g0 = sqrt(1. - (*solpos * *solpos)/C::c/C::c);
	g1 = (*solpos * *respos) / C::c;
	g2 = in.getValue();
	in = g2 * g0/(1 + g1);
	break;

      case LSRK_BARY:
	solpos = (MVPosition *) mc.getStruct(MFrequency::MVPOS1);
	*solpos = MVPosition(MeasData::velocityLSRK(0));
	respos = (MVDirection *) mc.getStruct(MFrequency::MVDIR1);
	MFrequency::Ref::frameDirection(outref, inref).
	  getJ2000(*respos);
	g0 = sqrt(1. - (*solpos * *solpos)/C::c/C::c);
	g1 = (*solpos * *respos) / C::c;
	g2 = in.getValue();
	in = g2 * g0/(1 + g1);
	break;

      case BARY_LSRK:
	solpos = (MVPosition *) mc.getStruct(MFrequency::MVPOS1);
	*solpos = MVPosition(MeasData::velocityLSRK(0));
	respos = (MVDirection *) mc.getStruct(MFrequency::MVDIR1);
	MFrequency::Ref::frameDirection(inref, outref).
	  getJ2000(*respos);
	g0 = sqrt(1. - (*solpos * *solpos)/C::c/C::c);
	g1 = (*solpos * *respos) / C::c;
	g2 = in.getValue();
	in = g2 * g0/(1 - g1);
	break;

      case REST_LSR:
	MFrequency::Ref::frameRadialVelocity(inref, outref).
	  getLSR(g1);
	respos = (MVDirection *) mc.getStruct(MFrequency::MVDIR1);
	MFrequency::Ref::frameDirection(inref, outref).
	  getJ2000(*respos);
	g0 = sqrt(1. - g1 * g1/C::c/C::c);
	g1 /= C::c;
	g2 = in.getValue();
	in = g2 * g0/(1 + g1);
	break;

      case LSR_REST:
	MFrequency::Ref::frameRadialVelocity(inref, outref).
	  getLSR(g1);
	respos = (MVDirection *) mc.getStruct(MFrequency::MVDIR1);
	MFrequency::Ref::frameDirection(inref, outref).
	  getJ2000(*respos);
	g0 = sqrt(1. - g1 * g1/C::c/C::c);
	g1 /= C::c;
	g2 = in.getValue();
	in = g2 * g0/(1 - g1);
	break;

      default:
	break;
      }
    }
}
