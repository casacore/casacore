//# MCFrequency.cc: MFrequency conversion routines 
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
typedef Quantum<Double> gpp_mepoch_bug1;
#endif
#include <aips/Mathematics/Constants.h>
#include <aips/Quanta/QMath.h>
#include <aips/Measures/MCFrequency.h>
#include <aips/Measures/MCFrame.h>
#include <aips/Quanta/MVPosition.h>
#include <aips/Quanta/MVDirection.h>
#include <aips/Measures/Aberration.h>
#include <aips/Measures/MeasTable.h>

//# Constructors
MCFrequency::MCFrequency() :
  MVPOS1(0), MVDIR1(0), ABERTO(0), ABERFROM(0) {}

//# Destructor
MCFrequency::~MCFrequency() {
  clearConvert();
}

//# Operators

//# Member functions

void MCFrequency::getConvert(MConvertBase &mc,
			     const MRBase &inref, 
			     const MRBase &outref) {
// Array of conversion routines to call
  static const MCFrequency::Routes 
    FromTo[MFrequency::N_Types][MFrequency::N_Types] = {
      { MCFrequency::N_Routes,
	MCFrequency::REST_LSR,
	MCFrequency::REST_LSR,
	MCFrequency::REST_LSR,
	MCFrequency::REST_LSR,
	MCFrequency::REST_LSR,
	MCFrequency::REST_LSR},
      { MCFrequency::LSR_REST,
	MCFrequency::N_Routes,
	MCFrequency::LSR_BARY,
	MCFrequency::LSR_BARY,
	MCFrequency::LSR_BARY,
	MCFrequency::LSR_BARY,
	MCFrequency::LSR_GALACTO},
      { MCFrequency::LSRK_BARY,
	MCFrequency::LSRK_BARY,
	MCFrequency::N_Routes,
	MCFrequency::LSRK_BARY,
	MCFrequency::LSRK_BARY,
	MCFrequency::LSRK_BARY,
	MCFrequency::LSRK_BARY},
      { MCFrequency::BARY_LSR,
	MCFrequency::BARY_LSR,
	MCFrequency::BARY_LSRK,
	MCFrequency::N_Routes,
	MCFrequency::BARY_GEO,
	MCFrequency::BARY_GEO,
	MCFrequency::BARY_LSR},
      { MCFrequency::GEO_BARY,
	MCFrequency::GEO_BARY,
	MCFrequency::GEO_BARY,
	MCFrequency::GEO_BARY,
	MCFrequency::N_Routes,
	MCFrequency::GEO_TOPO,
	MCFrequency::GEO_BARY},
      { MCFrequency::TOPO_GEO,
	MCFrequency::TOPO_GEO,
	MCFrequency::TOPO_GEO,
	MCFrequency::TOPO_GEO,
	MCFrequency::TOPO_GEO,
	MCFrequency::N_Routes,
	MCFrequency::TOPO_GEO},
      { MCFrequency::GALACTO_LSR,
	MCFrequency::GALACTO_LSR,
	MCFrequency::GALACTO_LSR,
	MCFrequency::GALACTO_LSR,
	MCFrequency::GALACTO_LSR,
	MCFrequency::GALACTO_LSR,
	MCFrequency::N_Routes}
    };

// List of codes converted to
    static const MFrequency::Types ToRef[MCFrequency::N_Routes] = {
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

void MCFrequency::clearConvert() {
  delete MVPOS1; MVPOS1 = 0;
  delete MVDIR1; MVDIR1 = 0;
  delete ABERFROM; ABERFROM = 0;
  delete ABERTO; ABERTO = 0;
}

//# Conversion routines
void MCFrequency::initConvert(uInt which, MConvertBase &mc) {
  if (!MVPOS1) {
    MVPOS1 = new MVPosition();
  };
  if (!MVDIR1) {
    MVDIR1 = new MVDirection();
  };

  switch (which) {
      
  case LSR_BARY:
    break;
      
  case BARY_LSR:
    break;
      
  case BARY_GEO:
    if (ABERFROM) delete ABERFROM;
    ABERFROM = new Aberration(Aberration::STANDARD);
    break;
      
  case GEO_TOPO:
    break;
      
  case GEO_BARY:
    if (ABERTO) delete ABERTO;
    ABERTO = new Aberration(Aberration::STANDARD);
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

void MCFrequency::doConvert(MeasValue &in,
			    MRBase &inref,
			    MRBase &outref,
			    const MConvertBase &mc) {
  doConvert(*(MVFrequency*)&in,
	    inref, outref, mc);
}

void MCFrequency::doConvert(MVFrequency &in,
			    MRBase &inref,
			    MRBase &outref,
			    const MConvertBase &mc) {
  Double g0, g1, g2, g3, lengthE, tdbTime;

  MCFrame::make(inref.getFrame());
  MCFrame::make(outref.getFrame());

  for (Int i=0; i<mc.nMethod(); i++) {

    switch (mc.getMethod(i)) {

    case LSR_BARY: {
      *MVPOS1 = MVPosition(MeasTable::velocityLSR(0));
      ((MCFrame *)(MFrequency::Ref::frameDirection(outref, inref).
		   getMCFramePoint()))->
	getJ2000(*MVDIR1);
      g0 = sqrt(1. - (*MVPOS1 * *MVPOS1)/C::c/C::c);
      g1 = (*MVPOS1 * *MVDIR1) / C::c;
      g2 = in.getValue();
      in = g2 * g0/(1 - g1);
    }
    break;

    case BARY_LSR: {
      *MVPOS1 = MVPosition(MeasTable::velocityLSR(0));
      ((MCFrame *)(MFrequency::Ref::frameDirection(inref, outref).
		   getMCFramePoint()))->
	getJ2000(*MVDIR1);
      g0 = sqrt(1. - (*MVPOS1 * *MVPOS1)/C::c/C::c);
      g1 = (*MVPOS1 * *MVDIR1) / C::c;
      g2 = in.getValue();
      in = g2 * g0/(1 + g1);
    }
    break;

    case BARY_GEO: {
      ((MCFrame *)(MFrequency::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      *MVPOS1 = ABERFROM->operator()(tdbTime);
      ((MCFrame *)(MFrequency::Ref::frameDirection(outref, inref).
		   getMCFramePoint()))->
	getJ2000(*MVDIR1);
      g0 = sqrt(1. - (*MVPOS1 * *MVPOS1));
      g1 = (*MVPOS1 * *MVDIR1);
      g2 = in.getValue();
      in = g2 * g0/(1 - g1);
    }	
    break;

    case GEO_TOPO: {
      ((MCFrame *)(MFrequency::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getLASTr(g1);
      ((MCFrame *)(MFrequency::Ref::framePosition(outref, inref).
		   getMCFramePoint()))->
	getRadius(lengthE);
      ((MCFrame *)(MFrequency::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      ((MCFrame *)(MFrequency::Ref::framePosition(outref, inref).
		   getMCFramePoint()))->
	getLat(g3);
      g2 = MeasTable::diurnalAber(lengthE, tdbTime);
      *MVPOS1 = MVDirection(C::pi_2 + g1, 0.0);
      MVPOS1->readjust(g2 * cos(g3));
      ((MCFrame *)(MFrequency::Ref::frameDirection(outref, inref).
		   getMCFramePoint()))->
	getApp(*MVDIR1);
      g0 = sqrt(1. - (*MVPOS1 * *MVPOS1));
      g1 = (*MVPOS1 * *MVDIR1);
      g2 = in.getValue();
      in = g2 * g0/(1 - g1);
    }
    break;

    case GEO_BARY: {
      ((MCFrame *)(MFrequency::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      *MVPOS1 = ABERTO->operator()(tdbTime);
      ((MCFrame *)(MFrequency::Ref::frameDirection(outref, inref).
		   getMCFramePoint()))->
	getApp(*MVDIR1);
      g0 = sqrt(1. - (*MVPOS1 * *MVPOS1));
      g1 = (*MVPOS1 * *MVDIR1);
      g2 = in.getValue();
      in = g2 * g0/(1 + g1);
    }	
    break;

    case TOPO_GEO: {
      ((MCFrame *)(MFrequency::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getLASTr(g1);
      ((MCFrame *)(MFrequency::Ref::framePosition(inref, outref).
		   getMCFramePoint()))->
	getRadius(lengthE);
      ((MCFrame *)(MFrequency::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      ((MCFrame *)(MFrequency::Ref::framePosition(inref, outref).
		   getMCFramePoint()))->
	getLat(g3);
      g2 = MeasTable::diurnalAber(lengthE, tdbTime);
      *MVPOS1 = MVDirection(C::pi_2 + g1, 0.0);
      MVPOS1->readjust(g2);
      ((MCFrame *)(MFrequency::Ref::frameDirection(outref, inref).
		   getMCFramePoint()))->
	getApp(*MVDIR1);
      g0 = sqrt(1. - (*MVPOS1 * *MVPOS1));
      g1 = (*MVPOS1 * *MVDIR1);
      g2 = in.getValue();
      in = g2 * g0/(1 + g1);
    }
    break;

    case LSR_GALACTO:
      *MVPOS1 = MVPosition(MeasTable::velocityLSRGal(0));
      ((MCFrame *)(MFrequency::Ref::frameDirection(inref, outref).
		   getMCFramePoint()))->
	getJ2000(*MVDIR1);
      g0 = sqrt(1. - (*MVPOS1 * *MVPOS1)/C::c/C::c);
      g1 = (*MVPOS1 * *MVDIR1) / C::c;
      g2 = in.getValue();
      in = g2 * g0/(1 + g1);
      break;
      
    case GALACTO_LSR:
      *MVPOS1 = MVPosition(MeasTable::velocityLSRGal(0));
      ((MCFrame *)(MFrequency::Ref::frameDirection(outref, inref).
		   getMCFramePoint()))->
	getJ2000(*MVDIR1);
      g0 = sqrt(1. - (*MVPOS1 * *MVPOS1)/C::c/C::c);
      g1 = (*MVPOS1 * *MVDIR1) / C::c;
      g2 = in.getValue();
      in = g2 * g0/(1 - g1);
      break;

    case LSRK_BARY:
      *MVPOS1 = MVPosition(MeasTable::velocityLSRK(0));
      ((MCFrame *)(MFrequency::Ref::frameDirection(outref, inref).
		   getMCFramePoint()))->
	getJ2000(*MVDIR1);
      g0 = sqrt(1. - (*MVPOS1 * *MVPOS1)/C::c/C::c);
      g1 = (*MVPOS1 * *MVDIR1) / C::c;
      g2 = in.getValue();
      in = g2 * g0/(1 - g1);
      break;

    case BARY_LSRK:
      *MVPOS1 = MVPosition(MeasTable::velocityLSRK(0));
      ((MCFrame *)(MFrequency::Ref::frameDirection(inref, outref).
		   getMCFramePoint()))->
	getJ2000(*MVDIR1);
      g0 = sqrt(1. - (*MVPOS1 * *MVPOS1)/C::c/C::c);
      g1 = (*MVPOS1 * *MVDIR1) / C::c;
      g2 = in.getValue();
      in = g2 * g0/(1 + g1);
      break;

    case REST_LSR:
      ((MCFrame *)(MFrequency::Ref::frameRadialVelocity(inref, outref).
		   getMCFramePoint()))->
	  getLSR(g1);
      ((MCFrame *)(MFrequency::Ref::frameDirection(inref, outref).
		   getMCFramePoint()))->
	getJ2000(*MVDIR1);
      g0 = sqrt(1. - g1 * g1/C::c/C::c);
      g1 /= C::c;
      g2 = in.getValue();
      in = g2 * g0/(1 + g1);
      break;

    case LSR_REST:
      ((MCFrame *)(MFrequency::Ref::frameRadialVelocity(inref, outref).
		   getMCFramePoint()))->
	getLSR(g1);
      ((MCFrame *)(MFrequency::Ref::frameDirection(inref, outref).
		   getMCFramePoint()))->
	getJ2000(*MVDIR1);
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
