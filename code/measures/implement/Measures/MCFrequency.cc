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
#include <aips/Mathematics/Constants.h>
#include <aips/Measures/MCFrequency.h>
#include <aips/Measures/MCFrame.h>
#include <aips/Quanta/MVPosition.h>
#include <aips/Quanta/MVDirection.h>
#include <aips/Measures/Aberration.h>
#include <aips/Measures/MeasTable.h>

//# Statics
Bool MCFrequency::stateMade_p = False;
uInt MCFrequency::ToRef_p[N_Routes][3] = {
  {MFrequency::LSR,	MFrequency::BARY,	0},
  {MFrequency::BARY,	MFrequency::LSR,	0},
  {MFrequency::BARY,	MFrequency::GEO,	0},
  {MFrequency::GEO,	MFrequency::TOPO,	0},
  {MFrequency::GEO,	MFrequency::BARY,	0},
  {MFrequency::TOPO,	MFrequency::GEO,	0},
  {MFrequency::LSR,	MFrequency::GALACTO,	0},
  {MFrequency::GALACTO,	MFrequency::LSR,	0},
  {MFrequency::LSRK,	MFrequency::BARY,	0},
  {MFrequency::BARY,	MFrequency::LSRK,	0},
  {MFrequency::REST,	MFrequency::LSR,	2},
  {MFrequency::LSR,	MFrequency::REST,	2} };
uInt MCFrequency::FromTo_p[MFrequency::N_Types][MFrequency::N_Types];


//# Constructors
MCFrequency::MCFrequency() :
  MVPOS1(0), MVDIR1(0), ABERFROM(0), ABERTO(0) {
  if (!stateMade_p) {
    MCBase::makeState(MCFrequency::stateMade_p, MCFrequency::FromTo_p[0],
		      MFrequency::N_Types, MCFrequency::N_Routes,
		      MCFrequency::ToRef_p);
  };
}

//# Destructor
MCFrequency::~MCFrequency() {
  clearConvert();
}

//# Operators

//# Member functions

void MCFrequency::getConvert(MConvertBase &mc,
			     const MRBase &inref, 
			     const MRBase &outref) {

  Int iin  = inref.getType();
  Int iout = outref.getType();
  Int tmp;
  while (iin != iout) {
    tmp = FromTo_p[iin][iout];
    iin = ToRef_p[tmp][1];
    mc.addMethod(tmp);
    initConvert(tmp, mc);
  };
}

void MCFrequency::clearConvert() {
  delete MVPOS1; MVPOS1 = 0;
  delete MVDIR1; MVDIR1 = 0;
  delete ABERFROM; ABERFROM = 0;
  delete ABERTO; ABERTO = 0;
}

//# Conversion routines
void MCFrequency::initConvert(uInt which, MConvertBase &mc) {

  if (False) initConvert(which, mc);	// Stop warning

  if (!MVPOS1) MVPOS1 = new MVPosition();
  if (!MVDIR1) MVDIR1 = new MVDirection();

  switch (which) {
      
  case BARY_GEO:
    if (ABERFROM) delete ABERFROM;
    ABERFROM = new Aberration(Aberration::STANDARD);
    break;
      
  case GEO_BARY:
    if (ABERTO) delete ABERTO;
    ABERTO = new Aberration(Aberration::STANDARD);
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
    }; // switch
  }; //for
}
