//# MCRadialVelocity.cc: MRadialVelocity conversion routines 
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
#include <aips/Measures/MCRadialVelocity.h>
#include <aips/Measures/MCFrame.h>
#include <aips/Measures/MVPosition.h>
#include <aips/Measures/MVDirection.h>
#include <aips/Measures/Aberration.h>
#include <aips/Measures/MeasTable.h>
#include <aips/Measures/MDoppler.h>

//# Constructors
MCRadialVelocity::MCRadialVelocity() :
  MVPOS1(0), MVDIR1(0), ABERTO(0), ABERFROM(0) {}

//# Destructor
MCRadialVelocity::~MCRadialVelocity() {
  clearConvert();
}

//# Operators

//# Member functions

void MCRadialVelocity::getConvert(MConvertBase &mc,
				  const MRBase &inref, 
				  const MRBase &outref) {
// Array of conversion routines to call
  static const MCRadialVelocity::Routes 
    FromTo[MRadialVelocity::N_Types][MRadialVelocity::N_Types] = {
      { MCRadialVelocity::N_Routes,
	MCRadialVelocity::LSR_BARY,
	MCRadialVelocity::LSR_BARY,
	MCRadialVelocity::LSR_BARY,
	MCRadialVelocity::LSR_BARY,
	MCRadialVelocity::LSR_GALACTO},
      { MCRadialVelocity::LSRK_BARY,
	MCRadialVelocity::N_Routes,
	MCRadialVelocity::LSRK_BARY,
	MCRadialVelocity::LSRK_BARY,
	MCRadialVelocity::LSRK_BARY,
	MCRadialVelocity::LSRK_BARY},
      { MCRadialVelocity::BARY_LSR,
	MCRadialVelocity::BARY_LSRK,
	MCRadialVelocity::N_Routes,
	MCRadialVelocity::BARY_GEO,
	MCRadialVelocity::BARY_GEO,
	MCRadialVelocity::BARY_LSR},
      { MCRadialVelocity::GEO_BARY,
	MCRadialVelocity::GEO_BARY,
	MCRadialVelocity::GEO_BARY,
	MCRadialVelocity::N_Routes,
	MCRadialVelocity::GEO_TOPO,
	MCRadialVelocity::GEO_BARY},
      { MCRadialVelocity::TOPO_GEO,
	MCRadialVelocity::TOPO_GEO,
	MCRadialVelocity::TOPO_GEO,
	MCRadialVelocity::TOPO_GEO,
	MCRadialVelocity::N_Routes,
	MCRadialVelocity::TOPO_GEO},
      { MCRadialVelocity::GALACTO_LSR,
	MCRadialVelocity::GALACTO_LSR,
	MCRadialVelocity::GALACTO_LSR,
	MCRadialVelocity::GALACTO_LSR,
	MCRadialVelocity::GALACTO_LSR,
	MCRadialVelocity::N_Routes}
    };
    
// List of codes converted to
    static const MRadialVelocity::Types ToRef[MCRadialVelocity::N_Routes] = {
      MRadialVelocity::BARY,
      MRadialVelocity::LSR,
      MRadialVelocity::GEO,
      MRadialVelocity::TOPO,
      MRadialVelocity::BARY,
      MRadialVelocity::GEO,
      MRadialVelocity::GALACTO,
      MRadialVelocity::LSR,
      MRadialVelocity::BARY,
      MRadialVelocity::LSRK
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

void MCRadialVelocity::clearConvert() {
  delete MVPOS1; MVPOS1 = 0;
  delete MVDIR1; MVDIR1 = 0;
  delete ABERFROM; ABERFROM = 0;
  delete ABERTO; ABERTO = 0;
}

//# Conversion routines
void MCRadialVelocity::initConvert(uInt which, MConvertBase &mc) {
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

  default:
    break;
  }
}

void MCRadialVelocity::doConvert(MeasValue &in,
				 MRBase &inref,
				 MRBase &outref,
				 const MConvertBase &mc) {
  doConvert(*(MVRadialVelocity*)&in,
	    inref, outref, mc);
}

void MCRadialVelocity::doConvert(MVRadialVelocity &in,
				 MRBase &inref,
				 MRBase &outref,
				 const MConvertBase &mc) {
  Double g1, g2, g3, lengthE, tdbTime;

  MCFrame::make(inref.getFrame());
  MCFrame::make(outref.getFrame());

  for (Int i=0; i<mc.nMethod(); i++) {

    switch (mc.getMethod(i)) {

    case LSR_BARY: {
      *MVPOS1 = MVPosition(MeasTable::velocityLSR(0));
      ((MCFrame *)(MRadialVelocity::Ref::frameDirection(outref, inref).
		   getMCFramePoint()))->
	getJ2000(*MVDIR1);
      g1 = (*MVPOS1 * *MVDIR1) / C::c;
      g2 = in.getValue()/C::c;
      in = (g2 - g1)/(1 - g2 * g1) * C::c;
    }
    break;

    case BARY_LSR: {
      *MVPOS1 = MVPosition(MeasTable::velocityLSR(0));
      ((MCFrame *)(MRadialVelocity::Ref::frameDirection(inref, outref).
		   getMCFramePoint()))->
	getJ2000(*MVDIR1);
      g1 = (*MVPOS1 * *MVDIR1) / C::c;
      g2 = in.getValue()/C::c;
      in = (g2 + g1)/(1 + g2 * g1) * C::c;
    }
    break;

    case BARY_GEO: {
      ((MCFrame *)(MRadialVelocity::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      *MVPOS1 = ABERFROM->operator()(tdbTime);
      ((MCFrame *)(MRadialVelocity::Ref::frameDirection(outref, inref).
		   getMCFramePoint()))->
	getJ2000(*MVDIR1);
      g1 = *MVPOS1 * *MVDIR1;
      g2 = in.getValue()/C::c;
      in = (g2 - g1)/(1 - g2 * g1) * C::c;
    }	
    break;

    case GEO_TOPO: {
      ((MCFrame *)(MRadialVelocity::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getLASTr(g1);
      ((MCFrame *)(MRadialVelocity::Ref::framePosition(outref, inref).
		   getMCFramePoint()))->
	getRadius(lengthE);
      ((MCFrame *)(MRadialVelocity::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      ((MCFrame *)(MRadialVelocity::Ref::framePosition(outref, inref).
		   getMCFramePoint()))->
	getLat(g3);
      g2 = MeasTable::diurnalAber(lengthE, tdbTime);
      *MVPOS1 = MVDirection(C::pi_2 + g1, 0.0);
      MVPOS1->readjust(g2 * cos(g3));
      ((MCFrame *)(MRadialVelocity::Ref::frameDirection(outref, inref).
		   getMCFramePoint()))->
	getApp(*MVDIR1);
      g1 = *MVPOS1 * *MVDIR1;
      g2 = in.getValue()/C::c;
      in = (g2 - g1)/(1 - g2 * g1) * C::c;
    }
    break;

    case GEO_BARY: {
      ((MCFrame *)(MRadialVelocity::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      *MVPOS1 = ABERTO->operator()(tdbTime);
      ((MCFrame *)(MRadialVelocity::Ref::frameDirection(outref, inref).
		   getMCFramePoint()))->
	getApp(*MVDIR1);
      g1 = *MVPOS1 * *MVDIR1;
      g2 = in.getValue()/C::c;
      in = (g2 + g1)/(1 + g2 * g1) * C::c;
    }	
    break;

    case TOPO_GEO: {
      ((MCFrame *)(MRadialVelocity::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getLASTr(g1);
      ((MCFrame *)(MRadialVelocity::Ref::framePosition(inref, outref).
		   getMCFramePoint()))->
	getRadius(lengthE);
      ((MCFrame *)(MRadialVelocity::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      ((MCFrame *)(MRadialVelocity::Ref::framePosition(inref, outref).
		   getMCFramePoint()))->
	getLat(g3);
      g2 = MeasTable::diurnalAber(lengthE, tdbTime);
      *MVPOS1 = MVDirection(C::pi_2 + g1, 0.0);
      MVPOS1->readjust(g2 * cos(g3));
      ((MCFrame *)(MRadialVelocity::Ref::frameDirection(outref, inref).
		   getMCFramePoint()))->
	getApp(*MVDIR1);
      g1 = *MVPOS1 * *MVDIR1;
      g2 = in.getValue()/C::c;
      in = (g2 + g1)/(1 + g2 * g1) * C::c;
    }
    break;

    case LSR_GALACTO:
      *MVPOS1 = MVPosition(MeasTable::velocityLSRGal(0));
      ((MCFrame *)(MRadialVelocity::Ref::frameDirection(inref, outref).
		   getMCFramePoint()))->
	getJ2000(*MVDIR1);
      g1 = (*MVPOS1 * *MVDIR1) / C::c;
      g2 = in.getValue()/C::c;
      in = (g2 + g1)/(1 + g2 * g1) * C::c;
      break;

    case GALACTO_LSR:
      *MVPOS1 = MVPosition(MeasTable::velocityLSRGal(0));
      ((MCFrame *)(MRadialVelocity::Ref::frameDirection(outref, inref).
		   getMCFramePoint()))->
	getJ2000(*MVDIR1);
      g1 = (*MVPOS1 * *MVDIR1) / C::c;
      g2 = in.getValue()/C::c;
      in = (g2 - g1)/(1 - g2 * g1) * C::c;
      break;

    case LSRK_BARY:
      *MVPOS1 = MVPosition(MeasTable::velocityLSRK(0));
      ((MCFrame *)(MRadialVelocity::Ref::frameDirection(outref, inref).
		   getMCFramePoint()))->
	getJ2000(*MVDIR1);
      g1 = (*MVPOS1 * *MVDIR1) / C::c;
      g2 = in.getValue()/C::c;
      in = (g2 - g1)/(1 - g2 * g1) * C::c;
      break;

    case BARY_LSRK:
      *MVPOS1 = MVPosition(MeasTable::velocityLSRK(0));
      ((MCFrame *)(MRadialVelocity::Ref::frameDirection(inref, outref).
		   getMCFramePoint()))->
	getJ2000(*MVDIR1);
      g1 = (*MVPOS1 * *MVDIR1) / C::c;
      g2 = in.getValue()/C::c;
      in = (g2 + g1)/(1 + g2 * g1) * C::c;
      break;

    default:
      break;
    }
  }
}

