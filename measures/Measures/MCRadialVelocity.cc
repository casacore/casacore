//# MCRadialVelocity.cc: MRadialVelocity conversion routines 
//# Copyright (C) 1995-1998,2000,2001,2003,2007
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
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/measures/Measures/MCRadialVelocity.h>
#include <casacore/casa/Quanta/MVPosition.h>
#include <casacore/casa/Quanta/MVDirection.h>
#include <casacore/measures/Measures/Aberration.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/measures/Measures/MDoppler.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Statics
uInt MCRadialVelocity::ToRef_p[N_Routes][3] = {
  {MRadialVelocity::LSRD,	MRadialVelocity::BARY,	 	0},
  {MRadialVelocity::BARY,	MRadialVelocity::LSRD,		0},
  {MRadialVelocity::BARY,	MRadialVelocity::GEO,		0},
  {MRadialVelocity::GEO,	MRadialVelocity::TOPO,		2},
  {MRadialVelocity::GEO,	MRadialVelocity::BARY,		0},
  {MRadialVelocity::TOPO,	MRadialVelocity::GEO,		2},
  {MRadialVelocity::LSRD,	MRadialVelocity::GALACTO,	0},
  {MRadialVelocity::GALACTO,	MRadialVelocity::LSRD,		0},
  {MRadialVelocity::LSRK,	MRadialVelocity::BARY,		0},
  {MRadialVelocity::BARY,	MRadialVelocity::LSRK,		0},
  {MRadialVelocity::BARY,	MRadialVelocity::LGROUP,	0},
  {MRadialVelocity::LGROUP,	MRadialVelocity::BARY,	 	0},
  {MRadialVelocity::BARY,	MRadialVelocity::CMB,		0},
  {MRadialVelocity::CMB,	MRadialVelocity::BARY,		0} };
uInt MCRadialVelocity::
FromTo_p[MRadialVelocity::N_Types][MRadialVelocity::N_Types];
MutexedInit MCRadialVelocity::theirMutexedInit (MCRadialVelocity::doFillState);

//# Constructors
MCRadialVelocity::MCRadialVelocity() :
  MVPOS1(0), MVDIR1(0), ABERFROM(0), ABERTO(0) {
    fillState();
}

//# Destructor
MCRadialVelocity::~MCRadialVelocity() {
  clearConvert();
}

//# Operators

//# Member functions

void MCRadialVelocity::getConvert(MConvertBase &mc,
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

  if (False) initConvert(which, mc);	// Stop warning

  if (!MVPOS1) MVPOS1 = new MVPosition();
  if (!MVDIR1) MVDIR1 = new MVDirection();

  switch (which) {

  case BARY_GEO:
    if (ABERFROM) delete ABERFROM;
    ABERFROM = new Aberration(Aberration::STANDARD);
    mc.addFrameType(MeasFrame::EPOCH);
    mc.addFrameType(MeasFrame::DIRECTION);
    break;

  case GEO_BARY:
    if (ABERTO) delete ABERTO;
    ABERTO = new Aberration(Aberration::STANDARD);
    mc.addFrameType(MeasFrame::EPOCH);
    mc.addFrameType(MeasFrame::DIRECTION);
    break;

  case LSRD_BARY:
  case BARY_LSRD:
  case LSRD_GALACTO:
  case GALACTO_LSRD:
  case LSRK_BARY:
  case BARY_LSRK:
  case LGROUP_BARY:
  case BARY_LGROUP:
  case CMB_BARY:
  case BARY_CMB:
    mc.addFrameType(MeasFrame::DIRECTION);
    break;

  case GEO_TOPO:
  case TOPO_GEO:
    mc.addFrameType(MeasFrame::EPOCH);
    mc.addFrameType(MeasFrame::DIRECTION);
    mc.addFrameType(MeasFrame::POSITION);
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

  for (Int i=0; i<mc.nMethod(); i++) {

    switch (mc.getMethod(i)) {

    case LSRD_BARY: {
      *MVPOS1 = MVPosition(MeasTable::velocityLSR(0));
      MRadialVelocity::Ref::frameDirection(outref, inref).
	getJ2000(*MVDIR1);
      g1 = (*MVPOS1 * *MVDIR1) / C::c;
      g2 = in.getValue()/C::c;
      in = (g2 - g1)/(1 - g2 * g1) * C::c;
    }
    break;

    case BARY_LSRD: {
      *MVPOS1 = MVPosition(MeasTable::velocityLSR(0));
      MRadialVelocity::Ref::frameDirection(inref, outref).
	getJ2000(*MVDIR1);
      g1 = (*MVPOS1 * *MVDIR1) / C::c;
      g2 = in.getValue()/C::c;
      in = (g2 + g1)/(1 + g2 * g1) * C::c;
    }
    break;

    case BARY_GEO: {
      MRadialVelocity::Ref::frameEpoch(outref, inref).
	getTDB(tdbTime);
      *MVPOS1 = ABERFROM->operator()(tdbTime);
      MRadialVelocity::Ref::frameDirection(outref, inref).
	getJ2000(*MVDIR1);
      g1 = *MVPOS1 * *MVDIR1;
      g2 = in.getValue()/C::c;
      in = (g2 - g1)/(1 - g2 * g1) * C::c;
    }	
    break;

    case GEO_TOPO: {
      MRadialVelocity::Ref::frameEpoch(outref, inref).
	getLASTr(g1);
      MRadialVelocity::Ref::framePosition(outref, inref).
	getRadius(lengthE);
      MRadialVelocity::Ref::frameEpoch(outref, inref).
	getTDB(tdbTime);
      MRadialVelocity::Ref::framePosition(outref, inref).
	getLat(g3);
      g2 = MeasTable::diurnalAber(lengthE, tdbTime);
      *MVPOS1 = MVDirection(C::pi_2 + g1, 0.0);
      MVPOS1->readjust(g2 * cos(g3));
      MRadialVelocity::Ref::frameDirection(outref, inref).
	getApp(*MVDIR1);
      g1 = *MVPOS1 * *MVDIR1;
      g2 = in.getValue()/C::c;
      in = (g2 - g1)/(1 - g2 * g1) * C::c;
    }
    break;

    case GEO_BARY: {
      MRadialVelocity::Ref::frameEpoch(inref, outref).
	getTDB(tdbTime);
      *MVPOS1 = ABERTO->operator()(tdbTime);
      MRadialVelocity::Ref::frameDirection(outref, inref).
	getJ2000(*MVDIR1);
      g1 = *MVPOS1 * *MVDIR1;
      g2 = in.getValue()/C::c;
      in = (g2 + g1)/(1 + g2 * g1) * C::c;
    }	
    break;

    case TOPO_GEO: {
      MRadialVelocity::Ref::frameEpoch(inref, outref).
	getLASTr(g1);
      MRadialVelocity::Ref::framePosition(inref, outref).
	getRadius(lengthE);
      MRadialVelocity::Ref::frameEpoch(inref, outref).
	getTDB(tdbTime);
      MRadialVelocity::Ref::framePosition(inref, outref).
	getLat(g3);
      g2 = MeasTable::diurnalAber(lengthE, tdbTime);
      *MVPOS1 = MVDirection(C::pi_2 + g1, 0.0);
      MVPOS1->readjust(g2 * cos(g3));
      MRadialVelocity::Ref::frameDirection(outref, inref).
	getApp(*MVDIR1);
      g1 = *MVPOS1 * *MVDIR1;
      g2 = in.getValue()/C::c;
      in = (g2 + g1)/(1 + g2 * g1) * C::c;
    }
    break;

    case LSRD_GALACTO:
      *MVPOS1 = MVPosition(MeasTable::velocityLSRGal(0));
      MRadialVelocity::Ref::frameDirection(inref, outref).
	getJ2000(*MVDIR1);
      g1 = (*MVPOS1 * *MVDIR1) / C::c;
      g2 = in.getValue()/C::c;
      in = (g2 + g1)/(1 + g2 * g1) * C::c;
      break;

    case GALACTO_LSRD:
      *MVPOS1 = MVPosition(MeasTable::velocityLSRGal(0));
      MRadialVelocity::Ref::frameDirection(outref, inref).
	getJ2000(*MVDIR1);
      g1 = (*MVPOS1 * *MVDIR1) / C::c;
      g2 = in.getValue()/C::c;
      in = (g2 - g1)/(1 - g2 * g1) * C::c;
      break;

    case LSRK_BARY:
      *MVPOS1 = MVPosition(MeasTable::velocityLSRK(0));
      MRadialVelocity::Ref::frameDirection(outref, inref).
	getJ2000(*MVDIR1);
      g1 = (*MVPOS1 * *MVDIR1) / C::c;
      g2 = in.getValue()/C::c;
      in = (g2 - g1)/(1 - g2 * g1) * C::c;
      break;

    case BARY_LSRK:
      *MVPOS1 = MVPosition(MeasTable::velocityLSRK(0));
      MRadialVelocity::Ref::frameDirection(inref, outref).
	getJ2000(*MVDIR1);
      g1 = (*MVPOS1 * *MVDIR1) / C::c;
      g2 = in.getValue()/C::c;
      in = (g2 + g1)/(1 + g2 * g1) * C::c;
      break;

    case LGROUP_BARY:
      *MVPOS1 = MVPosition(MeasTable::velocityLGROUP(0));
      MRadialVelocity::Ref::frameDirection(outref, inref).
	getJ2000(*MVDIR1);
      g1 = (*MVPOS1 * *MVDIR1) / C::c;
      g2 = in.getValue()/C::c;
      in = (g2 - g1)/(1 - g2 * g1) * C::c;
      break;

    case BARY_LGROUP:
      *MVPOS1 = MVPosition(MeasTable::velocityLGROUP(0));
      MRadialVelocity::Ref::frameDirection(inref, outref).
	getJ2000(*MVDIR1);
      g1 = (*MVPOS1 * *MVDIR1) / C::c;
      g2 = in.getValue()/C::c;
      in = (g2 + g1)/(1 + g2 * g1) * C::c;
      break;

    case CMB_BARY:
      *MVPOS1 = MVPosition(MeasTable::velocityCMB(0));
      MRadialVelocity::Ref::frameDirection(outref, inref).
	getJ2000(*MVDIR1);
      g1 = (*MVPOS1 * *MVDIR1) / C::c;
      g2 = in.getValue()/C::c;
      in = (g2 - g1)/(1 - g2 * g1) * C::c;
      break;

    case BARY_CMB:
      *MVPOS1 = MVPosition(MeasTable::velocityCMB(0));
      MRadialVelocity::Ref::frameDirection(inref, outref).
	getJ2000(*MVDIR1);
      g1 = (*MVPOS1 * *MVDIR1) / C::c;
      g2 = in.getValue()/C::c;
      in = (g2 + g1)/(1 + g2 * g1) * C::c;
      break;

    default:
      break;
    } // switch
  } // for
}

String MCRadialVelocity::showState() {
  fillState();
  return MCBase::showState(MCRadialVelocity::FromTo_p[0],
			   MRadialVelocity::N_Types, MCRadialVelocity::N_Routes,
			   MCRadialVelocity::ToRef_p);
}

void MCRadialVelocity::doFillState (void*) {
  MRadialVelocity::checkMyTypes();
  MCBase::makeState(FromTo_p[0], MRadialVelocity::N_Types, N_Routes, ToRef_p);
}

} //# NAMESPACE CASACORE - END

