//# MCDoppler.cc: MDoppler conversion routines 
//# Copyright (C) 1995,1996,1997,1998,2000
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
#include <casacore/measures/Measures/MCDoppler.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Statics
uInt MCDoppler::ToRef_p[N_Routes][3] = {
    {MDoppler::RADIO,	MDoppler::RATIO,	0}, 
    {MDoppler::Z,	MDoppler::RATIO,	0},
    {MDoppler::BETA,	MDoppler::RATIO,	0},
    {MDoppler::GAMMA,	MDoppler::RATIO,	0},
    {MDoppler::RATIO,	MDoppler::RADIO,	0}, 
    {MDoppler::RATIO,	MDoppler::Z,		0},
    {MDoppler::RATIO,	MDoppler::BETA,		0},
    {MDoppler::RATIO,	MDoppler::GAMMA,	0} };
uInt MCDoppler::FromTo_p[MDoppler::N_Types][MDoppler::N_Types];
MutexedInit MCDoppler::theirMutexedInit (MCDoppler::doFillState);

//# Constructors
MCDoppler::MCDoppler() {
    fillState();
}

//# Destructor
MCDoppler::~MCDoppler() {
  clearConvert();
}

//# Operators

//# Member functions

void MCDoppler::getConvert(MConvertBase &mc,
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

void MCDoppler::clearConvert() {
}

//# Conversion routines
void MCDoppler::initConvert(uInt which, MConvertBase &mc) {

  if (False) initConvert(which, mc);	// Stop warning

  switch (which) {
    
  default:
    break;
  }
}

void MCDoppler::doConvert(MeasValue &in,
			  MRBase &inref,
			  MRBase &outref,
			  const MConvertBase &mc) {
  doConvert(*(MVDoppler*)&in,
	    inref, outref, mc);
}

void MCDoppler::doConvert(MVDoppler &in,
			  MRBase &inref,
			  MRBase &outref,
			  const MConvertBase &mc) {

  if (False) {inref.getType(); outref.getType(); } // to stop warning
    
  Double t = (Double) in;

  for (Int i=0; i<mc.nMethod(); i++) {

    switch (mc.getMethod(i)) {
	
    case RADIO_RATIO:
      t = 1- t;
      break;

    case Z_RATIO:
      t = 1.0/(t+1.0);
      break;

    case BETA_RATIO:
      t = sqrt((1.0-t)/(1.0+t));
      break;

    case GAMMA_RATIO:
      t = t*(1.0-sqrt(1.0-1.0/t/t));
      break;

    case RATIO_RADIO:
      t = 1.0- t;
      break;

    case RATIO_Z:
      t = 1.0/t -1.0;
      break;

    case RATIO_BETA:
      t = (1.0-t*t)/(1.0+t*t);
      break;

    case RATIO_GAMMA:
      t = (1.0+t*t)/2.0/t;
      break;

    default:
      break;
    } // switch
    in = t;
  } // for
}

String MCDoppler::showState() {
  fillState();
  return MCBase::showState(MCDoppler::FromTo_p[0],
			   MDoppler::N_Types, MCDoppler::N_Routes,
			   MCDoppler::ToRef_p);
}

  void MCDoppler::doFillState (void*) {
  MDoppler::checkMyTypes();
  MCBase::makeState(FromTo_p[0],  MDoppler::N_Types, N_Routes, ToRef_p);
}

} //# NAMESPACE CASACORE - END

