//# MCDoppler.cc: MDoppler conversion routines 
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
typedef Quantum<Double> gpp_MCDoppler_bug1;
#endif
#include <aips/Measures/MCDoppler.h>
#include <aips/Quanta/QMath.h>

//# Constructors
MCDoppler::MCDoppler() {}

//# Destructor
MCDoppler::~MCDoppler() {
  clearConvert();
}

//# Operators

//# Member functions

void MCDoppler::getConvert(MConvertBase &mc,
			   const MRBase &inref, 
			   const MRBase &outref) {

// Array of conversion routines to call
  static const MCDoppler::Routes 
    FromTo[MDoppler::N_Types][MDoppler::N_Types] = {
      { MCDoppler::N_Routes,
	MCDoppler::RADIO_RATIO,
	MCDoppler::RADIO_RATIO,
	MCDoppler::RADIO_RATIO,
	MCDoppler::RADIO_RATIO},
      { MCDoppler::Z_RATIO,
	MCDoppler::N_Routes,
	MCDoppler::Z_RATIO,
	MCDoppler::Z_RATIO,
	MCDoppler::Z_RATIO},
      { MCDoppler::RATIO_RADIO,
	MCDoppler::RATIO_Z,
	MCDoppler::N_Routes,
	MCDoppler::RATIO_BETA,
	MCDoppler::RATIO_GAMMA},
      { MCDoppler::BETA_RATIO,
	MCDoppler::BETA_RATIO,
	MCDoppler::BETA_RATIO,
	MCDoppler::N_Routes,
	MCDoppler::BETA_RATIO},
      { MCDoppler::GAMMA_RATIO,
	MCDoppler::GAMMA_RATIO,
	MCDoppler::GAMMA_RATIO,
	MCDoppler::GAMMA_RATIO,
	MCDoppler::N_Routes}
    };

// List of codes converted to
    static const MDoppler::Types ToRef[MCDoppler::N_Routes] = {
      MDoppler::RATIO,
      MDoppler::RATIO,
      MDoppler::RATIO,
      MDoppler::RATIO,
      MDoppler::RADIO,
      MDoppler::Z,
      MDoppler::BETA,
      MDoppler::GAMMA
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

void MCDoppler::clearConvert() {
}

//# Conversion routines
void MCDoppler::initConvert(uInt which, MConvertBase &mc) {

  switch (which) {

  case RADIO_RATIO:
    break;

  case Z_RATIO:
    break;

  case BETA_RATIO:
    break;

  case GAMMA_RATIO:
    break;

  case RATIO_RADIO: 
    break;

  case RATIO_Z:
    break;

  case RATIO_BETA:
    break;

  case RATIO_GAMMA:
    break;

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
    }
    in = t;
  }
}
