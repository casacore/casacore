//# MDoppler.cc: A Measure: Doppler shift
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
#ifdef __GNUG__
#include <aips/Measures/Quantum.h>
typedef Quantum<Double> gpp_mdoppler_bug1;
#endif
#include <aips/Utilities/Assert.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Measures/MDoppler.h>
#include <aips/Measures/QC.h>
#include <aips/Measures/QMath.h>

//# Constructors
MDoppler::MDoppler() :
  MeasBase<MVDoppler,MDoppler::Ref>() {}

MDoppler::MDoppler(const MVDoppler &dt) : 
  MeasBase<MVDoppler,MDoppler::Ref>(dt,MDoppler::DEFAULT) {}

MDoppler::MDoppler(const MVDoppler &dt, const MDoppler::Ref &rf) : 
  MeasBase<MVDoppler,MDoppler::Ref>(dt,rf) {}

MDoppler::MDoppler(const MVDoppler &dt, uInt rf) : 
  MeasBase<MVDoppler,MDoppler::Ref>(dt,rf) {}

MDoppler::MDoppler(const Quantity &dt) : 
  MeasBase<MVDoppler,MDoppler::Ref>(dt,MDoppler::DEFAULT) {}

MDoppler::MDoppler(const Quantity &dt, const MDoppler::Ref &rf) : 
  MeasBase<MVDoppler,MDoppler::Ref>(dt,rf) {}

MDoppler::MDoppler(const Quantity &dt, uInt rf) : 
  MeasBase<MVDoppler,MDoppler::Ref>(dt,rf) {}

//# Destructor
MDoppler::~MDoppler() {}

//# Operators

//# Member functions
const String &MDoppler::tellMe() const {
    return MDoppler::showMe();
}

const String &MDoppler::showMe() {
    static const String name("Doppler");
    return name;
}

const String &MDoppler::showType(uInt tp) {
    static const String tname[MDoppler::N_Types] = {
	"RADIO", 
	"Z",
	"RATIO",
	"BETA",
	"GAMMA"};

    DebugAssert(tp < MDoppler::N_Types, AipsError);
    return tname[tp];
}

Bool MDoppler::giveMe(const String &in, MDoppler::Ref &mr) {
    static const Int N_name = 7;
    static const String tname[N_name] = {
	"RADIO", 
	"Z",
	"RATIO",
	"BETA",
	"GAMMA",
	"OPTICAL",
	"RELATIVISTIC"};

    static const uInt oname[N_name] = {
	MDoppler::RADIO, 
	MDoppler::Z,
	MDoppler::RATIO,
	MDoppler::BETA,
	MDoppler::GAMMA,
	MDoppler::Z,
	MDoppler::BETA};

    uInt i = Measure::giveMe(in, N_name, tname);

    if (i>=N_name) {
	mr = MDoppler::Ref();
	return False;
    } else {
	mr = MDoppler::Ref(oname[i]);
    };
    return True;
}

Quantity MDoppler::get(const Unit &un) const {
    return data.get(un);
}
    
void *MDoppler::clone() const {
    return ((void *) new MDoppler(*this));
}

void MDoppler::getConvert(MDoppler::Convert &mc,
			    const MDoppler::Ref &inref, 
			    const MDoppler::Ref &outref) {

// Array of conversion routines to call
    static const MDoppler::Routes 
	FromTo[MDoppler::N_Types][MDoppler::N_Types] = {
	{ MDoppler::N_Routes,
	  MDoppler::RADIO_RATIO,
	  MDoppler::RADIO_RATIO,
	  MDoppler::RADIO_RATIO,
	  MDoppler::RADIO_RATIO},
        { MDoppler::Z_RATIO,
	  MDoppler::N_Routes,
	  MDoppler::Z_RATIO,
	  MDoppler::Z_RATIO,
	  MDoppler::Z_RATIO},
        { MDoppler::RATIO_RADIO,
	  MDoppler::RATIO_Z,
	  MDoppler::N_Routes,
	  MDoppler::RATIO_BETA,
	  MDoppler::RATIO_GAMMA},
        { MDoppler::BETA_RATIO,
	  MDoppler::BETA_RATIO,
	  MDoppler::BETA_RATIO,
	  MDoppler::N_Routes,
	  MDoppler::BETA_RATIO},
        { MDoppler::GAMMA_RATIO,
	  MDoppler::GAMMA_RATIO,
	  MDoppler::GAMMA_RATIO,
	  MDoppler::GAMMA_RATIO,
	  MDoppler::N_Routes}
	};

// List of codes converted to
    static const MDoppler::Types ToRef[MDoppler::N_Routes] = {
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

void MDoppler::clearConvert(MDoppler::Convert &mc) {
}

//# Conversion routines
void MDoppler::initConvert(uInt which, MDoppler::Convert &mc) {
    mc.addStruct(0,0);		// To stop warnings
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

void MDoppler::doConvert(MVDoppler &in,
			 const MDoppler::Ref &inref,
			 const MDoppler::Ref &outref,
			 const MDoppler::Convert &mc) {
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
