//# MCuvw.cc:  Muvw conversion routines 
//# Copyright (C) 1998,1999,2000
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
#include <aips/Exceptions.h>
#include <aips/Measures/MCuvw.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Utilities/Assert.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Measures/MCFrame.h>

//# Statics
Bool MCuvw::stateMade_p = False;
uInt MCuvw::ToRef_p[N_Routes][3] = {
    {Muvw::ITRF,	Muvw::JNAT,		0},
    {Muvw::JNAT,	Muvw::ITRF,		0},
    {Muvw::GALACTIC,	Muvw::J2000,		0},
    {Muvw::GALACTIC,	Muvw::B1950,		3},
    {Muvw::J2000,	Muvw::GALACTIC,		0},
    {Muvw::B1950,	Muvw::GALACTIC,		3},
    {Muvw::J2000,	Muvw::B1950,		3},
    {Muvw::B1950,	Muvw::J2000,		3},
    {Muvw::J2000,	Muvw::JMEAN,		0},
    {Muvw::B1950,	Muvw::BMEAN,		3},
    {Muvw::JMEAN,	Muvw::J2000,		0},
    {Muvw::JMEAN,	Muvw::JTRUE,		0},
    {Muvw::BMEAN,	Muvw::B1950,		3},
    {Muvw::BMEAN,	Muvw::BTRUE,		3},
    {Muvw::JTRUE,	Muvw::JMEAN,		0},
    {Muvw::BTRUE,	Muvw::BMEAN,		3},
    {Muvw::J2000,	Muvw::JNAT,		0},
    {Muvw::JNAT,	Muvw::J2000,		0},
    {Muvw::B1950,	Muvw::APP,		3},
    {Muvw::APP,	 	Muvw::B1950,		3},
    {Muvw::APP,		Muvw::HADEC,		2},
    {Muvw::HADEC,	Muvw::AZEL,		2},
    {Muvw::AZEL,	Muvw::HADEC,		2},
    {Muvw::HADEC,	Muvw::APP,		2},
    {Muvw::AZEL,	Muvw::AZELSW,		0},
    {Muvw::AZELSW,	Muvw::AZEL,		0},
    {Muvw::APP,		Muvw::JNAT,		1},
    {Muvw::JNAT,	Muvw::APP,		1},
    {Muvw::J2000,	Muvw::ECLIPTIC,		0},
    {Muvw::ECLIPTIC,	Muvw::J2000,		0},
    {Muvw::JMEAN,	Muvw::MECLIPTIC,	0},
    {Muvw::MECLIPTIC,	Muvw::JMEAN,		0},
    {Muvw::JTRUE,	Muvw::TECLIPTIC,	0},
    {Muvw::TECLIPTIC,	Muvw::JTRUE,		0},
    {Muvw::GALACTIC,	Muvw::SUPERGAL,		0},
    {Muvw::SUPERGAL,	Muvw::GALACTIC,		0},
    {Muvw::ITRF,	Muvw::HADEC,		2},
    {Muvw::HADEC,	Muvw::ITRF,		2} };
uInt MCuvw::FromTo_p[Muvw::N_Types][Muvw::N_Types];

//# Constructors
MCuvw::MCuvw() : measMath() {
  if (!stateMade_p) {
    MCBase::makeState(MCuvw::stateMade_p, MCuvw::FromTo_p[0],
		      Muvw::N_Types, MCuvw::N_Routes,
		      MCuvw::ToRef_p);
  };
}

//# Destructor
MCuvw::~MCuvw() {
  clearConvert();
}

//# Operators

//# Member functions

void MCuvw::getConvert(MConvertBase &mc,
		       const MRBase &inref, 
		       const MRBase &outref) {
    
  Int iin  = inref.getType();
  Int iout = outref.getType();
  if (iin != iout) {
    Int tmp;
    while (iin != iout) {
      tmp = FromTo_p[iin][iout];
      iin = ToRef_p[tmp][1];
      mc.addMethod(tmp);
      initConvert(tmp, mc);
    };
  };
}

void MCuvw::clearConvert() {
}

//# Conversion routines
void MCuvw::initConvert(uInt which, MConvertBase &mc) {
  if (False) initConvert(which, mc);	// Stop warning
  
  switch (which) {
    
  case ITRF_JNAT:
    measMath.createPrecNutat();
    break;

  case JNAT_ITRF:
    measMath.createPrecNutat();
    break;

  case J2000_JMEAN:
    measMath.createPrecession();
    break;

  case B1950_BMEAN:
    measMath.createPrecessionB1950();
    break;

  case JMEAN_J2000:
    measMath.createPrecession();
    break;

  case JMEAN_JTRUE:
    measMath.createNutation();
    break;

  case BMEAN_B1950:
    measMath.createPrecessionB1950();
    break;

  case BMEAN_BTRUE:
    measMath.createNutationB1950();
    break;

  case JTRUE_JMEAN:
    measMath.createNutation();
    break;
    
  case BTRUE_BMEAN:
    measMath.createNutationB1950();
    break;

  case JNAT_APP:
    measMath.createPrecNutat();
    break;

  case APP_JNAT:
    measMath.createPrecNutat();
    break;

  case B1950_APP:
    measMath.createPrecNutatB1950();
    break;

  case APP_B1950:
    measMath.createPrecNutatB1950();
    break;
    
  default:
    break;
    
  };
}

void MCuvw::doConvert(MeasValue &in,
			    MRBase &inref,
			    MRBase &outref,
			    const MConvertBase &mc) {
  doConvert((MVuvw &) in,
	    inref, outref, mc);
}

void MCuvw::doConvert(MVuvw &in,
			    MRBase &inref,
			    MRBase &outref,
			    const MConvertBase &mc) {
  Double g2;
  
  measMath.initFrame(inref, outref);
  
  for (Int i=0; i<mc.nMethod(); i++) {
    
    switch (mc.getMethod(i)) {
      
    case ITRF_JNAT:
      measMath.applyPolarMotionLong(in);
      measMath.deapplyPrecNutat(in);
      break;

    case JNAT_ITRF:
      measMath.applyPrecNutat(in);
      measMath.deapplyPolarMotionLong(in);
      break;

    case HADEC_ITRF:
      measMath.applyHADECtoITRF(in);
      break;

    case ITRF_HADEC:
      measMath.deapplyHADECtoITRF(in);
      break;

    case GAL_J2000:
      measMath.applyGALtoJ2000(in);
      break;

    case GAL_B1950:
      measMath.applyGALtoB1950(in);
      break;
      
   case J2000_GAL:
      measMath.deapplyGALtoJ2000(in);
      break;

    case B1950_GAL:
      measMath.deapplyGALtoB1950(in);
      break;

    case J2000_B1950:
      in.adjust(g2);
      measMath.applyJ2000toB1950(in);
      in.readjust(g2);
      break;

    case B1950_J2000:
      in.adjust(g2);
      measMath.deapplyJ2000toB1950(in);
      in.readjust(g2);
      break;

    case J2000_JMEAN:
      measMath.applyPrecession(in);
      break;

    case B1950_BMEAN:
      measMath.applyPrecessionB1950(in);
      break;
    
    case JMEAN_J2000:
      measMath.deapplyPrecession(in);
      break;

    case JMEAN_JTRUE:
      measMath.applyNutation(in);
      break;
    
    case BMEAN_B1950:
      measMath.deapplyPrecessionB1950(in);
      break;

    case BMEAN_BTRUE:
      measMath.applyNutationB1950(in);
      break;

    case JTRUE_JMEAN:
      measMath.deapplyNutation(in);
      break;

    case BTRUE_BMEAN:
      measMath.deapplyNutationB1950(in);
      break;
    
    case J2000_JNAT: {
    }
    break;
    
    case JNAT_APP:
      measMath.applyPrecNutat(in);
      break;

    case APP_JNAT:
      measMath.deapplyPrecNutat(in);
      break;

    case JNAT_J2000: {
    }
    break;

    case B1950_APP:
      in.adjust(g2);
      measMath.applyETerms(in);
      in.readjust(g2);
      measMath.applyPrecNutatB1950(in);
      break;

    case APP_B1950:
      measMath.deapplyPrecNutatB1950(in);
      in.adjust(g2);
      measMath.deapplyETerms(in);
      in.readjust(g2);
      break;
    
    case APP_HADEC:
      measMath.deapplyPolarMotion(in);
      break;

    case HADEC_AZEL:
      measMath.applyHADECtoAZEL(in);
      break;

    case AZEL_HADEC:
      measMath.deapplyHADECtoAZEL(in);
      break;

    case HADEC_APP:
      measMath.applyPolarMotion(in);
      break;

    case AZEL_AZELSW:
    case AZELSW_AZEL:
      measMath.applyAZELtoAZELSW(in);
      break;

    case ECLIP_J2000:
      measMath.applyECLIPtoJ2000(in);
      break;

    case J2000_ECLIP:
      measMath.deapplyECLIPtoJ2000(in);
      break;

    case MECLIP_JMEAN:
      measMath.applyMECLIPtoJMEAN(in);
      break;

    case JMEAN_MECLIP:
      measMath.deapplyMECLIPtoJMEAN(in);
      break;

    case TECLIP_JTRUE:
      measMath.applyTECLIPtoJTRUE(in);
      break;

    case JTRUE_TECLIP:
      measMath.deapplyTECLIPtoJTRUE(in);
      break;

    case GAL_SUPERGAL:
      measMath.applyGALtoSUPERGAL(in);
      break;

    case SUPERGAL_GAL:
      measMath.deapplyGALtoSUPERGAL(in);
      break;
    
    default:
      break;
      
    };	// switch
  };	// for
}

String MCuvw::showState() {
  if (!stateMade_p) {
    MCBase::makeState(MCuvw::stateMade_p, MCuvw::FromTo_p[0],
		      Muvw::N_Types, MCuvw::N_Routes,
		      MCuvw::ToRef_p);
  };
  return MCBase::showState(MCuvw::stateMade_p, MCuvw::FromTo_p[0],
			   Muvw::N_Types, MCuvw::N_Routes,
			   MCuvw::ToRef_p);
}
