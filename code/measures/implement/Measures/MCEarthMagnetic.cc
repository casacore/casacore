//# MCEarthMagnetic.cc:  MEarthMagnetic conversion routines 
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
#include <aips/Measures/MCEarthMagnetic.h>
#include <aips/Measures/EarthField.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Utilities/Assert.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Measures/MCFrame.h>
#include <aips/Quanta/MVPosition.h>

//# Statics
Bool MCEarthMagnetic::stateMade_p = False;
uInt MCEarthMagnetic::ToRef_p[N_Routes][3] = {
    {MEarthMagnetic::ITRF,	MEarthMagnetic::JNAT,		0},
    {MEarthMagnetic::JNAT,	MEarthMagnetic::ITRF,		0},
    {MEarthMagnetic::GALACTIC,	MEarthMagnetic::J2000,		0},
    {MEarthMagnetic::GALACTIC,	MEarthMagnetic::B1950,		3},
    {MEarthMagnetic::J2000,	MEarthMagnetic::GALACTIC,	0},
    {MEarthMagnetic::B1950,	MEarthMagnetic::GALACTIC,	3},
    {MEarthMagnetic::J2000,	MEarthMagnetic::B1950,		3},
    {MEarthMagnetic::B1950,	MEarthMagnetic::J2000,		3},
    {MEarthMagnetic::J2000,	MEarthMagnetic::JMEAN,		0},
    {MEarthMagnetic::B1950,	MEarthMagnetic::BMEAN,		3},
    {MEarthMagnetic::JMEAN,	MEarthMagnetic::J2000,		0},
    {MEarthMagnetic::JMEAN,	MEarthMagnetic::JTRUE,		0},
    {MEarthMagnetic::BMEAN,	MEarthMagnetic::B1950,		3},
    {MEarthMagnetic::BMEAN,	MEarthMagnetic::BTRUE,		3},
    {MEarthMagnetic::JTRUE,	MEarthMagnetic::JMEAN,		0},
    {MEarthMagnetic::BTRUE,	MEarthMagnetic::BMEAN,		3},
    {MEarthMagnetic::J2000,	MEarthMagnetic::JNAT,		0},
    {MEarthMagnetic::JNAT,	MEarthMagnetic::J2000,		0},
    {MEarthMagnetic::B1950,	MEarthMagnetic::APP,		3},
    {MEarthMagnetic::APP,	MEarthMagnetic::B1950,		3},
    {MEarthMagnetic::APP,	MEarthMagnetic::HADEC,		2},
    {MEarthMagnetic::HADEC,	MEarthMagnetic::AZEL,		2},
    {MEarthMagnetic::AZEL,	MEarthMagnetic::HADEC,		2},
    {MEarthMagnetic::HADEC,	MEarthMagnetic::APP,		2},
    {MEarthMagnetic::AZEL,	MEarthMagnetic::AZELSW,		0},
    {MEarthMagnetic::AZELSW,	MEarthMagnetic::AZEL,		0},
    {MEarthMagnetic::APP,	MEarthMagnetic::JNAT,		1},
    {MEarthMagnetic::JNAT,	MEarthMagnetic::APP,		1},
    {MEarthMagnetic::J2000,	MEarthMagnetic::ECLIPTIC,	0},
    {MEarthMagnetic::ECLIPTIC,	MEarthMagnetic::J2000,		0},
    {MEarthMagnetic::JMEAN,	MEarthMagnetic::MECLIPTIC,	0},
    {MEarthMagnetic::MECLIPTIC,	MEarthMagnetic::JMEAN,		0},
    {MEarthMagnetic::JTRUE,	MEarthMagnetic::TECLIPTIC,	0},
    {MEarthMagnetic::TECLIPTIC,	MEarthMagnetic::JTRUE,		0},
    {MEarthMagnetic::GALACTIC,	MEarthMagnetic::SUPERGAL,	0},
    {MEarthMagnetic::SUPERGAL,	MEarthMagnetic::GALACTIC,	0},
    {MEarthMagnetic::ITRF,	MEarthMagnetic::HADEC,		2},
    {MEarthMagnetic::HADEC,	MEarthMagnetic::ITRF,		2} };
uInt MCEarthMagnetic::
FromTo_p[MEarthMagnetic::N_Types][MEarthMagnetic::N_Types];

//# Constructors
MCEarthMagnetic::MCEarthMagnetic() :
  MVPOS1(0), EFIELD(0), measMath() {
  if (!stateMade_p) {
    MCBase::makeState(MCEarthMagnetic::stateMade_p,
		      MCEarthMagnetic::FromTo_p[0],
		      MEarthMagnetic::N_Types, MCEarthMagnetic::N_Routes,
		      MCEarthMagnetic::ToRef_p);
  };
}

//# Destructor
MCEarthMagnetic::~MCEarthMagnetic() {
  clearConvert();
}

//# Operators

//# Member functions

void MCEarthMagnetic::getConvert(MConvertBase &mc,
			     const MRBase &inref, 
			     const MRBase &outref) {
    
  Int iin  = inref.getType();
  Int iout = outref.getType();
  if (iin != iout) {
    Bool iplan = ToBool(iin & MEarthMagnetic::EXTRA);
    Bool oplan = ToBool(iout & MEarthMagnetic::EXTRA);
    if (iplan) {
      mc.addMethod(MCEarthMagnetic::R_MODEL0);
      mc.addMethod((iin & ~MEarthMagnetic::EXTRA) + 
		   MCEarthMagnetic::R_IGRF);
      mc.addMethod(MCEarthMagnetic::R_MODEL);
      initConvert(MCEarthMagnetic::R_MODEL, mc);
      iin = MEarthMagnetic::ITRF;
    };
    if (oplan) iout = MEarthMagnetic::ITRF;
    Int tmp;
    while (iin != iout) {
      tmp = FromTo_p[iin][iout];
      iin = ToRef_p[tmp][1];
      mc.addMethod(tmp);
      initConvert(tmp, mc);
    };
  };
}

void MCEarthMagnetic::clearConvert() {
  delete MVPOS1;     MVPOS1 = 0;
  delete EFIELD;     EFIELD=0;
}

//# Conversion routines
void MCEarthMagnetic::initConvert(uInt which, MConvertBase &mc) {

  if (False) initConvert(which, mc);	// Stop warning
  if (!MVPOS1)  MVPOS1 = new MVPosition();
  
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
    
  }
}

void MCEarthMagnetic::doConvert(MeasValue &in,
				MRBase &inref,
				MRBase &outref,
				const MConvertBase &mc) {
  doConvert((MVEarthMagnetic &) in,
	    inref, outref, mc);
}

void MCEarthMagnetic::doConvert(MVEarthMagnetic &in,
				MRBase &inref,
				MRBase &outref,
				const MConvertBase &mc) {
  Double g2, tdbTime;
  EarthField::EarthFieldTypes modID(EarthField::IGRF);

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
    
    case R_MODEL0:
      break;
    
    case R_MODEL:
      if (!EFIELD) {
	((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(inref, outref).
		     getMCFramePoint()))->
	  getTDB(tdbTime);
	EFIELD = new EarthField(modID, tdbTime);
      };
      ((MCFrame *)(MEarthMagnetic::Ref::framePosition(outref, inref).
		   getMCFramePoint()))->
	getITRF(*MVPOS1);
      in = EFIELD->operator()(*MVPOS1);
      break;

    case R_IGRF:
      modID = EarthField::IGRF;
      break;

    default:
      break;
      
    };	// switch
  };	// for
}

String MCEarthMagnetic::showState() {
  if (!stateMade_p) {
    MCBase::makeState(MCEarthMagnetic::stateMade_p,
		      MCEarthMagnetic::FromTo_p[0],
		      MEarthMagnetic::N_Types, MCEarthMagnetic::N_Routes,
		      MCEarthMagnetic::ToRef_p);
  };
  return MCBase::showState(MCEarthMagnetic::stateMade_p,
			   MCEarthMagnetic::FromTo_p[0],
			   MEarthMagnetic::N_Types, MCEarthMagnetic::N_Routes,
			   MCEarthMagnetic::ToRef_p);
}
