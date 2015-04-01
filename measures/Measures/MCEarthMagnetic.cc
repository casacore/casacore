//# MCEarthMagnetic.cc:  MEarthMagnetic conversion routines 
//# Copyright (C) 1998-2002,2004,2007
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
#include <casacore/casa/Exceptions.h>
#include <casacore/measures/Measures/MCEarthMagnetic.h>
#include <casacore/measures/Measures/EarthField.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Quanta/MVPosition.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Statics
uInt MCEarthMagnetic::ToRef_p[N_Routes][3] = {
  {MEarthMagnetic::GALACTIC,	 	MEarthMagnetic::J2000,		0},
  {MEarthMagnetic::GALACTIC,		MEarthMagnetic::B1950,		2},
  {MEarthMagnetic::J2000,		MEarthMagnetic::GALACTIC,	0},
  {MEarthMagnetic::B1950,		MEarthMagnetic::GALACTIC,	2},
  {MEarthMagnetic::J2000,		MEarthMagnetic::B1950,		2},
  {MEarthMagnetic::B1950,		MEarthMagnetic::J2000,		2},
  {MEarthMagnetic::J2000,		MEarthMagnetic::JMEAN,		0},
  {MEarthMagnetic::B1950,		MEarthMagnetic::BMEAN,		2},
  {MEarthMagnetic::JMEAN,		MEarthMagnetic::J2000,		0},
  {MEarthMagnetic::JMEAN,		MEarthMagnetic::JTRUE,		0},
  {MEarthMagnetic::BMEAN,		MEarthMagnetic::B1950,		2},
  {MEarthMagnetic::BMEAN,		MEarthMagnetic::BTRUE,		2},
  {MEarthMagnetic::JTRUE,		MEarthMagnetic::JMEAN,		0},
  {MEarthMagnetic::BTRUE,		MEarthMagnetic::BMEAN,		2},
  {MEarthMagnetic::J2000,		MEarthMagnetic::JNAT,		0},
  {MEarthMagnetic::JNAT,		MEarthMagnetic::J2000,		0},
  {MEarthMagnetic::B1950,		MEarthMagnetic::APP,	 	2},
  {MEarthMagnetic::APP,			MEarthMagnetic::B1950,		2},
  {MEarthMagnetic::APP,			MEarthMagnetic::TOPO,		0},
  {MEarthMagnetic::HADEC,		MEarthMagnetic::AZEL,		0},
  {MEarthMagnetic::HADEC,           	MEarthMagnetic::AZELGEO,        0},
  {MEarthMagnetic::AZEL,		MEarthMagnetic::HADEC,		0},
  {MEarthMagnetic::AZELGEO,         	MEarthMagnetic::HADEC,          0},
  {MEarthMagnetic::HADEC,		MEarthMagnetic::TOPO,		0},
  {MEarthMagnetic::AZEL,		MEarthMagnetic::AZELSW,		0},
  {MEarthMagnetic::AZELGEO,         	MEarthMagnetic::AZELSWGEO,      0},
  {MEarthMagnetic::AZELSW,		MEarthMagnetic::AZEL,		0},
  {MEarthMagnetic::AZELSWGEO,      	MEarthMagnetic::AZELGEO,        0},
  {MEarthMagnetic::APP,			MEarthMagnetic::JNAT,		0},
  {MEarthMagnetic::JNAT,		MEarthMagnetic::APP,		0},
  {MEarthMagnetic::J2000,		MEarthMagnetic::ECLIPTIC,	0},
  {MEarthMagnetic::ECLIPTIC,		MEarthMagnetic::J2000,		0},
  {MEarthMagnetic::JMEAN,		MEarthMagnetic::MECLIPTIC,	0},
  {MEarthMagnetic::MECLIPTIC,		MEarthMagnetic::JMEAN,		0},
  {MEarthMagnetic::JTRUE,		MEarthMagnetic::TECLIPTIC,	0},
  {MEarthMagnetic::TECLIPTIC,		MEarthMagnetic::JTRUE,		0},
  {MEarthMagnetic::GALACTIC,		MEarthMagnetic::SUPERGAL,	0},
  {MEarthMagnetic::SUPERGAL,		MEarthMagnetic::GALACTIC,	0},
  {MEarthMagnetic::ITRF,		MEarthMagnetic::HADEC,		0},
  {MEarthMagnetic::HADEC,		MEarthMagnetic::ITRF,		0},
  {MEarthMagnetic::TOPO,		MEarthMagnetic::HADEC,		0},
  {MEarthMagnetic::TOPO,		MEarthMagnetic::APP,		0},
  {MEarthMagnetic::ICRS,		MEarthMagnetic::J2000,		0},
  {MEarthMagnetic::J2000,		MEarthMagnetic::ICRS,		0} };
uInt MCEarthMagnetic::
FromTo_p[MEarthMagnetic::N_Types][MEarthMagnetic::N_Types];
MutexedInit MCEarthMagnetic::theirMutexedInit (MCEarthMagnetic::doFillState);

//# Constructors
MCEarthMagnetic::MCEarthMagnetic() :
  MVPOS1(0), EFIELD(0), measMath() {
    fillState();
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
    Bool iplan = (iin & MEarthMagnetic::EXTRA);
    Bool oplan = (iout & MEarthMagnetic::EXTRA);
    if (iplan) {
      mc.addMethod(MCEarthMagnetic::R_MODEL0);
      mc.addMethod((iin & ~MEarthMagnetic::EXTRA) + 
		   MCEarthMagnetic::R_IGRF);
      mc.addMethod(MCEarthMagnetic::R_MODEL);
      initConvert(MCEarthMagnetic::R_MODEL, mc);
      iin = MEarthMagnetic::ITRF;
    }
    if (oplan) iout = MEarthMagnetic::ITRF;
    Int tmp;
    while (iin != iout) {
      tmp = FromTo_p[iin][iout];
      iin = ToRef_p[tmp][1];
      mc.addMethod(tmp);
      initConvert(tmp, mc);
    }
  }
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

  case J2000_JMEAN:
    measMath.createPrecession();
    mc.addFrameType(MeasFrame::EPOCH);
    break;
    
  case B1950_BMEAN:
    measMath.createPrecessionB1950();
    mc.addFrameType(MeasFrame::EPOCH);
    break;
    
  case JMEAN_J2000:
    measMath.createPrecession();
    mc.addFrameType(MeasFrame::EPOCH);
    break;
    
  case JMEAN_JTRUE:
    measMath.createNutation();
    mc.addFrameType(MeasFrame::EPOCH);
    break;
    
  case BMEAN_B1950:
    measMath.createPrecessionB1950();
    mc.addFrameType(MeasFrame::EPOCH);
    break;
    
  case BMEAN_BTRUE:
    measMath.createNutationB1950();
    mc.addFrameType(MeasFrame::EPOCH);
    break;
    
  case JTRUE_JMEAN:
    measMath.createNutation();
    mc.addFrameType(MeasFrame::EPOCH);
    break;
    
  case BTRUE_BMEAN:
    measMath.createNutationB1950();
    mc.addFrameType(MeasFrame::EPOCH);
    break;
    
  case J2000_JNAT:
    measMath.createSolarPos();
    mc.addFrameType(MeasFrame::EPOCH);
    break;
    
  case JNAT_APP:
    measMath.createAberration();
    measMath.createPrecNutat();
    mc.addFrameType(MeasFrame::EPOCH);
    break;
    
  case JNAT_J2000:
    measMath.createSolarPos();
    mc.addFrameType(MeasFrame::EPOCH);
    break;
    
  case APP_JNAT:
    measMath.createAberration();
    measMath.createPrecNutat();
    mc.addFrameType(MeasFrame::EPOCH);
    break;
    
  case B1950_APP:
    measMath.createAberrationB1950();
    measMath.createPrecNutatB1950();
    mc.addFrameType(MeasFrame::EPOCH);
    break;
    
  case APP_B1950:
    measMath.createAberrationB1950();
    measMath.createPrecNutatB1950();
    mc.addFrameType(MeasFrame::EPOCH);
    break;
    
  case HADEC_ITRF:
  case ITRF_HADEC: 
  case HADEC_AZEL:
  case AZEL_HADEC:
  case HADEC_AZELGEO:
  case AZELGEO_HADEC:
  case MECLIP_JMEAN:
  case JMEAN_MECLIP:
  case TECLIP_JTRUE:
  case JTRUE_TECLIP:
    mc.addFrameType(MeasFrame::POSITION);
    break;

  case TOPO_HADEC: 
  case HADEC_TOPO: 
  case APP_TOPO: 
  case TOPO_APP: 
    mc.addFrameType(MeasFrame::EPOCH);
    mc.addFrameType(MeasFrame::POSITION);
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
  // Planetary aberration factor
  Double lengthP = 0;
  EarthField::EarthFieldTypes modID(EarthField::IGRF);

  measMath.initFrame(inref, outref);
  
  for (Int i=0; i<mc.nMethod(); i++) {
    
    switch (mc.getMethod(i)) {

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
    
    
    case J2000_JNAT:
      in.adjust(g2);
      measMath.applySolarPos(in);
      in.readjust(g2);
      break;
    
    case JNAT_APP:
      in.adjust(g2);
      measMath.applyAberration(in);
      in.readjust(g2);
      measMath.applyPrecNutat(in);
      break;
    
    case APP_JNAT:
      measMath.deapplyPrecNutat(in);
      in.adjust(g2);
      measMath.deapplyAberration(in);
      in.readjust(g2);
      break;
    
    case JNAT_J2000:
      in.adjust(g2);
      measMath.deapplySolarPos(in);
      in.readjust(g2);
      break;
    
    case B1950_APP: 
      measMath.applyPrecNutatB1950(in);
      in.adjust(g2);
      measMath.applyAberrationB1950(in);
      in.readjust(g2);
      break;
    
    case APP_B1950:
      in.adjust(g2);
      measMath.deapplyAberrationB1950(in);
      in.readjust(g2);
      measMath.deapplyPrecNutatB1950(in);
      break;
    
    case TOPO_HADEC: 
      in.adjust(g2);
      measMath.applyTOPOtoHADEC(in);
      in.readjust(g2);
      break;
    
    case HADEC_AZEL:
      measMath.applyHADECtoAZEL(in);
      break;

    case HADEC_AZELGEO: 
      measMath.applyHADECtoAZELGEO(in);
      break;
    
    case AZEL_HADEC:
      measMath.deapplyHADECtoAZEL(in);
      break;

    case AZELGEO_HADEC:
      measMath.deapplyHADECtoAZELGEO(in);
      break;
     
    case HADEC_TOPO: 
      in.adjust(g2);
      measMath.deapplyTOPOtoHADEC(in);
      in.readjust(g2);
      break;
    
    case APP_TOPO: 
      in.adjust(g2);
      measMath.applyAPPtoTOPO(in, lengthP);
      in.readjust(g2);
      break;
   
    case TOPO_APP: 
      in.adjust(g2);
      measMath.deapplyAPPtoTOPO(in, lengthP);
      in.readjust(g2);
      break;
    
    case AZEL_AZELSW: 
    case AZELSW_AZEL:
    case AZELGEO_AZELSWGEO:
    case AZELSWGEO_AZELGEO:
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
 
    case ICRS_J2000:
      measMath.applyICRStoJ2000(in);
      break;
      
    case J2000_ICRS:
      measMath.deapplyICRStoJ2000(in);
      break;
    
    case R_MODEL0:
      break;
    
    case R_MODEL:
      if (!EFIELD) {
	MEarthMagnetic::Ref::frameEpoch(inref, outref).
	  getTDB(tdbTime);
	EFIELD = new EarthField(modID, tdbTime);
      }
      MEarthMagnetic::Ref::framePosition(outref, inref).
	getITRF(*MVPOS1);
      in = EFIELD->operator()(*MVPOS1);
      break;

    case R_IGRF:
      modID = EarthField::IGRF;
      break;

    default:
      break;
      
    }	// switch
  }	// for
}

String MCEarthMagnetic::showState() {
  fillState();
  return MCBase::showState(MCEarthMagnetic::FromTo_p[0],
			   MEarthMagnetic::N_Types, MCEarthMagnetic::N_Routes,
			   MCEarthMagnetic::ToRef_p);
}

void MCEarthMagnetic::doFillState (void*) {
  MEarthMagnetic::checkMyTypes();
  MCBase::makeState(FromTo_p[0], MEarthMagnetic::N_Types, N_Routes, ToRef_p);
}

} //# NAMESPACE CASACORE - END

