//# MCDirection.cc:  MDirection conversion routines 
//# Copyright (C) 1995-1998,2000-2002,2004,2007
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
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/measures/Measures/MCDirection.h>
#include <casacore/measures/Measures/MeasFrame.h>
#include <casacore/casa/Quanta/MVPosition.h>
#include <casacore/measures/Measures/Nutation.h>
#include <casacore/measures/Measures/MeasTable.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Statics
uInt MCDirection::ToRef_p[N_Routes][3] = {
  {MDirection::GALACTIC,	MDirection::J2000,	0},
  {MDirection::GALACTIC,	MDirection::B1950,	2},
  {MDirection::J2000,		MDirection::GALACTIC,	0},
  {MDirection::B1950,		MDirection::GALACTIC,	2},
  {MDirection::J2000,		MDirection::B1950,	2},
  {MDirection::J2000,		MDirection::B1950_VLA,	2},
  {MDirection::B1950,		MDirection::J2000,	2},
  {MDirection::B1950_VLA,	MDirection::J2000,	2},
  {MDirection::B1950,		MDirection::B1950_VLA,	0},
  {MDirection::B1950_VLA,	MDirection::B1950,	0},
  {MDirection::J2000,		MDirection::JMEAN,	0},
  {MDirection::B1950,		MDirection::BMEAN,	2},
  {MDirection::JMEAN,		MDirection::J2000,	0},
  {MDirection::JMEAN,		MDirection::JTRUE,	0},
  {MDirection::BMEAN,		MDirection::B1950,	2},
  {MDirection::BMEAN,		MDirection::BTRUE,	2},
  {MDirection::JTRUE,		MDirection::JMEAN,	0},
  {MDirection::BTRUE,		MDirection::BMEAN,	2},
  {MDirection::J2000,		MDirection::JNAT,	0},
  {MDirection::JNAT,		MDirection::J2000,	0},
  {MDirection::B1950,		MDirection::APP,	2},
  {MDirection::APP,		MDirection::B1950,	2},
  {MDirection::APP,		MDirection::TOPO,	0},
  {MDirection::HADEC,		MDirection::AZEL,	0},
  {MDirection::HADEC,           MDirection::AZELGEO,    0},
  {MDirection::AZEL,		MDirection::HADEC,	0},
  {MDirection::AZELGEO,         MDirection::HADEC,      0},
  {MDirection::HADEC,		MDirection::TOPO,	0},
  {MDirection::AZEL,		MDirection::AZELSW,	0},
  {MDirection::AZELGEO,         MDirection::AZELSWGEO,  0},
  {MDirection::AZELSW,		MDirection::AZEL,	0},
  {MDirection::AZELSWGEO,       MDirection::AZELGEO,    0},
  {MDirection::APP,		MDirection::JNAT,	0},
  {MDirection::JNAT,		MDirection::APP,	0},
  {MDirection::J2000,		MDirection::ECLIPTIC,	0},
  {MDirection::ECLIPTIC,	MDirection::J2000,	0},
  {MDirection::JMEAN,		MDirection::MECLIPTIC,	0},
  {MDirection::MECLIPTIC,	MDirection::JMEAN,	0},
  {MDirection::JTRUE,		MDirection::TECLIPTIC,	0},
  {MDirection::TECLIPTIC,	MDirection::JTRUE,	0},
  {MDirection::GALACTIC,	MDirection::SUPERGAL,	0},
  {MDirection::SUPERGAL,	MDirection::GALACTIC,	0},
  {MDirection::ITRF,		MDirection::HADEC,	0},
  {MDirection::HADEC,		MDirection::ITRF,	0},
  {MDirection::TOPO,		MDirection::HADEC,	0},
  {MDirection::TOPO,		MDirection::APP,	0},
  {MDirection::ICRS,		MDirection::J2000,	0},
  {MDirection::J2000,		MDirection::ICRS,	0} };
uInt MCDirection::FromTo_p[MDirection::N_Types][MDirection::N_Types];
MutexedInit MCDirection::theirMutexedInit (MCDirection::doFillState);

//# Constructors
MCDirection::MCDirection() :
  MVPOS1(0), MVPOS2(0), MVPOS3(0),
  VEC61(0), VEC62(0), VEC63(0), measMath() {
    fillState();
}

//# Destructor
MCDirection::~MCDirection() {
  clearConvert();
}

//# Operators

//# Member functions

void MCDirection::getConvert(MConvertBase &mc,
			     const MRBase &inref, 
			     const MRBase &outref) {

    uInt iin  = inref.getType();
    uInt iout = outref.getType();
    if (iin != iout) {
      Bool iplan = (iin & MDirection::EXTRA);
      Bool oplan = (iout & MDirection::EXTRA);
      if (iplan) {
	if (iin != MDirection::COMET) {
	  mc.addMethod(MCDirection::R_PLANET0);
	  mc.addFrameType(MeasFrame::EPOCH);
	  mc.addMethod((iin & ~MDirection::EXTRA) + MCDirection::R_MERCURY);
	  mc.addMethod(MCDirection::R_PLANET);
	  initConvert(MCDirection::R_PLANET, mc);
	  iin = MDirection::JNAT;
	} else {
	  mc.addMethod(MCDirection::R_COMET0);
	  mc.addFrameType(MeasFrame::EPOCH);
	  mc.addFrameType(MeasFrame::COMET);
	  mc.addFrameType(MeasFrame::POSITION);
	  mc.addMethod(MCDirection::R_COMET);
	  initConvert(MCDirection::R_COMET, mc);
	  iin = MDirection::APP;
	}
      }
      if (oplan) iout = MDirection::J2000;
      Int tmp;
      while (iin != iout) {
	tmp = FromTo_p[iin][iout];
	iin = ToRef_p[tmp][1];
	mc.addMethod(tmp);
	initConvert(tmp, mc);
      }
    }
}

void MCDirection::clearConvert() {
  delete MVPOS1;     MVPOS1 = 0;
  delete MVPOS2;     MVPOS2 = 0;
  delete MVPOS3;     MVPOS3 = 0;
  delete VEC61;	     VEC61 = 0;
  delete VEC62;	     VEC62 = 0;
  delete VEC63;	     VEC63 = 0;
}

//# Conversion routines
void MCDirection::initConvert(uInt which, MConvertBase &mc) {

  if (False) initConvert(which, mc);	// Stop warning
  if (!MVPOS1)  MVPOS1 = new MVPosition();
  if (!MVPOS2)  MVPOS2 = new MVPosition();
  if (!MVPOS3)  MVPOS3 = new MVPosition();
  if (!VEC61)   VEC61 = new Vector<Double>(6);
  if (!VEC62)   VEC62 = new Vector<Double>(6);
  if (!VEC63)   VEC63 = new Vector<Double>(6);
  
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
    mc.addFrameType(MeasFrame::EPOCH);
    measMath.createPrecession();
    break;
    
  case JMEAN_JTRUE:
    mc.addFrameType(MeasFrame::EPOCH);
    measMath.createNutation();
    break;
    
  case BMEAN_B1950:
    mc.addFrameType(MeasFrame::EPOCH);
    measMath.createPrecessionB1950();
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
  case HADEC_AZELGEO:
  case AZEL_HADEC:
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

void MCDirection::doConvert(MeasValue &in,
			    MRBase &inref,
			    MRBase &outref,
			    const MConvertBase &mc) {
  doConvert(*(MVDirection*)&in,
	    inref, outref, mc);
}

void MCDirection::doConvert(MVDirection &in,
			    MRBase &inref,
			    MRBase &outref,
			    const MConvertBase &mc) {
  Double g1, g2, g3, lengthE, tdbTime;
  // Planetary aberration factor
  Double lengthP = 0;
  MeasTable::Types planID = MeasTable::MERCURY; // to stop warning
  uInt comID = static_cast<uInt>(MDirection::APP);
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
      measMath.applyJ2000toB1950(in);
      break;
      
    case J2000_B1950_VLA:
      measMath.applyJ2000toB1950_VLA(in);
      break;
    
    case B1950_J2000:
      measMath.deapplyJ2000toB1950(in);
      break;
    
    case B1950_VLA_J2000:
      measMath.deapplyJ2000toB1950_VLA(in);
      break;

    case B1950_B1950_VLA:
      measMath.deapplyJ2000toB1950(in);
      measMath.applyJ2000toB1950_VLA(in);
      break;

    case B1950_VLA_B1950:
      measMath.deapplyJ2000toB1950_VLA(in);
      measMath.applyJ2000toB1950(in);
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
      measMath.applySolarPos(in);
      break;
    
    case JNAT_APP:
      measMath.applyAberration(in);
      measMath.applyPrecNutat(in);
      break;
    
    case APP_JNAT:
      measMath.deapplyPrecNutat(in);
      measMath.deapplyAberration(in);
      break;
    
    case JNAT_J2000:
      measMath.deapplySolarPos(in);
      break;
    
    case B1950_APP:
      measMath.applyPrecNutatB1950(in);
      measMath.applyAberrationB1950(in);
      break;
    
    case APP_B1950:
      measMath.deapplyAberrationB1950(in);
      measMath.deapplyPrecNutatB1950(in);
      break;
    
    case TOPO_HADEC: 
      measMath.applyTOPOtoHADEC(in);
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
      measMath.deapplyTOPOtoHADEC(in);
    break;
    
    case APP_TOPO: 
      measMath.applyAPPtoTOPO(in, lengthP);
      break;
    
    case R_COMET:
      if (comID == MDirection::APP) break;
    case TOPO_APP: 
      measMath.deapplyAPPtoTOPO(in, lengthP);
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
   
    case R_PLANET0: {
      MDirection::Ref::frameEpoch(outref, inref).
	getTDB(tdbTime);
      *VEC62 = MeasTable::Planetary(MeasTable::EARTH, tdbTime); // Eb
      *VEC63 = MeasTable::Planetary(MeasTable::SUN, tdbTime);   // Sb
      for (Int j=0; j<3; j++)
	(*MVPOS3)(j) = (*VEC62)(j) - (*VEC63)(j);		// E
    }
    break;
    
    case R_PLANET: {
      // Correct for light time
      MVPOS3->adjust(g2);
      lengthE = 0;
      do {
	g3 = lengthE;
	*VEC61 = MeasTable::Planetary(planID, tdbTime - g3);
	*VEC63 = MeasTable::Planetary(MeasTable::SUN, tdbTime - g3); // Sb
	for (Int j=0; j<3; j++) {
	  (*MVPOS1)(j) = (*VEC61)(j) - (*VEC62)(j);		// P
	  (*MVPOS2)(j) = (*VEC61)(j) - (*VEC63)(j);		// Q
	}
	MVPOS1->adjust(lengthE);
	lengthP = Quantity(lengthE, "AU").getBaseValue();
	MVPOS2->adjust(g1);
	if (planID != MeasTable::SUN)
	  lengthE += 2*MeasTable::Planetary(MeasTable::GMS) *
	  log((g2+lengthE+g1)/(g2-lengthE+g1));
	lengthE /= MeasTable::Planetary(MeasTable::CAU);
      } while (abs(g3-lengthE) > 1e-9*lengthE);
      in = *MVPOS1; in.adjust();
      // Correct for light deflection
      // Check if near sun
      if (planID != MeasTable::SUN &&
	  (g3 = in * *MVPOS3, 
	   !nearAbs(g3, 1.0,
		    1.0-cos(MeasTable::Planetary(MeasTable::RADS)/g2)))) {
	g1 = 2*MeasTable::Planetary(MeasTable::GMS) / g2;
	g1 /= (1.0 + (*MVPOS2)*(*MVPOS3));
	in += g1 * ((in*(*MVPOS2))* *MVPOS3 - ((*MVPOS3)*in)* *MVPOS2);
      }
      in.adjust();
    }
    break;
    
    case R_MERCURY:
      planID = MeasTable::MERCURY;
      break;
    
    case R_VENUS:
      planID = MeasTable::VENUS;
      break;
    
    case R_MARS:
      planID = MeasTable::MARS;
      break;
    
    case R_JUPITER:
      planID = MeasTable::JUPITER;
      break;
    
    case R_SATURN:
      planID = MeasTable::SATURN;
      break;
    
    case R_URANUS:
      planID = MeasTable::URANUS;
      break;
    
    case R_NEPTUNE:
      planID = MeasTable::NEPTUNE;
      break;
    
    case R_PLUTO:
      planID = MeasTable::PLUTO;
      break;
    
    case R_SUN:
      planID = MeasTable::SUN;
      break;
    
    case R_MOON:
      planID = MeasTable::MOON;
      break;
    
    case R_COMET0: {
      MDirection::Ref::frameComet(inref, outref).
	getCometType(comID);
      if (!MDirection::Ref::frameComet(inref, outref).
	  getComet(*MVPOS1)) {
	throw(AipsError("No or outside range comet table specified"));
      }
      MVPOS1->adjust(lengthP);
      in = *MVPOS1;
    }
    break;

    default:
      break;
      
    }	// switch
  }	// for
}

String MCDirection::showState() {
  fillState();
  return MCBase::showState(MCDirection::FromTo_p[0],
			   MDirection::N_Types, MCDirection::N_Routes,
			   MCDirection::ToRef_p);
}

void MCDirection::doFillState (void*) {
  MDirection::checkMyTypes();
  MCBase::makeState(FromTo_p[0],  MDirection::N_Types, N_Routes, ToRef_p);
}

} //# NAMESPACE CASACORE - END

