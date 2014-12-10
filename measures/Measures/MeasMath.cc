//# MeasMath.cc:  Measure conversion aid routines
//# Copyright (C) 1998-2000,2002-2004,2007
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
#include <casacore/measures/Measures/MeasMath.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/System/AipsrcValue.h>
#include <casacore/measures/Measures/Aberration.h>
#include <casacore/measures/Measures/MeasData.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/measures/Measures/MRBase.h>
#include <casacore/measures/Measures/Nutation.h>
#include <casacore/measures/Measures/Precession.h>
#include <casacore/measures/Measures/SolarPos.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Static data
// Note: this static is not mutexed, because it does not harm if accidently
// two threads fill it at the same time.
uInt MeasMath::b1950_reg_p = 0;

//# Constructors
MeasMath::MeasMath() :
  inOK_p(False), outOK_p(False),
  inFrame_p(0), outFrame_p(0),
  SOLPOSIAU(0),
  ABERIAU(0), ABERB1950(0),
  NUTATIAU(0), NUTATB1950(0),
  PRECESIAU(0), PRECESB1950(0) {
  for (uInt i=0; i<N_FrameType; i++) {
    frameOK_p[i] = False;
    applyFrame_p[i] = 0;
    deapplyFrame_p[i] = 0;
  }
}

//# Destructor
MeasMath::~MeasMath() {
  delete SOLPOSIAU; 	SOLPOSIAU = 0;
  delete ABERIAU;   	ABERIAU = 0;
  delete ABERB1950;    	ABERB1950 = 0;
  delete NUTATIAU;   	NUTATIAU = 0;
  delete NUTATB1950; 	NUTATB1950 = 0;
  delete PRECESIAU;  	PRECESIAU = 0;
  delete PRECESB1950; 	PRECESB1950 = 0;
}

//# Operators

//# Member functions

void MeasMath::initFrame(MRBase &outref, MRBase &inref) {
  // Make sure frames are attached
  // Reset all calculations
  for (uInt i=0; i<N_FrameInfo; i++) infoOK_p[i] = False;
  // Get correct frame
  inOK_p = True;
  if (!inref.empty()) {
    inFrame_p = &(inref.getFrame());
  } else if (!outref.empty()) {
    inFrame_p = &(outref.getFrame());
  } else inOK_p = False;
  outOK_p = True;
  if (!outref.empty()) {
    outFrame_p = &(outref.getFrame());
  } else if (!inref.empty()) {
    outFrame_p = &(inref.getFrame());
  } else outOK_p = False;
}

void MeasMath::getFrame(FrameType i) {
  // Frame information group methods
  static FRFCT frameInfo[N_FrameType] = {
    &MeasFrame::epoch,
    &MeasFrame::position,
    &MeasFrame::direction,
    &MeasFrame::radialVelocity };
  
  // Get correct frame
  if (!frameOK_p[i]) {
    frameOK_p[i] = True;
    if (inOK_p && (inFrame_p->*frameInfo[i])()) {
      applyFrame_p[i] = inFrame_p;
    } else if (outOK_p && (outFrame_p->*frameInfo[i])()) {
      applyFrame_p[i] = outFrame_p;
    } else {
      frameOK_p[i] = False;
    }
    if (frameOK_p[i]) {
      if (outOK_p && (outFrame_p->*frameInfo[i])()) {
	deapplyFrame_p[i] = outFrame_p;
      } else {
	deapplyFrame_p[i] = inFrame_p;
      }
    }
  }
}

// Precession
void MeasMath::createPrecession() {
  if (!PRECESIAU) {
    if (MeasTable::useIAU2000()) {
      PRECESIAU = new Precession(Precession::IAU2000);
    } else {
      PRECESIAU = new Precession(Precession::IAU1976);
    }
  }
}

void MeasMath::applyPrecession(MVPosition &in) {
    if (MeasTable::useIAU2000()) {
      getInfo(TT);
      in *= MeasTable::frameBias00();
      in *= (*PRECESIAU)(info_p[TT]);
    } else {
      getInfo(TDB);
      in *= (*PRECESIAU)(info_p[TDB]);
    }
}

void MeasMath::deapplyPrecession(MVPosition &in) {
  if (MeasTable::useIAU2000()) {
    getInfo(TT);
    in = MeasTable::frameBias00() * in;
    in = (*PRECESIAU)(info_p[TT]) * in;
  } else {
    getInfo(TDB);
    in = (*PRECESIAU)(info_p[TDB]) * in;
  }
}

void MeasMath::createPrecessionB1950() {
  if (!PRECESB1950) PRECESB1950 = new Precession(Precession::B1950);
}

void MeasMath::applyPrecessionB1950(MVPosition &in) {
  getInfo(TDB);
  in *= (*PRECESB1950)(info_p[TDB]);
}

void MeasMath::deapplyPrecessionB1950(MVPosition &in) {
  getInfo(TDB);
  in = (*PRECESB1950)(info_p[TDB]) * in;
}

// Nutation
void MeasMath::createNutation() {
  if (!NUTATIAU) {
    if (MeasTable::useIAU2000()) {
      if (MeasTable::useIAU2000A()) {
	NUTATIAU = new Nutation(Nutation::IAU2000A);
      } else {
	NUTATIAU = new Nutation(Nutation::IAU2000B);
      }
    } else {
      NUTATIAU = new Nutation(Nutation::IAU1980);
    }
  }
}

void MeasMath::applyNutation(MVPosition &in) {
  if (MeasTable::useIAU2000()) {
    getInfo(TT);
    in *= (*NUTATIAU)(info_p[TT]);
  } else {
    getInfo(TDB);
    in *= (*NUTATIAU)(info_p[TDB]);
  }
}

void MeasMath::deapplyNutation(MVPosition &in) {
  if (MeasTable::useIAU2000()) {
    getInfo(TT);
    in = (*NUTATIAU)(info_p[TT]) * in;
  } else {
    getInfo(TDB);
    in = (*NUTATIAU)(info_p[TDB]) * in;
  }
}

void MeasMath::createNutationB1950() {
  if (!NUTATB1950) NUTATB1950 = new Nutation(Nutation::B1950);
}

void MeasMath::applyNutationB1950(MVPosition &in) {
  getInfo(TDB);
  in *= (*NUTATB1950)(info_p[TDB]);
}

void MeasMath::deapplyNutationB1950(MVPosition &in) {
  getInfo(TDB);
  in = (*NUTATB1950)(info_p[TDB]) * in;
}

// Precession and Nutation
void MeasMath::createPrecNutat() {
  createPrecession();
  createNutation();
}

void MeasMath::applyPrecNutat(MVPosition &in) {
  if (MeasTable::useIAU2000()) {
    getInfo(TT);
    in *= (RotMatrix((*PRECESIAU)(info_p[TT])) *
	   RotMatrix((*NUTATIAU)(info_p[TT])));
  } else {
    getInfo(TDB);
    in *= (RotMatrix((*PRECESIAU)(info_p[TDB])) *
	   RotMatrix((*NUTATIAU)(info_p[TDB])));
  }
}

void MeasMath::deapplyPrecNutat(MVPosition &in) {
  if (MeasTable::useIAU2000()) {
    getInfo(TT);
    in = (RotMatrix((*PRECESIAU)(info_p[TT])) *
	  RotMatrix((*NUTATIAU)(info_p[TT]))) * in;
  } else {
    getInfo(TDB);
    in = (RotMatrix((*PRECESIAU)(info_p[TDB])) *
	  RotMatrix((*NUTATIAU)(info_p[TDB]))) * in;
  }
}

void MeasMath::createPrecNutatB1950() {
  if (!PRECESB1950) PRECESB1950 = new Precession(Precession::B1950);
  if (!NUTATB1950) NUTATB1950 = new Nutation(Nutation::B1950);
}

void MeasMath::applyPrecNutatB1950(MVPosition &in, Bool doin) {
  getInfo(TDB);
  applyETerms(in, doin);
  in *= (RotMatrix((*PRECESB1950)(info_p[TDB])) *
	 RotMatrix((*NUTATB1950)(info_p[TDB])));
}

void MeasMath::deapplyPrecNutatB1950(MVPosition &in, Bool doin) {
  getInfo(TDB);
  in = (RotMatrix((*PRECESB1950)(info_p[TDB])) *
	RotMatrix((*NUTATB1950)(info_p[TDB]))) * in;
  deapplyETerms(in, doin);
}

// Aberration
void MeasMath::createAberration() {
  if (!ABERIAU) ABERIAU = new Aberration(Aberration::STANDARD);
}

void MeasMath::applyAberration(MVPosition &in, Bool doin) {
  getInfo(TDB);
  // Aberration
  MVPOS1 = (*ABERIAU)(info_p[TDB]);
  // Get length
  lengthE = MVPOS1.radius();
  // Beta^-1 (g1)
  g1 = sqrt(1 - lengthE * lengthE);
  if (doin) MVPOS4 = in;
  else {
    getInfo(J2000DIR);
    MVPOS4 = infomvd_p[J2000DIR-N_FrameDInfo];
  }
  g2 = MVPOS4 * MVPOS1;
  // Shift
  MVPOS2 = ((g1-1.0-g2)*MVPOS4 + (1+g2/(1+g1)) * MVPOS1)*(1.0/(1.0+g2));
  /// Really use JNAT
  rotateShift(in, MVPOS2, J2000LONG, J2000LAT, doin);
}

void MeasMath::deapplyAberration(MVPosition &in, Bool doin) {
  getInfo(TDB);
  // Aberration
  MVPOS1 = (*ABERIAU)(info_p[TDB]);
  // Get length
  lengthE = MVPOS1.radius();
  // Beta^-1 (g1)
  g1 = sqrt(1 - lengthE * lengthE);
  if (doin) MVPOS4 = in;
  else {
    getInfo(J2000DIR);
    MVPOS4 = infomvd_p[J2000DIR-N_FrameDInfo];
  }
  // First guess
  MVPOS2 = MVPOS4 - MVPOS1;
  // Solve for aberration solution
  do {
    g2 = MVPOS2 * MVPOS1;
    MVPOS3 = ((g1 * MVPOS2 + 
		(1+g2/(1+g1)) * MVPOS1)*(1.0/(1.0+g2)));
    MVPOS3.adjust();
    for (Int j=0; j<3; j++) {
      g3 = MVPOS1(j);
      MVPOS2(j) -= 
	(MVPOS3(j) - MVPOS4(j))/
	(((g1+g3*g3/(1+g1))-
	  g3 * MVPOS3(j))/(1+g2));
    }
    MVPOS3 -= MVPOS4;
  } while (MVPOS3.radius() > 1e-10);
  MVPOS2 -= MVPOS4;
  rotateShift(in, MVPOS2, J2000LONG, J2000LAT, doin);
}

void MeasMath::createAberrationB1950() {
  if (!ABERB1950) ABERB1950 = new Aberration(Aberration::B1950);
}

void MeasMath::applyAberrationB1950(MVPosition &in, Bool doin) {
  getInfo(TDB);
  // Aberration
  MVPOS1 = (*ABERB1950)(info_p[TDB]);
  /// Really should use precessed and nutated B1950
  rotateShift(in, MVPOS1, APPLONG, APPLAT, doin);
}

void MeasMath::deapplyAberrationB1950(MVPosition &in, Bool doin) {
  getInfo(TDB);
  // Aberration
  MVPOS1 = (*ABERB1950)(info_p[TDB]);
  /// Really should use B1950 apparent
  rotateShift(in, -MVPOS1, APPLONG, APPLAT, doin);
}

// Solar bending
void MeasMath::createSolarPos() {
  if (!SOLPOSIAU) SOLPOSIAU = new SolarPos(SolarPos::STANDARD);
}

void MeasMath::applySolarPos(MVPosition &in, Bool doin) {
  getInfo(TDB);
  // Solar position in rectangular coordinates
  MVPOS1 = (*SOLPOSIAU)(info_p[TDB]);
  // Get length and unit vector
  MVPOS1.adjust(lengthE);
  g1 = -1.974e-8 / lengthE;
  if (doin) MVPOS2 = in;
  else {
    getInfo(J2000DIR);
    MVPOS2 = infomvd_p[J2000DIR-N_FrameDInfo];
  } 
  g2 = MVPOS2 * MVPOS1;
  // Check if near sun
  if (!nearAbs(g2, 1.0,
	       1.0-cos(MeasData::SunSemiDiameter()/lengthE))) {
    MVPOS1 -= g2 * MVPOS2;
    MVPOS1 *= (g1 / (1.0 - g2));
    rotateShift(in, MVPOS1, J2000LONG, J2000LAT, doin);
  }
}

void MeasMath::deapplySolarPos(MVPosition &in, Bool doin) {
  getInfo(TDB);
  // Solar position in rectangular coordinates
  MVPOS1 = (*SOLPOSIAU)(info_p[TDB]);
  // Get length and unit vector
  MVPOS1.adjust(lengthE);
  g1 = -1.974e-8 / lengthE;
  if (doin) MVPOS4 = in;
  else {
    getInfo(J2000DIR);
    MVPOS4 = infomvd_p[J2000DIR-N_FrameDInfo];
  }
  g2 = MVPOS4 * MVPOS1;
  // Check if near sun
  if (!nearAbs(g2, 1.0,
	       1.0-cos(MeasData::SunSemiDiameter()/lengthE))) {
    // First guess
    MVPOS2 = MVPOS4;
    do {
      MVPOS3 = (MVPOS1 - g2 * MVPOS2) * (g1/(1.0 - g2));
      MVPOS3.adjust();
      for (Int j=0; j<3; j++) {
	g3 = MVPOS1(j);
	MVPOS2(j) -= 
	  (MVPOS3(j) + 
	   MVPOS2(j) - MVPOS4(j))/
	  (1 + (g3 * MVPOS3(j) -
		g1 * (g2 + g3 *
		      MVPOS2(j)))/(1-g2));
      }
      g2 = MVPOS2 * MVPOS1;
      MVPOS3 += MVPOS2;
      MVPOS3 -= MVPOS4;
    } while (MVPOS3.radius() > 1e-10);
    // Correction
    MVPOS2 -= MVPOS4;
    rotateShift(in, MVPOS2, J2000LONG, J2000LAT, doin);
  }
}

// Various conversions
void MeasMath::applyHADECtoITRF(MVPosition &in) {
  getInfo(LONG);
  in *= RotMatrix(Euler(info_p[LONG], 3u));
  in(1) = -in(1);
}

void MeasMath::deapplyHADECtoITRF(MVPosition &in) {
  getInfo(LONG);
  in(1) = -in(1);
  in = RotMatrix(Euler(info_p[LONG], 3u)) * in;
}

void MeasMath::applyHADECtoAZEL(MVPosition &in) {
  getInfo(LAT);
  in *= RotMatrix(Euler(C::pi_2 - info_p[LAT] , 2u, C::pi, 3u));
}

void MeasMath::deapplyHADECtoAZEL(MVPosition &in) {
  getInfo(LAT);
  in = RotMatrix(Euler(C::pi_2 - info_p[LAT] , 2u, C::pi, 3u)) * in;
}

void MeasMath::applyHADECtoAZELGEO(MVPosition &in) {
  getInfo(LATGEO);   
  in *= RotMatrix(Euler(C::pi_2 - info_p[LATGEO] , 2u, C::pi, 3u));
}

void MeasMath::deapplyHADECtoAZELGEO(MVPosition &in) {
  getInfo(LATGEO); 
  in = RotMatrix(Euler(C::pi_2 - info_p[LATGEO] , 2u, C::pi, 3u)) * in;
}

void MeasMath::applyJ2000toB1950(MVPosition &in, Bool doin) {
  if (!MeasMath::b1950_reg_p) {
    b1950_reg_p = 
      AipsrcValue<Double>::registerRC(String("measures.b1950.d_epoch"),
				      Unit("a"), Unit("a"), 2000.0);
  }
  Double epo;
  if (getInfo(UT1, True)) {
    epo = (info_p[UT1]-MeasData::MJD2000)/MeasData::JDCEN;
  } else epo = (AipsrcValue<Double>::get(MeasMath::b1950_reg_p)-2000.0)/100.0;
  applyJ2000toB1950(in, epo, doin);
}

void MeasMath::applyJ2000toB1950_VLA(MVPosition &in, Bool doin) {
  Double epo = 19.799-20.0;
  applyJ2000toB1950(in, epo, doin);
}

void MeasMath::applyJ2000toB1950(MVPosition &in, Double epo, Bool doin) {
  MVPosition VPOS3;
  VPOS3 = in;
  // Frame rotation
  in *= MeasData::MToB1950(4);
  in.adjust();
  // E-terms
  deapplyETerms(in, doin, epo);
  MVPosition VPOS4;
  do {
    VPOS4 = in;
    deapplyJ2000toB1950(VPOS4, epo, doin);
    VPOS4 -= VPOS3;
    in -= VPOS4*MeasData::MToB1950(4);
  } while (VPOS4.radius() > 1e-12);
}

void MeasMath::deapplyJ2000toB1950(MVPosition &in, Bool doin) {
  if (!MeasMath::b1950_reg_p) {
    b1950_reg_p = 
      AipsrcValue<Double>::registerRC(String("measures.b1950.d_epoch"),
				      Unit("a"), Unit("a"), 2000.0);
  }
  Double epo;
  if (getInfo(UT1, True)) {
    epo = (info_p[UT1]-MeasData::MJD2000)/MeasData::JDCEN;
  } else epo = (AipsrcValue<Double>::get(MeasMath::b1950_reg_p)-2000.0)/100.0;
  deapplyJ2000toB1950(in, epo, doin);
}

void MeasMath::deapplyJ2000toB1950_VLA(MVPosition &in, Bool doin) {
  Double epo = 19.799-20.0;
  deapplyJ2000toB1950(in, epo, doin);
}

void MeasMath::deapplyJ2000toB1950(MVPosition &in, Double epo, Bool doin) {
  applyETerms(in, doin, epo);
  // Frame rotation
  MVPOS1 = in*MeasData::MToJ2000(2);
  in *= MeasData::MToJ2000(0);
  in += (epo*C::arcsec)*MVPOS1;
  in.adjust();
}

void MeasMath::applyETerms(MVPosition &in, Bool doin, Double epo) {
  // E-terms
  MVPOS1 = MVPosition(MeasTable::AberETerm(0));
  epo += 0.5;
  MVPOS1 += (epo*C::arcsec)*MVPosition(MeasTable::AberETerm(1));
  if (doin) MVPOS2 = in;
  else {
    getInfo(B1950DIR);
    MVPOS2 = infomvd_p[B1950DIR-N_FrameDInfo];
  } 
  g1 = MVPOS2 * MVPOS1;
  MVPOS1 = g1 * MVPOS2 - MVPOS1;
  rotateShift(in, MVPOS1, B1950LONG, B1950LAT, doin);
}

void MeasMath::deapplyETerms(MVPosition &in, Bool doin, Double epo) {
  // E-terms
  // Iterate
  MVPOS1 = MVPosition(MeasTable::AberETerm(0));
  epo += 0.5;
  MVPOS1 += (epo*C::arcsec)*MVPosition(MeasTable::AberETerm(1));
  if (doin) MVPOS4 = in;
  else {
    getInfo(B1950DIR);
    MVPOS4 = infomvd_p[B1950DIR-N_FrameDInfo];
  }
  MVPOS2 = MVPOS4;
  do {
    g1 = MVPOS2 * MVPOS1;
    MVPOS3 = MVPOS2 - MVPOS1 + (g1 * MVPOS2);
    MVPOS3.adjust();
    MVPOS3 -= MVPOS4;
    MVPOS2 -= MVPOS3;
  } while (MVPOS3.radius() > 1e-5);
  MVPOS2 -= MVPOS4;
  rotateShift(in, MVPOS2, B1950LONG, B1950LAT, doin);
}

void MeasMath::applyGALtoJ2000(MVPosition &in) {
  in = MeasData::GALtoJ2000() * in;
}

void MeasMath::deapplyGALtoJ2000(MVPosition &in) {
  in = MeasData::J2000toGAL() * in;
}

void MeasMath::applyGALtoB1950(MVPosition &in) {
  in = MeasData::GALtoB1950() * in;
}

void MeasMath::deapplyGALtoB1950(MVPosition &in) {
  in = MeasData::B1950toGAL() * in;
}

void MeasMath::applyGALtoSUPERGAL(MVPosition &in) {
  in = MeasTable::galToSupergal() * in;
}

void MeasMath::deapplyGALtoSUPERGAL(MVPosition &in) {
  in *= MeasTable::galToSupergal();
}

void MeasMath::applyICRStoJ2000(MVPosition &in) {
  in = MeasTable::ICRSToJ2000() * in;
}

void MeasMath::deapplyICRStoJ2000(MVPosition &in) {
  in *= MeasTable::ICRSToJ2000();
}

void MeasMath::applyTOPOtoHADEC(MVPosition &in, Bool doin) {
  getInfo(LASTR);
  getInfo(TDB);
  getInfo(RADIUS);
  getInfo(LAT);
  g2 = MeasTable::diurnalAber(info_p[RADIUS], info_p[TDB]);
  MVPOS1 = MVDirection(info_p[LASTR], info_p[LAT]);
  MVPOS1.readjust(g2);
  /// Really should use topo for planets
  rotateShift(in, MVPOS1, APPLONG, APPLAT, doin);
  deapplyPolarMotion(in);
}

void MeasMath::deapplyTOPOtoHADEC(MVPosition &in, Bool doin) {
  getInfo(LASTR);
  getInfo(TDB);
  getInfo(RADIUS);
  getInfo(LAT);
  g2 = MeasTable::diurnalAber(info_p[RADIUS], info_p[TDB]);
  MVPOS1 = MVDirection(info_p[LASTR], info_p[LAT]);
  MVPOS1.readjust(g2);
  applyPolarMotion(in);  
  /// Really use topo for planets
  rotateShift(in, -MVPOS1, APPLONG, APPLAT, doin);
}

void MeasMath::applyPolarMotion(MVPosition &in) {
  getInfo(TDB);
  getInfo(LASTR);
  in(1) = -in(1);
  Euler EULER1 = MeasTable::polarMotion(info_p[TDB]);
  EULER1(2) = info_p[LASTR];
  in = RotMatrix(EULER1) * in;
}

void MeasMath::deapplyPolarMotion(MVPosition &in) {
  getInfo(TDB);
  getInfo(LASTR);
  Euler EULER1 = MeasTable::polarMotion(info_p[TDB]);
  EULER1(2) = info_p[LASTR];
  in *= RotMatrix(EULER1);
  in(1) = -in(1);
}

void MeasMath::applyAZELtoAZELSW(MVPosition &in) {
  in(0) = -in(0);
  in(1) = -in(1);
}

void MeasMath::applyECLIPtoJ2000(MVPosition &in) {
  in = RotMatrix(Euler(MeasTable::fundArg(0)(0.0), 1, 0, 0)) * in;
}

void MeasMath::deapplyECLIPtoJ2000(MVPosition &in) {
  in *= RotMatrix(Euler(MeasTable::fundArg(0)(0.0), 1, 0, 0));
}

void MeasMath::applyMECLIPtoJMEAN(MVPosition &in) {
  getInfo(TDB);
  in = RotMatrix(Euler(MeasTable::fundArg(0)((info_p[TDB] - 
					      MeasData::MJD2000)/
					     MeasData::JDCEN), 1, 0, 0)) * in;
}

void MeasMath::deapplyMECLIPtoJMEAN(MVPosition &in) {
  getInfo(TDB);
  in *= RotMatrix(Euler(MeasTable::fundArg(0)((info_p[TDB] - 
					       MeasData::MJD2000)/
					      MeasData::JDCEN), 1, 0, 0));
}

void MeasMath::applyTECLIPtoJTRUE(MVPosition &in) {
  getInfo(TDB);
  in = RotMatrix(Euler(-Nutation(Nutation::STANDARD)(info_p[TDB])(2),
		       1, 0, 0)) * in;
}

void MeasMath::deapplyTECLIPtoJTRUE(MVPosition &in) {
  getInfo(TDB);
  in *= RotMatrix(Euler(-Nutation(Nutation::STANDARD)(info_p[TDB])(2),
			1, 0, 0));
}

void MeasMath::applyAPPtoTOPO(MVPosition &in, const Double len,
			      Bool doin) {
  if (len != 0) {
    getInfo(LASTR);
    getInfo(LONG);
    getInfo(LAT);
    getInfo(RADIUS);
    ROTMAT1 = RotMatrix(Euler(info_p[LASTR] - info_p[LONG], 3u));
    // Correction
    MVPOS1 = (ROTMAT1 *
	      MVPosition(Quantity(info_p[RADIUS], "m"),
			 info_p[LONG], info_p[LAT])) * (1.0/len);
    rotateShift(in, -MVPOS1, APPLONG, APPLAT, doin);
  }
}

void MeasMath::deapplyAPPtoTOPO(MVPosition &in, const Double len,
				Bool doin) {
  if (len != 0) {
    getInfo(LASTR);
    getInfo(LONG);
    getInfo(LAT);
    getInfo(RADIUS);
    ROTMAT1 = RotMatrix(Euler(info_p[LASTR] - info_p[LONG], 3u));
    // Correction
    MVPOS1 = (ROTMAT1 *
	      MVPosition(Quantity(info_p[RADIUS], "m"),
			 info_p[LONG], info_p[LAT])) * (1.0/len);
    rotateShift(in, MVPOS1, APPLONG, APPLAT, doin);
  }
}

// General support
Bool MeasMath::getInfo(FrameInfo i, Bool ret) {
  // Frame information groups
  static FrameType InfoType[N_FrameInfo] = {
    EPOCH, EPOCH, EPOCH, EPOCH, POSITION, POSITION, POSITION, POSITION,
    DIRECTION, DIRECTION, DIRECTION, DIRECTION, DIRECTION,
    DIRECTION, DIRECTION, DIRECTION, DIRECTION };
  // Frame information methods
  static FRDINFO InfoDFrame[N_FrameDInfo] = {
    &MeasFrame::getTDB,
    &MeasFrame::getLASTr,
    &MeasFrame::getTT,
    &MeasFrame::getUT1,
    &MeasFrame::getLong,
    &MeasFrame::getLat,
    &MeasFrame::getRadius,
    &MeasFrame::getLatGeo,
    &MeasFrame::getJ2000Long,
    &MeasFrame::getJ2000Lat,
    &MeasFrame::getB1950Long,
    &MeasFrame::getB1950Lat,
    &MeasFrame::getAppLong,
    &MeasFrame::getAppLat };

  static FRMVDINFO InfoMVDFrame[N_FrameMVDInfo] = {
    &MeasFrame::getJ2000,
    &MeasFrame::getB1950,
    &MeasFrame::getApp };

  if (!infoOK_p[i]) {
    // Make sure there has not been an epoch added
    getFrame(InfoType[i]);
    if (frameOK_p[InfoType[i]]) {
      if (i < N_FrameDInfo) {
	(applyFrame_p[InfoType[i]]->*InfoDFrame[i])(info_p[i]);
      } else {
	(applyFrame_p[InfoType[i]]->*InfoMVDFrame[i-N_FrameDInfo])
	  (infomvd_p[i-N_FrameDInfo]);
      }
    } else {
      if (ret) return False;
      throw(AipsError(String("Missing information in Frame ") +
		      "specified for conversion"));
    }
    infoOK_p[i] = True;
  }
  return True;
}

void MeasMath::rotateShift(MVPosition &in, const MVPosition &shft,
			   const FrameInfo lng, const FrameInfo lat,
			   Bool doin) {
  if (doin) {
    in += shft;
    in.adjust();
  } else {
    getInfo(lat); getInfo(lng);
    // Rotation towards direction
    ROTMAT1 = RotMatrix(Euler(-C::pi_2 + info_p[lat], 2u,
			      -info_p[lng], 3u));
    // Rotation towards correction
    ROTMAT1 = RotMatrix(Euler(-(ROTMAT1*shft).getLong(), 3u)) * ROTMAT1;
    // Rotate over correction
    in = ((RotMatrix(Euler((ROTMAT1*shft).getValue()(0), 2u)) *
	   ROTMAT1) * in) * ROTMAT1;
  }
}

void MeasMath::getAPP(MVPosition &out) {
  getInfo(APPDIR);
  out = infomvd_p[APPDIR-N_FrameDInfo];
}

void MeasMath::getJ2000(MVPosition &out) {
  getInfo(J2000DIR);
  out = infomvd_p[J2000DIR-N_FrameDInfo];
}

void MeasMath::getB1950(MVPosition &out) {
  getInfo(B1950DIR);
  out = infomvd_p[B1950DIR-N_FrameDInfo];
}

} //# NAMESPACE CASACORE - END

