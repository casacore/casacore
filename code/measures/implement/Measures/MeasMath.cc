//# MeasMath.cc:  Measure conversion aid routines
//# Copyright (C) 1998
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
#include <aips/Measures/MeasMath.h>
#include <aips/Exceptions/Error.h>
#include <aips/Measures/MeasData.h>
#include <aips/Measures/MeasTable.h>
#include <aips/Measures/SolarPos.h>
#include <aips/Measures/Aberration.h>
#include <aips/Measures/Precession.h>
#include <aips/Measures/Nutation.h>
#include <aips/Measures/MCFrame.h>
#include <aips/Measures/MRBase.h>

//# Constructors
MeasMath::MeasMath() :
  SOLPOSIAU(0),
  ABERIAU(0), ABERB1950(0),
  NUTATIAU(0), NUTATB1950(0),
  PRECESIAU(0), PRECESB1950(0) {
  inOK_p = False;
  outOK_p = False;
  inFrame_p = MeasFrame();
  outFrame_p = MeasFrame();
  for (uInt i=0; i<N_FrameType; i++) {
    frameOK_p[i] = False;
    applyFrame_p[i] = MeasFrame();
    deapplyFrame_p[i] = MeasFrame();
  };
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
  MCFrame::make(inref.getFrame());
  MCFrame::make(outref.getFrame());
  // Reset all calculations
  for (uInt i=0; i<N_FrameInfo; i++) infoOK_p[i] = False;
  // Get correct frame
  inOK_p = True;
  if (!inref.empty()) inFrame_p = inref.getFrame();
  else if (!outref.empty()) inFrame_p = outref.getFrame();
  else inOK_p = False;
  outOK_p = True;
  if (!outref.empty()) outFrame_p = outref.getFrame();
  else if (!inref.empty()) outFrame_p = inref.getFrame();
  else outOK_p = False;
}

void MeasMath::getFrame(FrameType i) {
  // Frame information group methods
  static FRFCT frameInfo[N_FrameType] = {
    MeasFrame::epoch,
    MeasFrame::position,
    MeasFrame::direction,
    MeasFrame::radialVelocity };
  
  // Get correct frame
  if (!frameOK_p[i]) {
    frameOK_p[i] = True;
    if (inOK_p && (inFrame_p.*frameInfo[i])()) {
      applyFrame_p[i] = inFrame_p;
    } else if (outOK_p && (outFrame_p.*frameInfo[i])()) {
      applyFrame_p[i] = outFrame_p;
    } else {
      frameOK_p[i] = False;
    };
    if (frameOK_p[i]) {
      if (outOK_p && (outFrame_p.*frameInfo[i])()) {
	deapplyFrame_p[i] = outFrame_p;
      } else {
	deapplyFrame_p[i] = inFrame_p;
      };
    };
  };
}

// Precession
void MeasMath::createPrecession() {
  if (!PRECESIAU) PRECESIAU = new Precession(Precession::STANDARD);
}

void MeasMath::applyPrecession(MVPosition &in) {
  getInfo(TDB);
  ROTMAT1 = (*PRECESIAU)(info_p[TDB]);
  in *= ROTMAT1;
}

void MeasMath::deapplyPrecession(MVPosition &in) {
  getInfo(TDB);
  ROTMAT1 = (*PRECESIAU)(info_p[TDB]);
  in = ROTMAT1 * in;
}

void MeasMath::createPrecessionB1950() {
  if (!PRECESB1950) PRECESB1950 = new Precession(Precession::B1950);
}

void MeasMath::applyPrecessionB1950(MVPosition &in) {
  getInfo(TDB);
  ROTMAT1 = (*PRECESB1950)(info_p[TDB]);
  in *= ROTMAT1;
}

void MeasMath::deapplyPrecessionB1950(MVPosition &in) {
  getInfo(TDB);
  ROTMAT1 = (*PRECESB1950)(info_p[TDB]);
  in = ROTMAT1 * in;
}

// Nutation
void MeasMath::createNutation() {
  if (!NUTATIAU) NUTATIAU = new Nutation(Nutation::STANDARD);
}

void MeasMath::applyNutation(MVPosition &in) {
  getInfo(TDB);
  ROTMAT1 = (*NUTATIAU)(info_p[TDB]);
  in *= ROTMAT1;
}

void MeasMath::deapplyNutation(MVPosition &in) {
  getInfo(TDB);
  ROTMAT1 = (*NUTATIAU)(info_p[TDB]);
  in = ROTMAT1 * in;
}

void MeasMath::createNutationB1950() {
  if (!NUTATB1950) NUTATB1950 = new Nutation(Nutation::B1950);
}

void MeasMath::applyNutationB1950(MVPosition &in) {
  getInfo(TDB);
  ROTMAT1 = (*NUTATB1950)(info_p[TDB]);
  in *= ROTMAT1;
}

void MeasMath::deapplyNutationB1950(MVPosition &in) {
  getInfo(TDB);
  ROTMAT1 = (*NUTATB1950)(info_p[TDB]);
  in = ROTMAT1 * in;
}

// Precession and Nutation
void MeasMath::createPrecNutat() {
  if (!PRECESIAU) PRECESIAU = new Precession(Precession::STANDARD);
  if (!NUTATIAU) NUTATIAU = new Nutation(Nutation::STANDARD);
}

void MeasMath::applyPrecNutat(MVPosition &in) {
  getInfo(TDB);
  ROTMAT1 = (*PRECESIAU)(info_p[TDB]);
  ROTMAT1 *= (*NUTATIAU)(info_p[TDB]);
  in *= ROTMAT1;
}

void MeasMath::deapplyPrecNutat(MVPosition &in) {
  getInfo(TDB);
  ROTMAT1 = (*PRECESIAU)(info_p[TDB]);
  ROTMAT1 *= (*NUTATIAU)(info_p[TDB]);
  in = ROTMAT1 * in;
}

void MeasMath::createPrecNutatB1950() {
  if (!PRECESB1950) PRECESB1950 = new Precession(Precession::B1950);
  if (!NUTATB1950) NUTATB1950 = new Nutation(Nutation::B1950);
}

void MeasMath::applyPrecNutatB1950(MVPosition &in) {
  getInfo(TDB);
  applyETerms(in);
  ROTMAT1 = (*PRECESB1950)(info_p[TDB]);
  ROTMAT1 *= (*NUTATB1950)(info_p[TDB]);
  in *= ROTMAT1;
}

void MeasMath::deapplyPrecNutatB1950(MVPosition &in) {
  getInfo(TDB);
  ROTMAT1 = (*PRECESB1950)(info_p[TDB]);
  ROTMAT1 *= (*NUTATB1950)(info_p[TDB]);
  in = ROTMAT1 * in;
  deapplyETerms(in);
}

// Aberration
void MeasMath::createAberration() {
  if (!ABERIAU) ABERIAU = new Aberration(Aberration::STANDARD);
}

void MeasMath::applyAberration(MVPosition &in) {
  getInfo(TDB);
  // Aberration
  MVPOS1 = (*ABERIAU)(info_p[TDB]);
  // Get length
  lengthE = MVPOS1.radius();
  // Beta^-1 (g1)
  g1 = sqrt(1 - lengthE * lengthE);
  g2 = in * MVPOS1;
  in = (g1*in + (1+g2/(1+g1)) * MVPOS1)*(1.0/(1.0+g2));
  in.adjust();
}

void MeasMath::deapplyAberration(MVPosition &in) {
  getInfo(TDB);
  // Aberration
  MVPOS1 = (*ABERIAU)(info_p[TDB]);
  // Get length
  lengthE = MVPOS1.radius();
  // Beta^-1 (g1)
  g1 = sqrt(1 - lengthE * lengthE);
  // First guess
  MVPOS2 = in - MVPOS1;
  // Solve for aberration solution
  do {
    g2 = MVPOS2 * MVPOS1;
    MVPOS3 = ((g1 * MVPOS2 + 
		(1+g2/(1+g1)) * MVPOS1)*(1.0/(1.0+g2)));
    MVPOS3.adjust();
    for (Int j=0; j<3; j++) {
      g3 = MVPOS1(j);
      MVPOS2(j) -= 
	(MVPOS3(j) - in(j))/
	(((g1+g3*g3/(1+g1))-
	  g3 * MVPOS3(j))/(1+g2));
    };
    MVPOS3 -= in;
  } while (MVPOS3.radius() > 1e-10);
  in = MVPOS2;
  in.adjust();
}
void MeasMath::createAberrationB1950() {
  if (!ABERB1950) ABERB1950 = new Aberration(Aberration::B1950);
}

void MeasMath::applyAberrationB1950(MVPosition &in) {
  getInfo(TDB);
  // Aberration
  MVPOS1 = (*ABERB1950)(info_p[TDB]);
  in += MVPOS1;
  in.adjust();
}

void MeasMath::deapplyAberrationB1950(MVPosition &in) {
  getInfo(TDB);
  // Aberration
  MVPOS1 = (*ABERB1950)(info_p[TDB]);
  in -= MVPOS1;
  in.adjust();
}

// Solar bending
void MeasMath::createSolarPos() {
  if (!SOLPOSIAU) SOLPOSIAU = new SolarPos(SolarPos::STANDARD);
}

void MeasMath::applySolarPos(MVPosition &in) {
  getInfo(TDB);
  // Solar position in rectangular coordinates
  MVPOS1 = (*SOLPOSIAU)(info_p[TDB]);
  // Get length and unit vector
  MVPOS1.adjust(lengthE);
  g1 = -1.974e-8 / lengthE;
  g2 = in * MVPOS1;
  // Check if near sun
  if (!nearAbs(g2, 1.0,
	       1.0-cos(MeasData::SunSemiDiameter()/lengthE))) {
    MVPOS1 -= g2 * in;
    MVPOS1 *= (g1 / (1.0 - g2));
    in += MVPOS1;
    in.adjust();
  };
}

void MeasMath::deapplySolarPos(MVPosition &in) {
  getInfo(TDB);
  // Solar position in rectangular coordinates
  MVPOS1 = (*SOLPOSIAU)(info_p[TDB]);
  // Get length and unit vector
  MVPOS1.adjust(lengthE);
  g1 = -1.974e-8 / lengthE;
  g2 = in * MVPOS1;
  // Check if near sun
  if (!nearAbs(g2, 1.0,
	       1.0-cos(MeasData::SunSemiDiameter()/lengthE))) {
    // First guess
    MVPOS2 = in;
    do {
      MVPOS3 = (MVPOS1 - g2 * MVPOS2) * (g1/(1.0 - g2));
      MVPOS3.adjust();
      for (Int j=0; j<3; j++) {
	g3 = MVPOS1(j);
	MVPOS2(j) -= 
	  (MVPOS3(j) + 
	   MVPOS2(j) - in(j))/
	  (1 + (g3 * MVPOS3(j) -
		g1 * (g2 + g3 *
		      MVPOS2(j)))/(1-g2));
      };
      g2 = MVPOS2 * MVPOS1;
      MVPOS3 += MVPOS2;
      MVPOS3 -= in;
    } while (MVPOS3.radius() > 1e-10);
    in = MVPOS2;
    in.adjust();
  };
}

// Various conversions
void MeasMath::applyHADECtoITRF(MVPosition &in) {
  getInfo(LONG);
  ROTMAT1 = RotMatrix(Euler(info_p[LONG], (uInt) 3));
  in *= ROTMAT1;
  in(1) = -in(1);
}

void MeasMath::deapplyHADECtoITRF(MVPosition &in) {
  getInfo(LONG);
  ROTMAT1 = RotMatrix(Euler(info_p[LONG], (uInt) 3));
  in(1) = -in(1);
  in = ROTMAT1 * in;
}

void MeasMath::applyJ2000toB1950(MVPosition &in) {
  // Frame rotation
  ROTMAT1 = MeasData::MToB1950(4);
  in *= ROTMAT1;
  in.adjust();
  // E-terms
  deapplyETerms(in);
}

void MeasMath::deapplyJ2000toB1950(MVPosition &in) {
  applyETerms(in);
  // Frame rotation
  ROTMAT1 = MeasData::MToJ2000(0);
  in *= ROTMAT1;
  in.adjust();
}

void MeasMath::applyETerms(MVPosition &in) {
  // E-terms
  MVPOS1 = MeasTable::AberETerm(0);
  g1 = in * MVPOS1;
  in += g1 * in;
  in -= MVPOS1;
  in.adjust();
}

void MeasMath::deapplyETerms(MVPosition &in) {
  // E-terms
  // Iterate
  MVPOS1 = MeasTable::AberETerm(0);
  MVPOS2 = in;
  do {
    g1 = MVPOS2 * MVPOS1;
    MVPOS3 = MVPOS2 - MVPOS1 + (g1 * MVPOS2);
    MVPOS3.adjust();
    MVPOS3 -= in;
    MVPOS2 -= MVPOS3;
  } while (MVPOS3.radius() > 1e-10);
  in = MVPOS2;
  in.adjust();
}

// general support
void MeasMath::getInfo(FrameInfo i) {
  // Frame information groups
  static FrameType InfoType[N_FrameInfo] = {
    EPOCH, EPOCH, POSITION, POSITION, POSITION };
  // Frame information methods
  static FRINFO InfoFrame[N_FrameInfo] = {
    MCFrame::getTDB,
    MCFrame::getLASTr,
    MCFrame::getLong,
    MCFrame::getLat,
    MCFrame::getRadius };

  if (!infoOK_p[i]) {
    // Make sure there has not been an epoch added
    getFrame(InfoType[i]);
    if (frameOK_p[InfoType[i]]) {
      (((MCFrame *)(applyFrame_p[InfoType[i]].
		    getMCFramePoint()))->*InfoFrame[i])(info_p[i]);
    } else {
      throw(AipsError(String("Missing information in Frame ") +
		      "specified for conversion"));
    };
    infoOK_p[i] = True;
  };
}
