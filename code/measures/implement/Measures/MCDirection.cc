//# MCDirection.cc:  MDirection conversion routines 
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
#include <aips/Exceptions.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Utilities/Assert.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Quanta/Quantum.h>
#ifdef __GNUG__
typedef Quantum<Double> gpp_direction_bug1;
#endif
#include <aips/Measures/MCDirection.h>
#include <aips/Measures/MCFrame.h>
#include <aips/Quanta/RotMatrix.h>
#include <aips/Quanta/MVPosition.h>
#include <aips/Measures/SolarPos.h>
#include <aips/Measures/Aberration.h>
#include <aips/Measures/Precession.h>
#include <aips/Measures/Nutation.h>
#include <aips/Measures/MeasTable.h>

//# Statics
Bool MCDirection::stateMade_p = False;
uInt MCDirection::ToRef_p[N_Routes][3] = {
  {MDirection::GALACTIC,	MDirection::J2000,	0},
  {MDirection::GALACTIC,	MDirection::B1950,	2},
  {MDirection::J2000,		MDirection::GALACTIC,	0},
  {MDirection::B1950,		MDirection::GALACTIC,	2},
  {MDirection::J2000,		MDirection::B1950,	2},
  {MDirection::B1950,		MDirection::J2000,	2},
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
  {MDirection::AZEL,		MDirection::HADEC,	0},
  {MDirection::HADEC,		MDirection::TOPO,	0},
  {MDirection::AZEL,		MDirection::AZELSW,	0},
  {MDirection::AZELSW,		MDirection::AZEL,	0},
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
  {MDirection::TOPO,		MDirection::APP,	0} };
uInt MCDirection::FromTo_p[MDirection::N_Types][MDirection::N_Types];

//# Constructors
MCDirection::MCDirection() :
  ROTMAT1(0),
  EULER1(0),
  MVPOS1(0), MVPOS2(0), MVPOS3(0),
  SOLPOSFROM(0), SOLPOSTO(0),
  ABERFROM(0), ABERTO(0),
  NUTATFROM(0), NUTATTO(0),
  PRECESFROM(0), PRECESTO(0),
  VEC61(0), VEC62(0), VEC63(0) {
  if (!stateMade_p) {
    MCBase::makeState(MCDirection::stateMade_p, MCDirection::FromTo_p[0],
		      MDirection::N_Types, MCDirection::N_Routes,
		      MCDirection::ToRef_p);
  };
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
  
    Int iin  = inref.getType();
    Int iout = outref.getType();
    if (iin != iout) {
      Bool iplan = ToBool(iin & MDirection::EXTRA);
      Bool oplan = ToBool(iout & MDirection::EXTRA);
      if (iplan) {
	mc.addMethod(MCDirection::R_PLANET0);
	mc.addMethod((iin & ~MDirection::EXTRA) + MCDirection::R_MERCURY);
	mc.addMethod(MCDirection::R_PLANET);
	initConvert(MCDirection::R_PLANET, mc);
	iin = MDirection::JNAT;
      };
      if (oplan) iout = MDirection::J2000;
      Int tmp;
      while (iin != iout) {
	tmp = FromTo_p[iin][iout];
	iin = ToRef_p[tmp][1];
	mc.addMethod(tmp);
	initConvert(tmp, mc);
      };
    };
}

void MCDirection::clearConvert() {
  delete ROTMAT1;    ROTMAT1 = 0;
  delete EULER1;     EULER1 = 0;
  delete MVPOS1;     MVPOS1 = 0;
  delete MVPOS2;     MVPOS2 = 0;
  delete MVPOS3;     MVPOS3 = 0;
  delete SOLPOSFROM; SOLPOSFROM = 0;
  delete SOLPOSTO;   SOLPOSTO = 0;
  delete ABERFROM;   ABERFROM = 0;
  delete ABERTO;     ABERTO = 0;
  delete NUTATFROM;  NUTATFROM = 0;
  delete NUTATTO;    NUTATTO = 0;
  delete PRECESFROM; PRECESFROM = 0;
  delete PRECESTO;   PRECESTO = 0;
  delete VEC61;	     VEC61 = 0;
  delete VEC62;	     VEC62 = 0;
  delete VEC63;	     VEC63 = 0;
}

//# Conversion routines
void MCDirection::initConvert(uInt which, MConvertBase &mc) {

  if (False) initConvert(which, mc);	// Stop warning
  if (!ROTMAT1) ROTMAT1 = new RotMatrix();
  if (!MVPOS1)  MVPOS1 = new MVPosition();
  if (!MVPOS2)  MVPOS2 = new MVPosition();
  if (!MVPOS3)  MVPOS3 = new MVPosition();
  if (!EULER1)  EULER1 = new Euler();
  if (!VEC61)   VEC61 = new Vector<Double>(6);
  if (!VEC62)   VEC62 = new Vector<Double>(6);
  if (!VEC63)   VEC63 = new Vector<Double>(6);
  
  switch (which) {
    
  case J2000_JMEAN:
    if (PRECESFROM) delete PRECESFROM;
    PRECESFROM = new Precession(Precession::STANDARD);
    break;
    
  case B1950_BMEAN:
    if (PRECESFROM) delete PRECESFROM;
    PRECESFROM = new Precession(Precession::B1950);
    break;
    
  case JMEAN_J2000:
    if (PRECESTO) delete PRECESTO;
    PRECESTO = new Precession(Precession::STANDARD);
    break;
    
  case JMEAN_JTRUE:
    if (NUTATFROM) delete NUTATFROM;
    NUTATFROM = new Nutation(Nutation::STANDARD);
    break;
    
  case BMEAN_B1950:
    if (PRECESTO) delete PRECESTO;
    PRECESTO = new Precession(Precession::B1950);
    break;
    
  case BMEAN_BTRUE:
    if (NUTATFROM) delete NUTATFROM;
    NUTATFROM = new Nutation(Nutation::B1950);
    break;
    
  case JTRUE_JMEAN:
    if (NUTATTO) delete NUTATTO;
    NUTATTO = new Nutation(Nutation::STANDARD);
    break;
    
  case BTRUE_BMEAN:
    if (NUTATTO) delete NUTATTO;
    NUTATTO = new Nutation(Nutation::B1950);
    break;
    
  case J2000_JNAT:
    if (SOLPOSFROM) delete SOLPOSFROM;
    SOLPOSFROM = new SolarPos(SolarPos::STANDARD);
    break;
    
  case JNAT_APP:
    if (ABERFROM) delete ABERFROM;
    ABERFROM = new Aberration(Aberration::STANDARD);
    if (NUTATFROM) delete NUTATFROM;
    NUTATFROM = new Nutation(Nutation::STANDARD);
    if (PRECESFROM) delete PRECESFROM;
    PRECESFROM = new Precession(Precession::STANDARD);
    break;
    
  case JNAT_J2000:
    if (SOLPOSTO) delete SOLPOSTO;
    SOLPOSTO = new SolarPos(SolarPos::STANDARD);
    break;
    
  case APP_JNAT:
    if (ABERTO) delete ABERTO;
    ABERTO = new Aberration(Aberration::STANDARD);
    if (NUTATTO) delete NUTATTO;
    NUTATTO = new Nutation(Nutation::STANDARD);
    if (PRECESTO) delete PRECESTO;
    PRECESTO = new Precession(Precession::STANDARD);
    break;
    
  case B1950_APP:
    if (ABERFROM) delete ABERFROM;
    ABERFROM = new Aberration(Aberration::B1950);
    if (NUTATFROM) delete NUTATFROM;
    NUTATFROM = new Nutation(Nutation::B1950);
    if (PRECESFROM) delete PRECESFROM;
    PRECESFROM = new Precession(Precession::B1950);
    break;
    
  case APP_B1950:
    if (ABERTO) delete ABERTO;
    ABERTO = new Aberration(Aberration::B1950);
    if (NUTATTO) delete NUTATTO;
    NUTATTO = new Nutation(Nutation::B1950);
    if (PRECESTO) delete PRECESTO;
    PRECESTO = new Precession(Precession::B1950);
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
  
  MCFrame::make(inref.getFrame());
  MCFrame::make(outref.getFrame());
  
  for (Int i=0; i<mc.nMethod(); i++) {
    
    switch (mc.getMethod(i)) {
 
    case HADEC_ITRF: {
      ((MCFrame *)(MDirection::Ref::framePosition(inref, outref).
		   getMCFramePoint()))->
	getLong(g3);
      *ROTMAT1 = RotMatrix(Euler(g3, (uInt) 3));
      in *= *ROTMAT1;
      in(1) = -in(1);
    };
    break;
    
    case ITRF_HADEC: {
      ((MCFrame *)(MDirection::Ref::framePosition(inref, outref).
		   getMCFramePoint()))->
	getLong(g3);
      *ROTMAT1 = RotMatrix(Euler(g3, (uInt) 3));
      in(1) = -in(1);
      in = *ROTMAT1 * in;
    };
    break;
     
    case GAL_J2000:
      in = MeasData::GALtoJ2000() * in;
      break;
      
    case GAL_B1950:
      in = MeasData::GALtoB1950() * in;
      break;
      
    case J2000_GAL:
      in = MeasData::J2000toGAL() * in;
      break;
      
    case B1950_GAL:
      in = MeasData::B1950toGAL() * in;
      break;
      
    case J2000_B1950: {
      // Frame rotation
      *ROTMAT1 = MeasData::MToB1950(4);
      in *= *ROTMAT1;
      in.adjust();
      // E-terms
      // Iterate
      *MVPOS1 = MeasTable::AberETerm(0);
      *MVPOS2 = in;
      do {
	g1 = *MVPOS2 * *MVPOS1;
	*MVPOS3 = *MVPOS2 - *MVPOS1 + (g1 * *MVPOS2);
	MVPOS3->adjust();
	*MVPOS3 -= in;
	*MVPOS2 -= *MVPOS3;
      } while (MVPOS3->radius() > 1e-10);
      in = *MVPOS2;
      in.adjust();
    }
    break;
    
    case B1950_J2000: {
      // E-terms
      *MVPOS1 = MeasTable::AberETerm(0);
      g1 = in * *MVPOS1;
      in += g1 * in;
      in -= *MVPOS1;
      in.adjust();
      // Frame rotation
      *ROTMAT1 = MeasData::MToJ2000(0);
      in *= *ROTMAT1;
      in.adjust();
    }	
    break;
    
    case J2000_JMEAN: {
      ((MCFrame *)(MDirection::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Precession
      *ROTMAT1 = PRECESFROM->operator()(tdbTime);
      in *= *ROTMAT1;
    }
    break;
    
    case B1950_BMEAN: {
      ((MCFrame *)(MDirection::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Precession
      *ROTMAT1 = PRECESFROM->operator()(tdbTime);
      in *= *ROTMAT1;
    }
    break;
    
    case JMEAN_J2000: {
      ((MCFrame *)(MDirection::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Precession
      *ROTMAT1 = PRECESTO->operator()(tdbTime);
      in = *ROTMAT1 * in;
    }
    break;
    
    case JMEAN_JTRUE: {
      ((MCFrame *)(MDirection::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Nutation
      *ROTMAT1 = NUTATFROM->operator()(tdbTime);
      in *= *ROTMAT1;
    }
    break;
    
    case BMEAN_B1950: {
      ((MCFrame *)(MDirection::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Precession
      *ROTMAT1 = PRECESTO->operator()(tdbTime);
      in = *ROTMAT1 * in;
    }
    break;
    
    case BMEAN_BTRUE: {
      ((MCFrame *)(MDirection::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Nutation
      *ROTMAT1 = NUTATFROM->operator()(tdbTime);
      in *= *ROTMAT1;
    }
    break;
    
    case JTRUE_JMEAN: {
      ((MCFrame *)(MDirection::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Nutation
      *ROTMAT1 = NUTATTO->operator()(tdbTime);
      in = *ROTMAT1 * in;
    }
    break;
    
    case BTRUE_BMEAN: {
      ((MCFrame *)(MDirection::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Nutation
      *ROTMAT1 = NUTATTO->operator()(tdbTime);
      in = *ROTMAT1 * in;
    }
    break;
    
    case J2000_JNAT: {
      ((MCFrame *)(MDirection::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Solar position in rectangular coordinates
      *MVPOS1 = SOLPOSFROM->operator()(tdbTime);
      // Get length and unit vector
      MVPOS1->adjust(lengthE);
      g1 = -1.974e-8 / lengthE;
      g2 = in * *MVPOS1;
      // Check if near sun
      if (!nearAbs(g2, 1.0,
		   1.0-cos(MeasData::SunSemiDiameter()/lengthE))) {
	*MVPOS1 -= g2 * in;
	*MVPOS1 *= (g1 / (1.0 - g2));
	in += *MVPOS1;
	in.adjust();
      };
    }
    break;
    
    case JNAT_APP: {
      ((MCFrame *)(MDirection::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Aberration
      *MVPOS1 =  ABERFROM->operator()(tdbTime);
      // Get length
      lengthE = MVPOS1->radius();
      // Beta^-1 (g1)
      g1 = sqrt(1 - lengthE * lengthE);
      g2 = in * *MVPOS1;
      in = (g1*in + (1+g2/(1+g1)) * *MVPOS1)*(1.0/(1.0+g2));
      in.adjust();
      // Precession
      *ROTMAT1 = PRECESFROM->operator()(tdbTime);
      // Nutation
      *ROTMAT1 *= NUTATFROM->operator()(tdbTime);
      in *= *ROTMAT1;
    }
    break;
    
    case APP_JNAT: {
      ((MCFrame *)(MDirection::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Precession
      *ROTMAT1 = PRECESTO->operator()(tdbTime);
      // Nutation
      *ROTMAT1 *= NUTATTO->operator()(tdbTime);
      in = *ROTMAT1 * in;
      // Aberration
      *MVPOS1 = ABERTO->operator()(tdbTime);
      // Get length
      lengthE = MVPOS1->radius();
      // Beta^-1 (g1)
      g1 = sqrt(1 - lengthE * lengthE);
      // First guess
      *MVPOS2 = in - *MVPOS1;
      // Solve for aberration solution
      do {
	g2 = *MVPOS2 * *MVPOS1;
	*MVPOS3 = ((g1 * *MVPOS2 + 
		    (1+g2/(1+g1)) * *MVPOS1)*(1.0/(1.0+g2)));
	MVPOS3->adjust();
	for (Int j=0; j<3; j++) {
	  g3 = MVPOS1->operator()(j);
	  MVPOS2->operator()(j) -= 
	    (MVPOS3->operator()(j) - in(j))/
	    (((g1+g3*g3/(1+g1))-
	      g3*MVPOS3->operator()(j))/(1+g2));
	};
	*MVPOS3 -= in;
      }
      while (MVPOS3->radius() > 1e-10);
      in = *MVPOS2;
      in.adjust();
    }
    break;
    
    case JNAT_J2000: {
      ((MCFrame *)(MDirection::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Solar position in rectangular coordinates
      *MVPOS1 = SOLPOSTO->operator()(tdbTime);
      // Get length and unit vector
      MVPOS1->adjust(lengthE);
      g1 = -1.974e-8 / lengthE;
      // Check if near sun
      g2 = in * *MVPOS1;
      if (!nearAbs(g2, 1.0,
		   1.0-cos(MeasData::SunSemiDiameter()/lengthE))) {
	// First guess
	*MVPOS2 = in;
	do {
	  *MVPOS3 = (*MVPOS1 - g2 * *MVPOS2) * (g1/(1.0 - g2));
	  MVPOS3->adjust();
	  for (Int j=0; j<3; j++) {
	    g3 = MVPOS1->operator()(j);
	    MVPOS2->operator()(j) -= 
	      (MVPOS3->operator()(j) + 
	       MVPOS2->operator()(j) - in(j))/
	      (1 + (g3 * MVPOS3->operator()(j) -
		    g1 * (g2 + g3 *
			  MVPOS2->operator()(j)))/(1-g2));
	  };
	  g2 = *MVPOS2 * *MVPOS1;
	  *MVPOS3 += *MVPOS2;
	  *MVPOS3 -= in;
	}
	while (MVPOS3->radius() > 1e-10);
	in = *MVPOS2;
	in.adjust();
      };
    }
    break;
    
    case B1950_APP: {
      ((MCFrame *)(MDirection::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // E-terms
      *MVPOS1 = MeasTable::AberETerm(0);
      g1 = in * *MVPOS1;
      in += g1 * in;
      in -= *MVPOS1;
      in.adjust();
      // Precession
      *ROTMAT1 = PRECESFROM->operator()(tdbTime);
      // Nutation
      *ROTMAT1 *= NUTATFROM->operator()(tdbTime);
      in *= *ROTMAT1;
      // Aberration
      *MVPOS1 = ABERFROM->operator()(tdbTime);
      in += *MVPOS1;
      in.adjust();
    }
    break;
    
    case APP_B1950: {
      ((MCFrame *)(MDirection::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Aberration
      *MVPOS1 = ABERTO->operator()(tdbTime);
      in -= *MVPOS1;
      in.adjust();
      // Precession
      *ROTMAT1 = PRECESTO->operator()(tdbTime);
      // Nutation
      *ROTMAT1 *= NUTATTO->operator()(tdbTime);
      in = *ROTMAT1 * in;
      // E-terms
      // Iterate
      *MVPOS1 = MeasTable::AberETerm(0);
      *MVPOS2 = in;
      do {
        g1 = *MVPOS2 * *MVPOS1;
        *MVPOS3 = *MVPOS2 - *MVPOS1 + (g1 * *MVPOS2);
        MVPOS3->adjust();
        *MVPOS3 -= in;
        *MVPOS2 -= *MVPOS3;
      } while (MVPOS3->radius() > 1e-10);
      in = *MVPOS2;
      in.adjust();
    }
    break;
    
    case TOPO_HADEC: {
      ((MCFrame *)(MDirection::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getLASTr(g1);
      ((MCFrame *)(MDirection::Ref::framePosition(outref, inref).
		   getMCFramePoint()))->
	getRadius(lengthE);
      ((MCFrame *)(MDirection::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      ((MCFrame *)(MDirection::Ref::framePosition(outref, inref).
		   getMCFramePoint()))->
	getLat(g3);
      g2 = MeasTable::diurnalAber(lengthE, tdbTime);
      *MVPOS3 = MVDirection(g1, g3);
      MVPOS3->readjust(g2);
      in += *MVPOS3;
      *EULER1 = MeasTable::polarMotion(tdbTime);
      EULER1->operator()(2) = g1;
      *ROTMAT1 = RotMatrix(*EULER1);
      in *= *ROTMAT1;
      in(1) = -in(1);
      in.adjust();
    }
    break;
    
    case HADEC_AZEL:
      ((MCFrame *)(MDirection::Ref::framePosition(outref, inref).
		   getMCFramePoint()))->
	getLat(g1);
    *ROTMAT1 = RotMatrix(Euler(C::pi_2-g1 ,(uInt) 2,
			       C::pi, (uInt) 3));
    in *= *ROTMAT1;
    break;
    
    case AZEL_HADEC:
      ((MCFrame *)(MDirection::Ref::framePosition(inref, outref).
		   getMCFramePoint()))->
	getLat(g1);
    *ROTMAT1 = RotMatrix(Euler(C::pi_2-g1 ,(uInt) 2,
			       C::pi, (uInt) 3));
    in = *ROTMAT1 * in;
    break;
    
    case HADEC_TOPO: {
      ((MCFrame *)(MDirection::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getLASTr(g1);
      ((MCFrame *)(MDirection::Ref::framePosition(inref, outref).
		   getMCFramePoint()))->
	getRadius(lengthE);
      ((MCFrame *)(MDirection::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      ((MCFrame *)(MDirection::Ref::framePosition(inref, outref).
		   getMCFramePoint()))->
	getLat(g3);
      g2 = MeasTable::diurnalAber(lengthE, tdbTime);
      *MVPOS3 = MVDirection(g1, g3);
      MVPOS3->readjust(g2);
      in(1) = -in(1);
      *EULER1 = MeasTable::polarMotion(tdbTime);
      EULER1->operator()(2) = g1;
      *ROTMAT1 = RotMatrix(*EULER1);
      in = *ROTMAT1 * in;
      in -= *MVPOS3;
      in.adjust();
    }
    break;
    
    case APP_TOPO: {
      if (lengthP != 0) {
	((MCFrame *)(MDirection::Ref::frameEpoch(inref, outref).
		     getMCFramePoint()))->
	  getLASTr(g1);
	((MCFrame *)(MDirection::Ref::framePosition(inref, outref).
		     getMCFramePoint()))->
	  getLong(g3);
	((MCFrame *)(MDirection::Ref::framePosition(inref, outref).
		     getMCFramePoint()))->
	  getLat(g2);
	((MCFrame *)(MDirection::Ref::framePosition(inref, outref).
		     getMCFramePoint()))->
	  getRadius(lengthE);
	*ROTMAT1 = RotMatrix(Euler(g1-g3, (uInt) 3));
	*MVPOS1 = MVPosition(Quantity(lengthE, "m"), g3, g2);
	in -= (*ROTMAT1 * *MVPOS1) * (1.0/lengthP);
	in.adjust();
      };
    }
    break;
    
    case TOPO_APP: {
      if (lengthP != 0) {
	((MCFrame *)(MDirection::Ref::frameEpoch(inref, outref).
		     getMCFramePoint()))->
	  getLASTr(g1);
	((MCFrame *)(MDirection::Ref::framePosition(inref, outref).
		     getMCFramePoint()))->
	  getLong(g3);
	((MCFrame *)(MDirection::Ref::framePosition(inref, outref).
		     getMCFramePoint()))->
	  getLat(g2);
	((MCFrame *)(MDirection::Ref::framePosition(inref, outref).
		     getMCFramePoint()))->
	  getRadius(lengthE);
	*ROTMAT1 = RotMatrix(Euler(g1-g3, (uInt) 3));
	*MVPOS1 = MVPosition(Quantity(lengthE, "m"), g3, g2);
	in += (*ROTMAT1 * *MVPOS1) * (1.0/lengthP);
	in.adjust();
      };
    }
    break;

    case AZEL_AZELSW: 
    case AZELSW_AZEL: {
      in(0) = -in(0);
      in(1) = -in(1);
    }
    break;

    case ECLIP_J2000:
      *ROTMAT1 = RotMatrix(Euler(MeasTable::fundArg(0)(0), 1, 0, 0));
      in = *ROTMAT1 * in;
      break;

    case J2000_ECLIP:
      *ROTMAT1 = RotMatrix(Euler(MeasTable::fundArg(0)(0), 1, 0, 0));
      in *= *ROTMAT1;
      break;

    case MECLIP_JMEAN:
      ((MCFrame *)(MDirection::Ref::frameEpoch(outref, inref).
                   getMCFramePoint()))->
        getTDB(tdbTime);
      *ROTMAT1 = 
	RotMatrix(Euler(MeasTable::fundArg(0)((tdbTime - 
					       MeasData::MJD2000)/
					      MeasData::JDCEN), 1, 0, 0));
      in = *ROTMAT1 * in;
      break;

    case JMEAN_MECLIP:
      ((MCFrame *)(MDirection::Ref::frameEpoch(outref, inref).
                   getMCFramePoint()))->
        getTDB(tdbTime);
      *ROTMAT1 =
        RotMatrix(Euler(MeasTable::fundArg(0)((tdbTime -
                                               MeasData::MJD2000)/
                                              MeasData::JDCEN), 1, 0, 0));
      in *= *ROTMAT1;
      break;

    case TECLIP_JTRUE:
      ((MCFrame *)(MDirection::Ref::frameEpoch(outref, inref).
                   getMCFramePoint()))->
        getTDB(tdbTime);
      *ROTMAT1 = 
	RotMatrix(Euler(-Nutation(Nutation::STANDARD)(tdbTime)(2), 1, 0, 0));
      in = *ROTMAT1 * in;
      break;

    case JTRUE_TECLIP:
      ((MCFrame *)(MDirection::Ref::frameEpoch(outref, inref).
                   getMCFramePoint()))->
        getTDB(tdbTime);
      *ROTMAT1 =
        RotMatrix(Euler(-Nutation(Nutation::STANDARD)(tdbTime)(2), 1, 0, 0));
      in *= *ROTMAT1;
      break;

    case GAL_SUPERGAL:
      in = MeasTable::galToSupergal() * in;
      break;
      
    case SUPERGAL_GAL:
      in *= MeasTable::galToSupergal();
      break;
    
    case R_PLANET0: {
      ((MCFrame *)(MDirection::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      *VEC62 = MeasTable::Planetary(MeasTable::EARTH, tdbTime); // Eb
      *VEC63 = MeasTable::Planetary(MeasTable::SUN, tdbTime);   // Sb
      for (Int i=0; i<3; i++)
	(*MVPOS3)(i) = (*VEC62)(i) - (*VEC63)(i);		// E
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
	for (Int i=0; i<3; i++) {
	  (*MVPOS1)(i) = (*VEC61)(i) - (*VEC62)(i);		// P
	  (*MVPOS2)(i) = (*VEC61)(i) - (*VEC63)(i);		// Q
	};
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
      };
      in.adjust();
    }
    break;
    
    case R_MERCURY: {
      planID = MeasTable::MERCURY;
    }
    break;
    
    case R_VENUS: {
      planID = MeasTable::VENUS;
    }
    break;
    
    case R_MARS: {
      planID = MeasTable::MARS;
    }
    break;
    
    case R_JUPITER: {
      planID = MeasTable::JUPITER;
    }
    break;
    
    case R_SATURN: {
      planID = MeasTable::SATURN;
    }
    break;
    
    case R_URANUS: {
      planID = MeasTable::URANUS;
    }
    break;
    
    case R_NEPTUNE: {
      planID = MeasTable::NEPTUNE;
    }
    break;
    
    case R_PLUTO: {
      planID = MeasTable::PLUTO;
    }
    break;
    
    case R_SUN: {
      planID = MeasTable::SUN;
    }
    break;
    
    case R_MOON: {
      planID = MeasTable::MOON;
    }
    break;
    
    default:
      break;
      
    };	// switch
  };	// for
}
