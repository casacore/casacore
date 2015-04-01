//# MCuvw.cc:  Muvw conversion routines 
//# Copyright (C) 1998-2000,2002,2004,2007
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
#include <casacore/measures/Measures/MCuvw.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Statics
uInt MCuvw::ToRef_p[N_Routes][3] = {
  {Muvw::GALACTIC,	 	Muvw::J2000,	0},
  {Muvw::GALACTIC,		Muvw::B1950,	2},
  {Muvw::J2000,		 	Muvw::GALACTIC,	0},
  {Muvw::B1950,			Muvw::GALACTIC,	2},
  {Muvw::J2000,			Muvw::B1950,	2},
  {Muvw::J2000,			Muvw::B1950_VLA,2},
  {Muvw::B1950,			Muvw::J2000,	2},
  {Muvw::B1950_VLA,		Muvw::J2000,	2},
  {Muvw::B1950,			Muvw::B1950_VLA,0},
  {Muvw::B1950_VLA,		Muvw::B1950,	0},
  {Muvw::J2000,			Muvw::JMEAN,	0},
  {Muvw::B1950,			Muvw::BMEAN,	2},
  {Muvw::JMEAN,			Muvw::J2000,	0},
  {Muvw::JMEAN,			Muvw::JTRUE,	0},
  {Muvw::BMEAN,			Muvw::B1950,	2},
  {Muvw::BMEAN,			Muvw::BTRUE,	2},
  {Muvw::JTRUE,			Muvw::JMEAN,	0},
  {Muvw::BTRUE,			Muvw::BMEAN,	2},
  {Muvw::J2000,			Muvw::JNAT,	0},
  {Muvw::JNAT,			Muvw::J2000,	0},
  {Muvw::B1950,			Muvw::APP,	2},
  {Muvw::APP,			Muvw::B1950,	2},
  {Muvw::APP,			Muvw::TOPO,	0},
  {Muvw::HADEC,			Muvw::AZEL,	0},
  {Muvw::HADEC,           	Muvw::AZELGEO,  0},
  {Muvw::AZEL,			Muvw::HADEC,	0},
  {Muvw::AZELGEO,         	Muvw::HADEC,    0},
  {Muvw::HADEC,			Muvw::TOPO,	0},
  {Muvw::AZEL,			Muvw::AZELSW,	0},
  {Muvw::AZELGEO,         	Muvw::AZELSWGEO,0},
  {Muvw::AZELSW,		Muvw::AZEL,	0},
  {Muvw::AZELSWGEO,       	Muvw::AZELGEO,  0},
  {Muvw::APP,			Muvw::JNAT,	0},
  {Muvw::JNAT,			Muvw::APP,	0},
  {Muvw::J2000,			Muvw::ECLIPTIC,	0},
  {Muvw::ECLIPTIC,		Muvw::J2000,	0},
  {Muvw::JMEAN,			Muvw::MECLIPTIC,0},
  {Muvw::MECLIPTIC,		Muvw::JMEAN,	0},
  {Muvw::JTRUE,			Muvw::TECLIPTIC,0},
  {Muvw::TECLIPTIC,		Muvw::JTRUE,	0},
  {Muvw::GALACTIC,		Muvw::SUPERGAL,	0},
  {Muvw::SUPERGAL,		Muvw::GALACTIC,	0},
  {Muvw::ITRF,			Muvw::HADEC,	0},
  {Muvw::HADEC,			Muvw::ITRF,	0},
  {Muvw::TOPO,			Muvw::HADEC,	0},
  {Muvw::TOPO,			Muvw::APP,	0},
  {Muvw::ICRS,			Muvw::J2000,	0},
  {Muvw::J2000,			Muvw::ICRS,	0} };
uInt MCuvw::FromTo_p[Muvw::N_Types][Muvw::N_Types];
MutexedInit MCuvw::theirMutexedInit (MCuvw::doFillState);

//# Constructors
MCuvw::MCuvw() : measMath() {
    fillState();
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
    }
  }
}

void MCuvw::clearConvert() {
}

//# Conversion routines
void MCuvw::initConvert(uInt which, MConvertBase &mc) {
  if (False) initConvert(which, mc);	// Stop warning
  
  switch (which) {

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
    measMath.createPrecession();
    measMath.createNutation();
    break;

  case BMEAN_B1950:
    measMath.createPrecessionB1950();
    break;

  case BMEAN_BTRUE:
    measMath.createPrecessionB1950();
    measMath.createNutationB1950();
    break;

  case JTRUE_JMEAN:
    measMath.createPrecession();
    measMath.createNutation();
    break;
    
  case BTRUE_BMEAN:
    measMath.createPrecessionB1950();
    measMath.createNutationB1950();
    break;
    
  case J2000_JNAT:
    measMath.createSolarPos();
    break;

  case JNAT_APP:
    measMath.createAberration();
    measMath.createPrecNutat();
    measMath.createSolarPos();
    break;
    
  case JNAT_J2000:
    measMath.createSolarPos();
    break;
    
  case APP_JNAT:
    measMath.createAberration();
    measMath.createPrecNutat();
    break;

  case B1950_APP:
    measMath.createAberrationB1950();
    measMath.createPrecNutatB1950();
    break;

  case APP_B1950:
    measMath.createAberrationB1950();
    measMath.createPrecNutatB1950();
    break;

  case MECLIP_JMEAN:
    measMath.createPrecession();
    break;

  case JMEAN_MECLIP:
    measMath.createPrecession();
    break;

  case TECLIP_JTRUE:
    measMath.createPrecession();
    break;

  case JTRUE_TECLIP:
    measMath.createPrecession();
    break;

  default:
    break;
    
  }
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
  // Planetary aberration factor
  Double lengthP = 0;
  
  measMath.initFrame(inref, outref);
  
  for (Int i=0; i<mc.nMethod(); i++) {
    
    switch (mc.getMethod(i)) {

    case HADEC_ITRF:
      getAPP();
      measMath.applyAPPtoTOPO(MVDIR1, lengthP);
      measMath.applyTOPOtoHADEC(MVDIR1);
      toPole(in);
      measMath.applyHADECtoITRF(in);
      measMath.applyHADECtoITRF(MVDIR1);
      break;

    case ITRF_HADEC:
      getAPP();
      measMath.applyAPPtoTOPO(MVDIR1, lengthP);
      measMath.applyTOPOtoHADEC(MVDIR1);
      measMath.applyHADECtoITRF(MVDIR1);
      toPole(in);
      measMath.deapplyHADECtoITRF(in);
      measMath.deapplyHADECtoITRF(MVDIR1);
      break;

    case GAL_J2000:
      getJ2000();
      measMath.deapplyGALtoJ2000(MVDIR1);
      toPole(in);
      measMath.applyGALtoJ2000(in);
      measMath.applyGALtoJ2000(MVDIR1);
      break;

    case GAL_B1950:
      getB1950();
      measMath.deapplyGALtoB1950(MVDIR1);
      toPole(in);
      measMath.applyGALtoB1950(in);
      measMath.applyGALtoB1950(MVDIR1);
      break;
      
   case J2000_GAL:
      getJ2000();
      toPole(in);
      measMath.deapplyGALtoJ2000(in);
      measMath.deapplyGALtoJ2000(MVDIR1);
     break;

    case B1950_GAL:
      getB1950();
      toPole(in);
      measMath.deapplyGALtoB1950(in);
      measMath.deapplyGALtoB1950(MVDIR1);
      break;

    case J2000_B1950:
      getJ2000();
      toPole(in);
      in.adjust(g2);
      measMath.applyJ2000toB1950(in, False);
      in.readjust(g2);
      measMath.applyJ2000toB1950(MVDIR1);
      break;
      
    case J2000_B1950_VLA:
      getJ2000();
      toPole(in);
      in.adjust(g2);
      measMath.applyJ2000toB1950_VLA(in, False);
      in.readjust(g2);
      measMath.applyJ2000toB1950_VLA(MVDIR1);
      break;

    case B1950_J2000:
      getB1950();
      toPole(in);
      in.adjust(g2);
      measMath.deapplyJ2000toB1950(in, False);
      in.readjust(g2);
      measMath.deapplyJ2000toB1950(MVDIR1);
      break;
    
    case B1950_VLA_J2000:
      getB1950();
      toPole(in);
      in.adjust(g2);
      measMath.deapplyJ2000toB1950_VLA(in, False);
      in.readjust(g2);
      measMath.deapplyJ2000toB1950_VLA(MVDIR1);
      break;

    case B1950_B1950_VLA:

    case B1950_VLA_B1950:
      getB1950();
      toPole(in);
      break;

    case J2000_JMEAN:
      getJ2000();
      toPole(in);
      measMath.applyPrecession(in);
      measMath.applyPrecession(MVDIR1);
      break;

    case B1950_BMEAN:
      getB1950();
      toPole(in);
      measMath.applyPrecessionB1950(in);
      measMath.applyPrecessionB1950(MVDIR1);
      break;
    
    case JMEAN_J2000:
      getJ2000();
      measMath.applyPrecession(MVDIR1);
      toPole(in);
      measMath.deapplyPrecession(in);
      measMath.deapplyPrecession(MVDIR1);
      break;

    case JMEAN_JTRUE:
      getJ2000();
      measMath.applyPrecession(MVDIR1);
      toPole(in);
      measMath.applyNutation(in);
      measMath.applyNutation(MVDIR1);
      break;
    
    case BMEAN_B1950:
      getB1950();
      measMath.applyPrecessionB1950(MVDIR1);
      toPole(in);
      measMath.deapplyPrecessionB1950(in);
      measMath.deapplyPrecessionB1950(MVDIR1);
      break;

    case BMEAN_BTRUE:
      getB1950();
      measMath.applyPrecessionB1950(MVDIR1);
      toPole(in);
      measMath.applyNutationB1950(in);
      measMath.applyNutationB1950(MVDIR1);
      break;

    case JTRUE_JMEAN:
      getJ2000();
      measMath.applyPrecession(MVDIR1);
      measMath.applyNutation(MVDIR1);
      toPole(in);
      measMath.deapplyNutation(in);
      measMath.deapplyNutation(MVDIR1);
      break;

    case BTRUE_BMEAN:
      getB1950();
      measMath.applyPrecessionB1950(MVDIR1);
      measMath.applyNutationB1950(MVDIR1);
      toPole(in);
      measMath.deapplyNutationB1950(in);
      measMath.deapplyNutationB1950(MVDIR1);
      break;
    
    case J2000_JNAT: 
      getJ2000();
      toPole(in);
      in.adjust(g2);
      measMath.applySolarPos(in, False);
      in.readjust(g2);
      measMath.applySolarPos(MVDIR1);
      break;
    
    case JNAT_APP:
      getJ2000();
      measMath.applySolarPos(MVDIR1);
      toPole(in);
      in.adjust(g2);
      measMath.applyAberration(in, False);
      in.readjust(g2);
      measMath.applyPrecNutat(in);
      measMath.applyAberration(MVDIR1);
      measMath.applyPrecNutat(MVDIR1);
      break;

    case APP_JNAT:
      getAPP();
      toPole(in);
      measMath.deapplyPrecNutat(in);
      in.adjust(g2);
      measMath.deapplyAberration(in, False);
      in.readjust(g2);
      measMath.deapplyPrecNutat(MVDIR1);
      measMath.deapplyAberration(MVDIR1);
      break;

    case JNAT_J2000:
      getJ2000();
      measMath.applySolarPos(MVDIR1);
      toPole(in);
      in.adjust(g2);
      measMath.deapplySolarPos(in, False);
      in.readjust(g2);
      measMath.deapplySolarPos(MVDIR1);
      break;

    case B1950_APP:
      getB1950();
      toPole(in);
      in.adjust(g2);
      measMath.applyPrecNutatB1950(in, False);
      measMath.applyAberrationB1950(in, False);
      in.readjust(g2);
      measMath.applyPrecNutatB1950(MVDIR1);
      measMath.applyAberrationB1950(MVDIR1);
      break;

    case APP_B1950:
      getAPP();
      toPole(in);
      in.adjust(g2);
      measMath.deapplyAberrationB1950(in, False);
      measMath.deapplyPrecNutatB1950(in, False);
      in.readjust(g2);
      measMath.deapplyAberrationB1950(MVDIR1);
      measMath.deapplyPrecNutatB1950(MVDIR1);
      break;
    
    case TOPO_HADEC: 
      getAPP();
      measMath.applyAPPtoTOPO(in, lengthP);
      toPole(in);
      in.adjust(g2);
      measMath.applyTOPOtoHADEC(in, False);
      in.readjust(g2);
      measMath.applyTOPOtoHADEC(MVDIR1);
      break;

    case HADEC_AZEL:
      getAPP();
      measMath.applyAPPtoTOPO(MVDIR1, lengthP);
      measMath.applyTOPOtoHADEC(MVDIR1);
      toPole(in);
      measMath.applyHADECtoAZEL(in);
      measMath.applyHADECtoAZEL(MVDIR1);
      break;
    
    case HADEC_AZELGEO:    
      getAPP();    
      measMath.applyAPPtoTOPO(MVDIR1, lengthP);  
      measMath.applyTOPOtoHADEC(MVDIR1);    
      toPole(in);  
      measMath.applyHADECtoAZELGEO(in);   
      measMath.applyHADECtoAZELGEO(MVDIR1);    
      break;  
 
    case AZEL_HADEC:
      getAPP();
      measMath.applyAPPtoTOPO(MVDIR1, lengthP);
      measMath.applyTOPOtoHADEC(MVDIR1);
      measMath.applyHADECtoAZEL(MVDIR1);
      toPole(in);
      measMath.deapplyHADECtoAZEL(in);
      measMath.deapplyHADECtoAZEL(MVDIR1);
      break;

    case AZELGEO_HADEC:   
      getAPP(); 
      measMath.applyAPPtoTOPO(MVDIR1, lengthP);    
      measMath.applyTOPOtoHADEC(MVDIR1);    
      measMath.applyHADECtoAZELGEO(MVDIR1);    
      toPole(in);      
      measMath.deapplyHADECtoAZELGEO(in);      
      measMath.deapplyHADECtoAZELGEO(MVDIR1);  
      break;    
      
    case HADEC_TOPO: 
      getAPP();
      measMath.applyAPPtoTOPO(MVDIR1, lengthP);
      measMath.applyTOPOtoHADEC(MVDIR1);
      toPole(in);
      in.adjust(g2);
      measMath.deapplyTOPOtoHADEC(in, False);
      in.readjust(g2);
      measMath.deapplyTOPOtoHADEC(MVDIR1);
      break;
 
    case APP_TOPO: 
      getAPP();
      toPole(in);
      in.adjust(g2);
      measMath.applyAPPtoTOPO(in, lengthP, False);
      in.readjust(g2);
      measMath.applyAPPtoTOPO(MVDIR1, lengthP);
      break;
   
    case TOPO_APP: 
      getAPP();
      measMath.applyAPPtoTOPO(in, lengthP);
      toPole(in);
      in.adjust(g2);
      measMath.deapplyAPPtoTOPO(in, lengthP, False);
      in.readjust(g2);
      measMath.deapplyAPPtoTOPO(MVDIR1, lengthP);
      break;

    case AZEL_AZELSW:
    case AZELSW_AZEL:
      getAPP();
      measMath.applyAPPtoTOPO(MVDIR1, lengthP);
      measMath.applyTOPOtoHADEC(MVDIR1);
      measMath.applyHADECtoAZEL(MVDIR1);
      toPole(in);
      measMath.applyAZELtoAZELSW(in);
      measMath.applyAZELtoAZELSW(MVDIR1);
      break;
  
    case AZELGEO_AZELSWGEO:  
    case AZELSWGEO_AZELGEO:  
      getAPP(); 
      measMath.applyAPPtoTOPO(MVDIR1, lengthP);    
      measMath.applyTOPOtoHADEC(MVDIR1);    
      measMath.applyHADECtoAZELGEO(MVDIR1);    
      toPole(in);      
      measMath.applyAZELtoAZELSW(in);
      measMath.applyAZELtoAZELSW(MVDIR1);   
      break;    
 
    case ECLIP_J2000:
      getJ2000();
      measMath.deapplyECLIPtoJ2000(MVDIR1);
      toPole(in);
      measMath.applyECLIPtoJ2000(in);
      measMath.applyECLIPtoJ2000(MVDIR1);
      break;

    case J2000_ECLIP:
      getJ2000();
      toPole(in);
      measMath.deapplyECLIPtoJ2000(in);
      measMath.deapplyECLIPtoJ2000(MVDIR1);
      break;

    case MECLIP_JMEAN:
      getJ2000();
      measMath.applyPrecession(MVDIR1);
      measMath.deapplyMECLIPtoJMEAN(MVDIR1);
      toPole(in);
      measMath.applyMECLIPtoJMEAN(in);
      measMath.applyMECLIPtoJMEAN(MVDIR1);
      break;

    case JMEAN_MECLIP:
      getJ2000();
      measMath.applyPrecession(MVDIR1);
      toPole(in);
      measMath.deapplyMECLIPtoJMEAN(in);
      measMath.deapplyMECLIPtoJMEAN(MVDIR1);
      break;

    case TECLIP_JTRUE:
      getJ2000();
      measMath.applyPrecession(MVDIR1);
      measMath.deapplyTECLIPtoJTRUE(MVDIR1);
      toPole(in);
      measMath.applyTECLIPtoJTRUE(in);
      measMath.applyTECLIPtoJTRUE(MVDIR1);
      break;

    case JTRUE_TECLIP:
      getJ2000();
      measMath.applyPrecession(MVDIR1);
      toPole(in);
      measMath.deapplyTECLIPtoJTRUE(in);
      measMath.deapplyTECLIPtoJTRUE(MVDIR1);
      break;

    case GAL_SUPERGAL:
      getJ2000();
      measMath.deapplyGALtoJ2000(MVDIR1);
      toPole(in);
      measMath.applyGALtoSUPERGAL(in);
      measMath.applyGALtoSUPERGAL(MVDIR1);
      break;

    case SUPERGAL_GAL:
      getJ2000();
      measMath.deapplyGALtoJ2000(MVDIR1);
      measMath.applyGALtoSUPERGAL(MVDIR1);
      toPole(in);
      measMath.deapplyGALtoSUPERGAL(in);
      measMath.deapplyGALtoSUPERGAL(MVDIR1);
     break;
 
    case ICRS_J2000:
      getJ2000();
      measMath.deapplyICRStoJ2000(MVDIR1);
      toPole(in);
      measMath.applyICRStoJ2000(in);
      measMath.applyICRStoJ2000(MVDIR1);
      break;
      
    case J2000_ICRS:
      getJ2000();
      measMath.applyICRStoJ2000(MVDIR1);
      toPole(in);
      measMath.deapplyICRStoJ2000(in);
      measMath.deapplyICRStoJ2000(MVDIR1);
      break;
    
    default:
      break;
      
    }	// switch
    // Get in correct direction
    fromPole(in);
  }	// for
}

String MCuvw::showState() {
  fillState();
  return MCBase::showState(MCuvw::FromTo_p[0],
			   Muvw::N_Types, MCuvw::N_Routes,
			   MCuvw::ToRef_p);
}

void MCuvw::doFillState (void*) {
  Muvw::checkMyTypes();
  MCBase::makeState(FromTo_p[0], Muvw::N_Types, N_Routes, ToRef_p);
}

void MCuvw::getAPP() {
  measMath.getAPP(MVDIR1);
}

void MCuvw::getJ2000() {
  measMath.getJ2000(MVDIR1);
}

void MCuvw::getB1950() {
  measMath.getB1950(MVDIR1);
}

void MCuvw::toPole(MVPosition &in) {
  in *= RotMatrix(Euler(-C::pi_2 + MVDIR1.getLat(), 2u,
			-MVDIR1.getLong(), 3u));
}

void MCuvw::fromPole(MVPosition &in) {
  in = RotMatrix(Euler(-C::pi_2 + MVDIR1.getLat(), 2u,
		       -MVDIR1.getLong(), 3u)) * in;
}

} //# NAMESPACE CASACORE - END

