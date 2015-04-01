//# MCBaseline.cc:  MBaseline conversion routines 
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
#include <casacore/measures/Measures/MCBaseline.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Statics
uInt MCBaseline::ToRef_p[N_Routes][3] = {
  {MBaseline::GALACTIC,	 	MBaseline::J2000,	0},
  {MBaseline::GALACTIC,		MBaseline::B1950,	2},
  {MBaseline::J2000,		MBaseline::GALACTIC,	0},
  {MBaseline::B1950,		MBaseline::GALACTIC,	2},
  {MBaseline::J2000,		MBaseline::B1950,	2},
  {MBaseline::J2000,		MBaseline::B1950_VLA,	2},
  {MBaseline::B1950,		MBaseline::J2000,	2},
  {MBaseline::B1950_VLA,	MBaseline::J2000,	2},
  {MBaseline::B1950,		MBaseline::B1950_VLA,	0},
  {MBaseline::B1950_VLA,	MBaseline::B1950,	0},
  {MBaseline::J2000,		MBaseline::JMEAN,	0},
  {MBaseline::B1950,		MBaseline::BMEAN,	2},
  {MBaseline::JMEAN,		MBaseline::J2000,	0},
  {MBaseline::JMEAN,		MBaseline::JTRUE,	0},
  {MBaseline::BMEAN,		MBaseline::B1950,	2},
  {MBaseline::BMEAN,		MBaseline::BTRUE,	2},
  {MBaseline::JTRUE,		MBaseline::JMEAN,	0},
  {MBaseline::BTRUE,		MBaseline::BMEAN,	2},
  {MBaseline::J2000,		MBaseline::JNAT,	0},
  {MBaseline::JNAT,		MBaseline::J2000,	0},
  {MBaseline::B1950,		MBaseline::APP,	 	2},
  {MBaseline::APP,		MBaseline::B1950,	2},
  {MBaseline::APP,		MBaseline::TOPO,	0},
  {MBaseline::HADEC,		MBaseline::AZEL,	0},
  {MBaseline::HADEC,            MBaseline::AZELGEO,     0},
  {MBaseline::AZEL,		MBaseline::HADEC,	0},
  {MBaseline::AZELGEO,          MBaseline::HADEC,       0},
  {MBaseline::HADEC,		MBaseline::TOPO,	0},
  {MBaseline::AZEL,		MBaseline::AZELSW,	0},
  {MBaseline::AZELGEO,          MBaseline::AZELSWGEO,   0},
  {MBaseline::AZELSW,		MBaseline::AZEL,	0},
  {MBaseline::AZELSWGEO,        MBaseline::AZELGEO,     0},
  {MBaseline::APP,		MBaseline::JNAT,	0},
  {MBaseline::JNAT,		MBaseline::APP,		0},
  {MBaseline::J2000,		MBaseline::ECLIPTIC,	0},
  {MBaseline::ECLIPTIC,		MBaseline::J2000,	0},
  {MBaseline::JMEAN,		MBaseline::MECLIPTIC,	0},
  {MBaseline::MECLIPTIC,	MBaseline::JMEAN,	0},
  {MBaseline::JTRUE,		MBaseline::TECLIPTIC,	0},
  {MBaseline::TECLIPTIC,	MBaseline::JTRUE,	0},
  {MBaseline::GALACTIC,		MBaseline::SUPERGAL,	0},
  {MBaseline::SUPERGAL,		MBaseline::GALACTIC,	0},
  {MBaseline::ITRF,		MBaseline::HADEC,	0},
  {MBaseline::HADEC,		MBaseline::ITRF,	0},
  {MBaseline::TOPO,		MBaseline::HADEC,	0},
  {MBaseline::TOPO,		MBaseline::APP,		0},
  {MBaseline::ICRS,		MBaseline::J2000,	0},
  {MBaseline::J2000,		MBaseline::ICRS,	0} };
uInt MCBaseline::FromTo_p[MBaseline::N_Types][MBaseline::N_Types];
MutexedInit MCBaseline::theirMutexedInit (MCBaseline::doFillState);

//# Constructors
MCBaseline::MCBaseline() : measMath() {
  fillState();
}

//# Destructor
MCBaseline::~MCBaseline() {
  clearConvert();
}

//# Member functions

void MCBaseline::getConvert(MConvertBase &mc,
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

void MCBaseline::clearConvert() {
}

//# Conversion routines
void MCBaseline::initConvert(uInt which, MConvertBase &mc) {
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
    
  case J2000_JNAT:
    measMath.createSolarPos();
    break;
    
  case JNAT_APP:
    measMath.createAberration();
    measMath.createPrecNutat();
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
    
  default:
    break;
    
  }
}

void MCBaseline::doConvert(MeasValue &in,
			    MRBase &inref,
			    MRBase &outref,
			    const MConvertBase &mc) {
  doConvert((MVBaseline &) in,
	    inref, outref, mc);
}

void MCBaseline::doConvert(MVBaseline &in,
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
      measMath.applyJ2000toB1950(in, False);
      in.readjust(g2);
      break;
      
    case J2000_B1950_VLA:
      in.adjust(g2);
      measMath.applyJ2000toB1950_VLA(in, False);
      in.readjust(g2);
      break;
    
    case B1950_J2000:
      in.adjust(g2);
      measMath.deapplyJ2000toB1950(in, False);
      in.readjust(g2);
      break;
    
    case B1950_VLA_J2000:
      in.adjust(g2);
      measMath.deapplyJ2000toB1950_VLA(in, False);
      in.readjust(g2);
      break;

    case B1950_B1950_VLA:
      in.adjust(g2);
      measMath.deapplyJ2000toB1950(in, False);
      in.readjust(g2);
      in.adjust(g2);
      measMath.applyJ2000toB1950_VLA(in, False);
      in.readjust(g2);
      break;

    case B1950_VLA_B1950:
      in.adjust(g2);
      measMath.deapplyJ2000toB1950_VLA(in, False);
      in.readjust(g2);
      in.adjust(g2);
      measMath.applyJ2000toB1950(in, False);
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
      measMath.applySolarPos(in, False);
      in.readjust(g2);
      break;
    
    case JNAT_APP:
      in.adjust(g2);
      measMath.applyAberration(in, False);
      in.readjust(g2);
      measMath.applyPrecNutat(in);
      break;
    
    case APP_JNAT:
      measMath.deapplyPrecNutat(in);
      in.adjust(g2);
      measMath.deapplyAberration(in, False);
      in.readjust(g2);
      break;
    
    case JNAT_J2000:
      in.adjust(g2);
      measMath.deapplySolarPos(in, False);
      in.readjust(g2);
      break;
    
    case B1950_APP: 
      in.adjust(g2);
      measMath.applyPrecNutatB1950(in, False);
      measMath.applyAberrationB1950(in, False);
      in.readjust(g2);
      break;
    
    case APP_B1950:
      in.adjust(g2);
      measMath.deapplyAberrationB1950(in, False);
      measMath.deapplyPrecNutatB1950(in, False);
      in.readjust(g2);
      break;
    
    case TOPO_HADEC: 
      in.adjust(g2);
      measMath.applyTOPOtoHADEC(in, False);
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
      measMath.deapplyTOPOtoHADEC(in, False);
      in.readjust(g2);
      break;
    
    case APP_TOPO: 
      in.adjust(g2);
      measMath.applyAPPtoTOPO(in, lengthP, False);
      in.readjust(g2);
      break;
   
    case TOPO_APP: 
      in.adjust(g2);
      measMath.deapplyAPPtoTOPO(in, lengthP, False);
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
    
    default:
      break;
      
    }	// switch
  }	// for
}

String MCBaseline::showState() {
  fillState();
  return MCBase::showState(MCBaseline::FromTo_p[0],
			   MBaseline::N_Types, MCBaseline::N_Routes,
			   MCBaseline::ToRef_p);
}

void MCBaseline::doFillState (void*) {
  MBaseline::checkMyTypes();
  MCBase::makeState(FromTo_p[0], MBaseline::N_Types, N_Routes, ToRef_p);
}

} //# NAMESPACE CASACORE - END

