//# MCBaseline.cc:  MBaseline conversion routines 
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
#ifdef __GNUG__
#include <aips/Measures/Quantum.h>
typedef Quantum<Double> gpp_Baseline_bug1;
#endif
#include <aips/Exceptions.h>
#include <trial/Measures/MCBaseline.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Utilities/Assert.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Measures/MCFrame.h>
#include <aips/Measures/RotMatrix.h>
#include <aips/Measures/MVPosition.h>
#include <aips/Measures/Precession.h>
#include <aips/Measures/Nutation.h>
#include <aips/Measures/MeasTable.h>

//# Constructors
MCBaseline::MCBaseline() :
  ROTMAT1(0),
  EULER1(0),
  MVPOS1(0), MVPOS2(0), MVPOS3(0),
  NUTATFROM(0), NUTATTO(0),
  PRECESFROM(0), PRECESTO(0) {}

//# Destructor
MCBaseline::~MCBaseline() {
  clearConvert();
}

//# Operators

//# Member functions

void MCBaseline::getConvert(MConvertBase &mc,
			     const MRBase &inref, 
			     const MRBase &outref) {
  
  // Array of conversion routines to call
  static const MCBaseline::Routes 
    FromTo[MBaseline::N_Types][MBaseline::N_Types] = {
      { MCBaseline::N_Routes,
	MCBaseline::ITRF_JNAT,
	MCBaseline::ITRF_JNAT,
	MCBaseline::ITRF_JNAT,
	MCBaseline::ITRF_JNAT,
	MCBaseline::ITRF_JNAT,
	MCBaseline::ITRF_JNAT,
	MCBaseline::ITRF_JNAT,
	MCBaseline::ITRF_JNAT,
	MCBaseline::ITRF_HADEC,
	MCBaseline::ITRF_HADEC,
	MCBaseline::ITRF_HADEC,
	MCBaseline::ITRF_JNAT,
	MCBaseline::ITRF_JNAT,
	MCBaseline::ITRF_JNAT,
	MCBaseline::ITRF_JNAT,
	MCBaseline::ITRF_JNAT },
      { MCBaseline::J2000_JNAT,
	MCBaseline::N_Routes,
	MCBaseline::J2000_JMEAN,
	MCBaseline::J2000_JMEAN,
	MCBaseline::J2000_JNAT, 
	MCBaseline::J2000_B1950,
	MCBaseline::J2000_B1950, 
	MCBaseline::J2000_B1950,
	MCBaseline::J2000_GAL,
	MCBaseline::J2000_JNAT,
	MCBaseline::J2000_JNAT,
	MCBaseline::J2000_JNAT,
	MCBaseline::J2000_JNAT,
	MCBaseline::J2000_ECLIP,
	MCBaseline::J2000_JMEAN,
	MCBaseline::J2000_JMEAN,
	MCBaseline::J2000_GAL },
      { MCBaseline::JMEAN_J2000, 
	MCBaseline::JMEAN_J2000, 
	MCBaseline::N_Routes,    
	MCBaseline::JMEAN_JTRUE,
	MCBaseline::JMEAN_J2000, 
	MCBaseline::JMEAN_J2000, 
	MCBaseline::JMEAN_J2000, 
	MCBaseline::JMEAN_J2000,
	MCBaseline::JMEAN_J2000,
	MCBaseline::JMEAN_J2000, 
	MCBaseline::JMEAN_J2000, 
	MCBaseline::JMEAN_J2000, 
	MCBaseline::JMEAN_J2000,
	MCBaseline::JMEAN_J2000,
	MCBaseline::JMEAN_MECLIP,
	MCBaseline::JMEAN_JTRUE,
	MCBaseline::JMEAN_J2000 },
      { MCBaseline::JTRUE_JMEAN, 
	MCBaseline::JTRUE_JMEAN, 
	MCBaseline::JTRUE_JMEAN, 
	MCBaseline::N_Routes,
	MCBaseline::JTRUE_JMEAN,  
	MCBaseline::JTRUE_JMEAN, 
	MCBaseline::JTRUE_JMEAN, 
	MCBaseline::JTRUE_JMEAN,
	MCBaseline::JTRUE_JMEAN,
	MCBaseline::JTRUE_JMEAN,  
	MCBaseline::JTRUE_JMEAN,  
	MCBaseline::JTRUE_JMEAN,  
	MCBaseline::JTRUE_JMEAN,
	MCBaseline::JTRUE_JMEAN,
	MCBaseline::JTRUE_JMEAN,
	MCBaseline::JTRUE_TECLIP,
	MCBaseline::JTRUE_JMEAN },
      { MCBaseline::APP_JNAT,  
	MCBaseline::APP_JNAT,  
	MCBaseline::APP_JNAT,  
	MCBaseline::APP_JNAT,  
	MCBaseline::N_Routes,    
	MCBaseline::APP_B1950,  
	MCBaseline::APP_B1950,  
	MCBaseline::APP_B1950,
	MCBaseline::APP_JNAT,
	MCBaseline::APP_HADEC, 
	MCBaseline::APP_HADEC, 
	MCBaseline::APP_HADEC,
	MCBaseline::APP_JNAT,
	MCBaseline::APP_JNAT,
	MCBaseline::APP_JNAT,
	MCBaseline::APP_JNAT,
	MCBaseline::APP_JNAT },
      { MCBaseline::B1950_J2000, 
	MCBaseline::B1950_J2000, 
	MCBaseline::B1950_J2000, 
	MCBaseline::B1950_J2000, 
	MCBaseline::B1950_APP, 
	MCBaseline::N_Routes,    
	MCBaseline::B1950_BMEAN,
	MCBaseline::B1950_BMEAN,
	MCBaseline::B1950_GAL,
	MCBaseline::B1950_APP, 
	MCBaseline::B1950_APP, 
	MCBaseline::B1950_APP,
	MCBaseline::B1950_J2000,
	MCBaseline::B1950_J2000,
	MCBaseline::B1950_J2000,
	MCBaseline::B1950_J2000,
	MCBaseline::B1950_GAL },
      { MCBaseline::BMEAN_B1950, 
	MCBaseline::BMEAN_B1950, 
	MCBaseline::BMEAN_B1950,
	MCBaseline::BMEAN_B1950,
	MCBaseline::BMEAN_B1950, 
	MCBaseline::BMEAN_B1950, 
	MCBaseline::N_Routes,
	MCBaseline::BMEAN_BTRUE,
	MCBaseline::BMEAN_B1950,
	MCBaseline::BMEAN_B1950, 
	MCBaseline::BMEAN_B1950, 
	MCBaseline::BMEAN_B1950, 
	MCBaseline::BMEAN_B1950,
	MCBaseline::BMEAN_B1950,
	MCBaseline::BMEAN_B1950,
	MCBaseline::BMEAN_B1950,
	MCBaseline::BMEAN_B1950 },
      { MCBaseline::BTRUE_BMEAN, 
	MCBaseline::BTRUE_BMEAN, 
	MCBaseline::BTRUE_BMEAN,
	MCBaseline::BTRUE_BMEAN, 
	MCBaseline::BTRUE_BMEAN, 
	MCBaseline::BTRUE_BMEAN, 
	MCBaseline::BTRUE_BMEAN, 
	MCBaseline::N_Routes,   
	MCBaseline::BTRUE_BMEAN,
	MCBaseline::BTRUE_BMEAN, 
	MCBaseline::BTRUE_BMEAN, 
	MCBaseline::BTRUE_BMEAN, 
	MCBaseline::BTRUE_BMEAN,
	MCBaseline::BTRUE_BMEAN,
	MCBaseline::BTRUE_BMEAN,
	MCBaseline::BTRUE_BMEAN,
	MCBaseline::BTRUE_BMEAN },
      { MCBaseline::GAL_J2000,   
	MCBaseline::GAL_J2000,   
	MCBaseline::GAL_J2000, 
	MCBaseline::GAL_J2000,   
	MCBaseline::GAL_J2000,   
	MCBaseline::GAL_B1950,   
	MCBaseline::GAL_B1950,
	MCBaseline::GAL_B1950,
	MCBaseline::N_Routes,
	MCBaseline::GAL_J2000,   
	MCBaseline::GAL_J2000,   
	MCBaseline::GAL_J2000,   
	MCBaseline::GAL_J2000,
	MCBaseline::GAL_J2000,
	MCBaseline::GAL_J2000,
	MCBaseline::GAL_J2000,
	MCBaseline::GAL_SUPERGAL },
      { MCBaseline::HADEC_ITRF,   
	MCBaseline::HADEC_APP,   
	MCBaseline::HADEC_APP, 
	MCBaseline::HADEC_APP,   
	MCBaseline::HADEC_APP,   
	MCBaseline::HADEC_APP,   
	MCBaseline::HADEC_APP,
	MCBaseline::HADEC_APP,
	MCBaseline::HADEC_APP,
	MCBaseline::N_Routes,    
	MCBaseline::HADEC_AZEL,
	MCBaseline::HADEC_AZEL,
	MCBaseline::HADEC_APP,
	MCBaseline::HADEC_APP,
	MCBaseline::HADEC_APP,
	MCBaseline::HADEC_APP,
	MCBaseline::HADEC_APP },
      { MCBaseline::AZEL_HADEC,  
	MCBaseline::AZEL_HADEC,  
	MCBaseline::AZEL_HADEC,  
	MCBaseline::AZEL_HADEC,  
	MCBaseline::AZEL_HADEC,  
	MCBaseline::AZEL_HADEC,  
	MCBaseline::AZEL_HADEC,  
	MCBaseline::AZEL_HADEC,  
	MCBaseline::AZEL_HADEC,  
	MCBaseline::AZEL_HADEC,  
	MCBaseline::N_Routes,
	MCBaseline::AZEL_AZELSW,
	MCBaseline::AZEL_HADEC,
	MCBaseline::AZEL_HADEC,
	MCBaseline::AZEL_HADEC,
	MCBaseline::AZEL_HADEC,
	MCBaseline::AZEL_HADEC },  
      { MCBaseline::AZELSW_AZEL,
	MCBaseline::AZELSW_AZEL,
	MCBaseline::AZELSW_AZEL,
	MCBaseline::AZELSW_AZEL,
	MCBaseline::AZELSW_AZEL,
	MCBaseline::AZELSW_AZEL,
	MCBaseline::AZELSW_AZEL,
	MCBaseline::AZELSW_AZEL,
	MCBaseline::AZELSW_AZEL,
	MCBaseline::AZELSW_AZEL,
	MCBaseline::AZELSW_AZEL,
	MCBaseline::N_Routes,
	MCBaseline::AZELSW_AZEL,
	MCBaseline::AZELSW_AZEL,
	MCBaseline::AZELSW_AZEL,
	MCBaseline::AZELSW_AZEL,
	MCBaseline::AZELSW_AZEL },
      { MCBaseline::JNAT_ITRF,
	MCBaseline::JNAT_J2000,
	MCBaseline::JNAT_J2000,
	MCBaseline::JNAT_J2000,
	MCBaseline::JNAT_APP,
	MCBaseline::JNAT_J2000,
	MCBaseline::JNAT_J2000,
	MCBaseline::JNAT_J2000,
	MCBaseline::JNAT_J2000,
	MCBaseline::JNAT_APP,
	MCBaseline::JNAT_APP,
	MCBaseline::JNAT_APP,
	MCBaseline::N_Routes,
	MCBaseline::JNAT_J2000,
	MCBaseline::JNAT_J2000,
	MCBaseline::JNAT_J2000,
	MCBaseline::JNAT_J2000 },
      { MCBaseline::ECLIP_J2000,
	MCBaseline::ECLIP_J2000,
	MCBaseline::ECLIP_J2000,
	MCBaseline::ECLIP_J2000,
	MCBaseline::ECLIP_J2000,
	MCBaseline::ECLIP_J2000,
	MCBaseline::ECLIP_J2000,
	MCBaseline::ECLIP_J2000,
	MCBaseline::ECLIP_J2000,
	MCBaseline::ECLIP_J2000,
	MCBaseline::ECLIP_J2000,
	MCBaseline::ECLIP_J2000,
	MCBaseline::ECLIP_J2000,
	MCBaseline::N_Routes,
	MCBaseline::ECLIP_J2000,
	MCBaseline::ECLIP_J2000,
	MCBaseline::ECLIP_J2000 },
      { MCBaseline::MECLIP_JMEAN,
	MCBaseline::MECLIP_JMEAN,
	MCBaseline::MECLIP_JMEAN,
	MCBaseline::MECLIP_JMEAN,
	MCBaseline::MECLIP_JMEAN,
	MCBaseline::MECLIP_JMEAN,
	MCBaseline::MECLIP_JMEAN,
	MCBaseline::MECLIP_JMEAN,
	MCBaseline::MECLIP_JMEAN,
	MCBaseline::MECLIP_JMEAN,
	MCBaseline::MECLIP_JMEAN,
	MCBaseline::MECLIP_JMEAN,
	MCBaseline::MECLIP_JMEAN,
	MCBaseline::MECLIP_JMEAN,
	MCBaseline::N_Routes,
	MCBaseline::MECLIP_JMEAN,
	MCBaseline::MECLIP_JMEAN },
      { MCBaseline::TECLIP_JTRUE,
	MCBaseline::TECLIP_JTRUE,
	MCBaseline::TECLIP_JTRUE,
	MCBaseline::TECLIP_JTRUE,
	MCBaseline::TECLIP_JTRUE,
	MCBaseline::TECLIP_JTRUE,
	MCBaseline::TECLIP_JTRUE,
	MCBaseline::TECLIP_JTRUE,
	MCBaseline::TECLIP_JTRUE,
	MCBaseline::TECLIP_JTRUE,
	MCBaseline::TECLIP_JTRUE,
	MCBaseline::TECLIP_JTRUE,
	MCBaseline::TECLIP_JTRUE,
	MCBaseline::TECLIP_JTRUE,
	MCBaseline::TECLIP_JTRUE,
	MCBaseline::N_Routes,
	MCBaseline::TECLIP_JTRUE },
      { MCBaseline::SUPERGAL_GAL,
	MCBaseline::SUPERGAL_GAL,
	MCBaseline::SUPERGAL_GAL,
	MCBaseline::SUPERGAL_GAL,
	MCBaseline::SUPERGAL_GAL,
	MCBaseline::SUPERGAL_GAL,
	MCBaseline::SUPERGAL_GAL,
	MCBaseline::SUPERGAL_GAL,
	MCBaseline::SUPERGAL_GAL,
	MCBaseline::SUPERGAL_GAL,
	MCBaseline::SUPERGAL_GAL,
	MCBaseline::SUPERGAL_GAL,
	MCBaseline::SUPERGAL_GAL,
	MCBaseline::SUPERGAL_GAL,
	MCBaseline::SUPERGAL_GAL,
	MCBaseline::SUPERGAL_GAL,
	MCBaseline::N_Routes }
    };
    
    // List of codes converted to
    static const MBaseline::Types ToRef[MCBaseline::N_Routes] = {
      MBaseline::JNAT,    	MBaseline::ITRF,
      MBaseline::J2000,    	MBaseline::B1950,
      MBaseline::GALACTIC, 	MBaseline::GALACTIC,
      MBaseline::B1950,    	MBaseline::J2000,
      MBaseline::JMEAN,    	MBaseline::BMEAN,
      MBaseline::J2000,    	MBaseline::JTRUE,
      MBaseline::B1950,    	MBaseline::BTRUE,
      MBaseline::JMEAN,		MBaseline::BMEAN,    	
      MBaseline::JNAT,      	MBaseline::J2000,
      MBaseline::APP,      	MBaseline::B1950,
      MBaseline::HADEC,    	MBaseline::AZEL,
      MBaseline::HADEC,    	MBaseline::APP,
      MBaseline::AZELSW,    	MBaseline::AZEL,
      MBaseline::JNAT,		MBaseline::APP,
      MBaseline::ECLIPTIC,	MBaseline::J2000,
      MBaseline::MECLIPTIC,	MBaseline::JMEAN,
      MBaseline::TECLIPTIC,	MBaseline::JTRUE,
      MBaseline::SUPERGAL,	MBaseline::GALACTIC,
      MBaseline::HADEC,		MBaseline::ITRF
    };
    
    Int iin  = inref.getType();
    Int iout = outref.getType();
    if (iin != iout) {
      Int tmp;
      while (iin != iout) {
	tmp = FromTo[iin][iout];
	iin = ToRef[tmp];
	mc.addMethod(tmp);
	initConvert(tmp, mc);
      };
    };
}

void MCBaseline::clearConvert() {
  delete ROTMAT1;    ROTMAT1 = 0;
  delete EULER1;     EULER1 = 0;
  delete MVPOS1;     MVPOS1 = 0;
  delete MVPOS2;     MVPOS2 = 0;
  delete MVPOS3;     MVPOS3 = 0;
  delete NUTATFROM;  NUTATFROM = 0;
  delete NUTATTO;    NUTATTO = 0;
  delete PRECESFROM; PRECESFROM = 0;
  delete PRECESTO;   PRECESTO = 0;
}

//# Conversion routines
void MCBaseline::initConvert(uInt which, MConvertBase &mc) {
  if (False) initConvert(which, mc);	// Stop warning
  if (!ROTMAT1) ROTMAT1 = new RotMatrix();
  if (!MVPOS1)  MVPOS1 = new MVPosition();
  if (!MVPOS2)  MVPOS2 = new MVPosition();
  if (!MVPOS3)  MVPOS3 = new MVPosition();
  if (!EULER1)  EULER1 = new Euler();
  
  switch (which) {
    
  case ITRF_JNAT:
    if (NUTATTO) delete NUTATTO;
    NUTATTO = new Nutation(Nutation::STANDARD);
    if (PRECESTO) delete PRECESTO;
    PRECESTO = new Precession(Precession::STANDARD);
    break;

  case JNAT_ITRF:
    if (NUTATFROM) delete NUTATFROM;
    NUTATFROM = new Nutation(Nutation::STANDARD);
    if (PRECESFROM) delete PRECESFROM;
    PRECESFROM = new Precession(Precession::STANDARD);
    break;

  case GAL_J2000:
    break;
    
  case GAL_B1950:
    break;
    
  case J2000_GAL:
    break;
    
  case B1950_GAL:
    break;
    
  case J2000_B1950:
    break;
    
  case B1950_J2000:
    break;
    
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
    break;
    
  case JNAT_APP:
    if (NUTATFROM) delete NUTATFROM;
    NUTATFROM = new Nutation(Nutation::STANDARD);
    if (PRECESFROM) delete PRECESFROM;
    PRECESFROM = new Precession(Precession::STANDARD);
    break;
    
  case JNAT_J2000:
    break;
    
  case APP_JNAT:
    if (NUTATTO) delete NUTATTO;
    NUTATTO = new Nutation(Nutation::STANDARD);
    if (PRECESTO) delete PRECESTO;
    PRECESTO = new Precession(Precession::STANDARD);
    break;
    
  case B1950_APP:
    if (NUTATFROM) delete NUTATFROM;
    NUTATFROM = new Nutation(Nutation::B1950);
    if (PRECESFROM) delete PRECESFROM;
    PRECESFROM = new Precession(Precession::B1950);
    break;
    
  case APP_B1950:
    if (NUTATTO) delete NUTATTO;
    NUTATTO = new Nutation(Nutation::B1950);
    if (PRECESTO) delete PRECESTO;
    PRECESTO = new Precession(Precession::B1950);
    break;
    
  case APP_HADEC:
    break;
    
  case HADEC_AZEL:
    break;
    
  case AZEL_HADEC:
    break;
    
  case HADEC_APP:
    break;
    
  case AZEL_AZELSW:
    break;
    
  case AZELSW_AZEL:
    break;
    
  case GAL_SUPERGAL:
    break;

  case SUPERGAL_GAL:
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
  Double g1, g2, g3, tdbTime;
  
  MCFrame::make(inref.getFrame());
  MCFrame::make(outref.getFrame());
  
  for (Int i=0; i<mc.nMethod(); i++) {
    
    switch (mc.getMethod(i)) {
      
    case ITRF_JNAT: {
      ((MCFrame *)(MBaseline::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      ((MCFrame *)(MBaseline::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getLASTr(g1);
      ((MCFrame *)(MBaseline::Ref::framePosition(inref, outref).
		   getMCFramePoint()))->
	getLong(g3);
      g1 += g3;
      *EULER1 = MeasTable::polarMotion(tdbTime);
      EULER1->operator()(2) = g1;
      in(1) = -in(1);
      *ROTMAT1 = RotMatrix(*EULER1);
      in = *ROTMAT1 * in;
      // Precession
      *ROTMAT1 = PRECESTO->operator()(tdbTime);
      // Nutation
      *ROTMAT1 *= NUTATTO->operator()(tdbTime);
      in = *ROTMAT1 * in;
    };
    break;
    
    case JNAT_ITRF: {
      ((MCFrame *)(MBaseline::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      ((MCFrame *)(MBaseline::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getLASTr(g1);
      ((MCFrame *)(MBaseline::Ref::framePosition(inref, outref).
		   getMCFramePoint()))->
	getLong(g3);
      g1 += g3;
      // Precession
      *ROTMAT1 = PRECESFROM->operator()(tdbTime);
      // Nutation
      *ROTMAT1 *= NUTATFROM->operator()(tdbTime);
     in *= *ROTMAT1;
      *EULER1 = MeasTable::polarMotion(tdbTime);
      EULER1->operator()(2) = g1;
      *ROTMAT1 = RotMatrix(*EULER1);
      in *= *ROTMAT1;
      in(1) = -in(1);
    };
    break;

    case HADEC_ITRF: {
      ((MCFrame *)(MBaseline::Ref::framePosition(inref, outref).
		   getMCFramePoint()))->
	getLong(g3);
      *ROTMAT1 = RotMatrix(Euler(g3, 3, 0, 0));
      in *= *ROTMAT1;
      in(1) = -in(1);
    };
    break;
    
    case ITRF_HADEC: {
      ((MCFrame *)(MBaseline::Ref::framePosition(inref, outref).
		   getMCFramePoint()))->
	getLong(g3);
      *ROTMAT1 = RotMatrix(Euler(g3, 3, 0, 0));
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
      in.adjust(g2);
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
      in.readjust(g2);
    }
    break;
    
    case B1950_J2000: {
      // E-terms
      *MVPOS1 = MeasTable::AberETerm(0);
      in.adjust(g2);
      g1 = in * *MVPOS1;
      in += g1 * in;
      in -= *MVPOS1;
      in.adjust();
      // Frame rotation
      *ROTMAT1 = MeasData::MToJ2000(0);
      in *= *ROTMAT1;
      in.readjust(g2);
    }	
    break;
    
    case J2000_JMEAN: {
      ((MCFrame *)(MBaseline::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Precession
      *ROTMAT1 = PRECESFROM->operator()(tdbTime);
      in *= *ROTMAT1;
    }
    break;
    
    case B1950_BMEAN: {
      ((MCFrame *)(MBaseline::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Precession
      *ROTMAT1 = PRECESFROM->operator()(tdbTime);
      in *= *ROTMAT1;
    }
    break;
    
    case JMEAN_J2000: {
      ((MCFrame *)(MBaseline::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Precession
      *ROTMAT1 = PRECESTO->operator()(tdbTime);
      in = *ROTMAT1 * in;
    }
    break;
    
    case JMEAN_JTRUE: {
      ((MCFrame *)(MBaseline::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Nutation
      *ROTMAT1 = NUTATFROM->operator()(tdbTime);
      in *= *ROTMAT1;
    }
    break;
    
    case BMEAN_B1950: {
      ((MCFrame *)(MBaseline::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Precession
      *ROTMAT1 = PRECESTO->operator()(tdbTime);
      in = *ROTMAT1 * in;
    }
    break;
    
    case BMEAN_BTRUE: {
      ((MCFrame *)(MBaseline::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Nutation
      *ROTMAT1 = NUTATFROM->operator()(tdbTime);
      in *= *ROTMAT1;
    }
    break;
    
    case JTRUE_JMEAN: {
      ((MCFrame *)(MBaseline::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Nutation
      *ROTMAT1 = NUTATTO->operator()(tdbTime);
      in = *ROTMAT1 * in;
    }
    break;
    
    case BTRUE_BMEAN: {
      ((MCFrame *)(MBaseline::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Nutation
      *ROTMAT1 = NUTATTO->operator()(tdbTime);
      in = *ROTMAT1 * in;
    }
    break;
    
    case J2000_JNAT: {
    }
    break;
    
    case JNAT_APP: {
      ((MCFrame *)(MBaseline::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Precession
      *ROTMAT1 = PRECESFROM->operator()(tdbTime);
      // Nutation
      *ROTMAT1 *= NUTATFROM->operator()(tdbTime);
      in *= *ROTMAT1;
    }
    break;
    
    case APP_JNAT: {
      ((MCFrame *)(MBaseline::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Precession
      *ROTMAT1 = PRECESTO->operator()(tdbTime);
      // Nutation
      *ROTMAT1 *= NUTATTO->operator()(tdbTime);
      in = *ROTMAT1 * in;
    }
    break;
    
    case JNAT_J2000: {
    }
    break;
    
    case B1950_APP: {
      ((MCFrame *)(MBaseline::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // E-terms
      *MVPOS1 = MeasTable::AberETerm(0);
      in.adjust(g2);
      g1 = in * *MVPOS1;
      in += g1 * in;
      in -= *MVPOS1;
      in.readjust(g2);
      // Precession
      *ROTMAT1 = PRECESFROM->operator()(tdbTime);
      // Nutation
      *ROTMAT1 *= NUTATFROM->operator()(tdbTime);
      in *= *ROTMAT1;
    }
    break;
    
    case APP_B1950: {
      ((MCFrame *)(MBaseline::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Precession
      *ROTMAT1 = PRECESTO->operator()(tdbTime);
      // Nutation
      *ROTMAT1 *= NUTATTO->operator()(tdbTime);
      in = *ROTMAT1 * in;
      // E-terms
      // Iterate
      *MVPOS1 = MeasTable::AberETerm(0);
      in.adjust(g2);
      *MVPOS2 = in;
      do {
	g1 = *MVPOS2 * *MVPOS1;
	*MVPOS3 = *MVPOS2 - *MVPOS1 + (g1 * *MVPOS2);
	MVPOS3->adjust();
	*MVPOS3 -= in;
	*MVPOS2 -= *MVPOS3;
      } while (MVPOS3->radius() > 1e-10);
      in = *MVPOS2;
      in.readjust(g2);
    }
    break;
    
    case APP_HADEC: {
      ((MCFrame *)(MBaseline::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getLASTr(g1);
      ((MCFrame *)(MBaseline::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      *EULER1 = MeasTable::polarMotion(tdbTime);
      EULER1->operator()(2) = g1;
      *ROTMAT1 = RotMatrix(*EULER1);
      in *= *ROTMAT1;
      in(1) = -in(1);
    }
    break;
    
    case HADEC_AZEL:
      ((MCFrame *)(MBaseline::Ref::framePosition(outref, inref).
		   getMCFramePoint()))->
	getLat(g1);
    *ROTMAT1 = RotMatrix(Euler(C::pi_2-g1 ,(uInt) 2,
			       C::pi, (uInt) 3));
    in *= *ROTMAT1;
    break;
    
    case AZEL_HADEC:
      ((MCFrame *)(MBaseline::Ref::framePosition(inref, outref).
		   getMCFramePoint()))->
	getLat(g1);
    *ROTMAT1 = RotMatrix(Euler(C::pi_2-g1 ,(uInt) 2,
			       C::pi, (uInt) 3));
    in = *ROTMAT1 * in;
    break;
    
    case HADEC_APP: {
      ((MCFrame *)(MBaseline::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getLASTr(g1);
      ((MCFrame *)(MBaseline::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      in(1) = -in(1);
      *EULER1 = MeasTable::polarMotion(tdbTime);
      EULER1->operator()(2) = g1;
      *ROTMAT1 = RotMatrix(*EULER1);
      in = *ROTMAT1 * in;
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
      ((MCFrame *)(MBaseline::Ref::frameEpoch(outref, inref).
                   getMCFramePoint()))->
        getTDB(tdbTime);
      *ROTMAT1 = 
	RotMatrix(Euler(MeasTable::fundArg(0)((tdbTime - 
					       MeasData::MJD2000)/
					      MeasData::JDCEN), 1, 0, 0));
      in = *ROTMAT1 * in;
      break;

    case JMEAN_MECLIP:
      ((MCFrame *)(MBaseline::Ref::frameEpoch(outref, inref).
                   getMCFramePoint()))->
        getTDB(tdbTime);
      *ROTMAT1 =
        RotMatrix(Euler(MeasTable::fundArg(0)((tdbTime -
                                               MeasData::MJD2000)/
                                              MeasData::JDCEN), 1, 0, 0));
      in *= *ROTMAT1;
      break;

    case TECLIP_JTRUE:
      ((MCFrame *)(MBaseline::Ref::frameEpoch(outref, inref).
                   getMCFramePoint()))->
        getTDB(tdbTime);
      *ROTMAT1 = 
	RotMatrix(Euler(-Nutation(Nutation::STANDARD)(tdbTime)(2), 1, 0, 0));
      in = *ROTMAT1 * in;
      break;

    case JTRUE_TECLIP:
      ((MCFrame *)(MBaseline::Ref::frameEpoch(outref, inref).
                   getMCFramePoint()))->
        getTDB(tdbTime);
      *ROTMAT1 =
        RotMatrix(Euler(-Nutation(Nutation::STANDARD)(tdbTime)(2), 1, 0, 0));
      in *= *ROTMAT1;
      break;

    case GAL_SUPERGAL:
      *ROTMAT1 = MeasTable::galToSupergal();
      in = *ROTMAT1 * in;
      break;
      
    case SUPERGAL_GAL:
      *ROTMAT1 = MeasTable::galToSupergal();
      in *= *ROTMAT1;
      break;
    
    default:
      break;
      
    };	// switch
  };	// for
}
