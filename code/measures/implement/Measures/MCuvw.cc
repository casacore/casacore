//# MCuvw.cc:  Muvw conversion routines 
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
typedef Quantum<Double> gpp_uvw_bug1;
#endif
#include <aips/Exceptions.h>
#include <trial/Measures/MCuvw.h>
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
MCuvw::MCuvw() :
  ROTMAT1(0),
  EULER1(0),
  MVPOS1(0), MVPOS2(0), MVPOS3(0),
  NUTATFROM(0), NUTATTO(0),
  PRECESFROM(0), PRECESTO(0),
  VEC61(0), VEC62(0), VEC63(0) {}

//# Destructor
MCuvw::~MCuvw() {
  clearConvert();
}

//# Operators

//# Member functions

void MCuvw::getConvert(MConvertBase &mc,
			     const MRBase &inref, 
			     const MRBase &outref) {
  
  // Array of conversion routines to call
  static const MCuvw::Routes 
    FromTo[Muvw::N_Types][Muvw::N_Types] = {
      { MCuvw::N_Routes,
	MCuvw::ITRF_JNAT,
	MCuvw::ITRF_JNAT,
	MCuvw::ITRF_JNAT,
	MCuvw::ITRF_JNAT,
	MCuvw::ITRF_JNAT,
	MCuvw::ITRF_JNAT,
	MCuvw::ITRF_JNAT,
	MCuvw::ITRF_JNAT,
	MCuvw::ITRF_JNAT,
	MCuvw::ITRF_JNAT,
	MCuvw::ITRF_JNAT,
	MCuvw::ITRF_JNAT,
	MCuvw::ITRF_JNAT,
	MCuvw::ITRF_JNAT,
	MCuvw::ITRF_JNAT,
	MCuvw::ITRF_JNAT },
      { MCuvw::J2000_JNAT,
	MCuvw::N_Routes,
	MCuvw::J2000_JMEAN,
	MCuvw::J2000_JMEAN,
	MCuvw::J2000_JNAT, 
	MCuvw::J2000_B1950,
	MCuvw::J2000_B1950, 
	MCuvw::J2000_B1950,
	MCuvw::J2000_GAL,
	MCuvw::J2000_JNAT,
	MCuvw::J2000_JNAT,
	MCuvw::J2000_JNAT,
	MCuvw::J2000_JNAT,
	MCuvw::J2000_ECLIP,
	MCuvw::J2000_JMEAN,
	MCuvw::J2000_JMEAN,
	MCuvw::J2000_GAL },
      { MCuvw::JMEAN_J2000, 
	MCuvw::JMEAN_J2000, 
	MCuvw::N_Routes,    
	MCuvw::JMEAN_JTRUE,
	MCuvw::JMEAN_J2000, 
	MCuvw::JMEAN_J2000, 
	MCuvw::JMEAN_J2000, 
	MCuvw::JMEAN_J2000,
	MCuvw::JMEAN_J2000,
	MCuvw::JMEAN_J2000, 
	MCuvw::JMEAN_J2000, 
	MCuvw::JMEAN_J2000, 
	MCuvw::JMEAN_J2000,
	MCuvw::JMEAN_J2000,
	MCuvw::JMEAN_MECLIP,
	MCuvw::JMEAN_JTRUE,
	MCuvw::JMEAN_J2000 },
      { MCuvw::JTRUE_JMEAN, 
	MCuvw::JTRUE_JMEAN, 
	MCuvw::JTRUE_JMEAN, 
	MCuvw::N_Routes,
	MCuvw::JTRUE_JMEAN,  
	MCuvw::JTRUE_JMEAN, 
	MCuvw::JTRUE_JMEAN, 
	MCuvw::JTRUE_JMEAN,
	MCuvw::JTRUE_JMEAN,
	MCuvw::JTRUE_JMEAN,  
	MCuvw::JTRUE_JMEAN,  
	MCuvw::JTRUE_JMEAN,  
	MCuvw::JTRUE_JMEAN,
	MCuvw::JTRUE_JMEAN,
	MCuvw::JTRUE_JMEAN,
	MCuvw::JTRUE_TECLIP,
	MCuvw::JTRUE_JMEAN },
      { MCuvw::APP_JNAT,  
	MCuvw::APP_JNAT,  
	MCuvw::APP_JNAT,  
	MCuvw::APP_JNAT,  
	MCuvw::N_Routes,    
	MCuvw::APP_B1950,  
	MCuvw::APP_B1950,  
	MCuvw::APP_B1950,
	MCuvw::APP_JNAT,
	MCuvw::APP_HADEC, 
	MCuvw::APP_HADEC, 
	MCuvw::APP_HADEC,
	MCuvw::APP_JNAT,
	MCuvw::APP_JNAT,
	MCuvw::APP_JNAT,
	MCuvw::APP_JNAT,
	MCuvw::APP_JNAT },
      { MCuvw::B1950_J2000, 
	MCuvw::B1950_J2000, 
	MCuvw::B1950_J2000, 
	MCuvw::B1950_J2000, 
	MCuvw::B1950_APP, 
	MCuvw::N_Routes,    
	MCuvw::B1950_BMEAN,
	MCuvw::B1950_BMEAN,
	MCuvw::B1950_GAL,
	MCuvw::B1950_APP, 
	MCuvw::B1950_APP, 
	MCuvw::B1950_APP,
	MCuvw::B1950_J2000,
	MCuvw::B1950_J2000,
	MCuvw::B1950_J2000,
	MCuvw::B1950_J2000,
	MCuvw::B1950_GAL },
      { MCuvw::BMEAN_B1950, 
	MCuvw::BMEAN_B1950, 
	MCuvw::BMEAN_B1950,
	MCuvw::BMEAN_B1950,
	MCuvw::BMEAN_B1950, 
	MCuvw::BMEAN_B1950, 
	MCuvw::N_Routes,
	MCuvw::BMEAN_BTRUE,
	MCuvw::BMEAN_B1950,
	MCuvw::BMEAN_B1950, 
	MCuvw::BMEAN_B1950, 
	MCuvw::BMEAN_B1950, 
	MCuvw::BMEAN_B1950,
	MCuvw::BMEAN_B1950,
	MCuvw::BMEAN_B1950,
	MCuvw::BMEAN_B1950,
	MCuvw::BMEAN_B1950 },
      { MCuvw::BTRUE_BMEAN, 
	MCuvw::BTRUE_BMEAN, 
	MCuvw::BTRUE_BMEAN,
	MCuvw::BTRUE_BMEAN, 
	MCuvw::BTRUE_BMEAN, 
	MCuvw::BTRUE_BMEAN, 
	MCuvw::BTRUE_BMEAN, 
	MCuvw::N_Routes,   
	MCuvw::BTRUE_BMEAN,
	MCuvw::BTRUE_BMEAN, 
	MCuvw::BTRUE_BMEAN, 
	MCuvw::BTRUE_BMEAN, 
	MCuvw::BTRUE_BMEAN,
	MCuvw::BTRUE_BMEAN,
	MCuvw::BTRUE_BMEAN,
	MCuvw::BTRUE_BMEAN,
	MCuvw::BTRUE_BMEAN },
      { MCuvw::GAL_J2000,   
	MCuvw::GAL_J2000,   
	MCuvw::GAL_J2000, 
	MCuvw::GAL_J2000,   
	MCuvw::GAL_J2000,   
	MCuvw::GAL_B1950,   
	MCuvw::GAL_B1950,
	MCuvw::GAL_B1950,
	MCuvw::N_Routes,
	MCuvw::GAL_J2000,   
	MCuvw::GAL_J2000,   
	MCuvw::GAL_J2000,   
	MCuvw::GAL_J2000,
	MCuvw::GAL_J2000,
	MCuvw::GAL_J2000,
	MCuvw::GAL_J2000,
	MCuvw::GAL_SUPERGAL },
      { MCuvw::HADEC_APP,   
	MCuvw::HADEC_APP,   
	MCuvw::HADEC_APP, 
	MCuvw::HADEC_APP,   
	MCuvw::HADEC_APP,   
	MCuvw::HADEC_APP,   
	MCuvw::HADEC_APP,
	MCuvw::HADEC_APP,
	MCuvw::HADEC_APP,
	MCuvw::N_Routes,    
	MCuvw::HADEC_AZEL,
	MCuvw::HADEC_AZEL,
	MCuvw::HADEC_APP,
	MCuvw::HADEC_APP,
	MCuvw::HADEC_APP,
	MCuvw::HADEC_APP,
	MCuvw::HADEC_APP },
      { MCuvw::AZEL_HADEC,  
	MCuvw::AZEL_HADEC,  
	MCuvw::AZEL_HADEC,  
	MCuvw::AZEL_HADEC,  
	MCuvw::AZEL_HADEC,  
	MCuvw::AZEL_HADEC,  
	MCuvw::AZEL_HADEC,  
	MCuvw::AZEL_HADEC,  
	MCuvw::AZEL_HADEC,  
	MCuvw::AZEL_HADEC,  
	MCuvw::N_Routes,
	MCuvw::AZEL_AZELSW,
	MCuvw::AZEL_HADEC,
	MCuvw::AZEL_HADEC,
	MCuvw::AZEL_HADEC,
	MCuvw::AZEL_HADEC,
	MCuvw::AZEL_HADEC },  
      { MCuvw::AZELSW_AZEL,
	MCuvw::AZELSW_AZEL,
	MCuvw::AZELSW_AZEL,
	MCuvw::AZELSW_AZEL,
	MCuvw::AZELSW_AZEL,
	MCuvw::AZELSW_AZEL,
	MCuvw::AZELSW_AZEL,
	MCuvw::AZELSW_AZEL,
	MCuvw::AZELSW_AZEL,
	MCuvw::AZELSW_AZEL,
	MCuvw::AZELSW_AZEL,
	MCuvw::N_Routes,
	MCuvw::AZELSW_AZEL,
	MCuvw::AZELSW_AZEL,
	MCuvw::AZELSW_AZEL,
	MCuvw::AZELSW_AZEL,
	MCuvw::AZELSW_AZEL },
      { MCuvw::JNAT_ITRF,
	MCuvw::JNAT_J2000,
	MCuvw::JNAT_J2000,
	MCuvw::JNAT_J2000,
	MCuvw::JNAT_APP,
	MCuvw::JNAT_J2000,
	MCuvw::JNAT_J2000,
	MCuvw::JNAT_J2000,
	MCuvw::JNAT_J2000,
	MCuvw::JNAT_APP,
	MCuvw::JNAT_APP,
	MCuvw::JNAT_APP,
	MCuvw::N_Routes,
	MCuvw::JNAT_J2000,
	MCuvw::JNAT_J2000,
	MCuvw::JNAT_J2000,
	MCuvw::JNAT_J2000 },
      { MCuvw::ECLIP_J2000,
	MCuvw::ECLIP_J2000,
	MCuvw::ECLIP_J2000,
	MCuvw::ECLIP_J2000,
	MCuvw::ECLIP_J2000,
	MCuvw::ECLIP_J2000,
	MCuvw::ECLIP_J2000,
	MCuvw::ECLIP_J2000,
	MCuvw::ECLIP_J2000,
	MCuvw::ECLIP_J2000,
	MCuvw::ECLIP_J2000,
	MCuvw::ECLIP_J2000,
	MCuvw::ECLIP_J2000,
	MCuvw::N_Routes,
	MCuvw::ECLIP_J2000,
	MCuvw::ECLIP_J2000,
	MCuvw::ECLIP_J2000 },
      { MCuvw::MECLIP_JMEAN,
	MCuvw::MECLIP_JMEAN,
	MCuvw::MECLIP_JMEAN,
	MCuvw::MECLIP_JMEAN,
	MCuvw::MECLIP_JMEAN,
	MCuvw::MECLIP_JMEAN,
	MCuvw::MECLIP_JMEAN,
	MCuvw::MECLIP_JMEAN,
	MCuvw::MECLIP_JMEAN,
	MCuvw::MECLIP_JMEAN,
	MCuvw::MECLIP_JMEAN,
	MCuvw::MECLIP_JMEAN,
	MCuvw::MECLIP_JMEAN,
	MCuvw::MECLIP_JMEAN,
	MCuvw::N_Routes,
	MCuvw::MECLIP_JMEAN,
	MCuvw::MECLIP_JMEAN },
      { MCuvw::TECLIP_JTRUE,
	MCuvw::TECLIP_JTRUE,
	MCuvw::TECLIP_JTRUE,
	MCuvw::TECLIP_JTRUE,
	MCuvw::TECLIP_JTRUE,
	MCuvw::TECLIP_JTRUE,
	MCuvw::TECLIP_JTRUE,
	MCuvw::TECLIP_JTRUE,
	MCuvw::TECLIP_JTRUE,
	MCuvw::TECLIP_JTRUE,
	MCuvw::TECLIP_JTRUE,
	MCuvw::TECLIP_JTRUE,
	MCuvw::TECLIP_JTRUE,
	MCuvw::TECLIP_JTRUE,
	MCuvw::TECLIP_JTRUE,
	MCuvw::N_Routes,
	MCuvw::TECLIP_JTRUE },
      { MCuvw::SUPERGAL_GAL,
	MCuvw::SUPERGAL_GAL,
	MCuvw::SUPERGAL_GAL,
	MCuvw::SUPERGAL_GAL,
	MCuvw::SUPERGAL_GAL,
	MCuvw::SUPERGAL_GAL,
	MCuvw::SUPERGAL_GAL,
	MCuvw::SUPERGAL_GAL,
	MCuvw::SUPERGAL_GAL,
	MCuvw::SUPERGAL_GAL,
	MCuvw::SUPERGAL_GAL,
	MCuvw::SUPERGAL_GAL,
	MCuvw::SUPERGAL_GAL,
	MCuvw::SUPERGAL_GAL,
	MCuvw::SUPERGAL_GAL,
	MCuvw::SUPERGAL_GAL,
	MCuvw::N_Routes }
    };
    
    // List of codes converted to
    static const Muvw::Types ToRef[MCuvw::N_Routes] = {
      Muvw::JNAT,    	Muvw::ITRF,
      Muvw::J2000,    	Muvw::B1950,
      Muvw::GALACTIC, 	Muvw::GALACTIC,
      Muvw::B1950,    	Muvw::J2000,
      Muvw::JMEAN,    	Muvw::BMEAN,
      Muvw::J2000,    	Muvw::JTRUE,
      Muvw::B1950,    	Muvw::BTRUE,
      Muvw::JMEAN,	Muvw::BMEAN,    	
      Muvw::JNAT,      	Muvw::J2000,
      Muvw::APP,      	Muvw::B1950,
      Muvw::HADEC,    	Muvw::AZEL,
      Muvw::HADEC,    	Muvw::APP,
      Muvw::AZELSW,    	Muvw::AZEL,
      Muvw::JNAT,	Muvw::APP,
      Muvw::ECLIPTIC,	Muvw::J2000,
      Muvw::MECLIPTIC,	Muvw::JMEAN,
      Muvw::TECLIPTIC,	Muvw::JTRUE,
      Muvw::SUPERGAL,	Muvw::GALACTIC
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

void MCuvw::clearConvert() {
  delete ROTMAT1;    ROTMAT1 = 0;
  delete EULER1;     EULER1 = 0;
  delete MVPOS1;     MVPOS1 = 0;
  delete MVPOS2;     MVPOS2 = 0;
  delete MVPOS3;     MVPOS3 = 0;
  delete NUTATFROM;  NUTATFROM = 0;
  delete NUTATTO;    NUTATTO = 0;
  delete PRECESFROM; PRECESFROM = 0;
  delete PRECESTO;   PRECESTO = 0;
  delete VEC61;	     VEC61 = 0;
  delete VEC62;	     VEC62 = 0;
  delete VEC63;	     VEC63 = 0;
}

//# Conversion routines
void MCuvw::initConvert(uInt which, MConvertBase &mc) {
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
  Double g1, g2, g3, tdbTime;
  
  MCFrame::make(inref.getFrame());
  MCFrame::make(outref.getFrame());
  
  for (Int i=0; i<mc.nMethod(); i++) {
    
    switch (mc.getMethod(i)) {
      
    case ITRF_JNAT: {
      ((MCFrame *)(Muvw::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      ((MCFrame *)(Muvw::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getLASTr(g1);
      ((MCFrame *)(Muvw::Ref::framePosition(inref, outref).
		   getMCFramePoint()))->
	getLong(g3);
      g1 -= g3;
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
      ((MCFrame *)(Muvw::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      ((MCFrame *)(Muvw::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getLASTr(g1);
      ((MCFrame *)(Muvw::Ref::framePosition(inref, outref).
		   getMCFramePoint()))->
	getLong(g3);
      g1 -= g3;
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
      ((MCFrame *)(Muvw::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Precession
      *ROTMAT1 = PRECESFROM->operator()(tdbTime);
      in *= *ROTMAT1;
    }
    break;
    
    case B1950_BMEAN: {
      ((MCFrame *)(Muvw::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Precession
      *ROTMAT1 = PRECESFROM->operator()(tdbTime);
      in *= *ROTMAT1;
    }
    break;
    
    case JMEAN_J2000: {
      ((MCFrame *)(Muvw::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Precession
      *ROTMAT1 = PRECESTO->operator()(tdbTime);
      in = *ROTMAT1 * in;
    }
    break;
    
    case JMEAN_JTRUE: {
      ((MCFrame *)(Muvw::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Nutation
      *ROTMAT1 = NUTATFROM->operator()(tdbTime);
      in *= *ROTMAT1;
    }
    break;
    
    case BMEAN_B1950: {
      ((MCFrame *)(Muvw::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Precession
      *ROTMAT1 = PRECESTO->operator()(tdbTime);
      in = *ROTMAT1 * in;
    }
    break;
    
    case BMEAN_BTRUE: {
      ((MCFrame *)(Muvw::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Nutation
      *ROTMAT1 = NUTATFROM->operator()(tdbTime);
      in *= *ROTMAT1;
    }
    break;
    
    case JTRUE_JMEAN: {
      ((MCFrame *)(Muvw::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Nutation
      *ROTMAT1 = NUTATTO->operator()(tdbTime);
      in = *ROTMAT1 * in;
    }
    break;
    
    case BTRUE_BMEAN: {
      ((MCFrame *)(Muvw::Ref::frameEpoch(outref, inref).
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
      ((MCFrame *)(Muvw::Ref::frameEpoch(outref, inref).
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
      ((MCFrame *)(Muvw::Ref::frameEpoch(inref, outref).
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
      ((MCFrame *)(Muvw::Ref::frameEpoch(outref, inref).
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
      ((MCFrame *)(Muvw::Ref::frameEpoch(inref, outref).
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
      ((MCFrame *)(Muvw::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getLASTr(g1);
      ((MCFrame *)(Muvw::Ref::frameEpoch(outref, inref).
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
      ((MCFrame *)(Muvw::Ref::framePosition(outref, inref).
		   getMCFramePoint()))->
	getLat(g1);
    *ROTMAT1 = RotMatrix(Euler(C::pi_2-g1 ,(uInt) 2,
			       C::pi, (uInt) 3));
    in *= *ROTMAT1;
    break;
    
    case AZEL_HADEC:
      ((MCFrame *)(Muvw::Ref::framePosition(inref, outref).
		   getMCFramePoint()))->
	getLat(g1);
    *ROTMAT1 = RotMatrix(Euler(C::pi_2-g1 ,(uInt) 2,
			       C::pi, (uInt) 3));
    in = *ROTMAT1 * in;
    break;
    
    case HADEC_APP: {
      ((MCFrame *)(Muvw::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getLASTr(g1);
      ((MCFrame *)(Muvw::Ref::frameEpoch(inref, outref).
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
      ((MCFrame *)(Muvw::Ref::frameEpoch(outref, inref).
                   getMCFramePoint()))->
        getTDB(tdbTime);
      *ROTMAT1 = 
	RotMatrix(Euler(MeasTable::fundArg(0)((tdbTime - 
					       MeasData::MJD2000)/
					      MeasData::JDCEN), 1, 0, 0));
      in = *ROTMAT1 * in;
      break;

    case JMEAN_MECLIP:
      ((MCFrame *)(Muvw::Ref::frameEpoch(outref, inref).
                   getMCFramePoint()))->
        getTDB(tdbTime);
      *ROTMAT1 =
        RotMatrix(Euler(MeasTable::fundArg(0)((tdbTime -
                                               MeasData::MJD2000)/
                                              MeasData::JDCEN), 1, 0, 0));
      in *= *ROTMAT1;
      break;

    case TECLIP_JTRUE:
      ((MCFrame *)(Muvw::Ref::frameEpoch(outref, inref).
                   getMCFramePoint()))->
        getTDB(tdbTime);
      *ROTMAT1 = 
	RotMatrix(Euler(-Nutation(Nutation::STANDARD)(tdbTime)(2), 1, 0, 0));
      in = *ROTMAT1 * in;
      break;

    case JTRUE_TECLIP:
      ((MCFrame *)(Muvw::Ref::frameEpoch(outref, inref).
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
