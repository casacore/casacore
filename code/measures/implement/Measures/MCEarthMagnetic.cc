//# MCEarthMagnetic.cc:  MEarthMagnetic conversion routines 
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
#include <aips/Quanta/Quantum.h>
typedef Quantum<Double> gpp_EarthMagnetic_bug1;
#endif
#include <aips/Exceptions.h>
#include <aips/Measures/MCEarthMagnetic.h>
#include <aips/Measures/EarthField.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Utilities/Assert.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Measures/MCFrame.h>
#include <aips/Quanta/RotMatrix.h>
#include <aips/Quanta/MVPosition.h>
#include <aips/Measures/Precession.h>
#include <aips/Measures/Nutation.h>
#include <aips/Measures/MeasTable.h>

//# Constructors
MCEarthMagnetic::MCEarthMagnetic() :
  ROTMAT1(0),
  EULER1(0),
  MVPOS1(0), MVPOS2(0), MVPOS3(0),
  NUTATFROM(0), NUTATTO(0),
  PRECESFROM(0), PRECESTO(0), EFIELD(0) {}

//# Destructor
MCEarthMagnetic::~MCEarthMagnetic() {
  clearConvert();
}

//# Operators

//# Member functions

void MCEarthMagnetic::getConvert(MConvertBase &mc,
			     const MRBase &inref, 
			     const MRBase &outref) {
  
  // Array of conversion routines to call
  static const MCEarthMagnetic::Routes 
    FromTo[MEarthMagnetic::N_Types][MEarthMagnetic::N_Types] = {
      { MCEarthMagnetic::N_Routes,
	MCEarthMagnetic::ITRF_JNAT,
	MCEarthMagnetic::ITRF_JNAT,
	MCEarthMagnetic::ITRF_JNAT,
	MCEarthMagnetic::ITRF_JNAT,
	MCEarthMagnetic::ITRF_JNAT,
	MCEarthMagnetic::ITRF_JNAT,
	MCEarthMagnetic::ITRF_JNAT,
	MCEarthMagnetic::ITRF_JNAT,
	MCEarthMagnetic::ITRF_HADEC,
	MCEarthMagnetic::ITRF_HADEC,
	MCEarthMagnetic::ITRF_HADEC,
	MCEarthMagnetic::ITRF_JNAT,
	MCEarthMagnetic::ITRF_JNAT,
	MCEarthMagnetic::ITRF_JNAT,
	MCEarthMagnetic::ITRF_JNAT,
	MCEarthMagnetic::ITRF_JNAT },
      { MCEarthMagnetic::J2000_JNAT,
	MCEarthMagnetic::N_Routes,
	MCEarthMagnetic::J2000_JMEAN,
	MCEarthMagnetic::J2000_JMEAN,
	MCEarthMagnetic::J2000_JNAT, 
	MCEarthMagnetic::J2000_B1950,
	MCEarthMagnetic::J2000_B1950, 
	MCEarthMagnetic::J2000_B1950,
	MCEarthMagnetic::J2000_GAL,
	MCEarthMagnetic::J2000_JNAT,
	MCEarthMagnetic::J2000_JNAT,
	MCEarthMagnetic::J2000_JNAT,
	MCEarthMagnetic::J2000_JNAT,
	MCEarthMagnetic::J2000_ECLIP,
	MCEarthMagnetic::J2000_JMEAN,
	MCEarthMagnetic::J2000_JMEAN,
	MCEarthMagnetic::J2000_GAL },
      { MCEarthMagnetic::JMEAN_J2000, 
	MCEarthMagnetic::JMEAN_J2000, 
	MCEarthMagnetic::N_Routes,    
	MCEarthMagnetic::JMEAN_JTRUE,
	MCEarthMagnetic::JMEAN_J2000, 
	MCEarthMagnetic::JMEAN_J2000, 
	MCEarthMagnetic::JMEAN_J2000, 
	MCEarthMagnetic::JMEAN_J2000,
	MCEarthMagnetic::JMEAN_J2000,
	MCEarthMagnetic::JMEAN_J2000, 
	MCEarthMagnetic::JMEAN_J2000, 
	MCEarthMagnetic::JMEAN_J2000, 
	MCEarthMagnetic::JMEAN_J2000,
	MCEarthMagnetic::JMEAN_J2000,
	MCEarthMagnetic::JMEAN_MECLIP,
	MCEarthMagnetic::JMEAN_JTRUE,
	MCEarthMagnetic::JMEAN_J2000 },
      { MCEarthMagnetic::JTRUE_JMEAN, 
	MCEarthMagnetic::JTRUE_JMEAN, 
	MCEarthMagnetic::JTRUE_JMEAN, 
	MCEarthMagnetic::N_Routes,
	MCEarthMagnetic::JTRUE_JMEAN,  
	MCEarthMagnetic::JTRUE_JMEAN, 
	MCEarthMagnetic::JTRUE_JMEAN, 
	MCEarthMagnetic::JTRUE_JMEAN,
	MCEarthMagnetic::JTRUE_JMEAN,
	MCEarthMagnetic::JTRUE_JMEAN,  
	MCEarthMagnetic::JTRUE_JMEAN,  
	MCEarthMagnetic::JTRUE_JMEAN,  
	MCEarthMagnetic::JTRUE_JMEAN,
	MCEarthMagnetic::JTRUE_JMEAN,
	MCEarthMagnetic::JTRUE_JMEAN,
	MCEarthMagnetic::JTRUE_TECLIP,
	MCEarthMagnetic::JTRUE_JMEAN },
      { MCEarthMagnetic::APP_JNAT,  
	MCEarthMagnetic::APP_JNAT,  
	MCEarthMagnetic::APP_JNAT,  
	MCEarthMagnetic::APP_JNAT,  
	MCEarthMagnetic::N_Routes,    
	MCEarthMagnetic::APP_B1950,  
	MCEarthMagnetic::APP_B1950,  
	MCEarthMagnetic::APP_B1950,
	MCEarthMagnetic::APP_JNAT,
	MCEarthMagnetic::APP_HADEC, 
	MCEarthMagnetic::APP_HADEC, 
	MCEarthMagnetic::APP_HADEC,
	MCEarthMagnetic::APP_JNAT,
	MCEarthMagnetic::APP_JNAT,
	MCEarthMagnetic::APP_JNAT,
	MCEarthMagnetic::APP_JNAT,
	MCEarthMagnetic::APP_JNAT },
      { MCEarthMagnetic::B1950_J2000, 
	MCEarthMagnetic::B1950_J2000, 
	MCEarthMagnetic::B1950_J2000, 
	MCEarthMagnetic::B1950_J2000, 
	MCEarthMagnetic::B1950_APP, 
	MCEarthMagnetic::N_Routes,    
	MCEarthMagnetic::B1950_BMEAN,
	MCEarthMagnetic::B1950_BMEAN,
	MCEarthMagnetic::B1950_GAL,
	MCEarthMagnetic::B1950_APP, 
	MCEarthMagnetic::B1950_APP, 
	MCEarthMagnetic::B1950_APP,
	MCEarthMagnetic::B1950_J2000,
	MCEarthMagnetic::B1950_J2000,
	MCEarthMagnetic::B1950_J2000,
	MCEarthMagnetic::B1950_J2000,
	MCEarthMagnetic::B1950_GAL },
      { MCEarthMagnetic::BMEAN_B1950, 
	MCEarthMagnetic::BMEAN_B1950, 
	MCEarthMagnetic::BMEAN_B1950,
	MCEarthMagnetic::BMEAN_B1950,
	MCEarthMagnetic::BMEAN_B1950, 
	MCEarthMagnetic::BMEAN_B1950, 
	MCEarthMagnetic::N_Routes,
	MCEarthMagnetic::BMEAN_BTRUE,
	MCEarthMagnetic::BMEAN_B1950,
	MCEarthMagnetic::BMEAN_B1950, 
	MCEarthMagnetic::BMEAN_B1950, 
	MCEarthMagnetic::BMEAN_B1950, 
	MCEarthMagnetic::BMEAN_B1950,
	MCEarthMagnetic::BMEAN_B1950,
	MCEarthMagnetic::BMEAN_B1950,
	MCEarthMagnetic::BMEAN_B1950,
	MCEarthMagnetic::BMEAN_B1950 },
      { MCEarthMagnetic::BTRUE_BMEAN, 
	MCEarthMagnetic::BTRUE_BMEAN, 
	MCEarthMagnetic::BTRUE_BMEAN,
	MCEarthMagnetic::BTRUE_BMEAN, 
	MCEarthMagnetic::BTRUE_BMEAN, 
	MCEarthMagnetic::BTRUE_BMEAN, 
	MCEarthMagnetic::BTRUE_BMEAN, 
	MCEarthMagnetic::N_Routes,   
	MCEarthMagnetic::BTRUE_BMEAN,
	MCEarthMagnetic::BTRUE_BMEAN, 
	MCEarthMagnetic::BTRUE_BMEAN, 
	MCEarthMagnetic::BTRUE_BMEAN, 
	MCEarthMagnetic::BTRUE_BMEAN,
	MCEarthMagnetic::BTRUE_BMEAN,
	MCEarthMagnetic::BTRUE_BMEAN,
	MCEarthMagnetic::BTRUE_BMEAN,
	MCEarthMagnetic::BTRUE_BMEAN },
      { MCEarthMagnetic::GAL_J2000,   
	MCEarthMagnetic::GAL_J2000,   
	MCEarthMagnetic::GAL_J2000, 
	MCEarthMagnetic::GAL_J2000,   
	MCEarthMagnetic::GAL_J2000,   
	MCEarthMagnetic::GAL_B1950,   
	MCEarthMagnetic::GAL_B1950,
	MCEarthMagnetic::GAL_B1950,
	MCEarthMagnetic::N_Routes,
	MCEarthMagnetic::GAL_J2000,   
	MCEarthMagnetic::GAL_J2000,   
	MCEarthMagnetic::GAL_J2000,   
	MCEarthMagnetic::GAL_J2000,
	MCEarthMagnetic::GAL_J2000,
	MCEarthMagnetic::GAL_J2000,
	MCEarthMagnetic::GAL_J2000,
	MCEarthMagnetic::GAL_SUPERGAL },
      { MCEarthMagnetic::HADEC_ITRF,   
	MCEarthMagnetic::HADEC_APP,   
	MCEarthMagnetic::HADEC_APP, 
	MCEarthMagnetic::HADEC_APP,   
	MCEarthMagnetic::HADEC_APP,   
	MCEarthMagnetic::HADEC_APP,   
	MCEarthMagnetic::HADEC_APP,
	MCEarthMagnetic::HADEC_APP,
	MCEarthMagnetic::HADEC_APP,
	MCEarthMagnetic::N_Routes,    
	MCEarthMagnetic::HADEC_AZEL,
	MCEarthMagnetic::HADEC_AZEL,
	MCEarthMagnetic::HADEC_APP,
	MCEarthMagnetic::HADEC_APP,
	MCEarthMagnetic::HADEC_APP,
	MCEarthMagnetic::HADEC_APP,
	MCEarthMagnetic::HADEC_APP },
      { MCEarthMagnetic::AZEL_HADEC,  
	MCEarthMagnetic::AZEL_HADEC,  
	MCEarthMagnetic::AZEL_HADEC,  
	MCEarthMagnetic::AZEL_HADEC,  
	MCEarthMagnetic::AZEL_HADEC,  
	MCEarthMagnetic::AZEL_HADEC,  
	MCEarthMagnetic::AZEL_HADEC,  
	MCEarthMagnetic::AZEL_HADEC,  
	MCEarthMagnetic::AZEL_HADEC,  
	MCEarthMagnetic::AZEL_HADEC,  
	MCEarthMagnetic::N_Routes,
	MCEarthMagnetic::AZEL_AZELSW,
	MCEarthMagnetic::AZEL_HADEC,
	MCEarthMagnetic::AZEL_HADEC,
	MCEarthMagnetic::AZEL_HADEC,
	MCEarthMagnetic::AZEL_HADEC,
	MCEarthMagnetic::AZEL_HADEC },  
      { MCEarthMagnetic::AZELSW_AZEL,
	MCEarthMagnetic::AZELSW_AZEL,
	MCEarthMagnetic::AZELSW_AZEL,
	MCEarthMagnetic::AZELSW_AZEL,
	MCEarthMagnetic::AZELSW_AZEL,
	MCEarthMagnetic::AZELSW_AZEL,
	MCEarthMagnetic::AZELSW_AZEL,
	MCEarthMagnetic::AZELSW_AZEL,
	MCEarthMagnetic::AZELSW_AZEL,
	MCEarthMagnetic::AZELSW_AZEL,
	MCEarthMagnetic::AZELSW_AZEL,
	MCEarthMagnetic::N_Routes,
	MCEarthMagnetic::AZELSW_AZEL,
	MCEarthMagnetic::AZELSW_AZEL,
	MCEarthMagnetic::AZELSW_AZEL,
	MCEarthMagnetic::AZELSW_AZEL,
	MCEarthMagnetic::AZELSW_AZEL },
      { MCEarthMagnetic::JNAT_ITRF,
	MCEarthMagnetic::JNAT_J2000,
	MCEarthMagnetic::JNAT_J2000,
	MCEarthMagnetic::JNAT_J2000,
	MCEarthMagnetic::JNAT_APP,
	MCEarthMagnetic::JNAT_J2000,
	MCEarthMagnetic::JNAT_J2000,
	MCEarthMagnetic::JNAT_J2000,
	MCEarthMagnetic::JNAT_J2000,
	MCEarthMagnetic::JNAT_APP,
	MCEarthMagnetic::JNAT_APP,
	MCEarthMagnetic::JNAT_APP,
	MCEarthMagnetic::N_Routes,
	MCEarthMagnetic::JNAT_J2000,
	MCEarthMagnetic::JNAT_J2000,
	MCEarthMagnetic::JNAT_J2000,
	MCEarthMagnetic::JNAT_J2000 },
      { MCEarthMagnetic::ECLIP_J2000,
	MCEarthMagnetic::ECLIP_J2000,
	MCEarthMagnetic::ECLIP_J2000,
	MCEarthMagnetic::ECLIP_J2000,
	MCEarthMagnetic::ECLIP_J2000,
	MCEarthMagnetic::ECLIP_J2000,
	MCEarthMagnetic::ECLIP_J2000,
	MCEarthMagnetic::ECLIP_J2000,
	MCEarthMagnetic::ECLIP_J2000,
	MCEarthMagnetic::ECLIP_J2000,
	MCEarthMagnetic::ECLIP_J2000,
	MCEarthMagnetic::ECLIP_J2000,
	MCEarthMagnetic::ECLIP_J2000,
	MCEarthMagnetic::N_Routes,
	MCEarthMagnetic::ECLIP_J2000,
	MCEarthMagnetic::ECLIP_J2000,
	MCEarthMagnetic::ECLIP_J2000 },
      { MCEarthMagnetic::MECLIP_JMEAN,
	MCEarthMagnetic::MECLIP_JMEAN,
	MCEarthMagnetic::MECLIP_JMEAN,
	MCEarthMagnetic::MECLIP_JMEAN,
	MCEarthMagnetic::MECLIP_JMEAN,
	MCEarthMagnetic::MECLIP_JMEAN,
	MCEarthMagnetic::MECLIP_JMEAN,
	MCEarthMagnetic::MECLIP_JMEAN,
	MCEarthMagnetic::MECLIP_JMEAN,
	MCEarthMagnetic::MECLIP_JMEAN,
	MCEarthMagnetic::MECLIP_JMEAN,
	MCEarthMagnetic::MECLIP_JMEAN,
	MCEarthMagnetic::MECLIP_JMEAN,
	MCEarthMagnetic::MECLIP_JMEAN,
	MCEarthMagnetic::N_Routes,
	MCEarthMagnetic::MECLIP_JMEAN,
	MCEarthMagnetic::MECLIP_JMEAN },
      { MCEarthMagnetic::TECLIP_JTRUE,
	MCEarthMagnetic::TECLIP_JTRUE,
	MCEarthMagnetic::TECLIP_JTRUE,
	MCEarthMagnetic::TECLIP_JTRUE,
	MCEarthMagnetic::TECLIP_JTRUE,
	MCEarthMagnetic::TECLIP_JTRUE,
	MCEarthMagnetic::TECLIP_JTRUE,
	MCEarthMagnetic::TECLIP_JTRUE,
	MCEarthMagnetic::TECLIP_JTRUE,
	MCEarthMagnetic::TECLIP_JTRUE,
	MCEarthMagnetic::TECLIP_JTRUE,
	MCEarthMagnetic::TECLIP_JTRUE,
	MCEarthMagnetic::TECLIP_JTRUE,
	MCEarthMagnetic::TECLIP_JTRUE,
	MCEarthMagnetic::TECLIP_JTRUE,
	MCEarthMagnetic::N_Routes,
	MCEarthMagnetic::TECLIP_JTRUE },
      { MCEarthMagnetic::SUPERGAL_GAL,
	MCEarthMagnetic::SUPERGAL_GAL,
	MCEarthMagnetic::SUPERGAL_GAL,
	MCEarthMagnetic::SUPERGAL_GAL,
	MCEarthMagnetic::SUPERGAL_GAL,
	MCEarthMagnetic::SUPERGAL_GAL,
	MCEarthMagnetic::SUPERGAL_GAL,
	MCEarthMagnetic::SUPERGAL_GAL,
	MCEarthMagnetic::SUPERGAL_GAL,
	MCEarthMagnetic::SUPERGAL_GAL,
	MCEarthMagnetic::SUPERGAL_GAL,
	MCEarthMagnetic::SUPERGAL_GAL,
	MCEarthMagnetic::SUPERGAL_GAL,
	MCEarthMagnetic::SUPERGAL_GAL,
	MCEarthMagnetic::SUPERGAL_GAL,
	MCEarthMagnetic::SUPERGAL_GAL,
	MCEarthMagnetic::N_Routes }
    };
    
    // List of codes converted to
    static const MEarthMagnetic::Types ToRef[MCEarthMagnetic::N_Routes] = {
      MEarthMagnetic::JNAT,    		MEarthMagnetic::ITRF,
      MEarthMagnetic::J2000,    	MEarthMagnetic::B1950,
      MEarthMagnetic::GALACTIC, 	MEarthMagnetic::GALACTIC,
      MEarthMagnetic::B1950,    	MEarthMagnetic::J2000,
      MEarthMagnetic::JMEAN,    	MEarthMagnetic::BMEAN,
      MEarthMagnetic::J2000,    	MEarthMagnetic::JTRUE,
      MEarthMagnetic::B1950,    	MEarthMagnetic::BTRUE,
      MEarthMagnetic::JMEAN,		MEarthMagnetic::BMEAN,    	
      MEarthMagnetic::JNAT,      	MEarthMagnetic::J2000,
      MEarthMagnetic::APP,      	MEarthMagnetic::B1950,
      MEarthMagnetic::HADEC,    	MEarthMagnetic::AZEL,
      MEarthMagnetic::HADEC,    	MEarthMagnetic::APP,
      MEarthMagnetic::AZELSW,    	MEarthMagnetic::AZEL,
      MEarthMagnetic::JNAT,		MEarthMagnetic::APP,
      MEarthMagnetic::ECLIPTIC,		MEarthMagnetic::J2000,
      MEarthMagnetic::MECLIPTIC,	MEarthMagnetic::JMEAN,
      MEarthMagnetic::TECLIPTIC,	MEarthMagnetic::JTRUE,
      MEarthMagnetic::SUPERGAL,		MEarthMagnetic::GALACTIC,
      MEarthMagnetic::HADEC,		MEarthMagnetic::ITRF
    };
    
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
	tmp = FromTo[iin][iout];
	iin = ToRef[tmp];
	mc.addMethod(tmp);
	initConvert(tmp, mc);
      };
    };
}

void MCEarthMagnetic::clearConvert() {
  delete ROTMAT1;    ROTMAT1 = 0;
  delete EULER1;     EULER1 = 0;
  delete MVPOS1;     MVPOS1 = 0;
  delete MVPOS2;     MVPOS2 = 0;
  delete MVPOS3;     MVPOS3 = 0;
  delete NUTATFROM;  NUTATFROM = 0;
  delete NUTATTO;    NUTATTO = 0;
  delete PRECESFROM; PRECESFROM = 0;
  delete PRECESTO;   PRECESTO = 0;
  delete EFIELD;     EFIELD=0;
}

//# Conversion routines
void MCEarthMagnetic::initConvert(uInt which, MConvertBase &mc) {
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
    
  case JNAT_APP:
    if (NUTATFROM) delete NUTATFROM;
    NUTATFROM = new Nutation(Nutation::STANDARD);
    if (PRECESFROM) delete PRECESFROM;
    PRECESFROM = new Precession(Precession::STANDARD);
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
  Double g1, g2, g3, tdbTime;
  EarthField::EarthFieldTypes modID;

  MCFrame::make(inref.getFrame());
  MCFrame::make(outref.getFrame());
  
  for (Int i=0; i<mc.nMethod(); i++) {
    
    switch (mc.getMethod(i)) {
      
    case ITRF_JNAT: {
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getLASTr(g1);
      ((MCFrame *)(MEarthMagnetic::Ref::framePosition(inref, outref).
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
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getLASTr(g1);
      ((MCFrame *)(MEarthMagnetic::Ref::framePosition(inref, outref).
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
      ((MCFrame *)(MEarthMagnetic::Ref::framePosition(inref, outref).
		   getMCFramePoint()))->
	getLong(g3);
      *ROTMAT1 = RotMatrix(Euler(g3, 3, 0, 0));
      in *= *ROTMAT1;
      in(1) = -in(1);
    };
    break;
    
    case ITRF_HADEC: {
      ((MCFrame *)(MEarthMagnetic::Ref::framePosition(inref, outref).
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
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Precession
      *ROTMAT1 = PRECESFROM->operator()(tdbTime);
      in *= *ROTMAT1;
    }
    break;
    
    case B1950_BMEAN: {
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Precession
      *ROTMAT1 = PRECESFROM->operator()(tdbTime);
      in *= *ROTMAT1;
    }
    break;
    
    case JMEAN_J2000: {
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Precession
      *ROTMAT1 = PRECESTO->operator()(tdbTime);
      in = *ROTMAT1 * in;
    }
    break;
    
    case JMEAN_JTRUE: {
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Nutation
      *ROTMAT1 = NUTATFROM->operator()(tdbTime);
      in *= *ROTMAT1;
    }
    break;
    
    case BMEAN_B1950: {
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Precession
      *ROTMAT1 = PRECESTO->operator()(tdbTime);
      in = *ROTMAT1 * in;
    }
    break;
    
    case BMEAN_BTRUE: {
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Nutation
      *ROTMAT1 = NUTATFROM->operator()(tdbTime);
      in *= *ROTMAT1;
    }
    break;
    
    case JTRUE_JMEAN: {
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      // Nutation
      *ROTMAT1 = NUTATTO->operator()(tdbTime);
      in = *ROTMAT1 * in;
    }
    break;
    
    case BTRUE_BMEAN: {
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(outref, inref).
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
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(outref, inref).
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
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(inref, outref).
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
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(outref, inref).
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
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(inref, outref).
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
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getLASTr(g1);
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(outref, inref).
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
      ((MCFrame *)(MEarthMagnetic::Ref::framePosition(outref, inref).
		   getMCFramePoint()))->
	getLat(g1);
    *ROTMAT1 = RotMatrix(Euler(C::pi_2-g1 ,(uInt) 2,
			       C::pi, (uInt) 3));
    in *= *ROTMAT1;
    break;
    
    case AZEL_HADEC:
      ((MCFrame *)(MEarthMagnetic::Ref::framePosition(inref, outref).
		   getMCFramePoint()))->
	getLat(g1);
    *ROTMAT1 = RotMatrix(Euler(C::pi_2-g1 ,(uInt) 2,
			       C::pi, (uInt) 3));
    in = *ROTMAT1 * in;
    break;
    
    case HADEC_APP: {
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(inref, outref).
		   getMCFramePoint()))->
	getLASTr(g1);
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(inref, outref).
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
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(outref, inref).
                   getMCFramePoint()))->
        getTDB(tdbTime);
      *ROTMAT1 = 
	RotMatrix(Euler(MeasTable::fundArg(0)((tdbTime - 
					       MeasData::MJD2000)/
					      MeasData::JDCEN), 1, 0, 0));
      in = *ROTMAT1 * in;
      break;

    case JMEAN_MECLIP:
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(outref, inref).
                   getMCFramePoint()))->
        getTDB(tdbTime);
      *ROTMAT1 =
        RotMatrix(Euler(MeasTable::fundArg(0)((tdbTime -
                                               MeasData::MJD2000)/
                                              MeasData::JDCEN), 1, 0, 0));
      in *= *ROTMAT1;
      break;

    case TECLIP_JTRUE:
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(outref, inref).
                   getMCFramePoint()))->
        getTDB(tdbTime);
      *ROTMAT1 = 
	RotMatrix(Euler(-Nutation(Nutation::STANDARD)(tdbTime)(2), 1, 0, 0));
      in = *ROTMAT1 * in;
      break;

    case JTRUE_TECLIP:
      ((MCFrame *)(MEarthMagnetic::Ref::frameEpoch(outref, inref).
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
