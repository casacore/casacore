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
#include <aips/Exceptions.h>
#include <aips/Measures/MCuvw.h>
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

//# Statics
Bool MCuvw::stateMade_p = False;
uInt MCuvw::ToRef_p[N_Routes][3] = {
    {Muvw::ITRF,	Muvw::JNAT,		0},
    {Muvw::JNAT,	Muvw::ITRF,		0},
    {Muvw::GALACTIC,	Muvw::J2000,		0},
    {Muvw::GALACTIC,	Muvw::B1950,		3},
    {Muvw::J2000,	Muvw::GALACTIC,		0},
    {Muvw::B1950,	Muvw::GALACTIC,		3},
    {Muvw::J2000,	Muvw::B1950,		3},
    {Muvw::B1950,	Muvw::J2000,		3},
    {Muvw::J2000,	Muvw::JMEAN,		0},
    {Muvw::B1950,	Muvw::BMEAN,		3},
    {Muvw::JMEAN,	Muvw::J2000,		0},
    {Muvw::JMEAN,	Muvw::JTRUE,		0},
    {Muvw::BMEAN,	Muvw::B1950,		3},
    {Muvw::BMEAN,	Muvw::BTRUE,		3},
    {Muvw::JTRUE,	Muvw::JMEAN,		0},
    {Muvw::BTRUE,	Muvw::BMEAN,		3},
    {Muvw::J2000,	Muvw::JNAT,		0},
    {Muvw::JNAT,	Muvw::J2000,		0},
    {Muvw::B1950,	Muvw::APP,		3},
    {Muvw::APP,	 	Muvw::B1950,		3},
    {Muvw::APP,		Muvw::HADEC,		2},
    {Muvw::HADEC,	Muvw::AZEL,		2},
    {Muvw::AZEL,	Muvw::HADEC,		2},
    {Muvw::HADEC,	Muvw::APP,		2},
    {Muvw::AZEL,	Muvw::AZELSW,		0},
    {Muvw::AZELSW,	Muvw::AZEL,		0},
    {Muvw::APP,		Muvw::JNAT,		1},
    {Muvw::JNAT,	Muvw::APP,		1},
    {Muvw::J2000,	Muvw::ECLIPTIC,		0},
    {Muvw::ECLIPTIC,	Muvw::J2000,		0},
    {Muvw::JMEAN,	Muvw::MECLIPTIC,	0},
    {Muvw::MECLIPTIC,	Muvw::JMEAN,		0},
    {Muvw::JTRUE,	Muvw::TECLIPTIC,	0},
    {Muvw::TECLIPTIC,	Muvw::JTRUE,		0},
    {Muvw::GALACTIC,	Muvw::SUPERGAL,		0},
    {Muvw::SUPERGAL,	Muvw::GALACTIC,		0},
    {Muvw::ITRF,	Muvw::HADEC,		2},
    {Muvw::HADEC,	Muvw::ITRF,		2} };
uInt MCuvw::FromTo_p[Muvw::N_Types][Muvw::N_Types];

//# Constructors
MCuvw::MCuvw() :
  ROTMAT1(0),
  EULER1(0),
  MVPOS1(0), MVPOS2(0), MVPOS3(0),
  NUTATFROM(0), NUTATTO(0),
  PRECESFROM(0), PRECESTO(0) {
  if (!stateMade_p) {
    MCBase::makeState(MCuvw::stateMade_p, MCuvw::FromTo_p[0],
		      Muvw::N_Types, MCuvw::N_Routes,
		      MCuvw::ToRef_p);
  };
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
}

//# Conversion routines
void MCuvw::initConvert(uInt which, MConvertBase &mc) {
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
    
  };
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
      ((MCFrame *)(Muvw::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getTDB(tdbTime);
      ((MCFrame *)(Muvw::Ref::frameEpoch(outref, inref).
		   getMCFramePoint()))->
	getLASTr(g1);
      ((MCFrame *)(Muvw::Ref::framePosition(inref, outref).
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
      ((MCFrame *)(Muvw::Ref::framePosition(inref, outref).
		   getMCFramePoint()))->
	getLong(g3);
      *ROTMAT1 = RotMatrix(Euler(g3, 3, 0, 0));
      in *= *ROTMAT1;
      in(1) = -in(1);
    };
    break;
    
    case ITRF_HADEC: {
      ((MCFrame *)(Muvw::Ref::framePosition(inref, outref).
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

String MCuvw::showState() {
  if (!stateMade_p) {
    MCBase::makeState(MCuvw::stateMade_p, MCuvw::FromTo_p[0],
		      Muvw::N_Types, MCuvw::N_Routes,
		      MCuvw::ToRef_p);
  };
  return MCBase::showState(MCuvw::stateMade_p, MCuvw::FromTo_p[0],
			   Muvw::N_Types, MCuvw::N_Routes,
			   MCuvw::ToRef_p);
}
