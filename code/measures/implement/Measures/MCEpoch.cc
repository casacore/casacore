//# MCEpoch.cc: MEpoch conversion routines
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
#ifdef __GNUG__
#include <aips/Quanta/Quantum.h>
typedef Quantum<Double> gpp_mepoch_bug1;
#endif
#include <aips/Mathematics/Constants.h>
#include <aips/Measures/MCEpoch.h>
#include <aips/Measures/MCFrame.h>
#include <aips/Measures/Nutation.h>
#include <aips/Measures/MeasTable.h>

//# Constructors
MCEpoch::MCEpoch() :
  NUTATFROM(0), NUTATTO(0) {}

//# Destructor
MCEpoch::~MCEpoch() {
  clearConvert();
}

//# Operators

//# Member functions

void MCEpoch::getConvert(MConvertBase &mc,
			 const MRBase &inref, 
			 const MRBase &outref) {
// Array of conversion routines to call
    static const MCEpoch::Routes 
      FromTo[MEpoch::N_Types][MEpoch::N_Types] = {
        { MCEpoch::N_Routes,   
	  MCEpoch::LAST_GAST,  
	  MCEpoch::LAST_GAST,  
	  MCEpoch::LAST_GAST,  
	  MCEpoch::LAST_GAST,  
	  MCEpoch::LAST_GAST,  
	  MCEpoch::LAST_GAST,  
	  MCEpoch::LAST_GAST,  
	  MCEpoch::LAST_GAST,  
	  MCEpoch::LAST_GAST,  
	  MCEpoch::LAST_GAST,  
	  MCEpoch::LAST_GAST},  
        { MCEpoch::LMST_GMST1,  
	  MCEpoch::N_Routes,
	  MCEpoch::LMST_GMST1,  
	  MCEpoch::LMST_GMST1,  
	  MCEpoch::LMST_GMST1,  
	  MCEpoch::LMST_GMST1,  
	  MCEpoch::LMST_GMST1,  
	  MCEpoch::LMST_GMST1,  
	  MCEpoch::LMST_GMST1,  
	  MCEpoch::LMST_GMST1,  
	  MCEpoch::LMST_GMST1,  
	  MCEpoch::LMST_GMST1},  
        { MCEpoch::GMST1_UT1,
	  MCEpoch::GMST1_LMST,
	  MCEpoch::N_Routes,  
	  MCEpoch::GMST1_UT1,  
	  MCEpoch::GMST1_UT1,  
	  MCEpoch::GMST1_UT1,  
	  MCEpoch::GMST1_UT1,  
	  MCEpoch::GMST1_UT1,  
	  MCEpoch::GMST1_UT1,  
	  MCEpoch::GMST1_UT1,  
	  MCEpoch::GMST1_UT1,  
	  MCEpoch::GMST1_UT1},
        { MCEpoch::GAST_LAST,
	  MCEpoch::GAST_UT1,
	  MCEpoch::GAST_UT1,
	  MCEpoch::N_Routes,  
	  MCEpoch::GAST_UT1,
	  MCEpoch::GAST_UT1,
	  MCEpoch::GAST_UT1,
	  MCEpoch::GAST_UT1,
	  MCEpoch::GAST_UT1,
	  MCEpoch::GAST_UT1,
	  MCEpoch::GAST_UT1,
	  MCEpoch::GAST_UT1},
        { MCEpoch::UT1_GAST,  
	  MCEpoch::UT1_GMST1,  
	  MCEpoch::UT1_GMST1,
	  MCEpoch::UT1_GAST,
	  MCEpoch::N_Routes,   
	  MCEpoch::UT1_UT2,    
	  MCEpoch::UT1_UTC,  
	  MCEpoch::UT1_UTC,    
	  MCEpoch::UT1_UTC,    
	  MCEpoch::UT1_UTC,  
	  MCEpoch::UT1_UTC,    
	  MCEpoch::UT1_UTC},
        { MCEpoch::UT2_UT1,    
	  MCEpoch::UT2_UT1,    
	  MCEpoch::UT2_UT1,  
	  MCEpoch::UT2_UT1,    
	  MCEpoch::UT2_UT1,    
	  MCEpoch::N_Routes,   
	  MCEpoch::UT2_UT1,  
	  MCEpoch::UT2_UT1,    
	  MCEpoch::UT2_UT1,    
	  MCEpoch::UT2_UT1,  
	  MCEpoch::UT2_UT1,    
	  MCEpoch::UT2_UT1},
        { MCEpoch::UTC_UT1,    
	  MCEpoch::UTC_UT1,    
	  MCEpoch::UTC_UT1,  
	  MCEpoch::UTC_UT1,  
	  MCEpoch::UTC_UT1,    
	  MCEpoch::UTC_UT1,    
	  MCEpoch::N_Routes, 
	  MCEpoch::UTC_TAI,    
	  MCEpoch::UTC_TAI,    
	  MCEpoch::UTC_TAI, 
	  MCEpoch::UTC_TAI,    
	  MCEpoch::UTC_TAI},
        { MCEpoch::TAI_UTC,    
	  MCEpoch::TAI_UTC,    
	  MCEpoch::TAI_UTC, 
	  MCEpoch::TAI_UTC, 
	  MCEpoch::TAI_UTC,    
	  MCEpoch::TAI_UTC,    
	  MCEpoch::TAI_UTC,           
	  MCEpoch::N_Routes,   
	  MCEpoch::TAI_TDT,    
	  MCEpoch::TAI_TDT, 
	  MCEpoch::TAI_TDT,    
	  MCEpoch::TAI_TDT},
        { MCEpoch::TDT_TAI,    
	  MCEpoch::TDT_TAI,    
	  MCEpoch::TDT_TAI, 
	  MCEpoch::TDT_TAI, 
	  MCEpoch::TDT_TAI,    
	  MCEpoch::TDT_TAI,    
	  MCEpoch::TDT_TAI, 
	  MCEpoch::TDT_TAI,    
	  MCEpoch::N_Routes,   
	  MCEpoch::TDT_TCG, 
	  MCEpoch::TDT_TDB,    
	  MCEpoch::TDT_TDB},
        { MCEpoch::TCG_TDT,    
	  MCEpoch::TCG_TDT,    
	  MCEpoch::TCG_TDT, 
	  MCEpoch::TCG_TDT, 
	  MCEpoch::TCG_TDT,    
	  MCEpoch::TCG_TDT,    
	  MCEpoch::TCG_TDT, 
	  MCEpoch::TCG_TDT,    
	  MCEpoch::TCG_TDT,    
	  MCEpoch::N_Routes, 
	  MCEpoch::TCG_TDT,    
	  MCEpoch::TCG_TDT},
        { MCEpoch::TDB_TDT,    
	  MCEpoch::TDB_TDT,    
	  MCEpoch::TDB_TDT, 
	  MCEpoch::TDB_TDT, 
	  MCEpoch::TDB_TDT,    
	  MCEpoch::TDB_TDT,    
	  MCEpoch::TDB_TDT, 
	  MCEpoch::TDB_TDT,    
	  MCEpoch::TDB_TDT,    
	  MCEpoch::TDB_TDT,  
	  MCEpoch::N_Routes,   
	  MCEpoch::TDB_TCB},
        { MCEpoch::TCB_TDB,    
	  MCEpoch::TCB_TDB,    
	  MCEpoch::TCB_TDB, 
	  MCEpoch::TCB_TDB, 
	  MCEpoch::TCB_TDB,    
	  MCEpoch::TCB_TDB,    
	  MCEpoch::TCB_TDB, 
	  MCEpoch::TCB_TDB,    
	  MCEpoch::TCB_TDB,    
	  MCEpoch::TCB_TDB, 
	  MCEpoch::TCB_TDB,    
	  MCEpoch::N_Routes}
    };

// List of codes converted to
    static const MEpoch::Types ToRef[MCEpoch::N_Routes] = {
	MEpoch::GAST, 	MEpoch::LAST,  
	MEpoch::GMST1, 	MEpoch::LMST,
	MEpoch::UT1,  	MEpoch::GMST1, 
	MEpoch::UT1,  	MEpoch::GAST, 
	MEpoch::UTC,   	MEpoch::UT1,
	MEpoch::UT2,  	MEpoch::UT1,   
	MEpoch::TAI,   	MEpoch::UTC,
	MEpoch::TDT,  	MEpoch::TAI,   
	MEpoch::TDB,   	MEpoch::TDT,
	MEpoch::TCG,  	MEpoch::TDT,   
	MEpoch::TCB,   	MEpoch::TDB
	};

    Int iin  = inref.getType();
    Bool iraze = ToBool(iin & MEpoch::RAZE);
    iin &= ~MEpoch::EXTRA;
    Int iout = outref.getType();
    Bool oraze = ToBool(iout & MEpoch::RAZE);
    iout &= ~MEpoch::EXTRA;
    Int tmp;
    while (iin != iout) {
	tmp = FromTo[iin][iout];
	iin = ToRef[tmp];
	mc.addMethod(tmp);
	initConvert(tmp, mc);
    };
    if (iraze) {
	mc.addMethod(MCEpoch::RAZING);
    };
}

void MCEpoch::clearConvert() {
  delete NUTATFROM; NUTATFROM = 0;
  delete NUTATTO;   NUTATTO = 0;
}

//# Conversion routines
void MCEpoch::initConvert(uInt which, MConvertBase &mc) {

    switch (which) {

	case LAST_GAST:
	break;

	case GAST_LAST:
	break;

	case LMST_GMST1:
	break;

	case GMST1_LMST:
	break;

	case GMST1_UT1:
	break;

	case UT1_GMST1:
	break;

	case GAST_UT1:
	  if (NUTATTO) delete NUTATTO;
	  NUTATTO = new Nutation(Nutation::STANDARD);
	break;

	case UT1_GAST:
	  if (NUTATFROM) delete NUTATFROM;
	  NUTATFROM = new Nutation(Nutation::STANDARD);
	break;

	case UT1_UTC:
	break;

	case UTC_UT1:
	break;

	case UT1_UT2:
	break;

	case UT2_UT1:
	break;

	case UTC_TAI:
	break;

	case TAI_UTC:
	break;

	case TAI_TDT:
	break;

	case TDT_TAI:
	break;

	case TDT_TDB:
	break;

	case TDB_TDT:
	break;

	case TDT_TCG:
	break;

	case TCG_TDT:
	break;

	case TDB_TCB:
	break;

	case TCB_TDB:
	break;

	default:
	break;
    }
}

void MCEpoch::doConvert(MeasValue &in,
			MRBase &inref,
			MRBase &outref,
			const MConvertBase &mc) {
  doConvert(*(MVEpoch*)&in,
	    inref, outref, mc);
}

void MCEpoch::doConvert(MVEpoch &in,
			MRBase &inref,
			MRBase &outref,
			const MConvertBase &mc) {
  Double locLong, eqox, ut;

  MCFrame::make(inref.getFrame());
  MCFrame::make(outref.getFrame());

     for (Int i=0; i<mc.nMethod(); i++) {

     switch (mc.getMethod(i)) {
	
      case LAST_GAST: {
	((MCFrame *)(MEpoch::Ref::framePosition(inref, outref).
		     getMCFramePoint()))->
	  getLong(locLong);
	in -= locLong/C::circle;
      };
      break;
      
      case GAST_LAST: {
	((MCFrame *)(MEpoch::Ref::framePosition(outref, inref).
		     getMCFramePoint()))->
	  getLong(locLong);
	in += locLong/C::circle;
      };
      break;
      
      case LMST_GMST1: {
	((MCFrame *)(MEpoch::Ref::framePosition(inref, outref).
		     getMCFramePoint()))->
	  getLong(locLong);
	in -= locLong/C::circle;
      };
      break;
      
      case GMST1_LMST: {
	((MCFrame *)(MEpoch::Ref::framePosition(outref, inref).
		     getMCFramePoint()))->
	  getLong(locLong);
	in += locLong/C::circle;
      };
      break;
      
      case GMST1_UT1: {
	ut = in.get();
	in += MeasTable::GMUT0(ut)*MeasData::JDCEN/MeasData::SECinDAY;
	in -= MVEpoch(6713.);
      };
      break;
      
      case UT1_GMST1: {
	ut = in.get();
	in += MeasTable::GMST0(ut)/MeasData::SECinDAY;
	in += MVEpoch(6713.);
      };
      break;
      
      case GAST_UT1: {
// Guess UT1 without equation of equinoxes
	ut = in.get();
	ut += MeasTable::GMUT0(ut)*MeasData::JDCEN/MeasData::SECinDAY;
	ut -= 6713.;
// Equation of equinoxes
	eqox = NUTATTO->eqox(ut);
	in -= eqox/C::circle;
// GMST1 to UT1
	ut = in.get();
	in += MeasTable::GMUT0(ut)*MeasData::JDCEN/MeasData::SECinDAY;
	in -= MVEpoch(6713.);
      };
      break;

      case UT1_GAST: {
// Make GMST1
	ut = in.get();
	in += MeasTable::GMST0(ut)/MeasData::SECinDAY;
	in += MVEpoch(6713.);
// Equation of equinoxes
	eqox = NUTATFROM->eqox(ut);
	in += eqox/C::circle;
      };
      break;

      case UT1_UTC:
	in -= MeasTable::dUT1(in.get())/MeasData::SECinDAY;
	break;

      case UTC_UT1:
	in += MeasTable::dUT1(in.get())/MeasData::SECinDAY;
	break;

      case UT1_UT2:
	break;

      case UT2_UT1:
	break;

      case UTC_TAI:
	in += MeasTable::dUTC(in.get())/MeasData::SECinDAY;
	break;

      case TAI_UTC:
	in -= MeasTable::dUTC(in.get())/MeasData::SECinDAY;
	break;
	
      case TAI_TDT:
	in += MeasTable::dTAI(in.get())/MeasData::SECinDAY;
	break;

      case TDT_TAI:
	in -= MeasTable::dTAI(in.get())/MeasData::SECinDAY;
	break;
	
      case TDT_TDB:
	in += MeasTable::dTDT(in.get())/MeasData::SECinDAY;
	break;

      case TDB_TDT:
	in -= MeasTable::dTDT(in.get())/MeasData::SECinDAY;
	break;

      case TDT_TCG:
	break;
	
      case TCG_TDT:
	break;
	
      case TDB_TCB:
	in += MeasTable::dTDB(in.get())/MeasData::SECinDAY;
	break;
			
      case TCB_TDB:
	in -= MeasTable::dTDB(in.get())/MeasData::SECinDAY;
	break;

      case RAZING:
	in = in.getDay();
	break;

      default:
	break;
      }
    }
}
