//# MEpoch.cc: A Measure: instant in time
//# Copyright (C) 1995, 1996
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
typedef Quantum<Double> gpp_mepoch_bug1;
#endif
#include <aips/Exceptions/Error.h>
#include <aips/Utilities/Assert.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/Nutation.h>
#include <aips/Measures/MeasData.h>

//# Constructors
MEpoch::MEpoch() : 
  MeasBase<MVEpoch,MEpoch::Ref>() {}

MEpoch::MEpoch(const MVEpoch &dt) : 
  MeasBase<MVEpoch,MEpoch::Ref>(dt,MEpoch::DEFAULT) {}

MEpoch::MEpoch(const MVEpoch &dt, const MEpoch::Ref &rf) : 
  MeasBase<MVEpoch,MEpoch::Ref>(dt,rf) {}

MEpoch::MEpoch(const MVEpoch &dt, uInt rf) : 
  MeasBase<MVEpoch,MEpoch::Ref>(dt,rf) {}

MEpoch::MEpoch(const Quantity &dt) : 
  MeasBase<MVEpoch,MEpoch::Ref>(dt,MEpoch::DEFAULT) {}

MEpoch::MEpoch(const Quantity &dt, const MEpoch::Ref &rf) : 
  MeasBase<MVEpoch,MEpoch::Ref>(dt,rf) {}

MEpoch::MEpoch(const Quantity &dt, uInt rf) : 
  MeasBase<MVEpoch,MEpoch::Ref>(dt,rf) {}

//# Destructor
MEpoch::~MEpoch() {}

//# Operators

//# Member functions

const String &MEpoch::tellMe() const {
    return MEpoch::showMe();
}

const String &MEpoch::showMe() {
    static const String name("Epoch");
    return name;
}

const String &MEpoch::showType(uInt tp) {
    static const String tname[MEpoch::N_Types] = {
	"LAST",
	"LMST",
	"GMST1",
	"GAST",
	"UT1",
	"UT2",
	"UTC",
	"TAI",
	"TDT",
	"TCG",
	"TDB",
	"TCB"};
    DebugAssert((tp & ~MEpoch::EXTRA) < MEpoch::N_Types, AipsError);
    return tname[tp & ~MEpoch::EXTRA];
}

Bool MEpoch::giveMe(const String &in, MEpoch::Ref &mr) {
    static const Int N_name = 16;
    static const String tname[N_name] = {
	"LAST",
	"LMST",
	"GMST1",
	"GAST",
	"UT1",
	"UT2",
	"UTC",
	"TAI",
	"TDT",
	"TCG",
	"TDB",
	"TCB",
	"IAT",
	"GMST",
	"TT",
	"UT"};

    static const uInt oname[N_name] = {
	MEpoch::LAST,
	MEpoch::LMST,
	MEpoch::GMST1,
	MEpoch::GAST,
	MEpoch::UT1,
	MEpoch::UT2,
	MEpoch::UTC,
	MEpoch::TAI,
	MEpoch::TDT,
	MEpoch::TCG,
	MEpoch::TDB,
	MEpoch::TCB,
	MEpoch::TAI,
	MEpoch::GMST1,
	MEpoch::TDT,
	MEpoch::UT1};

    uInt i = Measure::giveMe(in, N_name, tname);
    if (i>=N_name) {
	mr = MEpoch::Ref();
	return False;
    } else {
	mr = MEpoch::Ref(oname[i]);
    };
    return True;
}

Quantity MEpoch::get(const Unit &inunit) const {
    return (data.getTime().get(inunit));
}

void *MEpoch::clone() const {
    return ((void *) new MEpoch(*this));
}

void MEpoch::getConvert(MEpoch::Convert &mc,
			const MEpoch::Ref &inref, 
			const MEpoch::Ref &outref) {
// Array of conversion routines to call
    static const MEpoch::Routes FromTo[MEpoch::N_Types][MEpoch::N_Types] = {
        { MEpoch::N_Routes,   
	  MEpoch::LAST_GAST,  
	  MEpoch::LAST_GAST,  
	  MEpoch::LAST_GAST,  
	  MEpoch::LAST_GAST,  
	  MEpoch::LAST_GAST,  
	  MEpoch::LAST_GAST,  
	  MEpoch::LAST_GAST,  
	  MEpoch::LAST_GAST,  
	  MEpoch::LAST_GAST,  
	  MEpoch::LAST_GAST,  
	  MEpoch::LAST_GAST},  
        { MEpoch::LMST_GMST1,  
	  MEpoch::N_Routes,
	  MEpoch::LMST_GMST1,  
	  MEpoch::LMST_GMST1,  
	  MEpoch::LMST_GMST1,  
	  MEpoch::LMST_GMST1,  
	  MEpoch::LMST_GMST1,  
	  MEpoch::LMST_GMST1,  
	  MEpoch::LMST_GMST1,  
	  MEpoch::LMST_GMST1,  
	  MEpoch::LMST_GMST1,  
	  MEpoch::LMST_GMST1},  
        { MEpoch::GMST1_UT1,
	  MEpoch::GMST1_LMST,
	  MEpoch::N_Routes,  
	  MEpoch::GMST1_UT1,  
	  MEpoch::GMST1_UT1,  
	  MEpoch::GMST1_UT1,  
	  MEpoch::GMST1_UT1,  
	  MEpoch::GMST1_UT1,  
	  MEpoch::GMST1_UT1,  
	  MEpoch::GMST1_UT1,  
	  MEpoch::GMST1_UT1,  
	  MEpoch::GMST1_UT1},
        { MEpoch::GAST_LAST,
	  MEpoch::GAST_UT1,
	  MEpoch::GAST_UT1,
	  MEpoch::N_Routes,  
	  MEpoch::GAST_UT1,
	  MEpoch::GAST_UT1,
	  MEpoch::GAST_UT1,
	  MEpoch::GAST_UT1,
	  MEpoch::GAST_UT1,
	  MEpoch::GAST_UT1,
	  MEpoch::GAST_UT1,
	  MEpoch::GAST_UT1},
        { MEpoch::UT1_GAST,  
	  MEpoch::UT1_GMST1,  
	  MEpoch::UT1_GMST1,
	  MEpoch::UT1_GAST,
	  MEpoch::N_Routes,   
	  MEpoch::UT1_UT2,    
	  MEpoch::UT1_UTC,  
	  MEpoch::UT1_UTC,    
	  MEpoch::UT1_UTC,    
	  MEpoch::UT1_UTC,  
	  MEpoch::UT1_UTC,    
	  MEpoch::UT1_UTC},
        { MEpoch::UT2_UT1,    
	  MEpoch::UT2_UT1,    
	  MEpoch::UT2_UT1,  
	  MEpoch::UT2_UT1,    
	  MEpoch::UT2_UT1,    
	  MEpoch::N_Routes,   
	  MEpoch::UT2_UT1,  
	  MEpoch::UT2_UT1,    
	  MEpoch::UT2_UT1,    
	  MEpoch::UT2_UT1,  
	  MEpoch::UT2_UT1,    
	  MEpoch::UT2_UT1},
        { MEpoch::UTC_UT1,    
	  MEpoch::UTC_UT1,    
	  MEpoch::UTC_UT1,  
	  MEpoch::UTC_UT1,  
	  MEpoch::UTC_UT1,    
	  MEpoch::UTC_UT1,    
	  MEpoch::N_Routes, 
	  MEpoch::UTC_TAI,    
	  MEpoch::UTC_TAI,    
	  MEpoch::UTC_TAI, 
	  MEpoch::UTC_TAI,    
	  MEpoch::UTC_TAI},
        { MEpoch::TAI_UTC,    
	  MEpoch::TAI_UTC,    
	  MEpoch::TAI_UTC, 
	  MEpoch::TAI_UTC, 
	  MEpoch::TAI_UTC,    
	  MEpoch::TAI_UTC,    
	  MEpoch::TAI_UTC,           
	  MEpoch::N_Routes,   
	  MEpoch::TAI_TDT,    
	  MEpoch::TAI_TDT, 
	  MEpoch::TAI_TDT,    
	  MEpoch::TAI_TDT},
        { MEpoch::TDT_TAI,    
	  MEpoch::TDT_TAI,    
	  MEpoch::TDT_TAI, 
	  MEpoch::TDT_TAI, 
	  MEpoch::TDT_TAI,    
	  MEpoch::TDT_TAI,    
	  MEpoch::TDT_TAI, 
	  MEpoch::TDT_TAI,    
	  MEpoch::N_Routes,   
	  MEpoch::TDT_TCG, 
	  MEpoch::TDT_TDB,    
	  MEpoch::TDT_TDB},
        { MEpoch::TCG_TDT,    
	  MEpoch::TCG_TDT,    
	  MEpoch::TCG_TDT, 
	  MEpoch::TCG_TDT, 
	  MEpoch::TCG_TDT,    
	  MEpoch::TCG_TDT,    
	  MEpoch::TCG_TDT, 
	  MEpoch::TCG_TDT,    
	  MEpoch::TCG_TDT,    
	  MEpoch::N_Routes, 
	  MEpoch::TCG_TDT,    
	  MEpoch::TCG_TDT},
        { MEpoch::TDB_TDT,    
	  MEpoch::TDB_TDT,    
	  MEpoch::TDB_TDT, 
	  MEpoch::TDB_TDT, 
	  MEpoch::TDB_TDT,    
	  MEpoch::TDB_TDT,    
	  MEpoch::TDB_TDT, 
	  MEpoch::TDB_TDT,    
	  MEpoch::TDB_TDT,    
	  MEpoch::TDB_TDT,  
	  MEpoch::N_Routes,   
	  MEpoch::TDB_TCB},
        { MEpoch::TCB_TDB,    
	  MEpoch::TCB_TDB,    
	  MEpoch::TCB_TDB, 
	  MEpoch::TCB_TDB, 
	  MEpoch::TCB_TDB,    
	  MEpoch::TCB_TDB,    
	  MEpoch::TCB_TDB, 
	  MEpoch::TCB_TDB,    
	  MEpoch::TCB_TDB,    
	  MEpoch::TCB_TDB, 
	  MEpoch::TCB_TDB,    
	  MEpoch::N_Routes}
    };

// List of codes converted to
    static const MEpoch::Types ToRef[MEpoch::N_Routes] = {
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
	mc.addMethod(MEpoch::RAZING);
    };
}

void MEpoch::clearConvert(MEpoch::Convert &mc) {
  delete (Nutation *) mc.getStruct(MEpoch::NUTATFROM);
  delete (Nutation *) mc.getStruct(MEpoch::NUTATTO);
}

//# Conversion routines
void MEpoch::initConvert(uInt which, MEpoch::Convert &mc) {

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
	mc.addStruct(MEpoch::NUTATTO,
		     (void *) new Nutation(Nutation::STANDARD));
	break;

	case UT1_GAST:
	mc.addStruct(MEpoch::NUTATFROM,
		     (void *) new Nutation(Nutation::STANDARD));
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

void MEpoch::doConvert(MVEpoch &in,
		       const MEpoch::Ref &inref,
		       const MEpoch::Ref &outref,
		       const MEpoch::Convert &mc) {
    Double locLong, eqox, ut;

     for (Int i=0; i<mc.nMethod(); i++) {

     switch (mc.getMethod(i)) {
	
      case LAST_GAST: {
	MEpoch::Ref::framePosition(inref, outref).getLong(locLong);
	in -= locLong/C::circle;
      };
      break;
      
      case GAST_LAST: {
	MEpoch::Ref::framePosition(outref, inref).getLong(locLong);
	in += locLong/C::circle;
      };
      break;
      
      case LMST_GMST1: {
	MEpoch::Ref::framePosition(inref, outref).getLong(locLong);
	in -= locLong/C::circle;
      };
      break;
      
      case GMST1_LMST: {
	MEpoch::Ref::framePosition(outref, inref).getLong(locLong);
	in += locLong/C::circle;
      };
      break;
      
      case GMST1_UT1: {
	ut = in.get();
	in += MeasData::GMUT0(ut)*MeasData::JDCEN/MeasData::SECinDAY;
	in -= MVEpoch(6713.);
      };
      break;
      
      case UT1_GMST1: {
	ut = in.get();
	in += MeasData::GMST0(ut)/MeasData::SECinDAY;
	in += MVEpoch(6713.);
      };
      break;
      
      case GAST_UT1: {
// Guess UT1 without equation of equinoxes
	ut = in.get();
	ut += MeasData::GMUT0(ut)*MeasData::JDCEN/MeasData::SECinDAY;
	ut -= 6713.;
// Equation of equinoxes
	eqox = (((Nutation *) 
		 mc.getStruct(MEpoch::NUTATTO))->
		eqox(ut));
	in -= eqox/C::circle;
// GMST1 to UT1
	ut = in.get();
	in += MeasData::GMUT0(ut)*MeasData::JDCEN/MeasData::SECinDAY;
	in -= MVEpoch(6713.);
      };
      break;

      case UT1_GAST: {
// Make GMST1
	ut = in.get();
	in += MeasData::GMST0(ut)/MeasData::SECinDAY;
	in += MVEpoch(6713.);
// Equation of equinoxes
	eqox = (((Nutation *) 
		 mc.getStruct(MEpoch::NUTATFROM))->
		eqox(ut));
	in += eqox/C::circle;
      };
      break;

      case UT1_UTC:
	in -= MeasData::dUT1(in.get())/MeasData::SECinDAY;
	break;

      case UTC_UT1:
	in += MeasData::dUT1(in.get())/MeasData::SECinDAY;
	break;

      case UT1_UT2:
	break;

      case UT2_UT1:
	break;

      case UTC_TAI:
	in += MeasData::dUTC(in.get())/MeasData::SECinDAY;
	break;

      case TAI_UTC:
	in -= MeasData::dUTC(in.get())/MeasData::SECinDAY;
	break;
	
      case TAI_TDT:
	in += MeasData::dTAI(in.get())/MeasData::SECinDAY;
	break;

      case TDT_TAI:
	in -= MeasData::dTAI(in.get())/MeasData::SECinDAY;
	break;
	
      case TDT_TDB:
	in += MeasData::dTDT(in.get())/MeasData::SECinDAY;
	break;

      case TDB_TDT:
	in -= MeasData::dTDT(in.get())/MeasData::SECinDAY;
	break;

      case TDT_TCG:
	break;
	
      case TCG_TDT:
	break;
	
      case TDB_TCB:
	in += MeasData::dTDB(in.get())/MeasData::SECinDAY;
	break;
			
      case TCB_TDB:
	in -= MeasData::dTDB(in.get())/MeasData::SECinDAY;
	break;

      case RAZING:
	in = in.getDay();
	break;

      default:
	break;
      }
    }
}
