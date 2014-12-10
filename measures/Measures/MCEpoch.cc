//# MCEpoch.cc: MEpoch conversion routines
//# Copyright (C) 1995-2001,2004,2007
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
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/measures/Measures/MeasFrame.h>
#include <casacore/measures/Measures/Nutation.h>
#include <casacore/measures/Measures/MeasTable.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Statics
uInt MCEpoch::ToRef_p[N_Routes][3] = {
  {MEpoch::LAST,	MEpoch::GAST,	2},
  {MEpoch::GAST,	MEpoch::LAST,	2},
  {MEpoch::LMST,	MEpoch::GMST1,	2},
  {MEpoch::GMST1,	MEpoch::LMST,	2},
  {MEpoch::GMST1,	MEpoch::UT1,	2},
  {MEpoch::UT1,	 	MEpoch::GMST1,	2},
  {MEpoch::GAST,	MEpoch::UT1,	2},
  {MEpoch::UT1,		MEpoch::GAST,	2},
  {MEpoch::UT1,		MEpoch::UTC,	0},
  {MEpoch::UTC,		MEpoch::UT1,	0},
  {MEpoch::UT1,		MEpoch::UT2,	0},
  {MEpoch::UT2,		MEpoch::UT1,	0},
  {MEpoch::UTC,		MEpoch::TAI,	0},
  {MEpoch::TAI,		MEpoch::UTC,	0},
  {MEpoch::TAI,		MEpoch::TDT,	0},
  {MEpoch::TDT,		MEpoch::TAI,	0},
  {MEpoch::TDT,		MEpoch::TDB,	0},
  {MEpoch::TDB,		MEpoch::TDT,	0},
  {MEpoch::TDT,		MEpoch::TCG,	0},
  {MEpoch::TCG,		MEpoch::TDT,	0},
  {MEpoch::TDB,		MEpoch::TCB,	0},
  {MEpoch::TCB,		MEpoch::TDB,	0} };

uInt MCEpoch::FromTo_p[MEpoch::N_Types][MEpoch::N_Types];
MutexedInit MCEpoch::theirMutexedInit (MCEpoch::doFillState);

//# Constructors
MCEpoch::MCEpoch() :
  NUTATFROM(0), NUTATTO(0) {
    fillState();
}

//# Destructor
MCEpoch::~MCEpoch() {
  clearConvert();
}

//# Operators

//# Member functions

void MCEpoch::getConvert(MConvertBase &mc,
			 const MRBase &inref, 
			 const MRBase &outref) {

    Int iin  = inref.getType();
    Bool iraze = (iin & MEpoch::RAZE);
    iin &= ~MEpoch::EXTRA;
    Int iout = outref.getType();
    iout &= ~MEpoch::EXTRA;
    Int tmp;
    while (iin != iout) {
	tmp = FromTo_p[iin][iout];
	iin = ToRef_p[tmp][1];
	mc.addMethod(tmp);
	initConvert(tmp, mc);
    }
    if (iraze) {
	mc.addMethod(MCEpoch::RAZING);
    }
}

void MCEpoch::clearConvert() {
  delete NUTATFROM; NUTATFROM = 0;
  delete NUTATTO;   NUTATTO = 0;
}

//# Conversion routines
void MCEpoch::initConvert(uInt which, MConvertBase &mc) {

  if (False) initConvert(which, mc);	// Stop warning

  switch (which) {

  case GAST_UT1:
    if (NUTATTO) delete NUTATTO;
    NUTATTO = new Nutation(Nutation::STANDARD);
    break;

  case UT1_GAST:
    if (NUTATFROM) delete NUTATFROM;
    NUTATFROM = new Nutation(Nutation::STANDARD);
    break;

  case LAST_GAST:
  case GAST_LAST:
  case LMST_GMST1:
  case GMST1_LMST:
    mc.addFrameType(MeasFrame::POSITION);
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
  static MVEpoch mve6713(6713.);
  Double locLong, eqox, ut, tt, xx;
  
  for (Int i=0; i<mc.nMethod(); i++) {
    
    switch (mc.getMethod(i)) {
      
    case LAST_GAST: {
      MEpoch::Ref::framePosition(inref, outref).
	getLong(locLong);
      in -= locLong/C::circle;
    }
      break;
      
    case GAST_LAST: {
      MEpoch::Ref::framePosition(outref, inref).
	getLong(locLong);
      in += locLong/C::circle;
    }
      break;
      
    case LMST_GMST1: {
      MEpoch::Ref::framePosition(inref, outref).
	getLong(locLong);
      in -= locLong/C::circle;
    }
      break;
      
    case GMST1_LMST: {
      MEpoch::Ref::framePosition(outref, inref).
	getLong(locLong);
      in += locLong/C::circle;
    }
      break;
      
    case GMST1_UT1: {
      xx = ut = in.get();
      in += MeasTable::GMUT0(ut)*MeasData::JDCEN/MeasData::SECinDAY;
      in -= mve6713;
      if (MeasTable::useIAU2000()) {
	uInt i(0);
	do {
	  MVEpoch xe(in);
	  ut = xe.get();
	  xe -= MeasTable::dUT1(xe.get())/MeasData::SECinDAY;
	  xe += MeasTable::dUTC(xe.get())/MeasData::SECinDAY;
	  xe += MeasTable::dTAI(xe.get())/MeasData::SECinDAY;
	  xe += MeasTable::GMST00(ut, xe.get())/C::_2pi;
	  xe += mve6713;
	  tt = (xx-xe.get())/2.0055;
	  in += MVEpoch(tt);
	  i++;
	} while (abs(tt) > 1e-7 && i<10);
      }
    }
      break;
      
    case UT1_GMST1: {
      ut = in.get();
      if (MeasTable::useIAU2000()) {
	in -= MeasTable::dUT1(in.get())/MeasData::SECinDAY;
	in += MeasTable::dUTC(in.get())/MeasData::SECinDAY;
	in += MeasTable::dTAI(in.get())/MeasData::SECinDAY;
	in += MeasTable::GMST00(ut, in.get())/C::_2pi;
      } else {
	in += MeasTable::GMST0(ut)/MeasData::SECinDAY;
      }
      in += mve6713;
    }
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
      in -= mve6713;
    }
      break;
      
    case UT1_GAST: {
      // Make GMST1
      ut = in.get();
      in += MeasTable::GMST0(ut)/MeasData::SECinDAY;
      in += mve6713;
      // Equation of equinoxes
      eqox = NUTATFROM->eqox(ut);
      in += eqox/C::circle;
    }
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
    } // switch
  } //for
}
  
String MCEpoch::showState() {
  fillState();
  return MCBase::showState(MCEpoch::FromTo_p[0],
                           MEpoch::N_Types, MCEpoch::N_Routes,
                           MCEpoch::ToRef_p);
}
  
void MCEpoch::doFillState (void*) {
  MEpoch::checkMyTypes();
  MCBase::makeState(FromTo_p[0], MEpoch::N_Types, N_Routes, ToRef_p);
}

} //# NAMESPACE CASACORE - END

