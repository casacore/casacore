//# MeasJPL.cc: Interface to JPL DE tables
//# Copyright (C) 1996,1997,1998
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
typedef Quantum<Double> gpp_MeasJPL_bug1;
#endif
#include <aips/Mathematics/Math.h>
#include <aips/Exceptions/Error.h>
#include <aips/Measures/MeasJPL.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/MVEpoch.h>
#include <aips/Measures/MeasIERS.h>
#include <aips/OS/Time.h>
#include <aips/Logging.h>
#include <aips/Tasking/Aipsrc.h>
#include <aips/Tables/TableDesc.h>

//# Constants

//# Member functions
Bool MeasJPL::get(Vector<Double> &returnValue,
		  MeasJPL::Files file, 
		  MeasJPL::Types type, 
		  const MVEpoch &date) {
  static Double res[6];
  static Double res1[6];
  returnValue = 0.0;
  Double intv;
  if (!initMeas(file)) {
    return False;
  };
  if (!fillMeas(intv, file, date)) {
    return False;
  };
  // Interpolation fraction
  Bool dIt;
  Bool mulfr = True;
  const Double *dta = dval[file].getStorage(dIt);
  if (type == MeasJPL::BARYSOLAR) {
    for (uInt i=0; i<6; i++) res[i] = 0.0;
  } else if (type == MeasJPL::BARYEARTH) {
    interMeas(res, file, intv,
	      dmjd[file], idx[file][1][MeasJPL::EARTH-1], 3, 
	      idx[file][2][MeasJPL::EARTH-1],
	      dta + idx[file][0][MeasJPL::EARTH-1]);
  } else if (type == MeasJPL::EARTH || type == MeasJPL::MOON) {
    interMeas(res1, file, intv,
	      dmjd[file], idx[file][1][MeasJPL::MOON-1], 3, 
	      idx[file][2][MeasJPL::MOON-1],
	      dta + idx[file][0][MeasJPL::MOON-1]);
    interMeas(res, file, intv,
	      dmjd[file], idx[file][1][MeasJPL::EARTH-1], 3, 
	      idx[file][2][MeasJPL::EARTH-1],
	      dta + idx[file][0][MeasJPL::EARTH-1]);
    if (type == MeasJPL::EARTH) {
      for (uInt i=0; i<6; i++) res[i] -= res1[i]/emrat[file];
    } else {
      for (uInt i=0; i<6; i++) res[i] += res1[i];
    };
  } else if (type == MeasJPL::NUTATION) {
    if (idx[file][1][MeasJPL::BARYSOLAR-1] == 0) return False;
    interMeas(res, file, intv,
	      dmjd[file], idx[file][1][MeasJPL::BARYSOLAR-1], 2, 
	      idx[file][2][MeasJPL::BARYSOLAR-1],
	      dta + idx[file][0][MeasJPL::BARYSOLAR-1]);
    mulfr = False;
  } else if (type == MeasJPL::LIBRATION) {
    if (idx[file][1][MeasJPL::BARYEARTH-1] == 0) return False;
    interMeas(res, file, intv,
	      dmjd[file], idx[file][1][MeasJPL::BARYEARTH-1], 3, 
	      idx[file][2][MeasJPL::BARYEARTH-1],
	      dta + idx[file][0][MeasJPL::BARYEARTH-1]);
    mulfr = False;
  } else {
    interMeas(res, file, intv,
	      dmjd[file], idx[file][1][type-1], 3,
	      idx[file][2][type-1],
	      dta + idx[file][0][type-1]);
  };
  dval[file].freeStorage(dta, dIt);
  if (mulfr) {
    for (uInt i=0; i<6; i++)
      returnValue(i) = res[i]*aufac[file];
  } else {
    for (uInt i=0; i<6; i++)
      returnValue(i) = res[i];
  };
  
  return True;
}

Bool MeasJPL::getConst(Double &res, MeasJPL::Files which,
		       MeasJPL::Codes what) {
  if (initMeas(which)) {
    res = cn[which][what];
    return True;
  };
  return False;
}

Bool MeasJPL::getConst(Double &res, MeasJPL::Files which,
		       const String &nam) {
  if (initMeas(which)) {
    const TableRecord &tr = t[which].keywordSet();
    if (tr.isDefined(nam)) {
      res = tr.asDouble(nam);
      return True;
    };
  };
  return False;
}

Bool MeasJPL::initMeas(MeasJPL::Files which) {
  static const String names[MeasJPL::N_Columns] = {
    "MJD",
    "x" };
  static const String tplc[N_Files] = {"measures.DE200.directory",
				 "measures.DE405.directory"};

  if (!measured[which] && measFlag[which]) {
    measFlag[which] = False;
    TableRecord kws;
    Double dt;
    String vs;
    Bool ok = True;
    if (!MeasIERS::getTable(MeasJPL::t[which], kws, row[which],
			    rfp[which], vs, dt, 
			    1, names, tp[which],
			    tplc[which],
			    "aips/Measures")) {
      return False;
    };
    if (!kws.isDefined("MJD0") || kws.asDouble("MJD0") < 10000 ||
	!kws.isDefined("dMJD") || kws.asDouble("dMJD") < 8 ||
	!kws.isDefined("AU") || kws.asDouble("AU") < 1e8 ||
	!kws.isDefined("CLIGHT") || kws.asDouble("CLIGHT") < 2e5 ||
	!kws.isDefined("GMS") || kws.asDouble("GMS") < 2e-4 ||
	!((kws.isDefined("RADS") && kws.asDouble("RADS") > 6e5) ||
	(kws.isDefined("ASUN") && kws.asDouble("ASUN") > 6e5))||
	!kws.isDefined("EMRAT") || kws.asDouble("EMRAT") < 10 )
      ok = False;
    if (ok) {
      mjd0[which] = Int(kws.asDouble("MJD0"));
      dmjd[which] = Int(kws.asDouble("dMJD"));
      cn[which][MeasJPL::AU] = kws.asDouble("AU");
      aufac[which] = 1./cn[which][MeasJPL::AU];
      emrat[which] = 1.+kws.asDouble("EMRAT");
      cn[which][MeasJPL::CAU] = 86400 * kws.asDouble("CLIGHT")/
	cn[which][MeasJPL::AU];
      if (kws.isDefined("RADS"))
	cn[which][MeasJPL::RADS] = kws.asDouble("RADS")/
	cn[which][MeasJPL::AU];
      else
	cn[which][MeasJPL::RADS] = kws.asDouble("ASUN")/
	cn[which][MeasJPL::AU];
      cn[which][MeasJPL::GMS] = kws.asDouble("GMS")/
	cn[which][MeasJPL::CAU]/cn[which][MeasJPL::CAU];
      Int n = t[which].nrow();
      row[which].get(n-1);
      if (*(rfp[which][0]) != mjd0[which] + n*dmjd[which]) { 
	ok = False;
      } else {
	mjdl[which] = mjd0[which] + n*dmjd[which];
      };
    };
    if (ok) {
      const TableRecord &tr = t[which].tableDesc().columnDesc("x").
	keywordSet();
      if (tr.asInt("Rows") != 3 || tr.asInt("Columns") != 13) {
	ok = False;
      } else {
	Array<Int> xx = tr.asArrayInt("Description");
	uInt k = 0;
	for (uInt i=0; i<3; i++) {
	  for (uInt j=0; j<13; j++) {
	    idx[which][i][j] = xx(IPosition(1,k++));
	    if (i == 0) idx[which][i][j] -= 3;
	  };
	};
	acc[which].attach(t[which], "x");
      };
    };
    if (!ok) {
      LogIO os(LogOrigin("MeasJPL",
			 String("initMeas(MeasJPL::Files)"),
			 WHERE));
      os << String("Corrupted JPL table ") + tp[which] << LogIO::EXCEPTION;
    };
    measured[which] = True;
    chc[0] = 1;
    chc[1] = 0;
    chcv[0] = 0;
    chcv[1] = 1;
  };
  return ToBool(measured[which]);
}

Bool MeasJPL::fillMeas(Double &intv, MeasJPL::Files which,
		       const MVEpoch &utf) {
  Int ut = Int(utf.getDay());
  if (ut < mjd0[which] + dmjd[which] || ut >= mjdl[which] + dmjd[which]) {
    return False;
  };
  ut = (ut-mjd0[which])/dmjd[which];
  intv = ((utf.getDay() - (ut*dmjd[which] + mjd0[which]))
	   + utf.getDayFraction())/dmjd[which];
  if (ut == ldat[which]) return True;	// already there
  dval[which] = acc[which](ut-1);
  ldat[which] = ut;
  return True;
}

void MeasJPL::interMeas(Double res[], MeasJPL::Files which, Double intv, 
			Double ivf, Int ncf, Int ncm, Int na, 
			const Double buf[]) {
  Double tc = 2.0*(fmod(Double(na)*intv, Double(1.0)) + Int(intv)) - 1.0;
  Int l = Int(Double(na)*intv - Int(intv));
  if (tc != chc[1]) {
    chc[1] = tc;
    np = 2;
    nv =3;
    twot = 2*tc;
  };
  if (np < ncf) {
    for (uInt i=np; i<ncf; i++)
      chc[i] = twot*chc[i-1] - chc[i-2];
    np = ncf;
  };
  vfac = 2.0*Double(na)/ivf;
  chcv[2] = 2.0*twot;
  if (nv < ncf) {
    for (uInt i=nv; i<ncf; i++)
      chcv[i] = twot*chcv[i-1] + 2.0*chc[i-1] - chcv[i-2];
    nv = ncf;
  };
  { // Position
    for (uInt i=0; i<ncm; i++) {
      res[i] = 0;
      for (Int j=ncf-1; j>=0; j--) {
	res[i] += chc[j]*buf[(l*ncm+i)*ncf+j];
      };
    };
  }
  { // Velocity
    for (uInt i=0; i<ncm; i++) {
      res[i+ncm] = 0;
      for (Int j=ncf-1; j>0; j--) {
	res[i+ncm] += chcv[j]*buf[(l*ncm+i)*ncf+j];
      };
      res[i+ncm] *= vfac;
    };
  }
}

Bool MeasJPL::measFlag[MeasJPL::N_Files] = {True, True};
Bool MeasJPL::measured[MeasJPL::N_Files] = {False, False};
Table MeasJPL::t[MeasJPL::N_Files];
ROTableRow MeasJPL::row[MeasJPL::N_Files];
RORecordFieldPtr<Double> MeasJPL::rfp[MeasJPL::N_Files][MeasJPL::N_Types];
Int MeasJPL::mjd0[MeasJPL::N_Files] = {0, 0};
Int MeasJPL::mjdl[MeasJPL::N_Files] = {0, 0};
Int MeasJPL::dmjd[MeasJPL::N_Files] = {0, 0};
Bool MeasJPL::msgDone = False;
const String MeasJPL::tp[MeasJPL::N_Files] = {"DE200", "DE405"};
Int MeasJPL::idx[MeasJPL::N_Files][3][13];
Int MeasJPL::ldat[MeasJPL::N_Files] = {0, 0};
Vector<Double> MeasJPL::dval[MeasJPL::N_Files];
ROArrayColumn<Double> MeasJPL::acc[MeasJPL::N_Files];
Double MeasJPL::chc[18];
Double MeasJPL::chcv[18];
Double MeasJPL::aufac[MeasJPL::N_Files];
Double MeasJPL::emrat[MeasJPL::N_Files];
Double MeasJPL::cn[MeasJPL::N_Files][MeasJPL::N_Codes];
Int MeasJPL::np = 2;
Int MeasJPL::nv = 3;
Double MeasJPL::twot = 0.;
Double MeasJPL::vfac = 0.;
