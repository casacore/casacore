//# MeasIERS.cc: Interface to IERS tables
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
#include <aips/Measures/Quantum.h>
typedef Quantum<Double> gpp_measiers_bug1;
#endif
#include <aips/Measures/MeasIERS.h>
#include <aips/Measures/Quantum.h>
#include <aips/Measures/MVTime.h>
#include <aips/Exceptions/Error.h>
#include <aips/OS/Time.h>
#include <aips/Logging.h>
#include <aips/Tasking/Aipsrc.h>
#include <aips/Tasking/AipsrcValue.h>

//# Constants
const Double MeasIERS::INTV = 5;

//# Static data
uInt MeasIERS::predicttime_reg = 0;
uInt MeasIERS::notable_reg = 0;
uInt MeasIERS::forcepredict_reg = 0;

//# Member functions
Bool MeasIERS::get(Double &returnValue,
		   MeasIERS::Files file, 
		   MeasIERS::Types type, 
		   Double date) {
  returnValue = 0.0;
  if (!MeasIERS::predicttime_reg) {
    predicttime_reg = 
      AipsrcValue<Double>::registerRC(String("measures.measiers.d_predicttime"),
				      Unit("d"), Unit("d"),
				      MeasIERS::INTV);
    notable_reg = 
      AipsrcValue<Bool>::registerRC(String("measures.measiers.b_notable"),
				    False);
    forcepredict_reg = 
      AipsrcValue<Bool>::registerRC(String("measures.measiers.b_forcepredict"),
				    False);
  };
  if (AipsrcValue<Bool>::get(MeasIERS::notable_reg)) return True;
  Bool res = True;
  Double f;
  if (!dateNow) dateNow = Time().modifiedJulianDay();
  if (file == PREDICTED ||
      AipsrcValue<Bool>::get(MeasIERS::forcepredict_reg) ||
      (dateNow - date) <= 
      AipsrcValue<Double>::get(MeasIERS::predicttime_reg)) {
    res = False;			// do predict
  };
  MeasIERS::Files which = MeasIERS::MEASURED;
  if (res) {
    res = initMeas(which);
    if (res) {
      res = fillMeas(which, date);
    };
  };
  if (!res) {				// Retry for predicted
    which = MeasIERS::PREDICTED;
    if (!initMeas(which)) return False;
    if (!fillMeas(which, date)) {
      if (!msgDone) {
	msgDone = True;
	LogIO os(LogOrigin("MeasIERS",
			   String("fillMeas(MeasIERS::Files, Double)"),
			   WHERE));
	os << LogIO::WARN <<
	  String("A request has been "
		 "made for a time outside IERS tables"
		 "\nCalculations will proceed with less precision") << 
	  LogIO::POST;
      };
      return False;
    };
  };
  // Interpolation fraction
  f = date - ldat[which][0];
  returnValue = ldat[which+N_Files][type]*f - ldat[which][type]*(f-1.0);
  return True;
}

Bool MeasIERS::initMeas(MeasIERS::Files which) {
  static const String names[MeasIERS::N_Types] = {
    "MJD",
    "x",
    "y",
    "dUT1",
    "LOD",
    "dPsi",
    "dEps",
    "Dx",
    "Dy",
    "DdUT1",
    "DLOD",
    "DdPsi",
    "DdEps"};
  static const String tplc[N_Files] = {"measures.ierseop97.directory",
				 "measures.ierspredict.directory"};

  if (!measured[which] && measFlag[which]) {
    measFlag[which] = False;
    TableRecord kws;
    Double dt;
    String vs;
    Bool ok = True;
    if (!MeasIERS::getTable(MeasIERS::t[which], kws, row[which],
			    rfp[which], vs, dt, 
			    N_Types, names, tp[which],
			    tplc[which],
			    "aips/Measures")) {
      LogIO os(LogOrigin("MeasIERS",
			 String("initMeas(MeasIERS::Files)"),
			 WHERE));
      os << LogIO::WARN <<
	String("Cannot read IERS table ") + tp[which] +
	  "\nCalculations will proceed with lower precision" << 
	LogIO::POST;
      return False;
    };
    if (!kws.isDefined("MJD0") || kws.asDouble("MJD0") < 10000)
      ok = False;
    if (ok) {
      mjd0[which] = Int(kws.asDouble("MJD0"));
      Int n = t[which].nrow();
      row[which].get(n-1);
      if (*(rfp[which][0]) != mjd0[which] + n) { 
	ok = False;
      } else {
	mjdl[which] = mjd0[which] + n;
      };
    };
    if (!ok) {
      LogIO os(LogOrigin("MeasIERS",
			 String("initMeas(MeasIERS::Files)"),
			 WHERE));
      os << String("Corrupted IERS table ") + tp[which] << LogIO::EXCEPTION;
    };
    measured[which] = True;
    for (Int i = 0; i < MeasIERS::N_Types; i++) {
      for (Int j = 0; j < 2*MeasIERS::N_Files; j++) {
	ldat[j][i] = 0;
      };
    }; 
  };
  return ToBool(measured[which]);
}

// Table handling
Bool MeasIERS::getTable(Table &table, TableRecord &kws, ROTableRow &row,
			RORecordFieldPtr<Double> rfp[],
			String &vs, Double &dt,
			Int N, const String rfn[],
			const String &name,
			const String &rc, const String &dir) {
  String ldir;
  Bool ok = True;
  if (Aipsrc::find(ldir, rc)) {
    ldir += '/';
  } else {
    ldir = Aipsrc::aipsHome() + "/data/" + dir + '/';
    if (!Table::isReadable(ldir + name)) {
      ldir = Aipsrc::aipsRoot() + "/data/" + dir + '/';
      if (!Table::isReadable(ldir + name)) {
	ldir = Aipsrc::aipsHome() + "/code/trial/apps/measures/";
	if (!Table::isReadable(ldir + name)) {
	  ldir = Aipsrc::aipsRoot() + "/code/trial/apps/measures/";
	};
      };
    };
  };
  if (!Table::isReadable(ldir + name)) return False;
  Table tab(ldir + name);
  TableRecord ks(tab.keywordSet());
  if (!ks.isDefined("VS_DATE") || !ks.isDefined("VS_VERSION") ||
      !ks.isDefined("VS_CREATE") || !ks.isDefined("VS_TYPE") ||
      (tab.tableInfo().type() != String("IERS"))) {
    ok = False;
  };
  if (ok) {
    Quantity ldt;
    if (MVTime::read(ldt, ks.asString("VS_DATE"))) {
      dt = MVTime(ldt);
      vs = ks.asString("VS_VERSION");
    } else {
      ok = False;
    };
  };
  ROTableRow rw(tab);
  if (ok) {
    for (Int i=0; i < N; i++) {
      if (!rw.record().isDefined(rfn[i])) {
	ok = False; break;
      };
    };
  };
  if (!ok) {
    LogIO os(LogOrigin("MeasIERS",
		       String("getTable(Table &, TableRecord &, "
			      "ROTableRow &, RORecordFieldPtr<Double> *, "
			      "String &vs, Double &dt, "
			      "Int N, const String *, const String &, "
			      "const String &, const String &)"),
		       WHERE));
    os << String("A ") + name + 
      " table has been read that seems corrupted"
      "\nNotify the AIPS++ system manager about it" << LogIO::EXCEPTION;
    return False;
  };
  table = tab;
  table.makePermanent();
  kws = ks;
  row = rw;
  for (Int i=0; i < N; i++)
    rfp[i] = RORecordFieldPtr<Double>(row.record(), rfn[i]);
  return True;
}

Bool MeasIERS::fillMeas(MeasIERS::Files which, Double utf) {
  Int ut = ifloor(utf);
  if (ut < mjd0[which] + 1 || ut >= mjdl[which]) {
    return False;
  };
  if (utf >= ldat[which][0] &&
      utf <= ldat[which+N_Files][0]) {
    // Already there
  } else {
    if (utf >= ldat[which+N_Files][0] && 
	utf <= ldat[which+N_Files][0] + 1) {
      // Shift one
      for (Int i=0; i<MeasIERS::N_Types; i++) {
	ldat[which][i] = ldat[which+N_Files][i];
      };
      // For end points
      ut = Int(ldat[which][0]);
    } else {
      // Read first line
      row[which].get(ut-mjd0[which]-1);
      for (Int i=0; i<MeasIERS::N_Types; i++) {
	ldat[which][i] = *(rfp[which][i]);
      };
    };
    // Read second line
    row[which].get(Int(ldat[which][0])-mjd0[which]);
    for (Int i=0; i<MeasIERS::N_Types; i++) {
      ldat[which+N_Files][i] = *(rfp[which][i]);
    };
    if (ldat[which][0] != ut || ldat[which+N_Files][0] != ut+1) {
      LogIO os(LogOrigin("MeasIERS",
			 String("fillMeas(MeasIERS::Files, Double)"),
			 WHERE));
      os <<
	String("The IERS table ") + tp[which] +
	" has been corrupted: regenerate" <<
	LogIO::EXCEPTION;
    };
  };
  return True;
}
  
Bool MeasIERS::measFlag[MeasIERS::N_Files] = {True, True};
Bool MeasIERS::measured[MeasIERS::N_Files] = {False, False};
Double MeasIERS::dateNow = 0.0;
Table MeasIERS::t[MeasIERS::N_Files];
ROTableRow MeasIERS::row[MeasIERS::N_Files];
RORecordFieldPtr<Double> MeasIERS::rfp[MeasIERS::N_Files][MeasIERS::N_Types];
Int MeasIERS::mjd0[MeasIERS::N_Files] = {0, 0};
Int MeasIERS::mjdl[MeasIERS::N_Files] = {0, 0};
Double MeasIERS::ldat[2*MeasIERS::N_Files][MeasIERS::N_Types];
Bool MeasIERS::msgDone = False;
const String MeasIERS::tp[MeasIERS::N_Files] = {"IERSeop97", "IERSpredict"};
