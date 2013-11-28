//# MeasIERS.cc: Interface to IERS tables
//# Copyright (C) 1996-2003,2007,2008
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
#include <measures/Measures/MeasIERS.h>
#include <casa/Arrays/Vector.h>
#include <casa/Exceptions/Error.h>
#include <casa/Logging/LogIO.h>
#include <casa/BasicMath/Math.h>
#include <casa/OS/Time.h>
#include <casa/OS/Path.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Quanta/MVTime.h>
#include <casa/System/Aipsrc.h>
#include <casa/System/AipsrcValue.h>
#include <casa/BasicSL/String.h>

namespace casa { //# NAMESPACE CASA - BEGIN

#ifndef CASADATA
#define CASADATA "/usr/local"
#endif

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
  }
  if (AipsrcValue<Bool>::get(MeasIERS::notable_reg)) return True;
  Bool res = True;
  Double f;
  if (!dateNow) dateNow = Time().modifiedJulianDay();
  if (file == PREDICTED ||
      AipsrcValue<Bool>::get(MeasIERS::forcepredict_reg) ||
      (dateNow - date) <= 
      AipsrcValue<Double>::get(MeasIERS::predicttime_reg)) {
    res = False;			// do predict
  }
  MeasIERS::Files which = MeasIERS::MEASURED;
  if (res) {
    res = initMeas(which);
    if (res) {
      res = fillMeas(which, date);
    }
  }
  if (!res) {				// Retry for predicted
    which = MeasIERS::PREDICTED;
    if (!initMeas(which)) return False;
    if (!fillMeas(which, date)) {
      if (!msgDone) {
	LogIO os(LogOrigin("MeasIERS",
			   String("fillMeas(MeasIERS::Files, Double)"),
			   WHERE));
        Time now;       // current time
        
        if(date > now.modifiedJulianDay()){
          // People using times from the future are almost certainly simulating
          // data, and, even if they would be upset by the IERS table not being
          // available, there is not much they can do about it.
          os << LogIO::NORMAL3
             << "High precision Earth axis data is not yet available for requested JD "
             << date
             << LogIO::POST;
        }
        else{
          os << LogIO::NORMAL
             << "Requested JD " << date
             << " is outside the range of the IERS (Earth axis data) table." 	 
             << "\nCalculations will proceed with less precision"
             << LogIO::POST;
        }
	msgDone = True;
      }
      return False;
    }
  }
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

  if (measFlag[which]) {
    ScopedMutexLock locker(theirMutex);
    if (measFlag[which]) {
      TableRecord kws;
      Double dt;
      String vs;
      Bool ok = True;
      if (!MeasIERS::getTable(MeasIERS::t[which], kws, row[which],
                              rfp[Int(which)], vs, dt, 
                              N_Types, names, tp[which],
                              tplc[which],
                              "geodetic")) {
        LogIO os(LogOrigin("MeasIERS",
                           String("initMeas(MeasIERS::Files)"),
                           WHERE));
        os << LogIO::NORMAL1
           << "Cannot read IERS (Earth axis data) table " << tp[which]
           << "\nCalculations will proceed with lower precision"
           << LogIO::POST;
        return False;
      }
      if (ok) {
        MeasIERS::openNote(&MeasIERS::closeMeas);
        if (!kws.isDefined("MJD0") || kws.asDouble("MJD0") < 10000) {
          ok = False;
        }
      }
      if (ok) {
        mjd0[which] = Int(kws.asDouble("MJD0"));
        Int n = t[which].nrow();
        row[which].get(n-1);
        if (*(rfp[Int(which)][0]) != mjd0[which] + n) { 
          ok = False;
        } else {
          mjdl[which] = mjd0[which] + n;
        }
      }
      if (!ok) {
        LogIO os(LogOrigin("MeasIERS",
                           String("initMeas(MeasIERS::Files)"),
                           WHERE));
        os << String("Corrupted IERS table ") + tp[which] << LogIO::EXCEPTION;
      }
      for (Int i = 0; i < MeasIERS::N_Types; i++) {
        for (Int j = 0; j < 2*MeasIERS::N_Files; j++) {
          ldat[j][i] = 0;
        }
      }
      measFlag[which] = False;
    }
  }
  return (! t[which].isNull());
}

void MeasIERS::closeMeas() {
  for (uInt i=0; i<N_Files; ++i) {
    if (!measFlag[i]) {
      ScopedMutexLock locker(theirMutex);
      if (!measFlag[i]) {
        if (! t[i].isNull()) {
          dateNow = 0.0;
          mjd0[i] = 0;
          mjdl[i] = 0;
          msgDone = False;
	  row[i] = ROTableRow();
          t[i] = Table();
        }
        measFlag[i] = True;
      }
    }
  }
}

void MeasIERS::openNote(CLOSEFUN fun) {
  // Resize if too small.
  if (nNote >= sizeNote) {
    CLOSEFUN *tmp = new CLOSEFUN[sizeNote+10];
    for (uInt i=0; i<sizeNote; ++i) tmp[i] = toclose[i];
    for (uInt i=sizeNote; i<sizeNote+10; ++i) tmp[i] = 0;
    delete [] toclose;
    toclose = tmp;
    sizeNote += 10;
  }
  toclose[nNote++] = fun;
}

void MeasIERS::closeTables() {
  for (uInt i=nNote; i>0; --i) {
    if (toclose[i-1] != 0) {
      toclose[i-1]();
      toclose[i-1] = 0;
    }
  }
  delete [] toclose; toclose = 0;
  sizeNote = 0;
  nNote = 0;
}

// Table handling
Bool MeasIERS::getTable(Table &table, TableRecord &kws, ROTableRow &row,
			RORecordFieldPtr<Double> rfp[],
			String &vs, Double &dt,
			Int N, const String rfn[],
			const String &name,
			const String &rc, const String &dir,
			const Table *tabin) {
  Table tab;
  Bool ok = findTab(tab, tabin, rc, dir, name);

  if(!ok)
    return false;            // findTab logs its own errors.
  
  LogIO os(LogOrigin("MeasIERS",
		     String("getTable(Table &, TableRecord &, "
			    "ROTableRow &, RORecordFieldPtr<Double> *, "
			    "String &vs, Double &dt, "
			    "Int N, const String *, const String &, "
			    "const String &, const String &)"),
		     WHERE));

  TableRecord ks(tab.keywordSet());

  ok = handle_keywords(dt, vs, ks, tab);
  
  ROTableRow rw(tab);
  if (ok) {
    // Check that the table is not missing any expected columns.
    for (Int i=0; i < N; i++) {
      if (!rw.record().isDefined(rfn[i])) {
	os << LogIO::SEVERE
	   << "Column " << rfn[i] << " is missing."
	   << LogIO::POST;
	ok = False;// break;
      }
    }
  }
  if (!ok) {
    os << name + " has an incompatible format."
       << "\nYou may want to notify the CASA system manager about it."
       << LogIO::EXCEPTION;
    return False;
  }
  table = tab;
  kws = ks;
  row = rw;
  for (Int i=0; i < N; i++)
    rfp[i] = RORecordFieldPtr<Double>(row.record(), rfn[i]);
  return True;
}

Bool MeasIERS::getTable(Table &table, TableRecord &kws, ROTableRow &row,
			Vector<RORecordFieldPtr<Double> >& rfp,
			String &vs, Double &dt,
			const Vector<String>& reqcols,
			Vector<String>& optcols,
			const String &name,
			const String &rc, const String &dir,
			const Table *tabin)
{
  Table tab;
  Bool ok = findTab(tab, tabin, rc, dir, name);

  if(!ok)
    return false;            // findTab logs its own errors.
  
  LogIO os(LogOrigin("MeasIERS", "getTable(Vector<String>& optcols)",
		     WHERE));

  TableRecord ks(tab.keywordSet());

  ok = handle_keywords(dt, vs, ks, tab);
  
  ROTableRow rw(tab);
  if(ok){
    // Check that the table is not missing any required columns.
    for(Int i = reqcols.nelements(); i--;){
      if(!rw.record().isDefined(reqcols[i])){
	os << LogIO::SEVERE
	   << "Required column " << reqcols[i] << " is missing."
	   << LogIO::POST;
	ok = False;// break;
      }
    }
  }
  if(!ok){
    os << name + " has an incompatible format."
       << "\nYou may want to notify the CASA system manager about it."
       << LogIO::EXCEPTION;
    return False;
  }

  // Now look for optional columns.
  Vector<String> foundoptcols;
  uInt noptcolsfound = 0;
  for(uInt i = 0; i < optcols.nelements(); ++i){
    if(rw.record().isDefined(optcols[i])){
      ++noptcolsfound;
      foundoptcols.resize(noptcolsfound, true);
      foundoptcols[noptcolsfound - 1] = optcols[i];
    }
  }

  // Together these are equiv. to optcols.assign(foundoptcols), but I find this clearer.
  optcols.resize(noptcolsfound);
  optcols = foundoptcols;
  
  table = tab;
  kws = ks;
  row = rw;
  rfp.resize(reqcols.nelements() + noptcolsfound);
  for(uInt i = 0; i < reqcols.nelements(); ++i)
    rfp[i] = RORecordFieldPtr<Double>(row.record(), reqcols[i]);
  for(uInt i = 0; i < noptcolsfound; ++i)
    rfp[reqcols.nelements() + i] = RORecordFieldPtr<Double>(row.record(), optcols[i]);
  return True;
}

// Helper function for getTable().
Bool MeasIERS::findTab(Table& tab, const Table *tabin, const String &rc,
		       const String &dir, const String &name)
{
  Bool ok = true;
  LogIO os(LogOrigin("MeasIERS", "findTab", WHERE));
  
  if(!tabin){				// No table object given: search name
    String ldir;
    Vector<String> searched;

    if(name[0] == '/'){			// Absolute path given.
      ldir = "";
    }
    else{
      const String path[2] = {
	"/ephemerides/",
	"/geodetic/"
      };

      if (Aipsrc::find(ldir, rc)){
	ldir += '/';
	searched.resize(searched.nelements() + 1, True);
	searched[searched.nelements() - 1] = ldir;
      }
      else{
	String udir;

	if(!dir.empty())
	  udir = dir + '/';
	ldir = "./";
	searched.resize(searched.nelements()+1, True);
	searched[searched.nelements()-1] = ldir;
	if (!Table::isReadable(ldir + name)) {
	  ldir = "./data/";
	  searched.resize(searched.nelements() + 1, True);
	  searched[searched.nelements()-1] = ldir;
	  if (!Table::isReadable(ldir + name)) {
	    Bool found = False;
	    String mdir;
	    if (Aipsrc::find(mdir, "measures.directory")) {
              mdir.trim();
              Path mpath = Path(mdir);
              mpath.append(udir);
              ldir = mpath.absoluteName()+"/";
              searched.resize(searched.nelements()+1, True);
              searched[searched.nelements()-1] = ldir;
              if  (Table::isReadable(ldir+name)) {
                found = True;
              }
              if (!found) {
                for (Int i=0; i<2; i++) {
                  Path mpath = Path(mdir +"/" + path[i]);
                  ldir = mpath.absoluteName()+"/";
                  searched.resize(searched.nelements()+1, True);
                  searched[searched.nelements()-1] = ldir;
                  if  (Table::isReadable(ldir+name)) {
                    found = True;
                    break;
                  }
                }
              }
	    }
	    if (!found) {
	      for (Int i=0; i<2; i++) {
		ldir = Aipsrc::aipsHome() + "/data/" + path[i];
		searched.resize(searched.nelements()+1, True);
		searched[searched.nelements()-1] = ldir;
		if (Table::isReadable(ldir + name)) {
		  found = True;
		  break;
		}
		ldir = Aipsrc::aipsRoot() + "/data/" + path[i];
		searched.resize(searched.nelements()+1, True);
		searched[searched.nelements()-1] = ldir;
		if (Table::isReadable(ldir + name)) {
		  found = True;
		  break;
		}
		Path cdatapath(String(CASADATA));
		ldir = cdatapath.absoluteName() + path[i];
		searched.resize(searched.nelements() + 1, True);
		searched[searched.nelements() - 1] = ldir;
		if (Table::isReadable(ldir + name)) {
		  found = True;
		  break;
		}
		ldir = cdatapath.absoluteName() + "/share/casacore/data/" \
		  + path[i];
		ldir = cdatapath.absoluteName() + path[i];
		searched.resize(searched.nelements() + 1, True);
		searched[searched.nelements() - 1] = ldir;
		if (Table::isReadable(ldir + name)) {
		  found = True;
		  break;
		}              
	      }
	    }
	  }
	}
      }
    }
    if(!Table::isReadable(ldir + name)){
      os << LogIO::WARN <<
	String("Requested data table ") << name <<
	String(" cannot be found in the searched directories:\n");
      for(uInt i = 0; i < searched.nelements(); ++i)
	os << searched[i] << "\n";
      os << LogIO::POST;
      return False;
    }
    tab = Table(ldir + name);
  }
  else
    tab = *tabin;

  return ok;
}

// Helper function for getTable().
Bool MeasIERS::handle_keywords(Double &dt, String &vs, const TableRecord& ks,
			       const Table& tab)
{
  LogIO os(LogOrigin("MeasIERS", "handle_keywords", WHERE));
  Bool ok = true;
  
  if(!ks.isDefined("VS_DATE") || !ks.isDefined("VS_VERSION") ||
     !ks.isDefined("VS_CREATE") || !ks.isDefined("VS_TYPE") ||
     (tab.tableInfo().type() != String("IERS"))){
    ok = False;
    os << LogIO::DEBUG1
       << "ks.isDefined(VS_DATE) " << ks.isDefined("VS_DATE")
       << "\nks.isDefined(VS_VERSION) " << ks.isDefined("VS_VERSION")
       << "\nks.isDefined(VS_CREATE) " << ks.isDefined("VS_CREATE")
       << "\nks.isDefined(VS_TYPE) " << ks.isDefined("VS_TYPE")
       << "\ntab.tableInfo().type() " << tab.tableInfo().type()
       << LogIO::POST;
  }
  if (ok) {
    Quantity ldt;
    if (MVTime::read(ldt, ks.asString("VS_DATE"))) {
      dt = MVTime(ldt);
      vs = ks.asString("VS_VERSION");
    } else {
      ok = False;
    }
  }
  return ok;
}

Bool MeasIERS::fillMeas(MeasIERS::Files which, Double utf) {
  Int ut = ifloor(utf);
  if (ut < mjd0[which] + 1 || ut >= mjdl[which]) {
    return False;
  }
  if (utf >= ldat[which][0] &&
      utf <= ldat[which+N_Files][0]) {
    // Already there
  } else {
    if (utf >= ldat[which+N_Files][0] && 
	utf <= ldat[which+N_Files][0] + 1) {
      // Shift one
      for (Int i=0; i<MeasIERS::N_Types; i++) {
	ldat[which][i] = ldat[which+N_Files][i];
      }
      // For end points
      ut = Int(ldat[which][0]);
    } else {
      // Read first line
      row[which].get(ut-mjd0[which]-1);
      for (Int i=0; i<MeasIERS::N_Types; i++) {
	ldat[which][i] = *(rfp[which][i]);
      }
    }
    // Read second line
    row[which].get(Int(ldat[which][0])-mjd0[which]);
    for (Int i=0; i<MeasIERS::N_Types; i++) {
      ldat[which+N_Files][i] = *(rfp[which][i]);
    }
    if (ldat[which][0] != ut || ldat[which+N_Files][0] != ut+1) {
      LogIO os(LogOrigin("MeasIERS",
			 String("fillMeas(MeasIERS::Files, Double)"),
			 WHERE));
      os <<
	String("The IERS table ") + tp[which] +
	" has been corrupted: regenerate" <<
	LogIO::EXCEPTION;
    }
  }
  return True;
}
  
volatile Bool MeasIERS::measFlag[MeasIERS::N_Files] = {True, True};
Double MeasIERS::dateNow = 0.0;
Table MeasIERS::t[MeasIERS::N_Files];
ROTableRow MeasIERS::row[MeasIERS::N_Files];
RORecordFieldPtr<Double> MeasIERS::rfp[MeasIERS::N_Files][MeasIERS::N_Types];
Int MeasIERS::mjd0[MeasIERS::N_Files] = {0, 0};
Int MeasIERS::mjdl[MeasIERS::N_Files] = {0, 0};
Double MeasIERS::ldat[2*MeasIERS::N_Files][MeasIERS::N_Types];
Bool MeasIERS::msgDone = False;
const String MeasIERS::tp[MeasIERS::N_Files] = {"IERSeop97", "IERSpredict"};
uInt MeasIERS::sizeNote = 0;
uInt MeasIERS::nNote = 0;
MeasIERS::CLOSEFUN *MeasIERS::toclose = 0;
Mutex MeasIERS::theirMutex;

} //# NAMESPACE CASA - END
