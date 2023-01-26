//# MeasIERS.cc: Interface to IERS tables
//# Copyright (C) 1996-2003,2007,2008,2016
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

//# Includes
#include <casacore/measures/Measures/MeasIERS.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/OS/Time.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/System/Aipsrc.h>
#include <casacore/casa/System/AipsrcValue.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/System/AppState.h>

#include <atomic>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

#ifndef CASADATA
#define CASADATA "/usr/local/share/data/casacore"
#endif

//# Constants
const double MeasIERS::INTV = 5;

//# Static data
std::once_flag MeasIERS::theirCallOnceFlag;
uint32_t MeasIERS::predicttime_reg = 0;
uint32_t MeasIERS::notable_reg = 0;
uint32_t MeasIERS::forcepredict_reg = 0;
double MeasIERS::dateNow = 0.0;
Vector<double> MeasIERS::ldat[MeasIERS::N_Files][MeasIERS::N_Types];
const String MeasIERS::tp[MeasIERS::N_Files] = {"IERSeop97", "IERSpredict"};
uint32_t MeasIERS::sizeNote = 0;
uint32_t MeasIERS::nNote = 0;
MeasIERS::CLOSEFUN *MeasIERS::toclose = 0;


//# Member functions
bool MeasIERS::get(double &returnValue,
                   MeasIERS::Files file,
                   MeasIERS::Types type,
                   double date) {
  returnValue = 0.0;
  std::call_once(theirCallOnceFlag, initMeas);

  // Exit if no table has to be used.
  if (AipsrcValue<bool>::get(MeasIERS::notable_reg)) {
    return true;
  }

  // Test if PREDICTED has to be used.
  int32_t which = MEASURED;
  if (file == PREDICTED ||
      ldat[MEASURED][0].empty() ||
      AipsrcValue<bool>::get(MeasIERS::forcepredict_reg) ||
      (dateNow-date) <= AipsrcValue<double>::get(MeasIERS::predicttime_reg)) {
    which = PREDICTED;
  }

  int32_t ut = ifloor(date);
  if (which == MEASURED) {
    const Vector<double>& mjds = ldat[which][0];
    if (ut < mjds[0]  ||  ut >= mjds[mjds.size()-1]) {
      which = PREDICTED;
    }
  }

  if (which == PREDICTED) {
#if defined(USE_THREADS)
    static std::atomic<bool> msgDone;
#else
    static bool msgDone;
#endif
    const Vector<double>& mjds = ldat[which][0];
    if (mjds.empty()  ||  ut < mjds[0]  ||  ut >= mjds[mjds.size()-1]) {
      // It is harmless if the message accidentally appears multiple times.
      if (!msgDone) {
        msgDone = true;
        LogIO os(LogOrigin("MeasIERS",
                           "fillMeas(MeasIERS::Files, double)",
                           WHERE));
        Time now;       // current time
        if (date > now.modifiedJulianDay()){
          // People using times from the future are almost certainly simulating
          // data, and, even if they would be upset by the IERS table not being
          // available, there is not much they can do about it.
          os << LogIO::NORMAL3
             << "High precision Earth axis data is not yet available for requested JD "
             << date
             << LogIO::POST;
        } else {
          os << LogIO::NORMAL
             << "Requested JD " << date
             << " is outside the range of the IERS (Earth axis data) table."
             << "\nCalculations will proceed with less precision"
             << LogIO::POST;
        }
      }
      return false;
    }
  }

  // Interpolation fraction
  int32_t indx = int32_t(date - ldat[which][0][0]);

  // old version in use up to Jan 2016
  // if (indx >= 0  &&  indx < int32_t(ldat[which][0].size())-1) {
  //   double f = date - ldat[which][0][indx];
  //   returnValue = ldat[which][type][indx+1]*f - ldat[which][type][indx]*(f-1.0);
  //   return true;
  // }

  if (indx >= 0  &&  indx < int32_t(ldat[which][0].size())-1) {
    double f = date - ldat[which][0][indx]; // Fraction
    double vlo = ldat[which][type][indx]; // Get daily values
    double vhi = ldat[which][type][indx+1];
    if (abs(vhi-vlo) > 0.5) { // Jump
      vhi -= sign(vhi-vlo); // Remove jump
    }
    returnValue = vhi*f - vlo*(f-1.0);
    return true;
  }

  return false;
}


void MeasIERS::initMeas() {
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

  predicttime_reg = 
    AipsrcValue<double>::registerRC(String("measures.measiers.d_predicttime"),
                                    Unit("d"), Unit("d"),
                                    MeasIERS::INTV);
  notable_reg = 
    AipsrcValue<bool>::registerRC(String("measures.measiers.b_notable"),
                                  false);
  forcepredict_reg = 
    AipsrcValue<bool>::registerRC(String("measures.measiers.b_forcepredict"),
                                  false);
  dateNow = Time().modifiedJulianDay();

  TableRecord kws;
  Table tab;
  TableRow row;
  RORecordFieldPtr<double> rfp[N_Types];
  double dt;
  String vs;
  for (int32_t which=0; which<N_Files; ++which) {
    if (!MeasIERS::getTable(tab, kws, row,
                            rfp, vs, dt, 
                            N_Types, names, tp[which],
                            tplc[which],
                            "geodetic")) {
      LogIO os(LogOrigin("MeasIERS", "initMeas(MeasIERS::Files)", WHERE));
      os << LogIO::NORMAL1
         << "Cannot read IERS (Earth axis data) table " << tp[which]
         << "\nCalculations will proceed with lower precision"
         << LogIO::POST;
    } else {
      MeasIERS::openNote(&MeasIERS::closeMeas);
      // Read the entire file.
      for (int32_t i=0; i<MeasIERS::N_Types; ++i) {
        ScalarColumn<double>(tab, names[i]).getColumn (ldat[which][i]);
      }
      // Check if MJD in first and last row match and have step 1.
      const Vector<double>& mjds = ldat[which][0];
      if (mjds[mjds.size()-1] != mjds[0] + mjds.size()-1) {
        LogIO os(LogOrigin("MeasIERS",
                           "initMeas(MeasIERS::Files)",
                           WHERE));
        os << "IERS table " << tp[which]
           << " seems to be corrupted (time step not 1)" << LogIO::EXCEPTION;
      }
    }
  }
}

void MeasIERS::closeMeas() {
  // Cannot get this fast & thread-safe without rewriting initMeas/closeMeas.
  // But this is only used to check for memory leaks at the end and possibly
  // to compare tables in tests, so don't bother. Apply pray and HACK below...
  dateNow = 0.0;
  for (uint32_t i=0; i<N_Files; ++i) {
    for (uint32_t j=0; j<N_Types; ++j) {
      ldat[i][j].resize();
    }
  }
#if defined(USE_THREADS)
  std::atomic_thread_fence(std::memory_order_release); // pray
#endif
  new (&theirCallOnceFlag) std::once_flag; // HACK
}

void MeasIERS::openNote(CLOSEFUN fun) {
  // Resize if too small.
  if (nNote >= sizeNote) {
    CLOSEFUN *tmp = new CLOSEFUN[sizeNote+10];
    for (uint32_t i=0; i<sizeNote; ++i) tmp[i] = toclose[i];
    for (uint32_t i=sizeNote; i<sizeNote+10; ++i) tmp[i] = 0;
    delete [] toclose;
    toclose = tmp;
    sizeNote += 10;
  }
  toclose[nNote++] = fun;
}

void MeasIERS::closeTables() {
  for (uint32_t i=nNote; i>0; --i) {
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
bool MeasIERS::getTable(Table &table, TableRecord &kws, ROTableRow &row,
                        RORecordFieldPtr<double> rfp[],
                        String &vs, double &dt,
                        int32_t N, const String rfn[],
                        const String &name,
                        const String &rc, const String &dir,
                        const Table *tabin) {
  Table tab;
  bool ok = findTab(tab, tabin, rc, dir, name);

  if(!ok)
    return false;            // findTab logs its own errors.
  
  LogIO os(LogOrigin("MeasIERS",
                     String("getTable(Table &, TableRecord &, "
                            "ROTableRow &, RORecordFieldPtr<double> *, "
                            "String &vs, double &dt, "
                            "Int N, const String *, const String &, "
                            "const String &, const String &)"),
                     WHERE));

  TableRecord ks(tab.keywordSet());

  ok = handle_keywords(dt, vs, ks, tab);
  
  ROTableRow rw(tab);
  if (ok) {
    // Check that the table is not missing any expected columns.
    for (int32_t i=0; i < N; i++) {
      if (!rw.record().isDefined(rfn[i])) {
        os << LogIO::SEVERE
           << "Column " << rfn[i] << " is missing."
           << LogIO::POST;
        ok = false;// break;
      }
    }
  }
  if (!ok) {
    os << name << " has an incompatible format."
       << "\nYou may want to notify the CASA system manager about it."
       << LogIO::EXCEPTION;
    return false;
  }
  table = tab;
  kws = ks;
  row = rw;
  for (int32_t i=0; i < N; i++)
    rfp[i] = RORecordFieldPtr<double>(row.record(), rfn[i]);
  return true;
}

bool MeasIERS::getTable(Table &table, TableRecord &kws, ROTableRow &row,
                        Vector<RORecordFieldPtr<double> >& rfp,
                        String &vs, double &dt,
                        const Vector<String>& reqcols,
                        Vector<String>& optcols,
                        const String &name,
                        const String &rc, const String &dir,
                        const Table *tabin)
{
  Table tab;
  bool ok = findTab(tab, tabin, rc, dir, name);

  if(!ok)
    return false;            // findTab logs its own errors.
  
  LogIO os(LogOrigin("MeasIERS", "getTable(Vector<String>& optcols)",
                     WHERE));

  TableRecord ks(tab.keywordSet());

  ok = handle_keywords(dt, vs, ks, tab);
  
  ROTableRow rw(tab);
  if(ok){
    // Check that the table is not missing any required columns.
    for(int32_t i = reqcols.nelements(); i--;){
      if(!rw.record().isDefined(reqcols[i])){
        os << LogIO::SEVERE
           << "Required column " << reqcols[i] << " is missing."
           << LogIO::POST;
        ok = false;// break;
      }
    }
  }
  if(!ok){
    os << name + " has an incompatible format."
       << "\nYou may want to notify the CASA system manager about it."
       << LogIO::EXCEPTION;
    return false;
  }

  // Now look for optional columns.
  Vector<String> foundoptcols;
  uint32_t noptcolsfound = 0;
  for(uint32_t i = 0; i < optcols.nelements(); ++i){
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
  for(uint32_t i = 0; i < reqcols.nelements(); ++i)
    rfp[i] = RORecordFieldPtr<double>(row.record(), reqcols[i]);
  for(uint32_t i = 0; i < noptcolsfound; ++i)
    rfp[reqcols.nelements() + i] = RORecordFieldPtr<double>(row.record(), optcols[i]);
  return true;
}

// Helper function for getTable().
bool MeasIERS::findTab(Table& tab, const Table *tabin, const String &rc,
                       const String &dir, const String &name)
{
  bool ok = true;
  LogIO os(LogOrigin("MeasIERS", "findTab", WHERE));
  
  if(!tabin){                                // No table object given: search name
    String ldir;
    Vector<String> searched;

    if(name[0] == '/'){                        // Absolute path given.
      ldir = "";
    }
    else{
      const String path[2] = {
        "/ephemerides/",
        "/geodetic/"
      };

      bool found = false;
      const std::string &measures_data = AppStateSource::fetch( ).measuresDir( );
      if ( measures_data.size( ) > 0 ) {
        for (int32_t i=0; i<2; i++) {
          Path mpath = Path(measures_data + "/" + (std::string) path[i]);
          ldir = mpath.absoluteName()+"/";
          searched.resize(searched.nelements()+1, true);
          searched[searched.nelements()-1] = ldir;
          if  (Table::isReadable(ldir+name)) {
            found = true;
            break;
          }
        }
          if ( found == false ) {
            throw(AipsError(std::string("Measures directory specified which does not contain the IERS data: ") + measures_data));
          }
      }

      if ( found == false ) {
        const std::list<std::string> &state_path = AppStateSource::fetch( ).dataPath( );
        if ( state_path.size( ) > 0 ) {
          String mdir;
          for ( std::list<std::string>::const_iterator it=state_path.begin(); ! found && it != state_path.end(); ++it ) {
            for (int32_t i=0; i<2; i++) {
              Path mpath = Path(*it + "/" + (std::string) path[i]);
              ldir = mpath.absoluteName()+"/";
              searched.resize(searched.nelements()+1, true);
              searched[searched.nelements()-1] = ldir;
              if  (Table::isReadable(ldir+name)) {
                found = true;
                break;
              }
            }
          }
        } else if ( ! found ) {

          if (Aipsrc::find(ldir, rc)){
            ldir += '/';
            searched.resize(searched.nelements() + 1, true);
            searched[searched.nelements() - 1] = ldir;
          }
          else {
            String udir;

            if(!dir.empty()) {
              udir = dir + '/';
            }

            String mdir;
            if (Aipsrc::find(mdir, "measures.directory")) {
              mdir.trim();
              Path mpath = Path(mdir);
              mpath.append(udir);
              for (int32_t i=0; i<2; i++) {
                Path mpath = Path(mdir +"/" + path[i]);
                ldir = mpath.absoluteName()+"/";
                searched.resize(searched.nelements()+1, true);
                searched[searched.nelements()-1] = ldir;
                if  (Table::isReadable(ldir+name)) {
                  found = true;
                  break;
                }
              }
            }
            if (!found) {
              String casadata=String(CASADATA);
              casadata.gsub("%CASAROOT%", Aipsrc::aipsRoot());
              casadata.gsub("%CASAHOME%", Aipsrc::aipsHome());
              Path cdatapath(casadata);
              for (int32_t i=0; i<2; i++) {
                ldir = cdatapath.absoluteName() + path[i];
                searched.resize(searched.nelements() + 1, true);
                searched[searched.nelements() - 1] = ldir;
                if (Table::isReadable(ldir + name)) {
                  found = true;
                  break;
                }
              }
            }
          }
        }
      }
    }
    if(!Table::isReadable(ldir + name)){
      os << LogIO::WARN
         << "Requested data table " << name
         << " cannot be found in the searched directories:\n";
      for(uint32_t i = 0; i < searched.nelements(); ++i) {
        os << searched[i] << "\n";
      }
      os << LogIO::POST;
      return false;
    }
    tab = Table(ldir + name);
  } else {
    tab = *tabin;
  }

  return ok;
}

// Helper function for getTable().
bool MeasIERS::handle_keywords(double &dt, String &vs, const TableRecord& ks,
                               const Table& tab)
{
  LogIO os(LogOrigin("MeasIERS", "handle_keywords", WHERE));
  bool ok = true;
  
  if(!ks.isDefined("VS_DATE") || !ks.isDefined("VS_VERSION") ||
     !ks.isDefined("VS_CREATE") || !ks.isDefined("VS_TYPE") ||
     (tab.tableInfo().type() != "IERS")) {
    ok = false;
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
      ok = false;
    }
  }
  return ok;
}

  
} //# NAMESPACE CASACORE - END
