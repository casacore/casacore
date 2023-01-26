//# MeasComet.cc: To define position for comets and other solar system bodies
//# Copyright (C) 2000-2002,2007
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
#include <casacore/measures/Measures/MeasComet.h>
#include <casacore/measures/Measures/MeasIERS.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Quanta/MVRadialVelocity.h>
#include <casacore/casa/Quanta/MVDirection.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/System/Aipsrc.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/System/Aipsrc.h>
#include <casacore/casa/OS/Path.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors
MeasComet::MeasComet() :
  tab_p(), measFlag_p(true), measured_p(false),
  row_p(),
  mjd0_p(0), mjdl_p(0), dmjd_p(0), nrow_p(0), name_p(), topo_p(),
  mtype_p(MDirection::APP), // default, if the keyword obsloc is not defined, is apparent geocentric
  msgDone_p(false), tp_p(),
  haveDiskLongLat_p(false),
  ncols_p(5), hasPosrefsys_p(false),  posrefsystype_p(MDirection::APP)
{
  String path;
  if (Aipsrc::find(path, String("measures.comet.file"))) initMeas(path);
  for (uint32_t i=0; i<2; i++) lnr_p[i] = -1;
}

MeasComet::MeasComet(const String &path) :
  tab_p(), measFlag_p(true), measured_p(false),
  row_p(),
  mjd0_p(0), mjdl_p(0), dmjd_p(0), nrow_p(0), name_p(), topo_p(),
  mtype_p(MDirection::APP),
  msgDone_p(false), tp_p(path),
  haveDiskLongLat_p(false),
  ncols_p(5), hasPosrefsys_p(false),  posrefsystype_p(MDirection::APP)
{
  initMeas(path);
  for (uint32_t i=0; i<2; i++) lnr_p[i] = -1;
}

MeasComet::MeasComet(const Table &tabin, const String &path) :
  tab_p(), measFlag_p(true), measured_p(false),
  row_p(),
  mjd0_p(0), mjdl_p(0), dmjd_p(0), nrow_p(0), name_p(), topo_p(),
  mtype_p(MDirection::APP),
  msgDone_p(false), tp_p(path),
  haveDiskLongLat_p(false),
  ncols_p(5), hasPosrefsys_p(false),  posrefsystype_p(MDirection::APP)
{
  initMeas(path, &tabin);
  for (uint32_t i=0; i<2; i++) lnr_p[i] = -1;
}

MeasComet::MeasComet(const MeasComet &other) :
  tab_p(), measFlag_p(true), measured_p(false),
  row_p(),
  mjd0_p(0), mjdl_p(0), dmjd_p(0), nrow_p(0), name_p(), topo_p(),
  mtype_p(MDirection::APP),
  msgDone_p(false), tp_p(other.tp_p),
  haveDiskLongLat_p(other.haveDiskLongLat_p),
  ncols_p(other.ncols_p), hasPosrefsys_p(other.hasPosrefsys_p),  posrefsystype_p(other.posrefsystype_p)
{
  initMeas(other.tp_p);
  for (uint32_t i=0; i<2; i++) lnr_p[i] = -1;
}

MeasComet &MeasComet::operator=(const MeasComet &other) {
  if (this != &other) {
    initMeas(other.tp_p);
    for (uint32_t i=0; i<2; i++) lnr_p[i] = -1;
  }
  return *this;
}

MeasComet::~MeasComet() {}

//# Member functions
const String &MeasComet::getName() const {
  return name_p;
}

const MVPosition &MeasComet::getTopo() const {
  return topo_p;
}

MDirection::Types MeasComet::getType() const {
  return mtype_p;
}

double MeasComet::getStart() const {
  return mjd0_p + dmjd_p;
}

double MeasComet::getEnd() const {
  return mjdl_p;
}

int32_t MeasComet::nelements() const {
  return nrow_p;
}
 
  bool MeasComet::hasPosrefsys() const {
    return hasPosrefsys_p;
  }
  MDirection::Types MeasComet::getPosrefsysType() const {

    return posrefsystype_p;
  }

bool MeasComet::get(MVPosition &returnValue, double date) const {
  if(!fillMeas(date)){
    returnValue = MVPosition();
    return false;
  }
  
  double f = (date - ldat_p[0][0])/dmjd_p;

  returnValue = getRelPosition(0);
  const MVPosition deltaX(getRelPosition(1) - returnValue);
  returnValue += f * deltaX;

  return true;
}

MVPosition MeasComet::getRelPosition(const uint32_t index) const
{
  return MVPosition(Quantity(ldat_p[index][MeasComet::RHO], "AU"),
                    Quantity(ldat_p[index][MeasComet::RA], "deg"),
                    Quantity(ldat_p[index][MeasComet::DEC], "deg"));
}

bool MeasComet::getDisk(MVDirection &returnValue, double date) const {
  if(!haveDiskLongLat_p || !fillMeas(date)){
    returnValue = MVDirection();
    return false;
  }
  
  double f = (date - ldat_p[0][0])/dmjd_p;
  returnValue = getDiskLongLat(0);
  const MVDirection ll_on_second_date(getDiskLongLat(1));
  double sep = returnValue.separation(ll_on_second_date);
  double pa = returnValue.positionAngle(ll_on_second_date);
  
  returnValue.shiftAngle(f * sep, pa);
  return true;
}

MVDirection MeasComet::getDiskLongLat(const uint32_t index) const
{
  return MVDirection(Quantity(ldat_p[index][MeasComet::DISKLONG], "deg"),
                     Quantity(ldat_p[index][MeasComet::DISKLAT], "deg"));
}

bool MeasComet::getRadVel(MVRadialVelocity &returnValue, double date) const {
  returnValue = 0.0;
  if (!fillMeas(date)) return false;
  double f = (date - ldat_p[0][0])/dmjd_p;
  double radvel = ldat_p[0][MeasComet::RADVEL];
  double deltarv = ldat_p[1][MeasComet::RADVEL] - radvel;
  
  radvel += f * deltarv;
  returnValue = MVRadialVelocity(Quantity(radvel, "AU/d"));
  return true;
}

MeasComet *MeasComet::clone() const {
  return (new MeasComet(*this));
}

bool MeasComet::initMeas(const String &which, const Table *tabin) {
  Vector<String> reqcols(5);  // Required columns.
  reqcols[0] = "MJD";
  reqcols[1] = "RA";
  reqcols[2] = "DEC";  
  reqcols[3] = "Rho";                    // Distance from Earth in AU.
  reqcols[4] = "RadVel";		 // AU/d
  Vector<String> optcols(2);  // Get these columns if the table has them.
  optcols[0] = "DiskLong";    // The positions of surface features may be 
  optcols[1] = "DiskLat";     // neither known nor needed.
  static const String tplc = "measures.comet.directory";

  if (!measured_p && measFlag_p) {
    LogIO os(LogOrigin("MeasComet", String("initMeas(String, Table *)"),
		       WHERE));

    closeMeas(); // seems to need this to ensure full initialization (TT) 
    measFlag_p = false;
    tp_p = which;
    TableRecord kws;
    double dt;
    String vs;
    bool ok = true;
    if (!MeasIERS::getTable(tab_p, kws, row_p,
			    rfp_p, vs, dt, 
			    reqcols, optcols, tp_p,
			    tplc,
      			    String("ephemerides"), tabin)) {
      return false;
    }

    ncols_p = reqcols.nelements() + optcols.nelements();
    ldat_p[0].resize(ncols_p);
    ldat_p[1].resize(ncols_p);
    
    // Make this more sophisticated if the number of optional columns grows.
    // That could also cause problems where enums like MeasComet::DiskLong are
    // used in ldat_p.
    haveDiskLongLat_p = (optcols.nelements() == 2);
    
    if (!kws.isDefined("MJD0") || kws.asDouble("MJD0") < 10000 ||
	!kws.isDefined("dMJD") || kws.asDouble("dMJD") <= 0 ||
	!kws.isDefined("NAME")){
      ok = false;
      os << LogIO::SEVERE;
      if(!kws.isDefined("MJD0"))
	os << "MJD0 is not defined.\n";
      else if(kws.asDouble("MJD0") < 10000)
	os << "MJD0, " << kws.asDouble("MJD0") << " is < 10000.\n";
      if(!kws.isDefined("dMJD"))
	os << "dMJD is not defined.\n";
      else if(kws.asDouble("dMJD") <= 0.0)
	os << "dMJD, " << kws.asDouble("dMJD") << " is < 0.\n";
      if(!kws.isDefined("NAME"))
	os << "NAME is not defined.";
      os << LogIO::POST;
    }
    if (ok) {
      name_p = kws.asString("NAME");
      topo_p = MVPosition(Quantity(kws.asDouble("GeoDist"), "km"),
			  Quantity(kws.asDouble("GeoLong"), "deg"),
			  Quantity(kws.asDouble("GeoLat"), "deg"));
      if (kws.isDefined("posrefsys")) {
	String prs = kws.asString("posrefsys");
	prs.upcase();
	if(prs.contains("J2000")){
	  mtype_p = MDirection::J2000;
	}else if(prs.contains("B1950")){
	  mtype_p = MDirection::B1950;	
	}else if(prs.contains("APP")){
	  mtype_p = MDirection::APP;
	}else if(prs.contains("ICRS")){
	  mtype_p = MDirection::ICRS;
	}else if(prs.contains("TOPO")){
	  mtype_p = MDirection::TOPO;
	}else{
	  os << LogIO::SEVERE
             << "Unrecognized position reference frame (posrefsys): "
	     << kws.asString("posrefsys")
             << " - possible are J2000, B1950, APP, ICRS, TOPO" << LogIO::POST;
	}
	posrefsystype_p=mtype_p;
	hasPosrefsys_p=true;
      } else if (kws.asDouble("GeoDist") != 0.0){
	mtype_p = MDirection::TOPO;
      }
	  
      mjd0_p = kws.asDouble("MJD0");
      dmjd_p = kws.asDouble("dMJD");
      nrow_p = tab_p.nrow();
      row_p.get(nrow_p-1);
      if (!nearAbs(*(rfp_p[0]), mjd0_p + nrow_p*dmjd_p, 0.1*dmjd_p)) { 
	os << LogIO::SEVERE << "MJD has a problem." << LogIO::POST;
	os << LogIO::DEBUG1
	   << "*(rfp_p[0]) = " << *(rfp_p[0])
	   << "\nmjd0_p = " << mjd0_p
	   << "\nnrow_p = " << nrow_p
	   << "\ndmjd_p = " << dmjd_p
	   << LogIO::POST;
	ok = false;
      } else {
	mjdl_p = mjd0_p + nrow_p*dmjd_p;
      }
    }
    if (!ok) {
      os << String("Invalid comet table ") + tp_p << LogIO::EXCEPTION;
    }
    measured_p = true;
  }

  haveTriedExtras_p = false;	// Defer reading them until asked to.

  return (measured_p);
}

double MeasComet::getTemperature(const bool squawk)
{
  if(!haveTriedExtras_p)
    getExtras();

  if(temperature_p < 0.0 && squawk){
    LogIO os(LogOrigin("MeasComet", String("getTemperature(true)"), WHERE));

    os << LogIO::SEVERE
       << "The comet table is missing the T_mean keyword, which holds the temperature."
       << LogIO::POST;
  }
  return temperature_p;
}

double MeasComet::getMeanRad(const bool squawk)
{
  if(!haveTriedExtras_p)
    getExtras();

  if(mean_rad_p < 0.0 && squawk){
    LogIO os(LogOrigin("MeasComet", String("getMeanRad(true)"), WHERE));

    os << LogIO::SEVERE		// Remove/modify this when it starts supporting triaxiality.
       << "The table is missing the meanrad keyword, needed to calculate the apparent diameter."
       << LogIO::POST;
  }
  return mean_rad_p;
}

double MeasComet::get_Quantity_keyword(const TableRecord& ks,
				       const String& kw,
				       const Unit& unit,
				       bool& success)
{
  try{
    const Record rec(ks.asRecord(kw));
    const Quantity q(rec.asDouble("value"), rec.asString("unit"));
  
    success = true;
    return q.get(unit).getValue();
  }
  catch(...){
    success = false;
    return 0.0;
  }
}

String MeasComet::getTablePath()
{
  return Path(tab_p.tableName()).absoluteName();
}

bool MeasComet::getExtras() {
  if(haveTriedExtras_p)		// That was easy.
    return true;

  const TableRecord ks(tab_p.keywordSet());
  bool got_q = true;

  // Use impossible values to indicate failure to _successfully_ read any given
  // quantity.
  haveTriedExtras_p = true;

  temperature_p = get_Quantity_keyword(ks, "T_mean", "K", got_q);
  if(!got_q)
    temperature_p = -1;  // Hopefully a model for the obj will supply a
			 // temperature later.

  mean_rad_p = get_Quantity_keyword(ks, "meanrad", "AU", got_q);
  if(!got_q)
    mean_rad_p = -1.0;

  return true;
}

void MeasComet::closeMeas() {
  if (Table::isOpened(tp_p) || measured_p || !measFlag_p) {
    measFlag_p = true;
    measured_p = false;
    mjd0_p = 0;
    mjdl_p = 0;
    dmjd_p = 0;
    nrow_p = 0;
    tp_p   = "";
    msgDone_p = false;
    for (uint32_t i=0; i<2; ++i)  lnr_p[i] = -1;
    row_p = ROTableRow();
    tab_p = Table();
  }
}

bool MeasComet::fillMeas(double utf) const {
  int32_t ut = ifloor((utf-mjd0_p)/dmjd_p)-1;
  if (ut<0 || ut >= nrow_p-1) return false;
  if (ut != lnr_p[0]) {
    if (ut == lnr_p[1]) { 
      // Shift one
      for(uint32_t i = 0; i < ncols_p; ++i)
	ldat_p[0][i] = ldat_p[1][i];
      lnr_p[0] = lnr_p[1];
    } else {
      // Read first line
      row_p.get(ut);
      for(uint32_t i = 0; i < ncols_p; ++i)
	ldat_p[0][i] = *(rfp_p[i]);
      lnr_p[0] = ut;
    }
    // Read second line
    row_p.get(ut+1);
    for(uint32_t i = 0; i < ncols_p; ++i)
      ldat_p[1][i] = *(rfp_p[i]);
    lnr_p[1] = ut+1;
  }
  return true;
}

} //# NAMESPACE CASACORE - END

