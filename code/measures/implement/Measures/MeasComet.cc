//# MeasComet.cc: To define position for comets and other solar system bodies
//# Copyright (C) 2000
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
//#
//# $Id$

//# Includes
#include <aips/Measures/MeasComet.h>
#include <aips/Measures/MeasIERS.h>
#include <aips/Arrays/Vector.h>
#include <aips/Quanta/MVRadialVelocity.h>
#include <aips/Quanta/MVDirection.h>
#include <aips/Logging.h>
#include <aips/Tasking/Aipsrc.h>
#include <aips/Mathematics/Math.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tasking/Aipsrc.h>

//# Constructors
MeasComet::MeasComet() :
  tab_p(), measFlag_p(True), measured_p(False),
  row_p(),
  mjd0_p(0), mjdl_p(0), dmjd_p(0), nrow_p(0), name_p(), topo_p(),
  mtype_p(MDirection::APP),
  msgDone_p(False), tp_p() {
  String path;
  if (Aipsrc::find(path, String("measures.comet.file"))) initMeas(path);
  for (uInt i=0; i<2; i++) lnr_p[i] = -1;
}

MeasComet::MeasComet(const String &path) :
  tab_p(), measFlag_p(True), measured_p(False),
  row_p(),
  mjd0_p(0), mjdl_p(0), dmjd_p(0), nrow_p(0), name_p(), topo_p(),
  mtype_p(MDirection::APP),
  msgDone_p(False), tp_p(path) {
  initMeas(path);
  for (uInt i=0; i<2; i++) lnr_p[i] = -1;
}

MeasComet::MeasComet(const MeasComet &other) :
  tab_p(), measFlag_p(True), measured_p(False),
  row_p(),
  mjd0_p(0), mjdl_p(0), dmjd_p(0), nrow_p(0), name_p(), topo_p(),
  mtype_p(MDirection::APP),
  msgDone_p(False), tp_p(other.tp_p) {
  initMeas(other.tp_p);
}

MeasComet &MeasComet::operator=(const MeasComet &other) {
  if (this != &other) {
    initMeas(other.tp_p);
  };
  return *this;
}

MeasComet::~MeasComet() {}

//# Member functions
const String &MeasComet::getName() const {
  return name_p;
};

const MVPosition &MeasComet::getTopo() const {
  return topo_p;
};

MDirection::Types MeasComet::getType() const {
  return mtype_p;
};

Double MeasComet::getStart() const {
  return mjd0_p + dmjd_p;
}

Double MeasComet::getEnd() const {
  return mjdl_p;
}

Int MeasComet::nelements() const {
  return nrow_p;
};

Bool MeasComet::get(MVPosition &returnValue, Double date) const {
  returnValue = MVPosition();
  if (!fillMeas(date)) return False;
  Double f = (date - ldat_p[0][0])/dmjd_p;
  returnValue =
    MVPosition(Quantity(ldat_p[1][MeasComet::RHO], "AU"),
	       Quantity(ldat_p[1][MeasComet::RA], "deg"),
	       Quantity(ldat_p[1][MeasComet::DEC], "deg"))*f -
    MVPosition(Quantity(ldat_p[0][MeasComet::RHO], "AU"),
	       Quantity(ldat_p[0][MeasComet::RA], "deg"),
	       Quantity(ldat_p[0][MeasComet::DEC], "deg"))*(f-1);
  return True;
}

Bool MeasComet::getDisk(MVDirection &returnValue, Double date) const {
  returnValue = MVDirection();
  if (!fillMeas(date)) return False;
  Double f = (date - ldat_p[0][0])/dmjd_p;
  returnValue =
    MVDirection(Quantity(ldat_p[1][MeasComet::DISKLONG], "deg"),
	       Quantity(ldat_p[1][MeasComet::DISKLAT], "deg"))*f -
    MVDirection(Quantity(ldat_p[0][MeasComet::DISKLONG], "deg"),
	       Quantity(ldat_p[0][MeasComet::DISKLAT], "deg"))*(f-1);
  return True;
}

Bool MeasComet::getRadVel(MVRadialVelocity &returnValue, Double date) const {
  returnValue = 0.0;
  if (!fillMeas(date)) return False;
  Double f = (date - ldat_p[0][0])/dmjd_p;
  returnValue =
    MVRadialVelocity(Quantity(ldat_p[1][MeasComet::RADVEL]*f -
			      ldat_p[0][MeasComet::RADVEL]*(f-1.0),
			      "AU/d"));
  return True;
}

MeasComet *MeasComet::clone() const {
  return (new MeasComet(*this));
}

Bool MeasComet::initMeas(const String &which) {
  static const String names[MeasComet::N_Columns] = {
    "MJD",
    "RA", "DEC",
    "Rho", "RadVel", "DiskLong", "DiskLat" };
  static const String tplc = "measures.comet.directory";

  if (!measured_p && measFlag_p) {
    measFlag_p = False;
    tp_p = which;
    TableRecord kws;
    Double dt;
    String vs;
    Bool ok = True;
    if (!MeasIERS::getTable(tab_p, kws, row_p,
			    rfp_p, vs, dt, 
			    MeasComet::N_Columns, names, tp_p,
			    tplc,
			    String("aips/Measures"))) {
      return False;
    };
    if (!kws.isDefined("MJD0") || kws.asDouble("MJD0") < 10000 ||
	!kws.isDefined("dMJD") || kws.asDouble("dMJD") <= 0 ||
	!kws.isDefined("NAME")) ok = False;
    if (ok) {
      name_p = kws.asString("NAME");
      topo_p = MVPosition(Quantity(kws.asDouble("GeoDist"), "km"),
			  Quantity(kws.asDouble("GeoLong"), "deg"),
			  Quantity(kws.asDouble("GeoLat"), "deg"));
      if (kws.asDouble("GeoDist") != 0.0) mtype_p = MDirection::TOPO;
      mjd0_p = kws.asDouble("MJD0");
      dmjd_p = kws.asDouble("dMJD");
      nrow_p = tab_p.nrow();
      row_p.get(nrow_p-1);
      if (!nearAbs(*(rfp_p[0]), mjd0_p + nrow_p*dmjd_p, 0.1*dmjd_p)) { 
	ok = False;
      } else {
	mjdl_p = mjd0_p + nrow_p*dmjd_p;
      };
    };
    if (!ok) {
      LogIO os(LogOrigin("MeasComet",
			 String("initMeas(String)"),
			 WHERE));
      os << String("Corrupted Comet table ") + tp_p << LogIO::EXCEPTION;
    };
    measured_p = True;
  };
  return ToBool(measured_p);
}

Bool MeasComet::fillMeas(Double utf) const {
  Int ut = ifloor((utf-mjd0_p)/dmjd_p)-1;
  if (ut<0 || ut >= nrow_p-1) return False;
  if (ut != lnr_p[0]) {
    if (ut == lnr_p[1]) { 
      // Shift one
      for (Int i=0; i<MeasComet::N_Columns; i++) ldat_p[0][i] = ldat_p[1][i];
      lnr_p[0] = lnr_p[1];
    } else {
      // Read first line
      row_p.get(ut);
      for (Int i=0; i<MeasComet::N_Columns; i++) {
	ldat_p[0][i] = *(rfp_p[i]);
      };
      lnr_p[0] = ut;
    };
    // Read second line
    row_p.get(ut+1);
    for (Int i=0; i<MeasComet::N_Columns; i++) {
      ldat_p[1][i] = *(rfp_p[i]);
    };
    lnr_p[1] = ut+1;
  };
  return True;
}
