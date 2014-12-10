//# MeasComet.h: To define position for comets and other solar system bodies
//# Copyright (C) 1999,2000,2002,2007
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

#ifndef MEASURES_MEASCOMET_H
#define MEASURES_MEASCOMET_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableRow.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Quanta/MVPosition.h>
#include <casacore/measures/Measures/MDirection.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MVRadialVelocity;
class MVDirection;
template <class T> class Vector;

// <summary>Position for comets and other solar system bodies</summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasComet" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=MeasTable>MeasTable</linkto>
// </prerequisite>
//
// <etymology>
// From Measure and Comet
// </etymology>
//
// <synopsis>
// MeasComet is the interface class between generated Comet position
// tables and the Direction conversion machinery.
// Tables are found using the aipsrc 
// (using <src>measures.<table>.directory</src>)
// mechanism. If not provided they are assumed to reside in standard places
// Tables are assumed to have the
// VS_VERSION, VS_DATE, VS_CREATE, VS_TYPE,
// MJD0 (first MJD in table - 1.0 * dMJD, >= 10000),
// dMJD (increment between successive MJDs, in days, > 0),
// and NAME
// keywords, be gapless (constant dMJD), and be of type IERS,
// or else an exception will be thrown.<br>
// They are also assumed to have the MJD, RA, DEC, Rho, and RadVel columns.
// The DiskLong and DiskLat columns can be used if they are present, but they
// are no longer expected.
// The <src>get()</src> method will obtain data from the cometary
// tables. The data obtained will be in the specified frame.
// Note that the normal usage of these tables is through the Measures system.
// 
// <note>
//	A message is logged (once) if a date outside the range in
//	the Tables is asked for. 
// </note>
// <thrown>
//     <li> AipsError if table opened has wrong format or otherwise corrupted.
// </thrown>
// </synopsis>
//
// <example>
// See test/tMeasComet.cc.
// <srcblock>
//   tbd
// </srcblock>
// </example>
//
// <motivation>
// To use the JPL data for positions of solar system bodies
// </motivation>
//
// <todo asof="2000/01/20">
// </todo>

class MeasComet {	

 public:
  //# Constants
  
  //# Enumerations
  // Types of known data
  enum Types {
    // MJD (must be first in list)
    MJD,
    // Columns with data
    RA, 
    DEC,
    RHO,
    RADVEL,
    DISKLONG,
    DISKLAT,
    // Number of columns
    N_Columns,
    N_Types };
  
  //# Constructors
  // Construct using the aipsrc value (measures.comet.file)
  MeasComet();
  // Construct a table from the named path.
  explicit MeasComet(const String &path);
  // Construct a table from the name and the input table
  MeasComet(const Table &tabin, const String &path);
  // Copy constructor
  MeasComet(const MeasComet &other);
  // Copy assign
  MeasComet &operator=(const MeasComet &other);

  //# Destructor
  ~MeasComet();

  //# General Member Functions
  // Is it a valid comet class (i.e. can it be used)
  Bool ok() const {return measured_p;} ;
  // Get the name of the comet
  const String &getName() const;
  // Get the topo position
  const MVPosition &getTopo() const;
  // Get the direction type
  MDirection::Types getType() const;
  // Get the start of the table (in MJD)
  Double getStart() const;
  // Get the end of the table (in MJD)
  Double getEnd() const;
  // Get number of entries
  Int nelements() const;
  // Get a comet position
  Bool get(MVPosition &returnValue, Double date) const;
  // Get the local on-disk direction.  Returns False if the time or sub-observer
  // longitude and latitude are unavailable, True on success.
  Bool getDisk(MVDirection &returnValue, Double date) const;
  // Get the velocity from a comet table, interpolated for date(in MJD(TDB)).
  Bool getRadVel(MVRadialVelocity &returnValue, Double date) const;

  // Return the temperature in K, or -1 if the table does not have it.
  // If squawk is true an error message will also be posted.
  Double getTemperature(const Bool squawk);

  // Return the mean radius in AU, or -1 if the table does not have it.
  // If squawk is true an error message will also be posted.
  Double getMeanRad(const Bool squawk);  

  // Create a clone
  MeasComet *clone() const;

  // Close the Comet tabls only
  void closeMeas();

  // Convenience function that returns ks[kw] in units of unit, setting
  // success.
  static Double get_Quantity_keyword(const TableRecord& ks, const String& kw,
				     const Unit& unit, Bool& success);

  // Convenience function that returns the absolute path to the ephemeris table
  // connected to the MeasComet object
  String getTablePath();

 private:
  
  //# General member functions
  // Initialise table from the name given
  Bool initMeas(const String &which, const Table *tabin=0);
  // Fill Table lines
  Bool fillMeas(Double utf) const;

  // Helper functions for accessing ldat_p.  index should be either 0 or 1, but
  // that isn't checked!
  MVPosition getRelPosition(const uInt index) const;
  MVDirection getDiskLongLat(const uInt index) const;  // Must not be called if !haveDiskLongLat_p

  // Try to read mean_rad_p and temperature_p, returning whether or not it was
  // successful.  (but the real mark of success is whether or not they are
  // positive.)
  // It sets haveTriedExtras_p to true and will return right away if it is
  // already true.
  Bool getExtras();

  //# Data members

  // Initialized in the "initialization list" of the c'tors, so maintain order:

  // Actual table
  Table tab_p;
  // Measured data readable
  Bool measFlag_p;
  // Measured data present
  Bool measured_p;
  // Row descriptions
  ROTableRow row_p;
  // First MJD in list - 1.0 * dmjd_p
  Double mjd0_p;
  // Last MJD in list
  Double mjdl_p;
  // Increment in rows
  Double dmjd_p;
  // Number of rows
  Int nrow_p;
  // Name of comet
  String name_p;
  // Position on Earth
  MVPosition topo_p;
  // Type of coordinates
  MDirection::Types mtype_p;
  // Message given
  Bool msgDone_p;
  // File names
  String tp_p;

  // Whether or not the sub-observer longitude and latitude are available.
  Bool haveDiskLongLat_p;

  uInt ncols_p;	// # of columns.

  // These may be initialized _inside_ the c'tors, but the order here is
  // unimportant:

  // Field pointers
  Vector<RORecordFieldPtr<Double> > rfp_p;
  // Lines in memory
  mutable Int lnr_p[2];			    // Why are these mutables here?
  // Last read data (measlow - meashigh)
  mutable Vector<Double> ldat_p[2];         // They allow declaring a const
					    // which isn't.
  Bool haveTriedExtras_p;
  Double temperature_p;
  Double mean_rad_p;
};

//# Inline Implementations


} //# NAMESPACE CASACORE - END

#endif
