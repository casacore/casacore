//# MeasComet.h: To define position for comets and other solar system bodies
//# Copyright (C) 1999, 2000
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

#if !defined(AIPS_MEASCOMET_H)
#define AIPS_MEASCOMET_H

//# Includes
#include <aips/aips.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableRow.h>
#include <aips/Containers/RecordField.h>
#include <aips/Utilities/String.h>
#include <aips/Quanta/MVPosition.h>
#include <aips/Measures/MDirection.h>

//# Forward Declarations
class MVRadialVelocity;
class MVDirection;
template <class T> class Vector;

// <summary>Position for comets and other solar system bodies</summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tMeasComet" demos="">
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
// VS_VERSION, VS_DATE, VS_CREATE and VS_TYPE keywords, and be of type IERS,
// else an exception will be thrown.<br>
// The <src>get()</src> method will obtain data from the cometary
// tables. The data obtained will be in the specified frame.
// Note that the normal usage of these tables is through the Measures system.
// 
// <logged>
//	<li> A message is logged (once) if a date outside the range in
//	the Tables is asked for. 
// </logged>
// <thrown>
//     <li> AipsError if table opened has wrong format or otherwise corrupted.
// </thrown>
// </synopsis>
//
// <example>
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
  // Get the local on-disk direction
  Bool getDisk(MVDirection &returnValue, Double date) const;
  // Get the velocity from a comet table, interpolated for date(in MJD(TDB)).
  Bool getRadVel(MVRadialVelocity &returnValue, Double date) const;
  // Create a clone
  MeasComet *clone() const;

 private:
  
  //# General member functions
  // Initialise table from the name given
  Bool initMeas(const String &which);
  // Fill Table lines
  Bool fillMeas(Double utf) const;

  //# Data members
  // Actual table
  Table tab_p;
  // Measured data readable
  Bool measFlag_p;
  // Measured data present
  Bool measured_p;
  // Row descriptions
  ROTableRow row_p;
  // Field pointers
  RORecordFieldPtr<Double> rfp_p[MeasComet::N_Columns];
  // First (-1) MJD in list
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
  // Type of ccordinates
  MDirection::Types mtype_p;
  // Lines in memory
  mutable Int lnr_p[2];
  // Last read data (measlow - meashigh)
  mutable Double ldat_p[2][N_Columns];
  // Message given
  Bool msgDone_p;
  // File names
  String tp_p;
};

//# Inline Implementations

#endif
