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

//# Forward Declarations
class String;
class MVEpoch;

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
  // Construct a table from the named path.
  explicit MeasComet(const String &path);

  //# Destructor
  ~MeasComet();

  //# General Member Functions
  // Get the values from a comet table, interpolated for date(in MJD(TDB)).
  Bool get(Vector<Double> &returnValue,
	   const MVEpoch &date);
  // Get indicated special value interpolated for  date(in MJD(TDB)).
  Bool get(Double &res, MeasComet::Types which,
	   const MVEpoch &date);

 private:
  
  //# Constructors

  // Copy constructor, NOT defined
  MeasComet(const MeasComet &other);
  // Copy assign, NOT defined
  MeasComet &operator=(const MeasComet &other);
  
  
  //# General member functions

  //# Data members
  // Actual table
  Table tab_p;
};

//# Inline Implementations

#endif
