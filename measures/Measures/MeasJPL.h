//# MeasJPL.h: Interface to JPL DE tables
//# Copyright (C) 1996,1997,1998,1999,2002
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

#ifndef MEASURES_MEASJPL_H
#define MEASURES_MEASJPL_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableRow.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/OS/Mutex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class String;
class MVEpoch;

// <summary> Interface to JPL DE tables </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasJPL" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=MeasTable>MeasTable</linkto>
// </prerequisite>
//
// <etymology>
// From Measure and JPL
// </etymology>
//
// <synopsis>
// MeasJPL is the interface class to the JPL DE planetary data.
// It has only static memebers.<br>
// Tables are found using the aipsrc 
// (using <src>measures.<table>.directory</src>)
// mechanism. If not provided they are assumed to reside in standard places
// (i.e. in $AIPSROOT/data/ephemerides) Tables are assumed to have the
// VS_VERSION, VS_DATE, VS_CREATE and VS_TYPE keywords, and be of type IERS,
// else an exception will be thrown.<br>
// The <src>get()</src> method will obtain data from the JPL planetary
// tables (i.e. the <src>DE200</src> and
// the <src>DE405</src> tables). The data obtained will be the barycentric
// position (AU) and velocity (AU/d) of planets; the nutation (rad, rad/d)
// or the libration (rad, rad/d; DE405 only). All in the J2000 system.<br>
// The JPL DE Tables have a large set of constants attach to it. Some
// will be available by their own special code, the others their filed name.
// (See the <src>get</src> functions.<br>
// The enumeration code gives the available data and planets. See
// E.M. Standish et al., JPL IOM 314.10 - 127 for further details.
// <br>
// Note that the normal usage of these tables is through the Measures system.
// 
// <note>
// 	A message is Logged (once) if a table cannot be found.
//	A message is logged (once) if a date outside the range in
//	the Tables is asked for. 
// </note>
// <thrown>
//     <li> AipsError if table opened has wrong format or otherwise corrupted.
// </thrown>
// </synopsis>
//
// <example>
// <srcblock>
//	#include <casacore/casa/aips.h>
//	#include <casacore/casa/Quanta/MVEpoch.h>
//     	#include <casacore/measures/Measures/MeasJPL.h>
//	#include <casacore/casa/Arrays/Vector.h>
//    	const MVEpoch dat = 51116; // a date (1998/10/30) in TDB
//	Vector<Double> val(6), valE(6);		// results
//	// Get position and velocity of Venus (barycentric)
//	if (!MeasJPL::get(val, MeasJPL::DE200, MeasJPL::VENUS, dat)) {
//		cout << "Some error getting Venus position" << endl;
//	// Get Earth position and velocity (barycentric)
//	} else if (!MeasJPL::get(valE, MeasJPL::DE200, MeasJPL::VENUS, dat)) {
//		cout << "Some error getting Earth position" << endl;
//	} else {
//		cout << "Venus (geocentric): " << (val-valE) << endl;
//	};
// </srcblock>
// </example>
//
// <motivation>
// To use the JPL data for planetary positions and high precision nutation
// </motivation>
//
// <todo asof="1998/08/24">
// </todo>

class MeasJPL {	

public:
  //# Constants
  
  //# Enumerations
  // Types of known data
  enum Types {
    // MJD (must be first in list)
    MJD,
    // Column with data
    X,
    // Number of columns
    N_Columns,
    // Planets
    MERCURY = 1,
    VENUS = 2,
    EARTH = 3,
    MARS = 4,
    JUPITER = 5,
    SATURN = 6,
    URANUS = 7,
    NEPTUNE = 8,
    PLUTO = 9,
    MOON = 10,
    SUN = 11,
    // Solar system barycentre
    BARYSOLAR = 12,
    // Earth-Moon system barycentre
    BARYEARTH = 13,
    // Nutations
    NUTATION = 14,
    // Librations
    LIBRATION = 15,
    // Number of types
    N_Types };
  
  // Types of files
  enum Files {
    // DE200
    DE200,
    // DE405
    DE405,
    // # of known types
    N_Files,
    // Default
    DEFAULT = DE200 };
  
  // Codes for special constants
  enum Codes {
    // Light velocity used in AU/d
    CAU,
    // Solar mass (GM0)/c<sup>2</sup> in AU
    GMS,
    // AU in km
    AU,
    // Solar radius in AU
    RADS,
    // # of codes
    N_Codes };

  
  //# General Member Functions
  // Get the values from a DE table, interpolated for date(in MJD(TDB)).
  // The file can be DE200 or DE405, the type as given in enum.
  static Bool get(Vector<Double> &returnValue,
		  MeasJPL::Files file, 
		  MeasJPL::Types type,
		  const MVEpoch &date);
  // Get indicated special constant
  static Bool getConst(Double &res, MeasJPL::Files which,
		       MeasJPL::Codes what);
  // Get filed constant with name nam
  static Bool getConst(Double &res, MeasJPL::Files which,
		       const String &nam);

  // Close the set of JPL tables only
  static void closeMeas();

private:
  
  //# Constructors
  // Default constructor, NOT defined
  MeasJPL();
  
  // Copy assign, NOT defined
  MeasJPL &operator=(const MeasJPL &other);
  
  //# Destructor
  //  Destructor, NOT defined and not declared to stop warning
  // ~MeasJPL();
  
  //# General member functions
  // Initialise tables
  static Bool initMeas(MeasJPL::Files which);
  static Bool doInitMeas(MeasJPL::Files which);
  // Get a pointer to the data for the given date. It reads the data if needed.
  static const Double* fillMeas(Double &intv, MeasJPL::Files which,
                                const MVEpoch &utf);
  // Interpolate Chebyshev polymomial to res
  static void interMeas(Double res[], MeasJPL::Files  which, Double intv, 
			Double ivf, Int ncf, Int ncm, Int na, 
			const Double buf[]);

  //# Data members
  // Measured data readable
  static volatile Bool needInit[N_Files];
  // Tables present
  static Table t[N_Files];
  // Data column descriptor
  static ArrayColumn<Double> acc[N_Files];
  // First (-1) MJD in list
  static Int mjd0[N_Files];
  // Last MJD in list
  static Int mjdl[N_Files];
  // Interval in days (i.e., date step between subsequent rows)
  static Int dmjd[N_Files];
  // File names
  static const String tp[N_Files];
  // Index in record
  static Int idx[N_Files][3][13];
  // Dates of the data read in buffer.
  static vector<Int> curDate[N_Files];
  // Data read in.
  static vector<Vector<Double> > dval[N_Files];
  // Some helper data read from the table keywords
  // <group>
  static Double aufac[N_Files];
  static Double emrat[N_Files];
  static Double cn[N_Files][N_Codes];
  // </group>
  // Mutex for thread-safety.
  static Mutex theirMutex;
};

//# Inline Implementations


} //# NAMESPACE CASACORE - END

#endif
