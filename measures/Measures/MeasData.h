//# MeasData.h: MeasData provides Measure computing data
//# Copyright (C) 1995,1996,1997,1999,2003
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

#ifndef MEASURES_MEASDATA_H
#define MEASURES_MEASDATA_H

//# Includes
#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class RotMatrix;

// <summary>
// MeasData provides Measure computing data
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasMath" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Measure>Measure</linkto> class 
// </prerequisite>
//
// <etymology>
// MeasData from Measure and Data
// </etymology>
//
// <synopsis>
// MeasData contains the constant data 
// necessary for precession, nutation and other 
// <linkto class=Measure>Measure</linkto> related calculations.<br>
// Database (Table) related data, or data that can be changed by the user,
// is available in the <linkto class=MeasTable>MeasTable</linkto> class. <br>
// All data. apart from a set of simple constants:
// <srcblock>
// 	MeasData::MJD2000
// 	MeasData::MJDB1950 
// 	MeasData::MJDB1900 
// 	MeasData::MJDB1850 
// 	MeasData::TROPCEN 
// 	MeasData::JDCEN 
// 	MeasData::SECinDAY
// </srcblock>
// are obtained by calls to a method.
// This class contains no constructors or destructors, only static
// methods and (static) constants.
// <br> References:<br> Explanatory supplements to the Astronomical Almanac
// <br> C. Ron and J. Vondrak, Bull. Astron. Inst. Czechosl. 37, p96, 1986
// <br> M. Soma, Th. Hirayama and H. Kinoshita, Celest. Mech. 41, p389, 1988
// <br> V.S. Gubanov, Astron. Zh. 49, p1112, 1972 (English translation:
//      Sov. Astronomy - AJ, Vol. 16, No. 5, p. 907)
// </synopsis>
//
// <example>
// Usage examples can be found in <linkto class=Precession>Precession</linkto>
// </example>
//
// <motivation>
// To create a clean interface between the actual calculations and the
// methods to obtain the parameters for these calculations. Note that the
// tables are in general in the format and units found in the literature. This
// is to be able to easy check and change them. However, in the future
// re-arrangement could produce faster and more compact code.
// </motivation>
//
// <todo asof="1997/04/17">
//   <li> more precise data for VLBI and pulsar
// </todo>

class MeasData {

public:
  
  //# 	Constants
  // General constants
  // <group>
  // MJD of J2000.0
  static const Double MJD2000;
  // MJD of B1950.0
  static const Double MJDB1950;
  // MJD of B1900.0
  static const Double MJDB1900;
  // MJD of B1850.0
  static const Double MJDB1850;
  // Length Tropical century
  static const Double TROPCEN;
  // Length Julian century
  static const Double JDCEN;
  // Length of day in sec
  static const Double SECinDAY;
  // </group>
  
  //# General Member Functions

  // Get the rotation matrices for galactic coordinates
  // <group>
  static const RotMatrix &GALtoB1950();
  static const RotMatrix &GALtoJ2000();
  static const RotMatrix &J2000toGAL();
  static const RotMatrix &B1950toGAL();
  // </group>

  // Get one of the 4 3x3 sub rotation matrices for B1950-J2000 conversions
  // <group>
  static const RotMatrix &MToB1950(uInt which);
  static const RotMatrix &MToJ2000(uInt which);
  // </group>

  // Get the solar semi diameter at 1 AU in rad
  static Double SunSemiDiameter();

  // J2000 obliquity
  static Double eps0J2000();
  
private:
  
  //# Constructors
  // Default constructor, NOT defined
  MeasData();
  
  // Copy assign, NOT defined
  MeasData &operator=(const MeasData &other);
  
  //# Destructor
  //  Destructor (NOT defined) and not declared to stop warning
  // ~MeasData();
  //# General member functions
};


} //# NAMESPACE CASACORE - END

#endif
