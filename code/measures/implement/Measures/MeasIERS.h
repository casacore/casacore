//# MeasIERS.h: Interface to IERS tables
//# Copyright (C) 1996
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

#if !defined(AIPS_MEASIERS_H)
#define AIPS_MEASIERS_H

#if defined(_AIX)
#pragma implementation ("MeasIERS.cc")
#endif

//# Includes
#include <aips/aips.h>
#include <aips/Measures/MeasDetail.h>

//# Forward Declarations
class MeasureReferenceData;
class String;

//# Constants (SUN compiler does not accept non-simple default arguments)

// <summary> Interface to IERS tables </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tMeasMath" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=MeasDetail>MeasDetail</linkto>
//   <li> <linkto class=MRefData>MRefData</linkto>
// </prerequisite>
//
// <etymology>
// From Measure and IERS
// </etymology>
//
// <synopsis>
// MeasIERS is the interface class to the IERS data obtained by the
// <linkto class=MRefData>MRefData</linkto> class.<br>
// It was written for speed reasons (enums in stead of Strings), and
// has only a single static instantiation.<br>
// MeasIERS looks at some <linkto class=MeasDetail>MeasDetail</linkto>
// values to determine actions:
// <ul>
//  <li> MeasIERS::B_NoTable : Do not use IERS tables to convert measures
//  <li> MeasIERS::B_UseNEOS : Use the NEOS tables in stead of the IERS_CB
//	(default)
//  <li> MeasIERS::B_ForcePredict : Use values from prediction tables
//	even if Measured table asked by program.
//  <li> MeasIERS::D_PredictTime : Use values from prediction tables if
//	(now - time) less than value given (default 5) (days)
// </ul>
// These values can be set in aipsrc as well in future.<br>
// <logged>
// 	<li> A message is Logged if an IERS table cannot be opened
// </logged>
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// To use the IERS data for time and nutation calculations
// </motivation>
//
// <todo asof="1996/07/09">
// </todo>

class MeasIERS {	
//# Friends
  friend class MeasIERS_init;
public:
//# Constants
// Default interval (days) over which no measured data is expected as yet
// if compared to 'now'
  static const Double INTV;
//# Enumerations
// Types of known data
  enum Types {X,
	      Y,
	      dUT1,
	      dUT1R,
	      D,
	      DR,
	      dPsi,
	      dEpsilon,
	      OmegaR,
	      dUTC_TAI,
	      dUT1R_TAI,
	      N_Types,
	      LeapSecond=dUTC_TAI};
// Known MeasDetails
    enum {
      BASE = MeasDetail::IERS_BASE,
      // Use Predict always
      B_ForcePredict 	= BASE + MeasDetail::BASE_B,
      // Use no table
      B_NoTable,
      // Use NEOS, iso IERS
      B_UseNEOS,
      // Expected time after which measured available
      D_PredictTime   	= BASE + MeasDetail::BASE_D
    };

// Types of files
  enum Files {MEASURED,
	      PREDICTED};

//# Constructors
// Default constructor generates interface to IERS tables
  MeasIERS();

// Destructor
  ~MeasIERS();

//# Operators

//# General Member Functions
  static Bool get(MeasIERS::Files file, 
	   MeasIERS::Types type,
	   Double date,
	   Double &returnValue);

private:
//# Constructors
// Copy constructor (not implemented)
  MeasIERS(const MeasIERS &other);

//# Operators
// Copy assignment (not implemented)
  MeasIERS &operator=(const MeasIERS &other);

//# General member functions
// Initial tables
// <group>
  static Bool initMeas();
  static Bool initPredict();
// </group>

//# Data members
// Data tables
// <group>
  static MeasureReferenceData *measured;
  static MeasureReferenceData *predicted;
// </group>
// Data tables readable
// <group>
  static Bool measFlag;
  static Bool predictFlag;
  static Double dateNow;
// </group>
};


// <summary>
// Class used to force construction of <linkto class=MeasIERS>MeasIERS</linkto>.
// </summary>

// <synopsis>
// A static object of this class is used to make sure that
// <linkto class=MeasIERS>MeasIERS</linkto>
// is constructed before it is needed, and therefore that its static data
// members are defined.  See Meyers, p. 47.
// </synopsis>

// <use visibility=local>

// <linkfrom anchor="MeasIERS_init" classes="MeasIERS">
//   <here>MeasIERS_init</here> --
// Class used to force construction of <linkto class=MeasIERS>MeasIERS</linkto>.
// </linkfrom>

class MeasIERS_init {
public:
  MeasIERS_init();
  ~MeasIERS_init();
private:
  static uShort count;
};


// <summary>
// Object used to force construction of 
// <linkto class=MeasIERS>MeasIERS</linkto>.
// </summary>

// <synopsis>
// This static object of the <linkto class=MeasIERS_init>MeasIERS_init</linkto>
// class is used to make sure that MeasIERS
// is constructed before it is needed, and therefore that its static data
// members are defined.  See Meyers, p. 47.
// </synopsis>

// <use visibility=local>

// <linkfrom anchor="MeasIERS initialization object" classes="MeasIERS MeasIERS_init">
//   <here>MeasIERS initialization object</here> --
// Object used to force construction of <linkto class=MeasIERS>MeasIERS</linkto>.
// </linkfrom>

// <group name="MeasIERS initialization object">

static MeasIERS_init measIERS_init;

// </group>

//# Inline Implementations

#endif
