//# Nutation.h: Nutation class
//# Copyright (C) 1995,1996,1997,1998,2003,2004
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

#ifndef MEASURES_NUTATION_H
#define MEASURES_NUTATION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/Euler.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

// <summary> Nutation class and calculations </summary>

// <use visibility=export>

// <reviewed reviewer="Tim Cornwell" date="1996/07/01" tests="tMeasMath"
//	demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Measure>Measure</linkto> class
//   <li> <linkto class=Euler>Euler</linkto>
//   <li> <linkto class=MeasData>MeasData</linkto> class for constants
// </prerequisite>
//
// <etymology>
// Nutation
// </etymology>
//
// <synopsis>
// Nutation forms the class for Nutation calculations. It is a simple
// container with the selected method, and the mean epoch.
// It acts as a cache
// for values and their derivatives, to enable fast calculations for time
// epochs close together (see the <em>aipsrc</em> variable
// <em>measures.nutation.d_interval</em>).
//
// The calculation method is selected from one of the following:
// <ul>
//   <li> Nutation::STANDARD   (at 1995/09/04 the IAU1980 definition,
//				from 2004/01/01 IAU2000B)
//   <li> Nutation::NONE       (nutation of zero returned)
//   <li> Nutation::IAU1980
//   <li> Nutation::B1950
//   <li> Nutation::IAU2000
//   <li> Nutation::IAU2000A   (equal to the full precision (uas) IAU2000)
//   <li> Nutation::IAU2000B   (official lower precision (mas) of IAU2000)
// </ul>
// Epochs can be specified as the MJD (with defined constants MeasData::MJD2000
// and MeasData::MJDB1950 or the actual MJD),
// leading to the following constructors:
// <ul>
//   <li> Nutation() default; assuming STANDARD
//   <li> Nutation(method) 
// </ul>
// Actual Nutation for a certain Epoch is calculated by the () operator
// as Nutation(epoch), with epoch Double MJD. Values are returned as an
// <linkto class=Euler>Euler</linkto>.
// The derivative (d<sup>-1</sup>) can be obtained as well by 
// derivative(epoch). <br>
// A Nutation can be re-initialed with a different method and/or zero
// epoch with the <src>init()</src> functions (same format as constructors).
// To bypass the full calculation actual returned values are calculated
// using the derivative if within about 2 hours (error less than about
// 10<sup>-5</sup> mas). A call to refresh() will re-initiate calculations
// from scratch.<br>
// The following details can be set with the 
// <linkto class=Aipsrc>Aipsrc</linkto> mechanism:
// <ul>
//  <li> measures.nutation.d_interval: approximation interval as time 
//	(fraction of days is default unit) over which linear approximation
//	is used (default 0.04d (about 1 hour)).
//  <li> measures.nutation.b_usejpl: use the JPL database nutations for
//		 IAU1980.
//	Else analytical expression, relative error about 10<sup>-9</sup>
//	Note that the JPL database to be used can be set with 
//		measures.jpl.ephemeris (at the moment of writing DE200
//		 (default), or DE405)
//  <li> measures.nutation.b_useiers: use the IERS Database nutation
//		 corrections for IAU1980 (default False)
// </ul>
// </synopsis>
//
// <example>
//  <srcblock>
// #include <casacore/measures/Measures.h>
//	MVDirection pos(Quantity(10,"degree"),Quantity(-10.5,"degree"));
//						// direction RA=10; DEC=-10.5
//	Nutation mine(Nutation::IAU1980);	// define nutation type
//	RotMatrix rotat(mine(45837.0));		// rotation matrix for 84/05/17
//	MVDirection new = rotat*pos;		// apply nutation
//  </srcblock>
// The normal way to use Nutation is by using the
// <linkto class=MeasConvert>MeasConvert</linkto> class.
// </example>
//
// <motivation>
// To calculate the Nutation angles. An alternate route could have been
// a global function, but having a simple container allows caching of some
// calculations for speed.<br>
// Using MJD (JD-2400000.5) rather than JD is for precision reasons.
// </motivation>
//
// <todo asof="2003/10/20">
//   <li> Correct deval_p (derivative complimentary eqox terms)
//   <li> Improve speed by a bit more lazyness in derivative calculations
//	and separate eqox calculations 
// </todo>

class Nutation {
 public:
  //# Constants
  // Interval to be used for linear approximation (in days)
  static const Double INTV;
  
  //# Enumerations
  // Types of known Nutation calculations (at 1995/09/04 STANDARD == IAU1980,
  //	after 2004/01/01 it will be IAU2000B))
  enum NutationTypes {
    NONE, IAU1980, B1950, IAU2000A, IAU2000B,
    IAU2000 = IAU2000A,
    STANDARD = IAU1980 };
  
  //# Constructors
  // Default constructor, generates default J2000 Nutation identification
  Nutation();
  // Copy constructor
  Nutation(const Nutation &other);
  // Constructor with type
  explicit Nutation(NutationTypes type);
  // Copy assignment
  Nutation &operator=(const Nutation &other);
  
  //# Destructor
  ~Nutation();
  
  //# Operators
  // Return the Nutation angles
  const Euler &operator()(Double epoch);
  
  //# General Member Functions
  // Return derivative of Nutation (d<sup>-1</sup>)
  const Euler &derivative(Double epoch);
  
  // Re-initialise Nutation object
  // <group>
  void init();
  void init(NutationTypes type);
  // </group>
  
  // Refresh calculations
  void refresh();
  
  // Get the equation of equinox
  // <group>
  Double eqox(Double epoch) ;
  Quantity getEqoxAngle(Double epoch);
  Quantity getEqoxAngle(Double epoch, const Unit &unit) ;
  // </group>
  // Get the derivative of the equation of equinoxes in d<sup>-1</sup>
  Double derivativeEqox(Double epoch);
  // Get the complimentary terms of the equation of equinoxes
  Double eqoxCT(Double epoch);
  // Get the derivative of the complimentary terms of the equation of equinoxes
  Double derivativeEqoxCT(Double epoch);

 private:
  
  //# Data members
  // Method to be used
  NutationTypes method_p;
  // Check epoch for linear approximation
  Double checkEpoch_p;
  // Check epoch for calculation of derivatives
  Double checkDerEpoch_p;
  // Cached calculated angles
  Double nval_p[3];
  // Cached derivatives
  Double dval_p[3];
  // Cached equation of equinoxes
  Double eqeq_p;
  // Cached derivative equation of equinoxes
  Double deqeq_p;
  // Cached complimentary terms equation of equinoxes
  Double neval_p;
  // Cached derivative of complimentary terms equation of equinoxes
  Double deval_p;
  // To be able to use references rather than copies, and also to use these
  // references in simple (up to 4 terms of Nutation results) expressions,
  // results are calculated in circulating buffer
  Int lres_p;
  // Last calculation
  Euler result_p[4];
  // Interpolation interval
  static uInt myInterval_reg;
  // IERS use
  static uInt myUseiers_reg;
  // JPL use
  static uInt myUsejpl_reg;
  //# Member functions
  // Make a copy
  void copy(const Nutation &other);
  // Fill an empty copy
  void fill();
  // Calculate Nutation angles for time t; also derivatives if True given
  void calcNut(Double t, Bool calcDer = False);
};


} //# NAMESPACE CASACORE - END

#endif


