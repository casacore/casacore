//# Nutation.h: Nutation class
//# Copyright (C) 1995,1996,1997,1998
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

#if !defined(AIPS_NUTATION_H)
#define AIPS_NUTATION_H

//# Includes
#include <aips/aips.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/Euler.h>

//# Forward Declarations

// <summary> Nutation class and calculations </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tMeasMath" demos="">
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
// container with the selected method, and the mean epoch.<br>
// The method is selected from one of the following:
// <ul>
//   <li> Nutation::STANDARD   (at 1995/09/04 the IAU1980 definition)
//   <li> Nutation::NONE       (nutation of zero returned)
//   <li> Nutation::IAU1980
//   <li> Nutation::B1950
// </ul>
// Epochs can be specified as the MJD (with defined constants MeasData::MJD2000
// and MeasData::MJDB1950 or the actual MJD),
// leading to the following constructors:
// <ul>
//   <li> Nutation() default; assuming JD2000, IAU1980
//   <li> Nutation(method) assuming the correct default epoch of
//		JD2000 or B1950
//   <li> Nutation(method,epoch) with epoch Double(MJD)
// </ul>
// Actual Nutation for a certain Epoch is calculated by the () operator
// as Nutation(epoch), with epoch Double MJD. values returned as an
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
//  <li> measures.nutation.b_usejpl: use the JPL database nutations for IAU1980.
//	Else analytical expression, relative error about 10<sup>-9</sup>
//	Note that the JPL database to be used can be set with 
//		measures.jpl.ephemeris (at the moment of writing DE200 (default),
//		or DE405)
//  <li> measures.nutation.b_useiers: use the IERS Database nutation corrections
//		for IAU1980 (default False)
// </ul>
// </synopsis>
//
// <example>
// #include <aips/Measures.h>
//	MVDirection pos(Quantity(10,"degree"),Quantity(-10.5,"degree"));
//						// direction RA=10; DEC=-10.5
//	Nutation mine(Nutation::IAU1980);	// define nutation type
//	RotMatrix rotat(mine(45837.0));		// rotation matrix for 84/05/17
//	MVDirection new = rotat*pos;		// apply nutation
// </example>
//
// <motivation>
// To calculate the Nutation angles. An alternate route could have been
// a global function, but having a simple container allows caching of some
// calculations for speed.<br>
// Using MJD (JD-2400000.5) rather than JD is for precision reasons.
// </motivation>
//
// <todo asof="1997/12/04">
// </todo>

class Nutation {
public:
//# Constants
// Interval to be used for linear approximation (in days)
    static const Double INTV;

//# Enumerations
// Types of known Nutation calculations (at 1995/09/04 STANDARD == IAU1976)
    enum NutationTypes {STANDARD,NONE,IAU1980,B1950};

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

private:

//# Data members
// Method to be used
    NutationTypes method;
// Check epoch for linear approximation
    Double checkEpoch;
// Cached calculated angles
    Double nval[3];
// Cached derivatives
    Double dval[3];
// Cached equation of equinoxes
    Double eqeq;
// Cached derivative equation of equinoxes
    Double deqeq;
// To be able to use references rather than copies, and also to use these
// references in simple (up to 4 terms of Nutation results) expressions,
// results are calculated in circulating buffer
    Int lres;
// Last calculation
    Euler result[4];
// Interpolation interval
    static uInt interval_reg;
// IERS use
    static uInt useiers_reg;
// JPL use
    static uInt usejpl_reg;
//# Member functions
// Make a copy
    void copy(const Nutation &other);
// Fill an empty copy
    void fill();
// Calculate Nutation angles for time t
    void calcNut(Double t);
};

#endif


