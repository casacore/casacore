//# Precession.h: Precession class
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

#if !defined(AIPS_PRECESSION_H)
#define AIPS_PRECESSION_H

#if defined(_AIX)
#pragma implementation ("Precession.cc")
#endif

//# Includes
#include <aips/aips.h>
#include <aips/Quanta/Euler.h>
#include <aips/Functionals/Polynomial.h>

//# Forward Declarations

// <summary> Precession class and calculations </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tMeasMath" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Measure>Measure</linkto> class for use
//   <li> <linkto class=Euler>Euler</linkto> class for format
//   <li> <linkto class=MeasData>MeasData</linkto> class for constants
//   <li> <linkto class=MeasTable>MeasTable</linkto> class for other data
// </prerequisite>
//
// <etymology>
// Precession
// </etymology>
//
// <synopsis>
// Precession forms the class for precession calculations. It is a simple
// container with the selected method, and the mean epoch.<br>
// The method is selected from one of the following:
// <ul>
//   <li> Precession::STANDARD  (at 1995/09/04 the IAU1976 definition)
//   <li> Precession::NONE	(precession of zero returned
//   <li> Precession::IAU1976
//   <li> Precession::B1950
// </ul>
// Epochs can be specified as the MJD (with defined constants
// MeasData::MJD2000 and MeasData::MJD1950 or the actual MJD),
// leading to the following constructors:
// <ul>
//   <li> Precession() default; assuming JD2000, IAU1976
//   <li> Precession(method) assuming the correct default epoch of 
//		JD2000 or JD1950
//   <li> Precession(method,epoch) with epoch Double(MJD)
// </ul>
// Actual precession for a certain Epoch is calculated by the () operator
// as Precession(epoch), with epoch Double MJD. Values returned  as an 
// <linkto class=Euler>Euler</linkto>.
// The derivative (d<sup>-1</sup>) can be obtained as well by 
// derivative(epoch). <br>
// A Precession can be re-initialed with a different method and/or zero
// epoch with the <src>init()</src> functions (same format as constructors).
// To bypass the full calculation actual returned values are calculated
// using the derivative if within about 2 hours (error less than about
// 10<sup>-5</sup> mas). A call to refresh() will re-initiate calculations
// from scratch.<br>
// The following details can be set with the 
// <linkto class=Aipsrc>Aipsrc</linkto> mechanism:
// <ul>
//  <li> measures.precession.d_interval: approximation interval as time 
//	(fraction of days is default unit) over which linear approximation
//	is used
// </ul>
// </synopsis>
//
// <example>
//  <srcblock>
// #include <aips/Measures.h>
//	MVDirection pos(Quantity(10,"degree"),Quantity(-10.5,"degree"));
//						// direction RA=10; DEC=-10.5
//	Precession mine(Precession::IAU1976);	// define precession type
//	RotMatrix rotat(mine(45837.0));		// rotation matrix for 84/05/17
//	MVDirection new = rotat*pos;		// apply precession
//	rotat = RotMatrix(mine(45839.0));       // interpolate new precession
//						// assuming d_interval set large
//  </srcblock>
// </example>
//
// <motivation>
// To calculate the precession angles. An alternate route could have been
// a global function, but having a simple container allows caching of some
// calculations for speed.<br>
// Using MJD (JD-2400000.5) rather than JD is for precision reasons.
// </motivation>
//
// <todo asof="1996/02/04">
// </todo>

class Precession
{
public:
//# Constants
// Default interval to be used for linear approximation (in days)
    static const Double INTV;

//# Enumerations
// Types of known precession calculations (at 1995/09/04 STANDARD == IAU1976)
    enum PrecessionTypes {STANDARD,NONE,IAU1976,B1950};

//# Constructors
// Default constructor, generates default J2000 precession identification
    Precession();
// Copy constructor
    Precession(const Precession &other);
// Constructor with epoch in Julian days
    explicit Precession(PrecessionTypes type, Double catepoch=0);
// Copy assignment
    Precession &operator=(const Precession &other);

//# Destructor
    ~Precession();

//# Operators
// Return the precession angles
    const Euler &operator()(Double epoch);

//# General Member Functions
// Return derivative of precession (d<sup>-1</sup>)
    const Euler &derivative(Double epoch);
// Re-initialise Precession object
// <group>
    void init();
    void init(PrecessionTypes type, Double catepoch=0);
// </group>
// Refresh calculations
    void refresh();

private:
//# Data members
// Method to be used
    PrecessionTypes method;
// Fixed epoch to be used (MJD)
    Double fixedEpoch;
// Fixed epoch in centuries from base epoch
    Double T;
// Length of century
    Double cent;
// Reference epoch;
    Double refEpoch;
// Check epoch
    Double checkEpoch;
// Polynomial coefficients for zeta,z,theta
    Polynomial<Double> zeta[3];
// Cached calculated angles
    Double pval[3];
// Cached derivatives
    Double dval[3];
// To reference results, and use a few in interim calculations, results are
// calculated in a circular buffer.
// Current result pointer
    Int lres;
// Last calculation
    Euler result[4];
// Interpolation interval
    static uInt interval_reg;

//# Member functions
// Make a copy
    void copy(const Precession &other);
// Create correct default fixedEpoch and catalogue epoch data
    void fillEpoch();
// Calculate precession angles for time t
    void calcPrec(Double t);
};

#endif


