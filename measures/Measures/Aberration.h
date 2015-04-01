//# Aberration.h: Aberration class
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

#ifndef MEASURES_ABERRATION_H
#define MEASURES_ABERRATION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Quanta/MVPosition.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Aberration class and calculations
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasMath" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Measure>Measure</linkto> class, 
//		especially <linkto class=MEpoch>MEpoch</linkto>
//   <li> <linkto class=MeasData>MeasData</linkto> class for constants
// </prerequisite>
//
// <etymology>
// Aberration
// </etymology>
//
// <synopsis>
// Aberration forms the class for Aberration calculations. It is a simple
// container with the selected method, and the mean epoch.<br>
// The method is selected from one of the following:
// <ul>
//   <li> Aberration::STANDARD   (at 1995/09/04 the IAU1980 definition)
//   <li> Aberration::NONE
//   <li> Aberration::B1950
// </ul>
// Epochs can be specified as the MJD (with defined constants MeasData::MJD2000
// and MeasData::MJDB1950 or the actual MJD),
// leading to the following constructors:
// <ul>
//   <li> Aberration() default; assuming JD2000, IAU1980
//   <li> Aberration(method) assuming the correct default epoch of
//		JD2000 or B1950
//   <li> Aberration(method,epoch) with epoch Double(MJD).
// </ul>
// Actual Aberration for a certain Epoch is calculated by the () operator
// as Aberration(epoch), with epoch Double MJD, values returned as an
// MVPosition.<br>
// The derivative (d<sup>-1</sup>) can be obtained as well by
// derivative(epoch).<br>
// The following details can be set with the 
// <linkto class=Aipsrc>Aipsrc</linkto> mechanism:
// <ul>
//  <li> measures.aberration.d_interval: approximation interval as time 
//	(fraction of days is default unit) over which linear approximation
//	is used
//  <li> measures.aberration.b_usejpl: use the JPL database values for IAU1980.
//	Else analytical expression, relative error about 10<sup>-9</sup>
//	Note that the JPL database to be used can be set with 
//		measures.jpl.ephemeris (at the moment of writing DE200 (default),
//		or DE405). If using the JPL database, the d_interval (and the
//		output of derivative()) are irrelevant.
// </ul>
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// To calculate the Aberration angles. An alternate route could have been
// a global function, but having a simple container allows
// caching of some calculations for speed.<br>
// Using MJD (JD-2400000.5) rather than JD is for precision reasons.
// </motivation>
//
// <todo asof="1997/12/02">
// </todo>

class Aberration
{
public:
//# Constants
// Interval to be used for linear approximation (in days)
    static const Double INTV;

//# Enumerations
// Types of known Aberration calculations (at 1995/09/04 STANDARD == IAU1980)
    enum AberrationTypes {STANDARD,NONE,B1950};

//# Constructors
// Default constructor, generates default J2000 Aberration identification
    Aberration();
// Copy constructor
    Aberration(const Aberration &other);
// Constructor with type
    Aberration(AberrationTypes type);
// Copy assignment
    Aberration &operator=(const Aberration &other);

//# Destructor
    ~Aberration();

//# Operators
// Operator () calculates the Aberration direction cosine vector
    const MVPosition &operator()(Double epoch);

//# General Member Functions
// Return derivative of Aberration (d<sup>-1</sup>) w.r.t. time
    const MVPosition &derivative (Double epoch);

// Re-initialise Aberration object
// <group>
    void init();
    void init(AberrationTypes type);
// </group>

// Refresh calculations
    void refresh();

private:
//# Data menbers
// Method to be used
    AberrationTypes method;
// Check epoch for linear approximation
    Double checkEpoch;
// Cached calculated angles
    Double aval[3];
// Cached derivatives
    Double dval[3];
// To be able to use referenced results in simple calculations, a circular
// result buffer is used.
// Current buffer pointer.
    Int lres;
// Last calculation
    MVPosition result[4];
// Interpolation interval
    static uInt interval_reg;
// JPL use
    static uInt usejpl_reg;

//# Member functions
// Copy
    void copy(const Aberration &other);
// Fill an empty copy
    void fill();
// Calculate Aberration angles for time t
    void calcAber(Double t);
};


} //# NAMESPACE CASACORE - END

#endif


