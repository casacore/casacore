//# SolarPos.h: Solar position class
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

#ifndef MEASURES_SOLARPOS_H
#define MEASURES_SOLARPOS_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Quanta/MVPosition.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> Solar position class and calculations </summary>

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
// SolarPos from Solar Position
// </etymology>
//
// <synopsis>
// SolarPos forms the class for Solar Position calculations. It is a simple
// container with the selected method, and the mean epoch.<br>
// The method is selected from one of the following:
// <ul>
//   <li> SolarPos::STANDARD   (at 1995/09/04 the IAU1980 definition)
//   <li> SolarPos::NONE
// </ul>
// Epochs can be specified as the MJD (with defined constants MeasData::MJD2000
// and MeasData::MJDB1950 or the actual MJD),
// leading to the following constructors:
// <ul>
//   <li> SolarPos() default; assuming JD2000, IAU1980
//   <li> SolarPos(method) assuming the correct default epoch of
//		JD2000
//   <li> SolarPos(method,epoch) with epoch Double(MJD)
// </ul>
// Actual SolarPos for a certain Epoch is calculated by the () operator
// as SolarPos(epoch), with epoch Double MJD, as an MVPosition vector.<br>
// It returns the geocentric position of the heliocentre in rectangular
// coordinates in AU.<br>
// The derivative (d<sup>-1</sup>) can be obtained as well by 
// derivative(epoch), baryEarthDerivative() and barySunDerivative().<br>
// The Earth's and solar barycentric position can be obtained by the
// members <src>baryEarth</src> and <src>barySun</src>.
// The following details can be set with the 
// <linkto class=Aipsrc>Aipsrc</linkto> mechanism:
// <ul>
//  <li> measures.solarpos.d_interval: approximation interval as time 
//	(fraction of days is default unit) over which linear approximation
//	is used
//  <li> measures.solarpos.b_usejpl: use the JPL database for solar position.
//	Else analytical expression, relative error about 10<sup>-9</sup>
//	Note that the JPL database to be used can be set with 
//		measures.jpl.ephemeris (at the moment of writing DE200 (default),
//		or DE405)
// </ul>
// Reference: M. Soma et al., Cel. Mech. 41 (1988), 389;
// E.M. Standish, Astron. Astroph. 114 (1982), 297.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// To calculate the solar/Earth positions for gravitational deflection.
// An alternate route could have been
// a global function, but having a simple container allows
// caching of some calculations for speed.<br>
// Using MJD (JD-2400000.5) rather than JD is for precision reasons.
// </motivation>
//
// <todo asof="1996/02/18">
// </todo>

class SolarPos {
public:
//# Constants
// Interval to be used for linear approximation (in days)
    static const Double INTV;

//# Enumerations
// Types of known SolarPos calculations (at 1995/09/04 STANDARD == IAU1980)
    enum SolarPosTypes {STANDARD,NONE};

//# Constructors
// Default constructor, generates default J2000 SolarPos identification
    SolarPos();
// Copy constructor
    SolarPos(const SolarPos &other);
// Constructor with type
    SolarPos(SolarPosTypes type);
// Copy assignment
    SolarPos &operator=(const SolarPos &other);

//# Destructor
    ~SolarPos();

//# Operators
// Operator () calculates the geocentric Solar Position in AU
    const MVPosition&operator()(Double epoch);

//# General Member Functions
// <group>
// Return derivatives of SolarPos (d<sup>-1</sup>)
    const MVPosition &derivative (Double epoch);
    const MVPosition &baryEarthDerivative (Double epoch);
    const MVPosition &barySunDerivative (Double epoch);
// </group>
// Barycentric position of Earth
    const MVPosition &baryEarth(Double epoch);
// Barycentric position of Sun
    const MVPosition &barySun(Double epoch);

// Re-initialise SolarPos object
// <group>
    void init();
    void init(SolarPosTypes type);
// </group>

// Refresh calculations
    void refresh();

private:
//# Data menbers
// Method to be used
    SolarPosTypes method;
// Check epoch for linear approximation
    Double checkEpoch;
    Double checkSunEpoch;
// Cached calculated Earth positions
    Double eval[3];
// Cached derivatives
    Double deval[3];
// Cached calculated Sun positions
    Double sval[3];
// Cached derivatives
    Double dsval[3];
// To be able to use references in simple calculations, results are calculated
// in a circular buffer.
// Current buffer pointer
    Int lres;
// Last calculation
    MVPosition result[6];
// Interpolation interval
    static uInt interval_reg;
// JPL use
    static uInt usejpl_reg;

//# Member functions
// Copy
    void copy(const SolarPos &other);
// Fill an empty copy
    void fill();
// Calculate heliocentric Earth position for time t
    void calcEarth(Double t);
// Calculate heliocentric barycentre position
    void calcSun(Double t);
};


} //# NAMESPACE CASACORE - END

#endif
