//# MeasTable.h: MeasTable provides Measure computing database data
//# Copyright (C) 1995,1996,1997,1998,1999,2000
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

#if !defined(AIPS_MEASTABLE_H)
#define AIPS_MEASTABLE_H

//# Includes
#include <aips/aips.h>
#include <aips/Measures/MeasData.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Functionals/Polynomial.h>

//# Forward Declarations
class RotMatrix;
class Euler;

// <summary>
// MeasTable provides Measure computing database data
// </summary>

// <use visibility=local>

// <reviewed reviewer="tcornwel" date="yyyy/mm/dd" tests="tMeasMath" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Measure>Measure</linkto> class 
//   <li> <linkto class=MeasData>MeasData</linkto> class for constant data
//   <li> <linkto class=Aipsrc>Aipsrc</linkto> class for data placement
// </prerequisite>
//
// <etymology>
// MeasTable from Measure and Table
// </etymology>
//
// <synopsis>
// MeasTable contains the database interface for all
// data necessary for precession, nutation and other 
// <linkto class=Measure>Measure</linkto> related calculations.<br>
// All data are obtained by calls to a method. E.g.
// <src> fundArg(1) </src> will provide the first fundamental argument for
// nutation calculations, i.e. 'l'. <br>
// This class contains no constructors or destructors, only static
// methods and (static) constants.
// <br> References:<br> Explanatory supplements to the Astronomical Almanac
// <br> C. Ron and J. Vondrak, Bull. Astron. Inst. Czechosl. 37, p96, 1986
// <br> M. Soma, Th. Hirayama and H. Kinoshita, Celest. Mech. 41, p389, 1988
// <br> V.S. Gubanov, Astron. Zh. 49, p1112, 1992
//
// Where strings are passed in as arguments (observatory names, sources), they
// will be case insensitive, and minimum match.
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
// <todo asof="1997/09/02">
//   <li> more database interfaces, rather than constants
// </todo>

class MeasTable {

public:
  
  //# Enumerations
  // Types to be used in different calls
  enum Types {
    // Planetary information
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

  // Codes for JPL constants: order should be same as in MeasJPL, length less
  // than or equal
  enum JPLconst {
    // Light velocity used in AU/d
    CAU,
    // Solar mass (GM0)/c<sup>2</sup> in AU
    GMS,
    // AU in km
    AU,
    // Solar radius in AU
    RADS,
    // # of codes
    N_JPLconst };

  //# General Member Functions
  // Precession related data
  // <group>
  // Generate the precession calculation polynomials for a fixed Epoch T
  // in the result area specified. T is given in Julian centuries since J2000.0.
  static void
  precessionCoef(Double T,
		 Polynomial<Double> result[3]);
  
  // Generate the precession polynomials for 1950 system for a fixed Epoch T
  // in the area specified. T is given in Tropical centuries since B1850.0
  static void
  precessionCoef1950(Double T,
		     Polynomial<Double> result[3]);
  // </group>
  
  // Nutation related data
  // <group>
  // Generate the polynomial for the fundamental arguments (eps, l, l',
  // F, D, omega) as a function of Julian centuries
  // <group>
  static const Polynomial<Double> &fundArg(uInt which);
  static const Polynomial<Double> &fundArg1950(uInt which);
  // </group>
  
  // Generate the which' vector of the nutation series arguments
  // <group>
  static const Vector<Char> &mulArg(uInt which);
  static const Vector<Char> &mulArg1950(uInt which);
  // </group>
  
  // Generate the which' vector of the nutation series multipliers
  // at T, measured in Julian centuries since J2000.0, respectively B1900.0
  // <group>
  static const Vector<Double> &mulSC(uInt which, Double T);
  static const Vector<Double> &mulSC1950(uInt which, Double T);
  // </group>

  // Get nutation angles corrections for UTC T in rad.
  // which = 0 : dPsi as given by IERS for IAU nutation theory;
  // = 1: dEps as same.
  static Double dPsiEps(uInt which, Double T);
  // </group>

  // Planetary (JPL DE) related data
  // <group>
  // Get the position (AU or rad) and velocity (AU/d or rad/d) for specified
  // code at TDB T. The ephemeris to use (now DE200 or DE405) can be selected
  // with the 'measures.jpl.ephemeris' aipsrc resource (default DE200).
  static const Vector<Double> &Planetary(MeasTable::Types which, 
					 Double T); 
  // Get the JPL DE constant indicated
  static const Double &Planetary(MeasTable::JPLconst what);
  // </group>

  // Observatory positions
  // <group>
  // Initialise list of all observatories from Observatories table
  static void initObservatories();
  // Get list of all observatories
  static const Vector<String> &Observatories();
  // Get position of observatory nam (False if not present)
  static const Bool Observatory(MPosition &obs, const String &nam);
  // </group>

  // Source list positions
  // <group>
  // Initialise list of all source from Sources table
  static void initSources();
  // Get list of all sources
  static const Vector<String> &Sources();
  // get position of source nam (False if not present)
  static const Bool Source(MDirection &obs, const String &nam);
  // </group>
  
  // Rest frequencies
  // <group>
  // Initialise list from internal Table for now
  static void initLines();
  // Get list of all frequencies
  static const Vector<String> &Lines();
  // Get frequency of line name (False if not present)
  static const Bool Line(MFrequency &obs, const String &nam);
  // </group>

  // Earth magnetic field (IGRF) data
  // <group>
  // Get the harmonic terms for specified time (mjd)
  static const Vector<Double> &IGRF(Double t);
  // </group>

  // Aberration related data
  // <group>
  // Generate the polynomial for the fundamental arguments (l1-l8, w, D, l,
  // l', F) for the Ron/Vondrak aberration calculations as a function of 
  // Julian centuries(J2000), or the comparable ones for the Gubanov expansion
  // (B1950). 
  // <group>
  static const Polynomial<Double> &aberArg(uInt which);
  static const Polynomial<Double> &aber1950Arg(uInt which);
  // </group>
  
  // Generate the which' vector of the aberration series arguments
  // <group>
  static const Vector<Char> &mulAberArg(uInt which);
  static const Vector<Char> &mulAber1950Arg(uInt which);
  static const Vector<Char> &mulAberSunArg(uInt which);
  static const Vector<Char> &mulAberEarthArg(uInt which);
  // </group>
  
  // Generate the which' vector of the aberration series multipliers
  // at T, measured in Julian centuries since J2000.0 (or comparable for
  // B1950).
  // <group>
  static const Vector<Double> &mulAber(uInt which, Double T);
  static const Vector<Double> &mulAber1950(uInt which, Double T);
  static const Vector<Double> &mulSunAber(uInt which);
  static const Vector<Double> &mulEarthAber(uInt which);
  // </group>
  
  // Get the E-terms of Aberration correction (0 for position, 1 for velocity)
  // <group>
  static const Vector<Double> &AberETerm(uInt which);
  // </group>
  
  // </group>
  
  // Diurnal aberration factor
  static Double diurnalAber(Double radius, Double T);
  
  // LSR (kinematical) velocity conversion: 0 gives J2000; 1 gives B1950.
  // In both cases a velocity
  // of 19.5 km/s assumed, and a B1900 RA/Dec direction of (270,30) degrees
  static const Vector<Double> &velocityLSRK(uInt which);
  // LSR (dynamical, IAU definition). Velocity (9,12,7) km/s in galactic
  // coordinates. Or 16.552945 towards l,b = 53.13, +25.02 deg.
  // 0 gives J2000, 1 gives B1950 velocities.
  static const Vector<Double> &velocityLSR(uInt which);
  // Velocity of LSR with respect to galactic centre. 220 km/s in direction
  // l,b = 270, +0 deg. 0 returns J2000, 1 B1950
  static const Vector<Double> &velocityLSRGal(uInt which);
  
  // Earth and Sun position related data
  // <group>
  // Fundamental arguments for Soma et al. methods
  // <group>
  static const Polynomial<Double> &posArg(uInt which);
  // </group>
  // Generate the which' vector of the position series arguments
  // <group>
  static const Vector<Char> &mulPosEarthXYArg(uInt which);
  static const Vector<Char> &mulPosEarthZArg(uInt which);
  static const Vector<Char> &mulPosSunXYArg(uInt which);
  static const Vector<Char> &mulPosSunZArg(uInt which);
  // </group>
  
  // Generate the which' vector of the position series multipliers
  // at T, measured in Julian centuries since J2000.0
  // <group>
  static const Vector<Double> &mulPosEarthXY(uInt which, Double T);
  static const Vector<Double> &mulPosEarthZ(uInt which, Double T);
  static const Vector<Double> &mulPosSunXY(uInt which, Double T);
  static const Vector<Double> &mulPosSunZ(uInt which, Double T);
  // </group>
  // Get the rotation matrix to change position from ecliptic to rectangular
  // for Soma et al. analytical expression
  static const RotMatrix &posToRect();
  // Get the rotation matrix to change position from rectangular to ecliptic
  // for Soma et al. analytical expression
  static const RotMatrix &rectToPos();
  // Get the rotation matrix from galactic to supergalactic.
  // Based on matrix as available in Miriad, leading to Euler Angles
  // of 90, 83.68, 47.34 degrees
  static const RotMatrix &galToSupergal();
  // </group>
  
  // Position related routines
  // <group>
  // Equatorial radius (0) and flattening(1) of geodetic reference spheroids
  static Double WGS84(uInt which);
  // </group>
  
  // Polar motion related routines
  // <group>
  // Get the polar motion (-x,-y,0)(2,1,3) angles
  static const Euler &polarMotion(Double ut);
  // </group>
  
  // Time related routines
  // <logged>
  //   <li> HIGH, WARNING given if correction not obtainable
  // </logged>
  // <thrown>
  //  <li> AipsError if table seems to be corrupted
  // </thrown>
  // <group>
  // Give TAI-UTC (in s) for MJD utc UTC
  static Double dUTC(Double utc);
  // UT1-UTC (in s) for MJD tai TAI
  static Double dUT1(Double utc);
  // TDT-TAI (in s) for MJD tai TAI
  static Double dTAI(Double tai);
  // TDB-TDT (in s) for MJD ut1 UT1
  static Double dTDT(Double ut1);
  // TCB-TDB (in s) for MJD tai TAI
  static Double dTDB(Double tai);
  // TCG-TT (in s) for MJD tai TAI
  static Double dTCG(Double tai);
  // GMST1 at MJD ut1 UT1
  static Double GMST0(Double ut1);
  // UT1 at GMSD gmst1 GMST1
  static Double GMUT0(Double gmst1);
  // Ratio UT1/MST at MJD ut1 UT1
  static Double UTtoST(Double ut1);
  // </group>

private:
  
  //# Constructors
  // Default constructor, NOT defined
  MeasTable();
  
  // Copy assign, NOT defined
  MeasTable &operator=(const MeasTable &other);
  
  //# Destructor
  //  Destructor, NOT defined and not declared to stop warning
  // ~MeasTable();

  //# General member functions

  // Calculate precessionCoef
  static void calcPrecesCoef(Double T, Polynomial<Double> result[3],
			     const Double coeff[3][6]); 
  // Calculate fundArg
  static void calcFundArg(Bool &need, Polynomial<Double> result[3],
			  const Double coeff[6][4]); 
  // Calculate mulArg
  static void calcMulArg(Bool &need, Vector<Char> result[],
			 const Char coeff[][5], Int row); 
  // Calculate mulSC
  static void calcMulSC(Bool &need, Double &check, Double T,
			Vector<Double> result[], Int resrow,
			Polynomial<Double> poly[],
			const Long coeffTD[][5], Int TDrow,
			const Short coeffSC[][2]);

  //# Data
  // Observatories table data
  // <group>
  static Bool obsNeedInit;
  static Vector<String> obsNams;
  static Vector<MPosition> obsPos;
  // </group>
  // Spectral line table data
  // <group>
  static Bool lineNeedInit;
  static Vector<String> lineNams;
  static Vector<MFrequency> linePos;
  // </group>
  // Sources table data
  // <group>
  static Bool srcNeedInit;
  static Vector<String> srcNams;
  static Vector<MDirection> srcPos;
  // </group>
  // IGRF data
  // <group>
  static Double timeIGRF;
  static Double dtimeIGRF;
  static Double time0IGRF;
  static Double firstIGRF;
  static Double lastIGRF;
  static Vector<Double> coefIGRF;
  static Vector<Double> dIGRF;
  static Vector<Double> resIGRF;
  // </group>
};

#endif
