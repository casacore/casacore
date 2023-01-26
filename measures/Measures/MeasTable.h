//# MeasTable.h: MeasTable provides Measure computing database data
//# Copyright (C) 1995-1999,2000-2004
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

#ifndef MEASURES_MEASTABLE_H
#define MEASURES_MEASTABLE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MeasJPL.h>      // calcPlanetary(MeasJPL::Files *)
#include <casacore/measures/Measures/MeasTableMul.h>
#include <casacore/measures/Measures/MeasData.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/scimath/Functionals/Polynomial.h>

#include <mutex>
#include <vector>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class RotMatrix;
class Euler;

// <summary>
// MeasTable provides Measure computing database data
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasMath" demos="">
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
// <br> V.S. Gubanov, Astron. Zh. 49, p1112, 1972
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
  // Selection related data
  // <group>
  // Are the IAU2000 precession/nutation to be used or not (IAU1984)
  // Note that an Aipsrc::reRead() is not reflected in the return value here.
  static bool useIAU2000();
  // If IAU2000 model, do we use the high precision 2000A model?
  // Note that an Aipsrc::reRead() is not reflected in the return value here.
  static bool useIAU2000A();
  // </group>

  // Precession related data
  // <group>
  // Get the precession-rate part of the IAU2000 precession-nutation models
  // (which 0=dpsi (long) and 1=deps (obliquity) and 2 =0)
  static double precRate00(const uint32_t which);

  // Get the frame bias matrix for IAU2000 model.
  static RotMatrix frameBias00();

  // Generate the precession calculation polynomials for a fixed Epoch T
  // in the result area specified.
  // T is given in Julian centuries since J2000.0.
  static void
  precessionCoef(double T, Polynomial<double> result[3]);
  
  // Generate the precession polynomials for IAU2000 system.
  static void
  precessionCoef2000(Polynomial<double> result[3]);
  
  // Generate the precession polynomials for 1950 system for a fixed Epoch T
  // in the area specified. T is given in Tropical centuries since B1850.0
  static void
  precessionCoef1950(double T, Polynomial<double> result[3]);
  // </group>
  
  // Nutation related data
  // <group>
  // Generate the polynomial for the fundamental arguments (eps, l, l',
  // F, D, omega) as a function of Julian centuries
  // <group>
  static const Polynomial<double> &fundArg(uint32_t which);
  static const Polynomial<double> &fundArg1950(uint32_t which);
  static const Polynomial<double> &fundArg2000(uint32_t which);
  // </group>

  // Get the planetary arguments (L, L', F, D, Om, Me, Ve, E, Ma, Ju Sa,
  // Ur, Ne, pre) 
  static const Polynomial<double> &planetaryArg2000(uint32_t which);

  // Generate the which' vector of the nutation series arguments
  // <group>
  static const double* mulArg(uint32_t which);
  static const double* mulArg1950(uint32_t which);
  static const double* mulArg2000A(uint32_t which);
  static const double* mulArg2000B(uint32_t which);
  static const double* mulPlanArg2000A(uint32_t which);
  // </group>

  // Generate the which' vector of the equation of equinoxes (IAU2000)
  // complementary terms series arguments
  static const double* mulArgEqEqCT2000(uint32_t which);

  // Generate the which' vector of the nutation series multipliers
  // at T, measured in Julian centuries since J2000.0, respectively B1900.0
  // <group>
  static CountedPtr<Matrix<double> > mulSC(double time, double epsilon);
  static CountedPtr<Matrix<double> > mulSC1950(double time, double epsilon);
  static CountedPtr<Matrix<double> > mulSC2000A(double time, double epsilon);
  static CountedPtr<Matrix<double> > mulSC2000B(double time, double epsilon);
  static const double* mulPlanSC2000A(uint32_t which);
  // </group>

  // Generate the which' vector of the equation of equinoxes (IAU2000)
  // complementary terms series multipliers
  // at T, measured in Julian centuries since J2000.0, respectively B1900.0
  static const double* mulSCEqEqCT2000(uint32_t which);

  // Get nutation angles corrections for UTC T in rad.
  // which = 0 : dPsi as given by IERS for IAU nutation theory;
  // = 1: dEps as same.
  static double dPsiEps(uint32_t which, double T);
  // </group>

  // Planetary (JPL DE) related data
  // <group>
  // Get the position (AU or rad) and velocity (AU/d or rad/d) for specified
  // code at TDB T. The ephemeris to use (now DE200 or DE405) can be selected
  // with the 'measures.jpl.ephemeris' aipsrc resource (default DE200).
  static Vector<double> Planetary(MeasTable::Types which, double T);
  // Get the JPL DE constant indicated
  static double Planetary(MeasTable::JPLconst what);
  // </group>

  // Observatory positions
  // <group>
  // Initialise list of all observatories from Observatories table
  // Called using theirObsInitOnce.
  static void initObservatories();
  // Get list of all observatories
  static const Vector<String> &Observatories();
  // Get position of observatory nam (false if not present)
  static bool Observatory(MPosition &obs, const String &nam);

  // Get _absolute_ path to AntennaResponses table of observatory
  // <src>nam</src>. It returns false if no _valid_ path can be found or the
  // observatory is unknown. If the observatory is known, antRespPath will 
  // be set to the entry in the AntennaResponses column of the
  // Observatories table even if it doesn't describe a valid path; if the
  // entry is not an absolute path, the data directory name will be
  // prepended and validity verified.
  static bool AntennaResponsesPath(String &antRespPath, const String &nam);
  // </group>

  // Source list positions
  // <group>
  // Initialise list of all source from Sources table
  // Called using theirSrcInitOnce.
  static void initSources();
  // Get list of all sources
  static const Vector<String> &Sources();
  // Get position of source <src>nam</src> (false if not present)
  static bool Source(MDirection &obs, const String &nam);
  // </group>
  
  // Rest frequencies
  // <group>
  // Initialise list from internal Table for now
  // Called using theirLinesInitOnce.
  static void initLines();
  // Get list of all frequencies
  static const Vector<String> &Lines();
  // Get frequency of line name (false if not present)
  static bool Line(MFrequency &obs, const String &nam);
  // </group>

  // Initialise list of IGRF data
  // Called using theirIGRFInitOnce.
  static void initIGRF();
  // Earth magnetic field (IGRF) data
  // Get the harmonic terms for specified time (mjd)
  static Vector<double> IGRF(double t);

  // Aberration related data
  // <group>
  // Generate the polynomial for the fundamental arguments (l1-l8, w, D, l,
  // l', F) for the Ron/Vondrak aberration calculations as a function of 
  // Julian centuries(J2000), or the comparable ones for the Gubanov expansion
  // (B1950). 
  // <group>
  static const Polynomial<double> &aberArg(uint32_t which);
  static const Polynomial<double> &aberArgDeriv(uint32_t which);
  static const Polynomial<double> &aber1950Arg(uint32_t which);
  static const Polynomial<double> &aber1950ArgDeriv(uint32_t which);
  // </group>
  
  // Generate the 'which' vector of the aberration series arguments
  // <group>
  static const double* mulAberArg(uint32_t which);
  static const double* mulAber1950Arg(uint32_t which);
  static const double* mulAberSunArg(uint32_t which);
  static const double* mulAberEarthArg(uint32_t which);
  // </group>
  
  // Generate the 'which' vector of the aberration series multipliers
  // at T, measured in Julian centuries since J2000.0 (or J1900.0, yes,
  // J1900.0, for B1950).
  // <group>
  static CountedPtr<Matrix<double> > mulAber(double time, double epsilon);
  static CountedPtr<Matrix<double> > mulAber1950(double time, double epsilon);
  static const Vector<double> &mulSunAber(uint32_t which);
  static const Vector<double> &mulEarthAber(uint32_t which);
  // </group>
  
  // Get the E-terms of Aberration correction (0 for position, 1 for velocity)
  // <group>
  static const Vector<double> &AberETerm(uint32_t which);
  // </group>
  
  // </group>
  
  // Diurnal aberration factor
  static double diurnalAber(double radius, double T);
  
  // LSR (kinematical) velocity conversion: 0 gives J2000; 1 gives B1950.
  // In both cases a velocity of 20.0 km/s is assumed, and a B1900 RA/Dec
  // direction of (270,30) degrees. This value has been defined between
  // the groups doing HI radio work in the mid 1950s.
  static const Vector<double> &velocityLSRK(uint32_t which);
  // LSR (dynamical, IAU definition). Velocity (9,12,7) km/s in galactic
  // coordinates. Or 16.552945 towards l,b = 53.13, +25.02 deg.
  // 0 gives J2000, 1 gives B1950 velocities.
  static const Vector<double> &velocityLSR(uint32_t which);
  // Velocity of LSR with respect to galactic centre. 220 km/s in direction
  // l,b = 270, +0 deg. 0 returns J2000, 1 B1950
  static const Vector<double> &velocityLSRGal(uint32_t which);
  // Velocity of Local Group wrt bary center (F.Ghigo): 308km/s towards
  // l,b = 105,-7. 0 for J2000, 1 for B1950
  static const Vector<double> &velocityCMB(uint32_t which);
  // Velocity of CMB wrt bary center (F.Ghigo): 369.5km/s towards
  // l,b = 264.4,48.4. 0 for J2000, 1 for B1950

  static const Vector<double> &velocityLGROUP(uint32_t which);
  // Earth and Sun position related data
  // <group>
  // Fundamental arguments for Soma et al. methods
  // <group>
  static const Polynomial<double> &posArg(uint32_t which);
  // Precomputed derivative of PosArg
  static const Polynomial<double> &posArgDeriv(uint32_t which);
  // </group>
  // Generate the which' vector of the position series arguments
  // <group>
  static const double* mulPosEarthXYArg(uint32_t which);
  static const double* mulPosEarthZArg(uint32_t which);
  static const double* mulPosSunXYArg(uint32_t which);
  static const double* mulPosSunZArg(uint32_t which);
  // </group>
  
  // Generate the which' vector of the position series multipliers
  // at T, measured in Julian centuries since J2000.0
  // <group>
  static CountedPtr<Matrix<double> > mulPosEarthXY(double time, double epsilon);
  static CountedPtr<Matrix<double> > mulPosEarthZ (double time, double epsilon);
  static CountedPtr<Matrix<double> > mulPosSunXY  (double time, double epsilon);
  static CountedPtr<Matrix<double> > mulPosSunZ   (double time, double epsilon);
  // </group>
  // Get the rotation matrix to change position from ecliptic to rectangular
  // for Soma et al. analytical expression
  static const RotMatrix &posToRect();
  // Get the rotation matrix to change position from rectangular to ecliptic
  // for Soma et al. analytical expression
  static const RotMatrix &rectToPos();
  // Get the rotation matrix from galactic to supergalactic.
  // Based on De Vaucouleurs 1976:  Pole at 47.37/6.32 deg; 137.37 l0
  // Euler angles: 90, 83.68, 47.37 degrees
  static const RotMatrix &galToSupergal();
  // Get the rotation matrix from ICRS to J2000/FK5.
  // Based on the IAU 2000 resolutions (the bias matrix)
  static const RotMatrix &ICRSToJ2000();
  // </group>
  
  // Position related routines
  // <group>
  // Equatorial radius (0) and flattening(1) of geodetic reference spheroids
  static double WGS84(uint32_t which);
  // </group>
  
  // Polar motion related routines
  // <group>
  // Get the polar motion (-x,-y,0)(2,1,3) angles at the given epoch
  static Euler polarMotion(double ut);
  // </group>
  
  // Time related routines
  // <note>
  //   WARNING given if correction not obtainable
  // </note>
  // <thrown>
  //  <li> AipsError if table seems to be corrupted
  // </thrown>
  // <group>
  // Give TAI-UTC (in s) for MJD utc UTC
  static double dUTC(double utc);
  // UT1-UTC (in s) for MJD tai TAI
  static double dUT1(double utc);
  // TDT-TAI (in s) for MJD tai TAI. Note this is equal to TT2000-TAI
  static double dTAI(double tai=0.0);
  // TDB-TDT (in s) for MJD ut1 UT1
  static double dTDT(double ut1);
  // TCB-TDB (in s) for MJD tai TAI
  static double dTDB(double tai);
  // TCG-TT (in s) for MJD tai TAI
  static double dTCG(double tai);
  // GMST1 at MJD ut1 UT1
  static double GMST0(double ut1);
  // GMST (IAU2000) including the ERA (IAU2000 Earth Rotation Angle) in rad
  static double GMST00(double ut1, double tt);
  // Earth Rotation Angle (IAU2000) in rad
  static double ERA00(double ut1);
  // s' (IAU2000) in rad (approximate value)
  static double sprime00(double tt);
  // UT1 at GMSD gmst1 GMST1
  static double GMUT0(double gmst1);
  // Ratio UT1/MST at MJD ut1 UT1
  static double UTtoST(double ut1);
  // </group>

private:
  
  // Copy assign, NOT defined
  MeasTable &operator=(const MeasTable &other);
  
  //# General member functions

  static void doInitObservatories();
  static void doInitLines();
  static void doInitSources();
  static void doInitIGRF();

  // The calcNNN() functions are helpers to initialize
  // function scope static variables in the NNN() callers.

  // Calculate precessionCoef
  // <group>
  static void calcPrecesCoef(double T, Polynomial<double> result[3],
			     const double coeff[3][6]); 
  static void calcPrecesCoef2000(Polynomial<double> result[3],
				 const double coeff[3][6]); 
  // </group>

  // Calculate fundArg
  // <group>
  static std::vector<Polynomial<double> > calcFundArg(const double coeff[6][4]);
  static std::vector<Polynomial<double> > calcFundArg00(const double coeff[6][5]);
  static std::vector<Polynomial<double> > calcPlanArg00(const double coeff[8][2]);
  // </group>

  // Calculate planetary data
  // <group>
  static void calcPlanetary(MeasJPL::Files* fil);
  static void calcPlanetaryConstants(double cn[MeasTable::N_JPLconst]);
  // </group>

  // Calculate aberration data
  // <group>
  static std::vector<Polynomial<double> > calcAberArg();
  static std::vector<Polynomial<double> > calcAberArgDeriv();
  static std::vector<Polynomial<double> > calcAber1950Arg();
  static std::vector<Polynomial<double> > calcAber1950ArgDeriv();
  static std::vector<Vector<double> > calcMulSunAber();
  static std::vector<Vector<double> > calcMulEarthAber();
  static std::vector<Vector<double> > calcAberETerm();
  // </group>

  // Calculate velocity data
  // <group>
  static std::vector<Vector<double> > calcVelocityLSRK();
  static std::vector<Vector<double> > calcVelocityLSR();
  static std::vector<Vector<double> > calcVelocityLSRGal();
  static std::vector<Vector<double> > calcVelocityLGROUP();
  static std::vector<Vector<double> > calcVelocityCMB();
  // </group>

  // Calculate Earth and Sun position data
  // <group>
  static std::vector<Polynomial<double> > calcPosArg();
  static std::vector<Polynomial<double> > calcPosArgDeriv();
  // </group>

  // Calculate some of the rotation matrices for coordinate conversion
  // <group>
  static RotMatrix calcRectToPos();
  static RotMatrix calcICRSToJ2000();
  // </group>

  // Calculate time related conversion data

  // For dUTC() pack vars for clean initialization of function scope statics.
  // Thread-safe (C++11). For pre-C++11 depends on compiler (GCC, Clang make it so).
  struct Statics_dUTC {
    double (*LEAP)[4];
    int32_t N;
  };
  // <group>
  static Statics_dUTC calc_dUTC();
  static Polynomial<double> calcGMST0();
  static Polynomial<double> calcGMST00();
  static Polynomial<double> calcERA00();
  static Polynomial<double> calcGMUT0();
  static Polynomial<double> calcUTtoST();
  // </group>

  //# Data
  // Planetary table data
  // <group>
  static std::once_flag theirPlanetaryInitOnceFlag;
  static std::once_flag theirPlanetaryConstantsInitOnceFlag;
  // </group>

  // Multipliers for nutation, etc.
  // <group>
  static MeasTableMulSC         theirMulSC;
  static MeasTableMulSC1950     theirMulSC1950;
  static MeasTableMulSC2000A    theirMulSC2000A;
  static MeasTableMulSC2000B    theirMulSC2000B;
  static MeasTableMulAber       theirMulAber;
  static MeasTableMulAber1950   theirMulAber1950;
  static MeasTableMulPosSunXY   theirMulPosSunXY;
  static MeasTableMulPosSunZ    theirMulPosSunZ;
  static MeasTableMulPosEarthXY theirMulPosEarthXY;
  static MeasTableMulPosEarthZ  theirMulPosEarthZ;
  // </group>

  // Observatories table data
  // <group>
  static std::once_flag theirObsInitOnceFlag;
  static Vector<String> obsNams;
  static Vector<MPosition> obsPos;
  static Vector<String> antResponsesPath;
  // </group>
  // Spectral line table data
  // <group>
  static std::once_flag theirLinesInitOnceFlag;
  static Vector<String> lineNams;
  static Vector<MFrequency> linePos;
  // </group>
  // Sources table data
  // <group>
  static std::once_flag theirSrcInitOnceFlag;
  static Vector<String> srcNams;
  static Vector<MDirection> srcPos;
  // </group>
  // IGRF data
  // <group>
  static std::once_flag theirIGRFInitOnceFlag;
  static double dtimeIGRF;
  static double firstIGRF;
  static double lastIGRF;
  static double time0IGRF;
  static double timeIGRF;
  static std::vector<Vector<double> > coefIGRF;
  static std::vector<Vector<double> > dIGRF;
  // </group>

  ///#if !defined(USE_THREADS) || defined(__APPLE__)
  ///  static std::mutex theirdUT1Mutex;
  ///#endif
};


} //# NAMESPACE CASACORE - END

#endif
