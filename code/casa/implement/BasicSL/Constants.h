//# Constants.h: Mathematical and physical constants
//# Copyright (C) 1993,1994,1995,1997,1998
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

#if !defined(AIPS_C_H)
#define AIPS_C_H

#if defined (sun) && ! defined (AIPS_SOLARIS)
#   include <sys/limits.h>
#else
#   include <limits.h>
#endif

#if defined (AIPS_IRIX)
#   undef FLT_DIG
#   undef FLT_MIN
#   undef FLT_MAX
#   undef DBL_DIG
#   undef DBL_MIN
#   undef DBL_MAX
#endif

#include <float.h>
#include <values.h>
#if defined (AIPS_OSF)
#   define LN_MAXFLOAT (M_LN2 * FMAXEXP)
#   define LN_MINFLOAT (M_LN2 * (FMINEXP -1))
#endif
#include <aips/aips.h>

// <summary>Constants class for mathematical, numerical and physical constants.</summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tConstants" demos="">

//# // <prerequisite>
//# // </prerequisite>

//# // <etymology>
//# // </etymology>

// <synopsis>
// The constants and conversion factors are defined here as double precision
// values.  Where single precision calculations are done in a situation where
// processing speed is of concern, for example within the inner loop of an
// expensive algorithm, a separate single precision variable should be defined
// for use within the loop.
//
// </synopsis>

// <h3> Machine constants </h3>
//
// Implementation-defined limits usually defined in <src><limits.h></src>,
// <src><float.></src>h, and <src><values.h></src> as preprocessor
// defines. They are 
// Inclusion of <src><aips/Mathematics/Constants.h</src> is
// sufficient to ensure that they are defined for any particular
// implementation, and the correct functioning of the <src>tConstants</src>
// test program guarantees this.
//
// <srcblock>
//
// Refer to Section 3.2c, pp28-30 of
// "The Annotated C++ Reference Manual",
// Ellis, M.A., and Stroustrup, B.,
// Addison-Wesley Publishing Company, 1990.
// IBSN 0-201-51459-1.
//
//    and
//
// Appendix B11, pp257-8 of
// "The C Programming Language", 2nd ed.,
// Kernighan, B.W., and Ritchie, D.M.,
// Prentice Hall Software Series, 1988.
// IBSN 0-13-110362-8.
//
// </srcblock>

// <h3> Constants defined in limits.h </h3>
// (these are part of the ANSI C and hence POSIX standards).
// Acceptable limits defined by the standard are quoted.
// <srcblock>
//
// CHAR_BIT              8  Maximum bits in a byte.
// CHAR_MIN           0 or  Minimum value of 'char'.
//               SCHAR_MIN
// CHAR_MAX   UCHAR_MAX or  Maximum value of 'char'.
//               SCHAR_MAX
// SCHAR_MIN          -127  Minimum value of 'signed char'.
// SCHAR_MAX          +127  Maximum value of 'signed char'.
// UCHAR_MAX           255  Maximum value of 'unsigned char'.
// MB_LEN_MAX               Maximum bytes in multibyte character.
//
// SHRT_MIN         -32767  Minimum value of 'short'.
// SHRT_MAX         +32767  Maximum value of 'short'.
// USHRT_MAX         65535  Maximum value of 'unsigned short'.
//
// INT_MIN          -32767  Minimum value of 'int'.
// INT_MAX          +32767  Maximum value of 'int'.
// UINT_MAX          65535  Maximum value of 'unsigned int'.
//
// LONG_MIN    -2147483647  Minimum value of 'long'.
// LONG_MAX    +2147483647  Maximum value of 'long'.
// ULONG_MAX    4294967295  Maximum value of 'unsigned long'.
//
// </srcblock>

// <h3> Constants defined in float.h </h3>
// (these are part of the ANSI C and hence POSIX standards).
// Acceptable limits defined by the standard are quoted.
// <srcblock>
//
// FLT_RADIX             2  Radix of exponent representation.
// FLT_ROUNDS               Floating point rounding mode for addition
//                            -1: indeterminate
//                             0: towards zero
//                             1: to nearest
//                             2: toward +infinity
//                             3: toward -infinity
//
// FLT_MIN_EXP              Minimum negative integer N such that FLT_RADIX
// DBL_MIN_EXP              raised to the Nth minus 1 is a normalized
// LDBL_MIN_EXP             floating point number.
//
// FLT_MAX_EXP              Maximum integer N such that FLT_RADIX raised to
// DBL_MAX_EXP              the Nth minus 1 is representable.
// LDBL_MAX_EXP
//
// FLT_MIN_10_EXP      -37  Minimum negative integer N such that 10 raised
// DBL_MIN_10_EXP      -37  to the Nth is in the range of normalized
// LDBL_MIN_10_EXP     -37  floating point numbers.
//
// FLT_MAX_10_EXP       37  Maximum integer N such that 10 raised to the
// DBL_MAX_10_EXP       37  Nth minus 1 is representable.
// LDBL_MAX_10_EXP      37
//
// FLT_MANT_DIG             Number of base FLT_RADIX digits in mantissa.
// DBL_MANT_DIG
// LDBL_MANT_DIG 
//
// FLT_DIG               6  Decimal digits of precision.
// DBL_DIG              10
// LDBL_DIG             10
//
// FLT_EPSILON        1E-5  Minimum floating point number X such  that
// (use C::flt_epsilon in preference to this)
// DBL_EPSILON        1E-9  1.0 + X does not equal 1.0.
// (use C::dbl_epsilon in preference to this)
// LDBL_EPSILON       1E-9
//
// FLT_MIN           1E-37  Minimum normalized positive floating point
// (use C::flt_min in preference to this)
// DBL_MIN           1E-37  number
// (use C::dbl_min in preference to this)
// LDBL_MIN          1E-37
//
// FLT_MAX           1E+37  Maximum representable floating point number.
// (use C::flt_max in preference to this)
// DBL_MAX           1E+37
// (use C::dbl_max in preference to this)
// LDBL_MAX          1E+37
//
// </srcblock>

// <h3> Constants defined in values.h </h3>
// (not part of the POSIX standard).
// <srcblock>
//
// HIBITS            Value of a short integer with only the high-order
//                   bit set (in most implementations, 0x8000).
//
// HIBITL            Value of a long integer with only the high-order
//                   bit set (in most implementations, 0x80000000).
//
// MAXSHORT          Maximum value of a signed short integer (in most
//                   implementations, 0x7FFF = 32767).
//
// MAXLONG           Maximum value of a signed long integer (in most
//                   implementations, 0x7FFFFFFF = 2147483647).
//
// MAXINT            Maximum value of a signed regular integer (usually
//                   the same as MAXSHORT or MAXLONG).
//
// MINFLOAT          Minimum positive value of a single-precision
//                   floating-point number (use C::minfloat in preference 
//                   to this)
//
// MINDOUBLE         Minimum positive value of a double-precision
//                   floating-point number (use C::mindouble in preference 
//                   to this)
//
// MAXFLOAT          Maximum value of a single-precision floating-point number
//
// MAXDOUBLE         Maximum value of a double-precision floating-point number
//
// FSIGNIF           Number of significant bits in the mantissa of a
//                   single-precision floating-point number.
//
// DSIGNIF           Number of significant bits in the mantissa of a
//                   double-precision floating-point number.
// </srcblock>


// <h3> Physical units </h3>
//
// The conversion factors convert to an internal representation.  For
// example, C::inch when used as a factor converts from inchs to internal
// units (in this case metres).  When used as a divisor it converts from
// internal units to inchs.
// <srcblock>
//
// Compound usage accords with conventional dimensional analysis, e.g.:
//
//    * Factor to convert seconds to days:
//         C::second / C::day
//
//    * Factor to convert kilometers to feet:
//         (C::kilo * C::metre) / C::foot
//
//    * Factor to convert acres to hectares:
//         C::acre / (C::hecto * C::are)
//
//    * Factor to convert millibars to kilopascals:
//         (C::milli * C::bar) / (C::kilo * C::Pascal)
//
// The units used internally are as follows:
//
//     angle                        radian
//     solid angle                  steradian
//     time                  [T]    second
//     length                [L]    metre
//     mass                  [M]    kilogram
//     charge                [Q]    Coulomb
//     luminous intensity    [Iv]   candela
//     amount of substance   [N]    mole
//
// </srcblock>
//
// For derived quantities such as volume, velocity, force, and power, the
// units are specified in terms of those of the fundamental quantities.

class C {
public:

   //#--------------------------------------------------------------------
   //  Mathematical constants
   //#--------------------------------------------------------------------
   //# <group>

   // Irrationals:
   // <group>
   // sqrt(2)
   static Double sqrt2;
   // sqrt(3)
   static Double sqrt3;
   // 1/sqrt(2)
   static Double _1_sqrt2;
   // 1/sqrt(3)
   static Double _1_sqrt3;
   // </group>

   // Pi and functions thereof:
   // <group>
   // pi
   static Double pi;
   // 2*pi
   static Double _2pi;
   // pi/2
   static Double pi_2;
   // pi/4
   static Double pi_4;
   // 1/pi
   static Double _1_pi;
   // 2/pi
   static Double _2_pi;
   // 1/sqrt(pi)
   static Double _1_sqrtpi;
   // 2/sqrt(pi)
   static Double _2_sqrtpi;
   // </group>

   // e and functions thereof:
   // <group>
   // e
   static Double e;
   // ln(2)
   static Double ln2;  
   // ln(10)
   static Double ln10; 
   // log2(e)
   static Double log2e; 
   // log10(e)
   static Double log10e;
   // </group>

   // gamma and functions thereof:
   // <group>
   // gamma
   static Double gamma;
   // ln(gamma)
   static Double lngamma;
   // e**gamma
   static Double etogamma;
   // </group>

   //#--------------------------------------------------------------------
   //# Mathematical constants
   //#--------------------------------------------------------------------
   //# </group>


   //#--------------------------------------------------------------------
   //  Machine constants
   //#--------------------------------------------------------------------
   //# <group>
   // floating point limits
  // <group>
  // the minimum single precision floating point number, 
  // excluding denormalised numbers
   static Double flt_min;
  // the minimum single precision floating point number, 
  // including denormalised numbers
   static Double minfloat;
  // the minimum double precision floating point number,
  // excluding denormalised numbers
   static Double dbl_min;
  // the minimum double precision floating point number,
  // including denormalised numbers
   static Double mindouble;
  // the maximum single precision floating point number 
   static Double flt_max;
  // the maximum double precision floating point number 
   static Double dbl_max;
  // Minimum single precision floating point number X such that 1+X does not
  // equal X
   static Double flt_epsilon;
  // Minimum double precision floating point number X such that 1+X does not
  // equal X
   static Double dbl_epsilon;
  // </group>

   //#--------------------------------------------------------------------
   //# Machine constants
   //#--------------------------------------------------------------------
   //# </group>

   //#--------------------------------------------------------------------
   //#  Physical constants, and quantities
   //#--------------------------------------------------------------------
   //# <group>

   // Fundamental physical constants (SI units):
   // <group>
   // velocity of light
   static Double c;
   // Gravitational constant
   static Double Gravity;
   // Planck's constant
   static Double Planck;
   // gas constant
   static Double GasConst;
   // Avogardo's constant
   static Double Avogadro; 
   // electron charge
   static Double qe;
   // proton mass
   static Double mp;
   // proton mass / electron mass
   static Double mp_me;
   // </group>

   // Derived physical constants (SI units):
   // <group>
   // magnetic permeability of vacuum
   static Double mu0;
   // electric permittivity of vacuum
   static Double epsilon0; 
   // Planck's constant divided by 2*pi
   static Double Planck_2pi;
   // atomic mass unit
   static Double u;
   // Boltzmann's constant
   static Double Boltzmann; 
   // Faraday's constant
   static Double Faraday;
   // electron mass
   static Double me;
   // classical electron radius
   static Double re;
   // Bohr radius
   static Double a0;
   // </group>

   // Physical quantities (SI units):
   // <group>
   // solar radius
   static Double R0;
   // </group>

   //#--------------------------------------------------------------------
   //# Physical constants, and quantities
   //#--------------------------------------------------------------------
   //# </group>



   //#--------------------------------------------------------------------
   //#  Physical units
   //#--------------------------------------------------------------------
   //# <group>

   //#-----------------------------
   //#  Numerical conversion factors
   //#-----------------------------
   //# <group>

   //  Numerical conversion factors
   // <group>
   // e+24 (Y)
   static Double yotta;
   // e+21 (Z)
   static Double zetta;
   // e+18 (E)
   static Double exa;
   // e+15 (P)
   static Double peta;
   // e+12 (T)
   static Double tera;
   // e+09 (G)
   static Double giga;
   // e+06 (M)
   static Double mega;
   // e+03 (k)
   static Double kilo;
   // e+02 (h)
   static Double hecto;
   // e+01 (D)
   static Double deka;
   // e-01 (d)
   static Double deci;
   // e-02 (c)
   static Double centi;
   // e-03 (m)
   static Double milli;
   // e-06 (u)
   static Double micro;
   // e-09 (n)
   static Double nano;
   // e-12 (p)
   static Double pico;
   // e-15 (f)
   static Double femto;
   // e-18 (a)
   static Double atto;
   // e-21 (z)
   static Double zepto;
   // e-24 (y)
   static Double yocto;
   // </group>

   // Angular measure:
   // <group>
   // radian
   static Double radian;
   // circle
   static Double circle; 
   // circuit
   static Double circuit;
   // cycle
   static Double cycle;
   // revolution
   static Double rev;
   // revolution
   static Double revolution;
   // rotation
   static Double rotation;
   // degree
   static Double degree;
   // arcminute
   static Double arcmin;
   // arcsecond
   static Double arcsec;
   // grad
   static Double grad;
   // </group>

   // Solid angular measure:
   // <group>
   // steradian
   static Double steradian;
   // sphere
   static Double sphere;
   // square degree
   static Double square_degree;
   // square arcminute
   static Double square_arcmin;
   // square arcsecond
   static Double square_arcsec;
   // </group>

   //#-----------------------------
   //# Numerical conversion factors
   //#-----------------------------
   //# </group>


   //#----------------------------
   //#  Physical conversion factors
   //#----------------------------
   //# <group>

   // Time interval [T]:
   // <group>
   // second
   static Double second;
   // minute
   static Double minute;
   // hour
   static Double hour;
   // day
   static Double day;
   // week
   static Double week;
   // fortnight
   static Double fortnight;
   // </group>

   // Frequency [1/T]:
   // <group>
   // Hertz
   static Double Hertz;
   // </group>

   // Length [L]:
   // <group>
   // metre
   static Double metre;
   // metre (American spelling)
   static Double meter;
   // Fermi
   static Double Fermi;
   // Angstrom
   static Double Angstrom;
   // inch
   static Double inch;
   // thou (inch/1000)
   static Double thou;
   // hand
   static Double hand;
   // span
   static Double span; 
   // foot
   static Double foot;
   // yard
   static Double yard;
   // fathom
   static Double fathom;
   // rod
   static Double rod;
   // perch
   static Double perch;
   // rope
   static Double rope; 
   // chain
   static Double chain;
   // furlong
   static Double furlong;
   // English statute mile
   static Double mile;
   // nautical mile
   static Double nautical_mile;
   // point
   static Double point;
   // pica
   static Double pica;
   // astronomical unit
   static Double astronomical_unit;
   // light second
   static Double light_second;
   // light year
   static Double light_year;
   // parsec
   static Double parsec;
   // </group>

   // Area [L*L]:
   // <group>
   // square metre
   static Double square_metre;
   // square meter (American spelling)
   static Double square_meter;
   // are
   static Double are;
   // barn
   static Double barn;
   // square inch
   static Double square_inch;
   // square foot
   static Double square_foot;
   // square yard
   static Double square_yard;
   // square mile
   static Double square_mile;
   // square perch
   static Double square_perch;
   // rood
   static Double rood;
   // acre 
   static Double acre;
   // square
   static Double square;
   // </group>

   // Volume [L*L*L]:
   // <group>
   // cubic metre
   static Double cubic_metre;
   // cubic meter (American spelling)
   static Double cubic_meter;
   // stere
   static Double stere;
   // litre
   static Double litre;
   // liter (American spelling)  
   static Double liter;
   // cubic inch
   static Double cubic_inch;
   // cubic foot
   static Double cubic_foot;
   // cubic yard
   static Double cubic_yard;
   // cubic mile
   static Double cubic_mile;
   // (Brit) gallon
   static Double gallon;
   // (Brit) quart
   static Double quart;
   // (Brit) pint
   static Double pint;
   // (Brit) gill
   static Double gill;
   // (Brit) fluid ounce
   static Double fluid_ounce; 
   // (Brit) drachm
   static Double drachm;
   // (Brit) scruple
   static Double scruple;
   // (Brit) minim
   static Double minim;
   // (US liq) gallon
   static Double USgallon;
   // (US liq) quart
   static Double USquart;
   // (US liq) pint
   static Double USpint;
   // (US liq) gill
   static Double USgill;
   // (US liq) fluid ounce
   static Double USfluid_ounce;
   // (US liq) dram
   static Double USdram;
   // (US liq) minim
   static Double USminim;
   // </group>

   // Speed [L/T]:
   // nautical miles per hour
   // <group>
   static Double knot;
   // </group>

   // Acceleration (speed / time) [L/(T*T)]:
   // gravitational acceleration
   // <group>
   static Double g;
   // </group>

   // Mass [M]:
   // <group>
    // gram
   static Double gram;
   // metric ton
   static Double tonne;
   // metric carat
   static Double carat;
   // pound (avoirdupois)
   static Double pound;
   // ounce (avoirdupois)
   static Double ounce;
   // stone
   static Double stone;
   // (long) quarter (Brit)
   static Double quarter;
   // (long) hundredweight
   static Double hundredweight;
   // (long) ton
   static Double ton;
   // cental
   static Double cental;
   // short quarter (Brit)
   static Double shortquarter;
   // short hundredweight
   static Double shortcwt;
   // short ton
   static Double shortton;
   // </group>

   // Force (mass * acceleration) [M*L/(T*T)]:
   // <group>
   // Newton
   static Double Newton;
   // dyne
   static Double dyne; 
   // </group>

   // Pressure (force / area) [M/(L*T*T)]:
   // <group>
   // Pascal
   static Double Pascal;
   // atmosphere
   static Double atmosphere;
   // bar
   static Double bar;
   // torr
   static Double torr; 
   // mm of Mercury
   static Double mmHg;
   // </group>

   // Energy (force * length) [M*L*L/(T*T)]:
   // <group>
   // Joule
   static Double Joule;
   // kiloWatt*hour
   static Double kWh;
   // erg
   static Double erg;
   // calorie (thermochemical)
   static Double calorie;
   // calorie (International Steam)
   static Double calorie_IT;
   // British thermal unit
   static Double Btu;
   // electron volt
   static Double eV;
   // </group>

   // Temperature difference (energy) [M*L*L/(T*T)]:
   // <group>
   // Kelvin
   static Double Kelvin;
   // Celsius
   static Double Celsius;
   // Centigrade
   static Double Centigrade;
   // Fahrenheit
   static Double Fahrenheit;
   // Rankine
   static Double Rankine;
   // </group>

   // Temperature at 0 on each temperature scale:
   // <group>
   static Double Kelvin_0;
   static Double Celsius_0;
   static Double Centigrade_0;
   static Double Fahrenheit_0;
   static Double Rankine_0;
   // </group>

   // Power (energy / time) [M*L*L/(T*T*T)]:
   // <group>
   // Watt
   static Double Watt;
   // horsepower
   static Double horsepower;
   // </group>

   // Flux density (power / area / frequency) [M/(T*T)]:
   // <group>
   // Jansky
   static Double Jansky;
   // flux units
   static Double fu;
   // </group>

   // Electric charge [Q] (Coulomb):
   // <group>
   // Coulomb
   static Double Coulomb;
   // abCoulomb (emu)
   static Double abCoulomb;
   // statCoulomb (esu)
   static Double statCoulomb;
   // </group>

   // Electric current (charge / time) [Q/T]:
   // <group>
   // Ampere
   static Double Ampere;
   // abAmpere (emu)
   static Double abAmpere;
   // statAmpere (esu)
   static Double statAmpere;
   // </group>

   // Electric field strength (force / charge) [M*L/(T*T*Q)]:

   // Electric potential (energy / charge) [M*L*L/(T*T*Q)]:
   // <group>
   // Volt
   static Double Volt;
   // abVolt (emu)
   static Double abVolt;
   // statVolt (esu)
   static Double statVolt;
   // </group>

   // Electric resistance (potential / current) [M*L*L/(T*Q*Q)]]:
   // <group>
   // Ohm
   static Double Ohm;
   // abOhm (emu)
   static Double abOhm;
   // starOhm (esu)
   static Double statOhm;
   // </group>

   // Electric conductance (current / potential) [T*Q*Q/(M*L*L)]:
   // <group>
   // Siemens
   static Double Siemens; 
   // mho
   static Double mho;
   // </group>

   // Electric capacitance (charge / potential) [T*T*Q*Q/(M*L*L)]:
   // <group>
   // Farad
   static Double Farad;
   // abFarad (emu)
   static Double abFarad;
   // statFarad (esu)
   static Double statFarad;
   // </group>

   // Electric inductance (potential * time / current) [M*L*L/(Q*Q)]:
   // <group>
   // Henry
   static Double Henry;
   // abHenry (emu)
   static Double abHenry;
   // statHenry (esu)
   static Double statHenry;
   // </group>

   // Magnetic induction (force / charge / velocity) [M/(T*Q)]:
   // <group>
   // Tesla
   static Double Tesla;
   // Gauss (emu)
   static Double Gauss;
   // </group>

   // Magnetic flux (magnetic induction * area) [M*L*L/(T*Q)]:
   // <group>
   // Weber
   static Double Weber;
   // Maxwell (emu)
   static Double Maxwell;
   // line (emu)
   static Double line;
   // </group>

   // Magnetomotance = magnetomotive force (current) [Q/T]:
   // <group>
   // Ampere-turn
   static Double Ampere_turn;
   // abAmpere-turn
   static Double abAmpere_turn;
   // Gilbert (emu)
   static Double Gilbert;
   // praGilbert (emu)
   static Double praGilbert;
   // </group>

   // Magnetic field intensity (current / length) [Q/(T*L)]:
   // <group>
   // Oersted (emu)
   static Double Oersted;
   // praOersted (emu)
   static Double praOersted;
   // </group>

   // Radioactivity (Bequerel):
   // Bequerel
   static Double Bequerel;

   // Luminous intensity [Iv]:
   // candela
   static Double candela; 

   // Amount of substance [N]:
   // <group>
   // mole
   static Double mole;
   // number of molecules
   static Double molecule;
   // </group>

   //#----------------------------
   //# Physical conversion factors
   //#----------------------------
   //# </group>

   //#--------------------------------------------------------------------
   //# Physical units
   //#--------------------------------------------------------------------
   //# </group>

};


// <summary>
// Class used to force construction of <linkto class=C>C</linkto>.
// </summary>

// <synopsis>
// A static object of this class is used to make sure that
// <linkto class=C>C</linkto>
// is constructed before it is needed, and therefore that its static data
// members are defined.  See Meyers, p. 47.
// </synopsis>

// <use visibility=local>

// <linkfrom anchor="C_init" classes="C">
//   <here>C_init</here> --
// Class used to force construction of <linkto class=C>C</linkto>.
// </linkfrom>

class C_init {
  public:
     C_init();
     ~C_init();
  private:
     static uShort count;
};

// <summary>
// Object used to force construction of <linkto class=C>C</linkto>.
// </summary>

// <synopsis>
// This static object of the <linkto class=C_init>C_init</linkto>
// class is used to make sure that
// <linkto class=C>C</linkto>
// is constructed before it is needed, and therefore that its static data
// members are defined.  See Meyers, p. 47.
// </synopsis>

// <use visibility=local>

// <linkfrom anchor="C initialization object" classes="C C_init">
//   <here>C initialization object</here> --
// Object used to force construction of <linkto class=C>C</linkto>.
// </linkfrom>

// <group name="C initialization object">

static C_init c_init;

// </group>

#endif
