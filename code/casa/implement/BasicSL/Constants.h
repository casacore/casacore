//# Constants.h: Mathematical and physical constants
//# Copyright (C) 1993, 1994, 1995
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

#if defined(_AIX)
#pragma implementation ("<ClassFileName>.cc")
#endif 

#include <float.h>
#include <values.h>

#include <aips/aips.h>

// <summary>
// Constants class for mathematical and physical constants.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tConstants" demos="">

//# // <prerequisite>
//# // </prerequisite>

//# // <etymology>
//# // </etymology>

// <synopsis>
// All constants and conversion factors are here defined as double precision
// values.  Where single precision calculations are done in a situation where
// processing speed is of concern, for example within the inner loop of an
// expensive algorithm, a separate single precision variable should be defined
// for use within the loop.
//
// See the source file where <linkto class=C>C</linkto> is defined to see
// what contants are defined, and what their names are.
// </synopsis>

// <h3> Machine constants </h3>
//
// Implementation-defined limits usually defined in <src><limits.h></src>,
// <src><float.></src>h, and <src><values.h></src> as preprocessor defines.
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
// DBL_EPSILON        1E-9  1.0 + X does not equal 1.0.
// LDBL_EPSILON       1E-9
//
// FLT_MIN           1E-37  Minimum normalized positive floating point
// DBL_MIN           1E-37  number.
// LDBL_MIN          1E-37
//
// FLT_MAX           1E+37  Maximum representable floating point number.
// DBL_MAX           1E+37
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
// HIBITI            Value of a regular integer with only the high-order
//                   bit set (usually the same as HIBITS or HIBITL).
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
// LN_MINFLOAT       floating-point number, and its natural logarithm.
//
// MINDOUBLE         Minimum positive value of a double-precision
// LN_MINDOUBLE      floating-point number, and its natural logarithm.
//
// MAXFLOAT          Maximum value of a single-precision
// LN_MAXFLOAT       floating- point number, and its natural logarithm.
//
// MAXDOUBLE         Maximum value of a double-precision
// LN_MAXDOUBLE      floating- point number, and its natural logarithm.
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

struct C {


   //#--------------------------------------------------------------------
   //  Mathematical constants
   //#--------------------------------------------------------------------
   // <group>

   // Irrationals:
   // <group>
   static Double sqrt2;               // sqrt(2)
   static Double sqrt3;               // sqrt(3)
   static Double _1_sqrt2;            // 1/sqrt(2)
   static Double _1_sqrt3;            // 1/sqrt(3)
   // </group>

   // Pi and functions thereof:
   // <group>
   static Double pi;                  // pi
   static Double _2pi;                // 2*pi
   static Double pi_2;                // pi/2
   static Double pi_4;                // pi/4
   static Double _1_pi;               // 1/pi
   static Double _2_pi;               // 2/pi
   static Double _1_sqrtpi;           // 1/sqrt(pi)
   static Double _2_sqrtpi;           // 2/sqrt(pi)
   // </group>

   // e and functions thereof:
   // <group>
   static Double e;                   // e
   static Double ln2;                 // ln(2)
   static Double ln10;                // ln(10)
   static Double log2e;               // log2(e)
   static Double log10e;              // log10(e)
   // </group>

   // gamma and functions thereof:
   // <group>
   static Double gamma;               // gamma
   static Double lngamma;             // ln(gamma)
   static Double etogamma;            // e**gamma
   // </group>

   //#--------------------------------------------------------------------
   //# Mathematical constants
   //#--------------------------------------------------------------------
   // </group>


   //#--------------------------------------------------------------------
   //  Physical constants, and quantities
   //#--------------------------------------------------------------------
   // <group>

   // Fundamental physical constants (SI units):
   // <group>
   static Double c;                   // velocity of light
   static Double Gravity;             // Gravitational constant
   static Double Planck;              // Planck's constant
   static Double GasConst;            // gas constant
   static Double Avogadro;            // Avogardo's constant
   static Double qe;                  // electron charge
   static Double mp;                  // proton mass
   static Double mp_me;               // proton mass / electron mass
   // </group>

   // Derived physical constants (SI units):
   // <group>
   static Double mu0;                 // magnetic permeability of vacuum
   static Double epsilon0;            // electric permittivity of vacuum
   static Double Planck_2pi;          // Planck's constant divided by 2*pi
   static Double u;                   // atomic mass unit
   static Double Boltzmann;           // Boltzmann's constant
   static Double Faraday;             // Faraday's constant
   static Double me;                  // electron mass
   static Double re;                  // classical electron radius
   static Double a0;                  // Bohr radius
   // </group>

   // Physical quantities (SI units):
   static Double R0;                  // solar radius

   //#--------------------------------------------------------------------
   //# Physical constants, and quantities
   //#--------------------------------------------------------------------
   // </group>



   //#--------------------------------------------------------------------
   //  Physical units
   //#--------------------------------------------------------------------
   // <group>

   //#-----------------------------
   //  Numerical conversion factors
   //#-----------------------------
   // <group>

   static Double yotta;               // e+24 (Y)
   static Double zetta;               // e+21 (Z)
   static Double exa;                 // e+18 (E)
   static Double peta;                // e+15 (P)
   static Double tera;                // e+12 (T)
   static Double giga;                // e+09 (G)
   static Double mega;                // e+06 (M)
   static Double kilo;                // e+03 (k)
   static Double hecto;               // e+02 (h)
   static Double deka;                // e+01 (D)
   static Double deci;                // e-01 (d)
   static Double centi;               // e-02 (c)
   static Double milli;               // e-03 (m)
   static Double micro;               // e-06 (u)
   static Double nano;                // e-09 (n)
   static Double pico;                // e-12 (p)
   static Double femto;               // e-15 (f)
   static Double atto;                // e-18 (a)
   static Double zepto;               // e-21 (z)
   static Double yocto;               // e-24 (y)

   // Angular measure:
   // <group>
   static Double radian;              // radian
   static Double circle;              // circle
   static Double circuit;             // circuit
   static Double cycle;               // cycle
   static Double rev;                 // revolution
   static Double revolution;          // revolution
   static Double rotation;            // rotation
   static Double degree;              // degree
   static Double arcmin;              // arcminute
   static Double arcsec;              // arcsecond
   static Double grad;                // grad
   // </group>

   // Solid angular measure:
   // <group>
   static Double steradian;           // steradian
   static Double sphere;              // sphere
   static Double square_degree;       // square degree
   static Double square_arcmin;       // square arcminute
   static Double square_arcsec;       // square arcsecond
   // </group>

   //#-----------------------------
   //# Numerical conversion factors
   //#-----------------------------
   // </group>


   //#----------------------------
   //  Physical conversion factors
   //#----------------------------
   // <group>

   // Time interval [T]:
   // <group>
   static Double second;              // second
   static Double minute;              // minute
   static Double hour;                // hour
   static Double day;                 // day
   static Double week;                // week
   static Double fortnight;           // fortnight
   // </group>

   // Frequency [1/T]:
   static Double Hertz;               // Hertz

   // Length [L]:
   // <group>
   static Double metre;               // metre
   static Double meter;               // metre (American spelling)
   static Double Fermi;               // Fermi
   static Double Angstrom;            // Angstrom
   static Double inch;                // inch
   static Double thou;                // thou (inch/1000)
   static Double hand;                // hand
   static Double span;                // span
   static Double foot;                // foot
   static Double yard;                // yard
   static Double fathom;              // fathom
   static Double rod;                 // rod
   static Double perch;               // perch
   static Double rope;                // rope
   static Double chain;               // chain
   static Double furlong;             // furlong
   static Double mile;                // English statute mile
   static Double nautical_mile;       // nautical mile
   static Double point;               // point
   static Double pica;                // pica
   static Double astronomical_unit;   // astronomical unit
   static Double light_second;        // light second
   static Double light_year;          // light year
   static Double parsec;              // parsec
   // </group>

   // Area [L*L]:
   // <group>
   static Double square_metre;        // square metre
   static Double square_meter;        // square meter (American spelling)
   static Double are;                 // are
   static Double barn;                // barn
   static Double square_inch;         // square inch
   static Double square_foot;         // square foot
   static Double square_yard;         // square yard
   static Double square_mile;         // square mile
   static Double square_perch;        // square perch
   static Double rood;                // rood
   static Double acre;                // acre
   static Double square;              // square
   // </group>

   // Volume [L*L*L]:
   // <group>
   static Double cubic_metre;         // cubic metre
   static Double cubic_meter;         // cubic meter (American spelling)
   static Double stere;               // stere
   static Double litre;               // litre
   static Double liter;               // liter (American spelling)
   static Double cubic_inch;          // cubic inch
   static Double cubic_foot;          // cubic foot
   static Double cubic_yard;          // cubic yard
   static Double cubic_mile;          // cubic mile
   static Double gallon;              // (Brit) gallon
   static Double quart;               // (Brit) quart
   static Double pint;                // (Brit) pint
   static Double gill;                // (Brit) gill
   static Double fluid_ounce;         // (Brit) fluid ounce
   static Double drachm;              // (Brit) drachm
   static Double scruple;             // (Brit) scruple
   static Double minim;               // (Brit) minim
   static Double USgallon;            // (US liq) gallon
   static Double USquart;             // (US liq) quart
   static Double USpint;              // (US liq) pint
   static Double USgill;              // (US liq) gill
   static Double USfluid_ounce;       // (US liq) fluid ounce
   static Double USdram;              // (US liq) dram
   static Double USminim;             // (US liq) minim
   // </group>

   // Speed [L/T]:
   static Double knot;                // nautical miles per hour

   // Acceleration (speed / time) [L/(T*T)]:
   static Double g;                   // gravitational acceleration

   // Mass [M]:
   // <group>
   static Double gram;                // gram
   static Double tonne;               // metric ton
   static Double carat;               // metric carat
   static Double pound;               // pound (avoirdupois)
   static Double ounce;               // ounce (avoirdupois)
   static Double stone;               // stone
   static Double quarter;             // (long) quarter (Brit)
   static Double hundredweight;       // (long) hundredweight
   static Double ton;                 // (long) ton
   static Double cental;              // cental
   static Double shortquarter;        // short quarter (Brit)
   static Double shortcwt;            // short hundredweight
   static Double shortton;            // short ton
   // </group>

   // Force (mass * acceleration) [M*L/(T*T)]:
   // <group>
   static Double Newton;              // Newton
   static Double dyne;                // dyne
   // </group>

   // Pressure (force / area) [M/(L*T*T)]:
   // <group>
   static Double Pascal;              // Pascal
   static Double atmosphere;          // atmosphere
   static Double bar;                 // bar
   static Double torr;                // torr
   static Double mmHg;                // mm of Mercury
   // </group>

   // Energy (force * length) [M*L*L/(T*T)]:
   // <group>
   static Double Joule;               // Joule
   static Double kWh;                 // kiloWatt*hour
   static Double erg;                 // erg
   static Double calorie;             // calorie (thermochemical)
   static Double calorie_IT;          // calorie (International Steam)
   static Double Btu;                 // British thermal unit
   static Double eV;                  // electron volt
   // </group>

   // Temperature difference (energy) [M*L*L/(T*T)]:
   // <group>
   static Double Kelvin;              // Kelvin
   static Double Celsius;             // Celsius
   static Double Centigrade;          // Centigrade
   static Double Fahrenheit;          // Fahrenheit
   static Double Rankine;             // Rankine
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
   static Double Watt;                // Watt
   static Double horsepower;          // horsepower
   // </group>

   // Flux density (power / area / frequency) [M/(T*T)]:
   // <group>
   static Double Jansky;              // Jansky
   static Double fu;                  // flux units
   // </group>

   // Electric charge [Q] (Coulomb):
   // <group>
   static Double Coulomb;             // Coulomb
   static Double abCoulomb;           // abCoulomb (emu)
   static Double statCoulomb;         // statCoulomb (esu)
   // </group>

   // Electric current (charge / time) [Q/T]:
   // <group>
   static Double Ampere;              // Ampere
   static Double abAmpere;            // abAmpere (emu)
   static Double statAmpere;          // statAmpere (esu)
   // </group>

   // Electric field strength (force / charge) [M*L/(T*T*Q)]:

   // Electric potential (energy / charge) [M*L*L/(T*T*Q)]:
   // <group>
   static Double Volt;                // Volt
   static Double abVolt;              // abVolt (emu)
   static Double statVolt;            // statVolt (esu)
   // </group>

   // Electric resistance (potential / current) [M*L*L/(T*Q*Q)]]:
   // <group>
   static Double Ohm;                 // Ohm
   static Double abOhm;               // abOhm (emu)
   static Double statOhm;             // starOhm (esu)
   // </group>

   // Electric conductance (current / potential) [T*Q*Q/(M*L*L)]:
   // <group>
   static Double Siemens;             // Siemens
   static Double mho;                 // mho
   // </group>

   // Electric capacitance (charge / potential) [T*T*Q*Q/(M*L*L)]:
   // <group>
   static Double Farad;               // Farad
   static Double abFarad;             // abFarad (emu)
   static Double statFarad;           // statFarad (esu)
   // </group>

   // Electric inductance (potential * time / current) [M*L*L/(Q*Q)]:
   // <group>
   static Double Henry;               // Henry
   static Double abHenry;             // abHenry (emu)
   static Double statHenry;           // statHenry (esu)
   // </group>

   // Magnetic induction (force / charge / velocity) [M/(T*Q)]:
   // <group>
   static Double Tesla;               // Tesla
   static Double Gauss;               // Gauss (emu)
   // </group>

   // Magnetic flux (magnetic induction * area) [M*L*L/(T*Q)]:
   // <group>
   static Double Weber;               // Weber
   static Double Maxwell;             // Maxwell (emu)
   static Double line;                // line (emu)
   // </group>

   // Magnetomotance = magnetomotive force (current) [Q/T]:
   // <group>
   static Double Ampere_turn;         // Ampere-turn
   static Double abAmpere_turn;       // abAmpere-turn
   static Double Gilbert;             // Gilbert (emu)
   static Double praGilbert;          // praGilbert (emu)
   // </group>

   // Magnetic field intensity (current / length) [Q/(T*L)]:
   // <group>
   static Double Oersted;             // Oersted (emu)
   static Double praOersted;          // praOersted (emu)
   // </group>

   // Radioactivity (Bequerel):
   static Double Bequerel;            // Bequerel

   // Luminous intensity [Iv]:
   static Double candela;             // candela

   // Amount of substance [N]:
   // <group>
   static Double mole;                // mole
   static Double molecule;            // number of molecules
   // </group>

   //#----------------------------
   //# Physical conversion factors
   //#----------------------------
   // </group>

   //#--------------------------------------------------------------------
   //# Physical units
   //#--------------------------------------------------------------------
   // </group>

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
