//# Constants.cc: Mathematical and physical constants
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

#include <aips/Mathematics/Constants.h>

//----------------------------------------------------------------------------
// Mathematical constants
//----------------------------------------------------------------------------
// Reference:
// "Handbook of Mathematical Functions",
// Abramowitz & Stegun, 1964.
// Dover Publications Inc., New York (9th printing, 1972)
//----------------------------------------------------------------------------

// Irrationals:
Double C::sqrt2               ;
Double C::sqrt3               ;
Double C::_1_sqrt2            ;
Double C::_1_sqrt3            ;

// Pi and functions thereof:
Double C::pi                  ;
Double C::_2pi                ;
Double C::pi_2                ;
Double C::pi_4                ;
Double C::_1_pi               ;
Double C::_2_pi               ;
Double C::_1_sqrtpi           ;
Double C::_2_sqrtpi           ;

// e and functions thereof:
Double C::e                   ;
Double C::ln2                 ;
Double C::ln10                ;
Double C::log2e               ;
Double C::log10e              ;

// gamma and functions thereof:
Double C::gamma               ;
Double C::lngamma             ;
Double C::etogamma            ;


//----------------------------------------------------------------------------
// Machine constants
//----------------------------------------------------------------------------
Double C::flt_min;
Double C::minfloat;
Double C::dbl_min;
Double C::mindouble;
Double C::flt_max;
Double C::dbl_max;
Double C::flt_epsilon;
Double C::dbl_epsilon;

//----------------------------------------------------------------------------
// Physical constants and conversion factors
//----------------------------------------------------------------------------
// References:
//     "Explanatory Supplement to the Astronomical Almanac",
//
//     "Handbook of Chemistry and Physics", 55th ed.
//      Robert C. Weast (Ed), 1974.
//      CRC Press Inc.
//      ISBN 087819-54-1
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// Physical constants, and quantities
//----------------------------------------------------------------------------

// Fundamental physical constants (SI units):
Double C::c                   ;
Double C::Gravity             ;
Double C::Planck              ;
Double C::GasConst            ;
Double C::Avogadro            ;
Double C::qe                  ;
Double C::mp                  ;
Double C::mp_me               ;

// Derived physical constants (SI units):
Double C::mu0                 ;
Double C::epsilon0            ;
Double C::Planck_2pi          ;
Double C::u                   ;
Double C::Boltzmann           ;
Double C::Faraday             ;
Double C::me                  ;
Double C::re                  ;
Double C::a0                  ;

// Physical quantities (SI units):
Double C::R0                  ;

//----------------------------------------------------------------------------
// Numerical conversion factors
//----------------------------------------------------------------------------

// Metric prefixes (unity):
Double C::yotta               ;
Double C::zetta               ;
Double C::exa                 ;
Double C::peta                ;
Double C::tera                ;
Double C::giga                ;
Double C::mega                ;
Double C::kilo                ;
Double C::hecto               ;
Double C::deka                ;
Double C::deci                ;
Double C::centi               ;
Double C::milli               ;
Double C::micro               ;
Double C::nano                ;
Double C::pico                ;
Double C::femto               ;
Double C::atto                ;
Double C::zepto               ;
Double C::yocto               ;

// Angular measure:
Double C::radian              ;
Double C::circle              ;
Double C::circuit             ;
Double C::cycle               ;
Double C::rev                 ;
Double C::revolution          ;
Double C::rotation            ;
Double C::degree              ;
Double C::arcmin              ;
Double C::arcsec              ;
Double C::grad                ;

// Solid angular measure:
Double C::steradian           ;
Double C::sphere              ;
Double C::square_degree       ;
Double C::square_arcmin       ;
Double C::square_arcsec       ;


//----------------------------------------------------------------------------
// Physical conversion factors
//----------------------------------------------------------------------------

// Time interval [T]:
Double C::second              ;
Double C::minute              ;
Double C::hour                ;
Double C::day                 ;
Double C::week                ;
Double C::fortnight           ;

// Frequency [1/T]:
Double C::Hertz               ;

// Length [L]:
Double C::metre               ;
Double C::meter               ;
Double C::Fermi               ;
Double C::Angstrom            ;
Double C::inch                ;
Double C::thou                ;
Double C::hand                ;
Double C::span                ;
Double C::foot                ;
Double C::yard                ;
Double C::fathom              ;
Double C::rod                 ;
Double C::perch               ;
Double C::rope                ;
Double C::chain               ;
Double C::furlong             ;
Double C::mile                ;
Double C::nautical_mile       ;
Double C::point               ;
Double C::pica                ;
Double C::astronomical_unit   ;
Double C::light_second        ;
Double C::light_year          ;
Double C::parsec              ;

// Area [L*L]:
Double C::square_metre        ;
Double C::square_meter        ;
Double C::are                 ;
Double C::barn                ;
Double C::square_inch         ;
Double C::square_foot         ;
Double C::square_yard         ;
Double C::square_mile         ;
Double C::square_perch        ;
Double C::rood                ;
Double C::acre                ;
Double C::square              ;

// Volume [L*L*L]:
Double C::cubic_metre         ;
Double C::cubic_meter         ;
Double C::stere               ;
Double C::litre               ;
Double C::liter               ;
Double C::cubic_inch          ;
Double C::cubic_foot          ;
Double C::cubic_yard          ;
Double C::cubic_mile          ;
Double C::gallon              ;
Double C::quart               ;
Double C::pint                ;
Double C::gill                ;
Double C::fluid_ounce         ;
Double C::drachm              ;
Double C::scruple             ;
Double C::minim               ;
Double C::USgallon            ;
Double C::USquart             ;
Double C::USpint              ;
Double C::USgill              ;
Double C::USfluid_ounce       ;
Double C::USdram              ;
Double C::USminim             ;

// Speed [L/T]:
Double C::knot                ;

// Acceleration (speed / time) [L/(T*T)]:
Double C::g                   ;

// Mass [M]:
Double C::gram                ;
Double C::tonne               ;
Double C::carat               ;
Double C::pound               ;
Double C::ounce               ;
Double C::stone               ;
Double C::quarter             ;
Double C::hundredweight       ;
Double C::ton                 ;
Double C::cental              ;
Double C::shortquarter        ;
Double C::shortcwt            ;
Double C::shortton            ;

// Force (mass * acceleration) [M*L/(T*T)]:
Double C::Newton              ;

Double C::dyne                ;

// Pressure (force / area) [M/(L*T*T)]:
Double C::Pascal              ;
Double C::atmosphere          ;
Double C::bar                 ;
Double C::torr                ;
Double C::mmHg                ;

// Energy (force * length) [M*L*L/(T*T)]:
Double C::Joule               ;
Double C::kWh                 ;
Double C::erg                 ;
Double C::calorie             ;
Double C::calorie_IT          ;
Double C::Btu                 ;
Double C::eV                  ;

// Temperature difference (energy) [M*L*L/(T*T)]:
Double C::Kelvin              ;
Double C::Celsius             ;
Double C::Centigrade          ;
Double C::Fahrenheit          ;
Double C::Rankine             ;

// Temperature at 0 on each temperature scale:
Double C::Kelvin_0            ;
Double C::Celsius_0           ;
Double C::Centigrade_0        ;
Double C::Fahrenheit_0        ;
Double C::Rankine_0           ;

// Power (energy / time) [M*L*L/(T*T*T)]:
Double C::Watt                ;
Double C::horsepower          ;

// Flux density (power / area / frequency) [M/(T*T)]:
Double C::Jansky              ;
Double C::fu                  ;

// Electric charge [Q] (Coulomb):
Double C::Coulomb             ;
Double C::abCoulomb           ;
Double C::statCoulomb         ;

// Electric current (electric charge / time) [Q/T]:
Double C::Ampere              ;
Double C::abAmpere            ;
Double C::statAmpere          ;

// Electric field strength (force / electric charge) [M*L/(T*T*Q)]:

// Electric potential (energy / electric charge) [M*L*L/(T*T*Q)]:
Double C::Volt                ;
Double C::abVolt              ;
Double C::statVolt            ;

// Electric resistance (electric potential / current) [M*L*L/(T*Q*Q)]]:
Double C::Ohm                 ;
Double C::abOhm               ;
Double C::statOhm             ;

// Electric conductance (current / electric potential) [T*Q*Q/(M*L*L)]:
Double C::Siemens             ;
Double C::mho                 ;

// Electric capacitance (charge / electric potential) [T*T*Q*Q/(M*L*L)]:
Double C::Farad               ;
Double C::abFarad             ;
Double C::statFarad           ;

// Electric inductance (electric potential * time / current) [M*L*L/(Q*Q)]:
Double C::Henry               ;
Double C::abHenry             ;
Double C::statHenry           ;

// Magnetic induction (force / electric charge / velocity) [M/(T*Q)]:
Double C::Tesla               ;
Double C::Gauss               ;

// Magnetic flux (magnetic induction * area) [M*L*L/(T*Q)]:
Double C::Weber               ;
Double C::Maxwell             ;
Double C::line                ;

// Magnetomotance ;
Double C::Ampere_turn         ;
Double C::abAmpere_turn       ;
Double C::Gilbert             ;
Double C::praGilbert          ;

// Magnetic field intensity (electric current / length) [Q/(T*L)]:
Double C::Oersted             ;
Double C::praOersted          ;

// Radioactivity (Bequerel):
Double C::Bequerel            ;

// Luminous intensity [Iv]:
Double C::candela             ;

// Amount of substance [N]:
Double C::mole                ;
Double C::molecule            ;

//		This is the class that is used to make sure
//		C gets instantiated.  There is a static 
//		C_init object defined in Constants.h
//		See Meyers, item 47.

uShort C_init::count;

C_init::C_init()
{
   if (count++ == 0) {
// Fill them all

// Irrationals:
C::sqrt2               =  1.4142135623730950488;
C::sqrt3               =  1.7320508075688772935;
C::_1_sqrt2            =  0.70710678118654752440;
C::_1_sqrt3            =  0.57735026918962576451;

// Pi and functions thereof:
C::pi                  =  3.141592653589793238462643;
C::_2pi                =  6.283185307179586476925286;
C::pi_2                =  1.570796326794896619231322;
C::pi_4                =  0.7853981633974483096156608;
C::_1_pi               =  0.3183098861837906715377675;
C::_2_pi               =  0.6366197723675813430755350;
C::_1_sqrtpi           =  0.5641895835477562869480795;
C::_2_sqrtpi           =  1.1283791670955125738961590;

// e and functions thereof:
C::e                   =  2.718281828459045235360287;
C::ln2                 =  0.6931471805599453094172321;
C::ln10                =  2.3025850929940456840179915;
C::log2e               =  1.4426950408889634074;
C::log10e              =  0.4342944819032518276511289;

// gamma and functions thereof:
C::gamma               =  0.577215664901532860606512;
C::lngamma             = -0.549539312981644822337662;
C::etogamma            =  1.7810724179901979852;


//----------------------------------------------------------------------------
// Machine constants
//----------------------------------------------------------------------------

// Minimum normalised number
 C::flt_min = FLT_MIN;
 C::dbl_min = DBL_MIN;

// Minimum denormalised number
//# Any double closer to zero than -2.2250738585072017e-308 (except 0) causes a
//# Floating Exception on the alpha when used in any way.
#ifdef __alpha
 C::minfloat = FLT_MIN;
 C::mindouble = DBL_MIN;
#else
 C::minfloat = MINFLOAT;
 C::mindouble = MINDOUBLE;
#endif

 // Maximum floating point number
 C::flt_max = FLT_MAX;
 C::dbl_max = DBL_MAX;

 // minimum value such that  (1+epsilon) != 1
 C::flt_epsilon = FLT_EPSILON;
 C::dbl_epsilon = DBL_EPSILON;

//----------------------------------------------------------------------------
// Physical constants and conversion factors
//----------------------------------------------------------------------------
// References:
//     "Explanatory Supplement to the Astronomical Almanac",
//
//     "Handbook of Chemistry and Physics", 55th ed.
//      Robert C. Weast (Ed), 1974.
//      CRC Press Inc.
//      ISBN 087819-54-1
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// Physical constants, and quantities
//----------------------------------------------------------------------------

// Fundamental physical constants (SI units):
C::c                   = 2.99792458e+08;
C::Gravity             = 6.67259e-11;
C::Planck              = 6.6260755e-34;
C::GasConst            = 8.314510;
C::Avogadro            = 6.0221367e+23;
C::qe                  = 1.60217733e-19;
C::mp                  = 1.6726231e-27;
C::mp_me               = 1836.152701;

// Derived physical constants (SI units):
C::mu0                 = 4.0e-7 * C::pi;
C::epsilon0            = 1.0 / ( C::mu0 * C::c * C::c );
C::Planck_2pi          = C::Planck / C::_2pi;
C::u                   = 1.0e-3 / C::Avogadro;
C::Boltzmann           = C::GasConst / C::Avogadro;
C::Faraday             = C::Avogadro * C::qe;
C::me                  = C::mp / C::mp_me;
C::re                  = 2.8179e-15;
C::a0                  = 5.2918e-11;

// Physical quantities (SI units):
C::R0                  = 6.9599e+08;

//----------------------------------------------------------------------------
// Numerical conversion factors
//----------------------------------------------------------------------------

// Metric prefixes (unity):
C::yotta               = 1.0e+24;
C::zetta               = 1.0e+21;
C::exa                 = 1.0e+18;
C::peta                = 1.0e+15;
C::tera                = 1.0e+12;
C::giga                = 1.0e+09;
C::mega                = 1.0e+06;
C::kilo                = 1.0e+03;
C::hecto               = 1.0e+02;
C::deka                = 1.0e+01;
C::deci                = 1.0e-01;
C::centi               = 1.0e-02;
C::milli               = 1.0e-03;
C::micro               = 1.0e-06;
C::nano                = 1.0e-09;
C::pico                = 1.0e-12;
C::femto               = 1.0e-15;
C::atto                = 1.0e-18;
C::zepto               = 1.0e-21;
C::yocto               = 1.0e-24;

// Angular measure:
C::radian              = 1.0;
C::circle              = (2.0 * C::pi) * C::radian;
C::circuit             =                 C::circle;
C::cycle               =                 C::circle;
C::rev                 =                 C::circle;
C::revolution          =                 C::circle;
C::rotation            =                 C::circle;
C::degree              = (1.0 / 360.0) * C::circle;
C::arcmin              = (1.0 /  60.0) * C::degree;
C::arcsec              = (1.0 /  60.0) * C::arcmin;
C::grad                = (1.0 / 400.0) * C::circle;

// Solid angular measure:
C::steradian           = 1.0;
C::sphere              = (4.0 * C::pi) * C::steradian;
C::square_degree       =     C::degree * C::degree;
C::square_arcmin       =     C::arcmin * C::arcmin;
C::square_arcsec       =     C::arcsec * C::arcsec;


//----------------------------------------------------------------------------
// Physical conversion factors
//----------------------------------------------------------------------------

// Time interval [T]:
C::second              = 1.0;
C::minute              = 60.0 * C::second;
C::hour                = 60.0 * C::minute;
C::day                 = 24.0 * C::hour;
C::week                =  7.0 * C::day;
C::fortnight           =  2.0 * C::week;

// Frequency [1/T]:
C::Hertz               = 1.0 / C::second;

// Length [L]:
C::metre               =  1.0;
C::meter               =                C::metre;
C::Fermi               =      1.0e-15 * C::metre;
C::Angstrom            =      1.0e-10 * C::metre;
C::inch                =      2.54e-2 * C::metre;
C::thou                =       1.0e-3 * C::inch;
C::hand                =          4.0 * C::inch;
C::span                =          9.0 * C::inch;
C::foot                =         12.0 * C::inch;
C::yard                =          3.0 * C::foot;
C::fathom              =          6.0 * C::foot;
C::rod                 =         16.5 * C::foot;
C::perch               =                C::rod;
C::rope                =         20.0 * C::foot;
C::chain               =         22.0 * C::yard;
C::furlong             =        220.0 * C::yard;
C::mile                =       5280.0 * C::foot;
C::nautical_mile       =       6080.0 * C::foot;
C::point               = (1.0 / 72.0) * C::inch;
C::pica                =         12.0 * C::point;
C::astronomical_unit   = 1.4959787066e+11 * C::metre;
C::light_second        =             C::c * C::metre;
C::light_year          =   9.46073047e+15 * C::metre;
C::parsec              =       3.26156378 * C::light_year;

// Area [L*L]:
C::square_metre        = C::metre * C::metre;
C::square_meter        =            C::square_metre;
C::are                 =    100.0 * C::square_metre;
C::barn                =  1.0e-28 * C::square_metre;
C::square_inch         =  C::inch * C::inch;
C::square_foot         =  C::foot * C::foot;
C::square_yard         =  C::yard * C::yard;
C::square_mile         =  C::mile * C::mile;
C::square_perch        = C::perch * C::perch;
C::rood                =     40.0 * C::square_perch;
C::acre                =      4.0 * C::rood;
C::square              =    100.0 * C::square_foot;

// Volume [L*L*L]:
C::cubic_metre         =     C::metre * C::square_metre;
C::cubic_meter         =                C::cubic_metre;
C::stere               =                C::cubic_metre;
C::litre               =       1.0e-3 * C::cubic_metre;
C::liter               =                C::litre;
C::cubic_inch          =      C::inch * C::square_inch;
C::cubic_foot          =      C::foot * C::square_foot;
C::cubic_yard          =      C::yard * C::square_yard;
C::cubic_mile          =      C::mile * C::square_mile;
C::gallon              =     277.4193 * C::cubic_inch;
C::quart               =  (1.0 / 4.0) * C::gallon;
C::pint                =  (1.0 / 2.0) * C::quart;
C::gill                =  (1.0 / 4.0) * C::pint;
C::fluid_ounce         =  (1.0 / 5.0) * C::gill;
C::drachm              =  (1.0 / 8.0) * C::fluid_ounce;
C::scruple             =  (1.0 / 3.0) * C::drachm;
C::minim               = (1.0 / 20.0) * C::scruple;
C::USgallon            =        231.0 * C::cubic_inch;
C::USquart             =  (1.0 / 4.0) * C::USgallon;
C::USpint              =  (1.0 / 2.0) * C::USquart;
C::USgill              =  (1.0 / 4.0) * C::USpint;
C::USfluid_ounce       =  (1.0 / 4.0) * C::USgill;
C::USdram              =  (1.0 / 8.0) * C::USfluid_ounce;
C::USminim             = (1.0 / 60.0) * C::USdram;

// Speed [L/T]:
C::knot                = C::nautical_mile / C::hour;

// Acceleration (speed / time) [L/(T*T)]:
C::g                   = 9.80665 * C::metre / C::second / C::second;

// Mass [M]:
C::gram                = 0.001;
C::tonne               =       1000.0 * C::kilo*C::gram;
C::carat               =  (1.0 / 5.0) * C::gram;
C::pound               =   0.45359237 * C::kilo*C::gram;
C::ounce               = (1.0 / 16.0) * C::pound;
C::stone               =         14.0 * C::pound;
C::quarter             =          2.0 * C::stone;
C::hundredweight       =          4.0 * C::quarter;
C::ton                 =         20.0 * C::hundredweight;
C::cental              =        100.0 * C::pound;
C::shortquarter        =         25.0 * C::pound;
C::shortcwt            =          4.0 * C::shortquarter;
C::shortton            =         20.0 * C::shortcwt;

// Force (mass * acceleration) [M*L/(T*T)]:
C::Newton              = C::kilo*C::gram * C::metre /
                                    C::second / C::second;
C::dyne                = 1.0e-5 * C::Newton;

// Pressure (force / area) [M/(L*T*T)]:
C::Pascal              =  C::Newton / C::square_metre;
C::atmosphere          =       1.01325e+5 * C::Pascal;
C::bar                 =           1.0e+5 * C::Pascal;
C::torr                =    (1.0 / 760.0) * C::atmosphere;
C::mmHg                = (13.5951 * C::g) * C::Pascal;

// Energy (force * length) [M*L*L/(T*T)]:
C::Joule               =  C::Newton * C::metre;
C::kWh                 =     3.6e+6 * C::Joule;
C::erg                 =     1.0e-7 * C::Joule;
C::calorie             =      4.184 * C::Joule;
C::calorie_IT          =     4.1868 * C::Joule;
C::Btu                 = 1.05435e+3 * C::Joule;
C::eV                  =      C::qe * C::Joule;

// Temperature difference (energy) [M*L*L/(T*T)]:
C::Kelvin              = C::Boltzmann * C::Joule;
C::Celsius             =                C::Kelvin;
C::Centigrade          =                C::Celsius;
C::Fahrenheit          =  (5.0 / 9.0) * C::Kelvin;
C::Rankine             =                C::Fahrenheit;

// Temperature at 0 on each temperature scale:
C::Kelvin_0            =    0.0 * C::Kelvin;
C::Celsius_0           = 273.15 * C::Kelvin;
C::Centigrade_0        =          C::Celsius_0;
C::Fahrenheit_0        = 459.67 * C::Fahrenheit;
C::Rankine_0           =    0.0 * C::Fahrenheit;

// Power (energy / time) [M*L*L/(T*T*T)]:
C::Watt                = C::Joule / C::second;
C::horsepower          = 745.7 * C::Watt;

// Flux density (power / area / frequency) [M/(T*T)]:
C::Jansky              = 1.0e-26 * C::Watt / C::square_metre / C::Hertz;
C::fu                  = C::Jansky;

// Electric charge [Q] (Coulomb):
C::Coulomb             = 1.0;
C::abCoulomb           =         10.0 * C::Coulomb;
C::statCoulomb         = (0.1 / C::c) * C::Coulomb;

// Electric current (electric charge / time) [Q/T]:
C::Ampere              = C::Coulomb / C::second;
C::abAmpere            =         10.0 * C::Ampere;
C::statAmpere          = (0.1 / C::c) * C::Ampere;

// Electric field strength (force / electric charge) [M*L/(T*T*Q)]:

// Electric potential (energy / electric charge) [M*L*L/(T*T*Q)]:
C::Volt                = C::Joule / C::Coulomb;
C::abVolt              =          1.0e-8 * C::Volt;
C::statVolt            = (C::c * 1.0e-6) * C::Volt;

// Electric resistance (electric potential / current) [M*L*L/(T*Q*Q)]]:
C::Ohm                 = C::Volt / C::Ampere;
C::abOhm               =          1.0e-9 * C::Ohm;
C::statOhm             = (3.0e+3 * C::c) * C::Ohm;

// Electric conductance (current / electric potential) [T*Q*Q/(M*L*L)]:
C::Siemens             = C::Ampere / C::Volt;
C::mho                 = C::Siemens;

// Electric capacitance (charge / electric potential) [T*T*Q*Q/(M*L*L)]:
C::Farad               = C::Coulomb / C::Volt;
C::abFarad             =                1.0e+9 * C::Farad;
C::statFarad           = 1.0 / (3.0e+3 * C::c) * C::Farad;

// Electric inductance (electric potential * time / current) [M*L*L/(Q*Q)]:
C::Henry               = C::Volt * C::second / C::Ampere;
C::abHenry             =          1.0e-9 * C::Henry;
C::statHenry           = (3.0e+3 * C::c) * C::Henry;

// Magnetic induction (force / electric charge / velocity) [M/(T*Q)]:
C::Tesla               = C::Newton / C::Coulomb / (C::metre / C::second);
C::Gauss               = 1.0e-4 * C::Tesla;

// Magnetic flux (magnetic induction * area) [M*L*L/(T*Q)]:
C::Weber               = C::Tesla / C::square_metre;
C::Maxwell             = 1.0e-8 * C::Weber;
C::line                =          C::Maxwell;

// Magnetomotance = magnetomotive force (electric current) [Q/T]:
C::Ampere_turn         = C::Ampere;
C::abAmpere_turn       = 10.0 * C::Ampere_turn;
C::Gilbert             = 10.0 / (4.0 * C::pi) * C::Ampere_turn;
C::praGilbert          =        (4.0 * C::pi) * C::Ampere_turn;

// Magnetic field intensity (electric current / length) [Q/(T*L)]:
C::Oersted             = 1000.0 / (4.0 * C::pi) * C::Ampere / C::metre;
C::praOersted          =          (4.0 * C::pi) * C::Ampere / C::metre;

// Radioactivity (Bequerel):
C::Bequerel            = 1.0;

// Luminous intensity [Iv]:
C::candela             = 1.0;

// Amount of substance [N]:
C::mole                = 1.0;
C::molecule            = (1.0 / C::Avogadro) * C::mole;

   }
}

C_init::~C_init()
{
  if (--count == 0) {
  }
}
