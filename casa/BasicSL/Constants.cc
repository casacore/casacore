//# Constants.cc: Mathematical and physical constants
//# Copyright (C) 1993,1994,1995,1997,1998,1999,2002
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

#include <casacore/casa/BasicSL/Constants.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Been changed for SUN CC port

//----------------------------------------------------------------------------
// Mathematical constants
//----------------------------------------------------------------------------
// Reference:
// "Handbook of Mathematical Functions",
// Abramowitz & Stegun, 1964.
// Dover Publications Inc., New York (9th printing, 1972)
//----------------------------------------------------------------------------

// Irrationals:
const Double C::sqrt2 		= 1.4142135623730950488;
const Double C::sqrt3 		= 1.7320508075688772935;
const Double C::_1_sqrt2	= 0.70710678118654752440;
const Double C::_1_sqrt3 	= 0.57735026918962576451;

// Pi and functions thereof:
const Double C::pi		= 3.141592653589793238462643;
const Double C::_2pi		= 6.283185307179586476925286;
const Double C::pi_2		= 1.570796326794896619231322;
const Double C::pi_4		= 0.7853981633974483096156608;
const Double C::_1_pi		= 0.3183098861837906715377675;
const Double C::_2_pi		= 0.6366197723675813430755350;
const Double C::_1_sqrtpi	= 0.5641895835477562869480795;
const Double C::_2_sqrtpi	= 1.1283791670955125738961590;

// e and functions thereof:
const Double C::e		= 2.718281828459045235360287;
const Double C::ln2		= 0.6931471805599453094172321;  
const Double C::ln10		= 2.3025850929940456840179915; 
const Double C::log2e		= 1.4426950408889634074; 
const Double C::log10e		= 0.4342944819032518276511289;

// gamma and functions thereof:
const Double C::gamma 		=  0.577215664901532860606512;
const Double C::lngamma 	= -0.549539312981644822337662;
const Double C::etogamma	=  1.7810724179901979852;


//----------------------------------------------------------------------------
// Machine constants
//----------------------------------------------------------------------------
const Double C::flt_min	      	= FLT_MIN;
const Double C::minfloat	= FLT_MIN;
const Double C::dbl_min 	= DBL_MIN;
const Double C::mindouble	= DBL_MIN;
const Double C:: flt_max	= FLT_MAX;
const Double C:: dbl_max	= DBL_MAX;
const Double C::flt_epsilon	= FLT_EPSILON;
const Double C::dbl_epsilon	= DBL_EPSILON;

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
const Double C::c		= 2.99792458e+08;

//----------------------------------------------------------------------------
// Numerical conversion factors
//----------------------------------------------------------------------------

// Metric prefixes (unity):
const Double C::yotta 		= 1.0e+24;
const Double C::zetta 		= 1.0e+21;
const Double C::exa 		= 1.0e+18;
const Double C::peta 		= 1.0e+15;
const Double C::tera 		= 1.0e+12;
const Double C::giga 		= 1.0e+09;
const Double C::mega 		= 1.0e+06;
const Double C::kilo 		= 1.0e+03;
const Double C::hecto 		= 1.0e+02;
const Double C::deka 		= 1.0e+01;
const Double C::deci 		= 1.0e-01;
const Double C::centi 		= 1.0e-02;
const Double C::milli 		= 1.0e-03;
const Double C::micro 		= 1.0e-06;
const Double C::nano 		= 1.0e-09;
const Double C::pico 		= 1.0e-12;
const Double C::femto 		= 1.0e-15;
const Double C::atto 		= 1.0e-18;
const Double C::zepto 		= 1.0e-21;
const Double C::yocto 		= 1.0e-24;

// Angular measure:
const Double C::radian         	= 1.0;
const Double C::circle		= 6.2831853071795864769252867; 
const Double C::degree		= 0.0174532925199432957692369;
const Double C::arcmin		= 0.000290888208665721596153948459;
const Double C::arcsec		= 0.00000484813681109535993589914098765;

// Solid angular measure:
const Double C::steradian	=  1.0;
const Double C::sphere		= 12.56637061435917295385057344;
const Double C::square_degree	=  0.30461741978670859934674354486e-3;
const Double C::square_arcmin	=  0.8461594994075238870742876246233e-7;
const Double C::square_arcsec	=  0.235044305390978857520635451284e-10;


//----------------------------------------------------------------------------
// Physical conversion factors
//----------------------------------------------------------------------------

// Time interval [T]:
const Double C::second		= 1.0;
const Double C::minute		= 60.0;
const Double C::hour		= 3600.0;
const Double C::day		= 86400.0;
const Double C::MJD0 = 2400000.5;


} //# NAMESPACE CASACORE - END

