//# tConstants.cc: This program tests the Casacore C (constants) class
//# Copyright (C) 1993,1994,1995,2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

//# Includes

#include <casacore/casa/iostream.h>
#include <casacore/casa/BasicSL/Constants.h>

#include <casacore/casa/namespace.h>

int main()
{
   cout << ">>> List of machine constants."                 << endl
        << endl
        << "Machine constants from limits.h:"               << endl
        << "CHAR_BIT..............  " << CHAR_BIT           << endl
        << "CHAR_MIN..............  " << CHAR_MIN           << endl
        << "CHAR_MAX..............  " << CHAR_MAX           << endl
        << "SCHAR_MIN.............  " << SCHAR_MIN          << endl
        << "SCHAR_MAX.............  " << SCHAR_MAX          << endl
        << "UCHAR_MAX.............  " << UCHAR_MAX          << endl
        << "MB_LEN_MAX............  " << MB_LEN_MAX         << endl
        << "SHRT_MIN..............  " << SHRT_MIN           << endl
        << "SHRT_MAX..............  " << SHRT_MAX           << endl
        << "USHRT_MAX.............  " << USHRT_MAX          << endl
        << "INT_MIN...............  " << INT_MIN            << endl
        << "INT_MAX...............  " << INT_MAX            << endl
        << "UINT_MAX..............  " << UINT_MAX           << endl
        << "LONG_MIN..............  " << LONG_MIN           << endl
        << "LONG_MAX..............  " << LONG_MAX           << endl
        << "ULONG_MAX.............  " << ULONG_MAX          << endl;

   cout << endl
        << "Machine constants from float.h:"                << endl
        << "FLT_RADIX.............  " << FLT_RADIX          << endl
        << "FLT_ROUNDS............  " << FLT_ROUNDS         << endl
        << "FLT_MIN_EXP...........  " << FLT_MIN_EXP        << endl
        << "DBL_MIN_EXP...........  " << DBL_MIN_EXP        << endl
        << "LDBL_MIN_EXP..........  " << LDBL_MIN_EXP       << endl
        << "FLT_MAX_EXP...........  " << FLT_MAX_EXP        << endl
        << "DBL_MAX_EXP...........  " << DBL_MAX_EXP        << endl
        << "LDBL_MAX_EXP..........  " << LDBL_MAX_EXP       << endl
        << "FLT_MIN_10_EXP........  " << FLT_MIN_10_EXP     << endl
        << "DBL_MIN_10_EXP........  " << DBL_MIN_10_EXP     << endl
        << "LDBL_MIN_10_EXP.......  " << LDBL_MIN_10_EXP    << endl
        << "FLT_MAX_10_EXP........  " << FLT_MAX_10_EXP     << endl
        << "DBL_MAX_10_EXP........  " << DBL_MAX_10_EXP     << endl
        << "LDBL_MAX_10_EXP.......  " << LDBL_MAX_10_EXP    << endl
        << "FLT_MANT_DIG..........  " << FLT_MANT_DIG       << endl
        << "DBL_MANT_DIG..........  " << DBL_MANT_DIG       << endl
        << "LDBL_MANT_DIG.........  " << LDBL_MANT_DIG      << endl
        << "FLT_DIG...............  " << FLT_DIG            << endl
        << "DBL_DIG...............  " << DBL_DIG            << endl
        << "LDBL_DIG..............  " << LDBL_DIG           << endl
        << "FLT_EPSILON...........  " << FLT_EPSILON        << endl
        << "DBL_EPSILON...........  " << DBL_EPSILON        << endl
        << "LDBL_EPSILON..........  ";
                            if (LDBL_MAX_EXP == DBL_MAX_EXP) {
                                 cout << LDBL_EPSILON       << endl;
                            } else {
                                 cout << "(unprintable)"    << endl;
                            }
   cout << "FLT_MIN...............  " << FLT_MIN            << endl
        << "DBL_MIN...............  " << DBL_MIN            << endl
        << "LDBL_MIN..............  ";
                            if (LDBL_MAX_EXP == DBL_MAX_EXP) {
                                 cout << LDBL_MIN           << endl;
                            } else {
                                 cout << "(unprintable)"    << endl;
                            }
   cout << "FLT_MAX...............  " << FLT_MAX            << endl
        << "DBL_MAX...............  " << DBL_MAX            << endl
        << "LDBL_MAX..............  ";
                            if (LDBL_MAX_EXP == DBL_MAX_EXP) {
                                 cout << LDBL_MAX           << endl;
                            } else {
                                 cout << "(unprintable)"    << endl;
                            }
#if !defined(AIPS_DARWIN) && !defined(AIPS_BSD)
   cout << endl
        << "Machine constants from values.h:"               << endl
        << "HIBITS................  " << HIBITS             << endl
        << "HIBITL................  " << HIBITL             << endl
        << "MAXSHORT..............  " << MAXSHORT           << endl
        << "MAXLONG...............  " << MAXLONG            << endl
        << "MAXINT................  " << MAXINT             << endl
        << "MINFLOAT..............  " << MINFLOAT           << endl
        << "MINDOUBLE.............  " << MINDOUBLE          << endl
        << "MAXFLOAT..............  " << MAXFLOAT           << endl
        << "MAXDOUBLE.............  " << MAXDOUBLE          << endl;
#endif
   cout << endl
        << "<<< End of machine constants." << endl;

   cout << endl
        << "Irrationals:"                                   << endl
        << "C::sqrt2..............  " << C::sqrt2           << endl
        << "C::sqrt3..............  " << C::sqrt3           << endl
        << "C::_1_sqrt2...........  " << C::_1_sqrt2        << endl
        << "C::_1_sqrt3...........  " << C::_1_sqrt3        << endl;

   cout << endl
        << "Pi and functions thereof:"                      << endl
        << "C::pi.................  " << C::pi              << endl
        << "C::_2pi...............  " << C::_2pi            << endl
        << "C::pi_2...............  " << C::pi_2            << endl
        << "C::pi_4...............  " << C::pi_4            << endl
        << "C::_1_pi..............  " << C::_1_pi           << endl
        << "C::_2_pi..............  " << C::_2_pi           << endl
        << "C::_1_sqrtpi..........  " << C::_1_sqrtpi       << endl
        << "C::_2_sqrtpi..........  " << C::_2_sqrtpi       << endl;

   cout << endl
        << "e and functions thereof:"                       << endl
        << "C::e..................  " << C::e               << endl
        << "C::ln2................  " << C::ln2             << endl
        << "C::ln10...............  " << C::ln10            << endl
        << "C::log2e..............  " << C::log2e           << endl
        << "C::log10e.............  " << C::log10e          << endl;

   cout << endl
        << "gamma and functions thereof:"                   << endl
        << "C::gamma..............  " << C::gamma           << endl
        << "C::lngamma............  " << C::lngamma         << endl
        << "C::etogamma...........  " << C::etogamma        << endl;

   cout << endl
        << "Fundamental physical constants (SI units):"     << endl
        << "C::c..................  " << C::c               << endl;

   cout << endl
        << "Metric prefixes:"                               << endl
        << "C::yotta..............  " << C::yotta           << endl
        << "C::zetta..............  " << C::zetta           << endl
        << "C::exa................  " << C::exa             << endl
        << "C::peta...............  " << C::peta            << endl
        << "C::tera...............  " << C::tera            << endl
        << "C::giga...............  " << C::giga            << endl
        << "C::mega...............  " << C::mega            << endl
        << "C::kilo...............  " << C::kilo            << endl
        << "C::hecto..............  " << C::hecto           << endl
        << "C::deka...............  " << C::deka            << endl
        << "C::deci...............  " << C::deci            << endl
        << "C::centi..............  " << C::centi           << endl
        << "C::milli..............  " << C::milli           << endl
        << "C::micro..............  " << C::micro           << endl
        << "C::nano...............  " << C::nano            << endl
        << "C::pico...............  " << C::pico            << endl
        << "C::femto..............  " << C::femto           << endl
        << "C::atto...............  " << C::atto            << endl
        << "C::zepto..............  " << C::zepto           << endl
        << "C::yocto..............  " << C::yocto           << endl;

   cout << endl
        << "Angular measure:"                               << endl
        << "C::radian.............  " << C::radian          << endl
	<< "C::circle.............  " << C::circle	    << endl
        << "C::degree.............  " << C::degree          << endl
        << "C::arcmin.............  " << C::arcmin          << endl
        << "C::arcsec.............  " << C::arcsec          << endl;

   cout << endl
        << "Solid angular measure:"                         << endl
        << "C::steradian..........  " << C::steradian       << endl
        << "C::sphere.............  " << C::sphere          << endl
        << "C::square_degree......  " << C::square_degree   << endl
        << "C::square_arcmin......  " << C::square_arcmin   << endl
        << "C::square_arcsec......  " << C::square_arcsec   << endl;

   cout << endl
        << "Time interval:"                                 << endl
        << "C::second.............  " << C::second          << endl
        << "C::minute.............  " << C::minute          << endl
        << "C::hour...............  " << C::hour            << endl
        << "C::day................  " << C::day             << endl;

   return 0;
}
