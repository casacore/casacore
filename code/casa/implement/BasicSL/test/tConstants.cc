//# tConstants.cc: This program tests the AIPS++ C (constants) class
//# Copyright (C) 1993,1994,1995,1999
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

#include <iostream.h>
#include <aips/Mathematics/Constants.h>

main()
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
        << "C::c..................  " << C::c               << endl
        << "C::Gravity............  " << C::Gravity         << endl
        << "C::Planck.............  " << C::Planck          << endl
        << "C::GasConst...........  " << C::GasConst        << endl
        << "C::Avogadro...........  " << C::Avogadro        << endl
        << "C::qe.................  " << C::qe              << endl
        << "C::mp.................  " << C::mp              << endl
        << "C::mp_me..............  " << C::mp_me           << endl;

   cout << endl
        << "Derived physical constants (SI units):"         << endl
        << "C::mu0................  " << C::mu0             << endl
        << "C::epsilon0...........  " << C::epsilon0        << endl
        << "C::Planck_2pi.........  " << C::Planck_2pi      << endl
        << "C::u..................  " << C::u               << endl
        << "C::Boltzmann..........  " << C::Boltzmann       << endl
        << "C::Faraday............  " << C::Faraday         << endl
        << "C::me.................  " << C::me              << endl
        << "C::re.................  " << C::re              << endl
        << "C::a0.................  " << C::a0              << endl;

   cout << endl
        << "Physical quantities (SI units):"                << endl
        << "C::R0.................  " << C::R0              << endl;

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
        << "C::circle.............  " << C::circle          << endl
        << "C::circuit............  " << C::circuit         << endl
        << "C::cycle..............  " << C::cycle           << endl
        << "C::rev................  " << C::rev             << endl
        << "C::revolution.........  " << C::revolution      << endl
        << "C::rotation...........  " << C::rotation        << endl
        << "C::degree.............  " << C::degree          << endl
        << "C::arcmin.............  " << C::arcmin          << endl
        << "C::arcsec.............  " << C::arcsec          << endl 
        << "C::grad...............  " << C::grad            << endl;

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
        << "C::day................  " << C::day             << endl
        << "C::week...............  " << C::week            << endl
        << "C::fortnight..........  " << C::fortnight       << endl;

   cout << endl
        << "Frequency:"                                     << endl
        << "C::Hertz..............  " << C::Hertz           << endl;

   cout << endl
        << "Length:"                                        << endl
        << "C::metre..............  " << C::metre           << endl
        << "C::meter..............  " << C::meter           << endl
        << "C::Fermi..............  " << C::Fermi           << endl
        << "C::Angstrom...........  " << C::Angstrom        << endl
        << "C::inch...............  " << C::inch            << endl
        << "C::thou...............  " << C::thou            << endl
        << "C::hand...............  " << C::hand            << endl
        << "C::span...............  " << C::span            << endl
        << "C::foot...............  " << C::foot            << endl
        << "C::yard...............  " << C::yard            << endl
        << "C::fathom.............  " << C::fathom          << endl
        << "C::rod................  " << C::rod             << endl
        << "C::perch..............  " << C::perch           << endl
        << "C::rope...............  " << C::rope            << endl
        << "C::chain..............  " << C::chain           << endl
        << "C::furlong............  " << C::furlong         << endl
        << "C::mile...............  " << C::mile            << endl
        << "C::nautical_mile......  " << C::nautical_mile   << endl
        << "C::point..............  " << C::point           << endl
        << "C::pica...............  " << C::pica            << endl
        << "C::astronomical_unit..  " << C::astronomical_unit << endl
        << "C::light_second.......  " << C::light_second    << endl
        << "C::light_year.........  " << C::light_year      << endl
        << "C::parsec.............  " << C::parsec          << endl;

   cout << endl
        << "Area:"                                          << endl
        << "C::square_metre.......  " << C::square_metre    << endl
        << "C::square_meter.......  " << C::square_meter    << endl
        << "C::are................  " << C::are             << endl
        << "C::barn...............  " << C::barn            << endl
        << "C::square_inch........  " << C::square_inch     << endl
        << "C::square_foot........  " << C::square_foot     << endl
        << "C::square_yard........  " << C::square_yard     << endl
        << "C::square_mile........  " << C::square_mile     << endl
        << "C::square_perch.......  " << C::square_perch    << endl
        << "C::rood...............  " << C::rood            << endl
        << "C::acre...............  " << C::acre            << endl
        << "C::square.............  " << C::square          << endl;

   cout << endl
        << "Volume:"                                        << endl
        << "C::cubic_metre........  " << C::cubic_metre     << endl
        << "C::cubic_meter........  " << C::cubic_meter     << endl
        << "C::stere..............  " << C::stere           << endl
        << "C::litre..............  " << C::litre           << endl
        << "C::liter..............  " << C::liter           << endl
        << "C::cubic_inch.........  " << C::cubic_inch      << endl
        << "C::cubic_foot.........  " << C::cubic_foot      << endl
        << "C::cubic_yard.........  " << C::cubic_yard      << endl
        << "C::cubic_mile.........  " << C::cubic_mile      << endl
        << "C::gallon.............  " << C::gallon          << endl
        << "C::quart..............  " << C::quart           << endl
        << "C::pint...............  " << C::pint            << endl
        << "C::gill...............  " << C::gill            << endl
        << "C::fluid_ounce........  " << C::fluid_ounce     << endl
        << "C::drachm.............  " << C::drachm          << endl
        << "C::scruple............  " << C::scruple         << endl
        << "C::minim..............  " << C::minim           << endl
        << "C::USgallon...........  " << C::USgallon        << endl
        << "C::USquart............  " << C::USquart         << endl
        << "C::USpint.............  " << C::USpint          << endl
        << "C::USgill.............  " << C::USgill          << endl
        << "C::USfluid_ounce......  " << C::USfluid_ounce   << endl
        << "C::USdram.............  " << C::USdram          << endl
        << "C::USminim............  " << C::USminim         << endl;

   cout << endl
        << "Speed:"                                         << endl
        << "C::knot...............  " << C::knot            << endl;

   cout << endl
        << "Acceleration:"                                  << endl
        << "C::g..................  " << C::g               << endl;

   cout << endl
        << "Mass:"                                          << endl
        << "C::gram...............  " << C::gram            << endl
        << "C::tonne..............  " << C::tonne           << endl
        << "C::carat..............  " << C::carat           << endl
        << "C::pound..............  " << C::pound           << endl
        << "C::ounce..............  " << C::ounce           << endl
        << "C::stone..............  " << C::stone           << endl
        << "C::quarter............  " << C::quarter         << endl
        << "C::hundredweight......  " << C::hundredweight   << endl
        << "C::ton................  " << C::ton             << endl
        << "C::cental.............  " << C::cental          << endl
        << "C::shortquarter.......  " << C::shortquarter    << endl
        << "C::shortcwt...........  " << C::shortcwt        << endl
        << "C::shortton...........  " << C::shortton        << endl;

   cout << endl
        << "Force:"                                         << endl
        << "C::Newton.............  " << C::Newton          << endl
        << "C::dyne...............  " << C::dyne            << endl;

   cout << endl
        << "Pressure:"                                      << endl
        << "C::Pascal.............  " << C::Pascal          << endl
        << "C::atmosphere.........  " << C::atmosphere      << endl
        << "C::bar................  " << C::bar             << endl
        << "C::torr...............  " << C::torr            << endl
        << "C::mmHg...............  " << C::mmHg            << endl;

   cout << endl
        << "Energy:"                                        << endl
        << "C::Joule..............  " << C::Joule           << endl
        << "C::kWh................  " << C::kWh             << endl
        << "C::erg................  " << C::erg             << endl
        << "C::calorie............  " << C::calorie         << endl
        << "C::calorie_IT.........  " << C::calorie_IT      << endl
        << "C::Btu................  " << C::Btu             << endl
        << "C::eV.................  " << C::eV              << endl;

   cout << endl
        << "Temperature difference:"                        << endl
        << "C::Kelvin.............  " << C::Kelvin          << endl
        << "C::Celsius............  " << C::Celsius         << endl
        << "C::Centigrade.........  " << C::Centigrade      << endl
        << "C::Fahrenheit.........  " << C::Fahrenheit      << endl
        << "C::Rankine............  " << C::Rankine         << endl;

   cout << endl
        << "Temperature at 0 on each temperature scale:"    << endl
        << "C::Kelvin_0...........  " << C::Kelvin_0        << endl
        << "C::Celsius_0..........  " << C::Celsius_0       << endl
        << "C::Centigrade_0.......  " << C::Centigrade_0    << endl
        << "C::Fahrenheit_0.......  " << C::Fahrenheit_0    << endl
        << "C::Rankine_0..........  " << C::Rankine_0       << endl;

   cout << endl
        << "Power:"                                         << endl
        << "C::Watt...............  " << C::Watt            << endl
        << "C::horsepower.........  " << C::horsepower      << endl;

   cout << endl
        << "Flux density:"                                  << endl
        << "C::Jansky.............  " << C::Jansky          << endl
        << "C::fu.................  " << C::fu              << endl;

   cout << endl
        << "Electric charge:"                               << endl
        << "C::Coulomb............  " << C::Coulomb         << endl
        << "C::abCoulomb..........  " << C::abCoulomb       << endl
        << "C::statCoulomb........  " << C::statCoulomb     << endl;

   cout << endl
        << "Electric current:"                              << endl
        << "C::Ampere.............  " << C::Ampere          << endl
        << "C::abAmpere...........  " << C::abAmpere        << endl
        << "C::statAmpere.........  " << C::statAmpere      << endl;

   cout << endl
        << "Electric potential:"                            << endl
        << "C::Volt...............  " << C::Volt            << endl
        << "C::abVolt.............  " << C::abVolt          << endl
        << "C::statVolt...........  " << C::statVolt        << endl;

   cout << endl
        << "Electric resistance:"                           << endl
        << "C::Ohm................  " << C::Ohm             << endl
        << "C::abOhm..............  " << C::abOhm           << endl
        << "C::statOhm............  " << C::statOhm         << endl;

   cout << endl
        << "Electric conductance:"                          << endl
        << "C::Siemens............  " << C::Siemens         << endl
        << "C::mho................  " << C::mho             << endl;

   cout << endl
        << "Electric capacitance:"                          << endl
        << "C::Farad..............  " << C::Farad           << endl
        << "C::abFarad............  " << C::abFarad         << endl
        << "C::statFarad..........  " << C::statFarad       << endl;

   cout << endl
        << "Electric inductance:"                           << endl
        << "C::Henry..............  " << C::Henry           << endl
        << "C::abHenry............  " << C::abHenry         << endl
        << "C::statHenry..........  " << C::statHenry       << endl;

   cout << endl
        << "Magnetic induction:"                            << endl
        << "C::Tesla..............  " << C::Tesla           << endl
        << "C::Gauss..............  " << C::Gauss           << endl;

   cout << endl
        << "Magnetic flux:"                                 << endl
        << "C::Weber..............  " << C::Weber           << endl
        << "C::Maxwell............  " << C::Maxwell         << endl
        << "C::line...............  " << C::line            << endl;

   cout << endl
        << "Magnetomotance:"                                << endl
        << "C::Ampere_turn........  " << C::Ampere_turn     << endl
        << "C::abAmpere_turn......  " << C::abAmpere_turn   << endl
        << "C::Gilbert............  " << C::Gilbert         << endl
        << "C::praGilbert.........  " << C::praGilbert      << endl;

   cout << endl
        << "Magnetic field intensity:"                      << endl
        << "C::Oersted............  " << C::Oersted         << endl
        << "C::praOersted.........  " << C::praOersted      << endl;

   cout << endl
        << "Radioactivity:"                                 << endl
        << "C::Bequerel...........  " << C::Bequerel        << endl;

   cout << endl
        << "Luminous intensity:"                            << endl
        << "C::candela............  " << C::candela         << endl;

   cout << endl
        << "Amount of substance:"                           << endl
        << "C::mole...............  " << C::mole            << endl
        << "C::molecule...........  " << C::molecule        << endl;

   return 0;
}
