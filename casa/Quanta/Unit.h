//# Unit.h: defines the Unit class
//# Copyright (C) 1994-1996,1998-2000,2008
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

#ifndef CASA_UNIT_H
#define CASA_UNIT_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Quanta/UnitVal.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

// <summary>
// defines physical units
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tUnit">
// </reviewed>
//
//# // <prerequisite>
//# // </prerequisite>
//
//# // <etymology>
//# // </etymology>
//
// <synopsis> 
// Physical units are basically used as quantities (see the
// <linkto class=Quantum>Quantum</linkto> class), i.e.
// a value and a dimension. The Unit class, or one of its subsidaries,  will
// in general not be called separately. The only reason to make use of these
// classes is to generate additional 'tagged' units, i.e. units with a
// special name, e.g. 'beam' for a telescope  beam, or 'JY', a non-SI name
// for Jy.
//  <h3> Units </h3>
// A Unit is a String, and can be defined as either a Unit or a String
// everywhere where a Unit is required.<br>
// If defined as a Unit, the format of the string will be checked for a
// legal definition and its value will be stored. If defined as a String,
// the checking and determination of the value will be done each time
// the string is encountered when a Unit is expected.<br>
// <note role=tip> The use of a separate Unit variable will give a tremendous
// speed increase, if compared to using the String representation in
// e.g. <linkto class=Quantum>Quantity(5,"deg")</linkto> </note>
// <note role=caution>
// If using an explicit Unit variable (e.g. <src>Unit a("5Bolton/beam")</src>),
// the check on the legality of the given string, and the conversion to the 
// cached canonical value in the variable 'a', is only done at creation time. This
// means that if the user changes the value of a unit involved by the 
// <linkto class=UnitMap>putUser()</linkto> method, the unit using it should be
// re-created (<src> a = Unit("5Bolton/beam");</src>).
// </note>
// A unit is a string of one or more fields separated 
// by 'space' or '.' or '*' (FITS option)
// (to indicate multiply) or '/' (to indicate divide).
// Multiple separators are acted upon (i.e. m//s == m.s).
// Separators are acted upon left-to-right (i.e. m/s/A == (m/s)/A; use
// () to indicate otherwise (e.g. m/(s/A))).
//
// A field is a name, or a unit enclosed in (), optionally followed by an,
// optionally signed, decimal constant.
// The decimal constant may be proceeded by '**' or '^' (FITS option)
//
// E.g. m.(m/s)-2 == m-1.s2)
// <note role=tip>
// A 'space' or '.' before an opening '(' can be omitted.
// </note>
// A name can consist of case-sensitive letters, '_', ''', ':', '"' and '0'
// ('0' not as first character). Digits 1-9 are allowed if preceded with
// an '_'.
//
// Possible legal names are e.g. Jy, R0, R_1, "_2.
// <note role=tip>
// <ul>
//   <li> ' is used for arcmin
//   <li> '' or " for arcsec
//   <li> : :: and ::: are used for h, min, s respectively
//   <li> _ is used for an undimensioned value (like beam or pixel)
// </ul>
// </note>
// <note role=caution> The standard naming conventions for SI units are that they are
// all in lowercase, unless derived from a person's name, when they start
// with a capital letter. Notable exceptions are some of the astronomical
// SI related units (e.g. AU).
// </note>
// A name can be preceded by a (standard) decimal prefix.
//
// A name must be defined in a Unit map before it can be used.
//
// All SI units and some customary units are part of the classes. User
// defined names can be added by the UnitMap::putUser() function (see
// the <linkto class=UnitMap>UnitMap</linkto> class).
//
// Example:
//	km/s/(Mpc.s)2  is identical to km.s-1.Mpc-2.s-2
//
// There are 5 name lists in the UnitMap, which are searched in reverse order:
// <ol>
//   <li> Defining units: 	m, kg, s, A, K, cd, mol, rad, sr, _
//   <li> SI units:		including a.o. g, Jy, AU
//   <li> Customary units:	e.g. lb, hp, ly
//   <li> User defined units:	defined by user (e.g. beam, KPH, KM)
//   <li> Cached units:	for speed in operations
// </ol>
// All known names can be viewed by running the tUnit test program, or
// using the MapUnit::list() routine.
// They are also (at least the 1999/09/15 values) available in the
// <linkto module="Quanta">Quanta module documentation</linkto>.
// <note role=caution>
// There is a difference between units without a dimension (non-dimensioned
// I will call them), and undimensioned units. Non-dimensioned examples are
// "", "%"; undimensioned examples: "beam", "pixel".
// </note>
//
//  <h3> Unit class </h3>
// The Unit class is not directly based on the String class, but Strings and 
// Units are interchangeable in all Unit and Quantum related calls.
// (But notice the earlier note on speed if using explicit Strings often.)
//
// To calculate with Units (or Strings representing units), use the
// <linkto class=UnitVal>UnitVal</linkto> class. To use dimensioned values,
// use the <linkto class=Quantum>Quantum</linkto> (cq Quantity) class.
//
// Using Unit i.s.o. String will give an immediate check of the legality
// of the unit string.
// In addition the UnitVal class contains a check facility to determine the
// legality of a unit string:
// <srcblock>
// Bool UnitVal::check("string");
// </srcblock>
// 
// </synopsis> 
//
// <example>
// <srcblock>
// #include <casacore/casa/Quanta.h>
// // check if a string is a valid unit
// if ( !UnitVal::check("Km") ) { cout << "Invalid unit string " << "Km" << endl;
// // define some units
// String unit1="km/Mpc";
// Unit unit2="uJy/Mpc";
// // define your own unit name
// UnitMap::putUser("my_univ", UnitVal( C::pi, unit2), "My universe param");
// // use the units in model calculations
// Quantity observed( 8.97, "Mmy_univ/a");
// Quantity theory (3.8e-9, "mmy_univ/s");
// if ( ( observed / theory) < 1.) { cout << "Eureka" << endl;
// </srcblock>
// </example>
//
// <motivation>
// Make basis for all dimensioned values the SI system of units
// </motivation>
//
// <todo asof="941110">
//   <li> Some inlining (did not work first go)
//   <li> Look into possiblity of conversion routine from rad2 to sr
// </todo>

class Unit {
    public:
//# Constructors
// Default empty string constructor
    Unit();
// Copy constructor
    Unit(const Unit &other);
// String based constructors.
// <thrown>
//   <li> AipsError if illegal unit string
// </thrown>
// <group name="constructor">
    Unit(const std::string &other);
    Unit(const Char *other);
    explicit Unit(Char other);
    Unit(const Char *other, Int len);
// </group>
// Destructor
    ~Unit();

//* Operators
// Copy assignment
    Unit& operator=(const Unit &other);
// Comparisons. Comparisons are done on the basis of the inherent units. I.e.
// <src>m/s</src> are identical to <src>AU/cy</src>.
// <group>
    Bool operator==(const Unit &other) const;
    Bool operator!=(const Unit &other) const;
// Fast check for "" units
    Bool empty() const;
// </group>
//# Member functions
// Get the unit value
    const UnitVal &getValue() const;
// Get the unit name
    const String &getName() const;
// Set the unit value
    void setValue(const UnitVal &in);
// Set the unit name
    void setName(const String &in);

private:
//# Data
    String uName;
    UnitVal uVal;

//# Member functions
// Check format of unit string
// <thrown>
//   <li> AipsError
// </thrown>
    void check();
};

//# Inline Implementations


} //# NAMESPACE CASACORE - END

#endif
