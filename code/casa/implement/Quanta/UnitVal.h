//# UnitVal.h: defines the class describing a unit as a value and a dimension
//# Copyright (C) 1994,1995,1996,1997,1998
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

#if !defined(AIPS_UNITVAL_H)
#define AIPS_UNITVAL_H

#if defined(_AIX)
#pragma implementation ("UnitVal.cc")
#endif 

//# Includes
#include <aips/aips.h>
#include <aips/Quanta/UnitDim.h>

//# Forward Declarations
class String;
class MUString;
class UnitMap;
#if defined(AIPS_STDLIB)
#include <iosfwd.h>
#else
class ostream;
#endif

// 
// <summary>
// describes any valid unit as a factor and a dimenion of SI units
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tUnit">
//
// <prerequisite>
// You should have at least a preliminary understanding of these classes:
//   <li> <linkto class=Unit>Unit</linkto>
// </prerequisite>
//
// <etymology>
// The class name derives from Units and gives a Value for a unit string
// </etymology>
//
// <synopsis> 
// Physical units are strings consisting of one or more names of known
// basic units, separated by '.' or ' ' (for multiplication) or '/' (for
// division). Each name can optionally be preceded by a standard decimal 
// prefix, and/or followed by an (optionally signed) exponent.
// Example:
//	km/s/(Mpc.s)2  is identical to km.s-1.Mpc-2.s-2
//
// See the <linkto class="Unit">Unit</linkto> class for more details.
//
// The UnitVal class maps a Unit string to a factor and a dimension of SI
// defining units. E.g 'km/s' will be 1000 m.s-1 .
// This class is only of interest if the manipulation of units is of
// direct interest. Normally units will be used as Quantities and Quantums
// (see the <linkto class=Quantum>Quantum</linkto> class) only,
// i.e. as a physical quantity having a value and unit.
// The class can also be used to check the validity of a unit string.
//
//  <h3> Constructing UnitVal values </h3>
//
// UnitVal has the following constructors:
// <ul>
//   <li> UnitVal()		creates an (undimensioned) value 1.
//   <li> UnitVal(Double f)	creates an (undimensioned) value f.
//   <li> UnitVal(Double f, String s) creates value f with unit s
//   <li> UnitVal(Double f, Int i) (private) creates value f with unit
//				at position i in dimension vector
// </ul>
// 
//
//  <h3> Manipulating unit values </h3>
//
// The UnitVal can be manipulated by the following operators and functions:
// <ul>
//   <li> *, /	generates combined UnitVal (e.g. 1 yd * 1 m = 0.9 m2)
//   <li> pow(Int)	UnitVal(2,"km")->pow(2) = 4000000 m2
//   <li> ==, !=	compares dimensions only: 1 yd == 5 ly: True
//   <li> getFac()	will return the factor (Double)
//   <li> getDim()	will return the dimensions (as UnitDim)
//   <li> <<		will output formatted unit (factor and dimension)
// </ul>
// To aid in checking the dimensionality of units, the following constants
// are available:
// <ul>
//   <li> UnitVal::NODIM
//   <li> UnitVal::LENGTH
//   <li> UnitVal::MASS
//   <li> UnitVal::ANGLE
//   <li> UnitVal::SOLIDANGLE
//   <li> UnitVal::MOLAR
//   <li> UnitVal::CURRENT
//   <li> UnitVal::TIME
//   <li> UnitVal::TEMPERATURE
//   <li> UnitVal::INTENSITY
// </ul>
// <note role=tip>
// Any other dimension can be checked by a combination. To check e.g. if
// a unit is an acceleration, use: UnitVal::LENGTH/UnitVal::TIME/UnitVal::TIME
// </note>
//
//  <h3> Checking for valid unit strings </h3>
//
// The validity of a unit string can be checked by:
// <srcblock>
// // Check if the given String is a valid unit representation. The String
// // will be cached in the unit maps for later reference if True
// if ( UnitVal::check( "km/s/Mpc") ) {...}
// </srcblock>
//
// </synopsis> 
//
// <example>
// An observation contains values in Janskys and in Westerbork Units. The
// data can be combined by the following code:
// <srcblock>
// // The Fits tape gave JY, we check if defined, else we define them
//   if ( !UnitVal::check( "JY")) {
//	UnitMap::putUser("JY", UnitVal(1.,"Jy"), "FITS way to write Jy");
//   }
// // The Fits tape gave WU (which are defined):
// // We check if JY and WU are of the same dimension:
//   if (UnitVal(1.,"JY") != UnitVal(1.,"WU")) {
//	cerr << "Wrong dimension for either JY ( " << 
//		UnitVal(1.,"JY")->getDim() <<
//		") or WU ( " <<
//		UnitVal(1.,"WU")->getDim() << ")" << endl;
//   }
// // And output the relation between WU and JY, and the WU value:
//   cout << "1 WU = " << ( UnitVal(1.,"WU")/UnitVal(1.,"Jy") )->getVal() <<
//	     " JY with 1 WU = " << UnitVal(1.,"WU") << endl;
// </srcblock>
// </example>

// <motivation>
// To separate the actual manipulation of unit values from the related
// quantity
// </motivation>
//
// <todo asof="941110">
//   <li> Some inlining (did not work first go)
// </todo>

class UnitVal {
//# Friends
///    friend class UnitVal_init;
// Multiply
    friend UnitVal operator*(const UnitVal &in, const UnitVal &other);
// Divide
    friend UnitVal operator/(const UnitVal &in, const UnitVal &other);
// Output a unit as a value and a string of SI defining units
    friend ostream& operator<<(ostream &os, const UnitVal &ku);

public:
//# Constructors
// Construct an undimensioned value of 1
    UnitVal();
// Copy constructor
    UnitVal(const UnitVal &other);

// Construct an undimensioned value
    UnitVal(Double factor);

// Construct a fully dimensioned value
// <thrown>
//   <li> AipsError
// </thrown>
    UnitVal(Double factor, const String &s);

// Construct a value with a single unit at position specified
    UnitVal(Double factor, Int pos);

// Destructor
    ~UnitVal();

//# Operators
// Assignment (copy semantics)
    UnitVal &operator=(const UnitVal &other);

// Manipulate units
// <group name="manipulate">
// Multiply different units
    UnitVal &operator*=(const UnitVal &other);

// Divide different units
    UnitVal &operator/=(const UnitVal &other);

// Compare the dimensionality of different units
    Bool operator==(const UnitVal &other) const;
    Bool operator!=(const UnitVal &other) const;
// </group>

//# General member functions

// Raise a unit to an integer power
    UnitVal pow(Int p);

// Get the data parts of the unit value definition
// <group name="get data">
// Get the dimensions in the defining SI units
    const UnitDim &getDim() const;

// Get the factor of the unit (as compared to pure SI units)
    Double getFac() const;
// </group>

//# Helper functions
// Convert a unit string to a proper unit value and cache the result. The
// function will return False if invalid string specified
    static Bool check(const String &s);

// Convert a unit string to a proper unit value, cache the result and compare
// the dimension with the specified unit value. False if any of the steps fails
    static Bool check(const String &s, UnitVal &loc);


//# Data members
// Some constants to check type of units
// <group name="unit kinds">
static UnitVal NODIM;
static UnitVal LENGTH;
static UnitVal MASS;
static UnitVal TIME;
static UnitVal CURRENT;
static UnitVal TEMPERATURE;
static UnitVal INTENSITY;
static UnitVal MOLAR;
static UnitVal ANGLE;
static UnitVal SOLIDANGLE;
// </group>

private:
//# Data members
// The factor necessary to express the specified unit in the defining SI units
    Double kindFactor;

// The dimensions of the unit in the defining SI units
    UnitDim kindDim;

//# Helper functions
// This function is used, in conjunction with the
// <linkto class=UnitVal_init>UnitVal_init</linkto>
// class to force construction of statics (see ARM 3.4).
static void init();

// Convert (and check) a unit string to an SI value representation
// <group>
  static Bool create(const String &s, UnitVal &res);
  static Bool create(MUString &str, UnitVal &res);
// </group>

// Determine sign of unit power (i.e. if '.' or '/')
    static Int psign(MUString &str);

// Determine exponent of unit symbol
    static Int power(MUString &str);

// Determine symbol name in unit string
    static Bool field(MUString &str, UnitVal &res);

};


// <summary>
// Class used to force construction of <linkto class=UnitVal>UnitVal</linkto>.
// </summary>

// <synopsis>
// A static object of this class is used to make sure that
// <linkto class=UnitVal>UnitVal</linkto>
// is constructed before it is needed, and therefore that its static data
// members are defined.  See Meyers, p. 47.
// </synopsis>

// <use visibility=local>

// <linkfrom anchor="UnitVal_init" classes="UnitVal">
//   <here>UnitVal_init</here> --
// Class used to force construction of <linkto class=UnitVal>UnitVal</linkto>.
// </linkfrom>

class UnitVal_init {
public:
    UnitVal_init();
    ~UnitVal_init();
private:
    static uShort count;
};

// <summary>
// Object used to force construction of <linkto class=UnitVal>UnitVal</linkto>.
// </summary>

// <synopsis>
// This static object of the <linkto class=UnitVal_init>UnitVal_init</linkto>
// class is used to make sure that
// <linkto class=UnitVal>UnitVal</linkto>
// is constructed before it is needed, and therefore that its static data
// members are defined.  See Meyers, p. 47.
// </synopsis>

// <use visibility=local>

// <linkfrom anchor="UnitVal initialization object" classes="UnitVal UnitVal_init">
//   <here>UnitVal initialization object</here> --
// Object used to force construction of <linkto class=UnitVal>UnitVal</linkto>.
// </linkfrom>

// <group name="UnitVal initialization object">

static UnitVal_init unitval_init;

// </group>

//# Inline Implementations

//# Global functions
// Output
ostream& operator<<(ostream &os, const UnitVal &ku);

#endif
