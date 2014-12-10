//# UnitDim.h: defines the (private) class describing basic SI dimensions
//# Copyright (C) 1994,1995,1996,1997,1999,2000,2001
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

#ifndef CASA_UNITDIM_H
#define CASA_UNITDIM_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class String;
class UnitVal;
class UnitMap;

// 
// <summary>
// describes a unit in basic SI unit dimensions
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tUnit">
//
// <prerequisite>
// You should have at least a preliminary understanding of these classes:
//   <li> <linkto class=Unit>Unit</linkto>
// </prerequisite>
//
// <etymology>
// Based on Unit and the Dimension of a unit in SI defining units
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
// See the <linkto class=Unit>Unit</linkto> for more details.
//
// The UnitDim class is a private class for use by the Unit classes. It
// contains the dimensions in the 9 basic defining SI units of a unit.
// </synopsis> 
//
// <example>
// </example>
//
// <motivation>
// The UnitDim class has been separated to keep the interface between a
// complex unit description string and the basic SI units clean.
// </motivation>
//
// <todo asof="941110">
//   <li> Some inlining (did not work first go)
// </todo>

class UnitDim {

//# Friends
    friend class UnitVal;
    friend class UnitMap;
// Output the SI dimensions (e.g. 'km/s/g' as 'm kg-1 s-1')
    friend ostream& operator<<(ostream &os, const UnitDim &du);

public:
//# Enumerations
// Enumeration of the order and number of the defining SI units.
// If order or contents changed, change also in dimName() and dimFull().
    enum Dim {Dm=0, Dkg, Ds, DA, DK, Dcd, Dmol, Drad, Dsr, Dnon, Dnumber};
// Constants
// Number of Longs to cater for 9 bytes.
#define UNITDIM_DLNUMBER 3

// Destructor
    ~UnitDim();
protected:
    void init( );
    void init(Int pos);

private:
//# Constructors
// Construct a unit with zero dimension in all SI units
    UnitDim() { init( ); }

// Copy constructor
    UnitDim(const UnitDim &other);

// Construct a unit dimension with a one in the indicated position (as
// Dim enumerator) and zeroes in all other units
    UnitDim(Int pos) { init(pos); }

//# Operators
// Assignment (copy semantics)
    UnitDim &operator=(const UnitDim &other);
// Operators to combine unit dimensions
// <group name="combine">
// Multiplication adds the unit dimensions of all SI units
    UnitDim &operator*=(const UnitDim &other);
    UnitDim operator*(const UnitDim &other) const;

// Division subtracts the unit dimensions of all SI units
    UnitDim &operator/=(const UnitDim &other);
    UnitDim operator/(const UnitDim &other) const;
// </group>
// Compare dimension of units
// <group name="compare">
// Compare for equal dimensions
    Bool operator==(const UnitDim &other) const;
// Compare for unequal dimensions
    Bool operator!=(const UnitDim &other) const;
// </group>

//# General Member Functions
// Raise all SI defining units to an integer power
    UnitDim pow(Int p);

// Get the tag for specified dimension
  static const String& dimName(uInt which);

// Get the full name for the specified dimension
  static const String& dimFull(uInt which);

//# Data Members
// 1-byte vector to contain the dimensions of the defining SI units
// (using same storage as Long vector for speed reasons)
    Long unitLong[UNITDIM_DLNUMBER];
    Char *unitDim;

};


//# Inline Implementations

//# Global definitions
// Output
    ostream& operator<<(ostream &os, const UnitDim &du);


} //# NAMESPACE CASACORE - END

#endif
