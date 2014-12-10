//# UnitName.h: defines a tagged unit definition
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001
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

#ifndef CASA_UNITNAME_H
#define CASA_UNITNAME_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/Quanta/UnitVal.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

//# Constants
// <note role=warning>
// SUN compiler does not accept non-simple default arguments
// </note>
static const String EmptyString="";

// <summary>
// handles physical units
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tUnit">
//
// <prerequisite>
// You should have at least a preliminary understanding of these classes:
//   <li> <linkto class=Unit>Unit</linkto>
// </prerequisite>
//
// <etymology>
// The class name derives from the basic Unit and the Name giving possibilities
// of this class to a newly defined unit tag.
// </etymology>
//
// <synopsis>
// Physical units are strings consisting of one or more names of known
// basic units, separated by '.' or ' ' (for multiplication) or '/' (for
// division). Each name can optionally be preceded by a standard decimal
// prefix, and/or followed by an (optionally signed) exponent.
//
// Example:
//	km/s/(Mpc.s)2  is identical to km.s-1.Mpc-2.s-2
//
// See the <linkto class="Unit">Unit</linkto> class for more details.
//
// The UnitName class defines new basic, tagged units. If, e.g., for one
// reason or another you want, in addition to the standard defined SI and
// customary units, to define a unit with a name 'KPH' to stand for the
// composite SI unit 'km/s', it can be done by creating a UnitName, and
// mapping it to the UnitMap lists.
// <note role=tip> The UnitMap::putUser can also be used without creating a UnitName
// first
// </note>
// <srcblock>
// UnitName myKPH( "KPH", UnitVal(3.6,"km/ks"));
// UnitMap::putUser(myKPH);
// </srcblock>
//
//  <h3> Constructing a tagged unit definition </h3>
// The following constructors are available:
// <ol>
//   <li> UnitName()			create unnamed value 1.
//   <li> UnitName(const UnitName&)	copy constructor
//   <li> UnitName("tag", UnitVal, "full name")
// </ol>
//
// An assignment (copy semantics) is available.
// 
//
//  <h3> Obtaining information about tagged unit </h3>
// The following information can be obatined from a UnitName:
// <ol>
//   <li> UnitVal getVal() const	will return the unit definition value
//   <li> String  getName() const	will return the unit name
// </ol>
// 
//
// </synopsis>
//
// <example>
// To obtain the definition of a Jy, you could:
// <srcblock>
// // Define a UnitVal unit definition
// UnitVal mydef;
// // And fill it with the appropiate definition
// mydef = (UnitMap::getUnit("Jy"))->getVal();
// </srcblock>
// </example>
//
//# // <motivation>
//# // </motivation>
//
// <todo asof="941110">
//   <li> Some inlining (did not work first go)
// </todo>

class UnitName {
//# friends
// Output the unit tag, description and its definition
    friend ostream& operator<< (ostream &os, const UnitName &name);

public:
//# Constructors
// Default constructor
    UnitName();

// Copy constructor
    UnitName(const UnitName &other);

// Construct from different parts
// <group>
    UnitName(const String &tag, const UnitVal &kind,
	      const String &name = EmptyString);
    UnitName(const Unit &tag, const String &name = EmptyString);
// </group>


// Destructor
    ~UnitName();

//# Operators
// Assigment (copy semantics)
    UnitName &operator=(const UnitName &other);

//# General member functions
// Get definition value of the unit
    const UnitVal &getVal() const;

// Get the name tag of the defined unit
    const String &getName() const;

private:
//# Data members
// Value of defined unit
    UnitVal basicKind;

// Name tag of unit
    String basicTag;

// Full name and description of unit
    String basicName;

};

//# Inline Implementations


} //# NAMESPACE CASACORE - END

#endif
