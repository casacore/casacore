//# QBase.h: base class for Quantum
//# Copyright (C) 1994,1995,1996,1998,1999
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

#ifndef CASA_QBASE_H
#define CASA_QBASE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Quanta/Unit.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

class LogIO;

//# Typedefs

// 
// <summary>
// Base for Quantities (i.e. dimensioned values)
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tQuantum">
//
// <prerequisite>
//   <li> <linkto class=Unit>Unit</linkto>
// </prerequisite>
//
// <etymology>
// QBase is the base class for <linkto class=Quantum>Quantum</linkto>.
// </etymology>
//
// <synopsis> 
// Quantities are values with a unit. Their basic specification can be one of
// two forms:
// <srcblock>
// Quantity( Double value, String unit);	// or: Unit unit
// Quantum<Type> ( Type value, String unit)	// or: Unit unit
// </srcblock>
// See <linkto class=Quantum>Quantum</linkto> for details.
// </synopsis> 
//
// <motivation>
// To provide the possibilty of mixing units from different
// <src>Quantum<T1></src>, <src>Quantum<T2></src>
// </motivation>
//
// <todo asof="941123">
//   <li> Some inlining (did not work first go)
// </todo>

class QBase {
  //# Friends

public:
  //# Constructors
  // Default constructor, generates ""
  QBase();
  // Copy constructor
  QBase(const QBase &other);
  // Construct dimensioned QBase (e.g. 'km/Mpc')
  // <thrown>
  //   <li> AipsError if non-matching unit dimensions
  // </thrown>
  // <group>
  QBase(const Unit &s);
  // </group>
  
  // Destructor
  virtual ~QBase();
  
  //# Operators
  // Assignment (copy)
  QBase &operator=(const QBase &other);
  
  //# Member functions
  // Get units of QBase
  // <group name="get">
  // Return the string representation of the current units attached to QBase
  const String &getUnit() const;
  // </group>
  
  // Re-specify parts of a QBase
  // <group name="set">
  // Set new unit, without changing value
  void setUnit(const Unit &s);
  // Set new unit, copied from specified QBase, without changing value
  void setUnit(const QBase &other);
  // </group>
  
  // Check for conformal matching units (e.g. dam and Mpc)
  // <group name="check">
  // Using specified units
  Bool isConform(const Unit &s) const;
  // Using units specified in QBase
  Bool isConform(const QBase &other) const;
  // </group>
  
  // Get a copy of Quantum
  virtual QBase *clone() const = 0;
  // Get the unit attached to the Quantum (use getUnit() if only interested in
  // the String part of the unit)
  virtual const Unit &getFullUnit() const = 0;
  // Print a Quantum
  virtual void print(ostream &os) const = 0;
  // Get the type of derived Quantum (using QuantumType).
  // All should have:
  // static uInt myType();
  virtual uInt type() const = 0;
  
protected:
  //# Data members
  Unit qUnit;
};

//# Inline Implementations

//# Global functions
// <summary> Global functions </summary>
// <group name=Output>
// Output declaration
ostream &operator<<(ostream &os, const QBase &meas);
LogIO &operator<<(LogIO &os, const QBase &meas);

// </group>


} //# NAMESPACE CASACORE - END

#endif
