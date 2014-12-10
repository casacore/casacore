//# MeasBase.h: Base class for all measures
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2002
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
//#
//# $Id$

#ifndef MEASURES_MEASBASE_H
#define MEASURES_MEASBASE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/Measure.h>
#include <casacore/casa/Quanta/Quantum.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class Vector;

//# Typedefs

// <summary> Base class for all measures </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasure" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Measure>Measure</linkto> class
// </prerequisite>
//
// <etymology>
// Measure and Base
// </etymology>
//
// <synopsis>
// MeasBase forms derived Measure class for all actual measures
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// To have most work in single routine
// </motivation>
//
// <todo asof="1997/04/15">
//	<li>
// </todo>
//# Made non-virtual for MeasureHolder
//#template<class Mv, class Mr> class MeasBase : public virtual Measure {
template<class Mv, class Mr> class MeasBase : public Measure {

public:
  //# Friends

  //# Enumerations

  //# Typedefs

  //# Constructors
  // <note role=tip> In the following constructors and other functions, all 
  // <em>MeasRef</em> can be replaced with simple <src>Measure::TYPE</src>
  // where no offsets or frames are needed in the reference. For reasons
  // of compiler limitations the formal arguments had to be specified as
  // <em>uInt</em> rather than the Measure enums that should be used as actual 
  // arguments.</note>
  // Default constructor
  MeasBase();
  // Copy constructor
  MeasBase(const MeasBase<Mv,Mr> &other);
  // Copy assignment
  MeasBase<Mv,Mr> &operator=(const MeasBase<Mv,Mr> &other);
  // Create from data and reference
  // <group>
  MeasBase(const Mv &dt, const Mr &rf);
  MeasBase(const Mv &dt, uInt rf);
  MeasBase(const Quantity &dt, const Mr &rf);
  MeasBase(const Quantity &dt, uInt rf);
  MeasBase(const Measure *dt);
  MeasBase(const Mr &rf);
  MeasBase(const uInt rf);
  // </group>
  
  //# Destructor
  virtual ~MeasBase();
  
  //# Operators
  
  //# General Member Functions
  // Check the type of derived entity
  virtual Bool areYou(const String &tp) const;
  // Assert that we are the correct type
  // <thrown>
  //   <li> AipsError if wrong Measure
  // </thrown>
  virtual void assured(const String &tp) const;
  
  // Refill the specified entities
  // <group>
  void set(const Mv &dt);
  void set(const Mr &rf);
  void set(const Mv &dt, const Mr &rf);
  void set(const Unit &inunit);
  virtual void set(const MeasValue &dt);
  virtual Bool putValue(const Vector<Quantum<Double> > &in);
  // </group>
  
  // Get reference
  Mr getRef() const;
  
  // Get Measure data
  // <group>
  const Mv &getValue() const;
  // </group>
  
  // Get Unit
  const Unit &getUnit() const;
  
  // Get reference pointer
  virtual MRBase *getRefPtr() const;
  
  // Get pointer to data
  virtual const MeasValue* getData() const;
  
  // Print a Measure
  virtual void print(std::ostream &os) const;
  
protected:
  //# Enumerations
  
  //# Data
  // The measure value (e.g. instant in time)
  Mv data;
  // Reference frame data
  Mr ref;
  // Possible input units
  Unit unit;
  // Error information
  // MeasErr error;
  
  
private:
  //# Member functions
  // Clear the measure
  void clear();
  
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/measures/Measures/MeasBase.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
