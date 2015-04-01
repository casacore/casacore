//# MVEpoch.h: a class for high precision time
//# Copyright (C) 1996,1997,1998,1999,2000
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

#ifndef CASA_MVEPOCH_H
#define CASA_MVEPOCH_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/MeasValue.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class Unit;

// <summary> A class for high precision time </summary>

// <use visibility=export>

// <reviewed reviewer="tcornwel" date="1996/02/22" tests="tMeasMath" demos="">
// </reviewed>

// <prerequisite>
//  <li> <linkto class=MeasValue>MeasValue</linkto> class
// </prerequisite>
//
// <etymology>
// MVEpoch from Measure, Value and Epoch
// </etymology>
//
// <synopsis>
// MVEpoch is a class for high precision (10<sup>-16</sup> s) epochs over a
// period of 6*10<sup>10</sup> a.<br>
// MVEpochs can be compared, a time interval can be added or subtracted, and
// the time difference can be found.
// The following constructors:
// <ul>
//   <li> <src>MVEpoch()</src> default; assuming 0
//   <li> <src>MVEpoch(Double)</src> with time given in days
//   <li> <src>MVEpoch(Double, Double=0)</src> with times given in days
//   <li> <src>MVEpoch(Quantity, Quantity=0)</src> with times given
//   <li> <src>MVEpoch(Quantum<Vector<Double> >)</src> with times given
//   <li> <src>MVEpoch(Vector<Double>)</src> with times in days
//   <li> <src>MVEpoch(Vector<Quantity>)</src> with times
// </ul>
// </synopsis>
//
// <example>
// See <linkto class=MEpoch>MEpoch</linkto>
// </example>
//
// <motivation>
// To have high precision timing
// </motivation>
//
// <todo asof="1996/02/04">
//   <li> A proper high precision time, including multiplication etc should
//		be considered. E.g. a multi-byte number with 6 bytes day
//		and 8 bytes fractional day.
// </todo>

class MVEpoch : public MeasValue {

public:
  
  //# Friends
  
  //# Constructors
  // Default constructor, generates default 0 epoch
  MVEpoch();
  // Copy constructor
  MVEpoch(const MVEpoch &other);
  // Constructor with time in days
  // <group>
  MVEpoch(Double inday, Double infrac=0);
  MVEpoch(const Vector<Double> &inday);
  // </group>
  // Constructor with Quantities
  // <group>
  MVEpoch(const Quantity &in);
  MVEpoch(const Quantity &in1, const Quantity &in2);
  MVEpoch(const Quantum<Vector<Double> > &in);
  MVEpoch(const Vector<Quantity> &in);
  // </group>
  
  //# Destructor
  ~MVEpoch();
  
  //# Operators
  // Copy assignment
  MVEpoch &operator=(const MVEpoch &other);
  
  // Add times
  // <group>
  MVEpoch &operator+=(const MVEpoch &other);
  MVEpoch operator+(const MVEpoch &other) const;
  // </group>
  
  // Difference times
  // <group>
  MVEpoch &operator-=(const MVEpoch &other);
  MVEpoch operator-(const MVEpoch &other) const;
  // </group>
  
  // Comparisons
  // <group>
  Bool operator==(const MVEpoch &other) const;
  Bool operator!=(const MVEpoch &other) const;
  Bool near(const MVEpoch &other, Double tol = 1e-13) const;
  Bool nearAbs(const MVEpoch &other, Double tol = 1e-13) const;
  // </group>
  
  //# General Member Functions
  // Constants
  static const Double secInDay;
  
  // Tell me your type
  // <group>
  virtual uInt type() const;
  static void assure(const MeasValue &in);
  // </group>
  
  // Adjust the time to its constituent parts. The returned result is always 1.0
  // <group>
  virtual void adjust();
  virtual void adjust(Double &res);
  // </group>
  
  // Get value in days
  Double get() const;
  
  // Get value with units
  // <group>
  Quantity getTime() const;
  Quantity getTime(const Unit &unit) const;
  // </group>
  
  // Get value of integer days
  Double getDay() const;
  
  // Get fraction of days
  Double getDayFraction() const;

  // Print a value
  virtual void print(ostream &os) const;
  
  // Clone a value
  virtual MeasValue *clone() const;

  // Get the value in internal units
  virtual Vector<Double> getVector() const;
  // Set the value from internal units (set 0 for empty vector)
  virtual void putVector(const Vector<Double> &in);
  // Get the internal value as a <src>Vector<Quantity></src>. Usable in
  // records. The getXRecordValue() gets additional information for records.
  // Note that the Vectors could be empty.
  // <group>
  virtual Vector<Quantum<Double> > getRecordValue() const;
  // </group>
  // Set the internal value if correct values and dimensions
  virtual Bool putValue(const Vector<Quantum<Double> > &in);
  
private:
  //# Data members
  // Whole days
  // Note that if higher precision is needed, the splitting could be in
  // 0.001 days and fractions thereof
  Double wday;
  // Fraction of days
  Double frday;
  
  //# Member functions
  // Make days from quantity
  Double makeDay(const Quantity &in) const;
  // Add time from days
  void addTime(Double in);
};

//# Global functions


} //# NAMESPACE CASACORE - END

#endif
