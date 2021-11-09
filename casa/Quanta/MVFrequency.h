//# MVFrequency.h: Internal value for MFrequency
//# Copyright (C) 1996,1997,1998,1999,2000,2001
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

#ifndef CASA_MVFREQUENCY_H
#define CASA_MVFREQUENCY_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Quanta/QC.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/MeasValue.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

// <summary> Internal value for MFrequency </summary>

// <use visibility=export>

// <reviewed reviewer="tcornwel" date="1996/02/23" tests="tMeasMath" demos="">
// </reviewed>

// <prerequisite>
// <li> <linkto class=MeasValue>MeasValue</linkto>
// </prerequisite>
//
// <etymology>
// From Measure, Value and Frequency
// </etymology>
//
// <synopsis>
// An MVFrequency is a simple Double, to be used in the MFrequency measure.
// Requirements can be found in the 
// <linkto class=MeasValue>MeasValue</linkto> base class.<br>
// The only reasonable constructor is (but all MeasValue constructors are
// present)
// <src>MVFrequency(Double)</src> (with assumed Hz units);
// and an <src>operator Double</src> takes
// care of all other possibilities. Its external use is for
//  <linkto class=MeasConvert>MeasConvert</linkto>, to distinguish between
// input in internal Measure units, and values which have to have
// units applied.<br>
// The MVFrequency(Quantum) constructors recognise the type of wave
// characteristics presented from its units. Recognised are:
// <ul>
//   <li> frequency (1/time)
//   <li> time
//   <li> angle/time
//   <li> wavelength
//   <li> 1/wavelength (in 2pi units)
//   <li> energy (h.nu)
//   <li> impulse
// </ul>
// <br> The frequency is returned in Hz with getValue(); or as a Quantity
// in Hz with get(); or in one of the above units with get(unit).
// </synopsis>
//
// <example>
// See <linkto class=MFrequency>MFrequency</linkto>
// </example>
//
// <motivation>
// To aid coordinate transformations possibilities
// </motivation>
//
// <todo asof="1996/04/15">
// </todo>

class MVFrequency : public MeasValue {

public:
    
  //# Constructors
  // Default constructor: generate a zero value
  MVFrequency();
  // Copy constructor
  MVFrequency(const MVFrequency &other);
  // Copy assignment
  MVFrequency &operator=(const MVFrequency &other);
  // Constructor from Double, assuming Hz
  MVFrequency(Double d);
  // Constructor from Quantum : value taken will be the canonical value
  // <group>
  MVFrequency(const Quantity &other);
  MVFrequency(const Quantum<Vector<Double> > &other);
  // </group>
  // Constructor from Vector. A zero value will be taken for an empty vector,
  // the canonical value for a quantum vector.
  // <thrown>
  //  <li> AipsError if vector length > 1
  // </thrown>
  // <group>
  MVFrequency(const Vector<Double> &other);
  MVFrequency(const Vector<Quantity> &other);
  // </group>
  
  // Destructor
  ~MVFrequency();
  
  //# Operators
  // Conversion operator
  operator Double() const;
  
  // Addition
  // <group>
  MVFrequency &operator+=(const MVFrequency &other);
  MVFrequency &operator-=(const MVFrequency &other);
  // </group>
  // Comparisons
  // <group>
  Bool operator==(const MVFrequency &other) const;
  Bool operator!=(const MVFrequency &other) const;
  Bool near(const MVFrequency &other, Double tol = 1e-13) const;
  Bool nearAbs(const MVFrequency &other, Double tol = 1e-13) const;
  // </group>
  
  //# General member functions
  
  // Tell me your type
  // <group>
  static void assure(const MeasValue &in);
  // </group>
  
  // Print data
  virtual void print(ostream &os) const;
  // Clone
  virtual MeasValue *clone() const;
  // Adjust value: taken from base class, a NOP.
  // Get value in Hz
  Double getValue() const;
  // Get quantity in Hz
  Quantity get() const;
  // Get the wave characteristics in (recognised) specified units
  Quantity get(const Unit &unit) const;
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
  //# Data
  // Value
  Double val;
  
  //# Member functions
  // Get correct data type conversion factor from input Quantum
  Double makeF(Double v, const Unit &dt, Bool rev=False) const;
};


} //# NAMESPACE CASACORE - END

#endif
