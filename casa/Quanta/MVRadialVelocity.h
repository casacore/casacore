//# MVRadialVelocity.h: Internal value for MRadialvelocity
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2003
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

#ifndef CASA_MVRADIALVELOCITY_H
#define CASA_MVRADIALVELOCITY_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/MeasValue.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class Quantum;

// <summary> Internal value for MRadialVelocity </summary>

// <use visibility=export>

// <reviewed reviewer="tcornwel" date="1996/02/23" tests="tMeasMath" demos="">
// </reviewed>

// <prerequisite>
// <li> <linkto class=MeasValue>MeasValue</linkto>
// </prerequisite>
//
// <etymology>
// From Measure, Value and Radial Velocity
// </etymology>
//
// <synopsis>
// An MVRadialVelocity is a simple double, to be used in the MRadialVelocity 
// measure.
// Requirements can be found in the 
// <linkto class=MeasValue>MeasValue</linkto> base class.<br>
// The only reasonable constructor is (but all MeasValue constructors are
// present)
// <src>MVRadialVelocity(double)</src>; and an <src>operator double</src> takes
// care of all other possibilities. Its external use is for
//  <linkto class=MeasConvert>MeasConvert</linkto>, to distinguish between
// input in internal Measure units, and values which have to have
// units applied.<br>
// The MVRadialVelocity(Quantum) constructors recognise the type of wave
// characteristics presented from its units. Recognised are:
// <ul>
//   <li> velocity (length/time)
// </ul>
// <br> The velocity is returned in m/s with getValue(); or as a Quantity
// in m/s with get(); or in the specified units with get(unit).
//
// A <em>shiftFrequency()</em> method can shift frequencies.
// </synopsis>
//
// <example>
// See <linkto class=MRadialVelocity>MRadialVelocity</linkto>
// </example>
//
// <motivation>
// To aid coordinate transformations possibilities
// </motivation>
//
// <todo asof="1996/04/15">
// </todo>

class MVRadialVelocity : public MeasValue {
  
public:
  
  //# Constructors
  // Default constructor: generate a zero value
  MVRadialVelocity();
  // Copy constructor
  MVRadialVelocity(const MVRadialVelocity &other);
  // Copy assignment
  MVRadialVelocity &operator=(const MVRadialVelocity &other);
  // Constructor from double (assume m/s)
  MVRadialVelocity(double d);
  // Constructor from Quantum
  // <group>
  MVRadialVelocity(const Quantity &other);
  MVRadialVelocity(const Quantum<Vector<double> > &other);
  // </group>
  // Constructor from Vector. A zero value will be taken for an empty vector,
  // the first element for a quantum vector.
  // <thrown>
  //  <li> AipsError if vector length > 1
  // </thrown>
  // <group>
  MVRadialVelocity(const Vector<double> &other);
  MVRadialVelocity(const Vector<Quantity> &other);
  // </group>
  
  // Destructor
  ~MVRadialVelocity();
  
  //# Operators
  // Conversion operator
  operator double() const;
  
  // Addition
  // <group>
  MVRadialVelocity &operator+=(const MVRadialVelocity &other);
  MVRadialVelocity &operator-=(const MVRadialVelocity &other);
  // </group>
  // Comparisons
  // <group>
  bool operator==(const MVRadialVelocity &other) const;
  bool operator!=(const MVRadialVelocity &other) const;
  bool near(const MVRadialVelocity &other, double tol = 1e-13) const;
  bool nearAbs(const MVRadialVelocity &other, double tol = 1e-13) const;
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
  // Get value in m/s
  double getValue() const;
  // Get quantity in m/s
  Quantity get() const;
  // Get the wave characteristics in (recognised) specified units
  Quantity get(const Unit &unit) const;
  // Get the value in internal units
  virtual Vector<double> getVector() const;
  // Set the value from internal units (set 0 for empty vector)
  virtual void putVector(const Vector<double> &in);
  // Get the internal value as a <src>Vector<Quantity></src>. Usable in
  // records. The getXRecordValue() gets additional information for records.
  // Note that the Vectors could be empty.
  // <group>
  virtual Vector<Quantum<double> > getRecordValue() const;
  // </group>
  // Set the internal value if correct values and dimensions
  virtual bool putValue(const Vector<Quantum<double> > &in);
  // Shift the input frequencies to the output frequencies. In the case of
  // simple double inputs, it is assumed that the values are linearly dependent
  // on frequency. I.e. frequencies given as wavelength or time cannot be used.
  // <group>
  Vector<double> shiftFrequency(const Vector<double> &freq) const;
  Quantum<Vector<double> >
    shiftFrequency(const Quantum<Vector<double> > &freq) const;
  // </group>
  
private:
  //# Data
  // Value
  double val;
  
  //# Member functions
  // Get correct data type conversion factor from input Quantum
  double makeF(const Unit &dt) const;
};


} //# NAMESPACE CASACORE - END

#endif
