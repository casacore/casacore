//# MVDoppler.h: Internal value for MDoppler
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

#ifndef CASA_MVDOPPLER_H
#define CASA_MVDOPPLER_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Quanta/QC.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/MeasValue.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

// <summary> Internal value for MDoppler </summary>

// <use visibility=export>

// <reviewed reviewer="tcornwel" date="1996/02/23" tests="tMeasMath" demos="">
// </reviewed>

// <prerequisite>
// <li> <linkto class=MeasValue>MeasValue</linkto>
// </prerequisite>
//
// <etymology>
// From Measure, Value and Doppler
// </etymology>
//
// <synopsis>
// An MVDoppler is a simple double, to be used in the MDoppler measure.
// Requirements can be found in the 
// <linkto class=MeasValue>MeasValue</linkto> base class.<br>
// The only reasonable constructor is (but all MeasValue constructors are
// present)
// <src>MVDoppler(double)</src>; and an <src>operator double</src> takes
// care of all other possibilities. Its external use is for
//  <linkto class=MeasConvert>MeasConvert</linkto>, to distinguish between
// input in internal Measure units, and values which have to have
// units applied.<br>
// The MVDoppler(Quantum) constructors recognise the type of representation
// characteristics presented from its units. Recognised are:
// <ul>
//   <li> no dimensions: value assumed to be dimensionless as is
//   <li> velocity: value will be divided by c (light velocity)
// </ul>
// <br> The Doppler is returned dimensionless with getValue(); or as a Quantity
// in m/s with get(); or in one of the related units with get(unit).
// </synopsis>
//
// <example>
// See <linkto class=MDoppler>MDoppler</linkto>
// </example>
//
// <motivation>
// To aid coordinate transformations possibilities
// </motivation>
//
// <todo asof="1996/04/15">
// </todo>

class MVDoppler : public MeasValue {
  
public:
  
  //# Constructors
  // Default constructor: generate a zero value
  MVDoppler();
  // Copy constructor
  MVDoppler(const MVDoppler &other);
  // Copy assignment
  MVDoppler &operator=(const MVDoppler &other);
  // Constructor from double
  MVDoppler(double d);
  // Constructor from Quantum : value taken will be the canonical value
  // <group>
  MVDoppler(const Quantity &other);
  MVDoppler(const Quantum<Vector<double> > &other);
  // </group>
  // Constructor from Vector. A zero value will be taken for an empty vector,
  // the canonical value for a quantum vector.
  // <thrown>
  //  <li> AipsError if vector length > 1
  // </thrown>
  // <group>
  MVDoppler(const Vector<double> &other);
  MVDoppler(const Vector<Quantity> &other);
  // </group>
  
  // Destructor
  ~MVDoppler();
  
  //# Operators
  // Conversion operator
  operator double() const;
  
  // Addition
  // <group>
  MVDoppler &operator+=(const MVDoppler &other);
  MVDoppler &operator-=(const MVDoppler &other);
  // </group>
  // Comparisons
  // <group>
  bool operator==(const MVDoppler &other) const;
  bool operator!=(const MVDoppler &other) const;
  bool near(const MVDoppler &other, double tol = 1e-13) const;
  bool nearAbs(const MVDoppler &other, double tol = 1e-13) const;
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
  // Get value as ratio
  double getValue() const;
  // Get quantity in m/s
  Quantity get() const;
  // Get the Doppler value in (recognised) specified units
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

private:
  //# Data
  // Value
  double val;
  
  //# Member functions
  // Get correct data type conversion factor from input Quantum
  double makeD(double v, const Unit &dt, bool rev=false) const;
};


} //# NAMESPACE CASACORE - END

#endif
