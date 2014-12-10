//# ArraySampledFunctional:
//# Copyright (C) 1996,1997,1999
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

#ifndef SCIMATH_ARRAYSAMPLEDFUNCTIONAL_H
#define SCIMATH_ARRAYSAMPLEDFUNCTIONAL_H

#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/SampledFunctional.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/IPosition.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> Index into an array using the longest axis </summary>

// <use visibility=export>

// <reviewed reviewer="wyoung" date="1996/10/19" tests="tSampledFunctional.cc">
// </reviewed>

// <prerequisite>
//   <li> SampledFunctional
//   <li> Array
// </prerequisite>

// <etymology>
// A SampledFunctional is an interface that allows random access to a fixed
// size data set. An ArraySampledFunctional allows you to access slices of
// an an Array<T> object. 
// </etymology>

// <synopsis> 
//
// An ArraySampledFunctional allows an Array<T> object to be sliced up with
// each sample being one slice of the original Array. The slices are always
// the same size and the indexing is always done along the last
// non-degenerate dimension. For example a(4,3,20,1) is interpreted as a
// SampledFunctional with 20 elements, each one being a 4 by 3 matrix.
//
// The Array that is passed to the constructor is copied by this class but
// because Arrays themselves use reference symantics, the actual data is not
// copied but referenced. This means that modifying the data in the original
// array will correspondingly modify the data accessed by this class. 
//
// Similarly the Array that is returned for each Slice is a reference to the
// actual data so that modifying this array really modifies the original
// data. This is not recommended as the operator() function is not supposed
// to be used to get a modifiable portion of the data.
//   </synopsis>

// <example>
// Constructing and using ArraySampledFunctionals
// <srcblock> 
// Array<Float> a(IPosition(4,4,3,20,1)); // Create an array
// ... Fill the array any way you like ... 
// ArraySampledFunctional<Array<Float> >fa(a);
// for(uInt i = 0; i < 20; i++)
//  cout << "f(" << i << ") = " << fa(i) << endl;
// // Each 'slice' is a 4 by 3 Matrix
// </srcblock>
// </example>

// <motivation>
// I needed a SampledFunctional which could return Arrays of arbitrary (but
// fixed) size for each index. This could be done using a <src>
// ScalarSampledFunctional<Array<T> > </src> but is ineffecient as each
// element is stored as a separate Array. This class is a more efficient way
// to solve this problem.
// </motivation>

// <templating arg=T>
// <li> The template type MUST be an Array of some arbitrary type. This is
// because this class will return a slice of this Array. The Array template
// type cannot be subsumed into the class definition because the definition
// of the inherited operator() function means that the return type must be
// the template type
// </templating>

// <thrown>
// <li> Exceptions are not thrown directly by this class.
// </thrown>

// <todo asof="1996/10/19">
//   <li> Nothing I can think of.
// </todo>

template<class T> class ArraySampledFunctional
  :public SampledFunctional<T>
{
public:
  // These constructors copy the array that is passed to them. But because
  // arrays use reference symantics the data is not copied. The default
  // constructor is basically useless, as there is no way to add the data
  // once the class has been constructed.  
  // <group>
  ArraySampledFunctional();
  ArraySampledFunctional(const T & data);
  // </group>

  // The standard copy constructor and assignment operator
  // <group>
  ArraySampledFunctional(ArraySampledFunctional<T> & other);
  ArraySampledFunctional<T> & operator=(ArraySampledFunctional<T> &other);
  // </group>

  // Define the functions for the SampledFunction interface
  // <group>
  virtual T operator()(const uInt & index) const;
  virtual uInt nelements() const;
  virtual ~ArraySampledFunctional();
  // </group>

  // An alternate version of the sampling function which is more effecient
  // because it does not need to create as many temporary objects or copy the
  // Array data.
  // <group>
  const T operator()(const uInt & index);
  // </group>

private:
  T theRefData;
  IPosition theEnd;
  uInt theLastAxis;
  uInt theNelements;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/ArraySampledFunctional.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
