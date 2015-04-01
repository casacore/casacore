//# Interpolate1D.h: Interpolate in one dimension
//# Copyright (C) 1996,1997,1999,2002,2005
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
//# $Id$

#ifndef SCIMATH_INTERPOLATE1D_H
#define SCIMATH_INTERPOLATE1D_H

#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Function1D.h>
#include <casacore/casa/Containers/Block.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class Range> class SampledFunctional;

// <summary> Interpolate in one dimension </summary>

// <use visibility=export>

// <reviewed reviewer="wyoung" date="1996/10/18" tests="tInterpolate1D" demos="dInterpolate1D">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=SampledFunctional>SampledFunctional</linkto> 
//   <li> <linkto class=Function1D>Function1D</linkto> 
// </prerequisite>

// <etymology>
// The Interpolate1D class does interpolation in one dimension only.
// </etymology>

// <synopsis>
// This class will, given the abscissa and ordinates of a set of one
// dimensional data, interpolate on this data set giving the value at any
// specified ordinate. It will extrapolate if necessary, but this is will
// usually give a poor result. There is no requirement for the ordinates to
// be regularly spaced, or even sorted in any numerical order. However each
// abscissa should have a unique value. 
//
// Interpolation can be done using the following methods:
// <ul>
//   <li> Nearest Neighbour  (default if there is one data point)
//   <li> Linear (default unless there is only one data point)
//   <li> Cubic Polynomial
//   <li> Natural Cubic Spline
// </ul>
//
// The restriction that each abcissus has a unique value can be lifted
// by setting the <src>uniq=True </src> option in the appropriate
// functions. This imposes the following additional restrictions on
// interpolation. 
// <ul>
// <li> You cannot use cubic spline interpolation. 
// <li> You cannot cannot interpolate within two data points of a repeated
//      x-value when using cubic interpolation. 
// <li> You cannot  interpolate within one data point of a repeated
//      x-value when using linear or nearest neighbour interpolation. 
// </ul>
//
// The abscissa must be a SampledFunctional that returns a scalar value that
// can be ordered. ie. an uInt, Int, Float or Double (not Complex). The
// ordinate can be any data type that has addition, and subtraction defined
// as well as multiplication by a scalar. So the ordinate can be complex
// numbers, where the interpolation is done separately on the real and
// imaginary components, or an array, where the interpolation is done
// separately on an element by element basis.
//
// This class will curently make an internal copy of the data supplied to
// it, and sort the data if it is not told it is already sorted, by using
// the <src> sorted=True </src> flag.
// </synopsis>

// <example>
// This code fragment sets the interpolation method to cubic before
// interpolating on the supplied (x,y) vectors.
// <srcblock>
//  Vector<Float> x(4); indgen(x); 
//  Vector<Double> y(4); indgen(y); y = y*y*y;
//  ScalarSampledFunctional<Float> fx(x)
//  ScalarSampledFunctional<Double> fy(y);
//  Interpolate1D<Float, Double> gain(fx, fy);
//  gain.setMethod(Interpolate1D<Float,Double>::cubic);
//  for (Float xs = -1; xs < 5; xs += 0.1)
//    cout << "gain(" << xs << "):" << gain(xs) << endl;
// </srcblock>
// </example>

// <motivation>
// This class is motivated by the need to interpolate over the gain
// solutions obtained from calibrator observations, in order to get the gain
// at arbitrary times. 
// </motivation>

// <templating arg=Domain>
//  <li> The Domain class must be a type that can be ordered in a mathematical
// sense. This includes uInt, Int, Float, Double, but not Complex. 
// </templating>

// <templating arg=Range>
// <li> The Range class must have addition and subtraction of Range objects with
// each other as well as multiplication by a scalar defined. Besides the
// scalar types listed above this includes Complex, DComplex, and Arrays of
// any of these types.
// </templating>

// <thrown>
//    <li> AipsError
// </thrown>

// <todo asof="1996/10/22">
//   <li> avoid an internal copy of the data and have an index array as the
//        only private data (plus the interpolation method and pointers to
//        the actual data). 
//   <li> Review the use of copy semantics in the copy constructor &
//        assignment operator after making the above change. 
// </todo>

template <class Domain, class Range> class Interpolate1D :
public Function1D<Domain, Range> {
public:
  // The different interpolation methods are enumerated here
  enum Method {
    // Crude but sometimes useful
    nearestNeighbour, 
    // The most common method and the Default
    linear,           
    // Fits a third order polynomial to 4 pts 
    cubic,            
    // Natural Cubic Splines
    spline
  };

  // The default constructor generates a useless object until the setData
  // function has been called.
  Interpolate1D();

  // Construct an object with the specified data
  Interpolate1D(const SampledFunctional<Domain> &x, 
		const SampledFunctional<Range> &y, 
		const Bool sorted=False, 
		const Bool uniq=False);

  // Define a new data set for the class to operate on. Equivalent in many
  // aspects to creating a new object.
  void setData(const SampledFunctional<Domain> &x, 
	       const SampledFunctional<Range> &y, 
	       const Bool sorted=False, 
	       const Bool uniq=False);

  // The standard copy constructor, assignment operator and
  // destructor. Internal data is copied in both cases (copy semantics)
  // <group>
  Interpolate1D(const Interpolate1D<Domain, Range>& other);
  Interpolate1D<Domain, Range> & 
    operator=(const Interpolate1D<Domain, Range> & other);
  ~Interpolate1D();
  // </group>

  // Name of function
  virtual const String &name() const { static String x("interpolate1d");
    return x; }

  // Interpolation is done using the () operator (see example above). Actual
  // use is through the virtual <src>eval()</src> function.
  virtual Range eval(typename Function1D<Domain, Range>::FunctionArg x)
    const;

  // inquire/set the current interpolation method. uInts are used as
  // arguments instead of the Interpolate1D::Method enumerator due to
  // compiler limitations. See the example above (or the demo code) for the
  // recommended way to call these functions.
  // <group>
  uInt getMethod() const {return curMethod;}
  void setMethod(uInt method);
  // </group>

  // Access the data set that interpolation is done over. This will usually be
  // sorted. 
  // <group>
  Vector<Domain> getX() const;
  Vector<Range> getY() const;
  // </group>

  // A function to copy the Interpolate1D object
  // <group>
  virtual Function<Domain, Range> *clone() const;
  // </group>

private:
  // A private function for doing polynomial interpolation
  Range polynomialInterpolation(const Domain x, uInt n, uInt offset) const;

  uInt curMethod;        // interpolation method to use
  uInt nElements;        // how many elements in the data set
  Block<Domain> xValues; // the abscissa of the data set (sorted)
  Block<Range> yValues;  // The corresponding ordinate of the data set
  Block<Range> y2Values; // The numerical second derivates (only for splines)

};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/Interpolate1D.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
