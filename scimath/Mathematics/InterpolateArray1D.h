//# Interpolate1DArray.h: Interpolation in last dimension of an Array
//# Copyright (C) 1997,1999,2000,2001
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
//# $Id: Interpolate1DArray.h,v 8.1 1997/05/21 22:59:29 rm

#ifndef SCIMATH_INTERPOLATEARRAY1D_H
#define SCIMATH_INTERPOLATEARRAY1D_H


#include <casa/aips.h>

namespace casa { //# NAMESPACE CASA - BEGIN

template <class T> class PtrBlock;
template <class T> class Block;
template <class T> class Array;
template <class T> class Vector;
template <class T> class Cube;

// <summary> Interpolate in one dimension </summary>
 
// <use visibility=export>
 
// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>
 
// <prerequisite>
//   <li> <linkto class=Array>Array</linkto> 
//   <li> <linkto class=Vector>Vector</linkto> 
// </prerequisite>
 
// <etymology>
// The InterpolateArray1D class does interpolation in one dimension of
// an Array only.
// </etymology>
 
// <synopsis>
// This class will, given the abscissa and ordinates of a set of one
// dimensional data, interpolate on this data set giving the value at any
// specified ordinate. It will extrapolate if necessary, but this is will
// usually give a poor result. There is no requirement for the ordinates to
// be regularly spaced, however they do need to be sorted and each
// abscissa should have a unique value. 
//
// Interpolation can be done using the following methods:
// <ul>
//   <li> Nearest Neighbour 
//   <li> Linear (default unless there is only one data point)
//   <li> Cubic Polynomial
//   <li> Natural Cubic Spline
// </ul>
//
// The abscissa must be a simple type (scalar value) that
// can be ordered. ie. an uInt, Int, Float or Double (not Complex). The
// ordinate can be an Array of any data type that has addition, and 
// subtraction defined as well as multiplication by a scalar of the abcissa 
// type. 
// So the ordinate can be complex numbers, where the interpolation is done 
// separately on the real and imaginary components.
// Use of Arrays as the the Range type is discouraged, operations will 
// be very slow, it would be better to construct a single higher dimensional 
// array that contains all the data.
//
// Note: this class (and these docs) are heavily based on the
// <linkto class=Interpolate1D>Interpolate1D</linkto>
// class in aips/Functionals. That class proved to be
// too slow for interpolation of large data volumes (i.e. spectral line
// visibility datasets) mainly due to the interface which forced the 
// creation of large numbers of temporary Vectors and Arrays.
// This class is 5-10 times faster than Interpolate1D in cases where
// large amounts of data are to be interpolated.
// </synopsis>
 
// <example>
// This code fragment does cubic interpolation on (xin,yin) pairs to
// produce (xout,yout) pairs.
// <srcblock>
//  Vector<Float> xin(4); indgen(xin); 
//  Vector<Double> yin(4); indgen(yin); yin = yin*yin*yin;
//  Vector<Float> xout(20); 
//  for (Int i=0; i<20; i++) xout(i) = 1 + i*0.1;
//  Vector<Double> yout;
//  InterpolateArray1D<Float, Double>::interpolate(yout, xout, xin, yin, 
//                         InterpolateArray1D<Float,Double>::cubic);
// </srcblock>
// </example>
 
// <motivation>
// This class was motivated by the need to interpolate visibilities
// in frequency to allow selection and gridding in velocity space
// with on-the-fly doppler correction.
// </motivation>
 
// <templating arg=Domain>
// <li> The Domain class must be a type that can be ordered in a mathematical
// sense. This includes uInt, Int, Float, Double, but not Complex. 
// </templating>
 
// <templating arg=Range>
// <li> The Range class must have addition and subtraction of Range objects with
// each other as well as multiplication by a scalar defined. Besides the
// scalar types listed above this includes Complex, DComplex, and Arrays of
// any of these types. Use of Arrays is discouraged however.
// </templating>
 
// <thrown>
//    <li> AipsError
// </thrown> 
// <todo asof="1997/06/17">
//   <li> Implement flagging in cubic and spline interpolation
// </todo>
 

template <class Domain, class Range>
class InterpolateArray1D
{
public:
  // Interpolation methods
  enum InterpolationMethod {
    // nearest neighbour
    nearestNeighbour,
    // linear
    linear,
    // cubic
    cubic,
    // cubic spline
    spline
  };

  // Interpolate in the last dimension of array yin whose x coordinates 
  // along this dimension are given by xin. 
  // Output array yout has interpolated values for x coordinates xout.
  // E.g., interpolate a Cube(pol,chan,time) in the time direction, all
  // values in the pol-chan plane are interpolated to produce the output
  // pol-chan plane.
  static void interpolate(Array<Range>& yout, 
			  const Vector<Domain>& xout,
			  const Vector<Domain>& xin, 
			  const Array<Range>& yin,
			  Int method);

  // deprecated version of previous function using Blocks - no longer needed
  // now that Vector has a fast index operator [].
  static void interpolate(Array<Range>& yout, 
			  const Block<Domain>& xout,
			  const Block<Domain>& xin, 
			  const Array<Range>& yin,
			  Int method);

  // Interpolate in the last dimension of array yin whose x coordinates 
  // along this dimension are given by xin. 
  // Output array yout has interpolated values for x coordinates xout.
  // This version handles flagged data in a simple way: all outputs
  // depending on a flagged input are flagged.
  // If goodIsTrue==True, then that means
  // a good data point has a flag value of True (usually for 
  // visibilities, good is False and for images good is True)
  // If extrapolate==False, then xout points outside the range of xin
  // will always be marked as flagged.
  // TODO: implement flags for cubic and spline (presently input flags
  // are copied to output).  
  static void interpolate(Array<Range>& yout, 
			  Array<Bool>& youtFlags,
			  const Vector<Domain>& xout,
			  const Vector<Domain>& xin, 
			  const Array<Range>& yin,
			  const Array<Bool>& yinFlags,
			  Int method,
                          Bool goodIsTrue=False,
			  Bool extrapolate=False);

  // deprecated version of previous function using Blocks - no longer needed
  // now that Vector has a fast index operator [].
  static void interpolate(Array<Range>& yout, 
			  Array<Bool>& youtFlags,
			  const Block<Domain>& xout,
			  const Block<Domain>& xin, 
			  const Array<Range>& yin,
			  const Array<Bool>& yinFlags,
			  Int method,
                          Bool goodIsTrue=False,
			  Bool extrapolate=False);

  // Interpolate in the middle axis in 3D array (yin) whose x coordinates along the
  // this dimension are given by xin.
  // Interpolate a Cube(pol,chan,time) in the chan direction.
  // Currently only linear interpolation method is implemented.
  // TODO: add support for nearest neiborhood, cubic, and cubic spline. 
  static void interpolatey(Cube<Range>& yout,
                          const Vector<Domain>& xout,
                          const Vector<Domain>& xin,
                          const Cube<Range>& yin,
                          Int method);

  // Interpolate in the middle dimension of 3D array yin whose x coordinates 
  // along this dimension are given by xin. 
  // Output array yout has interpolated values for x coordinates xout.
  // This version handles flagged data in a simple way: all outputs
  // depending on a flagged input are flagged.
  // If goodIsTrue==True, then that means
  // a good data point has a flag value of True (usually for 
  // visibilities, good is False and for images good is True)
  // If extrapolate==False, then xout points outside the range of xin
  // will always be marked as flagged.
  // Currently only linear interpolation method is implemented.
  // TODO: add support for nearest neiborhood, cubic, and cubic spline. 
  static void interpolatey(Cube<Range>& yout,
			  Cube<Bool>& youtFlags,
                          const Vector<Domain>& xout,
                          const Vector<Domain>& xin,
                          const Cube<Range>& yin,
			  const Cube<Bool>& yinFlags,
                          Int method,
                          Bool goodIsTrue=False,
			  Bool extrapolate=False);

private:
  // Interpolate the y-vectors of length ny from x values xin to xout.
  static void interpolatePtr(PtrBlock<Range*>& yout, 
			     Int ny, 
			     const Vector<Domain>& xout, 
			     const Vector<Domain>& xin,
			     const PtrBlock<const Range*>& yin, 
			     Int method);

  // Interpolate the y-vectors of length ny from x values xin to xout.
  // Take flagging into account
  static void interpolatePtr(PtrBlock<Range*>& yout, 
			     PtrBlock<Bool*>& youtFlags,
			     Int ny, 
			     const Vector<Domain>& xout, 
			     const Vector<Domain>& xin,
			     const PtrBlock<const Range*>& yin, 
			     const PtrBlock<const Bool*>& yinFlags, 
			     Int method, Bool goodIsTrue,
			     Bool extrapolate);

  // Interpolate along yaxis 
  static void interpolateyPtr(PtrBlock<Range*>& yout,
                             Int na,
                             Int nb,
                             Int nc,
                             const Vector<Domain>& xout,
                             const Vector<Domain>& xin,
                             const PtrBlock<const Range*>& yin,
                             Int method);

  // Take flagging into account
  static void interpolateyPtr(PtrBlock<Range*>& yout, 
			     PtrBlock<Bool*>& youtFlags,
                             Int na,
			     Int nb, 
			     Int nc, 
			     const Vector<Domain>& xout, 
			     const Vector<Domain>& xin,
			     const PtrBlock<const Range*>& yin, 
			     const PtrBlock<const Bool*>& yinFlags, 
			     Int method, Bool goodIsTrue,
			     Bool extrapolate);

  // Interpolate the y-vectors of length ny from x values xin to xout
  // using polynomial interpolation with specified order.
  static void polynomialInterpolation(PtrBlock<Range*>& yout, 
				      Int ny, 
				      const Vector<Domain>& xout, 
				      const Vector<Domain>& xin,
				      const PtrBlock<const Range*>& yin, 
				      Int order);

};



} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <scimath/Mathematics/InterpolateArray1D.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
