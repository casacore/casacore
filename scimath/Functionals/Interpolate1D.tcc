//# Interpolate1D.cc:  implements Interpolation in one dimension
//# Copyright (C) 1996,1997,2000,2002
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

#ifndef SCIMATH_INTERPOLATE1D_TCC
#define SCIMATH_INTERPOLATE1D_TCC

#include <casacore/scimath/Functionals/Interpolate1D.h>
#include <casacore/scimath/Functionals/SampledFunctional.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/BinarySearch.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class Domain, class Range> Interpolate1D<Domain, Range>::
Interpolate1D() {
}

template <class Domain, class Range> Interpolate1D<Domain, Range>::
Interpolate1D(const SampledFunctional<Domain> &x,
	      const SampledFunctional<Range> &y, 
	      const Bool sorted, 
	      const Bool uniq){
  setData(x, y, sorted, uniq);
}

// Do all the real construction work here
template <class Domain, class Range> void Interpolate1D<Domain, Range>::
setData(const SampledFunctional<Domain> &x,
	const SampledFunctional<Range> &y, 
	const Bool sorted, 
	const Bool uniq){
  nElements = x.nelements();

  // Set the default interpolation method
  if (nElements == 0){
    throw(AipsError("Interpolate1D::setData"
 		    " abcissa is of zero length"));
  }
  else if (nElements == 1)         
    curMethod = nearestNeighbour;   
  else
    curMethod = linear;
  
  // Now check that the ordinate has enough elements to correspond to all the
  // elements in the abcissa. 
  if (nElements != y.nelements()) 
    throw(AipsError("Interpolate1D::setData"
		    " ordinate is a different length from the abcissa"));
  
  // Sort the x and y data if required.
  xValues.resize(nElements);
  yValues.resize(nElements);
  if (sorted == False) {
    Vector<uInt> index;
    // I will copy the data to a block prior to sorting as the
    // genSort function cannot handle a SampledFunctional
    for (uInt j = 0; j < nElements; j++)
      xValues[j] = x(j);
    (void) genSort(index, xValues);
    Int idx; 
    for (uInt i = 0; i < nElements; i++) {
      idx = index(i);
      xValues[i] = x(idx);
      yValues[i] = y(idx);
    }
  }
  else {
    for (uInt k = 0; k < nElements; k++) {
      xValues[k] = x(k);
      yValues[k] = y(k);
    }
  }
  // Check that each x_value is unique. If it isn't then throw an
  // exception. This check can be turned off (by setting uniq=True), but the
  // user will then have to interpolate under the following restrictions:
  // 1/ spline interpolation cannot be used
  // 2/ linear and nearestNeighbour interpolation cannot be used when when the
  //    specified x value is within one data point of a repeated x value.
  // 3/ cubic interpolation cannot be used when when the specified x value is
  //    within two data points of a repeated x value.
  if (uniq == False) 
    for (uInt i=0; i < nElements-1; i++) {
      if (nearAbs(xValues[i], xValues[i+1])) {
	throw(AipsError("Interpolate1D::setData"
			" data has repeated x values"));
      }
    }
  //  I will not initialise the y2Values as they are not used unless the
  //  interpolation method is changed to spline. The y2Values are hence
  //  initialised by method.
}

template <class Domain, class Range> Interpolate1D<Domain, Range>::
Interpolate1D(const Interpolate1D<Domain, Range> & other):
  Function1D<Domain, Range> (other),
  curMethod(other.curMethod),
  nElements(other.nElements),
  xValues(other.xValues),
  yValues(other.yValues),
  y2Values(other.y2Values){
}

template <class Domain, class Range> 
Interpolate1D<Domain, Range> & Interpolate1D<Domain, Range>::
operator=(const Interpolate1D<Domain, Range> & other){
  if (this != &other){
    curMethod = other.curMethod;
    nElements = other.nElements;
    xValues = other.xValues;
    yValues = other.yValues;
    y2Values = other.y2Values;
  }
  return *this;
}

template <class Domain, class Range> Interpolate1D<Domain, Range>::
~Interpolate1D(){}

template <class Domain, class Range> 
Function<Domain, Range> *Interpolate1D<Domain, Range>::clone() const {
  return new Interpolate1D<Domain, Range>(*this);
}

template <class Domain, class Range> Range Interpolate1D<Domain, Range>::
polynomialInterpolation(const Domain x_req, uInt n, uInt offset) const {
  // A private function for doing polynomial interpolation
  // Based on Nevilles Algorithm (Numerical Recipies 2nd ed., Section 3.1)
  // x is the point we want to estimate, n is the number of points to use
  // in the interpolation, and offset controls which n points are used
  // (normally the nearest points)
  
  // copy the x, y data into the working arrays
  Block<Range> c(n), d(n);
  Block<Domain> x(n);
  uInt i;
  for (i = 0; i < n; i++){
    d[i] = c[i] = yValues[offset]; 
    x[i] = xValues[offset];
    offset++;
  }
  // Now do the interpolation using the rather opaque algorithm
  Range w, y;
  y = c[0];
  const Float one = 1;
  for (i = 1; i < n; i++){
    // Calculate new C's and D's for each interation 
    for (uInt j = 0; j < n-i; j++){
      if (nearAbs(x[j+i], x[j])) 
	throw(AipsError("Interpolate1D::polynomailInterpolation"
			" data has repeated x values"));
      w = (c[j+1] - d[j]) * (one / (x[j] - x[j+i]));
      c[j] = (x[j] - x_req) * w;
      d[j] = (x[j+i] - x_req) * w;
    }
    y += c[0];
  }
  return y;
}

template <class Domain, class Range> void Interpolate1D<Domain, Range>::
setMethod(uInt newMethod) {
  // Are we are switching to spline interpolation from something else?
  if (newMethod == spline && curMethod != spline){ // Calculate the y2Values
    y2Values.resize(nElements);
    // The y2Values are initialised here.  I need to calculate the second
    // derivates of the interpolating curve at each x_value.  As described
    // in Numerical Recipies 2nd Ed. Sec. 3.3, this is done by requiring
    // that the first derivative is continuous at each data point. This
    // leads to a set of equations that has a tridiagonal form that can be
    // solved using an order(N) algorithm.
    //
    // The first part of this solution is to do the Gaussian elimination so
    // that all the coefficients on the diagonal are one, and zero below the
    // diagonal.  Because the system is tridiagonal the only non-zero
    // coefficients are in the diagonal immediately above the main
    // one. These values are stored in y2Values temporarily. The temporary
    // storage t, is used to hold the right hand side.
    Block<Domain> t(nElements);
    Domain c;
    t[0] = 0; 
    y2Values[0] = t[0] * yValues[0]; // This obscure initialisation is to
                                     // ensure that if y2Values is a block
                                     // of arrays, it gets initialised to the
                                     // right size.
    y2Values[nElements-1] = y2Values[0];
    c = xValues[1] - xValues[0];
    if (nearAbs(xValues[1],  xValues[0])) 
      throw(AipsError("Interpolate1D::setMethod"
		      " data has repeated x values"));
    Domain a, b, delta;
    const Domain six = 6;
    const Float one = 1;
    Range r;
    uInt i;
    for (i = 1; i < nElements-1; i++){
      a = c;
      b = 2*(xValues[i+1] - xValues[i-1]);
      if (nearAbs(xValues[i+1],  xValues[i])) 
	throw(AipsError("Interpolate1D::setMethod"
			" data has repeated x values"));
      c = (xValues[i+1] - xValues[i]);
      r = (one/c) * (yValues[i+1] - yValues[i]) - 
	(one/a) * (yValues[i] - yValues[i-1]);
      delta = a * t[i-1];
      if (nearAbs(b, delta)) 
	throw(AipsError("Interpolate1D::setMethod"
			" trouble constructing second derivatives"));
      delta = b - delta;
      t[i] = c/delta;
      y2Values[i] = (one/delta)*(six*r - a*y2Values[i-1]);
    }
    // The second part of the solution is to do the back-substitution to
    // iteratively obtain the second derivatives.
    for (i = nElements-2; i > 1; i--){
      y2Values[i] = y2Values[i] - t[i]*y2Values[i+1];
    }
  }
  else if (curMethod == spline && newMethod != spline){ 
    // Delete the y2Values
    y2Values.resize(uInt(0));
  }
  curMethod = newMethod;
}

template <class Domain, class Range> Vector<Domain> Interpolate1D<Domain, Range>::
getX() const{
  Vector<Domain> x(xValues, nElements);
  return x;
}

template <class Domain, class Range> Vector<Range> Interpolate1D<Domain, Range>::
getY() const {
  Vector<Range> y(yValues, nElements);
  return y;
}

template <class Domain, class Range> Range Interpolate1D<Domain, Range>::
eval(typename Function1D<Domain, Range>::FunctionArg x) const {
  Bool found;
  uInt where = binarySearchBrackets(found, xValues, x[0], nElements);  
  Domain x1,x2;
  Range y1,y2;
  switch (curMethod) {
  case nearestNeighbour: // This does nearest neighbour interpolation
    if (where == nElements)
      return yValues[nElements-1];
    else if (where == 0) 
      return yValues[0];
    else if (xValues[where] - x[0] < .5)
      return yValues[where];
    else
      return yValues[where-1];
  case linear: // Linear interpolation is the default
    if (where == nElements)
      where--;
    else if (where == 0)
      where++;
    x2 = xValues[where]; y2 = yValues[where];
    where--;
    x1 = xValues[where]; y1 = yValues[where];
    if (nearAbs(x1, x2)) 
      throw(AipsError("Interpolate1D::operator()"
		      " data has repeated x values"));
    return y1 + ((x[0]-x1)/(x2-x1)) * (y2-y1);
  case cubic:// fit a cubic polynomial to the four nearest points
             // It is relatively simple to change this to any order polynomial
    if (where > 1 && where < nElements - 1)
      where  = where - 2;
    else if (where <= 1)
      where = 0;
    else
      where = nElements - 4;
    return polynomialInterpolation(x[0], (uInt) 4, where);
  case spline: // natural cubic splines
    {
      if (where == nElements)
	where--;
      else if (where == 0)
	where++;
      Domain dx, h, a, b;
      Range y1d, y2d;
      
      x2 = xValues[where]; y2 = yValues[where]; y2d = y2Values[where];
      where--;
      x1 = xValues[where]; y1 = yValues[where]; y1d = y2Values[where];
      if (nearAbs(x1, x2)) 
	throw(AipsError("Interpolate1D::operator()"
			" data has repeated x values"));
      dx = x2-x1;
      a = (x2-x[0])/dx; 
      b = 1-a;
      h = static_cast<Domain>(dx*dx/6.);
      return a*y1 + b*y2 + h*(a*a*a-a)*y1d + h*(b*b*b-b)*y2d;
    }
  default:
    throw AipsError("Interpolate1D::operator() - unknown type");
  }
  return y1;       // to make compiler happy
}

} //# NAMESPACE CASACORE - END


#endif
