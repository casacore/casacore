//# ArraySampledFunctional.cc:  
//# Copyright (C) 1996,1997,2001
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

#ifndef SCIMATH_ARRAYSAMPLEDFUNCTIONAL_TCC
#define SCIMATH_ARRAYSAMPLEDFUNCTIONAL_TCC

#include <casacore/scimath/Functionals/ArraySampledFunctional.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> ArraySampledFunctional<T>::
ArraySampledFunctional()
  :theRefData(),
   theEnd(),
   theLastAxis(0),
   theNelements(0){
}

template<class T> ArraySampledFunctional<T>::
ArraySampledFunctional(const T & data) 
  :theRefData(data),
   theEnd(data.endPosition()),
   theLastAxis(0),
   theNelements(0)
{
  const uInt ndim = theEnd.nelements();
  for (uInt i = 0; i < ndim; i++)
    if (theEnd(i) > 0)
      theLastAxis = i;
  theNelements = theEnd(theLastAxis) + 1;
  theEnd(theLastAxis) = 0;
}

template<class T> ArraySampledFunctional<T>::
ArraySampledFunctional(ArraySampledFunctional<T> & other)
  : SampledFunctional<T>(other),
    theRefData(other.theRefData),
    theEnd(other.theEnd),
    theLastAxis(other.theLastAxis),
    theNelements(other.theNelements)
{
}

template<class T> ArraySampledFunctional<T> & ArraySampledFunctional<T>::
operator=(ArraySampledFunctional<T> &other){
  if (this != &other) {
    theRefData.reference(other.theRefData);
    theEnd = other.theEnd;
    theLastAxis = other.theLastAxis;
    theNelements = other.theNelements; 
  }
  return *this;
}

template<class T> T ArraySampledFunctional<T>::
operator()(const uInt & index) const {
  IPosition blc(theEnd.nelements(), 0);
  blc(theLastAxis) = index;
  IPosition trc(theEnd);
  trc(theLastAxis) = index;
  // Because refData is const I cannot use the operator() function as this
  // returns a reference. The way around this is to create a non const
  // pointer to the array, call the operator() function and then create a
  // copy (using the copy() function). 
  T *nonConstPtr = (T *) &theRefData;
  T theSubArray = nonConstPtr->operator()(blc, trc);
  return theSubArray.nonDegenerate(theLastAxis);
}

template<class T> const T ArraySampledFunctional<T>::
operator()(const uInt & index) {
  IPosition blc(theEnd.nelements(), 0);
  blc(theLastAxis) = index;
  theEnd(theLastAxis) = index;
  return theRefData(blc, theEnd).nonDegenerate(theLastAxis);
}

template<class T> uInt ArraySampledFunctional<T>::
nelements() const {
  return theNelements;
}

template<class T> ArraySampledFunctional<T>::
~ArraySampledFunctional() {
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 ArraySampledFunctional"
// End: 

} //# NAMESPACE CASACORE - END


#endif
