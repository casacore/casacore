//# LELFunction.cc:  this defines templated classes in LELFunction.h
//# Copyright (C) 1997,1998,1999,2000
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

#include <trial/Lattices/LELFunction.h>
#include <trial/Lattices/LELFunctionEnums.h>
#include <trial/Lattices/LELScalar.h>
#include <trial/Lattices/LELArray.h>
#include <aips/Arrays/Slicer.h>
#include <trial/Lattices/LatticeExpr.h>
#include <aips/Lattices/LatticeIterator.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Utilities/GenSort.h>
#include <aips/Utilities/COWPtr.h>
#include <aips/Exceptions/Error.h> 
#include <aips/Mathematics/NumericTraits.h> 


// LELFunction1D
template <class T>
LELFunction1D<T>::LELFunction1D(const LELFunctionEnums::Function function,
				const CountedPtr<LELInterface<T> >& expr)
: function_p(function)
{
   switch (function_p) {
   case LELFunctionEnums::MIN1D :
   case LELFunctionEnums::MAX1D :
   case LELFunctionEnums::MEAN1D :
   case LELFunctionEnums::SUM :
      setAttr(LELAttribute());          // these result in a scalar
      break;
   case LELFunctionEnums::VALUE :
   {
      // The value gets unmasked.
      const LELAttribute& argAttr = expr->getAttribute();
      if (argAttr.isScalar()) {
	 setAttr (LELAttribute());
      } else {
	 setAttr (LELAttribute (False, argAttr.shape(), argAttr.tileShape(),
				argAttr.coordinates()));
      }
      break;
   }
   default:
      setAttr(expr->getAttribute());
   }
   // Fill this variable here, so an exception in setAttr does
   // not leave it undestructed.
   pExpr_p = expr;
#if defined(AIPS_TRACE)
   cout << "LELFunction1D: constructor" << endl;
#endif
}

template <class T>
LELFunction1D<T>::~LELFunction1D()
{
#if defined(AIPS_TRACE)
   cout << "LELFunction1D: destructor" << endl;
#endif
}


template <class T>
void LELFunction1D<T>::eval(LELArray<T>& result,
			    const Slicer& section) const
{
#if defined(AIPS_TRACE)
   cout << "LELFunction1D:: eval" << endl;
#endif

// Evaluate the expression
   pExpr_p->eval(result, section);

// Apply the 1D function
   switch(function_p) {
   case LELFunctionEnums::SIN :
   {
      Array<T> tmp(sin(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::SINH :
   {
      Array<T> tmp(sinh(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::COS :
   {
      Array<T> tmp(cos(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::COSH :
   {
      Array<T> tmp(cosh(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::EXP :
   {
      Array<T> tmp(exp(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::LOG :
   {
      Array<T> tmp(log(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::LOG10 :
   {
      Array<T> tmp(log10(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::SQRT :
   {
      Array<T> tmp(sqrt(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::VALUE :
   {
      result.removeMask();
      break;
   }
   default:
      throw(AipsError("LELFunction1D::eval - unknown function"));
   }

}

template <class T>
LELScalar<T> LELFunction1D<T>::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELFunction1D:: getScalar" << endl;
#endif

// Apply the 1D function

   switch(function_p) {
   case LELFunctionEnums::SIN :
      return sin(pExpr_p->getScalar().value());
   case LELFunctionEnums::SINH :
      return sinh(pExpr_p->getScalar().value());
   case LELFunctionEnums::COS :
      return cos(pExpr_p->getScalar().value());
   case LELFunctionEnums::COSH :
      return cosh(pExpr_p->getScalar().value());
   case LELFunctionEnums::EXP :
      return exp(pExpr_p->getScalar().value());
   case LELFunctionEnums::LOG :
      return log(pExpr_p->getScalar().value());
   case LELFunctionEnums::LOG10 :
      return log10(pExpr_p->getScalar().value());
   case LELFunctionEnums::SQRT :
      return sqrt(pExpr_p->getScalar().value());
   case LELFunctionEnums::VALUE :
      return pExpr_p->getScalar();
   case LELFunctionEnums::MIN1D :
   {
      if (pExpr_p->isScalar()) {
         return pExpr_p->getScalar();
      }
      Bool firstTime = True;
      T minVal = T();
      LatticeExpr<T> latExpr(pExpr_p);
      RO_LatticeIterator<T> iter(latExpr, latExpr.niceCursorShape());
      if (! latExpr.isMasked()) {
	 while (! iter.atEnd()) {
	    T minv = min(iter.cursor());
	    if (firstTime  ||  minv < minVal) {
	       firstTime = False;
	       minVal = minv;
	    }
	    iter++;
	 }
      } else {
////	 RO_LatticeIterator<T> maskiter(latExpr, latExpr.niceCursorShape());
	 Bool delMask, delData;
	 Array<Bool> mask;
	 while (! iter.atEnd()) {
	    const Array<T>& array = iter.cursor();
	    latExpr.getMaskSlice (mask, iter.position(), array.shape());
	    const Bool* maskPtr = mask.getStorage (delMask);
	    const T* dataPtr = array.getStorage (delData);
	    uInt n = array.nelements();
	    for (uInt i=0; i<n; i++) {
	       if (maskPtr[i]) {
		  if (firstTime  ||  dataPtr[i] < minVal) {
		     firstTime = False;
		     minVal = dataPtr[i];
		  }
	       }
	    }
	    mask.freeStorage (maskPtr, delMask);
	    array.freeStorage (dataPtr, delData);
	    iter++;
	 }
      }
      if (firstTime) {
	  return LELScalar<T>();           // no element found
      }
      return minVal;
   }
   case LELFunctionEnums::MAX1D :
   {
      if (pExpr_p->isScalar()) {
         return pExpr_p->getScalar();
      }
      Bool firstTime = True;
      T maxVal = T();
      LatticeExpr<T> latExpr(pExpr_p);
      RO_LatticeIterator<T> iter(latExpr, latExpr.niceCursorShape());
      if (! latExpr.isMasked()) {
	 while (! iter.atEnd()) {
	    T maxv = max(iter.cursor());
	    if (firstTime  ||  maxv > maxVal) {
	       firstTime = False;
	       maxVal = maxv;
	    }
	    iter++;
	 }
      } else {
////	 RO_LatticeIterator<T> maskiter(latExpr, latExpr.niceCursorShape());
	 Bool delMask, delData;
	 Array<Bool> mask;
	 while (! iter.atEnd()) {
	    const Array<T>& array = iter.cursor();
	    latExpr.getMaskSlice (mask, iter.position(), array.shape());
	    const Bool* maskPtr = mask.getStorage (delMask);
	    const T* dataPtr = array.getStorage (delData);
	    uInt n = array.nelements();
	    for (uInt i=0; i<n; i++) {
	       if (maskPtr[i]) {
		  if (firstTime  ||  dataPtr[i] > maxVal) {
		     firstTime = False;
		     maxVal = dataPtr[i];
		  }
	       }
	    }
	    mask.freeStorage (maskPtr, delMask);
	    array.freeStorage (dataPtr, delData);
	    iter++;
	 }
      }
      if (firstTime) {
	  return LELScalar<T>();           // no element found
      }
      return maxVal;
   }
   case LELFunctionEnums::MEAN1D :
   {
      if (pExpr_p->isScalar()) {
         return pExpr_p->getScalar();
      }
      NumericTraits<T>::PrecisionType sumVal = 0;
      Int nrVal = 0;
      LatticeExpr<T> latExpr(pExpr_p);
      RO_LatticeIterator<T> iter(latExpr, latExpr.niceCursorShape());
      Bool delData, delMask;

// Do the sum ourselves to avoid round off

      if (! latExpr.isMasked()) {
	 while (! iter.atEnd()) {
	    const Array<T>& array = iter.cursor();
	    const T* dataPtr = array.getStorage(delData);
	    uInt n = array.nelements();
	    for (uInt i=0; i<n; i++) {
		sumVal += dataPtr[i];
	    }
	    array.freeStorage(dataPtr, delData);
	    nrVal += n;
	    iter++;
	 }
      } else {
////	 RO_LatticeIterator<T> maskiter(latExpr, latExpr.niceCursorShape());
	 Array<Bool> mask;
	 while (! iter.atEnd()) {
	    const Array<T>& array = iter.cursor();
	    latExpr.getMaskSlice (mask, iter.position(), array.shape());
	    const Bool* maskPtr = mask.getStorage (delMask);
	    const T* dataPtr = array.getStorage (delData);
	    uInt n = array.nelements();
	    for (uInt i=0; i<n; i++) {
	       if (maskPtr[i]) {
		  sumVal += dataPtr[i];
		  nrVal++;
	       }
	    }
	    mask.freeStorage (maskPtr, delMask);
	    array.freeStorage (dataPtr, delData);
	    iter++;
	 }
      }
      if (nrVal == 0) {
	  return LELScalar<T>();           // no element found
      }
      return T(sumVal / nrVal);
   }
   case LELFunctionEnums::SUM :
   {
      if (pExpr_p->isScalar()) {
         return pExpr_p->getScalar();
      }
      NumericTraits<T>::PrecisionType sumVal = 0;
      LatticeExpr<T> latExpr(pExpr_p);
      RO_LatticeIterator<T> iter(latExpr, latExpr.niceCursorShape());
      Bool delData, delMask;

// Do the sum ourselves to avoid round off

      if (! latExpr.isMasked()) {
	 while (! iter.atEnd()) {
	    const Array<T>& array = iter.cursor();
	    const T* dataPtr = array.getStorage(delData);
	    uInt n = array.nelements();
	    for (uInt i=0; i<n; i++) {
		sumVal += dataPtr[i];
	    }
	    array.freeStorage(dataPtr, delData);
	    iter++;
	 }
      } else {
////	 RO_LatticeIterator<T> maskiter(latExpr, latExpr.niceCursorShape());
	 Array<Bool> mask;
	 while (! iter.atEnd()) {
	    const Array<T>& array = iter.cursor();
	    latExpr.getMaskSlice (mask, iter.position(), array.shape());
	    const Bool* maskPtr = mask.getStorage (delMask);
	    const T* dataPtr = array.getStorage (delData);
	    uInt n = array.nelements();
	    for (uInt i=0; i<n; i++) {
	       if (maskPtr[i]) {
		  sumVal += dataPtr[i];
	       }
	    }
	    mask.freeStorage (maskPtr, delMask);
	    array.freeStorage (dataPtr, delData);
	    iter++;
	 }
      }
      return T(sumVal);
   }
   default:
      throw(AipsError("LELFunction1D::getScalar - unknown function"));
   }
   return pExpr_p->getScalar();         // to make compiler happy
}

template <class T>
Bool LELFunction1D<T>::prepareScalarExpr()
{
#if defined(AIPS_TRACE)
   cout << "LELFunction1D::prepare" << endl;
#endif

   return LELInterface<T>::replaceScalarExpr (pExpr_p);
}

template <class T>
String LELFunction1D<T>::className() const
{
   return String("LELFunction1D");
}


template<class T>
Bool LELFunction1D<T>::lock (FileLocker::LockType type, uInt nattempts)
{
  return pExpr_p->lock (type, nattempts);
}
template<class T>
void LELFunction1D<T>::unlock()
{
    pExpr_p->unlock();
}
template<class T>
Bool LELFunction1D<T>::hasLock (FileLocker::LockType type) const
{
    return pExpr_p->hasLock (type);
}
template<class T>
void LELFunction1D<T>::resync()
{
    pExpr_p->resync();
}




// LELFunctionReal1D
template <class T>
LELFunctionReal1D<T>::LELFunctionReal1D
                               (const LELFunctionEnums::Function function,
				const CountedPtr<LELInterface<T> >& exp)
: function_p(function)
{
   switch (function_p) {
   case LELFunctionEnums::MEDIAN1D :
      setAttr(LELAttribute());          // these result in a scalar
      break;
   default:
      setAttr(exp->getAttribute());
   }
   // Fill this variable here, so an exception in setAttr does
   // not leave it undestructed.
   pExpr_p = expr;
#if defined(AIPS_TRACE)
   cout << "LELFunctionReal1D: constructor" << endl;
#endif
}

template <class T>
LELFunctionReal1D<T>::~LELFunctionReal1D()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionReal1D: destructor" << endl;
#endif
}


template <class T>
void LELFunctionReal1D<T>::eval(LELArray<T>& result,
			    const Slicer& section) const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionReal1D:: eval" << endl;
#endif

// Evaluate the expression
   pExpr_p->eval(result, section);

// Apply the Real1D function
   switch(function_p) {
   case LELFunctionEnums::ASIN :
   {
      Array<T> tmp(asin(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::ACOS :
   {
      Array<T> tmp(acos(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::TAN :
   {
      Array<T> tmp(tan(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::TANH :
   {
      Array<T> tmp(tanh(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::ATAN :
   {
      Array<T> tmp(atan(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::ROUND :
   {
      Bool deleteIt;
      T* data = result.value().getStorage (deleteIt);
      uInt nr = result.value().nelements();
      for (uInt i=0; i<nr; i++) {
	 if (data[i] < 0) {
	    data[i] = ceil (data[i] - 0.5);
	 } else {
	    data[i] = floor (data[i] + 0.5);
	 }
      }
      result.value().putStorage (data, deleteIt);
      break;
   }
   case LELFunctionEnums::CEIL :
   {
      Array<T> tmp(ceil(result.value()));
      result.value().reference(tmp);
      break;
   }
   case LELFunctionEnums::FLOOR :
   {
      Array<T> tmp(floor(result.value()));
      result.value().reference(tmp);
      break;
   }
   default:
      throw(AipsError("LELFunctionReal1D::eval - unknown function"));
   }

}

template <class T>
LELScalar<T> LELFunctionReal1D<T>::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionReal1D:: getScalar" << endl;
#endif

// Apply the Real1D function

   switch(function_p) {
   case LELFunctionEnums::ASIN :
      return asin(pExpr_p->getScalar().value());
   case LELFunctionEnums::ACOS :
      return acos(pExpr_p->getScalar().value());
   case LELFunctionEnums::TAN :
      return tan(pExpr_p->getScalar().value());
   case LELFunctionEnums::TANH :
      return tanh(pExpr_p->getScalar().value());
   case LELFunctionEnums::ATAN :
      return atan(pExpr_p->getScalar().value());
   case LELFunctionEnums::ROUND :
   {
      T value = pExpr_p->getScalar().value();
      if (value < 0) {
	 return ceil(value - 0.5);
      }
      return floor (value + 0.5);
   }
   case LELFunctionEnums::CEIL :
      return ceil(pExpr_p->getScalar().value());
   case LELFunctionEnums::FLOOR :
      return floor(pExpr_p->getScalar().value());
   case LELFunctionEnums::MEDIAN1D :
   {
      if (pExpr_p->isScalar()) {
         return pExpr_p->getScalar();
      }
      return maskedMedian (LatticeExpr<T>(pExpr_p));
   }
   default:
      throw(AipsError("LELFunctionReal1D::getScalar - unknown function"));
   }
   return pExpr_p->getScalar();         // to make compiler happy
}

template <class T>
Bool LELFunctionReal1D<T>::prepareScalarExpr()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionReal1D::prepare" << endl;
#endif

   if (! pExpr_p.null()) {
      return LELInterface<T>::replaceScalarExpr (pExpr_p);
   }
   return False;
}

template <class T>
String LELFunctionReal1D<T>::className() const
{
   return String("LELFunctionReal1D");
}



template<class T>
Bool LELFunctionReal1D<T>::lock (FileLocker::LockType type, uInt nattempts)
{
  return pExpr_p->lock (type, nattempts);
}
template<class T>
void LELFunctionReal1D<T>::unlock()
{
    pExpr_p->unlock();
}
template<class T>
Bool LELFunctionReal1D<T>::hasLock (FileLocker::LockType type) const
{
    return pExpr_p->hasLock (type);
}
template<class T>
void LELFunctionReal1D<T>::resync()
{
    pExpr_p->resync();
}


template <class T>
LELScalar<T> LELFunctionReal1D<T>::unmaskedMedian (const Lattice<T>& lattice,
						   uInt smallSize)
{
  // Determine the number of elements in the lattice.
  // If small enough, we read them all and do it in memory.
  uInt ntodo = lattice.shape().product();
  if (ntodo == 0) {
    return LELScalar<T>();
  }
  if (ntodo <= smallSize) {
    return median (lattice.get());
  }
  // Bad luck. We have to do some more work.
  // Do a first binning while determining min/max at the same time.
  // Hopefully the start and end values make some sense.
  T stv = -5000;
  T endv = 5000;
  T minv = 0;
  T maxv = 0;
  Bool firstTime = True;
  const uInt nbins = 10000;
  // Make the block 1 element larger, because possible roundoff errors
  // could result in a binnr just beyond the end.
  Block<uInt> hist(nbins+1, 0);
  RO_LatticeIterator<T> iter(lattice, lattice.niceCursorShape());
  while (! iter.atEnd()) {
    Bool delData;
    const Array<T>& array = iter.cursor();
    const T* dataPtr = array.getStorage (delData);
    uInt n = array.nelements();
    if (firstTime) {
      firstTime = False;
      minv = dataPtr[0];
      maxv = dataPtr[0];
    }
    for (uInt i=0; i<n; i++) {
      if (dataPtr[i] <= minv) {
	minv = dataPtr[i];
      } else if (dataPtr[i] >= maxv) {
	maxv = dataPtr[i];
      }
      Int bin = Int(dataPtr[i] - stv);
      if (bin < 0) {
	hist[0]++;
      } else if (bin >= Int(nbins)) {
	hist[nbins-1]++;
      } else {
	hist[bin]++;
      }
    }
    array.freeStorage (dataPtr, delData);
    iter++;
  }
  // The index of the median in the lattice is the middle one.
  // In case of an even nr of elements, it is the first one of the
  // two middle ones.
  T step = 1;
  uInt medianInx = (ntodo-1)/2;
  uInt medianBin;

  // Iterate until the bin containing the median does not
  // contain too many values anymore.

  while (True) {
    // First determine which bin contains the median.
    // If the bin is not found, it means that there were rounding problems.
    // In that case return the end of the interval.
    medianBin = 0;
    uInt ndone = 0;
    while (ndone <= medianInx) {
      if (medianBin == nbins) {
	return endv;
      }
      ndone += hist[medianBin++];
    }
    medianBin--;
    ntodo = hist[medianBin];
    ndone -= ntodo;
    // Now medianBin is the bin containing the median.
    // Determine the index of the median in this bin.
    // Use the boundaries of this bin as the new start/end values.
    medianInx -= ndone;
    if (medianBin == 0) {
      stv = minv;
    } else {
      stv += medianBin * step;
    }
    if (medianBin == nbins-1) {
      endv = maxv;
    } else {
      endv = stv + step;
    }
    minv = stv;
    maxv = endv;
    // If only a 'few' more points to do, stop making histograms.
    // Otherwise histogram the median bin with a much smaller bin size.
    if (ntodo <= smallSize) {
      break;
    }
    hist = 0;
    step = (endv - stv) / nbins;
    uInt nfound = 0;
    iter.reset();
    while (! iter.atEnd()  &&  nfound<ntodo) {
      const Array<T>& array = iter.cursor();
      Bool delData;
      const T* dataPtr = array.getStorage (delData);
      uInt n = array.nelements();
      for (uInt i=0; i<n; i++) {
	if (dataPtr[i] >= stv  &&  dataPtr[i] < endv) {
	  Int bin = Int((dataPtr[i] - stv) / step);
	  // Due to rounding the bin number might get too high.
	  // However, the block has 1 element extra, so it is no problem.
	  hist[bin]++;
	  nfound++;
	}
      }
      array.freeStorage (dataPtr, delData);
      iter++;
    }
    // In principle the last bin should be empty, but roundoff errors
    // might have put a few in there. So add them to previous one.
    hist[nbins-1] += hist[nbins];
  }
  // There are only a 'few' points left.
  // So read them all in and determine the medianInx'th-largest.
  // Again, due to rounding we might find a few elements more or less.
  // So take care that the receiving block is not exceeded and that
  // the number of elements found are used in kthLargest.
  // Besides, it makes sense to stop the iteration when we found all
  // elements. It may save a few reads from the lattice.
  Block<T> tmp(ntodo);
  T* tmpPtr = tmp.storage();
  uInt nfound = 0;
  iter.reset();
  while (! iter.atEnd()  &&  nfound<ntodo) {
    const Array<T>& array = iter.cursor();
    Bool delData;
    const T* dataPtr = array.getStorage (delData);
    uInt n = array.nelements();
    for (uInt i=0; nfound<ntodo && i<n; i++) {
      if (dataPtr[i] >= stv  &&  dataPtr[i] < endv) {
	tmpPtr[nfound++] = dataPtr[i];
      }
    }
    array.freeStorage (dataPtr, delData);
    iter++;
  }
  // By rounding it is possible that not enough elements were found.
  // In that case return the middle of the (very small) interval.
  if (medianInx >= nfound) {
    return (stv+endv)/2;
  }
  return GenSort<T>::kthLargest (tmp.storage(), nfound, medianInx);
}

template <class T>
LELScalar<T> LELFunctionReal1D<T>::maskedMedian
                       (const MaskedLattice<T>& lattice, uInt smallSize)
{
  // If unmasked, a simpler way can be used.
  if (! lattice.isMasked()) {
    return unmaskedMedian (lattice);
  }
  // Determine the number of elements in the lattice.
  // If small enough, we read them all and do it in memory.
  uInt ntodo = lattice.shape().product();
  if (ntodo <= smallSize) {
    return smallMaskedMedian (lattice);
  }
  ntodo = 0;
  // Bad luck. We have to do some more work.
  // Do a first binning while determining min/max at the same time.
  // Hopefully the start and end values make some sense.
  T stv = -5000;
  T endv = 5000;
  T minv = 0;
  T maxv = 0;
  Bool firstTime = True;
  const uInt nbins = 10000;
  // Make the block 1 element larger, because possible roundoff errors
  // could result in a binnr just beyond the end.
  Block<uInt> hist(nbins+1, 0);
  COWPtr<Array<Bool> > mask;
  RO_LatticeIterator<T> iter(lattice, lattice.niceCursorShape());
  while (! iter.atEnd()) {
    Bool delData, delMask;
    const Array<T>& array = iter.cursor();
    lattice.getMaskSlice (mask, iter.position(), array.shape());
    const Bool* maskPtr = mask->getStorage (delMask);
    const T* dataPtr = array.getStorage (delData);
    uInt n = array.nelements();
    for (uInt i=0; i<n; i++) {
      if (maskPtr[i]) {
	ntodo++;
	if (firstTime) {
	  firstTime = False;
	  minv = dataPtr[0];
	  maxv = dataPtr[0];
	} else {
	  if (dataPtr[i] <= minv) {
	    minv = dataPtr[i];
	  } else if (dataPtr[i] >= maxv) {
	    maxv = dataPtr[i];
	  }
	}
	Int bin = Int(dataPtr[i] - stv);
	if (bin < 0) {
	  hist[0]++;
	} else if (bin >= Int(nbins)) {
	  hist[nbins-1]++;
	} else {
	  hist[bin]++;
	}
      }
    }
    array.freeStorage (dataPtr, delData);
    mask->freeStorage (maskPtr, delMask);
    iter++;
  }
  if (ntodo == 0) {
    return LELScalar<T>();
  }
  // The index of the median in the lattice is the middle one.
  // In case of an even nr of elements, it is the first one of the
  // two middle ones.
  T step = 1;
  uInt medianInx = (ntodo-1)/2;
  uInt medianBin;

  // Iterate until the bin containing the median does not
  // contain too many values anymore.

  while (True) {
    // First determine which bin contains the median.
    // If the bin is not found, it means that there were rounding problems.
    // In that case return the end of the interval.
    medianBin = 0;
    uInt ndone = 0;
    while (ndone <= medianInx) {
      if (medianBin == nbins) {
	return endv;
      }
      ndone += hist[medianBin++];
    }
    medianBin--;
    ntodo = hist[medianBin];
    ndone -= ntodo;
    // Now medianBin is the bin containing the median.
    // Determine the index of the median in this bin.
    // Use the boundaries of this bin as the new start/end values.
    medianInx -= ndone;
    if (medianBin == 0) {
      stv = minv;
    } else {
      stv += medianBin * step;
    }
    if (medianBin == nbins-1) {
      endv = maxv;
    } else {
      endv = stv + step;
    }
    minv = stv;
    maxv = endv;
    // If only a 'few' more points to do, stop making histograms.
    // Otherwise histogram the median bin with a much smaller bin size.
    if (ntodo <= smallSize) {
      break;
    }
    hist = 0;
    step = (endv - stv) / nbins;
    uInt nfound = 0;
    iter.reset();
    while (! iter.atEnd()  &&  nfound<ntodo) {
      Bool delData, delMask;
      const Array<T>& array = iter.cursor();
      lattice.getMaskSlice (mask, iter.position(), array.shape());
      const Bool* maskPtr = mask->getStorage (delMask);
      const T* dataPtr = array.getStorage (delData);
      uInt n = array.nelements();
      for (uInt i=0; i<n; i++) {
	if (maskPtr[i]  &&  dataPtr[i] >= stv  &&  dataPtr[i] < endv) {
	  Int bin = Int((dataPtr[i] - stv) / step);
	  // Due to rounding the bin number might get too high.
	  // However, the block has 1 element extra, so it is no problem.
	  hist[bin]++;
	  nfound++;
	}
      }
      array.freeStorage (dataPtr, delData);
      mask->freeStorage (maskPtr, delMask);
      iter++;
    }
    // In principle the last bin should be empty, but roundoff errors
    // might have put a few in there. So add them to previous one.
    hist[nbins-1] += hist[nbins];
  }
  // There are only a 'few' points left.
  // So read them all in and determine the medianInx'th-largest.
  // Again, due to rounding we might find a few elements more or less.
  // So take care that the receiving block is not exceeded and that
  // the number of elements found are used in kthLargest.
  // Besides, it makes sense to stop the iteration when we found all
  // elements. It may save a few reads from the lattice.
  Block<T> tmp(ntodo);
  T* tmpPtr = tmp.storage();
  uInt nfound = 0;
  iter.reset();
  while (! iter.atEnd()  &&  nfound<ntodo) {
    Bool delData, delMask;
    const Array<T>& array = iter.cursor();
    lattice.getMaskSlice (mask, iter.position(), array.shape());
    const Bool* maskPtr = mask->getStorage (delMask);
    const T* dataPtr = array.getStorage (delData);
    uInt n = array.nelements();
    for (uInt i=0; nfound<ntodo && i<n; i++) {
      if (maskPtr[i]  &&  dataPtr[i] >= stv  &&  dataPtr[i] < endv) {
	tmpPtr[nfound++] = dataPtr[i];
      }
    }
    array.freeStorage (dataPtr, delData);
    mask->freeStorage (maskPtr, delMask);
    iter++;
  }
  // By rounding it is possible that not enough elements were found.
  // In that case return the middle of the (very small) interval.
  if (medianInx >= nfound) {
    return (stv+endv)/2;
  }
  return GenSort<T>::kthLargest (tmp.storage(), nfound, medianInx);
}

template <class T>
LELScalar<T> LELFunctionReal1D<T>::smallMaskedMedian
                                          (const MaskedLattice<T>& lattice)
{
  // Make a buffer to hold all masked-on elements.
  // The number of values is not more than the number of elements in the
  // lattice, so make the buffer that long.
  uInt size = lattice.shape().product();
  Block<T> buffer(size);
  T* bufPtr = buffer.storage();
  // Iterate through the lattice and assemble all masked-on elements.
  COWPtr<Array<Bool> > mask;
  RO_LatticeIterator<T> iter(lattice, lattice.niceCursorShape());
  while (! iter.atEnd()) {
    Bool delData, delMask;
    const Array<T>& array = iter.cursor();
    lattice.getMaskSlice (mask, iter.position(), array.shape());
    const Bool* maskPtr = mask->getStorage (delMask);
    const T* dataPtr = array.getStorage (delData);
    uInt n = array.nelements();
    for (uInt i=0; i<n; i++) {
      if (maskPtr[i]) {
	*bufPtr++ = dataPtr[i];
      }
    }
    array.freeStorage (dataPtr, delData);
    mask->freeStorage (maskPtr, delMask);
    iter++;
  }
  uInt npts = bufPtr - buffer.storage();
  if (npts == 0) {
    return LELScalar<T>();
  }
  // Use median of an Array instead of kthLargest directly, because
  // for a small array with an even number of elements, median takes
  // the average of the 2 middle elements.
  return median (Array<T> (IPosition(1,npts), buffer.storage(), SHARE));
}


// LELFunctionND
template <class T>
LELFunctionND<T>::LELFunctionND(const LELFunctionEnums::Function function,
				const Block<LatticeExprNode>& exp)
: function_p(function)
{
   switch (function_p) {
   case LELFunctionEnums::IIF :
   {
      if (exp.nelements() != 3) {
         throw (AipsError ("LELFunctionND - "
			   "function IIF should have 3 arguments"));
      }
//# The 1st argument must be Bool, the 2nd and 3rd must be T.
//# The arguments do not need to be lattices.

      Block<Int> argType(3);
      argType[0] = TpBool;
      argType[1] = whatType(static_cast<T*>(0));
      argType[2] = whatType(static_cast<T*>(0));
      setAttr (LatticeExprNode::checkArg (exp, argType, False));
      break;
   }
   case LELFunctionEnums::REPLACE :
   {
      if (exp.nelements() != 2) {
         throw (AipsError ("LELFunctionND - "
			   "function REPLACE should have 2 arguments"));
      }
//# The 1st and 2nd argument must be T.
//# The first arguments has to be a lattice.

      if (exp[0].isScalar()) {
	 throw (AipsError ("LELFunctionND - "
			   "first argument of function REPLACE cannot be "
			   " a scalar"));
      }
      Block<Int> argType(2);
      argType[0] = whatType(static_cast<T*>(0));
      argType[1] = whatType(static_cast<T*>(0));
      LatticeExprNode::checkArg (exp, argType, False);
      setAttr (exp[0].getAttribute());
      break;
   }
   default:
      throw (AipsError ("LELFunctionND::constructor - unknown function"));
   }
   // Fill the node block here, so an exception does
   // not leave the nodes undestructed.
   arg_p = exp;
#if defined(AIPS_TRACE)
   cout << "LELFunctionND: constructor" << endl;
#endif
}

template <class T>
LELFunctionND<T>::~LELFunctionND()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionND: destructor" << endl;
#endif
}


template <class T>
void LELFunctionND<T>::eval(LELArray<T>& result,
			    const Slicer& section) const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionND:: eval" << endl;
#endif

   switch (function_p) {
   case LELFunctionEnums::IIF :
   {
//# Evaluation is not difficult, but there are many scalar/lattice
//# combinations.
//# The optional masks make life even much more difficult.

// If the condition is a scalar, the result is simply the 1st or 2nd operand.
// If the operand taken is a scalar, its mask is certainly true.
// (otherwise prepareScalarExpr would have tackled it).

      if (arg_p[0].isScalar()) {
	 T tmp;
	 Bool tmpb;
	 arg_p[0].eval (tmpb);
	 if (tmpb) {
	    if (arg_p[1].isScalar()) {
	       arg_p[1].eval (tmp);
	       result.value() = tmp;
	    } else {
	       arg_p[1].eval (result, section);
	    }
	 } else {
	    if (arg_p[2].isScalar()) {
	       arg_p[2].eval (tmp);
	       result.value() = tmp;
	    } else {
	       arg_p[2].eval (result, section);
	    }
	 }
      } else {

// So the condition is an array.
// The result might get a mask. That is the case if one of the operands
// is an invalid scalar or an array with mask,

	 LELArrayRef<Bool> tmpb(result.shape());
	 arg_p[0].evalRef (tmpb, section);
	 Bool deleteTmpb;
	 const Bool* tmpbData = tmpb.value().getStorage (deleteTmpb);
	 uInt n = tmpb.value().nelements();
	 Bool deleteRes, deleteMask;
	 T* resData = 0;
	 Bool* maskData = 0;
	 T tmp1, tmp2;
	 // The combination of left and right gets a mask if either
	 // of them has a mask.
	 Array<Bool> newMask;
	 Bool makeMask = ToBool (arg_p[1].isInvalidScalar()
                              || arg_p[1].isMasked()
			      || arg_p[2].isInvalidScalar()
			      || arg_p[2].isMasked());

// There are 4 different scalar/array combinations for 1st and 2nd operand.
// Each of them must handle the optional new mask.
// Because efficiency is needed, the test for a new mask is done
// outside the loop.

	 if (arg_p[1].isScalar()) {
	    arg_p[1].eval (tmp1);
	    Bool mask1 = ToBool (!arg_p[1].isInvalidScalar());
	    if (arg_p[2].isScalar()) {
		// Handle scalar,scalar case.
	       resData = result.value().getStorage (deleteRes);
	       arg_p[2].eval (tmp2);
	       Bool mask2 = ToBool (!arg_p[2].isInvalidScalar());
	       if (makeMask) {
		  newMask.resize (result.shape());
		  maskData = newMask.getStorage (deleteMask);
		  for (uInt i=0; i<n; i++) {
		     if (tmpbData[i]) {
		        resData[i] = tmp1;
			maskData[i] = mask1;
		     } else {
		        resData[i] = tmp2;
			maskData[i] = mask2;
		     }
		  }
	       } else {
		  for (uInt i=0; i<n; i++) {
		     if (tmpbData[i]) {
			resData[i] = tmp1;
		     } else {
			resData[i] = tmp2;
		     }
		  }
	       }
	    } else {
		// Handle scalar,array case.
	       arg_p[2].eval (result, section);
	       resData = result.value().getStorage (deleteRes);
	       if (makeMask) {
		  if (result.isMasked()) {
		     newMask.reference (result.mask());
		  } else {
		     newMask.resize (result.shape());
		     newMask = True;
		  }
		  maskData = newMask.getStorage (deleteMask);
		  for (uInt i=0; i<n; i++) {
		     if (tmpbData[i]) {
			resData[i] = tmp1;
			maskData[i] = mask1;
		     }
		  }
	       } else {
		  for (uInt i=0; i<n; i++) {
		     if (tmpbData[i]) {
			resData[i] = tmp1;
		     }
		  }
	       }
	    }

// The first operand is an array.

	 } else {
	    arg_p[1].eval (result, section);
	    resData = result.value().getStorage (deleteRes);
	    if (makeMask) {
	       if (result.isMasked()) {
		  newMask.reference (result.mask());
	       } else {
		  newMask.resize (result.shape());
		  newMask = True;
	       }
	    }
	    if (arg_p[2].isScalar()) {
		// Handle array,scalar case.
	       arg_p[2].eval (tmp2);
	       if (makeMask) {
		  Bool mask2 = ToBool (!arg_p[2].isInvalidScalar());
		  maskData = newMask.getStorage (deleteMask);
		  for (uInt i=0; i<n; i++) {
		     if (! tmpbData[i]) {
		        resData[i] = tmp2;
			maskData[i] = mask2;
		     }
		  }
	       } else {
		  for (uInt i=0; i<n; i++) {
		     if (! tmpbData[i]) {
		        resData[i] = tmp2;
		     }
		  }
	       }
	    } else {
		// Handle array,array case.
	       LELArrayRef<T> tmp(result.shape());
	       arg_p[2].evalRef (tmp, section);
	       Bool deleteTmp, deleteTmpMask;
	       const T* tmpData = tmp.value().getStorage (deleteTmp);
	       if (makeMask) {
		  maskData = newMask.getStorage (deleteMask);
		  if (tmp.isMasked()) {
		     const Bool* tmpMaskData = tmp.mask().getStorage
                                                               (deleteTmpMask);
		     for (uInt i=0; i<n; i++) {
			if (! tmpbData[i]) {
			   resData[i] = tmpData[i];
			   maskData[i] = tmpMaskData[i];
			}
		     }
		     tmp.mask().freeStorage (tmpMaskData, deleteTmpMask);
		  } else {
		     for (uInt i=0; i<n; i++) {
			if (! tmpbData[i]) {
			   resData[i] = tmpData[i];
			   maskData[i] = True;
			}
		     }
		  }
		  tmp.value().freeStorage (tmpData, deleteTmp);
	       } else {
		  for (uInt i=0; i<n; i++) {
		     if (! tmpbData[i]) {
			resData[i] = tmpData[i];
		     }
		  }
	       }
	    }
	 }
	 tmpb.value().freeStorage (tmpbData, deleteTmpb);
	 if (resData != 0) {
	    result.value().putStorage (resData, deleteRes);
	 }
	 if (maskData != 0) {
	    newMask.putStorage (maskData, deleteMask);
	 }
	 result.setMask (tmpb);
	 if (makeMask) {
	    result.combineMask (newMask);
	 }
      }
      break;
   }

   case LELFunctionEnums::REPLACE :
   {
      //# The first argument is always an array.
      //# Replacing is only needed if the first argument has a mask.
      arg_p[0].eval (result, section);
      if (arg_p[0].isMasked()) {
	 uInt n = result.value().nelements();
	 Bool deleteRes, deleteMask;
	 T* resData = result.value().getStorage (deleteRes);
	 const Bool* maskData = result.mask().getStorage (deleteMask);
	 //# Handle the scalar case. Use 0 for an invalid scalar.
	 if (arg_p[1].isScalar()) {
	    T tmp;
	    if (arg_p[1].isInvalidScalar()) {
	       tmp = 0;
	    } else {
	       arg_p[1].eval (tmp);
	    }
	    for (uInt i=0; i<n; i++) {
	       if (! maskData[i]) {
		  resData[i] = tmp;
	       }
	    }
	 } else {
	    LELArrayRef<T> tmp(result.shape());
	    arg_p[1].evalRef (tmp, section);
	    Bool deleteTmp;
	    const T* tmpData = tmp.value().getStorage (deleteTmp);
	    for (uInt i=0; i<n; i++) {
	       if (! maskData[i]) {
		  resData[i] = tmpData[i];
	       }
	    }
	    tmp.value().freeStorage (tmpData, deleteTmp);
	 }
	 result.value().putStorage (resData, deleteRes);
	 result.mask().freeStorage (maskData, deleteMask);
      }
      break;
   }
	 
   default:
      throw(AipsError("LELFunctionND::eval - unknown function"));
   }
}

template <class T>
LELScalar<T> LELFunctionND<T>::getScalar() const
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionND:: getScalar" << endl;
#endif

// Apply the ND function

   T tmp;
   switch(function_p) {
   case LELFunctionEnums::IIF :
   {
      Bool tmpb;
      arg_p[0].eval (tmpb);
      if (tmpb) {
	 arg_p[1].eval (tmp);
      } else {
	 arg_p[2].eval (tmp);
      }
      return tmp;
   }
   default:
      throw(AipsError("LELFunctionND::getScalar - unknown function"));
   }
   return tmp;                      // to make compiler happy
}

template <class T>
Bool LELFunctionND<T>::prepareScalarExpr()
{
#if defined(AIPS_TRACE)
   cout << "LELFunctionND::prepare" << endl;
#endif

   uInt i;
   for (i=0; i<arg_p.nelements(); i++) {
       Bool invalid = arg_p[i].replaceScalarExpr();
       if (invalid  &&  function_p != LELFunctionEnums::IIF
       &&  function_p != LELFunctionEnums::REPLACE) {
	  return True;
       }
   }
   // REPLACE is never invalid.
   if (function_p == LELFunctionEnums::REPLACE) {
      return False;
   }
   // IIF is invalid if:
   // - the condition is an invalid scalar.
   // - both operands are invalid scalars.
   // - condition is scalar and corresponding operand is invalid scalar.
   if (arg_p[0].isInvalidScalar()) {
      return True;
   }
   if (arg_p[1].isInvalidScalar() && arg_p[2].isInvalidScalar()) {
      return True;
   }
   if (arg_p[0].isScalar()) {
      i = (arg_p[0].getBool()  ?  1 : 2);
      if (arg_p[i].isInvalidScalar()) {
	 return True;
      }
   }  
   return False;
}

template <class T>
String LELFunctionND<T>::className() const
{
   return String("LELFunctionND");
}


template<class T>
Bool LELFunctionND<T>::lock (FileLocker::LockType type, uInt nattempts)
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    if (! arg_p[i].lock (type, nattempts)) {
      return False;
    }
  }
  return True;
}
template<class T>
void LELFunctionND<T>::unlock()
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    arg_p[i].unlock();
  }
}
template<class T>
Bool LELFunctionND<T>::hasLock (FileLocker::LockType type) const
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    if (! arg_p[i].hasLock (type)) {
      return False;
    }
  }
  return True;
}
template<class T>
void LELFunctionND<T>::resync()
{
  for (uInt i=0; i<arg_p.nelements(); i++) {
    arg_p[i].resync();
  }
}
