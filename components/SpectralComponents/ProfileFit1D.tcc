//# ProfileFit1D.cc: Class to fit Spectral components to vectors
//# Copyright (C) 2004
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
//#   $Id$

#include <components/SpectralComponents/ProfileFit1D.h>

#include <casa/Arrays/ArrayLogical.h>
#include <casa/Exceptions/Error.h>
#include <components/SpectralComponents/SpectralEstimate.h>
#include <components/SpectralComponents/SpectralElement.h>
#include <casa/Utilities/DataType.h>
#include <casa/Utilities/Assert.h>

namespace casa {

template <class T> 
ProfileFit1D<T>::ProfileFit1D()
{
   checkType();
}

template <class T> 
ProfileFit1D<T>::ProfileFit1D(const ProfileFit1D& other)
{
   checkType();
   copy(other);
}

template <class T> 
ProfileFit1D<T>& ProfileFit1D<T>::operator=(const ProfileFit1D& other)
{
  if (this != &other) {
     copy(other);
  }
  return *this;
}

template <class T> 
ProfileFit1D<T>::~ProfileFit1D()
{;}

template <class T> 
Bool ProfileFit1D<T>::setData (const Vector<Double>& x, const Vector<T>& y, 
                               const Vector<Bool>& mask, const Vector<Double>& weight)
{
   if (x.nelements()==0) {
      itsError = "The X vector must have some elements";
      return False;
   }
   const uInt n = x.nelements();
//
   if (y.nelements() != n) {
      itsError = "The Y vector must have the same number of elements as the X vector";
      return False;
   }
//
   if (weight.nelements() != n && weight.nelements()!=0) {
      itsError = "The weights vector must have the same number of elements as the X vector";
      return False;
   }
//
   if (mask.nelements() != n && mask.nelements() != 0) {
      itsError = "The mask vector must have the same number of elements (or zero) as the data";
      return False;
   }
//
   itsX.resize(n);
   itsY.resize(n);
   itsDataMask.resize(n);
//
   itsX = x;
   itsY = y;
//
   if (weight.nelements()==0) {
      itsWeight.resize(0);
   } else {
      itsWeight.resize(n);
      itsWeight = weight;
   }
//
   if (mask.nelements()==0) {
      itsDataMask = True;
   } else {
      itsDataMask = mask;
   }
   return True;
}


template <class T> 
Bool ProfileFit1D<T>::setData (const Vector<Double>& x, const Vector<T>& y, 
                               const Vector<Bool>& mask)
{
   Vector<Double> weight;
   return setData (x, y, mask, weight);
}

template <class T> 
Bool ProfileFit1D<T>::setData (const Vector<Double>& x, const Vector<T>& y)
{
   Vector<Bool> mask(x.nelements(), True);
   return setData (x, y, mask);
}


template <class T> 
void ProfileFit1D<T>::setElements (const SpectralList& list)
{
   itsList = list;
}

template <class T> 
Bool ProfileFit1D<T>::setGaussianElements (uInt nGauss)
{
   if (nGauss==0) {
      itsError = "You must specify some Gaussian components";
      return False;
   }
//
   if (itsY.nelements()==0) {
      itsError = "You must call function setData to set some data first";
      return False;
   }

// Clear list

   itsList.clear();

// Make estimate for Gaussians.  

   SpectralEstimate estimator (nGauss);
   estimator.setQ(5);
   SpectralList listGauss = estimator.estimate (itsX, itsY);    // Ignores masked data
   itsList.add (listGauss);
   return True;
}

template <class T> 
void ProfileFit1D<T>::addElements (const SpectralList& list)
{
   itsList.add (list);
}

template <class T> 
void ProfileFit1D<T>::addElement (const SpectralElement& el)
{
   itsList.add (el);
}


template <class T> 
void ProfileFit1D<T>::clearList ()
{
   itsList.clear();
}


template <class T> 
Bool ProfileFit1D<T>::setRangeMask (const Vector<uInt>& start,
                                    const Vector<uInt>& end,
                                    Bool insideIsGood)
{
   AlwaysAssert(start.nelements()==end.nelements(), AipsError);
   if (itsX.nelements()==0) {
      itsError = "You must call function setData to set some data first";
      return False;
   }
//
   const uInt n = itsX.nelements();
   itsRangeMask.resize(n);
   Bool value = !insideIsGood;
   itsRangeMask = value;
//
   for (uInt i=0; i<start.nelements(); i++) {
      if (start[i] > end[i]) {
         itsError = "The start index must be < the end index";
         return False;
      }
      if (start[i]>=n) {
         itsError = "The start index must be in the range 0->nElements-1";
         return False;
      }
      if (end[i]>=n) {
         itsError = "The end index must be in the range 0->nElements-1";
         return False;
      }
//
      for (uInt j=start[i]; j<end[i]+1; j++) {
         itsRangeMask[j] = !value;
      }
   }
   return True;
}



template <class T> 
Bool ProfileFit1D<T>::setRangeMask (const Vector<T>& start,
                                    const Vector<T>& end,
                                    Bool insideIsGood)
{
   if (start.nelements()!=end.nelements()) {
      itsError = "Start and end vectors must be the same length";
      return False;
   }
   if (itsX.nelements()==0) {
      itsError = "You must call function setData to set some data first";
      return False;
   }
//
   const uInt n = itsX.nelements();
   itsRangeMask.resize(n);
   Bool value = !insideIsGood;
   itsRangeMask = value;
//
   Vector<uInt> startIndex(start.nelements());
   Vector<uInt> endIndex(end.nelements());
   
   for (uInt i=0; i<start.nelements(); i++) {
      if (start[i] > end[i]) {
         itsError = "The start range must be < the end range";
         return False;
      }
      if (start[i]<itsX[0] || start[i]>itsX[n-1]) {
         itsError = "The start range must be in the X-range of the data";
         return False;
      }
      if (end[i]<itsX[0] || end[i]>itsX[n-1]) {
         itsError = "The end range must be in the X-range of the data";
         return False;
      }

// Find the indices for this range

      Bool doneStart = False;
      Bool doneEnd = False;
      for (uInt j=0; j<n; j++) {
         if (!doneStart && itsX[j] >= start[i]) {
            startIndex[i] = j;
            doneStart = True;
         }
         if (!doneEnd && itsX[j] >= end[i]) {
            endIndex[i] = j;
            doneEnd = True;
         }
         if (!doneEnd) endIndex[i] = n-1;
      }
   }
//
   return setRangeMask (startIndex, endIndex);
}


template <class T> 
Bool ProfileFit1D<T>::fit ()
{
   if (itsX.nelements()==0) {
      itsError = "You must call function setData to set some data first";
      return False;
   }
   if (itsList.nelements()==0) {
      itsError = "You must call function setElements to set some fit components first";
      return False;
   }

// Set list in fitter
   itsFitter.clear();
   itsFitter.addFitElement (itsList);
// Do the fit with the total mask

   Bool converged(False);
   if (itsWeight.nelements()==0) {
      converged = itsFitter.fit (itsY, itsX, makeTotalMask());
   } else {
      converged = itsFitter.fit (itsWeight, itsY, itsX, makeTotalMask());
   }
   return converged;
}

template <class T> 
const SpectralList& ProfileFit1D<T>::getList (Bool fit) const
{
   if (fit) {
      return itsFitter.list();
   } else {
      return itsList;
   }
}


template <class T>   
Vector<T> ProfileFit1D<T>::getEstimate (Int which) const
{
   Vector<T> tmp;
   if (itsX.nelements()==0) {
      itsError = "You must call function setData to set some data first";
      return tmp;
   }
//
   if (which<0) {
      itsList.evaluate(tmp, itsX);
   } else {
      SpectralList list = getSubsetList (itsList, which);
      list.evaluate(tmp, itsX);
   }
//
   return tmp;
}

template <class T> 
Vector<T> ProfileFit1D<T>::getFit (Int which) const
{
   Vector<T> tmp;
   if (itsX.nelements()==0) {
      itsError = "You must call function setData to set some data first";
      return tmp;
   }
//
   const SpectralList& fitList = itsFitter.list();
   if (fitList.nelements()==0) {
      itsError = "You must call function fit first";
      return tmp;
   }
//

   if (which<0) {
      fitList.evaluate(tmp, itsX);
   } else {
      SpectralList list = getSubsetList (fitList, which);
      list.evaluate(tmp, itsX);
   }
//
   return tmp;
}

template <class T> 
Vector<T> ProfileFit1D<T>::getResidual (Int which, Bool fit)  const
{
   Vector<T> tmp;
   if (itsX.nelements()==0) {
      itsError = "You must call function setData to set some data first";
      return tmp;
   }
//
   SpectralList list;
   if (fit) {
      list = itsFitter.list();
      if (list.nelements()==0) {
         throw (AipsError("You must call function fit first"));
      }
   } else {
      list = itsList;
   }
//
   tmp = itsY;
   if (which<0) {
      list.residual(tmp, itsX);
   } else {
      SpectralList subList = getSubsetList (list, which);
      subList.residual(tmp, itsX);
   }
//
   return tmp;
}


// Private functions


template <class T> 
SpectralList ProfileFit1D<T>::getSubsetList (const SpectralList& list, Int which)  const
{
	const Int n = list.nelements();
	if (which+1 > n) {
		throw AipsError("Illegal spectral element index");
	}
	SpectralList listOut;
	listOut.add(*list[which]);
	return listOut;
}

template <class T> 
Vector<Bool> ProfileFit1D<T>::makeTotalMask () const
{
   Vector<Bool> mask;
   if (itsRangeMask.nelements()==0) {
      mask = itsDataMask;
   } else {
      mask = itsDataMask && itsRangeMask;
   }
   return mask;
}

template <class T> 
void ProfileFit1D<T>::copy(const ProfileFit1D& other)
{
  itsX.resize(0);
  itsX = other.itsX;
//
  itsY.resize(0);
  itsY = other.itsY;
//
  itsWeight.resize(0);
  itsWeight = other.itsWeight;
//
  itsDataMask.resize(0);
  itsDataMask = other.itsDataMask;
//
  itsRangeMask.resize(0);
  itsRangeMask = other.itsRangeMask;
//
  itsList = other.itsList;
//
  itsFitter = other.itsFitter;
//
  itsError = other.itsError;
}

template <class T> 
void ProfileFit1D<T>::checkType() const
{
   T* p=0;
   AlwaysAssert(whatType(p)==TpDouble,AipsError);
}

} //#End casa namespace
