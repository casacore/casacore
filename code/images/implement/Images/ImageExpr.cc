//# ImageExpr.cc: defines the ImageExpr class
//# Copyright (C) 1994,1995,1996,1997,1998
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

#include <trial/Images/ImageExpr.h>
#include <trial/Images/ImageCoord.h>
#include <trial/Lattices/LatticeNavigator.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/LatticeIterInterface.h>

#include <aips/Arrays/Array.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Utilities/COWPtr.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <aips/Measures/Unit.h>

#include <iostream.h>


template <class T>
ImageExpr<T>::ImageExpr()
{} 
 
template <class T>
ImageExpr<T>::ImageExpr (const LatticeExpr<T>& latticeExpr)
: latticeExpr_p(latticeExpr)
{
   const LatticeCoordinates latticeCoordinate = latticeExpr_p.coordinates();
   const LattCoord* pLattCoord = &(latticeCoordinate.coordinates());
   if (!pLattCoord->hasCoordinates() || pLattCoord->classname()!="ImageCoord") {
      throw (AipsError ("ImageExpr::constructor - the "
                        "LatticeExpr does not have coordinates"));
   }

// Caste to get at ImageCoord

   const ImageCoord* pImCoord = (ImageCoord*)pLattCoord;
   coords_p = pImCoord->coordinates();
   throughmask_p = False;    // No masks yet

}

template <class T>
ImageExpr<T>::ImageExpr (const ImageExpr<T>& other)
: ImageInterface<T>(other),
  latticeExpr_p (other.latticeExpr_p),
  pBool_p(other.pBool_p)

//
// Copy constructor.  Uses reference semantics
//
{}
 
template <class T>
ImageExpr<T>& ImageExpr<T>::operator=(const ImageExpr<T>& other)
// 
// Assignment. Uses reference semantics
//
{
   if (this != &other) {
      ImageInterface<T>::operator= (other);
      latticeExpr_p = other.latticeExpr_p;
      pBool_p = other.pBool_p;
   }
   return *this;
} 
 
template <class T>
ImageExpr<T>::~ImageExpr()
//
// Destructor does nothing
//
{}


template <class T>
Lattice<T>* ImageExpr<T>::clone() const
//
// Return a copy of the ImageExpr object. Uses
// reference semantics.
{
   return new ImageExpr (*this);
}   

template <class T>
IPosition ImageExpr<T>::shape() const  
{ 
   return latticeExpr_p.shape();
}

template <class T>
void ImageExpr<T>::resize(const TiledShape &newShape)
{
   throw (AipsError ("ImageExpr::resize - an ImageExpr is not writable"));
}

template <class T>
Bool ImageExpr<T>::getSlice (COWPtr<Array<T> >& buffer,
                             const IPosition& start,
                             const IPosition& shape,
                             const IPosition& stride,
                             Bool removeDegenerateAxes) const
{  
   return latticeExpr_p.getSlice(buffer, start, shape, stride, 
                                 removeDegenerateAxes);
}  
   
template<class T>
Bool ImageExpr<T>::getSlice (COWPtr<Array<T> >& buffer,
                             const Slicer& section,
                             Bool removeDegenerateAxes) const
{
   return latticeExpr_p.getSlice(buffer, section, removeDegenerateAxes);
}

template <class T>
Bool ImageExpr<T>::getSlice(Array<T>& buffer,
                              const IPosition& start,
                              const IPosition& shape,
                              const IPosition& stride,
                              Bool removeDegenerateAxes)
{
   return latticeExpr_p.getSlice(buffer, start, shape, stride, 
                                 removeDegenerateAxes);
}


template <class T>
Bool ImageExpr<T>::getSlice(Array<T>& buffer,
                              const Slicer& section,
                              Bool removeDegenerateAxes)
{
   return latticeExpr_p.getSlice(buffer, section, removeDegenerateAxes);
} 
   

template <class T>
void ImageExpr<T>::putSlice (const Array<T>&, const IPosition&)
{
   throw (AipsError ("ImageExpr::putSlice - is not possible as ImageExpr is not writable"));
}   
template <class T>
void ImageExpr<T>::putSlice (const Array<T>&, const IPosition&,
                               const IPosition&)
{
   throw (AipsError ("ImageExpr::putSlice - is not possible as ImageExpr is not writable"));
}


template <class T> 
Lattice<Bool>& ImageExpr<T>::mask()
{
   throw(AipsError("ImageExpr<T>::mask - no mask in ImageExpr"));

// Shut compiler up

   return *pBool_p;
}

template <class T> 
const Lattice<Bool>& ImageExpr<T>::mask() const
{
   throw(AipsError("ImageExpr<T>::mask - ImageExpr is not writable"));

// Shut compiler up

   return *pBool_p;

}

template <class T> 
Bool ImageExpr<T>::isMasked() const 
{
  return False;
}

template<class T> 
Bool ImageExpr<T>::setUnits(const Unit &newUnits)
{  
   throw(AipsError("ImageExpr<T>::setUnits - ImageExpr is not writable"));
   return False;
}
   
template<class T> Unit ImageExpr<T>::units() const
{  
  Unit unit;
  return unit;
}


template <class T> 
String ImageExpr<T>::name(const Bool stripPath) const
{
   String string;
   return string;
}

template <class T> 
Bool ImageExpr<T>::setCoordinateInfo(const CoordinateSystem & coords)
{
   throw(AipsError("ImageExpr<T>::setCoordinateInfo - ImageExpr is not writable"));
   return False;
}
     


template <class T>
LatticeCoordinates ImageExpr<T>::latticeCoordinates() const
{
   return latticeExpr_p.coordinates();
}
 
template <class T> 
T ImageExpr<T>::getAt(const IPosition & where) const
{  

  const IPosition shape(where.nelements(),1);
  const IPosition stride(where.nelements(),1);
  const IPosition origin(where.nelements(),0);
  Array<T> arr(shape);
  COWPtr<Array<T> > ptr;
  latticeExpr_p.getSlice(ptr, where, shape, stride, False);
  arr.reference(ptr.rwRef());
  return arr(origin);

//  }
}
   
template <class T> 
void ImageExpr<T>::putAt(const T & value, const IPosition & where) 
{
   throw(AipsError("ImageExpr<T>::putAt - ImageExpr is not writable"));
}

template<class T> 
const RecordInterface & ImageExpr<T>::miscInfo() const
{

// Don't have access to this yet

  return rec_p;
}
 
template<class T> 
Bool ImageExpr<T>::setMiscInfo(const RecordInterface & newInfo)
{ 
   throw(AipsError("ImageExpr<T>::setMiscInfo - ImageExpr is not writable"));
   return False;
}
 
template <class T>
Bool ImageExpr<T>::isWritable() const
{  
   return False;
}

template <class T>
IPosition ImageExpr<T>::niceCursorShape (uInt maxPixels) const
{ 
   return latticeExpr_p.niceCursorShape(maxPixels);
} 


template <class T>
Bool ImageExpr<T>::ok() const
{
   return True;
}  


template <class T>
LatticeIterInterface<T>* ImageExpr<T>::makeIter(const LatticeNavigator &navigator) const
{
// There is no specific iterator for LatticeExpr so use
// a generic one

  return Lattice<T>::makeIter(navigator);
}
