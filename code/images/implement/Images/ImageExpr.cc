//# ImageExpr.cc: defines the ImageExpr class
//# Copyright (C) 1998,1999,2000
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
#include <trial/Images/LELImageCoord.h>
#include <aips/Lattices/LatticeNavigator.h>
#include <aips/Lattices/LatticeIterator.h>
#include <aips/Lattices/LatticeIterInterface.h>

#include <aips/Arrays/Array.h>
#include <aips/Exceptions/Error.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Arrays/Slicer.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <aips/Quanta/Unit.h>

#include <iostream.h>


template <class T>
ImageExpr<T>::ImageExpr()
{} 
 
template <class T>
ImageExpr<T>::ImageExpr (const LatticeExpr<T>& latticeExpr,
			 const String& name)
: latticeExpr_p(latticeExpr)
{
  if (! name.empty()) {
    name_p = "Expression: " + name;
  }
   const LELCoordinates lelCoordinate = latticeExpr_p.lelCoordinates();
   const LELLattCoordBase* pLattCoord = &(lelCoordinate.coordinates());
   if (! pLattCoord->hasCoordinates()
   ||  pLattCoord->classname() != "LELImageCoord") {
      throw (AipsError ("ImageExpr::constructor - the "
                        "LatticeExpr does not have coordinates"));
   }
// Cast to get at LELImageCoord
   const LELImageCoord* pImCoord =
                         dynamic_cast<const LELImageCoord*>(pLattCoord);
   AlwaysAssert (pImCoord != 0, AipsError);
   setCoordsMember (pImCoord->coordinates());
}

template <class T>
ImageExpr<T>::ImageExpr (const ImageExpr<T>& other)
: ImageInterface<T>(other),
  latticeExpr_p (other.latticeExpr_p),
  unit_p        (other.unit_p),
  pBool_p       (other.pBool_p),
  name_p        (other.name_p)
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
      unit_p        = other.unit_p;
      pBool_p       = other.pBool_p;
      name_p        = other.name_p;
   }
   return *this;
} 
 
template <class T>
ImageExpr<T>::~ImageExpr()
{}


template <class T>
ImageInterface<T>* ImageExpr<T>::cloneII() const
{
   return new ImageExpr (*this);
}


template <class T>
Bool ImageExpr<T>::isMasked() const
{
   return latticeExpr_p.isMasked();
}

template <class T>
const LatticeRegion* ImageExpr<T>::getRegionPtr() const
{
   return latticeExpr_p.getRegionPtr();
}

template <class T>
IPosition ImageExpr<T>::shape() const  
{ 
   return latticeExpr_p.shape();
}

template <class T>
void ImageExpr<T>::resize(const TiledShape&)
{
   throw (AipsError ("ImageExpr::resize - an ImageExpr is not writable"));
}

template <class T>
Bool ImageExpr<T>::doGetSlice(Array<T>& buffer,
			      const Slicer& section)
{
   return latticeExpr_p.doGetSlice(buffer, section);
} 
   

template <class T>
void ImageExpr<T>::doPutSlice (const Array<T>&, const IPosition&,
			       const IPosition&)
{
   throw (AipsError ("ImageExpr::putSlice - "
		     "is not possible as ImageExpr is not writable"));
}

template<class T> 
Bool ImageExpr<T>::setUnits(const Unit& unit)
{  
   unit_p = unit;
   return True;
}
   
template<class T> Unit ImageExpr<T>::units() const
{  
  return unit_p;
}


template <class T> 
String ImageExpr<T>::name (Bool) const
{
   return name_p;
}


template<class T> 
const RecordInterface & ImageExpr<T>::miscInfo() const
{
  return rec_p;
}
 
template<class T> 
Bool ImageExpr<T>::setMiscInfo(const RecordInterface&)
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
IPosition ImageExpr<T>::doNiceCursorShape (uInt maxPixels) const
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
  return latticeExpr_p.makeIter(navigator);
}


template <class T>
Bool ImageExpr<T>::doGetMaskSlice (Array<Bool>& buffer, const Slicer& section)
{
   return latticeExpr_p.doGetMaskSlice (buffer, section);
}


template <class T>
Bool ImageExpr<T>::lock (FileLocker::LockType type, uInt nattempts)
{
   return latticeExpr_p.lock (type, nattempts);
}
template<class T>
void ImageExpr<T>::unlock()
{
  latticeExpr_p.unlock();
}
template<class T>
Bool ImageExpr<T>::hasLock (FileLocker::LockType type) const
{
  return latticeExpr_p.hasLock (type);
}
template<class T>
void ImageExpr<T>::resync()
{
  latticeExpr_p.resync();
}
template<class T>
void ImageExpr<T>::tempClose()
{
  latticeExpr_p.tempClose();
}
template<class T>
void ImageExpr<T>::reopen()
{
  latticeExpr_p.reopen();
}
