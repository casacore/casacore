//# ImageExpr.cc: defines the ImageExpr class
//# Copyright (C) 1998
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
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <aips/Measures/Unit.h>

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
   const LatticeCoordinates latticeCoordinate = latticeExpr_p.coordinates();
   const LattCoord* pLattCoord = &(latticeCoordinate.coordinates());
   if (!pLattCoord->hasCoordinates() || pLattCoord->classname()!="ImageCoord") {
      throw (AipsError ("ImageExpr::constructor - the "
                        "LatticeExpr does not have coordinates"));
   }

// Cast to get at ImageCoord

   const ImageCoord* pImCoord = (ImageCoord*)pLattCoord;
   coords_p = pImCoord->coordinates();

}

template <class T>
ImageExpr<T>::ImageExpr (const ImageExpr<T>& other)
: MaskedImage<T>(other),
  latticeExpr_p (other.latticeExpr_p),
  pBool_p(other.pBool_p),
  name_p(other.name_p)
{}
 
template <class T>
ImageExpr<T>& ImageExpr<T>::operator=(const ImageExpr<T>& other)
// 
// Assignment. Uses reference semantics
//
{
   if (this != &other) {
      MaskedImage<T>::operator= (other);
      latticeExpr_p = other.latticeExpr_p;
      pBool_p = other.pBool_p;
      name_p = other.name_p;
   }
   return *this;
} 
 
template <class T>
ImageExpr<T>::~ImageExpr()
{}


template <class T>
Lattice<T>* ImageExpr<T>::clone() const
{
   return new ImageExpr (*this);
}   
template <class T>
MaskedImage<T>* ImageExpr<T>::cloneMI() const
{
   return new ImageExpr (*this);
}


template <class T>
Bool ImageExpr<T>::isMasked() const
{
   return latticeExpr_p.isMasked();
}

template <class T>
const LatticeRegion& ImageExpr<T>::region() const
{
   return latticeExpr_p.region();
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
   throw (AipsError ("ImageExpr::putSlice - is not possible as ImageExpr is not writable"));
}

template<class T> 
Bool ImageExpr<T>::setUnits(const Unit&)
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
String ImageExpr<T>::name(const Bool) const
{
   return name_p;
}

template <class T> 
Bool ImageExpr<T>::setCoordinateInfo(const CoordinateSystem&)
{
   throw(AipsError("ImageExpr<T>::setCoordinateInfo - ImageExpr is not writable"));
   return False;
}
     


template <class T>
LatticeCoordinates ImageExpr<T>::latticeCoordinates() const
{
   return latticeExpr_p.coordinates();
}

template<class T> 
const RecordInterface & ImageExpr<T>::miscInfo() const
{

// Don't have access to this yet

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

