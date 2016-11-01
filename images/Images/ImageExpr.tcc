//# ImageExpr.cc: defines the ImageExpr class
//# Copyright (C) 1998,1999,2000,2001,2003
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

#ifndef IMAGES_IMAGEEXPR_TCC
#define IMAGES_IMAGEEXPR_TCC

#include <casacore/images/Images/ImageExpr.h>
#include <casacore/images/Images/LELImageCoord.h>
#include <casacore/lattices/Lattices/LatticeNavigator.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeIterInterface.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Quanta/Unit.h>

#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T>
ImageExpr<T>::ImageExpr()
{} 
 
template <class T>
ImageExpr<T>::ImageExpr (const LatticeExpr<T>& latticeExpr,
			 const String& expr, const String& fileName)
  : latticeExpr_p(latticeExpr),
    fileName_p   (fileName)
{
  exprString_p = expr;
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
  this->setCoordsMember (pImCoord->coordinates());
  this->setImageInfoMember (pImCoord->imageInfo());
  this->setMiscInfoMember (pImCoord->miscInfo());
  this->setUnitMember (pImCoord->unit());
}

template <class T>
ImageExpr<T>::ImageExpr (const LatticeExpr<T>& latticeExpr,
			 const String& expr, const String& fileName,
                         const LELImageCoord& imCoord)

  : latticeExpr_p(latticeExpr),
    fileName_p   (fileName)
{
  exprString_p = expr;
  this->setCoordsMember (imCoord.coordinates());
  this->setImageInfoMember (imCoord.imageInfo());
  this->setMiscInfoMember (imCoord.miscInfo());
  this->setUnitMember (imCoord.unit());
}

template <class T>
ImageExpr<T>::ImageExpr (const ImageExpr<T>& other)
: ImageInterface<T>(other),
  latticeExpr_p (other.latticeExpr_p),
  unit_p        (other.unit_p),
  exprString_p  (other.exprString_p),
  fileName_p    (other.fileName_p)
{}
 
template <class T>
ImageExpr<T>& ImageExpr<T>::operator=(const ImageExpr<T>& other)
{
  if (this != &other) {
    ImageInterface<T>::operator= (other);
    latticeExpr_p = other.latticeExpr_p;
    unit_p        = other.unit_p;
    exprString_p  = other.exprString_p;
    fileName_p    = other.fileName_p;
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

template<class T>
void ImageExpr<T>::save (const String& fileName) const
{
  // Check that the expression is given.
  if (exprString_p.empty()) {
    throw AipsError ("ImageExpr cannot be made persistent, because "
                     "its expression string is empty");
  }
  // Create the AipsIO file.
  AipsIO aio(fileName, ByteIO::New);
  // Start with a general header (used by ImageOpener)
  // and the data type of the image.
  aio.putstart ("CompoundImage-Expr", 0);
  aio << Int(this->dataType());
  // Now save the expression string.
  aio.putstart ("ImageExpr", 1);
  aio << exprString_p;
  aio.putend();
  aio.putend();
  fileName_p = fileName;
}

template<class T>
String ImageExpr<T>::imageType() const
{
  return "ImageExpr";
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
Bool ImageExpr<T>::doGetSlice (Array<T>& buffer,
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

template <class T> 
String ImageExpr<T>::name (Bool stripPath) const
{
  if (fileName_p.empty()) {
    if (exprString_p.empty()) {
      return exprString_p;
    }
    return "Expression: " + exprString_p;
  }
  Path path(fileName_p);
  if (!stripPath) {
    return path.absoluteName();
  } 
  return path.baseName();
}

template <class T>
Bool ImageExpr<T>::isPersistent() const
{  
  return ! fileName_p.empty();
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
LatticeIterInterface<T>* ImageExpr<T>::makeIter
                                   (const LatticeNavigator &navigator,
				    Bool useRef) const
{
  return latticeExpr_p.makeIter(navigator, useRef);
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

} //# NAMESPACE CASACORE - END


#endif
