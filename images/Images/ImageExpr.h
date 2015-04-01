//# ImageExpr.h: contains expressions involving images
//# Copyright (C) 1994,1995,1996,1997,1999,2000,2001,2003
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

#ifndef IMAGES_IMAGEEXPR_H
#define IMAGES_IMAGEEXPR_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/images/Images/ImageInterface.h>
#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Quanta/Unit.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class IPosition;
class Slicer;
template <class T> class Array;
class LatticeNavigator;
template <class T> class LatticeIterInterface;
class String;


// <summary>
// Hold mathematical expressions involving ImageInterface objects
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="tImageExpr.cc">
// </reviewed>
//
// <prerequisite>
//   <li> LatticeExpr
//   <li> ImageInterface
// </prerequisite>
//
// <etymology>
//  This class holds a LatticeExpr object but inherits from
//  ImageInterface hence ImageExpr
// </etymology>
//
// <synopsis> 
//  An ImageExpr object holds a LatticeExpr object which can be used 
//  to evaluate mathematical expressions involving Lattices.  ImageExpr
//  exists so that direct manipulation of LatticeExpr objects by methods 
//  expecting an ImageInterface, rather than a Lattice can occur.  
//
//  The ImageExpr object is constructed from a LatticeExpr object, but
//  only if the latter has true Coordinates associated with it.
//  The ImageExpr object is not writable, so the ImageExpr object
//  functions like a read only ImageInterface.
// </synopsis> 
//
// <example>
// <srcblock>
//    PagedImage<Float> a("imageB");                // Open PagedImages
//    PagedImage<Float> b("imageB");
//
//    LatticeExprNode node(a+b);                    // Create ImageExpr
//    LatticeExpr<Float> lExpr(node);
//    ImageExpr<Float> iExpr(lExpr);
//
//    LogOrigin or("imageImpl", "main()", WHERE);   // Create statistics object
//    LogIO logger(or);
//    ImageStatistics<Float> stats(iExpr, logger);
//    Bool ok = stats.display();                              // Display statistics
//    
// </srcblock>
// The ImageExpr object is evaluated during the call to 
// <src>stats.dislay()</src>.  Previously, the expression tree 
//  has been constructed, but not evaluated.
// </example>
//
// <motivation>
// This enables one to evaluate expressions but not to have to write them
// out to an output image.
// </motivation>
//
// <todo asof="1998/02/09">
// </todo>


template <class T> class ImageExpr: public ImageInterface<T>
{
public: 
  // The default constructor
  ImageExpr();

  // Construct an ImageExpr from a LatticeExpr.
  // The expr given should be the original expression string.
  // The fileName argument is meant for ImageOpener.
  // The coordinates are taken from the expression, usually the first image.
  // An exception is thrown if the expression has no coordinates.
  ImageExpr(const LatticeExpr<T>& latticeExpr, const String& expr,
            const String& fileName = String());

  // Same as previous constructor, but the coordinates are taken from the
  // given LELImageCoord object.
  ImageExpr(const LatticeExpr<T>& latticeExpr,
            const String& expr, const String& fileName,
            const LELImageCoord& imCoord);

  // Copy constructor (reference semantics)
  ImageExpr(const ImageExpr<T>& other);

  // Destructor does nothing
  ~ImageExpr();

  // Assignment (reference semantics)
  ImageExpr<T>& operator=(const ImageExpr<T>& other);
  
  // Make a copy of the object (reference semantics).
  virtual ImageInterface<T>* cloneII() const;

  // Save the image in an AipsIO file with the given name.
  // It can be opened by ImageOpener::openExpr.
  virtual void save (const String& fileName) const;

  // Set the file name.
  void setFileName (const String& name)
    { fileName_p = name; }

  // Get the image type (returns name of derived class).
  virtual String imageType() const;

  // Has the object really a mask?
  virtual Bool isMasked() const;

  // Get the region used.
  virtual const LatticeRegion* getRegionPtr() const;

  // return the shape of the ImageExpr
  virtual IPosition shape() const;

  // Function which changes the shape of the ImageExpr.
  // Throws an exception as ImageExpr is not writable.
  virtual void resize(const TiledShape& newShape);

  // Do the actual get of the mask data.
  // The return value is always False, thus the buffer does not reference
  // another array.
  virtual Bool doGetMaskSlice (Array<Bool>& buffer, const Slicer& section);

  // Do the actual get of the data.
  virtual Bool doGetSlice (Array<T>& buffer, const Slicer& theSlice);

  // putSlice is not possible on an expression, so it throws an exception.
  virtual void doPutSlice (const Array<T>& sourceBuffer,
			   const IPosition& where,
			   const IPosition& stride);

  // If the object is persistent, the file name is given.
  // Otherwise it returns the expression string given in the constructor.
  virtual String name (Bool stripPath=False) const;
  
  // Check class invariants.
  virtual Bool ok() const;
  
  // These are the implementations of the LatticeIterator letters.
  // <note> not for public use </note>
  virtual LatticeIterInterface<T>* makeIter(
                                 const LatticeNavigator& navigator,
				 Bool useRef) const;

  // Returns False, as the ImageExpr is not writable.
  virtual Bool isWritable() const;

  // Is the lattice persistent and can it be loaded by other processes as well?
  virtual Bool isPersistent() const;

  // Help the user pick a cursor for most efficient access if they only want
  // pixel values and don't care about the order or dimension of the
  // cursor. 
  virtual IPosition doNiceCursorShape (uInt maxPixels) const;

  // Handle the (un)locking and syncing.
  // <group>
  virtual Bool lock (FileLocker::LockType, uInt nattempts);
  virtual void unlock();
  virtual Bool hasLock (FileLocker::LockType) const;
  virtual void resync();
  virtual void tempClose();
  virtual void reopen();
  // </group>

  // Get the lattice expression.
  const LatticeExpr<T>& expression() const
    { return latticeExpr_p; }


private:  
  LatticeExpr<T> latticeExpr_p;
  Unit unit_p;
  String exprString_p;
  mutable String fileName_p;
};




} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/images/Images/ImageExpr.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
