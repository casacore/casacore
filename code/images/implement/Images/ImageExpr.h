//# ImageExpr.h: contains expressions involving images
//# Copyright (C) 1994,1995,1996,1997
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

#if !defined(AIPS_IMAGEEXPR_H)
#define AIPS_IMAGEEXPR_H


//# Includes
#include <aips/aips.h>
#include <aips/Containers/Record.h>
#include <trial/Images/ImageInterface.h>
#include <trial/Lattices/ArrayLattice.h>
#include <trial/Lattices/LatticeExpr.h>



//# Forward Declarations
class IPosition;
class Slicer;
template <class T> class Array;
template <class T> class COWPtr;
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
// <list>
//   <item> LatticeExpr
//   <item> ImageInterface
// </list>
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

  // Construct an ImageExpr from a LatticeExpr
  ImageExpr(const LatticeExpr<T>& latticeExpr);

  // Copy constructor (reference semantics)
  ImageExpr(const ImageExpr<T>& other);

  // Destructor does nothing
  ~ImageExpr();

  // Assignment (reference semantics)
  ImageExpr<T>& operator=(const ImageExpr<T>& other);
  
  // Make a copy of the object (reference semantics).
  virtual Lattice<T>* clone() const;

  // return the shape of the ImageExpr
  virtual IPosition shape() const;

  // Function which changes the shape of the ImageExpr.
  // Throws an exception as ImageExpr is not writable.
  virtual void resize(const TiledShape &newShape);

  // Function which extracts an Array of values from a Image - a read-only
  // operation.
  // getSlice parameters:
  // <ul>
  // <li> buffer: a COWPtr<Array<T> > or an Array<T>.
  // <li> start: an IPosition which must have the same number of axes  
  //      as the underlying Image, otherwise, throw an exception.
  // <li> shape: an IPosition which must have equal or fewer axes than the
  //      true shape od the Image, otherwise, throw an exception
  // <li> stride: an IPosition which must have the same number of axes
  //      as the underlying Image, otherwise, throw an exception.
  // <li> removeDegenerateAxes: a Bool which dictates whether to remove    
  //      "empty" axis created in buffer. (e.g. extracting an n-dimensional
  //      from an (n+1)-dimensional will fill 'buffer' with an array that
  //      has a degenerate axis (i.e. one axis will have a length = 1.))  
  // </ul>
  //
  // The sub-class implementation of these functions return
  // 'True' if the buffer points to a reference
  // and 'False' if it points to a copy.
  // <note role=tip>
  // In most cases, it will be more efficient in execution, if you
  // use a LatticeIterator class to move through the Image.
  // LatticeIterators are optimized for that purpose.  If you are doing
  // unsystematic traversal, or random gets and puts, then getSlice and   
  // putSlice or operator() may be the right tools to use.
  // </note>
  // <group>
  virtual Bool getSlice(COWPtr<Array<T> > &buffer, const IPosition &start, 
                        const IPosition &shape, const IPosition &stride,   
                        Bool removeDegenerateAxes=False) const;
  
  virtual Bool getSlice(COWPtr<Array<T> > &buffer, const Slicer &theSlice,
                        Bool removeDegenerateAxes=False) const;
  virtual Bool getSlice(Array<T> &buffer, const IPosition &start,
                        const IPosition &shape, const IPosition &stride,
                        Bool removeDegenerateAxes=False);
  
  virtual Bool getSlice(Array<T> &buffer, const Slicer &theSlice,
                        Bool removeDegenerateAxes=False);
  // </group>
  //
  // Functions which place an Array of values within this instance of the
  // Lattice at the location specified.  These throw an exception as
  // the ImageExpr is not writable
  // <group>
  virtual void putSlice(const Array<T> & sourceBuffer, const IPosition & where);
  virtual void putSlice(const Array<T> & sourceBuffer, const IPosition & where,
                        const IPosition & stride);
  // </group>

  // Function which returns the whole mask Lattice to allow iteration or
  // Lattice functions.  The non const version throws an exception
  // because ImageExpr is not writable.  ImageExpr objects do yet 
  // contain a mask so throw an exception for the other too.
  virtual const Lattice<Bool> &mask() const;
  virtual Lattice<Bool> &mask();
  // </group>

  // Function which returns True if the image has a mask, returns False
  // otherwise.  Currently returns false.
  virtual Bool isMasked() const;
  
  // Function which get and set the units associated with the image
  // pixels (i.e. the "brightness" unit). <src>setUnits()</src> throws
  // an exception as ImageExpr is not writable. <src>getUnits</src>
  // returns an empty Unit as ImageExpr does not have access to 
  // units yet.
  // <group>   
  virtual Bool setUnits(const Unit &newUnits);
  virtual Unit units() const;
  // </group>


  // Return the name of the current ImageInterface object. 
  // Returns an empty String as an ImageExpr object does not
  // have persistence.
  virtual String name(const Bool stripPath=False) const;
  
  // Functions to set or replace the coordinate information.
  // <src>setCoordinate</src> throws an exception as the ImageExpr 
  // is not writable.  
  virtual Bool setCoordinateInfo(const CoordinateSystem &coords);
  
  // Function to get a LatticeCoordinate object containing the coordinates.
  virtual LatticeCoordinates latticeCoordinates() const;
  
  // These are the true implementations of the paren operator.
  // <src>putAt</src> throws an exception as ImageExpr is not writable.
  // <note> Not for public use </note>
  // <group>
  virtual T getAt(const IPosition &where) const;
  virtual void putAt(const T &value, const IPosition &where);
  // </group>

  // Often we have miscellaneous information we want to attach to an image.
  // <src>setMiscInfo</src> throws an exception as ImageExpr is not
  // writable
  // <group>
  virtual const RecordInterface &miscInfo() const;
  virtual Bool setMiscInfo(const RecordInterface &newInfo);
  // </group>
  
  // Check class invariants.
  virtual Bool ok() const;
  
  // These are the implementations of the LatticeIterator letters.
  // <note> not for public use </note>
  virtual LatticeIterInterface<T> *makeIter(
                                 const LatticeNavigator &navigator) const;

// Returns False, as the ImageExpr is not writable.
   virtual Bool isWritable() const;

// Help the user pick a cursor for most efficient access if they only want
// pixel values and don't care about the order or dimension of the
// cursor. 
   virtual IPosition niceCursorShape (uInt maxPixels) const;


private:  
  LatticeExpr<T> latticeExpr_p;

// These are used to return null object by reference

  Lattice<Bool>* pBool_p;  
  Record rec_p;


};



#endif
