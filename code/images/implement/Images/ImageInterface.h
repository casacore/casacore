//# ImageInterface.h: a base class for astronomical images
//# Copyright (C) 1996,1997
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

#if !defined(AIPS_IMAGEINTERFACE_H)
#define AIPS_IMAGEINTERFACE_H

#if defined(_AIX)
#pragma implementation ("ImageInterface.cc")
#endif 

#include <aips/aips.h>

#include <trial/Lattices/Lattice.h>
#include <trial/Coordinates/CoordinateSystem.h>

#include <aips/Measures/Unit.h>

//# predeclarations
template <class T> class RO_LatticeIterInterface;
template <class T> class LatticeIterInterface;
template <class T> class Vector;
template <class T> class COWPtr;
class IPosition;
class LogIO;
class RecordInterface;

// <summary> a base class for astronomical images </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=Lattice>Lattices</linkto>
//   <li> <linkto class=CoordinateSystem>CoordinateSystem</linkto>
// </prerequisite>
//
// <etymology>
// The ImageInterface class name is derived from its role as the cookie cutter
// Interface base class for Images.  
// </etymology>
//
// <synopsis> 
// The ImageInterface class is an abstract base class. All Image classes
// should derive from this class to ensure functions which operate on Images
// will work for all Image derivations.
// 
// An Image is currently defined as an Array of pixels, a Boolean mask,
// defining which pixels are valid and coordinates to define the reference
// frame. The only concrete class currently derived from this Interface is
// PagedImage, which allows the image to be stored on disk, and only reads
// specified portions of the image into memory.  
// </synopsis>
//
// <example>
// As this is an abstract base class it is not possible to construct an
// instance of this object.  It can however be used as a function argument.<br>
// eg 1. (used in dImageInterface.cc)
// <srcblock>
// Float sumPixels(const ImageInterface<Float>& image){
//   uInt rowLength = image.shape()(0);
//   IPosition rowShape(image.ndim());
//   rowShape = 1; rowShape(0) = rowLength;
//   Float sumPix = 0;
//   RO_LatticeIterator<Float> iter(image, rowShape);
//   while(!iter.atEnd()){
//     sumPix += sum(iter.vectorCursor().ac());
//     iter++;
//   }
//   return sumPix;
// }
// </srcblock>
//
// The main purpose of this class is for programming objects, the following
// example is of how one would derive from ImageInterface: <br>
// eg 2.
// <srcblock>
// template <class T> class myNewImage : public ImageInterface<T>
// {
// public:
//   // default constructor
//   myNewImage();
//
//   // argumented constructor
//   myNewImage(...);
//
//   // destructor
//   ~myNewImage
//   
//   // the shape function is forced upon us by the Lattice base class
//   IPosition shape() const;
//   
//   // getSlice is another function required of all Lattice objects.
//   Bool getSlice(COWPtr<Array<T> > &buffer, const IPosition &start, 
//		   const IPosition &shape, const IPosition &stride, 
//		   Bool removeDegenerateAxes=False) const;
//
//  // etc...
// private:
//  // put the actual map data down here.
//  // etc...
// };
// </srcblock>
// </example>
//
// <motivation> 
// The use of abstract base classes to guide inheritance seemed appropriate
// for Images to ensure that CoordinateSystems and masking get handled
// uniformly.
// </motivation>
//
// <todo asof="1995/04/25">
//   <li> replace ImageCoordinates
// </todo>

template <class T> class ImageInterface: public Lattice<T>
{
public: 

  // function which returns the shape of the Image.
  virtual IPosition shape() const = 0;

  // Function which changes the shape of the image (N.B. the data is thrown 
  // away - the Image will be filled with nonsense afterwards)
  virtual void resize(const IPosition &newShape) = 0;
    
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
			Bool removeDegenerateAxes=False) const = 0;
  
  virtual Bool getSlice(COWPtr<Array<T> > &buffer, const Slicer &theSlice, 
			Bool removeDegenerateAxes=False) const = 0;
  
  virtual Bool getSlice(Array<T> &buffer, const IPosition &start, 
			const IPosition &shape, const IPosition &stride,
			Bool removeDegenerateAxes=False) = 0;
  
  virtual Bool getSlice(Array<T> &buffer, const Slicer &theSlice, 
			Bool removeDegenerateAxes=False) = 0;
  // </group>
  
  // Function which places an Array of values within the Image
  virtual void putSlice(const Array<T> &sourceBuffer, const IPosition &where, 
			const IPosition &stride) = 0;
  
  // Function which returns the whole mask Lattice to allow iteration or 
  // Lattice functions.
  // <note> The mask object will be deleted upon destruction of this instance
  // of Image.  Survival past the lifetime of the parent Image isn't 
  // guaranteed. </note>
  // <group>    
  virtual const Lattice<Bool> &mask() const = 0;
  virtual Lattice<Bool> &mask() = 0;
  // </group>
  
  // Function to toggle the ability to write through the image mask.
  // newValue = True implies writes to Image will alter the map.  newValue = 
  // False implies writes to Image will fail.
  void writeThroughMask(Bool newValue);

  // function which returns True if the image has a mask, returns False 
  // otherwise. 
  virtual Bool isMasked() const = 0;
  
  // Function to return the value of the mask toggle
  Bool writeThroughMask() const;
  
  // Function to set the default Value.  This value will be returned when 
  // access to a masked element is attempted.
  virtual void setDefaultValue(const T &newValue) = 0;
  
  // Function to return the value of the stored default Value.
  virtual const T &defaultValue() const = 0;
  
  // Function which sets the units associated with the map.
  void setUnits(const Unit &newUnits);

  // Return the name of the current ImageInterface object. This will generally 
  // be a file name for images that have a persistent form.
  virtual String name() const = 0;

  // Function to return the units of the map
  const Unit &units() const;

  // Functions to set or replace the coordinate information in the Image
  // The default implementation of setCoordinateInfo merely updates the
  // CoordinateSystem object in the interface. Derived classes will
  // normally flush them to disk as well. Returns False on failure, e.g.
  // if the number of axes do not match.
  // <group>
  virtual Bool setCoordinateInfo(const CoordinateSystem &coords);
  const CoordinateSystem &coordinates() const;
  // </group>
  
  // Allow messages to be logged to this ImageInterface.
  virtual LogIO &logSink() = 0;
  
  // These are the true implementations of the paren operator.
  // <note> Not for public use </note>
  // <group>
  virtual T getAt(const IPosition &where) const = 0;
  virtual void putAt(const T &value, const IPosition &where) = 0;
  // </group>

  // Often we have miscellaneous information we want to attach to an image.
  // This is how it is done. Eventually we will want to register that some
  // of the information is to be destroyed if the image changes so that, e.g.
  // data max/min values can be removed if the image changes.
  //
  // Note that setMiscInfo REPLACES the information with the new information.
  // If can fail if, e.g., the underlying table is not writable.
  // <group>
  virtual const RecordInterface &miscInfo() const = 0;
  virtual Bool setMiscInfo(const RecordInterface &newInfo) = 0;
  // </group>
  
  // Check class invariants. 
  virtual Bool ok() const = 0;
  
  // These are the implementations of the LatticeIterator letters.
  // <note> not for public use </note>
  // <group>
  virtual RO_LatticeIterInterface<T> *makeIter(
				 const LatticeNavigator &navigator) const = 0;

  virtual RO_LatticeIterInterface<T> *makeIter(
                                      const IPosition &cursorShape) const = 0;
  virtual LatticeIterInterface<T> *makeIter(
				 const LatticeNavigator &navigator) = 0;

  virtual LatticeIterInterface<T> *makeIter(
                                      const IPosition &cursorShape) = 0;
  //</group>

  //<group>
  // Function to work around the g++ upcast bug
  ImageInterface<T>& ic() {return *this;}
  const ImageInterface<T>& ic() const {return *this;}
  //</group>
 
 
protected:

  ImageInterface();
  ImageInterface(const CoordinateSystem &coords, Bool masking);

  CoordinateSystem coords_p;
  Bool throughmask_p;
  Unit units_p;
  
};

#endif
