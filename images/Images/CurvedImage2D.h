//# CurvedImage2D.h: An image crosscut based on a curve in a plane
//# Copyright (C) 2003
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

#ifndef IMAGES_CURVEDIMAGE2D_H
#define IMAGES_CURVEDIMAGE2D_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/images/Images/ImageInterface.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class CurvedLattice2D;
template <class T> class CLInterpolator2D;
class PixelCurve1D;


// <summary>
// An image crosscut based on a curve in a plane.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="tCurvedImage2D.cc">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=ImageInterface>ImageInterface</linkto>
//   <li> <linkto class=CurvedLattice2D>CurvedLattice2D</linkto>
// </prerequisite>
//
// <synopsis> 
// Class CurvedImage2D can be used to make a crosscut through an image
// with a dimensionality >= 2. The dimensionality of the resulting image
// is one less.
// The crosscut is based on a curve defined by a
// <linkto class=PixelCurve1D>PixelCurve1D</linkto> object. The curve
// can be any 1-dim function (e.g. straight line, spline)
// supported by the Functionals module. The curve must be in one of the
// main planes of the image as defined by the axes arguments in the
// constructor.
// <br>For example: in an RA-DEC-FREQ image a straight line can be
// defined in the RA-DEC plane (axis1=0, axis2=1) from blc {0,0) to
// trc (511,511). The crosscut will follow this line, so the result is
// a 2-dim image with axes 'line' and FREQ. So it contains the spectrum
// for all points on the line (points (0,0), (1,1) ... (511,511)).
// <br>In this example the line only contains exact grid points. In
// practice that usually won't be case, so interpolation has to be done.
// This is done by a class derived from
// <linkto class=CLInterpolator2D>CLInterpolator2D</linkto>, so any
// interpolation scheme is possible. Currently only the nearest neighbour
// scheme is implemented (<linkto class=CLIPNearest2D>CLIPNearest2D</linkto>).
// </synopsis> 
//
// <example>
// The following example uses a 3-dim image.
// It makes a crosscut using a line from the blc to the trc in the XY plane.
// The number of points on the line is the maximum of the number of points
// in X and Y.
// <srcblock>
// // Open an image.
// PagedImage<Float> image("name.img");
// // Make a straight line from (0,0) to the trc.
// IPosition shp = lat.shape();
// Int xtop = shp(0);
// Int ytop = shp(1);
// Int nr = xtop;
// if (nr > ytop) nr = ytop;
// PixelCurve1D pc(0, 0, xtop-1, ytop-1, nr);
// // Create the crosscut image.
// // The new axis (the curve axis) is the first axis in the result.
// CurvedImage2D<Float> clat(image, CLIPNearest2D<Float>(), pc, 0, 1, 0);
// </srcblock>
// Note that in the general case the line (or any curve) won't be from
// the blc to the trc. In fact, it is possible to give any starting and
// end point and any number of points on the curve.
// </example>
//
// <motivation>
// Users like to view arbitrary image crosscuts.
// </motivation>
//
//# <todo asof="1998/02/09">
//# </todo>


template <class T> class CurvedImage2D: public ImageInterface<T>
{
public: 
  // The default constructor
  CurvedImage2D();

  // Take a curved slice from the given image.
  // The <linkto class=PixelCurve1D>PixelCurve1D</linkto> object defines
  // the curve in one of the planes of the image. The arguments axis1
  // and axis2 define the plane the curve is in.
  // The <linkto class=CLInterpolator2D>CLInterpolator2D</linkto> object
  // defines the interpolation scheme for pixels that are not on grid points.
  // An example is CLIPNearest2D which takes the nearest neighbour.
  // The dimensionality of the CurvedImage2D is one less than the
  // dimensionality of the given image. Two axes (axis1 and axis2) are
  // replaced by the new axis representing the curve. The argument
  // curveAxis defines the axis number of the new axis. It defaults to the
  // last axis.
  // An exception is thrown if the dimensionality of the input image is < 2
  // or if the given axes numbers are too high.
  // Note that the output CoordinateSystem of the CurvedImage is just a dummy
  // LinearCoordinate at this point.  The values are all arbitrary.
  CurvedImage2D (const ImageInterface<T>&, const CLInterpolator2D<T>&,
		 const PixelCurve1D&, uInt axis1, uInt axis2,
		 Int curveAxis=-1);
  
  // Copy constructor (reference semantics).
  CurvedImage2D (const CurvedImage2D<T>& other);
    
  virtual ~CurvedImage2D();

  // Assignment (reference semantics).
  CurvedImage2D<T>& operator= (const CurvedImage2D<T>& other);

  // Make a copy of the object (reference semantics).
  // <group>
  virtual ImageInterface<T>* cloneII() const;
  // </group>

  // Get the image type (returns name of derived class).
  virtual String imageType() const;

  // Is the CurvedImage2D masked?
  // It is if its parent image is masked.
  virtual Bool isMasked() const;

  // Does the image object have a pixelmask?
  // It does if its parent has a pixelmask.
  virtual Bool hasPixelMask() const;

  // Get access to the pixelmask in use (thus to the pixelmask of the parent).
  // An exception is thrown if the parent does not have a pixelmask.
  // <group>
  virtual const Lattice<Bool>& pixelMask() const;
  virtual Lattice<Bool>& pixelMask();
  // </group>

  // Get the region used (always returns 0).
  virtual const LatticeRegion* getRegionPtr() const;

  // A CurvedImage2D is not persistent.
  virtual Bool isPersistent() const;

  // Is the CurvedImage2D paged to disk?
  virtual Bool isPaged() const;

  // An CurvedImage2D is not writable
  virtual Bool isWritable() const;

  // Returns the shape of the CurvedImage2D
  virtual IPosition shape() const;
  
  // This function returns the recommended maximum number of pixels to
  // include in the cursor of an iterator.
  virtual uInt advisedMaxPixels() const;

  // Function which changes the shape of the CurvedImage2D.
  // Throws an exception as resizing an CurvedImage2D is not possible.
  virtual void resize(const TiledShape& newShape);

  // Return the name of the parent ImageInterface object. 
  virtual String name (Bool stripPath=False) const;
  
  // Check class invariants.
  virtual Bool ok() const;

  // Get access to the attribute handler (of the parent image).
  // If a handler keyword does not exist yet, it is created if
  // <src>createHandler</src> is set.
  // Otherwise the handler is empty and no groups can be created for it.
  virtual ImageAttrHandler& attrHandler (Bool createHandler=False);

  // Do the actual getting of an array of values.
  virtual Bool doGetSlice (Array<T>& buffer, const Slicer& section);

  // Putting data is not possible.
  virtual void doPutSlice (const Array<T>& sourceBuffer,
			   const IPosition& where,
			   const IPosition& stride);
  
  // Get a section of the mask.
  virtual Bool doGetMaskSlice (Array<Bool>& buffer, const Slicer& section);

  // This function is used by the LatticeIterator class to generate an
  // iterator of the correct type for this Lattice. Not recommended
  // for general use. 
  virtual LatticeIterInterface<T>* makeIter
                            (const LatticeNavigator& navigator,
			     Bool useRef) const;

  // Get the best cursor shape.
  virtual IPosition doNiceCursorShape (uInt maxPixels) const;

  // Handle the (un)locking and syncing, etc.
  // <group>
  virtual Bool lock (FileLocker::LockType, uInt nattempts);
  virtual void unlock();
  virtual Bool hasLock (FileLocker::LockType) const;
  virtual void resync();
  virtual void flush();
  virtual void tempClose();
  virtual void reopen();
  // </group>

private:
  //# itsImagePtr points to the parent image.
  ImageInterface<T>*  itsImagePtr;
  CurvedLattice2D<T>* itsCurLatPtr;

  //# Make members of parent class known.
public:
  using ImageInterface<T>::logger;
protected:
  using ImageInterface<T>::setCoordsMember;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/images/Images/CurvedImage2D.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
