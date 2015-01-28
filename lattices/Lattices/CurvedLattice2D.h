//# CurvedLattice2D.h: A lattice crosscut based on a curve in a plane
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
//# You should have receied a copy of the GNU Library General Public License
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

#ifndef LATTICES_CURVEDLATTICE2D_H
#define LATTICES_CURVEDLATTICE2D_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/Lattices/MaskedLattice.h>
#include <casacore/lattices/Lattices/PixelCurve1D.h>
#include <casacore/lattices/LatticeMath/CLInterpolator2D.h>
#include <casacore/casa/Arrays/AxesMapping.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations


// <summary>
// A lattice crosscut based on a curve in a plane.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tCurvedLattice2D.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=PixelCurve1D>PixelCurve1D</linkto>
//   <li> <linkto class=CLInterpolator2D>CLInterpolator2D</linkto>
// </prerequisite>

// <synopsis>
// Class CurvedImage2D can be used to make a crosscut through an image
// with a dimensionality >= 2.
// The crosscut is based on a curve defined by a
// <linkto class=PixelCurve1D>PixelCurve1D</linkto> object. The curve
// can be any 1-dim function (e.g. straight line, spline)
// supported by the Functionals module. The curve must be in one of the
// main planes of the image as defined by the axes arguments in the
// constructor.
// <br>See class <linkto class=CurvedImage2D>CurvedImage2D</linkto> for
// a more detailed description.
// </synopsis>

// <example>
// See example in <linkto class=CurvedImage2D>CurvedImage2D</linkto>.
// </example>

// <motivation>
// Users like to view arbitrary image crosscuts.
// </motivation>


template<class T>
class CurvedLattice2D: public MaskedLattice<T>
{
public:

  // Default constructor
  CurvedLattice2D();

  // Take a curved slice from the given MaskedLattice. For example, define
  // a spline in the RA-DEC plane and extend it in the FREQ direction.
  // The result is a 2D lattice with axes FREQ and 'spline'.
  // <br>
  // The <linkto class=PixelCurve1D>PixelCurve1D</linkto> object defines
  // the curve in one of the planes of the lattice. The arguments axis1
  // and axis2 define the plane the curve is in.
  // The <linkto class=CLInterpolator2D>CLInterpolator2D</linkto> object
  // defines the interpolation scheme for pixels that are not on grid points.
  // An example is CLIPNearest2D which takes the nearest neighbour.
  // The dimensionality of the CurvedLattice2D is one less than the
  // dimensionality of the given lattice. Two axes (axis1 and axis2) are
  // replaced by the new axis representing the curve. The argument
  // curveAxis defines the axis number of the new axis. It defaults to the
  // last axis.
  // An exception is thrown if the dimensionality of the input lattice is < 2
  // or if the given axes numbers are too high.
  CurvedLattice2D (const MaskedLattice<T>&, const CLInterpolator2D<T>&,
		   const PixelCurve1D&, uInt axis1, uInt axis2,
		   Int curveAxis=-1);

  // Copy constructor (reference semantics)
  CurvedLattice2D(const CurvedLattice2D<T>& other);

  // Destructor, does nothing
  virtual ~CurvedLattice2D();

  // Assignment (reference semantics)
  CurvedLattice2D<T>& operator=(const CurvedLattice2D<T>& other);

  // Make a copy of the object (reference semantics).
  virtual MaskedLattice<T>* cloneML() const;

  // Is the lattice masked?
  // It is if its parent lattice is masked.
  virtual Bool isMasked() const;

  // Is the lattice paged to disk?
  virtual Bool isPaged() const;

  // The lattice is not writable.
  virtual Bool isWritable() const;

  // Handle ocking of the lattice which is delegated to its parent.
  // <br>It is strongly recommended to use class
  // <linkto class=LatticeLocker>LatticeLocker</linkto> to
  // handle lattice locking. It also contains a more detailed
  // explanation of the locking process.
  // <group>
  virtual Bool lock (FileLocker::LockType, uInt nattempts);
  virtual void unlock();
  virtual Bool hasLock (FileLocker::LockType) const;
  // </group>

  // Resynchronize the Lattice object with the lattice file.
  // This function is only useful if no read-locking is used, ie.
  // if the table lock option is UserNoReadLocking or AutoNoReadLocking.
  // In that cases the table system does not acquire a read-lock, thus
  // does not synchronize itself automatically.
  virtual void resync();

  // Flush the data.
  virtual void flush();

  // Close the Lattice temporarily (if it is paged to disk).
  // It'll be reopened automatically when needed or when
  // <src>reopen</src> is called explicitly.
  virtual void tempClose();

  // If needed, reopen a temporarily closed Lattice.
  virtual void reopen();

  // Get a pointer the region/mask object.
  // It returns 0.
  virtual const LatticeRegion* getRegionPtr() const;

  // Returns the shape of the lattice.
  virtual IPosition shape() const;
  
  // Return the name of the parent lattice.
  virtual String name (Bool stripPath=False) const;

  // This function returns the recommended maximum number of pixels to
  // include in the cursor of an iterator.
  virtual uInt advisedMaxPixels() const;

  // Check class internals - used for debugging. Should always return True
  virtual Bool ok() const;

  // Do the actual getting of an array of values.
  virtual Bool doGetSlice (Array<T>& buffer, const Slicer& section);

  // Do the actual getting of an array of values.
  virtual void doPutSlice (const Array<T>& sourceBuffer,
			   const IPosition& where,
			   const IPosition& stride);
  
  // Get a section of the mask.
  virtual Bool doGetMaskSlice (Array<Bool>& buffer, const Slicer& section);

  // Get the best cursor shape.
  virtual IPosition doNiceCursorShape (uInt maxPixels) const;


private:
  // Make the AxesMapping object to map input to output axes.
  void makeMapping (uInt axis1, uInt axis2, Int curveAxis);


  MaskedLattice<T>*       itsLatticePtr;
  CLInterpolator2D<T>*    itsInterpolator;
  PixelCurve1D            itsCurve;
  uInt                    itsAxis1;
  uInt                    itsAxis2;
  uInt                    itsCurveAxis;
  AxesMapping             itsAxesMap;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/Lattices/CurvedLattice2D.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
