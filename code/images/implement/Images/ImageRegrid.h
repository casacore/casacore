//# DOdeconvolver: defines classes for deconvolver DO.
//# Copyright (C) 1996,1997,1998,1999
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
//#
//# $Id$

#if !defined(TRIAL_DO_DECONVOLVER_H)
#define TRIAL_DO_DECONVOLVER_H

#include <aips/aips.h>
#include <trial/Tasking/ApplicationObject.h>
#include <aips/Tasking/Index.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MRadialVelocity.h>
#include <trial/Lattices/LatticeCleaner.h>

template<class T> class ImageInterface;


// <summary>This regrids one image to match the coordinate system of another</summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="ImageInterface">ImageInterface</linkto>
//   <li> <linkto class="CoordinateSystem">CoordinateSystem</linkto>
// </prerequisite>
//
// <etymology>
// ImageRegrid : hey, it Regrids Images!
// </etymology>
//
// <synopsis>
// </synopsis>
//
// <example>
// 
// <srcblock>
// </srcblock>
// </example>
//
// <motivation> 
// This class was written to regrid one image to another.
// </motivation>
//
// <thrown>
// <li> AipsError 
// </thrown>
//
// <todo asof="1999/03/04">
//   <li> Currently, we can't really regrid as in HGEOM.  The
//  pixles need to share the same basic grid, we just make one image shape and
//  reference pixel look like another.
// </todo>

template <class T> class ImageRegrid
{
public:

  // default constructor
  ImageRegrid();

  // constructor which takes the coordinate information required
  // to specify the template
  // (use this instead of the actual images, which may be C++/Templated
  // differently than the data image)

  // also, currently outShape does nothing (ie, == templateShape)
  // interpolation order does nothing

  ImageRegrid(const IPosition& templateShape,
	      const CoordinateSystem& templateCoords,
	      const uInt interpOrder,
	      const IPosition& outShape);

  ImageRegrid(const IPosition& templateShape,
	      const CoordinateSystem& templateCoords);

  // copy constructor
  ImageRegrid(const ImageRegrid &other);

  // destructor
  ~ImageRegrid();

  // operator=
  ImageRegrid<T>& operator=(const ImageRegrid& other);

  // regrid dataImage onto the grid specified in the state data;
  // If stokesImageConventions == True, "some restrictions may apply"
  ImageInterface<T>* regrid(ImageInterface<T>& dataImage, Bool stokesImageConventions=False);

  ImageInterface<T>* fitIntoImage(ImageInterface<T>& dataImage, Bool stokesImageConventions=False);
    
  void setTemplate (const IPosition& tShape,
		    const CoordinateSystem& tCoords);

  void setInterpOrder (uInt order);

  void setOutShape (const IPosition& oShape);  // current does nothing

 private:

  // Private data
  // in future: interp order,
  // actual output image size (templateShape refers to
  // the coordinate system and is a slight misnomer)
  IPosition templateShape;
  const CoordinateSystem * templateCoords_p;
  uInt interpOrder;
  IPosition outShape;

  // private methods
  
  // Fit imageData into an image which is consistent in
  // size and reference pixel with templateShape and templateCoords.
  // return 0 pointer if reference value or cell sizes are different.
  // If stokesImageConventions == True, "some restrictions may apply"
  ImageInterface<T>* fitIntoStokesImage(ImageInterface<T>& dataImage);

  /*      // hgeom algorithm and generic case - don't exist yet
  ImageInterface<T>* fitIntoGenericImage(ImageInterface<T>& dataImage);
  ImageInterface<T>* hgeomStokesImage(ImageInterface<T>& dataImage);
  ImageInterface<T>* hgeomGenericImage(ImageInterface<T>& dataImage);
  */

};

 
#endif
