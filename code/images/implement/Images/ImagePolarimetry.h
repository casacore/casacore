//# ImagePolarimetry.h: generate Polarimetry from an image
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
//# $Id$

#if !defined(AIPS_IMAGEPOLARIMETRY_H)
#define AIPS_IMAGEPOLARIMETRY_H


//# Includes
#include <aips/aips.h>
#include <aips/Measures/Stokes.h>
#include <aips/Mathematics/Complex.h>
#include <trial/Images/ImageInterface.h>

//# Forward Declarations
template <class T> class SubImage;
template <class T> class ImageExpr;
//
class CoordinateSystem;
class IPosition;
class LatticeExprNode;
class LCBox;
class LogIO;



// <summary>
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=ImageInterface>ImageInterface</linkto>
// </prerequisite>

// <etymology>
// </etymology>

// <synopsis>
// </synopsis>
//
// <motivation>
// </motivation>

// <todo asof="1998/11/01">
//   <li> 
// </todo>


class ImagePolarimetry 
{
public:

// Constructor
   ImagePolarimetry (const ImageInterface<Float>& image);

// Copy constructor (reference semantics)
   ImagePolarimetry(const ImagePolarimetry& other);

// Destructor
   virtual ~ImagePolarimetry ();
   
// Assignment operator (reference semantics)
   ImagePolarimetry& operator=(const ImagePolarimetry& other);

// Summary
   void summary(LogIO& os) const;

// Get the CoordinateSystem of the construction image
   CoordinateSystem coordinates() const {return itsInImagePtr->coordinates();};

// Get the shape of the construction image
   IPosition shape() const {return itsInImagePtr->shape();};

// Is the construction image masked ?
   Bool isMasked() const {return itsInImagePtr->isMasked();};

// Get the shape of an image for a single Stokes pixel
// Thus, if the construction shape was [10,10,4,20] where
// axis 2 (shape 4) is the Stokes axis, this function
// would return [10,10,1,20]
   IPosition stokesShape() const;

// Get Stokes I 
   ImageExpr<Float> stokesI() const;

// Get Stokes Q
   ImageExpr<Float> stokesQ() const;

// Get Stokes U 
   ImageExpr<Float> stokesU() const;

// Get Stokes V 
   ImageExpr<Float> stokesV() const;

// Get Linearly polarized intensity
   ImageExpr<Float> linPolInt(Bool debias, Float var) const;

// Get Total polarized intensity.  
   ImageExpr<Float> totPolInt(Bool debias, Float var) const;

// Get Linearly polarized position angle
   ImageExpr<Float> linPolPosAng() const;

// Get Fractional linear polarization 
   ImageExpr<Float> fracLinPol(Bool debias, Float var) const;

// Get Fractional total polarization 
   ImageExpr<Float> fracTotPol(Bool debias, Float var) const;

// Fourier Rotation Measure.  Coordinates, ImageInfo, MiscInfo, Units,
// history are updated/copied to the output.  If the output has a mask,
// and the input is masked, the mask is copied.  If the output
// has a mask, it should already have been initialized to True
   void fourierRotationMeasure(ImageInterface<Complex>& lag,
                               Bool zeroZeroLag);


private:
   const ImageInterface<Float>* itsInImagePtr;
   ImageInterface<Float>* itsIImagePtr;
   ImageInterface<Float>* itsQImagePtr;
   ImageInterface<Float>* itsUImagePtr;
   ImageInterface<Float>* itsVImagePtr;
// 
   void cleanup();
   void fiddleStokesCoordinate(ImageInterface<Float>& ie, Stokes::StokesTypes type) const;
   void fiddleStokesCoordinate(ImageInterface<Complex>& ie, Stokes::StokesTypes type) const;
   void findStokes();
   LatticeExprNode makePolIntNode(LogIO& os, Bool debias, Float var,
                                  Bool doLin, Bool doCirc) const;
   ImageExpr<Float> makeStokesExpr(ImageInterface<Float>* imPtr,
                               const String& s, const String& name) const;
   ImageInterface<Float>* makeSubImage (IPosition& blc, IPosition& trc,
                                    Int axis, Int pix) const;
};


#endif

