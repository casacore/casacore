//# ImagePolarimetry.h: Polarimetric analysis of images
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002
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

#ifndef IMAGES_IMAGEPOLARIMETRY_H
#define IMAGES_IMAGEPOLARIMETRY_H


//# Includes
#include <casa/aips.h>
#include <casa/Arrays/Vector.h>
#include <casa/Containers/Block.h> 
#include <measures/Measures/Stokes.h>
#include <casa/BasicSL/Complex.h>
#include <images/Images/ImageInterface.h>
#include <scimath/Fitting/LinearFitSVD.h>


namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations
template <class T> class SubImage;
template <class T> class ImageExpr;
template <class T> class Quantum;
template <class T> class LatticeStatistics;
//
class CoordinateSystem;
class IPosition;
class LatticeExprNode;
class LCBox;
class LogIO;
class PGPlotter;


// <summary>
// Polarimetric analysis of images
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=ImageExpr>ImageExpr</linkto>
//   <li> <linkto class=ImageInterface>ImageInterface</linkto>
// </prerequisite>

// <etymology>
//  Polarimetric analysis of Images
// </etymology>

// <synopsis>
// This class provides polarimetric image analysis capability.
// It takes an image with a Stokes axis (some combination
// of IQUV is needed) as its input.
//
// Many functions return ImageExpr objects.  These are
// read-only images.
//
// Sometimes the standard deviation of the noise is needed.
// This is for debiasing polarized intensity images or working out
// error images.  By default it is worked out for you with a
// clipped mean algorithm.  However, you can provide sigma if you
// know it accurately.   It should be the standard deviation of the noise in
// the absence of signal.  You won't measure that very well from
// Stokes I if it is dynamic range limited.  Better to get it from 
// V or Q or U.  When this class needs the standard deviation of
// the noise, it will try and get it from V or Q and U and finally I.
//
// However, note that the functions sigmaStokes{I,Q,U,V} DO return the standard
// deviation of the noise for that specific Stokes type.
//
// The ImageExpr objects returned have the brightness units and ImageInfo
// set.  The MiscInfo (a permanent record) and logSink are not set.
// 
// </synopsis>
//
// <motivation>
// Basic image analysis capability
// </motivation>

// <todo asof="1999/11/01">
//   <li> plotting for function rotationMeasure 
//   <li> some assessment of the curvature of pa-l**2
// </todo>


class ImagePolarimetry 
{
public:

// Stokes types
   enum StokesTypes {I, Q, U, V}; 

// Constructor.  The input image must have a Stokes
// axis with some subset of I,Q,U, and V
   ImagePolarimetry (const ImageInterface<Float>& image);

// Copy constructor (reference semantics)
   ImagePolarimetry(const ImagePolarimetry& other);

// Destructor
   virtual ~ImagePolarimetry ();
   
// Assignment operator (reference semantics)
   ImagePolarimetry& operator=(const ImagePolarimetry& other);

// Summary.  Just invokes the ImageSummary list function
// to summarize the header of the construction image
   void summary(LogIO& os) const;

// Get the ImageInterface pointer of the construction image
// Don't delete it !
   const ImageInterface<Float>* imageInterface() const {return itsInImagePtr;};

// Get the CoordinateSystem of the construction image
   CoordinateSystem coordinates() const {return itsInImagePtr->coordinates();};

// Get the shape of the construction image
   IPosition shape() const {return itsInImagePtr->shape();};

// Is the construction image masked ?
   Bool isMasked() const {return itsInImagePtr->isMasked();};

// Get the shape and CoordinateSystem of an image for a single Stokes pixel
// Thus, if the construction image shape was [10,10,4,20] where
// axis 2 (shape 4) is the Stokes axis, this function
// would return [10,10,1,20]    Specify the type of Stokes pixel
// you want.  
   IPosition singleStokesShape(CoordinateSystem& cSys, Stokes::StokesTypes type) const;

// Complex linear polarization
   ImageExpr<Complex> complexLinearPolarization ();

// Complex fractional linear polarization
   ImageExpr<Complex> complexFractionalLinearPolarization ();

// Get the Stokes I image and the standard deviation of the
// I image.  This  is worked out by first clipping 
// outliers from the mean at the specified level.
// <group>
   ImageExpr<Float> stokesI() const;
   Float sigmaStokesI (Float clip=10.0);
// </group>

// Get the Stokes Q image and the standard deviation 
// of the Q image.  This  is worked out by first clipping 
// outliers from the mean at the specified level.
// <group>
   ImageExpr<Float> stokesQ() const;
   Float sigmaStokesQ (Float clip=10.0);
// </group>

// Get the Stokes U image and the standard deviation 
// of the U image.  This  is worked out by first clipping 
// outliers from the mean at the specified level.
// <group>
   ImageExpr<Float> stokesU() const;
   Float sigmaStokesU (Float clip=10.0);
// </group>

// Get the Stokes V image and the standard deviation 
// of the V image.  This  is worked out by first clipping 
// outliers from the mean at the specified level.
// <group>
   ImageExpr<Float> stokesV() const;
   Float sigmaStokesV (Float clip=10.0);
// </group>

// Get the specified Stokes image and the standard deviation 
// of the image.  This  is worked out by first clipping 
// outliers from the mean at the specified level.
// <group>
   ImageExpr<Float> stokes(ImagePolarimetry::StokesTypes index) const;
   Float sigmaStokes (ImagePolarimetry::StokesTypes index, Float clip=10.0);
// </group>

// Get the best estimate of the statistical noise. This gives you
// the standard deviation with outliers from the mean
// clipped first. The idea is to not be confused by source or dynamic range issues.
// Generally Stokes V is empty of sources (not always), then Q and U are generally
// less bright than I.  So this function first tries V, then Q and U 
// and lastly I to give you its noise estimate
   Float sigma (Float clip=10.0);

// Get the linearly polarized intensity image and its
// standard deviation.  If wish to debias the image, you
// can either provide <src>sigma</src> (the standard 
// deviation of the termal noise ) or if <src>sigma</src> is non-positive, 
// it will  be worked out for you with a clipped mean algorithm.
// <group>
   ImageExpr<Float> linPolInt(Bool debias, Float clip=10.0, Float sigma=-1.0);
   Float sigmaLinPolInt (Float clip=10.0, Float sigma=-1.0);
// </group>

// Get the total polarized intensity (from whatever combination
// of Q, U, and V the construction image has) image and its error 
// (standard deviation).  If wish to debias the image, you
// can either provide <src>sigma</src> (the standard  deviation 
// of the thermal noise) or if <src>sigma</src> is 
// non-positive, it will be worked out for you with a 
// clipped mean algorithm.
// <group>
   ImageExpr<Float> totPolInt(Bool debias, Float clip=10.0, Float sigma=-1.0);
   Float sigmaTotPolInt (Float clip=10.0, Float sigma=-1.0);
// </group>

// Get linearly polarized position angle (degrees or radians) image
// and error (standard deviation).   If you provide 
// <src>sigma</src> it is the  standard deviation of 
// the termal noise.  If <src>sigma</src> is non-positive, it will be 
// worked out for you with a  clipped mean algorithm.
// <group>
   ImageExpr<Float> linPolPosAng(Bool radians) const;
   ImageExpr<Float> sigmaLinPolPosAng (Bool radians, Float clip=10.0, Float sigma=-1.0);
// </group>

// Get fractional linear polarization image 
// and error (standard deviation).   If wish to debias the image, you
// can either provide <src>sigma</src> (the standard 
// deviation of the termal noise) or if <src>sigma</src> is non-positive, 
// it will  be worked out for you with a clipped mean algorithm.
// <group>
   ImageExpr<Float> fracLinPol(Bool debias, Float clip=10.0, Float sigma=-1.0);
   ImageExpr<Float> sigmaFracLinPol (Float clip=10.0, Float sigma=-1.0);
// </group>

// Get Fractional total polarization and error (standard deviation)
// <src>var</src> is the standard deviation  of the thermal noise.
// If <src>sigma</src> is non-positive, 
// it will  be worked out for you with a clipped mean algorithm.
// <group>
   ImageExpr<Float> fracTotPol(Bool debias, Float clip=10.0, Float sigma=-1.0);
   ImageExpr<Float> sigmaFracTotPol (Float clip=10.0, Float sigma=-1.0);
// </group>

// Fourier Rotation Measure.  The output image is the complex polarization
// (Q + iU) with the spectral axis replaced by a RotationMeasure axis.
// The appropriate shape and CoordinateSystem must be obtained with
// function singleStokesShape (ask for type STokes::Plinear).  
// Howeverm this function will replace the SpectralCoordinate 
// by a LinearCoordinate describing  the Rotation Measure.  
// ImageInfo, and Units are copied to the output.  MiscInfo and
// history are not.  If the output has a mask,
// and the input is masked, the mask is copied.  If the output
// has a mask, it should already have been initialized to True
   void fourierRotationMeasure(ImageInterface<Complex>& pol,
                               Bool zeroZeroLag);

// This function is used in concert with the rotationMeasure function.
// It tells you what the shape of the output RM image should be, and
// gives you its CoordinateSystem.  Because the ImagePolarimetry 
// construction image may house the frequencies coordinate description
// in a Spectral, Tabular or Linear coordinate, you may explicitly
// specify which axis is the Spectral axis (spectralAxis).  By default,
// it tries to find the SpectralCoordinate.  If there is none, it will
// look for Tabular or Linear coordinates with a "FREQ" label.
// It returns to you the frequencyAxis (i.e. the one it is concluded
// houses the frequency spectrum) and the stokesAxis that it
// finds.
   IPosition rotationMeasureShape(CoordinateSystem& cSys,
                                  Int& frequencyAxis, Int& stokesAxis, 
                                  LogIO& os, Int spectralAxis=-1) const;

// This function is used in concert with the rotationMeasure function.
// It tells you what the shape of the output Position Angle image should be, and
// gives you its CoordinateSystem.  Because the ImagePolarimetry
// construction image may house the frequencies coordinate description
// in a Spectral, Tabular or Linear coordinate, you may explicitly
// specify which axis is the Spectral axis (spectralAxis).  By default,
// it tries to find the SpectralCoordinate.  If there is none, it will
// look for Tabular or Linear coordinates with a "FREQ" label.
   IPosition positionAngleShape(CoordinateSystem& cSys,
                                Int& frequencyAxis, Int& stokesAxis, 
                                LogIO& os, Int spectralAxis=-1) const;

// This function applies a traditional (i.e. non-Fourier) Rotation Measure 
// algorithm (Leahy et al, A&A, 156, 234) approach.     For the RM images
// you must get the shape and CoordinateSYstem from function
// rotationMeasureShape.  For the position angle images, use function
// singleStokesShape.  Null pointer ImageInterface objects are 
// not accessed so you can select which output images you want.
// Any output images not masked will be given a mask.
// The position angles are all in degrees. The RM images in rad/m/m.
// ImageInfo and Units, are copied to the output.  MiscInfo and history are not.
// You specify which axis houses the frequencies, the noise level of
// Q and U  if you  know it (by default it is worked out for you) 
// for error images, the value of the foreground RM if you know it
// (helps for unwinding ambiguity), the absolute maximum RM it should 
// solve for, and the maximum error in the position angle that should
// be allowed.  The state of the plotter should be set by the caller
// (e.g. character size, number of plots in x and y etc).
   void rotationMeasure(ImageInterface<Float>*& rmPtr,  ImageInterface<Float>*& rmErrPtr, 
                        ImageInterface<Float>*& pa0Ptr, ImageInterface<Float>*& pa0ErrPtr,
                        ImageInterface<Float>*& nTurns, ImageInterface<Float>*& rChiSqPtr,
                        PGPlotter& plotter,
                        Int spectralAxis,  Float rmMax, Float maxPaErr=1.0e30,
                        Float sigma=-1.0, Float rmFg=0.0, Bool showProgress=False);

// Depolarization ratio image and error.  Requires two images hence static
// functions.
// <group>
   static ImageExpr<Float> depolarizationRatio (const ImageInterface<Float>& im1,
                                                const ImageInterface<Float>& im2,
                                                Bool debias, Float clip=10.0, Float sigma=-1.0);

   static ImageExpr<Float> sigmaDepolarizationRatio (const ImageInterface<Float>& im1,
                                                     const ImageInterface<Float>& im2,
                                                     Bool debias, Float clip=10.0, Float sigma=-1.0);
// </group>

private:
   const ImageInterface<Float>* itsInImagePtr;
   LinearFitSVD<Float>* itsFitterPtr;
   Float itsOldClip;

// These blocks are always size 4, with IQUV in slots 0,1,2,3
// If your image is IV only, they still use slots 0 and 3

   PtrBlock<ImageInterface<Float>* > itsStokesPtr;
   PtrBlock<LatticeStatistics<Float>* > itsStokesStatsPtr;

// Delete all private pointers
   void cleanup();

// Copy data and mask
   void copyDataAndMask(ImageInterface<Float>& out,
                        ImageInterface<Float>& in) const;

// For traiditional RM approach, give output a mask if possible
   Bool dealWithMask (Lattice<Bool>*& pMask, ImageInterface<Float>*& pIm, LogIO& os, const String& type) const;

// Change the Stokes Coordinate for the given float image to be of the specified Stokes type
   void fiddleStokesCoordinate(ImageInterface<Float>& ie, Stokes::StokesTypes type) const;
   void fiddleStokesCoordinate(CoordinateSystem& cSys, Stokes::StokesTypes type) const;

// Change the Stokes Coordinate for the given complex image to be of the specified Stokes type
   void fiddleStokesCoordinate(ImageInterface<Complex>& ie, Stokes::StokesTypes type) const;

// Change the time coordinate to be rotation measure
   void fiddleTimeCoordinate(ImageInterface<Complex>& ie, const Quantum<Double>& f, Int coord) const;

// Find the central frequency from the given spectral coordinate
   Quantum<Double> findCentralFrequency(const Coordinate& coord, Int shape) const;

// Fit the spectrum of position angles to find the rotation measure via Leahy algorithm
   Bool findRotationMeasure (Float& rmFitted, Float& rmErrFitted,
                             Float& pa0Fitted, Float& pa0ErrFitted, Float& rChiSqFitted, 
                             Float& nTurns,
                             const Vector<uInt>& sortidx, const Vector<Float>& wsq, 
                             const Vector<Float>& pa, 
                             const Array<Bool>& paMask, 
                             const Array<Float>& paerr, 
                             Float rmfg, Float rmmax, Float paErrMax,
                             PGPlotter& plotter, const String& posString);

// Find the Stokes in the construction image and assign pointers
   void findStokes();

// Find the spectral coordinate. 
   Int findSpectralCoordinate(const CoordinateSystem& cSys, LogIO& os, Bool fail) const;

// FInd frequency axis
   void findFrequencyAxis (Int& spectralCoord, Int& fAxis,
                           const CoordinateSystem& cSys, Int spectralAxis) const;
// So we have Q and U ?  Excpetion if not
   void hasQU () const;

// Make a LEN for the give types of polarized intensity
   LatticeExprNode makePolIntNode(LogIO& os, Bool debias, Float clip, Float sigma,
                                  Bool doLin, Bool doCirc);

// Make an IE for the specified Stokes
   ImageExpr<Float> makeStokesExpr(ImageInterface<Float>* imPtr,
                                   Stokes::StokesTypes type, const String& name) const;

// Make a SubImage from the construction image for the specified pixel
// along the specified pixel axis
   ImageInterface<Float>* makeSubImage (IPosition& blc, IPosition& trc,
                                        Int axis, Int pix) const;

// Least squares fit to find RM from position angles
   Bool rmLsqFit (Vector<Float>& pars, const Vector<Float>& wsq, 
                  const Vector<Float> pa, const Vector<Float>& paerr) const;

// Fit the spectrum of position angles to find the rotation measure via Leahy algorithm
// for primary (n>2) points
   Bool rmPrimaryFit (Float& nTurns, Float& rmFitted, Float& rmErrFitted,
                      Float& pa0Fitted, Float& pa0ErrFitted,
                      Float& rChiSqFitted, const Vector<Float>& wsq, 
                      const Vector<Float>& pa, const Vector<Float>& paerr, 
                      Float rmmax, PGPlotter& plotter, const String& posString);

// Fit the spectrum of position angles to find the rotation measure via Leahy algorithm
// for supplementary (n==2) points
   Bool rmSupplementaryFit (Float& nTurns, Float& rmFitted, Float& rmErrFitted,
                            Float& pa0Fitted, Float& pa0ErrFitted,
                            Float& rChiSqFitted, const Vector<Float>& wsq, 
                            const Vector<Float>& pa, const Vector<Float>& paerr);

// Return I, Q, U or V for specified integer index (0-3)
   String stokesName (ImagePolarimetry::StokesTypes index) const;

// Return I, Q, U or V for specified integer index (0-3)
   Stokes::StokesTypes stokesType (ImagePolarimetry::StokesTypes index) const;

// Find the standard deviation for the Stokes image specified by the integer index
   Float sigma (ImagePolarimetry::StokesTypes index, Float clip);

// Subtract profile mean from image
   void subtractProfileMean (ImageInterface<Float>& im, uInt axis) const;

};


} //# NAMESPACE CASA - END

#endif
