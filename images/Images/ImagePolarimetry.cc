//# ImagePolarimetry.cc: polarimetric analysis
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
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

#include <casa/OS/Timer.h>

#include <images/Images/ImagePolarimetry.h>

#include <casa/Arrays/Array.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/MaskedArray.h>
#include <casa/Arrays/MaskArrMath.h>
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <coordinates/Coordinates/StokesCoordinate.h>
#include <coordinates/Coordinates/LinearCoordinate.h>
#include <casa/Exceptions/Error.h>
#include <scimath/Functionals/Polynomial.h>
#include <images/Images/ImageInterface.h>
#include <images/Images/SubImage.h>
#include <images/Images/ImageExpr.h>
#include <images/Images/ImageFFT.h>
#include <images/Regions/ImageRegion.h>
#include <images/Images/ImageSummary.h>
#include <images/Images/TempImage.h>
#include <lattices/Lattices/Lattice.h>
#include <lattices/Lattices/LCSlicer.h>
#include <lattices/Lattices/LatticeExprNode.h>
#include <lattices/Lattices/LatticeExpr.h>
#include <lattices/Lattices/TiledLineStepper.h>
#include <lattices/Lattices/LatticeStepper.h>
#include <lattices/Lattices/LatticeIterator.h>
#include <lattices/Lattices/MaskedLatticeIterator.h>
#include <lattices/Lattices/LatticeStatistics.h>
#include <lattices/Lattices/LCPagedMask.h>
#include <casa/Logging/LogIO.h>
#include <casa/Logging/LogOrigin.h>
#include <casa/BasicMath/Math.h>
#include <casa/BasicSL/Constants.h>
#include <scimath/Mathematics/NumericTraits.h>
#include <casa/System/PGPlotter.h>
#include <casa/System/ProgressMeter.h>
#include <casa/Quanta/QC.h>
#include <casa/Quanta/MVAngle.h>
#include <casa/Utilities/GenSort.h>
#include <casa/Utilities/Assert.h>
#include <casa/BasicSL/String.h>

#include <casa/sstream.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// Public functions

ImagePolarimetry::ImagePolarimetry (const ImageInterface<Float>& image)
: itsFitterPtr(0),
  itsOldClip(0.0)
{
   itsInImagePtr = image.cloneII();
//
   itsStokesPtr.resize(4);
   itsStokesStatsPtr.resize(4);
   itsStokesPtr.set(0);
   itsStokesStatsPtr.set(0);
   findStokes();
}


ImagePolarimetry::ImagePolarimetry(const ImagePolarimetry &other) 
{
   operator=(other);
}


ImagePolarimetry &ImagePolarimetry::operator=(const ImagePolarimetry &other)
{
   if (this != &other) {
      if (itsInImagePtr!= 0) delete itsInImagePtr;
      itsInImagePtr = other.itsInImagePtr->cloneII();
//
      const uInt n = itsStokesPtr.nelements();
      for (uInt i=0; i<n; i++) {
         if (itsStokesPtr[i]!=0) {
            delete itsStokesPtr[i];
            itsStokesPtr[i] = 0;
         }
         if (other.itsStokesPtr[i]!=0) {
            itsStokesPtr[i] = other.itsStokesPtr[i]->cloneII();
         }
      }

// Just delete fitter. It will make a new one when needed.

      if (itsFitterPtr!= 0) {
         delete itsFitterPtr;
         itsFitterPtr = 0;
      }

// Remake Statistics objects as needed

      itsOldClip = 0.0;
      for (uInt i=0; i<n; i++) {
         if (itsStokesStatsPtr[i]!=0) {
            delete itsStokesStatsPtr[i];
            itsStokesStatsPtr[i] = 0;
         }
      }
   }
   return *this;
}


ImagePolarimetry::~ImagePolarimetry()
{
   cleanup();
}


// Public methods

ImageExpr<Complex> ImagePolarimetry::complexLinearPolarization()
{
   hasQU();
   LatticeExprNode node(formComplex(*itsStokesPtr[ImagePolarimetry::Q], 
                        *itsStokesPtr[ImagePolarimetry::U]));
   LatticeExpr<Complex> le(node);
   ImageExpr<Complex> ie(le, String("ComplexLinearPolarization"));
   fiddleStokesCoordinate(ie, Stokes::Plinear);    // Need a Complex Linear Polarization type
//
   ie.setUnits(itsInImagePtr->units());
   ie.setImageInfo(itsInImagePtr->imageInfo());
//
   return ie;
}

ImageExpr<Complex> ImagePolarimetry::complexFractionalLinearPolarization()
{
   LogIO os(LogOrigin("ImagePolarimetry", "complexFractionalLinearPolarization(...)", WHERE));
   hasQU();
   if (itsStokesPtr[ImagePolarimetry::I]==0) {
      os << "This image does not have Stokes I so cannot provide fractional linear polarization" << LogIO::EXCEPTION;
   }
//
   LatticeExprNode nodeQU(formComplex(*itsStokesPtr[ImagePolarimetry::Q], 
                          *itsStokesPtr[ImagePolarimetry::U]));
   LatticeExprNode nodeI(*itsStokesPtr[ImagePolarimetry::I]);
   LatticeExpr<Complex> le(nodeQU/nodeI);
   ImageExpr<Complex> ie(le, String("ComplexFractionalLinearPolarization"));
   fiddleStokesCoordinate(ie, Stokes::PFlinear);    // Need a Complex Linear Polarization type
//
   ie.setUnits(Unit(""));
   ImageInfo ii = itsInImagePtr->imageInfo();
//   ii.removeRestoringBeam();
   ie.setImageInfo(ii);
//
   return ie;
}

ImageExpr<Float> ImagePolarimetry::fracLinPol(Bool debias, Float clip, Float sigma) 
{
   LogIO os(LogOrigin("ImagePolarimetry", "fracLinPol(...)", WHERE));
   hasQU();
   if (itsStokesPtr[ImagePolarimetry::I]==0) {
      os << "This image does not have Stokes I so cannot provide fractional linear polarization" << LogIO::EXCEPTION;
   }

// Make nodes

   LatticeExprNode nodePol = makePolIntNode(os, debias, clip, sigma, True, False);
   LatticeExprNode nodeI(*itsStokesPtr[ImagePolarimetry::I]);

// Make expression

   LatticeExpr<Float> le(nodePol/nodeI);
   ImageExpr<Float> ie(le, String("FractionalLinearPolarization"));
//
   ie.setUnits(Unit(""));
   ImageInfo ii = itsInImagePtr->imageInfo();
   ii.removeRestoringBeam();
   ie.setImageInfo(ii);

// Fiddle Stokes coordinate in ImageExpr

   fiddleStokesCoordinate(ie, Stokes::PFlinear);
//
   return ie;
}


ImageExpr<Float> ImagePolarimetry::sigmaFracLinPol(Float clip, Float sigma) 
//
// sigma_m = m * sqrt( (sigmaP/p)**2 + (sigmaI/I)**2) )
// sigmaP = sigmaQU
// sigmaI = sigmaI
//
{
   LogIO os(LogOrigin("ImagePolarimetry", "sigmaFracLinPol(...)", WHERE));
   hasQU();
   if (itsStokesPtr[ImagePolarimetry::I]==0) {
      os << "This image does not have Stokes I so cannot provide fractional linear polarization" << LogIO::EXCEPTION;
   }

// Make nodes.  Don't bother debiasing.

   Bool debias = False;
   LatticeExprNode nodePol = makePolIntNode(os, debias, clip, sigma, True, False);
   LatticeExprNode nodeI(*itsStokesPtr[ImagePolarimetry::I]);

// Make expression.  We assume sigmaI = sigmaQU which is true with
// no dynamic range limititation.  Perhaps we should work out
// sigmaI as well.

   Float sigma2 = sigmaLinPolInt(clip, sigma);
   LatticeExprNode n0(nodePol / nodeI);
   LatticeExprNode n1(pow(sigma2/nodePol,2));
   LatticeExprNode n2(pow(sigma2/nodeI,2));
   LatticeExpr<Float> le(n0 * sqrt(n1 + n2));
   ImageExpr<Float> ie(le, String("FractionalLinearPolarizationError"));
//
   ie.setUnits(Unit(""));
   ImageInfo ii = itsInImagePtr->imageInfo();
   ii.removeRestoringBeam();
   ie.setImageInfo(ii);

// Fiddle Stokes coordinate in ImageExpr

   fiddleStokesCoordinate(ie, Stokes::PFlinear);
//
   return ie;
}


ImageExpr<Float> ImagePolarimetry::fracTotPol(Bool debias, Float clip, Float sigma)
{
   LogIO os(LogOrigin("ImagePolarimetry", "fracTotPol(...)", WHERE));
//
   Bool doLin = (itsStokesPtr[ImagePolarimetry::Q]!=0 && 
                 itsStokesPtr[ImagePolarimetry::U]!=0);
   Bool doCirc = (itsStokesPtr[ImagePolarimetry::V]!=0);
   AlwaysAssert((doLin||doCirc), AipsError);    // Should never happen
   if (itsStokesPtr[ImagePolarimetry::I]==0) {
      os << "This image does not have Stokes I so cannot provide fractional linear polarization" 
         << LogIO::EXCEPTION;
   }

// Make nodes

   LatticeExprNode nodePol = makePolIntNode(os, debias, clip, sigma, doLin, doCirc);
   LatticeExprNode nodeI(*itsStokesPtr[ImagePolarimetry::I]);

// Make expression

   LatticeExpr<Float> le(nodePol/nodeI);
   ImageExpr<Float> ie(le, String("FractionalTotalPolarization"));
//
   ie.setUnits(Unit(""));
   ImageInfo ii = itsInImagePtr->imageInfo();
   ii.removeRestoringBeam();
   ie.setImageInfo(ii);

// Fiddle Stokes coordinate in ImageExpr

   fiddleStokesCoordinate(ie, Stokes::PFtotal);
//
   return ie;
}

ImageExpr<Float> ImagePolarimetry::sigmaFracTotPol(Float clip, Float sigma) 
//
// sigma_m = m * sqrt( (sigmaP/P)**2 + (sigmaI/I)**2) )
// sigmaP = sigmaQU
// sigmaI = sigmaI
//
{
   LogIO os(LogOrigin("ImagePolarimetry", "sigmaFracTotPol(...)", WHERE));
   Bool doLin = (itsStokesPtr[ImagePolarimetry::Q]!=0 && 
                 itsStokesPtr[ImagePolarimetry::U]!=0);
   Bool doCirc = (itsStokesPtr[ImagePolarimetry::V]!=0);
   AlwaysAssert((doLin||doCirc), AipsError);    // Should never happen
   if (itsStokesPtr[ImagePolarimetry::I]==0) {
      os << "This image does not have Stokes I so cannot provide fractional linear polarization" 
         << LogIO::EXCEPTION;
   }

// Make nodes.  Don't bother debiasing.

   Bool debias = False;
   LatticeExprNode nodePol = makePolIntNode(os, debias, clip, sigma, doLin, doCirc);
   LatticeExprNode nodeI(*itsStokesPtr[ImagePolarimetry::I]);

// Make expression.  We assume sigmaI = sigmaQU which is true with
// no dynamic range limitation.  Perhaps we should work out
// sigmaI as well.

   Float sigma2 = sigmaTotPolInt(clip, sigma);
   LatticeExprNode n0(nodePol / nodeI);
   LatticeExprNode n1(pow(sigma2/nodePol,2));
   LatticeExprNode n2(pow(sigma2/nodeI,2));
   LatticeExpr<Float> le(n0 * sqrt(n1 + n2));
   ImageExpr<Float> ie(le, String("FractionalLinearPolarizationError"));
//
   ie.setUnits(Unit(""));
   ImageInfo ii = itsInImagePtr->imageInfo();
   ii.removeRestoringBeam();
   ie.setImageInfo(ii);

// Fiddle Stokes coordinate in ImageExpr

   fiddleStokesCoordinate(ie, Stokes::PFlinear);
//
   return ie;
}



void ImagePolarimetry::fourierRotationMeasure(ImageInterface<Complex>& cpol,
                                              Bool zeroZeroLag)
{
   LogIO os(LogOrigin("ImagePolarimetry", "fourierRotationMeasure(...)", WHERE));
   hasQU();

// Check image shape

   CoordinateSystem dCS;
   Stokes::StokesTypes dType = Stokes::Plinear;
   IPosition shape = singleStokesShape(dCS, dType);
   if (!cpol.shape().isEqual(shape)) {
      os << "The provided  image has the wrong shape " << cpol.shape() << endl;
      os << "It should be of shape " << shape  << LogIO::EXCEPTION;
   }

// Find spectral coordinate.  

   const CoordinateSystem& cSys = itsInImagePtr->coordinates();
   Int spectralCoord, fAxis;
   findFrequencyAxis (spectralCoord, fAxis, cSys, -1);

// Make Complex (Q,U) image

   LatticeExprNode node;
   if (zeroZeroLag) {
      TempImage<Float> tQ(itsStokesPtr[ImagePolarimetry::Q]->shape(), 
                          itsStokesPtr[ImagePolarimetry::Q]->coordinates());
      if (itsStokesPtr[ImagePolarimetry::Q]->isMasked()) {
        tQ.makeMask(String("mask0"), True, True, False, False);
      }
      copyDataAndMask (tQ, *itsStokesPtr[ImagePolarimetry::Q]);
      subtractProfileMean (tQ, fAxis);
//
      TempImage<Float> tU(itsStokesPtr[ImagePolarimetry::U]->shape(), 
                          itsStokesPtr[ImagePolarimetry::U]->coordinates());
      if (itsStokesPtr[ImagePolarimetry::U]->isMasked()) {
        tU.makeMask(String("mask0"), True, True, False, False);
      }
      copyDataAndMask (tU, *itsStokesPtr[ImagePolarimetry::U]);
      subtractProfileMean (tU, fAxis);

// The TempImages will be cloned be LatticeExprNode so it's ok
// that they go out of scope

      node = LatticeExprNode(formComplex(tQ, tU));
   } else {
      node = LatticeExprNode(formComplex(*itsStokesPtr[ImagePolarimetry::Q], 
					 *itsStokesPtr[ImagePolarimetry::U]));
   }
   LatticeExpr<Complex> le(node);
   ImageExpr<Complex> ie(le, String("ComplexLinearPolarization"));

// Do FFT of spectral coordinate

   Vector<Bool> axes(ie.ndim(),False);
   axes(fAxis) = True;
   ImageFFT fftserver;
   fftserver.fft(ie, axes);

// Recover Complex result. Coordinates are updated to include Fourier coordinate,
// miscellaneous things (MiscInfo, ImageInfo, units, history) and mask
// (if output has one) are copied to cpol

   fftserver.getComplex(cpol);

// Fiddle time coordinate to be a RotationMeasure coordinate

   Quantum<Double> f = findCentralFrequency(cSys.coordinate(spectralCoord), ie.shape()(fAxis));
   fiddleTimeCoordinate(cpol, f, spectralCoord);

// Set Stokes coordinate to be correct type

   fiddleStokesCoordinate(cpol, Stokes::Plinear);

// Set units and ImageInfo

   cpol.setUnits(itsInImagePtr->units());
   cpol.setImageInfo(itsInImagePtr->imageInfo());
}


ImageExpr<Float> ImagePolarimetry::linPolInt(Bool debias, Float clip, Float sigma) 
{
   LogIO os(LogOrigin("ImagePolarimetry", "linPolInt(...)", WHERE));
   if (itsStokesPtr[ImagePolarimetry::Q]==0 && itsStokesPtr[ImagePolarimetry::U]==0) {
      os << "This image does not have Stokes Q and U so cannot provide linear polarization" << LogIO::EXCEPTION;
   }

// Make node.  

   LatticeExprNode node = makePolIntNode(os, debias, clip, sigma, True, False);

// Make expression

   LatticeExpr<Float> le(node);
   ImageExpr<Float> ie(le, String("LinearlyPolarizedIntensity"));
//
   ie.setUnits(itsInImagePtr->units());
   ImageInfo ii = itsInImagePtr->imageInfo();
//   ii.removeRestoringBeam();
   ie.setImageInfo(ii);

// Fiddle Stokes coordinate in ImageExpr

   fiddleStokesCoordinate(ie, Stokes::Plinear);
//
   return ie;
}


Float ImagePolarimetry::sigmaLinPolInt(Float clip, Float sigma) 
//
// sigma_P = sigma_QU
//
{
   LogIO os(LogOrigin("ImagePolarimetry", "sigmaLinPolInt(...)", WHERE));
   if (itsStokesPtr[ImagePolarimetry::Q]==0 && itsStokesPtr[ImagePolarimetry::U]==0) {
      os << "This image does not have Stokes Q and U so cannot provide linear polarization" << LogIO::EXCEPTION;
   }

// Get value

   Float sigma2 = 0.0;
   if (sigma > 0) {
      sigma2 = sigma;
   } else {
      sigma2 = ImagePolarimetry::sigma(clip);
   }
   return sigma2;
}



ImageExpr<Float> ImagePolarimetry::linPolPosAng(Bool radians) const
{
   LogIO os(LogOrigin("ImagePolarimetry", "linPolPosAng(...)", WHERE));
   if (itsStokesPtr[ImagePolarimetry::Q]==0 && itsStokesPtr[ImagePolarimetry::U]==0) {
      os << "This image does not have Stokes Q and U so cannot provide linear polarization" << LogIO::EXCEPTION;
   }

// Make expression. LEL function "pa" returns degrees

   Float fac = 1.0;
   if (radians) fac = C::pi / 180.0;
   LatticeExprNode node(fac*pa(*itsStokesPtr[ImagePolarimetry::U], 
                               *itsStokesPtr[ImagePolarimetry::Q])); 
   LatticeExpr<Float> le(node);
   ImageExpr<Float> ie(le, String("LinearlyPolarizedPositionAngle"));
//
   if (radians) {
      ie.setUnits(Unit("rad"));
   } else {
      ie.setUnits(Unit("deg"));
   }
   ImageInfo ii = itsInImagePtr->imageInfo();
   ii.removeRestoringBeam();
   ie.setImageInfo(ii);

// Fiddle Stokes coordinate

   fiddleStokesCoordinate(ie, Stokes::Pangle);
//
   return ie;
}


ImageExpr<Float> ImagePolarimetry::sigmaLinPolPosAng(Bool radians, Float clip, Float sigma) 
//
// sigma_PA = sigmaQU / 2P 
//
{
   LogIO os(LogOrigin("ImagePolarimetry", "sigmaLinPolPosAng(...)", WHERE));
   if (itsStokesPtr[ImagePolarimetry::Q]==0 && itsStokesPtr[ImagePolarimetry::U]==0) {
      os << "This image does not have Stokes Q and U so cannot provide linear polarization" << LogIO::EXCEPTION;
   }

// Make expression 

   Float sigma2 = 0.0;
   if (sigma > 0) {
      sigma2 = sigma;
   } else {
      sigma2 = ImagePolarimetry::sigma(clip);
   }
   Float fac = 0.5 * sigma2;
   if (!radians) fac *= 180 / C::pi;
   LatticeExprNode node(fac / 
      amp(*itsStokesPtr[ImagePolarimetry::U], *itsStokesPtr[ImagePolarimetry::Q])); 
   LatticeExpr<Float> le(node);
   ImageExpr<Float> ie(le, String("LinearlyPolarizedPositionAngleError"));
//
   if (radians) {
      ie.setUnits(Unit("rad"));
   } else {
      ie.setUnits(Unit("deg"));
   }
   ImageInfo ii = itsInImagePtr->imageInfo();
   ii.removeRestoringBeam();
   ie.setImageInfo(ii);

// Fiddle Stokes coordinate

   fiddleStokesCoordinate(ie, Stokes::Pangle);
//
   return ie;
}

Float ImagePolarimetry::sigma(Float clip)
{
   LogIO os(LogOrigin("ImagePolarimetry", "noise(...)", WHERE));
   Float sigma2 = 0.0;
   if (itsStokesPtr[ImagePolarimetry::V]!=0) {
      os << LogIO::NORMAL << "Determined noise from V image to be ";
      sigma2 = ImagePolarimetry::sigma(ImagePolarimetry::V, clip);
   } else if (itsStokesPtr[ImagePolarimetry::Q]!=0 &&
              itsStokesPtr[ImagePolarimetry::U]!=0) {
      os << LogIO::NORMAL << "Determined noise from Q&U images to be ";
      Float sq = ImagePolarimetry::sigma(ImagePolarimetry::Q, clip);
      Float su = ImagePolarimetry::sigma(ImagePolarimetry::U, clip);
      sigma2 = (sq+su)/2.0;
   } else if (itsStokesPtr[ImagePolarimetry::Q]!=0) {
      os << LogIO::NORMAL << "Determined noise from Q image to be " << LogIO::POST;
      sigma2 = ImagePolarimetry::sigma(ImagePolarimetry::Q, clip);
   } else if (itsStokesPtr[ImagePolarimetry::U]!=0) {
      os << LogIO::NORMAL << "Determined noise from U image to be " << LogIO::POST;
      sigma2 = ImagePolarimetry::sigma(ImagePolarimetry::U, clip);
   } else if (itsStokesPtr[ImagePolarimetry::I]!=0) {
      os << LogIO::NORMAL << "Determined noise from I image to be " << LogIO::POST;
      sigma2 = ImagePolarimetry::sigma(ImagePolarimetry::I, clip);
   }
   os << sigma2 << LogIO::POST;
   return sigma2;
}



void ImagePolarimetry::rotationMeasure(ImageInterface<Float>*& rmOutPtr, 
                                       ImageInterface<Float>*& rmOutErrorPtr,
                                       ImageInterface<Float>*& pa0OutPtr, 
                                       ImageInterface<Float>*& pa0OutErrorPtr,
                                       ImageInterface<Float>*& nTurnsOutPtr, 
                                       ImageInterface<Float>*& chiSqOutPtr,
                                       PGPlotter& plotter,
                                       Int axis,  Float rmMax, Float maxPaErr,
                                       Float sigma, Float rmFg, Bool showProgress)
{
   LogIO os(LogOrigin("ImagePolarimetry", "rotationMeasure(...)", WHERE));
   hasQU();

// Do we have anything to do ?

   if (!rmOutPtr && !rmOutErrorPtr && !pa0OutPtr && !pa0OutErrorPtr) {
      os << "No output images specified" << LogIO::EXCEPTION;
   }

// Find expected shape of output RM images (Stokes and spectral axes gone)

   CoordinateSystem cSysRM;
   Int fAxis, sAxis;
   IPosition shapeRM = rotationMeasureShape(cSysRM, fAxis, sAxis, os, axis);
   IPosition shapeNTurns = shapeRM;
   IPosition shapeChiSq = shapeRM;

// Check RM image shapes

   if (rmOutPtr && !rmOutPtr->shape().isEqual(shapeRM)) {
      os << "The provided Rotation Measure image has the wrong shape " << rmOutPtr->shape() << endl;
      os << "It should be of shape " << shapeRM << LogIO::EXCEPTION;
   }
   if (rmOutErrorPtr && !rmOutErrorPtr->shape().isEqual(shapeRM)) {
      os << "The provided Rotation Measure error image has the wrong shape " << rmOutErrorPtr->shape() << endl;
      os << "It should be of shape " << shapeRM << LogIO::EXCEPTION;
   }

// Check position angle image shapes

   CoordinateSystem cSysPA;
   IPosition shapePA = positionAngleShape(cSysPA, fAxis, sAxis, os, axis);
   if (pa0OutPtr && !pa0OutPtr->shape().isEqual(shapePA)) {
      os << "The provided position angle at zero wavelength image has the wrong shape " << pa0OutPtr->shape() << endl;
      os << "It should be of shape " << shapePA << LogIO::EXCEPTION;
   }
   if (pa0OutErrorPtr && !pa0OutErrorPtr->shape().isEqual(shapePA)) {
      os << "The provided position angle at zero wavelength image has the wrong shape " << pa0OutErrorPtr->shape() << endl;
      os << "It should be of shape " << shapePA << LogIO::EXCEPTION;
   }

// nTurns and chi sq

   if (nTurnsOutPtr && !nTurnsOutPtr->shape().isEqual(shapeNTurns)) {
      os << "The provided nTurns image has the wrong shape " << nTurnsOutPtr->shape() << endl;
      os << "It should be of shape " << shapeNTurns << LogIO::EXCEPTION;
   }
   if (chiSqOutPtr && !chiSqOutPtr->shape().isEqual(shapeChiSq)) {
      os << "The provided chi squared image has the wrong shape " << chiSqOutPtr->shape() << endl;
      os << "It should be of shape " << shapeChiSq << LogIO::EXCEPTION;
   }

// Generate linear polarization position angle image expressions
// and error in radians

   Bool radians = True;
   Float clip = 10.0;
   ImageExpr<Float> pa = linPolPosAng(radians);
   ImageExpr<Float> paerr = sigmaLinPolPosAng(radians, clip, sigma);
   CoordinateSystem cSys0 = pa.coordinates();

// Set frequency axis units to Hz

   Int fAxisWorld = cSys0.pixelAxisToWorldAxis(fAxis);
   if (fAxisWorld <0) {
      os << "World axis has been removed for the frequency pixel axis" << LogIO::EXCEPTION;
   }
//
   Vector<String> axisUnits = cSys0.worldAxisUnits();
   axisUnits(fAxisWorld) = String("Hz");
   if (!cSys0.setWorldAxisUnits(axisUnits)) {
      os << "Failed to set frequency axis units to Hz because " 
         << cSys0.errorMessage() << LogIO::EXCEPTION;
   }

// Do we have enough frequency pixels ?

   const uInt nFreq = pa.shape()(fAxis);
   if (nFreq < 3) {
      os << "This image only has " << nFreq << "frequencies, this is not enough"
         << LogIO::EXCEPTION;
   }

// Copy units only over.  The output images don't have a beam 
// so unset beam.   MiscInfo and history require writable II.  
// We leave this to the caller  who knows what sort of II these are.

   ImageInfo ii = itsInImagePtr->imageInfo();
   ii.removeRestoringBeam();   
   if (rmOutPtr) {
      rmOutPtr->setImageInfo(ii);
      rmOutPtr->setUnits(Unit("rad/m/m"));
   }
   if (rmOutErrorPtr) {
      rmOutErrorPtr->setImageInfo(ii);
      rmOutErrorPtr->setUnits(Unit("rad/m/m"));
   }      
   if (pa0OutPtr) {
      pa0OutPtr->setImageInfo(ii);
      pa0OutPtr->setUnits(Unit("deg"));
   }
   if (pa0OutErrorPtr) {
      pa0OutErrorPtr->setImageInfo(ii);
      pa0OutErrorPtr->setUnits(Unit("deg"));
   }   
   if (nTurnsOutPtr) {
      nTurnsOutPtr->setImageInfo(ii);
      nTurnsOutPtr->setUnits(Unit(""));
   }      
   if (chiSqOutPtr) {
      chiSqOutPtr->setImageInfo(ii);
      chiSqOutPtr->setUnits(Unit(""));
   }      

// Get lambda squared in m**2

   Vector<Double> freqs(nFreq);
   Vector<Float> wsq(nFreq);
   Vector<Double> world;
   Vector<Double> pixel(cSys0.referencePixel().copy());
   Double c = QC::c.getValue(Unit("m/s"));
   Double csq = c*c;
   for (uInt i=0; i<nFreq; i++) {
      pixel(fAxis) = i;
      if (!cSys0.toWorld(world, pixel)) {
         os << "Failed to convert pixel to world because " 
         << cSys0.errorMessage() << LogIO::EXCEPTION;
      }
      freqs(i) = world(fAxisWorld);
      wsq(i) = csq / freqs(i) / freqs(i);     // m**2
   }

// Sort into increasing wavelength 

   Vector<uInt> sortidx;
   GenSortIndirect<Float>::sort (sortidx, wsq, Sort::Ascending, Sort::QuickSort|Sort::NoDuplicates);
   Vector<Float> wsqsort(sortidx.nelements());
   for (uInt i=0; i<wsqsort.nelements(); i++) wsqsort(i) = wsq(sortidx(i));

// Make fitter

   if (itsFitterPtr==0) {
      itsFitterPtr = new LinearFitSVD<Float>;

// Create and set the polynomial functional
// p = c(0) + c(1)*x where x = lambda**2
// PA = PA0 + RM*Lambda**2

      Polynomial<AutoDiff<Float> > poly1(1);

// Makes a copy of poly1

      itsFitterPtr->setFunction(poly1);
   }

// Deal with masks.  The outputs are all given a mask if possible as we
// don't know at this point whether output points will be masked or not

   IPosition whereRM;
   Bool isMaskedRM = False;
   Lattice<Bool>* outRMMaskPtr = 0;
   if (rmOutPtr) {
      isMaskedRM = dealWithMask (outRMMaskPtr, rmOutPtr, os, String("RM"));
      whereRM.resize(rmOutPtr->ndim());
      whereRM = 0;
   }
//
   Bool isMaskedRMErr = False;
   Lattice<Bool>* outRMErrMaskPtr = 0;
   if (rmOutErrorPtr) {
      isMaskedRMErr = dealWithMask (outRMErrMaskPtr, rmOutErrorPtr, os, String("RM error"));
      whereRM.resize(rmOutErrorPtr->ndim());
      whereRM = 0;
   }
//
   IPosition wherePA;
   Bool isMaskedPa0 = False;
   Lattice<Bool>* outPa0MaskPtr = 0;
   if (pa0OutPtr) {
      isMaskedPa0 = dealWithMask (outPa0MaskPtr, pa0OutPtr, os, String("Position Angle"));
      wherePA.resize(pa0OutPtr->ndim());
      wherePA = 0;
   }
//
   Bool isMaskedPa0Err = False;
   Lattice<Bool>* outPa0ErrMaskPtr = 0;
   if (pa0OutErrorPtr) {
      isMaskedPa0Err = dealWithMask (outPa0ErrMaskPtr, pa0OutErrorPtr, os, String("Position Angle error"));
      wherePA.resize(pa0OutErrorPtr->ndim());
      wherePA = 0;
   }
//
   IPosition whereNTurns;
   Bool isMaskedNTurns = False;
   Lattice<Bool>* outNTurnsMaskPtr = 0;
   if (nTurnsOutPtr) {
      isMaskedNTurns = dealWithMask (outNTurnsMaskPtr, nTurnsOutPtr, os, String("nTurns"));
      whereNTurns.resize(nTurnsOutPtr->ndim());
      whereNTurns = 0;
   }
//
   IPosition whereChiSq;
   Bool isMaskedChiSq = False;
   Lattice<Bool>* outChiSqMaskPtr = 0;
   if (chiSqOutPtr) {
      isMaskedChiSq = dealWithMask (outChiSqMaskPtr, chiSqOutPtr, os, String("chi sqared"));
      whereChiSq.resize(nTurnsOutPtr->ndim());
      whereChiSq = 0;
   }
//
   Array<Bool> tmpMaskRM(IPosition(shapeRM.nelements(), 1), True);
   Array<Float> tmpValueRM(IPosition(shapeRM.nelements(), 1), 0.0);
   Array<Bool> tmpMaskPA(IPosition(shapePA.nelements(), 1), True);
   Array<Float> tmpValuePA(IPosition(shapePA.nelements(), 1), 0.0);
   Array<Float> tmpValueNTurns(IPosition(shapeNTurns.nelements(), 1), 0.0);
   Array<Bool> tmpMaskNTurns(IPosition(shapeNTurns.nelements(), 1), True);
   Array<Float> tmpValueChiSq(IPosition(shapeChiSq.nelements(), 1), 0.0);
   Array<Bool> tmpMaskChiSq(IPosition(shapeChiSq.nelements(), 1), True);

// Iterate

   const IPosition tileShape = pa.niceCursorShape();
   TiledLineStepper ts(pa.shape(), tileShape, fAxis);
   RO_MaskedLatticeIterator<Float> it(pa, ts);
//
   Float rm, rmErr, pa0, pa0Err, rChiSq, nTurns;
   uInt j, k, l, m;
//
   maxPaErr *= C::pi / 180.0;
   maxPaErr = abs(maxPaErr);
   Bool doRM = whereRM.nelements() > 0;
   Bool doPA = wherePA.nelements() > 0;
   Bool doNTurns = whereNTurns.nelements() > 0;
   Bool doChiSq = whereChiSq.nelements() > 0;
//
   ProgressMeter* pProgressMeter = 0;   
   if (showProgress) {
     Double nMin = 0.0;
     Double nMax = 1.0;
     for (Int i=0; i<Int(pa.ndim()); i++) {
        if (i!=fAxis) nMax *= pa.shape()(i);
     }
     pProgressMeter = new ProgressMeter(nMin, nMax, String("Profiles fitted"),
                                        String("Fitting"),
                                        String(""), String(""),
                                        True, max(1,Int(nMax/100)));
   }

// As a (temporary?) workaround the cache of the main image is set up in
// such a way that it can hold the full frequency and stokes axes.
// The stokes axis is important, otherwise the cache is set up
// (by the TiledStMan) such that it can hold only 1 stokes
// with the result that iterating is tremendously slow.
// We also need to cast the const away from itsInImagePtr.

   const IPosition mainShape = itsInImagePtr->shape();
   uInt nrtiles = (1 + (mainShape(fAxis)-1) / tileShape(fAxis)) *
                  (1 + (mainShape(sAxis)-1) / tileShape(sAxis));
   ImageInterface<Float>* mainImagePtr =
                          const_cast<ImageInterface<Float>*>(itsInImagePtr);
   mainImagePtr->setCacheSizeInTiles (nrtiles);
//
   String posString;
   Bool ok = False;
   IPosition shp;
   for (it.reset(); !it.atEnd(); it++) {

// Find rotation measure for this line

      if (plotter.isAttached()) {
         ostringstream oss;
         oss << it.position() + 1;
         posString = String(oss);
      }
      ok = findRotationMeasure (rm, rmErr, pa0, pa0Err, rChiSq, nTurns,
                                sortidx, wsqsort, it.vectorCursor(),
                                it.getMask(False),
                                paerr.getSlice(it.position(),it.cursorShape()),
                                rmFg, rmMax, maxPaErr, plotter, posString);

// Plonk values into output  image.  This is slow and clunky, but should be relatively fast
// c.f. the fitting.  Could be reimplemented with LatticeApply if need be.  Buffering 
// is hard because the navigator doesn't take a regular path.  If I used a LatticeStepper
// instead, the path would be regular and then I could buffer, but then the iteration 
// would be less efficient !!!

      j = k = l = m = 0;
      for (Int i=0; i<Int(it.position().nelements()); i++) {
         if (doRM && i!=fAxis && i!=sAxis) {
            whereRM(j) = it.position()(i);
            j++;
         }
         if (doPA && i!=fAxis) {
            wherePA(k) = it.position()(i);
            k++;
         }
         if (doNTurns && i!=fAxis && i!=sAxis) {
            whereNTurns(l) = it.position()(i);
            l++;
         }
         if (doChiSq && i!=fAxis && i!=sAxis) {
            whereChiSq(m) = it.position()(i);
            m++;
         }
      }
//
      if (isMaskedRM) {
         tmpMaskRM.set(ok);
         outRMMaskPtr->putSlice (tmpMaskRM, whereRM);
      }
      if (isMaskedRMErr) {
         tmpMaskRM.set(ok);
         outRMErrMaskPtr->putSlice (tmpMaskRM, whereRM);
      }
      if (isMaskedPa0) {
         tmpMaskPA.set(ok);
         outPa0MaskPtr->putSlice (tmpMaskPA, wherePA);
      }
      if (isMaskedPa0Err) {
         tmpMaskPA.set(ok);
         outPa0ErrMaskPtr->putSlice (tmpMaskPA, wherePA);
      }
      if (isMaskedNTurns) {
         tmpMaskNTurns.set(ok);
         outNTurnsMaskPtr->putSlice (tmpMaskNTurns, whereNTurns);
      }
      if (isMaskedChiSq) {
         tmpMaskChiSq.set(ok);
         outChiSqMaskPtr->putSlice (tmpMaskChiSq, whereChiSq);
      }

// If the output value is masked, the value itself is 0

      if (rmOutPtr) {
         tmpValueRM.set(rm);
         rmOutPtr->putSlice(tmpValueRM, whereRM);
      }
      if (rmOutErrorPtr) {
         tmpValueRM.set(rmErr);
         rmOutErrorPtr->putSlice(tmpValueRM, whereRM);
      }

// Position angles in degrees

      if (pa0OutPtr) {
         tmpValuePA.set(pa0*180/C::pi);
         pa0OutPtr->putSlice(tmpValuePA, wherePA);
      }
//
      if (pa0OutErrorPtr) {
         tmpValuePA.set(pa0Err*180/C::pi);
         pa0OutErrorPtr->putSlice(tmpValuePA, wherePA);
      }

// Number of turns and chi sq

      if (nTurnsOutPtr) {
         tmpValueNTurns.set(nTurns);
         nTurnsOutPtr->putSlice(tmpValueNTurns, whereNTurns);
      }
      if (chiSqOutPtr) {
         tmpValueChiSq.set(rChiSq);
         chiSqOutPtr->putSlice(tmpValueChiSq, whereChiSq);
      }
//
      if (showProgress) pProgressMeter->update(Double(it.nsteps())); 
   }
   if (showProgress) delete pProgressMeter;


// Clear the cache of the main image again.
   mainImagePtr->clearCache();
}

IPosition ImagePolarimetry::rotationMeasureShape(CoordinateSystem& cSys, Int& fAxis, 
                                                 Int& sAxis, LogIO&, Int spectralAxis) const
{

// Construction image CS

   CoordinateSystem cSys0 = coordinates();

// Find frequency axis

   Int spectralCoord;
   findFrequencyAxis (spectralCoord, fAxis, cSys0, spectralAxis);

// Find Stokes axis (we know it has one)

   Int afterCoord = -1;
   Int stokesCoord = cSys0.findCoordinate(Coordinate::STOKES, afterCoord);
   Vector<Int> pixelAxes = cSys0.pixelAxes(stokesCoord);
   sAxis = pixelAxes(0);

// What shape should the image be ?  Frequency and stokes axes should be gone.

   IPosition shape0 = ImagePolarimetry::shape();
   IPosition shape(shape0.nelements()-2);
//
   Int j = 0;
   for (Int i=0; i<Int(shape0.nelements()); i++) {
      if (i!=fAxis && i!=sAxis) {
        shape(j) = shape0(i);
        j++;
      }
   }

// Create output coordinate system

   CoordinateSystem tmp;
   cSys = tmp;
   for (Int i=0;i<Int(cSys0.nCoordinates()); i++) {
      if (i!=spectralCoord && i!=stokesCoord) {
         cSys.addCoordinate(cSys0.coordinate(i));
      }
   }
//
   return shape;
}


IPosition ImagePolarimetry::positionAngleShape(CoordinateSystem& cSys, 
                                               Int& fAxis, Int& sAxis, LogIO&, Int spectralAxis) const
{

// Construction image CS

   CoordinateSystem cSys0 = coordinates();

// Find frequency axis

   Int spectralCoord = -1;
   findFrequencyAxis (spectralCoord, fAxis, cSys0, spectralAxis);

// Find Stokes axis (we know it has one)

   Int afterCoord = -1;
   Int stokesCoord = cSys0.findCoordinate(Coordinate::STOKES, afterCoord);
   Vector<Int> pixelAxes = cSys0.pixelAxes(stokesCoord);
   sAxis = pixelAxes(0);

// Fiddle StokesCoordinate

   fiddleStokesCoordinate(cSys0, Stokes::Pangle);

// Create output coordinate system

   CoordinateSystem tmp;
   cSys = tmp;
   for (Int i=0;i<Int(cSys0.nCoordinates()); i++) {
      if (i!=spectralCoord) {
         cSys.addCoordinate(cSys0.coordinate(i));
      }
   }

// What shape should the image be ?  Frequency axis should be gone.
// and Stokes length 1

   IPosition shape0 = ImagePolarimetry::shape();
   IPosition shape(shape0.nelements()-1);
//
   Int j = 0;
   for (Int i=0; i<Int(shape0.nelements()); i++) {
      if (i==sAxis) {
         shape(j) = 1;
         j++;
      } else {         
        if (i!=fAxis) {
           shape(j) = shape0(i);
           j++;
        }
      }
   }
//
   return shape;
}


ImageExpr<Float> ImagePolarimetry::stokesI() const
{
   return makeStokesExpr(itsStokesPtr[ImagePolarimetry::I], Stokes::I, String("StokesI"));
}

Float ImagePolarimetry::sigmaStokesI(Float clip) 
{
   return ImagePolarimetry::sigma(ImagePolarimetry::I, clip);
}

ImageExpr<Float> ImagePolarimetry::stokesQ() const
{
   return makeStokesExpr(itsStokesPtr[ImagePolarimetry::Q], Stokes::Q, String("StokesQ"));
}

Float ImagePolarimetry::sigmaStokesQ(Float clip) 
{
   return ImagePolarimetry::sigma(ImagePolarimetry::Q, clip);
}

ImageExpr<Float> ImagePolarimetry::stokesU() const
{
   return makeStokesExpr(itsStokesPtr[ImagePolarimetry::U], Stokes::U, String("StokesU"));
}

Float ImagePolarimetry::sigmaStokesU(Float clip) 
{
   return ImagePolarimetry::sigma(ImagePolarimetry::U, clip);
}

ImageExpr<Float> ImagePolarimetry::stokesV() const
{
   return makeStokesExpr(itsStokesPtr[ImagePolarimetry::V], Stokes::V, String("StokesV"));
}

Float ImagePolarimetry::sigmaStokesV(Float clip) 
{
   return ImagePolarimetry::sigma(ImagePolarimetry::V, clip);
}

ImageExpr<Float> ImagePolarimetry::stokes(ImagePolarimetry::StokesTypes stokes) const
{
   Stokes::StokesTypes type = stokesType(stokes);
   return makeStokesExpr(itsStokesPtr[stokes], type, stokesName(stokes));
}

Float ImagePolarimetry::sigmaStokes(ImagePolarimetry::StokesTypes stokes, Float clip)
{
   return ImagePolarimetry::sigma(stokes, clip);
}

void ImagePolarimetry::summary(LogIO& os) const
{
   ImageSummary<Float> s(*itsInImagePtr);
   s.list(os);
}



ImageExpr<Float> ImagePolarimetry::totPolInt(Bool debias, Float clip, Float sigma) 
{
   LogIO os(LogOrigin("ImagePolarimetry", "totPolInt(...)", WHERE));
//
   Bool doLin = (itsStokesPtr[ImagePolarimetry::Q]!=0 && 
                 itsStokesPtr[ImagePolarimetry::U]!=0);
   Bool doCirc = (itsStokesPtr[ImagePolarimetry::V]!=0);
   AlwaysAssert((doLin||doCirc), AipsError);    // Should never happen

// Make node.  

   LatticeExprNode node = makePolIntNode(os, debias, clip, sigma, doLin, doCirc);

// Make expression

   LatticeExpr<Float> le(node);
   ImageExpr<Float> ie(le, String("totalPolarizedIntensity"));
   ie.setUnits(itsInImagePtr->units());     // Dodgy. The beam is now rectified
   ie.setImageInfo(itsInImagePtr->imageInfo());

// Fiddle Stokes coordinate in ImageExpr

   fiddleStokesCoordinate(ie, Stokes::Ptotal);
//
   return ie;
}


Float ImagePolarimetry::sigmaTotPolInt(Float clip, Float sigma) 
//
// sigma_P = sigma_QUV
//
{
   LogIO os(LogOrigin("ImagePolarimetry", "sigmaTotPolInt(...)", WHERE));
   Bool doLin = (itsStokesPtr[ImagePolarimetry::Q]!=0 && 
                 itsStokesPtr[ImagePolarimetry::U]!=0);
   Bool doCirc = (itsStokesPtr[ImagePolarimetry::V]!=0);
   AlwaysAssert((doLin||doCirc), AipsError);    // Should never happen

//
// Make expression 

   Float sigma2 = 0.0;
   if (sigma > 0) {
      sigma2 = sigma;
   } else {
      sigma2 = ImagePolarimetry::sigma(clip);
   }
//
   return sigma2;
}


IPosition ImagePolarimetry::singleStokesShape(CoordinateSystem& cSys, Stokes::StokesTypes type) const
{
// We know the image has a Stokes coordinate or it
// would have failed at construction

   CoordinateSystem cSys0 = itsInImagePtr->coordinates();
   fiddleStokesCoordinate(cSys0, type);   
   cSys = cSys0;
//
   Int afterCoord = -1;
   Int iStokes = cSys0.findCoordinate(Coordinate::STOKES, afterCoord);
   Vector<Int> pixelAxes = cSys0.pixelAxes(iStokes);
   IPosition shape = itsInImagePtr->shape();
   shape(pixelAxes(0)) = 1;
//
   return shape;
}


ImageExpr<Float> ImagePolarimetry::depolarizationRatio (const ImageInterface<Float>& im1, 
                                                        const ImageInterface<Float>& im2,
                                                        Bool debias, Float clip, Float sigma)
{
   ImagePolarimetry p1 = ImagePolarimetry(im1);
   ImagePolarimetry p2 = ImagePolarimetry(im2);
//
   ImageExpr<Float> m1(p1.fracLinPol(debias, clip, sigma));
   ImageExpr<Float> m2(p2.fracLinPol(debias, clip, sigma));
   LatticeExprNode n1(m1/m2);
   LatticeExpr<Float> le(n1);
   ImageExpr<Float> depol(le, String("DepolarizationRatio"));
   return depol;
}

ImageExpr<Float> ImagePolarimetry::sigmaDepolarizationRatio (const ImageInterface<Float>& im1, 
                                                             const ImageInterface<Float>& im2,
                                                             Bool debias, Float clip, Float sigma)
 {
   ImagePolarimetry p1 = ImagePolarimetry(im1);
   ImagePolarimetry p2 = ImagePolarimetry(im2);
//
   ImageExpr<Float> m1 = p1.fracLinPol(debias, clip, sigma);
   ImageExpr<Float> sm1 = p1.sigmaFracLinPol (clip, sigma);
//
   ImageExpr<Float> m2 = p2.fracLinPol(debias, clip, sigma);
   ImageExpr<Float> sm2 = p2.sigmaFracLinPol (clip, sigma);
//
   LatticeExprNode n0(m1/m2);
   LatticeExprNode n1(sm1*sm1/m1/m1);   
   LatticeExprNode n2(sm2*sm2/m2/m2);
   LatticeExprNode n3(n0 * sqrt(n1+n2));
   LatticeExpr<Float> le(n3);
   ImageExpr<Float> sigmaDepol(le, String("DepolarizationRatioError"));
   return sigmaDepol;
}



// Private functions


void ImagePolarimetry::cleanup()
{
   delete itsInImagePtr;   
   itsInImagePtr = 0;   
//
   for (uInt i=0; i<4; i++) {
      delete itsStokesPtr[i];
      itsStokesPtr[i] = 0;
//
      delete itsStokesStatsPtr[i];
      itsStokesStatsPtr[i] = 0;
   }
//
   if (itsFitterPtr!= 0) {
     delete itsFitterPtr;
     itsFitterPtr = 0;
   }
}


void ImagePolarimetry::copyDataAndMask(ImageInterface<Float>& out, 
                                       ImageInterface<Float>& in) const
//
// Copy the data and mask from input to output. If the input is 
// masked, the output must already be masked and ready
//
{
// Do we need to stuff about with masks ?
   
   Bool doMask = in.isMasked() && out.hasPixelMask();
   Lattice<Bool>* pMaskOut = 0;
   if (doMask) {
      pMaskOut = &out.pixelMask();
      if (!pMaskOut->isWritable()) {
         doMask = False;
      }
   }

// Use the same stepper for input and output.
   
   IPosition cursorShape = out.niceCursorShape();
   LatticeStepper stepper (out.shape(), cursorShape, LatticeStepper::RESIZE);
   
// Create an iterator for the output to setup the cache.
// It is not used, because using putSlice directly is faster and as easy.

   LatticeIterator<Float> dummyIter(out);
   RO_MaskedLatticeIterator<Float> iter(in, stepper);
   for (iter.reset(); !iter.atEnd(); iter++) {
      out.putSlice (iter.cursor(), iter.position());
      if (doMask) {
         pMaskOut->putSlice(iter.getMask(False), iter.position());
      }
   }
}


void ImagePolarimetry::findFrequencyAxis (Int& spectralCoord, Int& fAxis, 
                                          const CoordinateSystem& cSys, Int spectralAxis) const
{
   LogIO os(LogOrigin("ImagePolarimetry", "findFrequencyAxis(...)", WHERE));
   spectralCoord = -1;
   fAxis = -1;
   if (spectralAxis >=0) {
      if (spectralAxis < Int(cSys.nPixelAxes())) { 
         fAxis = spectralAxis;
         Int axisInCoordinate;
         cSys.findPixelAxis(spectralCoord, axisInCoordinate, fAxis);

// Check coordinate type is one of expected types

         Bool ok = cSys.type(spectralCoord)==Coordinate::TABULAR ||
                   cSys.type(spectralCoord)==Coordinate::LINEAR ||  
                   cSys.type(spectralCoord)==Coordinate::SPECTRAL;
         if (!ok) {
            os << "The specified axis of type " << cSys.showType(spectralCoord) 
               << " cannot be a frequency axis" << LogIO::EXCEPTION;
         }
      } else {
         os << "Illegal spectral axis " << spectralAxis+1 << " given" << LogIO::EXCEPTION;
      }
   } else {   
      spectralCoord = findSpectralCoordinate(cSys, os, False);
      if (spectralCoord < 0) {
         for (uInt i=0; i<cSys.nCoordinates(); i++) {
            if (cSys.type(i)==Coordinate::TABULAR ||
                cSys.type(i)==Coordinate::LINEAR) {
               Vector<String> axisNames = cSys.coordinate(i).worldAxisNames();
               String tmp = axisNames(0);
               tmp.upcase();
               if (tmp.contains(String("FREQ"))) {
                  spectralCoord = i;
                  break;
               }
            }
         }
      }
      if (spectralCoord < 0) {
         os << "Cannot find SpectralCoordinate in this image" << LogIO::EXCEPTION;
      } else {
         Vector<Int> pixelAxes = cSys.pixelAxes(spectralCoord);
         fAxis = pixelAxes(0);
      }
   }
}


void ImagePolarimetry::findStokes()
{
   LogIO os(LogOrigin("ImagePolarimetry", "findStokes(...)", WHERE));

// Do we have any Stokes ?

   const CoordinateSystem& cSys = itsInImagePtr->coordinates();
   Int afterCoord = -1;
   Int iStokes = cSys.findCoordinate(Coordinate::STOKES, afterCoord);
   if (iStokes<0) {
      cleanup();
      os << "There is no Stokes Coordinate in this image" << LogIO::EXCEPTION;
   }
   if (afterCoord>0) {
      os << LogIO::WARN 
         << "There is more than one Stokes coordinate in this image. Only first considered" << LogIO::POST;
   }

// Find the pixel axis of the image which is Stokes

   Vector<Int> pixelAxes = cSys.pixelAxes(iStokes);

// Make the regions

   const StokesCoordinate& stokes = cSys.stokesCoordinate(iStokes);
   const uInt ndim = itsInImagePtr->ndim();
   IPosition shape = itsInImagePtr->shape();
   IPosition blc(ndim,0);
   IPosition trc(shape-1);
//
   Int pix;
   if (stokes.toPixel(pix, Stokes::I)) {
      itsStokesPtr[ImagePolarimetry::I] = makeSubImage(blc, trc, pixelAxes(0), pix);
   }
   if (stokes.toPixel(pix, Stokes::Q)) {
      itsStokesPtr[ImagePolarimetry::Q] = makeSubImage(blc, trc, pixelAxes(0), pix);
   }
   if (stokes.toPixel(pix, Stokes::U)) {
      itsStokesPtr[ImagePolarimetry::U] = makeSubImage(blc, trc, pixelAxes(0), pix);
   }
   if (stokes.toPixel(pix, Stokes::V)) { 
      itsStokesPtr[ImagePolarimetry::V] = makeSubImage(blc, trc, pixelAxes(0), pix);
   }
//
   if ( (itsStokesPtr[ImagePolarimetry::Q]!=0 && itsStokesPtr[ImagePolarimetry::U]==0) ||
        (itsStokesPtr[ImagePolarimetry::Q]==0 && itsStokesPtr[ImagePolarimetry::U]!=0)) {
      cleanup();
      os << "This Stokes coordinate has only one of Q and U. This is not useful" << LogIO::EXCEPTION;
   }
   if (itsStokesPtr[ImagePolarimetry::Q]==0 && 
       itsStokesPtr[ImagePolarimetry::U]==0 && 
       itsStokesPtr[ImagePolarimetry::V]==0) {
      cleanup();
      os << "This image has no Stokes Q, U, or V.  This is not useful" << LogIO::EXCEPTION;
   }
}


void ImagePolarimetry::fiddleStokesCoordinate(ImageInterface<Float>& im, Stokes::StokesTypes type) const
{
   CoordinateSystem cSys = im.coordinates();
   fiddleStokesCoordinate(cSys, type);
   im.setCoordinateInfo(cSys);
}

void ImagePolarimetry::fiddleStokesCoordinate(CoordinateSystem& cSys, Stokes::StokesTypes type) const
{   
   Int afterCoord = -1;
   Int iStokes = cSys.findCoordinate(Coordinate::STOKES, afterCoord);
//
   Vector<Int> which(1);
   which(0) = Int(type);
   StokesCoordinate stokes(which);
   cSys.replaceCoordinate(stokes, iStokes);   
}

void ImagePolarimetry::fiddleStokesCoordinate(ImageInterface<Complex>& ie, Stokes::StokesTypes type) const
{   
   CoordinateSystem cSys = ie.coordinates();
//
   Int afterCoord = -1;
   Int iStokes = cSys.findCoordinate(Coordinate::STOKES, afterCoord);
//
   Vector<Int> which(1);
   which(0) = Int(type);
   StokesCoordinate stokes(which);
   cSys.replaceCoordinate(stokes, iStokes);   
   ie.setCoordinateInfo(cSys);
}

void ImagePolarimetry::fiddleTimeCoordinate(ImageInterface<Complex>& ie, const Quantum<Double>& f, 
                                            Int coord) const
{   
   LogIO os(LogOrigin("ImagePolarimetry", "fiddleTimeCoordinate(...)", WHERE));
//
   CoordinateSystem cSys = ie.coordinates();
   Coordinate* pC = cSys.coordinate(coord).clone();
   AlwaysAssert(pC->nPixelAxes()==1,AipsError);
   AlwaysAssert(pC->type()==Coordinate::LINEAR,AipsError);
//
   Vector<String> axisUnits = pC->worldAxisUnits();
   axisUnits = String("s");
   if (!pC->setWorldAxisUnits(axisUnits)) {
      os << "Failed to set TimeCoordinate units to seconds because " << pC->errorMessage() << LogIO::EXCEPTION;
   }

// Find factor to convert from time (s) to rad/m/m

   Vector<Double> inc = pC->increment();
   Double ff = f.getValue(Unit("Hz"));
   Double lambda = QC::c.getValue(Unit("m/s")) / ff;
   Double fac = -C::pi * ff / 2.0 / lambda / lambda;
   inc *= fac;
//
   Vector<String> axisNames(1);
   axisNames = String("RotationMeasure");
   axisUnits = String("rad/m/m");
   Vector<Double> refVal(1,0.0);
//
   LinearCoordinate lC(axisNames, axisUnits, refVal, inc, 
                       pC->linearTransform().copy(), pC->referencePixel().copy());
//
   cSys.replaceCoordinate(lC, coord);   
   ie.setCoordinateInfo(cSys);
   delete pC;
}


Quantum<Double> ImagePolarimetry::findCentralFrequency(const Coordinate& coord, Int shape) const
{
   AlwaysAssert(coord.nPixelAxes()==1,AipsError);
//
   Vector<Double> pixel(1);
   Vector<Double> world;
   pixel(0) = Double(shape - 1) / 2.0;
   if (!coord.toWorld(world, pixel)) {
      LogIO os(LogOrigin("ImagePolarimetry", "findCentralFrequency(...)", WHERE));
      os << "Failed to convert pixel to world for SpectralCoordinate because " 
         << coord.errorMessage() << LogIO::EXCEPTION;
  }
  Vector<String> units = coord.worldAxisUnits();
  return Quantum<Double>(world(0), units(0));
}


Int ImagePolarimetry::findSpectralCoordinate(const CoordinateSystem& cSys, LogIO& os,
                                             Bool fail) const
{
   Int afterCoord = -1;
   Int coord = cSys.findCoordinate(Coordinate::SPECTRAL, afterCoord);
   if (coord<0) {
      if (fail) os << "No spectral coordinate in this image" << LogIO::EXCEPTION;
   }
   if (afterCoord>0) {
      os << LogIO::WARN << "This image has more than one spectral coordinate; only first used"
         << LogIO::POST;
   }
   return coord;
}

Bool ImagePolarimetry::findRotationMeasure (Float& rmFitted, Float& rmErrFitted,
                                            Float& pa0Fitted, Float& pa0ErrFitted, 
                                            Float& rChiSqFitted, Float& nTurns,
                                            const Vector<uInt>& sortidx,
                                            const Vector<Float>& wsq2, const Vector<Float>& pa2, 
                                            const Array<Bool>& paMask2, 
                                            const Array<Float>& paerr2, 
                                            Float rmFg, Float rmMax, Float maxPaErr,
                                            PGPlotter& plotter, const String& posString)
//
// wsq is lambda squared in m**2 in increasing wavelength order
// pa is position angle in radians
// paerr is pa error in radians
// maxPaErr is maximum tolerated error in position angle
// rmfg is a user specified foreground RM rad/m/m
// rmmax is a user specified maximum RM
//
{ 
   static Vector<Float> paerr;
   static Vector<Float> pa;
   static Vector<Float> wsq;

// Abandon if less than 2 points

   uInt n = sortidx.nelements();
   rmFitted = rmErrFitted = pa0Fitted = pa0ErrFitted = rChiSqFitted = 0.0;
   if (n<2) return False;

// Sort into decreasing frequency order and correct for foreground rotation
// Remember wsq already sorted.  Discard points that are too noisy or masked

   const Vector<Float>& paerr1(paerr2.nonDegenerate(0));
   const Vector<Bool>& paMask1(paMask2.nonDegenerate(0));
   paerr.resize(n);
   pa.resize(n);
   wsq.resize(n);
//
   uInt j = 0;
   for (uInt i=0; i<n; i++) {
      if (abs(paerr1(sortidx(i)))<maxPaErr && paMask1(sortidx(i))) {
         pa(j) = pa2(sortidx(i)) - rmFg*wsq2(i);
         paerr(j) = paerr1(sortidx(i));
         wsq(j) = wsq2(i);
         j++;
      }
   }
   n = j;
   if (n<=1) return False;
//
   pa.resize(n,True);
   paerr.resize(n,True);
   wsq.resize(n, True);

// Treat supplementary and primary points separately

   Bool ok = False;
   if (n==2) {
      ok = rmSupplementaryFit(nTurns, rmFitted, rmErrFitted, pa0Fitted, pa0ErrFitted, 
                              rChiSqFitted, wsq, pa, paerr);
   } else {
      ok = rmPrimaryFit(nTurns, rmFitted, rmErrFitted, pa0Fitted, pa0ErrFitted, 
                        rChiSqFitted, wsq, pa, paerr, rmMax, plotter, posString);
   }

// Put position angle into the range 0->pi

   static MVAngle tmpMVA1;
   if (ok) {
      MVAngle tmpMVA0(pa0Fitted);
      tmpMVA1 = tmpMVA0.binorm(0.0);
      pa0Fitted = tmpMVA1.radian();

// Add foreground back on

      rmFitted += rmFg;
   }
   return ok;
}

void ImagePolarimetry::hasQU () const
{
   Bool has = itsStokesPtr[ImagePolarimetry::Q]!=0 && 
              itsStokesPtr[ImagePolarimetry::U]!=0;
   if (!has) {
      LogIO os(LogOrigin("ImagePolarimetry", "hasQU(...)", WHERE));
      os << "This image does not have Stokes Q and U which are required for this function" << LogIO::EXCEPTION;
   }
}


ImageExpr<Float> ImagePolarimetry::makeStokesExpr(ImageInterface<Float>* imPtr,
                                                 Stokes::StokesTypes type, const String& name) const
{
   LogIO os(LogOrigin("ImagePolarimetry", "makeStokesExpr(...)", WHERE));
   if (imPtr==0) {
      os << "This image does not have Stokes " << Stokes::name(type) << LogIO::EXCEPTION;
   }

// Make node.  

   LatticeExprNode node(*imPtr);

// Make expression

   LatticeExpr<Float> le(node);
   ImageExpr<Float> ie(le, name);
   ie.setUnits(itsInImagePtr->units());
   fiddleStokesCoordinate(ie, type);
//
   return ie;
}



ImageInterface<Float>* ImagePolarimetry::makeSubImage (IPosition& blc, 
                                                       IPosition& trc, 
                                                       Int axis, Int pix) const
{
    blc(axis) = pix;
    trc(axis) = pix;
    LCSlicer slicer(blc, trc, RegionType::Abs);
    ImageRegion region(slicer);
    return new SubImage<Float>(*itsInImagePtr, region);
}


LatticeExprNode ImagePolarimetry::makePolIntNode(LogIO& os, Bool debias, Float clip, Float sigma,
                                                 Bool doLin, Bool doCirc) 
{ 
   LatticeExprNode linNode, circNode, node;
//
   Float sigma2 = 0.0;
   if (doLin) {
      if (debias) {
         if (sigma > 0.0) {
            sigma2 = sigma;
         } else {
            sigma2 = ImagePolarimetry::sigma(clip);
         }
      }
      linNode = LatticeExprNode(pow(*itsStokesPtr[ImagePolarimetry::U],2) + 
                                pow(*itsStokesPtr[ImagePolarimetry::Q],2));
   }
//
   if (doCirc) {
      if (debias) {
         if (sigma > 0.0) {
            sigma2 = sigma;
         } else {
            sigma2 = ImagePolarimetry::sigma(clip);
         }
      }
      circNode = LatticeExprNode(pow(*itsStokesPtr[ImagePolarimetry::V],2));
   }
//
   Float sigmasq = sigma2 * sigma2;
   if (doLin && doCirc) {
      if (debias) {
         node = linNode + circNode - LatticeExprNode(sigmasq);
         os << LogIO::NORMAL << "Debiasing with sigma = " << sqrt(sigmasq) << LogIO::POST;
      } else {
         node = linNode + circNode;         
      }
   } else if (doLin) {
      if (debias) {
         node = linNode - LatticeExprNode(sigmasq);
         os << LogIO::NORMAL << "Debiasing with sigma  = " << sqrt(sigmasq) << LogIO::POST;
      } else {
         node = linNode;
      }
   } else if (doCirc) {  
      if (debias) {
         node = circNode - LatticeExprNode(sigmasq);
         os << LogIO::NORMAL << "Debiasing with sigma = " << sqrt(sigmasq) << LogIO::POST;
      } else {
         node = circNode;
      }
   }
//
   return LatticeExprNode(sqrt(node));
}


Bool ImagePolarimetry::rmPrimaryFit(Float& nTurns, Float& rmFitted, Float& rmErrFitted,
                                    Float& pa0Fitted, Float& pa0ErrFitted, 
                                    Float& rChiSqFitted, const Vector<Float>& wsq, 
                                    const Vector<Float>& pa, const Vector<Float>& paerr, 
                                    Float rmMax, PGPlotter& plotter, const String& posString)
{
   static Vector<Float> plotPA;
   static Vector<Float> plotPAErr;
   static Vector<Float> plotPAErrY1;
   static Vector<Float> plotPAErrY2;
   static Vector<Float> plotPAFit;

// Assign position angle to longest wavelength consistent with
// RM < RMMax

   const uInt n = wsq.nelements();
   Double dwsq = wsq(n-1) - wsq(0);
//
   Float ppa = abs(rmMax)*dwsq + pa(0);
   Float diff = ppa - pa(n-1);
   Float t = 0.5;
   if (diff < 0) t = -0.5;
   Int maxnpi = Int(diff/C::pi + t);
//
   ppa = -abs(rmMax)*dwsq + pa(0);
   diff = ppa - pa(n-1);
   t = 0.5;
   if (diff < 0) t = -0.5;
   Int minnpi = Int(diff/C::pi + t);
// cout << "primary:: minnpi, maxnpi=" << minnpi << ", " << maxnpi << endl;

// Resize plotting vectors

   if (plotter.isAttached()) {
      plotPA.resize(n);
      plotPAErr.resize(n);
      plotPAErrY1.resize(n);
      plotPAErrY2.resize(n);
      plotPAFit.resize(n);
   }

// Loop over range of n*pi ambiguity

   Vector<Float> fitpa(n);
   Vector<Float> pars;
   Float chiSq = 1e30;
   for (Int h=minnpi; h<=maxnpi; h++) {
     fitpa(n-1) = pa(n-1) + C::pi*h;
     Float rm0 = (fitpa(n-1) - pa(0))/ dwsq;

// Assign position angles to remaining wavelengths

     for (uInt k=1; k<n-1; k++) {
       ppa = pa(0) + rm0*(wsq(k)-wsq(0));
       diff = ppa - pa(k);
//
       t = 0.5;
       if (diff < 0) t = -0.5;
       Int npi = Int(diff/C::pi + t);
       fitpa(k) = pa(k) + npi*C::pi;
     }
     fitpa(0) = pa(0);

// Do least squares fit

     if (!rmLsqFit (pars, wsq, fitpa, paerr)) return False;

//
     if (pars(4) < chiSq) {
        chiSq = pars(4);
//
        nTurns = h;                   // Number of turns
        rmFitted = pars(0);           // Fitted RM
        rmErrFitted = pars(1);        // Error in RM
        pa0Fitted = pars(2);          // Fitted intrinsic angle
        pa0ErrFitted = pars(3);       // Error in angle
        rChiSqFitted = pars(4);       // Recued chi squared
        if (n > 2) rChiSqFitted /= Float(n - 2);
//
        if (plotter.isAttached()) {
           plotPA = fitpa;
           plotPAErr = paerr;
//
           for (uInt k=0; k<n; k++) {
              plotPAFit(k) = pars(2) + pars(0)*wsq(k);
           }
        }
     }
   }

// Make plot

   if (plotter.isAttached()) {
     plotPA *= Float(180.0) / Float(C::pi);
     plotPAErr *= Float(180.0) / Float(C::pi);
     plotPAFit *= Float(180.0) / Float(C::pi);
     plotPAErrY1 = plotPA - plotPAErr;
     plotPAErrY2 = plotPA + plotPAErr;
//
     Float yMinVal, yMaxVal;
     minMax(yMinVal, yMaxVal, plotPA);
     Float dy = 0.05 * (yMaxVal - yMinVal);
//
     ostringstream oss;
     oss << "  nT = " << nTurns << ", ChiSq = " << rChiSqFitted;
//
     plotter.page();
     Float dx = 0.05 * (wsq(n-1) - wsq(0));
     plotter.vstd();
     plotter.swin(wsq(0)-dx, wsq(n-1)+dx, yMinVal-dy, yMaxVal+dy);
     plotter.box("BCNST", 0.0, 0, "BCNST", 0.0, 0);
//
     plotter.lab("\\gl\\u2\\d (m\\u2\\d)", 
                 "Position Angle (degrees)", 
                 String("Pos=") + posString + String(oss));
//     
     plotter.pt(wsq, plotPA, 17);
     plotter.erry (wsq, plotPAErrY1, plotPAErrY2, 1.0);
     plotter.line(wsq, plotPAFit);

   }
//
   return True;
}



Bool ImagePolarimetry::rmSupplementaryFit(Float& nTurns, Float& rmFitted, Float& rmErrFitted,
                                          Float& pa0Fitted, Float& pa0ErrFitted, 
                                          Float& rChiSqFitted,  const Vector<Float>& wsq, 
                                          const Vector<Float>& pa, const Vector<Float>& paerr)
{

// For supplementary points find lowest residual RM

   const uInt n = wsq.nelements();
//
   Float absRM = 1e30;
   Vector<Float> fitpa(pa.copy());
   Vector<Float> pars;
   for (Int i=-2; i<3; i++) {
     fitpa(n-1) = pa(n-1) + C::pi*i;

// Do least squares fit

     if (!rmLsqFit (pars, wsq, fitpa, paerr)) return False;

// Save solution  with lowest absolute RM

     if (abs(pars(0)) < absRM) {
        absRM = abs(pars(0));
//
        nTurns = i;                        // nTurns
        rmFitted = pars(0);                // Fitted RM
        rmErrFitted = pars(1);             // Error in RM
        pa0Fitted = pars(2);               // Fitted intrinsic angle
        pa0ErrFitted = pars(3);            // Error in angle
        rChiSqFitted = pars(4);            // Reduced chi squared
        if (n > 2) rChiSqFitted /= Float(n - 2);
     }

   }
//
  return True;
}




Bool ImagePolarimetry::rmLsqFit (Vector<Float>& pars, const Vector<Float>& wsq, 
                                 const Vector<Float> pa, const Vector<Float>& paerr) const
{

// Perform fit on unmasked data

   static Vector<Float> solution;
   try {
     solution = itsFitterPtr->fit(wsq, pa, paerr);
   } catch (AipsError x) {
     return False;
   } 
//
   const Vector<Double>& cv = itsFitterPtr->compuCovariance().diagonal();
   pars.resize(5);
   pars(0) = solution(1);
   pars(1) = sqrt(cv(1));
   pars(2) = solution(0);
   pars(3) = sqrt(cv(0));
   pars(4) = itsFitterPtr->chiSquare();
// 
   return True;
}


String ImagePolarimetry::stokesName (ImagePolarimetry::StokesTypes index) const
{
   if (index==ImagePolarimetry::I) {
      return String("I");
   } else if (index==ImagePolarimetry::Q) {
      return String("Q");
   } else if (index==ImagePolarimetry::U) {
      return String("U");
   } else if (index==ImagePolarimetry::V) {
      return String("V");
   } else {
      return String("??");  
   }
}


Stokes::StokesTypes ImagePolarimetry::stokesType (ImagePolarimetry::StokesTypes index) const
{
   if (index==ImagePolarimetry::I) {
      return Stokes::I;
   } else if (index==ImagePolarimetry::Q) {
      return Stokes::Q;
   } else if (index==ImagePolarimetry::U) {
      return Stokes::U;
   } else if (index==ImagePolarimetry::V) {
      return Stokes::V;
   } else {
      return Stokes::Undefined;
   }
}


Float ImagePolarimetry::sigma (ImagePolarimetry::StokesTypes index, Float clip)
{
   Float clip2 = abs(clip);
   if (clip2==0.0) clip2 = 10.0;
//
   if (clip2 != itsOldClip && itsStokesStatsPtr[index]!=0) {
      delete itsStokesStatsPtr[index];
      itsStokesStatsPtr[index] = 0;
   }
   if (itsStokesStatsPtr[index]==0) {

// Find sigma for all points inside +/- clip-sigma of the mean
// More joys of LEL

      const ImageInterface<Float>* p = itsStokesPtr[index];
      LatticeExprNode n1 (*p);
      LatticeExprNode n2 (n1[abs(n1-mean(n1)) < clip2*stddev(n1)]);
      LatticeExpr<Float> le(n2);
//
      itsStokesStatsPtr[index] = new LatticeStatistics<Float>(le, False, False);
   }
//
   Array<Float> sigmaA;
   itsStokesStatsPtr[index]->getConvertedStatistic(sigmaA, LatticeStatsBase::SIGMA);
   if (sigmaA.nelements()==0) {
      LogIO os(LogOrigin("ImagePolarimetry", "sigma(...)", WHERE));
      os << "No good points in clipped determination of the noise " 
         << "for the Stokes " << stokesName(index) << " image" << LogIO::EXCEPTION;
   }
//
   itsOldClip = clip2;
   return sigmaA(IPosition(1,0));
}


void ImagePolarimetry::subtractProfileMean (ImageInterface<Float>& im, uInt axis) const
{
   const IPosition tileShape = im.niceCursorShape();
   TiledLineStepper ts(im.shape(), tileShape, axis);
   LatticeIterator<Float> it(im, ts);
//
   Float dMean;
   if (im.isMasked()) {
      const Lattice<Bool>& mask = im.pixelMask();
      for (it.reset(); !it.atEnd(); it++) {
         const Array<Float>& p = it.cursor();
         const Array<Bool>& m = mask.getSlice(it.position(), it.cursorShape());
         const MaskedArray<Float> ma(p, m, True);
         dMean = mean(ma);
//
         it.rwVectorCursor() -= dMean;
      }

   } else {
      for (it.reset(); !it.atEnd(); it++) {
         dMean = mean(it.vectorCursor());
         it.rwVectorCursor() -= dMean;
      }
   }
}


Bool ImagePolarimetry::dealWithMask (Lattice<Bool>*& pMask, ImageInterface<Float>*& pIm, 
                                     LogIO& os, const String& type) const
{
   Bool isMasked = False;
   if (!pIm->isMasked()) {
      if (pIm->canDefineRegion()) {
         pIm->makeMask("mask0", True, True, True, True);
         isMasked = True;
      } else {
         os << LogIO::WARN << "Could not create a mask for the output " << type << " image" << LogIO::POST;
      }
   } else {
      isMasked = True;
   }
//
   if (isMasked) {
      pMask = &(pIm->pixelMask());
      if (!pMask->isWritable()) {
         isMasked = False;
         os << LogIO::WARN << "The output " << type << " image has a mask but it's not writable" << LogIO::POST;
      }
   }
   return isMasked;
}

} //# NAMESPACE CASA - END

