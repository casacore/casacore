//# ImagePolarimetry.cc: polarimetric analysis
//# Copyright (C) 1996,1997,1998,1999,2000
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

#include <trial/Images/ImagePolarimetry.h>

#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/StokesCoordinate.h>
#include <trial/Coordinates/LinearCoordinate.h>
#include <aips/Exceptions/Error.h>
#include <trial/Fitting/LinearFitSVD.h>
#include <aips/Functionals/Polynomial.h>
#include <trial/Functionals/LinearComb.h>
#include <trial/Images/ImageInterface.h>
#include <trial/Images/SubImage.h>
#include <trial/Images/ImageExpr.h>
#include <trial/Images/ImageFFT.h>
#include <trial/Images/ImageRegion.h>
#include <trial/Images/ImageSummary.h>
#include <trial/Lattices/LCSlicer.h>
#include <trial/Lattices/LatticeExprNode.h>
#include <trial/Lattices/LatticeExpr.h>
#include <trial/Lattices/TiledLineStepper.h>
#include <trial/Lattices/LatticeStepper.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/LatticeStatistics.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/NumericTraits.h>
#include <trial/Tasking/PGPlotter.h>
#include <aips/Quanta/QC.h>
#include <aips/Utilities/GenSort.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>


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
//
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


ImageExpr<Float> ImagePolarimetry::fracLinPol(Bool debias, Float clip, Float sigma) 
{
   LogIO os(LogOrigin("ImagePolarimetry", "fracLinPol(...)", WHERE));
   if (itsStokesPtr[ImagePolarimetry::Q]==0 && itsStokesPtr[ImagePolarimetry::U]==0) {
      os << "This image does not have Stokes Q and U so cannot provide linear polarization" << LogIO::EXCEPTION;
   }
   if (itsStokesPtr[ImagePolarimetry::I]==0) {
      os << "This image does not have Stokes I so cannot provide fractional linear polarization" << LogIO::EXCEPTION;
   }

// Make nodes

   LatticeExprNode nodePol = makePolIntNode(os, debias, clip, sigma, True, False);
   LatticeExprNode nodeI(*itsStokesPtr[ImagePolarimetry::I]);

// Make expression

   LatticeExpr<Float> le(nodePol/nodeI);
   ImageExpr<Float> ie(le, String("fractionalLinearPolarization"));

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
   if (itsStokesPtr[ImagePolarimetry::Q]==0 && itsStokesPtr[ImagePolarimetry::U]==0) {
      os << "This image does not have Stokes Q and U so cannot provide linear polarization" << LogIO::EXCEPTION;
   }
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
   ImageExpr<Float> ie(le, String("fractionalLinearPolarizationError"));

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
   ImageExpr<Float> ie(le, String("fractionalTotalPolarization"));

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
   ImageExpr<Float> ie(le, String("fractionalLinearPolarizationError"));

// Fiddle Stokes coordinate in ImageExpr

   fiddleStokesCoordinate(ie, Stokes::PFlinear);
//
   return ie;
}



void ImagePolarimetry::fourierRotationMeasure(ImageInterface<Complex>& cpol,
                                              Bool zeroZeroLag)
{
   LogIO os(LogOrigin("ImagePolarimetry", "fourierRotationMeasure(...)", WHERE));

// Check image shape

   if (!cpol.shape().isEqual(singleStokesShape())) {
      os << "The provided  image has the wrong shape " << cpol.shape() << endl;
      os << "It should be of shape " << singleStokesShape() << LogIO::EXCEPTION;
   }

// Make Complex (Q,U) image

   LatticeExprNode node;
   if (zeroZeroLag) {
      LatticeExprNode node1( (*itsStokesPtr[ImagePolarimetry::Q])
                               - sum(*itsStokesPtr[ImagePolarimetry::Q]));
      LatticeExprNode node2( (*itsStokesPtr[ImagePolarimetry::U]) 
                               - sum(*itsStokesPtr[ImagePolarimetry::U]));
      node = LatticeExprNode(complex(node1, node2));
   } else {
      node = LatticeExprNode(complex(*itsStokesPtr[ImagePolarimetry::Q], 
                                     *itsStokesPtr[ImagePolarimetry::U]));
   }
   LatticeExpr<Complex> le(node);
   ImageExpr<Complex> ie(le, String("ComplexLinearPolarization"));

// Find spectral coordinate

   const CoordinateSystem& cSys = ie.coordinates();
   Int coord = findSpectralCoordinate(cSys, os, True);
   Vector<Int> pixelAxes = cSys.pixelAxes(coord);

// Find central frequency

   Vector<Bool> axes(ie.ndim(),False);
   axes(pixelAxes(0)) = True;
   Quantum<Double> f = findCentralFrequency(cSys.coordinate(coord), ie.shape()(pixelAxes(0)));

// Do FFT of spectral coordinate

   ImageFFT fftserver;
   fftserver.fft(ie, axes);

// Recover result. Coordinates are updated to include Fourier coordinate,
// miscellaneous things (MiscInfo, ImageInfo, units, history) and mask
// (if output has one) are copied to cpol

   fftserver.getComplex(cpol);
   fiddleStokesCoordinate(cpol, Stokes::Plinear);
   fiddleTimeCoordinate(cpol, f, coord);
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
   ImageExpr<Float> ie(le, String("linearlyPolarizedIntensity"));

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

// Make expression 

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
   ImageExpr<Float> ie(le, String("linearlyPolarizedPositionAngle"));

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
   ImageExpr<Float> ie(le, String("linearlyPolarizedPositionAngleError"));

// Fiddle Stokes coordinate

   fiddleStokesCoordinate(ie, Stokes::Pangle);
//
   return ie;
}

Float ImagePolarimetry::sigma(Float clip)
{
   LogIO os(LogOrigin("ImagePolarimetry", "noise(...)", WHERE));
   if (itsStokesPtr[ImagePolarimetry::V]!=0) {
      os << LogIO::NORMAL << "Determining noise from V image" << LogIO::POST;
      return ImagePolarimetry::sigma(ImagePolarimetry::V, clip);
   } else if (itsStokesPtr[ImagePolarimetry::Q]!=0 &&
              itsStokesPtr[ImagePolarimetry::U]!=0) {
      os << LogIO::NORMAL << "Determining noise from Q&U images" << LogIO::POST;
      Float sq = ImagePolarimetry::sigma(ImagePolarimetry::Q, clip);
      Float su = ImagePolarimetry::sigma(ImagePolarimetry::U, clip);
      return (sq+su)/2.0;
   } else if (itsStokesPtr[ImagePolarimetry::Q]!=0) {
      os << LogIO::NORMAL << "Determining noise from Q image" << LogIO::POST;
      return ImagePolarimetry::sigma(ImagePolarimetry::Q, clip);
   } else if (itsStokesPtr[ImagePolarimetry::U]!=0) {
      os << LogIO::NORMAL << "Determining noise from U image" << LogIO::POST;
      return ImagePolarimetry::sigma(ImagePolarimetry::U, clip);
   } else if (itsStokesPtr[ImagePolarimetry::I]!=0) {
      os << LogIO::NORMAL << "Determining noise from I image" << LogIO::POST;
      return ImagePolarimetry::sigma(ImagePolarimetry::I, clip);
   }
   return 0.0;
}



void ImagePolarimetry::rotationMeasure(ImageInterface<Float>& rmOut, ImageInterface<Float>& rmOutError,
                                       Int axis, Float sigma, Float rmFg, Float rmMax, Float maxPaErr)
{
   LogIO os(LogOrigin("ImagePolarimetry", "rotationMeasure(...)", WHERE));


// Find expected shape of output image (Stokes and spectral axes gone)

   IPosition shape;
   CoordinateSystem cSys;
   uInt fAxis, sAxis;
   rotationMeasureShape(shape, cSys, fAxis, sAxis, os, axis);

// Check image shape

   if (!rmOut.shape().isEqual(shape)) {
      os << "The provided Rotation Measure image has the wrong shape " << rmOut.shape() << endl;
      os << "It should be of shape " << shape << LogIO::EXCEPTION;
   }
   if (!rmOutError.shape().isEqual(shape)) {
      os << "The provided Rotation Measure error image has the wrong shape " << rmOutError.shape() << endl;
      os << "It should be of shape " << shape << LogIO::EXCEPTION;
   }

// Generate linear polarization position angle image and error in radians

   Bool radians = True;
   Float clip = 10.0;
   ImageExpr<Float> pa = linPolPosAng(radians);
   ImageExpr<Float> paerr = sigmaLinPolPosAng(radians, clip, sigma);
   CoordinateSystem cSys0 = pa.coordinates();


// Do we have enough frequency pixels ?

   const uInt nFreq = pa.shape()(fAxis);
   if (nFreq < 3) {
      os << "This image only has " << nFreq << "frequencies, this is not enough"
         << LogIO::EXCEPTION;
   }

// Overwrite output CS

   if (!rmOut.setCoordinateInfo(cSys)) {
      os << "Failed to set the CoordinateSystem in the output Rotation Measure image" << LogIO::EXCEPTION;
   }
   if (!rmOutError.setCoordinateInfo(cSys)) {
      os << "Failed to set the CoordinateSystem in the output Rotation Measure error image" << LogIO::EXCEPTION;
   }

// Copy miscellaneous things over

   copyMiscellaneous(rmOut);
   copyMiscellaneous(rmOutError);

// Set frequency axis units to Hz

   Int fAxisWorld = cSys0.pixelAxisToWorldAxis(fAxis);
   if (fAxisWorld <0) {
      os << "World axis has been removed for the frequency pixel axis" << LogIO::EXCEPTION;
   }
//
   Vector<String> axisUnits = cSys0.worldAxisUnits();
   axisUnits(fAxisWorld) = String("Hz");
   if (!cSys0.setWorldAxisUnits(axisUnits, True)) {
      os << "Failed to set frequency axis units to Hz because " 
         << cSys0.errorMessage() << LogIO::EXCEPTION;
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
/*
   cout << "wsq = " << wsq << endl;
   cout << "wsqsort = " << wsqsort << endl;
*/

// Copy the input mask to the output if we can

   copyMask(rmOut, *itsInImagePtr);
   copyMask(rmOutError, *itsInImagePtr);
// 
   Bool isMasked = rmOut.isMasked() && rmOut.isMaskWritable();
   Bool isMaskedErr = rmOutError.isMasked() && rmOutError.isMaskWritable();

// Make fitter

   if (itsFitterPtr==0) {
      itsFitterPtr = new LinearFitSVD<Float>;
      LinearComb<Float,Float> comb;

// Create and set the polynomial functional
// p = c(0) + c(1)*x where x = lambda**2
// PA = PA0 + RM*Lambda**2
// Don't ask me to explain this idiotic interface

      Polynomial<Float> poly0(0);
      Polynomial<Float> poly1(1);
      poly0.setCoefficient(0, 1.0);
      poly1.setCoefficient(1, 1.0);
      comb.addFunction(poly0);
      comb.addFunction(poly1);


// Makes a copy of comb

      itsFitterPtr->setFunction(comb);
   }


// Make iterator

   const IPosition tileShape = pa.niceCursorShape();
   TiledLineStepper ts(pa.shape(), tileShape, fAxis);
   RO_LatticeIterator<Float> it(pa, ts);

// Iterate

   Float rm, rmErr, pa0, pa0Err, rChiSq;
   IPosition where(rmOut.shape().nelements(),0);
   uInt j;
//
   Array<Bool> tmpMask(IPosition(rmOut.ndim(), 1), True);
   Array<Float> tmpValue(IPosition(rmOut.ndim(), 1), 0.0);
//
   for (it.reset(); !it.atEnd(); it++) {

// Find rotation measure for this line

      Bool ok = findRotationMeasure (rm, rmErr, pa0, pa0Err, rChiSq, 
                                     sortidx, wsqsort, it.vectorCursor(),
                                     paerr.getSlice(it.position(),it.cursor().shape()),
                                     rmFg, rmMax, maxPaErr);

// Plonk values into output  image.  This is slow and clunky, but should be relatively fast
// c.f. the fitting.  Could be reimplemented with LatticeApply if need be.  Buffering 
// is hard because the navigator doesn't take a regular path.  If I used a LatticeStepper
// instead, the path would be regular and then I could buffer, but then the iteration 
// would be less efficient !!!

        j = 0;
        for (uInt i=0; i<it.position().nelements(); i++) {
           if (i!=fAxis && i!=sAxis) {
              where(j) = it.position()(i);
              j++;
           }
        }
//
        if (isMasked) {
           tmpMask.set(ok);
           rmOut.putMaskSlice (tmpMask, where);
        }
        if (isMaskedErr) {
           tmpMask.set(ok);
           rmOutError.putMaskSlice (tmpMask, where);
        }

// If the output value is masked, the value itself is 0

        tmpValue.set(rm);
        rmOut.putSlice(tmpValue, where);
        tmpValue.set(rmErr);
        rmOutError.putSlice(tmpValue, where);
   }
}

void ImagePolarimetry::rotationMeasureShape(IPosition& shape, CoordinateSystem& cSys, 
                                            uInt& fAxis, uInt& sAxis, LogIO& os, Int axis) const
{

// Construction image CS

   CoordinateSystem cSys0 = coordinates();

// Find spectral and stokes axes

   Int spectralCoord = -1;
   if (axis >=0) {
      if (axis < Int(cSys0.nPixelAxes())) { 
         fAxis = axis;
         Int axisInCoordinate;
         cSys0.findPixelAxis(spectralCoord, axisInCoordinate, fAxis);

// Check coordinate type is one of expected types

         Bool ok = cSys0.type(spectralCoord)==Coordinate::TABULAR ||
                   cSys0.type(spectralCoord)==Coordinate::LINEAR ||  
                   cSys0.type(spectralCoord)==Coordinate::SPECTRAL;
         if (!ok) {
            os << "The specified axis of type " << cSys0.showType(spectralCoord) 
               << " cannot be a frequency axis" << LogIO::EXCEPTION;
         }
      } else {
         os << "Illegal spectral axis " << axis+1 << " given" << LogIO::EXCEPTION;
      }
   } else {   
      spectralCoord = findSpectralCoordinate(cSys0, os, False);
      if (spectralCoord < 0) {
         for (uInt i=0; i<cSys0.nCoordinates(); i++) {
            if (cSys0.type(i)==Coordinate::TABULAR ||
                cSys0.type(i)==Coordinate::LINEAR ||
                cSys0.type(i)==Coordinate::SPECTRAL) {
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
         Vector<Int> pixelAxes = cSys0.pixelAxes(spectralCoord);
         fAxis = pixelAxes(0);
      }
   }
   os << "Conclude pixel axis " << fAxis+1 << " is the frequency axis" << LogIO::POST;

// Find Stokes axis (we know it has one)

   Int afterCoord = -1;
   Int stokesCoord = cSys0.findCoordinate(Coordinate::STOKES, afterCoord);
   Vector<Int> pixelAxes = cSys0.pixelAxes(stokesCoord);
   sAxis = pixelAxes(0);


// What shape should the image be ?  Frequency and stokes axes should be gone.

   IPosition shape2 = ImagePolarimetry::shape();
   shape.resize(shape2.nelements()-2);
//
   uInt j = 0;
   for (uInt i=0; i<shape2.nelements(); i++) {
      if (i!=fAxis && i!=sAxis) {
        shape(j) = shape2(i);
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
   if (cSys.nCoordinates()<=0) {
      os << "RotationMeasure CoordinateSystem is empty !" << LogIO::EXCEPTION;
   }
}


ImageExpr<Float> ImagePolarimetry::stokesI() const
{
   return makeStokesExpr(itsStokesPtr[ImagePolarimetry::I], String("I"), String("StokesI"));
}

Float ImagePolarimetry::sigmaStokesI(Float clip) 
{
   return ImagePolarimetry::sigma(ImagePolarimetry::I, clip);
}

ImageExpr<Float> ImagePolarimetry::stokesQ() const
{
   return makeStokesExpr(itsStokesPtr[ImagePolarimetry::Q], String("Q"), String("StokesQ"));
}

Float ImagePolarimetry::sigmaStokesQ(Float clip) 
{
   return ImagePolarimetry::sigma(ImagePolarimetry::Q, clip);
}

ImageExpr<Float> ImagePolarimetry::stokesU() const
{
   return makeStokesExpr(itsStokesPtr[ImagePolarimetry::U], String("U"), String("StokesU"));
}

Float ImagePolarimetry::sigmaStokesU(Float clip) 
{
   return ImagePolarimetry::sigma(ImagePolarimetry::U, clip);
}

ImageExpr<Float> ImagePolarimetry::stokesV() const
{
   return makeStokesExpr(itsStokesPtr[ImagePolarimetry::V], String("V"), String("StokesV"));
}

Float ImagePolarimetry::sigmaStokesV(Float clip) 
{
   return ImagePolarimetry::sigma(ImagePolarimetry::V, clip);
}

ImageExpr<Float> ImagePolarimetry::stokes(ImagePolarimetry::StokesTypes stokes) const
{
   return makeStokesExpr(itsStokesPtr[stokes], stokesName(stokes), stokesName(stokes));
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


IPosition ImagePolarimetry::singleStokesShape() const
{
// We know the image has a Stokes coordinate or it
// would have failed at construction

   const CoordinateSystem& cSys = itsInImagePtr->coordinates();
   Int afterCoord = -1;
   Int iStokes = cSys.findCoordinate(Coordinate::STOKES, afterCoord);
   Vector<Int> pixelAxes = cSys.pixelAxes(iStokes);
//
   IPosition shape = itsInImagePtr->shape();
   shape(pixelAxes(0)) = 1;
   return shape;
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


void ImagePolarimetry::copyMask (ImageInterface<Float>& out,
                                 const ImageInterface<Float>& in) const
{
   if (in.isMasked()) {
      if (!out.isMasked() || !out.isMaskWritable()) {
         LogIO os(LogOrigin("ImagePolariemtry", "copyMask(...)", WHERE));
         os << LogIO::WARN << "The input image is masked but the output image does "<< endl;
         os << "not have a writeable mask.  Therefore no mask will be transferred" << LogIO::POST;
         return;
      }
   
// Use the same stepper for input and output.
    
      IPosition cursorShape = out.niceCursorShape();
      LatticeStepper stepper (out.shape(), cursorShape, LatticeStepper::RESIZE);

// Create an iterator for the output to setup the cache.
// It is not used, because using putSlice directly is faster and as easy.
 
      LatticeIterator<Float> dummyIter(out);
      RO_LatticeIterator<Float> iter(in, stepper);   
      for (iter.reset(); !iter.atEnd(); iter++) {
         out.putMaskSlice(in.getMaskSlice(iter.position(), iter.cursorShape()), 
                          iter.position());
      }
   }   
}  
   

void ImagePolarimetry::copyMiscellaneous (ImageInterface<Float>& out) const
{
   out.setMiscInfo(itsInImagePtr->miscInfo());
   out.setImageInfo(itsInImagePtr->imageInfo());
   out.mergeTableLogSink(itsInImagePtr->logSink());
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



void ImagePolarimetry::fiddleStokesCoordinate(ImageInterface<Float>& ie, Stokes::StokesTypes type) const
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
   if (!pC->setWorldAxisUnits(axisUnits, True)) {
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
                                            Float& rChiSqFitted, const Vector<uInt>& sortidx,
                                            const Vector<Float>& wsq2, const Vector<Float>& pa2, 
                                            const Array<Float>& paerr2, Float rmFg, 
                                            Float rmMax, Float maxPaErr)
//
// wsq is lambda squared in m**2 in increasing wavelength order
// pa is position angle in radians
// paerr is pa error in radians
// maxPaErr is maximum tolerated error in position angle
// rmfg is a user specified foreground RM rad/m/m
// rmmax is a user specified maximum RM
//
{

// Abandon if less than 2 points

   uInt n = sortidx.nelements();
   rmFitted = rmErrFitted = pa0Fitted = pa0ErrFitted = rChiSqFitted = 0.0;
   if (n<2) return False;

// Sort into order of decreasing frequency (increasing wavelength)
// Better for user to do this when they make the image. Warn them.

   Vector<Float> paerr1(paerr2.nonDegenerate(0));
   Vector<Float> paerr(sortidx.nelements());
   Vector<Float> pa(sortidx.nelements());
   Vector<Float> wsq(sortidx.nelements());

// Sort into decreasing frequency order and correct for foreground rotation
// Remember wsq already sorted.  Discard points that are too noisy.

   uInt j = 0;
   for (uInt i=0; i<n; i++) {
      if (paerr1(sortidx(i)) < maxPaErr) {
         pa(j) = pa2(sortidx(i)) - rmFg*wsq(i);
         paerr(j) = paerr1(sortidx(i));
         wsq(j) = wsq2(i);
         j++;
      }
   }
   n = j;
   pa.resize(n,True);
   paerr.resize(n,True);
   wsq.resize(n);

// Treat supplementary and primary points separately

   Bool ok;
   if (n==2) {
      ok = rmSupplementaryFit(rmFitted, rmErrFitted, pa0Fitted, pa0ErrFitted, 
                              rChiSqFitted, wsq, pa, paerr);
   } else {
      ok = rmPrimaryFit(rmFitted, rmErrFitted, pa0Fitted, pa0ErrFitted, 
                        rChiSqFitted, wsq, pa, paerr, rmMax);
   }

// Put position angle into the range 0->pi

   if (ok) {
      Float tmp = fmod(pa0Fitted, C::pi);  
      if (tmp < 0.0) tmp += C::pi;   
      pa0Fitted = tmp;

// Add foreground back on

      rmFitted += rmFg;
   }
   return ok;
}



ImageExpr<Float> ImagePolarimetry::makeStokesExpr(ImageInterface<Float>* imPtr,
                                                 const String& s, const String& name) const
{
   LogIO os(LogOrigin("ImagePolarimetry", "makeStokesExpr(...)", WHERE));
   if (imPtr==0) {
      os << "This image does not have Stokes " << s << LogIO::EXCEPTION;
   }

// Make node.  

   LatticeExprNode node(*imPtr);

// Make expression

   LatticeExpr<Float> le(node);
   ImageExpr<Float> ie(le, name);
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


Bool ImagePolarimetry::rmPrimaryFit(Float& rmFitted, Float& rmErrFitted,
                                    Float& pa0Fitted, Float& pa0ErrFitted, 
                                    Float& rChiSqFitted, const Vector<Float>& wsq, 
                                    const Vector<Float>& pa, const Vector<Float>& paerr, 
                                    Float rmMax)
{

// Assign position angle to longest wavelength consistent with
// RM < RMMax

   const uInt n = wsq.nelements();
   Double dwsq = wsq(n-1) - wsq(0);
//
   Float ppa = abs(rmMax)*dwsq + pa(0);
   Float diff = ppa - pa(n-1);
//cout << "diff0 = " << diff << endl;
   Float t = 0.5;
   if (diff < 0) t = -0.5;
   Int maxnpi = ifloor(diff/C::pi + t);
//
   ppa = -abs(rmMax)*dwsq + pa(0);
   diff = ppa - pa(n-1);
//cout << "diff1 = " << diff << endl;
   t = 0.5;
   if (diff < 0) t = -0.5;
   Int minnpi = ifloor(diff/C::pi + t);
// cout << "primary:: minnpi, maxnpi=" << minnpi << ", " << maxnpi << endl;
//
   uInt istore = 0;
   const uInt nstore = maxnpi - minnpi + 1;
   Vector<Float> storeRm(nstore);
   Vector<Float> storeRmErr(nstore);
   Vector<Float> storePa0(nstore);
   Vector<Float> storePa0Err(nstore);
   Vector<Float> storeRChiSq(nstore);

// Loop over range of n*pi ambiguity

   Vector<Float> fitpa(n);
   Vector<Float> pars;
   Float rm0;
   Int npi;
   for (Int h=minnpi; h<=maxnpi; h++) {
//cout << "h=" << h << endl;
     fitpa(n-1) = pa(n-1) + C::pi*h;
//cout << "fitps(n-1) = " << fitpa(n-1) << endl;
     rm0 = (fitpa(n-1) - pa(0))/ dwsq;

// Assign position angles to remaining wavelengths

     for (uInt k=1; k<n-2; k++) {
       ppa = pa(0) + rm0*(wsq(k)-wsq(0));
       diff = ppa - pa(k);
//
       t = 0.5;
       if (diff < 0) t = -0.5;
       npi = ifloor(diff/C::pi + t);
       fitpa(k) = pa(k) + npi*C::pi;
/*
cout  << "rm0, k, npi, pa(k), fitpa(k) = " << rm0 << ", " << k << ", " << npi << ", " 
      << pa(k) << ", " << fitpa(k) << endl;
*/
     }
     fitpa(0) = pa(0);

/*
   PGPlotter pl("/xs");
   Float minVal, maxVal;
   minMax(minVal, maxVal, pa);
   pl.sci(1);
   pl.env(wsq(0), wsq(n-1), minVal, maxVal, 0, 0);
   pl.line(wsq, pa);
*/
//   pl.sci(7);
//   pl.line(wsq, fitpa);



// Do least squares fit
// ***** get rid of following line when it all works

//     fitpa = pa;

     if (!rmLsqFit (pars, wsq, fitpa, paerr)) return False;

// Store fit for this guess at the NPI ambiguity

     storeRm(istore) = pars(0);      // Fitted RM
     storeRmErr(istore) = pars(1);   // Error in RM
     storePa0(istore) = pars(2);     // Fitted intrinsic angle
     storePa0Err(istore) = pars(3);  // Error in angle
     storeRChiSq(istore) = pars(4);  // Reduced chi squared
   }

// Find the best fit

   IPosition minPos(1), maxPos(1);
   Float minVal, maxVal;
   minMax(minVal, maxVal, minPos, maxPos, storeRChiSq);
   uInt idx = minPos(0);
//
   rmFitted = storeRm(idx);
   rmErrFitted = storeRmErr(idx);
   pa0Fitted = storePa0(idx);
   pa0ErrFitted = storePa0Err(idx);
   rChiSqFitted = storeRChiSq(idx);
   if (n > 2) rChiSqFitted /= Float(n - 2);
   return True;
}



Bool ImagePolarimetry::rmSupplementaryFit(Float& rmFitted, Float& rmErrFitted,
                                          Float& pa0Fitted, Float& pa0ErrFitted, 
                                          Float& rChiSqFitted,  const Vector<Float>& wsq, 
                                          const Vector<Float>& pa, const Vector<Float>& paerr)
{

// For supplementary points find lowest residual RM

   const uInt nstore = 5;
   Vector<Float> storeAbsRm(nstore);
   Vector<Float> storeRm(nstore);
   Vector<Float> storeRmErr(nstore);
   Vector<Float> storePa0(nstore);
   Vector<Float> storePa0Err(nstore);
   Vector<Float> storeRChiSq(nstore);
   const uInt n = wsq.nelements();
//
   Vector<Float> fitpa(pa.copy());
   Vector<Float> pars;
   for (Int i=-2; i<3; i++) {
     fitpa(n-1) = pa(n-1) + C::pi*i;

// Do least squares fit

     if (!rmLsqFit (pars, wsq, fitpa, paerr)) return False;

// Store fit for this guess at the NPI ambiguity

     storeAbsRm(i+2) = abs(pars(0));      // Fitted RM
     storeRm(i+2) = pars(0);              // Fitted RM
     storeRmErr(i+2) = pars(1);           // Error in RM
     storePa0(i+2) = pars(2);             // Fitted intrinsic angle
     storePa0Err(i+2) = pars(3);          // Error in angle
     storeRChiSq(i+2) = pars(4);          // Reduced chi squared
   }

// Return the fit with the smallest absolute RM

   IPosition minPos(1), maxPos(1);
   Float minVal, maxVal;
   minMax(minVal, maxVal, minPos, maxPos, storeAbsRm);
   uInt idx = minPos(0);
//
   rmFitted = storeRm(idx);
   rmErrFitted = storeRmErr(idx);
   pa0Fitted = storePa0(idx);
   pa0ErrFitted = storePa0Err(idx);
   rChiSqFitted = storeRChiSq(idx);
   if (n > 2) rChiSqFitted /= Float(n - 2);
   return True;
}




Bool ImagePolarimetry::rmLsqFit (Vector<Float>& pars, const Vector<Float>& wsq, 
                                 const Vector<Float> pa, const Vector<Float>& paerr) const
{

// Perform fit on unmasked data

//   uInt n = pa.nelements();
//   cout << "pa = " << pa << endl;
//   cout << "paerr = " << paerr << endl;
//
   Vector<Float> solution;
   try {
     solution = itsFitterPtr->fit(wsq, pa, paerr);
   } catch (AipsError x) {
      return False;
   } end_try;

  
// Return values of fit

//   cout << "Solution = " << solution << endl;
   Vector<Double> cv = itsFitterPtr->compuCovariance().diagonal();
//
   pars.resize(5);
   pars(0) = solution(1);
   pars(1) = sqrt(cv(1));
   pars(2) = solution(0);
   pars(3) = sqrt(cv(0));
   pars(4) = itsFitterPtr->chiSquare(wsq, pa, paerr, solution);

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
   itsStokesStatsPtr[index]->getSigma(sigmaA);
   if (sigmaA.nelements()==0) {
      LogIO os(LogOrigin("ImagePolarimetry", "sigma(...)", WHERE));
      os << "No good points in clipped determination of the noise " 
         << "for the Stokes " << stokesName(index) << " image" << LogIO::EXCEPTION;
   }
//
   itsOldClip = clip2;
   return sigmaA(IPosition(1,0));
}
