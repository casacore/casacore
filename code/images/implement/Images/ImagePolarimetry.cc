//# ImagePolarimetry.cc: polarimetric analysis
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

#include <trial/Images/ImagePolarimetry.h>

#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/StokesCoordinate.h>
#include <trial/Coordinates/LinearCoordinate.h>
#include <aips/Exceptions/Error.h>
#include <trial/Lattices/LCSlicer.h>
#include <trial/Lattices/LatticeExprNode.h>
#include <trial/Lattices/LatticeExpr.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>
#include <trial/Images/ImageInterface.h>
#include <trial/Images/SubImage.h>
#include <trial/Images/ImageExpr.h>
#include <trial/Images/ImageFFT.h>
#include <trial/Images/ImageRegion.h>
#include <trial/Images/ImageStatistics.h>
#include <trial/Images/ImageSummary.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/NumericTraits.h>
#include <aips/Quanta/QC.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>


// Public functions

ImagePolarimetry::ImagePolarimetry (const ImageInterface<Float>& image)
: itsInImagePtr(0),
  itsIImagePtr(0),
  itsQImagePtr(0),
  itsUImagePtr(0),
  itsVImagePtr(0)
{
   itsInImagePtr = image.cloneII();
   findStokes();
}


ImagePolarimetry::ImagePolarimetry(const ImagePolarimetry &other) 
: itsInImagePtr(0),
  itsIImagePtr(0),
  itsQImagePtr(0),
  itsUImagePtr(0),
  itsVImagePtr(0)
{
   operator=(other);
}


ImagePolarimetry &ImagePolarimetry::operator=(const ImagePolarimetry &other)
{
   if (this != &other) {
      if (itsInImagePtr != 0) delete itsInImagePtr;
      itsInImagePtr = other.itsInImagePtr->cloneII();
//
      if (itsIImagePtr!=0) delete itsIImagePtr;
      if (other.itsIImagePtr!=0) {
         itsIImagePtr = other.itsIImagePtr->cloneII();
      }
//
      if (itsQImagePtr!=0) delete itsQImagePtr;
      if (other.itsQImagePtr!=0) {
         itsQImagePtr = other.itsQImagePtr->cloneII();
      }
//
      if (itsUImagePtr!=0) delete itsUImagePtr;
      if (other.itsUImagePtr!=0) {
         itsUImagePtr = other.itsUImagePtr->cloneII();
      }
//
      if (itsVImagePtr!=0) delete itsVImagePtr;
      if (other.itsVImagePtr!=0) {
         itsVImagePtr = other.itsVImagePtr->cloneII();
     }
   }
   return *this;
}


ImagePolarimetry::~ImagePolarimetry()
{
   cleanup();
}


// Public methods


ImageExpr<Float> ImagePolarimetry::linPolInt(Bool debias, Float var) const
{
   LogIO os(LogOrigin("ImagePolarimetry", "linPolInt(...)", WHERE));
   if (itsQImagePtr==0 && itsUImagePtr==0) {
      os << "This image does not have Stokes Q and U so cannot provide linear polarization" << LogIO::EXCEPTION;
   }

// Make node.  

   LatticeExprNode node = makePolIntNode(os, debias, var, True, False);

// Make expression

   LatticeExpr<Float> le(node);
   ImageExpr<Float> ie(le, String("linearlyPolarizedIntensity"));

// Fiddle Stokes coordinate in ImageExpr

   fiddleStokesCoordinate(ie, Stokes::Plinear);
//
   return ie;
}


ImageExpr<Float> ImagePolarimetry::linPolPosAng() const
{
   LogIO os(LogOrigin("ImagePolarimetry", "linPolPosAng(...)", WHERE));
   if (itsQImagePtr==0 && itsUImagePtr==0) {
      os << "This image does not have Stokes Q and U so cannot provide linear polarization" << LogIO::EXCEPTION;
   }

// Make expression

   LatticeExprNode node(pa(*itsUImagePtr, *itsQImagePtr)); 
   LatticeExpr<Float> le(node);
   ImageExpr<Float> ie(le, String("linearlyPolarizedPositionAngle"));

// Fiddle Stokes coordinate

   fiddleStokesCoordinate(ie, Stokes::Pangle);
//
   return ie;
}

ImageExpr<Float> ImagePolarimetry::stokesI() const
{
   return makeStokesExpr(itsIImagePtr, String("I"), String("StokesI"));
}

ImageExpr<Float> ImagePolarimetry::stokesQ() const
{
   return makeStokesExpr(itsQImagePtr, String("Q"), String("StokesQ"));
}

ImageExpr<Float> ImagePolarimetry::stokesU() const
{
   return makeStokesExpr(itsUImagePtr, String("U"), String("StokesU"));
}

ImageExpr<Float> ImagePolarimetry::stokesV() const
{
   return makeStokesExpr(itsVImagePtr, String("V"), String("StokesV"));
}

void ImagePolarimetry::summary(LogIO& os) const
{
   ImageSummary<Float> s(*itsInImagePtr);
   s.list(os);
}



ImageExpr<Float> ImagePolarimetry::totPolInt(Bool debias, Float var) const
{
   LogIO os(LogOrigin("ImagePolarimetry", "totPolInt(...)", WHERE));
//
   Bool doLin = (itsQImagePtr!=0 && itsUImagePtr!=0);
   Bool doCirc = (itsVImagePtr!=0);
   AlwaysAssert((doLin||doCirc), AipsError);    // Should never happen

// Make node.  

   LatticeExprNode node = makePolIntNode(os, debias, var, doLin, doCirc);

// Make expression

   LatticeExpr<Float> le(node);
   ImageExpr<Float> ie(le, String("totalPolarizedIntensity"));

// Fiddle Stokes coordinate in ImageExpr

   fiddleStokesCoordinate(ie, Stokes::Ptotal);
//
   return ie;
}


ImageExpr<Float> ImagePolarimetry::fracLinPol(Bool debias, Float var) const
{
   LogIO os(LogOrigin("ImagePolarimetry", "fracLinPol(...)", WHERE));
   if (itsQImagePtr==0 && itsUImagePtr==0) {
      os << "This image does not have Stokes Q and U so cannot provide linear polarization" << LogIO::EXCEPTION;
   }
   if (itsIImagePtr==0) {
      os << "This image does not have Stokes I so cannot provide fractional linear polarization" << LogIO::EXCEPTION;
   }

// Make nodes

   LatticeExprNode nodePol = makePolIntNode(os, debias, var, True, False);
   LatticeExprNode nodeI(*itsIImagePtr);

// Make expression

   LatticeExpr<Float> le(nodePol/nodeI);
   ImageExpr<Float> ie(le, String("fractionalLinearPolarization"));

// Fiddle Stokes coordinate in ImageExpr

   fiddleStokesCoordinate(ie, Stokes::PFlinear);
//
   return ie;
}


ImageExpr<Float> ImagePolarimetry::fracTotPol(Bool debias, Float var) const
{
   LogIO os(LogOrigin("ImagePolarimetry", "fracTotPol(...)", WHERE));
//
   Bool doLin = (itsQImagePtr!=0 && itsUImagePtr!=0);
   Bool doCirc = (itsVImagePtr!=0);
   AlwaysAssert((doLin||doCirc), AipsError);    // Should never happen
   if (itsIImagePtr==0) {
      os << "This image does not have Stokes I so cannot provide fractional linear polarization" << LogIO::EXCEPTION;
   }


// Make nodes

   LatticeExprNode nodePol = makePolIntNode(os, debias, var, doLin, doCirc);
   LatticeExprNode nodeI(*itsIImagePtr);

// Make expression

   LatticeExpr<Float> le(nodePol/nodeI);
   ImageExpr<Float> ie(le, String("fractionalTotalPolarization"));

// Fiddle Stokes coordinate in ImageExpr

   fiddleStokesCoordinate(ie, Stokes::PFtotal);
//
   return ie;
}



void ImagePolarimetry::fourierRotationMeasure(ImageInterface<Complex>& lag,
                                              Bool zeroZeroLag)
{
   LogIO os(LogOrigin("ImagePolarimetry", "fourierRotationMeasure(...)", WHERE));

// Make Complex (Q,U) image

   LatticeExprNode node;
   if (zeroZeroLag) {
      LatticeExprNode node1( (*itsQImagePtr) - sum(*itsQImagePtr));
      LatticeExprNode node2( (*itsUImagePtr) - sum(*itsUImagePtr));
      node = LatticeExprNode(complex(node1, node2));
   } else {
      node = LatticeExprNode(complex(*itsQImagePtr, *itsUImagePtr));
   }
   LatticeExpr<Complex> le(node);
   ImageExpr<Complex> ie(le, String("Complex Polarization"));

// Find spectral coordinate

   const CoordinateSystem& cSys = ie.coordinates();
   Int afterCoord = -1;
   Int coord = cSys.findCoordinate(Coordinate::SPECTRAL, afterCoord);
   if (coord<0) {
      os << "No spectral coordinate to do FourierTransform over" << endl;
   }
//
   Vector<Int> pixelAxes = cSys.pixelAxes(coord);
   Vector<Bool> axes(ie.ndim(),False);
   axes(pixelAxes(0)) = True;
   Quantum<Double> f = findCentralFrequency(cSys, coord, ie.shape()(pixelAxes(0)));

// Do FFT of spectral coordinate

   ImageFFT fftserver;
   fftserver.fft(ie, axes);

// Recover result. Coordinates are updated to include Fourier coordinate,
// miscellaneous things (MiscInfo, ImageInfo, units, history) and mask
// (if output has one) are copied to lag

   fftserver.getComplex(lag);
   fiddleStokesCoordinate(lag, Stokes::Plinear);
   fiddleTimeCoordinate(lag, f, coord);
}


IPosition ImagePolarimetry::stokesShape() const
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
      itsIImagePtr = makeSubImage(blc, trc, pixelAxes(0), pix);
   }
   if (stokes.toPixel(pix, Stokes::Q)) {
      itsQImagePtr = makeSubImage(blc, trc, pixelAxes(0), pix);
   }
   if (stokes.toPixel(pix, Stokes::U)) {
      itsUImagePtr = makeSubImage(blc, trc, pixelAxes(0), pix);
   }
   if (stokes.toPixel(pix, Stokes::V)) { 
      itsVImagePtr = makeSubImage(blc, trc, pixelAxes(0), pix);
   }
//
   if ( (itsQImagePtr!=0 && itsUImagePtr==0) ||
        (itsQImagePtr==0 && itsUImagePtr!=0)) {
      cleanup();
      os << "This Stokes coordinate has only one of Q and U. This is not useful" << LogIO::EXCEPTION;
   }
   if (itsQImagePtr==0 && itsUImagePtr==0 && itsVImagePtr==0) {
      cleanup();
      os << "This image has no Stokes Q, U, or V.  This is not useful" << LogIO::EXCEPTION;
   }
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

LatticeExprNode ImagePolarimetry::makePolIntNode(LogIO& os, Bool debias, Float var,
                                                 Bool doLin, Bool doCirc) const
{ 
   LatticeExprNode linNode, circNode, node;
//
   Float varQ2(0.0), varU2(0.0), varV2(0.0);
   if (doLin) {
      if (debias) {
         if (var > 0.0) {
            varQ2 = var;
            varU2 = var;
         } else {
            Array<Float> varA;
            ImageStatistics<Float> stats(*itsQImagePtr, os, False);
            stats.getVariance(varA);
            varQ2 = varA(IPosition(1,0));
//
            stats.setNewImage(*itsUImagePtr);
            stats.getVariance(varA);
            varU2 = varA(IPosition(1,0));
         }
      }
      linNode = LatticeExprNode(pow(*itsUImagePtr,2)+pow(*itsQImagePtr,2));
   }
//
   if (doCirc) {
      if (debias) {
         if (var > 0.0) {
            varV2 = var;
         } else {
            Array<Float> varA;
            ImageStatistics<Float> stats(*itsVImagePtr, os, False);
            stats.getVariance(varA);
            varV2 = varA(IPosition(1,0));
         }
      }
      circNode = LatticeExprNode(pow(*itsVImagePtr,2));
   }
//
   if (doLin && doCirc) {
      if (debias) {
         Float tmp = (varQ2 + varU2 + varV2) / 3.0;
         node = linNode + circNode - LatticeExprNode(tmp);
         os << LogIO::NORMAL << "Debiasing with variance(Q,U,V) = " << tmp << LogIO::POST;
      } else {
         node = linNode + circNode;         
      }
   } else if (doLin) {
      if (debias) {
         Float tmp = (varQ2 + varU2) / 2.0;
         node = linNode - LatticeExprNode(tmp);
         os << LogIO::NORMAL << "Debiasing with variance(Q,U) = " << tmp << LogIO::POST;
      } else {
         node = linNode;
      }
   } else if (doCirc) {  
      if (debias) {
         Float tmp = varV2;
         node = circNode - LatticeExprNode(tmp);
         os << LogIO::NORMAL << "Debiasing with variance(V) = " << tmp << LogIO::POST;
      } else {
         node = circNode;
      }
   }
//
   return LatticeExprNode(sqrt(node));
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


Quantum<Double> ImagePolarimetry::findCentralFrequency(const CoordinateSystem& cSys, Int coord, Int shape) const
{
   const Coordinate& c = cSys.coordinate(coord);
   AlwaysAssert(c.nPixelAxes()==1,AipsError);
//
   Vector<Double> pixel(1);
   Vector<Double> world;
   pixel(0) = (shape - 1) / 2.0;
   if (!c.toWorld(world, pixel)) {
      LogIO os(LogOrigin("ImagePolarimetry", "findCentralFrequency(...)", WHERE));
      os << "Failed to convert pixel to world for SpectralCoordinate because " 
         << c.errorMessage() << LogIO::EXCEPTION;
  }
  Vector<String> units = c.worldAxisUnits();
  return Quantum<Double>(world(0), units(0));
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

void ImagePolarimetry::cleanup()
{
   delete itsInImagePtr;   
   delete itsIImagePtr;
   delete itsQImagePtr;
   delete itsUImagePtr;
   delete itsVImagePtr;
//
   itsInImagePtr = 0;
   itsIImagePtr = 0;
   itsQImagePtr = 0;
   itsUImagePtr = 0;
   itsVImagePtr = 0;
}
