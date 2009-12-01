//# ImageProfileFit.cc: Class to fit spectra from images
//# Copyright (C) 1997,1998,1999,2000,2001,2002,2003
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
//# Correspondence concerning AIPS++ should be addessed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#   $Id$

#include <images/Images/ImageProfileFit.h>

#include <casa/Arrays/IPosition.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Containers/Record.h>
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <coordinates/Coordinates/SpectralCoordinate.h>
#include <coordinates/Coordinates/CoordinateUtil.h>
#include <casa/Exceptions/Error.h>
#include <images/Images/ImageInterface.h>
#include <images/Regions/ImageRegion.h>
#include <images/Images/SubImage.h>
#include <lattices/Lattices/TiledLineStepper.h>
#include <lattices/Lattices/MaskedLatticeIterator.h>
#include <lattices/Lattices/LCRegion.h>
#include <lattices/Lattices/MaskedLattice.h>
#include <lattices/Lattices/LatticeUtilities.h>
#include <casa/Logging/LogIO.h>
#include <casa/BasicSL/Constants.h>
#include <casa/BasicMath/Math.h>
#include <casa/System/ProgressMeter.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Utilities/Assert.h>
#include <components/SpectralComponents/SpectralFit.h>
#include <components/SpectralComponents/SpectralEstimate.h>
#include <components/SpectralComponents/SpectralElement.h>
#include <components/SpectralComponents/SpectralList.h>

//#include <casa/OS/Timer.h>


#include <casa/ostream.h>


namespace casa { //# NAMESPACE CASA - BEGIN

ImageProfileFit::ImageProfileFit()
: itsImagePtr(0),
  itsSigmaImagePtr(0),
  itsFitDone(False),
  itsMask(0),
  itsSigma(0),
  itsSpectralFitPtr(0),
  itsProfileAxis(-1),
  itsDoppler(""),
  itsXAbs(True),
  itsFitRegion(False)
{
  UnitMap::putUser("pix",UnitVal(1.0), "pixel units");
  itsSpectralFitPtr = new SpectralFit;
}

ImageProfileFit::ImageProfileFit(const ImageProfileFit& other)
: itsImagePtr(0),
  itsSigmaImagePtr(0),
  itsFitDone(other.itsFitDone),
  itsMask(other.itsMask.copy()),
  itsSigma(other.itsSigma.copy()),
  itsCoords(other.itsCoords),
  itsProfileAxis(other.itsProfileAxis),
  itsDoppler(other.itsDoppler),
  itsXAbs(other.itsXAbs),
  itsFitRegion(other.itsFitRegion)
{
   UnitMap::putUser("pix",UnitVal(1.0), "pixel units");
   itsX = other.itsX;   // Get a copy of the vector
   itsY = other.itsY;   // Get a copy of the vector
//
   itsSpectralFitPtr = new SpectralFit (*(other.itsSpectralFitPtr));
}


ImageProfileFit::~ImageProfileFit()
{
   delete itsSpectralFitPtr;
   itsSpectralFitPtr = 0;
   if (itsImagePtr) delete itsImagePtr;
   if (itsSigmaImagePtr) delete itsSigmaImagePtr;
}


ImageProfileFit& ImageProfileFit::operator=(const ImageProfileFit& other)
{
   if (this != &other) {
      if (itsImagePtr) {
         delete itsImagePtr;
         itsImagePtr = 0;
      }
      if (other.itsImagePtr) {
         itsImagePtr = other.itsImagePtr->cloneII();
      }
//
      if (itsSigmaImagePtr) {
         delete itsSigmaImagePtr;
         itsSigmaImagePtr = 0;
      }
      if (other.itsSigmaImagePtr) {
         itsSigmaImagePtr = other.itsSigmaImagePtr->cloneII();
      }
//
      itsFitDone = other.itsFitDone;

// Does this look after resizing of itsX, itsY vectors ?

      itsX = other.itsX;
      itsY = other.itsY;
//
      itsMask.resize(0);
      itsMask = other.itsMask.copy();
      itsSigma.resize(0);
      itsSigma= other.itsSigma.copy();
//
      itsCoords = other.itsCoords;
      itsProfileAxis = other.itsProfileAxis;
//
      itsDoppler = other.itsDoppler;
      itsXAbs = other.itsXAbs;
      itsFitRegion = other.itsFitRegion;
//
      delete itsSpectralFitPtr; 
      itsSpectralFitPtr = 0;
      itsSpectralFitPtr = new SpectralFit (*(other.itsSpectralFitPtr));
   }
   return *this;
}

void ImageProfileFit::setData (const ImageInterface<Float>& image, 
                               const ImageRegion& region,
                               uInt profileAxis, Bool average)
{
   itsCoords = image.coordinates(); 
   itsProfileAxis = profileAxis;
   const SubImage<Float> subImage(image, region);
   const Slicer& sl = region.asLCRegion().boundingBox();
//
   const ImageInterface<Float>* pSubSigma = 0;
//
   setData(pSubSigma, subImage, sl, average);
}

void ImageProfileFit::setData (const ImageInterface<Float>& image, 
                               const ImageInterface<Float>& sigma, 
                               const ImageRegion& region,
                               uInt profileAxis, Bool average)
{
   itsCoords = image.coordinates(); 
   itsProfileAxis = profileAxis;
   const SubImage<Float> subImage(image, region);
   const Slicer& sl = region.asLCRegion().boundingBox();
//
   const SubImage<Float> subSigma(sigma, region);
   const ImageInterface<Float>* pSubSigma = &subSigma;
//
   setData(pSubSigma, subImage, sl, average);
}


void ImageProfileFit::setData (const ImageInterface<Float>& image,
                               uInt profileAxis, Bool average)
{
   itsCoords = image.coordinates(); 
   itsProfileAxis = profileAxis;
   IPosition start(image.ndim(),0);
   Slicer sl(start, image.shape(), Slicer::endIsLength);
//
   const ImageInterface<Float>* pSubSigma = 0;
//
   setData(pSubSigma, image, sl, average);
}




void ImageProfileFit::setData (const ImageInterface<Float>& image,
                               const ImageInterface<Float>& sigma,
                               uInt profileAxis, Bool average)
{
   itsCoords = image.coordinates(); 
   itsProfileAxis = profileAxis;
   IPosition start(image.ndim(),0);
   Slicer sl(start, image.shape(), Slicer::endIsLength);
//
   const ImageInterface<Float>* pSubSigma = &sigma;
//
   setData(pSubSigma, image, sl, average);
}

void ImageProfileFit::setData (const CoordinateSystem& cSys,
                               uInt profileAxis,
                               const Quantum<Vector<Float> >& x, 
                               const Quantum<Vector<Float> >& y,
                               const Vector<Float>& sigma,
                               Bool isAbs, const String& doppler)
{
   Vector<Bool> mask;
   setData (cSys, profileAxis, x, y, mask, sigma, isAbs, doppler);
}

void ImageProfileFit::setData (const CoordinateSystem& cSys,
                               uInt profileAxis,
                               const Quantum<Vector<Float> >& x, 
                               const Quantum<Vector<Float> >& y,
                               const Vector<Bool>& mask,
                               const Vector<Float>& sigma,
                               Bool isAbs, const String& doppler)
{
   uInt n = x.getValue().nelements();
   AlwaysAssert(n==y.getValue().nelements(),AipsError);
   AlwaysAssert(n>0,AipsError);
//
   itsMask.resize(n);
   if (mask.nelements()!=n) {
      itsMask = True;
   } else {
      itsMask = mask;
   }      
//
   itsSigma.resize(n);
   if (sigma.nelements()!=n) {
      itsSigma = 1.0;
   } else {
      itsSigma = sigma.copy();
   }      
//
   itsCoords =  cSys;
   itsProfileAxis = profileAxis;
   itsFitRegion = False;
   itsDoppler = doppler;
   itsXAbs = isAbs;

// Does this take care of resizing ?

   itsX = x;
   itsY = y;
}



uInt ImageProfileFit::addElements (const RecordInterface& rec)
//
// Decode the record and add the SpectralElements to the
// private fitter. We convert the units of the model
// to those of the data source
//
{
   LogIO os(LogOrigin("ImageProfileFit", "addElements", WHERE));
   if (!rec.isDefined("xunit")) {
      os << "Record holding model is missing 'xunit' field" << LogIO::EXCEPTION;
   }
   if (!rec.isDefined("yunit")) {
      os << "Record holding model is missing 'yunit' field" << LogIO::EXCEPTION;
   }
   if (!rec.isDefined("xabs")) {
      os << "Record holding model is missing 'xabs' field" << LogIO::EXCEPTION;
   }
   if (!rec.isDefined("elements")) {
      os << "Record holding model is missing 'elements' field" << LogIO::EXCEPTION;
   }

// Setup input units etc

   String xUnitIn = rec.asString("xunit");
   String dopplerIn;
   if (rec.isDefined("doppler")) {
      dopplerIn = rec.asString("doppler");
   }
   Bool xAbsIn = rec.asBool("xabs");
   String yUnitIn = rec.asString("yunit");

// Setup output units

   String xUnitOut = itsX.getFullUnit().getName();
   Bool xAbsOut = itsXAbs;
   String dopplerOut = itsDoppler;
   String yUnitOut = itsY.getFullUnit().getName();

// Loop over elements in record

   if (rec.dataType("elements") != TpRecord) {
     os << "Record holding model is invalid - 'elements' is not a record" << LogIO::EXCEPTION;
   }
   Record rec2 = rec.asRecord("elements");
//
   const uInt n = rec2.nfields();
   if (n <=0) {
     os << "The 'elements' field of record is of zero length" << LogIO::EXCEPTION;
   }
//
   String error;
   for (uInt i=0; i<n; i++) {
      Record rec3 =  rec2.asRecord(i);
      SpectralElement se;
      if (!se.fromRecord(error, rec3)) {
         os << error << LogIO::EXCEPTION;
      }

// Convert element to required units

      SpectralElement seOut = convertSpectralElement (se, xAbsIn, xAbsOut,
                                                      xUnitIn, xUnitOut,
                                                      dopplerIn, dopplerOut,
                                                      yUnitIn, yUnitOut);

// See if we have the 'fixed' record; set fixed parameters as indicated

      Vector<Bool> fixed;
      if (rec3.isDefined("fixed")) fixed = rec3.asArrayBool("fixed");
//
      if (fixed.nelements()!=0) {
         if (seOut.getType()==SpectralElement::GAUSSIAN) {
            if (fixed.nelements()==3) {
               if (fixed(0)) seOut.fixAmpl(True);
               if (fixed(1)) seOut.fixCenter(True);
               if (fixed(2)) seOut.fixFWHM(True);
            } else {
               os << "'fixed' vector must be of length 3 for a Gaussian" << LogIO::EXCEPTION;
            }
         } else {
            os << "element type not supported" << LogIO::EXCEPTION;
         }
      }
//
      itsSpectralFitPtr->addFitElement(seOut);
   }
//
   itsSpectralFitter = *itsSpectralFitPtr;
   return itsSpectralFitPtr->list().nelements() - 1;
}

Bool ImageProfileFit::getList (RecordInterface& rec, 
                               Bool xAbsOut,
                               const String& xUnitOut,
                               const String& dopplerOut)
{
   const SpectralList& list = itsSpectralFitter.list();
   SpectralList list2 = filterList(list);
   Bool ok = getElements(rec, xAbsOut, xUnitOut, dopplerOut, list2);
   return ok;
}

SpectralList ImageProfileFit::getList () const
{
   const SpectralList& list = itsSpectralFitter.list();
   return filterList(list);
}



Bool ImageProfileFit::getElements (RecordInterface& rec,
                                   Bool xAbsOut,
                                   const String& xUnitOut,
                                   const String& dopplerOut,
                                   const SpectralList& list)

// 
// Convert internal model to a record
//
{
   LogIO os(LogOrigin("ImageProfileFit", "getElements", WHERE));
   String pix("pix");

// Setup units

   Bool xAbsIn = itsXAbs;
   String xUnitIn = itsX.getFullUnit().getName();
   String dopplerIn = itsDoppler;
   String yUnitIn = itsY.getFullUnit().getName();
//
   String yUnitOut = yUnitIn;
//
   Record recVec;
   const uInt n = list.nelements();
   for (uInt i=0; i<n; i++) {

// Convert element to desired units

      SpectralElement seOut = convertSpectralElement (list[i], xAbsIn, xAbsOut,
                                                      xUnitIn, xUnitOut,
                                                      dopplerIn, dopplerOut,
                                                      yUnitIn, yUnitOut);

// Fill record for this element

      Record recEl;
      String error;
      if (!seOut.toRecord(error, recEl)) {
         os << error << LogIO::EXCEPTION;
      }

// Stick it in output vector record

      recVec.defineRecord(i, recEl);
   }

// Put vector into field "elements"

   rec.defineRecord("elements", recVec);

// Add extra fields

   rec.define("xunit", xUnitOut);
   rec.define("xabs", xAbsOut);
   rec.define("doppler", dopplerOut);
   rec.define("yunit", yUnitOut);
//
   return True;
}


void ImageProfileFit::reset () 
{
   delete itsSpectralFitPtr;
   itsSpectralFitPtr = 0;
   itsSpectralFitPtr = new SpectralFit;
   itsSpectralFitter = *itsSpectralFitPtr;
   itsFitDone = False;
}


Bool ImageProfileFit::estimate (uInt nMax)
{
   delete itsSpectralFitPtr;
   itsSpectralFitPtr = 0;
   itsSpectralFitPtr = new SpectralFit;

// The estimate is made with x-units of absolute 0-rel pixels
// so we need to convert it to the data source units

   SpectralEstimate est;
   est.setQ(5);
   Vector<Float> der(itsY.getValue().nelements());
   const SpectralList& list = est.estimate(itsY.getValue(), &der);
//
   uInt nEl = 0;
   if (nMax==0) {
      nEl = list.nelements();
   } else {
      nEl = min(nMax, list.nelements());
   }
   if (nEl==0) {
      return False;
   }

// Setup input units 

   String xUnitIn("pix");
   String dopplerIn = itsDoppler;
   Bool xAbsIn = True;
   String yUnitIn = itsY.getFullUnit().getName();

// Setup output units

   String xUnitOut = itsX.getFullUnit().getName();
   Bool xAbsOut = itsXAbs;
   String dopplerOut = dopplerIn;
   String yUnitOut = yUnitIn;

// Convert

   for (uInt i=0; i<nEl; i++) {   
      SpectralElement se = convertSpectralElement (list[i], xAbsIn, xAbsOut,
                                                   xUnitIn, xUnitOut,
                                                   dopplerIn, dopplerOut,
                                                   yUnitIn, yUnitOut);

      itsSpectralFitPtr->addFitElement(se);
   }
   itsSpectralFitter = *itsSpectralFitPtr;
//
   return True;
}


uInt ImageProfileFit::nElements ()
{
   return itsSpectralFitPtr->list().nelements();
}
    

Bool ImageProfileFit::fit(Int order)
{
   itsSpectralFitter = *itsSpectralFitPtr;
//
   if (order >= 0) {

// Add polynomial to fitter

      SpectralElement el(order);
      itsSpectralFitter.addFitElement(el);
   }

// Do fit

   Bool ok = itsSpectralFitter.fit(itsSigma, 
                                   itsY.getValue(), 
                                   itsX.getValue(),  
                                   itsMask);
   itsFitDone = True;
   return ok;
}


void ImageProfileFit::residual(Vector<Float>& resid, 
                               const Vector<Float>& x) const
{
   resid.resize(0);
   resid = itsY.getValue();
   itsSpectralFitter.list().residual(resid, x);
}


void ImageProfileFit::residual(Vector<Float>& resid) const
{
   resid.resize(0);
   resid = itsY.getValue();
   itsSpectralFitter.list().residual(resid, itsX.getValue());
}


void ImageProfileFit::model(Vector<Float>& model, 
                            const Vector<Float>& x) const
{
   itsSpectralFitter.list().evaluate(model, x);
}

void ImageProfileFit::model(Vector<Float>& model) const
{
   itsSpectralFitter.list().evaluate(model, itsX.getValue());
}

// Private functions

void ImageProfileFit::collapse (Vector<Float>& profile, Vector<Bool>& mask,
                                uInt profileAxis, const MaskedLattice<Float>& lat)  const
{
   if (lat.ndim()==1) {

// Nothing to collapse

      profile.resize(0);
      mask.resize(0);
      profile = lat.get();
      mask = lat.getMask();
   } else {
      IPosition axes = IPosition::otherAxes(lat.ndim(), IPosition(1,profileAxis));
      Array<Float> p;
      Array<Bool> m;
      LatticeUtilities::collapse(p, m, axes, lat, True);
//
      uInt n = p.shape()(0);  
      profile.resize(n);
      mask.resize(n);
      profile = p.reform(IPosition(1,n));
      mask = m.reform(IPosition(1,n));
   }
 }




void ImageProfileFit::setData (const ImageInterface<Float>*& pSigma,
                               const ImageInterface<Float>& image,
                               const Slicer& sl, Bool average)
{
   itsXAbs = True;
   const uInt n = image.shape()(itsProfileAxis);
   if (average) {

// Average data over region except along profile axis

      Vector<Float> y;
      collapse(y, itsMask, itsProfileAxis, image);
      itsY = Quantum<Vector<Float> >(y, image.units());
//
      Vector<Float> x(y.nelements());
      indgen(x, Float(sl.start()(itsProfileAxis)));
      itsX = Quantum<Vector<Float> >(x, Unit("pix"));
//
      if (pSigma) {
         AlwaysAssert (image.shape().conform(pSigma->shape()), AipsError);
//
         IPosition axes = IPosition::otherAxes(pSigma->ndim(), IPosition(1,itsProfileAxis));
         Array<Float> p;
         Array<Bool> m;
         LatticeUtilities::collapse(p, m, axes, *pSigma, True);    // Ignore mask of sigma
         itsSigma.resize(0);
         itsSigma = p;
      } else {
         itsSigma.resize(n);
         itsSigma = 1.0;
      }
//
      itsFitRegion = False;
   } else {

// We are going to fit all profiles in the region. However, the
// estimate function might be used to fish out an initial
// estimate and it uses itsX and itsY...
// I think i need to rewrite this class !

      Vector<Float> x(n);
      indgen(x, Float(sl.start()(itsProfileAxis)));
      itsX = Quantum<Vector<Float> >(x, Unit("pix"));
//
      IPosition blc(image.ndim(),0);
      IPosition trc = blc;
      trc(itsProfileAxis) = n-1;
      Vector<Float> y = image.getSlice(blc,trc-blc+1,True);
      itsY = Quantum<Vector<Float> >(y, image.units());
//
      itsFitRegion = True;
      itsImagePtr = image.cloneII();
//
      if (pSigma) {
         AlwaysAssert (image.shape().conform(pSigma->shape()), AipsError);
         itsSigmaImagePtr = pSigma->cloneII();
      }
   }
}


SpectralElement ImageProfileFit::convertSpectralElement (const SpectralElement& elIn,
                                                         Bool xAbsIn, Bool xAbsOut,
                                                         const String& xUnitIn, 
                                                         const String& xUnitOut,
                                                         const String& dopplerIn,
                                                         const String& dopplerOut,
                                                         const String& yUnitIn,
                                                         const String& yUnitOut)
{
   LogIO os(LogOrigin("ImageProfileFit", "convertSpectralElement", WHERE));
//
   SpectralElement elOut(elIn);
   if (elOut.getType()==SpectralElement::GAUSSIAN) {

// Brightness

      if (yUnitIn != yUnitOut) {
         Unit tIn(yUnitIn);
         Quantum<Double> amp(elOut.getAmpl(), tIn);
         Quantum<Double> ampErr(elOut.getAmplErr(), tIn);
//
         Unit tOut(yUnitOut);
         elOut.setAmpl(amp.getValue(tOut));
         Vector<Double> errors;
         elOut.getError(errors);
         errors(0) = ampErr.getValue(tOut);
         elOut.setError(errors);
      }
//
      convertGaussianElementX (elOut, itsCoords, itsProfileAxis,
                               xAbsIn, xAbsOut,
                               xUnitIn, xUnitOut,
                               dopplerIn, dopplerOut);
   } else {
      os << "Elements of type " << elOut.getType() << " are not yet supported" << LogIO::EXCEPTION;
   }
//
   return elOut;
}




void ImageProfileFit::convertGaussianElementX (SpectralElement& el,
                                               CoordinateSystem& cSys,
                                               uInt profileAxis,
                                               Bool absIn, Bool absOut,
                                               const String& unitIn,
                                               const String& unitOut,
                                               const String& dopplerIn,
                                               const String& dopplerOut)
//
// The data source is an image
//
{
   LogIO os(LogOrigin("ImageProfileFit", "convertGaussianElementX", WHERE));
//
   Unit velUnit(String("m/s"));
   String pix("pix");

// See if we have something to do.  One of the doppler strings
// might be empty so only check if doing velocity.

   if (unitIn==unitOut && absIn==absOut) {
      Unit tIn(unitIn);
      Unit tOut(unitOut);
      if (tIn==velUnit && tOut==velUnit) {
         if (dopplerIn==dopplerOut) return;
      }
   }

// Assumes world and pixel axes the same.
// Should really check order as well...

   AlwaysAssert (cSys.nWorldAxes() == cSys.nPixelAxes(), AipsError);  
   const uInt n = cSys.nWorldAxes();

// Get dopplers sorted out

   MDoppler::Types dopplerTypeIn, dopplerTypeOut;
   if (Unit(unitIn)==velUnit) {
      if (dopplerIn.empty()) {
         os << "Input doppler is not specified" << LogIO::EXCEPTION;
      }
      if (!MDoppler::getType(dopplerTypeIn, dopplerIn)) {
         os << "Input doppler type is invalid" << LogIO::EXCEPTION;
      }
   }
   if (Unit(unitOut)==velUnit) {
      if (dopplerOut.empty()) {
         os << "Output doppler is not specified" << LogIO::EXCEPTION;
      }
      if (!MDoppler::getType(dopplerTypeOut, dopplerOut)) {
         os << "Output doppler type is invalid" << LogIO::EXCEPTION;
      }
   }

// Find input fractional error

   Vector<Double> errors, pars;
   el.getError(errors);
   el.get(pars);
   errors /= pars;

// Declare conversion vectors

   Vector<Double> coordIn(n), coordOut(n);
   Vector<Bool> absIns(n), absOuts(n);
   Vector<String> unitsIn(n), unitsOut(n);

// Input; first make a vector at the reference of the correct abs/rel

   if (unitIn==pix) {
      coordIn = cSys.referencePixel();
      unitsIn = pix;
      if (!absIn) cSys.makePixelRelative(coordIn);
   } else {
      coordIn = cSys.referenceValue();
      unitsIn = cSys.worldAxisUnits();
      if (!absIn) cSys.makeWorldRelative(coordIn);
   }
   unitsIn(profileAxis) = unitIn;
   absIns = absIn;

// Output;  Just convert all except profile axis to pixels to make it easy.

   absOuts = absOut;
   unitsOut = pix;
   unitsOut(profileAxis) = unitOut;

// Setup pixel offsets (always 0-rel).

   Double offsetIn = 0.0;
   Double offsetOut = 0.0;
//
// Get center and set values
//
   Double centerValue = el.getCenter();
   coordIn(profileAxis) = centerValue;

// Convert to 0-rel absolute pixels

   if (!cSys.convert (coordOut, coordIn, absIns, unitsIn, dopplerTypeIn,
                           absOuts, unitsOut, dopplerTypeOut, offsetIn, offsetOut)) {   
      os << cSys.errorMessage() << LogIO::EXCEPTION;
   }

// Fish out error then set new center value in element (sets error to 0)

   Double centerValueErr = el.getCenterErr();
   Double newCenterValue = coordOut(profileAxis);
   el.setCenter(newCenterValue);

// Now compute new error

   coordIn(profileAxis) = centerValue + centerValueErr;  
   if (!cSys.convert (coordOut, coordIn, absIns, unitsIn, dopplerTypeIn,
                           absOuts, unitsOut, dopplerTypeOut, offsetIn, offsetOut)) {   
      throw (AipsError(cSys.errorMessage()));
   }
   Double newCenterValueErr = abs(newCenterValue - coordOut(profileAxis));
//
// Convert width
//
   if (unitIn==pix) {
      coordIn = cSys.referencePixel();
      unitsIn = pix;
      cSys.makePixelRelative(coordIn);
   } else {
      coordIn = cSys.referenceValue();
      unitsIn = cSys.worldAxisUnits();
      cSys.makeWorldRelative(coordIn);
   }
   unitsIn(profileAxis) = unitIn;
   absIns = False;
   absOuts = False;
   coordIn(profileAxis) = el.getFWHM();
   if (!cSys.convert (coordOut, coordIn, absIns, unitsIn, dopplerTypeIn,
                           absOuts, unitsOut, dopplerTypeOut, 0.0, 0.0)) {
      os << cSys.errorMessage() << LogIO::EXCEPTION;
   }

// Set new width

   el.setFWHM(abs(coordOut(profileAxis)));

// Compute fractional errors

   el.get(pars);
   errors *= pars;

// Overwrite position error

   errors(1) = newCenterValueErr;

// Set errors

   el.setError(abs(errors));
}


void ImageProfileFit::fit (Bool fillRecord, RecordInterface& rec, 
                           Bool xAbsOut,
                           const String& xUnitOut, 
                           const String& dopplerOut,
                           ImageInterface<Float>* pFit,
                           ImageInterface<Float>* pResid,
                           Int order)

{
   LogIO os(LogOrigin("ImageProfileFit", "fit", WHERE));
   if (!itsFitRegion) {
      os << "You cannot call this function as you are averaging all profiles" << LogIO::EXCEPTION;
   }
//
   IPosition inShape = itsImagePtr->shape();
   if (pFit!=0) {
      AlwaysAssert(inShape.isEqual(pFit->shape()), AipsError);
   }
   if (pResid!=0) {
      AlwaysAssert(inShape.isEqual(pResid->shape()), AipsError);
   }
//
   IPosition inTileShape = itsImagePtr->niceCursorShape();
   TiledLineStepper stepper (itsImagePtr->shape(), inTileShape, itsProfileAxis);
   RO_MaskedLatticeIterator<Float> inIter(*itsImagePtr, stepper);
//
   PtrHolder<LatticeIterator<Float> > fitIter;
   PtrHolder<LatticeIterator<Bool> > fitMaskIter;
   PtrHolder<LatticeIterator<Float> > residIter;
   PtrHolder<LatticeIterator<Bool> > residMaskIter;
//
   LatticeIterator<Float>* pFitIter = 0;
   LatticeIterator<Bool>* pFitMaskIter = 0;
   LatticeIterator<Float>* pResidIter = 0;
   LatticeIterator<Bool>* pResidMaskIter = 0;
   if (pFit) {
      fitIter.set(new LatticeIterator<Float>(*pFit, stepper));
      pFitIter = fitIter.ptr();
      if (pFit->hasPixelMask()) {
         fitMaskIter.set(new LatticeIterator<Bool>(pFit->pixelMask(), stepper));
         pFitMaskIter = fitMaskIter.ptr();
      }
   }
   if (pResid) {
      residIter.set(new LatticeIterator<Float>(*pResid, stepper));	
      pResidIter = residIter.ptr();
      if (pResid->hasPixelMask()) {
         residMaskIter.set(new LatticeIterator<Bool>(pResid->pixelMask(), stepper));
         pResidMaskIter = residMaskIter.ptr();
      }
   }
//
   Int nProfiles = itsImagePtr->shape().product()/inIter.vectorCursor().nelements();
   ProgressMeter clock(0.0, Double(nProfiles), "Profile fitting", "Profiles fitted",
                        "", "", True, max(1,Int(nProfiles/20)));
   Double meterValue = 0.0;
//
   Vector<Float> x(inShape(itsProfileAxis));
   Vector<Float> y(inShape(itsProfileAxis));
   for (uInt i=0; i<x.nelements(); i++) x[i] = i;

// Fish out initial estimate (defines number of components to fit)
// We just use it to get the number of components

   const SpectralList& l = itsSpectralFitPtr->list();
   uInt nEl = max(l.nelements(),uInt(1));

// Make an auto-estimator, which we use for each spectrum
// after the initial one

   SpectralEstimate estimator(nEl);

// Make fitter and poly element if desired

   SpectralFit fitter;
   SpectralElement poly(order);
   SpectralList estimate;
//
   Vector<Bool> inMask;
   Vector<Float> inSigma;
   Bool ok = False;
   uInt nFail = 0;
//
   while (!inIter.atEnd()) {

// Get data and mask (reflects pixelMask and region mask of SubImage)

      const Vector<Float>& data = inIter.vectorCursor();
      inMask = inIter.getMask(True);

// Make estimate

      estimate = estimator.estimate(data);

// Set fitter

      fitter.clear();
      fitter.addFitElement(estimate);
      if (order >= 0) fitter.addFitElement(poly);
//
      if (itsSigmaImagePtr) {
         inSigma = itsSigmaImagePtr->getSlice(inIter.position(), 
                                              inIter.cursorShape(), True);
         try {
            ok = fitter.fit(inSigma, data, x, inMask);
         } catch (AipsError x) {
            ok = False;
         }

      } else {
         try {
            ok = fitter.fit(data, x, inMask);
         } catch (AipsError x) {
            ok = False;
         }
      }

// Evaluate

      SpectralList list(fitter.list());
      if (ok) {
         if (pFit) {
            list.evaluate(pFitIter->rwVectorCursor());   
         }
         if (pFitMaskIter) {
            pFitMaskIter->rwVectorCursor() = inMask;
         }
         if (pResid) {
            pResidIter->rwVectorCursor() = data;
            list.residual(pResidIter->rwVectorCursor());   
         }
         if (pResidMaskIter) {
            pResidMaskIter->rwVectorCursor() = inMask;
         }
      } else {
         nFail++;
         if (pFit) {
            pFitIter->rwVectorCursor() = 0.0;
         }
         if (pFitMaskIter) {
            pFitMaskIter->rwVectorCursor() = False;
         }
         if (pResid) {
            pResidIter->rwVectorCursor() = 0.0;
         }
         if (pResidMaskIter) {
            pResidMaskIter->rwVectorCursor() = False;
         }
      }
//
      if (fillRecord) {
         Record rec4, rec3;
         rec4.define ("pixel", inIter.position().asVector());
         rec3.defineRecord("position", rec4);    
//
         Record rec2;
         getElements (rec2, xAbsOut, xUnitOut, dopplerOut, list);
         rec3.defineRecord("fit", rec2);
//
         rec.defineRecord(inIter.nsteps(), rec3);
      }
//  
      inIter++;
      if (pFitIter) (*pFitIter)++;
      if (pResidIter) (*pResidIter)++;
      if (pFitMaskIter) (*pFitMaskIter)++;
      if (pResidMaskIter) (*pResidMaskIter)++;
//
       meterValue += 1.0;
       clock.update(meterValue);
    }
//
    os << "Number of    profiles = " << nProfiles << LogIO::POST;
    os << "Number of   good fits = " << nProfiles - nFail << LogIO::POST;
    os << "Number of failed fits = " << nFail << LogIO::POST;
}


SpectralList ImageProfileFit::filterList (const SpectralList& listIn) const
{
   SpectralList listOut;
   const uInt n = listIn.nelements();
   if (n==0) return listOut;
//
   for (uInt i=0; i<n; i++) {
      const SpectralElement& el = listIn[i];

// Filter out polynomials

      if (el.getType()!=SpectralElement::POLYNOMIAL) {
         listOut.add(el);
      }
   }
//
   return listOut;
}


} //# NAMESPACE CASA - END

