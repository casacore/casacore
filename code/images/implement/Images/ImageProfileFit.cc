//# ImageProfileFit.cc: Class to fit spectra from images
//# Copyright (C) 1997,1998,1999,2000,2001
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

#include <trial/Images/ImageProfileFit.h>

#include <aips/Arrays/IPosition.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Containers/Record.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/SpectralCoordinate.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <aips/Glish/GlishRecord.h>
#include <aips/Exceptions/Error.h>
#include <trial/Images/ImageInterface.h>
#include <trial/Images/ImageRegion.h>
#include <trial/Images/SubImage.h>
#include <aips/Lattices/TiledLineStepper.h>
#include <aips/Lattices/LatticeIterator.h>
#include <trial/Lattices/LatticeStatistics.h>
#include <trial/Lattices/LCRegion.h>
#include <trial/Lattices/MaskedLattice.h>
#include <aips/Logging/LogIO.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/Math.h>
#include <trial/Tasking/ProgressMeter.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Utilities/Assert.h>
#include <trial/Wnbt/SpectralFit.h>
#include <trial/Wnbt/SpectralEstimate.h>
#include <trial/Wnbt/SpectralElement.h>
#include <trial/Wnbt/SpectralList.h>


#include <aips/ostream.h>


ImageProfileFit::ImageProfileFit()
: itsImagePtr(0),
  itsFitDone(False),
  itsMask(0),
  itsSpectralFitPtr(0),
  itsProfileAxis(-1),
  itsDopplerType(MDoppler::RADIO),
  itsXUnit(""),
  itsXAbs(True),
  itsFitRegion(False)
{
  UnitMap::putUser("pix",UnitVal(1.0), "pixel units");
  itsSpectralFitPtr = new SpectralFit;
}

ImageProfileFit::ImageProfileFit(const ImageProfileFit& other)
: itsImagePtr(0),
  itsFitDone(other.itsFitDone),
  itsMask(other.itsMask.copy()),
  itsCoords(other.itsCoords),
  itsProfileAxis(other.itsProfileAxis),
  itsDopplerType(other.itsDopplerType),
  itsXUnit(other.itsXUnit),
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
      itsFitDone = other.itsFitDone;
      itsMask.resize(other.itsMask.nelements());
//
      itsX = other.itsX;
      itsY = other.itsY;
      itsMask = other.itsMask;
      itsCoords = other.itsCoords;
      itsProfileAxis = other.itsProfileAxis;
//
      itsDopplerType = other.itsDopplerType;
      itsXUnit = other.itsXUnit;
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
   setData(subImage, sl, average);
}

void ImageProfileFit::setData (const ImageInterface<Float>& image,
                               uInt profileAxis, Bool average)
{
   itsCoords = image.coordinates(); 
   itsProfileAxis = profileAxis;
   IPosition start(image.ndim(),0);
   Slicer sl(start, image.shape(), Slicer::endIsLength);
   setData(image, sl, average);
}

void ImageProfileFit::setData (const Quantum<Vector<Float> >& x, 
                               const Quantum<Vector<Float> >& y,
                               Bool isAbs)
{
   AlwaysAssert(x.getValue().nelements()==y.getValue().nelements(),AipsError);
   AlwaysAssert(x.getValue().nelements()>0,AipsError);
//
   itsMask.resize(0);
   itsX = x;
   itsY = y;
   itsXAbs = isAbs;
   itsFitRegion = False;
}

void ImageProfileFit::setData (const Quantum<Vector<Float> >& x, 
                               const Quantum<Vector<Float> >& y,
                               const Vector<Bool>& mask,
                               Bool isAbs)
{
   AlwaysAssert(x.getValue().nelements()==mask.nelements(),AipsError);
   setData(x, y, isAbs);
//
   itsMask.resize(mask.nelements());
   itsMask = mask;
   itsFitRegion = False;
}



uInt ImageProfileFit::addElements (const RecordInterface& rec)
{
   LogIO os(LogOrigin("image", "addElements", WHERE));
   if (!rec.isDefined("xunit")) {
      throw (AipsError("Record holding model is missing 'xunit' field"));
   }
   if (!rec.isDefined("yunit")) {
      throw (AipsError("Record holding model is missing 'yunit' field"));
   }
   if (!rec.isDefined("xabs")) {
      throw (AipsError("Record holding model is missing 'xabs' field"));
   }
//
   Unit yUnitEst(rec.asString("yunit"));
   Unit xUnitEst(rec.asString("xunit"));
   String doppler;
   if (rec.isDefined("doppler")) {
      doppler = rec.asString("doppler");
   }
   Bool xAbs = rec.asBool("xabs");
//
   Unit yUnitOut = itsY.getFullUnit();

// Loop over elements in record

   if (!rec.isDefined("elements")) {
      throw (AipsError("Record holding model is missing 'elements' field"));
   }
   if (rec.dataType("elements") != TpRecord) {
     throw (AipsError("Record holding model is invalid - 'elements' is not a record"));
   }
   Record rec2 = rec.asRecord("elements");
//
   const uInt n = rec2.nfields();
   AlwaysAssert(n>0, AipsError);
//
   String error;
   for (uInt i=0; i<n; i++) {
      Record rec3 =  rec2.asRecord(i);
      SpectralElement se;
      if (!se.fromRecord(error, rec3)) {
         throw(AipsError(error));
      }

// Deal with each SpectralElement type

      if (se.getType()==SpectralElement::GAUSSIAN) {
         Quantum<Double> v(se.getAmpl(), yUnitEst);
         se.setAmpl(v.getValue(yUnitOut));

// Deal with center and width.  If the data source is an
// image we convert the estimate x-values to pixels.
// Otherwise we convert as best we can to the data source
// x-units

         if (itsProfileAxis==-1) {                     // Data source is vectors

// Set abs/rel

            itsXAbs = xAbs;

// Data source is just vectors

           Unit xUnitData = itsX.getFullUnit();
           Unit velUnit(String("m/s"));
//
           if (xUnitEst==velUnit) {            // dimensional test
              if (xUnitData!=velUnit) {
                 throw (AipsError("X-data units must be km/s because estimate units are km/s"));
              }
           }

// Need code here to handle 'pix' units for both the data source
// and estimate units (unless they are both pix)

           if (xUnitData.getName()==String("pix") ||
               xUnitEst.getName()==String("pix")) {
              if (!(xUnitData.getName()==String("pix") && 
                    xUnitEst.getName()==String("pix"))) {
                 throw (AipsError("x-units of 'pix' are not yet handled for data or model"));
              }
           }
//
           if (xUnitEst.getName() != xUnitData.getName()) {
              Quantum<Double> vCen(se.getCenter(), xUnitEst);
              se.setCenter(vCen.getValue(xUnitData));
//
              Quantum<Double> vWidth(se.getFWHM(), xUnitEst);
              se.setFWHM(vWidth.getValue(xUnitData));
           }
         } else {

// Data source is an image.  Convert 1-rel to 0-rel pixels in here.

           convertXEstimateToPixels(se, xAbs, xUnitEst, doppler);
         }

// See if we have the 'fixed' record

         Vector<Bool> fixed;
         if (rec3.isDefined("fixed")) fixed = rec3.asArrayBool("fixed");
//
         if (fixed.nelements()!=0) {
            if (fixed.nelements()==3) {
               if (fixed(0)) se.fixAmpl(True);
               if (fixed(1)) se.fixCenter(True);
               if (fixed(2)) se.fixFWHM(True);
            } else {
               throw (AipsError("'fixed' vector must be of length 3 for a Gaussian"));
            }
         }
      } else if (se.getType()==SpectralElement::POLYNOMIAL) {
         if (itsProfileAxis==-1) {

// Convert coefficients to data source units

         } else {

// Convert coefficients to absolute pixels

         }
      } else {
         throw (AipsError("element type not supported"));
      }
//
      itsSpectralFitPtr->addFitElement(se);
   }
//
   return itsSpectralFitPtr->list().nelements() - 1;
}


Bool ImageProfileFit::getElements (RecordInterface& rec,
                                   const String& xUnit)
{

// If the data source is an image, the x-units are pixels.
// Convert these to preferred world axis units.
// If the data source is vectors, don't convert any units

   CoordinateSystem cSys = itsCoords;                  // Copy to play with
//
   String pString;
   Unit pUnit;
   Vector<Double> pixel, world;
   Unit velUnit("km/s");
//
   if (itsProfileAxis!=-1) {

// Work out what x-units to use.  If the user gave an estimate
// via addElements, use those units.  Else use the preferred units.
// If they are empty use native units.

      if (xUnit.empty()) {
         pUnit = itsXUnit;
      } else {      
         pUnit = Unit(xUnit);
      }
      if (pUnit.getName().empty()) {
         Int worldAxis = cSys.pixelAxisToWorldAxis(itsProfileAxis);
         pString = cSys.preferredWorldAxisUnits()(worldAxis);
         if (pString.empty()) {
            pString = cSys.worldAxisUnits()(worldAxis);
         }
         pUnit = Unit(pString);
      }
   }
//
   Record recVec;
   const SpectralList& list = itsSpectralFitPtr->list();
   const uInt n = list.nelements();
   for (uInt i=0; i<n; i++) {
      SpectralElement se = list[i];
//
      if (se.getType()==SpectralElement::GAUSSIAN) {
         if (itsProfileAxis!=-1) {

// Data source is an image

           convertXEstimateFromPixels(se, itsXAbs, pUnit, itsDopplerType);
         }
      } else {
         throw (AipsError("cannot handle this type of element"));
      }

// Fill record for this element

      Record recEl;
      String error;
      if (!se.toRecord(error, recEl)) {
         throw(AipsError(error));
      }

// Stick it in output vector record

      recVec.defineRecord(i, recEl);
   }

// Put vector into field "elements"

   rec.defineRecord("elements", recVec);

// Add fields 'xunit', 'yunit', 'doppler'

   if (itsProfileAxis!=-1) {
      rec.define("xunit", pUnit.getName());
      if (pUnit==velUnit) {
         String sDoppler = MDoppler::showType(itsDopplerType);
         rec.define("doppler", sDoppler);
      }
   } else {
      rec.define("xunit", itsX.getFullUnit().getName());
   }
   rec.define("xabs", itsXAbs);
   rec.define("yunit", itsY.getFullUnit().getName());
//
   return True;
}



void ImageProfileFit::listElements(LogIO& os, const SpectralList& list) const
{
   const uInt n = list.nelements();
   for (uInt i=0; i<n; i++) {
     SpectralElement se = list[i];
     os.output() << se << endl;
   }
}



void ImageProfileFit::reset () 
{
   delete itsSpectralFitPtr;
   itsSpectralFitPtr = 0;
   itsSpectralFitPtr = new SpectralFit;
}

Bool ImageProfileFit::estimate (uInt nMax)
{
   delete itsSpectralFitPtr;
   itsSpectralFitPtr = 0;
   itsSpectralFitPtr = new SpectralFit;

// The x-units for the estimate are always pixels

   SpectralEstimate est;
   est.setQ(5);
   Vector<Float> der(itsY.getValue().nelements());
   const SpectralList& list = est.estimate(itsY.getValue(), &der);

// If the data source is not an image, convert units
// to data source x-units.
// If the data source is an image, pixels are the correct
// units.  

   uInt nEl = 0;
   if (nMax==0) {
      nEl = list.nelements();
   } else {
      nEl = min(nMax, list.nelements());
   }
   if (nEl==0) {
      return False;
   }
//
   if (itsProfileAxis==-1) { 

// Average increment
      
      const Vector<Float>& x = itsX.getValue();
      const uInt nP = x.nelements();
      Double inc = (x(nP-1) - x(0)) / Double(nP);

// Convert

      Double v;
      for (uInt i=0; i<nEl; i++) {   
         SpectralElement el = list[i];
         v = x(0) + inc*el.getCenter();
         el.setCenter(v);
         v = el.getFWHM();
         el.setFWHM(v*inc);
//
         itsSpectralFitPtr->addFitElement(el);
      }
   } else {
      for (uInt i=0; i<nEl; i++) {
        itsSpectralFitPtr->addFitElement(list[i]);
      }
   }
//
   return True;
}


uInt ImageProfileFit::nElements ()
{
   return itsSpectralFitPtr->list().nelements();
}
    

Bool ImageProfileFit::fit()
{
   itsFitDone = True;
   return itsSpectralFitPtr->fit(itsY.getValue(), 
                                 itsX.getValue(), 
                                 itsMask);
}


void ImageProfileFit::residual(Vector<Float>& resid, 
                               const Vector<Float>& x) const
{
   resid.resize(0);
   resid = itsY.getValue();
   itsSpectralFitPtr->list().residual(resid, x);
}


void ImageProfileFit::residual(Vector<Float>& resid) const
{
   resid.resize(0);
   resid = itsY.getValue();
   itsSpectralFitPtr->list().residual(resid, itsX.getValue());
}


void ImageProfileFit::model(Vector<Float>& model, 
                            const Vector<Float>& x) const
{
   itsSpectralFitPtr->list().evaluate(model, x);
}

void ImageProfileFit::model(Vector<Float>& model) const
{
   itsSpectralFitPtr->list().evaluate(model, itsX.getValue());
}

// Private functions

void ImageProfileFit::collapse (Vector<Float>& profile, Vector<Bool>& mask,
                                uInt profileAxis, const MaskedLattice<Float>& lat) const
{
   AlwaysAssert(profileAxis<lat.ndim(), AipsError);
   LatticeStatistics<Float> stats(lat, False, False);
   IPosition excludeAxes(1, profileAxis);
   IPosition axes = IPosition::otherAxes(lat.ndim(), excludeAxes);
   stats.setAxes(axes.asVector());
//
   Array<Float> tmp;
   Bool dropDegenerateAxes = True;
   stats.getMean(tmp, dropDegenerateAxes);
   if (tmp.nelements()==0) {
      throw(AipsError("There were no good points in the region"));
   }
   Array<Float> nPts;
   stats.getNPts(nPts, dropDegenerateAxes);
//
   uInt n = tmp.shape()(0);
   profile.resize(n);
   mask.resize(n);
//
   profile = tmp.reform(IPosition(1,n));

// Handle mask in rather ugly way...

   Bool deleteN, deleteM;
   const Float* pN = nPts.getStorage(deleteN);
   Bool* pM = mask.getStorage(deleteM);
//
   for (uInt i=0; i<n; i++) {
      pM[i] = True;
      if (pN[i] < 0.5) pM[i] = False;
   }   
//  
   nPts.freeStorage(pN, deleteN);
   mask.putStorage(pM, deleteM);
 }




void ImageProfileFit::setData (const ImageInterface<Float>& image,
                               const Slicer& sl, Bool average)
{
   if (average) {

// Average data over region except along profile axis

      Vector<Float> y;
      collapse(y, itsMask, itsProfileAxis, image);
      itsY = Quantum<Vector<Float> >(y, image.units());
//
      Vector<Float> x(y.nelements());
      indgen(x, Float(sl.start()(itsProfileAxis)));
      itsX = Quantum<Vector<Float> >(x, Unit("pix"));
      itsFitRegion = False;
   } else {

// We are going to fit all profiles in the region. Just set the units

      itsX.setUnit(Unit("pix"));
      itsY.setUnit(image.units());
//
      itsFitRegion = True;
      itsImagePtr = image.cloneII();
   }
}

void ImageProfileFit::convertXEstimateToPixels (SpectralElement& el,
                                                Bool xAbsIn,
                                                const Unit& xUnitEstIn,
                                                const String& dopplerIn)
//
// Convert estimate to absolute 0-rel pixels
//
{
   itsXAbs = True;
   itsXUnit = xUnitEstIn;
   Unit xUnitData = itsX.getFullUnit();
   Unit velUnit(String("m/s"));
   String xUnit = xUnitEstIn.getName();

// Assumes world and pixel axes the same.
// Should really check order as well...

   AlwaysAssert (itsCoords.nPixelAxes() == itsCoords.nPixelAxes(), AipsError);  
   const uInt n = itsCoords.nWorldAxes();
   Vector<Double> coordIn, coordOut;
//
   Vector<Bool> absIn(n, xAbsIn);
   Vector<Bool> absOut(n, True);
//
   Vector<String> unitsIn(n);
   Vector<String> unitsOut(n, String("pix"));

// Get doppler sorted out

   if (xUnitEstIn==velUnit) {
      if (dopplerIn.empty()) {
         throw (AipsError("Record holding estimate does not specify the doppler type"));
      }
      if (!MDoppler::getType(itsDopplerType, dopplerIn)) {
         throw (AipsError("Record holding estimate has an invalid doppler type"));
      }
   }

// Find fractional error

   Vector<Double> errors, pars;
   el.getError(errors);
   el.get(pars);
   errors /= pars;

// Convert position

   Double offset = 0.0;
   if (xUnit==String("pix")) {
      coordIn = itsCoords.referencePixel();
      unitsIn = String("pix");
      if (!xAbsIn) {
         itsCoords.makePixelRelative(coordIn);
      } else {
         offset = -1.0;                             // make 0-rel
      }
   } else {
      coordIn = itsCoords.referenceValue();
      unitsIn = itsCoords.worldAxisUnits();
      if (!xAbsIn) {
         itsCoords.makeWorldRelative(coordIn);
      }
   }
   unitsIn(itsProfileAxis) = xUnit;
   Double centerValue = el.getCenter() + offset;
   Double centerValueErr = el.getCenterErr();
//
   coordIn(itsProfileAxis) = centerValue;
   if (!itsCoords.convert (coordOut, coordIn, absIn, unitsIn, itsDopplerType,
                           absOut, unitsOut, itsDopplerType, 0.0, 0.0)) {   
      throw (AipsError(itsCoords.errorMessage()));
   }
   Double newCenterValue = coordOut(itsProfileAxis);
   el.setCenter(newCenterValue);
//
   coordIn(itsProfileAxis) = centerValue + centerValueErr;        // setCenter sets error to 0
   if (!itsCoords.convert (coordOut, coordIn, absIn, unitsIn, itsDopplerType,
                           absOut, unitsOut, itsDopplerType, 0.0, 0.0)) {   
      throw (AipsError(itsCoords.errorMessage()));
   }
   Double newCenterValueErr = abs(newCenterValue - coordOut(itsProfileAxis));

// Convert width

   if (xUnit==String("pix")) {
      coordIn = itsCoords.referencePixel();
      unitsIn = String("pix");
      itsCoords.makePixelRelative(coordIn);
   } else {
      coordIn = itsCoords.referenceValue();
      unitsIn = itsCoords.worldAxisUnits();
      itsCoords.makeWorldRelative(coordIn);
   }
   absIn = False;
   absOut = False;
   unitsIn(itsProfileAxis) = xUnit;
   coordIn(itsProfileAxis) = el.getFWHM();
   if (!itsCoords.convert (coordOut, coordIn, absIn, unitsIn, itsDopplerType,
                           absOut, unitsOut, itsDopplerType, 0.0, 0.0)) {
      throw (AipsError(itsCoords.errorMessage()));
   }
   el.setFWHM(abs(coordOut(itsProfileAxis)));

// Set fractional errors

   el.get(pars);
   errors *= pars;

// Overwrite position error

   errors(1) = newCenterValueErr;

// Set errors

   el.setError(abs(errors));
}


void ImageProfileFit::convertXEstimateFromPixels (SpectralElement& el,
                                                  Bool xAbsOut,
                                                  const Unit& xUnitOut,
                                                  MDoppler::Types dopplerOut)
//
// Convert estimate from absolute 0-rel pixels
// If the output unit is 'pix' then they are 1-rel
{
   Unit velUnit(String("m/s"));
   String xUnit = xUnitOut.getName();
   String pix("pix");

// Assumes world and pixel axes the same...

   AlwaysAssert (itsCoords.nPixelAxes() == itsCoords.nPixelAxes(), AipsError);  
   const uInt n = itsCoords.nWorldAxes();
   Vector<Double> coordIn, coordOut;
   coordIn = itsCoords.referencePixel();
//
   Vector<Bool> absIn(n, True);
   Vector<Bool> absOut(n, xAbsOut);
//
   Double offset = 0.0;
   Vector<String> unitsIn(n, String("pix"));
   Vector<String> unitsOut(n);
   if (xUnit==pix) {
      unitsOut = pix;
      if (xAbsOut) offset = 1.0;
   } else {
      unitsOut = itsCoords.worldAxisUnits();
      unitsOut(itsProfileAxis) = xUnit;
   }

// Find fractional error

   Vector<Double> errors, pars;
   el.getError(errors);
   el.get(pars);
   errors /= pars;

// Convert position

   Double centerValue = el.getCenter();
   Double centerValueErr = el.getCenterErr();
   coordIn(itsProfileAxis) = centerValue;
   if (!itsCoords.convert (coordOut, coordIn, absIn, unitsIn, dopplerOut,
                           absOut, unitsOut, dopplerOut, 0.0, offset)) {
      throw (AipsError(itsCoords.errorMessage()));
   }
   Double newCenterValue = coordOut(itsProfileAxis);
   el.setCenter(newCenterValue);                      // Sets error to 0
//
   coordIn(itsProfileAxis) = centerValue + centerValueErr;
   if (!itsCoords.convert (coordOut, coordIn, absIn, unitsIn, dopplerOut,
                           absOut, unitsOut, dopplerOut, 0.0, offset)) {   
      throw (AipsError(itsCoords.errorMessage()));
   }
   Double newCenterValueErr = abs(newCenterValue - coordOut(itsProfileAxis));

// Convert width

   absIn = False;
   absOut = False;
   coordIn = itsCoords.referencePixel();
   itsCoords.makePixelRelative(coordIn);
   coordIn(itsProfileAxis) = el.getFWHM();
   if (!itsCoords.convert (coordOut, coordIn, absIn, unitsIn, itsDopplerType,
                           absOut, unitsOut, itsDopplerType, 0.0, 0.0)) {
      throw (AipsError(itsCoords.errorMessage()));
   }
   el.setFWHM(abs(coordOut(itsProfileAxis)));

// Set fractional errors

   el.get(pars);
   errors *= pars;

// Overwrite position error

   errors(1) = newCenterValueErr;

// Set

   el.setError(abs(errors));
}


void ImageProfileFit::fit (ImageInterface<Float>*& pFit,
                           ImageInterface<Float>*& pResid)
{
   LogIO os(LogOrigin("image", "setDataAndFit", WHERE));
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
   RO_LatticeIterator<Float> inIter(*itsImagePtr, stepper);
//
   LatticeIterator<Float>* pFitIter = 0;
   LatticeIterator<Bool>* pFitMaskIter = 0;
   LatticeIterator<Float>* pResidIter = 0;
   LatticeIterator<Bool>* pResidMaskIter = 0;
   if (pFit) {
      pFitIter = new LatticeIterator<Float>(*pFit, stepper);
      if (pFit->hasPixelMask()) {
         pFitMaskIter = new LatticeIterator<Bool>(pFit->pixelMask(), stepper);
      }
   }
   if (pResid) {
      pResidIter = new LatticeIterator<Float>(*pResid, stepper);
      if (pResid->hasPixelMask()) {
         pResidMaskIter = new LatticeIterator<Bool>(pResid->pixelMask(), stepper);
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
   for (uInt i=0; i<x.nelements(); i++) x(i) = i;

// See if we have an estimate set in the class state

   SpectralFit fit;
   Bool est = False;
   const SpectralList& l = itsSpectralFitPtr->list();
   if (l.nelements() > 0) {
      fit.addFitElement(l);
      est = True;
   }
//   
   Vector<Bool> inMask;
   Bool ok = False;
   uInt nFail = 0;
   while (!inIter.atEnd()) {

// Use user give or auto estimate for first profile. Else use last fit
// as starting place

      if (inIter.nsteps()==0) {
         if (!est) {
            SpectralEstimate se;
            fit.addFitElement(se.estimate(inIter.vectorCursor()));
         }
      }

// Get mask (reflects pixelMask and region mask of SubImage)

      inMask = itsImagePtr->getMaskSlice(inIter.position(), inIter.cursorShape(), True);

//
      try {
         ok = fit.fit(inIter.vectorCursor(), x, inMask);
      } catch (AipsError x) {
         ok = False;
      }

// If the fit fails, the state of the fit object is the failed
// fit, not the estimate it started with...

      if (ok) {
         SpectralList list(fit.list());
         if (pFit) {
            list.evaluate(pFitIter->rwVectorCursor());   
         }
         if (pFitMaskIter) {
            pFitMaskIter->rwVectorCursor() = inMask;
         }
         if (pResid) {
            pResidIter->rwVectorCursor() = inIter.vectorCursor();
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
//
    if (pFitIter) delete pFitIter;
    if (pFitMaskIter) delete pFitMaskIter;
    if (pResidIter) delete pResidIter;
    if (pResidMaskIter) delete pResidMaskIter;
}
