//# ImageMoments.cc:  generate moments from an image
//# Copyright (C) 1995,1996,1997,1998
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
//   

#include <aips/aips.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Containers/Block.h>
#include <aips/Containers/Record.h>
#include <aips/Containers/RecordFieldId.h>
#include <aips/Exceptions/Error.h>
#include <aips/Functionals/Gaussian1D.h>
#include <aips/Logging/LogIO.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Convolver.h>
#include <aips/Quanta/Unit.h>
#include <aips/Quanta/QMath.h>
#include <aips/OS/Directory.h>
#include <aips/OS/File.h>
#include <aips/OS/Path.h>
#include <aips/Tables/Table.h>
#include <aips/Utilities/DataType.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/LinearSearch.h>
#include <aips/Arrays/ArrayPosIter.h>

#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Fitting/NonLinearFitLM.h>
#include <trial/Images/ImageMomentsProgress.h>
#include <trial/Images/ImageStatistics.h>
#include <trial/Images/ImageHistograms.h>
#include <trial/Images/ImageUtilities.h>
#include <trial/Images/MomentCalculator.h>
#include <trial/Images/PagedImage.h>
#include <trial/Images/SubImage.h>
#include <trial/Images/MaskedImage.h>
#include <trial/Lattices/ArrayLattice.h>
#include <trial/Lattices/LatticeApply.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/TiledLineStepper.h>
#include <trial/Tasking/PGPlotter.h>

#include <strstream.h>
#include <iomanip.h>

#include <trial/Images/ImageMoments.h>




template <class T> 
ImageMoments<T>::ImageMoments (MaskedImage<T>& image, 
                               LogIO &os,
                               Bool showProgressU)
: os_p(os),
  showProgress_p(showProgressU),
  momentAxisDefault_p(-10),
  peakSNR_p(T(3)),
  stdDeviation_p(T(0.0)),
  yMin_p(T(0.0)),
  yMax_p(T(0.0)),
  out_p(""),
  psfOut_p(""),
  smoothOut_p(""),
  goodParameterStatus_p(True),
  doWindow_p(False),
  doFit_p(False),
  doAuto_p(True),
  doSmooth_p(False),
  noInclude_p(True),
  noExclude_p(True),
  fixedYLimits_p(False)
//
// Constructor. 
//
{

   momentAxis_p = momentAxisDefault_p;
   moments_p.resize(1);
   moments_p(0) = INTEGRATED;
   kernelTypes_p.resize(0);
   kernelWidths_p.resize(0);
   nxy_p.resize(0);
   selectRange_p.resize(0);
   smoothAxes_p.resize(0);

   if (setNewImage(image)) {
      goodParameterStatus_p = True;
   } else {
      goodParameterStatus_p = False;
   }
}

template <class T>
ImageMoments<T>::ImageMoments(const ImageMoments<T> &other)
: os_p(other.os_p),
  showProgress_p(other.showProgress_p),
  momentAxisDefault_p(other.momentAxisDefault_p),
  peakSNR_p(other.peakSNR_p),
  stdDeviation_p(other.stdDeviation_p),
  yMin_p(other.yMin_p),
  yMax_p(other.yMax_p),
  out_p(other.out_p),
  psfOut_p(other.psfOut_p),
  smoothOut_p(other.smoothOut_p),
  goodParameterStatus_p(other.goodParameterStatus_p),
  doWindow_p(other.doWindow_p),
  doFit_p(other.doFit_p),
  doAuto_p(other.doAuto_p),
  doSmooth_p(other.doSmooth_p),
  noInclude_p(other.noInclude_p),
  noExclude_p(other.noExclude_p),
  fixedYLimits_p(other.fixedYLimits_p),
  momentAxis_p(other.momentAxis_p),
  kernelTypes_p(other.kernelTypes_p),
  kernelWidths_p(other.kernelWidths_p),
  nxy_p(other.nxy_p),
  moments_p(other.moments_p),
  selectRange_p(other.selectRange_p),
  smoothAxes_p(other.smoothAxes_p),
  pInImage_p(other.pInImage_p),
  plotter_p(other.plotter_p)
//
// Copy constructor
//
{}

template <class T>
ImageMoments<T>::ImageMoments(ImageMoments<T> &other)
: os_p(other.os_p),
  showProgress_p(other.showProgress_p),
  momentAxisDefault_p(other.momentAxisDefault_p),
  peakSNR_p(other.peakSNR_p),
  stdDeviation_p(other.stdDeviation_p),
  yMin_p(other.yMin_p),
  yMax_p(other.yMax_p),
  out_p(other.out_p),
  psfOut_p(other.psfOut_p),
  smoothOut_p(other.smoothOut_p),
  goodParameterStatus_p(other.goodParameterStatus_p),
  doWindow_p(other.doWindow_p),
  doFit_p(other.doFit_p),
  doAuto_p(other.doAuto_p),
  doSmooth_p(other.doSmooth_p),
  noInclude_p(other.noInclude_p),
  noExclude_p(other.noExclude_p),
  fixedYLimits_p(other.fixedYLimits_p),
  momentAxis_p(other.momentAxis_p),
  kernelTypes_p(other.kernelTypes_p),
  kernelWidths_p(other.kernelWidths_p),
  nxy_p(other.nxy_p),
  moments_p(other.moments_p),
  selectRange_p(other.selectRange_p),
  smoothAxes_p(other.smoothAxes_p),
  pInImage_p(other.pInImage_p),
  plotter_p(other.plotter_p)
//
// Copy constructor
//
{}



template <class T> 
ImageMoments<T>::~ImageMoments ()
//
// Destructor does nothing
//
{}


template <class T>
ImageMoments<T> &ImageMoments<T>::operator=(const ImageMoments<T> &other)
//
// Assignment operator
//
{
   if (this != &other) {
      
// Assign to image pointer
      
      pInImage_p = other.pInImage_p;  
      

// Do the rest
      
      os_p = other.os_p;
      showProgress_p = other.showProgress_p;
      momentAxis_p = other.momentAxis_p;
      momentAxisDefault_p = other.momentAxisDefault_p;
      kernelTypes_p = other.kernelTypes_p;
      kernelWidths_p = other.kernelWidths_p;
      nxy_p = other.nxy_p;
      moments_p = other.moments_p;
      selectRange_p = other.selectRange_p;
      smoothAxes_p = other.smoothAxes_p;
      peakSNR_p = other.peakSNR_p;
      stdDeviation_p = other.stdDeviation_p;
      yMin_p = other.yMin_p;
      yMax_p = other.yMax_p;
      plotter_p = other.plotter_p;
      out_p = other.out_p;
      psfOut_p = other.psfOut_p;
      smoothOut_p = other.smoothOut_p;
      goodParameterStatus_p = other.goodParameterStatus_p;
      doWindow_p = other.doWindow_p;
      doFit_p = other.doFit_p;
      doAuto_p = other.doAuto_p;
      doSmooth_p = other.doSmooth_p;
      noInclude_p = other.noInclude_p;
      noExclude_p = other.noExclude_p;
      fixedYLimits_p = other.fixedYLimits_p;
   }
   return *this;
}


template <class T> 
Bool ImageMoments<T>::setNewImage(MaskedImage<T>& image)

//
// Assign pointer to image
//
{
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }

   T *dummy = 0;
   DataType imageType = whatType(dummy);

   if (imageType !=TpFloat && imageType != TpDouble) {
       os_p << LogIO::SEVERE << "Moments can only be evaluated from images of type : " <<
         TpFloat << " and " << TpDouble << LogIO::POST;
      goodParameterStatus_p = False;
      pInImage_p = 0;
      return False;
   }

// Assign pointer 

   pInImage_p = &image;

   return True;
}


template <class T>
Bool ImageMoments<T>::setMoments(const Vector<Int>& momentsU)
//
// Assign the desired moments
//
{
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }

   moments_p.resize(0);
   moments_p = momentsU;


// Check number of moments

   uInt nMom = moments_p.nelements();
   if (nMom == 0) {
      os_p << LogIO::SEVERE << "No moments requested" << LogIO::POST;
      goodParameterStatus_p = False;
      return False;
   } else if (nMom > NMOMENTS) {
      os_p << LogIO::SEVERE << "Too many moments specified" << LogIO::POST;
      goodParameterStatus_p = False;
      return False;
   }

   for (uInt i=0; i<nMom; i++) {
      if (moments_p(i) < 0 || moments_p(i) > NMOMENTS-1) {
         os_p << LogIO::SEVERE << "Illegal moment requested" << LogIO::POST;
         goodParameterStatus_p = False;
         return False;
      }
   }
   return True;
}


template <class T>
Bool ImageMoments<T>::setMomentAxis(const Int& momentAxisU)
//
// Assign the desired moment axis.  Zero relative.
//
{
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }

   momentAxis_p= momentAxisU;
   if (momentAxis_p == momentAxisDefault_p) {
     momentAxis_p = CoordinateUtil::findSpectralAxis(pInImage_p->coordinates());
     if (momentAxis_p == -1) {
       os_p << LogIO::SEVERE << "There is no spectral axis in this image -- specify the axis" << LogIO::POST;
       goodParameterStatus_p = False;
       return False;
     }
   } else {
      if (momentAxis_p < 0 || momentAxis_p > Int(pInImage_p->ndim()-1)) {
         os_p << LogIO::SEVERE << "Illegal moment axis; out of range" << LogIO::POST;
         goodParameterStatus_p = False;
         return False;
      }
      if (pInImage_p->shape()(momentAxis_p) <= 1) {
         os_p << LogIO::SEVERE << "Illegal moment axis; it has only 1 pixel" << LogIO::POST;
         goodParameterStatus_p = False;
         return False;
      }
   }

   return True;
}



template <class T>
Bool ImageMoments<T>::setWinFitMethod(const Vector<Int>& methodU)
//
// Assign the desired windowing and fitting methods
//
{

   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }

// No extra methods set

   if (methodU.nelements() == 0) return True;


// Check legality

   for (uInt i = 0; i<uInt(methodU.nelements()); i++) {
      if (methodU(i) < 0 || methodU(i) > NMETHODS-1) {
         os_p << LogIO::SEVERE << "Illegal method given" << LogIO::POST;
         goodParameterStatus_p = False;
         return False;
      }
   }


// Assign Boooools

   linearSearch(doWindow_p, methodU, Int(WINDOW), methodU.nelements());
   linearSearch(doFit_p, methodU, Int(FIT), methodU.nelements());
   linearSearch(doAuto_p, methodU, Int(INTERACTIVE), methodU.nelements());
   doAuto_p  = ToBool(!doAuto_p);

   return True;
}


template <class T>
Bool ImageMoments<T>::setSmoothMethod(const Vector<Int>& smoothAxesU,
                                      const Vector<Int>& kernelTypesU,
                                      const Vector<Double>& kernelWidthsU)
//
// Assign the desired smoothing parameters. 
//
{
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }
 

// First check the smoothing axes

   Int i;
   if (smoothAxesU.nelements() > 0) {
      smoothAxes_p = smoothAxesU;
      for (i=0; i<Int(smoothAxes_p.nelements()); i++) {
         if (smoothAxes_p(i) < 0 || smoothAxes_p(i) > Int(pInImage_p->ndim()-1)) {
            os_p << LogIO::SEVERE << "Illegal smoothing axis given" << LogIO::POST;
            goodParameterStatus_p = False;
            return False;
         }
      }
      doSmooth_p = True;
   } else {
      doSmooth_p = False;
      return True;
   }


// Now check the smoothing types

   if (kernelTypesU.nelements() > 0) {
      kernelTypes_p = kernelTypesU;
      for (i=0; i<Int(kernelTypes_p.nelements()); i++) {
         if (kernelTypes_p(i) < 0 || kernelTypes_p(i) > NKERNELS-1) {
            os_p << LogIO::SEVERE << "Illegal smoothing kernel types given" << LogIO::POST;
            goodParameterStatus_p = False;
            return False;
         }
      }
   } else {
      os_p << LogIO::SEVERE << "Smoothing kernel types were not given" << LogIO::POST;
      goodParameterStatus_p = False;
      return False;
   }


// Check user gave us enough smoothing types
 
   if (smoothAxesU.nelements() != kernelTypes_p.nelements()) {
      os_p << LogIO::SEVERE << "Different number of smoothing axes to kernel types" << LogIO::POST;
      goodParameterStatus_p = False;
      return False;
   }


// Now the desired smoothing kernels widths.  Allow for Hanning
// to not be given as it is always 1/4, 1/2, 1/4

   kernelWidths_p.resize(smoothAxes_p.nelements());
   Int nK = kernelWidthsU.nelements();
   for (i=0; i<Int(smoothAxes_p.nelements()); i++) {
      if (kernelTypes_p(i) == HANNING) {

// For Hanning, width is always 3

         kernelWidths_p(i) = 3;
      } else if (kernelTypes_p(i) == BOXCAR) {

// For box must be odd number greater than 1

         if (i > nK-1) {
            os_p << LogIO::SEVERE << "Not enough smoothing widths given" << LogIO::POST;
            goodParameterStatus_p = False;
            return False;
         } else {
            Int intKernelWidth = Int(kernelWidthsU(i)+0.5);
            if (intKernelWidth < 2) {
               os_p << LogIO::SEVERE << "Boxcar kernel width of " << intKernelWidth << 
                       " is too small" << LogIO::POST;
               goodParameterStatus_p = False;
               return False;
            } else {

// Make sure it's an odd integer

               if(makeOdd(intKernelWidth)) {
                  os_p << LogIO::SEVERE << "Increasing boxcar width on axis " << i+1 <<
                          " to " << intKernelWidth << LogIO::POST;
               }
               kernelWidths_p(i) = Double(intKernelWidth);
            }
         }
      } else if (kernelTypes_p(i) == GAUSSIAN) {
         if (i > nK-1) {
            os_p << LogIO::SEVERE << "Not enough smoothing widths given" << LogIO::POST;
            goodParameterStatus_p = False;
            return False;
         } else {
            if (kernelWidthsU(i) < 1.5) {
               os_p << LogIO::SEVERE << "Gaussian kernel width of " << kernelWidthsU(i) << 
                       " is too small" << LogIO::POST;
               goodParameterStatus_p = False;
               return False;
            } else {
               kernelWidths_p(i) = kernelWidthsU(i);
            }
         }
      } else {
         os_p << LogIO::SEVERE << "Internal logic error" << LogIO::POST;
         goodParameterStatus_p = False;
         return False;
      }
   }

   return True;
}


template <class T>  
Bool ImageMoments<T>::setInExCludeRange(const Vector<T>& includeU,
                                        const Vector<T>& excludeU)
//
// Assign the desired exclude range           
//
{
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }

   Vector<T> include = includeU;
   Vector<T> exclude = excludeU;

   ostrstream os;
   if (!setIncludeExclude(selectRange_p, noInclude_p, noExclude_p,
                          include, exclude, os)) {
      os_p << LogIO::SEVERE << "Invalid pixel inclusion/exclusion ranges" << LogIO::POST;
      goodParameterStatus_p = False;
      return False;
   }

   return True; 
}


template <class T> 
Bool ImageMoments<T>::setSnr(const T& peakSNRU,
                             const T& stdDeviationU)
//
// Assign the desired snr.  The default assigned in
// the constructor is 3,0
//
{
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }

   if (peakSNRU <= 0.0) {
      peakSNR_p = T(3.0);
   } else {
      peakSNR_p = peakSNRU;
   }
   if (stdDeviationU <= 0.0) {
      stdDeviation_p = 0.0;
   } else {
      stdDeviation_p = stdDeviationU;
   }

   return True;
} 


template <class T>
Bool ImageMoments<T>::setOutName(const String& outU)
//
// Assign the desired output file name
//
{
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }

   out_p = outU;
   return True;
}
 

template <class T>
Bool ImageMoments<T>::setPsfOutName(const String& psfOutU)
//
// Assign the desired output PSF file name
//
{
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }

   psfOut_p = psfOutU;
   return True;
}



template <class T>
Bool ImageMoments<T>::setSmoothOutName(const String& smoothOutU) 
//
// Assign the desired smoothed image output file name
// 
{ 
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }

   smoothOut_p = smoothOutU;  
   return True;
}


template <class T>
Bool ImageMoments<T>::setPlotting(PGPlotter& plotterU,
                                  const Vector<Int>& nxyU,
                                  const Bool yIndU)
//   
// Assign the desired PGPLOT device name and number
// of subplots
//
{ 
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }

// Is new plotter attached ?
         
   if (!plotterU.isAttached()) {
       os_p << LogIO::SEVERE << "Input plotter is not attached" << LogIO::POST;
      goodParameterStatus_p = False;
      return False;
   }

// Don't reattach to the same plotter.  The assignment will
// close the previous device
   
   if (plotter_p.isAttached()) {
      if (plotter_p.qid() != plotterU.qid()) plotter_p = plotterU;
   } else {
      plotter_p = plotterU;
   }


// Set number subplots

   fixedYLimits_p = ToBool(!yIndU);
   nxy_p.resize(0);
   nxy_p = nxyU;
   if (!ImageUtilities::setNxy(nxy_p, os_p.output())) {
      goodParameterStatus_p = False;
      return False;
   }
   return True;
}
 

template <class T>
void ImageMoments<T>::closePlotting()
{  
   if (plotter_p.isAttached()) plotter_p.detach();
}
 




template <class T>
Vector<Int> ImageMoments<T>::toMethodTypes (const String& methods)
// 
// Helper function to convert a string containing a list of desired smoothed kernel types
// to the correct <src>Vector<Int></src> required for the <src>setSmooth</src> function.
// 
// Inputs:
//   methods     SHould contain some of "win", "fit", "inter"
//
{
   Vector<Int> methodTypes(3);
   if (!methods.empty()) {
      String tMethods = methods;
      tMethods.upcase();

      Int i = 0;
      if (tMethods.contains("WIN")) {
         methodTypes(i) = WINDOW;
         i++;
      }
      if (tMethods.contains("FIT")) {
         methodTypes(i) = FIT;
         i++;
      }
      if (tMethods.contains("INTER")) {
         methodTypes(i) = INTERACTIVE;
         i++;
      }
      methodTypes.resize(i, True);
   } else {
      methodTypes.resize(0);
   }
   return methodTypes;
} 


template <class T>
Vector<Int> ImageMoments<T>::toKernelTypes (const String& kernels)
// 
// Helper function to convert a string containing a list of desired smoothed kernel types
// to the correct <src>Vector<Int></src> required for the
// <src>setSmooth</src> function.
// 
// Inputs:
//   kernels   Should contain some of "box", "gauss", "hann"
//
{
// Convert to an array of strings

   const Vector<String> kernelStrings = ImageUtilities::getStrings(kernels);

// Convert strings to appropriate enumerated value

   Vector<Int> kernelTypes(kernelStrings.nelements());

   for (uInt i=0; i<uInt(kernelStrings.nelements()); i++) {
      String tKernels= kernelStrings(i);
      tKernels.upcase();

      if (tKernels.contains("BOX")) {
         kernelTypes(i) = BOXCAR;
      } else if (tKernels.contains("GAUSS")) {
         kernelTypes(i) = GAUSSIAN;
      } else if (tKernels.contains("HANN")) {
         kernelTypes(i) = HANNING;
      }
   }

// Return result

   return kernelTypes;
} 


template <class T>
Bool ImageMoments<T>::createMoments()
//
// This function does all the work
//
{
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << LogIO::POST 
           << "Internal status of class is bad.  You have ignored errors" << endl;
      os_p << "in setting the arguments." << LogIO::POST;
      return False;
   }
   

// Find spectral axis and its units

   if (momentAxis_p == momentAxisDefault_p) {
     momentAxis_p = CoordinateUtil::findSpectralAxis(pInImage_p->coordinates());
     if (momentAxis_p == -1) {
       os_p << LogIO::SEVERE << endl << "There is no spectral axis in this image -- specify "
	 "the axis" << LogIO::POST;
       return False;
     }
     if (pInImage_p->shape()(momentAxis_p) <= 1) {
        os_p << LogIO::SEVERE << "Illegal moment axis; it has only 1 pixel" << LogIO::POST;
        goodParameterStatus_p = False;
        return False;
     }
   }
   Int worldMomentAxis = pInImage_p->coordinates().pixelAxisToWorldAxis(momentAxis_p);
   String momentAxisUnits = pInImage_p->coordinates().worldAxisUnits()(worldMomentAxis);
//   cout << "momentAxisUnits = " << momentAxisUnits << endl;
   os_p << LogIO::NORMAL << endl << "Moment axis type is "
        << pInImage_p->coordinates().worldAxisNames()(worldMomentAxis) << LogIO::POST;


// Check the user's requests are allowed

   if (!checkMethod()) return False;


// Check that input and output image names aren't the same.
// if there is only one output image

   if (moments_p.nelements() == 1) {
      if (!out_p.empty() && (out_p == pInImage_p->name())) {
         os_p << LogIO::SEVERE << "Input image and output image have same name" << LogIO::POST;
         return False;
      }
   } 


// Try and set some useful Booools.

   Bool smoothClipMethod = False;
   Bool windowMethod = False;
   Bool fitMethod = False;
   Bool clipMethod = False;
   Bool doPlot = plotter_p.isAttached();

   if (doSmooth_p && !doWindow_p) {
      smoothClipMethod = True;      
   } else if (doWindow_p) {
      windowMethod = True;
   } else if (doFit_p) {
      fitMethod = True;
   } else {
      clipMethod = True;
   }     


// Create table to map input to output axes

         
   uInt i, j;
   uInt  inDim = pInImage_p->ndim();
   uInt outDim = pInImage_p->ndim() - 1;
   IPosition ioMap(outDim);
   for (i=0,j=0; i<inDim; i++) {
      if (Int(i) != momentAxis_p) {
         ioMap(j) = i;
         j++;
      }
   }            

// We only smooth the image if we are doing the smooth/clip method
// or possibly the interactive window method.  Note that the convolution
// routines can only handle convolution when the image fits fully in core
// at present.
   
   MaskedImage<T>* pSmoothedImage = 0;
   String smoothName;
   if (doSmooth_p) {
      pSmoothedImage = smoothImage(smoothName);
      if (pSmoothedImage != 0) {
         os_p << LogIO::SEVERE << "Error convolving image" << LogIO::POST;
         return False;
      }


// Find the auto Y plot range.   The smooth & clip and the window
// methods only plot the smoothed data.

      if (doPlot && fixedYLimits_p && (smoothClipMethod || windowMethod)) {
         ImageStatistics<T> stats(*pSmoothedImage, False);
         Array<T> data;
         stats.getMin(data);
         yMin_p = data(IPosition(data.nelements(),0));
         stats.getMax(data);
         yMax_p = data(IPosition(data.nelements(),0));
      }
   }


// Find the auto Y plot range if not smoothing and stretch
// limits for plotting

   if (fixedYLimits_p && doPlot) {
      if (!doSmooth_p && (clipMethod || windowMethod || fitMethod)) {
         ImageStatistics<T> stats(*pInImage_p, False);

         Array<T> data;
         if (!stats.getMin(data)) {
            os_p << LogIO::SEVERE << "Error finding minimum of input image" << LogIO::POST;
            return False;
         }
         yMin_p = data(IPosition(data.nelements(),0));
         stats.getMax(data);
         yMax_p = data(IPosition(data.nelements(),0));
      }
   }


// Set output images shape
   
   IPosition outImageShape(outDim);
   for (j=0; j<outDim; j++) outImageShape(j) = pInImage_p->shape()(ioMap(j));

//   cout << "In  shape = " <<  pInImage_p->shape() << endl;
//   cout << "Out shape = " << outImageShape << endl;

// Account for removal of the collapsed moment axis in the coordinate system.  

   CoordinateSystem outImageCoord = pInImage_p->coordinates();
   Bool ok = outImageCoord.removeWorldAxis(worldMomentAxis,
                 outImageCoord.referenceValue()(worldMomentAxis));
   if (!ok) {
      os_p << String("Failed to remove moment axis because ") 
           << outImageCoord.errorMessage() << LogIO::EXCEPTION;
   }     


// Create a vector of pointers for output images 

   PtrBlock<Lattice<T> *> outPt(moments_p.nelements());
   for (i=0; i<outPt.nelements(); i++) outPt[i] = 0;


// Loop over desired output moments

   String suffix;
   Bool goodUnits;
   Bool giveMessage = True;
   Unit imageUnits = pInImage_p->units();
   
   for (i=0; i<moments_p.nelements(); i++) {

// Set moment image units and assign pointer to output moments array
// Value of goodUnits is the same for each output moment image

      Unit momentUnits;
      goodUnits = setOutThings(suffix, momentUnits, imageUnits, momentAxisUnits, moments_p(i));
   
// Create output image(s)

      PagedImage<T>* imgp;
      const String in = pInImage_p->name(False);   
      if (moments_p.nelements() == 1) {
         if (out_p.empty()) out_p = in+suffix;
         imgp = new PagedImage<T>(outImageShape, outImageCoord, out_p);
         os_p << LogIO::NORMAL << "Created " << out_p << LogIO::POST;
         imgp->setMiscInfo(pInImage_p->miscInfo());
      } else {
         if (out_p.empty()) out_p = in;
         imgp = new PagedImage<T>(outImageShape, outImageCoord,
				  out_p+suffix);
         os_p << LogIO::NORMAL << "Created " << out_p+suffix << LogIO::POST;
         imgp->setMiscInfo(pInImage_p->miscInfo());
      }
      outPt[i] = imgp;

// Set output image units if possible

      if (goodUnits) {
         imgp->setUnits(momentUnits);
      } else {
        if (giveMessage) {
           os_p << LogIO::NORMAL 
                << "Could not determine the units of the moment image(s) so the units " << endl;
           os_p << "will be the same as those of the input image. This may not be very useful." << LogIO::POST;
           giveMessage = False;
        }
      }
   } 



// If the user is using the automatic, non-fitting window method, they need
// a good assement of the noise.  The user can input that value, but if
// they don't, we work it out here.

   T noise;
   if ( stdDeviation_p <= T(0) && ( (doWindow_p && doAuto_p) || (doFit_p && !doWindow_p && doAuto_p) ) ) {
      if (pSmoothedImage) {
         os_p << LogIO::NORMAL << "Evaluating noise level from smoothed image" << LogIO::POST;
         if (!whatIsTheNoise (noise, *pSmoothedImage)) return False;
      } else {
         os_p << LogIO::NORMAL << "Evaluating noise level from input image" << LogIO::POST;
         if (!whatIsTheNoise (noise, *pInImage_p)) return False;
      }
      stdDeviation_p = noise;
   }


// Set up some plotting things
         
   if (doPlot) {
      plotter_p.subp(nxy_p(0), nxy_p(1));
      plotter_p.ask(True);
      plotter_p.sch(1.5);
      plotter_p.vstd();
   }        
   

// Create appropriate MomentCalculator object 

   os_p << LogIO::NORMAL << "Begin computation of moments" << LogIO::POST;

   MomentCalcBase<T>* pMomentCalculator = 0;   
   if (clipMethod || smoothClipMethod) {
      pMomentCalculator = new MomentClip<T>(pSmoothedImage, *this, os_p, outPt.nelements());
   } else if (windowMethod) {
      pMomentCalculator = new MomentWindow<T>(pSmoothedImage, *this, os_p, outPt.nelements());
   } else if (fitMethod) {
      pMomentCalculator = new MomentFit<T>(*this, os_p, outPt.nelements());
   }

// Iterate optimally through the image, compute the moments, fill the output lattices

   ImageMomentsProgress* pProgressMeter = 0;
   if (showProgress_p) pProgressMeter = new ImageMomentsProgress();
   LatticeApply<T>::lineMultiApply(outPt, *pInImage_p, *pMomentCalculator, 
                                   momentAxis_p, pProgressMeter);

// Clean up
         
   if (windowMethod || fitMethod) {
      if (pMomentCalculator->nFailedFits() != 0) {
         os_p << LogIO::NORMAL << "There were " <<  pMomentCalculator->nFailedFits() << " failed fits" << LogIO::POST;
      }
   }
   delete pMomentCalculator;
   if (pProgressMeter != 0) delete pProgressMeter;
   for (i=0; i<moments_p.nelements(); i++) delete outPt[i];

   if (pSmoothedImage) {
      delete pSmoothedImage;
       
// Remove the smoothed image file if they don't want to save it
 
      if (smoothOut_p.empty()) {
         Directory dir(smoothName);
         dir.removeRecursive();
      }
   }
   

// Success guarenteed !

   return True;

}





// Private member functions

template <class T> 
Bool ImageMoments<T>::checkMethod ()
// 
// Make sure we can do what the user wants
//  
{


// Make a plotting check. They must give the plotting device for interactive methods.  
// Plotting can be invoked passively for other methods.

   if ( ((doWindow_p && !doAuto_p) ||
         (!doWindow_p && doFit_p && !doAuto_p)) && !plotter_p.isAttached()) {
      os_p << LogIO::SEVERE << "You have not given a plotting device" << LogIO::POST;
      return False;
   } 


// Only can have the median coordinate under certain conditions
   
   Bool found;
   if(linearSearch(found, moments_p, Int(MEDIAN_COORDINATE), moments_p.nelements()) != -1) {
      Bool noGood = False;
      if (doWindow_p || doFit_p || doSmooth_p) {
         noGood = True;
      } else {
         if (noInclude_p && noExclude_p) {
            noGood = True;
         } else {
           if (selectRange_p(0)*selectRange_p(1) < T(0)) noGood = True;
         }
      }
      if (noGood) {
         os_p << LogIO::SEVERE;
         os_p << "You have asked for the median coordinate moment, but it is only" << endl;
         os_p << "available with the basic (no smooth, no window, no fit) method " << endl;
         os_p << "and a pixel range that is either all positive or all negative" << LogIO::POST;
         return False;
      }
   }


// Now check all the silly methods

   const Bool doInter = ToBool(!doAuto_p);

   if (!( (!doSmooth_p && !doWindow_p && !doFit_p && ( noInclude_p &&  noExclude_p) && !doInter) ||
          ( doSmooth_p && !doWindow_p && !doFit_p && (!noInclude_p || !noExclude_p) && !doInter) ||
          (!doSmooth_p && !doWindow_p && !doFit_p && (!noInclude_p || !noExclude_p) && !doInter) ||

          ( doSmooth_p &&  doWindow_p && !doFit_p && ( noInclude_p &&  noExclude_p) &&  doInter) ||
          (!doSmooth_p &&  doWindow_p && !doFit_p && ( noInclude_p &&  noExclude_p) &&  doInter) ||
          ( doSmooth_p &&  doWindow_p && !doFit_p && ( noInclude_p &&  noExclude_p) && !doInter) ||
          (!doSmooth_p &&  doWindow_p && !doFit_p && ( noInclude_p &&  noExclude_p) && !doInter) ||
          (!doSmooth_p &&  doWindow_p &&  doFit_p && ( noInclude_p &&  noExclude_p) &&  doInter) ||
          (!doSmooth_p &&  doWindow_p &&  doFit_p && ( noInclude_p &&  noExclude_p) && !doInter) ||
          ( doSmooth_p &&  doWindow_p &&  doFit_p && ( noInclude_p &&  noExclude_p) &&  doInter) ||
          ( doSmooth_p &&  doWindow_p &&  doFit_p && ( noInclude_p &&  noExclude_p) && !doInter) ||
   
          (!doSmooth_p && !doWindow_p &&  doFit_p && ( noInclude_p &&  noExclude_p) &&  doInter) ||
          (!doSmooth_p && !doWindow_p &&  doFit_p && ( noInclude_p &&  noExclude_p) && !doInter) )) {

      os_p << LogIO::NORMAL << "You have asked for an invalid combination of methods" << LogIO::POST;
      os_p << LogIO::NORMAL << "Valid combinations are: " << LogIO::POST << LogIO::POST;



      os_p <<  "Smooth    Window      Fit   in/exclude   Interactive " << endl;
      os_p <<  "-----------------------------------------------------" << endl;
   
// Basic method. Just use all the data
   
      os_p <<  "  N          N         N        N            N       " << endl;
                       
// Smooth and clip, or just clip
                  
      os_p <<  "  Y/N        N         N        Y            N       " << endl << endl;
                  
// Direct interactive window selection with or without smoothing
                  
      os_p <<  "  Y/N        Y         N        N            Y       " << endl;

// Automatic windowing via Bosma's algorithm with or without smoothing
 
      os_p <<  "  Y/N        Y         N        N            N       " << endl;

// Windowing by fitting Gaussians (selecting +/- 3-sigma) automatically or interactively 
// with or without out smoothing
          
      os_p <<  "  Y/N        Y         Y        N            Y/N     " << endl;
          
// Interactive and automatic Fitting of Gaussians and the moments worked out
// directly from the fits
          
      os_p <<  "  N          N         Y        N            Y/N     " << endl << endl;


      os_p <<  "You have asked for" << endl << endl;
      if (doSmooth_p) 
         os_p <<  "  Y";
      else
         os_p <<  "  N";

      if (doWindow_p) 
         os_p <<  "          Y";
      else
         os_p <<  "          N";
      if (doFit_p) 
         os_p <<  "         Y";
      else
         os_p <<  "         N";
      if (noInclude_p && noExclude_p)
         os_p <<  "        N";
      else
         os_p <<  "        Y";
      if (doAuto_p)
         os_p <<  "            Y";
      else
         os_p <<  "            N";
      os_p <<  endl;
      os_p <<  "-----------------------------------------------------" << endl << LogIO::POST;
      return False;
   }


// Tell them what they are getting
          
   os_p << endl << endl
        << "***********************************************************************" << endl;
   os_p << LogIO::NORMAL << "You have selected the following methods" << endl;
   if (doWindow_p) {
      os_p << "The window method" << endl;
      if (doFit_p) {
         if (doInter)
            os_p << "   with window selection via interactive Gaussian fitting" << endl;
         else
            os_p << "   with window selection via automatic Gaussian fitting" << endl;
      } else {
         if (doInter)
            os_p << "   with interactive direct window selection" << endl;
         else     
            os_p << "   with automatic window selection via the Bosma algorithm" << endl;
      }           
      if (doSmooth_p) {
         os_p << "   operating on the smoothed image.  The moments are still" << endl;
         os_p << "   evaluated from the unsmoothed image" << endl;
      } else
         os_p << "   operating on the unsmoothed image" << endl;
   } else if (doFit_p) {
      if (doInter)
         os_p << "The interactive Gaussian fitting method" << endl;
      else
         os_p << "The automatic Gaussian fitting method" << endl;
          
      os_p << "   operating on the unsmoothed data" << endl;
      os_p << "   The moments are evaluated from the fits" << endl;
   } else if (doSmooth_p) {
      os_p << "The smooth and clip method.  The moments are evaluated from" << endl;
      os_p << "   the masked unsmoothed image" << endl;
   } else {
      if (noInclude_p && noExclude_p)
         os_p << "The basic show it as it is method !" << endl;
      else
         os_p << "The clip method" << endl;
   }
   os_p << endl << endl << LogIO::POST;
   
      
   return True;   
}


template <class T> 
void ImageMoments<T>::drawHistogram (const Vector<T>& x,
                                     const Vector<T>& y,
                                     PGPlotter& plotter)
{
   plotter.box ("BCNST", 0.0, 0, "BCNST", 0.0, 0);
   plotter.lab ("Intensity", "Number", "");

   const Float width = convertT(x(1) - x(0)) / 2.0;
   Float xx, yy;

   for (uInt i=0; i<x.nelements(); i++) {
      xx = convertT(x(i)) - width;
      yy = convertT(y(i));
   
      plotter.move (xx, 0.0);
      plotter.draw (xx, yy);
         
      plotter.move (xx, yy);
      xx = x(i) + width;
      plotter.draw (xx, yy);
   
      plotter.move (xx, yy);
      plotter.draw (xx, 0.0);
    }
}
 


template <class T> 
void ImageMoments<T>::drawVertical (const T& loc,
                                    const T& yMin,
                                    const T& yMax,
                                    PGPlotter& plotter) 
{
// If the colour index is zero, we are trying to rub something
// out, so don't monkey with the ci then

   Int ci;
   ci = plotter.qci();
   if (ci!=0) plotter.sci (3);

   plotter.move (convertT(loc), convertT(yMin));
   plotter.draw (convertT(loc), convertT(yMax));
   plotter.updt();
   plotter.sci (ci);
}


template <class T> 
void ImageMoments<T>::drawLine (const Vector<T>& x,
                                const Vector<T>& y,
                                PGPlotter& plotter)
//
// Draw  a spectrum on the current panel
// with the box already drawn
//
{
// Copy from templated floating type to float

   const uInt n = x.nelements();
   Vector<Float> xData(n);
   Vector<Float> yData(n);
   for (uInt i=0; i<n; i++) {
      xData(i) = convertT(x(i));
      yData(i) = convertT(y(i));
   }
   plotter.line (xData, yData);
   plotter.updt ();
}



template <class T> 
Bool ImageMoments<T>::getLoc (T& x,
                              T& y,
                              PGPlotter& plotter)
//
// Read the PGPLOT cursor and return its coordinates if not 
// off the plot and any button other than last pushed
//
{
// Fish out window

   Vector<Float> minMax(4);
   minMax = plotter.qwin();

// Position and read cursor

   Float xx = convertT(x);
   Float yy = convertT(y);
   String str;

   readCursor(plotter, xx, yy, str);
   if (xx >= minMax(0) && xx <= minMax(1) && 
       yy >= minMax(2) && yy <= minMax(3)) {
      x = xx;
      y = yy;
   } else {
      plotter.message("Cursor out of range");
      return False;
   }
   return True;
}



template <class T> 
Bool ImageMoments<T>::makeOdd (Int& i)
{
   const Int j = i / 2;
   if (2*j == i) {
      i++;
      return True;
   }
   return False;
}


template <class T> 
void ImageMoments<T>::makePSF (Array<T>& psf,
                               Matrix<T>& psfSep)
//
// Generate an array containing the convolving function
//
// Output:
//   psf             PSF 
//   psfSep          Separable PSF
//
{

   uInt i, j, k;

// Find the largest axis number the user wants to smooth

   const uInt psfDim = max(smoothAxes_p.ac()) + 1;


// Work out the shape of the PSF.

   IPosition psfShape(psfDim);
   Bool found;
   for (i=0,k=0; i<psfDim; i++) {
      if (linearSearch(found, smoothAxes_p, Int(i), smoothAxes_p.nelements())==-1) {
         psfShape(i) = 1;
      } else {
         if (kernelTypes_p(k) == GAUSSIAN) { 
            const Double sigma = kernelWidths_p(k) / sqrt(Double(8.0) * C::ln2);
            psfShape(i) = (Int(5*sigma + 0.5) + 1) * 2;
         } else if (kernelTypes_p(k) == BOXCAR) {
            const uInt intKernelWidth = uInt(kernelWidths_p(k)+0.5);
            psfShape(i) = intKernelWidth + 1;
         } else if (kernelTypes_p(k) == HANNING) {
            psfShape(i) = 4;
         }
         k++;
      }
   }


// Resize separable PSF matrix

   const uInt nAxes = psfDim;
   const uInt nPts = max(psfShape.asVector().ac());
   psfSep.resize(nPts,nAxes);


// Now fill the separable PSF

   for (i=0,k=0; i<psfDim; i++) {

      if(linearSearch(found, smoothAxes_p, Int(i), smoothAxes_p.nelements())==-1) {


// If this axis is not in the user's list, make the shape
// of the PSF array 1

         psfShape(i) = 1;
         psfSep(0,i) = 1.0;
      } else {
         if (kernelTypes_p(k) == GAUSSIAN) { 

// Gaussian. The volume error is less than 6e-5% for +/- 5 sigma limits

            const Double sigma = kernelWidths_p(k) / sqrt(Double(8.0) * C::ln2);
            const Int refPix = psfShape(i)/2;

            const Double norm = 1.0 / (sigma * sqrt(2.0 * C::pi));
            const Double gWidth = kernelWidths_p(k);
            const Gaussian1D<Double> gauss(norm, Double(refPix), gWidth);
//            os_p << LogIO::NORMAL << "Volume = " << 1/norm << LogIO::POST;

            for (j=0; j<uInt(psfShape(i)); j++) psfSep(j,i) = gauss(Double(j));
         } else if (kernelTypes_p(k) == BOXCAR) {
            const Int intKernelWidth = Int(kernelWidths_p(k)+0.5);
            const Int refPix = psfShape(i)/2;

            const Int iw = (intKernelWidth-1) / 2;
            for (j=0; j<uInt(psfShape(i)); j++) {
               if (abs(Int(j)-refPix) > iw) {
                  psfSep(j,i) = 0.0;
               } else {
                  psfSep(j,i) = 1.0 / Float(intKernelWidth);
               }
            }
         } else if (kernelTypes_p(k) == HANNING) {
            psfSep(0,i) = 0.25;
            psfSep(1,i) = 0.5;
            psfSep(2,i) = 0.25;
            psfSep(3,i) = 0.0;
         }
         k++;
      }
   }

//   os_p << LogIO::NORMAL << "PSF shape = " << psfShape << LogIO::POST;


// Resize non-separable PSF array

   psf.resize(psfShape);


// Set up position iterator

   ArrayPositionIterator posIterator (psf.shape(), IPosition(psfDim,0), 0);


// Iterate through PSF array and fill it with product of separable PSF

   Float val;
   Float sum = 0.0;
   uInt index;
   for (posIterator.origin(); !posIterator.pastEnd(); posIterator.next()) {
      for (i=0,val=1.0; i<psfDim; i++) {
         index = posIterator.pos()(i);
         val *= psfSep(index,i);
      }
      psf(posIterator.pos()) = val;
      sum = sum + val;
   } 

//   os_p << LogIO::NORMAL << "Sum of PSF = " << sum << LogIO::POST;

}



template <class T> 
Bool ImageMoments<T>::setOutThings(String& suffix, 
                                   Unit& momentUnits,
                                   const Unit& imageUnits,
                                   const String& momentAxisUnits,
                                   const Int moment)
//
// Set the output image suffixes and units
//
// Input:
//   momentAxisUnits
//                The units of the moment axis
//   moment       The current selected moment
//   imageUnits   The brightness units of the input image.
// Outputs:
//   momentUnits  The brightness units of the moment
//                image. Depends upon moment type
//   suffix       suffix for output file name
//   Bool         True if could set units for moment image, false otherwise
{
   String temp;

   Bool goodUnits = True;
   Bool goodImageUnits = ToBool(!imageUnits.getName().empty());
   Bool goodAxisUnits = ToBool(!momentAxisUnits.empty());

   if (moment == AVERAGE) {
      suffix = ".MAverage";
      temp = imageUnits.getName();
      goodUnits = goodImageUnits;
   } else if (moment == INTEGRATED) {
      suffix = ".MIntegrated";
      temp = imageUnits.getName() + "." + momentAxisUnits;
      goodUnits = ToBool(goodImageUnits && goodAxisUnits);
   } else if (moment == WEIGHTED_MEAN_COORDINATE) {
      suffix = ".MWeighted_Mean_Coord";
      temp = momentAxisUnits;
      goodUnits = goodAxisUnits;
   } else if (moment == WEIGHTED_DISPERSION_COORDINATE) {
      suffix = ".MWeighted_Dispersion_Coord";
      temp = momentAxisUnits + "." + momentAxisUnits;
      goodUnits = goodAxisUnits;
   } else if (moment == MEDIAN) {
      suffix = ".MMedian";
      temp = imageUnits.getName();
      goodUnits = goodImageUnits;
   } else if (moment == STANDARD_DEVIATION) {
      suffix = ".MStandard_Deviation";
      temp = imageUnits.getName();
      goodUnits = goodImageUnits;
   } else if (moment == RMS) {
      suffix = ".MRms";
      temp = imageUnits.getName();
      goodUnits = goodImageUnits;
   } else if (moment == ABS_MEAN_DEVIATION) {
      suffix = ".MAbs_Mean_Dev";
      temp = imageUnits.getName();
      goodUnits = goodImageUnits;
   } else if (moment == MAXIMUM) {
      suffix = ".MMaximum";
      temp = imageUnits.getName();
      goodUnits = goodImageUnits;
   } else if (moment == MAXIMUM_COORDINATE) {
      suffix = ".MMaximum_Coord";
      temp = momentAxisUnits;
      goodUnits = goodAxisUnits;
   } else if (moment == MINIMUM) {
      suffix = ".MMinimum";
      temp = imageUnits.getName();
      goodUnits = goodImageUnits;
   } else if (moment == MINIMUM_COORDINATE) {
      suffix = ".MMinimum_Coord";
      temp = momentAxisUnits;
      goodUnits = goodAxisUnits;
   } else if (moment == MEDIAN_COORDINATE) {
      suffix = ".MMedian_Coord";
      temp = momentAxisUnits;
      goodUnits = goodAxisUnits;
   }
   if (goodUnits) momentUnits.setName(temp);
   return goodUnits;
}

template <class T> 

MaskedImage<T>* ImageMoments<T>::smoothImage (String& smoothName)
//
// Smooth image.  
//
// Output
//   pSmoothedImage Pointer to smoothed Lattice
//   smoothName     Name of smoothed image file
//   Bool           True for success
{

// Check axes

   Int axMax = max(smoothAxes_p.ac()) + 1;
   if (axMax > Int(pInImage_p->ndim())) {
      os_p << LogIO::SEVERE << "You have specified a smoothing axis larger" << endl;
      os_p <<                  "than the number of axes in the image" << LogIO::POST;
      return 0;
   }
      


// Generate convolving function

   Array<T> psf;
   Matrix<T> psfSep;
   makePSF(psf, psfSep);

// Save PSF to disk. It won't be very big generally, so use an ArrayLattice
   
   if (!psfOut_p.empty()) {
      os_p << LogIO::NORMAL << "Saving PSF file" << LogIO::POST;

// Create ArrayLattice

      ArrayLattice<T>* pPSF = new ArrayLattice<T>(psf);

// Fiddle CoordinateSystem
 
      CoordinateSystem psfCSys = pInImage_p->coordinates();
      Int coordinate, axisInCoordinate, worldAxis, pixelAxis;
      Vector<Double> refPix(smoothAxes_p.nelements());
      Bool found;
      Int i;
      for (i=0,pixelAxis=0; pixelAxis<Int(psfCSys.nPixelAxes()); pixelAxis++) {
         if(linearSearch(found, smoothAxes_p, pixelAxis, smoothAxes_p.nelements())==-1) {
            psfCSys.findPixelAxis(coordinate, axisInCoordinate, pixelAxis);
            worldAxis = psfCSys.worldAxes(coordinate)(axisInCoordinate);

            psfCSys.removePixelAxis(pixelAxis, 0.0);
            psfCSys.removeWorldAxis(worldAxis, 0.0);
         } else {
            refPix(i) = psf.shape()(i)/2.0;
            i++;
         }
      }
      Vector<Double> refValue(psfCSys.nWorldAxes());
      refValue = 0.0;
      psfCSys.setReferenceValue(refValue);

// Save image to disk
    
      PagedImage<T> psfOut(pPSF->shape(), psfCSys, psfOut_p);
      psfOut.copyData(*pPSF);
      delete pPSF;
   }


// Create smoothed image as a PagedImage.  We delete it later
// if the user doesn't want to save it

   if (smoothOut_p.empty()) {
      File inputImageName(pInImage_p->name());
      const String path = inputImageName.path().dirName() + "/";
      Path fileName = File::newUniqueName(path, String("ImageMoments_Smooth_"));
      smoothName = fileName.absoluteName();
   } else {
      smoothName = smoothOut_p;
   }

   PagedImage<T> smoothedImage(pInImage_p->shape(), 
                               pInImage_p->coordinates(), smoothName);
   smoothedImage.setMiscInfo(pInImage_p->miscInfo());
   if (!smoothOut_p.empty()) {
      os_p << LogIO::NORMAL << "Created " << smoothName << LogIO::POST;
   }

// First copy input to output 

   smoothedImage.copyData(*pInImage_p);

// Create SubImage which can handle masks

   SubImage<T> smoothedSubImage(smoothedImage, True);


// Smooth in situ.  PSF is separable so convolve by rows for each axis.  

   for (uInt i=0; i<psf.ndim(); i++) {
      if (psf.shape()(i) > 1) {
         os_p << LogIO::NORMAL << "Convolving axis " << i+1 << LogIO::POST;
         Vector<T> psfRow = psfSep.column(i);
         psfRow.resize(psf.shape()(i),True);
         smoothProfiles (smoothedSubImage, i, psfRow);
      }
   }

// Reurn pointer to masked smoothed image.  This is ok because 
// underneath the tables are reference counted.  Although 
// "smoothedSubImage" is destructed, its storage isn't.

   return smoothedSubImage.cloneMI();
}


template <class T>
Bool ImageMoments<T>::setIncludeExclude (Vector<T>& range,
                                         Bool& noInclude,
                                         Bool& noExclude,
                                         const Vector<T>& include,
                                         const Vector<T>& exclude,
                                         ostream& os)
//
// Take the user's data inclusion and exclusion data ranges and
// generate the range and Booleans to say what sort it is
//
// Inputs: 
//   include   Include range given by user. Zero length indicates
//             no include range
//   exclude   Exclude range given by user. As above.
//   os        Output stream for reporting
// Outputs:
//   noInclude If True user did not give an include range
//   noExclude If True user did not give an exclude range
//   range     A pixel value selection range.  Will be resized to
//             zero length if both noInclude and noExclude are True
//   Bool      True if successfull, will fail if user tries to give too
//             many values for includeB or excludeB, or tries to give
//             values for both
{  
   noInclude = True;
   range.resize(0);
   if (include.nelements() == 0) {
     ;   
   } else if (include.nelements() == 1) {
      range.resize(2);
      range(0) = -abs(include(0));
      range(1) =  abs(include(0));
      noInclude = False;
   } else if (include.nelements() == 2) {
      range.resize(2);   
      range(0) = min(include(0),include(1));
      range(1) = max(include(0),include(1));
      noInclude = False;
   } else {
      os << endl << "Too many elements for argument include" << endl;
      return False;
   }
 
   noExclude = True;
   if (exclude.nelements() == 0) {
      ;
   } else if (exclude.nelements() == 1) {
      range.resize(2);                      
      range(0) = -abs(exclude(0));
      range(1) =  abs(exclude(0));
      noExclude = False;
   } else if (exclude.nelements() == 2) {
      range.resize(2);
      range(0) = min(exclude(0),exclude(1));
      range(1) = max(exclude(0),exclude(1));
      noExclude = False;
   } else {
      os << endl << "Too many elements for argument exclude" << endl;
      return False;
   }
   if (!noInclude && !noExclude) {
      os << "You can only give one of arguments include or exclude" << endl;
      return False;
   }
   return True;   
}


template <class T> 
void ImageMoments<T>::smoothProfiles (MaskedLattice<T>& in,
                                      const Int& axis,
                                      const Vector<T>& psf)
//
// Smooth all the profiles extracted along
// one axis of the input image
//
{
  TiledLineStepper navIn(in.shape(),
			 in.niceCursorShape(in.maxPixels()),
			 axis);
  LatticeIterator<T> inIt(in, navIn);
  Vector<T> result(in.shape()(axis));

  IPosition sh(1, in.shape()(axis));  
  Convolver<T> conv(psf, sh);

  uInt i = 0;
  while (!inIt.atEnd()) {
    conv.linearConv(result, inIt.vectorCursor());
    inIt.woVectorCursor() = result;
    inIt++;
    i++;
  }
}



template <class T> 
Bool ImageMoments<T>::whatIsTheNoise (T& sigma,
                                      MaskedImage<T>& image)
//
// Determine the noise level in the image by first making a histogram of 
// the image, then fitting a Gaussian between the 25% levels to give sigma
//
{

// Find a histogram of the image

   ImageHistograms<T> histo(image, False);
   const uInt nBins = 100;
   histo.setNBins(nBins);

// It is safe to use Vector rather than Array because 
// we are binning the whole image and ImageHistograms will only resize
// these Vectors to a 1-D shape

   Vector<T> values, counts;
   if (!histo.getHistograms(values, counts)) {
      os_p << LogIO::SEVERE << "Unable to make histogram of image" << LogIO::POST;
      return False;
   }


// Enter into a plot/fit loop

   T binWidth = values(1) - values(0);
   T xMin, xMax, yMin, yMax;
   xMin = values(0) - binWidth;
   xMax = values(nBins-1) + binWidth;
   Float xMinF = convertT(xMin);
   Float xMaxF = convertT(xMax);
   ImageUtilities::stretchMinMax(xMinF, xMaxF);

   IPosition yMinPos(1), yMaxPos(1);
   minMax (yMin, yMax, yMinPos, yMaxPos, counts.ac());
   Float yMinF = 0.0;
   Float yMaxF = convertT(yMax);
   yMaxF += yMaxF/20;

   if (plotter_p.isAttached()) {
      plotter_p.subp(1,1);
      plotter_p.swin (xMinF, xMaxF, yMinF, yMaxF);
   }


   Bool first = True;
   Bool more = True;
   T x1, x2;
   while (more) {

// Plot histogram

      if (plotter_p.isAttached()) {
         plotter_p.page();
         drawHistogram (values, counts, plotter_p);
      }

      Int iMin = 0;
      Int iMax = 0;
      if (first) {
         first = False;
         iMax = yMaxPos(0);
	 uInt i;
         for (i=yMaxPos(0); i<nBins; i++) {
            if (counts(i) < yMax/4) {
               iMax = i; 
               break;
             }      
          } 
          iMin = yMinPos(0);
          for (i=yMaxPos(0); i>0; i--) { 
             if (counts(i) < yMax/4) {
                iMin = i; 
                break;
              }      
          }

// Check range is sensible

         if (iMax <= iMin || abs(iMax-iMin) < 3) {
           os_p << LogIO::NORMAL << "The image histogram is strangely shaped, fitting to all bins" << LogIO::POST;
           iMin = 0;
           iMax = nBins-1;
         }


// Draw on plot

         if (plotter_p.isAttached()) {
            x1 = values(iMin);
            x2 = values(iMax);
            drawVertical (x1, yMin, yMax, plotter_p);
            drawVertical (x2, yMin, yMax, plotter_p);
         }

      } else if (plotter_p.isAttached()) {

// We are redoing the fit so let the user mark where they think
// the window fit should be done

         x1 = (xMin+xMax)/2;
         T y1 = (yMin+yMax)/2;
         Int i1, i2;
         i1 = i2 = 0;
  
         plotter_p.message("Mark the locations for the window");
         while (i1==i2) {
            while (!getLoc(x1, y1, plotter_p)) {};
            i1 = Int((x1 - (values(0) - binWidth/2))/binWidth);
            i1 = min(Int(nBins-1),max(0,i1));
            drawVertical (values(i1), yMin, yMax, plotter_p);

            T x2 = x1;
            while (!getLoc(x2, y1, plotter_p)) {};
            i2 = Int((x2 - (values(0) - binWidth/2))/binWidth);
            i2 = min(Int(nBins-1),max(0,i2));
            drawVertical (values(i2), yMin, yMax, plotter_p);

            if (i1 == i2) {
               plotter_p.message("Degenerate window, try again");
               plotter_p.eras ();
               drawHistogram (values, counts, plotter_p);
            }
         }


// Set window 

         iMin = min(i1, i2);
         iMax = max(i1, i2);
      }

// Now generate the distribution we want to fit.  Normalize to
// peak 1 to help fitter.  

      const uInt nPts2 = iMax - iMin + 1; 
      Vector<T> xx(nPts2);
      Vector<T> yy(nPts2);
      Int i;
      for (i=iMin; i<=iMax; i++) {
         xx(i-iMin) = values(i);
         yy(i-iMin) = counts(i)/yMax;
      }


// Create fitter

      NonLinearFitLM<T> fitter;
      Gaussian1D<T> gauss;
      fitter.setFunction(gauss);


// Initial guess

      Vector<T> v(3);
      v(0) = 1.0;                          // height
      v(1) = values(yMaxPos(0));           // position
      v(2) = nPts2*binWidth/2;             // width


// Fit

      fitter.setFittedFuncParams(v);
      fitter.setMaxIter(50);
      T tol = 0.001;
      fitter.setCriteria(tol);

      Vector<T> resultSigma(nPts2);
      resultSigma = 1;
      Vector<T> solution;
      Bool fail = False;
      try {
        solution = fitter.fit(xx, yy, resultSigma);
      } catch (AipsError x) {
        fail = True;
      } end_try;
//      os_p << LogIO::NORMAL << "Solution=" << solution.ac() << LogIO::POST;


// Return values of fit 

      if (!fail && fitter.converged()) {
         sigma = T(abs(solution(2)) / sqrt(2.0));
         os_p << LogIO::NORMAL 
              << "*** The fitted standard deviation of the noise is " << sigma
              << endl << LogIO::POST;

// Now plot the fit 

         if (plotter_p.isAttached()) {
            Int nGPts = 100;
            T dx = (values(nBins-1) - values(0))/nGPts;

            Gaussian1D<T> gauss(solution(0), solution(1), abs(solution(2)));
            Vector<T> xG(nGPts);
            Vector<T> yG(nGPts);

            T xx;
            for (i=0,xx=values(0); i<nGPts; xx+=dx,i++) {
               xG(i) = xx;
               yG(i) = gauss(xx) * yMax;
            }
            plotter_p.sci (7);
            drawLine (xG, yG, plotter_p);
            plotter_p.sci (1);
         }
      } else {
         os_p << LogIO::NORMAL << "The fit to determine the noise level failed." << endl;
         os_p << "Try inputting it directly" << endl;
         if (plotter_p.isAttached()) os_p << "or try a different window " << LogIO::POST;
      }

// Another go

      if (plotter_p.isAttached()) {
         plotter_p.message("Accept (click left), redo (click middle), give up (click right)");

         Float xx = convertT(xMin+xMax)/2;
         Float yy = convertT(yMin+yMax)/2;
         String str;
         readCursor(plotter_p, xx, yy, str);
         str.upcase();
 
         if (str == "D") {
            plotter_p.message("Redoing fit");
         } else if (str == "X") {
            return False;
         } else {
            more = False;
         }
      } else {
         more = False;
      }
   }
     
   return True;

}


template <class T> 
Bool ImageMoments<T>::readCursor (PGPlotter& plotter, Float& x,
                                  Float& y, String& ch)
{
   Record r;
   r = plotter.curs(x, y);
   Bool gotCursor;
   r.get(RecordFieldId(0), gotCursor);
   r.get(RecordFieldId(1), x);
   r.get(RecordFieldId(2), y);
   r.get(RecordFieldId(3), ch);
   return gotCursor;
}
 
