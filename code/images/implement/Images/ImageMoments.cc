//# ImageMoments.cc:  generate moments from an image
//# Copyright (C) 1995,1996,1997
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
#include <aips/Arrays/MaskedArray.h>
#include <aips/Arrays/MaskArrMath.h>
#include <aips/Arrays/VectorIter.h>
#include <aips/Exceptions/Error.h>
#include <aips/Functionals/Gaussian1D.h>
#include <aips/Functionals/Polynomial.h>
#include <aips/Functionals/SumFunction.h>
#include <aips/Inputs/Input.h>
#include <aips/Lattices/Slice.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Convolver.h>
#include <aips/Utilities/DataType.h>
#include <aips/Utilities/GenSort.h>
#include <aips/Utilities/String.h>
                          
#include <trial/Fitting/NonLinearFitLM.h>
#include <trial/Functionals/FuncWithAutoDerivs.h>
#include <trial/Coordinates.h>
#include <trial/Images/ImageInterface.h>
#include <trial/Images/ImageUtilities.h>
#include <trial/Images/PagedImage.h>
#include <trial/Images/ImageMoments.h>
#include <trial/Lattices/ArrayLattice.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/LatticeStepper.h>
#include <trial/Mathematics/AutoDiffIO.h>
               
#include <strstream.h>
#include <iomanip.h>


// C wrappers for PGPLOT
   
extern "C" {
#include <cpgplot.h>
};


template <class T> 
ImageMoments<T>::ImageMoments (const ImageInterface<T>& image, 
                               LogIO &os) : os_p(os)
//
// Constructor. 
//
{
   goodParameterStatus_p = True;

   if (setNewImage(image)) {
      moments_p.resize(1);
      moments_p(0) = INTEGRATED;
      momentAxisDefault_p = -10;
      momentAxis_p = momentAxisDefault_p;
      doWindow_p = False;
      doFit_p = False;
      doAuto_p = True;   
      doSmooth_p = False;
      noInclude_p = True;
      noExclude_p = True;
      peakSNR_p = 3;
      stdDeviation_p = 0.0;
   } else {
      goodParameterStatus_p = False;
   }
}

template <class T>
ImageMoments<T>::ImageMoments(const ImageMoments<T> &other)
                      : os_p(other.os_p),
                        momentAxis_p(other.momentAxis_p),
                        momentAxisDefault_p(other.momentAxisDefault_p),
                        kernelTypes_p(other.kernelTypes_p),
                        kernelWidths_p(other.kernelWidths_p),
                        nxy_p(other.nxy_p),
                        pixelIn_p(other.pixelIn_p),
                        moments_p(other.moments_p),
                        range_p(other.range_p),
                        smoothAxes_p(other.smoothAxes_p),
                        worldOut_p(other.worldOut_p),
                        peakSNR_p(other.peakSNR_p),
                        stdDeviation_p(other.stdDeviation_p),
                        device_p(other.device_p),
                        out_p(other.out_p),
                        psfOut_p(other.psfOut_p),
                        smoothOut_p(other.smoothOut_p),
                        goodParameterStatus_p(other.goodParameterStatus_p),
                        doWindow_p(other.doWindow_p),
                        doFit_p(other.doFit_p),
                        doAuto_p(other.doAuto_p),
                        doSmooth_p(other.doSmooth_p),
                        noInclude_p(other.noInclude_p),
                        noExclude_p(other.noExclude_p)
//
// Copy constructor
//
{
   
// Assign to image pointer 
   
   pInImage_p = other.pInImage_p;
   
}



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
      momentAxis_p = other.momentAxis_p;
      momentAxisDefault_p = other.momentAxisDefault_p;
      kernelTypes_p = other.kernelTypes_p;
      kernelWidths_p = other.kernelWidths_p;
      nxy_p = other.nxy_p;
      pixelIn_p = other.pixelIn_p;
      moments_p = other.moments_p;
      range_p = other.range_p;
      smoothAxes_p = other.smoothAxes_p;
      worldOut_p = other.worldOut_p;
      peakSNR_p = other.peakSNR_p;
      stdDeviation_p = other.stdDeviation_p;
      device_p = other.device_p;
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

      return *this;
   }
   
}


template <class T> 
Bool ImageMoments<T>::setNewImage(const ImageInterface<T>& image)
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

   Int nMom = moments_p.nelements();
   if (nMom <= 0) {
      os_p << LogIO::SEVERE << "No moments requested" << LogIO::POST;
      goodParameterStatus_p = False;
      return False;
   } else if (nMom > NMOMENTS) {
      os_p << LogIO::SEVERE << "Too many moments specified" << LogIO::POST;
      goodParameterStatus_p = False;
      return False;
   }

   for (Int i=0; i<nMom; i++) {
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
     momentAxis_p = findSpectralAxis(pInImage_p);
     if (momentAxis_p == -1) {
       os_p << LogIO::SEVERE << "There is no spectral axis in this image -- specify the axis" << LogIO::POST;
       goodParameterStatus_p = False;
       return False;
     }
   } else {
      if (momentAxis_p < 0 || momentAxis_p > pInImage_p->ndim()-1) {
         os_p << LogIO::SEVERE << "Illegal moment axis" << LogIO::POST;
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


// Check legality

   for (Int i = 0; i<methodU.nelements(); i++) {
      if (methodU(i) < 0 || methodU(i) > NMETHODS-1) {
         os_p << LogIO::SEVERE << "Illegal method given" << LogIO::POST;
         goodParameterStatus_p = False;
         return False;
      }
   }


// Assign Boooools

   doWindow_p = Bool(ImageUtilities::inVector(WINDOW, methodU)!=-1);
   doFit_p    = Bool(ImageUtilities::inVector(FIT, methodU)!=-1);
   doAuto_p   = Bool(ImageUtilities::inVector(INTERACTIVE, methodU)==-1);
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
      for (i=0; i<smoothAxes_p.nelements(); i++) {
         if (smoothAxes_p(i) < 0 || smoothAxes_p(i) > pInImage_p->ndim()-1) {
            os_p << LogIO::SEVERE << "Illegal smoothing axis given" << LogIO::POST;
            goodParameterStatus_p = False;
            return False;
         }
      }
      doSmooth_p = True;
   } else {
      os_p << LogIO::SEVERE << "No smoothing axes given" << LogIO::POST;
      goodParameterStatus_p = False;
      return False;
   }


// Now check the smoothing types

   if (kernelTypesU.nelements() > 0) {
      kernelTypes_p = kernelTypesU;
      for (i=0; i<kernelTypes_p.nelements(); i++) {
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
   for (i=0; i<smoothAxes_p.nelements(); i++) {
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
Bool ImageMoments<T>::setInExCludeRange(const Vector<Double>& includeU,
                                        const Vector<Double>& excludeU)
//
// Assign the desired exclude range           
//
{
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }

   Vector<Double> include = includeU;
   Vector<Double> exclude = excludeU;

   ostrstream os;
   if (!ImageUtilities::setIncludeExclude(range_p, noInclude_p, noExclude_p,
                                          include, exclude, os)) {
      os_p << LogIO::SEVERE << "Invalid pixel inclusion/exclusion ranges" << LogIO::POST;
      goodParameterStatus_p = False;
      return False;
   }
   return True; 

}


template <class T> 
Bool ImageMoments<T>::setSnr(const Double& peakSNRU,
                             const Double& stdDeviationU)
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
      peakSNR_p = 3.0;
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
Bool ImageMoments<T>::setPlotting(const String& deviceU,
                                  const Vector<Int>& nxyU)
//   
// Assign the desired PGPLOT device name and number
// of subplots
//
{ 
   if (!goodParameterStatus_p) {
      os_p << LogIO::SEVERE << "Internal class status is bad" << LogIO::POST;
      return False;
   }

   device_p = deviceU;
   nxy_p.resize(0);
   nxy_p = nxyU;
   if (!ImageUtilities::setNxy(nxy_p, os_p.output())) {
      goodParameterStatus_p = False;
      return False;
   }
   return True;
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
   

// Make remaining consistency checks

   if (pInImage_p->name() == out_p && moments_p.nelements()==1) {
      os_p << LogIO::SEVERE << endl << "Input and output names are the same !" << LogIO::POST;
      return False;
   }
 
   if (momentAxis_p == momentAxisDefault_p) {
     momentAxis_p = findSpectralAxis(pInImage_p);
     if (momentAxis_p == -1) {
       os_p << LogIO::SEVERE << endl << "There is no spectral axis in this image -- specify "
	 "the axis" << LogIO::POST;
       return False;
     }
   }


// Only can have the median coordinate under certain conditions
   
   if (ImageUtilities::inVector(MEDIAN_COORDINATE, moments_p) != -1) {
      Bool noGood = False;
      if (doWindow_p || doFit_p || doSmooth_p) {
         noGood = True;
      } else {
         if (noInclude_p && noExclude_p) {
            noGood = True;
         } else {
           if (range_p(0)*range_p(1) < 0) noGood = True;
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

// Make sure we can do what user asks for.

   if ( ((doWindow_p && !doAuto_p) ||
         (!doWindow_p && doFit_p && !doAuto_p)) && device_p.empty()) {
      os_p << LogIO::SEVERE << "You have not given a plotting device" << LogIO::POST;
      return False;
   }
   if (!checkMethod()) return False;


// Fish out the coordinate systems and remove the collapsed moment pixel axis, 
// leaving behind the world axis
       
   os_p << LogIO::NORMAL << endl << "Moment axis type is "
      << pInImage_p->coordinates().worldAxisNames()(momentAxis_p) << LogIO::POST;

   CoordinateSystem outImageCoord = pInImage_p->coordinates();

   uInt removeAxis = momentAxis_p;
   outImageCoord.removePixelAxis(removeAxis, 1.0);


// Resize the moment axis coordinate transformation arrays

   worldOut_p.resize(pInImage_p->ndim());
   pixelIn_p.resize (pInImage_p->ndim());



// Create a vector, each element of which points to the appropriate
// moment in the moments array
         
   Vector<Int> selMom(NMOMENTS);
   selMom = 0;
    

// Create array of pointers for output images

   PagedImage<T>* outPt[NMOMENTS];
   for (Int i=0; i<NMOMENTS; i++)  outPt[i] = 0;
   

// Loop over the user specified moments and assign output image pointers and the
// selection array
   
   String suffix;
   Bool doMedianI = False;
   Bool doMedianV = False;
   Bool doAbsDev = False;

   for (i=0; i<moments_p.nelements(); i++) {
      selectMoment (doMedianI, doMedianV, doAbsDev, suffix, selMom(i), i);
   
// Create output image(s)

      IPosition outImageShape(pInImage_p->shape());
      outImageShape(momentAxis_p) = 1;
      const String in = pInImage_p->name();   

      if (moments_p.nelements() == 1) {
         if (out_p.empty()) out_p = in+suffix;
         outPt[i] = new PagedImage<T>(outImageShape, outImageCoord, out_p);
         os_p << LogIO::NORMAL << "Created " << out_p << LogIO::POST;
      } else {
         if (out_p.empty()) out_p = in;
         outPt[i] = new PagedImage<T>(outImageShape, outImageCoord,
                                      out_p+suffix);
         os_p << LogIO::NORMAL << "Created " << out_p+suffix << LogIO::POST;
      }
   }


// We only smooth the image if we are doing the smooth/clip method
// or possibly the interactive window method.  Note that the convolution
// routines can only handle convolution when the image fits fully in core
// at present.
   
   ArrayLattice<Bool>* pMask = 0;
   ArrayLattice<T>* pSmoothedImage = 0;
   if (doSmooth_p) {

      Int axMax = max(smoothAxes_p.ac()) + 1;
      if (axMax > pInImage_p->ndim()) {
         os_p << LogIO::SEVERE << "You have specified a smoothing axis larger" << endl;
         os_p <<                  "than the number of axes in the image" << LogIO::POST;
         return False;
      }
      
// Smooth image
    
      pSmoothedImage = new ArrayLattice<T>(pInImage_p->shape());
      if (!smoothImage(pSmoothedImage)) {
         os_p << LogIO::SEVERE << "Error smoothing image" << LogIO::POST;
         return False;
      }
        
      if (!doWindow_p) {
   
// We only require the mask for the smooth and clip method, not the window method
   
         pMask = new ArrayLattice<Bool>(pInImage_p->shape());
         pMask->set(True);
         makeMask (pMask, pSmoothedImage);

// We may require the smoothed image for the window method only,
// so delete it here.

         delete pSmoothedImage;   
         pSmoothedImage = 0;
      }
   }


// Set up image iterator 
   
   IPosition cursorShape(pInImage_p->ndim(),1);
   cursorShape(momentAxis_p) = pInImage_p->shape()(momentAxis_p);   
   RO_LatticeIterator<T> imageIterator(*pInImage_p, cursorShape);
   
      
// Set up smoothed image iterator and navigator
         
   RO_LatticeIterator<T>* pSmoothedImageIterator = 0;
   if (pSmoothedImage) {
      pSmoothedImageIterator = new RO_LatticeIterator<T>(*pSmoothedImage, cursorShape);
   }


// Set up mask iterator and navigator if required
         
   RO_LatticeIterator<Bool>* pMaskIterator = 0;
   if (pMask) {
      pMaskIterator = new RO_LatticeIterator<Bool>(*pMask, cursorShape);
   }
            
            
// If the user is using the automatic, non-fitting window method, it
// it needs a good assement of the noise.  The user can input that value, but
// if they don't, we work it out here.
    
   Double noise;
   if ( stdDeviation_p <=0 && ( (doWindow_p && doAuto_p) || (doFit_p && !doWindow_p && doAuto_p) ) ) {
      if (pSmoothedImage) {
         os_p << LogIO::NORMAL << "Evaluating noise level from smoothed image" << LogIO::POST;
         if (!whatIsTheNoise (noise, pSmoothedImage)) return False;
      } else {
         os_p << LogIO::NORMAL << "Evaluating noise level from image" << LogIO::POST;
         if (!whatIsTheNoise (noise, pInImage_p)) return False;
      }
      stdDeviation_p = noise;
   }
   

// Array to hold moments
   
   Vector<T> calcMoments(NMOMENTS);   
   calcMoments = 0.0;
   String momAxisType = pInImage_p->coordinates().worldAxisNames()(momentAxis_p);
   

// TESTING KS method

   const Double ks = 0.03;



// Open plot device
         
   Bool doPlot = False;
   if (doWindow_p || (!doWindow_p && doFit_p)) {
      if (!device_p.empty()) {
         if(cpgbeg(0, device_p.chars(), nxy_p(0), nxy_p(1)) != 1) {
	    os_p << LogIO::SEVERE << "Could not open display device" << LogIO::POST;
            return False;
         }
         cpgsch (1.5);
         cpgvstd();
         doPlot = True;
      }
   }        


      
// Iterate through image and do all the wonderful things

   os_p << LogIO::NORMAL << "Begin computation of moments" << LogIO::POST;
   while (!imageIterator.atEnd()) {

// Set pixel values of all axes (used for coordinate transformation)
// to be start of cursor array.  The values for the moment axis 
// are set in getMomentCoord

      for (i=0; i<pixelIn_p.nelements(); i++) {pixelIn_p(i) = imageIterator.position()(i);}

// Choose method

      if (doSmooth_p && !doWindow_p) {
      
// Smooth and clip
   
         doMomSm (calcMoments, imageIterator.vectorCursor(),
                  pMaskIterator->vectorCursor(),
                  doMedianI, doMedianV, doAbsDev);
      } else if (doWindow_p) {
    
// Window
     
         const Vector<T>* pSmoothedVector = 0;
         if (pSmoothedImage) {
            pSmoothedVector = &(pSmoothedImageIterator->vectorCursor());
         }
         doMomWin (calcMoments, imageIterator.vectorCursor(), pSmoothedVector,
                   doMedianI, doMedianV, doAbsDev, doPlot,
                   momAxisType, imageIterator.position(), ks);
      } else if (doFit_p) {

// Fit   
   
         doMomFit (calcMoments, imageIterator.vectorCursor(), 
                   doMedianI, doMedianV, doAbsDev, doPlot, 
                   momAxisType, imageIterator.position());
      } else {
            
// no clip or clip
         
         doMomCl (calcMoments, imageIterator.vectorCursor(), 
                  doMedianI, doMedianV, doAbsDev);
      }     

// Fill output images

      IPosition outPos(imageIterator.position()); 
      outPos(momentAxis_p) = 0;
      for (i=0; i<moments_p.nelements(); i++) (*(outPt[i]))(outPos) = calcMoments(selMom(i));
      

// Increment iterators
                  
      imageIterator++;
      if (pMaskIterator) (*pMaskIterator)++;
      if (pSmoothedImageIterator) (*pSmoothedImageIterator)++;
   }

// Delete memory
         
   for (i=0; i<moments_p.nelements(); i++) delete outPt[i];
   if (pMask) delete pMask;
   if (pMaskIterator) delete pMaskIterator;


// Success

   return True;

}




// Private member functions

template <class T> 
void ImageMoments<T>::accumSums (Double& s0,
                                 Double& s0Sq,
                                 Double& s1,
                                 Double& s2,
                                 Int& iMin,
                                 Int& iMax,
                                 Double& dMin,
                                 Double& dMax,
                                 const Int& i,
                                 const T& datum,
                                 const Double& coord)
// 
// Accumulate statistical sums from this datum
//
// Input:
//  i              Index
//  datum          Pixel value
//  coord          Coordinate value on moment axis
// Input/output:
//  iMin,max       index of dMin and dMax
//  dMin,dMax      minimum and maximum value
// Output:
//  s0             sum (I)
//  s0Sq           sum (I*I)
//  s1             sum (I*v)
//  s2             sum (I*v*v)
{           
   Double dDatum = Double(datum);
   s0 += dDatum;   
   s0Sq += dDatum*dDatum;
   s1 += dDatum*coord;
   s2 += dDatum*coord*coord;
   if (dDatum < dMin) {
     iMin = i;
     dMin = dDatum;
   }
   if (dDatum > dMax) {
     iMax = i;
     dMax = dDatum;
   }
}


template <class T> 
Bool ImageMoments<T>::allNoise (T& dMean,
                                const Vector<T>& data)
//                
// Try and work out whether this spectrum is all noise
// or not.  We don't bother with it if it is noise.
// We compare the peak with sigma and a cutoff SNR
//
{         
   T dMin, dMax;
   minMax (dMin, dMax, data.ac());
   dMean = mean(data.ac());
   T rat = max(abs(dMin),abs(dMax)) / stdDeviation_p;

   if (rat < peakSNR_p) {
      return True; 
   } else {
      return False;
   }
}        
   


template <class T> 
Bool ImageMoments<T>::allNoise (const Vector<T>& spectrum,
                                   const Double& sigma, 
                                   const Double& ks)
//
// Try and work out whether this spectrum is all noise
// or not.  We don't bother with it if it is noise.
// We use the Kolmogorov-Smirnov test for this.
//
// Inputs:
//  sigma  The sigma of the theoretical noise distribution
//
{
// Copy spectrum  

   Int nPts = spectrum.nelements();
   Vector<T> tSpectrum(nPts);
   tSpectrum = spectrum;

// Sort it

   uInt nSort = GenSort<T>::sort(tSpectrum, Sort::Ascending, Sort::QuickSort);

// Make cumulative and find maximum D statistic  
          
   Double CData = 0;
   Double step = 1.0 / Double(nSort);
   Double DMax = 0.0;
   Double CGauss;
   for (uInt i=1; i<nSort; i++) {
      
// Data
   
      CData += step;
         
// Model
        
      CGauss = getGaussianCumulativeProb(sigma, Double(tSpectrum(i)));

// Maximum D
               
      DMax = max(DMax, abs(CData-CGauss));
   }


// Get KS probability

   os_p << LogIO::NORMAL << "DMax = " << DMax << LogIO::POST;
   Double prob = getKSProbability(nSort, DMax);
   os_p << LogIO::NORMAL << "KS prob = " << prob << LogIO::POST;
   if (prob < ks) {
      return False;
   } else {
      return True;
   }
}




template <class T> 
Bool ImageMoments<T>::checkMethod ()
// 
// Make sure we can do what the user wants
//  
{
   Bool doInter = Bool(!doAuto_p);

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
      os_p << LogIO::NORMAL << "Smooth    Window      Fit   in/exclude   Interactive " << LogIO::POST;
      os_p << LogIO::NORMAL << "-----------------------------------------------------" << LogIO::POST;
   
// Basic method. Just use all the data
   
      os_p << LogIO::NORMAL << "  N          N         N        N            N       " << LogIO::POST;
                       
// Smooth and clip, or just clip
                  
      os_p << LogIO::NORMAL << "  Y/N        N         N        Y            N       " << LogIO::POST << LogIO::POST;
                  
// Direct interactive window selection with or without smoothing
                  
      os_p << LogIO::NORMAL << "  Y/N        Y         N        N            Y       " << LogIO::POST;

// Automatic windowing via Bosma's algorithm with or without smoothing
 
      os_p << LogIO::NORMAL << "  Y/N        Y         N        N            N       " << LogIO::POST;

// Windowing by fitting Gaussians (selecting +/- 3-sigma) automatically or interactively 
// with or without out smoothing
          
      os_p << LogIO::NORMAL << "  Y/N        Y         Y        N            Y/N     " << LogIO::POST;
          
// Interactive and automatic Fitting of Gaussians and the moments worked out
// directly from the fits
          
      os_p << LogIO::NORMAL << "  N          N         Y        N            Y/N     " << LogIO::POST;


      os_p << LogIO::NORMAL << LogIO::POST << "You have asked for" << LogIO::POST << LogIO::POST;
      if (doSmooth_p) 
         os_p << LogIO::NORMAL << "  Y";
      else
         os_p << LogIO::NORMAL << "  N";

      if (doWindow_p) 
         os_p << LogIO::NORMAL << "          Y";
      else
         os_p << LogIO::NORMAL << "          N";
      if (doFit_p) 
         os_p << LogIO::NORMAL << "         Y";
      else
         os_p << LogIO::NORMAL << "         N";
      if (noInclude_p && noExclude_p)
         os_p << LogIO::NORMAL << "        N";
      else
         os_p << LogIO::NORMAL << "        Y";
      if (doAuto_p)
         os_p << LogIO::NORMAL << "            Y";
      else
         os_p << LogIO::NORMAL << "            N";
      os_p << LogIO::NORMAL << LogIO::POST;
      os_p << LogIO::NORMAL << "-----------------------------------------------------" << LogIO::POST;
      return False;
   }


// Tell them what they are getting
          
   os_p << LogIO::NORMAL << LogIO::POST << LogIO::POST
        << "***********************************************************************" << LogIO::POST;
   os_p << LogIO::NORMAL << "You have selected the following methods" << LogIO::POST;
   if (doWindow_p) {
      os_p << LogIO::NORMAL << "The window method" << LogIO::POST;
      if (doFit_p) {
         if (doInter)
            os_p << LogIO::NORMAL << "   with window selection via interactive Gaussian fitting" << LogIO::POST;
         else
            os_p << LogIO::NORMAL << "   with window selection via automatic Gaussian fitting" << LogIO::POST;
      } else {
         if (doInter)
            os_p << LogIO::NORMAL << "   with interactive direct window selection" << LogIO::POST;
         else     
            os_p << LogIO::NORMAL << "   with automatic window selection via the Bosma algorithm" << LogIO::POST;
      }           
      if (doSmooth_p) {
         os_p << LogIO::NORMAL << "   operating on the smoothed image.  The moments are still" << LogIO::POST;
         os_p << LogIO::NORMAL << "   evaluated from the unsmoothed image" << LogIO::POST;
      } else
         os_p << LogIO::NORMAL << "   operating on the unsmoothed image" << LogIO::POST;
   } else if (doFit_p) {
      if (doInter)
         os_p << LogIO::NORMAL << "The interactive Gaussian fitting method" << LogIO::POST;
      else
         os_p << LogIO::NORMAL << "The automatic Gaussian fitting method" << LogIO::POST;
          
      os_p << LogIO::NORMAL << "   operating on the unsmoothed data" << LogIO::POST;
      os_p << LogIO::NORMAL << "   The moments are evaluated from the fits" << LogIO::POST;
   } else if (doSmooth_p) {
      os_p << LogIO::NORMAL << "The smooth and clip method.  The moments are evaluated from" << LogIO::POST;
      os_p << LogIO::NORMAL << "   the masked unsmoothed image" << LogIO::POST;
   } else {
      if (noInclude_p && noExclude_p)
         os_p << LogIO::NORMAL << "The basic show it as it is method !" << LogIO::POST;
      else
         os_p << LogIO::NORMAL << "The clip method" << LogIO::POST;
   }
   os_p << LogIO::NORMAL << LogIO::POST << LogIO::POST << LogIO::POST;
   
      
   return True;   
}




template <class T> 
void ImageMoments<T>::doMomCl (Vector<T>& calcMoments,
                               const Vector<T>& data,
                               const Bool& doMedianI,
                               const Bool& doMedianV,
                               const Bool& doAbsDev)
//
// Generate clipped moments of this profile
//
// Output:
//   calcMomentsThe many moments
// Input:
//   data       The data chunk
//   doMedian   Don't bother with median unless we really have to
//   doAbsDev   Don't bother with absolute deviations unless we really have to
{
   Bool doInclude = Bool(!noInclude_p);
   Bool doExclude = Bool(!noExclude_p);
   
// Assign array for median.  Is resized appropriately later
      
   Int nPts = data.nelements();
   Vector<T> medianArray(nPts);
   Vector<Int> medianArrayIndex(nPts);
                    
       
// Compute moments

   Double s0  = 0.0;
   Double s0Sq = 0.0;
   Double s1  = 0.0;
   Double s2  = 0.0;
   Int iMin = -1;   
   Int iMax = -1;
   Double dMin =  1.0e30;
   Double dMax = -1.0e30;
   Double coord;

   if (doInclude) {
      for (Int i=0,j=0; i<nPts; i++) {
         if (data(i) >= range_p(0) && data(i) <= range_p(1)) {
            coord = getMomentCoord (Double(i));
            accumSums (s0, s0Sq, s1, s2, iMin, iMax,
                       dMin, dMax, i, data(i), coord);
            medianArray(j) = data(i);
            medianArrayIndex(j) = i;
            j++;
         }
      }
      nPts = j;
   } else if (doExclude) {
      for (Int i=0,j=0; i<nPts; i++) {
         if (data(i) <= range_p(0) || data(i) >= range_p(1)) {
            Double coord = getMomentCoord (Double(i));
            accumSums (s0, s0Sq, s1, s2, iMin, iMax,
                       dMin, dMax, i, data(i), coord);
            medianArray(j) = data(i);
            medianArrayIndex(j) = i;
            j++;
         }
      }
      nPts = j;
   } else {
      for (Int i=0; i<nPts; i++) {
         Double coord = getMomentCoord (Double(i));
         accumSums (s0, s0Sq, s1, s2, iMin, iMax,
                    dMin, dMax, i, data(i), coord);
         medianArray(i) = data(i);
         medianArrayIndex(i) = i;
      }   
   }
         
    
// If no points make moments zero. Blank at a later date.
   
   if (nPts==0) {
      calcMoments = 0.0;
      return;
   }

 
// Median of I
            
   T dMedian = 0.0;
   if (doMedianI) {
      medianArray.resize(nPts,True);
      dMedian = median(medianArray.ac());
   }
              

// Median coordinate.  Only available with an include or exclude range
// and still pretty dodgy

   T vMedian = 0.0;
   if (doMedianV) {
      if (doInclude || doExclude) {

// Treat spectrum as a probability distribution for velocity
// and generate cumulative probability (it's already sorted
// of course).

         medianArray.resize(nPts,True);
         medianArray(0) = abs(medianArray(0));
         T dataMax = medianArray(0);
         for (Int i=1; i<nPts; i++) {
            medianArray(i) += abs(medianArray(i-1));
            dataMax = max(dataMax,medianArray(i));
         }

// Find 1/2 way value (well, the first one that occurs)

         T halfMax = dataMax/2.0;
         Int iVal;
         for (i=0; i<nPts; i++) {
            if (medianArray(i) >= halfMax) {
               iVal = i;
               break;
            }
         }

// Linearly interpolate to velocity index

         Double interpPixel;
         if (iVal > 0) {
            Double m = (medianArray(iVal) - medianArray(iVal-1)) /
                      (medianArrayIndex(iVal) - medianArrayIndex(iVal-1));
            Double b = medianArray(iVal) - m*medianArrayIndex(iVal);
            interpPixel = (medianArray(iVal) -b) / m;
         } else {
            interpPixel = medianArrayIndex(iVal);
         }           

// Find world coordinate of that pixel on the moment axis

        vMedian = getMomentCoord (interpPixel);
      }
   } 
               

// Absolute deviations of I from mean needs an extra pass.
               
   Double sumAbsDev = 0.0;
   if (doAbsDev) {
      Double iMean = s0 / nPts;
      nPts = data.nelements();

      if (doInclude) {
         for (Int i=0; i<nPts; i++) {
            if (data(i) >= range_p(0) && data(i) <= range_p(1))
               sumAbsDev += abs(data(i) - iMean);
         }
      } else if (doExclude) {
         for (Int i=0; i<nPts; i++) {
            if (data(i) <= range_p(0) || data(i) >= range_p(1))
               sumAbsDev += abs(data(i) - iMean);
         }
      } else
         for (Int i=0; i<nPts; i++) sumAbsDev += abs(Double(data(i)) - iMean);
   }


// Fill moments array
 
   setCalcMoments (calcMoments, dMedian, vMedian, nPts, s0, s1, s2, s0Sq, sumAbsDev,
                   dMin, dMax, iMin, iMax);
            
}



template <class T> 
void ImageMoments<T>::doMomFit (Vector<T>& calcMoments,
                                const Vector<T>& data,
                                const Bool& doMedianI,
                                const Bool& doMedianV,
                                const Bool& doAbsDev,
                                const Bool& doPlot,               
                                const String& momAxisType,
                                const IPosition& pos)
//
// Generate moments from a Gaussian fit of this profile
// 
// Output:
//   calcMoments The many moments
// Input:
//   data        The spectrum
//   doMedian    Don't bother with median unless we really have to
//   doAbsDev    Don't bother with absolute deviations unless we really have to
//   doPlot      Plotting device is active
//   momAxisType Name of moment axis
//   pos         Position in image of start of data vector
// 
{
   
// Create the abcissa array and some labels
   
   Vector<T> gaussPars(4);
   Vector<T> abcissa;
   makeAbcissa (abcissa, data.nelements());
   String xLabel;
   if (momAxisType.empty()) 
      xLabel = "x (pixels)";
   else
      xLabel = momAxisType + " (pixels)";
   String yLabel("Intensity");
   String title;
   setPosLabel (title, pos);
 

   if (doAuto_p) {
   
// Automatic

      if (!getAutoGaussianFit (gaussPars, abcissa, data, doPlot, xLabel, yLabel, title)) {
         calcMoments = 0;
         return;
      }
   
   } else {
      
// Interactive
   
      if (!getInterGaussianFit (gaussPars, abcissa, data, xLabel, yLabel, title)) {
         calcMoments = 0;
         return;
      }
   }
 
   
// Generate Gaussian array

   Int nPts = data.nelements();
   Vector<T> gData(nPts);
   Gaussian1D<T> gauss(gaussPars(0), gaussPars(1), gaussPars(2));
   T xx;
   for (Int i=0; i<nPts; i++) {
      xx = i;
      gData(i) = gauss(xx) + gaussPars(3);
   }


// Assign array for median.

   Vector<T> medianArray(nPts); 

   
// Compute moments
   
   Double s0  = 0.0;
   Double s0Sq = 0.0;
   Double s1  = 0.0;
   Double s2  = 0.0;
   Int iMin = -1;
   Int iMax = -1;
   Double dMin =  1.0e30;
   Double dMax = -1.0e30;   
   Double coord;
 
   for (i=0; i<nPts; i++) {
      coord = getMomentCoord (Double(i));
      accumSums (s0, s0Sq, s1, s2, iMin, iMax,
                 dMin, dMax, i, gData(i), coord);
      medianArray(i) = gData(i);
   }

// Medians
   
   T dMedian = 0.0;
   if (doMedianI) dMedian = median(medianArray.ac());

   T vMedian = 0.0;
   
         
// Absolute deviations of I from mean.  Requires a second pass
       
   Double sumAbsDev = 0.0;
   if (doAbsDev) {
      Double iMean = s0 / nPts;
      for (Int i=0; i<=nPts; i++) sumAbsDev += abs(Double(gData(i)) - iMean);
    }
   
         
// Fill moments array
       
   setCalcMoments (calcMoments, dMedian, vMedian, nPts, s0, s1, s2, s0Sq, sumAbsDev,
                     dMin, dMax, iMin, iMax);
}        



template <class T> 
void ImageMoments<T>::doMomSm (Vector<T>& calcMoments,
                               const Vector<T>& data, 
                               const Vector<Bool>& mask,
                               const Bool& doMedianI,
                               const Bool& doMedianV,
                               const Bool& doAbsDev)
//   
// Generate masked moments of this profile
//
// Output:
//   calcMomentsThe many moments
// Input:    
//   data       The data chunk
//   mask       The mask
//   doMedian   Don't bother with median unless we really have to
//   doAbsDev   Don't bother with absolute deviations unless we really have to
//
{

// Assign array for median.  

   Int nPts = data.nelements();
   Vector<T> medianArray(nPts);


// Compute moments

   Double s0  = 0.0;
   Double s0Sq = 0.0;
   Double s1  = 0.0;
   Double s2  = 0.0;
   Int iMin = -1;
   Int iMax = -1;
   Double dMin =  1.0e30;
   Double dMax = -1.0e30;
   Double coord;
 
   for (Int i=0,j=0; i<nPts; i++) {
      if (mask(i)) {
         coord = getMomentCoord (Double(i));
         accumSums (s0, s0Sq, s1, s2, iMin, iMax, dMin, dMax,
                    i, data(i), coord);
         medianArray(j) = data(i);
         j++;
      }
   }
   nPts = j;


// If no points make moments zero. Blank at a later date.

   if (nPts==0) {
      calcMoments = 0.0;
      return;
   }


// Medians

   T dMedian = 0.0;
   if (doMedianI) {
      medianArray.resize(nPts,True);
      dMedian = median(medianArray.ac());
   }

   T vMedian = 0.0;

// Absolute deviations of I from mean.  Requires a second pass

   Double sumAbsDev = 0.0;
   if (doAbsDev) {
      Double iMean = s0 / nPts;
      Int nPts = data.nelements();
      for (Int i=0; i<nPts; i++) if (mask(i)) sumAbsDev += abs(Double(data(i) - iMean));
    }


// Fill moments array

   setCalcMoments (calcMoments, dMedian, vMedian, nPts, s0, s1, s2, s0Sq, sumAbsDev, 
                   dMin, dMax, iMin, iMax);
}


template <class T> 
void ImageMoments<T>::doMomWin (Vector<T>& calcMoments,
                                const Vector<T>& data, 
                                const Vector<T>* pSmoothedData, 
                                const Bool& doMedianI,
                                const Bool& doMedianV,
                                const Bool& doAbsDev,
                                const Bool& doPlot,
                                const String& momAxisType,
                                const IPosition& pos, const Double& ks)
//   
// Generate windowed moments of this profile
//
// Output:
//   calcMoments The many moments
// Input:    
//   os          Output stream
//   data        The spectrum
//   pSmoothedData
//               Pointer to the smoothed spectrum
//   doMedian    Don't bother with median unless we really have to
//   doAbsDev    Don't bother with absolute deviations unless we really have to
//   doPlot      Plotting device is active
//   momAxisType Name of moment axis
//   pos         Position in image of start of data vector
//
{

// Make abcissa and labels

   static Bool allSubsequent = False;
   static Vector<Int> window(2);
   static Int nPts = 0;

   Vector<T> abcissa;
   makeAbcissa (abcissa, data.nelements());
   String xLabel;
   if (momAxisType.empty())
      xLabel = "x (pixels)";
   else 
      xLabel = momAxisType + " (pixels)";
   String yLabel("Intensity");
   String title;
   setPosLabel (title, pos);


// Set up pointer to the smoothed or unsmoothed data

   const Vector<T>* pData;
   if (pSmoothedData == 0) {
      pData = &data;
   } else {
      pData = pSmoothedData;
   }


   if (doAuto_p) {

// Define the window automatically

      Vector<T> gaussPars;
      getAutoWindow (window,  abcissa, *pData, doPlot,
                     xLabel, yLabel, title, ks);
   } else {

// Define the window interactively, unless the user has told us when
// doing the previous spectrum that they wish to apply that window
// to all subsequent ones


      if (!doFit_p && !allSubsequent) {
         os_p << LogIO::NORMAL << LogIO::POST;
         os_p << LogIO::NORMAL << "Mark extremum (left), redo (middle), reject (right), all subsequent (S)" << LogIO::POST;
      }

      if (!allSubsequent) {
         getInterWindow (allSubsequent, window, abcissa, *pData,
                         xLabel, yLabel, title);
      } else if (nPts != 0) {
         cpgpage();
         drawLine (abcissa, *pData, xLabel, yLabel, title);
         drawWindow (window);
      }
   }
   

// If no points make moments zero. Blank at a later date.

   nPts = window(1) - window(0) + 1;
   if (nPts==0) {
      calcMoments = 0.0;
      return;
   }


// Assign array for median.  

   Vector<T> medianArray(nPts);


// Compute moments

   Double s0  = 0.0;
   Double s0Sq = 0.0;
   Double s1  = 0.0;
   Double s2  = 0.0;
   Int iMin = -1;
   Int iMax = -1;
   Double dMin =  1.0e30;
   Double dMax = -1.0e30;
   Double coord;

   for (Int i=window(0); i<=window(1); i++) {
      coord = getMomentCoord (Double(i));
      accumSums (s0, s0Sq, s1, s2, iMin, iMax, 
                 dMin, dMax, i, data(i), coord);
                 medianArray(i-window(0)) = data(i);
   }

// Medians

   T dMedian = 0.0;
   if (doMedianI) dMedian = median(medianArray.ac());

   T vMedian = 0.0;

// Absolute deviations of I from mean.  Requires a second pass

   Double sumAbsDev = 0.0;
   if (doAbsDev) {
      Double iMean = s0 / nPts;
      for (Int i=window(0); i<=window(1); i++) sumAbsDev += abs(Double(data(i) - iMean));
    }


// Fill moments array

   setCalcMoments (calcMoments, dMedian, vMedian, nPts, s0, s1, s2, s0Sq, sumAbsDev, 
                   dMin, dMax, iMin, iMax);
  
}


template <class T> 
void ImageMoments<T>::drawHistogram (const T& dMin,
                                     const Int& nBins,
                                     const T& binWidth,
                                     const Vector<T>& y)
//
// Draw a histogram on the current window
//
{ 
   cpgbox ("BCNST", 0.0, 0, "BCNST", 0.0, 0);
   cpglab ("Intensity", "Number", "");

   float width = float(binWidth)/2.0;
   float centre = float(dMin) + width;
   for (Int i=0; i<nBins; i++) {
      float xx = centre - width;
      float yy = float(y(i));
      cpgmove (xx, 0.0);
      cpgdraw (xx, yy);
      cpgmove (xx, yy);

      xx = centre + width;
      cpgdraw (xx, yy);
      cpgmove (xx, yy);
      cpgdraw (xx, 0.0);
      
      centre += binWidth;
   }                     
}


template <class T> 
void ImageMoments<T>::drawLoc (const T& loc,
                               const T& yMin,
                               const T& yMax)
{
// If the colour index is zero, we are trying to rub something
// out, so don't monkey with the ci then

   Int ci;
   cpgqci (&ci);
   if (ci!=0) cpgsci (3);

   cpgmove (float(loc), float(yMin));
   cpgdraw (float(loc), float(yMax));
   cpgupdt();
   cpgsci (ci);
}


template <class T> 
void ImageMoments<T>::drawLine (const Vector<T>& x,
                                const Vector<T>& y)
//
// Draw  a spectrum on the current panel
// with the box already drawn
//
{
// Copy from templated floating type to float

   int n = x.nelements();
   float* pX = new float[n];
   float* pY = new float[n];
   for (Int i=0; i<n; i++) {
      pX[i] = float(x(i));
      pY[i] = float(y(i));
   }
   cpgline (n, pX, pY);
   cpgupdt ();
   delete [] pX;
   delete [] pY;
}


template <class T> 
void ImageMoments<T>::drawLine (const Vector<T>& x,
                                const Vector<T>& y,
                                const String& xLabel,
                                const String& yLabel,
                                const String& title)
//
// Draw and label a spectrum on the current panel
//
{
// Find extrema

   Int nPts = x.nelements();
   Float xMin = 0.0;
   Float xMax = Float(nPts);
   T yMin, yMax;
   minMax(yMin, yMax, y.ac());
   Float yMinF = Float(yMin);
   Float yMaxF = Float(yMax);
   ImageUtilities::stretchMinMax (xMin, xMax);
   ImageUtilities::stretchMinMax (yMinF, yMaxF);


// Plot

   cpgswin (float(xMin), float(xMax), yMinF, yMaxF);
   cpgbox ("BCNST", 0.0, 0, "BCNST", 0.0, 0);
   drawLine (x, y);
   cpglab (xLabel.chars(), yLabel.chars(), "");
   cpgmtxt ("T", 1.0, 0.5, 0.5, title.chars());
}


template <class T> 
void ImageMoments<T>::drawMeanSigma (const T& dMean,
                                     const T& dSigma)
//
// Draw a horizontal line on the spectrum plot at
// the mean value, and 2 horizontal lines at
// mean +/- sigma
//
{
   float xMin, xMax, yMin, yMax; 
   cpgqwin (&xMin, &xMax, &yMin, &yMax);

   cpgsci (5);
   cpgmove (xMin, float(dMean));
   cpgdraw (xMax, float(dMean));
   cpgmove (xMin, float(dMean+dSigma));      
   cpgdraw (xMax, float(dMean+dSigma));      
   cpgmove (xMin, float(dMean-dSigma));      
   cpgdraw (xMax, float(dMean-dSigma));      
   cpgsci (1);
}


template <class T> 
void ImageMoments<T>::drawWindow (const Vector<Int>& window)
//
// Mark the current window on the plot
//
{  
   float x1, x2, y1, y2;
   cpgqwin (&x1, &x2, &y1, &y2);
   drawLoc (float(window(0)), y1, y2);
   drawLoc (float(window(1)), y1, y2);
}


template <class T> 
Int ImageMoments<T>::findSpectralAxis (const ImageInterface<T>* pImage) 
//
// Return the index of the first axis in an image which is spectral
//   
// Input:
//   pImage   Pointer to image
// Output:
//   the axis number by value.  -1 if there isn't one.
//
{
 
   for (Int i=0; i<pImage->shape().nelements(); i++) {
      String tString = pImage->coordinates().worldAxisNames()(i);
      tString.upcase();
      if (tString.contains("FREQ") || tString.contains("VELO")) return i;
   }
   return -1;
}


template <class T> 
Bool ImageMoments<T>::fitGaussian (T& peak,
                                   T& pos,
                                   T& width, 
                                   T& level,
                                   const Vector<T>& x,
                                   const Vector<T>& y,
                                   const T& peakGuess,
                                   const T& posGuess,
                                   const T& widthGuess,
                                   const T& levelGuess)
//
// Fit Gaussian pos * exp(-4ln2*(x-pos)**2/width**2)
// width = fwhm
//
{

// Create fitter

   NonLinearFitLM<T> fitter;


// Create and set the functionals

   Gaussian1D<AutoDiff<T> > gauss;
   Polynomial<AutoDiff<T> > poly;
   SumFunction<AutoDiff<T>,AutoDiff<T> > func;
   func.addFunction(gauss);
   func.addFunction(poly);

   FuncWithAutoDerivs<T,T> autoFunc(func);
   fitter.setFunction(autoFunc);

//   Gaussian1D<AutoDiff<T> > gauss;
//   fitter.setFunction(gauss);



// Initial guess


   Vector<T> v(4);

   v(0) = peakGuess;             // peak
   v(1) = posGuess;              // position
   v(2) = widthGuess;            // width
   v(3) = levelGuess;            // level

   fitter.setFittedFuncParams(v);

      
// Set maximum number of iterations to 50.  Default is 10  

   fitter.setMaxIter(50);
     

// Set converge criteria.  

   T tol = 0.001;
   fitter.setCriteria(tol);


// perform fit
 
   Vector<T> resultSigma(x.nelements());
   resultSigma = 1;
   Vector<T> solution = fitter.fit(x, y, resultSigma);

// Return values of fit 

//   cout << "SOlution = " << solution.ac() << LogIO::POST;

   peak  = solution(0);
   pos   = solution(1);
   width = abs(solution(2));
   level = solution(3);   


// Return status
    
   return fitter.converged();

}


template <class T> 
void ImageMoments<T>::getAutoGaussianGuess (T& peakGuess, 
                                            T& posGuess,
                                            T& widthGuess,
                                            const Vector<T>& x,
                                            const Vector<T>& y)
//
// Make a wild stab in the dark as to what the Gaussian
// parameters of this spectrum might be
//
{

// Find peak and position of peak

   IPosition minPos(1);
   IPosition maxPos(1);
   T dMin, dMax;
   minMax(dMin, dMax, minPos, maxPos, y.ac());

   posGuess = x(maxPos(0));
   peakGuess = dMax;

// Nothing much is very robust.  Assume the line is reasonably
// sampled and set its width to a few pixels.  Totally ridiculous.

   widthGuess = 5;

//   cout << "Guess: peak,pos,width=" << peakGuess << ", " << posGuess << "," <<
//           widthGuess << LogIO::POST;
  
}


template <class T> 
Bool ImageMoments<T>::getAutoGaussianFit (Vector<T>& gaussPars,
                                          const Vector<T>& x,
                                          const Vector<T>& y,
                                          const Bool& doPlot,
                                          const String& xLabel,
                                          const String& yLabel,
                                          const String& title)
//
// Automatically fit a Gaussian and return the Gaussian parameters.
// If a plotting device is active, we also plot the spectra and fits
//
// Inputs:
//   x,y        Vector containing the data
//   doPlot     Plot spectrum and optionally the  window
//   x,yLabel   Labels
//   title
// Output:
//   gaussPars  The gaussian parameters, peak, pos, fwhm
//   Bool       If False then this spectrum has been rejected (all
//              noise, failed fit)
//
{


// Plot spectrum if desired

   if (doPlot) {
      cpgpage();
      drawLine (x, y, xLabel, yLabel, title);
   }


// See if this spectrum is all noise first.  If so, forget it.

   T dMean;
   Bool noisy = allNoise(dMean, y.ac());

// Draw on mean and sigma

   T sigma = stdDeviation_p;
   if (doPlot) {
      drawMeanSigma (dMean, sigma);
      if (noisy) cpgmtxt ("T", 1.0, 0.0, 0.0, "NOISE");
   }
   if (noisy) {
      gaussPars = 0;
      return False;
   }


// Work out guesses for Gaussian

   T peakGuess, posGuess, widthGuess, levelGuess;
   T pos, width, peak, level;
   getAutoGaussianGuess (peakGuess, posGuess, widthGuess, x, y);
   levelGuess = mean(y.ac());
   peakGuess = peakGuess - levelGuess;

// Fit gaussian. Do it twice.

   if (!fitGaussian (peak, pos, width, level, x, y, peakGuess, posGuess, 
                     widthGuess, levelGuess)) {
      gaussPars = 0;
      return False;
   }
   gaussPars(0) = peak;
   gaussPars(1) = pos;
   gaussPars(2) = width;
   gaussPars(3) = level;


// Plot the fit

   if (doPlot) showGaussFit (peak, pos, width, level, x, y);

}




template <class T> 
void ImageMoments<T>::getAutoWindow (Vector<Int>& window,
                                     const Vector<T>& x,
                                     const Vector<T>& y,
                                     const Bool& doPlot,
                                     const String& xLabel,
                                     const String& yLabel,
                                     const String& title, const Double& ks)
//
// Automatically fit a Gaussian and return the +/- 3-sigma window or
// invoke Bosma's method to set a window.  If a plotting device is 
// active, we also plot the spectra and fits
//
// Inputs:
//   x,y        Spectrum
//   doPlot     Plot spectrum and optionally the  window
//   x,yLabel   x label for plots
//   title
// Output:
//   window     The window (pixels).  If both 0,  then discard this spectrum 
//              and blank moments
//
{
   if (doFit_p) {
      Vector<T> gaussPars(4);
      if (!getAutoGaussianFit (gaussPars, x, y, doPlot, xLabel, yLabel, title)) {
         window = 0;
         return;
      } else {

// Set 3-sigma limits.  

         if (!setNSigmaWindow (window, gaussPars(1), gaussPars(2), 
                               y.nelements(), 3)) {
            window = 0;
            return;
         }
      }
   } else {

// Invoke Albert's method (see AJ, 86, 1791)

      if (!getBosmaWindow (window, x, y, doPlot, xLabel, yLabel, title, ks)) {
         window = 0;
         return;
      }
   }

// Plot window if desired

   if (doPlot) drawWindow (window);

}

template <class T> 
Double ImageMoments<T>::getMomentCoord (const Double& momentPixel)
//
// Find the value of the world coordinate on the moment axis
// for the given moment axis pixel value.  The pixel coordinate
// for the other axes are input already set 
{
   pixelIn_p(momentAxis_p) = momentPixel;
   Bool ok = pInImage_p->coordinates().toWorld(worldOut_p, pixelIn_p);
   return worldOut_p(momentAxis_p);
}


template <class T> 
Double ImageMoments<T>::getErfC (Double& x)
//
// Find 1-erf(x) using the Chebyshev polynomial
// fit in Numerical recipes p 164
//
{
  Double z = abs(x);
  Double t = 1.0 / (1+z/2.0);
  Double v = t * exp(-z*z-1.26551223 + t*(1.00002368+t*(0.37409196+
            t*(0.09678418+t*(-0.18628806+t*(0.27886807+t*(-1.13520398+
            t*(1.48851587+t*(-0.82215223+t*0.17087277)))))))));
 
  if (x < 0) v = 2.0-v;
  return v;
}


template <class T> 
Double ImageMoments<T>::getGaussianCumulativeProb (const Double& sigma, 
                                                   const Double& x)
//
// Evaluate the cumulative probability function for
// a zero mean Gaussian
//
// Inputs
//   sigma    width of Gaussian (exp(-x**2/2/sigma**2)
//   x        location to evaluate it for
// Output:
//   Double   the value
{
   Double z = -x / sigma * C::_1_sqrt2;
   return getErfC(z) / 2.0;
}



template <class T> 
void ImageMoments<T>::getInterDirectWindow (Bool& allSubsequent,
                                            Vector<Int>& window,
                                            const Vector<T>& x,
                                            const Vector<T>& y,
                                            const String& xLabel,
                                            const String& yLabel,
                                            const String& title)
//
// With the cursor, mark the range_p for the window method
//
// Outputs:
//  window    The window (pixels)
//
{

// First plot the spectrum

   cpgpage();
   drawLine (x, y, xLabel, yLabel, title);

// Try and get a decent range_p from user

   float xMin, xMax, yMin, yMax;
   cpgqwin (&xMin, &xMax, &yMin, &yMax);
   Bool more = True;
   Bool ditch, redo;
   Int nPts = y.nelements();
   T tX, tY1, tY2;

   while (more) {

// Get and draw first location

      Bool final = False;
      T x1 = nPts/2;
      allSubsequent = True;
      while (!getLoc(x1, allSubsequent, ditch, redo, final)) {};
      if (ditch) {
         window = 0;
         return;
      }
   
      if (!redo) {
         window(0) = max(0,Int(x1+0.5)); 
         tX = window(0);
         tY1 = yMin;
         tY2 = yMax;
         drawLoc (tX, tY1, tY2);


// Get and draw second location

         T x2 = Float(window(0));
         final = True;
         allSubsequent = True;
         while (!getLoc(x2, allSubsequent, ditch, redo, final)) {};
         if (ditch) {
            window = 0;
            return;
         } else if (redo) {
            cpgeras();
            drawLine (x, y, xLabel, yLabel, title);
         } else {
            window(1) = min(nPts-1,Int(x2+0.5));
            tX = window(1);
            drawLoc (tX, tY1, tY2);

// Set window

            Int iTemp = window(0);
            window(0) = min(iTemp, window(1));
            window(1) = max(iTemp, window(1));


// If they stuffed it up, have another go.  Erase the line and redraw 
// the spectrum segment

            if (window(0) == window(1)) {
               os_p << LogIO::NORMAL << "Degenerate window, try again" << LogIO::POST;
               cpgeras();
               drawLine (x, y, xLabel, yLabel, title);
            } else
               more = False;
         }
      }
   }
}


template <class T> 
void ImageMoments<T>::getInterGaussianGuess  (T& peakGuess, 
                                              T& posGuess,
                                              T& widthGuess, 
                                              Vector<Int>& window,
                                              Bool& reject,
                                              const Int& nPts)
//
// Use the cursor to get the user's guess for the
// Gaussian peak, position and width (fwhm)
// and fitting window
//
{
   os_p << LogIO::NORMAL << "Mark the location of the peak and position" << LogIO::POST;
   os_p << LogIO::NORMAL << "Press right button to reject spectrum" << LogIO::POST;

   float x1, x2, y1, y2;
   cpgqwin (&x1, &x2, &y1, &y2);


// Peak/pos first

   char ch;
   static float x = (x1+x2)/2;
   static float y = (y1+y2)/2;
   Bool miss=True;
   while (miss) {
     cpgcurs (&x, &y, &ch);
     miss = Bool(x<x1 || x>x2 || y<y1 || y>y2);
     if (miss) os_p << LogIO::NORMAL << "Cursor off image" << LogIO::POST;
   }
   String str(ch);
   str.upcase();
   reject = False;

   if (str == "X") {
     os_p << LogIO::NORMAL << "Rejecting spectrum" << LogIO::POST;
     reject = True;
     return;
   }

   cpgsci(3);
   cpgpt (1, &x, &y, 2);
   cpgupdt ();
   cpgsci (1);
   posGuess = x;
   peakGuess = y;


// Now FWHM
   
   os_p << LogIO::NORMAL << LogIO::POST;
   os_p << LogIO::NORMAL << "Mark the location of the FWHM" << LogIO::POST;
   os_p << LogIO::NORMAL << "Press right button to reject spectrum" << LogIO::POST;
   miss = True;
   while (miss) {
     cpgcurs (&x, &y, &ch);
     miss = Bool(x<x1 || x>x2 || y<y1 || y>y2);
     if (miss) os_p << LogIO::NORMAL << "Cursor off image" << LogIO::POST;
   }
   str = ch;
   str.upcase();
   if (str == "X") {
     os_p << LogIO::NORMAL << "Rejecting spectrum" << LogIO::POST;
     reject = True;
   }  
   cpgsci (3);
   cpgpt (1, &x, &y, 2);
   cpgsci (1);
   y = float(peakGuess)/2;
   cpgupdt ();
   widthGuess = 2*abs(posGuess-x);
  

// Now window

   os_p << LogIO::NORMAL << LogIO::POST;
   os_p << LogIO::NORMAL << "Mark the location of the fit window" << LogIO::POST;
   os_p << LogIO::NORMAL << "Press right button to reject spectrum" << LogIO::POST;
   os_p << LogIO::NORMAL << "Press middle button to fit the whole spectrum" << LogIO::POST;
   miss=True;
   while (miss) {
     cpgcurs (&x, &y, &ch);
     miss = Bool(x<x1 || x>x2 || y<y1 || y>y2);
     if (miss) os_p << LogIO::NORMAL << "Cursor off image" << LogIO::POST;
   }
   str = ch;
   str.upcase();
   if (str == "X") {
     os_p << LogIO::NORMAL << "Rejecting spectrum" << LogIO::POST;
     reject = True;
     return;
   } else if (str == "D") {
     os_p << LogIO::NORMAL << "Fit to entire spectrum" << LogIO::POST;
     window(0) = 0;
     window(1) = nPts-1;
     return;
   }
   T tX = x;
   T tY1 = y1;
   T tY2 = y2;
   drawLoc (tX, tY1, tY2);
   window(0) = Int(x+0.5);

   miss = True;
   while (miss) {
     cpgcurs (&x, &y, &ch);
     miss = Bool(x<x1 || x>x2 || y<y1 || y>y2);
     if (miss) os_p << LogIO::NORMAL << "Cursor off image" << LogIO::POST;
   }
   str.upcase();
   if (str == "X") {
     os_p << LogIO::NORMAL << "Rejecting spectrum" << LogIO::POST;
     reject = True;
     return;
   } else if (str == "D") {
     os_p << LogIO::NORMAL << "Fit to entire spectrum" << LogIO::POST;
     window(0) = 0;
     window(1) = nPts-1;
     return;
   }
   tX = x;
   tY1 = y1;
   tY2 = y2;
   drawLoc (tX, tY1, tY2);
   window(1) = Int(x+0.5);  
   Int iTemp = window(0);
   window(0) = min(iTemp, window(1));
   window(1) = max(iTemp, window(1));
   window(0) = max(0,window(0));
   window(1) = min(nPts-1,window(1));

   cpgsci(1);
//   cout << "Guess:peak,pos,width=" << peakGuess << "," << posGuess << "," << widthGuess << LogIO::POST;
}



template <class T> 
Bool ImageMoments<T>::getInterGaussianFit (Vector<T>& gaussPars,
                                           const Vector<T>& x,
                                           const Vector<T>& y,
                                           const String& xLabel,
                                           const String& yLabel,
                                           const String& title)
//
// With the cursor, define a guess for a Gaussian fit,
// and do the fit over and over until they are happy. 
// Then return the Gaussian parameters.
//
// Inputs:
//   x,y       The abcissa and spectrum
//   x,yLabel  Labels
//   title     Title of plot
// Outputs:
//   gaussPars The gaussian parameters (peak, pos, width, level)
//   Bool      True if all successful, False if spectrum rejected
//
{

// First draw the spectrum

   cpgpage();
   drawLine (x, y, xLabel, yLabel, title);

// Get users guess and fit until satisfied

   Bool more = True;
   Bool ditch, redo;
   Vector<Int> window(2);
   os_p << LogIO::NORMAL << LogIO::POST;

   while (more) {

// Get users guess for position, peak and width

      T peakGuess, posGuess, widthGuess, levelGuess, level;
      Bool reject;
      getInterGaussianGuess (peakGuess, posGuess, widthGuess, window, 
                             reject, y.nelements());
      if (reject) {
         gaussPars = 0;
         return False;
      }


// Get guess for level and adjust peak

      levelGuess = mean(y.ac());
      peakGuess = peakGuess - levelGuess;


// Fit a Gaussian

      Int n = window(1) - window(0) + 1;
      Vector<T> xFit(n);
      Vector<T> yFit(n);
      for (Int i=0; i<n; i++) {
         xFit(i) = x(i+window(0));
         yFit(i) = y(i+window(0));
      }
      T pos, width, peak;
      if (fitGaussian (peak, pos, width, level, xFit, yFit, peakGuess,
                       posGuess, widthGuess, levelGuess)) {

// Show fit 
      
         showGaussFit (peak, pos, width, level, x, y);
      } else {
         os_p << LogIO::NORMAL << "Fit did not converge" << LogIO::POST;
      }


// Are we happy ?

      os_p << LogIO::NORMAL << "Accept (left),  redo (middle), reject (right)" << LogIO::POST;
      getButton(ditch, redo);
      if (ditch) {
         os_p << LogIO::NORMAL << "Rejecting spectrum" << LogIO::POST;
         gaussPars = 0;
         return False;
      } else if (redo) {

// Redraw spectrum

         cpgeras();
         drawLine (x, y, xLabel, yLabel, title);
      } else {

// OK, set parameters of fit

         more = False;
         gaussPars(0) = peak;
         gaussPars(1) = pos;
         gaussPars(2) = width;
         gaussPars(3) = level;
         return True;
      }
   }
}


template <class T> 
void ImageMoments<T>::getInterWindow (Bool& allSubsequent,
                                      Vector<Int>& window,
                                      const Vector<T>& x,
                                      const Vector<T>& y,
                                      const String& xLabel,
                                      const String& yLabel,
                                      const String& title)
//
// Interactively select the moment window by fitting a Gaussian 
// or directly setting the window with the cursor.
//
// Inputs:
//   x,y        Spectrum
//   x,yLabel   Labels for plots
//   title
// Output:
//   window     Include pixels in this range of indices.  If both 0,
//              then discard this spectrum and blank moments
//   allSubsequent
//              If True, then the user has instructed that
//              all subsequent spectra are to use this window
//              and we are to stop the interactive plotting
{
   if (doFit_p) {


// We interactively fit a Gaussian and choose +/- 3 sigma limits as the range

      Vector<T> gaussPars(4);
      if (!getInterGaussianFit (gaussPars, x, y, xLabel, yLabel, title)) {
         window = 0;
         return;
      } else {

// Set 3-sigma range

         if (!setNSigmaWindow (window, gaussPars(1), gaussPars(2), y.nelements(), 3)) {
            os_p << LogIO::NORMAL << "Window too small for this spectrum" << LogIO::POST;
            window = 0;
            return;
         }

// Mark window on plot

         cpgeras ();
         drawLine (x, y, xLabel, yLabel, title);
         drawWindow (window);
      }
      allSubsequent = False;
   } else {

// The user just marks the range with the cursor

      getInterDirectWindow (allSubsequent, window, x, y, xLabel, yLabel, title);
   }

   return;
}



template <class T> 
Double ImageMoments<T>::getKSProbability (const uInt& n,
                                          const Double& D)
//
// Get the probability that D > the observed value
// for the Kolmogorov-Smirnov test (see Numerical
// Recipes p 474)
//
// Inputs:
//   n       Number of points
//   D       The D statistic
// Outputs:
//   Double  The probability. 0 means that the
//           distributions being compared are different
//           1 means they are the same
//
{
   const Int itMax = 50;
   const Double alpha = 0.001;
   const Double beta  = 1e-8;
   Double lamFac = Double(-2) * Double(n) * D * D;
   Double sum = 0.0;
   Double term, oldTerm;
   Int sign = 1;
   Bool converge = False;
   Bool first = True;
   for (Int i=0; (!converge && i<itMax); i++) {
      term = sign * exp(lamFac*(i+1)*(i+1));
      sum += term;
      if (!first && (abs(term)<alpha*oldTerm || abs(term)<= beta*sum) ) {
         converge = True;
      } else {
         sign = -sign;
         oldTerm = abs(term);
         first = False;
      }
   }

   if (!converge){
      os_p << LogIO::NORMAL << "KS probability did not converge, final difference=" << sum << LogIO::POST;
      return 0.0;
   } else {
      os_p << LogIO::NORMAL << "KS probability converged in " << i << " iterations" << LogIO::POST;
      return 2 * sum;
   }
}



template <class T> 
Bool ImageMoments<T>::getLoc (T& x,
                              Bool& allSubsequent,
                              Bool& ditch,
                              Bool& redo,
                              const Bool& final)
//
// Read the PGPLOT cursor and return its coordinates if not off the plot
// Also interpret which button was pressed
//
// Inputs:
//   final   If we are marking a window, this indicates that we are
//           trying to mark the last location, so that allSubsequent
//           might be activated.
// Input/Output:
//   x       X location of cursor.  Input value is used to position cursor
// Outputs:
//   allSubsequent
//           If True it means that whatever we have done to this spectrum, 
//           the user would like it done to all subsequent ones.
//   ditch   The user has indicated to reject this spectrum
//   redo    The user has indicated to redo whaetver it is we are doing !
//   Bool    False if cursor off the window
//
{
// Fish out window

   float xMin, xMax, yMin, yMax;
   cpgqwin (&xMin, &xMax, &yMin, &yMax);

// Position and read cursor

   float xx = float(x);
   static float yy = 0.0;
   char ch;
   cpgcurs (&xx, &yy, &ch);

// Interpret location and character

   String str = ch;
   str.upcase();
   ditch = False;
   redo = False;
   allSubsequent = False;

   if (str == "X") {
      os_p << LogIO::NORMAL << "Rejecting spectrum" << LogIO::POST;
      ditch = True;
   } else if (str == "D") {
      os_p << LogIO::NORMAL << "Redoing window for this spectrum" << LogIO::POST;     
      redo = True;
   } else {
      if (xx >= xMin && xx <= xMax) {
         x = xx;
      } else {
         os_p << LogIO::NORMAL << "Cursor out of range" << LogIO::POST;
         return False;
      }

      if (str == "S") {
         if (!final) {
            os_p << LogIO::NORMAL << 
	              "You must define both ends of the range before it can be" << endl;
            os_p   << "applied to all subsequent spectra. Enter S to define the" << endl;
            os_p   << "second extremum and indicate it will be used for all " << endl;
            os_p   << "subsequent spectra" << LogIO::POST;
            return False;
         } else {
            os_p << LogIO::NORMAL << "All subsequent spectra will use this window" << LogIO::POST;
            allSubsequent = True;
         }
      }
   }
   return True;
}



template <class T> 
Bool ImageMoments<T>::getLoc (T& x,
                              T& y)
//
// Read the PGPLOT cursor and return its coordinates if not off the plot
//
{
// Fish out window

   float xMin, xMax, yMin, yMax;
   cpgqwin (&xMin, &xMax, &yMin, &yMax);

// Position and read cursor

   float xx = float(x);
   float yy = float(y);
   char ch;
   cpgcurs (&xx, &yy, &ch);

   if (xx >= xMin && xx <= xMax && yy >= yMin && yy <= yMax) {
         x = xx;
         y = yy;
   } else {
      os_p << LogIO::NORMAL << "Cursor out of range" << LogIO::POST;
      return False;
   }
   return True;
}



template <class T> 
void ImageMoments<T>::getButton (Bool& ditch,
                                 Bool& redo)
//
// Read the PGPLOT cursor and interpret the button
// pushed
//
{
// Fish out window

   float xMin, xMax, yMin, yMax;
   cpgqwin (&xMin, &xMax, &yMin, &yMax);

   float x = (xMin+xMax)/2;
   float y = (yMin+yMax)/2;
   char ch;
   cpgcurs (&x, &y, &ch);

   String str = ch;
   str.upcase();
   ditch = False;
   redo = False;
   if (str == "X") 
      ditch = True;
   else if (str == "D") 
      redo = True;

}



template <class T> 
void ImageMoments<T>::makeAbcissa (Vector<T>& x, 
                                   const Int& n)
{
   x.resize(n);
   for (Int i=0; i<n; i++) x(i) = i;
}


template <class T> 
Bool ImageMoments<T>::makeOdd (Int& i)
{
   Int j = i / 2;
   if (2*j == i) {
      i++;
      return True;
   }
   return False;
}

typedef ArrayLattice<Bool> gpp_ArrayLatticeBool;
template <class T>
void ImageMoments<T>::makeMask (gpp_ArrayLatticeBool* pMask,
                                const ArrayLattice<T>* pSmoothedImage)
//
// Generate a mask which signals bad values when the
// smoothed image is outside of a given pixel range
//
// Direct pixel access of lattices is pretty slow.  I should
// do something cleverer somewhen as pMask is a lattice  Eventually
// smoothedImage will be too and then I will really need to fix it up.
// This won't be too slow at the moment assuming the number of
// False mask pixels is small.
//
//
// Inputs:
//   pSmoothedImage
//             Pointer to the smoothed image
// Input/Output:
//   pMask     Pointer to mask lattice.  All true on input, so just set 
//             the bad ones
{

// Construct conformant vector iterators

   IPosition shape(pSmoothedImage->ndim(),1);
   Int n1 = pSmoothedImage->shape()(0);
   shape(0) = n1;

   RO_LatticeIterator<T> imageIt(*pSmoothedImage, shape);
   LatticeIterator<Bool>  maskIt(*pMask, shape);
   T datum;

// Iterate and create mask
  
   if (!noInclude_p) {
      while (!imageIt.atEnd()) {      
         Int orig = imageIt.vectorCursor().origin()(0);
         for (Int i=0; i<n1; i++) {
            datum = imageIt.vectorCursor()(i+orig);
            if (Float(datum) < range_p(0) || Float(datum) > range_p(1)) 
               maskIt.vectorCursor()(i+orig) = False;
         }
         imageIt++;
         maskIt++;
      }
   } else {
      while (!imageIt.atEnd()) {      
         Int orig = imageIt.vectorCursor().origin()(0);
         for (Int i=0; i<n1; i++) {
            datum = imageIt.vectorCursor()(i+orig);
            if (Float(datum) >= range_p(0) && Float(datum) <= range_p(1))
               maskIt.vectorCursor()(i+orig) = False;
         }
         imageIt++;
         maskIt++;
      }
   }
}


template <class T> 
Bool ImageMoments<T>::makePSF (Array<T>& psf, 
                               const IPosition& imageShape)
//
// Generate an array containing the convolving function
//
// Private data:
//   kernelTypes_p   One of GAUSSIAN, BOXCAR, HANNING
//   kernelWidths_p  Convolving kernel width in pixels
//                      FWHM  for Gaussian
//                     Width for boxcar
//                     3     for Hanning
// Input:
//   imageShape    Shape of image to be convolved
// Output:
//   psf           The PSF to convolve by
//   Bool          True if successfull
{

   Int i, j, k;

// Find the largest axis number the user wants to smooth

   Int psfDim = max(smoothAxes_p.ac()) + 1;

// Define maximum size.  Use this constant Int to avoid brain ache
// with pointers to pointers. I must do better one day !

   const Int MAXDIM = 10;
   if (psfDim > MAXDIM) {
      os_p << LogIO::SEVERE << "Convolving kernel has too many dimensions" << LogIO::POST;
      return False;
   }
   float* pt[MAXDIM];


// Work out the size of the PSF and generate separable kernel arrays
// Make PSF array as small as possible, as there are advantages to
// this in the Convolution routines and PSF generation involving speed.  

   IPosition psfShape(psfDim);
   for (i=0,k=0; i<psfDim; i++) {

      if (ImageUtilities::inVector(i, smoothAxes_p)==-1) {

// If this axis is not in the user's list, make the shape
// of the PSF array 1

         psfShape(i) = 1;
         pt[i] = new float[1];
         pt[i][0] = 1.0;
      } else {


// Generate separable arrays for each type

         if (kernelTypes_p(k) == GAUSSIAN) { 


// Gaussian. The volume error is less than 6e-5% for +/- 5 sigma limits

            Double sigma = kernelWidths_p(k) / sqrt(Double(8.0) * C::ln2);
            psfShape(i) = (Int(5*sigma + 0.5) + 1) * 2;
            Int refPix = psfShape(i)/2;

            Double norm = 1.0 / (sigma * sqrt(2.0 * C::pi));
            Double gWidth = kernelWidths_p(i);
            Gaussian1D<Double> gauss(norm, Double(refPix), gWidth);
//            os_p << LogIO::NORMAL << "Volume = " << 1/norm << LogIO::POST;

            pt[i] = new float[psfShape(i)];
            for (j=0; j<psfShape(i); j++) pt[i][j] = gauss(Double(j));
         } else if (kernelTypes_p(k) == BOXCAR) {

// Boxcar

            Int intKernelWidth = Int(kernelWidths_p(k)+0.5);
            psfShape(i) = intKernelWidth + 1;
            Int refPix = psfShape(i)/2;

            Int iw = (intKernelWidth-1) / 2;
            pt[i] = new float[psfShape(i)];
            for (j=0; j<psfShape(i); j++) {
               if (abs(j-refPix) > iw) 
                  pt[i][j] = 0.0;
               else 
                  pt[i][j] = 1.0 / Float(intKernelWidth);
            }
         } else if (kernelTypes_p(k) == HANNING) {

// Hanning

            psfShape(i) = 4;
            pt[i] = new float[psfShape(i)];
            pt[i][0] = 0.25;
            pt[i][1] = 0.5;
            pt[i][2] = 0.25;
            pt[i][3] = 0.0;
         }
         k++;
      }
   }

//   os_p << LogIO::NORMAL << "PSF shape = " << psfShape << LogIO::POST;


// Resize PSF array

   psf.resize(psfShape);


// Set up position iterator

   ArrayPositionIterator posIterator (psf.shape(), IPosition(psfDim,0), 0);


// Iterate through PSF array and fill it with product of separable 
// convolving functions

   Float val;
   Float sum = 0.0;
   int index;
   for (posIterator.origin(); !posIterator.pastEnd(); posIterator.next()) {
      for (i=0,val=1.0; i<psfDim; i++) {
         index = posIterator.pos()(i);
         val *= pt[i][index];
      }
      psf(posIterator.pos()) = val;
      sum = sum + val;
   } 
//   os_p << LogIO::NORMAL << "Sum of PSF = " << sum << LogIO::POST;


// Delete memory

   for (i=0; i<psfDim; i++) delete [] pt[i];
   return True;
}



template <class T> 
void ImageMoments<T>::selectMoment (Bool& doMedianI, 
                                    Bool& doMedianV,
                                    Bool& doAbsDev, 
                                    String& suffix, 
                                    Int& selMom,
                                    const Int& index)
//
// Set the output image suffixes and fill the moment
// selection array according to what the user requests
//
// Input:
//   index        Array index of moments array for this moment
// Outputs:
//   doMedianI,V  The user has asked for median (I or V) moments
//   doAbsDev     The user has asked for the absolute deviation moment
//   selMom       pointer into moments array computed by the
//                doMom* functions for this moment
//   suffix       suffix for output file name
{
   if (moments_p(index) == AVERAGE) {
      suffix = "_MAverage";
      selMom = AVERAGE;
   } else if (moments_p(index) == INTEGRATED) {
      suffix = "_MIntegrated";
      selMom = INTEGRATED;
   } else if (moments_p(index) == WEIGHTED_MEAN_COORDINATE) {
      suffix = "_MWeighted_Mean_Coord";
      selMom = WEIGHTED_MEAN_COORDINATE;
   } else if (moments_p(index) == WEIGHTED_DISPERSION_COORDINATE) {
      suffix = "_MWeighted_Dispersion_Coord";
      selMom = WEIGHTED_DISPERSION_COORDINATE;
   } else if (moments_p(index) == MEDIAN) {
      suffix = "_MMedian";
      selMom = MEDIAN;
      doMedianI = True;
   } else if (moments_p(index) == STANDARD_DEVIATION) {
      suffix = "_MStandard_Deviation";
      selMom = STANDARD_DEVIATION;
   } else if (moments_p(index) == RMS) {
      suffix = "_MRms";
      selMom = RMS;
   } else if (moments_p(index) == ABS_MEAN_DEVIATION) {
      suffix = "_MAbs_Mean_Dev";
      selMom = ABS_MEAN_DEVIATION;
      doAbsDev = True;
   } else if (moments_p(index) == MAXIMUM) {
      suffix = "_MMaximum";
      selMom = MAXIMUM;
   } else if (moments_p(index) == MAXIMUM_COORDINATE) {
      suffix = "_MMaximum_Coord";
      selMom = MAXIMUM_COORDINATE;
   } else if (moments_p(index) == MINIMUM) {
      suffix = "_MMinimum";
      selMom = MINIMUM;
   } else if (moments_p(index) == MINIMUM_COORDINATE) {
      suffix = "_MMinimum_Coord";
      selMom = MINIMUM_COORDINATE;
   } else if (moments_p(index) == MEDIAN_COORDINATE) {
      suffix = "_MMedian_Coord";
      selMom = MEDIAN_COORDINATE;
      doMedianV = True;
   }
}


template <class T> 
Bool ImageMoments<T>::getBosmaWindow (Vector<Int>& window,
                                      const Vector<T>& x, 
                                      const Vector<T>& y,
                                      const Bool& doPlot,
                                      const String& xLabel,
                                      const String& yLabel,
                                      const String& title, const Double& ks)
//
// Automatically work out the spectral window
// with Albert Bosma's algorithm.
//
// Inputs:
//   x,y       Spectrum
//   doPlot    Plot device active if True
//   x,yLabel  Labels for plots
// Output:
//   window    The window
//   Bool      False if we reject this spectrum
//
{

   if (doPlot) {

// Plot spectrum

      cpgpage();
      drawLine (x, y, xLabel, yLabel, title);
   }


// See if this spectrum is all noise first.  If so, forget it.

   T dMean;
   Bool noisy1 = allNoise(dMean, y.ac());
//   Bool noisy2 = allNoise(y.ac(), ks);
//   cout << LogIO::POST << "noisy1, noisy2 =" << noisy1 << "," << noisy2 << LogIO::POST;
// Draw on mean and sigma

   T sigma = stdDeviation_p;
   if (doPlot) {
      drawMeanSigma (dMean, sigma);
      if (noisy1) cpgmtxt ("T", 1.0, 0.0, 0.0, "NOISE");
   }
   if (noisy1) {
      window = 0;
      return False;
   }


// Find peak

   Int nPts = y.nelements();
   IPosition minPos(1), maxPos(1);
   T yMin, yMax;
   minMax(yMin, yMax, minPos, maxPos, y.ac());
   Int iMin = max(0,maxPos(0)-2);
   Int iMax = min(nPts-1,maxPos(0)+2);
   Double tol = sigma / (nPts - (iMax-iMin-1));


// Iterate to convergence

   Bool first = True;
   Bool converged = False;
   Bool more = True;
   Double mean;
   Double oldMean = 0;

   while (more) {

//     os_p << LogIO::NORMAL << "iMin,iMax,oldmean,tol=" << iMin << "," << iMax << "," << oldMean << "," << tol << LogIO::POST;

// Find mean outside of peak region

      Double sum = 0.0;  
      for (Int i=0,j=0; i<nPts; i++) {
         if (i < iMin || i > iMax) {
            sum += Double(y(i));
            j++;
         }
      }
      if (j>0) mean = sum / Double(j);
      

// Interpret result

      if (!first && j>0 && abs(mean-oldMean) < tol) {
         converged = True;
         more = False;
      } else if (iMin==0 && iMax==nPts-1) 
         more = False;
      else {

// Widen window and redetermine tolerance

         oldMean = mean; 
         iMin = max(0,iMin - 2);
         iMax = min(nPts-1,iMax+2);
         tol = sigma / (nPts - (iMax-iMin-1));
      }
      first = False;
   }

// Return window

   if (converged) {
      window(0) = iMin;
      window(1) = iMax;
      return True;
   } else {
      window = 0;
      return False;
   }
}


template <class T> 
void ImageMoments<T>::saveLattice (const Lattice<T>* pLattice,
                                   const CoordinateSystem& coordinate,
                                   const String& fileName)
//
// Save a Lattice to disk as a Paged Image
//
// Inputs:
//  fileName    name of disk file
//  coordinate  Coordinate to construct image with
//  pLattice    Pointer to Lattice
{

// Set up coordinates

   CoordinateSystem outCoord(coordinate);

// Create output image

   IPosition outShape(pLattice->shape());
   PagedImage<T> outImage(outShape, outCoord, fileName);


// Construct iterator to iterate through in optimal tiles

   IPosition shape(outImage.niceCursorShape(outImage.maxPixels())); 
   RO_LatticeIterator<T> inImageIterator(*pLattice, shape);
   LatticeIterator<T>   outImageIterator(outImage, shape);


// Save

   while (!inImageIterator.atEnd()) {
      outImageIterator.cursor() = inImageIterator.cursor();
      inImageIterator++;
      outImageIterator++;
   }
}



template <class T> 
Bool ImageMoments<T>::setNSigmaWindow (Vector<Int>& window,
                                       const T& pos,
                                       const T& width,
                                       const Int& nPts,
                                       const Int& N)
//
// Take the fitted Gaussian position and width and
// set an N-sigma window.  If the window is too small
// return a Fail condition.
//
// Inputs:
//   pos,width   The position and width in pixels
//   nPts        The number of points in the spectrum that was fit
//   N           The N-sigma
// Outputs:
//   window      The window in pixels
//   Bool        False if window too small to be sensible
//
{
   window(0) = Int((pos-N*width)+0.5);
   window(0) = min(nPts-1,max(0,window(0)));
   window(1) = Int((pos+N*width)+0.5);
   window(1) = min(nPts-1,max(0,window(1)));

   if ( abs(window(1)-window(0)) < 3) return False;
   return True;

}



template <class T> 
void ImageMoments<T>::setCalcMoments (Vector<T>& calcMoments,
                                      const T& dMedian,
                                      const T& vMedian,
                                      const Int& nPts,
                                      const Double& s0,
                                      const Double& s1,
                                      const Double& s2,
                                      const Double& s0Sq,
                                      const Double& sumAbsDev,
                                      const Double& dMin,
                                      const Double& dMax,
                                      const Int& iMin,
                                      const Int& iMax)

//
// Fill the moments array
//
// Outputs:
//   calcMoments The moments
//
{

// Normalize and fill moments

   calcMoments(AVERAGE) = s0 / nPts;
   calcMoments(INTEGRATED) = s0;
   calcMoments(WEIGHTED_MEAN_COORDINATE) = s1 / s0;
   calcMoments(WEIGHTED_DISPERSION_COORDINATE) = 
     (s2 / s0) - calcMoments(WEIGHTED_MEAN_COORDINATE) *
                 calcMoments(WEIGHTED_MEAN_COORDINATE);
   calcMoments(WEIGHTED_DISPERSION_COORDINATE) =
      abs(calcMoments(WEIGHTED_DISPERSION_COORDINATE));
   if (calcMoments(WEIGHTED_DISPERSION_COORDINATE) > 0.0) {
     calcMoments(WEIGHTED_DISPERSION_COORDINATE) =
        sqrt(calcMoments(WEIGHTED_DISPERSION_COORDINATE));
   }
   else {
//     cout << "m2=" << sqrt(abs(calcMoments(WEIGHTED_DISPERSION_COORDINATE))) << LogIO::POST;
     calcMoments(WEIGHTED_DISPERSION_COORDINATE) = 0.0;
   }

// Standard deviation about mean of I

   if (Float((s0Sq - s0*s0/nPts)/(nPts-1)) > 0) 
      calcMoments(STANDARD_DEVIATION) = sqrt((s0Sq - s0*s0/nPts)/(nPts-1));
   else
      calcMoments(STANDARD_DEVIATION) = 0;


// Rms of I

   calcMoments(RMS) = sqrt(s0Sq/nPts);


// Absolute mean deviation

   calcMoments(ABS_MEAN_DEVIATION) = sumAbsDev / nPts;


// Maximum value

   calcMoments(MAXIMUM) = dMax;


// Coordinate of maximum value

   calcMoments(MAXIMUM_COORDINATE) = getMomentCoord(Double(iMax));


// Minimum value

   calcMoments(MINIMUM) = dMin;


// Coordinate of minimum value

   calcMoments(MINIMUM_COORDINATE) = getMomentCoord(Double(iMin));


// Medians

   calcMoments(MEDIAN) = dMedian;
   calcMoments(MEDIAN_COORDINATE) = vMedian;
}


template <class T> 
void ImageMoments<T>::setPosLabel (String& title,
                                   const IPosition& pos)
{
   const int BUFL=64;
   char buf[BUFL];
   ostrstream oss(buf,BUFL,ios::out);

   oss << "Position = ";  
   oss << pos+1; 
   oss << ends;
   String temp(oss.str());
   title = temp;
}


template <class T> 
void ImageMoments<T>::showGaussFit   (const T& peak,
                                      const T& pos, 
                                      const T& width,
                                      const T& level,
                                      const Vector<T>& x,
                                      const Vector<T>& y)
//
// Plot the Gaussian fit and residual
//
{
   Int nDPts = x.nelements();
   T xMin = x(0);
   T xMax = x(nDPts-1);
   Int nGPts = 100;
   T dx = (xMax - xMin)/nGPts;

// Setup functional
   
   Gaussian1D<T> gauss(peak, pos, width);


// Allocate arrays

   Vector<T> xG(nGPts);
   Vector<T> yG(nGPts);


// Generate plot values

   int i = 0;
   Float xx;
   for (i=0,xx=xMin; i<nGPts; xx+=dx,i++) {
      xG(i) = xx;
      yG(i) = gauss(xx) + level;
   }
   cpgsci (7);
   drawLine (xG, yG);


// Now difference

   Vector<T> d(nDPts);
   for (i=0; i<nDPts; i++) {
      d(i) = y(i) - gauss(x(i));
   }
   cpgsci (2);
   drawLine (x, d);
   cpgsci (1);

}


template <class T> 
Bool ImageMoments<T>::smoothImage (ArrayLattice<T>* pSmoothedImage)
//
// Smooth image
//
// Input/output
//   pSmoothedImage Pointer to smoothed image
//   Bool           True for success
{

// Generate convolving function

   Array<T> psf;
   if (!makePSF(psf, pInImage_p->shape())) return False;


// Save PSF to disk
   
   if (!psfOut_p.empty()) {
      os_p << LogIO::NORMAL << "Saving PSF file" << LogIO::POST;
      ArrayLattice<T>* pPSF = new ArrayLattice<T>(psf);
      saveLattice (pPSF, pInImage_p->coordinates(), psfOut_p);
      delete pPSF;
   }


// Convolve.  The convolution classes cannot handle a kernel 
// like [nx,1,nz] or [1,1,nz] so we have to do some extra 
// iterating outside of the convolution

   IPosition cursorShape(pInImage_p->ndim(),1);
   for (Int i=0; i<min(pInImage_p->ndim(),psf.ndim()); i++) {
      if (psf.shape()(i) > 1) cursorShape(i) = pInImage_p->shape()(i);
   }
   

   RO_LatticeIterator<T> imageIterator(*pInImage_p, cursorShape);
   LatticeIterator<T> smoothedImageIterator(*pSmoothedImage, cursorShape);

   os_p << LogIO::NORMAL << "Begin convolution" << LogIO::POST;
   Convolver<T> conv(psf, cursorShape);
   Int k=0;
   while (!imageIterator.atEnd()) {
      k++;
//       os_p << LogIO::NORMAL << "Convolving iteration " << k << LogIO::POST;
      conv.linearConv(smoothedImageIterator.cursor(), imageIterator.cursor());
      imageIterator++;
      smoothedImageIterator++;
   }


// Output convolved image

   if (!smoothOut_p.empty()) {
      os_p << LogIO::NORMAL << "Saving smoothed image" << LogIO::POST;
      saveLattice (pSmoothedImage, pInImage_p->coordinates(), 
                   smoothOut_p);
   }

   return True;
}


template <class T> 
Bool ImageMoments<T>::whatIsTheNoise (Double& sigma,
                                      const Lattice<T>* pI)
//
// Determine the noise level in the image by first
// making a histogram of the image, then fitting a
// Gaussian between the 25% levels to give sigma
//
{
   os_p << LogIO::NORMAL << "First determine the noise -- requires two passes through the image" << LogIO::POST;

// Set up image iterator to read image optimally fast

   IPosition cursorShape(pI->ndim());
   cursorShape = pI->niceCursorShape(pI->maxPixels());
   RO_LatticeIterator<T> iterator(*pI, cursorShape);


// First pass to get data min and max

   Int i;
   T tMin, tMax;
   iterator++;
   minMax(tMin, tMax, iterator.cursor().ac());
   T dMin = tMin;
   T dMax = tMax;

   while (!iterator.atEnd()) {
      minMax(tMin, tMax, iterator.cursor().ac());
      dMin = min(dMin,tMin);
      dMax = max(dMax,tMax);
      iterator++;
   }  


// Second pass to make the histogram

   const Int nBins = 100;
   Vector<T> y(nBins);
   y = 0;
   T binWidth = (dMax - dMin) / nBins;

   Bool deleteIt;
   Int iBin;
   iterator.reset();
   while (!iterator.atEnd()) {
      const T* pt = iterator.cursor().getStorage(deleteIt);

      for (i=0; i<iterator.cursor().nelements(); i++) {
         iBin = min(nBins-1, Int((pt[i]-dMin)/binWidth));
         y(iBin) += 1.0;
      }

      iterator.cursor().freeStorage(pt, deleteIt);
      iterator++;
   }  


// Enter into a (plot), select window, fit cycle until content


   T xMin, xMax, yMin, yMax, x1, x2;
   IPosition yMinPos(1), yMaxPos(1);
   minMax (yMin, yMax, yMinPos, yMaxPos, y.ac());
   yMax += yMax/20.0;
   xMin = dMin - (dMax-dMin)/20.0;
   xMax = dMax + (dMax-dMin)/20.0;

   if (!device_p.empty()) {
      if (cpgbeg (0, device_p.chars(), 1, 1) != 1) return False;
      cpgswin (float(xMin), float(xMax), float(yMin), float(yMax));
   }

   Int iMin, iMax;
   Bool first = True;
   Bool more = True;
   while (more) {

// Plot histogram

      if (!device_p.empty()) {
         cpgpage();
         drawHistogram (dMin, nBins, binWidth, y);
      }

      if (first) {
         for (i=yMaxPos(0); i<nBins; i++) {
            if (y(i) < yMax/4) {
               iMax = i; 
               break;
             }      
          } 
          for (i=yMaxPos(0); i>0; i--) { 
             if (y(i) < yMax/4) {
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

         if (!device_p.empty()) {
            x1 = dMin + binWidth/2 + iMin*binWidth;
            x2 = dMin + binWidth/2 + iMax*binWidth;
            drawLoc (x1, yMin, yMax);
            drawLoc (x2, yMin, yMax);
         }
         first = False;

      } else if (!device_p.empty()) {

// We are redoing the fit so let the user mark where they think
// the window fit should be done

         x1 = (xMin+xMax)/2;
         T y1 = (yMin+yMax)/2;
         Int i1, i2;
         i1 = i2 = 0;
  
         os_p << LogIO::NORMAL << "Mark the locations for the window" << LogIO::POST;
         while (i1==i2) {
            while (!getLoc(x1, y1)) {};
            i1 = Int((x1 -dMin)/binWidth - 0.5);
            x1 = dMin + binWidth/2 + i1*binWidth;
            drawLoc (x1, yMin, yMax);

            T x2 = x1;
            while (!getLoc(x2, y1)) {};
            i2 = Int((x2 -dMin)/binWidth - 0.5);
            drawLoc (x2, yMin, yMax);

            if (i1 == i2) {
               os_p << LogIO::NORMAL << "Degenerate window, try again" << LogIO::POST;
               cpgeras ();
               drawHistogram (dMin, nBins, binWidth, y);
            }
         }


// Set window 

         iMin = min(i1, i2);
         iMax = max(i1, i2);
      }

// Now generate the distribution we want to fit.  Normalize to
// peak 1 to help fitter.  

      Int nPts2 = iMax - iMin + 1;
      Vector<T> xx(nPts2);
      Vector<T> yy(nPts2);
   
      for (i=0; i<nPts2; i++) {
         xx(i) = dMin + binWidth/2 + (i+iMin)*binWidth;
         yy(i) = y(i+iMin)/yMax;
      }


// Create fitter

      NonLinearFitLM<T> fitter;
      Gaussian1D<T> gauss;
//      FuncWithAutoDerivs<T,T> autoFunc(gauss);
      fitter.setFunction(gauss);


// Initial guess

      Vector<T> v(3);
      v(0) = 1.0;
      v(1) = dMin + binWidth/2 + yMaxPos(0)*binWidth;
      v(2) = nPts2*binWidth;


// Fit

      fitter.setFittedFuncParams(v);
      fitter.setMaxIter(50);
      T tol = 0.001;
      fitter.setCriteria(tol);

      Vector<T> resultSigma(nPts2);
      resultSigma = 1;
      Vector<T> solution = fitter.fit(xx, yy, resultSigma);
//      os_p << LogIO::NORMAL << "Solution=" << solution.ac() << LogIO::POST;


// Return values of fit 

      if (fitter.converged()) {
         sigma = abs(solution(2)) / sqrt(2.0);
         os_p << LogIO::NORMAL << "*** The standard deviation of the noise is " << sigma << LogIO::POST << LogIO::POST;

// Now plot the fit 

         if (!device_p.empty()) {
            Int nGPts = 100;
            T dx = (xMax - xMin)/nGPts;

            Gaussian1D<T> gauss(solution(0), solution(1), abs(solution(2)));
            Vector<T> xG(nGPts);
            Vector<T> yG(nGPts);

            T xx;
            for (i=0,xx=xMin; i<nGPts; xx+=dx,i++) {
               xG(i) = xx;
               yG(i) = gauss(xx) * yMax;
            }
            cpgsci (7);
            drawLine (xG, yG);
            cpgsci (1);
         }
      } else {
         os_p << LogIO::NORMAL << "The fit to determine the noise level failed." << LogIO::POST;
         os_p << LogIO::NORMAL << "Try inputting it directly" << LogIO::POST;
         if (!device_p.empty()) os_p << LogIO::NORMAL << "or try a different window " << LogIO::POST;
      }

// Another go

      if (!device_p.empty()) {
         os_p << LogIO::NORMAL << LogIO::POST << 
           "Accept (click left), redo (click middle), give up (click right)" << LogIO::POST;

         float xx = float(xMin+xMax)/2;
         float yy = float(yMin+yMax)/2;
         char ch;
         cpgcurs (&xx, &yy, &ch);

         String str = ch;
         str.upcase();
 
         if (str == "D") {
            os_p << LogIO::NORMAL << "Redoing fit" << LogIO::POST;
         } else if (str == "X")
            return False;
         else
            more = False;
      } else
         more = False;
   }
     
   if (!device_p.empty()) cpgend();
   return True;
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

   Vector<String> kernelStrings = ImageUtilities::getStrings(kernels);

// Convert strings to appropriate enumerated value
   Vector<Int> kernelTypes(kernelStrings.nelements());

   for (Int i=0; i<kernelStrings.nelements(); i++) {
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






