
//# ImageFit1D.h: Class to fit profiles to vectors from images
//# Copyright (C) 2004
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
//#   $Id$

#ifndef IMAGES_IMAGEFIT1D_H
#define IMAGES_IMAGEFIT1D_H

//# Includes
#include <casa/aips.h>
#include <casa/Arrays/Vector.h>
#include <scimath/Mathematics/NumericTraits.h>

#include <measures/Measures/MDirection.h>
#include <measures/Measures/MFrequency.h>
#include <measures/Measures/MDoppler.h>
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <components/SpectralComponents/ProfileFit1D.h>

namespace casa {

class SpectralElement;
class SpectralList;
class ImageRegion;
template<class T> class ImageInterface;


// <summary>
// Fit spectral components to a Vector of data from an image
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tImageFit1D.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="SpectralElement">SpectralElement</linkto> 
//   <li> <linkto class="SpectralList">SpectralList</linkto> 
//   <li> <linkto class="SpectralFit">SpectralFit</linkto> 
//   <li> <linkto class="ProfileFit1D">ProfileFit1D</linkto> 
// </prerequisite>

// <synopsis> 
// Fit lists (held in class SpectralList) of SpectralElements to a Vector of data 
// from the image.  Each SpectralElement can  be one from a variety of types.
// The values of the parameters for each SpectralElement provide the initial 
// starting guesses for the fitting process.  
//
// You specify the domain in which the fit is to be done via the 
// enum AbcissaType.  The CoordinateSystem in the image is used
// to convert the pixel coordinates to the desired abcissa.  
// You can change the units of the CoordinateSystem if you want
// to fit in different units.  If you set an estimate yourself
// (function setElements or addElement) it is the callers responsibility
// that the elements are in the correct abcissa domain.  Function
// setGaussianElements will automatically make an estimate in the 
// correct domain.
//
// Also, a SpectralElement object holds a mask indicating whether 
// a parameter should be held fixed or solved for.   After the 
// fitting is done, a new SpectralList holding SpectralElements with 
// the fitted parameters is created.  
//
// For all the functions that return a status Bool, True is good. If
// False is returned, an error message can be recovered with function
// <src>errorMessage</src>,  You should not proceed if False is returned.
// 
// Exceptions will be thrown if you do not set the Image and axis 
// via the constructor or <src>setImage</src> function.
// </synopsis> 

// <example>
// <srcblock>
// PagedImage<Float> im("myimage");
// Int axis = 2;
// ImageFit1D<Float> fitter(image, axis);
// IPosition pos(in.ndim(),0);
// fitter.setData(pos, ImageFit1D<Float>::IM_NATIVE);     // Fit in native coordinate space
// fitter.setGaussianElements(3);                      // FIt 3 Gaussians
// if (fitter.fit()) {
//    cerr << fitter.getList() << endl;                // Print result
// }
// 
// </srcblock>
// </example>

// <todo asof="2004/07/10">
//   <li> Add constraints
// </todo>

template <class T> class ImageFit1D 
{
public:

    enum AbcissaType {
       PIXEL = 0,
       IM_NATIVE = 1,
       VELOCITY = 2,
       N_TYPES};

    // Default Constructor (no image or axis set, so cannot be used).
    // You must call <src>setImage</src> to use this object.
    ImageFit1D();

    // Constructor.  Fitting weights are assumed all unity.
    ImageFit1D(const ImageInterface<T>& image, uInt axis=0);

    // Constructor with fitting weights image.  The data and weights images must
    // be the same shape.
    ImageFit1D(const ImageInterface<T>& image, 
               const ImageInterface<T>& weights,
               uInt axis=0);

    // Destructor
    ~ImageFit1D();

    // Copy constructor.  Uses reference semantics.
    ImageFit1D(const ImageFit1D& other);

    // Assignment operator. Uses reference semantics.
    ImageFit1D& operator=(const ImageFit1D& other);

    // Set Image(s) and axis
    // <group>
    void setImage (const ImageInterface<T>& im, const ImageInterface<T>& weights, uInt axis);
    void setImage (const ImageInterface<T>& im, uInt axis);
    // </group>

    // Set the data to be fit.  All non-profile axes data are averaged.
    // For the profile axis, the full spectrum is taken.  The abcissa
    // world values are computed when you call these functions.  The domain of the
    // abcissa values is controlled by <src>AbcissaType</src> and
    // <src>doAbs</src> (absolute coordinates).  The CoordinateSystem in
    // the image is used to convert from pixels to world values.
    // <group>
    Bool setData (const IPosition& pos, ImageFit1D<T>::AbcissaType type,
                  Bool doAbs=True);
    Bool setData (const ImageRegion& region, ImageFit1D<T>::AbcissaType type,
                  Bool doAbs=True);
    // </group>

    // Set a SpectralList of SpectralElements to fit for.    These elements
    // must be in the correct abcissa domain set in function <src>setData</src>.
    // You must have already called <src>setData</src> to call this function.
    // The SpectralElements in the list hold the
    // initial estimates.  They also contain the information about whether
    // specific parameters are to be held fixed or allowed to vary in
    // the fitting process.
    // You can recover the list of elements with function getList.
    void setElements (const SpectralList& list) {itsFitter.setElements(list);};

    // Add new SpectralElement(s) to the SpectralList (can be empty)
    // of SpectralElements to be fit for.  
    // You must have already called <src>setData</src> to call this function.
    //<group>
    void addElement (const SpectralElement& el) {itsFitter.addElement(el);};
    void addElements (const SpectralList& list) {itsFitter.addElements(list);};
    // </group>

    // Set a SpectralList of Gaussian SpectralElements to fit for.  
    // The initial estimates for the Gaussians will be automatically determined
    // in the correct abcissa domain.
    // All of the parameters created by this function will be solved for
    // by default. You can recover the list of elements with function getList.
    // Status is returned, if False, error message can be recovered with <src>errorMessage</src>
    Bool setGaussianElements (uInt nGauss);

    // Clear the SpectralList of elements to be fit for
    void clearList () {itsFitter.clearList();};

    // Do the fit and return convergence status.  Errors in the fitting
    // process will generate an AipsError exception and you should catch
    // these yourself.
    Bool fit ();

    // Get Chi Squared of fit
    Double getChiSquared () const {return itsFitter.getChiSquared();}

    // Get number of iterations for last fit
    Double getNumberIterations () const {return itsFitter.getNumberIterations();}

    // Recover the list of elements.  You can get the elements
    // as initially estimated (fit=False), or after fitting 
    // (fit=True).  In the latter case, the SpectralElements
    // hold the parameters and errors of the fit.
    const SpectralList& getList (Bool fit=True) const {return itsFitter.getList(fit);};

    // Recover vectors for the estimate, fit and residual.
    // If you don't specify which element, all elements are included
    // If the Vectors are returned with zero length, it means an error
    // condition exists (e.g. asking for fit before you do one). In this
    // case an error message can be recovered with function <src>errorMessage</src>.
    //<group>
    Vector<T> getEstimate (Int which=-1) const;
    Vector<T> getFit (Int which=-1) const;
    Vector<T> getResidual (Int which=-1, Bool fit=True)  const;
    //</group>

    // Get Total Mask (data and range mask)
    Vector<Bool> getTotalMask () const {return itsFitter.getTotalMask();};

    // Recover the error message
    String errorMessage () const {return itsError;};

    // Helper function.  Sets up the CoordinateSystem to reflect the choice of
    // abcissa unit and the doppler (if the axis is spectral).
    static Bool setAbcissaState (String& errMsg, ImageFit1D<T>::AbcissaType& type,
                                 CoordinateSystem& cSys, const String& xUnit,
                                 const String& doppler, uInt pixelAxis);
private:
   ImageInterface<T>* itsImagePtr;
   ImageInterface<T>* itsWeightPtr;
   uInt itsAxis;

// In the future I will be able to template the fitter on T. For now
// it must be Double.

   typedef typename NumericTraits<T>::PrecisionType FitterType;
   ProfileFit1D<FitterType> itsFitter;
   CoordinateSystem itsCS;
//
   mutable String itsError;                // Error message

// Functions
   
   void check() const;
   void checkType() const;
   void copy (const ImageFit1D<T>& other);
   Bool makeAbcissa (Vector<Double>& x, ImageFit1D<T>::AbcissaType type, Bool doAbs);
   void setWeightsImage (const ImageInterface<T>& im);

};

} //#End casa namespace

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <images/Images/ImageFit1D.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
