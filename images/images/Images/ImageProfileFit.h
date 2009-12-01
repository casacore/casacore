//# ImageProfileFit.h: Class to fit profiles in images
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
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#   $Id$

#ifndef IMAGES_IMAGEPROFILEFIT_H
#define IMAGES_IMAGEPROFILEFIT_H

//# Includes
#include <casa/aips.h>
#include <casa/Arrays/Vector.h>
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Quanta/Unit.h>
#include <measures/Measures/MDoppler.h>

#include <components/SpectralComponents/SpectralElement.h>
#include <components/SpectralComponents/SpectralFit.h>
#include <casa/Utilities/PtrHolder.h>


namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward declarations
template<class T> class ImageInterface;
template<class T> class MaskedLattice;
class GlishRecord;
class ImageRegion;
class IPosition;
class Slicer;
class LogIO;

// <summary>
// Class to fit profiles in images
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=></linkto>
// </prerequisite>

// <synopsis> 
// The Record used to contain models/fits is as follows.  Other fields
// will be ignored.
//
// <srcblock>
// Field                     Type             Description
//-----------------------------------------------------------
// xabs                      Bool              Are the x-values absolute or 
//                                             relative to the reference pixel
// xunit                     String            The x unit 
// yunit                     String            The y unit
// doppler                   String            doppler type ('radio', 'optical', 'true' etc)
//                                             Only required if x unit consistent with m/s
// elements                  Record            Holds SpectralElement descriptions.  
// elements[i]               Record            There will be N of these where N is the
//                                             number of spectral elements.  Each elements[i]
//                                             holds a record description that SpectralElement::fromRecord
//                                             can read or writes
//         .type             String            Type of SE (gaussian, polynomial, etc)
//         .parameters       Vector<Double>    The parameters of the element.
//         .errors           Vector<Double>    The errors of parameters of the element.
//         .fixed            Vector<Bool>      Says whether parameter is going to be held fixed when fitting
//                                             This field is not used by SpectralElement.  If its
//                                             not there, all parameters are fitted for.
//
// </srcblock>
// </synopsis> 
// <example>
// <srcblock>
// </srcblock>
// </example>

// <todo asof="2001/03/01">
// </todo>

class ImageProfileFit 
{
public:
    // Constructor
    explicit ImageProfileFit();

    // Destructor
    ~ImageProfileFit();

    // Copy constructor.  Uses copy semantics.
    ImageProfileFit(const ImageProfileFit& other);

    // Assignment operator. Uses copy semantics.
    ImageProfileFit& operator=(const ImageProfileFit& other);

    // Set data via an image. <src>profileAxis</src> specifies the profile pixel
    // axis. You can either average the data over all other axes in the
    // image (<src>average=True</src>) or fit all profiles in the 
    // image.
    //<group>
    void setData (const ImageInterface<Float>& image,
                  const ImageRegion& region,
                  uInt profileAxis, Bool average=True);
    void setData (const ImageInterface<Float>& image,
                  const ImageInterface<Float>& sigma,
                  const ImageRegion& region,
                  uInt profileAxis, Bool average=True);
//
    void setData (const ImageInterface<Float>& image,
                  uInt profileAxis, Bool average=True);
    void setData (const ImageInterface<Float>& image,
                  const ImageInterface<Float>& sigma,
                  uInt profileAxis, Bool average=True);

    //</group>

    // Set the data directly, and provide a coordinate system 
    // and specify the profile axis in the coordinate system.
    // The x-units can be 'pix'. If absolute
    // they must be 0-rel pixels. <src>isAbs</src> specifies whether
    // the coordinates are absolute or relative to the reference pixel.
    // If the weights vector, <src>sigma</src> is of zero length, 
    // it is treated as all unity.
    //<group>
    void setData (const CoordinateSystem& cSys,
                  uInt ProfileAxis,
                  const Quantum<Vector<Float> >& x, 
                  const Quantum<Vector<Float> >& y,
                  const Vector<Float>& sigma,
                  Bool isAbs, const String& doppler);

    void setData (const CoordinateSystem& cSys,
                  uInt ProfileAxis,
                  const Quantum<Vector<Float> >& x, 
                  const Quantum<Vector<Float> >& y,
                  const Vector<Bool>& mask,
                  const Vector<Float>& sigma,
                  Bool isAbs, const String& doppler);
    //</group>

    // Makes an estimate from the set data. This means it generates SpectralElements
    // holding the estimate, and replaces all elements you might have
    // put in place with function <src>addElement</src>.   Returns
    // False if it can't find any elements.
    Bool estimate (uInt nMax = 0);

    // Decode the Glish record holding SpectralElements and add
    // them to the fitter.   Absolute pixel coordinate units are assumed to be
    // 1-rel on input.  Return the number of the element added.
    uInt addElements (const RecordInterface& estimate);

    // Gets the internal SpectralElements (either estimate or fit
    // depending on what function you called last) into a record.  
    // Only returns False if the field is already defined. Absolute pixel 
    // coordinate units  are 1-rel on output.
    Bool getList (RecordInterface& rec,
                  Bool xAbsOut,
                  const String& xUnitOut,
                  const String& dopplerOut);

    // Gets the internal SpectralElements (either estimate or fit
    // depending on what function you called last) into a SpectralList
    // Only returns False if the field is already defined. Absolute pixel 
    // coordinate units  are 1-rel on output.
    SpectralList getList () const;

    // Reset the internal list of SpectralElements to null 
    void reset ();

    // Return number of SpectralElements set
    uInt nElements ();

    // Do the fit of the averaged profile. Specify the
    // order of the baseline you would also like to fit for.
    //<group>
    Bool fit(Int order=-1);
    //</group>

    // Fit all profiles with shapes + optional polynomial in the region and write out images.
    // If fillRecord is True, the output record is filled with the the parameters of 
    // every fit.  This can get VERY large so use with care.  If the output images
    // have a writable mask, the input mask is transferred to the output.
    //<group>
    void fit (Bool fillRecord, RecordInterface& rec,  
              Bool xAbsRec,
              const String& xUnitRec,
              const String& dopplerRec,
              ImageInterface<Float>* pFit,
              ImageInterface<Float>* pResid,
              Int order=-1);
    //</group>

    // Find the residuals (fit or estimate) of the averaged profile
    //<group>
    void residual(Vector<Float>& resid, const Vector<Float>& x) const;
    void residual(Vector<Float>& resid) const;
    //</group>

    // Find the model (fit or estimate) of the averaged profile
    //<group>
    void model (Vector<Float>& model, const Vector<Float>& x) const;
    void model (Vector<Float>& model) const;
    //</group>

private:

// Images holding data and weights.  Will be set only if fitting all profiles in region

   ImageInterface<Float>* itsImagePtr;
   ImageInterface<Float>* itsSigmaImagePtr;
//
   Bool itsFitDone;

// Holds the abcissa, ordinate and mask.  x-units will be pixels
// if data source is an image, else as specified in setData

   Quantum<Vector<Float> > itsX, itsY;
   Vector<Bool> itsMask;
   Vector<Float> itsSigma;

// The fitters.  The first one does not have a polynomial in it
// The second one may.
   SpectralFit* itsSpectralFitPtr;
   SpectralFit itsSpectralFitter;

// The coordinate system if the data source was an image
// itsProfileAxis specified the profile axis in the image
// Will be -1 if data source was just vectors

   CoordinateSystem itsCoords;
   Int itsProfileAxis;

// If the data source was an image, these give the doppler type
// (if any) and x-unit IF an estimate was specified by the user via
// function addElements.  They are used in function getElements
// so that the output record has the same units.

   String itsDoppler;                 // The doppler of the data source 
   Bool itsXAbs;                      // Is the data source in absolute coordinates ?
   Bool itsFitRegion;                 // True to fit all profiles in region
//
   void collapse (Vector<Float>& profile, Vector<Bool>& mask,
                         uInt profileAxis,  const MaskedLattice<Float>& lat) const;

// Convert SE
   SpectralElement convertSpectralElement (const SpectralElement& elIn,
                                           Bool xAbsIn, Bool xAbsOut,
                                           const String& xUnitIn,
                                           const String& xUnitOut,
                                           const String& dopplerIn,
                                           const String& dopplerOut,
                                           const String& yUnitIn,
                                           const String& yUnitOut);

// Convert Gaussian model x-units when data source is an image
    void convertGaussianElementX (SpectralElement& el,
                                  CoordinateSystem& cSys,
                                  uInt profileAxis,
                                  Bool absIn, Bool absOut,
                                  const String& unitIn,
                                  const String& unitOut,
                                  const String& dopplerIn,
                                  const String& dopplerOut);

// 
   SpectralList filterList (const SpectralList& listIn) const;

//
   Bool getElements (RecordInterface& rec,
                     Bool xAbsOut,
                     const String& xUnitOut,
                     const String& dopplerOut,
                     const SpectralList& list);
//
   void setData (const ImageInterface<Float>*& pSigma,
                 const ImageInterface<Float>& image,
                 const Slicer& sl, Bool average);
};


} //# NAMESPACE CASA - END

#endif
