//# ImageProfileFit.h: Class to fit profiles in images
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
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#   $Id$

#if !defined(AIPS_IMAGEPROFILEFIT_H)
#define AIPS_IMAGEPROFILEFIT_H

//# Includes
#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/Unit.h>
#include <aips/Measures/MDoppler.h>
//
#include <trial/SpectralComponents/SpectralElement.h>
#include <trial/SpectralComponents/SpectralFit.h>
#include <aips/Utilities/PtrHolder.h>


// Forward declarations
template<class T> class ImageInterface;
template<class T> class MaskedLattice;
class GlishRecord;
class ImageRegion;
class IPosition;
class Slicer;
class LogIO;

// <summary>
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
// <src>
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
// </src>
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

    // Set data. <src>profileAxis</src> specifies the profile pixel
    // axis. The data in the region are averaged over all other axes.
    // Exception if vectors not conformant.
    //<group>
    void setData (const ImageInterface<Float>& image,
                  const ImageRegion& region,
                  uInt profileAxis, Bool average=True);
    void setData (const ImageInterface<Float>& image,
                  uInt profileAxis, Bool average=True);
    //</group>

    // Set data directly. x-units can be 'pix'. if absolute
    // they must be 0-rel
    // Say whether positions are absolute or relative (to somewhere)
    //<group>
    void setData (const Quantum<Vector<Float> >& x, 
                  const Quantum<Vector<Float> >& y,
                  Bool isAbs);
    void setData (const Quantum<Vector<Float> >& x, 
                  const Quantum<Vector<Float> >& y,
                  const Vector<Bool>& mask,
                  Bool isAbs);
    //</group>

    // Makes an estimate.  This means it generates SpectralElements
    // holding the estimate, and replaces all elements you might have
    // put in place with function <src>addElement</src>.   Returns
    // False if can't find any elements.
    Bool estimate (uInt nMax = 0);

    // Decode the Glish record holding SpectralElements and adds
    // them to the fitter.   Absolute pixel coordinate units are 1-rel on input.
    uInt addElements (const RecordInterface& estimate);

    // Gets SpectralElements into a record.  When the data source is an image:
    // the xunits are selected (in order of preference) xunits specified in <src>addElements</src>,
    // preferred profile axis units in the CoordinateSystem of the image, native units
    // of the profile axis; xabs and doppler are those specified in <src>addElements</src>
    // or True and 'radio' of its not called. If the data source is not an image,
    // the xunits, yunits, xabs are those of the source vectors, the doppler is 'radio'
    //
    // Only returns False if the field is already defined. Absolute pixel 
    // coordinate units  are 1-rel on output.
    Bool getElements (RecordInterface& estimate,
                      const String& xUnit);

    // List all elements to stream (in same units as getElement)
    void listElements (LogIO& os, const SpectralList& list) const;

    // Reset to default state
    void reset ();

    // Return number of SpectralElements set
    uInt nElements ();

    // Do the fit of the averaged profile
    //<group>
    Bool fit();
    //</group>


    // Fit all profiles in the region and write out images.
    //<group>
    void fit (RecordInterface& rec, 
              PtrHolder<ImageInterface<Float> >& fit,
              PtrHolder<ImageInterface<Float> >& resid,
              const String& xUnitRec);
    //</group>

    // Find the residuals 
    //<group>
    void residual(Vector<Float>& resid, const Vector<Float>& x) const;
    void residual(Vector<Float>& resid) const;
    //</group>

    // Find the model
    //<group>
    void model (Vector<Float>& model, const Vector<Float>& x) const;
    void model (Vector<Float>& model) const;
    //</group>


private:

// Image holding data.  Only will be set if fitting all profiles in region

   ImageInterface<Float>* itsImagePtr;
//
   Bool itsFitDone;

// Holds the abcissa, ordinate and mask.  x-units will be pixels
// if data source is an image, else as specified in setData

   Quantum<Vector<Float> > itsX, itsY;
   Vector<Bool> itsMask;

// The fitter
   SpectralFit* itsSpectralFitPtr;

// The coordinate system if the data source was an image
// itsProfileAxis specified the profile axis in the image
// Will be -1 if data source was just vectors

   CoordinateSystem itsCoords;
   Int itsProfileAxis;

// If the data source was an image, these give the doppler type
// (if any) and x-unit IF an estimate was specified by the user via
// function addElements.  They are used in function getElements
// so that the output record has the same units.

   MDoppler::Types itsDopplerType;
   Unit itsXUnit;
   Bool itsXAbs;
   Bool itsFitRegion;                 // True to fit all profiles in region
//
   void collapse (Vector<Float>& profile, Vector<Bool>& mask,
                  uInt profileAxis,  const MaskedLattice<Float>& lat) const;

// Convert model to absolute pixels
   void convertXEstimateToPixels (SpectralElement& el,
                                  Bool xAbsIn,
                                  const Unit& xUnitIn,
                                  const String& dopplerIn);

// Convert from asbolute pixels to model
   void convertXEstimateFromPixels (SpectralElement& el,
                                    Bool xAbsOut,
                                    const Unit& xUnitOut,
                                    const MDoppler::Types dopplerOut);
//
   Bool getElements (RecordInterface& estimate,
                     const String& xUnit, const SpectralList& list);

//
   void setData (const ImageInterface<Float>& image,
                 const Slicer& sl, Bool average);
};

#endif
