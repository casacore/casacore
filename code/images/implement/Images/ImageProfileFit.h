//# ImageProfileFit.h: Class to fit profiles
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
#include <trial/Wnbt/SpectralElement.h>
#include <trial/Wnbt/SpectralEstimate.h>
#include <trial/Wnbt/SpectralFit.h>

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
                  uInt profileAxis);
    void setData (const ImageInterface<Float>& image,
                  uInt profileAxis);
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

    // Decode the Glish record holding estimates and adds
    // to the fitter.  
    uInt addElements (const RecordInterface& estimate);

    // Get elements into a record.  USe doppler and abs of addElements
    // Only returns False if the field is already defined.
    Bool getElements (RecordInterface& estimate,
                      const String& xUnit);

    // List all elements to stream (in same units as getElement)
    void listElements (LogIO& os) const;

    // Reset to default state
    void reset ();

    // Return number of SpectralElements set
    uInt nElements ();

    // Do the fit
    //<group>
    Bool fit();
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
   void setData (const ImageInterface<Float>& image,
                 const Slicer& sl);
};

#endif
