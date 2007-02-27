//# FrequenctAligner.h: Align spectra in frequency space
//# Copyright (C) 1998,1999,2000,2001,2003
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

#ifndef COORDINATES_FREQUENCYALIGNER_H
#define COORDINATES_FREQUENCYALIGNER_H


//# Includes
#include <coordinates/Coordinates/SpectralCoordinate.h>
#include <measures/Measures/MFrequency.h>
#include <measures/Measures/MeasRef.h>
#include <measures/Measures/MeasConvert.h>
#include <scimath/Mathematics/InterpolateArray1D.h>

namespace casa {

//# Forward Declarations
class MEpoch;
class MDirection;
class MPosition;
class String;



// <summary>
// Aligns spectra in frequency space
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="tFrequencyAligner.cc">
// </reviewed>
//
// <prerequisite>
// <list>
//   <item> <linkto class=InterpolateArray1D>InterpoateArray1D</linkto>
//   <item> <linkto class=Array>Array</linkto>
// </list>
// </prerequisite>
//// <synopsis> 
// Spectra are converted to the specified reference frame and aligned at 
// a specified instant in time.
//
// You should not try to convert from, say,  a SpectralCoordinate::TOPO to
// MFrequency::TOPO as this would be meaningless.  This class is designed
// mainly to convert say from a SpectralCoordinate::TOPO to say, a BARY 
// frame and align.
// </synopsis> 
//
// <motivation>
// Required for ASAP single-dish package
// </motivation>
//
// <todo asof="2004/11/01">
// </todo>


template <class T> class FrequencyAligner
{
public: 

// Default constructor (object not viable)
   FrequencyAligner();

// Constructor specifies a SpectralCoordinate (any extra reference conversion
// frame set in it will be ignored),  the number of pixels in the spectra to
// be aligned, a reference epoch to which all spectra will
// be aligned, a direction on the sky,  a position on the earth (the observatory),
// and desired frequency system to align in.  
   FrequencyAligner(const SpectralCoordinate& specCoord, uInt nPixels,
                    const MEpoch& refEpoch, const MDirection& dir, 
                    const MPosition& pos,  MFrequency::Types freqSystem);

// Copy constructor (copy semantics)
   FrequencyAligner (const FrequencyAligner<T>& other);

// Assignment (copy semantics)
   FrequencyAligner& operator=(const FrequencyAligner<T>& other);

// Destructor
  ~FrequencyAligner();

// Set a tolerance (in pixels) to trigger regridding (function <src>align</src>).
// If the maximum abcissa difference for the current spectrum abcissa compared 
// to the reference abcissa is greater than <src>tol (pixels)</src> then a 
// regrid is triggered. Otherwise the input is just copied to the output when 
// function <src>align</src> is called.  Set to 0 to turn this tolerance
// assessment off.  This function may be not really worth using.
   void setTolerance (Double tol) {itsDiffTol = abs(tol);};

// Align (via regridding) one spectrum taken at the specified epoch to 
// the reference epoch.  Your provide the ordinate and mask (True==Good)
// for the spectrum.  The lengths of these vectors must be the same
// as <src>nPixels</src> given in the constructor.  The output vectors
// are resized as needed.
// You can use the last cached abcissa (computed by
// this function) rather than recompute it if you have more than one spectrum 
// at the same epoch to convert (e.g. different polarizations).
// If you do this, it is your responsibility to make sure that you
// have called this function at least once with <src>useCachedAbcissa=False</src>.
//  If <src>extrapolate</src> is True, the regridding process is allowed 
// to extrapolate outside of the abcissa domain. Otherwise masked pixels will result.
// Returns True if a regrid triggered, else False if just copied (see function
// <src>setTolerance</src>. 
  Bool align (Vector<T>& yOut, Vector<Bool>& maskOut,
              const Vector<T>& yIn, const Vector<Bool>& maskIn,
              const MEpoch& epoch, Bool useCachedAbcissa,
              typename InterpolateArray1D<Double,T>::InterpolationMethod method,
              Bool extrapolate=False);              

// This function is the same as the previous except that you can specify the input abcissa as well 
// as the data and mask.  The input abcissa must be in the same units as the Construction
// SpectralCoordinate.  The abcissa values must be in the same base reference frame
// as the  Construction SpectralCoordinate.  So instead of the abcissa (in the
// output reference frame) being computed from the Construction SC, you get to specify
// the abcissa directly.  This might be useful if you have more than one set of
// spectra to align, all in the same Frame, but with different attributes such
// as reference value/pixel etc.   The output spectrum is still regridded to the
// abcissa at the reference time generated at construction.
// from the current 
  Bool align (Vector<T>& yOut, Vector<Bool>& maskOut,
              const Vector<Double>& xIn, const Vector<T>& yIn, const Vector<Bool>& maskIn,
              const MEpoch& epoch, Bool useCachedAbcissa,
              typename InterpolateArray1D<Double,T>::InterpolationMethod method,
              Bool extrapolate=False);              

// Align many spectra stored in an Array along the specified axis.  All spectra are aligned
// to the same frequency abcissa (as described in previous function).  If any alignment
// returns False, then the return value will be False, otherwise  True is returned.
  Bool alignMany (Array<T>& yOut, Array<Bool>& maskOut,
                  const Array<T>& yIn, const Array<Bool>& maskIn,
                  uInt axis, const MEpoch& epoch, 
                  typename InterpolateArray1D<Double,T>::InterpolationMethod method,
                  Bool extrapolate=False);              

// Get the reference abcissa (as a frequency in the axis units set in the SpectralCoordinate) at the reference epoch 
  void getReferenceAbcissa (Vector<Double>& xOut) const;

// Get the abcissa (as a frequency in the axis units set in the SpectralCoordinate) last cached by function <src>align</src>
  void getAbcissa (Vector<Double>& xOut) const;

// Get new aligned SpectralCoordinate.  It is probably non-linear, but if you would
// like a linear approximation, use the doLinear argument.
  SpectralCoordinate alignedSpectralCoordinate (Bool doLinear=True) const;

private:
  SpectralCoordinate itsSpecCoord;
  MFrequency::Convert itsMachine;
  MFrequency::Ref itsRefOut;                      // Need this as there is no easy way to update
                                                  // the conversion machines epoch otherwise
  MFrequency::Types itsFreqSystem;
//
  Vector<Double> itsRefFreqX;                     // Reference frequency abcissa
  Vector<Double> itsFreqX;                        // Frequency abcissa

  Double itsDiffTol;                              // Tolerance which triggers a regrid

// Internal copy
   void copyOther (const FrequencyAligner<T>& other);

// Create the Conversion machine
   void makeMachine (const MEpoch& refEpoch,
                     const MDirection& dir,
                     const MPosition& pos,
                     MFrequency::Types freqSystem,
                     const Unit& unit);

// Generate an abcissa with the machine
   Double makeAbcissa (Vector<Double>& f, Bool doMaxDiff);

// Regrid one spectrum
   Bool regrid (Vector<T>& yOut, Vector<Bool>& maskOut,
                const Vector<Double>& xOut,
                const Vector<Double>& xIn,
                const Vector<T>& yIn, const Vector<Bool>& maskIn,
                typename InterpolateArray1D<Double,T>::InterpolationMethod method,
                Bool extrapolate, Double maxDiff) const;
};


} //# End namespace casa
#ifndef AIPS_NO_TEMPLATE_SRC
#include <coordinates/Coordinates/FrequencyAligner.cc>
#endif //# AIPS_NO_TEMPLATE_SRC
#endif
