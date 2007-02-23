//# FITSSpectralUtil.h: Static functions to help with FITS spectral axes.
//# Copyright (C) 2002
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
//#
//# $Id$


#ifndef FITS_FITSSPECTRALUTIL_H
#define FITS_FITSSPECTRALUTIL_H

#include <casa/aips.h>
#include <measures/Measures/MDoppler.h>
#include <measures/Measures/MFrequency.h>

namespace casa { //# NAMESPACE CASA - BEGIN

class RecordInterface;
class String;
template<class T> class Vector;
class LogIO;

// <summary>
// A class with static functions to help deal with FITS spectral axes.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Eric Sessoms" date="2002/08/19" tests="tFITSKeywordUtil.cc">
// </reviewed>

// <prerequisite>
// <li> General knowlege of FITS, FITS keywords, and FITS coordinate
//      conventions is assumed.
// <li> Presumably you are using this in conjuction with the
//      <linkto class=FITSKeywordUtil>FITSKeywordUtil</linkto> class
//      to get or set the FITS coordinate axis information.
// </prerequisite>

// <etymology>
// This is a collection of static utility functions for use with
// FITS spectral axes.
// </etymology>

// <synopsis>
// This class provides functions to extract information from a FITS
// header about the spectral axis, to setup a FITS header with
// appropriate information for the spectral axis, and to translate
// to and from the MFrequency refrence frame codes and their FITS
// equivalents.
// It is never necessary to construct a FITSSpectralUtil, just use the 
// static functions to help handle FITS Spectral axes.
// </synopsis>
//
// <example>
// <srcblock>
// Record rec;
// ... extract the FITS keyword values into rec using FITSKeywordUtil
// Int whichAxis;
// Double refPix, refFreq, freqInc, restFreq;
// Vector<Double> freqs;
// MFrequency::Types refFrame;
// MDoppler::Types veldef;
// LogIO logger;
// FITSSpectralUtil::fromFITSHeader(whichAxis, refPix, refFreq,
//        freqInc, freqs, refFrame, veldef, logger, rec);
// </srcblock>
// </example>
//
// <motivation>
// This is designed to be used after the keywords have been extracted from
// the FITS file using the <linkto class=FITSKeywordUtil>FITSKeywordUtil</linkto>
// class.  Extracting spectral axis and related information requires detailed
// knowledge of FITS conventions that this class strives to encapsulize.
// </motivation>
//

class FITSSpectralUtil
{
public:
    // Get information about the spectral axis from a record containing the
    // FITS axis information.  Usually this will be from a FITS header using,
    // for example, the FITSKeywordUtil::getKeywords method.
    // referenceFrequency and deltaFrequency give the best
    // possible linear frequency scale.  prefix is the first character in the
    // set of keywords describing the axes (e.g. crpix, crval, ctype - the prefix is
    // 'c').  If oneRelative is False, the returned referenceChannel is decrimented
    // from that found in header by 1.  The naxis keywords are used to determine
    // the output length of the freqs vector.
    // This method returns False if:
    // <li> no spectral axis is found. The freqs vector will have a length of zero.
    // <li> The expected set of axis description keywords is not found.
    // <li> The spectral axis is FELO but there is no rest frequency (making it
    //      impossible to convert to frequency and construct a freqs vector).
    // <li> The combination FELO and RADIO is used (which does not make sense).
    // <li> The combination VELO and OPTICAL is used (not yet implemented).
    static Bool fromFITSHeader(Int &spectralAxis,
			       Double &referenceChannel,
			       Double &referenceFrequency,
			       Double &deltaFrequency,
			       Vector<Double> &frequencies,
			       MFrequency::Types &refFrame,
			       MDoppler::Types &velocityPreference,
			       Double &restFrequency,
			       LogIO &logger,
			       const RecordInterface &header,
			       char prefix = 'c',
			       Bool oneRelative=True);

    // Nearly the inverse of fromFITSHeader. This returns parameters which could
    // be used in filling in a header record with appropriate values for
    // the spectral axis given the arguments after the logger.
    // The alternate axis description values are set when sufficient information is available.
    // If they have been set, haveAlt will be set to True.
    // <li> Note that the output arguments after "haveAlt"
    //      should not be written to the FITS header unless haveAlt is true. 
    // <li> Note that restfreq is both an input and an output. If there is no rest
    //    frequency, set it to be <=0 on input.  

    // If preferVelocity is True, the
    // axis description parameters will be set to those appropriate for
    // a velocity axis given the referenceFrame, and velocityPreference
    // if possible.

    // This method always returns True.
    static Bool toFITSHeader(String &ctype, 
			     Double &crval, 
			     Double &cdelt,
			     Double &crpix, 
			     Bool &haveAlt, 
			     Double &altrval,
			     Double &altrpix,
			     Int &velref,
			     Double &restfreq,
			     LogIO &logger,
			     Double refFrequency,
			     Double refChannel,
			     Double freqIncrement,
			     MFrequency::Types referenceFrame,
			     Bool preferVelocity = True,
			     MDoppler::Types velocityPreference = 
			                               MDoppler::OPTICAL);

    // Convert a reference frame tag (typically found as the characters
    // after the first 4 characters in a ctype string for the
    // frequency-like axis) to a MFrequency::Types value.
    // A velref value (used in AIPS images to alternatively record
    // the velocity reference frame) may also optionally be supplied.
    // If tag is empty, velref will be used if it is >= 0.
    // This function returns False if:
    // <li> The tag is not empty but is unrecognized.
    // <li> The tag is empty and velref is unrecognized.
    // <li> The tag is empty and velref is < 0 (no velref was supplied).

    // The default value (set when the return value is False) is TOPO.
    static Bool frameFromTag(MFrequency::Types &referenceFrame,
			     const String &tag,
			     Int velref=-1);

    // Construct a reference frame tag from the given referenceFrame
    // An appropriate velref value is also constructed (this may need 
    // to be adjusted by +256 if the velocity definition is radio before
    // being used in a FITS file).  This returns False if the
    // reference frame is not recognized.  The value of tag defaults
    // to "-OBS".
    static Bool tagFromFrame(String &tag, Int &velref,
			     MFrequency::Types referenceFrame);
			  
};


} //# NAMESPACE CASA - END

#endif
