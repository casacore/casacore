//# MSSelectionKeywords.h: selection keywords for the MS
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
//#
//# $Id$

#ifndef MS_MSSELECTIONKEYWORDS_H
#define MS_MSSELECTIONKEYWORDS_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class K, class V> class SimpleOrderedMap;
template <class T> class Block;
// forward declare the class so we can typedef it
class MSSelectionKeywords;
class String;

// Define a shorthand notation for this class, so enums can be specified
// easily.
typedef MSSelectionKeywords MSS;

// <summary>
// MSSelectionKeywords specifies selection keywords for the MeasurementSet
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MeasurementSet
//   <li> MSSelector
// </prerequisite>
//
// <etymology>
// MSSelectionKeywords is a class that defines selection keywords
// </etymology>
//
// <synopsis>
// This class is used to specify selections on a MeasurementSet.
// It is a purely static class that just defines a mapping from 
// Strings to Enums, and provides these for use by classes like
// MSSelector and MSRange
//
// <example> <srcblock>
// </srcblock></example>
// </synopsis>
//
// <motivation>
// Selection keywords are needed for several classes, this class provides
// them to all, avoiding duplication in each class.
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> add this feature
// </todo>

class MSSelectionKeywords
{
public:
  // The fields in the MS for which selection and range operations are
  // defined. Some of these directly correspond to columns in the table,
  // others are derived quantities or columns in subtables.
  enum Field { 
    // undefined field
    UNDEFINED=0, 
    // the range of visibility amplitude
    AMPLITUDE,
    // the range of corrected vis amplitude
    CORRECTED_AMPLITUDE,
    // the range of model vis amplitude
    MODEL_AMPLITUDE,
    // the amplitude of the ratio corrected data/model data
    RATIO_AMPLITUDE,
    // the residual vis amplitude (corrected-model)
    RESIDUAL_AMPLITUDE,
    // the observed residual vis amplitude (observed-model)
    OBS_RESIDUAL_AMPLITUDE,
    // the list of antenna1 id values
    ANTENNA1,
    // the list of antenna2 id values
    ANTENNA2,
    // the list of antenna names
    ANTENNAS,
    // the list of array id values
    ARRAY_ID,
    // description of the data axes
    AXIS_INFO,
    // the channel frequencies, a vector for each selected spectral window
    CHAN_FREQ,
    // the list of polarizations present, this gives the String values
    CORR_NAMES,
    // the list of polarizations present, this gives the Stokes enum values
    CORR_TYPES,
    // the complex data
    DATA,
    // the complex corrected data
    CORRECTED_DATA,
    //the complex model data
    MODEL_DATA,
    // the ratio corrected data/model data
    RATIO_DATA,
    // the residual data (corrected - model)
    RESIDUAL_DATA,
    // the observed residual data (observed - model)
    OBS_RESIDUAL_DATA,
    // the list of dataDescription id values
    DATA_DESC_ID,
    // the list of feed1 id values
    FEED1,
    // the list of feed2 id values
    FEED2,
    // the list of field_id values
    FIELD_ID,
    // the list of field names
    FIELDS,
    // the flags
    FLAG,
    // the row flags
    FLAG_ROW,
    // a summary of flags (flag count summed over rows)
    FLAG_SUM,
    // the float data (optional single dish column)
    FLOAT_DATA,
    // Hour angle
    HA,
    // the list of interferometers (= 1000*ant1+ant2) present
    IFR_NUMBER,
    // the (range of the) imaginary part of the visibilities
    IMAGINARY,
    // the (range of the) imaginary part of the corrected visibilities
    CORRECTED_IMAGINARY,
    // the (range of the) imaginary part of the model visibilities
    MODEL_IMAGINARY,
    // the imaginary part of the ratio corrected data/model data
    RATIO_IMAGINARY,
    // the (range of the) imaginary part of the residual visibilities
    RESIDUAL_IMAGINARY,
    // the (range of the) imaginary part of the observed residual visibilities
    OBS_RESIDUAL_IMAGINARY,
    // Local Apparent Sidereal Time
    LAST,
    // the number of correlation products (polarizations) for selected spectral window
    NUM_CORR,
    // the number of spectral channels for selected spectral window
    NUM_CHAN,
    // the (range of the) phase of the visibilities
    PHASE,
    // the (range of the) phase of the corrected visibilities
    CORRECTED_PHASE,
    // the (range of the) phase of the model visibilities
    MODEL_PHASE,
    // the phase of the ratio corrected data/model data
    RATIO_PHASE,
    // the (range of the) phase of the residual visibilities
    RESIDUAL_PHASE,
    // the (range of the) phase of the observed residual visibilities
    OBS_RESIDUAL_PHASE,
    // the phase center direction for each field (matrix + epoch)
    PHASE_DIR,
    // the (range of the) real part of the visibilities
    REAL,
    // the (range of the) real part of the corrected visibilities
    CORRECTED_REAL,
    // the (range of the) real part of the model visibilities
    MODEL_REAL,
    // the real part of the ratio corrected data/model data
    RATIO_REAL,
    // the real part of the residual visibilities (corrected-model)
    RESIDUAL_REAL,
    // the real part of the observed residuals (observed-model)
    OBS_RESIDUAL_REAL,
    // the reference frequency for selected spectral window (or vector with all)
    REF_FREQUENCY,
    // the list of row numbers in the original MS
    ROWS,
    // the list of scan_number values
    SCAN_NUMBER,
    //# the list of spectral window id values
    //# SPECTRAL_WINDOW_ID,
    // the per spectrum sigmas
    SIGMA,
    // the range of times
    TIME,
    // the list of time values
    TIMES,
    // UT time (seconds of current day)
    UT,
    // the uvw coordinates
    UVW,
    // the (range of the) U coordinate (m)
//#    Note:order of U, V and W is important, no intervening items allowed
//#    without changing select() code.
    U,
    // the (range of the) V coordinate (m)
    V, 
    // the (range of the) W coordinate (m)
    W,
    // the (range of the) UV-distance (m)
    UVDIST,
    // the weights
    WEIGHT,
    // Number of keywords
    NUMBER_KEYWORDS
};


  // convert a keyword string to the corresponding enum
  static Field field(const String& keyword);

  // convert an enum value to the corresponding keyword string
  static const String& keyword(Field field);

protected:
  // This class is purely static, no instances are allowed.
  MSSelectionKeywords();
  MSSelectionKeywords(const MSSelectionKeywords& other);
  MSSelectionKeywords& operator=(const MSSelectionKeywords& other);
  
  // initialization function for the string to enum mapping
  static void initMap(SimpleOrderedMap<String,Int>*& fieldMap,
		      Block<Int>*& reverseMap);
};


} //# NAMESPACE CASACORE - END

#endif
