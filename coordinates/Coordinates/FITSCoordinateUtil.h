//# FITSCoordinateUtil.h: functions to inter-convert between CoordinateSystems and FITS
//# Copyright (C) 1997,1998,1999,2000,2001,2002,2003,2004
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
//# $Id: 

#ifndef COORDINATES_FITSCOORDINATEUTIL_H
#define COORDINATES_FITSCOORDINATEUTIL_H

#include <casa/aips.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MFrequency.h>
#include <coordinates/Coordinates/ObsInfo.h>


class wcsprm;

namespace casa { //# NAMESPACE CASA - BEGIN

class Coordinate;
class CoordinateSystem;
class StokesCoordinate;
class Projection;
class IPosition;
class LogIO;
class Record;




// <summary>
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=Coordinate>CoordinateSystem</linkto>
// </prerequisite>

// <synopsis>
// Helper functions to inter-converft between a CoordinateSystem and FITS
// headers.
// </synopsis>

// <note role=caution>
// </note>

// <example>
// </example>

// <motivation>
//  I hate FITS
// </motivation>
//
// <thrown>
//   <li>  AipsError
// </thrown>
//
// <todo asof="2004/08/23">
// </todo>
//


class FITSCoordinateUtil 
{
public:

    // Constructor
    FITSCoordinateUtil() {;};
   
    // Convert CoordinateSystem to a FITS header.  In the record
    // the keywords are vectors, it is expected that the actual FITS code will
    // split them into scalars and upcase the names. Returns False if one of the
    // keywords is already taken.
    // 
    // If writeWCS is True, attempt to write the WCS convention (Greisen and
    // Calabretta "Representation of celestial coordinates in FITS") as
    // approved in version 3.0 of the FITS standard.
    // Use <src>oneRelative=True</src> to convert zero-relative pixel coordinates to
    // one-relative FITS coordinates.
    //
    // prefix gives the prefix for the FITS keywords. E.g.,
    // if prefix="c" then crval, cdelt etc. 
    // if prefix="d" then drval, ddelt etc. 
    //# Much of the work in to/from fits should be moved to the individual
    //# classes.
    Bool toFITSHeader(RecordInterface &header, 
		      IPosition &shape,
                      const CoordinateSystem& cSys,
		      Bool oneRelative, 
		      Char prefix = 'c', Bool writeWCS=True,
		      Bool preferVelocity=True, 
		      Bool opticalVelocity=True) const;

    // Probably even if we return False we should set up the best linear
    // coordinate that we can.   On output, <src>stokesFITSValue</src>
    // holds the FITS value of any unofficial Stokes (beam, optical depth,
    // spectral index) for the last unofficial value accessed (-1 if none).
    // The idea is that if the Stokes axis is of length one and holds an unofficial value,
    // you should drop the STokes axis and convert that value to <src>ImageInfo::ImageTypes</src>
    // with <src>ImageInfo::imageTypeFromFITSValue</src>. If on input, <src>stokesFITSValue</src>
    // is positive, then a warning is issued if any unofficial values are encountered.
    // Otherwise no warning is issued.
    //# cf comment in toFITS.
    //<group>
    Bool fromFITSHeader(Int& stokesFITSValue, 
                        CoordinateSystem& coordsys, 
			RecordInterface& recHeader,
                        const Vector<String>& header,
                        const IPosition& shape,
                        uInt which=0) const;
    //</group>


    // Helper function to create a FITS style CTYPE vector from the 
    // axis names from a DirectionCoordinate
    static Vector<String> cTypeFromDirection (Bool& isNCP, const Projection& proj,
                                              const Vector<String>& axisNames,
                                              Double refLat, Bool printError);

private:
    // Generate actual FITS keywords
    Bool generateFITSKeywords (LogIO& os, Bool& isNCP,
                               Double& longPole, Double& latPole,
                               Vector<Double>& crval,
                               Vector<Double>& crpix,
                               Vector<Double>& cdelt,
                               //   Vector<Double>& crota,
			       //   Vector<Double>& projp,
                               Vector<Double>& pvi_ma,
                               Vector<String>& ctype,
                               Vector<String>& cunit,
                               Matrix<Double>& pc,
                               const CoordinateSystem& cSys,
                               Int skyCoord, Int longAxis, Int latAxis,
                               Int specAxis, Int stokesAxis, 
                               Bool writeWCS, Double offset,
                               const String& sprefix) const;

    // Special Stokes processing  for conversion to FITS header
    Bool toFITSHeaderStokes(Vector<Double>& crval,
                            Vector<Double>& crpix,
                            Vector<Double>& cdelt,
                            LogIO& os,
                            const CoordinateSystem& coordsys,
                            Int stokesAxis, Int stokesCoord) const;

    // Look for Coordinate type and add to CS
    // <group>
    Bool addDirectionCoordinate (CoordinateSystem& cSys, Vector<Int>& axes,
                                 const wcsprm& wcs, LogIO& os) const;
    Bool addSpectralCoordinate (CoordinateSystem& cSys, Int& axis,
                                const wcsprm& wcs, LogIO& os) const;
    Bool addStokesCoordinate (CoordinateSystem& cSys, Int& axis,  Int& stokesFITSValue,
                              const wcsprm& wcs, const IPosition& shape,
                              LogIO& os) const;
    Bool addLinearCoordinate (CoordinateSystem& cSys, Vector<Int>& axes,
                              const wcsprm& wcs, LogIO& os) const;
    // </group>

// Decode values from WCS structures which are generated via the wcs FITS parser
    // <group>
    Bool directionSystemFromWCS (LogIO& os, MDirection::Types& type, String& errMsg,
                                 const wcsprm& wcs) const;
    Bool frequencySystemFromWCS (LogIO& os, MFrequency::Types& type, String& errMsg,
                                 const wcsprm& wcs) const;
    Bool stokesCoordinateFromWCS (LogIO& os, StokesCoordinate& coord,  
                                  Int& stokesFITSValue, String& errMSg,
                                  const wcsprm& wcs, uInt shape, Bool warnStokes) const;
    // </group>

    // Decode ObsInfo from wcs structure
    ObsInfo getObsInfo(LogIO& os, RecordInterface& header, const wcsprm& wcs) const;

   // Call wcsset
   void setWCS (wcsprm& wcs) const;

    // Decode CD cards from FITS file header (Record interface)
    Bool getCDFromHeader(Matrix<Double>& cd, uInt n, const RecordInterface& header);

    // Decode PC matrix from FITS header (Record interface)
    void getPCFromHeader(LogIO& os, Int& rotationAxis, Matrix<Double>& pc,
                                uInt n, const RecordInterface& header,
                                const String& sprefix);

    // Helper function to convert a wcs structure holding FITS keywords
    // into a Record for later consumption.
    void cardsToRecord (LogIO& os, RecordInterface& rec, char* pHeader) const;
    
    // Fix up Coordinate for zero increments and the like
    // Possibly the wcs FITS parser could do this
    void fixCoordinate(Coordinate& c, LogIO& os) const;
};

} //# NAMESPACE CASA - END

#endif

