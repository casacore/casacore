//# MSWeatherEnums.h: Definitions for the MeasurementSet WEATHER table
//# Copyright (C) 1996,1999,2000
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

#ifndef MS_MSWEATHERENUMS_H
#define MS_MSWEATHERENUMS_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <use visibility=export>

// <summary> Enums for the MeasurementSet WEATHER table </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
// <etymology>
// This class contains the enums for the MeasurementSet WEATHER table
// </etymology>
// <synopsis>
// This class does nothing.  It is merely a container for the enumerations
// used by the MSWeather class.  These enumerations define the
// standard columns, keywords.
// </synopsis>

// <example>
// See the documentation for MeasurementSet for examples on the use of these
// enumerations.
// </example>
// <todo asof="1999/01/22">
// All the todo items which may be related to this class are
// grouped with the todo items for MeasurementSet
// </todo>
//

class MSWeatherEnums {
public:
    // The WEATHER table colums with predefined meaning.
    // Keys: ANTENNA_ID, TIME, INTERVAL.
    enum PredefinedColumns {
    // "True" columns are defined.<BR>
    // TYPE - UNIT - MEASURE
    UNDEFINED_COLUMN=0,
    // Antenna number<BR>
    // Int
    ANTENNA_ID,
    // Interval over which data is relevant <BR>
    // Double - s
    INTERVAL,
    // An MEpoch specifying the midpoint of the time for which data is
    // relevant <BR>
    // Double - s - EPOCH
    TIME,
    // The number of required columns <BR>
    NUMBER_REQUIRED_COLUMNS=TIME,
    // Dew point <BR>
    // Float - K
    DEW_POINT,
    // Flag for dew point <BR>
    // Bool
    DEW_POINT_FLAG,
    // Average column density of water-vapor <BR>
    // Float - m
    H2O,
    // Flag for H2O <BR>
    // Bool
    H2O_FLAG,
    // Average column density of electrons <BR>
    // Float - m^-2
    IONOS_ELECTRON,
    // Flag for IONOS_ELECTRON <BR>
    // Bool
    IONOS_ELECTRON_FLAG,
    // Ambient atmospheric pressure <BR>
    // Float - Pa
    PRESSURE,
    // Flag for pressure <BR>
    // Bool
    PRESSURE_FLAG,
    // Ambient relative humidity <BR>
    // Float - \%
    REL_HUMIDITY, 
    // Flag for rel humidity <BR>
    // Bool
    REL_HUMIDITY_FLAG,
    // Ambient Air Temperature for an antenna <BR>
    // Float - K
    TEMPERATURE,
    // Flag for temperature <BR>
    // Bool
    TEMPERATURE_FLAG,
    // Average wind direction <BR>
    // Float - rad 
    WIND_DIRECTION,
    // Flag for wind direction <BR>
    // Bool 
    WIND_DIRECTION_FLAG,
    // Average wind speed <BR>
    // Float - m/s
    WIND_SPEED,
    // Flag for  wind speed <BR>
    // Bool
    WIND_SPEED_FLAG,
    //
    // Not a column, but just a final enum specifying the number of enums.
    NUMBER_PREDEFINED_COLUMNS=WIND_SPEED_FLAG
    };
  
    // Keywords with a predefined meaning
    enum PredefinedKeywords {
    //
    // "True" keywords are defined. 
    UNDEFINED_KEYWORD=0,
    //
    // Not a keyword, but just a final enum specifying the number of enums.
    NUMBER_PREDEFINED_KEYWORDS=0
    };
};

} //# NAMESPACE CASACORE - END

#endif
