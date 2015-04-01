//# SDWeatherFiller.h: fills the WEATHER table for the SDFITS filler
//# Copyright (C) 2000
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

#ifndef MS_SDWEATHERHANDLER_H
#define MS_SDWEATHERHANDLER_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/RecordField.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MeasurementSet;
class MSWeather;
class MSWeatherColumns;
class Record;

template <class T> class Vector;

// <summary>
// </summary>

// <use visibility=local>   or   <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> SomeClass
//   <li> SomeOtherClass
//   <li> some concept
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <templating arg=T>
//    <li>
//    <li>
// </templating>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>

class SDWeatherHandler
{
public:
    // default ctor is not attached to a MS and hence is useless until attached
    SDWeatherHandler();

    // attach this to a MS - mark fields in row as handled
    SDWeatherHandler(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // copy ctor
    SDWeatherHandler(const SDWeatherHandler &other);

    ~SDWeatherHandler() {clearAll();}

    // assignment operator, uses copy semantics
    SDWeatherHandler &operator=(const SDWeatherHandler &other);

    // attach to a MS, mark fields in row as handled
    void attach(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // reset internals given indicated row, use the same MS
    void resetRow(const Record &row);
    
    // fill - a new row is added as necessary, there is no lookback to see if a row could be
    // reused.  Only the current row might be reused.
    void fill(const Record &row, Int antennaId, Double time, Vector<Double> &timeRange);
private:
    MSWeather *msWeather_p;
    MSWeatherColumns *msWeatherCols_p;

    Int rownr_p;

    Int humidityId_p, tambientId_p, pressureId_p, dewpointId_p, windspeeId_p, winddireId_p;

    // additional fields from an SDFITS file that had a previous life as a MS
    RORecordFieldPtr<Float> H2OField_p, ionosElectronField_p, pressureField_p, 
	humidityField_p, temperatureField_p, windDirField_p, windSpeedField_p;
    RORecordFieldPtr<Double> timeField_p, intervalField_p;

    // cleanup everything
    void clearAll();

    // cleanup row-related stuff
    void clearRow();

    // initialize everything
    void initAll(MeasurementSet &ms, Vector<Bool> &handledCols, const Record &row);

    // intialize the row related stuff
    void initRow(Vector<Bool> &handledCols, const Record &row);
};


} //# NAMESPACE CASACORE - END

#endif


