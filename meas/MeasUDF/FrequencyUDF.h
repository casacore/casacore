//# FrequencyUDF.h: TaQL UDFs for Frequency conversions
//# Copyright (C) 2016
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef MEAS_FREQUENCYUDF_H
#define MEAS_FREQUENCYUDF_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/meas/MeasUDF/FrequencyEngine.h>
#include <casacore/meas/MeasUDF/RadialVelocityEngine.h>
#include <casacore/meas/MeasUDF/DopplerEngine.h>
#include <casacore/meas/MeasUDF/DirectionEngine.h>
#include <casacore/meas/MeasUDF/EpochEngine.h>
#include <casacore/meas/MeasUDF/PositionEngine.h>
#include <casacore/tables/TaQL/UDFBase.h>

namespace casacore {

// <summary>
// TaQL UDFs for Frequency conversions.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tMeas.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> UDFBase
// </prerequisite>

// <synopsis>
// FrequencyUDF defines UDFs (user defined functions) that can be used in TaQL
// to convert Measures for frequencys.
// Special functions exist to convert to hourangle and azimuth/elevation.
// In this way such derived values appear to be ordinary TaQL functions.
//
// A function is called like:
// <srcblock>
//   meas.dir (toref, dir, time, pos)
//   meas.dir (toref, dir, time, pos)
// For example,
//   meas.dir ('B1950', [2rad,1rad])
//   meas.dir ('B1950', [[2rad,1rad], 'J2000'])
//   meas.dir ('APP', 'MOON', TIME, [[5d12m, 52deg, 11m], 'WGS84'])
//   meas.dir ('APP', 'MOON', TIME, POSITION)
// Or
//   meas.dir (toref, fromref, dir, pos, time)
// </srcblock>
// <ul>
//  <li>
//  <src>toref</src> is a single constant string.
//  <li>
// <src>dir</src> can have various value types. A single numeric array is
// a series of RA,DEC in J2000. If given as a set, the last argument of the
// set can be the reference types of the values in the set. The values can
// be strings (indicating planetary objects) or value pairs giving lon,lat.
// The default reference type is J2000. 
// </ul>
// All functions have data type double and unit radian.
// </synopsis>

// <motivation>
// It makes it possible to handle measures in TaQL.
// </motivation>

  class FrequencyUDF: public UDFBase
  {
  public:
    // Define the possible function types.
    enum FuncType {FREQUENCY, REST, SHIFT};

    // Create for the given function type.
    explicit FrequencyUDF (FuncType);

    // Function to create an object.
    static UDFBase* makeFREQ (const String&);
    static UDFBase* makeREST (const String&);
    static UDFBase* makeSHIFT (const String&);

    // Setup the object.
    virtual void setup (const Table&, const TaQLStyle&);

    // Get the value.
    virtual Double getDouble (const TableExprId& id);
    virtual MArray<Double> getArrayDouble (const TableExprId& id);

  private:
    // Handle a radial velocity or optionally doppler for REST conversion.
    // It returns True if Doppler is used.
    Bool handleRadVelDoppler (uInt& argnr, Bool mustRadVel);
    
    //# Data members.
    FrequencyEngine      itsEngine;
    DopplerEngine        itsDopplerEngine;        //# for frequency shift
    RadialVelocityEngine itsRadVelEngine;         //# for rest frequency
    DirectionEngine      itsDirectionEngine;
    EpochEngine          itsEpochEngine;
    PositionEngine       itsPositionEngine;
    FuncType             itsType;
    MFrequency::Types    itsRefType;
  };

} //end namespace

#endif
