//# EarthMagneticUDF.h: TaQL UDFs for EarthMagnetic conversions
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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef MEAS_EARTHMAGNETICUDF_H
#define MEAS_EARTHMAGNETICUDF_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/meas/MeasUDF/EarthMagneticEngine.h>
#include<casacore/meas/MeasUDF/EpochEngine.h>
#include<casacore/meas/MeasUDF/PositionEngine.h>
#include<casacore/meas/MeasUDF/DirectionEngine.h>
#include <casacore/tables/TaQL/UDFBase.h>

namespace casacore {

// <summary>
// TaQL UDFs for EarthMagnetic conversions.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tMeas.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> UDFBase
// </prerequisite>

// <synopsis>
// EarthMagneticUDF defines UDFs (user defined functions) that can be used
// in TaQL to convert Measures for earthMagnetics.
// Special functions exist to convert to hourangle and azimuth/elevation.
// In this way such derived values appear to be ordinary TaQL functions.
//
// A function is called like:
// <srcblock>
//   meas.em (toref, em, time, pos)
//   meas.em (toref, em, time, pos)
// For example,
//   meas.em ('J2000', 'IGRF', TIME, [[5d12m, 52deg, 11m], 'WGS84'])
// </srcblock>
// <ul>
//  <li>
//  <src>toref</src> is a single constant string giving the
//  EarthMagnetic reference type. It is not possible to use IGRF for
//  the 'toref' type.
//  <li>
//  <src>em</src> is a value array followed by the reference type.
//  Type IGRF is the IGRF, for which no value array should be given.
// </ul>
// All functions have data type double and unit Tesla.
// </synopsis>

// <motivation>
// It makes it possible to handle measures in TaQL.
// </motivation>

  class EarthMagneticUDF: public UDFBase
  {
  public:
    // Define the possible function types.
    enum FuncType {EMXYZ, EMANG, EMLEN, IGRFXYZ, IGRFANG, IGRFLEN,
                   IGRFLOS, IGRFLONG};

    // Create for the given function type.
    explicit EarthMagneticUDF (FuncType);

    // Function to create an object.
    static UDFBase* makeEMXYZ    (const String&);
    static UDFBase* makeEMANG    (const String&);
    static UDFBase* makeEMLEN    (const String&);
    static UDFBase* makeIGRFXYZ  (const String&);
    static UDFBase* makeIGRFANG  (const String&);
    static UDFBase* makeIGRFLEN  (const String&);
    static UDFBase* makeIGRFLOS  (const String&);
    static UDFBase* makeIGRFLONG (const String&);

    // Setup the object.
    virtual void setup (const Table&, const TaQLStyle&);

    // Get the value.
    virtual double getDouble (const TableExprId& id);
    virtual MArray<double> getArrayDouble (const TableExprId& id);

  private:
    //# Data members.
    EarthMagneticEngine   itsEngine;
    DirectionEngine       itsDirectionEngine;
    EpochEngine           itsEpochEngine;
    PositionEngine        itsPositionEngine;
    FuncType              itsType;
    int32_t                   itsValueType;
    MEarthMagnetic::Types itsRefType;
  };

} //end namespace

#endif
