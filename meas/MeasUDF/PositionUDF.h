//# PositionUDF.h: TaQL UDFs for Position conversions
//# Copyright (C) 2011
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

#ifndef MEAS_POSITIONUDF_H
#define MEAS_POSITIONUDF_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/meas/MeasUDF/PositionEngine.h>
#include <casacore/tables/TaQL/UDFBase.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MCPosition.h>
#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/measures/TableMeasures/ArrayMeasColumn.h>

namespace casacore {

// <summary>
// TaQL UDFs for Position conversions.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tMeas.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> UDFBase
// </prerequisite>

// <synopsis>
// PositionUDF defines UDFs (user defined functions) that can be used in TaQL
// to convert Measures for positions.
// In this way such derived values appear to be ordinary TaQL functions.
//
// A function is called like:
// <srcblock>
//   meas.pos (toref, pos)
// For example,
//   meas.dir ('ITRF', [1rad,1rad], 'WGS84')
// </srcblock>
// <ul>
//  <li>
//  <src>toref</src> is a single constant string.
//  <li>
// <src>pos</src> can have various value types. A single numeric array is
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

  class PositionUDF: public UDFBase
  {
  public:
    // Define the possible function types.
    enum FuncType {POS, ITRFXYZ, ITRFLL, ITRFH, WGSXYZ, WGSLL, WGSH};

    explicit PositionUDF (FuncType);

    // Function to create an object.
    static UDFBase* makePOS     (const String&);
    static UDFBase* makeITRFXYZ (const String&);
    static UDFBase* makeITRFLL  (const String&);
    static UDFBase* makeITRFH   (const String&);
    static UDFBase* makeWGSXYZ  (const String&);
    static UDFBase* makeWGSLL   (const String&);
    static UDFBase* makeWGSH    (const String&);

    // Setup the object.
    virtual void setup (const Table&, const TaQLStyle&);

    // Get the value.
    virtual Double getDouble (const TableExprId& id);
    virtual Array<Double> getArrayDouble (const TableExprId& id);

  private:
    //# Data members.
    PositionEngine   itsEngine;
    FuncType         itsType;
    MPosition::Types itsRefType;
    Int              itsValueType;
  };

} //end namespace

#endif
