//# DirectionEngine.h: Engine for TaQL UDF Direction conversions
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

#ifndef MEAS_DIRECTIONENGINE_H
#define MEAS_DIRECTIONENGINE_H

//# Includes
#include <casacore/casa/aips.h>
#include<casacore/meas/MeasUDF/EpochEngine.h>
#include<casacore/meas/MeasUDF/PositionEngine.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MCDirection.h>
#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/measures/TableMeasures/ArrayMeasColumn.h>

namespace casacore {

// <summary>
// Engine for TaQL UDF Direction conversions
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tMeas.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> EngineBase
// </prerequisite>

// <synopsis>
// DirectionEngine defines Engines (user defined functions) that can be used
// in TaQL to convert Measures for directions.
// In this way such derived values appear to be ordinary TaQL functions.
//
// In TaQL these functions can be called like:
// <srcblock>
//   meas.dir ('APP', 'MOON', date(), [1e6m,1e6m,1e6m], 'WGS84')
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
// All such functions return data with type double and unit radian.
//
// Futhermore, it is possible to get the rise/set date/time of a source given
// the source direction, position on earth, and date. These functions
// return data with type double and unit d (day).
// If the source is visible all day, the rise time is 0 and set time is 1.
// If the source is not visible at all, the rise time is 1 and set time is 0.
// For example:
//   meas.riseset ('SUN', date(), 'WSRT')

// Directions can be given like:
//    [x1,y1,z1,x2,y2,z2,...], fromRef
//    [lon1,lat1,lon2,lat2,...], fromRef
//    [lon1,lat1,lon2,lat2,...], [h1,h2,...], fromRef
// where fromRef is the reference type optionally followed by _xxx
// where xxx can be 'xyz' or 'll' to specify if the values are given as xyz
// or as lon,lat.
// If xxx is not given, it will be derived from the unit type of the values
// (length means xyz, angle means lon,lat with default height is 0).
// If xxx nor units are given, 3 values means xyz and 2 values means lon,lat.
// If heights are also given, xxx must be 'll' if it is also given.
//
// A direction can also be a table column which usually knows its type.
// It can also be an expression (e.g. DIRECTION[0,]) which also knows the type.
// </synopsis>

// <motivation>
// It makes it possible to handle measures in TaQL.
// </motivation>

  class DirectionEngine
  {
  public:
    DirectionEngine();

    // Get the reference type.
    MDirection::Types refType() const
      { return itsRefType; } 

    // Get the shape.
    const IPosition& shape() const
      { return itsShape; }

    // Get the dimensionality.
    Int ndim() const
      { return itsNDim; }

    // Tell if the expression is constant.
    Bool isConstant() const;

    // Get the unit.
    const Unit& unit() const
      { return itsUnit; }  

    // Get the values.
    // The Bools tell if rise/set times have to be calculated.
    Array<Double> getArrayDouble (const TableExprId& id, Bool riseSet);

    // Get the directions.
    Array<MDirection> getDirections (const TableExprId& id);

    // Handle the argument(s) giving the input directions and reference type.
    // The direction can be a column in a table.
    void handleDirection (PtrBlock<TableExprNodeRep*>& args,
                          uInt& argnr, Bool riseSet);

    // Handle a direction reference type.
    void handleDirType (TableExprNodeRep* operand);

    // Set the MeasConvert object.
    void setConverter (MDirection::Types toType);

    // Set the possible epoch engine.
    // It can be done only once.
    void setEpochEngine (EpochEngine& engine);

    // Set the possible position engine.
    // It can be done only once.
    void setPositionEngine (PositionEngine& engine);

  private:
    void handleScalars (TableExprNodeRep* e1, TableExprNodeRep* e2);
    void handleNames (TableExprNodeRep* operand);
    void handleDirArray (TableExprNodeRep*& operand);
    void handleConstant (TableExprNodeRep* operand);
    void handleValues (TableExprNode& operand,
                       const TableExprId& id,
                       Array<MDirection>& directions);

    // Calucate the rise and set time of a source for a given elevation,
    // latitude, and epoch.
    void calcRiseSet (double dec,
                      double el, double lat,
                      double ra, double epoch,
                      double& rise, double& set) const;

    //# Data members.
    IPosition                       itsShape;
    Int                             itsNDim;
    Unit                            itsUnit;
    MeasFrame                       itsFrame;       //# frame used by converter
    MDirection::Convert             itsConverter;
    Vector<MDirection>              itsConstants;
    MDirection::Types               itsRefType;
    TableExprNode                   itsExprNode;
    ArrayMeasColumn<MDirection>     itsMeasCol;
    EpochEngine*                    itsEpochEngine;
    PositionEngine*                 itsPositionEngine;
  };

} //end namespace

#endif
