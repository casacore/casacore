//# PositionEngine.h: Engine for TaQL UDF Position conversions
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

#ifndef MEAS_POSITIONENGINE_H
#define MEAS_POSITIONENGINE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MCPosition.h>
#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/measures/TableMeasures/ArrayMeasColumn.h>

namespace casacore {

// <summary>
// Engine for TaQL UDF Position conversions
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tMeas.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> EngineBase
// </prerequisite>

// <synopsis>
// PositionEngine defines Engines (user defined functions) that can be used in TaQL
// to convert Measures for positions.
// In this way such derived values appear to be ordinary TaQL functions.
//
// In TaQL these functions can be called like:
// <srcblock>
//   meas.pos (toref, pos, fromref)
//   meas.itrf (pos, fromref)
//   meas.wgs84 (pos, fromref)
// For example,
//   meas.pos ('ITRF', [1e6m,1e6m,1e6m], 'WGS84')
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

// Positions can be given like:
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
// A position can also be a table column which usually knows its type.
// It can also be an expression (e.g. POSITION[0,]) which also knows the type.
// </synopsis>

// <motivation>
// It makes it possible to handle measures in TaQL.
// </motivation>

  class PositionEngine
  {
  public:
    PositionEngine();

    // Get the reference type.
    MPosition::Types refType() const
      { return itsRefType; } 

    // Get the value type. It also gives the nr of output values per position.
    //  0=default, 1=height, 2=angles, 3=xyz
    Int valueType() const
      { return itsValueType; }

    // Get the shape.
    const IPosition& shape() const
      { return itsShape; }

    // Get the dimensionality.
    Int ndim() const
      { return itsNDim; }

    // Tell if the expression is constant.
    Bool isConstant() const
      { return itsConstants.size() > 0; }

    // Get the unit.
    const Unit& unit() const
      { return itsOutUnit; }  

    // Get the values.
    Array<Double> getArrayDouble (const TableExprId& id,
                                  MPosition::Types toRefType,
                                  Int toValueType);

    // Get the positions.
    Array<MPosition> getPositions (const TableExprId& id);

    // Handle the argument(s) giving the input positions and reference type.
    // The position can be a column in a table.
    void handlePosition (Int toValueType,
                         PtrBlock<TableExprNodeRep*>& args,
                         uInt& argnr);

    // Handle a position reference type and optional suffix.
    void handlePosType (TableExprNodeRep* operand);

  private:
    void makeDefaults (const Unit& unit);
    // Make an MPosition from xyz or angles,height.
    MPosition makePosition (const Quantity& qh,
                            const Quantity& q1,
                            const Quantity& q2) const;
    void handleScalars (TableExprNodeRep* e1, TableExprNodeRep* e2,
                        TableExprNodeRep* heightNode);
    void handleObservatory (TableExprNodeRep* operand);
    void handlePosArray (TableExprNodeRep*& operand);
    void handlePosArray (TableExprNodeRep* angles, TableExprNodeRep* height);
    void handleConstant (TableExprNodeRep* operand);
    void handleValues (TableExprNode& operand,
                       const TableExprId& id,
                       Array<MPosition>& positions);

    //# Data members.
    IPosition                      itsShape;
    Int                            itsNDim;
    Unit                           itsInUnit;
    Unit                           itsOutUnit;
    CountedPtr<MPosition::Convert> itsConvert;
    Array<MPosition>               itsConstants;
    MPosition::Types               itsRefType;
    Int                            itsValueType;
                                   //# 0=default, 1=xyz, 2=angles, 3=height
    TableExprNode                  itsExprNode;
    ArrayMeasColumn<MPosition>     itsMeasCol;
  };

} //end namespace

#endif
