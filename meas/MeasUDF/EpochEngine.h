//# EpochEngine.h: Engine for TaQL UDF Epoch conversions
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

#ifndef MEAS_EPOCHENGINE_H
#define MEAS_EPOCHENGINE_H

//# Includes
#include <casacore/casa/aips.h>
#include<casacore/meas/MeasUDF/PositionEngine.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/measures/TableMeasures/ArrayMeasColumn.h>

namespace casacore {

// <summary>
// Engine for TaQL UDF Epoch conversions
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tMeas.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> EngineBase
// </prerequisite>

// <synopsis>
// EpochEngine defines Engines (user defined functions) that can be used in TaQL
// to convert Measures for epochs.
// In this way such derived values appear to be ordinary TaQL functions.
//
// In TaQL these functions can be called like:
// <srcblock>
//   meas.epoch (toref, time, fromref)
//   meas.last (time, fromref, pos, posref)
// For example,
//   meas.epoch ('UTC', 1e9 s, 'WGS84')
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

// A epoch can also be a table column which usually knows its type.
// It can also be an expression (e.g. EPOCH[0,]) which also knows the type.
// </synopsis>

// <motivation>
// It makes it possible to handle measures in TaQL.
// </motivation>

  class EpochEngine
  {
  public:
    EpochEngine();

    // Get the reference type.
    MEpoch::Types refType() const
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
    Array<Double> getArrayDouble (const TableExprId& id);

    // Get the epochs.
    Array<MEpoch> getEpochs (const TableExprId& id);

    // Handle the argument(s) giving the input epochs and reference type.
    // The epoch can be a column in a table.
    void handleEpoch (PtrBlock<TableExprNodeRep*>& args,
                      uInt& argnr);

    // Handle a epoch reference type.
    // If the reference type is invalid, an exception is only thrown
    // if <src>doThrow=True</src>. In this way a string argument can
    // be an observatory name for a position.
    Bool handleEpochType (TableExprNodeRep* operand, Bool doThrow);

    // Set the MeasConvert object.
    void setConverter (MEpoch::Types toType);

    // Set the possible position engine.
    // It can be done only once.
    void setPositionEngine (PositionEngine& engine);

  private:
    void handleEpochArray (TableExprNodeRep* operand);
    void handleConstant (TableExprNodeRep* operand);

    //# Data members.
    IPosition                   itsShape;
    Int                         itsNDim;
    Unit                        itsUnit;
    MeasFrame                   itsFrame;       //# frame used by converter
    MEpoch::Convert             itsConverter;
    Vector<MEpoch>              itsConstants;
    MEpoch::Types               itsRefType;
    TableExprNode               itsExprNode;
    ArrayMeasColumn<MEpoch>     itsMeasCol;
    PositionEngine*             itsPositionEngine;
  };

} //end namespace

#endif
