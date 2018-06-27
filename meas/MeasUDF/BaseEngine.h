//# BaseEngine.h: Base class for the TaQL UDF conversion engines
//# Copyright (C) 2018
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

#ifndef MEAS_BASEENGINE_H
#define MEAS_BASEENGINE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/measures/TableMeasures/ArrayMeasColumn.h>
#include <casacore/casa/Arrays/Array.h>

namespace casacore {

  // <summary>
  // Abstract base class for the TaQL UDF conversion engines
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="" date="" tests="tMeas.cc">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> EngineBase
  // </prerequisite>

  // <synopsis>
  // DopplerEngine defines Engines (user defined functions) that can be used
  // in TaQL to convert Measures for dopplers.
  // In this way such derived values appear to be ordinary TaQL functions.
  //
  // Doppler conversions require a MeasFrame containing sky direction,
  // epoch and position on earth.
  // In TaQL these functions can be called like:
  // <srcblock>
  //   meas.rv ('TOPO', 1 'm/s', 'LSRK', 'CasA', date(),
  //            [1e6m,1e6m,1e6m], 'WGS84')
  // </srcblock>
  // which converts the dopplers from LSRK to TOPO.
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
  // All such functions return data with type double and unit Hz.
  //
  // Dopplers can be given like:
  //    [v1,v2,...], fromRef
  // where fromRef is the reference type.
  //
  // A doppler can also be a table column which usually knows its type.
  // It can also be an expression (e.g. DOPPLER[0,]) which also knows the type.
  // </synopsis>

  // <motivation>
  // It makes it possible to handle measures in TaQL.
  // </motivation>

  class BaseEngine
  {
  public:
    BaseEngine()
      : itsIsConst (False),
        itsNDim    (-1)
    {}

    virtual ~BaseEngine();

    // Adapt the output shape and dimensionality for possible constant values.
    // It also sets the itsIsConst flag.
    // If the given shape is not empty the shape is set to it and an extra axis
    // is added if nvalues>0 (for e.g. LONLAT).
    // If nvalues=1, the first axis is removed from the shape.
    // Note that the shape might have been set to the column's shape if a
    // measure column is used.
    void adaptForConstant (const IPosition& shapeConstant, uInt nvalues=0);

    // Extend the shape (if not empty) with the engine's shape.
    // If the engine is not const, itsIsConst is cleared.
    void extendBase (const BaseEngine&, Bool removeFirstAxis=False);

    // Get the output shape.
    const IPosition& shape() const
      { return itsShape; }

    // Get the output dimensionality.
    Int ndim() const
      { return itsNDim; }

    // Get the unit of the function's result.
    const Unit& unit() const
      { return itsOutUnit; }  

    // Get the unit of the expression.
    const Unit& inUnit() const
      { return itsInUnit; }
    
    // Tell if the expression is constant.
    Bool isConstant() const
      { return itsIsConst; }

  protected:
    // Let a derived class derive its attributes.
    // The default implementation does nothing.
    virtual void deriveAttr (const Unit& unit, Int nval);

    // Let a derived class set its value type.
    // By default is does nothing.
    virtual void setValueType (Int valueType);
    
    // Let a derived class strip part of the reference type.
    // The default implementation returns the full type string.
    virtual String stripMeasType (const String& type);

    
    //# Data members.
    Bool          itsIsConst;
    IPosition     itsShape;
    Int           itsNDim;    // <0 unknown shape, 0 scalar, >0 known shape
    Unit          itsInUnit;
    Unit          itsOutUnit;
    TableExprNode itsExprNode;
  };

} //end namespace

#endif
