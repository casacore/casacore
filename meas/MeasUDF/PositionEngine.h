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

#ifndef MEAS_POSITIONENGINE_H
#define MEAS_POSITIONENGINE_H

//# Includes
#include <casacore/casa/aips.h>
#include<casacore/meas/MeasUDF/MeasEngine.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MCPosition.h>
#include <casacore/measures/Measures/MeasConvert.h>

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
  // PositionEngine defines Engines (user defined functions) that can be used
  // in TaQL to convert Measures for positions.
  // In this way such derived values appear to be ordinary TaQL functions.
  //
  // In TaQL these functions can be called like:
  // <srcblock>
  //   meas.pos (toref, pos)
  //   meas.wgs (pos)
  //   meas.itrfxyz (pos)
  // For example,
  //   meas.pos ('ITRF', [1e6m,1e6m,1e6m], 'WGS84')
  // </srcblock>
  // <ul>
  //  <li> <src>toref</src> is a single constant string defining the 
  //       reference frame to convert to. Note it should be omitted for
  //       the functions (e.g., meas.wgs) with an implicit destination
  //       reference frame.
  //       The reference type WGS84 or ITRF can optionally have the suffix
  //       XYZ, LLH, LL (or LONLAT) and H (or HEIGHT) telling how the result
  //       should be returned.
  //  <li> <src>pos</src> specifies the position(s) which can be done in
  //       various ways.
  //   <ul>
  //    <li> An array of positions given as xyz, as lonlat, as lon-lat-height
  //         or as height. The latter is taken towards the pole.
  //         Note that specifying as lon-lat-height precludes use of units
  //         (angle and length units cannot be mixed in a TaQL value).
  //         It can be given as a single list or a multi-dim array.
  //         If given as lonlat it can be followed by an array defining
  //         the height for each lon,lat pair (their sizes should match).
  //         Finally it can be followed by a string defining the source
  //         reference type, which defaults to ITRF for x,y,z and WGS84
  //         for lon,lat.
  //         The source reference type can contain the suffix XYZ, LLH, LL
  //         or H to tell how the values are specified. If no suffix is given,
  //         it is derived from the unit of the first value (angle means LL,
  //         length means XYZ).
  //    <li> If a single constant position is used, it can be given as
  //         1, 2 or 3 scalar values, optionally followed by the source
  //         reference type. If x,y,z or lon,lat or lon,lat,h or h is given
  //         is derived in the same way as above.
  //    <li> The name of a column in a table or a subset of it such as
  //         <src>POSITION[0,]</src>. Often this is a TableMeasures column
  //         which is recognized as such, also its source reference frame.
  //         If such a column is used in a expression, it will not be
  //         recognized as a TableMeasures column and its reference frame
  //         should be given.
  //    <li> As a list containing (case-insensitive) names of known
  //         observatories such as 'WSRT' or 'VLA'.
  //   </ul>
  // </ul>
  //
  // The result of the function is an array with shape [2|3,pos] if
  // lon,lat(,h) or x,y,z is returned and shape [pos] if height is returned.
  // The last element is the shape of the position argument. It is omitted if
  // it has length 1. In such a case getting the height results in a scalar.
  // </synopsis>

  // <example>
  // <srcblock>
  // // Get the WGS84 lon,lat of WSRT (in degrees).
  //   meas.wgsll ('WSRT') deg
  // // Get the ITRF x,y,z of WSRT and VLA.
  //   meas.itrfxyz (['WSRT', 'VLA'])
  // </srcblock>
  // </example>

  // <motivation>
  // It makes it possible to handle measures in TaQL.
  // </motivation>

  class PositionEngine: public MeasEngine<MPosition>
  {
  public:
    PositionEngine();

    virtual ~PositionEngine();

    // Get the value type. It also gives the nr of output values per position.
    //  0=default, 1=height, 2=angles, 3=xyz
    Int valueType() const
      { return itsValueType; }

    // Get the values.
    Array<Double> getArrayDouble (const TableExprId& id,
                                  MPosition::Types toRefType,
                                  Int toValueType);

    // Get the positions.
    Array<MPosition> getPositions (const TableExprId& id);

    // Handle the argument(s) giving the input positions and reference type.
    // The position can be a column in a table.
    void handlePosition (Int toValueType,
                         const std::vector<TENShPtr>& args,
                         uInt& argnr);

  private:
    virtual String stripMeasType (const String& type);
    virtual void deriveAttr (const Unit& unit, Int nval);
    // Let a derived class set its value type.
    // By default is does nothing.
    virtual void setValueType (Int valueType);
    
    // Make an MPosition from xyz or height,angles.
    MPosition makePosition (const Quantity& qh,
                            const Quantity& q1,
                            const Quantity& q2) const;
    void handleScalars (const TENShPtr& e1, const TENShPtr& e2,
                        const TENShPtr& e3, Int nval);
    void handleObservatory (const TENShPtr& operand);
    void handlePosArray (const TENShPtr& angles, const TENShPtr& height);
    virtual void handleValues (TableExprNode& operand,
                               const TableExprId& id,
                               Array<MPosition>& positions);

    //# Data members.
    //# 0=unknown, 1=height, 2=angles, 3=xyz, -3=angles,height
    Int  itsValueType;
  };

} //end namespace

#endif
