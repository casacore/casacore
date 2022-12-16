//# EarthMagneticEngine.h: Engine for TaQL UDF EarthMagnetic conversions
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

#ifndef MEAS_EARTHMAGNETICENGINE_H
#define MEAS_EARTHMAGNETICENGINE_H

//# Includes
#include <casacore/casa/aips.h>
#include<casacore/meas/MeasUDF/MeasEngine.h>
#include <casacore/measures/Measures/EarthMagneticMachine.h>
#include <casacore/measures/Measures/MEarthMagnetic.h>
#include <casacore/measures/Measures/MCEarthMagnetic.h>
#include <casacore/measures/Measures/MeasConvert.h>

namespace casacore {


  //# Forward declarations
  class DirectionEngine;
  class EpochEngine;
  class PositionEngine;

  
  // <summary>
  // Engine for TaQL UDF EarthMagnetic conversions
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="" date="" tests="tMeas.cc">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> EngineBase
  // </prerequisite>

  // <synopsis>
  // EarthMagneticEngine defines Engines (user defined functions) that can be
  // used in TaQL to convert EarthMagnetic values from one frame to another
  // or to calculate them from the IGRF model.
  // In this way such derived values appear to be ordinary TaQL functions.
  //
  // In TaQL these functions can be called like:
  // <srcblock>
  //   meas.em   (toref, emvalues, epochs, positions)
  //   meas.igrf (toref, heights, directions, epochs, positions)
  //   meas.igrflos  (heights, directions, epochs, positions)
  //   meas.igrflong (heights, directions, epochs, positions)
  // </srcblock>
  // The first one converts the given EarthMagnetic values to the 'toref'
  // frame for all epoch and positions (their Cartesian product).
  // The second one calculates the IGRF model values in the 'toref' frame
  // for all heights, directions, epochs and positions.
  // The 3rd and 4th return the model value along the line-of-sight cq. as
  // longitude for all heights, directions, epochs and positions.
  // The first two function names can be followed by XYZ, ANGLES or LENGTH
  // to return the values according to the suffix.
  // <ul>
  //  <li> <src>toref</src> is a single constant string.
  //       If not given, it defaults to ITRF. Note that 'toref' can also be
  //       given for function IGRFLOS and IGRFLONG, but is neglected.
  //  <li> <src>emvalues</src> gives the EarthMagnetic values to be converted.
  //       They can be given in various forms.
  //   <ul>
  //    <li> An array of positions given as xyz or as lon-lat-flux
  //         Note that specifying as lon-lat-flux precludes use of units
  //         (angle and length units cannot be mixed in a TaQL value),
  //         while xyz must have a flux unit (e.g., nT). It means that the
  //         unit determines if xyz or lon-lat-flux is given.
  //         It can be given as a single list or a multi-dim array.
  //         It can be followed by a string defining the source
  //         reference type, which defaults to ITRF.
  //    <li> If a single constant position is used, it can be given as
  //         3 scalar values, optionally followed by the source
  //         reference type. The unit defines iff x,y,z or lon-lat-flux is
  //         given.
  //    <li> The name of a column in a table or a subset of it such as
  //         <src>EMVAL[0,]</src>. Often this is a TableMeasures column
  //         which is recognized as such, also its source reference frame.
  //         If such a column is used in a expression, it will not be
  //         recognized as a TableMeasures column and its reference frame
  //         should be given.
  //   </ul>
  //  <li> <src>heights</src> is one or more real values giving the heights
  //       above the earth at which the model has to be calculated.
  //       Default unit is m.
  //  <li> <src>directions</src> defines the directions in which the model
  //       has to be calculated. They can be given in all forms as described
  //       in class DirectionEngine.
  //  <li> <src>epochs</src> can be given as shown in class EpochEngine.
  //  <li> <src>positions</src> can be given as shown in class PositionEngine.
  // </ul>
  // All functions return data with type double and unit nT. Unit rad is
  // returned for functions with the ANGLES suffix.
  //
  // The result of a conversion is an array with shape [1|2|3,em,epoch,pos].
  // The model calculations result has shape [1|2|3,h,dir,epoch,pos]
  // The last 3 or 4 elements are the shapes of these arguments. They are
  // omitted if all of them have length 1.
  // </synopsis>

  // <example>
  // <srcblock>
  // // Get IGRF model value for today at the WSRT at 200 km height
  // // in the direction of the SUN.
  //   meas.igrf (200km, 'SUN', date(), 'WSRT')
  // // Similar, but the flux along the line of sight.
  //   meas.igrflos (200km, 'SUN', date(), 'WSRT')
  // // Convert an earthmagnetic value from ITRF to J2000 angles.
  //   meas.emang ('J2000', 677, 45441, 29517, date(), 'WSRT')
  // </srcblock>
  // </example>

  // <motivation>
  // It makes it possible to handle measures in TaQL.
  // </motivation>

  class EarthMagneticEngine: public MeasEngine<MEarthMagnetic>
  {
  public:
    EarthMagneticEngine();

    virtual ~EarthMagneticEngine();

    // Get the value type. It also gives the nr of output values per position.
    //  0=default, 1=length (in tesla), 2=angles (in radians)
    Int valueType() const
      { return itsValueType; }

    // Get the values.
    Array<Double> getArrayDouble (const TableExprId& id);

    // Handle the argument(s) giving the input earthMagnetics or direction
    // and reference type. The earthMagnetic can be a column in a table.
    // Note that direction (or height) can only be given for reftype IGRF.
    void handleEarthMagnetic (std::vector<TENShPtr>& args,
                              uInt& argnr);

    // Handle the heights argument.
    void handleHeight (TENShPtr& operand);

    // Set the MeasConvert object.
    // Set the possible epoch engine.
    // It can be done only once.
    void setEpochEngine (EpochEngine& engine);

    // Set the possible position engine.
    // It can be done only once.
    void setPositionEngine (PositionEngine& engine);

    // Set the possible direction engine.
    // It can be done only once.
    void setDirectionEngine (DirectionEngine& engine);

    // Set the types of the result.
    void set (MEarthMagnetic::Types toRefType, Int toValueType,
              Bool asLOS, Bool asLong, Bool useModel);

  private:
    // Strip a possible suffix from the reference type.
    virtual String stripMeasType (const String& type);
    virtual void deriveAttr (const Unit& unit, Int nval);
    virtual void setValueType (Int valueType);
    // Make an MEarthMagnetic from xyz or length,angles.
    MEarthMagnetic makeEarthMagnetic (const Quantity& qh,
                                      const Quantity& q1,
                                      const Quantity& q2) const;
    void handleScalars (const TENShPtr& e1, const TENShPtr& e2,
                        const TENShPtr& e3);
    virtual void handleValues (TableExprNode& operand,
                               const TableExprId& id,
                               Array<MEarthMagnetic>& earthMagnetics);
    Array<MEarthMagnetic> getEarthMagnetics (const TableExprId& id);
    Array<Double> getHeights (const TableExprId& id);
    void copyEM (const MVEarthMagnetic& em, double*& outPtr);
    void copyLLEM (EarthMagneticMachine& emm, double*& outPtr);

    //# Data members.
    MeasFrame                       itsFrame;      //# frame used by converter
    EarthMagneticMachine            itsMachine;    //# model calculations
    MEarthMagnetic::Convert         itsConverter;
    Int                             itsValueType;
                                    //# 3=xyz flux, -3=angle,flux
    Int                             itsToValueType;
    Bool                            itsAsLOS;      //# get as line-of-sight?
    Bool                            itsAsLong;     //# get as longitude?
    Bool                            itsUseModel;   //# use model calculation?
    Bool                            itsConvertModel; //# model to non-ITRF?
    EpochEngine*                    itsEpochEngine;
    PositionEngine*                 itsPositionEngine;
    DirectionEngine*                itsDirectionEngine;
  };

} //end namespace

#endif
