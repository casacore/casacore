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

#ifndef MEAS_DIRECTIONENGINE_H
#define MEAS_DIRECTIONENGINE_H

//# Includes
#include <casacore/casa/aips.h>
#include<casacore/meas/MeasUDF/MeasEngine.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MCDirection.h>
#include <casacore/measures/Measures/MeasConvert.h>

namespace casacore {

  //# Forward declarations
  class EpochEngine;
  class PositionEngine;

  
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
  // DirectionEngine defines Engines (user defined functions) that can be
  // used in TaQL to convert Measures for directions.
  // In this way such derived values appear to be ordinary TaQL functions.
  //
  // In TaQL these functions can be called like:
  // <srcblock>
  //   meas.dir    (toref, directions, epochs, positions)
  //   meas.j2000  (directions, epochs, positions)
  //   meas.dircos (toref, directions, epochs, positions)
  //   meas.riset  (directions, epochs, positions)
  // </srcblock>
  // The first two result in angles, the third in direction cosines, while
  // the fourth returns the rise/set time of sources as datetimes.
  // Note that the second form is a shorthand for
  // <src>meas.dir('j2000', ...)</src>. There are more such functions.
  // The exact number of arguments depends on how they are specified.
  // <ul>
  //  <li> <src>toref</src> is a single constant string defining the 
  //       reference frame to convert to. Note it should be omitted for
  //       the functions (e.g., meas.j2000) with an implicit destination
  //       reference frame.
  //  <li> <src>directions</src> is one or more directions which can be
  //       given in several ways.
  //   <ul>
  //    <li> An array of directions, each 2 angles or 3 directions cosines.
  //         It can be given as a single list or a multi-dim array. The choice
  //         between angles and direction cosines is based on the size of the
  //         first dimension. If divisible by 2, it is angles, by 3 is
  //         direction cosines, otherwise an error. Thus a list of 6 elements
  //         defines 3 directions with 2 angles each (default in radians).
  //         <br>It can be followed by a string defining the source
  //         reference frame. It defaults to 'J2000'.
  //    <li> If a single constant direction is used, it can be given as
  //         2 (for angles) or 3 (for direction cosines) scalar values,
  //         followed by the optional source reference frame.
  //    <li> The name of a column in a table or a subset of it such as
  //         <src>DELAY_DIR[0,]</src>. Often this is a TableMeasures column
  //         which is recognized as such, also its source reference frame.
  //         If such a column is given as part of an expression, it will not
  //         be recognized as a TableMeasures column and its reference frame
  //         should be given.
  //    <li> As a list of (case-insensitive) names of known sources
  //         such as 'CasA' or 'SUN'.
  //   </ul>
  //  <li> <src>epochs</src> can be given as shown in class EpochEngine.
  //  <li> <src>positions</src> can be given as shown in class PositionEngine.
  // </ul>
  // Note that epochs and positions are only needed if required by the
  // conversion from source reference frame to destination reference frame.
  // For example, J2000 to/from APP needs them, but not J2000 to/from B1950.
  //
  // The result of the function is an array with shape [2|3,dir,epoch,pos]
  // where the last 3 elements are the shapes of these arguments. They are
  // omitted if all of them have length 1.
  //
  // Futhermore, it is possible to get the rise/set date/time of a source
  // given the source direction, date and position on earth. These functions
  // return data with type double and unit d (day).
  // If the source is visible all day, the rise time is 0 and set time 1.
  // If the source is not visible at all, the rise time is 1 and set time 0.
  // For the sun and the moon it is possible to add a suffix to the name
  // telling if and which edge and twilight should be used. For the sun and
  // moon the default is -UR (the upper edge with refraction correction).
  // </synopsis>

  // <example>
  // <srcblock>
  // // Get rise/set time of the upper edge of the sun in the coming month
  // // at the WSRT site.
  //   meas.riseset ('SUN', date()+[0:31], 'WSRT')
  // // Get the apparent coordinates of CasA for the given date and position.
  //   meas.dircos ('APP', 'CasA', 12mar2015, [52deg,5deg])
  // // Get the J2000 coordinates (in degrees) of CygA.
  //   meas.j2000 ('cyga') deg
  // </srcblock>
  // </example>

  // <motivation>
  // It makes it possible to easily handle measures in TaQL.
  // </motivation>

  class DirectionEngine: public MeasEngine<MDirection>
  {
  public:
    DirectionEngine();

    virtual ~DirectionEngine();

    // Get the values.
    // The first bool tells if rise/set times have to be calculated.
    // The second bool tells if direction cosines have to be calculated.
    Array<double> getArrayDouble (const TableExprId& id, bool riseSet,
                                  bool asDirCos);

    // Get the directions.
    Array<MDirection> getDirections (const TableExprId& id);

    // Handle the argument(s) giving the input directions and reference type.
    // The direction can be a column in a table.
    void handleDirection (const std::vector<TENShPtr>& args,
                          uint32_t& argnr, bool riseSet, bool asDirCos);

    // Set the MeasConvert object.
    void setConverter (MDirection::Types toType);

    // Set the possible epoch engine.
    // It can be done only once.
    void setEpochEngine (EpochEngine& engine);

    // Set the possible position engine.
    // It can be done only once.
    void setPositionEngine (PositionEngine& engine);

  private:
    void handleScalars (const TENShPtr& e1, const TENShPtr& e2,
                        const TENShPtr& e3);
    void handleNames (const TENShPtr& operand);
    virtual void handleValues (TableExprNode& operand,
                               const TableExprId& id,
                               Array<MDirection>& directions);

    // Calucate the rise and set time of a source for a given position and
    // epoch. Argument <src>h</src> defines the possible edge of sun/moon.
    void calcRiseSet (const MDirection& dir,
                      const MPosition& pos,
                      const MEpoch& epoch,
                      double h,
                      double& rise, double& set);
    int fillRiseSet (double epoch,
                     const MDirection& dir,
                     double lat,
                     double h,
                     const MEpoch& off,
                     double* rise, double* set);

    //# Data members.
    MeasFrame                       itsFrame;       //# frame used by converter
    MDirection::Convert             itsConverter;
    Vector<double>                  itsH;           //# diff for sun or moon
    EpochEngine*                    itsEpochEngine;
    PositionEngine*                 itsPositionEngine;
  };

} //end namespace

#endif
