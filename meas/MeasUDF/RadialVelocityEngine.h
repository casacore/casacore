//# RadialVelocityEngine.h: Engine for TaQL UDF RadialVelocity conversions
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

#ifndef MEAS_RADIALVELOCITYENGINE_H
#define MEAS_RADIALVELOCITYENGINE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/meas/MeasUDF/MeasEngine.h>
#include <casacore/measures/Measures/MRadialVelocity.h>
#include <casacore/measures/Measures/MCRadialVelocity.h>
#include <casacore/measures/Measures/MeasConvert.h>

namespace casacore {

  //# Forward declarations
  class DopplerEngine;
  class DirectionEngine;
  class EpochEngine;
  class PositionEngine;

  
  // <summary>
  // Engine for TaQL UDF RadialVelocity conversions
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="" date="" tests="tMeas.cc">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> EngineBase
  // </prerequisite>

  // <synopsis>
  // RadialVelocityEngine defines Engines (user defined functions) that can be used
  // in TaQL to convert Measures for radial velocities.
  // In this way such derived values appear to be ordinary TaQL functions.
  //
  // RadialVelocity conversions require a MeasFrame containing sky direction,
  // epoch and position on earth.
  // In TaQL these functions can be called like:
  // <srcblock>
  //   meas.rv ('TOPO', 1 'm/s', 'LSRK', 'CasA', date(),
  //            [1e6m,1e6m,1e6m], 'WGS84')
  // </srcblock>
  // which converts the radial velocity from LSRK to TOPO.
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
  // Radial velocities can be given like:
  //    [v1,v2,...], fromRef
  // where fromRef is the reference type.
  //
  // A radial velocity can also be a table column which usually knows its type.
  // It can also be an expression (e.g. RADIALVELOCITY[0,]) which also knows the type.
  // </synopsis>

  // <motivation>
  // It makes it possible to handle measures in TaQL.
  // </motivation>

  class RadialVelocityEngine: public MeasEngine<MRadialVelocity>
  {
  public:
    RadialVelocityEngine();

    // Get the values.
    Array<Double> getArrayDouble (const TableExprId& id);

    // Get the radial velocities.
    Array<MRadialVelocity> getRadialVelocities (const TableExprId& id);

    // Handle the argument(s) giving the input radial velocities and reference type.
    // The radial velocity can be a column in a table.
    // If 'proper' is True, it is tested if a proper radial velocity is given
    // (with proper unit and/or type). If not. False is returned.
    // If 'proper' is False, the value is always considered as radial velocity.
    void handleRadialVelocity (std::vector<TENShPtr>& args,
                               uInt& argnr);

    // Set the MeasConvert object.
    void setConverter (MRadialVelocity::Types toType);

    // Set the possible doppler engine.
    // It can be done only once.
    void setDopplerEngine (DopplerEngine& engine);

    // Set the possible direction engine.
    // It can be done only once.
    void setDirectionEngine (DirectionEngine& engine);

    // Set the possible epoch engine.
    // It can be done only once.
    void setEpochEngine (EpochEngine& engine);

    // Set the possible position engine.
    // It can be done only once.
    void setPositionEngine (PositionEngine& engine);

  private:
    virtual void handleValues (TableExprNode& operand,
                               const TableExprId& id,
                               Array<MRadialVelocity>& radialVelocities);

    //# Data members.
    MeasFrame                         itsFrame;    //# frame used by converter
    MRadialVelocity::Convert          itsConverter;
    DopplerEngine*                    itsDopplerEngine;
    DirectionEngine*                  itsDirectionEngine;
    EpochEngine*                      itsEpochEngine;
    PositionEngine*                   itsPositionEngine;
  };

} //end namespace

#endif
