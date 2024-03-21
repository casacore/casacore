//# DopplerEngine.h: Engine for TaQL UDF Doppler conversions
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

#ifndef MEAS_DOPPLERENGINE_H
#define MEAS_DOPPLERENGINE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/meas/MeasUDF/MeasEngine.h>
#include <casacore/measures/Measures/MDoppler.h>
#include <casacore/measures/Measures/MCDoppler.h>
#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/casa/Quanta/MVFrequency.h>
#include <memory>

namespace casacore {

  //# Forward Declarations
  class RadialVelocityEngine;
  class FrequencyEngine;

  
  // <summary>
  // Engine for TaQL UDF Doppler conversions
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

  class DopplerEngine: public MeasEngine<MDoppler>
  {
  public:
    enum Type {DOPPLER, FREQ, RADVEL};

    DopplerEngine()
    {}

    virtual ~DopplerEngine();
    
    // Handle a possible rest frequency.
    // False is returned if it appears to be no rest frequency.
    Bool handleRestFreq (const TENShPtr&);
    
    // Get the values.
    Array<Double> getArrayDouble (const TableExprId& id);

    // Get the dopplers.
    Array<MDoppler> getDopplers (const TableExprId& id);

    // Handle the argument(s) giving the input dopplers and reference type.
    // The doppler can be a column in a table.
    // If 'proper' is True, it is tested if a proper doppler is given
    // (with proper type). If not. False is returned.
    // The 'allow' arguments tell if the doppler can be specified by means of
    // a radial velocity or freq/restfreq.
    void handleDoppler (std::vector<TENShPtr>& args,
                        uInt& argnr, Bool allowRadVel, Bool allowFreq);

    // Set the MeasConvert object.
    void setConverter (MDoppler::Types toType);

  private:
    void handleRestFreq (vector<TENShPtr>& args, uInt& argnr);
    void handleLine (const TENShPtr& operand);
    // Handle the values.
    virtual void handleValues (TableExprNode& operand,
                               const TableExprId& id,
                               Array<MDoppler>& dopplers);
    Array<MVFrequency> getRestFreqs (const TableExprId& id);

    //# Data members.
    Type                                  itsType;
    MDoppler::Convert                     itsConverter;
    std::shared_ptr<RadialVelocityEngine> itsRadVelEngine;
    std::shared_ptr<FrequencyEngine >     itsFreqEngine;
    Array<MVFrequency>                    itsConstRestFreqs;
    TENShPtr                              itsRestFreqNode;
  };

} //end namespace

#endif
