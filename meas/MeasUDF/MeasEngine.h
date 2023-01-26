//# MeasEngine.h: Templated base class for the TaQL UDF conversion engines
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

#ifndef MEAS_MEASENGINE_H
#define MEAS_MEASENGINE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/meas/MeasUDF/BaseEngine.h>
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/measures/TableMeasures/ArrayMeasColumn.h>
#include <casacore/casa/Arrays/Array.h>

namespace casacore {

  // <summary>
  // Templated base class for the TaQL UDF conversion engines
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="" date="" tests="tMeas.cc">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> BaseEngine
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

  template <typename M>
  class MeasEngine: public BaseEngine
  {
  public:
    MeasEngine()
      : itsRefType (M::N_Types)
    {}

    virtual ~MeasEngine();
    
    // Get the reference type.
    typename M::Types refType() const
      { return itsRefType; } 

    // Handle a doppler reference type.
    // If the reference type is invalid, an exception is only thrown
    // if <src>doThrow=true</src>. In this way a string argument can
    // be a source name for a direction.
    bool handleMeasType (const TENShPtr& operand, bool doThrow);

    // Make the expression result attributes.
    Record makeAttributes (typename M::Types refType,
                           int32_t valueType = 1) const;

  protected:
    // Handle the operand representing an array of Meas values.
    void handleMeasArray (const TENShPtr& operand);

    // Handle a constant Meas value.
    void handleConstant (const TENShPtr& operand);

    // Let a derive class handle the values.
    virtual void handleValues (TableExprNode& operand,
                               const TableExprId& id,
                               Array<M>& positions) = 0;
    
    //# Data members.
    Array<M>            itsConstants;
    typename M::Types   itsRefType;
    ScalarMeasColumn<M> itsMeasScaCol;
    ArrayMeasColumn<M>  itsMeasArrCol;
  };

} //end namespace


#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/meas/MeasUDF/MeasEngine.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
