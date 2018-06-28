//# Register.h: Register Measure UDFs
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

#ifndef MEAS_REGISTER_H
#define MEAS_REGISTER_H

#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/UDFBase.h>
#include <ostream>

// <group name=MeasUDF>
// This function registers the TaQL user defined functions handling
// Measure conversions.
// It is called when the dynamic library casa_meas.so/dylib is loaded.

extern "C" {
  void register_meas();
}

// </group>


namespace casacore {
  // <synopsis>
  // General meas function to show the available functions.
  // </synopsis>
  class HelpMeasUDF: public UDFBase
  {
  public:
    // Function to create an object.
    static UDFBase* makeHELP (const String&);

    // Setup the object.
    virtual void setup (const Table&, const TaQLStyle&);

    // Get the value.
    virtual String getString (const TableExprId& id);

    // Show the possible functions.
    static void showFuncsPosition  (std::ostream&, Bool showTypes);
    static void showFuncsEpoch     (std::ostream&, Bool showTypes);
    static void showFuncsDirection (std::ostream&, Bool showTypes);
  };
}

#endif
