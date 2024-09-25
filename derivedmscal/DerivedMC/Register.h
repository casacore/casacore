//# Register.h: Register virtual column engine to return derived MS values
//# Copyright (C) 2010
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

#ifndef DERIVEDMSCAL_REGISTER_H
#define DERIVEDMSCAL_REGISTER_H

#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/UDFBase.h>
#include <ostream>

// <group name=DerivedMSCal>
// This function registers the DerivedMSCal virtual column engine and
// the UDFMSCal UDFs.
// It is called when the dynamic library derivedmscal.so/dylib is loaded.

extern "C" {
  void register_derivedmscal();
}

// </group>



namespace casacore {
  // <synopsis>
  // General meas function to show the available functions.
  // </synopsis>
  class HelpMsCalUDF: public UDFBase
  {
  public:
    // Function to create an object.
    static UDFBase* makeHELP (const String&);

    // Setup the object.
    virtual void setup (const Table&, const TaQLStyle&);

    // Get the value.
    virtual String getString (const TableExprId& id);

    // Show the possible functions.
    static void showFuncsDerived   (std::ostream&);
    static void showFuncsStokes    (std::ostream&, Bool showStokes);
    static void showFuncsSelection (std::ostream&);
    static void showFuncsSubtable  (std::ostream&);
  };
}

#endif
