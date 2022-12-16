//# PycExcp.h: Functions to convert a C++ exception to Python
//# Copyright (C) 2006
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

#ifndef PYRAP_PYCEXCP_H
#define PYRAP_PYCEXCP_H

#include <casacore/casa/Containers/IterError.h>

namespace casacore { namespace python {

  // Convert an IterError exception to a Python StopIteration.
  // In this way an iteration loop can be done.
  void translate_iterexcp (const casacore::IterError& e);

  // Register exception translators for casacore::IterError.
  void register_convert_excp();

}}

#endif
