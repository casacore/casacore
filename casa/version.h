//# version.h: Get casacore version
//# Copyright (C) 2008
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

#ifndef CASA_VERSION_H
#define CASA_VERSION_H

#include <string>
#include <casacore/casa/aips.h>

#define CASACORE_VERSION "2.0.0"

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // Get the casacore version.
  const std::string getVersion();

  // Get the version of casacore on CASA's vendor branch
  // Note: CASA's private version of casacore has a lifecycle
  // which is not necessarily identical to versions of casacore
  // elsewhere. This function returns the version of casacore
  // on CASA's vendor branch.
  const std::string getVersionCASA();

} //# NAMESPACE CASACORE - END

#endif
