//# Register.cc: Templated function to provide simple type identification
//# Copyright (C) 1993,1994,1995,2001
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

#ifndef CASA_REGISTER_TCC
#define CASA_REGISTER_TCC

#include <casacore/casa/Utilities/Register.h>

#include <mutex>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

static inline void RegisterHelper(uInt *typeId) {
  *typeId = RegSequence::SgetNext();
}

// Could be done at compile-time, but take easy road to thread-safety, since custom RTTI has to go anyway.
template<class t> uInt Register(const t *) {
  static uInt typeId;
  static std::once_flag callOnceFlag;
  // None of the call once primitives deals with return values, so use a helper.
  std::call_once(callOnceFlag, RegisterHelper, &typeId);
  return typeId;
}

} //# NAMESPACE CASACORE - END


#endif
