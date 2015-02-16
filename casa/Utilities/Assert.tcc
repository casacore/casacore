//# Assert.cc: Throw exception when assertion fails.
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

#ifndef CASA_ASSERT_TCC
#define CASA_ASSERT_TCC

#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/stdio.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class t> assert_<t>::assert_(int expr, const char *msg, const char* file, Int line) {
  static char message[256];
  if (! expr) {
    sprintf(message,"(%s : %i) %s",file,line,msg);
    throw(t(message));
  }
}

template<class t> assert_<t>::assert_(const void *ptr, const char *msg, const char* file, Int line) {
  static char message[256];
  if (! ptr) {
    sprintf(message,"(%s : %i) %s",file,line,msg);
    throw(t(message));
  }
}

} //# NAMESPACE CASACORE - END


#endif
