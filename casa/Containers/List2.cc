//# List2.cc: (non-templated) exception functions for list classes
//# Copyright (C) 1993,1994,1995,1998
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

#include <casacore/casa/Containers/List.h>
#include <casacore/casa/Containers/IterError.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

void throw_list_end_error(){
  throw(IterBoundaryError("List operation performed with cursor past the "
			  "end of the list."));
}
void throw_list_init_error(){
  char *zero = 0;
  throw (IterInitError(zero));
}
void throw_list_swapright_same_error() {
  throw(IterError("Attempted 'swapRight' on same list."));
}
void throw_list_start_error(){
  throw(IterBoundaryError("Iterator backed past the beginning of the list."));
}

} //# NAMESPACE CASACORE - END

