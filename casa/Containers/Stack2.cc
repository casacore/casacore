//# Stack2.cc: Implementation of a stack using the doubly linked list class
//# Copyright (C) 1993,1994,1995,1996,1998,2000
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

#include <casacore/casa/Containers/Stack.h>
#include <casacore/casa/Containers/StackError.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Throw an error indicating operation error occured on an empty
// stack. This function must be out-of-line because of the dependence
// of the Cleanup (really CanDelete) objects on Stack<void *>.
void throw_empty_Stack_error(const char *msg) {
  throw(EmptyStackError(msg));   //# will enventually be more informative
}

} //# NAMESPACE CASACORE - END

