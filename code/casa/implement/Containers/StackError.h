//# StackError.h: Error class for the stack class
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

#if !defined(AIPS_STACKERROR_H)
#define AIPS_STACKERROR_H

#include <aips/aips.h>
#include <aips/Exceptions/Error.h>

//#
rtti_dcl_init(EmptyStackError);
class EmptyStackError : public AipsError {
public:
  EmptyStackError(const char *msg = 0);      // normal constructor
  EmptyStackError(ExcpError*);        // constructor for exception handler
  ~EmptyStackError ();
  rtti_dcl_mbrf_p1(EmptyStackError,AipsError);
};

#endif
