//# IterError.cc:
//# Copyright (C) 1993,1994,1995
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

#include <aips/Containers/IterError.h>

rtti_imp_init(IterError);
rtti_imp_mbrf(IterError);

rtti_imp_init(IterBoundaryError);
rtti_imp_mbrf(IterBoundaryError);

rtti_imp_init(IterInitError);
rtti_imp_mbrf(IterInitError);

rtti_imp_init(InvalidIterError);
rtti_imp_mbrf(InvalidIterError);

// The normal constructor when throwing the exception.
IterError::IterError (const char *msg) : 
          AipsError(msg ? msg : "Iterator Error.") {}

// The constructor for the exception handler.
IterError::IterError(ExcpError* excp) : AipsError(excp) {
    IterError *tmp;
    PCAST(tmp,IterError,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}

IterError::~IterError ()
{ ; }

// The normal constructor when throwing the exception.
IterBoundaryError::IterBoundaryError (const char *msg) : 
          IterError(msg ? msg : "Iterator boundaries exceeded.") {}

// The constructor for the exception handler.
IterBoundaryError::IterBoundaryError(ExcpError* excp) : IterError(excp) {
    IterBoundaryError *tmp;
    PCAST(tmp,IterBoundaryError,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}

IterBoundaryError::~IterBoundaryError ()
{ ; }


// The normal constructor when throwing the exception.
IterInitError::IterInitError (const char *msg) : 
          IterError(msg ? msg : "Iterator initialization error.") {}

// The constructor for the exception handler.
IterInitError::IterInitError(ExcpError* excp) : IterError(excp) {
    IterInitError *tmp;
    PCAST(tmp,IterInitError,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}

IterInitError::~IterInitError ()
{ ; }

// The normal constructor when throwing the exception.
InvalidIterError::InvalidIterError (const char *msg) : 
          IterError(msg ? msg : "Use of invalid iterator.") {}

// The constructor for the exception handler.
InvalidIterError::InvalidIterError(ExcpError* excp) : IterError(excp) {
    InvalidIterError *tmp;
    PCAST(tmp,InvalidIterError,excp);
    if (tmp) {
        _equal = True;
    }else{
        _equal = False;
    }
}

InvalidIterError::~InvalidIterError ()
{ ; }
