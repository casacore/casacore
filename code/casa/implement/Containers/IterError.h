//# IterError.h:
//# Copyright (C) 1993,1994,1995,2000
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

#if !defined(AIPS_ITERERROR_H)
#define AIPS_ITERERROR_H

//# Includes
#include <aips/aips.h>
#include <aips/Exceptions/Error.h>

// <summary>Iteration error class </summary>

class IterError : public AipsError {
public:
  IterError(const char *msg = 0);      // normal constructor
  ~IterError ();
};

// <summary>Iteration Boundary error class</summary>

class IterBoundaryError : public IterError {
public:
  IterBoundaryError(const char *msg = 0);      // normal constructor
  ~IterBoundaryError ();
};

// <summary>Iteration initialization error</summary>

class IterInitError : public IterError {
public:
  IterInitError(const char *msg = 0);      // normal constructor
  ~IterInitError ();
};

// <summary>Invalide iteration error class</summary>

class InvalidIterError : public IterError {
public:
  InvalidIterError(const char *msg = 0);      // normal constructor
  ~InvalidIterError ();
};

#endif
