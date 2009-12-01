//# stdexcept.h: Make standard exceptions availabe in casa namespace
//# Copyright (C) 2007
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

#ifndef CASA_STDEXCEPT_H
#define CASA_STDEXCEPT_H

#include <stdexcept>

namespace casa { //# NAMESPACE CASA - BEGIN

using std::exception;
using std::logic_error;
using std::domain_error;
using std::invalid_argument;
using std::length_error;
using std::out_of_range;
using std::runtime_error;
using std::range_error;
using std::overflow_error;
using std::underflow_error;

} //# NAMESPACE CASA - END

#endif
