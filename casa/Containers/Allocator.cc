//# Allocator.cc:
//# Copyright (C) 2015
//# National Astronomical Observatory of Japan
//# 2-21-1, Osawa, Mitaka, Tokyo, 181-8588, Japan.
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

#include <casacore/casa/Containers/Allocator.h>

namespace casacore {

#if defined(AIPS_CXX11)
constexpr ArrayInitPolicy ArrayInitPolicies::NO_INIT;
constexpr ArrayInitPolicy ArrayInitPolicies::INIT;
#else
ArrayInitPolicy const ArrayInitPolicies::NO_INIT = ArrayInitPolicy(false);
ArrayInitPolicy const ArrayInitPolicies::INIT = ArrayInitPolicy(true);
#endif

}
