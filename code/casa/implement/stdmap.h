//# stdmap.h: Interim solution for standard/nonstandard system map
//# Copyright (C) 2001,2002
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

#if !defined(AIPS_AIPS_STDMAP_H)
#define AIPS_AIPS_STDMAP_H

// Define the C standard C++ include file. 
// This is an interim solution to cater for the SGI non-existence of
// them (e.g. <cstring>)
// Make sure any special macros are set
#include <aips/aips.h>

#include <map>

using std::map;
using std::multimap;
using std::pair;
using std::allocator;
using std::less;

// A special macro to create the auxilliary template definitions for
// various compilers
// Use if defined a map<T, U> as AIPS_MAP_AUX_TEMPLATES(T, U)

#if defined(AIPS_MAP_AUX_TEMPLATES)
#undef AIPS_MAP_AUX_TEMPLATES
#endif

#if defined(AIPS_GCC)
#define AIPS_MAP_AUX_TEMPLATES(T, U) \
template class \
  _Rb_tree<T, pair<T const, U >, \
  _Select1st<pair<T const, U > >, \
  less<T>, allocator<U > >;

#else
#define AIPS_MAP_AUX_TEMPLATES(T, U)
#endif

#endif
