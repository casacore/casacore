//# vector.h: Interim solution for standard/nonstandard system vector
//# Copyright (C) 2002
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

#if !defined(AIPS_AIPS_VECTOR_H)
#define AIPS_AIPS_VECTOR_H

// Define the standard C++ include file. 
// This is an interim solution to cater for the SGI non-existence of
// them (e.g. <cstring>)
// Make sure any special macros are set
#include <aips/aips.h>

#include <vector>

using std::vector;

// A special macro to create the auxilliary template definitions for
// various compilers
// Use if defined a vector<T> as AIPS_VECTOR_AUX_TEMPLATES(T)

#if defined(AIPS_VECTOR_AUX_TEMPLATES)
#undef AIPS_VECTOR_AUX_TEMPLATES
#endif

#if defined(AIPS_GCC)
#define AIPS_VECTOR_AUX_TEMPLATES(T) \
template \
T *__uninitialized_copy_aux<T *, T *>(T *, T *, T *, __false_type); \
template \
T *__uninitialized_copy_aux<T const *, T *>(T const *, T const *, \
					    T *, __false_type); \
template \
T *fill_n<T *, uInt, T >(T *, uInt, T const &); \
template \
void fill<T *, T >(T *, T *, T const &); \
template \
T *__uninitialized_fill_n_aux<T *, uInt, T >(T *, uInt, \
			      T const &, __false_type); \
template \
void vector<T, allocator<T> >:: \
_M_assign_aux(T const *, T const *, forward_iterator_tag);

#else
#define AIPS_VECTOR_AUX_TEMPLATES(T)
#endif

#endif
