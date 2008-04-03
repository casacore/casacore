//# vector.h: Interim solution for standard/nonstandard system vector
//# Copyright (C) 2002,2003,2004
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

#ifndef CASA_STD_VECTOR_H
#define CASA_STD_VECTOR_H

//# Define the standard C++ include file. 
//# This is an interim solution to cater for the SGI non-existence of
//# them (e.g. <cstring>)
//# Make sure any special macros are set
#include <casa/aips.h>

#if !defined(AIPS_SGI)
#include <vector>
using std::vector;
#else
#include <vector.h>
#endif

namespace casa { //# NAMESPACE CASA - BEGIN

//# A special macro to create the auxilliary template definitions for
//# various compilers
//# Use if defined a vector<T> as AIPS_VECTOR_AUX_TEMPLATES(T)

#if defined(AIPS_VECTOR_AUX_TEMPLATES)
#undef AIPS_VECTOR_AUX_TEMPLATES
#endif

#if !defined(AIPS_AUTO_STL)
# if defined(AIPS_GCC)
#  if defined(AIPS_GCC3)
#   define AIPS_VECTOR_AUX_TEMPLATES(T) \
using casa::uInt; \
using casa::uLong; \
template \
   T *std::__uninitialized_copy_aux<T *, T *>(T *, T *, T *, __false_type); \
template \
   T* std::__uninitialized_copy_aux<vector<T >::iterator , T *>(vector<T >::iterator , vector<T >::iterator , T *, __false_type); \
template \
   T* std::__uninitialized_copy_aux<vector<T >::const_iterator , T *>(vector<T >::const_iterator , vector<T >::const_iterator , T *, __false_type); \
template \
   vector<T >::iterator std::__uninitialized_copy_aux<vector<T >::iterator , vector<T >::iterator>(vector<T >::iterator , vector<T >::iterator , vector<T >::iterator, __false_type); \
template \
   T *std::__uninitialized_copy_aux<T const *, T *>(T const *, T const *, \
			                                            T *, __false_type); \
template \
   vector<T >::iterator std::fill_n<vector<T >::iterator, uInt, T >(vector<T >::iterator, uInt, T const &); \
template \
   vector<T >::iterator std::fill_n<vector<T >::iterator, uLong, T >(vector<T >::iterator, uLong, T const &); \
template \
   T *std::fill_n<T *, uInt, T >(T *, uInt, T const &); \
template \
   T *std::fill_n<T *, uLong, T >(T *, uLong, T const &); \
template \
   void std::fill<vector<T >::iterator, T >(vector<T >::iterator, vector<T >::iterator, T const &); \
template \
   void std::fill<T *, T >(T *, T *, T const &); \
template \
   vector<T >::iterator std::__uninitialized_fill_n_aux<vector<T >::iterator, uInt, T >(vector<T >::iterator, uInt, \
              T const &, __false_type); \
template \
   T *std::__uninitialized_fill_n_aux<T *, uInt, T >(T *, uInt, \
T const &, __false_type); \
template \
   vector<T >::iterator std::__uninitialized_fill_n_aux<vector<T >::iterator, uLong, T >(vector<T >::iterator, uLong, \
              T const &, __false_type); \
template \
   T *std::__uninitialized_fill_n_aux<T *, uLong, T >(T *, uLong, \
T const &, __false_type); \
template \
   void vector<T, std::allocator<T> >:: \
_M_assign_aux(T const *, T const *, forward_iterator_tag);
#  else
#   define AIPS_VECTOR_AUX_TEMPLATES(T) \
template \
T *std::__uninitialized_copy_aux<T *, T *>(T *, T *, T *, __false_type); \
template \
T *std::__uninitialized_copy_aux<T const *, T *>(T const *, T const *, \
					    T *, __false_type); \
template \
   vector<T >::iterator std::__uninitialized_fill_n_aux<vector<T >::iterator, uInt, T >(vector<T >::iterator, uInt, \
              T const &, __false_type); \
template \
   vector<T >::iterator std::__uninitialized_fill_n_aux<vector<T >::iterator, uLong, T >(vector<T >::iterator, uLong, \
              T const &, __false_type); \
template \
T *std::fill_n<T *, uInt, T >(T *, uInt, T const &); \
template \
void std::fill<T *, T >(T *, T *, T const &); \
template \
void vector<T, std::allocator<T> >:: \
_M_assign_aux(T const *, T const *, forward_iterator_tag);
#  endif

# else
#  if defined(AIPS_SUN_NATIVE)
#   define AIPS_VECTOR_AUX_TEMPLATES(T) \
template T* std::copy_backward<const T*, T* >(const T*, const T*, T*);\
template T* std::copy_backward<T*, T* >(T*, T*, T*);\
template T* std::copy<const T*, T* >(const T*, const T*, T*);\
template T* std::copy<T*, T* >(T*, T*, T*);\
template void std::fill<T*, T >(T*, T*, const T&);
#  else
#   define AIPS_VECTOR_AUX_TEMPLATES(T)
#  endif
# endif
#else
# define AIPS_VECTOR_AUX_TEMPLATES(T)
#endif


} //# NAMESPACE CASA - END

#endif
