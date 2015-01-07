//# AipsIOCarray.h: Templated functions to get/put a C-array from/into AipsIO.
//# Copyright (C) 1993,1994,1995,1996,1999,2001
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

#ifndef CASA_AIPSIOCARRAY_H
#define CASA_AIPSIOCARRAY_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/AipsIO.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> 
// Templated functions to get/put a C-style array from/into AipsIO.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Gareth Hunt" date="95Feb24" tests="" demos="">

// <prerequisite>
//   <li> <linkto class="AipsIO:description">AipsIO</linkto>
// </prerequisite>

// <etymology>
// AipsIOCarray is simply the conventional shorthand for "aips input/output for
// C-style arrays".
// </etymology>

// <synopsis> 
// This file declares templated functions to get or put a C-style array
// of any data type from/into AipsIO.
// These functions are similar to the AipsIO functions put, get and getnew,
// but support any data type.
//
// Specializations (using these AipsIO functions) are made for
// the standard data types. These are much more efficient.
// </synopsis> 

// <example>
// <srcblock>
//  // Write an C-style array of type A into AipsIO.
//  // This will first write the number of elements.
//  {
//	A ap[1000];
//	AipsIO io ("file.data", ByteIO::New);
//	io.putstart ("some",1);
//	putAipsIO (io, uInt(1000), ap);
//	io.putend();
//  }
//  // Read the data back into a preallocated array.
//  // First the number of elements have to be read.
//  {
//	A api[1000];
//	uInt n;
//	AipsIO io ("file.data");
//	io.getstart ("some");
//	io >> n;
//	getAipsIO (io, n, api);
//  }
//  // Read the data back into an automatically allocated array.
//  // This will also read the number of elements.
//  // Delete the allocated array at the end.
//  {
//	A* api;
//	uInt n;
//	AipsIO io ("file.data");
//	io.getstart ("some");
//	getnewAipsIO (io, n, &api);
//	delete [] api;
//  }
// </srcblock>
// </example>


// <group name=AipsIOCarray>

// Put a C-style array of n elements.
// First the number of elements is put, thereafter all values.
template<class T>
void putAipsIO (AipsIO& aios, uInt n, const T* data);

// Get n elements into an already available C-style array.
// The data buffer must be large enough to hold n values.
template<class T>
void getAipsIO (AipsIO& aios, uInt n, T* data);

// Get elements into a C-style array to be allocated on the heap.
// First the number of elements will be read. The array will be allocated
// by this function and must be freed by the user. Its pointer is returned
// in data. The number of elements is returned in n.
//
// <note>
// Unfortunately the CFront compiler (and maybe others as well) fail to
// overload on <src>T*& data</src> iso. <src>T** data</src>.
// </note>
template<class T>
void getnewAipsIO (AipsIO& aios, uInt& n, T** data);

// </group>


//# Specializations for the builtin data types.
#define AIPSIO_FUNC_SPEC(T) \
inline void putAipsIO (AipsIO& aios, uInt n, const T* data) \
    { aios.put (n, data); } \
inline void getAipsIO (AipsIO& aios, uInt n, T* data) \
    { aios.get (n, data); } \
inline void getnewAipsIO (AipsIO& aios, uInt& n, T** data) \
    { aios.getnew (n, *data); }

//# These macros expand to generate the appropriate inline functions
//# for the built-in data types.

AIPSIO_FUNC_SPEC(Bool)
AIPSIO_FUNC_SPEC(Char)
AIPSIO_FUNC_SPEC(uChar)
AIPSIO_FUNC_SPEC(short)
AIPSIO_FUNC_SPEC(unsigned short)
AIPSIO_FUNC_SPEC(int)
AIPSIO_FUNC_SPEC(unsigned int)
AIPSIO_FUNC_SPEC(Int64)
AIPSIO_FUNC_SPEC(uInt64)
AIPSIO_FUNC_SPEC(float)
AIPSIO_FUNC_SPEC(double)
AIPSIO_FUNC_SPEC(Complex)
AIPSIO_FUNC_SPEC(DComplex)
AIPSIO_FUNC_SPEC(String)



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/IO/AipsIOCarray.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
