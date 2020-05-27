//# ArrayIO.cc: text output and binary IO for an array of any dimensionality.
//# Copyright (C) 1993,1994,1995,1996,1997,1999,2000,2001,2002,2003
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

#ifndef CASA_ARRAYIO_2_TCC
#define CASA_ARRAYIO_2_TCC

//# Includes
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayPosIter.h>
#include <casacore/casa/Arrays/ArrayUtil.h>

#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/IO/AipsIOCarray.h>
#include <casacore/casa/IO/ArrayIO.h>

#include <casacore/casa/Logging/LogIO.h>

#include <istream>
#include <fstream>
#include <cassert>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
LogIO &operator<<(LogIO &os, const Array<T> &a)
{
    os.output() << a;
    return os;
}

template<class T>
AipsIO &operator<<(AipsIO &ios, const Array<T> &a)
{
    putArray (ios, a, "Array");
    return ios;
}

template<class T>
void putArray (AipsIO &ios, const Array<T> &a, const Char* name)
{
    if (a.size() * sizeof(T) > 2147483647) {
      throw AipsError("AipsIO putArray too large (exceeds 2**31 bytes)");
    }
    ios.putstart(name, Array<T>::arrayVersion());
    // Write out dimensionality
    ios << uInt(a.ndim());
    // Write out length
    for (size_t i=0; i < a.ndim(); i++) {
      ios << uInt(a.shape()(i));
    }
    // Now write out the data
    bool deleteIt;
    const T *storage =  a.getStorage(deleteIt);
    putAipsIO (ios, a.nelements(), storage);
    a.freeStorage(storage, deleteIt);
    ios.putend();
}

// <thrown>
//     <item> ArrayError
// </thrown>
template<class T>
AipsIO &operator>>(AipsIO &ios, Array<T> &a)
{
    // Makes the argument unique, i.e. existing refs will be lost. At the moment
    // this is relatively inefficient as it makes a temporary copy.
    a.unique();
    
    // On 20-Nov-2000 use of the home-brew rtti was removed.
    // It meant that a name 'Array<int>' is now replaced by 'Array'.
    // In order to recognize those old names, we must do something special.
    String type = ios.getNextType();
    int version;
    if (type.length() > 6  &&  type.index("Array<") == 0) {
      version = ios.getstart(type);
    } else {
      version = ios.getstart("Array");
    }
    int ndim;
    ios >> ndim;
    IPosition shape(ndim);
    // Older versions contain an origin (which we discard).
    if (version < 3) {
	int orig;
	for (int i=0; i < ndim; i++) {
	    ios >> orig;
	}
    }
    uInt v;
    for (int i=0; i < ndim; i++) {
      ios >> v;
      shape(i) = v;
    }
    a.resize(shape);                // hopefully a no-op if unchanged

    // Now read in the data.

    bool deleteIt;
    T *storage = a.getStorage(deleteIt);

    uInt nwritten;
    ios >> nwritten;
    if (nwritten != a.nelements())
	throw(ArrayError("AipsIO &operator>>(AipsIO, Array<T>"
			  " - nelements() differs from number in file"));
    getAipsIO (ios, nwritten, storage);
    a.putStorage(storage, deleteIt);
    ios.getend();
    return ios;
}

inline AipsIO& operator<< (AipsIO& aio, const IPosition& ip)
{
  bool use32 = true;
  if (sizeof(ssize_t) > 4) {
    for (size_t i=0; i<ip.nelements(); ++i) {
      if (ip[i] > 2147483647) {
        use32 = false;
        break;
      }
    }
  }
  if (use32) {
    // Write values as int.
    aio.putstart("IPosition", 1);
    aio << (uInt) ip.nelements();
    for (size_t i=0; i<ip.nelements(); ++i) {
      aio << int(ip[i]);
    }
  } else {
    // Write values as long long.
    aio.putstart("IPosition", 2);
    aio << (uInt) ip.nelements();
    for (size_t i=0; i<ip.nelements(); ++i) {
      aio << (long long) (ip[i]);
    }
  }
  aio.putend();
  return aio;
}

// <thrown>
//    <item> ArrayError
// </thrown>
inline AipsIO& operator>> (AipsIO& aio, IPosition& ip)
{
  int vers = aio.getstart("IPosition");
  uInt nel;
  aio >> nel;
  ip.resize (nel, false);
  if (vers == 1) {
    int v;
    for (size_t i=0; i<nel; ++i) {
      aio >> v;
      ip[i] = v;
    }
  } else if (vers == 2) {
    long long v;
    if (sizeof(ssize_t) <= 4) {
      throw ArrayError ("AipsIO& operator>>(AipsIO& aio, IPosition& ip) - "
                       "cannot read back in an ssize_t of 4 bytes");
    }
    for (size_t i=0; i<nel; ++i) {
      aio >> v;
      ip[i] = v;
    }
  } else {
    throw(ArrayError("AipsIO& operator>>(AipsIO& aio, IPosition& ip) - "
                    "version on disk and in class do not match"));
  }
  aio.getend();
  assert (ip.ok());
  return aio;
}

inline LogIO& operator<< (LogIO& os, const IPosition& ip)
{
    os.output() << ip;
    return os;
}

template<typename T, typename Alloc>
Block<T> makeBlock(const Array<T, Alloc>& array)
{
	Block<T> block(array.nelements());
	if(array.contiguousStorage())
		std::copy(array.cbegin(), array.cend(), block.storage());
	else
		std::copy(array.begin(), array.end(), block.storage());
	return block;
}

template<typename T>
Vector<T> makeVector(const Block<T>& block)
{
	return Vector<T>(block.begin(), block.end());
}

inline Vector<String> stringToVector (const String& string, char delim)
{
	Vector<std::string> vec = strToVector(string, delim);
	return Vector<String>(vec.begin(), vec.end());
}

inline Vector<String> stringToVector (const String& string, const std::regex& delim)
{
	Vector<std::string> vec = strToVector(string, delim);
	return Vector<String>(vec.begin(), vec.end());
}

} //# NAMESPACE CASACORE - END


#endif
