//# Compare.cc: Templated function to compare two objects
//# Copyright (C) 1994,1995
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

#ifndef CASA_COMPARE_TCC
#define CASA_COMPARE_TCC

#include <casacore/casa/Utilities/Compare.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
ObjCompare<T>::~ObjCompare()
{}

template<class T>
int ObjCompare<T>::compare (const void* obj1, const void* obj2)
{
    return (*(const T*)obj1  < *(const T*)obj2  ?  -1 :
	   (*(const T*)obj1 == *(const T*)obj2  ?  0 : 1));
}

template<class T>
int ObjCompare<T>::comp (const void* obj1, const void* obj2) const
{
    return (*(const T*)obj1  < *(const T*)obj2  ?  -1 :
	   (*(const T*)obj1 == *(const T*)obj2  ?  0 : 1));
}

template<class T>
DataType ObjCompare<T>::dataType() const
{
    return whatType ((T*)0);
}


template<typename T>
CompareIntervalInt<T>::CompareIntervalInt(Int64 interval, Int64 start)
  : itsInterval(interval), itsStart(start)
{}

template<typename T>
CompareIntervalInt<T>::~CompareIntervalInt()
{}

template<typename T>
int CompareIntervalInt<T>::comp(const void * obj1, const void * obj2) const
{
  Int64 v1 = *static_cast<const T*>(obj1);
  Int64 v2 = *static_cast<const T*>(obj2);
  // Shortcut if values are equal.
  if (v1 == v2) return 0;
  // The times are binned in bins with a width of itsInterval.
  Int64 t1 = (v1-itsStart) / itsInterval;
  Int64 t2 = (v2-itsStart) / itsInterval;
  return (t1==t2  ?  0 : (t1<t2 ? -1 : 1));
}


template<typename T>
CompareIntervalReal<T>::CompareIntervalReal(Double interval, Double start)
  : itsInterval(interval), itsStart(start)
{}

template<typename T>
CompareIntervalReal<T>::~CompareIntervalReal()
{}

template<typename T>
int CompareIntervalReal<T>::comp(const void * obj1, const void * obj2) const
{
  T v1 = *static_cast<const T*>(obj1);
  T v2 = *static_cast<const T*>(obj2);
  // Shortcut if values are equal.
  if (v1 == v2) return 0;
  // The times are binned in bins with a width of interval_p.
  Double t1 = std::floor((v1 - itsStart) / itsInterval);
  Double t2 = std::floor((v2 - itsStart) / itsInterval);
  return (t1==t2  ?  0 : (t1<t2 ? -1 : 1));
}


} //# NAMESPACE CASACORE - END


#endif
