//# TVecTemp.cc: Template table vectors held in memory as a temporary
//# Copyright (C) 1994,1995,1999
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

#ifndef TABLES_TVECTEMP_TCC
#define TABLES_TVECTEMP_TCC

#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/TVecTemp.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/tables/Tables/TableError.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Construct from a vector.
template<class T>
TabVecTemp<T>::TabVecTemp (const Vector<T>& vec)
{
    nrel_p   = vec.nelements();
    vecPtr_p = new Vector<T>(vec);
    tag_p = TagTemp;
}

//# Construct a Vector.
template<class T>
TabVecTemp<T>::TabVecTemp (uInt leng)
{
    nrel_p   = leng;
    vecPtr_p = new Vector<T>(nrel_p);
    tag_p = TagTemp;
}


//# Destructor.
template<class T>
TabVecTemp<T>::~TabVecTemp ()
    { delete vecPtr_p; }


//# Get or put a value.
template<class T>
T TabVecTemp<T>::value (uInt i) const
    { return (*vecPtr_p)(i); }
template<class T>
void TabVecTemp<T>::getVal (uInt i, T& val) const
    { val = (*vecPtr_p)(i); }

template<class T>
void TabVecTemp<T>::putVal (uInt i, const T& val)
    { (*vecPtr_p)(i) = val; }

template<class T>
void TabVecTemp<T>::set (const T& val)
    { vecPtr_p->set (val); }

} //# NAMESPACE CASACORE - END


#endif
