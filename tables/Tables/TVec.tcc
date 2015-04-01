//# TVec.cc: Template table column or memory vectors
//# Copyright (C) 1994,1995,1996,2000
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

#ifndef TABLES_TVEC_TCC
#define TABLES_TVEC_TCC

#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/TVec.h>
#include <casacore/tables/Tables/TVecTemp.h>
#include <casacore/tables/Tables/TableError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Construct
template<class T>
TabVecRep<T>::TabVecRep()
: count_p(0),
  nrel_p (0)
{ ; }

//# Destructor.
template<class T>
TabVecRep<T>::~TabVecRep()
{ ; }


template<class T>
uInt TabVecRep<T>::nelem() const
    { return nrel_p; }


template<class T>
void TabVecRep<T>::validateConformance (uInt leng) const
{
    if (nelements() != leng) {
	throw TableVectorNonConform();
    }
}

//# Create a new vector (in memory).
template<class T>
void* TabVecRep<T>::newVec() const
{
    uInt nr = nelements();
    TabVecTemp<T>* tmvp = new TabVecTemp<T>(nr);
    return tmvp;
}


template<class T>
void TabVecRep<T>::assign (const TabVecRep<T>& that)
{
    uInt nr = that.nelements();
    validateConformance (nr);
    for (uInt i=0; i<nr; i++) {
	putVal (i, that.value(i));
    }
}

} //# NAMESPACE CASACORE - END


#endif
