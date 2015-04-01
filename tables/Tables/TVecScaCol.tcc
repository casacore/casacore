//# TVecScaCol.cc: Template table scalar column vectors
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

#ifndef TABLES_TVECSCACOL_TCC
#define TABLES_TVECSCACOL_TCC

#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/TVecScaCol.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Construct a table column vector.
template<class T>
TabVecScaCol<T>::TabVecScaCol (const TableColumn& column)
{
    //# Construct a scalar column.
    //# This will check the type, etc. and link to the BaseTable object.
    colPtr_p = new ScalarColumn<T> (column);
    tag_p  = TagScaCol;
    nrel_p = -1;                                 // #rows is #nelements
}


//# Destructor.
template<class T>
TabVecScaCol<T>::~TabVecScaCol ()
    { delete colPtr_p; }


template<class T>
uInt TabVecScaCol<T>::nelem() const
    { return colPtr_p->nrow(); }

template<class T>
T TabVecScaCol<T>::value (uInt i) const
    { return (*colPtr_p)(i); }

template<class T>
void TabVecScaCol<T>::getVal (uInt i, T& val) const
    { colPtr_p->get (i, val); }

template<class T>
void TabVecScaCol<T>::putVal (uInt i, const T& val)
    { colPtr_p->put (i, val); }

template<class T>
void TabVecScaCol<T>::set (const T& val)
{
    uInt nrrow = colPtr_p->nrow();
    for (uInt i=0; i<nrrow; i++) {
	colPtr_p->put (i, val);
    }
}

} //# NAMESPACE CASACORE - END


#endif
