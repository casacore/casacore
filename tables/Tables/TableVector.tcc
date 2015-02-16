//# TableVector.cc: Templated readonly table column vectors
//# Copyright (C) 1994,1995,2000
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

#ifndef TABLES_TABLEVECTOR_TCC
#define TABLES_TABLEVECTOR_TCC

#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/Tables/TableVector.h>
#include <casacore/tables/Tables/TVecScaCol.h>
#include <casacore/tables/Tables/TVecTemp.h>
#include <casacore/tables/Tables/TableError.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Construct an empty table vector.
template<class T>
TableVector<T>::TableVector()
: tabVecPtr_p(0)
{}

//# Construct from a vector (reference semantics).
template<class T>
TableVector<T>::TableVector (const Vector<T>& vec)
{
    tabVecPtr_p = new TabVecTemp<T>(vec);
    tabVecPtr_p->link();
}

//# Construct a vector with a given length.
template<class T>
TableVector<T>::TableVector (uInt leng)
{
    tabVecPtr_p = new TabVecTemp<T>(leng);
    tabVecPtr_p->link();
}

//# Copy constructor (reference semantics).
template<class T>
TableVector<T>::TableVector (const TableVector<T>& that)
: tabVecPtr_p (that.tabVecPtr_p)
{
    if (tabVecPtr_p != 0) {
	tabVecPtr_p->link();
    }
}

//# Construct a readonly table column vector.
template<class T>
TableVector<T>::TableVector (const Table& tab,
                             const String& columnName)
{
    tabVecPtr_p = new TabVecScaCol<T> (TableColumn (tab, columnName));
    tabVecPtr_p->link();
}

template<class T>
TableVector<T>::TableVector (const TableColumn& column)
{
    tabVecPtr_p = new TabVecScaCol<T> (column);
    tabVecPtr_p->link();
}

template<class T>
TableVector<T>::~TableVector()
    { destruct(); }

template<class T>
void TableVector<T>::destruct ()
{
    if (tabVecPtr_p->unlink() == 0) {
	delete tabVecPtr_p;
    }
}

template<class T>
void TableVector<T>::throwIfNull() const
{
    if (isNull()) {
	throw (TableInvOper ("TableVector is null"));
    }
}

template<class T>
void TableVector<T>::reference (const TableVector<T>& that)
{
    //# First destruct current vector.
    destruct();
    //# Now reference the other table vector.
    tabVecPtr_p = that.tabVecPtr_p;
    if (tabVecPtr_p != 0) {
	tabVecPtr_p->link();
    }
}

//# Make a Vector from the (RO)TableVector.
template<class T>
Vector<T> TableVector<T>::makeVector() const
{
    Vector<T> vect(nelements());
    TableVector<T> tvec(vect);
    tvec = *this;
    return vect;
}


} //# NAMESPACE CASACORE - END


#endif
