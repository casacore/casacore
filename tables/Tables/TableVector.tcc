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

#include <casa/aips.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/TableColumn.h>
#include <tables/Tables/TableVector.h>
#include <tables/Tables/TVecScaCol.h>
#include <tables/Tables/TVecTemp.h>
#include <tables/Tables/TableError.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Construct an empty table vector.
template<class T>
ROTableVector<T>::ROTableVector()
: tabVecPtr_p(0)
{}

//# Construct from a vector (reference semantics).
template<class T>
ROTableVector<T>::ROTableVector (const Vector<T>& vec)
{
    tabVecPtr_p = new TabVecTemp<T>(vec);
    checkLink();
}

//# Copy constructor (reference semantics).
template<class T>
ROTableVector<T>::ROTableVector (const ROTableVector<T>& that)
: tabVecPtr_p (that.tabVecPtr_p)
{
    if (tabVecPtr_p != 0) {
	tabVecPtr_p->link();
    }
}

//# Construct a readonly table column vector.
template<class T>
ROTableVector<T>::ROTableVector (const Table& tab,
				 const String& columnName)
{
    tabVecPtr_p = new TabVecScaCol<T> (ROTableColumn (tab, columnName));
    checkLink();
}

template<class T>
ROTableVector<T>::ROTableVector (const ROTableColumn& column)
{
    tabVecPtr_p = new TabVecScaCol<T> (column);
    checkLink();
}

template<class T>
ROTableVector<T>::~ROTableVector()
    { destruct(); }

template<class T>
void ROTableVector<T>::checkLink ()
{
    if (tabVecPtr_p == 0) {
	throw (AllocError ("(RO)TableVector", 1));
    }
    tabVecPtr_p->link();
}

template<class T>
void ROTableVector<T>::destruct ()
{
    if (tabVecPtr_p->unlink() == 0) {
	delete tabVecPtr_p;
    }
}

template<class T>
void ROTableVector<T>::throwIfNull() const
{
    if (isNull()) {
	throw (TableInvOper ("TableVector is null"));
    }
}

template<class T>
void ROTableVector<T>::reference (const ROTableVector<T>& that)
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
Vector<T> ROTableVector<T>::makeVector() const
{
    Vector<T> vect(nelements());
    TableVector<T> tvec(vect);
    tvec = *this;
    return vect;
}





//# Construct an empty table vector.
template<class T>
TableVector<T>::TableVector()
: ROTableVector<T>()
{}

//# Construct from a vector (reference semantics).
template<class T>
TableVector<T>::TableVector (const Vector<T>& vec)
: ROTableVector<T> (vec)
{}

//# Construct a vector with a given length.
template<class T>
TableVector<T>::TableVector (uInt leng)
: ROTableVector<T>()
{
    tabVecPtr_p = new TabVecTemp<T>(leng);
    checkLink();
}

//# Copy constructor (reference semantics).
template<class T>
TableVector<T>::TableVector (const TableVector<T>& that)
: ROTableVector<T> (that)
{}

//# Construct a read/write table column vector.
template<class T>
TableVector<T>::TableVector (const Table& tab, const String& columnName)
: ROTableVector<T>()
{
    tabVecPtr_p = new TabVecScaCol<T> (TableColumn (tab, columnName));
    checkLink();
}

template<class T>
TableVector<T>::TableVector (const TableColumn& column)
: ROTableVector<T>()
{
    tabVecPtr_p = new TabVecScaCol<T> (column);
    checkLink();
}

template<class T>
TableVector<T>::~TableVector()
{}


//# Referencing another vector.
template<class T>
void TableVector<T>::reference (const TableVector<T>& that)
    { ROTableVector<T>::reference (that); }

} //# NAMESPACE CASA - END

