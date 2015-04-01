//# TableIter.cc: Iterate through a Table
//# Copyright (C) 1994,1995,1996,1997,1999,2000
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

#include <casacore/tables/Tables/TableIter.h>
#include <casacore/tables/Tables/BaseTabIter.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/tables/Tables/TableError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableIterator::TableIterator()
: tabIterPtr_p (0)
{}

TableIterator::TableIterator (const Table& tab,
			      const String& key,
			      Order order,
			      Option option)
: tabIterPtr_p (0)
{
    Block<String> keys(1, key);
    Block<Int> ord(1, order);
    Block<CountedPtr<BaseCompare> > cmpObj(1);
    tabIterPtr_p = tab.baseTablePtr()->makeIterator (keys, cmpObj,
						     ord, option);
    next();                            // get first subtable
}

TableIterator::TableIterator (const Table& tab,
			      const Block<String>& keys,
			      Order order,
			      Option option)
: tabIterPtr_p (0)
{
    Block<Int> ord(keys.nelements(), order);
    Block<CountedPtr<BaseCompare> > cmpObj(keys.nelements());
    tabIterPtr_p = tab.baseTablePtr()->makeIterator (keys, cmpObj,
						     ord, option);
    next();                            // get first subtable
}

TableIterator::TableIterator (const Table& tab,
			      const Block<String>& keys,
			      const Block<Int>& orders,
			      Option option)
: tabIterPtr_p (0)
{
    Block<CountedPtr<BaseCompare> > cmpObj(keys.nelements());
    tabIterPtr_p = tab.baseTablePtr()->makeIterator (keys, cmpObj,
						     orders, option);
    next();                            // get first subtable
}

TableIterator::TableIterator (const Table& tab,
			      const Block<String>& keys,
			      const Block<CountedPtr<BaseCompare> >& cmpObjs,
			      const Block<Int>& orders,
			      Option option)
: tabIterPtr_p (0)
{
    tabIterPtr_p = tab.baseTablePtr()->makeIterator (keys, cmpObjs,
						     orders, option);
    next();                            // get first subtable
}

TableIterator::TableIterator (const TableIterator& iter)
: tabIterPtr_p (0)
{
    operator= (iter);
}

TableIterator& TableIterator::operator= (const TableIterator& iter)
{
    delete tabIterPtr_p;
    tabIterPtr_p = 0;
    subTable_p   = Table();
    if (iter.tabIterPtr_p != 0) {
	tabIterPtr_p = iter.tabIterPtr_p->clone();
	next();                            // get first subtable
    }
    return *this;
}

TableIterator::~TableIterator()
    { delete tabIterPtr_p; }

void TableIterator::reset()
{
    tabIterPtr_p->reset();
    next();
}

void TableIterator::throwIfNull() const
{
    if (isNull()) {
	throw (TableInvOper ("TableIterator is null"));
    }
}
void TableIterator::next()
{
    subTable_p = Table(tabIterPtr_p->next());
}

} //# NAMESPACE CASACORE - END

