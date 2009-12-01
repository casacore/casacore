//# TableIterProxy.cc: Holder of table iterators for the table glish client.
//# Copyright (C) 1994,1995,1996
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

#include <tables/Tables/TableIterProxy.h>
#include <tables/Tables/TableProxy.h>
#include <casa/Containers/IterError.h>
#include <casa/Arrays/Vector.h>

namespace casa { //# NAMESPACE CASA - BEGIN

TableIterProxy::TableIterProxy()
: firstTime_p (True)
{}

TableIterProxy::TableIterProxy (const TableProxy& tab,
				const Vector<String>& columns,
				const String& order, const String& sortType)
: firstTime_p (True)
{
  Block<String> names(columns.nelements());
  for (uInt i=0; i<names.nelements(); i++) {
    names[i] = columns(i);
  }
  String corder(order);
  corder.downcase();
  TableIterator::Order taborder = TableIterator::Ascending;
  if (! corder.empty()) {
    if (corder[0] == 'a') {
      taborder = TableIterator::Ascending;
    } else if (corder[0] == 'd') {
      taborder = TableIterator::Descending;
    }
  }
  String csort(sortType);
  csort.downcase();
  TableIterator::Option tabsort = TableIterator::HeapSort;
  if (! csort.empty()) {
    if (csort[0] == 'q') {
      tabsort = TableIterator::QuickSort;
    } else if (csort[0] == 'i') {
      tabsort = TableIterator::InsSort;
    } else if (csort[0] == 'n') {
      tabsort = TableIterator::NoSort;
    }
  }
  iter_p = TableIterator(tab.table(), names, taborder, tabsort);
}

TableIterProxy::TableIterProxy (const TableIterProxy& that)
: iter_p      (that.iter_p),
  firstTime_p (that.firstTime_p)
{}

TableIterProxy::~TableIterProxy()
{}

TableIterProxy& TableIterProxy::operator= (const TableIterProxy& that)
{
  if (this != &that) {
    iter_p      = that.iter_p;
    firstTime_p = that.firstTime_p;
  }
  return *this;
}

Bool TableIterProxy::nextPart (TableProxy& table)
{
  // The first iteration is already done by the TableIterator constructor.
  if (firstTime_p) {
    firstTime_p = False;
  } else {
    iter_p.next();
  }
  // Exit when no more subtables.
  if (iter_p.pastEnd()) {
    return False;
  }
  table = TableProxy (iter_p.table());
  return True;
}

TableProxy TableIterProxy::next()
{
  TableProxy tp;
  Bool ok = nextPart (tp);
  if (ok) {
    return tp;
  }
  throw IterError();
}

void TableIterProxy::reset()
{
  iter_p.reset();
  firstTime_p = True;
}


} //# NAMESPACE CASA - END
