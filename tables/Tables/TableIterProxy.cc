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

#include <casacore/tables/Tables/TableIterProxy.h>
#include <casacore/tables/Tables/TableProxy.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Containers/IterError.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableIterProxy::TableIterProxy()
: firstTime_p (True)
{}

TableIterProxy::TableIterProxy (const TableProxy& tab,
				const Vector<String>& columns,
				const String& order, const String& sortType,
                                const Vector<Double>& iterSteps)
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
    if (corder[0] == 'd'  ||  corder[0] == 'D') {
      taborder = TableIterator::Descending;
    }
  }
  String csort(sortType);
  csort.downcase();
  TableIterator::Option tabsort = TableIterator::ParSort;
  if (! csort.empty()) {
    if (csort[0] == 'q') {
      tabsort = TableIterator::QuickSort;
    } else if (csort[0] == 'h') {
      tabsort = TableIterator::HeapSort;
    } else if (csort[0] == 'i') {
      tabsort = TableIterator::InsSort;
    } else if (csort[0] == 'p') {
      tabsort = TableIterator::ParSort;
    } else if (csort[0] == 'n') {
      tabsort = TableIterator::NoSort;
    }
  }
  if (iterSteps.empty()  ||  tab.table().nrow() == 0) {
    iter_p = TableIterator(tab.table(), names, taborder, tabsort);
  } else {
    makeStepIter (tab.table(), names, iterSteps, taborder, tabsort);
  }
}

void TableIterProxy::makeStepIter (const Table& tab,
                                   const Block<String>& columns,
                                   const Vector<Double>& iterSteps,
                                   TableIterator::Order order,
                                   TableIterator::Option option)
{
  // First determine if all columns are scalar and have a valid data type.
  // Also find out if a case-insenstive string comparison is needed.
  Block<CountedPtr<BaseCompare> > comps(columns.size());
  Block<Int> orders (columns.size(), order);
  for (uInt i=0; i<iterSteps.size(); ++i) {
    if (i < columns.size()  &&  iterSteps[i] > 0) {
      const ColumnDesc& colDesc = tab.tableDesc()[columns[i]];
      if (! colDesc.isScalar()) {
        throw TableError ("Only scalar columns can be used in table "
                          "iteration");
      }
      DataType dtype = colDesc.dataType();
      switch (dtype) {
        // The following data types are valid.
      case TpUChar:
      case TpShort:
      case TpUShort:
      case TpInt:
      case TpUInt:
      case TpFloat:
      case TpDouble:
        break;
      case TpString:
        comps[i] = new CompareNoCase();
        break;
      default:
        throw TableError ("No iteration step can be given for column " +
                          columns[i]);
      }
    }
  }
  // First sort the table to fully order the columns with an interval.
  Table sortab(tab);
  if (option != TableIterator::NoSort) {
    Table sortab = tab.sort (columns, comps, orders, option);
  }
  // Now see if an interval comparison has to be done when iterating.
  for (uInt i=0; i<iterSteps.size(); ++i) {
    if (i < columns.size()  &&  iterSteps[i] > 0) {
      DataType dtype = sortab.tableDesc()[columns[i]].dataType();
      switch (dtype) {
      case TpUChar:
        {
          uChar start = ScalarColumn<uChar>(sortab, columns[i])(0);
          comps[i] = new CompareIntervalInt<uChar>(iterSteps[i], start);
        }
        break;
      case TpShort:
        {
          Short start = ScalarColumn<Short>(sortab, columns[i])(0);
          comps[i] = new CompareIntervalInt<Short>(iterSteps[i], start);
        }
        break;
      case TpUShort:
        {
          uShort start = ScalarColumn<uShort>(sortab, columns[i])(0);
          comps[i] = new CompareIntervalInt<uShort>(iterSteps[i], start);
        }
        break;
      case TpInt:
        {
          Int start = ScalarColumn<Int>(sortab, columns[i])(0);
          comps[i] = new CompareIntervalInt<Int>(iterSteps[i], start);
        }
        break;
      case TpUInt:
        {
          uInt start = ScalarColumn<uInt>(sortab, columns[i])(0);
          comps[i] = new CompareIntervalInt<uInt>(iterSteps[i], start);
        }
        break;
      case TpFloat:
        {
          Float start = ScalarColumn<Float>(sortab, columns[i])(0);
          comps[i] = new CompareIntervalReal<Float>(iterSteps[i], start);
        }
        break;
      case TpDouble:
        {
          Double start = ScalarColumn<Double>(sortab, columns[i])(0);
          comps[i] = new CompareIntervalReal<Double>(iterSteps[i], start);
        }
        break;
      default:
        break;
      }
    }
  }
  // Iterate over the sorted table (and don't sort again).
  iter_p = TableIterator(sortab, columns, comps, orders,
                         TableIterator::NoSort);
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


} //# NAMESPACE CASACORE - END
