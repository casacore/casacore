//# TableIterProxy.h: Proxy for table iterator access
//# Copyright (C) 1994,1995,1996,1999,2005
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

#ifndef TABLES_TABLEITERPROXY_H
#define TABLES_TABLEITERPROXY_H


//# Includes
#include <casa/aips.h>
#include <tables/Tables/TableIter.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations
class TableProxy;


// <summary>
// Proxy for table iterator access.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Paul Shannon" date="1995/09/15" tests="tgtable.g" demos="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> class TableIterator
// </prerequisite>

// <etymology>
// TableIterProxy holds a TableIterator object for the table
// glish client.
// </etymology>

// <synopsis> 
// A GlishTable object holds a TableIterator object for the table
// glish client. It is in fact similar to class
// <linkto class=GlishTable>GlishTable</linkto>.
// It is used by <linkto class=GlishTableProxy>GlishTableProxy</linkto>
// to keep track of all table iterators used in the glish client.
// <p>
// TableIterProxy also holds the id of the GlishTable
// object which is used by GlishTableProxy to hold the result of the
// last iterator step.
// </synopsis>

// <example>
// The iterator functions in class GlishTableProxy show clearly how
// TableIterProxy is used.
// For completeness, an example follows.
// <srcblock>
//    // Get a tableId for the subtables.
//    GlishTableProxy proxy;
//    Table table;
//    tableId = proxy.addTable (table);
//    // Construct a TableIterProxy object.
//    Table mainTable ("table.name");
//    TableIterator iter (table, "columnX");
//    TableIterProxy tgi (iter, tableId);
//    // Do a step.
//    Table subTable;
//    Bool pastEnd = tgi.next (subTable);
//    // Get the table id for the subtable resulting from the iteration step.
//    Int tableId = tgi.tableId();
// </srcblock>
// </example>

class TableIterProxy
{
public:
  // Default constructor initializes to not open.
  // This constructor is only needed for the Block container.
  TableIterProxy();

  // Construct for the given table column(s).
  // Order and sortType are case-insentive strings and only the first
  // character in it is important.
  // order[0]=a means ascending; d means descending.
  // sortType[0]=q means quikxsort, i means insertion sort,
  //             n means nosort, otherwise heapsort.
  TableIterProxy (TableProxy& tab, const Vector<String>& columns,
		  const String& order, const String& sortType);

  // Copy constructor (copy semantics).
  TableIterProxy (const TableIterProxy&);

  ~TableIterProxy();

  // Assignment (copy semantics).
  TableIterProxy& operator= (const TableIterProxy&);

  // Is the internal iterator object null?
  Bool isNull() const
    { return iter_p.isNull(); }

  // Get the TableIterator object.
  const TableIterator& iterator() const
    { return iter_p; }

  // Get the next subtable and return it in the TableProxy argument.
  // When no more subtables are available, it returns False.
  Bool next (TableProxy& table);

private:
  TableIterator iter_p;
  Bool          firstTime_p;           //# True = first time
};


} //# NAMESPACE CASA - END


#endif
