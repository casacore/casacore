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
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/TableIter.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

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
// TableIterProxy gives access to the table iterator functionality.
// It is primarily meant to be used in classes that wrap access to it
// from scripting languages (like Glish and Python).
// However, it can also be used directly from other C++ code.
//
// A TableIterProxy object is usually created by class
// <linkto class=TableProxy>TableProxy</linkto>.
// </synopsis>

// <example>
// <srcblock>
//    // Get a table proxy.
//    TableProxy proxy("sometable");
//    Vector<String> columns(1, "SOMECOL");
//    TableIterProxy tgi (proxy, columns, "a", "q");
//    TableProxy subTable;
//    // Iterate through the table.
//    while (tgi.next (subTable)) {
//       ..use Table object subTable.table()
//    }
// </srcblock>
// </example>

class TableIterProxy
{
public:
  // Default constructor initializes to not open.
  // This constructor is only needed for the Block container.
  TableIterProxy();

  // Construct iterator for the given table column(s).
  // Order and sortType are case-insentive strings and only the first
  // character in it is important.
  // <br>order[0]=a means ascending; d means descending.
  // <br>sortType[0]=q means quicksort, i means insertion sort,
  //                 n means nosort, h means heapsort, otherwise parsort
  // <br>For each column an iteration interval can be given making it possible
  // to iterate in e.g. time chunks of 1 minute. Not given or zero means
  // no interval is given for that column, thus a normal comparison is done.
  // It can only be used for numerical columns (not complex).
  // However, if for a string column the interbval is set to non-zero, it
  // means that case-insensitive comparison will be used.
  TableIterProxy (const TableProxy& tab, const Vector<String>& columns,
		  const String& order, const String& sortType,
                  const Vector<Double>& intervals = Vector<Double>());

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
  Bool nextPart (TableProxy& table);

  // Iterate to the next part (for Python use).
  // An IterError exception is thrown at the end of the loop.
  TableProxy next();

  // Reset the iterator (for Python use).
  void reset();


private:
  // Make an iterator where iteration intervals may have been given.
  void makeStepIter (const Table& tab,
                     const Block<String>& columns,
                     const Vector<Double>& iterSteps,
                     TableIterator::Order order,
                     TableIterator::Option sortType);

  //# Data members
  TableIterator iter_p;
  Bool          firstTime_p;           //# True = first time
};


} //# NAMESPACE CASACORE - END


#endif
