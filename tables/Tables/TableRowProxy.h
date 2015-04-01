//# TableRowProxy.h: Proxy for table row access
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

#ifndef TABLES_TABLEROWPROXY_H
#define TABLES_TABLEROWPROXY_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/TableRow.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableProxy;
class Record;


// <summary>
// Proxy for table row access.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Paul Shannon" date="1995/09/15" tests="tgtable.g" demos="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> class TableRow
// </prerequisite>

// <etymology>
// TableRowProxy holds a TableRow object for the table
// glish client.
// </etymology>

// <synopsis> 
// TableRowProxy gives access to row-based table accessor functions.
// It is primarily meant to be used in classes that wrap access to it
// from scripting languages (like Glish and Python).
// However, it can also be used directly from other C++ code.
//
// A TableRowProxy object is usually created by class
// <linkto class=TableProxy>TableProxy</linkto>.
// </synopsis>

class TableRowProxy
{
public:
  // Default constructor is only needed for the Block container.
  TableRowProxy();

  // Construct for the given columns in the table.
  TableRowProxy (const TableProxy& table,
		 const Vector<String>& columnNames, Bool exclude);

  // Copy constructor (copy semantics).
  TableRowProxy (const TableRowProxy&);

  ~TableRowProxy();

  // Assignment (copy semantics).
  TableRowProxy& operator= (const TableRowProxy&);

  // Test if the underlying TableRow object is invalid.
  Bool isNull() const;

  // Test if values can be written.
  Bool isWritable() const;

  // Get values for the given row.
  Record get (uInt rownr) const;

  // Put values for the given row.
  // The given record has to conform the fields in the table row.
  void put (uInt rownr, const Record& values, Bool matchingFields);

private:
  Bool       isWritable_p;
  ROTableRow rorow_p;
  TableRow   rwrow_p;
};


inline Bool TableRowProxy::isWritable() const
{
  return isWritable_p;
}


} //# NAMESPACE CASACORE - END


#endif
