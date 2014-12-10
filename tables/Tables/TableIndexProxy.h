//# TableIndexProxy.h: Proxy for table index access
//# Copyright (C) 2002,2005
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

#ifndef TABLES_TABLEINDEXPROXY_H
#define TABLES_TABLEINDEXPROXY_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/ColumnsIndex.h>
#include <casacore/tables/Tables/ColumnsIndexArray.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableProxy;


// <summary>
// Proxy for table index access.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Paul Shannon" date="1995/09/15" tests="tgtable.g" demos="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> class ColumnsIndex
//   <li> class ColumnsIndexArray
// </prerequisite>

// <synopsis> 
// TableIndexProxy gives access to indexed access to tables, both for
// scalar columns and array columns.
// It is primarily meant to be used in classes that wrap access to it
// from scripting languages (like Glish and Python).
// However, it can also be used directly from other C++ code.
//
// A TableIndexProxy object is usually created by class
// <linkto class=TableProxy>TableProxy</linkto>.
// </synopsis>

class TableIndexProxy
{
public:
  // Construct for the given columns in the table.
  TableIndexProxy (const TableProxy& table,
		   const Vector<String>& columnNames, Bool noSort);

  // Copy constructor.
  TableIndexProxy (const TableIndexProxy&);

  ~TableIndexProxy();

  // Are all keys in the index unique?
  Bool isUnique() const;

  // Return the names of the columns forming the index.
  Vector<String> columnNames() const;

  // Something has changed in the table, so the index has to be recreated.
  // An empty vector means that all columns have changed, otherwise
  // only the given columns.
  void setChanged (const Vector<String>& columnNames);

  // Find the row number matching the key. All keys have to be unique,
  // otherwise an exception is thrown.
  // If no match is found, -1 is returned.
  Int getRowNumber (const Record& key);

  // Find the row numbers matching the key. It should be used instead
  // of <src>getRowNumber</src> if the same key can exist multiple times.
  Vector<Int> getRowNumbers (const Record& key);

  // Find the row numbers matching the key range. The boolean arguments
  // tell if the lower and upper key are part of the range.
  Vector<Int> getRowNumbersRange (const Record& lower, const Record& upper,
				  Bool lowerInclusive, Bool upperInclusive);

private:
  // Assignment is forbidden.
  TableIndexProxy& operator= (const TableIndexProxy&);


  ColumnsIndex*      scaIndex_p;
  ColumnsIndexArray* arrIndex_p;
};


} //# NAMESPACE CASACORE - END


#endif
