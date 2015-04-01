//# ConcatScalarColumn.h: A typed scalar column in a concatenated table
//# Copyright (C) 2008
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

#ifndef TABLES_CONCATSCALARCOLUMN_H
#define TABLES_CONCATSCALARCOLUMN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/ConcatColumn.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary>
  // A typed column in a concatenated table
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="UNKNOWN" date="" tests="">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> ConcatTable
  //   <li> BaseColumn
  // </prerequisite>

  // <etymology>
  // ConcatTable represents a column in a ConcatTable. A ConcatTable is a table
  // referencing another table, usually as the result of a select, etc..
  // </etymology>

  // <synopsis> 
  // ConcatColumn handles the access of a column in a ConcatTable.
  // It calls the corresponding function in the referenced column
  // while converting the given row number to the row number in the
  // referenced table.
  // </synopsis> 

  // <motivation>
  // This class is untyped, i.e. not templated.
  // Every call is sent to the underlying referenced BaseColumn which
  // is typed by the virtual function mechanism.
  // A ConcatColumn can never be used directly. A user always has to
  // construct a typed ArrayColumn or ScalarColumn object to access a column.
  // This means everyting is fully type safe.
  // </motivation>

  // <todo asof="$DATE:$">
  //# A List of bugs, limitations, extensions or planned refinements.
  //   <li> Act upon removal of rows or the underlying column
  // </todo>

  template<typename T>
  class ConcatScalarColumn : public ConcatColumn
  {
  public:
    // Construct the ConcatColumn. It will point to the given column
    // description, ConcatTable and referenced column.
    // The ConcatTable will be used to convert the rownr to the rownr
    // in the referenced column.
    ConcatScalarColumn (const BaseColumnDesc*, ConcatTable*);

    ~ConcatScalarColumn();

    // Get the vector of all scalar values in a column.
    virtual void getScalarColumn (void* dataPtr) const;

    // Get the vector of some scalar values in a column.
    virtual void getScalarColumnCells (const RefRows& rownrs,
				       void* dataPtr) const;

    // Put the vector of all scalar values in the column.
    virtual void putScalarColumn (const void* dataPtr);

    // Get the vector of some scalar values in a column.
    virtual void putScalarColumnCells (const RefRows& rownrs,
				       const void* dataPtr);

    // Handle the creation and deletion of sort keys.
    // <group>
    virtual void makeSortKey (Sort& sortobj,
                              CountedPtr<BaseCompare>& cmpObj,
			      Int order,
			      const void*& dataSave);
    virtual void makeRefSortKey (Sort& sortobj,
                                 CountedPtr<BaseCompare>& cmpObj,
				 Int order,
				 const Vector<uInt>& rownrs,
				 const void*& dataSave);
    virtual void fillSortKey (const Vector<T>* vecPtr,
			      Sort& sortobj,
                              CountedPtr<BaseCompare>& cmpObj,
			      Int order);
    virtual void freeSortKey (const void*& dataSave);
    // </group>

  };

} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/tables/Tables/ConcatScalarColumn.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
