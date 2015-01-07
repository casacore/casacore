//# TableMeasColumn.h: Access to Measure Columns in Tables
//# Copyright (C) 1999,2000
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

#ifndef MEASURES_TABLEMEASCOLUMN_H
#define MEASURES_TABLEMEASCOLUMN_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/casa/Utilities/CountedPtr.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class String;
class Table;
class TableMeasDescBase;


// <summary>
// Read only access to table scalar Measure columns.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1999/12/23" tests="tTableMeasures.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto module=Measures>Measures</linkto>
//   <li> <linkto module=Tables>Tables</linkto>
//   <li> <linkto class=TableMeasDesc>TableMeasDesc</linkto>
// </prerequisite>

// <synopsis>
// TableMeasColumn is the base class for the templated classes
// <linkto class=ScalarMeasColumn>ScalarMeasColumn</linkto> and
// <linkto class=ArrayMeasColumn>ArrayMeasColumn</linkto>
// which give access to table columns containing
// <linkto module=Measures>measures</linkto>.
//
// This base class offers some common functionality like getting
// the column name and testing if a row of the column contains a value.
// Its main function is <src>measDesc()</src>, which gives access
// to the <linkto class=TableMeasDescBase>TableMeasDescBase</linkto>
// object containing a description of the measure column.
// </synopsis>

// <example>
// <srcblock>
//     // Create the object for measure column Time1.
//     TableMeasColumn timeCol(tab, "Time1");
// 	
//     // print some details about the column
//     if (timeCol.measDesc().isRefCodeVariable()) {
//        cout << "The column has variable references." << endl;
//     } else {
//         cout << "The fixed MeasRef for the column is: "
//	        << timeCol.getMeasRef() << endl;
//     }
// </srcblock>
// </example>

// <motivation>
// This class contains the common functionality for the templated
// derived classes.
// </motivation>

//# <todo asof="$DATE:$">
//# </todo>


class TableMeasColumn
{
public:
  // The default constructor creates a null object.  Useful for creating
  // arrays of ScalarMeasColumn objects.  Attempting to use a null object
  // will produce a segmentation fault so care needs to be taken to
  // initialise the objects first by using attach().
  // An ScalarMeasColumn object can be tested if it is null by using the
  // isNull() member.
  TableMeasColumn();

  // Create the ScalarMeasColumn from the table and column Name.
  TableMeasColumn (const Table& tab, const String& columnName);

  // Copy constructor (copy semantics).
  TableMeasColumn (const TableMeasColumn& that);

  virtual ~TableMeasColumn();

  // Change the reference to another column.
  void reference (const TableMeasColumn& that);

  // Attach another column to the object.
  void attach (const Table& tab, const String& columnName);

  // Tests if a row contains a Measure (i.e., if the row has a defined
  // value).
  Bool isDefined (uInt rownr) const;

  // Get access to the TableMeasDescBase describing the column.
  // <group>
  const TableMeasDescBase& measDesc() const
    { return *itsDescPtr; }
  TableMeasDescBase& measDesc()
    { return *itsDescPtr; }
  // </group>

  // Test if the object is null.
  Bool isNull() const
    { return itsDescPtr.null(); }

  // Throw an exception if the object is null.
  void throwIfNull() const;

  // Get the name of the column.
  const String& columnName() const;

  // Get the Table object this column belongs to.
  Table table() const;

  // Is the column a scalar measures column?
  // It is if the underlying column is a scalar column or an array
  // column with a fixed 1-dimensional shape.
  // <br>Otherwise it is an array measures column.
  // <note role=caution>
  // It is not 100% determined if a measure column is an array or a scalar.
  // If the underlying table column is an array with a variable shape,
  // this function will see it as an array measure column. However,
  // it might be accessible as a scalar measure column.
  // </note>
  Bool isScalar() const;

protected:
  //# The measure's value is represented by this many data components.
  uInt itsNvals;
  //# The Measure Column description.
  CountedPtr<TableMeasDescBase> itsDescPtr;
  //# The data column.
  TableColumn itsTabDataCol;
  //# Does the measure column have a variable reference or offset?
  Bool itsVarRefFlag;
  Bool itsVarOffFlag;

private:
  // Assignment makes no sense in a readonly class.
  // Declaring this operator private makes it unusable.
  TableMeasColumn& operator= (const TableMeasColumn& that);
};

// For backwards compatibility:

#define ROTableMeasColumn TableMeasColumn

} //# NAMESPACE CASACORE - END

#endif
