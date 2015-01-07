//# TableQuantumDesc.h: Defines a Quantum column in a Table.
//# Copyright (C) 1997,1998,1999,2000,2001
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

#ifndef MEASURES_TABLEQUANTUMDESC_H
#define MEASURES_TABLEQUANTUMDESC_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableDesc;
class Table;
class TableRecord;
class TableColumn;
class Unit;


// <summary>
// A class for defining Quantum columns in Tables.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1999/12/23" tests="tTableQuantum.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=Table>Table</linkto>
//   <li> <linkto class=Quantum>Quantum</linkto>
// </prerequisite>

// <synopsis>
// A TableQuantumDesc object is used to define a Quantum column in a Table.
// The use of this object and the associated Scalar- and ArrayQuantColumn
// objects make it possible to store (and retrieve) Quanta in Tables.<br>
//
// TableQuantumDesc objects are analogous to ColumnDesc objects in that they
// add information, describing the characteristics of a column, to the Table
// Descriptor before the Table is created.  However, rather than
// replacing the use of a ColumnDesc object, a TableQuantumDesc is
// used in conjunction with a ColumnDesc in the definition of
// Quantum columns.<br>
//
//   <note role=caution>
//	A good understanding of the Table system is essential
//	before attempting to use this class.
//   </note>
//
// Defining a Quantum column requires the following steps:
// <ol>
// <li> Use a normal Scalar- or ArrayColumnDesc to define a column to use for
//	the Quanta.
// <li> If needed (see
//      <A HREF="#TableQuantumDesc:Quantum Units">below</A>) define a column
//      for the Quantum Units.
// <li> Add the columns to the Table Descriptor.
// <li> Declare a TableQuantumDesc to associate the column defined in step 1
//	and the Unit column from step 2 and update the Table Descriptor.
// <li> Setup and create the Table.
// </ol>
// It is also possible to define a Quantum column after the table is created.
// which is useful when columns (to be used for quanta) are added to
// an already existing table. <br>
//
// The type of the quantum columns must match the type of the underlying
// Quanta that are to be stored in the column.  Hence, for a column of
// Quantum&lt;Complex&gt; a ScalarColumnDesc&lt;Complex&gt; must be used.<br>
//
// As with standard Table Columns Quanta can be stored in Scalar and Array
// columns. This must be specified in advance by using either a
// Scalar- or ArrayColumnDesc.<br>
//
// After the Table has be created a Quantum column can be accessed for writing
// and reading of Quanta via the
// <linkto class="ScalarQuantColumn">(RO)ScalarQuantColumn&lt;T&gt;</linkto>
// and
// <linkto class="ArrayQuantColumn">(RO)ArrayQuantColumn&lt;T&gt;</linkto>
// objects.
//
// <ANCHOR NAME="TableQuantumDesc:Quantum Units">
// <h3>Quantum Units</h3></ANCHOR>
// The treatment of the Unit component of a Quantum in the TableQuantumDesc
// class varies depending on your needs.    The main consideration
// is whether the Quanta to be stored in a specific column are to have the
// same Unit or whether their Units could differ.  In the simple case,
// where the
// Quanta have the same unit, a TableQuantumDesc is declared with the
// Unit value specified as a parameter. The following defines a Quantum
// column with units "deg":
//
// <srcblock>
//      ScalarColumnDesc<Double> scd("QuantumCol");
//      ...
//      // defines QuantumCol as a Quantum column with fix Units "deg"
//      TableQuantumDesc tqd(td, "QuantumCol", Unit("deg"));
// </srcblock>
//
// This constructor stores the value for the Unit as a
// column keyword.  In situations, however, where it is necessary to
// store a distinct Unit with each Quantum, it is necessary to define
// an additional column for storing the Unit component of each Quantum.
// The TableQuantumDesc constructor for this takes the name of
// the Unit column as
// a parameter.  Hence an additional column must be defined for storing the
// Units and its type must be string.  The following
// example shows how to set up a Quantum column with support for Quantum
// unit variability:
//
// <srcblock>
//      // the quanta values stored here
//      ScalarColumnDesc<Double> scd("QuantumCol");
//      // a String column for the Units
//      ScalarColumnDesc<String> scd("QuantumUnitCol");
//      ...
//      TableQuantumDesc tqd(td, "QuantumCol", "QuantumUnitCol");
// </srcblock>
//
// One further consideration is that for Array Quantum Columns it is
// necessary to
// decide on a level of granularity for the Unit storage you would like.
// In Array Quantum columns it is possible to store a distinct Unit per row or
// per array element per row.  This distinction is established when the
// Unit column is declared.  Defining a ScalarColumn for Units specifies per
// row variability, that is, each row in an array column of Quanta will
// have the same unit.  Alternatively, use of an ArrayColumn for the Unit
// column
// specifies that every Quantum stored will have its unit stored as well.
// In both cases the Unit column's type must be String.  The following
// defines an Array Quantum Column with per row Unit storage:
//
// <srcblock>
//      // for the Quanta values
//      ArrayColumnDesc<Double> scd("ArrayQuantumCol");
//      // per row storage of units
//      ScalarColumnDesc<String> scd("QuantumUnitCol");
//      ...
//      TableQuantumDesc tqd(td, "ArrayQuantumCol", "QuantumUnitCol");
// </srcblock>
//
// And finally, an array Quantum Column with an Array Unit Column:
//
// <srcblock>
//      // for Quanta values
//      ArrayColumnDesc<Double> scd("ArrayQuantumCol");
//      // per element storage of Units
//      ArrayColumnDesc<String> scd("ArrayUnitCol");
//      ...
//      TableQuantumDesc tqd(td, "ArrayQuantumCol", "ArrayUnitCol");
// </srcblock>
//
//
// After constructing an TableQuantumDesc object use of the write() member
// updates the Table Descriptor or Table object.
// <linkto class="ScalarQuantColumn">(RO)ScalarQuantColumn&lt;T&gt;</linkto>
// and
// <linkto class="ArrayQuantColumn">(RO)ArrayQuantColumn&lt;T&gt;</linkto>
// are subsequently used to read-only and read/write access the Quantum
// Columns.
// </synopsis>
//
// <example>
// <srcblock>
//     // create a table descriptor as normal
//     TableDesc td("measTD", "1", TableDesc::New);
//     td.comment() = "A table containing measures and quantums";
//
//     // This example sets up a Quantum<Complex> column but any valid Quantum
//     // type can be specified.  However, the type of the Quantums to be
//     // stored must match the type of the underlying table column.
//     ScalarColumnDesc<Complex> tcdQCplx("Quant", "A quantum complex column");
//
//     // For a Quantum array column an ArrayColumnDesc is first defined
//     ArrayColumnDesc<Double> tcdQDoub("QuantArray", "A quantum array col");
//
//     // The QuantumArray column has variable units.  A string is needed
//     // for these.  This could be done in two ways depending on what is
//     // wanted.  Units can vary per element of array per row or
//     // just per row.  In the first instance an ArrayColumn<String> would be
//     // require.  Here we want to vary units only per row.
//     ScalarColumnDesc<String> tcdUnits("VarQuantUnits", "Quantum units");
//
//     // Add the columns to the Table Descriptor
//     td.addColumn(tcdQplx);
//     td.addColumn(tcdQDoub);
//     td.addColumn(tcdUnits);
//
//     // Create the TableQuantumDesc with units "deg" and an Array Quantum
//     // Column with per row Unit granularity
//     TableQuantumDesc tqdS(td, "Quant", unit("deg"));
//     TableQuantumDesc tqdA(td, "QuantArray", "VarQuantUnits");
//
//     // Update the Table Descriptor
//     tqdA.write(td);
//     tqdS.write(td);
//
//     // Setup and create the new table as usual.
//     SetupNewTable newtab("mtab", td, Table::New);
//     Table qtab(newtab);
//
//     // Now ScalarQuantColumn and ArrayQuantColumn objects could be
//     // constructed to access the columns...
// </srcblock>
// Note that writing the Quantum description could also be done
// after the table is created. It is meaningless in this case, but
// it is useful when columns (to be used for quanta) are added to
// an already existing table.
// be used as 
// <srcblock>
//     // Setup and create the new table as usual.
//     SetupNewTable newtab("mtab", td, Table::New);
//     Table qtab(newtab);
//
//     // Update the Table Descriptor
//     tqdA.write(qtab);
//     tqdS.write(qtab);
// </srcblock>
// </example>

// <motivation>
// This class assists in the definition of a Quantum Table Column.
// </motivation>

// <thrown>
//    <li>AipsError during construction if the column doesn't exist.
//    <li>AipsError during construction if the unit's column doesn't
//	  exist (when variable units).
//    <li>AipsError during construction if the type of the variable unit's
//	  column is not String.
//    <li>AipsError during a reconstruct if the column doesn't have a Unit.
// </thrown>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>

class TableQuantumDesc
{
public:
  // Constructs a Quantum column descriptor with null units (Unit == "").
  // The column should have already been added to the TableDesc.
  // An exception is thrown if the column doesn't exist.
  TableQuantumDesc (const TableDesc& td, const String& column);

  // Constructs a Quantum column descriptor with the specified Quantum unit.
  // The column should have already been added to the TableDesc.
  // An exception is thrown if the column doesn't exist.
  TableQuantumDesc (const TableDesc& td, const String& column, const Unit&);

  // Constructs a Quantum column descriptor with the specified Quantum units.
  // The column should have already been added to the TableDesc.
  // An exception is thrown if the column doesn't exist.
  // <group>
  TableQuantumDesc (const TableDesc& td, const String& column,
		    const Vector<String>& unitNames);
  TableQuantumDesc (const TableDesc& td, const String& column,
		    const Vector<Unit>&);
  // </group>

  // Constructs a Quantum column descriptor with variable units stored in
  // unitCol.  Both the quantum and unit column should exist in the
  // TableDesc.
  //# Note that the Char* constructor is needed, otherwise the compiler
  //# cannot choose between String and Unit.
  //<group>
  TableQuantumDesc (const TableDesc& td, const String& column,
		    const String& unitCol);
  TableQuantumDesc (const TableDesc& td, const String& column,
		    const Char* unitCol);
  //</group>

  // Copy constructor (copy semantics).
  TableQuantumDesc (const TableQuantumDesc& that);

  ~TableQuantumDesc();

  // Reconstructs a previously constructed TableQuantumDesc.
  static TableQuantumDesc* reconstruct (const TableDesc& td,
					const String& column);

  // Assignment.
  TableQuantumDesc& operator= (const TableQuantumDesc& that);

  // Returns the Quantum column descriptor's units.  A empty vector is
  // returned if units have not been specified.  This could be because the null
  // unit constructor was used or because the units are variable.
  const Vector<String>& getUnits() const
    { return itsUnitsName; }

  // Returns True if descriptor set for variable units (one per row)
  Bool isUnitVariable() const
    { return (! itsUnitsColName.empty()); }

  // Returns the name of the quantum column.
  const String& columnName() const
    { return itsColName; }

  // Returns the name of the units column (an empty String is returned
  // if the units are not variable).
  const String& unitColumnName() const
    { return itsUnitsColName; }

  // Makes the TableQuantumDesc persistent (updates the Table Descriptor).
  // <group>
  void write (TableDesc&);
  void write (Table&);
  // </group>

  // Does this column contain table quanta?
  static Bool hasQuanta (const TableColumn& column);

private:
  // Name of column which stores the Quantum's values.
  String itsColName;
  // The Quantum's unit as a string.
  Vector<String> itsUnitsName;
  // Name of units column if units are variable.
  String itsUnitsColName;


  // Write the actual keywords.
  void writeKeys (TableRecord& columnKeyset);

  // Throw an exception if the quantum column doesn't exist.
  void checkColumn (const TableDesc& td) const;

  // Throw an exception if the variable units column isn't a string column.
  void checkUnitsColumn (const TableDesc& td) const;
};



} //# NAMESPACE CASACORE - END

#endif
