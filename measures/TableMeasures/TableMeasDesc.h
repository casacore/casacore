//# TableMeasDesc.h: Definition of a Measure in a Table.
//# Copyright (C) 1997,1999,2000,2001
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

#ifndef MEASURES_TABLEMEASDESC_H
#define MEASURES_TABLEMEASDESC_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/TableMeasures/TableMeasDescBase.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class String;
class Table;
class TableMeasRefDesc;
class TableMeasValueDesc;

// <summary>
// Definition of a Measure column in a Table.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1999/12/23" tests="tTableMeasures.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto module=Measures>Measures</linkto>
//   <li> <linkto module=Tables>Tables</linkto>
// </prerequisite>

// <synopsis>
// The TableMeasures system was created to add support for Measure
// columns to the Casacore Table system.
// Measures are not a fundamental type of the Tables system and hence
// cannot be represented directly.  Instead a Measure column can be created
// with the aid of
// the TableMeasDesc class hierarchy.  The TableMeasDesc class hierarchy
// creates a Measure column by associating some number of fundamental data
// type Table
// columns into a unit.  The associations between these columns
// is represented in the column keywords of each of
// the columns which make up a specific Measure column.
//
// Creating and using Measure columns
// is a three step process:
// <ol>
// <li> For each Measure column some number of columns are defined and added
//      to the Table descriptor.
// <li> A TableMeasDesc object is used to define a (empty) Measure column
//      from the columns created in the first step.
// <li> <linkto class="ScalarMeasColumn">(RO)ScalarMeasColumns</linkto> or
//  <linkto class="ArrayMeasColumn">(RO)ArrayMeasColumns</linkto> objects
//   are used to access the Measure column for the reading and writing
//   of Measures.
// </ol>
//
// Defining a Measure column (that is, steps 1 and 2 above) is the more complex
// operation.  However, for each Measure column it is a once only operation.
// After a Measure column has been created its subsequent use is not
// much different to using "ordinary" Table columns. For information
// on how to use a Measure column see the
// <linkto class="ScalarMeasColumn">(RO)ScalarMeasColumns</linkto> and
// <linkto class="ArrayMeasColumn">(RO)ArrayMeasColumns</linkto> classes.
// <p>
// The TableMeasDesc class hierarchy contains classes for defining each
// component of the Measures to be contained in column.  A
// <linkto class="TableMeasOffsetDesc">TableMeasOffsetDesc</linkto> is used
// to specify the offset component, a
// <linkto class="TableMeasRefDesc">TableMeasRefDesc</linkto> to set up
// the reference code component and a
// <linkto class="TableMeasValueDesc">TableMeasValueDesc</linkto> names the
// column used as the main Measure column through which the
// Measure column is subsequently accessed.
// <br>
// The final step needed to create a Measure column is the creation of a
// TableMeasDesc object whose
// constructor takes a TableMeasValueDesc and (optionally) a
// TableMeasRefDesc.   After construction the TableMeasDesc object's
// write() member is used to make the
// the Measure column persistent within the Table.
// <p>
// The following examples demonstrate the creation of Measure columns using
// the above components.  Further details about each of these components
// is available with each class description.
// <br>
// All examples write the measure description into a TableDesc object,
// i.e. the argument used in the TableMeasDesc::write function is a
// TableDesc object. It is, however, also possible to write them
// into a Table object which is useful if measure columns are added
// to an already existing table (see example 2).
// </synopsis>

// <example>
//<ol>
// <li> The following creates a MEpoch column with a fixed reference.
// <srcblock>
//    // Need a table to work with.
//    TableDesc td("measureTable_desc", "1", TableDesc::New);
//    td.comment() = "A test of TableMeasures class.";
//
//    // Define a column and add it to the table
//    // The main measure column is always an Array column of type Double
//    ArrayColumnDesc<Double> cdTime("Time", "An MEpoch column");
//    td.addColumn(cdtime);
//
//    // Create the Measure column for an MEpoch.  The MEpoch in
//    // the column has reference code MEpoch::TAI
//    TableMeasRefDesc measRef(MEpoch::TAI);
//    TableMeasValueDesc measVal(td, "Time");
//    TableMeasDesc<MEpoch> mepochCol(measVal, measRef);
//    // write makes the Measure column persistent.
//    mepochCol.write(td);
//
//    // create the table with 5 rows
//    SetupNewTable newtab("MeasuresTable", td, Table::New);
//    Table tab(newtab, 5);
// </srcblock>

// <li> Same as example above, but for an already existing table.
// <srcblock>
//    // Need a table to work with.
//    TableDesc td("measureTable_desc", "1", TableDesc::New);
//    td.comment() = "A test of TableMeasures class.";
//
//    // Define a column and add it to the table
//    // The main measure column is always an Array column of type Double
//    ArrayColumnDesc<Double> cdTime("Time", "An MEpoch column");
//    td.addColumn(cdtime);
//
//    // create the table with 5 rows
//    SetupNewTable newtab("MeasuresTable", td, Table::New);
//    Table tab(newtab, 5);
//
//    // Create the Measure column for an MEpoch.  The MEpoch in
//    // the column has reference code MEpoch::TAI
//    TableMeasRefDesc measRef(MEpoch::TAI);
//    TableMeasValueDesc measVal(tab.tableDesc(), "Time");
//    TableMeasDesc<MEpoch> mepochCol(measVal, measRef);
//    // write makes the Measure column persistent.
//    mepochCol.write(tab);
// </srcblock>

// <li> An MEpoch column with a variable reference code with a fixed offset:
// <srcblock>
//    // The following three columns will be used to set up a Scalar MEpoch
//    // column with variable references and offsets.  3 columns are needed.
//    // The "main" column where the MEpoch will be stored
//    ArrayColumnDesc<Double> cdTime("Time", "An MEpoch column");

//    // Variable (i.e., per row) reference code storage needs a column.
//    // The column type is either Int or String (Int is faster but String
//    // may be useful when browsing the table).  Either a Scalar column or
//    // Array column can be used here dependent on whether a Scalar or
//    // Array Measure column is used and whether in case of an Array Measure
//    // column the reference code has to be variable per array element.
//    ScalarColumnDesc<Int> cdRef("TimeRef", "Reference column for Time");
//
//    // add the columns to the Table decriptor
//    td.addColumn(cdTime);
//    td.addColumn(cdRef);
//
//    // now create the MEpoch column.
//    // want a fixed offset.  Offsets are Measures
//    MEpoch offset(MVEpoch(MVTime(1996, 5, 17), MEpoch::UTC);
//    TableMeasOffsetDesc offsetDesc(offset);
//    // the reference
//    TableMeasRefDesc measRef(td, "TimeRef", offsetDesc);
//    // the value descriptor, create and write the column
//    TableMeasValueDesc measVal(td, "Time");
//    TableMeasDesc<MEpoch> mepochCol(measVal, measRef);
//    mepochCol.write();
//
//    // create the table, etc
//    ...
// </srcblock>
//
// <li> An MEpoch column with a variable reference code and offset
// <srcblock>
//    // Variable (per row storage of) offsets needs its own column. Measure
//    // offsets are Measures therefore a Measure column is needed.
//    ArrayColumnDesc<Double> cdOffset("OffsetCol", "Variable Offset col");
//
//    // A column for the variable reference code
//    ScalarColumnDesc<String> cdRef("RefCol", "Variable reference column");
//
//    // The main (value) column for the Measure column
//    ArrayColumnDesc<Double> cdTime("Time", "MEpoch column");
//
//    // add the column descriptors to the table
//    td.addColumn(cdOffset);
//    td.addColumn(cdRef);
//    td.addColumn(cdTime);
//
//    // Create the Measure column
//
//    // The offset column is itself a Measure column, but write() is not
//    // called
//    TableMeasValueDesc offsetVal(td, "OffsetCol");
//    TableMeasDesc<MEpoch> offset(offsetVal);
//    TableMeasOffsetDesc offsetDesc(offset);
//
//    // the reference
//    TableMeasRefDesc ref(td, "RefCol", offsetDesc);
//
//    // create the Measure column
//    TableMeasValueDesc val(td, "Time");
//    TableMeasDesc<MEpoch> mepochCol(val, ref);
//    mepochCol.write();

//    // create the table, etc
//    ...
// </srcblock>
//</ol>
// </example>

// <motivation>
// Creating the required keyword for the definition of a Measure
// in a Table is somewhat complicated. This class assists in that
// process.
// </motivation>
//
// <thrown>
//    <li>AipsError if a reference code string is invalid.
// </thrown>
//
//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


template<class M> class TableMeasDesc : public TableMeasDescBase
{
public:
  // Constructor with measure value descriptor.  The Measure reference for
  // the column will be the default reference code for M.  Units for the
  // column will be the default for the Measure type.
  TableMeasDesc (const TableMeasValueDesc&);

  // Constructor with measure value descriptor and Vector of Units.
  // The Measure reference for the column will be the default reference
  // code for the Measure type.   Number of Units must be compatible
  // with the Measure.
  TableMeasDesc (const TableMeasValueDesc&, const Vector<Unit>&);

  // Constructor with value and reference descriptors. Units for the
  // column will be the default for Measure type.
  TableMeasDesc (const TableMeasValueDesc&, const TableMeasRefDesc&);

  // Constructor with value and reference descriptors and Vector of
  // Units. Number of Units must be compatible with the Measure.
  TableMeasDesc (const TableMeasValueDesc&, const TableMeasRefDesc&,
		 const Vector<Unit>&);

  // Clone the object.
  virtual TableMeasDescBase* clone() const;

  // Copy constructor (copy semantics).
  TableMeasDesc (const TableMeasDesc<M>& that);

  ~TableMeasDesc();

  // Assignment operator (copy semantics)
  TableMeasDesc<M>& operator= (const TableMeasDesc<M>& that);
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/measures/TableMeasures/TableMeasDesc.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
