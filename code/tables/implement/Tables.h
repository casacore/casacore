//# Tables.h: The Tables module - AIPS++ data storage
//# Copyright (C) 1994,1995,1996,1997,1998
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

#if !defined(AIPS_TABLES_H)
#define AIPS_TABLES_H

//# Includes
//#   table description
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/SubTabDesc.h>

//#   storage managers
#include <aips/Tables/StManAipsIO.h>
#include <aips/Tables/IncrementalStMan.h>
#include <aips/Tables/IncrStManAccessor.h>
#include <aips/Tables/TiledDataStMan.h>
#include <aips/Tables/TiledDataStManAccessor.h>
#include <aips/Tables/TiledCellStMan.h>
#include <aips/Tables/TiledColumnStMan.h>
#include <aips/Tables/TiledShapeStMan.h>

//#   virtual column engines
#include <aips/Tables/RetypedArrayEngine.h>
#include <aips/Tables/RetypedArraySetGet.h>
#include <aips/Tables/ScaledArrayEngine.h>
#include <aips/Tables/ForwardCol.h>
#include <aips/Tables/ForwardColRow.h>

//#   table access
#include <aips/Tables/Table.h>
#include <aips/Tables/TableLock.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/TableRow.h>
#include <aips/Arrays/Array.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Lattices/Slice.h>

//#   keywords
#include <aips/Tables/TableRecord.h>
#include <aips/Containers/RecordField.h>

//#   table expressions (for selection of rows)
#include <aips/Tables/ExprNode.h>
#include <aips/Tables/ExprNodeSet.h>
#include <aips/Tables/TableParse.h>

//#   table vectors
#include <aips/Tables/TableVector.h>
#include <aips/Tables/TabVecMath.h>
#include <aips/Tables/TabVecLogic.h>


// <module>

// <summary>
// Tables are the data storage mechanism for AIPS++
// </summary>

// <use visibility=export>

// <reviewed reviewer="jhorstko" date="1994/08/30" tests="" demos="">
// </reviewed>

// <prerequisite>
//    <li> <linkto class="Record:description">Record</linkto> class
// </prerequisite>

// <etymology>
// "Table" is a formal term from relational database theory: 
//   <cite> "The organizing principle in a relational database is the TABLE,
//    a rectangular, row/column arrangement of data values."</cite>
// AIPS++ tables are extensions to traditional tables, but are similar
// enough that we use the same name.  There is also a strong resemblance
// between the uses of AIPS++ tables, and FITS binary tables, which
// provides another reason to use "Tables" to describe the AIPS++ data
// storage mechanism.
// </etymology>

// <synopsis> 
// Tables are the fundamental storage mechanism for AIPS++. This document
// explains <A HREF="#Tables:motivation">why</A> they had to be made,
// <A HREF="#Tables:properties">what</A> their properties are, and 
// <A HREF="#Tables:open">how</A> to use them. The last subject is
// discussed and illustrated in a sequence of sections:
// <UL>
//  <LI> <A HREF="#Tables:open">opening</A> an existing table,
//  <LI> <A HREF="#Tables:read">reading</A> from a table,
//  <LI> <A HREF="#Tables:creation">creating</A> a new table,
//  <LI> <A HREF="#Tables:write">writing</A> into a table,
//  <LI> <A HREF="#Tables:row-access">accessing rows</A> in a table,
//  <LI> <A HREF="#Tables:select and sort">selection and sorting</A>
//       (also <A HREF=../../notes/199/199.html>Table Query Language</A>),
//  <LI> <A HREF="#Tables:iterate">iterating</A> through a table, and
//  <LI> <A HREF="#Tables:vectors">vector operations</A> on a column.
//  <LI> <A HREF="#Tables:LockSync">locking/synchronization</A>
//       for concurrent access.
// </UL>


// <A NAME="Tables:motivation">
// <motivation></A>
//
// The AIPS++ tables are mainly based upon the ideas of Allen Farris,
// as laid out in the
// <A HREF="http://aips2.cv.nrao.edu/aips++/docs/reference/Database.ps">
// AIPS++ Database document</A>, from where the following paragraph is taken:
// 
// <BLOCKQUOTE>
// Traditional relational database tables have two features that
// decisively limit their applicability to scientific data.  First, an item of
// data in a column of a table must be atomic -- it must have no internal
// structure.  A consequence of this restriction is that relational
// databases are unable to deal with arrays of data items.  Second, an
// item of data in a column of a table must not have any direct or
// implied linkages to other items of data or data aggregates.  This
// restriction makes it difficult to model complex relationships between
// collections of data.  While these restrictions may make it easy to
// define a mathematically complete set of data manipulation operations,
// they are simply intolerable in a scientific data-handling context.
// Multi-dimensional arrays are frequently the most natural modes in
// which to discuss and think about scientific data.  In addition,
// scientific data often requires complex calibration operations that
// must draw on large bodies of data about equipment and its performance
// in various states.  The restrictions imposed by the relational model
// make it very difficult to deal with complex problems of this nature.
// </BLOCKQUOTE>
// 
// In response to these limitations, and other needs, the AIPS++ tables were
// designed.
// </motivation>

// <A NAME="Tables:properties">
// <h3>Table Properties</h3></A>
//
// AIPS++ tables have the following properties:
// <ul>
//  <li> A table consists of a number of rows and columns.
//       <A HREF="#Tables:keywords">Keyword/value pairs</A> may be defined
//       for the table as a whole and for individual columns. A keyword/value
//       pair for a column could, for instance, define its unit.
//  <li> Each table has a <A HREF="#Tables:Table Description">description</A>
//       which specifies the number and type of columns, and maybe initial
//       keyword sets and default values for the columns. 
//  <li> A cell in a column may contain
//       <UL>
//        <LI> a scalar;
//        <LI> a "direct" array -- which must have the same shape in all
//             cells of a column, is usually small, and is stored in the
//             table itself;
//        <LI> an "indirect" array -- which may have different shapes in
//             different cells of the same column, is arbitrarily large,
//             and is stored in a separate file; or
//        <LI> another table -- allowing a table to express arbitrarily
//             complex data relationships.
//       </UL>
//  <li> A column may be
//       <UL>
//        <LI> "filled" -- containing actual data, or
//	  <LI> "virtual" -- containing a recipe telling how the data will
//             be generated dynamically
//       </UL>
//  <li> Only the standard AIPS++ data types can be used in filled
//       columns, be they scalars or arrays:  Bool, uChar, Short, uShort,
//       Int, uInt, float, double, Complex, DComplex and String.
//  <li> A column can have a default value, which will automatically be stored
//       in a cell of the column, when a row is added to the table.
//  <li> <A HREF="#Tables:Data Managers">Data managers</A> handle the
//       reading, writing and generation of data. Each column in a table can
//       be assigned its own data manager, which allows for optimization of the
//       data storage per column. The choice of data manager determines
//       whether a column is filled or virtual.
// </ul>
// Concurrent access from different processes to the same table is
// fully supported by means of a <A HREF="#Tables:LockSync">
// locking/synchronization</A> mechanism. Concurrent access over NFS is also
// supported.

// <A NAME="Tables:open">
// <h3>Opening an Existing Table</h3></A>
//
// To open an existing table you just create a
// <linkto class="Table:description">Table</linkto> object giving
// the name of the table, like:
//
// <srcblock>
//     Table readonly_table ("tableName");
//     // or
//     Table read_and_write_table ("tableName", Table::Update);
// </srcblock>
//
// The constructor option determines whether the table will be opened as
// readonly or as read/write. A readonly table file must be opened 
// as readonly, otherwise an exception is thrown. The functions
// <linkto class="Table">Table::isWritable(...)</linkto>
// can be used to determine if a table is writable.
//
// When the table is opened, the data managers are reinstantiated
// according to their definition at table creation.

// <A NAME="Tables:read">
// <h3>Reading from a Table</h3></A>
//
// You can read data from a table column with the "get" functions
// in the classes
// <linkto class="ROScalarColumn:description">ROScalarColumn&lt;T&gt;</linkto>
// and
// <linkto class="ROArrayColumn:description">ROArrayColumn&lt;T&gt;</linkto>.
// For scalars of a standard data type (i.e. Bool, uChar, Int, Short,
// uShort, uInt, float, double, Complex, DComplex and String) you could
// instead use 
// <linkto class="ROTableColumn">ROTableColumn::getScalar(...)</linkto> or
// <linkto class="ROTableColumn">ROTableColumn::asXXX(...)</linkto>.
// These functions offer an extra: they do automatic data type promotion;
// so that you can, for example, get a double value from a float column.
//
// These "get" functions are used in the same way as the simple"put"
// functions described in the previous section.
// <p>
// <linkto class="ScalarColumn:description">ScalarColumn&lt;T&gt;</linkto>
// is derived from ROScalarColumn&lt;T&gt;, and
// therefore has the same "get" functions. However, if a
// ScalarColumn&lt;T&gt; object is constructed for a non-writable column,
// an exception is thrown. Only ROScalarColumn&lt;T&gt; objects can be
// constructed for nonwritable columns.
// The same is true for
// <linkto class="ArrayColumn:description">ArrayColumn&lt;T&gt;</linkto> and
// <linkto class="TableColumn:description">TableColumn</linkto>.
// <p>
// A typical program could look like:
// <srcblock>
// #include <aips/Tables/Table.h>
// #include <aips/Tables/ScalarColumn.h>
// #include <aips/Tables/ArrayColumn.h>
// #include <aips/Arrays/Vector.h>
// #include <aips/Lattices/Slicer.h>
// #include <aips/Arrays/ArrayMath.h>
// #include <iostream.h>
// 
// main()
// {
//     // Open the table (readonly).
//     Table tab ("some.name");
//
//     // Construct the various column objects.
//     // Their data type has to match the data type in the table description.
//     ROScalarColumn<Int> acCol (tab, "ac");
//     ROArrayColumn<float> arr2Col (tab, "arr2");
//
//     // Loop through all rows in the table.
//     uInt nrrow = tab.nrow();
//     for (uInt i=0; i<nrow; i++) {
//         // Read the row for both columns.
//         cout << "Column ac in row i = " << acCol(i) << endl;
//         Array<float> array = arr2Col.get (i);
//     }
//
//     // Show the entire column ac,
//     // and show the 10th element of arr2 in each row..
//     cout << ac.getColumn();
//     cout << arr2.getColumn (Slicer(Slice(10)));
// }
// </srcblock>

// <A NAME="Tables:creation">
// <h3>Creating a Table</h3></A>
//
// The creation of a table is a multi-step process:
// <ol>
//  <li>
//   Create a <A HREF="#Tables:Table Description">table description</A>.
//  <li>
//   Create a <linkto class="SetupNewTable:description">SetupNewTable</linkto>
//   object with the name of the new table.
//  <li>
//   Create the necessary <A HREF="#Tables:Data Managers">data managers</A>.
//  <li>
//   Bind each column to the appropriate data manager.
//   The system will bind unbound columns to data managers which
//   are created internally using the default data manager name
//   defined in the column description.
//  <li>
//   Define the shape of direct columns (if that was not already done in the
//   column description).
//  <li>
//   Create the <linkto class="Table:description">Table</linkto>
//   object from the SetupNewTable object. Here, a final check is performed
//   and the necessary files are created.
// </ol>
//
// The following example shows how you can create a table. An example
// specifically illustrating the creation of the
// <A HREF="#Tables:Table Description">table description</A> is given
// in that section. Later sections will discuss the access to the table.
//
// <srcblock>
// #include <aips/Tables/TableDesc.h>
// #include <aips/Tables/SetupNewTab.h>
// #include <aips/Tables/Table.h>
// #include <aips/Tables/ScaColDesc.h>
// #include <aips/Tables/ArrColDesc.h>
// #include <aips/Tables/StManAipsIO.h>
// #include <aips/Tables/IncrementalStMan.h>
// 
// main()
// {
//     // Step1 -- Build the table description.
//     TableDesc td("tTableDesc", "1", TableDesc::Scratch);
//     td.comment() = "A test of class SetupNewTable";
//     td.addColumn (ScalarColumnDesc<Int> ("ab" ,"Comment for column ab"));
//     td.addColumn (ScalarColumnDesc<Int> ("ac"));
//     td.addColumn (ScalarColumnDesc<uInt> ("ad","comment for ad"));
//     td.addColumn (ScalarColumnDesc<float> ("ae"));
//     td.addColumn (ArrayColumnDesc<float> ("arr1",3,ColumnDesc::Direct));
//     td.addColumn (ArrayColumnDesc<float> ("arr2",0));
//     td.addColumn (ArrayColumnDesc<float> ("arr3",0,ColumnDesc::Direct));
// 
//     // Step 2 -- Setup a new table from the description.
//     SetupNewTable newtab("newtab.data", td, Table::New);
//
//     // Step 3 -- Create storage managers for it.
//     StManAipsIO stmanAipsIO_1;
//     StManAipsIO stmanAipsIO_2;
//     IncrementalStMan stmanIncr;
// 
//     // Step 4 -- First, bind all columns to the first storage
//     // manager. Then, bind a few columns to another storage manager
//     // (which will overwrite the previous bindings).
//     newtab.bindAll (stmanAipsIO_1);
//     newtab.bindColumn ("ab", stmanAipsIO_2);
//     newtab.bindColumn ("ae", stmanIncr);
//     newtab.bindColumn ("arr3", stmanIncr);
// 
//     // Step 5 -- Define the shape of the direct columns.
//     // (this could have been done in the column description).
//     newtab.setShapeColumn( "arr1", IPosition(3,2,3,4));
//     newtab.setShapeColumn( "arr3", IPosition(3,3,4,5));
// 
//     // Step 6 -- Finally, create the table consisting of 10 rows.
//     Table tab(newtab, 10);
// 
//     // Now we can fill the table, which is shown in a next section.
//     // The Table destructor will flush the table to the files.
// }
// </srcblock>

// <A NAME="Tables:write">
// <h3>Writing into a Table</h3></A>
//
// Once a table has been created or has been opened for read/write,
// you want to write data into it. Before doing that you may have
// to add one or more rows to the table.
// <note role=tip> When a table was created with a given number of rows, you
// do not need to add rows; you may not even be able to do so.
// </note>
//
// When adding new rows to the table, either via the
// <linkto class="Table">Table(...) constructor</linkto>
// or via the
// <linkto class="Table">Table::addRow(...)</linkto>
// function, you can choose to have those rows initialized with the
// default values given in the description.
//
// To actually write the data into the table you need the classes
// <linkto class="ScalarColumn:description">ScalarColumn&lt;T&gt;</linkto> and
// <linkto class="ArrayColumn:description">ArrayColumn&lt;T&gt;</linkto>.
// For each column you can construct one or
// more of these objects. Their put(...) functions
// let you write a value at a time or the entire column in one go.
// For arrays you can "put" subsections of the arrays.
//
// As an alternative for scalars of a standard data type (i.e. Bool,
// uChar, Int, Short, uShort, uInt, float, double, Complex, DComplex
// and String) you could use the functions
// <linkto class="TableColumn">TableColumn::putScalar(...)</linkto>.
// These functions offer an extra: automatic data type promotion; so that
// you can, for example, put a float value in a double column.
//
// A typical program could look like:
// <srcblock>
// #include <aips/Tables/TableDesc.h>
// #include <aips/Tables/SetupNewTab.h>
// #include <aips/Tables/Table.h>
// #include <aips/Tables/ScaColDesc.h>
// #include <aips/Tables/ArrColDesc.h>
// #include <aips/Tables/ScalarColumn.h>
// #include <aips/Tables/ArrayColumn.h>
// #include <aips/Arrays/Vector.h>
// #include <aips/Lattices/Slicer.h>
// #include <aips/Arrays/ArrayMath.h>
// #include <iostream.h>
// 
// main()
// {
//     // First build the table description.
//     TableDesc td("tTableDesc", "1", TableDesc::Scratch);
//     td.comment() = "A test of class SetupNewTable";
//     td.addColumn (ScalarColumnDesc<Int> ("ac"));
//     td.addColumn (ArrayColumnDesc<float> ("arr2",0));
// 
//     // Setup a new table from the description,
//     // and create the (still empty) table.
//     // Note that since we do not explicitly bind columns to
//     // data managers, all columns will be bound to the default
//     // AipsIO storage manager.
//     SetupNewTable newtab("newtab.data", td, Table::New);
//     Table tab(newtab);
//
//     // Construct the various column objects.
//     // Their data type has to match the data type in the description.
//     ScalarColumn<Int> ac (tab, "ac");
//     ArrayColumn<float> arr2 (tab, "arr2");
//     Vector<float> vec2(100);
//
//     // Write the data into the columns.
//     // In each cell arr2 will be a vector of length 100.
//     // Since its shape is not set explicitly, it is done implicitly.
//     for (uInt i=0; i<10; i++) {
//         tab.addRow();               // First add a row.
//         ac.put (i, i+10);           // value is i+10 in row i
//         indgen (vec2, float(i+20)); // vec2 gets i+20, i+21, ..., i+119
//         arr2.put (i, vec2); 
//     }
//
//     // Finally, show the entire column ac,
//     // and show the 10th element of arr2.
//     cout << ac.getColumn();
//     cout << arr2.getColumn (Slicer(Slice(10)));
//
//     // The Table destructor writes the table.
// }
// </srcblock>
//
// In this example we added rows in the for loop, but we could also have
// created 10 rows straightaway by constructing the Table object as:
// <srcblock>
//     Table tab(newtab, 10);
// </srcblock>
// in which case we would not include
// <srcblock>
//     tab.addRow()
// </srcblock>
//
// The classes 
// <linkto class="TableColumn:description">TableColumn</linkto>,
// <linkto class="ScalarColumn:description">ScalarColumn&lt;T&gt;</linkto>, and
// <linkto class="ArrayColumn:description">ArrayColumn&lt;T&gt;</linkto>
// contain several functions to put values into a single cell or into the
// whole column. This may look confusing, but is actually quite simple.
// The functions can be divided in two groups:
// <ol>
//  <li>
//   Put the given value into the column cell(s).
//   <ul>
//    <li>
//     The simplest put functions,
//     <linkto class="ScalarColumn">ScalarColumn::put(...)</linkto> and
//     <linkto class="ArrayColumn">ArrayColumn::put(...)</linkto>,
//     put a value into the given column cell. For convenience, there is an
//     <linkto class="ArrayColumn">ArrayColumn::putSlice(...)</linkto>
//     to put only a part of the array.
//    <li>
//     <linkto class="ScalarColumn">ScalarColumn::fillColumn(...)</linkto> and
//     <linkto class="ArrayColumn">ArrayColumn::fillColumn(...)</linkto>
//     fill an entire column by putting the given value into all the cells
//     of the column.
//    <li>
//     The simplest putColumn functions,
//     <linkto class="ScalarColumn">ScalarColumn::putColumn(...)</linkto> and
//     <linkto class="ArrayColumn">ArrayColumn::putColumn(...)</linkto>,
//     put an array of values into the column. There is a special
//     <linkto class="ArrayColumn">ArrayColumn::putColumn(...)</linkto>
//     version which puts only a part of the arrays.
//   </ul>
//
//  <li>
//   Copy values from another column to this column.<BR>
//   These functions have the advantage that the
//   data type of the input and/or output column can be unknown.
//   The generic (RO)TableColumn objects can be used for this purpose.
//   The put(Column) function checks the data types and, if possible,
//   converts them. If the conversion is not possible, it throws an
//   exception.
//   <ul>
//    <li>
//     The put functions copy the value in a cell of the input column
//     to a cell in the output column. The row numbers of the cells
//     in the columns can be different.
//    <li>
//     The putColumn functions copy the entire contents of the input column
//     to the output column. The lengths of the columns must be equal.
//   </ul>
//   Each class has its own set of these functions.
//   <ul>
//    <li>
//     <linkto class="TableColumn">TableColumn::put(...)</linkto> and
//     <linkto class="TableColumn">TableColumn::putColumn(...)</linkto> and
//     are the most generic. They can be
//     used when the data types of both input and output column are unknown.
//     Note that these functions are virtual.
//    <li>
//     <linkto class="ScalarColumn">ScalarColumn::put(...)</linkto>,
//     <linkto class="ArrayColumn">ArrayColumn::put(...)</linkto>,
//     <linkto class="ScalarColumn">ScalarColumn::putColumn(...)</linkto>, and
//     <linkto class="ArrayColumn">ArrayColumn::putColumn(...)</linkto>
//     are less generic and therefore potentially more efficient.
//     The most efficient variants are the ones taking a
//     ROScalar/ArrayColumn&lt;T&gt;, because they require no data type
//     conversion.
//   </ul>
// </ol>

// <A NAME="Tables:row-access">
// <h3>Accessing rows in a Table</h3></A>
//
// Apart from accessing a table column-wise as described in the
// previous two sections, it is also possible to access a table row-wise.
// The <linkto class=TableRow>TableRow</linkto> class makes it possible
// to access multiple fields in a table row as a whole. Note that like the
// XXColumn classes described above, there is also an ROTableRow class
// for access to readonly tables.
// <p>
// On construction of a TableRow object it has to be specified which
// fields (i.e. columns) are part of the row. For these fields a
// fixed structured <linkto class=TableRecord>TableRecord</linkto>
// object is constructed as part of the TableRow object. The TableRow::get
// function will fill this record with the table data for the given row.
// The user has access to the record and can use
// <linkto class=RecordFieldPtr>RecordFieldPtr</linkto> objects for
// speedier access to the record.
// <p>
// The class could be used as shown in the following example.
// <srcblock>
// // Open the table as readonly and define a row object to contain
// // the given columns.
// // Note that the function stringToVector is a very convenient
// // way to construct a Vector<String>.
// // Show the description of the fields in the row.
// Table table("Some.table");
// ROTableRow row (table, stringToVector("col1,col2,col3"));
// cout << row.record().description();
// // Since the structure of the record is known, the RecordFieldPtr
// // objects could be used to allow for easy and fast access to
// // the record which is refilled for each get.
// RORecordFieldPtr<String> col1(row.record(), "col1");
// RORecordFieldPtr<double> col2(row.record(), "col2");
// RORecordFieldPtr<Array<Int> > col3(row.record(), "col3");
// for (uInt i=0; i<table.nrow(); i++) {
//     row.get (i);
//     someString = *col1;
//     somedouble = *col2;
//     someArrayInt = *col3;
// }
// </srcblock>
// The description of TableRow contains some more extensive examples.

// <A NAME="Tables:select and sort">
// <h3>Table Selection and Sorting</h3></A>
//
// The result of a select and sort of a table is another table,
// which references the original table. This means that an update
// of a sorted or selected table results in the update of the original
// table. The result is, however, a table in itself, so all table
// functions (including select and sort) can be used with it.
// <p>
// Rows or columns can be selected from a table. Columns can be selected
// by the
// <linkto class="Table">Table::project(...)</linkto>
// function, while rows can be selected by the various
// <linkto class="Table">Table operator()</linkto> functions.
// Usually a row is selected by giving a select expression with
// <linkto class="TableExprNode:description">TableExprNode</linkto>
// objects. These objects represent the various nodes
// in an expression, e.g. a constant, a column, or a subexpression.
// The Table function
// <linkto class="Table">Table::col(...)</linkto>
// creates a TableExprNode object for a column. The function
// <linkto class="Table">Table::key(...)</linkto>
// does the same for a keyword by reading
// the keyword value and storing it as a constant in an expression node.
// All column nodes in an expression must belong to the same table,
// otherwise an exception is thrown.
// In the following example we select all rows with RA>10:
// <srcblock>
//    #include <aips/Tables/ExprNode.h>
//    Table table ("Table.name");
//    Table result = table (table.col("RA") > 10);
// </srcblock>
// while in the next one we select rows with RA and DEC in the given 
// intervals:
// <srcblock>
//    Table result = table (table.col("RA") > 10
//                       && table.col("RA") < 14
//                       && table.col("DEC") >= -10
//                       && table.col("DEC") <= 10);
// </srcblock>
// The following operators can be used to form arbitrarily
// complex expressions:
// <ul>
//  <li> Relational operators ==, !=, >, >=, < and <=.
//  <li> Logical operators &&, || and !.
//  <li> Arithmetic operators +, -, *, /, %, ^, and unary + and -.
//  <li> Operator() to take a subsection of an array.
// </ul>
// Many functions (like sin, max, conj) can be used in an expression.
// Class <linkto class=TableExprNode>TableExprNode</linkto> shows
// the available functions.
// E.g.
// <srcblock>
//    Table result = table (sin (table.col("RA")) > 0.5);
// </srcblock>
// Function <src>in</src> can be used to select from a set of values.
// A value set can be constructed using class
// <linkto class=TableExprNodeSet>TableExprNodeSet</linkto>.
// <srcblock>
//    TableExprNodeSet set;
//    set.add (TableExprNodeSetElem ("abc"));
//    set.add (TableExprNodeSetElem ("defg"));
//    set.add (TableExprNodeSetElem ("h"));
//    Table result = table (table.col("NAME).in (set));
// </srcblock>
// select rows with a NAME equal to <src>abc</src,
// <src>defg</src>, or <src>h</src>.
//
// <p>
// You can sort a table on one or more columns containing scalars.
// In this example we simply sort on column RA (default is ascending):
// <srcblock>
//    Table table ("Table.name");
//    Table result = table.sort ("RA");
// </srcblock>
// Multiple
// <linkto class="Table">Table::sort(...)</linkto>
// functions exist which allow for more flexible control over the sort order.
// In the next example we sort first on RA in descending order
// and then on DEC in ascending order:
// <srcblock>
//    Table table ("Table.name");
//    Block<String> sortKeys(2);
//    Block<int>    sortOrders(2);
//    sortKeys(0)   = "RA";
//    sortOrders(0) = Sort::Descending;
//    sortKeys(1)   = "DEC";
//    sortOrders(1) = Sort::Ascending;
//    Table result = table.sort (sortKeys, sortOrders);
// </srcblock>
//
// Tables stemming from the same root, can be combined in several
// ways with the help of the various logical
// <linkto class="Table">Table operators</linkto> (operator|, etc.).

// <h4>Table Query Language</h4>
// The selection and sorting mechanism described above can only be used
// in a hard-coded way in a C++ program.
// There is, however, another way. Strings containing selection and
// sorting commands can be used.
// The syntax of these commands is based on SQL and is described in the
// <a href="../../notes/199/199.html">Table Query Language</a> (TaQL).
// <br>Such a command can be executed with the static function
// <src>TableParse::tableCommand</src> defined in class
// <linkto class=TableParse>TableParse</linkto>.
// </note>

// <A NAME="Tables:iterate">
// <h3>Table Iterators</h3></A>
//
// You can iterate through a table in an arbitrary order by getting
// a subset of the table consisting of the rows in which the iteration
// columns have the same value.
// An iterator object is created by constructing a
// <linkto class="TableIterator:description">TableIterator</linkto>
// object with the appropriate column names.
//
// In the next example we define an iteration on the columns Time and
// Baseline. Each iteration step returns a table subset in which Time and
// Baseline have the same value.
//
// <srcblock>
//    // Iterate over Time and Baseline (by default in ascending order).
//    // Time is the main iteration order, thus the first column specified.
//    Table t;
//    Table tab ("UV_Table.data");
//    Block<String> iv0(2);
//    iv0[0] = "Time";
//    iv0[1] = "Baseline";
//    //
//    // Create the iterator. This will prepare the first subtable.
//    TableIterator iter(tab, iv0);
//    Int nr = 0;
//    while (!iter.pastEnd()) {
//        // Get the first subtable.
//        // This will contain rows with equal Time and Baseline.
//        t = iter.table();
//        cout << t.nrow() << " ";
//        nr++;
//        // Prepare the next subtable with the next Time,Baseline value.
//        iter.next();
//    }
//    cout << endl << nr << " iteration steps" << endl;
// </srcblock>
//
// You can define more than one iterator on the same table; they operate
// independently.
//
// Note that the result of each iteration step is a table in itself which
// references the original table, just as in the case of a sort or select.
// This means that the resulting table can be used again in a sort, select,
// iteration, etc..

// <A NAME="Tables:vectors">
// <h3>Table Vectors</h3></A>
//
// A table vector makes it possible to treat a column in a table
// as a vector. Almost all operators and functions defined for normal
// vectors, are also defined for table vectors. So it is, for instance,
// possible to add a constant to a table vector. This has the effect
// that the underlying column gets changed.
//
// You can use the templated classes
// <linkto class="ROTableVector:description">ROTableVector</linkto> and
// <linkto class="TableVector:description">TableVector</linkto> and
// to define a table vector (readonly and read/write, respectively) for
// a scalar column. Columns containing arrays or tables are not supported.
// The data type of the (RO)TableVector object must match the
// data type of the column.
// A table vector can also hold a normal vector so that (temporary)
// results of table vector operations can be handled.
//
// In the following example we double the data in column COL1 and
// store the result in a temporary table vector.
// <srcblock>
//    // Create a table vector for column COL1.
//    // It has to be a ROTableVector, because the table is opened
//    // as readonly.
//    Table tab ("Table.data");
//    ROTableVector<Int> tabvec(tab, "COL1");
//    // Multiply it by a constant.
//    // The result has to be stored in a TableVector,
//    // since a ROTableVector cannot be written to.
//    TableVector<Int> temp = 2 * tabvec;
// </srcblock>
//
// In the next example we double the data in COL1 and put the result back
// in the column.
// <srcblock>
//    // Create a table vector for column COL1.
//    // It has to be a TableVector to be able to change the column.
//    Table tab ("Table.data", Table::Update);
//    TableVector<Int> tabvec(tab, "COL1");
//    // Multiply it by a constant.
//    tabvec *= 2;;
// </srcblock>

// <A NAME="Tables:keywords">
// <h3>Table Keywords</h3></A>
//
// Any number of keyword/value pairs may be attached to the table as a whole,
// or to any individual column. They may be freely added, retrieved,
// re-assigned, or deleted. They are, in essence, a self-resizing list of
// values (any of the primitive types) indexed by Strings (the keyword).
//
// A table keyword/value pair might be
// <srcblock>
//      Observer = Grote Reber
//      Date = 10 october 1942
// </srcblock>
// Column keyword/value pairs might be
// <srcblock>
//      Units = mJy
//      Reference Pixel = 320
// </srcblock>
// The class 
// <linkto class="TableRecord:description">TableRecord</linkto>
// represents the keywords in a table.
// It is (indirectly) derived from the standard record classes in the class
// <linkto class="Record:description">Record</linkto>

// <A NAME="Tables:Table Description">
// <h3>Table Description</h3></A>
//
// A table contains a description of itself, which defines the layout of the
// columns and the keyword sets for the table and for the individual columns.
// It may also define initial keyword sets and default values for the columns.
// Such a default value is automatically stored in a cell in the table column,
// whenever a row is added to the table.
//
// The creation of the table descriptor is the first step in the creation of
// a new table. The description is part of the table itself, but may also
// exist in a separate file. This is useful when you need to create a number
// of tables with the same structure; in other circumstances it probably
// should be avoided.
//
// The public classes to set up a table description are:
// <ul>
//  <li> <linkto class="TableDesc:description">TableDesc</linkto>
//       -- holds the table description.
//  <li> <linkto class="ColumnDesc:description">ColumnDesc</linkto>
//       -- holds a generic column description.
//  <li> <linkto class="ScalarColumnDesc:description">ScalarColumnDesc&lt;T&gt;
//       </linkto>
//       -- defines a column containing a scalar value.
//  <li> <linkto class="ArrayColumnDesc:description">ArrayColumnDesc&lt;T&gt;
//       </linkto>
//       -- defines a column containing an (in)direct array.
//  <li> <linkto class="SubTableDesc:description">SubTableDesc</linkto>
//       -- defines a column containing a table.
// </ul>
//
// Here follows a typical example of the construction of a table
// description. For more specialized things -- like the definition of a
// default data manager -- we refer to the descriptions of the above
// mentioned classes.
//
// <srcblock>
// #include <aips/Tables/TableDesc.h>
// #include <aips/Tables/ScaColDesc.h>
// #include <aips/Tables/ArrColDesc.h>
// #include <aips/Tables/SubTabDesc.h>
// #include <aips/Tables/TableRecord.h>
// #include <aips/Lattices/IPosition.h>
// #include <aips/Arrays/Vector.h>
//
// main()
// {
//     // First build the new description of a subtable.
//     // Define keyword subkey (integer) having value 10.
//     // Define columns ra and dec (double).
//     TableDesc subTableDesc("tTableDesc_sub", "1", TableDesc::New);
//     subTableDesc.rwKeywordSet().define ("subkey", Int(10));
//     subTableDesc.addColumn (ScalarColumnDesc<double> ("ra"));
//     subTableDesc.addColumn (ScalarColumnDesc<double> ("dec"));
//
//     // Now create a new table description
//     // Define a comment for the table description.
//     // Define some keywords.
//     ColumnDesc colDesc1, colDesc2;
//     TableDesc td("tTableDesc", "1", TableDesc::New);
//     td.comment() = "A test of class TableDesc";
//     td.rwKeywordSet().define ("ra" float(3.14));
//     td.rwKeywordSet().define ("equinox", double(1950));
//     td.rwKeywordSet().define ("aa", Int(1));
//
//     // Define an integer column ab.
//     td.addColumn (ScalarColumnDesc<Int> ("ab", "Comment for column ab"));
//
//     // Add a scalar integer column ac, define keywords for it
//     // and define a default value 0.
//     // Overwrite the value of keyword unit.
//     ScalarColumnDesc<Int> acColumn("ac");
//     acColumn.rwKeywordSet().define ("scale" Complex(0,0));;
//     acColumn.rwKeywordSet().define ("unit", "");
//     acColumn.setDefault (0);
//     td.addColumn (acColumn);
//     td.rwColumnDesc("ac").rwKeywordSet().define ("unit", "DEG");
//
//     // Add a scalar string column ad and define its comment string.
//     td.addColumn (ScalarColumnDesc<String> ("ad","comment for ad"));
//
//     // Now define array columns.
//     // This one is indirect and has no dimensionality mentioned yet.
//     td.addColumn (ArrayColumnDesc<Complex> ("Arr1","comment for Arr1"));
//     // This one is indirect and has 3-dim arrays.
//     td.addColumn (ArrayColumnDesc<Int> ("A2r1","comment for Arr1",3));
//     // This one is direct and has 2-dim arrays with axes length 4 and 7.
//     td.addColumn (ArrayColumnDesc<uInt> ("Arr3","comment for Arr1",
//                                          IPosition(2,4,7),
//                                          ColumnDesc::Direct));
//
//     // Add columns containing tables.
//     // This is done in 3 slightly different ways, which all have
//     // their own (dis)advantages.
//     // See SubTabDesc.h for a description of the SubTableDesc constructors.
//     td.addColumn (SubTableDesc("sub1", "subtable by name","tTableDesc_sub"));
//     td.addColumn (SubTableDesc("sub2", "subtable copy",    subTableDesc));
//     td.addColumn (SubTableDesc("sub3", "subtable pointer", &subTableDesc));
// }
// </srcblock>

// <A NAME="Tables:Data Managers">
// <h3>Data Managers</h3></A>
//
// Data managers take care of the actual access to the data in a column.
// There are two kinds of data managers:
// <ol>
//  <li> <A HREF="#Tables:storage managers">Storage managers</A> --
//   which store the data as such. They can only handle the standard
//   data type (Bool,...,String) as discussed in the section about the
//   <A HREF="#Tables:properties">table properties</A>).
//  <li> <A HREF="#Tables:virtual column engines">Virtual column engines</A>
//   -- which manipulate the data.
//   An engine could be a simple thing like scaling the data (as done
//   in classic AIPS to reduce data storage), but it could also be an
//   elaborate thing like applying corrections on-the-fly.
//   <br>An engine must be used to store data objects with a non-standard type.
//   It has to break down the object into items with standard data types
//   which can be stored with a storage manager.
// </ol>
// In general the user of a table does not need to be aware which
// data managers are being used underneath. Only when the table is created
// data managers have to be bound to the columns. Thereafter it is
// completely transparent.

// <A NAME="Tables:storage managers">
// <h3>Storage Managers</h3></A>
//
// Several storage managers are currently supported:
// <ol>
//  <li>
//   <linkto class="StManAipsIO:description">StManAipsIO</linkto>
//   uses AipsIO to store the data in the columns.
//   It supports all table functionality, but its I/O is probably not
//   as efficient as other storage managers. It also requires that
//   a large part of the table fits in memory.
//
//  <li>
//   <linkto class="IncrementalStMan:description">IncrementalStMan</linkto>
//   uses a storage mechanism resembling "incremental backups". A value
//   is only stored when it is different from the previous row. It is
//   very well suited for slowly varying data.
//   <br>The class <linkto class="ROIncrementalStManAccessor:description">
//   ROIncrementalStManAccessor</linkto> can be used to tune the
//   behaviour of the <src>IncrementalStMan</src>. It contains functions
//   to deal with the cache size and to show the behaviour of the cache.
//
//  <li>
//   The <a href="#Tables:TiledStMan">Tiled Storage Managers</a>
//   store the data as a tiled hypercube allowing for more or less equally
//   efficient data access along all main axes. It can be used for
//   UV-data as well as for image data.
//
//  <li>
//   The <src>StManMirAIO</src> storage manager has been deprecated.
//   Instead <src>IncrementalStMan</src> (mentioned above) should be used.
// </ol>
//
// The storage manager framework makes it possible to support arbitrary files
// as tables. This has been used in a case where a file is filled
// by the data acquisition system of a telescope. The file is simultaneously
// used as a table using a dedicated storage manager. The table
// system and storage manager provide a sync function to synchronize
// the processes, i.e. to make the table system aware of changes
// in the file size (thus in the table size) by the filling process.
//
// <note role=tip>
// Not all data managers support all the table functionality. So, the choice
// of a data manager can greatly influence the type of operations you can do
// on the table as a whole.
// For example, if a column uses the tiled storage manager,
// it is not possible to delete rows from the table, because that storage
// manager will not support deletion of rows.
// </note>

// <A NAME="Tables:TiledStMan">
// <h3>Tiled Storage Manager</h3>
// The Tiled Storage Managers allow one to store the data of
// one or more columns in a tiled way. Tiling means
// that the data are stored without a preferred order to make access
// along the different main axes equally efficient. This is done by
// storing the data in so-called tiles (i.e. equally shaped subsets of an
// array) to increase data locality. The user can define the tile shape
// to optimize for the most frequently used access.
// <p>
// The Tiled Storage Manager has the following properties:
// <ul>
//  <li> There can be more than one Tiled Storage Manager in
//       a table; each with its own (unique) name.
//  <li> Each Tiled Storage Manager can store an
//       N-dimensional so-called hypercolumn (defined using
//       <linkto file="TableDesc.h#defineHypercolumn">
//       TableDesc::defineHypercolumn</linkto>).
//       <br>A hypercolumn consists of up to three types
//       of columns:
//       <dl>
//        <dt> Data columns
//        <dd> contain the data to be stored in a tiled way. This will
//             be done in tiled hypercubes.
//             There must be at least one data column.
//             <br> For example: a table contains UV-data with
//                  data columns "Visibility" and "Weight".
//        <dt> Coordinate columns
//        <dd> define the world coordinates of the pixels in the data columns.
//             Coordinate columns are optional, but when given there must
//             be N coordinate columns for an N-dimensional hypercolumn.
//             <br>
//             For example: the data in the example above is 4-dimensional
//             and has coordinate columns "Time", "Baseline", "Frequency",
//             and "Polarization".
//        <dt> Id columns
//        <dd> are needed when the data columns use more than one hypercube.
//             Different rows in the data columns can be stored in different
//             hypercube. The values in the id column(s) uniquely identify
//             the hypercube a row is stored in.
//             <br>
//             For example: the line and continuum data in a MeasurementSet
//             table need to be stored in 2 different hypercubes (because
//             their shapes are different (see below)). A column containing
//             the type (line or continuum) has to be used as an id column.
//       </dl>
//  <li> When multiple data columns are used, the shape of their data
//       must be conforming in each individual row.
//       When data in different rows have different shapes, they must be
//       stored in different hypercubes, because a hypercube can only hold
//       data with conforming shapes.
//       <br>
//       Thus in the example above, rows with line data will have conforming
//       shapes and can be stored in one hypercube. The continuum data
//       will have another shape and can be stored in another hypercube.
//       <br>
//       The storage manager keeps track of the mapping of rows to/from
//       hypercubes.
//  <li> Each hypercube can be tiled in its own way. It is not required
//       that an integer number of tiles fits in the hypercube. The last
//       tiles will be padded as needed.
//  <li> When a hypercolumn consists of multiple hypercubes, one or more id
//       columns have to be defined to be able to differentiate between them.
//       All rows in the same hypercube have to have the same id values.
//  <li> The last axis of a hypercube can be extensible. This means that
//       the size of that axis does not need to be defined when the
//       hypercube is defined in the storage manager. Instead, the hypercube
//       can be extended when another chunk of data has to be stored.
//       This can be very useful in, for example, a (quasi-)realtime
//       environment where the size of the time axis is not known.
//  <li> When coordinate columns are defined, they describe the coordinates
//       of the axes of the hypercubes. Each hypercube has its own set of
//       coordinates.
//  <li> Data and id columns have to be stored with the Tiled
//       Storage Manager. However, coordinate columns do not need to be
//       stored with the Tiled Storage Manager.
//       Especially in the case where the coordinates for a hypercube axis
//       are varying (i.e. dependent on other axes), another storage manager
//       has to be used (because the Tiled Storage Manager can only
//       hold constant coordinates).
// </ul>
// The Tiled Storage Managers use internal caches to minimize IO. It is
// possible to define a maximum cache size. The description of class
// <linkto class=ROTiledStManAccessor>ROTiledStManAccessor</linkto>
// contains a discussion about the effect of defining a maximum cache size.
// <p>
// The following Tiled Storage Managers are available:
// <dl>
//  <dt> <linkto class=TiledCellStMan:description>TiledCellStMan</linkto>
//  <dd> creates (automatically) a new hypercube for each row.
//       Thus each row of the hypercolumn is stored in a separate hypercube.
//       Note that the row number serves as the id value. So an id column
//       is not needed, although there are multiple hypercubes.
//       <br>
//       This storage manager is meant for tables where the data arrays
//       in the different rows are not accessed together. One can think
//       of a column containing images. Each row contains an image and
//       only one image is shown at a time.
//  <dt> <linkto class=TiledColumnStMan:description>TiledColumnStMan</linkto>
//  <dd> creates one hypercube for the entire hypercolumn. Thus all cells
//       in the hypercube have to have the same shape and therefore this
//       storage manager is only possible when all columns in the hypercolumn
//       have the attribute FixedShape.
//       <br>
//       This storage manager could be used for a table with a column
//       containing images for the Stokes parameters I, Q, U, and V.
//       By storing them in one hypercube, it is possible to retrieve
//       the 4 Stokes values for a subset of the image or for an individual
//       pixel in a very efficient way.
//  <dt> <linkto class=TiledDataStMan:description>TiledDataStMan</linkto>
//  <dd> allows one to control the creation and extension of hypercubes.
//       This is done by means of the class
//       <linkto class=TiledDataStManAccessor:description>
//       TiledDataStManAccessor</linkto>.
//       This makes it possible to store, say, row 0-9 in hypercube A,
//       row 10-34 in hypercube B, row 35-54 in hypercube A again, etc..
//       <br>
//       This storage manager could be used to store UV-data with a mix
//       of continuum and line data.
//  <dt> <linkto class=TiledShapeStMan:description>TiledShapeStMan</linkto>
//  <dd> can be seen as a specialization of <src>TiledDataStMan</src>
//       by using the array shape as the id value.
//       Similarly to <src>TiledDataStMan</src> it can maintain multiple
//       hypercubes and store multiple rows in a hypercube, but is is
//       easier to use, because the special <src>addHypercube</src> and
//       <src>extendHypercube</src> functions are not needed.
//       An hypercube is automatically added when a new array shape is
//       encountered.
//       <br>
//       This storage manager could be used for a table with a column
//       containing line and continuum data, which will result
//       in 2 hypercubes.
// </dl>
//
// For example:<br>
// UV-data and weights have to be stored in a table.
// The data have the coordinates Pol, Freq, Baseline and Time.
// There is continuum and line data, which have to be stored in 2 separate
// hypercubes. This could lead to the following scenario when creating/filling
// the table:
// <ul>
//  <li> Define a hypercolumn with data columns Data and Weight,
//       coordinate columns Pol, Freq, Baseline and Time and id column Id.
//       The id column is needed to differentiate between continuum and line.
//  <li> Use the storage manager TiledDataStMan to be able to drive which
//       hypercube is used.
//  <li> Add the two hypercubes (using TiledDataStManAccessor)
//       with their correct id values and coordinate values.
//       The last axis (i.e. time) is extensible.
//  <li> Read the data from a source (which will be in time-order).
//       Add rows to the table, extend the appropriate hypercube and put
//       the data into the row(s).
// </ul>
// An alternative scenario could be that the data in the source is not
// in time order, but that the size of the data is known. In that case
// the hypercubes can be defined with their correct shape and putColumn
// (with a Slicer) can be used to put the data (and reorder them implicitly).
// <br>
// Another alternative is to use TiledShapeStMan, so the hypercubes are
// added or extended automatically.

// <A NAME="Tables:virtual column engines">
// <h3>Virtual Column Engines</h3></A>
//
// Virtual column engines are used to implement the virtual (i.e.
// calculated-on-the-fly) columns. The Table system provides
// an abstract base class (or "interface class")
// <linkto class="VirtualColumnEngine:description">VirtualColumnEngine</linkto>
// that specifies the protocol for these engines.
// The programmer must derive a concrete class to implement
// the application-specific virtual column.
// <p>
// For example: the programmer
// needs a column in a table which is the difference between two other
// columns.  (Perhaps these two other columns are updated periodically
// during the execution of a program.)  A good way to handle this would
// be to have a virtual column in the table, and write a virtual column
// engine which knows how to calculate the difference between corresponding
// cells of the two other columns. So the result is that accessing a
// particular cell of the virtual column invokes the virtual column engine,
// which then gets the values from the other two columns, and returns their
// difference.
// <p>
// Several virtual column engines exist:
// <ol>
//  <li> The doubly templated class
//   <linkto class="ScaledArrayEngine:description">ScaledArrayEngine</linkto>
//   scales the data in an array from, for example,
//   float to short before putting it.
//  <li> The doubly templated class
//   <linkto class="RetypedArrayEngine:description">RetypedArrayEngine</linkto>
//   converts the data from one data type to another with the possibility
//   to reduce the number of dimensions. For example, it can be used to
//   store an 2-d array of StokesVector objects as a 3-d array of floats
//   by treating the 4 data elements as an extra array axis. When the
//   StokesVector class is simple, it can be done very efficiently.
//  <li> The class
//   <linkto class="ForwardColumnEngine:description">
//   ForwardColumnEngine</linkto>
//   forwards the gets and puts on a row in a column to the same row
//   in a column with the same name in another table. This provides
//   a virtual copy of the referenced column.
//  <li> The class
//   <linkto class="ForwardColumnIndexedRowEngine:description">
//   ForwardColumnIndexedRowEngine</linkto>
//   is similar to <src>ForwardColumnEngine.</src>.
//   However, instead of forwarding it to the same row it uses a
//   a column to map its row number to a row number in the referenced
//   table. In this way multiple rows can share the same data.
//   This data manager only allows for get operations.
//  <li> The calibration module has implemented a virtual column engine
//   to do on-the-fly calibration in a transparent way.
// </ol>
// To handle arbitrary data types the templated abstract base class
// <linkto class="VSCEngine:description">VSCEngine</linkto>
// has been written. An example of how to use this class can be
// found in the demo program <src>dVSCEngine.cc</src>.

// <A NAME="Tables:LockSync">
// <h3>Table locking and synchronization</h3></A>
//
// Multiple concurrent readers and writers (also via NFS) of a
// table are supported by means of a locking/synchronization mechanism.
// This mechanism is not very sophisticated in the sense that it is
// very coarse grained. When locking, the entire table gets locked.
// A special lock file is used to lock the table. This lock file also
// contains some synchronization data.
// <p>
// Three ways of locking are supported (see class
// <linkto class=TableLock>TableLock</linkto>):
// <dl>
//  <dt> TableLock::PermanentLocking(Wait)
//  <dd> locks the table permanently (from open till close). This means
//       that one writer OR multiple readers are possible.
//  <dt> TableLock::AutoLocking
//  <dd> does the locking automatically. This is the default mode.
//       This mode makes it possible that a table is shared amongst
//       processes without the user needing to write any special code.
//       It also means that a lock is only released when needed.
//  <dt> TableLock::UserLocking
//  <dd> requires that the programmer explicitly acquires and releases
//       a lock on the table. This makes some kind of transaction
//       processing possible. E.g. set a write lock, add a row,
//       write all data into the row and release the lock.
//       The Table functions <src>lock</src> and <src>unlock</src>
//       have to be used to acquire and release a (read or write) lock.
// </dl>
// Synchronization of the processes accessing the same table is done
// by means of the lock file. When a lock is released, the storage
// managers flush their data into the table files. Some synchronization data
// is written into the lock file telling the new number of table rows
// and telling which storage managers have written data.
// This information is read when another process acquires the lock
// and is used to determine which storage managers have to refresh
// their internal caches.
// <p>
// The function <src>Table::hasDataChanged</src> can be used to check
// if a table is (being) changed by another process. In this way
// a program can react on it. E.g. the table browser can refresh its
// screen when the underlying table is changed.
// <p>
// In general the default locking option will do.
// From the above it should be clear that heavy concurrent access
// results in a lot of flushing, thus will have a negative impact on
// performance. When uninterrupted access to a table is needed,
// the <src>PermanentLocking</src> option should be used.
// When transaction-like processing is done (e.g. updating a table
// containing an observation catalogue), the <src>UserLocking</src>
// option is probably best.
// <p>
// Creation or deletion of a table is not possible when that table
// is still open in another process. The function
// <src>Table::isMultiUsed()</src> can be used to check if a table
// is open in other processes.
// <br>
// The function <src>deleteTable</src> should be used to delete
// a table. Before deleting the table it ensures that it is writable
// and that it is not open in the current or another process
// <p>
// The following example wants to read the table uninterrupted, thus it uses
// the <src>PermanentLocking</src> option. It also wants to wait
// until the lock is actually acquired.
// Note that the destructor closes the table and releases the lock.
// <srcblock>
// // Open the table (readonly).
// // Acquire a permanent (read) lock.
// // It waits until the lock is acquired.
// Table tab ("some.name",
//            TableLock(TableLock::PermanentLockingWait));
// </srcblock>
//
// The following example uses the automatic locking..
// It tells the system to check about every 20 seconds if another
// process wants access to the table.
// <srcblock>
// // Open the table (readonly).
// Table tab ("some.name",
//            TableLock(TableLock::AutoLocking, 20));
// </srcblock>
//
// The following example gets data (say from a GUI) and writes it
// as a row into the table. The lock the table as little as possible
// the lock is acquired just before writing and released immediately
// thereafter.
// <srcblock>
// // Open the table (writable).
// Table tab ("some.name",
//            TableLock(TableLock::UserLocking),
//            Table::Update);
// while (True) {
//     get input data
//     tab.lock();     // Acquire a write lock and wait for it.
//     tab.addRow();
//     write data into the row
//     tab.unlock();   // Release the lock.
// }
// </srcblock>
//
// The following example deletes a table when it is not used in
// another process.
// <srcblock>
// Table tab ("some.name");
// if (! tab.isMultiUsed()) {
//     tab.markForDelete();
// }
// </srcblock>

// </synopsis>

// </module>


#endif
