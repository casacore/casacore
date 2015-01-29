//# Tables.h: The Tables module - Casacore data storage
//# Copyright (C) 1994-2010
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

#ifndef TABLES_TABLES_H
#define TABLES_TABLES_H

//# Includes
//#   table description
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScaRecordColDesc.h>

//#   table access
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableLock.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/TableRow.h>
#include <casacore/tables/Tables/TableCopy.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Slice.h>

//#   keywords
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Containers/RecordField.h>

//#   table lookup
#include <casacore/tables/Tables/ColumnsIndex.h>
#include <casacore/tables/Tables/ColumnsIndexArray.h>

//#   table vectors
#include <casacore/tables/Tables/TableVector.h>
#include <casacore/tables/Tables/TabVecMath.h>
#include <casacore/tables/Tables/TabVecLogic.h>

//#   data managers
#include <casacore/tables/DataMan.h>

//#   table expressions (for selection of rows)
#include <casacore/tables/TaQL.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>

// <summary>
// Tables are the data storage mechanism for Casacore
// </summary>

// <use visibility=export>

// <reviewed reviewer="jhorstko" date="1994/08/30" tests="" demos="">
// </reviewed>

// <prerequisite>
//    <li> <linkto class="Record:description">Record</linkto> class
// </prerequisite>

// <etymology>
// "Table" is a formal term from relational database theory: 
//   <em> "The organizing principle in a relational database is the TABLE,
//    a rectangular, row/column arrangement of data values."</em>
// Casacore tables are extensions to traditional tables, but are similar
// enough that we use the same name.  There is also a strong resemblance
// between the uses of Casacore tables, and FITS binary tables, which
// provides another reason to use "Tables" to describe the Casacore data
// storage mechanism.
// </etymology>

// <synopsis> 
// Tables are the fundamental storage mechanism for Casacore. This document
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
//       (see also <A HREF="../notes/199.html">Table Query Language</A>),
//  <LI> <A HREF="#Tables:concatenation">concatenating similar tables</A>
//  <LI> <A HREF="#Tables:iterate">iterating</A> through a table,
//  <LI> <A HREF="#Tables:LockSync">locking/synchronization</A>
//       for concurrent access,
//  <LI> <A HREF="#Tables:KeyLookup">indexing</A> a table for faster lookup,
//  <LI> <A HREF="#Tables:vectors">vector operations</A> on a column.
//  <LI> <A HREF="#Tables:performance">performance and robustness</A>
//       considerations with some information on
//       <A HREF="#Tables:iotracing">IO tracing</A>.
// </UL>
// A few <A HREF="Tables:applications">applications</A> exist to inspect
// and manipulate a table.


// <ANCHOR NAME="Tables:motivation">
// <motivation></ANCHOR>
//
// The Casacore tables are mainly based upon the ideas of Allen Farris,
// as laid out in the
// <A HREF="http://aips2.cv.nrao.edu/aips++/docs/reference/Database.ps.gz">
// AIPS++ Database document</A>, from where the following paragraph is taken:
// 
// <p>
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
// <p>
// 
// In response to these limitations, and other needs, the Casacore tables were
// designed.
// </motivation>

// <ANCHOR NAME="Tables:properties">
// <h3>Table Properties</h3></ANCHOR>
//
// Casacore tables have the following properties:
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
//       </UL>
//  <li> A column may be
//       <UL>
//        <LI> "filled" -- containing actual data, or
//	  <LI> "virtual" -- containing a recipe telling how the data will
//             be generated dynamically
//       </UL>
//  <li> Only the standard Casacore data types can be used in filled
//       columns, be they scalars or arrays:  Bool, uChar, Short, uShort,
//       Int, uInt, float, double, Complex, DComplex and String.
//       Furthermore scalars containing
//       <linkto class=TableRecord>record</linkto> values are possible
//  <li> A column can have a default value, which will automatically be stored
//       in a cell of the column, when a row is added to the table.
//  <li> <A HREF="#Tables:Data Managers">Data managers</A> handle the
//       reading, writing and generation of data. Each column in a table can
//       be assigned its own data manager, which allows for optimization of
//       the data storage per column. The choice of data manager determines
//       whether a column is filled or virtual.
//  <li> Table data are stored in a canonical format, so they can be read
//       on any machine. To avoid needless swapping of bytes, the data can
//       be stored in big endian (as used on e.g. SUN) or little endian
//       (as used on Intel PC-s) canonical format. 
//       By default it uses the format specified in the aipsrc variable
//       <code>table.endianformat</code> which defaults to
//       <code>Table::LocalEndian</code> (thus the endian format of the
//       machine being used).
//  <li> The SQL-like
//       <a href="../notes/199.html">Table Query Language</a> (TaQL)
//       can be used to do operations on tables like
//       select, sort, update, insert, delete, and create.
// </ul>
//
// Tables can be in one of three forms:
// <ul>
// <li> A plain table is a table stored on disk.
//      It can be shared by multiple processes.
// <li> A memory table is a table held in memory.
//      It is a process specific table, thus not sharable.
//      The <linkto class=Table>Table::copy</linkto> function can be used
//      to turn a memory table into a plain table.
// <li> A reference table is a table referencing a plain or memory table.
//      It is the result of a selection or sort on another table.
//      A reference table references the data in the other table, thus
//      changing data in a reference table means that the data in the
//      original table are changed.
//      The <linkto class=Table>Table::deepCopy</linkto> function can be
//      used to turn a reference table into a plain table.
// </ul>
// Concurrent access from different processes to the same plain table is
// fully supported by means of a <A HREF="#Tables:LockSync">
// locking/synchronization</A> mechanism. Concurrent access over NFS is also
// supported.
// <p>
// A (somewhat primitive) mechanism is available to do a
// <A HREF="#Tables:KeyLookup">table lookup</A> based on the contents
// of a key. In the future this might be replaced by a proper B+-tree index
// mechanism.

// <ANCHOR NAME="Tables:open">
// <h3>Opening an Existing Table</h3></ANCHOR>
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

// <ANCHOR NAME="Tables:read">
// <h3>Reading from a Table</h3></ANCHOR>
//
// You can read data from a table column with the "get" functions
// in the classes
// <linkto class="ScalarColumn:description">ScalarColumn&lt;T&gt;</linkto>
// and
// <linkto class="ArrayColumn:description">ArrayColumn&lt;T&gt;</linkto>.
// For scalars of a standard data type (i.e. Bool, uChar, Int, Short,
// uShort, uInt, float, double, Complex, DComplex and String) you could
// instead use 
// <linkto class="TableColumn">TableColumn::getScalar(...)</linkto> or
// <linkto class="TableColumn">TableColumn::asXXX(...)</linkto>.
// These functions offer an extra: they do automatic data type promotion;
// so that you can, for example, get a double value from a float column.
//
// These "get" functions are used in the same way as the simple "put"
// functions described in the previous section.
// <p>
// <linkto class="ScalarColumn:description">ScalarColumn&lt;T&gt;</linkto>
// can be constructed for a non-writable column. However, an exception
// is thrown if the put function is used for it.
// The same is true for
// <linkto class="ArrayColumn:description">ArrayColumn&lt;T&gt;</linkto> and
// <linkto class="TableColumn:description">TableColumn</linkto>.
// <p>
// A typical program could look like:
// <srcblock>
// #include <casacore/tables/Tables/Table.h>
// #include <casacore/tables/Tables/ScalarColumn.h>
// #include <casacore/tables/Tables/ArrayColumn.h>
// #include <casacore/casa/Arrays/Vector.h>
// #include <casacore/casa/Arrays/Slicer.h>
// #include <casacore/casa/Arrays/ArrayMath.h>
// #include <iostream>
// 
// main()
// {
//     // Open the table (readonly).
//     Table tab ("some.name");
//
//     // Construct the various column objects.
//     // Their data type has to match the data type in the table description.
//     ScalarColumn<Int> acCol (tab, "ac");
//     ArrayColumn<Float> arr2Col (tab, "arr2");
//
//     // Loop through all rows in the table.
//     uInt nrrow = tab.nrow();
//     for (uInt i=0; i<nrow; i++) {
//         // Read the row for both columns.
//         cout << "Column ac in row i = " << acCol(i) << endl;
//         Array<Float> array = arr2Col.get (i);
//     }
//
//     // Show the entire column ac,
//     // and show the 10th element of arr2 in each row..
//     cout << ac.getColumn();
//     cout << arr2.getColumn (Slicer(Slice(10)));
// }
// </srcblock>

// <ANCHOR NAME="Tables:creation">
// <h3>Creating a Table</h3></ANCHOR>
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
// The recipe above is meant for the creation a plain table, but the
// creation of a memory table is exactly the same. The only difference
// is that in call to construct the Table object the Table::Memory
// type has to be given. Note that in the SetupNewTable object the columns
// can be bound to any data manager. <src>MemoryTable</src> will rebind 
// stored columns to the <linkto class=MemoryStMan>MemoryStMan</linkto>
// storage manager, but virtual columns bindings are not changed.

//
// The following example shows how you can create a table. An example
// specifically illustrating the creation of the
// <A HREF="#Tables:Table Description">table description</A> is given
// in that section. Other sections discuss the access to the table.
//
// <srcblock>
// #include <casacore/tables/Tables/TableDesc.h>
// #include <casacore/tables/Tables/SetupNewTab.h>
// #include <casacore/tables/Tables/Table.h>
// #include <casacore/tables/Tables/ScaColDesc.h>
// #include <casacore/tables/Tables/ScaRecordColDesc.h>
// #include <casacore/tables/Tables/ArrColDesc.h>
// #include <casacore/tables/Tables/StandardStMan.h>
// #include <casacore/tables/Tables/IncrementalStMan.h>
// 
// main()
// {
//     // Step1 -- Build the table description.
//     TableDesc td("tTableDesc", "1", TableDesc::Scratch);
//     td.comment() = "A test of class SetupNewTable";
//     td.addColumn (ScalarColumnDesc<Int> ("ab" ,"Comment for column ab"));
//     td.addColumn (ScalarColumnDesc<Int> ("ac"));
//     td.addColumn (ScalarColumnDesc<uInt> ("ad","comment for ad"));
//     td.addColumn (ScalarColumnDesc<Float> ("ae"));
//     td.addColumn (ScalarRecordColumnDesc ("arec"));
//     td.addColumn (ArrayColumnDesc<Float> ("arr1",3,ColumnDesc::Direct));
//     td.addColumn (ArrayColumnDesc<Float> ("arr2",0));
//     td.addColumn (ArrayColumnDesc<Float> ("arr3",0,ColumnDesc::Direct));
// 
//     // Step 2 -- Setup a new table from the description.
//     SetupNewTable newtab("newtab.data", td, Table::New);
//
//     // Step 3 -- Create storage managers for it.
//     StandardStMan stmanStand_1;
//     StandardStMan stmanStand_2;
//     IncrementalStMan stmanIncr;
// 
//     // Step 4 -- First, bind all columns to the first storage
//     // manager. Then, bind a few columns to another storage manager
//     // (which will overwrite the previous bindings).
//     newtab.bindAll (stmanStand_1);
//     newtab.bindColumn ("ab", stmanStand_2);
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
// To create a table in memory, only step 6 has to be modified slightly to:
// <srcblock>
//     Table tab(newtab, Table::Memory, 10);
// </srcblock>

// <ANCHOR NAME="Tables:write">
// <h3>Writing into a Table</h3></ANCHOR>
//
// Once a table has been created or has been opened for read/write,
// you want to write data into it. Before doing that you may have
// to add one or more rows to the table.
// <note role=tip> If a table was created with a given number of rows, you
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
// #include <casacore/tables/Tables/TableDesc.h>
// #include <casacore/tables/Tables/SetupNewTab.h>
// #include <casacore/tables/Tables/Table.h>
// #include <casacore/tables/Tables/ScaColDesc.h>
// #include <casacore/tables/Tables/ArrColDesc.h>
// #include <casacore/tables/Tables/ScalarColumn.h>
// #include <casacore/tables/Tables/ArrayColumn.h>
// #include <casacore/casa/Arrays/Vector.h>
// #include <casacore/casa/Arrays/Slicer.h>
// #include <casacore/casa/Arrays/ArrayMath.h>
// #include <iostream>
// 
// main()
// {
//     // First build the table description.
//     TableDesc td("tTableDesc", "1", TableDesc::Scratch);
//     td.comment() = "A test of class SetupNewTable";
//     td.addColumn (ScalarColumnDesc<Int> ("ac"));
//     td.addColumn (ArrayColumnDesc<Float> ("arr2",0));
// 
//     // Setup a new table from the description,
//     // and create the (still empty) table.
//     // Note that since we do not explicitly bind columns to
//     // data managers, all columns will be bound to the default
//     // standard storage manager StandardStMan.
//     SetupNewTable newtab("newtab.data", td, Table::New);
//     Table tab(newtab);
//
//     // Construct the various column objects.
//     // Their data type has to match the data type in the description.
//     ScalarColumn<Int> ac (tab, "ac");
//     ArrayColumn<Float> arr2 (tab, "arr2");
//     Vector<Float> vec2(100);
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
//   The generic TableColumn objects can be used for this purpose.
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
//     used if the data types of both input and output column are unknown.
//     Note that these functions are virtual.
//    <li>
//     <linkto class="ScalarColumn">ScalarColumn::put(...)</linkto>,
//     <linkto class="ArrayColumn">ArrayColumn::put(...)</linkto>,
//     <linkto class="ScalarColumn">ScalarColumn::putColumn(...)</linkto>, and
//     <linkto class="ArrayColumn">ArrayColumn::putColumn(...)</linkto>
//     are less generic and therefore potentially more efficient.
//     The most efficient variants are the ones taking a
//     Scalar/ArrayColumn&lt;T&gt;, because they require no data type
//     conversion.
//   </ul>
// </ol>

// <ANCHOR NAME="Tables:row-access">
// <h3>Accessing rows in a Table</h3></ANCHOR>
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
// RORecordFieldPtr<Double> col2(row.record(), "col2");
// RORecordFieldPtr<Array<Int> > col3(row.record(), "col3");
// for (uInt i=0; i<table.nrow(); i++) {
//     row.get (i);
//     someString = *col1;
//     somedouble = *col2;
//     someArrayInt = *col3;
// }
// </srcblock>
// The description of TableRow contains some more extensive examples.

// <ANCHOR NAME="Tables:select and sort">
// <h3>Table Selection and Sorting</h3></ANCHOR>
//
// The result of a select and sort of a table is another table,
// which references the original table. This means that an update
// of a sorted or selected table results in the update of the original
// table. The result is, however, a table in itself, so all table
// functions (including select and sort) can be used with it.
// Note that a true copy of such a reference table can be made with
// the <linkto class=Table>Table::deepCopy</linkto> function.
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
//    #include <casacore/tables/Tables/ExprNode.h>
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
//  <li> Arithmetic operators +, -, *, /, %, and unary + and -.
//  <li> Bit operators ^, &, |, and unary ~.
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
// select rows with a NAME equal to <src>abc</src>,
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
// <a href="../notes/199.html">Table Query Language</a> (TaQL) note 199.
// The language supports UDFs (User Defined Functions) in dynamically
// loadable libraries as explained in the note.
// <br>A TaQL command can be executed with the static function
// <src>tableCommand</src> defined in class
// <linkto class=TableParse>TableParse</linkto>.

// <ANCHOR NAME="Tables:concatenation">
// <h3>Table Concatenation</h3></ANCHOR>
// Tables with identical descriptions can be concatenated in a virtual way
// using the Table concatenation constructor. Such a Table object behaves
// as any other Table object, thus any operation can be performed on it.
// An identical description means that the number of columns, the column names,
// and their data types of the columns must be the same. The columns do not
// need to be ordered in the same way nor to be stored in the same way.
// <br>Note that if tables have different column names, it is possible
// to form a projection (as described in the previous section) first
// to make them appear identical.
//
// Sometimes a MeasurementSet is partitioned, for instance in chunks of
// one hour. All those chunks can be virtually concatenated this way.
// Note that all tables in the concatenation will be opened, thus one might
// run out of file descriptors if there are many chunks.
//
// Similar to reference tables, it is possible to make a concatenated Table
// persistent by using the <src>rename</src> function. It will not copy the
// data; only the names of the tables used are written.
//
// The keywords of a concatenated table are taken from the first table.
// It is possible to change or add keywords, but that is not persistent,
// not even if the concatenated table is made persistent.
// <br>The keywords holding subtables can be handled in a special way.
// Normally the subtables of the concatenation are the subtables of the first
// table are used, but is it possible to concatenate subtables as well by
// giving their names in the constructor.
// In this way the, say, SYSCAL subtable of a MeasurementSet can be
// concatenated as well.
// <srcblock>
//   // Create virtual concatenation of ms0 and ms1.
//   Block<String> names(2);
//   names[0] = "ms0";
//   names[1] = "ms1";
//   // Also concatenate their SYSCAL subtables.
//   Block<String> subNames(1, "SYSCAL");
//   Table concTab (names, subNames);
// </srcblock>

// <ANCHOR NAME="Tables:iterate">
// <h3>Table Iterators</h3></ANCHOR>
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

// <ANCHOR NAME="Tables:vectors">
// <h3>Table Vectors</h3></ANCHOR>
//
// A table vector makes it possible to treat a column in a table
// as a vector. Almost all operators and functions defined for normal
// vectors, are also defined for table vectors. So it is, for instance,
// possible to add a constant to a table vector. This has the effect
// that the underlying column gets changed.
//
// You can use the templated class
// <linkto class="TableVector:description">TableVector</linkto>
// to make a scalar column appear as a (table) vector.
// Columns containing arrays or tables are not supported.
// The data type of the TableVector object must match the
// data type of the column.
// A table vector can also hold a normal vector so that (temporary)
// results of table vector operations can be handled.
//
// In the following example we double the data in column COL1 and
// store the result in a temporary table vector.
// <srcblock>
//    // Create a table vector for column COL1.
//    // Note that if the table is readonly, putting data in the table vector
//    // results in an exception.
//    Table tab ("Table.data");
//    TableVector<Int> tabvec(tab, "COL1");
//    // Multiply it by a constant. Result is kept in a Vector in memory.
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
//    tabvec *= 2;
// </srcblock>

// <ANCHOR NAME="Tables:keywords">
// <h3>Table Keywords</h3></ANCHOR>
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

// <ANCHOR NAME="Tables:Table Description">
// <h3>Table Description</h3></ANCHOR>
//
// A table contains a description of itself, which defines the layout of the
// columns and the keyword sets for the table and for the individual columns.
// It may also define initial keyword sets and default values for the columns.
// Such a default value is automatically stored in a cell in the table column,
// whenever a row is added to the table.
//
// The creation of the table descriptor is the first step in the creation of
// a new table. The description is part of the table itself, but may also
// exist in a separate file. This is useful if you need to create a number
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
//  <li> <linkto class="ScalarRecordColumnDesc:description">ScalarRecordColumnDesc;
//       </linkto>
//       -- defines a column containing a scalar record value.
//  <li> <linkto class="ArrayColumnDesc:description">ArrayColumnDesc&lt;T&gt;
//       </linkto>
//       -- defines a column containing an (in)direct array.
// </ul>
//
// Here follows a typical example of the construction of a table
// description. For more specialized things -- like the definition of a
// default data manager -- we refer to the descriptions of the above
// mentioned classes.
//
// <srcblock>
// #include <casacore/tables/Tables/TableDesc.h>
// #include <casacore/tables/Tables/ScaColDesc.h>
// #include <casacore/tables/Tables/ArrColDesc.h>
// #include <aips/Tables/ScaRecordTabDesc.h>
// #include <casacore/tables/Tables/TableRecord.h>
// #include <casacore/casa/Arrays/IPosition.h>
// #include <casacore/casa/Arrays/Vector.h>
//
// main()
// {
//     // Create a new table description
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
//     acColumn.rwKeywordSet().define ("scale" Complex(0,0));
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
//     // Add columns containing records.
//     td.addColumn (ScalarRecordColumnDesc ("Rec1"));
// }
// </srcblock>

// <ANCHOR NAME="Tables:Data Managers">
// <h3>Data Managers</h3></ANCHOR>
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
//
// Data managers needs to be registered, so they can be found when a table is
// opened. All data managers mentioned below are part of the system and
// pre-registered.
// It is, however, also possible to load data managers on demand. If a data
// manager is not registered it is tried to load a shared library with the
// part of the data manager name (in lowercase) before a dot or left arrow.
// The dot makes it possible to have multiple data managers in a shared library,
// while the left arrow is meant for templated data manager classes.
// <br>E.g. if <src>BitFlagsEngine<uChar></src> was not registered, the shared
// library <src>libbitflagsengine.so</src> (or .dylib) will be loaded. If
// successful, its function <src>register_bitflagsengine()</src> will be
// executed which should register the data manager(s). Thereafter it is known
// and will be used. For example in a file Register.h and Register.cc:
// <srcblock>
//   // Declare in .h file as C function, so no name mangling is done.
//   extern "C" {
//     void register_bitflagsengine();
//   }
//   // Implement in .cc file.
//   void register_bitflagsengine()
//   {
//     BitFlagsEngine<uChar>::registerClass();
//     BitFlagsEngine<Short>::registerClass();
//     BitFlagsEngine<Int>::registerClass();
//   }
// </srcblock>
// There are several functions that can give information which data managers
// are used for which columns and to obtain the characteristics and properties
// of them. Class RODataManAccessor and derived classes can be used for it
// as well as the functions <src>dataManagerInfo</src> and
// <src>showStructure</src> in class Table.

// <ANCHOR NAME="Tables:storage managers">
// <h3>Storage Managers</h3></ANCHOR>
//
// Storage managers are used to store the data contained in the column cells.
// At table construction time the binding of columns to storage managers is done.
// <br>Each storage manager uses one or more files (usually called table.fi_xxx
// where i is a sequence number and _xxx is some kind of extension).
// Typically several file are used to store the data of the columns of a table.
// <br>In order to reduce the number of files (and to support large block sizes),
// it is possible to have a single container file (a MultiFile) containing all
// data files used by the storage managers. Such a file is called table.mf.
// Note that the program <em>lsmf</em> can be used to see which
// files are contained in a MultiFile. The program <em>tomf</em> can
// convert the files in a MultiFile to regular files.
// <br>At table creation time it is decided if a MultiFile will be used. It
// can be done by means of the StorageOption object given to the SetupNewTable
// constructor and/or by the aipsrc variables:
// <ul>
//  <li> <src>table.storage.option</src> which can have the value
//       'multifile', 'sepfile' (meaning separate files), or 'default'.
//       Currently the default is to use separate files.
//  <li> <src>table.storage.blocksize</src> defines the block size to be
//       used by a MultiFile. If 0 is given, the file system's block size
//       will be used.
// </ul>
// About all standard storage managers support the MultiFile.
// The exception is StManAipsIO, because it is hardly ever used.
//
// Several storage managers exist, each with its own storage characteristics.
// The default and preferred storage manager is <src>StandardStMan</src>.
// Other storage managers should only be used if they pay off in
// file space (like <src>IncrementalStMan</src> for slowly varying data)
// or access speed (like the tiled storage managers for large data arrays).
// <br>The storage managers store the data in a big or little endian
// canonical format. The format can be specified when the table is created.
// By default it uses the endian format as specified in the aipsrc variable
// <code>table.endianformat</code> which can have the value local, big,
// or little. The default is local.
// <ol>
//  <li>
//   <linkto class="StandardStMan:description">StandardStMan</linkto>
//   stores all the values in so-called buckets (equally sized chunks
//   in the file). It requires little memory.
//   <br>It replaces the old <src>StManAipsIO</src>.
//
//  <li>
//   <linkto class="IncrementalStMan:description">IncrementalStMan</linkto>
//   uses a storage mechanism resembling "incremental backups". A value
//   is only stored if it is different from the previous row. It is
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
//   <linkto class="StManAipsIO:description">StManAipsIO</linkto>
//   uses <src>AipsIO</src> to store the data in the columns.
//   It supports all table functionality, but its I/O is probably not
//   as efficient as other storage managers. It also requires that
//   a large part of the table fits in memory.
//   <br>It should not be used anymore, because it uses a lot of memory
//   for larger tables and because it is not very robust in case an
//   application or system crashes.
//
//  <li>
//   <linkto class="MemoryStMan:description">MemoryStMan</linkto>
//   holds the data in memory. It means that data 'stored' with this
//   storage manager are NOT persistent.
//   <br>This storage manager is primarily meant for tables held in
//   memory, but it can also be useful for temporary columns in
//   normal tables. Note, however, that if a table is accessed
//   concurrently from multiple processes, MemoryStMan data cannot be
//   synchronized.
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
// However, it is always possible to delete all columns of a data
// manager in one single call.
// </note>

// <ANCHOR NAME="Tables:TiledStMan">
// <h3>Tiled Storage Manager</h3></ANCHOR>
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
//       N-dimensional so-called hypercolumn.
//       Elaborate hypercolumns can be defined using
//       <linkto file="TableDesc.h#defineHypercolumn">
//       TableDesc::defineHypercolumn</linkto>).
//       <br>Note that defining a hypercolumn is only necessary if it
//       contains multiple columns or if the TiledDataStMan is used.
//       It means that in practice it is hardly ever needed to define a
//       hypercolumn.
//       <br>A hypercolumn consists of up to three types of columns:
//       <dl>
//        <dt> Data columns
//        <dd> contain the data to be stored in a tiled way. This will
//             be done in tiled hypercubes.
//             There must be at least one data column.
//             <br> For example: a table contains UV-data with
//                  data columns "Visibility" and "Weight".
//        <dt> Coordinate columns
//        <dd> define the world coordinates of the pixels in the data columns.
//             Coordinate columns are optional, but if given there must
//             be N coordinate columns for an N-dimensional hypercolumn.
//             <br>
//             For example: the data in the example above is 4-dimensional
//             and has coordinate columns "Time", "Baseline", "Frequency",
//             and "Polarization".
//        <dt> Id columns
//        <dd> are needed if TiledDataStMan is used.
//             Different rows in the data columns can be stored in different
//             hypercubes. The values in the id column(s) uniquely identify
//             the hypercube a row is stored in.
//             <br>
//             For example: the line and continuum data in a MeasurementSet
//             table need to be stored in 2 different hypercubes (because
//             their shapes are different (see below)). A column containing
//             the type (line or continuum) has to be used as an id column.
//       </dl>
//  <li> If multiple data columns are used, the shape of their data
//       must be conforming in each individual row.
//       If data in different rows have different shapes, they must be
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
//  <li> The last axis of a hypercube can be extensible. This means that
//       the size of that axis does not need to be defined when the
//       hypercube is defined in the storage manager. Instead, the hypercube
//       can be extended when another chunk of data has to be stored.
//       This can be very useful in, for example, a (quasi-)realtime
//       environment where the size of the time axis is not known.
//  <li> If coordinate columns are defined, they describe the coordinates
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
// <p>
// The following Tiled Storage Managers are available:
// <dl>
//  <dt> <linkto class=TiledShapeStMan:description>TiledShapeStMan</linkto>
//  <dd> can be seen as a specialization of <src>TiledDataStMan</src>
//       by using the array shape as the id value.
//       Similarly to <src>TiledDataStMan</src> it can maintain multiple
//       hypercubes and store multiple rows in a hypercube, but it is
//       easier to use, because the special <src>addHypercube</src> and
//       <src>extendHypercube</src> functions are not needed.
//       An hypercube is automatically added when a new array shape is
//       encountered.
//       <br>
//       This storage manager could be used for a table with a column
//       containing line and continuum data, which will result
//       in 2 hypercubes.
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
//       storage manager is only possible if all columns in the hypercolumn
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
//       It makes it possible to store, say, row 0-9 in hypercube A,
//       row 10-34 in hypercube B, row 35-54 in hypercube A again, etc..
//       <br>
//       The drawback of this storage manager is that its hypercubes are not
//       automatically extended when adding new rows. The special functions
//       <src>addHypercube</src> and <src>extendHypercube</src> have to be
//       used making it somewhat tedious to use.
//       Therefore this storage manager may become obsolete in the near future.
// </dl>
// The Tiled Storage Managers have 3 ways to access and cache the data.
// Class <linkto class=TSMOption>TSMOption</linkto> can be used to setup an
// access choice and use it in a Table constructor.
// <ul>
//  <li> The old way (the only way until January 2010) uses a cache
//       of its own to keep tiles that might need to be reused. It will always
//       access entire tiles, even if only a small part is needed.
//       It is possible to define a maximum cache size. The description of class
//       <linkto class=ROTiledStManAccessor>ROTiledStManAccessor</linkto>
//       contains a discussion about the effect of defining a maximum cache
//       size.
//  <li> Memory-mapping the data files. In this way the operating system
//       takes care of the IO and caching. However, the limited address space
//       may preclude using it for large tables on 32-bit systems.
//  <li> Use buffered IO and let the kernel's file cache take care of caching.
//       It will access the data in chunks of the given buffer size, so the
//       entire tile does not need to be accessed if only a small part is
//       needed.
// </ul>
// Apart from reading, all access ways described above can also handle writing
// and extending tables. They create fully equal files. Both little and big
// endian data can be read or written.

// <ANCHOR NAME="Tables:virtual column engines">
// <h3>Virtual Column Engines</h3></ANCHOR>
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
// difference. This particular example could be done using 
// <linkto class="VirtualTaQLColumn:description">VirtualTaQLColumn</linkto>.
// <p>
// Several virtual column engines exist:
// <ol>
//  <li> The class
//   <linkto class="VirtualTaQLColumn:description">VirtualTaQLColumn</linkto>
//   makes it possible to define a column as an arbitrary expression of
//   other columns. It uses the <a href="../notes/199.html">TaQL</a>
//   CALC command. The virtual column can be a scalar or an array and
//   can have one of the standard data types supported by the Table System.
//  <li> The class
//   <linkto class="BitFlagsEngine:description">BitFlagsEngine</linkto>
//   maps an integer bit flags column to a Bool column. A read and write mask
//   can be defined telling which bits to take into account when mapping
//   to and from Bool (thus when reading or writing the Bool).
//  <li> The class
//   <linkto class="CompressFloat:description">CompressFloat</linkto>
//   compresses a single precision floating point array by scaling the
//   values to shorts (16-bit integer).
//  <li> The class
//   <linkto class="CompressComplex:description">CompressComplex</linkto>
//   compresses a single precision complex array by scaling the
//   values to shorts (16-bit integer). In fact, the 2 parts of the complex
//   number are combined to an 32-bit integer.
//  <li> The class
//   <linkto class="CompressComplexSD:description">CompressComplexSD</linkto>
//   does the same as CompressComplex, but optimizes for the case where the
//   imaginary part is zero (which is often the case for Single Dish data).
//  <li> The double templated class
//   <linkto class="ScaledArrayEngine:description">ScaledArrayEngine</linkto>
//   scales the data in an array from, for example,
//   float to short before putting it.
//  <li> The double templated class
//   <linkto class="MappedArrayEngine:description">MappedArrayEngine</linkto>
//   converts the data from one data type to another. Sometimes it might be
//   needed to store the residual data in an MS in double precision.
//   Because the imaging task can only handle single precision, this enigne
//   can be used to map the data from double to single precision.
//  <li> The double templated class
//   <linkto class="RetypedArrayEngine:description">RetypedArrayEngine</linkto>
//   converts the data from one data type to another with the possibility
//   to reduce the number of dimensions. For example, it can be used to
//   store an 2-d array of StokesVector objects as a 3-d array of floats
//   by treating the 4 data elements as an extra array axis. If the
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

// <ANCHOR NAME="Tables:LockSync">
// <h3>Table locking and synchronization</h3></ANCHOR>
//
// Multiple concurrent readers and writers (also via NFS) of a
// table are supported by means of a locking/synchronization mechanism.
// This mechanism is not very sophisticated in the sense that it is
// very coarsely grained. When locking, the entire table gets locked.
// A special lock file is used to lock the table. This lock file also
// contains some synchronization data.
// <p>
// Five ways of locking are supported (see class
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
//  <dt> TableLock::AutoNoReadLocking
//  <dd> is similar to AutoLocking. However, no lock is acquired when
//       reading the table making it possible to read the table while
//       another process holds a write-lock. It also means that for read
//       purposes no automatic synchronization is done when the table is
//       updated in another process.
//       Explicit synchronization can be done by means of the function
//       <src>Table::resync</src>.
//  <dt> TableLock::UserLocking
//  <dd> requires that the programmer explicitly acquires and releases
//       a lock on the table. This makes some kind of transaction
//       processing possible. E.g. set a write lock, add a row,
//       write all data into the row and release the lock.
//       The Table functions <src>lock</src> and <src>unlock</src>
//       have to be used to acquire and release a (read or write) lock.
//  <dt> TableLock::UserNoReadLocking
//  <dd> is similar to UserLocking. However, similarly to AutoNoReadLocking
//       no lock is needed to read the table.
//  <dt> TableLock::NoLocking
//  <dd> does not use table locking. It is the responsibility of the
//       user to ensure that no concurrent access is done on the same
//       bucket or tile in a storage manager, otherwise a table might
//       get corrupted.
//       <br>This mode is always used if Casacore is built with
//       -DAIPS_TABLE_NOLOCKING.
// </dl>
// Synchronization of the processes accessing the same table is done
// by means of the lock file. When a lock is released, the storage
// managers flush their data into the table files. Some synchronization data
// is written into the lock file telling the new number of table rows
// and telling which storage managers have written data.
// This information is read when another process acquires the lock
// and is used to determine which storage managers have to refresh
// their internal caches.
// <br>Note that for the NoReadLocking modes (see above) explicit
// synchronization might be needed using <src>Table::resync</src>.
// <p>
// The function <src>Table::hasDataChanged</src> can be used to check
// if a table is (being) changed by another process. In this way
// a program can react on it. E.g. the table browser can refresh its
// screen when the underlying table is changed.
// <p>
// In general the default locking option will do.
// From the above it should be clear that heavy concurrent access
// results in a lot of flushing, thus will have a negative impact on
// performance. If uninterrupted access to a table is needed,
// the <src>PermanentLocking</src> option should be used.
// If transaction-like processing is done (e.g. updating a table
// containing an observation catalogue), the <src>UserLocking</src>
// option is probably best.
// <p>
// Creation or deletion of a table is not possible if that table
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
// The following example deletes a table if it is not used in
// another process.
// <srcblock>
// Table tab ("some.name");
// if (! tab.isMultiUsed()) {
//     tab.markForDelete();
// }
// </srcblock>

// <ANCHOR NAME="Tables:KeyLookup">
// <h3>Table lookup based on a key</h3></ANCHOR>
//
// Class <linkto class=ColumnsIndex>ColumnsIndex</linkto> offers the
// user a means to find the rows matching a given key or key range.
// It is a somewhat primitive replacement of a B-tree index and in the
// future it may be replaced by a proper B+-tree implementation.
// <p>
// The <src>ColumnsIndex</src> class makes it possible to build an
// in-core index on one or more columns. Looking a key or key range
// is done using a binary search on that index. It returns a vector
// containing the row numbers of the rows matching the key (range).
// <p>
// The class is not capable of tracing changes in the underlying column(s).
// It detects a change in the number of rows and updates the index
// accordingly. However, it has to be told explicitly when a value
// in the underlying column(s) changes.
// <p>
// The following example shows how the class can be used.
// <example>
// Suppose one has an antenna table with key ANTENNA.
// <srcblock>
// // Open the table and make an index for column ANTENNA.
// Table tab("antenna.tab")
// ColumnsIndex colInx(tab, "ANTENNA");
// // Make a RecordFieldPtr for the ANTENNA field in the index key record.
// // Its data type has to match the data type of the column.
// RecordFieldPtr<Int> antFld(colInx.accessKey(), "ANTENNA");
// // Now loop in some way and find the row for the antenna
// // involved in that loop.
// Bool found;
// while (...) {
//     // Fill the key field and get the row number.
//     // ANTENNA is a unique key, so only one row number matches.
//     // Otherwise function getRowNumbers had to be used.
//     *antFld = antenna;
//     uInt antRownr = colInx.getRowNumber (found);
//     if (!found) {
//         cout << "Antenna " << antenna << " is unknown" << endl;
//     } else {
//         // antRownr can now be used to get data from that row in
//         // the antenna table.
//     }
// }
// </srcblock>
// </example>
// <linkto class=ColumnsIndex>ColumnsIndex</linkto> itself contains a more
// advanced example. It shows how to use a private compare function
// to adjust the lookup if the index does not contain single
// key values, but intervals instead. This is useful if a row in
// a (sub)table is valid for, say, a time range instead of a single
// timestamp.

// <ANCHOR NAME="Tables:performance">
// <h3>Performance and robustness considerations</h3></ANCHOR>
//
// The Table System resembles a database system, but it is not as robust.
// It lacks the transaction and logging facilities common to data base systems.
// It means that in case of a crash data might be lost.
// To reduce the risk of data loss to
// a minimum, it is advisable to regularly do a <tt>flush</tt>, optionally
// with an <tt>fsync</tt> to ensure that all data are really written.
// However, that can degrade the performance because it involves extra writes.
// So one should find the right balance between robustness and performance.
//
// To get a good feeling for the performance issues, it is important to
// understand some of the internals of the Table System.
// <br>The storage managers drive the performance. All storage managers use
// buckets (called tiles for the TiledStMan) which contain the data.
// All IO is done by bucket. The bucket/tile size is defined when creating
// the storage manager objects. Sometimes the default will do, but usually
// it is better to set it explicitly.
//
// It is best to do a flush when a tile is full.
// For example: <br>
// When creating a MeasurementSet containing N antennae (thus N*(N-1) baselines
// or N*(N+1) if auto-correlations are stored as well) it makes sense to
// store, say, N/2 rows in a tile and do a flush each time all baselines
// are written. In that way tiles are fully filled when doing the flush, so
// no extra IO is involved.
// <br>Here is some code showing this when creating a MeasurementSet.
// The code should speak for itself.
// <srcblock>
// MS* createMS (const String& msName, int nrchan, int nrant)
// {
//   // Get the MS main default table description.
//   TableDesc td = MS::requiredTableDesc();
//   // Add the data column and its unit.
//   MS::addColumnToDesc(td, MS::DATA, 2);
//   td.rwColumnDesc(MS::columnName(MS::DATA)).rwKeywordSet().
//                                                 define("UNIT","Jy");
//   // Store the DATA and FLAG column in two separate files.
//   // In this way accessing FLAG only is much cheaper than
//   // when combining DATA and FLAG.
//   // All data have the same shape, thus use TiledColumnStMan.
//   // Also store UVW with TiledColumnStMan.
//   Vector<String> tsmNames(1);
//   tsmNames[0] = MS::columnName(MS::DATA);
//   td.rwColumnDesc(tsmNames[0]).setShape (IPosition(2,itsNrCorr,itsNrFreq));
//   td.defineHypercolumn("TiledData", 3, tsmNames);
//   tsmNames[0] = MS::columnName(MS::FLAG);
//   td.rwColumnDesc(tsmNames[0]).setShape (IPosition(2,itsNrCorr,itsNrFreq));
//   td.defineHypercolumn("TiledFlag", 3, tsmNames);
//   tsmNames[0] = MS::columnName(MS::UVW);
//   td.defineHypercolumn("TiledUVW", 2, tsmNames);
//   // Setup the new table.
//   SetupNewTable newTab(msName, td, Table::New);
//   // Most columns vary slowly and use the IncrStMan.
//   IncrementalStMan incrStMan("ISMData");
//   // A few columns use he StandardStMan (set an appropriate bucket size).
//   StandardStMan    stanStMan("SSMData", 32768);
//   // Store all pol and freq and some rows in a single tile.
//   // autocorrelations are written, thus in total there are
//   // nrant*(nrant+1)/2 baselines. Ensure a baseline takes up an
//   // integer number of tiles.
//   TiledColumnStMan tiledData("TiledData",
//                              IPosition(3,4,nchan,(nrant+1)/2));
//   TiledColumnStMan tiledFlag("TiledFlag",
//                              IPosition(3,4,nchan,8*(nrant+1)/2));
//   TiledColumnStMan tiledUVW("TiledUVW", IPosition(2,3,));
//                             IPosition(2,3,nrant*(nrant+1)/2));
//   newTab.bindAll (incrStMan);
//   newTab.bindColumn(MS::columnName(MS::ANTENNA1),stanStMan);
//   newTab.bindColumn(MS::columnName(MS::ANTENNA2),stanStMan);
//   newTab.bindColumn(MS::columnName(MS::DATA),tiledData);
//   newTab.bindColumn(MS::columnName(MS::FLAG),tiledFlag);
//   newTab.bindColumn(MS::columnName(MS::UVW),tiledUVW);
//   // Create the MS and its subtables.
//   // Get access to its columns.
//   MS* msp = new MeasurementSet(newTab);
//   // Create all subtables.
//   // Do this after the creation of optional subtables,
//   // so the MS will know about those optional sutables.
//   msp->createDefaultSubtables (Table::New);
//   return msp;
// }
// </srcblock>

// <h4>Some more performance considerations</h4>
// Which storage managers to use and how to use them depends heavily on
// the type of data and the access patterns to the data. Here follow some
// guidelines:
// <ol>
//  <li> Scalar data can be stored with the StandardStMan (SSM) or
//       IncrementalStMan (ISM). For slowly varying data (e.g. the TIME column
//       in a MeasurementSet) it is best to use the ISM. Otherwise the SSM.
//       Note that very long strings (longer than the bucketsize) can only
//       be stored with the SSM.
//  <li> Any number of storage managers can be used. In fact, each column
//       can have a storage manager of its own resulting in column-wise
//       stored data which is more and more used in data base systems.
//       In that way a query or sort on that column is very fast, because
//       the buckets to read only contain data of that column.
//       In practice one can decide to combine a few frequently used columns
//       in a storage manager.
//  <li> Array data can be stored with any column manager. Small fixed size
//       arrays can be stored directly with the SSM
//       (or ISM if not changing much).
//       However, they can also be stored with a TiledStMan (TSM) as shown
//       for the UVW column in the example above.
//       <br> Large arrays should usually be stored with a TSM. However,
//       if it must be possible to change the shape of an array after it
//       was stored, the SSM (or ISM) must be used. Note that in that
//       case a lot of disk space can be wasted, because the SSM and ISM
//       store the array data at the end of the file if the array got
//       bigger and do not reuse the old space. The only way to
//       reclaim it is by making a deep copy of the entire table.
//  <li> If an array is stored with a TSM, it is important to decide
//       which TSM to use.
//       <ol>
//        <li> The TiledColumnStMan is the most efficient, but only suitable
//         for arrays having the same shape in the entire column.
//        <li> The TiledShapeStMan is suitable for columns where the arrays
//         can have a few shapes.
//        <li> The TiledCellStMan is suitable for columns where the arrays
//         can have many different shapes.
//       </ol>
//       This is discussed in more detail
//       <a href="#Tables:TiledStMan">above</a>.
//  <li> If storing an array with a TSM, it can be very important to
//       choose the right tile shape. Not only does this define the size
//       of a tile, but it also defines if access in other directions
//       than the natural direction can be fast. It is also discussed in
//       more detail <a href="#Tables:TiledStMan">above</a>.
//  <li> Columns can be combined in a single TiledStMan. For instance, combining DATA
//       and FLAG is advantageous if FLAG is always used with DATA. However, if FLAG
//       is used on its own (e.g. in combination with CORRECTED_DATA), it is better
//       to separate them, otherwise tiles containing FLAG also contain DATA making the
//       tiles much bigger, thus more expensive to access.
// </ol>
//
// <ANCHOR NAME="Tables:iotracing">
// <h4>IO Tracing</h4></ANCHOR>
//
// Several forms of tracing can be done to see how the Table I/O performs.
// <ul>
//  <li> On Linux/UNIX systems the <src>strace</src> command can be used to
//       collect trace information about the physical IO.
//  <li> The function <src>showCacheStatistics</src> in class
//       TiledStManAccessor can be used to show the number of actual reads
//       and writes and the percentage of cache hits.
//  <li> The software has some options to trace the operations done on
//       tables. It is possible to specify the columns and/or the operations
//       to be traced. The following <src>aipsrc</src> variables can be used.
//   <ul>
//    <li> <src>table.trace.filename</src> specifies the file to write the
//         trace output to. If not given or empty, no tracing will be done.
//         The file name can contain environment variables or a tilde.
//    <li> <src>table.trace.operation</src> specifies the operations to be
//         traced. It is a string containing s, r, and/or w where
//         s means tracing RefTable construction (selection/sort),
//         r means column reads, and w means column writes.
//         If empty, only the high level table operations (open, create, close)
//         will be traced.
//    <li> <src>table.trace.columntype</src> specifies the types of columns to
//         be traced. It is a string containing the characters s, a, and/or r.
//         s means all scalar columns, a all array columns, and r all record
//         columns. If empty and if <src>table.trace.column</src> is empty,
//         its default value is a.
//    <li> <src>table.trace.column</src> specifies names of columns to be
//         traced. Its value can be one or more glob-like patterns separated
//         by commas without any whitespace. The default is empty.
//         For example:
// <srcblock>
//    table.trace.column: *DATA,FLAG,WEIGHT*
// </srcblock>
//         to trace all DATA, the FLAG, and all WEIGHT columns.
//   </ul>
//       The trace output is a text file with the following columns
//       separated by a space.
//   <ul>
//    <li> The UTC time the trace line was written (with msec accuracy).
//    <li> The operation: n(ew), o(pen), c(lose), t(able), r(ead), w(rite),
//         s(election/sort/iter), p(rojection).
//         t means an arbitrary table operation as given in the name column.
//    <li> The table-id (as t=i) given at table creation (new) or open.
//    <li> The table name, column name, or table operation
//         (as <src>*oper*</src>).
//         <src>*reftable*</src> means that the operation is on a RefTable
//         (thus result of selection, sort, projection, or iteration).
//    <li> The row or rows to access (* means all rows).
//         Multiple rows are given as a series of ranges like s:e:i,s:e:i,...
//         where e and i are only given if applicable (default i is 1).
//         Note that e is inclusive and defaults to s.
//    <li> The optional array shape to access (none means scalar).
//         In case multiple rows are accessed, the last shape value is the
//         number of rows.
//    <li> The optional slice of the array in each row as [start][end][stride].
//   </ul>
//       Shape, start, end, and stride are given in Fortran-order as
//       [n1,n2,...].
// </ul>

// <ANCHOR NAME="Tables:applications">
// <h4>Applications to inspect/manipulate a table</h4></ANCHOR>
// <ul>
//  <li><em>showtable</em> shows the structure of a table. It can show:
//   <ul>
//    <li> the columns and their format (optionally sorted on name)
//    <li> the data managers used to store the column data
//    <li> the table and/or column keywords and their values
//    <li> recursively the same info of the subtables
//   </ul>
//  <li><em>showtablelock</em> if a table is locked or opened and by
//      which process.
//  <li><em>lsmf</em> shows the virtual files contained in a MultiFile.
//  <li><em>tomf</em> copies the given files to a MultiFile.
//  <li><em>taql</em> can be used to query a table using the
//       <a href="../notes/199.html">Table Query Language</a> (TaQL).
// </ul>
//
// </synopsis>
// </module>



} //# NAMESPACE CASACORE - END

#endif
