//# tMemoryTable.cc: Test program for the MemoryTable class
//# Copyright (C) 2003,2004
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/DataMan/VirtualTaQLColumn.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test program for the MemoryTable class.
// </summary>


// (re) open and write a few rows
// aMode == 0  open new
// aMode == 1  use existing
void init (uInt aMode, Table&);

// reopen table, and throw away a row
void deleteRow(const uInt aRow, Table&);

// reopen table, and throw away a few rows
void deleteRows(const Vector<uInt>& aNrRows, Table&);

// delete a Column
void deleteColumn(const String aColumn, Table&);

// delete a column && put it back again
void deleteAndRestore(Table&);

// add a Column
void addColumn(DataType aDataType, Table&);

// add a few direct arrays
void addDirectArrays(Table&);

// add an indirect string array
void addIndStringArray(Table&);

// add an indirect array
void addIndArray(Table&);

// add a keyword and subtable
void addKeys(Table&);

// show table info
void info(const Table& aTable);

// put/putColumn cache test
void putColumnTest(Table&);

// Copy the table to a plain table
void copyTable(const Table&);

// Copy a reftable to the memory table to a plain table
void copyRefTable(const Table&);

// Copy the table to a memory table
void copyMemoryTable(const Table&);

// Copy a subset of a table to a memory table
void copyMemoryTableSubSet(const Table&);


int main ()
{
    try {
	Table tab;
	init            (0, tab);
        init            (1, tab);
	deleteRow       (0, tab);
	deleteRow       (17, tab);
	// delete and restore Column 1 (should use perfect fit)
	deleteAndRestore(tab);
	// 
	putColumnTest(tab);
	// delete middle Column
       	deleteColumn    ("Col-2", tab);
	// add a Bool Column Should fit in freed space
	addColumn       (TpBool, tab);
	// add a DComplex Column Should use new index && space
	addColumn       (TpDComplex, tab);
	// Test the copy table stuff.
	{
	  cout << endl << "Test copying ..." << endl;
	  Table tabc = tab.copyToMemoryTable ("tmtestc");
	  // Add a keyword and a subtable.
	  addKeys (tabc);
	  // copy to a plain table
	  copyTable(tabc);
	  // copy a reference to a plain table
          ///	  copyRefTable(tabc);
	  // copy to a memory table
	  copyMemoryTable(tabc);
	  // copy a subset to a memory table
	  copyMemoryTableSubSet(tabc);
	  // copy from a plain table
	  {
	    Table tab2("tMemoryTable_tmp.tabcp");
	    copyMemoryTable(tab2);
	    copyMemoryTableSubSet(tab2);
	  }
	  cout << endl;
	}
	// delete first Column
	deleteColumn    ("Col-1", tab);
	// delete last Column
	deleteColumn    ("Col-3", tab);
	addDirectArrays (tab);
	addIndStringArray(tab);
	addIndArray     (tab);
        Vector<uInt> aNrRows(3);
	for (uInt i=0; i< 3; i++) {
	  aNrRows(i) = i+3;
	}
	deleteRows      (aNrRows, tab);
	deleteColumn    ("Col-7", tab);
       	addColumn       (TpString, tab);
	// remove all remaining rows to check freebucket performance
        Vector<uInt> aNewNrRows(15);
	for (uInt i=0; i< 15; i++) {
	  aNewNrRows(i) = i;
	}
	deleteRows      (aNewNrRows, tab);


    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}

void initArrays (Cube<Float>& arrf, Vector<DComplex>& arrdc, Cube<Bool>& arrb)
{
      // The static_cast is a workaround for an SGI compiler bug
    indgen (static_cast< Cube<Float> &>(arrf));
    arrdc(0) = DComplex(1.2, 3.4);
    arrdc(1) = DComplex(-2.3, 5.6);
    IPosition shape(arrb.shape());
    uInt n = 0;
    for (Int i=0; i<shape(2); i++) {
	for (Int j=0; j<shape(1); j++) {
	    for (Int k=0; k<shape(0); k++) {
		if (n++%3 == 2) {
		    arrb(k,j,i) = True;
		}else{
		    arrb(k,j,i) = False;
		}
	    }
	}
    }
}

void info (const Table& aTable)
{
  cout << "TableType = " << aTable.tableType() << endl;
  TableDesc tdesc = aTable.tableDesc();
  for (uInt i=0; i<tdesc.ncolumn(); i++) {
    const ColumnDesc& cdesc = tdesc.columnDesc(i);
    cout << cdesc.name() << ": " 
	 << cdesc.dataType() << endl;
    switch (cdesc.dataType()) {
    case TpInt:
      if (cdesc.isArray()) {
	ArrayColumn<Int> ad(aTable,cdesc.name());
	cout << ad.getColumn() << endl;
      } else {
	ScalarColumn<Int> aa(aTable,cdesc.name());
	cout << aa.getColumn() << endl;
      }
      break;
    case TpBool:
      if (cdesc.isArray()) {
	ArrayColumn<Bool> ad(aTable,cdesc.name());
	cout << ad.getColumn() << endl;
      } else {
	ScalarColumn<Bool> ab(aTable,cdesc.name());
	cout << ab.getColumn() << endl;
      }
      break;
    case TpDComplex:
      if (cdesc.isArray()) {
	ArrayColumn<DComplex> ad(aTable,cdesc.name());
	cout << ad.getColumn() << endl;
      } else {
	ScalarColumn<DComplex> ac(aTable,cdesc.name());
	cout << ac.getColumn() << endl;
      }
      break;
    case TpFloat:
      if (cdesc.isArray()) {
	ArrayColumn<float> ad(aTable,cdesc.name());
	cout << ad.getColumn() << endl;
      } else {
	ScalarColumn<float> ad(aTable,cdesc.name());
	cout << ad.getColumn() << endl;
      }
      break;
    case TpDouble:
      if (cdesc.isArray()) {
	ArrayColumn<double> ad(aTable,cdesc.name());
	cout << ad.getColumn() << endl;
      } else {
	ScalarColumn<double> ad(aTable,cdesc.name());
	cout << ad.getColumn() << endl;
      }
      break;
    case TpString:
      if (cdesc.isArray()) {
	ArrayColumn<String> ad(aTable,cdesc.name());
	cout << ad.getColumn() << endl;
      } else {
	ScalarColumn<String> ad(aTable,cdesc.name());
	cout << ad.getColumn() << endl;
      }
      break;
    default:
      cout << "Sorry, datatype not implemented yet." << endl;
    }
  }
  const TableRecord& rec = aTable.keywordSet();
  for (uInt i=0; i<rec.nfields(); i++) {
    cout << rec.name(i) << '=';
    switch (rec.type(i)) {
    case TpTable:
      cout << "Record" << endl;
      info (rec.asTable(i));
      break;
    case TpBool:
      cout << rec.asBool(i) << endl;
      break;
    case TpString:
      cout << rec.asString(i) << endl;
      break;
    case TpDComplex:
    case TpComplex:
      {
	DComplex val;
	rec.get (i, val);
	cout << val << endl;
      }
      break;
    default:
      {
	Double val;
	rec.get (i, val);
	cout << val << endl;
      }
    }
  }
}

// First build a description.
void init (uInt aMode, Table& aTable)
{
  if (aMode == 0) {
    // Build the table description.
    TableDesc td("", "1", TableDesc::Scratch);
    td.comment() = "A test of class TableDesc";
    td.addColumn (ScalarColumnDesc<DComplex>("Col-1"));
    td.addColumn (ScalarColumnDesc<Int>("Col-2"));
    td.addColumn (ScalarColumnDesc<Bool>("Col-3"));
    td.addColumn (ScalarColumnDesc<Int>("Colvirt"));
    
    // Now create a new table from the description.
    SetupNewTable aNewTab("tmtest", td, Table::New);
    VirtualTaQLColumn engine("2*rownumber()");
    aNewTab.bindColumn("Colvirt", engine);
    aTable = Table (aNewTab, Table::Memory, 10);
    AlwaysAssertExit (aTable.isColumnStored ("Col-1"));
    AlwaysAssertExit (aTable.isColumnStored ("Col-2"));
    AlwaysAssertExit (aTable.isColumnStored ("Col-3"));
    AlwaysAssertExit (!aTable.isColumnStored ("Colvirt"));
  }
  
  ScalarColumn<DComplex> aa(aTable,"Col-1");
  ScalarColumn<Int>      ab(aTable,"Col-2");
  ScalarColumn<Bool>     ac(aTable,"Col-3");
  
  // fill columns with data
  uInt i;
  uInt j = 0;
  for (i=0; i<10; i++) {
    if (aMode == 1) {
      aTable.addRow();
      j=10;
    }
    DComplex a(i+j,(i+j)*2);
    aa.put(i+j,a);
    ab.put(i+j,i+j);
    Bool b;
    if ((i+j)%2 == 0) {
      b=True;
    } else {
      b=False;
    }
    ac.put(i+j,b);
  }
  
  if (aMode == 0) {
    cout << "after creation" << endl;
  } else {
    cout << "after reopening and adding 10 rows" << endl;
  }
  
  // Print column data
  info(aTable);
}

void deleteRow(const uInt aRow, Table& aTable)
{
  ScalarColumn<DComplex> aa(aTable,"Col-1");

  // Make sure ColumnCache is filled.
  aa(0);

  aTable.removeRow(aRow);
  
  cout << "after removing row: " << aRow << endl;

  // Print column data
  info(aTable);
}

void deleteRows(const Vector<uInt>& aNrRows, Table& aTable)
{
  ScalarColumn<DComplex> ae(aTable,"Col-5");

  // Make sure ColumnCache is filled.
  ae(0);

  aTable.removeRow(aNrRows);
  
  cout << "after removing several rows at once: " << endl;

  // Print column data
  info(aTable);
}

void deleteColumn(const String aColumn, Table& aTable)
{
  cout << "Try to remove Column:" << aColumn << endl;
  
  aTable.removeColumn(aColumn);

  cout << "After removing Column: " << aColumn << endl;

  // Print column data
  info(aTable);
}

void deleteAndRestore (Table& aTable)
{
  ScalarColumn<DComplex> ac(aTable,"Col-1");
  cout << "Try to remove Column 1 after saving the contents" << endl;
  Vector<DComplex> save = ac.getColumn();
  aTable.removeColumn("Col-1");
  
  cout << "After removing Column 1" << endl;
  
  // Print column data
  info(aTable);
  
  cout << "Try to restore Column 1" << endl;
  aTable.addColumn(ScalarColumnDesc<DComplex>("Col-1"));
  
  if (aTable.tableDesc().isColumn("Col-1")) {
    ac.attach(aTable,"Col-1");
  }
  
  // refill again
  ac.putColumn(save);

  // Print column data
  cout << "After restoring Column1:" << endl;
  info(aTable);
}


void addColumn(DataType aDataType, Table& aTable)
{
  ScalarColumn<Bool>      ad;
  ScalarColumn<DComplex>  ae;
  ScalarColumn<String>    aj;
  
  switch (aDataType) {
    
  case TpBool:
    cout << "Try to add Column: Col-4 and fill it. "<< endl <<
      "It Should be using space just freed up"  << endl;
    
    aTable.addColumn(ScalarColumnDesc<Bool>("Col-4"));
    
    if (aTable.tableDesc().isColumn("Col-4")) {
      ad.attach(aTable,"Col-4");
    }
    
    // fill new column with data
    uInt i;
    Bool b;
    for (i=0; i<aTable.nrow(); i++) {
      if (i < 10) {
	b=True;
      } else {
	b=False;
      }
      ad.put(i,b);
    }
    break;
  case TpDComplex:
    cout << "Try to add Column: Col-5 and fill it. "<< endl <<
      "It should make a new index because there's not enough free space" << endl;
    
    aTable.addColumn(ScalarColumnDesc<DComplex>("Col-5"));
    
    if (aTable.tableDesc().isColumn("Col-5")) {
      ae.attach(aTable,"Col-5");
    }
    
    // fill new column with data
    for (uInt i=0; i<aTable.nrow(); i++) {
      DComplex a(aTable.nrow()-i,i);
      ae.put(i,a);
    }
    break;
  case TpString:
    {
      cout << "Try to add a string Column: Col-10 and fill it. "<< endl;
      
      aTable.addColumn(ScalarColumnDesc<String>("Col-10"));
      
      if (aTable.tableDesc().isColumn("Col-10")) {
	aj.attach(aTable,"Col-10");
      }
      
      String aString("String-1");
      
      // fill new column with data
      for (uInt i=0; i<aTable.nrow(); i++) {
	aj.put(i,aString);
	aString += " "+String::toString(i);
      }
    }
    break;
  default:
    cout << "Not implemented yet" << endl;
    break;
  }
  
  info(aTable);
}

void addDirectArrays (Table& aTable)
{
  ArrayColumn<float>      af;
  ArrayColumn<DComplex>   ag;
  ArrayColumn<Bool>       ah;
  

  cout << "Trying to add a few  Direct Array Columns." << endl;

  aTable.addColumn(ArrayColumnDesc<float>("Col-6", IPosition(3,2,3,1),
					  ColumnDesc::Direct));
    
  aTable.addColumn(ArrayColumnDesc<DComplex>("Col-7", IPosition(1,2),
					     ColumnDesc::Direct));
    
  aTable.addColumn(ArrayColumnDesc<Bool>("Col-8", IPosition(3,5,7,1),
					 ColumnDesc::Direct));
    

  Cube<float> arrf(IPosition(3,2,3,1));
  Vector<DComplex> arrdc(2);
  Cube<Bool> arrb(IPosition(3,5,7,1));
  initArrays (arrf, arrdc, arrb);

  if (aTable.tableDesc().isColumn("Col-6")) {
    af.attach(aTable,"Col-6");
  }
  if (aTable.tableDesc().isColumn("Col-7")) {
    ag.attach(aTable,"Col-7");
  }
  if (aTable.tableDesc().isColumn("Col-8")) {
    ah.attach(aTable,"Col-8");
  }

  for (uInt i=0; i<aTable.nrow(); i++) {
    af.put(i,arrf);
    ag.put(i,arrdc);
    ah.put(i,arrb);
    arrf += (float)(arrf.nelements());
    arrdc += DComplex(2, 3);
  }

  info(aTable);
}

void addIndStringArray (Table& aTable)
{
  ArrayColumn<String>       ai;

  cout << "Trying to add an  indirect String Array Column." << endl;

  aTable.addColumn(ArrayColumnDesc<String>("Col-9"));
    
  Vector<String> arrs(5);
  arrs(0)="Start-1";
  arrs(1)="Start-2";
  arrs(2)="Start-3";
  arrs(3)="Start-4";
  arrs(4)="Start-5";

  if (aTable.tableDesc().isColumn("Col-9")) {
    ai.attach(aTable,"Col-9");
  }

  for (uInt i=0; i<aTable.nrow(); i++) {
    ai.put(i,arrs);
    for (uInt j=0; j< 5;j++) {
      arrs(j) += " "+String::toString(i);
    }
  }

  info(aTable);
}

void addIndArray (Table& aTable)
{
  ArrayColumn<Int>       ak;

  cout << "Trying to add an  indirect Array Column." << endl;

  aTable.addColumn(ArrayColumnDesc<Int>("Col-11"));
    
  Vector<Int> arrs(5);
  arrs(0)=1;
  arrs(1)=2;
  arrs(2)=3;
  arrs(3)=4;
  arrs(4)=5;

  if (aTable.tableDesc().isColumn("Col-11")) {
    ak.attach(aTable,"Col-11");
  }

  for (uInt i=0; i<aTable.nrow(); i++) {
    ak.put(i,arrs);
    for (uInt j=0; j< 5;j++) {
      arrs(j) += i;
    }
  }

  info(aTable);
}

void addKeys(Table& tab)
{
  tab.rwKeywordSet().define ("Key1", "abc");
  // Create a subtable.
  TableDesc td("", "1", TableDesc::Scratch);
  td.addColumn (ScalarColumnDesc<String> ("str"));
  SetupNewTable aNewTab("tmtest", td, Table::New);
  Table aTable (aNewTab, Table::Memory, 2);
  ScalarColumn<String> sc(aTable, "str");
  sc.put (0, "str1");
  sc.put (0, "s1");
  sc.put (1, "sstr2");
  tab.rwKeywordSet().defineTable ("Subtab", aTable);
}

void putColumnTest (Table& aTable)
{
  ScalarColumn<Int> ab(aTable,"Col-2");

  // put value 3 in rownr 5
  ab.put(5,3);
  AlwaysAssertExit(ab(5) == 3);

  // put value 4 in column
  ab.putColumn(ab.getColumn() + 1);
  AlwaysAssertExit (ab(5) == 4);
}

void copyTable (const Table& aTable)
{
  cout << "Try to rename and copy the table" << endl;
  Table tab(aTable);
  cout << "old name = " << Path(tab.tableName()).baseName();
  tab.rename ("mt_newname", Table::Scratch);
  AlwaysAssertExit (!tab.isColumnStored ("Colvirt"));
  cout << "     new name = " << Path(tab.tableName()).baseName() << endl;
  aTable.copy ("tMemoryTable_tmp.tabcp", Table::New);
  Table tabc("tMemoryTable_tmp.tabcp");
  cout << "copy name = " << Path(tabc.tableName()).baseName() << endl;
  AlwaysAssertExit (!tabc.isColumnStored ("Colvirt"));
  info(tabc);
}

void copyRefTable (const Table& aTable)
{
  cout << "Try to copy a referenced memory table" << endl;
  Table tab (aTable(aTable.col("Colvirt") < 10));
  AlwaysAssertExit (!tab.isColumnStored ("Colvirt"));
  tab.deepCopy ("tMemoryTable_tmp.tabcpr", Table::New);
  Table tabc("tMemoryTable_tmp.tabcpr");
  cout << "copy name = " << Path(tabc.tableName()).baseName() << endl;
  AlwaysAssertExit (!tabc.isColumnStored ("Colvirt"));
  info(tabc);
  cout << tab.dataManagerInfo() << endl;
  cout << tabc.dataManagerInfo() << endl;
}

void copyMemoryTable (const Table& aTable)
{
  cout << "Try to copy the table to a MemoryTable" << endl;
  Table tab(aTable);
  AlwaysAssertExit (!tab.isColumnStored ("Colvirt"));
  cout << "name = " << Path(tab.tableName()).baseName() << endl;
  Table tabc = tab.copyToMemoryTable ("tMemoryTable.dat");
  cout << "copy name = " << Path(tabc.tableName()).baseName() << endl;
  AlwaysAssertExit (!tabc.isColumnStored ("Colvirt"));
  info(tabc);
}

void copyMemoryTableSubSet (const Table& aTable)
{
  cout << "Try to copy a table subset to a MemoryTable" << endl;
  Table tab = aTable(aTable.col("Col-3"));
  Table tabc = tab.copyToMemoryTable ("tMemoryTable.subdat");
  cout << "copy name = " << Path(tabc.tableName()).baseName() << endl;
  info(tabc);
}
