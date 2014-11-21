//# tMemoryStMan.cc: Test program for the MemoryStMan storage manager
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
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/DataMan/MemoryStMan.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test program for the MemoryStMan storage manager
// </summary>

// This program tests the MemoryStMan storage manager, especially the
// get and put functions.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.


// Have a global area to keep the data to be inserted when
// the table is reopened.
Record theData;


// (re) open and write a few rows
// aMode == 0  open new
// aMode == 1  reopen existing
void init (uInt aMode);

// reopen table, and throw away a row
void deleteRow(const uInt aRow);

// reopen table, and throw away a few rows
void deleteRows(const Vector<uInt>& aNrRows);

// delete a Column
void deleteColumn(const String aColumn);

// delete a column && put it back again
void deleteAndRestore();

// add aColumn
void addColumn(DataType aDataType);

//add a few direct arrays
void addDirectArrays();

//add an indirect string array
void addIndStringArray();

//add an indirect array
void addIndArray();

// show table info
void info(const Table aTable);

void saveData(const Table aTable);

void restoreData(const Table aTable);

// put/putColumn cache test
void putColumnTest();


int main ()
{
    try {
	init            (0);
        init            (1);
	deleteRow       (0);
	deleteRow       (17);
	// delete and restore Column 1 (should use perfect fit)
	deleteAndRestore();
	// 
	putColumnTest();
	// delete middle Column
       	deleteColumn    ("Col-2");
	// add a Bool Column Should fit in freed space
	addColumn       (TpBool);
	// add a DComplex Column Should use new index && space
	addColumn       (TpDComplex);
	// delete first Column
	deleteColumn    ("Col-1");
	// delete last Column
	deleteColumn    ("Col-3");
	addDirectArrays ();
	addIndStringArray();
	addIndArray     ();
        Vector<uInt> aNrRows(3);
	for (uInt i=0; i< 3; i++) {
	  aNrRows(i) = i+3;
	}
	deleteRows      (aNrRows);
	deleteColumn    ("Col-7");
       	addColumn(TpString);
	// remove all remaining rows to check freebucket performance
        Vector<uInt> aNewNrRows(15);
	for (uInt i=0; i< 15; i++) {
	  aNewNrRows(i) = i;
	}
	deleteRows      (aNewNrRows);


    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}

void initArrays(Cube<Float>& arrf, Vector<DComplex>& arrdc, Cube<Bool>& arrb)
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

void info( const Table aTable)
{
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
}

void saveData( const Table aTable)
{
  theData = Record();
  TableDesc tdesc = aTable.tableDesc();
  for (uInt i=0; i<tdesc.ncolumn(); i++) {
    const ColumnDesc& cdesc = tdesc.columnDesc(i);
    if (cdesc.dataType() == TpInt) {
      if (cdesc.isArray()) {
	ArrayColumn<Int> col(aTable, cdesc.name());
	theData.define (cdesc.name(), col.getColumn());
      } else {
	ScalarColumn<Int> col(aTable, cdesc.name());
	theData.define (cdesc.name(), col.getColumn());
      }
    } else if (cdesc.dataType() == TpBool) {
      if (cdesc.isArray()) {
	ArrayColumn<Bool> col(aTable, cdesc.name());
	theData.define (cdesc.name(), col.getColumn());
      } else {
	ScalarColumn<Bool> col(aTable, cdesc.name());
	theData.define (cdesc.name(), col.getColumn());
      }
    } else if (cdesc.dataType() == TpDComplex) {
      if (cdesc.isArray()) {
	ArrayColumn<DComplex> col(aTable, cdesc.name());
	theData.define (cdesc.name(), col.getColumn());
      } else {
	ScalarColumn<DComplex> col(aTable, cdesc.name());
	theData.define (cdesc.name(), col.getColumn());
    }
    } else if (cdesc.dataType() == TpFloat) {
      if (cdesc.isArray()) {
	ArrayColumn<Float> col(aTable, cdesc.name());
	theData.define (cdesc.name(), col.getColumn());
      } else {
	ScalarColumn<Float> col(aTable, cdesc.name());
	theData.define (cdesc.name(), col.getColumn());
    }
    } else if (cdesc.dataType() == TpString) {
      if (cdesc.isArray()) {
	ArrayColumn<String> col(aTable, cdesc.name());
	theData.define (cdesc.name(), col.getColumn());
      } else {
	ScalarColumn<String> col(aTable, cdesc.name());
	theData.define (cdesc.name(), col.getColumn());
      }
    } else {
      cout << "Sorry, datatype not implemented yet." << endl;
    }
  } 
}

void restoreData(const Table aTable)
{
  for (uInt i=0; i<theData.nfields(); i++) {
    String name = theData.name(i);
    switch (theData.dataType(i)) {
    case TpArrayInt:
    {
      const Array<Int>& arr = theData.asArrayInt(i);
      if (arr.ndim() > 1) {
	ArrayColumn<Int> col(aTable, name);
	col.putColumn (arr);
      } else {
	ScalarColumn<Int> col(aTable, name);
	col.putColumn (arr);
      }
      break;
    }
    case TpArrayBool:
    {
      const Array<Bool>& arr = theData.asArrayBool(i);
      if (arr.ndim() > 1) {
	ArrayColumn<Bool> col(aTable, name);
	col.putColumn (arr);
      } else {
	ScalarColumn<Bool> col(aTable, name);
	col.putColumn (arr);
      }
      break;
    }
    case TpArrayDComplex:
    {
      const Array<DComplex>& arr = theData.asArrayDComplex(i);
      if (arr.ndim() > 1) {
	ArrayColumn<DComplex> col(aTable, name);
	col.putColumn (arr);
      } else {
	ScalarColumn<DComplex> col(aTable, name);
	col.putColumn (arr);
      }
      break;
    }
    case TpArrayFloat:
    {
      const Array<Float>& arr = theData.asArrayFloat(i);
      if (arr.ndim() > 1) {
	ArrayColumn<Float> col(aTable, name);
	col.putColumn (arr);
      } else {
	ScalarColumn<Float> col(aTable, name);
	col.putColumn (arr);
      }
      break;
    }
    case TpArrayString:
    {
      const Array<String>& arr = theData.asArrayString(i);
      if (arr.ndim() > 1) {
	ArrayColumn<String> col(aTable, name);
	col.putColumn (arr);
      } else {
	ScalarColumn<String> col(aTable, name);
	col.putColumn (arr);
      }
      break;
    }
    default:
      cout << "Sorry, datatype not implemented yet." << endl;
    }
  } 
}

// First build a description.
void init (uInt aMode)
{
  Table aTable;
  if (aMode == 0) {
    DataManager::registerCtor ("MemoryStMan",
			       MemoryStMan::makeObject);
    // Build the table description.
    TableDesc td("", "1", TableDesc::Scratch);
    td.comment() = "A test of class TableDesc";
    td.addColumn (ScalarColumnDesc<DComplex>("Col-1"));
    td.addColumn (ScalarColumnDesc<Int>("Col-2"));
    td.addColumn (ScalarColumnDesc<Bool>("Col-3"));
    
    // Now create a new table from the description.
    SetupNewTable aNewTab("tMemoryStMan_tmp.data", td, Table::New);
    // Create a storage manager for it.
    MemoryStMan aSm1 ("MSM");
    aNewTab.bindAll (aSm1);
    aTable = Table (aNewTab, 10);
  } else {
    aTable = Table("tMemoryStMan_tmp.data", Table::Update);
    restoreData(aTable);
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
  saveData(aTable);
}

void deleteRow(const uInt aRow)
{
  Table aTable = Table("tMemoryStMan_tmp.data", Table::Update);
  restoreData(aTable);

  ScalarColumn<DComplex> aa(aTable,"Col-1");

  // Make sure ColumnCache is filled.
  aa(0);

  aTable.removeRow(aRow);
  
  cout << "after removing row: " << aRow << endl;

  // Print column data
  info(aTable);
  saveData(aTable);
}

void deleteRows(const Vector<uInt>& aNrRows)
{
  Table aTable = Table("tMemoryStMan_tmp.data", Table::Update);
  restoreData(aTable);

  ScalarColumn<DComplex> ae(aTable,"Col-5");

  // Make sure ColumnCache is filled.
  ae(0);

  aTable.removeRow(aNrRows);
  
  cout << "after removing several rows at once: " << endl;

  // Print column data
  info(aTable);
  saveData(aTable);
}

void deleteColumn(const String aColumn)
{
  Table aTable = Table("tMemoryStMan_tmp.data", Table::Update);
  restoreData(aTable);

  cout << "Try to remove Column:" << aColumn << endl;
  
  aTable.removeColumn(aColumn);

  cout << "After removing Column: " << aColumn << endl;

  // Print column data
  info(aTable);
  saveData(aTable);
}

void deleteAndRestore()
{
  Table aTable = Table("tMemoryStMan_tmp.data", Table::Update);
  restoreData(aTable);
  
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
  saveData(aTable);
}


void addColumn(DataType aDataType)
{
  Table aTable = Table("tMemoryStMan_tmp.data", Table::Update);
  restoreData(aTable);

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
  saveData(aTable);
}

void addDirectArrays()
{
  Table aTable = Table("tMemoryStMan_tmp.data", Table::Update);
  restoreData(aTable);

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
  saveData(aTable);
}

void addIndStringArray()
{
  Table aTable = Table("tMemoryStMan_tmp.data", Table::Update);
  restoreData(aTable);

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
  saveData(aTable);
}

void addIndArray()
{
  Table aTable = Table("tMemoryStMan_tmp.data", Table::Update);
  restoreData(aTable);

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
  saveData(aTable);
}

void putColumnTest()
{
  Table aTable = Table("tMemoryStMan_tmp.data", Table::Update);
  restoreData(aTable);

  ScalarColumn<Int> ab(aTable,"Col-2");

  // put value 3 in rownr 5
  ab.put(5,3);
  AlwaysAssertExit(ab(5) == 3);

  // put value 4 in column
  ab.putColumn(ab.getColumn() + 1);
  AlwaysAssertExit (ab(5) == 4);
  saveData(aTable);
}
















