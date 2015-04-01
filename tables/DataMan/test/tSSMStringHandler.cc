//# tSSMStringHandler.cc: Test program for the StringHandler part of the
//#                       StandardStMan storage manager
//# Copyright (C) 2000,2001,2003
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

#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/DataMan/StandardStMan.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/DataMan/StandardStManAccessor.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/sstream.h>


#include <casacore/casa/namespace.h>
// <summary>
// Test program for the SSMStringHandler part of the 
// StandardStMan storage manager
// </summary>

// This program tests the StandardStMan storage manager, especially the
// SSMStringHandler part.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.


// open and write a few rows
// aMode == 0  open new
void init (uInt aBucketSize,uInt aMode);

// reopen table, and throw away a row
void deleteRow(const uInt aRow);

// reopen table, and throw away a few rows
void deleteRows(const Vector<uInt>& aNrRows);

// delete a column
void deleteColumn(const String aColumn);

// add a direct ArrayColumn
void addDirArrayColumn();

// add an indirect array
void addIndArrayColumn();

// add small column (<=8)
void addSmallColumn();

// create empty column (only setShape, no puts)
void addEmptyColumn();

// replace a few string with same or smaller length
void replaceStrings();

// show table info
void info(const Table aTable);

int main (int argc, const char* argv[])
{
    uInt aNr = 500;
    if (argc > 1) {
	istringstream anIstr(argv[1]);
	anIstr >> aNr;
    }
    try {
	init               (aNr,0);
	init               (aNr,1);
	addDirArrayColumn  ();
	addIndArrayColumn  ();
	replaceStrings     ();
	deleteRow          (2);
	deleteRow          (40);
        Vector<uInt> aNrRows(35);
	for (uInt i=0; i< 35; i++) {
	  aNrRows(i) = i+3;
	}
	deleteRows         (aNrRows);
	deleteColumn       ("Col-2");
	addSmallColumn     ();
	deleteColumn       ("Col-1");
	addEmptyColumn     ();
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}

void info( const Table aTable)
{
  for (uInt i=0; i< aTable.tableDesc().ncolumn(); i++) {
    cout << aTable.tableDesc().columnNames()(i) << ": " 
	 << aTable.tableDesc().columnDesc(i).dataType() << endl;
    if (aTable.tableDesc().columnDesc(i).dataType() == TpString) {
      String aColName=aTable.tableDesc().columnNames()(i);
      if (aColName == "Col-1") {
	ScalarColumn<String> ad(aTable,aColName);
	cout << ad.getColumn() << endl;
      } else if (aColName == "Col-2" || aColName == "Col-3" ||
		 aColName == "Col-4" ||	aColName == "Col-5") {
	ArrayColumn<String> ab(aTable,aColName);
	cout << ab.getColumn() << endl;
      } else {
	cout << "Sorry, String not implemented yet for this case." << endl;
      }
    } else {
      cout << "Sorry, datatype not implemented for this test yet." << endl;
    }
  } 
}

// First build a description.
void init (uInt aBucketSize, uInt aMode)
{
  Table aTable;
  if (aMode == 0) {
    DataManager::registerCtor ("StandardStMan",
			       StandardStMan::makeObject);
    // Build the table description.
    TableDesc td("", "1", TableDesc::Scratch);
    td.comment() = "A test of StringHandler";
    td.addColumn (ScalarColumnDesc<String>("Col-1"));
    
    // Now create a new table from the description.
    SetupNewTable aNewTab("tSSMStringHandler_tmp.data", td, Table::New);
    // Create a storage manager for it.
    StandardStMan aSm1 ("SSM", aBucketSize);
    aNewTab.bindAll (aSm1);
    aTable = Table (aNewTab, 10);
  } else {
    aTable = Table("tSSMStringHandler_tmp.data", Table::Update);
  }
  
  ScalarColumn<String> aa(aTable,"Col-1");
  uInt start=0;
  String aString("String-1");
    
  if (aMode ==  1){
    aString="String-2";
    aTable.addRow(40);
    start=10;
  }
  
  // fill new column with data
  for (uInt i=start; i<aTable.nrow(); i++) {
    aa.put(i,aString);
    aString += " "+String::toString(i);
  }

  if (aMode == 0) {
    cout << "after creation" << endl;
  } else {
    cout << "after reopening and adding 40 rows" << endl;
  }

  ROStandardStManAccessor anA(aTable,"SSM");
  anA.showBaseStatistics(cout);
  anA.showIndexStatistics(cout);
  cout << endl << endl;
  
  // Print column data
  info(aTable);
}

void addDirArrayColumn()
{
  Table aTable = Table("tSSMStringHandler_tmp.data", Table::Update);
  ArrayColumn<String>      ab;

  cout << "Trying to add a Direct Array Column of String." << endl;

  aTable.addColumn(ArrayColumnDesc<String>("Col-2", IPosition(1,5),
					  ColumnDesc::Direct));
    
  if (aTable.tableDesc().isColumn("Col-2")) {
    ab.attach(aTable,"Col-2");
  }

  Vector<String> arrs(5);
  arrs(0)="Array-1";
  arrs(1)="Array-2";
  arrs(2)="Array-3";
  arrs(3)="Array-4";
  arrs(4)="Array-5";

  for (uInt i=0; i<aTable.nrow(); i++) {
    ab.put(i,arrs);
    for (uInt j=0; j< 5;j++) {
      arrs(j) += "-"+String::toString(i);
    }
  }

  ROStandardStManAccessor anA(aTable,"SSM");
  anA.showBaseStatistics(cout);
  anA.showIndexStatistics(cout);
  cout << endl << endl;

  info(aTable);
}

void addIndArrayColumn()
{
  Table aTable = Table("tSSMStringHandler_tmp.data", Table::Update);
  ArrayColumn<String>      ac;

  cout << "Trying to add an Indirect Array Column of String." << endl;

  aTable.addColumn(ArrayColumnDesc<String>("Col-3"));
    
  if (aTable.tableDesc().isColumn("Col-3")) {
    ac.attach(aTable,"Col-3");
  }

  Vector<String> arrs(5);
  arrs(0)="IndArr1";
  arrs(1)="IndArr2";
  arrs(2)="IndArr3";
  arrs(3)="IndArr4";
  arrs(4)="IndArr5";

  for (uInt i=0; i<aTable.nrow(); i++) {
    ac.put(i,arrs);
    for (uInt j=0; j< 5;j++) {
      arrs(j) += "-"+String::toString(i);
    }
  }

  ROStandardStManAccessor anA(aTable,"SSM");
  anA.showBaseStatistics(cout);
  anA.showIndexStatistics(cout);
  cout << endl << endl;

  info(aTable);
}

void addSmallColumn()
{
  Table aTable = Table("tSSMStringHandler_tmp.data", Table::Update);
  ArrayColumn<String>      ae;

  cout << "Trying to add a small fixed shape Column of String." << endl;
  
  aTable.addColumn(ArrayColumnDesc<String>("Col-4", IPosition(1,5),
		   ColumnDesc::FixedShape));

  if (aTable.tableDesc().isColumn("Col-4")) {
    ae.attach(aTable,"Col-4");
  }
  
  String aS("SFS");
 
  Vector<String> arrs(5);
  arrs(0)="SFS1";
  arrs(1)="SFS2";
  arrs(2)="SFS3";
  arrs(3)="SFS4";
  arrs(4)="SFS5";

  for (uInt i=0; i<aTable.nrow(); i++) {
    ae.put(i,arrs);
    for (uInt j=0; j< 5;j++) {
      arrs(j) = aS + String::toString(j) + "-"+String::toString(i);
    }
  }

  ROStandardStManAccessor anA(aTable,"SSM");
  anA.showBaseStatistics(cout);
  anA.showIndexStatistics(cout);
  cout << endl << endl;

  info(aTable);
}

void addEmptyColumn()
{
  Table aTable = Table("tSSMStringHandler_tmp.data", Table::Update);
  ArrayColumn<String>      af;

  cout << "Trying to add a Column without filling it." << endl;
  
  aTable.addColumn(ArrayColumnDesc<String>("Col-5"));

  if (aTable.tableDesc().isColumn("Col-5")) {
    af.attach(aTable,"Col-5");
  }
  

  for (uInt i=0; i<aTable.nrow(); i++) {
    af.setShape(i,IPosition(2,3,2));
  }

  // test replaceshape function.
  for (uInt i=0; i<aTable.nrow(); i++) {
    af.setShape(i,IPosition(2,2,2));
  }

  ROStandardStManAccessor anA(aTable,"SSM");
  anA.showBaseStatistics(cout);
  anA.showIndexStatistics(cout);
  cout << endl << endl;

  info(aTable);
}

void deleteRow(const uInt aRow)
{
  Table aTable = Table("tSSMStringHandler_tmp.data", Table::Update);

  aTable.removeRow(aRow);
  
  cout << "after removing row: " << aRow << endl;

  ROStandardStManAccessor anA(aTable,"SSM");
  anA.showBaseStatistics(cout);
  anA.showIndexStatistics(cout);
  cout << endl << endl;

  info(aTable);
}

void deleteRows(const Vector<uInt>& aNrRows)
{
  Table aTable = Table("tSSMStringHandler_tmp.data", Table::Update);

  cout << "Try to remove a few rows at the same time." << endl;
  
  aTable.removeRow(aNrRows);
  
  cout << "after removing several rows at once: " << endl;
  ROStandardStManAccessor anA(aTable,"SSM");
  anA.showBaseStatistics(cout);
  anA.showIndexStatistics(cout);
  cout << endl << endl;

  // Print column data
  info(aTable);
}

void deleteColumn(const String aColumn)
{
  Table aTable = Table("tSSMStringHandler_tmp.data", Table::Update);

  cout << "Try to remove Column:" << aColumn << endl;
  
  aTable.removeColumn(aColumn);

  cout << "After removing Column: " << aColumn << endl;
  ROStandardStManAccessor anA(aTable,"SSM");
  anA.showBaseStatistics(cout);
  anA.showIndexStatistics(cout);
  cout << endl << endl;

  // Print column data
  info(aTable);
}

void replaceStrings()
{
  Table aTable = Table("tSSMStringHandler_tmp.data", Table::Update);

  ScalarColumn<String> aa(aTable,"Col-1");
  ArrayColumn<String>  ab(aTable,"Col-2");
  ArrayColumn<String>  ac(aTable,"Col-3");

  cout << "Try to change some datain Column 1" << endl;
  String aString="Much Bigger I Believe";

  for (uInt i=0; i<aTable.nrow(); i++) {
    if (i == 25) {
      aString="Small";
    }
    aa.put(i,aString);
    aString += " "+String::toString(aTable.nrow()-i);
  }

  cout << "Try to change some datain Column 2" << endl;

  Vector<String> arrd(5);
  arrd(0)="A bigger Direct Array";
  arrd(1)="A bigger Direct Array";
  arrd(2)="A bigger Direct Array";
  arrd(3)="A bigger Direct Array";
  arrd(4)="A bigger Direct Array";

  for (uInt i=0; i<5; i++) {
    ab.put(i,arrd);
    for (uInt j=0; j< 5;j++) {
      arrd(j) += "-"+String::toString(i);
    }
  }

  arrd(0)="Small";
  arrd(1)="Small";
  arrd(2)="Small";
  arrd(3)="Small";
  arrd(4)="Small";

  for (uInt i=6; i<15; i++) {
    ab.put(i,arrd);
    for (uInt j=0; j< 5;j++) {
      arrd(j) += "-"+String::toString(i);
    }
  }

  cout << "Try to change some datain Column 3" << endl;

  Vector<String> arri(5);
  arri(0)="A bigger Indirect Array";
  arri(1)="A bigger Indirect Array";
  arri(2)="A bigger Indirect Array";
  arri(3)="A bigger Indirect Array";
  arri(4)="A bigger Indirect Array";

  for (uInt i=0; i<5; i++) {
    ac.put(i,arri);
    for (uInt j=0; j< 5;j++) {
      arri(j) += "-"+String::toString(i);
    }
  }

  arri(0)="Small";
  arri(1)="Small";
  arri(2)="Small";
  arri(3)="Small";
  arri(4)="Small";

  for (uInt i=6; i<15; i++) {
    ac.put(i,arri);
    for (uInt j=0; j< 5;j++) {
      arri(j) += "-"+String::toString(i);
    }
  }



  cout << "After Changing the data" << endl;
  ROStandardStManAccessor anA(aTable,"SSM");
  anA.showBaseStatistics(cout);
  anA.showIndexStatistics(cout);
  cout << endl << endl;

  // Print column data
  info(aTable);
}

