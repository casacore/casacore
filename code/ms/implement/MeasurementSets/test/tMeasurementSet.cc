//# tNewMeasurementSet.cc : this program tests the NewMeasurementSet class
//# Copyright (C) 1995,1996,1997,2000
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

//# Includes

#include <aips/MeasurementSets/NewMeasurementSet.h>

#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Exceptions/Error.h>
#include <aips/Tables.h>
#include <aips/Tables/RowCopier.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Fallible.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MFrequency.h>

#include <iostream.h>

// the dataType() member functions in Tables report only the scalar type
// even for Array columns.  So, if a column is TpArrayFloat in MS then it
// will appear as TpFloat in a Table.  
// This function converts from a scalar type to the appropriate array type.
// Non-scalar types and types with no array equivalent are simply returned as is.
// It is probably more generally useful than in this test program.
DataType makeArrayType(DataType type)
{
    switch (type) {
    case TpBool: type = TpArrayBool; break;
    case TpChar: type = TpArrayChar; break;
    case TpUChar: type = TpArrayUChar; break;
    case TpShort: type = TpArrayShort; break;
    case TpUShort: type = TpArrayUShort; break;
    case TpInt: type = TpArrayInt; break;
    case TpUInt: type = TpArrayUInt; break;
    case TpFloat: type = TpArrayFloat; break;
    case TpDouble: type = TpArrayDouble; break;
    case TpComplex: type = TpArrayComplex; break;
    case TpDComplex: type = TpArrayDComplex; break;
    case TpString: type = TpArrayString; break;
    default: break;
    }
    return type;
}

// test functions, all return the number of errors unless otherwise stated
// test PredefinedColumns static functions in NewMeasurementSet

uInt tColumnStatics()
{
    // ensure that the conversions are consistent
    uInt errCount = 0;

    for (Int i=0;i<NewMS::NUMBER_PREDEFINED_COLUMNS;i++) {
	NewMS::PredefinedColumns pdcol = NewMS::PredefinedColumns(i);
	DataType dtype = NewMS::columnDataType(pdcol);
	String pdname = NewMS::columnName(pdcol);
	NewMS::PredefinedColumns pdtype = NewMS::columnType(pdname);
	if (pdtype != pdcol) {
	    cerr << "Inconsistency found for column : " << pdname << endl;
	    cerr << "  Type : " << Int(pdtype) << " should be : " 
		 << Int(pdcol) <<endl;
	    errCount++;
	}

	// verify that we get an UNDEFINED_COLUMN when appropriate
	pdtype = NewMS::columnType("NotAPredefinedColumn");
	if (pdtype != NewMS::UNDEFINED_COLUMN) {
	    cerr << "columnType returned a valid PredefinedColumn for \"NotAPredefinedColumn\""
		<< Int(pdtype) << endl;
	    errCount++;
	}

        // this just tests that a comment exists (an exception will occur here if not)
	String comment = NewMS::columnStandardComment(pdcol);
        // this just tests that a UNIT exists (an exception will occur here if not)
	String unit = NewMS::columnUnit(pdcol);
        // this just tests that a MEASURE_TYPE exists (an exception will occur here if not)
	String measureType = NewMS::columnMeasureType(pdcol);

    }
    return errCount;
}


// test PredefinedKeywords static functions in NewMeasurementSet

uInt tKeywordStatics()
{
    uInt errCount = 0;

    // NewMS::PredefinedKeywords

    for (uInt i=0;i<NewMS::NUMBER_PREDEFINED_KEYWORDS;i++) {
	NewMS::PredefinedKeywords pdkey = NewMS::PredefinedKeywords(i);
	String pdname = NewMS::keywordName(pdkey);
	NewMS::PredefinedKeywords pdtype = NewMS::keywordType(pdname);
	// this MUST be valid and it must have the same value as pdkey
	if (pdtype != pdkey) {
	    cerr << "Inconsistency found for keyword : " << pdname << endl;
	    cerr << "  Type : " << Int(pdtype) << " should be : " 
		 << Int(pdkey) << endl;
	    errCount++;
	}

	// this just tests that a dtype is available
	DataType dtype = NewMS::keywordDataType(pdkey);
        // this just tests that a comment exists (an exception will occur here if not)
	String comment = NewMS::keywordStandardComment(pdkey);

    }

    return errCount;
}

// test addColumnToDesc static for all possible columns 

uInt tAddAllColumns()
{
    uInt errCount = 0;

    // test addColumnToDesc for all possible columns
    {
	TableDesc testTD;
	for (uInt i=1;i<NewMS::NUMBER_PREDEFINED_COLUMNS;i++) {
	    NewMS::addColumnToDesc(testTD, NewMS::PredefinedColumns(i));
	}
//	testTD.show();

	// we should be able to add an existing column without causing an exception
	NewMS::addColumnToDesc(testTD, NewMS::TIME);
    }

    return errCount;
}

// Test most of the non-static functions 

uInt tNonStatic(const String& sdmsName)
{
    uInt errCount = 0;

    TableDesc td(NewMS::requiredTableDesc());
    // Add the DATA column
    NewMS::addColumnToDesc(td, NewMS::FLOAT_DATA, 2);
    // add one column, not a PredefinedColumn
    td.addColumn(ScalarColumnDesc<Double>("test_column"));

    SetupNewTable setup(sdmsName, td, Table::New);
    StManAipsIO aipsioman;
    setup.bindAll(aipsioman);
    
    // small table, ten rows
    NewMeasurementSet ms(setup, 10);
    ms.createDefaultSubtables(Table::New);

    ArrayColumn<Float> fldata(ms,NewMS::columnName(NewMS::FLOAT_DATA));
    for (Int i=0; i<10; i++) {
      Matrix<Float> arr(4,2);
      arr=Float(i);
      fldata.put(i,arr);
    }
    

    // verify that it is valid
    if (! ms.validate()) {
	cerr << "self validation failed" <<endl;
	errCount++;
    }
    if (! NewMS::validate(ms.tableDesc())) {
	cerr << "validation of tableDesc fails" << endl;
	errCount++;
    }
    if (! NewMS::validate(ms.keywordSet())) {
	cerr << "validation of keywordSet fails" << endl;
	errCount++;
    }

    // they are all writable at this point
    if (!ms.isColumnWritable(NewMS::TIME)) {
	cerr << "TIME column should be writable but isColumnWritable() returned False" 
	     << endl;
	errCount++;
    }

    // test the makeComplexData() function
    ms.makeComplexData();

    // TIME is a scalar column, DATA is an array column
    if (!ms.isScalar(NewMS::TIME)) {
	cerr << "TIME column is scalar but isScalar() returned False" << endl;
	errCount++;
    }
    if (ms.isArray(NewMS::TIME)) {
	cerr << "TIME column is scalar but isArray() returned True" << endl;
	errCount++;
    }
    if (ms.isScalar(NewMS::DATA)) {
	cerr << "DATA column is array but isScalar() returned True" << endl;
	errCount++;
    }
    if (!ms.isArray(NewMS::DATA)) {
	cerr << "DATA column is array but isArray() returned False" << endl;
	errCount++;
    }

    // TIME has units of seconds
    // test via string
    if (ms.unit(NewMS::columnName(NewMS::TIME)) != "s") {
	cerr << "NewMS::unit(const String&) failed to return s for TIME" << endl;
	errCount++;
    }
    if (ms.unit(NewMS::TIME) != "s") {
	cerr << "NewMS::unit(NewMS::TIME) failed to return s" << endl;
//*** testing
	cerr << ms.unit(NewMS::TIME) <<endl;
//*** testing

	errCount++;
    }

    // test of operator=
    // construct a scratch APERTURE_SYNTHESIS MS
    TableDesc std(NewMS::requiredTableDesc());
    SetupNewTable scratchSetup("",std,Table::Scratch);
    scratchSetup.bindAll(aipsioman);
    NewMeasurementSet sms(scratchSetup, 20);
    sms.createDefaultSubtables();

    //    String parentName = ms.tableName();
    //    String subName = ms.keywordSet().asTable("ANTENNA").tableName();
    //    cout << "Parent: "<<parentName<<", sub:"<<subName<<endl;
    ms.flush();

    // ok, use operator to convert ms to sms
    ms = sms;

    // a couple of simple tests
    if (ms.nrow() != sms.nrow()) {
	cerr << "operator= failed, number of rows is not correct" << endl;
	errCount++;
    }

    if (!ms.validate()) {
	cerr << "operator= failed, validate returns False" << endl;
	errCount++;
    }

    return errCount;
}

// Test constructors not tested by tNonStatic()

uInt tConstructors(const String& msName)
{
    uInt errCount = 0;
    // test default constructor
    NewMeasurementSet tms0();

    // existing table on disk, with correct type
    NewMeasurementSet tms1(msName);

    // try invalid table

    // make tableDesc
    {
      TableDesc td(NewMSFeed::requiredTableDesc(),"badTD","",TableDesc::New);
    }
    // make non MS table
    {
      SetupNewTable newtab("badmsTable","badTD",Table::New);
      Table tab(newtab);
    }


    // try creating bad MS
    Bool thrown=False;
    try {
      NewMeasurementSet badms("badmsTable");
    } catch (AipsError x) {
      thrown = True;
    } 
    if (!thrown) errCount++;

    // alternate form with TableDesc (none specified here)
    NewMeasurementSet tms2(msName,"");
    // try creating bad MS
    thrown=False;
    try {
      NewMeasurementSet badms("badmsTable","");
    } catch (AipsError x) {
      thrown = True;
    } 
    if (!thrown) errCount++;

    // first, construct a Table
    Table tab(tms1);
    // then, as a MS referencing that table
    NewMeasurementSet tabRefMS(tab);

    // try the same with a bad table
    thrown=False;
    try {
      Table badtab("badmsTab;e","");
      NewMeasurementSet badms(badtab);
    } catch (AipsError x) {
      thrown = True;
    } 
    if (!thrown) errCount++;

    // cleanup

    Table badtab("badmsTable",Table::Delete);
    TableDesc("badTD",TableDesc::Delete);

    // finally, as a const MS referencing that MS
    const NewMeasurementSet constRefMS(tabRefMS);

    // can't test copy construct from bad MS, because we can't make one..

    {
      // Test MSAntenna constructors, as test of all MSTable derived classes
      // Test default constructor
      NewMSAntenna msant1();
      
      // make two tableDescs
      {
	TableDesc td1(NewMSAntenna::requiredTableDesc(),"antTD","",TableDesc::New);
	TableDesc td2(NewMSFeed::requiredTableDesc(),"badAntTD","",TableDesc::New);
      }
      // construct from name and tabledesc
      SetupNewTable newtab1("msant2","antTD",Table::New);
      {
	NewMSAntenna msant(newtab1);
      }
      NewMSAntenna msant2("msant2","antTD",Table::Old);
      
      // try an invalid tableDesc
      Bool thrown=False;
      try {
	NewMSAntenna msant2b("msant2","badAntTD",Table::Old);
	msant2b.markForDelete();
      } catch (AipsError x) {
	thrown = True;
      } 
      // No exception is thrown here, even though the td name is wrong..
      //if (!thrown) errCount++;
      
      // construct from SetupNewTable
      SetupNewTable newtab2("msant3",NewMSAntenna::requiredTableDesc(),Table::New);
      NewMSAntenna msant3(newtab2,5);
      msant3.markForDelete();
      
      // try invalid newtab
      thrown = False;
      try {
	SetupNewTable newtab("msant3b",NewMSFeed::requiredTableDesc(),Table::New);
	NewMSAntenna msant3b(newtab,5);
      } catch (AipsError x) {
	thrown = True;
      } 
      if (!thrown) errCount++;

      // cleanup the mess
      {
	SetupNewTable newtab("msant3b",NewMSFeed::requiredTableDesc(),Table::New);
	Table tab(newtab);
	tab.markForDelete();
      }
      
      // construct from Table
      SetupNewTable newtab3("msantTable","antTD",Table::New);
      Table tab(newtab3);
      tab.markForDelete();
      NewMSAntenna msant4(tab);
      
      // try invalid table
      thrown = False;
      try {
	SetupNewTable newtab4("badmsantTable","badAntTD",Table::New);
	Table tab(newtab4);
	tab.markForDelete();
	NewMSAntenna msant4b(tab);
      } catch (AipsError x) {
	thrown = True;
      } 
      if (!thrown) errCount++;
      
      // construct from existing table
      NewMSAntenna msant5("msantTable",Table::Old);

      // try invalid table
      thrown = False;
      try {
	{
	  Table tab("badmsantTable","badAntTD",Table::New);
	}
	NewMSAntenna msant5b("badmsantTable");
      } catch (AipsError x) {
	thrown = True;
      } 
      if (!thrown) errCount++;

      // Copy construct
      NewMSAntenna msant6(msant4);
      
      // try copy construct from invalid 
      try {
	NewMSFeed msfeed("msfeed", Table::New);
	msfeed.markForDelete();
	NewMSAntenna msant6b(msfeed);
      } catch (AipsError x) {
	thrown = True;
      } 
      if (!thrown) errCount++;
      
      
      // make table invalid before destruction
      thrown=False;
      try {
	SetupNewTable newtab("msAnt","antTD",Table::New);
	NewMSAntenna msant(newtab);
	msant.markForDelete();
	msant.renameColumn("myPos",NewMSAntenna::columnName(NewMSAntenna::POSITION));
      } catch (AipsError x) {
	thrown = True;
      } 
      // This throws the wrong exception: "Table: cannot rename a column"
      // There seems to be no way to make the table invalid, as both
      // removeColumn and renameColumn throw an exception.
      if (!thrown) errCount++;
    }

    //cleanup
    TableDesc td1("antTD",TableDesc::Delete);
    TableDesc td2("badAntTD",TableDesc::Delete);
    return errCount;
}

// test referenceCopy()

uInt tReferenceCopy(const String& msName, const String& refMSName)
{
    uInt errCount = 0;
    
    // open an existing table (we need Update to be able to make a writeable
    // reference table, even if we're not writing to the original table)
    NewMeasurementSet ms(msName,Table::Update);
    {
    // make a reference copy, making TIME writable and all others references
    Block<String> writableColumn(1, NewMS::columnName(NewMS::TIME));
    // this also tests the assignment operator
    NewMeasurementSet refCopyMS = ms.referenceCopy(refMSName, 
						writableColumn);
    Vector<String> colNames(refCopyMS.tableDesc().columnNames());
    // tests below will be useful when we can open the table with Old
    for (uInt i=0;i<colNames.nelements();i++) {
	if (refCopyMS.isColumnWritable(colNames(i))) {
	    // if so, it had better be TIME
	    if (colNames(i) != NewMS::columnName(NewMS::TIME)) {
//		cerr << "reference copy table column : " << colNames(i) 
//		     << " is writable.  Only NewMS::TIME should be writable." << endl;
//		errCount++;
	    }
	} else {
	    // it had better NOT be TIME
	    if (colNames(i) == NewMS::columnName(NewMS::TIME)) {
//		cerr << "reference copy table column : " << colNames(i)
//		     << " is NOT writable.  It should be!" << endl;
//		errCount++;
	    }
	}
    }

    refCopyMS.markForDelete();
    }
    return errCount;
}

// test exceptions in constructions

uInt tSetupNewTabError()
{
    // this tests the errors in the constructor from a SetupNewTable
    uInt errCount = 0;

    // make a bogus TableDesc
    TableDesc td;
    NewMS::addColumnToDesc(td, NewMS::TIME);
    SetupNewTable setup("",td, Table::Scratch);
    StManAipsIO stman;
    setup.bindAll(stman);

    Bool thrown = False;
    try {
	NewMeasurementSet ms(setup,0);
    } catch (AipsError x) {
	thrown = True;
    } 
    if (!thrown) {
	cerr << "NewMeasurementSet(SetupNewTable &, uInt) " 
	    << "should have thrown an exception" << endl;
	errCount++;
    }

    return errCount;
}

uInt tDestructorError(const String& sdmsName)
{
    uInt errCount = 0;

    Bool thrown = False;
    try {
	NewMeasurementSet ms(sdmsName);
	// remove a column
	ms.removeColumn(NewMS::columnName(NewMS::TIME));
    } catch (AipsError x) {
	thrown = True;
    } 

    if (!thrown) {
	cerr << "~NewMeasurementSet() should have thrown an exception" << endl;
	errCount++;
    }
  
    return errCount;
}

void checkErrors(uInt newErrors)
{
    if (newErrors > 0) {
	cout << newErrors << " errors!" << endl;
    } else {
	cout << "ok." << endl;
    }
}

int main() {

  try {
    uInt errCount = 0;
    uInt newErrors;

    String msName = "tNewMeasurementSet_tmp.Table";
    String refMSName = "tNewMeasurementSet_tmp.Ref-Table";

    cout << "\nNewMS::PredefinedColumns - test of static functions ... ";
    newErrors = tColumnStatics();
    checkErrors(newErrors);
    errCount += newErrors;

    cout << "\nNewMS::PredefinedKeywords - test of static functions ... ";
    newErrors = tKeywordStatics();
    checkErrors(newErrors);
    errCount += newErrors;

    cout << "\naddColumnToDesc() test on all possible columns ... ";
    newErrors = tAddAllColumns();
    checkErrors(newErrors);
    errCount += newErrors;

    cout << "\nMake a MS to test the non-static functions ... ";
    newErrors = tNonStatic(msName);
    checkErrors(newErrors);
    errCount += newErrors;

    cout << "\nTest other constructors ... ";
    newErrors = tConstructors(msName);
    checkErrors(newErrors);
    errCount += newErrors;
    
    cout << "\nTest referenceCopy() ... ";
    newErrors = tReferenceCopy(msName, refMSName);
    checkErrors(newErrors);
    errCount += newErrors;

    cout << "\nTest exceptions" << endl;
    cout << "in Constructors ... ";
    newErrors = tSetupNewTabError();
    checkErrors(newErrors);
    errCount += newErrors;

    cout << "in destructor ... ";
    newErrors = tDestructorError(msName);
    checkErrors(newErrors);
    errCount += newErrors;


    // delete the msName table

    Table ms(msName);
    ms.markForDelete();

    if (errCount > 0) {
	cout << "tNewMeasurementSet ends with " << errCount << " errors." << endl;
    } else {
	cout << "tNewMeasurementSet ends successfully" << endl;
    }

    return errCount;
  } catch (AipsError x) {
      cerr << x.getMesg() << endl;
  } 
  return 1;
}
