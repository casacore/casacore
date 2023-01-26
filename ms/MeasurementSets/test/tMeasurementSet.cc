//# tMeasurementSet.cc : this program tests the MeasurementSet class
//# Copyright (C) 1995,1996,1997,2000,2001,2002
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

//# Includes

#include <casacore/ms/MeasurementSets/MeasurementSet.h>

#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/tables/Tables.h>
#include <casacore/tables/Tables/RowCopier.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Fallible.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

// test functions, all return the number of errors unless otherwise stated
// test PredefinedColumns static functions in MeasurementSet

uint32_t tColumnStatics()
{
    // ensure that the conversions are consistent
    uint32_t errCount = 0;

    for (int32_t i=1;i<MS::NUMBER_PREDEFINED_COLUMNS;i++) {
	MS::PredefinedColumns pdcol = MS::PredefinedColumns(i);
	MS::columnDataType(pdcol);
	String pdname = MS::columnName(pdcol);
	MS::PredefinedColumns pdtype = MS::columnType(pdname);
	if (pdtype != pdcol) {
	    cerr << "Inconsistency found for column : " << pdname << endl;
	    cerr << "  Type : " << int32_t(pdtype) << " should be : " 
		 << int32_t(pdcol) <<endl;
	    errCount++;
	}

	// verify that we get an UNDEFINED_COLUMN when appropriate
	pdtype = MS::columnType("NotAPredefinedColumn");
	if (pdtype != MS::UNDEFINED_COLUMN) {
	    cerr << "columnType returned a valid PredefinedColumn for \"NotAPredefinedColumn\""
		<< int32_t(pdtype) << endl;
	    errCount++;
	}

        // this just tests that a comment exists (an exception will occur here if not)
	String comment = MS::columnStandardComment(pdcol);
        // this just tests that a UNIT exists (an exception will occur here if not)
	String unit = MS::columnUnit(pdcol);
        // this just tests that a MEASURE_TYPE exists (an exception will occur here if not)
	String measureType = MS::columnMeasureType(pdcol);

    }
    return errCount;
}


// test PredefinedKeywords static functions in MeasurementSet

uint32_t tKeywordStatics()
{
    uint32_t errCount = 0;

    // MS::PredefinedKeywords

    for (uint32_t i=1;i<MS::NUMBER_PREDEFINED_KEYWORDS;i++) {
	MS::PredefinedKeywords pdkey = MS::PredefinedKeywords(i);
	String pdname = MS::keywordName(pdkey);
	MS::PredefinedKeywords pdtype = MS::keywordType(pdname);
	// this MUST be valid and it must have the same value as pdkey
	if (pdtype != pdkey) {
	    cerr << "Inconsistency found for keyword : " << pdname << endl;
	    cerr << "  Type : " << int32_t(pdtype) << " should be : " 
		 << int32_t(pdkey) << endl;
	    errCount++;
	}

	// this just tests that a dtype is available
	MS::keywordDataType(pdkey);
        // this just tests that a comment exists (an exception will occur here if not)
	String comment = MS::keywordStandardComment(pdkey);

    }

    return errCount;
}

// test addColumnToDesc static for all possible columns 

uint32_t tAddAllColumns()
{
    uint32_t errCount = 0;

    // test addColumnToDesc for all possible columns
    {
	TableDesc testTD;
	for (uint32_t i=1;i<MS::NUMBER_PREDEFINED_COLUMNS;i++) {
	    MS::addColumnToDesc(testTD, MS::PredefinedColumns(i));
	}
//	testTD.show();

	// we should be able to add an existing column without causing an exception
	MS::addColumnToDesc(testTD, MS::TIME);
    }

    return errCount;
}

// Test most of the non-static functions 

uint32_t tNonStatic(const String& sdmsName)
{
    uint32_t errCount = 0;

    TableDesc td(MS::requiredTableDesc());
    // Add the DATA column and compress it.
    MS::addColumnToDesc(td, MS::FLOAT_DATA, 2);
    MS::addColumnCompression (td, MS::FLOAT_DATA);
    // add one column, not a PredefinedColumn
    td.addColumn(ScalarColumnDesc<double>("test_column"));
    td.defineHypercolumn ("TiledData", 3, stringToVector("FLOAT_DATA"));

    SetupNewTable setup(sdmsName, td, Table::New);
    StManAipsIO aipsioman;
    TiledShapeStMan tsm("TiledData", IPosition(3,2));
    setup.bindAll(aipsioman);
    setup.bindColumn("FLOAT_DATA", tsm);
    
    // small table, ten rows
    MeasurementSet ms(setup, 10);
    Record dminfo = ms.dataManagerInfo();
    // Check that the CompressFloat engine is created.
    bool fnd = false;
    for (uint32_t i=0; i<dminfo.nfields(); i++) {
      if (dminfo.subRecord(i).asString("TYPE") == "CompressFloat") {
	Vector<String> vec = dminfo.subRecord(i).asArrayString ("COLUMNS");
	if (vec.nelements() == 1  &&  vec(0) == "FLOAT_DATA") {
	  fnd = true;
	}
      }
    }
    AlwaysAssertExit(fnd);
    AlwaysAssertExit (ms.isColumnStored ("FLOAT_DATA_COMPRESSED"));
    AlwaysAssertExit (ms.isColumnStored ("FLOAT_DATA_SCALE"));
    AlwaysAssertExit (ms.isColumnStored ("FLOAT_DATA_OFFSET"));
    AlwaysAssertExit (! ms.isColumnStored ("FLOAT_DATA"));
    AlwaysAssertExit (ms.isColumnStored ("SIGMA"));
    // Check that the compressed column uses TiledShapeStMan.
    fnd = false;
    for (uint32_t i=0; i<dminfo.nfields(); i++) {
      if (dminfo.subRecord(i).asString("TYPE") == "TiledShapeStMan") {
	Vector<String> vec = dminfo.subRecord(i).asArrayString ("COLUMNS");
	if (vec.nelements() == 1  &&  vec(0) == "FLOAT_DATA_COMPRESSED") {
	  fnd = true;
	}
      }
    }
    AlwaysAssertExit(fnd);

    ms.createDefaultSubtables(Table::New);

    ArrayColumn<float> fldata(ms,MS::columnName(MS::FLOAT_DATA));
    ScalarColumn<bool> flrow(ms,MS::columnName(MS::FLAG_ROW));
    for (int32_t i=0; i<10; i++) {
      Matrix<float> arr(4,2);
      arr=float(i);
      fldata.put(i,arr);
      flrow.put(i,false);
    }
    

    // verify that it is valid
    if (! ms.validate()) {
	cerr << "self validation failed" <<endl;
	errCount++;
    }
    if (! MS::validate(ms.tableDesc())) {
	cerr << "validation of tableDesc fails" << endl;
	errCount++;
    }
    if (! MS::validate(ms.keywordSet())) {
	cerr << "validation of keywordSet fails" << endl;
	errCount++;
    }

    // they are all writable at this point
    if (!ms.isColumnWritable(MS::TIME)) {
	cerr << "TIME column should be writable but isColumnWritable() returned false" 
	     << endl;
	errCount++;
    }

    // test the makeComplexData() function
    ms.makeComplexData();

    // TIME is a scalar column, DATA is an array column
    if (!ms.isScalar(MS::TIME)) {
	cerr << "TIME column is scalar but isScalar() returned false" << endl;
	errCount++;
    }
    if (ms.isArray(MS::TIME)) {
	cerr << "TIME column is scalar but isArray() returned true" << endl;
	errCount++;
    }
    if (ms.isScalar(MS::DATA)) {
	cerr << "DATA column is array but isScalar() returned true" << endl;
	errCount++;
    }
    if (!ms.isArray(MS::DATA)) {
	cerr << "DATA column is array but isArray() returned false" << endl;
	errCount++;
    }

    // TIME has units of seconds
    // test via string
    if (ms.unit(MS::columnName(MS::TIME)) != "s") {
	cerr << "MS::unit(const String&) failed to return s for TIME" << endl;
	errCount++;
    }
    if (ms.unit(MS::TIME) != "s") {
	cerr << "MS::unit(MS::TIME) failed to return s" << endl;
//*** testing
	cerr << ms.unit(MS::TIME) <<endl;
//*** testing

	errCount++;
    }

    // test of operator=
    // construct a scratch APERTURE_SYNTHESIS MS
    TableDesc std(MS::requiredTableDesc());
    SetupNewTable scratchSetup("",std,Table::Scratch);
    scratchSetup.bindAll(aipsioman);
    MeasurementSet sms(scratchSetup, 20);
    sms.createDefaultSubtables(Table::TableOption(sms.tableOption()));

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
	cerr << "operator= failed, validate returns false" << endl;
	errCount++;
    }

    return errCount;
}

// Test constructors not tested by tNonStatic()

uint32_t tConstructors(const String& msName)
{
    uint32_t errCount = 0;
    // test default constructor
    MeasurementSet tms0;

    // existing table on disk, with correct type
    MeasurementSet tms1(msName);

    // try invalid table

    // make tableDesc
    {
        TableDesc td(MSFeed::requiredTableDesc(),"badTD","",TableDesc::New);
    }
    // make non MS table
    {
        SetupNewTable newtab("tMeasurementSet_tmp.badmsTable",
			     "badTD",Table::New);
        Table tab(newtab);
    }


    // try creating bad MS
    bool thrown=false;
    try {
	MeasurementSet badms("tMeasurementSet_tmp.badmsTable");
    } catch (std::exception& x) {
	thrown = true;
    } 
    if (!thrown) errCount++;

    // alternate form with TableDesc (none specified here)
    MeasurementSet tms2(msName,"");
    // try creating bad MS
    thrown=false;
    try {
	MeasurementSet badms("tMeasurementSet_tmp.badmsTable","");
    } catch (std::exception& x) {
	thrown = true;
    } 
    if (!thrown) errCount++;

    // first, construct a Table
    Table tab(tms1);
    // then, as a MS referencing that table
    MeasurementSet tabRefMS(tab);

    // try the same with a bad table
    thrown=false;
    try {
	Table badtab("tMeasurementSet_tmp.badmsTab;e","");
	MeasurementSet badms(badtab);
    } catch (std::exception& x) {
	thrown = true;
    } 
    if (!thrown) errCount++;

    // cleanup

    Table badtab("tMeasurementSet_tmp.badmsTable",Table::Delete);
    TableDesc("badTD",TableDesc::Delete);

    // finally, as a const MS referencing that MS
    const MeasurementSet constRefMS(tabRefMS);

    // can't test copy construct from bad MS, because we can't make one..

    {
	// Test MSAntenna constructors, as test of all MSTable derived classes
	// Test default constructor
	MSAntenna msant1;
      
	// make two tableDescs
	{
	    TableDesc td1(MSAntenna::requiredTableDesc(),"antTD","",TableDesc::New);
	    TableDesc td2(MSFeed::requiredTableDesc(),"badAntTD","",TableDesc::New);
	}
	// construct from name and tabledesc
	SetupNewTable newtab1("tMeasurementSet_tmp.msant2","antTD",Table::New);
	{
	    MSAntenna msant(newtab1);
	}
	MSAntenna msant2("tMeasurementSet_tmp.msant2","antTD",Table::Old);
      
	// try an invalid tableDesc
	bool thrown=false;
	try {
	    MSAntenna msant2b("tMeasurementSet_tmp.msant2","badAntTD",Table::Old);
	    msant2b.markForDelete();
	} catch (std::exception& x) {
	    thrown = true;
	} 
	// No exception is thrown here, even though the td name is wrong..
	//if (!thrown) errCount++;
      
	// construct from SetupNewTable
	SetupNewTable newtab2("tMeasurementSet_tmp.msant3",
			      MSAntenna::requiredTableDesc(),Table::New);
	MSAntenna msant3(newtab2,5);
	msant3.markForDelete();
      
	// try invalid newtab
	thrown = false;
	try {
	    SetupNewTable newtab("tMeasurementSet_tmp.msant3b",
				 MSFeed::requiredTableDesc(),Table::New);
	    MSAntenna msant3b(newtab,5);
	} catch (std::exception& x) {
	    thrown = true;
	} 
	if (!thrown) errCount++;

	// cleanup the mess
	{
	    SetupNewTable newtab("tMeasurementSet_tmp.msant3b",
				 MSFeed::requiredTableDesc(),Table::New);
	    Table tab(newtab);
	    tab.markForDelete();
	}
      
	// construct from Table
	SetupNewTable newtab3("tMeasurementSet_tmp.msantTable",
			      "antTD",Table::New);
	Table tab(newtab3);
	tab.markForDelete();
	MSAntenna msant4(tab);
      
	// try invalid table
	thrown = false;
	try {
	    SetupNewTable newtab4("tMeasurementSet_tmp.badmsantTable",
				  "badAntTD",Table::New);
	    Table tab(newtab4);
	    tab.markForDelete();
	    MSAntenna msant4b(tab);
	} catch (std::exception& x) {
	    thrown = true;
	} 
	if (!thrown) errCount++;
      
	// construct from existing table
	MSAntenna msant5("tMeasurementSet_tmp.msantTable",Table::Old);

	// try invalid table
	thrown = false;
	try {
	    {
		Table tab("tMeasurementSet_tmp.badmsantTable","badAntTD",Table::New);
	    }
	    MSAntenna msant5b("tMeasurementSet_tmp.badmsantTable");
	} catch (std::exception& x) {
	    thrown = true;
	} 
	if (!thrown) errCount++;

	// Copy construct
	MSAntenna msant6(msant4);
      
	// try copy construct from invalid 
	try {
	    MSFeed msfeed("msfeed", Table::New);
	    msfeed.markForDelete();
	    MSAntenna msant6b(msfeed);
	} catch (std::exception& x) {
	    thrown = true;
	} 
	if (!thrown) errCount++;
      
    }

    //cleanup
    TableDesc td1("antTD",TableDesc::Delete);
    TableDesc td2("badAntTD",TableDesc::Delete);
    return errCount;
}

// test referenceCopy()

uint32_t tReferenceCopy(const String& msName, const String& refMSName)
{
    uint32_t errCount = 0;
    
    // open an existing table (we need Update to be able to make a writeable
    // reference table, even if we're not writing to the original table)
    MeasurementSet ms(msName,Table::Update);
    {
    // make a reference copy, making TIME writable and all others references
    Block<String> writableColumn(1, MS::columnName(MS::TIME));
    // this also tests the assignment operator
    MeasurementSet refCopyMS = ms.referenceCopy(refMSName, 
						writableColumn);
    Vector<String> colNames(refCopyMS.tableDesc().columnNames());
    // tests below will be useful when we can open the table with Old
    for (uint32_t i=0;i<colNames.nelements();i++) {
	if (refCopyMS.isColumnWritable(colNames(i))) {
	    // if so, it had better be TIME
	    if (colNames(i) != MS::columnName(MS::TIME)) {
//		cerr << "reference copy table column : " << colNames(i) 
//		     << " is writable.  Only MS::TIME should be writable." << endl;
//		errCount++;
	    }
	} else {
	    // it had better NOT be TIME
	    if (colNames(i) == MS::columnName(MS::TIME)) {
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

// test null MS

uint32_t tNullMS(const String& msName)
{
  uint32_t errCount = 0;
  {
    // Test construction and destruction of null MS.
    MeasurementSet ms;
    if (! ms.isNull()) {
      cout << "tNullMS: MS should be null" << endl;
      errCount++;
    }
  }
  MeasurementSet nullMS;
  {
    // Test copy construction of null MS.
    MeasurementSet mscopy (nullMS);
    if (! mscopy.isNull()) {
      cout << "tNullMS: MS copy should be null" << endl;
      errCount++;
    }
  }
  {
    // Test assigment of null MS to null MS
    MeasurementSet ms;
    ms = nullMS;
    if (! ms.isNull()) {
      cout << "tNullMS: assign to null MS should be null" << endl;
      errCount++;
    }
  }
  {
    // Test assigment of null MS to non-null MS
    MeasurementSet ms(msName);
    ms = nullMS;
    if (! ms.isNull()) {
      cout << "tNullMS: assign to non-null MS should be null" << endl;
      errCount++;
    }
  }
  {
    // Test assigment of non-null MS to null MS
    MeasurementSet nms;
    MeasurementSet ms(msName);
    nms = ms;
    if (nms.isNull()) {
      cout << "tNullMS: assign to null MS should be non-null" << endl;
      errCount++;
    }
  }
  return errCount;
}

// test exceptions in constructions

uint32_t tSetupNewTabError()
{
    // this tests the errors in the constructor from a SetupNewTable
    uint32_t errCount = 0;

    // make a bogus TableDesc
    TableDesc td;
    MS::addColumnToDesc(td, MS::TIME);
    SetupNewTable setup("",td, Table::Scratch);
    StManAipsIO stman;
    setup.bindAll(stman);

    bool thrown = false;
    try {
	MeasurementSet ms(setup,0);
    } catch (std::exception& x) {
	thrown = true;
    } 
    if (!thrown) {
	cerr << "MeasurementSet(SetupNewTable &, uint32_t) " 
	    << "should have thrown an exception" << endl;
	errCount++;
    }

    return errCount;
}

uint32_t tDestructorError(const String& sdmsName)
{
    uint32_t errCount = 0;

    bool thrown = false;
    try {
	MeasurementSet ms(sdmsName);
	// remove a column
	ms.removeColumn(MS::columnName(MS::TIME));
    } catch (std::exception& x) {
	thrown = true;
    } 

    if (!thrown) {
	cerr << "~MeasurementSet() should have thrown an exception" << endl;
	errCount++;
    }
  
    return errCount;
}

void checkErrors(uint32_t newErrors)
{
    if (newErrors > 0) {
	cout << newErrors << " errors!" << endl;
    } else {
	cout << "ok." << endl;
    }
}

int main() {

  try {
    uint32_t errCount = 0;
    uint32_t newErrors;

    String msName = "tMeasurementSet_tmp.Table";
    String refMSName = "tMeasurementSet_tmp.Ref-Table";

    cout << "\nMS::PredefinedColumns - test of static functions ... ";
    newErrors = tColumnStatics();
    checkErrors(newErrors);
    errCount += newErrors;

    cout << "\nMS::PredefinedKeywords - test of static functions ... ";
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

    cout << "\nTest null MS ... ";
    newErrors = tNullMS(msName);
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
	cout << "tMeasurementSet ends with " << errCount << " errors." << endl;
    } else {
	cout << "tMeasurementSet ends successfully" << endl;
    }

    return errCount;
  } catch (std::exception& x) {
      cerr << x.what() << endl;
  } 
  return 1;
}
