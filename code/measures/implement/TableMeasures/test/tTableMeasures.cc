//# tTableMeasures.cc: test program for the TableMeasures class.
//# Copyright (C) 1997,1998,1999,2000
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

#include <aips/TableMeasures/ScalarMeasColumn.h>
#include <aips/TableMeasures/ArrayMeasColumn.h>
#include <aips/TableMeasures/TableMeasValueDesc.h>
#include <aips/TableMeasures/TableMeasOffsetDesc.h>
#include <aips/TableMeasures/TableMeasRefDesc.h>
#include <aips/TableMeasures/TableMeasDesc.h>
#include <aips/TableMeasures/ArrayQuantColumn.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MeasTable.h>
#include <aips/Measures/MeasData.h>
#include <aips/Measures/MeasRef.h>
#include <aips/Measures/MeasFrame.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Quanta/MVEpoch.h>
#include <aips/Quanta/MVTime.h>
#include <aips/Quanta/QLogical.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Utilities/DataType.h>
#include <aips/Utilities/ValType.h>
#include <aips/Utilities/Assert.h>
#include <aips/OS/Timer.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>


void showKeys (const TableRecord& record, uInt indent)
{
  for (uInt i=0; i<record.nfields(); i++) {
    for (uInt j=0; j<indent; j++) {
      cout << ' ';
    }
    cout << record.name(i) << " = ";
    switch (record.dataType(i)) {
    case TpBool:
      cout << record.asBool(i) << "  (Bool)" << endl;
      break;
    case TpUChar:
      cout << record.asuChar(i) << "  (uChar)" << endl;
      break;
    case TpShort:
      cout << record.asShort(i) << "  (Short)" << endl;
      break;
    case TpInt:
      cout << record.asInt(i) << "  (Int)" << endl;
      break;
    case TpUInt:
      cout << record.asuInt(i) << "  (uInt)" << endl;
      break;
    case TpFloat:
      cout << record.asFloat(i) << "  (Float)" << endl;
      break;
    case TpDouble:
      cout << record.asDouble(i) << "  (Double)" << endl;
      break;
    case TpComplex:
      cout << record.asComplex(i) << "  (Complex)" << endl;
      break;
    case TpDComplex:
      cout << record.asDComplex(i) << "  (DComplex)" << endl;
      break;
    case TpString:
      cout << record.asString(i) << "  (String)" << endl;
      break;
    case TpArrayString:
      cout << record.asArrayString(i) << "  (String)" << endl;
      break;
    case TpTable:
      cout << "  (Table)" << endl;
      break;
    case TpRecord:
      cout << "  (Record)" << endl;
      showKeys (record.subRecord(i), indent+2);
      break;
    default:
      cout << "  Array "
	   << ValType::getTypeStr(asScalar(record.dataType(i))) << ' '
	   << record.shape(i) << endl;
    }
  }
}


int main(int argc)
{
  try {
    Bool doExcep = ToBool(argc<2);
    if (doExcep) {
      cout << "Test of TableMeasures classes.\n";
    } else {
      cout << "Test of TableMeasures classes without exceptions.\n";
    }

    // Need a table to work with.
    TableDesc td("tTableMeasures_tmp", "1", TableDesc::New);
    td.comment() = "A test of TableMeasures class.";

    // Each measure column needs at exactly one ArrayColumn<Double> for storing
    // the value component of each measure.  Additional columns are required
    // for storing measure offsets and measure references when these components
    // vary per row.  The value column is always an ArrayColumn irrespective
    // of whether Scalar or Array measures are to be stored.

    // A scalar column of MDirection.  Static reference so no addional
    // columns required.
    ArrayColumnDesc<Double> cdMDir("MDirColumn", "Simple mdirection column",
				   IPosition(1,2), ColumnDesc::Direct);

    // A scalar MEpoch column with a fixed offset and reference.  Fixed
    // references and offsets do not need additional columns as they are
    // stored as keywords.
    ScalarColumnDesc<Double> cdTOffset("TimeOffset",
		  "MEpoch column with fix reference and offset");

    // The following three columns will be used to set up a Scalar MEpoch
    // column with variable references and offsets.  3 columns are needed.
    // The "main" column where the MEpoch will be stored
    ScalarColumnDesc<Double> cdTime("Time1", "An MEpoch column");
    // For the offsets. Offsets are also measures so this is effectively
    // another measure column.
    ScalarColumnDesc<Double> cdVarOffset("TimeVarOffset",
					"Variable Offset col");
    // an int column for the variable references
    ScalarColumnDesc<Int> cdTimeRef("TimeRef", "Reference column for Time1");

    // a scalar measure column with a variable string reference
    // a column for the measures.  No offset or it is to be static so
    // no offset column required.
    // The "main" column.
    ScalarColumnDesc<Double> cdMEVS("MEpochVarStr", "Another MEpoch column");
    // a string column for the variable string references
    ScalarColumnDesc<String> cdTimeRefStr("TimeRefStr",
					  "String variable reference column");

    // An array measure column with a variable (int) reference
    // A column for the measures
    ArrayColumnDesc<Double> cdTimeArr("Time1Arr", "An MEpoch array column");
    // An int column for the variable references.
    ScalarColumnDesc<Int> cdTimeArrRef("TimeArrRef", "VarRef co for TimeArr");

    // An array measure column with a variable (int) reference array column
    // and a variable offset column
    ArrayColumnDesc<Double> cdTime2Arr("Time2Arr", "An MEpoch array column");
    // the offset column
    ArrayColumnDesc<Double> cdTime2ArrOffset("Time2ArrOffset",
					     "Offset column for Time2Arr");
    // the reference column
    ArrayColumnDesc<Int> cdTime2ArrRef("Time2ArrRef",
				       "Ref column for Time2Arr");

    // An array measure column with variable (string) reference array column
    // The "main" date column
    ArrayColumnDesc<Double> cdTime3Arr("Time3Arr", "An MEpoch array column");
    // The string array column for the references
    ArrayColumnDesc<String> cdTime3StrRef("Time3ArrStrRef",
					  "Array string reference column");

    // An array measure column with a scalar reference (string) column and
    // a scalar (per row) offset column.
    // That is, one reference stored per row
    // The "main" date column
    ArrayColumnDesc<Double> cdTime4Arr("Time4Arr", "An MEpoch array column");
    // A scalar column for the references
    ScalarColumnDesc<String> cdTime4StrRef("Time4StrRef",
					   "Scalar int reference column");
    // An array column for the variable offsets.  Even though we want to
    // stored offsets per row the column must be an Array column because
    // offsets are Measures, i.e., offsets are stored in a Measure column
    ScalarColumnDesc<Double> cdTime4ScaOffset("Time4ScaOffset",
					     "Scalar offset column");

    // a "spare" column used for testing purposes
    ArrayColumnDesc<Double> cdTestCol("SpareCol1",
				      "Test of exception column");
    // a spare offset column
    ArrayColumnDesc<Double> cdTestArrOffset("SpareArrOffset",
					    "Spare int array column");

    // All of the above column descriptors are added to the table as usual
    td.addColumn(cdTime);
    td.addColumn(cdTOffset);
    td.addColumn(cdVarOffset);
    td.addColumn(cdMDir);
    td.addColumn(cdTimeRef);
    td.addColumn(cdTimeRefStr);
    td.addColumn(cdMEVS);
    td.addColumn(cdTimeArr);
    td.addColumn(cdTimeArrRef);
    td.addColumn(cdTime2Arr);
    td.addColumn(cdTime2ArrOffset);
    td.addColumn(cdTime2ArrRef);
    td.addColumn(cdTime3Arr);
    td.addColumn(cdTime3StrRef);
    td.addColumn(cdTime4Arr);
    td.addColumn(cdTime4StrRef);
    td.addColumn(cdTime4ScaOffset);
    td.addColumn(cdTestCol);
    td.addColumn(cdTestArrOffset);

    // We have the columns we need but there not yet measure columns.
    {
      // The following creates an (empty) MDirection column.  The column
      // used is "MDirColumn".   This is the simplest useful TableMeasDesc
      // declaration that can be done. Default MDirection::Ref is used
      // for the reference.

      // The value desc. specifies the column to use for the measures
      TableMeasValueDesc tmvdMDir(td, "MDirColumn");
      // the TableMeasDesc gives the column a type
      TableMeasDesc<MDirection> tmdMDir(tmvdMDir);
      // writing create the measure column
      tmdMDir.write(td);
    }
    {
      MEpoch obsTime((MVEpoch(MVTime(1995, 5, 17, (8+18./60.)/24.))),
		     MEpoch::UTC);
      // The following creates an (empty) Measure column.  This
      // particular column is to contain MEpoch with a fixed reference
      // and offset.  The column named "TimeOffset" is used for the
      // MEpochs.  Columns for the refernce and offset are not needed as
      // these are fixed (and stored as keywords in the TimeOffset column

      // The fixed offset which is itself an MEpoch
      TableMeasOffsetDesc tmodObsTime(obsTime);

      // The reference descriptor associates the fixed reference and the
      // just declared offset descriptor
      TableMeasRefDesc tmrdObs(MEpoch::TAI, tmodObsTime);

      // The value descriptor specifies the column to use for the Measures
      TableMeasValueDesc tmvdObs(td, "TimeOffset");

      // TableMeasDesc associate the value desc. and the reference desc. and
      // gives the column a type (MEpoch).
      TableMeasDesc<MEpoch> tmdObs(tmvdObs, tmrdObs);

      // (test purposes only for purify - test assign and copy contructors
      TableMeasDesc<MEpoch> tmdObs1 = tmdObs;
      TableMeasDesc<MEpoch> tmdObs1a(tmdObs1);

      // finally write the descriptor!  The column is now a TableMeasure
      // column.
      tmdObs1a.write(td);
    }
    {
      // Set up a MEpoch column with variable references and offsets
	
      // An offset is a measure. So for variable offsets a Measure
      // column is needed.  The Value descriptor specifies the column to
      // use.
      TableMeasValueDesc tmvdObs(td, "TimeVarOffset");
      // The descriptor gives the offset column a type.
      TableMeasDesc<MEpoch> tmMOS(tmvdObs);
      TableMeasOffsetDesc tmOsDesc(tmMOS);

      // NB: this descriptor is not written.  This is done via the write()
      // of the "main" column below.
	
      // Reference desc. specifies a column to use for the references and
      // associates the offset measure column.
      TableMeasRefDesc tmrd(td, "TimeRef", tmOsDesc);

      // The "main" measure column
      TableMeasValueDesc tmvd(td, "Time1");
      // The desc. associates the value "main" column with the reference
      // (which includes the offset measure column).
      TableMeasDesc<MEpoch> tmdMEpoch(tmvd, tmrd);
      // write creates the measure column
      tmdMEpoch.write(td);
    }
    {
      // A variable offset column is a Measure column, so a TableMeasDesc
      // is needed.
      TableMeasValueDesc tmvdObs(td, "TimeVarOffset");
      TableMeasDesc<MEpoch> tmMOS(tmvdObs);
      TableMeasOffsetDesc tmODesc(tmMOS);

      // Simplest useful TableMeasDesc declaration that can be done.
      TableMeasRefDesc tmrd(td, "TimeRefStr", tmODesc);
      TableMeasValueDesc tmvdMEpoch2(td, "MEpochVarStr");
      TableMeasDesc<MEpoch> tmdMEpoch2(tmvdMEpoch2, tmrd);
      tmdMEpoch2.write(td);
    }

    MEpoch mjdToday(MVEpoch(51234));
    {
      // An array MEpoch column descriptor.  The TableMeasDesc for an Array
      // measure column is identical to the Scalar measure column.
	
      TableMeasOffsetDesc tmodToday(mjdToday);
      TableMeasRefDesc tmrdGast(td, "TimeArrRef", tmodToday);
      TableMeasValueDesc tmvdGast(td, "Time1Arr");
      // create a tmp and test if copy constructor and assignment work
      Vector<Unit> u(1);
      u(0) = "h";
      TableMeasDesc<MEpoch> tmp(tmvdGast, tmrdGast, u);
      TableMeasDesc<MEpoch> tmp2 = tmp;
      TableMeasDesc<MEpoch> tmdArrGast(tmp2);
      tmdArrGast.write(td);
    }
    {
      // Used to demonstrate an exception, specifically, that a
      // ScalarMeasColumn cannot have an ArrayMeasColumn for its offsets

      // the measure offset column for the array measure column
      TableMeasValueDesc arrOffset(td, "SpareArrOffset");
      // The descriptor gives the offset column a type.
      TableMeasDesc<MEpoch> tmdOffset(arrOffset);

      // the True says wants to have an ArrayMeasColumn for offset
      TableMeasOffsetDesc tmodOS(tmdOffset, True);

      TableMeasRefDesc tmrdGast(MEpoch::GAST, tmodOS);
      TableMeasValueDesc tmvdGast(td, "SpareCol1");
      TableMeasDesc<MEpoch> tmdMDesc(tmvdGast, tmrdGast);	
      tmdMDesc.write(td);
    }
    {
      // An array MEpoch column desc. with variable offset and reference.

      // the measure offset column for the array measure column
      TableMeasValueDesc arrOffset(td, "Time2ArrOffset");
      // The descriptor gives the offset column a type.
      TableMeasDesc<MEpoch> tmdOffset(arrOffset);
      TableMeasOffsetDesc tmOSDesc(tmdOffset, True);
      // measure reference column and associated offset
      TableMeasRefDesc tmARef(td, "Time2ArrRef", tmOSDesc);
      // the "main" value descriptor
      TableMeasValueDesc tmAVal(td, "Time2Arr");

      // create a tmp and test if copy constructor and assignment work
      TableMeasDesc<MEpoch> tmp(tmAVal, tmARef);
      TableMeasDesc<MEpoch> tmp2 = tmp;
      TableMeasDesc<MEpoch> tmdArray(tmp2);	
	
      tmdArray.write(td);
    }
    {
      // An array MEpoch column desc. with variable string references.

      // measure reference column and associated offset
      TableMeasRefDesc tmARef(td, "Time3ArrStrRef");
      // the "main" value descriptor
      TableMeasValueDesc tmAVal(td, "Time3Arr");

      // create a tmp and test if copy constructor and assignment work
      TableMeasDesc<MEpoch> tmdArray(tmAVal, tmARef);
      tmdArray.write(td);
    }
    {
      td.show (cout);
      for (uInt i=0; i<td.ncolumn(); i++) {
	cout << "* " << td[i].name() << endl;
	showKeys (td[i].keywordSet(), 4);
      }
    }

    {
      // Check various exceptions
      if (doExcep) {
	try {
	  // test TMRefDesc no such column
	  TableMeasRefDesc tCol(td, "SillyColumnName");
	} catch (AipsError x) {
	  cout << "The following should report no such column ";
	  cout << " for TableMeasRefDesc.\n";
	  cout << x.getMesg() << endl;
	} 
	try {
	  // test TMRefDesc - column exist but is of the wrong type
	  TableMeasRefDesc tCol(td, "Time4ScaOffset");
	} catch (AipsError x) {
	  cout << "The following should report that the column's ";
	  cout << "type is no good.\n";
	  cout << x.getMesg() << endl;
	} 
	try {
	  // test TableMeasValueDesc - column doesn't exist exception
	  TableMeasValueDesc tCol(td, "SillyColumnName");
	} catch (AipsError x) {
	  cout << "The following should report no such column ";
	  cout << "for TableMeasValueDesc.\n";
	  cout << x.getMesg() << endl;
	} 
	try {
	  // test TableMeasValueDesc - column exists but not array
	  TableMeasValueDesc tCol(td, "Time4StrRef");
	} catch (AipsError x) {
	  cout << "The following should report that the column ";
	  cout << "is not array for TabelMeasValueDesc.\n";
	  cout << x.getMesg() << endl;
	} 
	try {
	  // test TableMeasValueDesc - column exists but is double
	  TableMeasValueDesc tCol(td, "Time2ArrRef");
	} catch (AipsError x) {
	  cout << "The following should report that the column's ";
	  cout << "type should be double for the TableMeasValueDesc.\n";
	  cout << x.getMesg() << endl;
	} 
	try {
	  // test TableMeasValueDesc - too many units
	  Vector<Unit> u(2);
	  TableMeasValueDesc tCol(td, "Time2Arr");
	  TableMeasDesc<MEpoch> tmp(tCol, u);
	} catch (AipsError x) {
	  cout << "The following should report that the column's ";
	  cout << "unit vector is too long.\n";
	  cout << x.getMesg() << endl;
	} 
	try {
	  // test TableMeasValueDesc - invalid unit
	  Vector<Unit> u(1);
	  u(0) = "m";
	  TableMeasValueDesc tCol(td, "Time2Arr");
	  TableMeasDesc<MEpoch> tmp(tCol, u);
	} catch (AipsError x) {
	  cout << "The following should report that the column ";
	  cout << "has an invalid unit.\n";
	  cout << x.getMesg() << endl;
	} 
      }


      // An array MEpoch column desc. with (string) references and offsets
      // per row.

      // the measure offset column for the array measure column
      TableMeasValueDesc scaOffset(td, "Time4ScaOffset");
      // The descriptor gives the offset column a type.
      TableMeasDesc<MEpoch> tmdOffset(scaOffset);
      TableMeasOffsetDesc tmOsDesc(tmdOffset);

      // test exception thrown on requesting the offset when it is variable
      if (doExcep) {
	try {
	  // get getOffset on a variable offset column
	  tmOsDesc.getOffset();
	} catch (AipsError x) {
	  cout << "Attempt to reference undefined Measure offset ";
	  cout << " exception on the TableMeasOffsetDesc object.\n";
	  cout << x.getMesg() << endl;
	} 
      }

      // measure reference column and associated offset
      TableMeasRefDesc tmARef_tmp(td, "Time4StrRef", tmOsDesc);
      TableMeasRefDesc tmARef(td, "Time4StrRef");
      if (!tmARef.isOffsetVariable() && !tmARef.isOffsetArray()) {
	cout << "PASS - TMRefDesc doesn't have an offset yet\n";
      } else {
	cout << "FAIL - Reference should not yet have an offset\n";
      }
      tmARef = tmARef_tmp;
      // the "main" value descriptor..testing assignment too
      TableMeasValueDesc tmAVal_tmp(td, "Time4Arr");
      TableMeasValueDesc tmAVal(td, "Time3Arr");
      tmAVal = tmAVal_tmp;

      // create a tmp and test if copy constructor and assignment work
      // test assignment too
      TableMeasDesc<MEpoch> tmdArray_tmp(tmAVal, tmARef);
      TableMeasDesc<MEpoch> tmdArray(tmAVal);
      tmdArray = tmdArray_tmp;
      tmdArray.write(td);

      // test getting on the new descriptor reference
      TableMeasRefDesc testRef = tmdArray.getRefDesc();
      if (testRef.hasOffset()) {
	cout << "PASS - Reference has column offset.\n";
      } else {
	cout << "FAIL - Reference apparantly doesn't have an offset!\n";
      }
    }

    // Finally create the table

    SetupNewTable newtab("tTableMeasures_tmp.tab", td, Table::New);

    // At this point a table called "tTableMeasures_tmp.tab" has been created.
    // It contains a number of empty Measure columns.  The remainder of this
    // program tests the usage of Scalar(Array)MeasColumn objects for
    // putting and getting Measures into and out of the table.


    MEpoch obsTime(MVEpoch(MVTime(1996, 5, 17, (8+18./60.)/24.)),
		   MEpoch::UTC);
    const uInt tabRows = 5;
    {
      Table tab(newtab);
      cout << "Create MEpochScaCol from column TimeOffset...\n";
      cout << "A column of MEpochs where the reference and offset are ";
      cout << "non-variable.\n";
      // create first a null object and attach it and copy it etc.
      // to show that these operations work.
      MEpoch::ScalarColumn tmpCol;
      if (tmpCol.isNull()) {
	tmpCol.attach(tab, "TimeOffset");
      }
      tmpCol.throwIfNull();
      AlwaysAssertExit (tmpCol.columnName() == "TimeOffset");
      cout << "Null MEpochScaCol successfully attached\n";
      // no assignment operator but there is a copy constructor
      MEpoch::ScalarColumn timeCol(tmpCol);
      Vector<Unit> u(1, "s");
      timeCol.setDescRefCode (MEpoch::GAST);
      timeCol.setDescOffset (obsTime);
      timeCol.setDescUnits (u);

      {
	ROTableMeasColumn tmcol(tab, "Time1Arr");
	AlwaysAssertExit (tmcol.columnName() == "Time1Arr");
	tmcol.attach (tab, "TimeOffset");
	AlwaysAssertExit (tmcol.columnName() == "TimeOffset");
	tmcol.reference (ROTableMeasColumn(tab, "Time1Arr"));
	AlwaysAssertExit (tmcol.columnName() == "Time1Arr");
      }

      // print some details things about the column
      if (timeCol.measDesc().isRefCodeVariable()) {
	cout << "The column has variable references.\n";
      } else {
	cout << "The MeasRef for the column is: " << timeCol.getMeasRef()
	     << endl;
      }

      {
	MEpoch::ArrayColumn arrayCol(tab, "Time1Arr");
	Vector<Unit> u(1);
	u(0) = "m";
	if (doExcep) {
	  try {
	    arrayCol.setDescUnits (u);
	  } catch (AipsError x) {
	    cout << "The following line should report an error ";
	    cout << "in ScalarMeasColumn::setDescUnits - invalid unit.\n";
	    cout << x.getMesg() << endl;
	  } 
	}
	cout << "Units of Time1Arr: "
	     << arrayCol.measDesc().getUnits()(0).getName() << endl;
	u(0) = "";
	arrayCol.setDescUnits (u);
	cout << "Units of Time1Arr: "
	     << arrayCol.measDesc().getUnits()(0).getName() << endl;
	u(0) = "s";
	arrayCol.setDescUnits (u);
	cout << "Units of Time1Arr: "
	     << arrayCol.measDesc().getUnits()(0).getName() << endl;
      }

      // Add the rows to the table.
      // Thereafter we'll do gets and puts.
      tab.addRow (tabRows);

      cout << "Adding a few MEpochs to column TimeOffset...\n";
      MEpoch tm(MVEpoch(1234.));
      uInt i;
      for (i=0; i<tabRows; i++) {
	tm.set(MVEpoch(1234 + (i/10.0)));
	timeCol.put(i, tm);
      }

      cout << "Reading the MEpochs back from TimeOffset...\n";
      // Create readonly measure column.  Create it null and then attach
      // a column (for coverage)
      MEpoch::ROScalarColumn timeColRead;
      timeColRead.attach(tab, "TimeOffset");
      for (i=0; i<tabRows; i++) {
	AlwaysAssertExit (timeColRead.isDefined(i));
	timeColRead.get(i, tm);
	AlwaysAssertExit (tm.getRef().getType() == MEpoch::GAST);
	const MEpoch* offptr = dynamic_cast<const MEpoch*>
                                                   (tm.getRef().offset());
	AlwaysAssertExit (offptr != 0);
	AlwaysAssertExit (near (offptr->get("s"), obsTime.get("s"), 1.e-10));
	tm = MEpoch::Convert (tm, MEpoch::UTC)();
	AlwaysAssertExit (near (tm.get("s"),
				MEpoch(MVEpoch(1234.+i/10.0)).get("s"),
				1.e-10));
      }

      // for coverage of reference member (via attach)
      cout << "TEST of attach/reference...\n";
      MEpoch::ScalarColumn testVarStrCol;
      testVarStrCol.attach(tab, "MEpochVarStr");
      cout << "Column attached...\n";

      // copy constructor
      MEpoch::ScalarColumn testCopy = testVarStrCol;
      // and get the values
      for (i=0; i<tabRows; i++) {
	testCopy.put(i, MVEpoch(1.));
      }
      for (i=0; i<tabRows; i++) {
	cout << testCopy(i) << endl;
      }

      // stuff to increase line coverage of classes
      if (doExcep) {
	try {
	  // try constructing with a non MeasureColumn
	  MEpoch::ROScalarColumn tScaCol(tab, "TimeRef");
	} catch (AipsError x) {
	  cout << "The following line should report an error ";
	  cout << "in reconstruct - invalid column exception.\n";
	  cout << x.getMesg() << endl;
	} 
	try {
	  // test throw if null exception
	  MEpoch::ScalarColumn nullCol;
	  nullCol.throwIfNull();
	} catch (AipsError x) {
	  cout << "The following line should be a ";
	  cout << "null column exception.\n";
	  cout << x.getMesg() << endl;
	} 
	try {
	  // try constructing a ScalarMeasColumn with an Array Offset
	  // column
	  MEpoch::ScalarColumn(tab, "SpareCol1");
	} catch (AipsError x) {
	  cout << "The following line should be an illegal ";
	  cout << "offset column type exception.\n";
	  cout << x.getMesg() << endl;
	} 
      }
    }

    {
      // reopen the table RO and read the measures
      cout << "Reopening the table read-only and reading contents...\n";
      Table tab("tTableMeasures_tmp.tab", Table::Old);
      MEpoch::ROScalarColumn timeColRead(tab, "TimeOffset");
      ROScalarColumn<Double> timeColSimple(tab, "TimeOffset");
      MEpoch tm;
      for (uInt i=0; i<tabRows; i++) {
	AlwaysAssertExit (timeColRead.isDefined(i));
	timeColRead.get(i, tm);
	AlwaysAssertExit (near (tm.get("s"),
                                Quantum<Double>(timeColSimple(i), "s"),
				1.e-10));
	AlwaysAssertExit (tm.getRef().getType() == MEpoch::GAST);
	const MEpoch* offptr = dynamic_cast<const MEpoch*>
                                        	  (tm.getRef().offset());
	AlwaysAssertExit (offptr != 0);
	AlwaysAssertExit (near (offptr->get("s"), obsTime.get("s"), 1.e-10));
	tm = MEpoch::Convert (tm, MEpoch::UTC)();
	AlwaysAssertExit (near (tm.get("s"),
				MEpoch(MVEpoch(1234.+i/10.0)).get("s"),
				1.e-10));

	MEpoch tm1 = timeColRead.convert (i, MEpoch::UTC);
	AlwaysAssertExit (tm1.getRef().getType() == MEpoch::UTC);
	offptr = dynamic_cast<const MEpoch*> (tm1.getRef().offset());
	AlwaysAssertExit (offptr == 0);
	AlwaysAssertExit (near (tm1.get("s"),
				MEpoch(MVEpoch(1234.+i/10.0)).get("s"),
				1.e-10));

	MEpoch tm2 = timeColRead.convert (i, tm1);
	AlwaysAssertExit (tm2.getRef().getType() == MEpoch::UTC);
	offptr = dynamic_cast<const MEpoch*> (tm2.getRef().offset());
	AlwaysAssertExit (offptr == 0);
	AlwaysAssertExit (near (tm2.get("s"),
				MEpoch(MVEpoch(1234.+i/10.0)).get("s"),
				1.e-10));

	MPosition mpobs;
	MeasTable::Observatory(mpobs, "WSRT");
	MEpoch::Ref mref(MEpoch::LAST, MeasFrame(mpobs));
	MEpoch tm3 = timeColRead.convert (i, mref);
	AlwaysAssertExit (tm3.getRef().getType() == MEpoch::LAST);
	offptr = dynamic_cast<const MEpoch*> (tm3.getRef().offset());
	AlwaysAssertExit (offptr == 0);
	MEpoch tm4 = MEpoch::Convert (tm3, mref)();
	AlwaysAssertExit (near (tm3.get("s"), tm4.get("s"), 1.e-10));

	MEpoch tm5 = timeColRead.convert (i, tm4);
	AlwaysAssertExit (tm5.getRef().getType() == MEpoch::LAST);
	offptr = dynamic_cast<const MEpoch*> (tm5.getRef().offset());
	AlwaysAssertExit (offptr == 0);
	AlwaysAssertExit (near (tm5.get("s"), tm4.get("s"), 1.e-10));
      }
    }

    {
      Table tab("tTableMeasures_tmp.tab", Table::Update);
      // A column of MDirections
      MDirection::ScalarColumn mdirCol(tab, "MDirColumn");
      if (mdirCol.measDesc().isRefCodeVariable()) {
	cout << "Error: reference is variable!\n";
      }

      cout << "Filling the MDirection column MDirColumn\n";
      MDirection mdir;
      for (uInt i=0; i<tabRows; i++) {
	MDirection mdir (Quantity(20, "deg"), Quantity(53, "deg"));
	cout << "put: " << mdir << endl;
	mdirCol.put(i, mdir);
      }
    }

    {
      Table tab("tTableMeasures_tmp.tab", Table::Update);
      // A column of MDirections
      MDirection::ROScalarColumn mdirCol(tab, "MDirColumn");
      if (mdirCol.measDesc().isRefCodeVariable()) {
	cout << "Error: reference is variable!\n";
      }
      cout << "Reading from MDirection column MDirColumn\n";
      for (uInt i=0; i<tabRows; i++) {
	cout << "retrieve: " << mdirCol(i) << endl;
      }
    }

    {
      Table tab("tTableMeasures_tmp.tab", Table::Update);
      cout << "Test of column TimeVarOffset...\n";
      cout << "A column of MEpoch where the reference and offset components";
      cout << " are variable.\n";
      // create meCol to use but test copy contructor and attach() at the
      // same time.
      MEpoch::ScalarColumn tmpMeCol;
      tmpMeCol.attach(tab, "Time1");
      MEpoch::ScalarColumn meCol = tmpMeCol;
      MEpoch offset;
      MEpoch me, mtmp;
      const MEpoch* offptr;

      offset.set(MVEpoch(1234));
      me.set(MVEpoch(51234.1), MEpoch::Ref(MEpoch::GMST1, offset));
      meCol.put(0, me);
      AlwaysAssertExit (meCol.isDefined(0));
      meCol.get(0, mtmp);
      AlwaysAssertExit (mtmp.getRef().getType() == MEpoch::GMST1);
      AlwaysAssertExit (near (mtmp.get("s"), me.get("s"), 1.e-10));
      offptr = dynamic_cast<const MEpoch*> (mtmp.getRef().offset());
      AlwaysAssertExit (offptr != 0);
      AlwaysAssertExit (near (offptr->get("s"), offset.get("s"), 1.e-10));

      offset.set(MVEpoch(1234.1));
      me.set(MVEpoch(51234.2), MEpoch::Ref(MEpoch::UTC, offset));
      meCol.put(1, me);
      AlwaysAssertExit (meCol.isDefined(1));
      meCol.get(1, mtmp);
      AlwaysAssertExit (mtmp.getRef().getType() == MEpoch::UTC);
      AlwaysAssertExit (near (mtmp.get("s"), me.get("s"), 1.e-10));
      offptr = dynamic_cast<const MEpoch*> (mtmp.getRef().offset());
      AlwaysAssertExit (offptr != 0);
      AlwaysAssertExit (near (offptr->get("s"), offset.get("s"), 1.e-10));

      offset.set(MVEpoch(1234.2));
      me.set(MVEpoch(51234.3), MEpoch::Ref(MEpoch::TAI, offset));
      meCol.put(2, me);
      AlwaysAssertExit (meCol.isDefined(2));
      meCol.get(2, mtmp);
      AlwaysAssertExit (mtmp.getRef().getType() == MEpoch::TAI);
      AlwaysAssertExit (near (mtmp.get("s"), me.get("s"), 1.e-10));
      offptr = dynamic_cast<const MEpoch*> (mtmp.getRef().offset());
      AlwaysAssertExit (offptr != 0);
      AlwaysAssertExit (near (offptr->get("s"), offset.get("s"), 1.e-10));

      offset.set(MVEpoch(1234.3));
      me.set(MVEpoch(51234.4), MEpoch::Ref(MEpoch::UTC, offset));
      meCol.put(3, me);	
      AlwaysAssertExit (meCol.isDefined(3));
      meCol.get(3, mtmp);
      AlwaysAssertExit (mtmp.getRef().getType() == MEpoch::UTC);
      AlwaysAssertExit (near (mtmp.get("s"), me.get("s"), 1.e-10));
      offptr = dynamic_cast<const MEpoch*> (mtmp.getRef().offset());
      AlwaysAssertExit (offptr != 0);
      AlwaysAssertExit (near (offptr->get("s"), offset.get("s"), 1.e-10));

      // put one in with no offset
      me.set(MVEpoch(51234.5), MEpoch::Ref(MEpoch::GMST1));
      meCol.put(4, me);
      offset.set(MVEpoch(0));
      AlwaysAssertExit (meCol.isDefined(4));
      meCol.get(4, mtmp);
      AlwaysAssertExit (mtmp.getRef().getType() == MEpoch::GMST1);
      AlwaysAssertExit (near (mtmp.get("s"), me.get("s"), 1.e-10));
      offptr = dynamic_cast<const MEpoch*> (mtmp.getRef().offset());
      AlwaysAssertExit (offptr != 0);
      AlwaysAssertExit (near (offptr->get("s"), offset.get("s"), 1.e-10));
	

      // Test of exception. Try putting a reference with a frame into
      // a column with variable references
      if (doExcep) {
	try {
	  MEpoch epoch_frame(Quantity(MeasData::MJDB1950,"d"));
	  MeasFrame frame(epoch_frame);
	  me.getRefPtr()->set(frame);
	  meCol.put(0, me);
	} catch (AipsError x) {
	  cout << "The following line should report an error ";
	  cout << "in ScalarMeasColumn::put - not allowed to put a ";
	  cout << "measure with a frame in variable column.\n";
	  cout << x.getMesg() << endl;
	} 
      }
    }

    Vector<MEpoch> ev(10);
    {
      Table tab("tTableMeasures_tmp.tab", Table::Update);
      cout << "Creating an MEpoch Array Column\n";
      MEpoch::ArrayColumn tmpArrCol;

      if (tmpArrCol.isNull()) {
	tmpArrCol.attach(tab, "Time1Arr");
      }
      tmpArrCol.throwIfNull();
      cout << "Null MEpochArrCol successfully attached\n";
      // no assignment operator but there is a copy constructor
      MEpoch::ArrayColumn arrayCol(tmpArrCol);

      MEpoch last(Quantity(13.45, "h"), MEpoch::Ref(MEpoch::TAI));
      for (uInt i=0; i<10; i++) {
	last.set(Quantity(13.45 + i, "h"));
	ev(i) = last;
      }

      cout << "Adding a vector to the column at row 0.\n";

      // before adding something check the isDefined() member
      if (!arrayCol.isDefined(0)) {
	cout << "PASS - nothing in the measure array column row yet\n";
      } else {
	cout << "FAIL - there shouldn't be a valid value in the row!\n";
      }
      arrayCol.put(0, ev);
      Vector<MEpoch> ew;
      arrayCol.get(0, ew, True);

      // now row 0 should contain a valid entry
      if (arrayCol.isDefined(0)) {
	cout << "PASS - valid entry in array column row 0\n";
      } else {
	cout << "FAIL - there should be something in row 0!\n";
      }
      for (uInt i=0; i<10; i++) {
	AlwaysAssertExit (ew(i).getRef().getType() == MEpoch::TAI);
	const MEpoch* offptr = dynamic_cast<const MEpoch*>
                                                   (ew(i).getRef().offset());
	AlwaysAssertExit (offptr != 0);
	AlwaysAssertExit (near (offptr->get("s"), mjdToday.get("s"), 1.e-10));
	MEpoch tmp = MEpoch::Convert (ew(i), MEpoch::TAI)();
	AlwaysAssertExit (near (tmp.get("s"), ev(i).get("s"), 1.e-10));
      }

      Vector<MEpoch> tm1 = arrayCol.convert (0, MEpoch::UTC);
      for (uInt i=0; i<10; i++) {
	AlwaysAssertExit (tm1(i).getRef().getType() == MEpoch::UTC);
	const MEpoch* offptr = dynamic_cast<const MEpoch*>
	                               (tm1(i).getRef().offset());
	AlwaysAssertExit (offptr == 0);
	MEpoch tmp = MEpoch::Convert (ev(i), MEpoch::UTC)();
	AlwaysAssertExit (near (tmp.get("s"), tm1(i).get("s"), 1.e-10));
      }

      Vector<MEpoch> tm2 = arrayCol.convert (0, tm1(0));
      for (uInt i=0; i<10; i++) {
	AlwaysAssertExit (tm2(i).getRef().getType() == MEpoch::UTC);
	const MEpoch* offptr = dynamic_cast<const MEpoch*>
	                               (tm2(i).getRef().offset());
	AlwaysAssertExit (offptr == 0);
	MEpoch tmp = MEpoch::Convert (ev(i), MEpoch::UTC)();
	AlwaysAssertExit (near (tmp.get("s"), tm2(i).get("s"), 1.e-10));
      }

      MPosition mpobs;
      MeasTable::Observatory(mpobs, "WSRT");
      MEpoch::Ref mref(MEpoch::LAST, MeasFrame(mpobs));
      Vector<MEpoch> tm3 = arrayCol.convert (0, mref);
      for (uInt i=0; i<10; i++) {
	AlwaysAssertExit (tm3(i).getRef().getType() == MEpoch::LAST);
	const MEpoch* offptr = dynamic_cast<const MEpoch*>
	                               (tm3(i).getRef().offset());
	AlwaysAssertExit (offptr == 0);
	MEpoch tmp = MEpoch::Convert (ev(i), mref)();
	AlwaysAssertExit (near (tmp.get("s"), tm3(i).get("s"), 1.e-10));
      }

      Vector<MEpoch> tm5 = arrayCol.convert (0, tm3(0));
      for (uInt i=0; i<10; i++) {
	AlwaysAssertExit (tm5(i).getRef().getType() == MEpoch::LAST);
	const MEpoch* offptr = dynamic_cast<const MEpoch*>
	                               (tm5(i).getRef().offset());
	AlwaysAssertExit (offptr == 0);
	AlwaysAssertExit (near (tm5(i).get("s"), tm3(i).get("s"), 1.e-10));
      }

      if (doExcep) {
	try {
	  Vector<Unit> u(1);
	  arrayCol.setDescUnits (u);
	} catch (AipsError x) {
	  cout << "The following line should report an error ";
	  cout << "in ScalarMeasColumn::setDescUnits - not allowed to put ";
	  cout << "when the table is not empty.\n";
	  cout << x.getMesg() << endl;
	} 
      }
    }

    {
      cout << "Open table again in RO mode to test ROArrayMeasColumn...\n";
      Table tab("tTableMeasures_tmp.tab", Table::Old);
      cout << "Creating an MEpoch Array Column\n";
      MEpoch::ROArrayColumn arrayCol(tab, "Time1Arr");	
      Vector<MEpoch> ew;
      arrayCol.get(0, ew, True);
      ew = arrayCol(0);
      for (uInt i=0; i<10; i++) {
	AlwaysAssertExit (ew(i).getRef().getType() == MEpoch::TAI);
	const MEpoch* offptr = dynamic_cast<const MEpoch*>
                                                   (ew(i).getRef().offset());
	AlwaysAssertExit (offptr != 0);
	AlwaysAssertExit (near (offptr->get("s"), mjdToday.get("s"), 1.e-10));
	MEpoch tmp = MEpoch::Convert (ew(i), MEpoch::TAI)();
	AlwaysAssertExit (near (tmp.get("s"), ev(i).get("s"), 1.e-10));
      }
    }

    {
      // a bunch of extra tests

      Table tab("tTableMeasures_tmp.tab", Table::Update);
      MEpoch::ArrayColumn arrMeasCol;
      arrMeasCol.attach(tab, "Time2Arr");

      // copy constructor
      MEpoch::ArrayColumn testCopy = arrMeasCol;

      MEpoch utcE(Quantity(1.45, "h"), MEpoch::Ref(MEpoch::UTC));
      MEpoch taiE(Quantity(1.45, "h"), MEpoch::Ref(MEpoch::TAI));
      Vector<MEpoch> inArr(10);
      for (uInt i=0; i<10; i++) {
	if (i%2 == 0) {
	  utcE.set(Quantity(11.45 + i, "h"));
	  utcE.setOffset(MEpoch(Quantity(12. - i, "h")));
	  inArr(i) = utcE;
	} else {
	  taiE.set(Quantity(13.45 + i, "h"));
	  taiE.setOffset(MEpoch(Quantity(15.6 - i, "h")));
	  inArr(i) = taiE;
	}
      }
      cout << "Adding vectors to the test measure column\n";
      for (uInt i=0; i<tabRows; i++) {
	testCopy.put(i, inArr);
      }

      // attach
      MEpoch::ROArrayColumn testAttach;
      testAttach.attach(tab, "Time2Arr");
      Vector<MEpoch> outArr;
      testAttach.get(0, outArr, True);
      const MEpoch* offptr;
      const MEpoch* offptrin;
      for (uInt i=0; i<10; i++) {
	AlwaysAssertExit (outArr(i).getRef().getType() ==
			                      inArr(i).getRef().getType());
	AlwaysAssertExit (near (outArr(i).get("s"), inArr(i).get("s"),
				1.e-10));
	offptr = dynamic_cast<const MEpoch*> (outArr(i).getRef().offset());
	AlwaysAssertExit (offptr != 0);
	offptrin = dynamic_cast<const MEpoch*> (inArr(i).getRef().offset());
	AlwaysAssertExit (near (offptr->get("s"), offptrin->get("s"),
				1.e-10));
      }

      // see if the reference is variable
      if (testCopy.measDesc().isRefCodeVariable()) {
	cout << "Reference for ArrayMeasCol is variable\n";
      } else {
	cout << "Reference for ArrayMeasCol is not variable\n";
      }

      // getRef for the column
      cout << "Reference for the ArrayMeasCol is: " << testCopy.getMeasRef();
      cout << endl;

      // stuff to increase line coverage of classes
      if (doExcep) {
	try {
	  // test throw if null exception
	  MEpoch::ArrayColumn nullCol;
	  nullCol.throwIfNull();
	} catch (AipsError x) {
	  cout << "The following line should be a ";
	  cout << "null column exception.\n";
	  cout << x.getMesg() << endl;
	} 
      }

      // array column with variable string references
      MEpoch::ArrayColumn varStrRefColtmp;
      varStrRefColtmp.attach(tab, "Time3Arr");
      MEpoch::ArrayColumn varStrRefCol = varStrRefColtmp;
      for (uInt i=0; i<tabRows; i++) {
	varStrRefCol.put(i, inArr);
      }
      varStrRefCol.get(0, outArr, True);
      for (uInt i=0; i<10; i++) {
	AlwaysAssertExit (outArr(i).getRef().getType() ==
			                      inArr(i).getRef().getType());
	offptrin = dynamic_cast<const MEpoch*> (inArr(i).getRef().offset());
	offptr = dynamic_cast<const MEpoch*> (outArr(i).getRef().offset());
	AlwaysAssertExit (offptr == 0);
	MEpoch tmp = MEpoch::Convert (outArr(i), inArr(i).getRef())();
	AlwaysAssertExit (near (tmp.get("s"), inArr(i).get("s"), 1.e-10));
      }

      // test putting of an empty array
      Vector<MEpoch> dummy;
      varStrRefCol.put(0, dummy);
      dummy.resize(1);
      varStrRefCol.get(0, dummy, True);
      AlwaysAssertExit (dummy.nelements() == 0);

      // last thing to test.  Array columns with scalar column offsets
      // and reference.  First test attach and copy constructor.
      MEpoch::ArrayColumn scaStrRefColtmp;
      scaStrRefColtmp.attach(tab, "Time4Arr");
      MEpoch::ArrayColumn scaStrRefCol = scaStrRefColtmp;

      // Only one reference and offset are stored per row.  The reference
      // and offset stored is taken from the first element of each
      // Measure array stored.
      for (uInt i=0; i<tabRows; i++) {
	scaStrRefCol.put(i, inArr);
      }
      scaStrRefCol.get(0, outArr, True);
      for (uInt i=0; i<10; i++) {
	AlwaysAssertExit (outArr(i).getRef().getType() ==
			                      inArr(0).getRef().getType());
	offptrin = dynamic_cast<const MEpoch*> (inArr(0).getRef().offset());
	offptr = dynamic_cast<const MEpoch*> (outArr(i).getRef().offset());
	AlwaysAssertExit (offptr != 0);
	AlwaysAssertExit (near (offptr->get("s"), offptrin->get("s"), 1.e-10));
	MEpoch tmp = MEpoch::Convert (outArr(i), inArr(i).getRef())();
	AlwaysAssertExit (near (tmp.get("s"), inArr(i).get("s"), 1.e-10));
      }

      // Check that the column can be accessed as a quantum.
      ROArrayQuantColumn<Double> qcol(tab, "Time4Arr");
      Vector<Quantum<Double> > q = qcol(0);
      for (uInt i=0; i<10; i++) {
	AlwaysAssertExit (near (outArr(i).get("d"), q(i), 1.e-10));
      }

      {
	// Resetting cannot be done, since the table is not empty.
	MEpoch::ArrayColumn arrayCol(tab, "Time1Arr");
	Bool excp = False;
	try {
	  arrayCol.setDescRefCode (MEpoch::TAI);
	} catch (AipsError) {
	  excp = True;
	}
	AlwaysAssertExit (excp);
	excp = False;
	try {
	  arrayCol.setDescOffset (obsTime);
	} catch (AipsError) {
	  excp = True;
	}
	AlwaysAssertExit (excp);
      }
      // One last thing to test...test array conformance exception
      if (doExcep) {
	try {
	  Array<MEpoch> badShapeArr(IPosition(2,2));
	  scaStrRefCol.get(0, badShapeArr, False);
	} catch (AipsError x) {
	  cout << "The following line should be a ";
	  cout << "Table array conformance error exception.\n";
	  cout << x.getMesg() << endl;
	} 
      }
    }

    // Do finally some performance checking.
    {
      Timer timer;
    }

    cout << "Test completed normally...bye.\n";
  } catch (AipsError x) {
    cout << "An error occurred.  The test ended early with the following";
    cout << " message:\n";
    cout << x.getMesg() << endl;
    exit(1);
  } 

  exit(0);
}
