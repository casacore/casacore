//# tTableRecord.cc: Test the TableRecord class
//# Copyright (C) 1995,1996,1999,2000,2001
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

#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// This program tests the TableRecord and TableRecordRep classes.
// Its expected output (which only consists of exception text)
// is stored in tTableRecord.out and can be checked using assay.
// This will also delete the temporary output file.
// <p>
// It can throw several exceptions, which may result in memory leaks.
// To check if no real memory leaks occur, the program can be run
// with an argument (e.g. tTableRecord 1). In that case no statements
// resulting in exceptions are executed, so no memory leaks should occur.
// <p>
// The ability to read old KeywordSet objects from a file has been tested
// in a separate program. That program is not checked into the system,
// because the KeywordSet classes are removed from it.


void check (const TableRecord&, Int intValue, uInt nrField);


// This function checks if a field name is correct.
// A name has to be > 0 characters and start with an uppercase.
// The extra argument should not be 10.
Bool nameCallBack (const String& name, DataType, const void* extraArgument,
		   String& message)
{
    if (name.length() < 1) {
	message = "length<1";
	return False;
    }
    if (name[0] < 'A'  ||  name[0] > 'Z') {
	message = "no uppercase";
	return False;
    }
    if (extraArgument != 0  &&  *(const Int*)extraArgument == 10) {
	message = "extra==10";
	return False;
    }
    return True;
}

// This function is doing define's and assign's in several ways.
// Many of them are incorrect and should result in an exception.
// TpArrayString2 has a fixed shape, while TpArrayString3's shape is variable.
// Note that assign always requires a matching shape, while a define
// only requires a matching shape for a fixed shape.
// It checks if the value is correct after each define/assign.
void doDefineAssign (const TableRecord& inrecord)
{
    TableRecord record (inrecord);
    RecordFieldPtr<Array<String> > rfstr2 (record, "TpArrayString2");
    RecordFieldPtr<Array<String> > rfstr3 (record, "TpArrayString3");
    Vector<String> vec;
    record.get (record.fieldNumber ("TpArrayString2"), vec);
    AlwaysAssertExit (allEQ (vec,
			     stringToVector ("abcd,ghi,jklmn")));
    record.get (record.fieldNumber ("TpArrayString3"), vec);
    AlwaysAssertExit (allEQ (vec,
			     stringToVector ("abc,dghij,klmn")));
    try {
	*rfstr2 = stringToVector ("abc");
    } catch (AipsError x) {
	cout << x.getMesg() << endl;           // incorrect shape
    } 
    try {
	*rfstr3 = stringToVector ("abc");
    } catch (AipsError x) {
	cout << x.getMesg() << endl;           // incorrect shape
    } 
    record.get (record.fieldNumber ("TpArrayString2"), vec);
    AlwaysAssertExit (allEQ (vec,
			     stringToVector ("abcd,ghi,jklmn")));
    record.get (record.fieldNumber ("TpArrayString3"), vec);
    AlwaysAssertExit (allEQ (vec,
			     stringToVector ("abc,dghij,klmn")));

    try {
	*rfstr2 = stringToVector ("abc");
    } catch (AipsError x) {
	cout << x.getMesg() << endl;           // incorrect shape
    } 
    try {
	*rfstr3 = stringToVector ("abc");
    } catch (AipsError x) {
	cout << x.getMesg() << endl;           // incorrect shape
    } 
    record.get (record.fieldNumber ("TpArrayString2"), vec);
    AlwaysAssertExit (allEQ (vec,
			     stringToVector ("abcd,ghi,jklmn")));
    record.get (record.fieldNumber ("TpArrayString3"), vec);
    AlwaysAssertExit (allEQ (vec,
			     stringToVector ("abc,dghij,klmn")));

    try {
	rfstr2.define (stringToVector ("abc"));
    } catch (AipsError x) {
	cout << x.getMesg() << endl;           // incorrect shape
    } 
    rfstr3.define (stringToVector ("a"));
    record.get (record.fieldNumber ("TpArrayString2"), vec);
    AlwaysAssertExit (allEQ (vec,
			     stringToVector ("abcd,ghi,jklmn")));
    vec.resize (1);
    record.get (record.fieldNumber ("TpArrayString3"), vec);
    AlwaysAssertExit (allEQ (vec,
			     stringToVector ("a")));
				 
    *rfstr2 = stringToVector ("a,b,c");
    *rfstr3 = stringToVector ("d");
    record.get (record.fieldNumber ("TpArrayString3"), vec);
    AlwaysAssertExit (allEQ (vec,
			     stringToVector ("d")));
    vec.resize (3);
    record.get (record.fieldNumber ("TpArrayString2"), vec);
    AlwaysAssertExit (allEQ (vec,
			     stringToVector ("a,b,c")));

    rfstr2.define (stringToVector ("g,h,i"));
    record.define (record.fieldNumber ("TpArrayString3"),
		   stringToVector ("j,k,l"));
    record.get (record.fieldNumber ("TpArrayString2"), vec);
    AlwaysAssertExit (allEQ (vec,
			     stringToVector ("g,h,i")));
    record.get (record.fieldNumber ("TpArrayString3"), vec);
    AlwaysAssertExit (allEQ (vec,
			     stringToVector ("j,k,l")));

    try {
        record.define ("TpBool", Vector<Bool>(2, False));
    } catch (AipsError x) {
        cout << x.getMesg() << endl;
    } 
}

void doSubRecord (Bool doExcp, const RecordDesc& desc)
{
    Int subField  = desc.fieldNumber ("SubRecord");
    Int subField1 = desc.fieldNumber ("SubRecord1");
    desc.subRecord (subField);
    TableRecord record(desc);
    RecordFieldPtr<TableRecord> sub (record, subField);
    RecordFieldPtr<TableRecord> sub1 (record, subField1);
    AlwaysAssertExit (! (*sub).conform (*sub1));
    // Add 2 fields, so now they are conforming.
    (*sub1).define ("f1", float(3));
    (*sub1).define ("i1", Int(2));
    AlwaysAssertExit ((*sub).conform (*sub1));

    // Create a copy of the record description and add the 2 fields
    // to SubRecord1.
    RecordDesc desc1(desc);
    RecordDesc& subDesc1  = desc1.rwSubRecord (subField1);
    subDesc1.addField ("fa", TpFloat);
    subDesc1.addField ("ia", TpInt);
    // Now create a record from that new description.
    // record conforms record1 and vice versa (because SubRecord1
    // is variable in record, but fixed in record1).
    TableRecord record1(desc1);
    AlwaysAssertExit (record.conform (record1));
    AlwaysAssertExit (record1.conform (record));

    // Test if the assignment works fine.
    record1 = record;
    AlwaysAssertExit (record1.subRecord(subField1).asfloat(0) == 3);
    AlwaysAssertExit (record1.subRecord(subField1).asInt(1) == 2);

    // Add another field to SubRecord1 in record.
    // This results in record1 not conforming record.
    // record still conforms record1, because its SubRecord1 is non-fixed.
    (*sub1).define ("i2", Int(2));
    AlwaysAssertExit (record.conform (record1));
    AlwaysAssertExit (! record1.conform (record));
    if (doExcp) {
	try {
	    record1 = record;
	} catch (AipsError x) {
	    cout << ">>> Instance-specific assertion error message:" << endl;
	    cout << x.getMesg() << endl;           // not conforming
	    cout << "<<<" << endl;
	} 
    }
    (*sub1).define (0, float(4));
    AlwaysAssertExit (record.subRecord(subField1).asfloat(0) == 4);
    record = record1;
    AlwaysAssertExit (record.subRecord(subField1).asfloat(0) == 3);
}

void doIt (Bool doExcp)
{
    // Create a record description with all types.
    Int extraArgument=0;
    RecordDesc rd;
    rd.addField ("TpBool", TpBool);
    rd.setComment (0, "comment for field TpBool");
    rd.addField ("TpUChar", TpUChar);
    rd.addField ("TpShort", TpShort);
    rd.addField ("TpInt", TpInt);
    rd.addField ("TpUInt", TpUInt);
    rd.addField ("TpFloat", TpFloat);
    rd.addField ("TpDouble", TpDouble);
    rd.addField ("TpComplex", TpComplex);
    rd.addField ("TpDComplex", TpDComplex);
    rd.addField ("TpString", TpString);
    rd.addField ("TpArrayBool", TpArrayBool, IPosition(1,1));
    rd.addField ("TpArrayUChar", TpArrayUChar, IPosition(1,1));
    rd.addField ("TpArrayShort", TpArrayShort, IPosition(1,1));
    rd.addField ("TpArrayInt", TpArrayInt, IPosition(1,1));
    rd.addField ("TpArrayUInt", TpArrayUInt, IPosition(1,1));
    rd.addField ("TpArrayFloat", TpArrayFloat, IPosition(1,1));
    rd.addField ("TpArrayDouble", TpArrayDouble, IPosition(1,1));
    rd.addField ("TpArrayComplex", TpArrayComplex, IPosition(1,1));
    rd.addField ("TpArrayDComplex", TpArrayDComplex, IPosition(1,1));
    rd.addField ("TpArrayString", TpArrayString);
    RecordDesc subDesc;
    rd.addField ("SubRecord1", subDesc);     // empty, thus arbitrary subrecord
    subDesc.addField ("SubFloat", TpFloat);
    subDesc.addField ("SubInt", TpInt);
    rd.addField ("SubRecord", subDesc);      // not empty, thus fixed subrecord

    //    TableRecord(const RecordDesc &description, ...);
    //    uInt nfields() const;
    //    const RecordDesc &description() const
    TableRecord record(rd, RecordInterface::Variable,
		       nameCallBack, &extraArgument);
    AlwaysAssertExit (record.comment ("TpBool") == "comment for field TpBool");
    record.setComment ("TpBool", "comment for TpBool");
    AlwaysAssertExit (record.comment ("TpBool") == "comment for TpBool");
    AlwaysAssertExit(record.nfields() == rd.nfields() &&
		     record.nfields() == 22);
    AlwaysAssertExit(record.description() == rd);

    // Test renameField.
    record.renameField ("newname", "TpInt");
    AlwaysAssertExit (! record.isDefined ("TpInt"));
    AlwaysAssertExit (record.fieldNumber ("newname") == 3);
    record.renameField ("TpInt", "newname");

    // Do some incorrect addField's.
    if (doExcp) {
	try {
	    record.define ("", (Int)0);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;           // empty name
	} 
	try {
	    record.define ("aB", (Int)0);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;           // first no uppercase
	} 
	extraArgument = 10;
	try {
	    record.define ("A", (Int)0);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;           // extra argument = 10
	} 
	extraArgument = 0;
	try {
	    record.define ("TpShort", (Int)0);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;           // invalid type
	} 
    }
    AlwaysAssertExit(record.nfields() == rd.nfields() &&
		     record.nfields() == 22);
    AlwaysAssertExit(record.description() == rd);

    //    void define (const String& name, value);
    //    void define (const String& name, value, Bool fixedShape);
    record.define ("TpBool2", False);
    rd.addField ("TpBool2a", TpBool);
    record.define ("TpUChar2", uChar(1));
    rd.addField ("TpUChar2a", TpUChar);
    record.define ("TpShort2", Short(2));
    rd.addField ("TpShort2a", TpShort);
    record.define ("TpInt2", Int(3));
    rd.addField ("TpInt2a", TpInt);
    record.define ("TpUInt2", uInt(4));
    rd.addField ("TpUInt2a", TpUInt);
    record.define ("TpFloat2", Float(5));
    rd.addField ("TpFloat2a", TpFloat);
    record.define ("TpDouble2", Double(6));
    rd.addField ("TpDouble2a", TpDouble);
    record.define ("TpComplex2", Complex(7,8));
    rd.addField ("TpComplex2a", TpComplex);
    record.define ("TpDComplex2", DComplex(9,10));
    rd.addField ("TpDComplex2a", TpDComplex);
    record.define ("TpArrayString2", stringToVector("abcd,ghi,jklmn"), True);
    rd.addField ("TpArrayString2a", TpArrayString, IPosition(1,3));
    record.define ("TpArrayString3", stringToVector("abc,dghij,klmn"));
    rd.addField ("TpArrayString3a", TpArrayString);
    record.define ("TpString2", "abc");
    rd.addField ("TpString2a", TpString);

    // Define a scalar using an array.
    AlwaysAssertExit (record.asInt("TpInt2") == 3);
    AlwaysAssertExit (record.asuInt("TpUInt2") == 4);
    AlwaysAssertExit (allEQ (record.asArrayuInt("TpUInt2"), uInt(4)));
    record.define ("TpInt2", Vector<Int>(1,6));
    AlwaysAssertExit (record.asInt("TpInt2") == 6);
    AlwaysAssertExit (allEQ (record.asArrayInt("TpInt2"), 6));
    record.define ("TpUInt2", uInt(10));
    AlwaysAssertExit (record.asuInt("TpUInt2") == 10);
    AlwaysAssertExit (allEQ (record.asArrayuInt("TpUInt2"), uInt(10)));
    record.define ("TpInt2", 3);
    record.define ("TpUInt2", Vector<uInt>(1,4u));
    AlwaysAssertExit (record.asInt("TpInt2") == 3);
    AlwaysAssertExit (allEQ (record.asArrayInt("TpInt2"), 3));
    AlwaysAssertExit (record.asuInt("TpUInt2") == 4);
    AlwaysAssertExit (allEQ (record.asArrayuInt("TpUInt2"), uInt(4)));

    // Do some erronous defines and assigns.
    if (doExcp) {
	doDefineAssign (record);
    }
    
    //    TableRecord();
    //    void restructure(const RecordDesc &newDescription);
    // Also check that a RecordFieldPtr gets detached.
    TableRecord record2;
    record2.restructure(subDesc);         // non-fixed -> possible
    RecordFieldPtr<Int> fld2(record2, 1);
    AlwaysAssertExit (fld2.isAttached());
    record2.restructure(subDesc);         // non-fixed -> possible and detaches
    AlwaysAssertExit (! fld2.isAttached());

    // restructure and operator= fail on a non-empty, fixed record.
    TableRecord record2a(RecordInterface::Fixed);
    if (doExcp) {
	try {
	    record2a.restructure(subDesc);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;  // fixed, not empty ->impossible
	} 
    }
    record2 = record;                     // non-fixed -> possible
    record2a = record;                    // same structure -> possible
    TableRecord record2b (subDesc);
    if (doExcp) {
	try {
	    record2a = record2b;
	} catch (AipsError x) {
	    cout << ">>> Instance-specific assertion error message:" << endl;
	    cout << x.getMesg() << endl;  // fixed; non-conforming
	    cout << "<<<" << endl;
	} 
    }
    
    //    TableRecord(const TableRecord &other);
    //    Bool conform(const TableRecord &other);
    TableRecord record3(record2);
    TableRecord record4(record2.description());
    record4 = record3;
    AlwaysAssertExit(rd == record2.description() &&
		     rd == record3.description() &&
		     rd == record4.description());
    AlwaysAssertExit(record.conform(record2));
    AlwaysAssertExit(record.conform(record2a));
    TableRecord record5;
    AlwaysAssertExit(! record.conform(record5));

    // Scalar fields
    RecordFieldPtr<Bool>     boolField(record, 0);
    RecordFieldPtr<uChar>    ucharField(record, 1);
    RecordFieldPtr<Short>    shortField(record, 2);
    RecordFieldPtr<Int>      intField(record, 3);
    RecordFieldPtr<uInt>     uintField(record, 4);
    RecordFieldPtr<Float>    floatField(record, 5);
    RecordFieldPtr<Double>   doubleField(record, 6);
    RecordFieldPtr<Complex>  complexField(record, 7);
    RecordFieldPtr<DComplex> dcomplexField(record, 8);
    RecordFieldPtr<String>   stringField(record, 9);
    //    RecordFieldPtr(TableRecord &record, uInt whichField);
    //    T &operator*()
    //    const T &operator*() const
    //    define (const T& value)
    *boolField = True;
    *ucharField = 255;
    AlwaysAssertExit(*((const RecordFieldPtr<uChar> &)ucharField) == 255);
    *shortField = 32767;
    *intField = -1234567;
    uintField.define (1234567);
    *floatField = 7.0f;
    *doubleField = 9.0;
    *complexField = Complex(1.0f, 11.0f);
    *dcomplexField = Complex(5.0, 1.0);
    *stringField = "Hello";

    // Array fields
    RecordFieldPtr<Array<Bool> >     arrayboolField(record, 10);
    RecordFieldPtr<Array<uChar> >    arrayucharField(record, 11);
    RecordFieldPtr<Array<Short> >    arrayshortField(record, 12);
    RecordFieldPtr<Array<Int> >      arrayintField(record, 13);
    RecordFieldPtr<Array<uInt> >     arrayuintField(record, 14);
    RecordFieldPtr<Array<Float> >    arrayfloatField(record, 15);
    RecordFieldPtr<Array<Double> >   arraydoubleField(record, 16);
    RecordFieldPtr<Array<Complex> >  arraycomplexField(record, 17);
    RecordFieldPtr<Array<DComplex> > arraydcomplexField(record, 18);
    RecordFieldPtr<Array<String> >   arraystringField(record, 19);
    arrayboolField.setComment ("comment for TpArrayBool");
    *arrayboolField = True;
    *arrayucharField = 255;
    *arrayshortField = 32767;
    *arrayintField = -1234567;
    *arrayuintField = 1234567;
    *arrayfloatField = 7.0f; 
    *arraydoubleField = 9.0; 
    *arraycomplexField = Complex(1.0f, 11.0f);
    *arraydcomplexField = DComplex(5.0, 1.0);
    *arraystringField = stringToVector ("Hello,Goodbye");

    // Sub-record fields
    if (doExcp) {
	try {
	    RecordFieldPtr<Record> fld(record, "SubRecord");
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;           // invalid type
	} 
    }
    RecordFieldPtr<TableRecord> recordField(record, "SubRecord");
    TableRecord& subrec = *recordField;
    AlwaysAssertExit(subrec.description() == subDesc);
    RecordFieldPtr<Float> subref (subrec, 0);
    *subref = 9.0;

    // TableRecord& rwSubRecord (Int whichField);
    // defineRecordField (const String& name, const TableRecord&);
    // RecordFieldPtr::define (const TableRecord&);
    // RecordFieldPtr::operator= (const TableRecord&);
    TableRecord& subrec1 = record.rwSubRecord
	                                  (record.fieldNumber ("SubRecord1"));
    TableRecord subrec1a;
    subrec1.defineRecord ("sub", subrec, RecordInterface::Fixed);
    subrec1.defineRecord ("sub1", subrec1a);
    subrec1.defineRecord ("sub2", subrec1a);
    RecordFieldPtr<TableRecord> sub1 (subrec1, 1);
    RecordFieldPtr<TableRecord> sub2 (subrec1, 2);
    *subref = 6.0;
    sub1.define (subrec);
    *subref = 8.0;
    *sub2 = subrec;

    // Check if the entire record is correct.
    check (record, -1234567, 34);

    // Now make a copy of the record and assign a value via RecordFieldPtr.
    // This has to result in a copy(-on-write) operation.
    TableRecord savrec2(record);
    check (savrec2, -1234567, 34);
    *intField += 1;
    *arrayintField = -1234566;
    check (savrec2, -1234567, 34);
    check (record, -1234566, 34);
    savrec2 = record;
    check (savrec2, -1234566, 34);

    // Change some more fields and check if the original is kept intact
    // (thus if copy-on-write works fine). This also checks if
    // reacquiring the RecordFieldPtr pointers after a copy works fine.
    TableRecord savrec2a(savrec2);
    RecordFieldPtr<Int> savrf (savrec2, 3);
    RecordFieldPtr<Array<Int> > savrfarray (savrec2, 13);
    savrf.define (savrf.get() + 11);
    *savrfarray = *savrf;
    check (savrec2, -1234555, 34);
    check (savrec2a, -1234566, 34);
    check (record, -1234566, 34);

    // Add some fields.
    // Check if removing a field results in deattaching and in
    // decrementing the field number.
    record.define ("TpString3", "abcd");
    record.define ("TpString4", "efghij");
    check (record, -1234566, 36);
    TableRecord savrec3(record);
    check (savrec3, -1234566, 36);
    RecordFieldPtr<String> tpstring2 (record, "TpString2");
    RecordFieldPtr<String> tpstring3 (record, "TpString3");
    RecordFieldPtr<String> tpstring4 (record, "TpString4");
    record.removeField (record.fieldNumber("TpString3"));
    AlwaysAssertExit (tpstring2.isAttached());
    AlwaysAssertExit (! tpstring3.isAttached());
    AlwaysAssertExit (tpstring4.isAttached());
    AlwaysAssertExit (tpstring2.fieldNumber() == 33);
    AlwaysAssertExit (tpstring3.fieldNumber() == -1);
    AlwaysAssertExit (tpstring4.fieldNumber() == 34);
    record.removeField (record.fieldNumber("TpString4"));
    check (savrec3, -1234566, 36);
    check (record, -1234566, 34);

    // OK, we've tested the TableRecord members, now test the remaining 
    // RecordFieldPtr members.
    //    RecordFieldPtr();
    //    void attachToRecord(TableRecord &record, uInt whichField);
    //    virtual Bool isAttached()
    RecordFieldPtr<uChar> ucharField2;
    AlwaysAssertExit(! ucharField2.isAttached());
    ucharField2.attachToRecord(record, 1);
    AlwaysAssertExit(*ucharField2 == *ucharField &&
		     ucharField2.isAttached());
    *ucharField = 99;
    AlwaysAssertExit(*ucharField2 == 99);
    //    RecordFieldPtr(const RecordFieldPtr<T> &other);
    RecordFieldPtr<uChar> ucharField3(ucharField);
    AlwaysAssertExit(*ucharField3 == *ucharField2 &&
		     ucharField3.isAttached());
    //    RecordFieldPtr<T> &operator=(const RecordFieldPtr<T> &other);
    RecordFieldPtr<uChar> ucharField4;
    ucharField4 = ucharField;
    AlwaysAssertExit(*ucharField4 == *ucharField3 &&
		     ucharField4.isAttached());
    *ucharField4 = 44;
    AlwaysAssertExit(*ucharField == 44);
    //    void detach();
    ucharField4.detach();
    AlwaysAssertExit(! ucharField4.isAttached());
    RecordDesc rd2;
    rd2.addField("foo", TpInt);
    TableRecord *record6 = new TableRecord(rd2);
    RecordFieldPtr<Int> rf1(*record6, 0);
    RecordFieldPtr<Int> rf2(*record6, 0);
    //    virtual void notify(const Notice &message); // implicit
    delete record6;
    AlwaysAssertExit(! rf1.isAttached());
    AlwaysAssertExit(! rf2.isAttached());

    // Check subRecord conformance.
    doSubRecord (doExcp, rd);

    *ucharField= 255;
    AipsIO aos ("tTableRecord_tmp.data", ByteIO::New);
    aos << record;
    aos.close();
    aos.open ("tTableRecord_tmp.data");
    aos >> record5;
    AlwaysAssertExit(record5.conform(record));
    check (record5, -1234566, 34);

    // Check removing a sub record.
    record5.defineRecord ("abcd", record);
    check (record5, -1234566, 35);
    record5.removeField (record5.fieldNumber("abcd"));
    check (record5, -1234566, 34);
    
    //    ~TableRecord()          // implicit
    //    ~RecordFieldPtr(); // implicit
}


// Check if the values in the record and subrecord are correct.
// The number of fields and the value of the Int fields can vary,
// so they are given as arguments.
void check (const TableRecord& record, Int intValue, uInt nrField)
{
    AlwaysAssertExit (record.nfields() == nrField);
    RORecordFieldPtr<Bool>     boolField(record, 0);
    RORecordFieldPtr<uChar>    ucharField(record, 1);
    RORecordFieldPtr<Short>    shortField(record, 2);
    RORecordFieldPtr<Int>      intField(record, 3);
    RORecordFieldPtr<uInt>     uintField(record, 4);
    RORecordFieldPtr<Float>    floatField(record, 5);
    RORecordFieldPtr<Double>   doubleField(record, 6);
    RORecordFieldPtr<Complex>  complexField(record, 7);
    RORecordFieldPtr<DComplex> dcomplexField(record, 8);
    RORecordFieldPtr<String>   stringField(record, 9);
    //    RORecordFieldPtr(TableRecord &record, uInt whichField);
    //    const T &operator*() const {return *field_ptr_p;}
    AlwaysAssertExit(boolField.comment() == "comment for TpBool");
    AlwaysAssertExit(*boolField == True);
    AlwaysAssertExit(*ucharField == 255);
    AlwaysAssertExit(*shortField == 32767);
    AlwaysAssertExit(intField.get() == intValue);
    AlwaysAssertExit(uintField.get() == 1234567);
    AlwaysAssertExit(*floatField == 7.0f);
    AlwaysAssertExit(*doubleField == 9.0);
    AlwaysAssertExit(*complexField == Complex(1.0f, 11.0f));
    AlwaysAssertExit(*dcomplexField == DComplex(5.0, 1.0));
    AlwaysAssertExit(*stringField == "Hello");

    Bool bv;
    uChar ucv;
    Short sv;
    Int iv;
    uInt uiv;
    Float fv;
    Double dv;
    Complex cv;
    DComplex dcv;
    String strv;
    //    TableRecord::get (T& value) const;
    record.get (22, bv);
    record.get (23, ucv);
    record.get (24, sv);
    record.get (25, iv);
    record.get (26, uiv);
    record.get (27, fv);
    record.get (28, dv);
    record.get (29, cv);
    record.get (30, dcv);
    record.get (33, strv);
    AlwaysAssertExit(bv == False);
    AlwaysAssertExit(ucv == 1);
    AlwaysAssertExit(sv == 2);
    AlwaysAssertExit(iv == 3);
    AlwaysAssertExit(uiv == 4);
    AlwaysAssertExit(fv == 5);
    AlwaysAssertExit(dv == 6);
    AlwaysAssertExit(cv == Complex(7,8));
    AlwaysAssertExit(dcv == DComplex(9,10));
    AlwaysAssertExit(strv == "abc");
    AlwaysAssertExit (allEQ (record.asArrayBool(22), bv));
    AlwaysAssertExit (allEQ (record.asArrayuChar(23), ucv));
    AlwaysAssertExit (allEQ (record.asArrayShort(24), sv));
    AlwaysAssertExit (allEQ (record.asArrayInt(25), iv));
    AlwaysAssertExit (allEQ (record.asArrayuInt(26), uiv));
    AlwaysAssertExit (allEQ (record.asArrayfloat(27), fv));
    AlwaysAssertExit (allEQ (record.asArraydouble(28), dv));
    AlwaysAssertExit (allEQ (record.asArrayComplex(29), cv));
    AlwaysAssertExit (allEQ (record.asArrayDComplex(30), dcv));
    AlwaysAssertExit (allEQ (record.asArrayString(33), strv));

    // Scalars as Arrays.
    RORecordFieldPtr<Array<Bool> >     boolFieldA(record, 0);
    RORecordFieldPtr<Array<uChar> >    ucharFieldA(record, 1);
    RORecordFieldPtr<Array<Short> >    shortFieldA(record, 2);
    RORecordFieldPtr<Array<Int> >      intFieldA(record, 3);
    RORecordFieldPtr<Array<uInt> >     uintFieldA(record, 4);
    RORecordFieldPtr<Array<Float> >    floatFieldA(record, 5);
    RORecordFieldPtr<Array<Double> >   doubleFieldA(record, 6);
    RORecordFieldPtr<Array<Complex> >  complexFieldA(record, 7);
    RORecordFieldPtr<Array<DComplex> > dcomplexFieldA(record, 8);
    RORecordFieldPtr<Array<String> >   stringFieldA(record, 9);
    AlwaysAssertExit (allEQ (*boolFieldA, Vector<Bool>(1, *boolField)));
    AlwaysAssertExit (allEQ (*ucharFieldA, Vector<uChar>(1, *ucharField)));
    AlwaysAssertExit (allEQ (*shortFieldA, Vector<Short>(1, *shortField)));
    AlwaysAssertExit (allEQ (*intFieldA, Vector<Int>(1, *intField)));
    AlwaysAssertExit (allEQ (*uintFieldA, Vector<uInt>(1, *uintField)));
    AlwaysAssertExit (allEQ (*floatFieldA, Vector<Float>(1, *floatField)));
    AlwaysAssertExit (allEQ (*doubleFieldA, Vector<Double>(1, *doubleField)));
    AlwaysAssertExit (allEQ (*complexFieldA, Vector<Complex>(1, *complexField)));
    AlwaysAssertExit (allEQ (*dcomplexFieldA, Vector<DComplex>(1, *dcomplexField)));
    AlwaysAssertExit (allEQ (*stringFieldA, Vector<String>(1, *stringField)));

    // Array fields
    RORecordFieldPtr<Array<Bool> >     arrayboolField(record, 10);
    RORecordFieldPtr<Array<uChar> >    arrayucharField(record, 11);
    RORecordFieldPtr<Array<Short> >    arrayshortField(record, 12);
    RORecordFieldPtr<Array<Int> >      arrayintField(record, 13);
    RORecordFieldPtr<Array<uInt> >     arrayuintField(record, 14);
    RORecordFieldPtr<Array<Float> >    arrayfloatField(record, 15);
    RORecordFieldPtr<Array<Double> >   arraydoubleField(record, 16);
    RORecordFieldPtr<Array<Complex> >  arraycomplexField(record, 17);
    RORecordFieldPtr<Array<DComplex> > arraydcomplexField(record, 18);
    RORecordFieldPtr<Array<String> >   arraystringField(record, 19);
    AlwaysAssertExit(arrayboolField.comment() == "comment for TpArrayBool");
    AlwaysAssertExit(allEQ(*arrayboolField, *boolField));
    AlwaysAssertExit(allEQ(*arrayucharField, *ucharField));
    AlwaysAssertExit(allEQ(*arrayshortField, *shortField));
    AlwaysAssertExit(allEQ(*arrayintField, *intField));
    AlwaysAssertExit(allEQ(*arrayuintField, *uintField));
    AlwaysAssertExit(allEQ(*arrayfloatField, *floatField));
    AlwaysAssertExit(allEQ(*arraydoubleField, *doubleField));
    AlwaysAssertExit(allEQ(*arraycomplexField, *complexField));
    AlwaysAssertExit(allEQ(*arraydcomplexField, *dcomplexField));
    AlwaysAssertExit(allEQ(*arraystringField,
			   stringToVector("Hello,Goodbye")));

    // Sub(-sub)-record fields
    RORecordFieldPtr<TableRecord> recordField(record, "SubRecord");
    const TableRecord& subrec = *recordField;
    AlwaysAssertExit(subrec.nfields() == 2);
    RORecordFieldPtr<Float> subref (subrec, 0);
    AlwaysAssertExit(*subref == 8.0);

    RORecordFieldPtr<Float> subref2 (record.subRecord
                                       (record.fieldNumber ("SubRecord")), 0);
    AlwaysAssertExit(*subref2 == 8.0);

    RORecordFieldPtr<TableRecord> recordField1(record, "SubRecord1");
    const TableRecord& subrec1 = *recordField1;
    AlwaysAssertExit(! subrec1.isFixed());
    AlwaysAssertExit(subrec1.nfields() == 3);

    RORecordFieldPtr<TableRecord> sub(subrec1, "sub");
    AlwaysAssertExit((*sub).isFixed());
    AlwaysAssertExit((*sub).nfields() == 2);
    RORecordFieldPtr<Float> subrefa (*sub, 0);
    AlwaysAssertExit(*subrefa == 9.0);

    RORecordFieldPtr<TableRecord> sub1(subrec1, "sub1");
    AlwaysAssertExit(! (*sub1).isFixed());
    AlwaysAssertExit((*sub1).nfields() == 2);
    AlwaysAssertExit((*sub1).asfloat(0) == 6.0);

    RORecordFieldPtr<TableRecord> sub2(subrec1, "sub2");
    AlwaysAssertExit(! (*sub2).isFixed());
    AlwaysAssertExit((*sub2).nfields() == 2);
    AlwaysAssertExit((*sub2).asdouble("SubFloat") == 8.0);
}

// Test Table specific things.
void testTable (Bool doExcp)
{
    // Create a table description and table.
    TableDesc td1 ("td1", TableDesc::Scratch);
    td1.addColumn (ScalarColumnDesc<Int> ("col1"));
    TableDesc td2 ("td2", TableDesc::Scratch);
    td2.addColumn (ScalarColumnDesc<float> ("col1"));
    td2.addColumn (ScalarColumnDesc<float> ("col2"));
    SetupNewTable newtab1 ("tTableRecord_tmp.tab1", td1, Table::New);
    Table tab1 (newtab1, 10);
    SetupNewTable newtab2 ("tTableRecord_tmp.tab2", td2, Table::New);
    Table tab2 (newtab2, 20);
    RecordDesc rd1;
    rd1.addTable ("tab1", "td1");
    rd1.addField ("tab2", TpTable);
    TableRecord rec1 (rd1, RecordInterface::Variable);
    rec1.defineTable (rec1.fieldNumber("tab1"), tab1);
    rec1.defineTable (rec1.fieldNumber("tab2"), tab1);
    Table t1 = rec1.asTable (rec1.fieldNumber("tab1"));
    AlwaysAssertExit (t1.nrow() == 10  &&  t1.tableDesc().ncolumn() == 1);
    Table t2 = rec1.asTable (rec1.fieldNumber("tab2"));
    AlwaysAssertExit (t2.nrow() == 10  &&  t2.tableDesc().ncolumn() == 1);
    if (doExcp) {
	try {
	    rec1.defineTable (rec1.fieldNumber("tab1"), tab2);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;           // non-conforming
	} 
	try {
	    RecordFieldPtr<Table> fld1 (rec1, "tab1");
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;           // invalid type
	} 
    }    
    rec1.defineTable (rec1.fieldNumber("tab2"), tab2);
    Table t3 = rec1.asTable (rec1.fieldNumber("tab2"));
    AlwaysAssertExit (t3.nrow() == 20  &&  t3.tableDesc().ncolumn() == 2);
    AipsIO aos ("tTableRecord_tmp.data", ByteIO::New);
    aos << rec1;

    // Copy constructor
    // defineTable
    // conform
    TableRecord rec2(rec1);
    AlwaysAssertExit (rec1.conform(rec2));
    rec2.defineTable ("tab1a", t3, RecordInterface::Fixed);
    AlwaysAssertExit (! rec1.conform(rec2));    // nfields() differ
    AlwaysAssertExit (! rec2.conform(rec1));    // nfields() differ
    rec2.removeField (1);
    AlwaysAssertExit (rec1.conform(rec2));      // second table type is empty
    AlwaysAssertExit (rec2.conform(rec1));      // second table type matches
    rec1.removeField(1);
    rec1.defineTable ("tab1a", t1);
    AlwaysAssertExit (rec1.conform(rec2));      // second table type is var.
    AlwaysAssertExit (! rec2.conform(rec1));    // second table type mismatches

    // Constructor from RecordInterface
    // The first one should dynamic cast itself to a TableRecord,
    TableRecord rec3((const RecordInterface&)rec1);
    AlwaysAssertExit (rec1.conform(rec3));
    Record rec4a;
    rec4a.define ("fld1", 1.);
    TableRecord rec4(rec4a);
    AlwaysAssertExit (rec4.nfields() == 1);
    AlwaysAssertExit (rec4.asDouble("fld1") == 1.);
}

void testTable2 (Bool)
{
    TableRecord rec1;
    AipsIO aos ("tTableRecord_tmp.data");
    aos >> rec1;
    {
	Table t1 = rec1.asTable (rec1.fieldNumber("tab1"));
	AlwaysAssertExit (t1.nrow() == 10  &&  t1.tableDesc().ncolumn() == 1);
	Table t3 = rec1.asTable (rec1.fieldNumber("tab2"));
	AlwaysAssertExit (t3.nrow() == 20  &&  t3.tableDesc().ncolumn() == 2);
    }
    rec1.closeTable ("tab1");
    rec1.closeTable ("tab2");
    rec1.closeTable ("tab2");
    Table t1 = rec1.asTable (rec1.fieldNumber("tab1"));
    AlwaysAssertExit (t1.nrow() == 10  &&  t1.tableDesc().ncolumn() == 1);
    //  defineTable()
    rec1.defineTable ("tab1a", t1);
    AlwaysAssertExit (rec1.nfields() == 3);
}


int main (int argc, const char*[])
{
    try {
	doIt ( (argc<2));
	testTable ( (argc<2));
	testTable2 ( (argc<2));
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    cout << "OK" << endl;
    return 0;                           // exit with success status
}
