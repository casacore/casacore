//# tRecord.cc: Test the Record class
//# Copyright (C) 1995,1996,1997,1998
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

#include <aips/Containers/Record.h>
#include <aips/Containers/RecordField.h>
#include <aips/Exceptions/Error.h>
#include <aips/Utilities/Assert.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayUtil.h>
#include <aips/IO/AipsIO.h>
#include <iostream.h>

// This program tests the Record and RecordRep classes.
// Its expected output (which only consists of exception text)
// is stored in tRecord.out and can be checked using assay.
// This will also delete the temporary output file.
// <p>
// It can throw several exceptions, which may result in memory leaks.
// To check if no real memory leaks occur, the program can be run as
// with an argument (e.g. tRecord 1). In that case no statements
// resulting in exceptions are executed, so no memory leaks should occur.
// <p>
// The ability to read old KeywordSet objects from a file has been tested
// in a separate program. That program is not checked into the system,
// because the KeywordSet classes are removed from it.


void check (const Record&, Int intValue, uInt nrField);
void doIt (Bool doExcp);

main (int argc)
{
    try {
	doIt (ToBool (argc<2));
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } end_try;
    cout << "OK" << endl;
    return 0;                           // exit with success status
}

// This function checks if a field name is correct.
// A name has be > 0 characters and start with an uppercase.
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
void doDefineAssign (const Record& inrecord)
{
    Record record (inrecord);
    RecordFieldPtr<Array<String> > rfstr2 (record, "TpArrayString2");
    RecordFieldPtr<Array<String> > rfstr3 (record, "TpArrayString3");
    Vector<String> vec;
    record.get (record.fieldNumber ("TpArrayString2"), vec);
    AlwaysAssertExit (allEQ (vec.ac(),
			     stringToVector ("abcd,ghi,jklmn").ac()));
    record.get (record.fieldNumber ("TpArrayString3"), vec);
    AlwaysAssertExit (allEQ (vec.ac(),
			     stringToVector ("abc,dghij,klmn").ac()));
    try {
	*rfstr2 = stringToVector ("abc");
    } catch (AipsError x) {
	cout << x.getMesg() << endl;           // incorrect shape
    } end_try;
    try {
	*rfstr3 = stringToVector ("abc");
    } catch (AipsError x) {
	cout << x.getMesg() << endl;           // incorrect shape
    } end_try;
    record.get (record.fieldNumber ("TpArrayString2"), vec);
    AlwaysAssertExit (allEQ (vec.ac(),
			     stringToVector ("abcd,ghi,jklmn").ac()));
    record.get (record.fieldNumber ("TpArrayString3"), vec);
    AlwaysAssertExit (allEQ (vec.ac(),
			     stringToVector ("abc,dghij,klmn").ac()));

    try {
	*rfstr2 = stringToVector ("abc");
    } catch (AipsError x) {
	cout << x.getMesg() << endl;           // incorrect shape
    } end_try;
    try {
	*rfstr3 = stringToVector ("abc");
    } catch (AipsError x) {
	cout << x.getMesg() << endl;           // incorrect shape
    } end_try;
    record.get (record.fieldNumber ("TpArrayString2"), vec);
    AlwaysAssertExit (allEQ (vec.ac(),
			     stringToVector ("abcd,ghi,jklmn").ac()));
    record.get (record.fieldNumber ("TpArrayString3"), vec);
    AlwaysAssertExit (allEQ (vec.ac(),
			     stringToVector ("abc,dghij,klmn").ac()));

    try {
	rfstr2.define (stringToVector ("abc"));
    } catch (AipsError x) {
	cout << x.getMesg() << endl;           // incorrect shape
    } end_try;
    rfstr3.define (stringToVector ("a"));
    record.get (record.fieldNumber ("TpArrayString2"), vec);
    AlwaysAssertExit (allEQ (vec.ac(),
			     stringToVector ("abcd,ghi,jklmn").ac()));
    vec.resize (1);
    record.get (record.fieldNumber ("TpArrayString3"), vec);
    AlwaysAssertExit (allEQ (vec.ac(),
			     stringToVector ("a").ac()));
				 
    *rfstr2 = stringToVector ("a,b,c");
    *rfstr3 = stringToVector ("d");
    record.get (record.fieldNumber ("TpArrayString3"), vec);
    AlwaysAssertExit (allEQ (vec.ac(),
			     stringToVector ("d").ac()));
    vec.resize (3);
    record.get (record.fieldNumber ("TpArrayString2"), vec);
    AlwaysAssertExit (allEQ (vec.ac(),
			     stringToVector ("a,b,c").ac()));

    rfstr2.define (stringToVector ("g,h,i"));
    record.define (record.fieldNumber ("TpArrayString3"),
		   stringToVector ("j,k,l"));
    record.get (record.fieldNumber ("TpArrayString2"), vec);
    AlwaysAssertExit (allEQ (vec.ac(),
			     stringToVector ("g,h,i").ac()));
    record.get (record.fieldNumber ("TpArrayString3"), vec);
    AlwaysAssertExit (allEQ (vec.ac(),
			     stringToVector ("j,k,l").ac()));
}

void doSubRecord (Bool doExcp, const RecordDesc& desc)
{
    Int subField  = desc.fieldNumber ("SubRecord");
    Int subField1 = desc.fieldNumber ("SubRecord1");
    Record record(desc);
    RecordFieldPtr<Record> sub (record, subField);
    RecordFieldPtr<Record> sub1 (record, subField1);
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
    Record record1(desc1);
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
	} catch (AipsError x) {                    // not conforming
	    cout << ">>> Instance-specific assertion error message:" << endl
		 << x.getMesg() << endl
		 << "<<<" << endl;
	} end_try;
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

    //    Record(const RecordDesc &description, ...);
    //    uInt nfields() const;
    //    const RecordDesc &description() const
    Record record(rd, RecordInterface::Variable, nameCallBack, &extraArgument);
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

    // Do some incorrect add's.
    if (doExcp) {
	try {
	    record.define (record.nfields()+1, (Int)0);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;           // index too high
	} end_try;
	try {
	    record.define ("", (Int)0);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;           // empty name
	} end_try;
	try {
	    record.define ("aB", (Int)0);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;           // first no uppercase
	} end_try;
	extraArgument = 10;
	try {
	    record.define ("A", (Int)0);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;           // extra argument = 10
	} end_try;
	extraArgument = 0;
	try {
	    record.define ("TpShort", (Int)0);
	} catch (AipsError x) {
                                                   // invalid type
	    cout << ">>> Instance-specific assertion error message:" << endl
		 << x.getMesg() << endl
		 << "<<<" << endl;
	} end_try;
	RecordDesc rd1(rd);
	rd1.addField ("TpTable", TpTable);
	try {
	    Record rec(rd1);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;           // invalid field type
	} end_try;
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

    // Do some erronous defines and assigns.
    if (doExcp) {
	doDefineAssign (record);
    }
    
    //    Record();
    //    void restructure(const RecordDesc &newDescription);
    // Also check that a RecordFieldPtr gets detached.
    Record record2;
    record2.restructure(subDesc);         // non-fixed -> possible
    RecordFieldPtr<Int> fld2(record2, 1);
    AlwaysAssertExit (fld2.isAttached());
    record2.restructure(subDesc);         // non-fixed -> possible and detaches
    AlwaysAssertExit (! fld2.isAttached());
    // restructure and operator= fail on a non-empty, fixed record.
    Record record2a(RecordInterface::Fixed);
    if (doExcp) {
	try {
	    record2a.restructure(subDesc);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;  // fixed, not empty ->impossible
	} end_try;
    }
    record2 = record;                     // non-fixed -> possible
    record2a = record;                    // same structure -> possible
    Record record2b (subDesc);
    if (doExcp) {
	try {
	    record2a = record2b;
	} catch (AipsError x) {           // fixed; non-conforming
	    cout << ">>> Instance-specific assertion error message:" << endl
		 << x.getMesg() << endl
		 << "<<<" << endl;
	} end_try;
    }
    
    //    Record(const Record &other);
    //    Bool conform(const Record &other);
    Record record3(record2);
    Record record4(record2.description());
    record4 = record3;
    AlwaysAssertExit(rd == record2.description() &&
		     rd == record3.description() &&
		     rd == record4.description());
    AlwaysAssertExit(record.conform(record2));
    AlwaysAssertExit(record.conform(record2a));
    Record record5;
    AlwaysAssertExit(! record.conform(record5));

    //    Record(const RecordInterface& other)
    Record record3a ((RecordInterface&)record3);
    AlwaysAssertExit(rd == record3a.description());
    AlwaysAssertExit(record3a.conform(record3));

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
    //    RecordFieldPtr(Record &record, uInt whichField);
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

    // Check if asComplex work okay.
    AlwaysAssertExit (record.asComplex ("TpComplex") == Complex(1.0f, 11.0f));
    AlwaysAssertExit (record.asComplex ("TpInt") == Complex(-1234567.0f));
    AlwaysAssertExit (record.asComplex ("TpDComplex") == Complex(5.0f, 1.0f));
    AlwaysAssertExit (record.asDComplex ("TpComplex") == DComplex(1.0, 11.0));
    AlwaysAssertExit (record.asDComplex ("TpInt") == DComplex(-1234567.0));
    AlwaysAssertExit (record.asDComplex ("TpDComplex") == DComplex(5.0, 1.0));
    if (doExcp) {
	try {
	    record.asComplex ("TpBool");
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;     // fixed -> add not possible
	} end_try;
    }

    // Check if shape works okay.
    AlwaysAssertExit (record.shape("TpBool") == IPosition(1,1));
    AlwaysAssertExit (record.shape(18) == IPosition(1,1));
    AlwaysAssertExit (record.shape(19) == IPosition(1,2));

    // Sub-record fields
    RecordFieldPtr<Record> recordField(record, "SubRecord");
    Record& subrec = *recordField;
    AlwaysAssertExit(subrec.description() == subDesc);
    RecordFieldPtr<Float> subref (subrec, 0);
    *subref = 9.0;

    // Record& rwSubRecord (Int whichField);
    // defineRecord (const String& name, const Record&);
    // RecordFieldPtr::define (const Record&);
    // RecordFieldPtr::operator= (const Record&);
    Record& subrec1 = record.rwSubRecord (record.fieldNumber ("SubRecord1"));
    if (doExcp) {
	try {
	    subrec.defineRecord ("abcd", subrec);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;     // fixed -> add not possible
	} end_try;
	try {
	    record.defineRecord (record.fieldNumber ("SubRecord"), subrec1);
	} catch (AipsError x) {              // fixed; non-conforming
	    cout << ">>> Instance-specific assertion error message:" << endl
		 << x.getMesg() << endl
		 << "<<<" << endl;
	} end_try;
    }
    Record subrec1a;
    subrec1.defineRecord ("sub", subrec, RecordInterface::Fixed);
    subrec1.defineRecord ("sub1", subrec1a);
    subrec1.defineRecord ("sub2", subrec1a);
    RecordFieldPtr<Record> sub1 (subrec1, 1);
    RecordFieldPtr<Record> sub2 (subrec1, 2);
    *subref = 6.0;
    sub1.define (subrec);
    *subref = 8.0;
    *sub2 = subrec;

    // Check if the entire record is correct.
    check (record, -1234567, 34);

    // Now make a copy of the record and assign a value via RecordFieldPtr.
    // This has to result in a copy(-on-write) operation.
    Record savrec2(record);
    check (savrec2, -1234567, 34);
    *intField += 1;
    *arrayintField = -1234566;
    check (savrec2, -1234567, 34);
    check (record, -1234566, 34);
    savrec2 = record;
    check (savrec2, -1234566, 34);

    // Clone the record.
    RecordInterface* recClone = record.clone();
    check (Record(*recClone), -1234566, 34);
    *intField += 11;
    *arrayintField = -1234555;
    Record reccp (record);
    check (reccp, -1234555, 34);
    check (Record(*recClone), -1234566, 34);
    reccp.assign (*recClone);
    check (reccp, -1234566, 34);
    check (Record(*recClone), -1234566, 34);
    delete recClone;
    *intField -= 11;
    *arrayintField = -1234566;
    check (record, -1234566, 34);

    // Change some more fields and check if the original is kept intact
    // (thus if copy-on-write works fine). This also checks if
    // reacquiring the RecordFieldPtr pointers after a copy works fine.
    Record savrec2a(savrec2);
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
    Record savrec3(record);
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

    // OK, we've tested the Record members, now test the remaining 
    // RecordFieldPtr members.
    //    RecordFieldPtr();
    //    void attachToRecord(Record &record, uInt whichField);
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
    Record *record6 = new Record(rd2);
    RecordFieldPtr<Int> rf1(*record6, 0);
    RecordFieldPtr<Int> rf2(*record6, 0);
    //    virtual void notify(const Notice &message); // implicit
    delete record6;
    AlwaysAssertExit(! rf1.isAttached());
    AlwaysAssertExit(! rf2.isAttached());

    // Check subRecord conformance.
    doSubRecord (doExcp, rd);

    *ucharField= 255;
    AipsIO aos ("tRecord_tmp.data", ByteIO::New);
    aos << record;
    aos.close();
    aos.open ("tRecord_tmp.data");
    aos >> record5;
    AlwaysAssertExit(record5.conform(record));
    check (record5, -1234566, 34);

    // Check defining and removing a subrecord.
    record5.defineRecord (34, record);
    AlwaysAssertExit (record5.name(34) == "*35");
    record5.renameField ("abcd", 34);
    record5.defineRecord ("abcd", record);
    check (record5, -1234566, 35);
    record5.removeField (record5.fieldNumber("abcd"));
    check (record5, -1234566, 34);

    // Check defining a field by number.
    record5.define (34, Int(2));
    check (record5, -1234566, 35);
    AlwaysAssertExit (record5.asInt("*35") == 2);
    record5.removeField (34);
    check (record5, -1234566, 34);

    // Check field merge.
    Record recordm(record5);
    check (recordm, -1234566, 34);
    recordm.merge (record, RecordInterface::SkipDuplicates);
    check (recordm, -1234566, 34);
    recordm.merge (record, RecordInterface::OverwriteDuplicates);
    check (recordm, -1234566, 34);
    recordm.merge (record, RecordInterface::RenameDuplicates);
    check (recordm, -1234566, 68);
    recordm.define (3, -1234555);
    RecordFieldPtr<Array<Int> > fldm (recordm, "TpArrayInt");
    *fldm = -1234555;
    check (recordm, -1234555, 68);
    AlwaysAssertExit (recordm.isDefined ("TpInt_1"));
    AlwaysAssertExit (recordm.fieldNumber ("TpInt_1") == 37);
    recordm.merge (record, RecordInterface::OverwriteDuplicates);
    AlwaysAssertExit (recordm.isDefined ("TpInt_1"));
    AlwaysAssertExit (recordm.fieldNumber ("TpInt_1") == 3);
    AlwaysAssertExit (recordm.fieldNumber ("TpInt") == 37);
    AlwaysAssertExit (recordm.asInt(37) == -1234566);
    RecordFieldPtr<Array<Int> > fldm2 (recordm, "TpArrayInt");
    AlwaysAssertExit (allEQ (*fldm2, -1234566));

    if (doExcp) {
	try {
	    record.merge (record, RecordInterface::SkipDuplicates);
	} catch (AipsError x) {              // merge of itself
	    cout << ">>> Instance-specific assertion error message:" << endl
		 << x.getMesg() << endl
		 << "<<<" << endl;
	} end_try;
	try {
	    record.mergeField (record5, "TpBool",
			       RecordInterface::ThrowOnDuplicates);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;     // duplicate field
	} end_try;
	try {
	    record2a.merge (record, RecordInterface::SkipDuplicates);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;     // fixed structure
	} end_try;
	try {
	    record2a.mergeField (record5, "TpBool",
				 RecordInterface::ThrowOnDuplicates);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;     // fixed structure
	} end_try;
    }
	

    //    ~Record()          // implicit
    //    ~RecordFieldPtr(); // implicit
}


// Check if the values in the record and subrecord are correct.
// The number of fields and the value of the Int fields can vary,
// so they are given as arguments.
void check (const Record& record, Int intValue, uInt nrField)
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
    //    RORecordFieldPtr(Record &record, uInt whichField);
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
    //    Record::get (T& value) const;
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
			   stringToVector("Hello,Goodbye").ac()));

    // Sub(-sub)-record fields
    RORecordFieldPtr<Record> recordField(record, "SubRecord");
    const Record& subrec = *recordField;
    AlwaysAssertExit(subrec.isFixed());
    AlwaysAssertExit(subrec.nfields() == 2);
    RORecordFieldPtr<Float> subref (subrec, 0);
    AlwaysAssertExit(*subref == 8.0);

    RORecordFieldPtr<Float> subref2 (record.subRecord
                                       (record.fieldNumber ("SubRecord")), 0);
    AlwaysAssertExit(*subref2 == 8.0);

    RORecordFieldPtr<Record> recordField1(record, "SubRecord1");
    const Record& subrec1 = *recordField1;
    AlwaysAssertExit(! subrec1.isFixed());
    AlwaysAssertExit(subrec1.nfields() == 3);

    RORecordFieldPtr<Record> sub(subrec1, "sub");
    AlwaysAssertExit((*sub).isFixed());
    AlwaysAssertExit((*sub).nfields() == 2);
    RORecordFieldPtr<Float> subrefa (*sub, 0);
    AlwaysAssertExit(*subrefa == 9.0);

    RORecordFieldPtr<Record> sub1(subrec1, "sub1");
    AlwaysAssertExit(! (*sub1).isFixed());
    AlwaysAssertExit((*sub1).nfields() == 2);
    AlwaysAssertExit((*sub1).asfloat(0) == 6.0);

    RORecordFieldPtr<Record> sub2(subrec1, "sub2");
    AlwaysAssertExit(! (*sub2).isFixed());
    AlwaysAssertExit((*sub2).nfields() == 2);
    AlwaysAssertExit((*sub2).asdouble("SubFloat") == 8.0);
}
