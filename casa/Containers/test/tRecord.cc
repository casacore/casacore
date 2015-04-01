//# tRecord.cc: Test the Record class
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001
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

#include <casacore/casa/Containers/Record.h>
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
// This program tests the Record and RecordRep classes.
// Its expected output (which only consists of exception text)
// is stored in tRecord.out and can be checked using assay.
// This will also delete the temporary output file.
// <p>
// It can throw several exceptions, which may result in memory leaks.
// To check if no real memory leaks occur, the program can be run
// with an argument (e.g. tRecord 1). In that case no statements
// resulting in exceptions are executed, so no memory leaks should occur.
// <p>
// The ability to read old KeywordSet objects from a file has been tested
// in a separate program. That program is not checked into the system,
// because the KeywordSet classes are removed from it.


void check (const Record&, Int intValue, uInt nrField);
void doIt (Bool doExcp);

int main (int argc, const char*[])
{
    try {
	doIt ( (argc<2));
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    cout << "OK" << endl;
    return 0;                           // exit with success status
}

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
void doDefineAssign (const Record& inrecord)
{
    Record record (inrecord);
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
    AlwaysAssertExit (record1.subRecord(subField1).asFloat(0) == 3);
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
	} 
    }
    (*sub1).define (0, float(4));
    AlwaysAssertExit (record.subRecord(subField1).asFloat(0) == 4);
    record = record1;
    AlwaysAssertExit (record.subRecord(subField1).asFloat(0) == 3);
    record.print (cout, 0, "  ");
    record.print (cout, -1, "  ");
    record.print (cout, 1, "  ");
    cout << record;
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
    rd.addField ("TpInt64", TpInt64);
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
    rd.addField ("TpArrayInt64", TpArrayInt64, IPosition(1,1));
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
		     record.nfields() == 24);
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
	} 
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
	RecordDesc rd1;
	rd1.addField ("TpTable", TpTable);
	try {
	    Record rec(rd1);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;           // invalid field type
	} 
    }
    AlwaysAssertExit(record.nfields() == rd.nfields() &&
		     record.nfields() == 24);
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
    record.define ("TpInt642", Int64(2e10));
    rd.addField ("TpInt642a", TpInt64);
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
    AlwaysAssertExit (record.asBool("TpInt2"));
    AlwaysAssertExit (allEQ (record.toArrayBool("TpInt2"), True));
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
	} 
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
	} 
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
    RecordFieldPtr<Int64>    int64Field(record, 5);
    RecordFieldPtr<Float>    floatField(record, 6);
    RecordFieldPtr<Double>   doubleField(record, 7);
    RecordFieldPtr<Complex>  complexField(record, 8);
    RecordFieldPtr<DComplex> dcomplexField(record, 9);
    RecordFieldPtr<String>   stringField(record, 10);
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
    *int64Field = Int64(3e10);
    *floatField = 7.0f;
    *doubleField = 9.0;
    *complexField = Complex(1.0f, 11.0f);
    *dcomplexField = Complex(5.0, 1.0);
    *stringField = "Hello";

    // Array fields
    RecordFieldPtr<Array<Bool> >     arrayboolField(record, 11);
    RecordFieldPtr<Array<uChar> >    arrayucharField(record, 12);
    RecordFieldPtr<Array<Short> >    arrayshortField(record, 13);
    RecordFieldPtr<Array<Int> >      arrayintField(record, 14);
    RecordFieldPtr<Array<uInt> >     arrayuintField(record, 15);
    RecordFieldPtr<Array<Int64> >    arrayint64Field(record, 16);
    RecordFieldPtr<Array<Float> >    arrayfloatField(record, 17);
    RecordFieldPtr<Array<Double> >   arraydoubleField(record, 18);
    RecordFieldPtr<Array<Complex> >  arraycomplexField(record, 19);
    RecordFieldPtr<Array<DComplex> > arraydcomplexField(record, 20);
    RecordFieldPtr<Array<String> >   arraystringField(record, 21);
    arrayboolField.setComment ("comment for TpArrayBool");
    *arrayboolField = True;
    *arrayucharField = 255;
    *arrayshortField = 32767;
    *arrayintField = -1234567;
    *arrayuintField = 1234567;
    *arrayint64Field = Int64(3e10);
    *arrayfloatField = 7.0f; 
    *arraydoubleField = 9.0; 
    *arraycomplexField = Complex(1.0f, 11.0f);
    *arraydcomplexField = DComplex(5.0, 1.0);
    *arraystringField = stringToVector ("Hello,Goodbye");

    // Check if asComplex works okay.
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
	} 
    }

    // Check if shape works okay.
    AlwaysAssertExit (record.shape("TpBool") == IPosition(1,1));
    AlwaysAssertExit (record.shape(20) == IPosition(1,1));
    AlwaysAssertExit (record.shape(21) == IPosition(1,2));

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
    check (record, -1234567, 37);

    // Now make a copy of the record and assign a value via RecordFieldPtr.
    // This has to result in a copy(-on-write) operation.
    Record savrec2(record);
    check (savrec2, -1234567, 37);
    *intField += 1;
    *arrayintField = -1234566;
    check (savrec2, -1234567, 37);
    check (record, -1234566, 37);
    savrec2 = record;
    check (savrec2, -1234566, 37);

    // Clone the record.
    RecordInterface* recClone = record.clone();
    check (Record(*recClone), -1234566, 37);
    *intField += 11;
    *arrayintField = -1234555;
    Record reccp (record);
    check (reccp, -1234555, 37);
    check (Record(*recClone), -1234566, 37);
    reccp.assign (*recClone);
    check (reccp, -1234566, 37);
    check (Record(*recClone), -1234566, 37);
    delete recClone;
    *intField -= 11;
    *arrayintField = -1234566;
    check (record, -1234566, 37);

    // Change some more fields and check if the original is kept intact
    // (thus if copy-on-write works fine). This also checks if
    // reacquiring the RecordFieldPtr pointers after a copy works fine.
    Record savrec2a(savrec2);
    RecordFieldPtr<Int> savrf (savrec2, 3);
    RecordFieldPtr<Array<Int> > savrfarray (savrec2, 14);
    savrf.define (savrf.get() + 11);
    *savrfarray = *savrf;
    check (savrec2, -1234555, 37);
    check (savrec2a, -1234566, 37);
    check (record, -1234566, 37);

    // Add some fields.
    // Check if removing a field results in deattaching and in
    // decrementing the field number.
    record.define ("TpString3", "abcd");
    record.define ("TpString4", "efghij");
    check (record, -1234566, 39);
    Record savrec3(record);
    check (savrec3, -1234566, 39);
    RecordFieldPtr<String> tpstring2 (record, "TpString2");
    RecordFieldPtr<String> tpstring3 (record, "TpString3");
    RecordFieldPtr<String> tpstring4 (record, "TpString4");
    record.removeField (record.fieldNumber("TpString3"));
    AlwaysAssertExit (tpstring2.isAttached());
    AlwaysAssertExit (! tpstring3.isAttached());
    AlwaysAssertExit (tpstring4.isAttached());
    AlwaysAssertExit (tpstring2.fieldNumber() == 36);
    AlwaysAssertExit (tpstring3.fieldNumber() == -1);
    AlwaysAssertExit (tpstring4.fieldNumber() == 37);
    record.removeField (record.fieldNumber("TpString4"));
    check (savrec3, -1234566, 39);
    check (record, -1234566, 37);

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
    check (record5, -1234566, 37);

    // Check defining and removing a subrecord.
    record5.defineRecord (37, record);
    AlwaysAssertExit (record5.name(37) == "*38");
    record5.renameField ("abcd", 37);
    record5.defineRecord ("abcd", record);
    check (record5, -1234566, 38);
    record5.removeField (record5.fieldNumber("abcd"));
    check (record5, -1234566, 37);

    // Check defining a field by number.
    record5.define (37, Int(2));
    check (record5, -1234566, 38);
    AlwaysAssertExit (record5.asInt("*38") == 2);
    record5.removeField (37);
    check (record5, -1234566, 37);

    // Check field merge.
    Record recordm(record5);
    check (recordm, -1234566, 37);
    recordm.merge (record, RecordInterface::SkipDuplicates);
    check (recordm, -1234566, 37);
    recordm.merge (record, RecordInterface::OverwriteDuplicates);
    check (recordm, -1234566, 37);
    recordm.merge (record, RecordInterface::RenameDuplicates);
    check (recordm, -1234566, 74);
    recordm.define (3, -1234555);
    RecordFieldPtr<Array<Int> > fldm (recordm, "TpArrayInt");
    *fldm = -1234555;
    check (recordm, -1234555, 74);
    AlwaysAssertExit (recordm.isDefined ("TpInt_1"));
    AlwaysAssertExit (recordm.fieldNumber ("TpInt_1") == 40);
    recordm.merge (record, RecordInterface::OverwriteDuplicates);
    AlwaysAssertExit (recordm.isDefined ("TpInt_1"));
    AlwaysAssertExit (recordm.fieldNumber ("TpInt_1") == 3);
    AlwaysAssertExit (recordm.fieldNumber ("TpInt") == 40);
    AlwaysAssertExit (recordm.asInt(40) == -1234566);
    RecordFieldPtr<Array<Int> > fldm2 (recordm, "TpArrayInt");
    AlwaysAssertExit (allEQ (*fldm2, -1234566));

    if (doExcp) {
	try {
	    record.merge (record, RecordInterface::SkipDuplicates);
	} catch (AipsError x) {              // merge of itself
	    cout << ">>> Instance-specific assertion error message:" << endl
		 << x.getMesg() << endl
		 << "<<<" << endl;
	} 
	try {
	    record.mergeField (record5, "TpBool",
			       RecordInterface::ThrowOnDuplicates);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;     // duplicate field
	} 
	try {
	    record2a.merge (record, RecordInterface::SkipDuplicates);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;     // fixed structure
	} 
	try {
	    record2a.mergeField (record5, "TpBool",
				 RecordInterface::ThrowOnDuplicates);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;     // fixed structure
	} 
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
    RORecordFieldPtr<Int64>    int64Field(record, 5);
    RORecordFieldPtr<Float>    floatField(record, 6);
    RORecordFieldPtr<Double>   doubleField(record, 7);
    RORecordFieldPtr<Complex>  complexField(record, 8);
    RORecordFieldPtr<DComplex> dcomplexField(record, 9);
    RORecordFieldPtr<String>   stringField(record, 10);
    //    RORecordFieldPtr(Record &record, uInt whichField);
    //    const T &operator*() const {return *field_ptr_p;}
    AlwaysAssertExit(boolField.comment() == "comment for TpBool");
    AlwaysAssertExit(*boolField == True);
    AlwaysAssertExit(*ucharField == 255);
    AlwaysAssertExit(*shortField == 32767);
    AlwaysAssertExit(intField.get() == intValue);
    AlwaysAssertExit(uintField.get() == 1234567);
    AlwaysAssertExit(*int64Field == Int64(3e10));
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
    Int64 i64v;
    Float fv;
    Double dv;
    Complex cv;
    DComplex dcv;
    String strv;
    //    Record::get (T& value) const;
    record.get (24, bv);
    record.get (25, ucv);
    record.get (26, sv);
    record.get (27, iv);
    record.get (28, uiv);
    record.get (29, i64v);
    record.get (30, fv);
    record.get (31, dv);
    record.get (32, cv);
    record.get (33, dcv);
    record.get (36, strv);
    AlwaysAssertExit(bv == False);
    AlwaysAssertExit(ucv == 1);
    AlwaysAssertExit(sv == 2);
    AlwaysAssertExit(iv == 3);
    AlwaysAssertExit(uiv == 4);
    AlwaysAssertExit(i64v == Int64(2e10));
    AlwaysAssertExit(fv == 5);
    AlwaysAssertExit(dv == 6);
    AlwaysAssertExit(cv == Complex(7,8));
    AlwaysAssertExit(dcv == DComplex(9,10));
    AlwaysAssertExit(strv == "abc");
    AlwaysAssertExit (allEQ (record.asArrayBool(24), bv));
    AlwaysAssertExit (allEQ (record.asArrayuChar(25), ucv));
    AlwaysAssertExit (allEQ (record.asArrayShort(26), sv));
    AlwaysAssertExit (allEQ (record.asArrayInt(27), iv));
    AlwaysAssertExit (allEQ (record.asArrayuInt(28), uiv));
    AlwaysAssertExit (allEQ (record.asArrayInt64(29), i64v));
    AlwaysAssertExit (allEQ (record.asArrayFloat(30), fv));
    AlwaysAssertExit (allEQ (record.asArrayDouble(31), dv));
    AlwaysAssertExit (allEQ (record.asArrayComplex(32), cv));
    AlwaysAssertExit (allEQ (record.asArrayDComplex(33), dcv));
    AlwaysAssertExit (allEQ (record.asArrayString(36), strv));

    AlwaysAssertExit (allEQ (record.toArrayBool(24), bv));
    AlwaysAssertExit (allEQ (record.toArrayuChar(25), ucv));
    AlwaysAssertExit (allEQ (record.toArrayShort(26), sv));
    AlwaysAssertExit (allEQ (record.toArrayInt(27), iv));
    AlwaysAssertExit (allEQ (record.toArrayuInt(28), uiv));
    AlwaysAssertExit (allEQ (record.toArrayInt64(29), i64v));
    AlwaysAssertExit (allEQ (record.toArrayFloat(30), fv));
    AlwaysAssertExit (allEQ (record.toArrayDouble(31), dv));
    AlwaysAssertExit (allEQ (record.toArrayComplex(32), cv));
    AlwaysAssertExit (allEQ (record.toArrayDComplex(33), dcv));
    AlwaysAssertExit (allEQ (record.toArrayString(36), strv));
    AlwaysAssertExit (allEQ (record.toArrayBool(11), *boolField));
    AlwaysAssertExit (allEQ (record.toArrayuChar(12), *ucharField));
    AlwaysAssertExit (allEQ (record.toArrayShort(12), Short(*ucharField)));
    AlwaysAssertExit (allEQ (record.toArrayInt(12), Int(*ucharField)));
    AlwaysAssertExit (allEQ (record.toArrayuInt(12), uInt(*ucharField)));
    AlwaysAssertExit (allEQ (record.toArrayInt64(12), Int64(*ucharField)));
    AlwaysAssertExit (allEQ (record.toArrayFloat(12), Float(*ucharField)));
    AlwaysAssertExit (allEQ (record.toArrayDouble(12), Double(*ucharField)));
    AlwaysAssertExit (allEQ (record.toArrayComplex(12),
			     Complex(*ucharField,0)));
    AlwaysAssertExit (allEQ (record.toArrayDComplex(12),
			     DComplex(*ucharField,0)));
    AlwaysAssertExit (allEQ (record.toArrayShort(13), *shortField));
    AlwaysAssertExit (allEQ (record.toArrayInt(13), Int(*shortField)));
    AlwaysAssertExit (allEQ (record.toArrayuInt(13), uInt(*shortField)));
    AlwaysAssertExit (allEQ (record.toArrayInt64(13), Int64(*shortField)));
    AlwaysAssertExit (allEQ (record.toArrayFloat(13), Float(*shortField)));
    AlwaysAssertExit (allEQ (record.toArrayDouble(13), Double(*shortField)));
    AlwaysAssertExit (allEQ (record.toArrayComplex(13),
			     Complex(*shortField,0)));
    AlwaysAssertExit (allEQ (record.toArrayDComplex(13),
			     DComplex(*shortField,0)));
    AlwaysAssertExit (allEQ (record.toArrayInt(14), *intField));
    AlwaysAssertExit (allEQ (record.toArrayInt64(14), Int64(*intField)));
    AlwaysAssertExit (allEQ (record.toArrayFloat(14), Float(*intField)));
    AlwaysAssertExit (allEQ (record.toArrayDouble(14), Double(*intField)));
    AlwaysAssertExit (allEQ (record.toArrayComplex(14),
			     Complex(*intField,0)));
    AlwaysAssertExit (allEQ (record.toArrayDComplex(14),
			     DComplex(*intField,0)));
    AlwaysAssertExit (allEQ (record.toArrayuInt(15), *uintField));
    AlwaysAssertExit (allEQ (record.toArrayInt64(15), Int64(*uintField)));
    AlwaysAssertExit (allEQ (record.toArrayFloat(15), Float(*uintField)));
    AlwaysAssertExit (allEQ (record.toArrayDouble(15), Double(*uintField)));
    AlwaysAssertExit (allEQ (record.toArrayComplex(15),
			     Complex(*uintField,0)));
    AlwaysAssertExit (allEQ (record.toArrayDComplex(15),
			     DComplex(*uintField,0)));
    AlwaysAssertExit (allEQ (record.toArrayInt64(16), *int64Field));
    AlwaysAssertExit (allEQ (record.toArrayFloat(16), Float(*int64Field)));
    AlwaysAssertExit (allEQ (record.toArrayDouble(16), Double(*int64Field)));
    AlwaysAssertExit (allEQ (record.toArrayComplex(16),
			     Complex(*int64Field,0)));
    AlwaysAssertExit (allEQ (record.toArrayDComplex(16),
			     DComplex(*int64Field,0)));
    AlwaysAssertExit (allEQ (record.toArrayFloat(17), *floatField));
    AlwaysAssertExit (allEQ (record.toArrayDouble(17), Double(*floatField)));
    AlwaysAssertExit (allEQ (record.toArrayComplex(17),
			     Complex(*floatField,0)));
    AlwaysAssertExit (allEQ (record.toArrayDComplex(17),
			     DComplex(*floatField,0)));
    AlwaysAssertExit (allEQ (record.toArrayDouble(18), *doubleField));
    AlwaysAssertExit (allEQ (record.toArrayFloat(18), Float(*doubleField)));
    AlwaysAssertExit (allEQ (record.toArrayComplex(18),
			     Complex(*doubleField,0)));
    AlwaysAssertExit (allEQ (record.toArrayDComplex(18),
			     DComplex(*doubleField,0)));
    AlwaysAssertExit (allEQ (record.toArrayComplex(19), *complexField));
    AlwaysAssertExit (allEQ (record.toArrayDComplex(19),
			     DComplex(*complexField)));
    AlwaysAssertExit (allEQ (record.toArrayDComplex(20), *dcomplexField));
    AlwaysAssertExit (allEQ (record.toArrayComplex(20),
			     Complex((*dcomplexField).real(),
				     (*dcomplexField).imag())));
    AlwaysAssertExit (allEQ (record.toArrayString(21),
                                    stringToVector("Hello,Goodbye")));

    // Scalars as Arrays.
    RORecordFieldPtr<Array<Bool> >     boolFieldA(record, 0);
    RORecordFieldPtr<Array<uChar> >    ucharFieldA(record, 1);
    RORecordFieldPtr<Array<Short> >    shortFieldA(record, 2);
    RORecordFieldPtr<Array<Int> >      intFieldA(record, 3);
    RORecordFieldPtr<Array<uInt> >     uintFieldA(record, 4);
    RORecordFieldPtr<Array<Int64> >    int64FieldA(record, 5);
    RORecordFieldPtr<Array<Float> >    floatFieldA(record, 6);
    RORecordFieldPtr<Array<Double> >   doubleFieldA(record, 7);
    RORecordFieldPtr<Array<Complex> >  complexFieldA(record, 8);
    RORecordFieldPtr<Array<DComplex> > dcomplexFieldA(record, 9);
    RORecordFieldPtr<Array<String> >   stringFieldA(record, 10);
    AlwaysAssertExit (allEQ (*boolFieldA, Vector<Bool>(1, *boolField)));
    AlwaysAssertExit (allEQ (*ucharFieldA, Vector<uChar>(1, *ucharField)));
    AlwaysAssertExit (allEQ (*shortFieldA, Vector<Short>(1, *shortField)));
    AlwaysAssertExit (allEQ (*intFieldA, Vector<Int>(1, *intField)));
    AlwaysAssertExit (allEQ (*uintFieldA, Vector<uInt>(1, *uintField)));
    AlwaysAssertExit (allEQ (*int64FieldA, Vector<Int64>(1, *int64Field)));
    AlwaysAssertExit (allEQ (*floatFieldA, Vector<Float>(1, *floatField)));
    AlwaysAssertExit (allEQ (*doubleFieldA, Vector<Double>(1, *doubleField)));
    AlwaysAssertExit (allEQ (*complexFieldA, Vector<Complex>(1, *complexField)));
    AlwaysAssertExit (allEQ (*dcomplexFieldA, Vector<DComplex>(1, *dcomplexField)));
    AlwaysAssertExit (allEQ (*stringFieldA, Vector<String>(1, *stringField)));

    // Array fields
    RORecordFieldPtr<Array<Bool> >     arrayboolField(record, 11);
    RORecordFieldPtr<Array<uChar> >    arrayucharField(record, 12);
    RORecordFieldPtr<Array<Short> >    arrayshortField(record, 13);
    RORecordFieldPtr<Array<Int> >      arrayintField(record, 14);
    RORecordFieldPtr<Array<uInt> >     arrayuintField(record, 15);
    RORecordFieldPtr<Array<Int64> >    arrayint64Field(record, 16);
    RORecordFieldPtr<Array<Float> >    arrayfloatField(record, 17);
    RORecordFieldPtr<Array<Double> >   arraydoubleField(record, 18);
    RORecordFieldPtr<Array<Complex> >  arraycomplexField(record, 19);
    RORecordFieldPtr<Array<DComplex> > arraydcomplexField(record, 20);
    RORecordFieldPtr<Array<String> >   arraystringField(record, 21);
    AlwaysAssertExit(arrayboolField.comment() == "comment for TpArrayBool");
    AlwaysAssertExit(allEQ(*arrayboolField, *boolField));
    AlwaysAssertExit(allEQ(*arrayucharField, *ucharField));
    AlwaysAssertExit(allEQ(*arrayshortField, *shortField));
    AlwaysAssertExit(allEQ(*arrayintField, *intField));
    AlwaysAssertExit(allEQ(*arrayuintField, *uintField));
    AlwaysAssertExit(allEQ(*arrayint64Field, *int64Field));
    AlwaysAssertExit(allEQ(*arrayfloatField, *floatField));
    AlwaysAssertExit(allEQ(*arraydoubleField, *doubleField));
    AlwaysAssertExit(allEQ(*arraycomplexField, *complexField));
    AlwaysAssertExit(allEQ(*arraydcomplexField, *dcomplexField));
    AlwaysAssertExit(allEQ(*arraystringField,
			   stringToVector("Hello,Goodbye")));

    // Sub(-sub)-record fields
    RORecordFieldPtr<Record> recordField(record, "SubRecord");
    const Record& subrec = *recordField;
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
    AlwaysAssertExit((*sub1).asFloat(0) == 6.0);

    RORecordFieldPtr<Record> sub2(subrec1, "sub2");
    AlwaysAssertExit(! (*sub2).isFixed());
    AlwaysAssertExit((*sub2).nfields() == 2);
    AlwaysAssertExit((*sub2).asDouble("SubFloat") == 8.0);
}
