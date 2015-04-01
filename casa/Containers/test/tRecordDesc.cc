//# tRecordDesc.cc: Test the RecordDesc class
//# Copyright (C) 1995,1996,2000,2001
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

#include <casacore/casa/Containers/RecordDesc.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
void doIt (Bool doExcp);

int main (int argc, const char*[])
{
    try {
	doIt ( (argc<2));
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}

void doIt (Bool doExcp)
{
    Bool equalDataTypes;
//    RecordDesc();
    RecordDesc a, b;
    AlwaysAssertExit(a == b && a.nfields() == 0 && b.nfields() == 0);
    AlwaysAssertExit (a.isDisjoint (b));
    AlwaysAssertExit (a.isEqual (b, equalDataTypes));
    AlwaysAssertExit (equalDataTypes);
    AlwaysAssertExit (a.isSubset (b, equalDataTypes));
    AlwaysAssertExit (a.isSuperset (b, equalDataTypes));
    AlwaysAssertExit (! a.isStrictSuperset (b, equalDataTypes));
    AlwaysAssertExit (! a.isStrictSubset (b, equalDataTypes));
    
//    uInt addField(const String &fieldName, DataType type);
//    void setComment (uInt whichField, const String& comment);
    a.addField("a", TpString);
    a.addField("b", TpArrayDComplex);
    a.setComment (1, "comment for field b");
//    Int fieldNumber(const String &fieldName);
//    uInt nfields() const;
//    DataType type(uInt whichField) const;
//    const String &comment (uInt whichField) const;
//    const String &name(uInt whichField) const;
//    const IPosition &shape(uInt whichField) const;
//    Bool isArray(uInt whichField) const;
//    Bool isScalar(uInt whichField) const;
//    Bool isSubRecord(uInt whichField) const;
//    Bool isTable(uInt whichField) const;
//    Bool operator==(const RecordDesc &other) const;
//    Bool operator!=(const RecordDesc &other) const;
    AlwaysAssertExit (a.isDisjoint (b));
    AlwaysAssertExit (b.isDisjoint (a));
    AlwaysAssertExit (! a.isEqual (b, equalDataTypes));
    AlwaysAssertExit (a.comment (0) == "");
    AlwaysAssertExit (a.comment (1) == "comment for field b");
    AlwaysAssertExit(a.nfields() == 2 && a.name(0) == "a" &&
		     a.name(1) == "b" && a.type(0) == TpString &&
		     a.type(1) == TpArrayDComplex && !a.isArray(0) &&
		     a.isArray(1) && a.isScalar(0) && !a.isScalar(1) &&
		     !a.isSubRecord(0) && !a.isSubRecord(1) &&
		     !a.isTable(0) && !a.isTable(1) &&
		     a.tableDescName(0).empty() &&
		     a == a && !(a != a) && a.fieldNumber("a") == 0 &&
		     a.fieldNumber("b") == 1 && a.fieldNumber("c") == -1 &&
		     a.shape(0) == IPosition(1,1) &&
		     a.shape(1) == IPosition(1,-1));

//    RecordDesc(const RecordDesc &other);
    RecordDesc c(a);
    AlwaysAssertExit(c == a && c.name(0) == "a" && c.name(1) == "b");
    AlwaysAssertExit (c.comment (0) == "");
    AlwaysAssertExit (c.comment (1) == "comment for field b");
    RecordDesc c1(a);
    AlwaysAssertExit(c1 == c && c1.name(0) == "a" && c1.name(1) == "b");

//    RecordDesc.addTable
    RecordDesc at(a);
    at.addTable("ctab","descName");
    at.addField("ctab1",TpTable);
    AlwaysAssertExit (at.nfields()==4 && at.tableDescName(2) == "descName"
		      && at.tableDescName(3) == "");
    AlwaysAssertExit (at.isTable(2) && at.isTable(3));
    AlwaysAssertExit (!at.isScalar(2) && !at.isArray(2) && !at.isSubRecord(2));
    
    a.addField("c1", TpRecord);

    if (doExcp) {
	Bool caught = False;
	try {
	    a.addField("a", TpDouble); // already exists
	} catch (AipsError x) {
	    caught = True;
	} 
	AlwaysAssertExit(caught);
	caught = False;
	try {
	    a.addField("a", TpDouble, IPosition(1,1)); // already exists
	} catch (AipsError x) {
	    caught = True;
	} 
	AlwaysAssertExit(caught);
	caught = False;
	try {
	    a.addField("a", TpDouble, IPosition(1,1)); // already exists
	} catch (AipsError x) {
	    caught = True;
	} 
	AlwaysAssertExit(caught);
	caught = False;
	try {
	    a.addField("aaa", TpOther);                // invalid type
	} catch (AipsError x) {
	    caught = True;
	} 
	AlwaysAssertExit(caught);
    }

//    RecordDesc &operator=(const RecordDesc &other);
    b = a;
    AlwaysAssertExit (b.comment (0) == "");
    AlwaysAssertExit (b.comment (1) == "comment for field b");
    AlwaysAssertExit(b == a && b.name(0) == "a" && b.name(1) == "b");
    AlwaysAssertExit (! a.isDisjoint (b));
    AlwaysAssertExit (a.isEqual (b, equalDataTypes));
    AlwaysAssertExit (equalDataTypes);
    AlwaysAssertExit (! a.isStrictSubset (b, equalDataTypes));

//    uInt addField(const String &fieldName, const RecordDesc &subDesc);
    b.addField("c", a);
    AlwaysAssertExit(b.nfields() == 4 && b.fieldNumber("c") == 3);
//    const RecordDesc &subRecord(uInt i) const;
    AlwaysAssertExit(b.subRecord(3) == a && b.isSubRecord(3) &&
	!b.isScalar(3) && !b.isArray(3));
//    uInt removeField(uInt whichField);
    b.removeField(2);
    AlwaysAssertExit(b.nfields() == 3);

//    uInt addField(const String &fieldName, DataType scalarOrArrayType,
//		  const IPosition &shape);
    uInt whichField = b.addField("array", TpArrayInt, IPosition(2,3,4));
    whichField--;
    AlwaysAssertExit(b.shape(whichField) == IPosition(2,3,4));
    b.removeField(whichField);

    {
//    uInt mergeField(const RecordDesc &other, uInt whichFieldFromOther,
//	       DuplicatesFlag DuplicateAction = ThrowOnDuplicates);
//    uInt merge(const RecordDesc &other, 
//	       DuplicatesFlag DuplicateAction = ThrowOnDuplicates);
	RecordDesc ab, cd;
	ab.addField("a", TpInt);
	ab.setComment (0, "a-comment");
	ab.addField("b", TpFloat);
	cd.addField("c", TpArrayFloat);
	cd.setComment (0, "c-comment");
	cd.addField("d", ab);
	ab.merge(cd);
	AlwaysAssertExit(ab.nfields() == 4 && ab.fieldNumber("c") == 2 &&
			 ab.fieldNumber("d") == 3);
	AlwaysAssertExit (ab.comment (2) == "c-comment");
	ab.mergeField(ab, 0, RecordInterface::RenameDuplicates);
	AlwaysAssertExit(ab.nfields() == 5);
	AlwaysAssertExit(ab.fieldNumber("a_1") == 4);
	AlwaysAssertExit (ab.comment (2) == "c-comment");
	AlwaysAssertExit (ab.comment (4) == "a-comment");
	ab.removeField(4);
	ab.mergeField(ab, 0, RecordInterface::SkipDuplicates);
	AlwaysAssertExit(ab.nfields() == 4);
	cd.addField("a", TpString);
	cd.setComment (2, "a-comment-a");
	ab.mergeField(cd, cd.fieldNumber("a"),
		      RecordInterface::OverwriteDuplicates);
	AlwaysAssertExit (ab.comment (1) == "c-comment");
	AlwaysAssertExit (ab.comment (3) == "a-comment-a");
	AlwaysAssertExit(ab.type(ab.fieldNumber("a")) == TpString);
	if (doExcp) {
	    Bool caught = False;
	    try {
		ab.mergeField(cd, cd.fieldNumber("a"), 
			      RecordInterface::ThrowOnDuplicates);
	    } catch (AipsError x) {
		caught = True;
	    } 
	    AlwaysAssertExit(caught);
	}
    }

    // Some other things to get the test coverage up
    RecordDesc d;
    AlwaysAssertExit(d != a); // Fails by nfields() differing
    d.addField("foo", a);
    d.addField("bar", a);
    AlwaysAssertExit(d != a); // fails by the type differing
    AlwaysAssertExit(d == d);
    RecordDesc e;
    e.addField("foo", d);
    AlwaysAssertExit(d != e);
    d = e;
    RecordDesc f;
    f.addField("bar", TpInt);
    AlwaysAssertExit(d != f);
    RecordDesc g;
    g.addField("bar", f);
    AlwaysAssertExit(d != g);
    g.addField("TpArrayBool", TpArrayBool, IPosition(1,1));
    g.addField("TpArrayChar", TpArrayChar, IPosition(1,1));
    g.addField("TpArrayUChar", TpArrayUChar, IPosition(1,1));
    g.addField("TpArrayShort", TpArrayShort, IPosition(1,1));
    g.addField("TpArrayUShort", TpArrayUShort, IPosition(1,1));
    g.addField("TpArrayInt", TpArrayInt, IPosition(1,1));
    g.addField("TpArrayUInt", TpArrayUInt, IPosition(1,1));
    g.addField("TpArrayInt64", TpArrayInt64, IPosition(1,1));
    g.addField("TpArrayFloat", TpArrayFloat, IPosition(1,1));
    g.addField("TpArrayDouble", TpArrayDouble, IPosition(1,1));
    g.addField("TpArrayComplex", TpArrayComplex, IPosition(1,1));
    g.addField("TpArrayDComplex", TpArrayDComplex, IPosition(1,1));
    g.addField("TpArrayString", TpArrayString, IPosition(1,1));
    Int gn = g.nfields() - 1;
    AlwaysAssert (gn == g.fieldNumber("TpArrayString"), AipsError);
    AlwaysAssert (g.shape(gn) == IPosition(1,1), AipsError);

    // Test renameField.
    g.renameField ("newname", gn);
    AlwaysAssertExit (g.fieldNumber("TpArrayString") < 0);
    AlwaysAssertExit (g.fieldNumber("newname") == gn);
    g.renameField ("TpArrayString", gn);
    AlwaysAssertExit (g.fieldNumber("TpArrayString") == gn);
    AlwaysAssertExit (g.fieldNumber("newname") < 0);
    {
	// operator<<()
	RecordDesc rd;
	rd.addField("foo", TpInt);
	rd.setComment (0, "foo comment");
	rd.addField("bar", TpDComplex, IPosition(2, 3, 4));
	rd.addField("bar64", TpInt64);
	rd.addField("fubar", RecordDesc(rd));
	rd.addTable("futab", "fuName");
	cout << rd;

	AipsIO aos ("tRecordDesc_tmp.data", ByteIO::New);
	aos << rd;
	aos.close();
	aos.open ("tRecordDesc_tmp.data");
	RecordDesc rd1;
        rd1.addField ("should be removed by >>", TpShort);
	aos >> rd1;
	AlwaysAssertExit(rd == rd1);
	AlwaysAssertExit (rd1.comment (0) == "foo comment");
	rd1.setComment (0, rd1.comment(0) + " bar");
	AlwaysAssertExit (rd.comment (0) == "foo comment");
	AlwaysAssertExit (rd1.comment (0) == "foo comment bar");
    }

    if (doExcp) {
	Bool caught = False;
	try {
	    g.addField("Other", TpRecord, IPosition(1,1));
	} catch (AipsError x) {
	    caught = True;
	} 
	AlwaysAssertExit(caught);
        caught = False;
	try {
	    g.addField("Other", TpTable, IPosition(1,1));
	} catch (AipsError x) {
	    caught = True;
	} 
	AlwaysAssertExit(caught);
    }

    cout << "OK" << endl;
//    ~RecordDesc();  // implicit
}
