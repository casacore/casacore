//# CopyRecord.cc:  definition of CopyRecordToTable
//# Copyright (C) 1995,1996,1997,1998,1999,2000
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

#include <casacore/fits/FITS/CopyRecord.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Containers/RecordInterface.h>
#include <casacore/casa/Containers/RecordDesc.h>
#include <casacore/tables/Tables.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

CopyRecordToTable::CopyRecordToTable(Table &outputTable,
                                     const RecordInterface &inputBuffer,
                                     const Vector<Int> inputMap)
{
    Block<Int> counts(TpNumberOfTypes);
    counts.set(0);
    // Count how many fields of each type exist
    uInt n = inputBuffer.nfields();
    uInt i;
    for (i=0; i < n; i++) {
        if (inputMap(i) != -1) counts[inputBuffer.description().type(i)]++;
    }
    uInt total = 0;
    table_bool.resize(counts[TpBool]); record_bool.resize(counts[TpBool]);
    total += counts[TpBool];
 
    table_char.resize(counts[TpUChar]); record_char.resize(counts[TpUChar]);
    total += counts[TpUChar];
 
    table_short.resize(counts[TpShort]); record_short.resize(counts[TpShort]);
    total += counts[TpShort];
 
    table_int.resize(counts[TpInt]); record_int.resize(counts[TpInt]);
    total += counts[TpInt];
 
    table_float.resize(counts[TpFloat]); record_float.resize(counts[TpFloat]);
    total += counts[TpFloat];
 
    table_double.resize(counts[TpDouble]);
    record_double.resize(counts[TpDouble]);
    total += counts[TpDouble];
 
    table_complex.resize(counts[TpComplex]);
    record_complex.resize(counts[TpComplex]);
    total += counts[TpComplex];
 
    table_dcomplex.resize(counts[TpDComplex]);
    record_dcomplex.resize(counts[TpDComplex]);
    total += counts[TpDComplex];
 
    table_string.resize(counts[TpString]);
    record_string.resize(counts[TpString]);
    total += counts[TpString];
 
    table_array_bool.resize(counts[TpArrayBool]);
    record_array_bool.resize(counts[TpArrayBool]);
    total += counts[TpArrayBool];
 
    table_array_char.resize(counts[TpArrayUChar]);
    record_array_char.resize(counts[TpArrayUChar]);
    total += counts[TpArrayUChar];

    table_array_short.resize(counts[TpArrayShort]);
    record_array_short.resize(counts[TpArrayShort]);
    total += counts[TpArrayShort];
 
    table_array_int.resize(counts[TpArrayInt]);
    record_array_int.resize(counts[TpArrayInt]);
    total += counts[TpArrayInt];
 
    table_array_float.resize(counts[TpArrayFloat]);
    record_array_float.resize(counts[TpArrayFloat]);
    total += counts[TpArrayFloat];
 
    table_array_double.resize(counts[TpArrayDouble]);
    record_array_double.resize(counts[TpArrayDouble]);
    total += counts[TpArrayDouble];
 
    table_array_complex.resize(counts[TpArrayComplex]);
    record_array_complex.resize(counts[TpArrayComplex]);
    total += counts[TpArrayComplex];
 
    table_array_dcomplex.resize(counts[TpArrayDComplex]);
    record_array_dcomplex.resize(counts[TpArrayDComplex]);
    total += counts[TpArrayDComplex];
 
    table_array_string.resize(counts[TpArrayString]);
    record_array_string.resize(counts[TpArrayString]);
    total += counts[TpArrayString];
 
    // Keeps track of what index we're writing into for each block.
    Block<uInt> where(TpNumberOfTypes);
    Vector<String> colnames(outputTable.tableDesc().columnNames());
    where.set(0);
    for (i=0; i < inputMap.nelements(); i++) {
	if (inputMap(i) != -1) {
	    uInt which = where[inputBuffer.description().type(i)];
	    switch(inputBuffer.description().type(i)) {
	    case TpBool:
		record_bool[which].attachToRecord(inputBuffer, i);
		table_bool[which] = new ScalarColumn<Bool>(outputTable,
							   colnames(inputMap(i)));
		AlwaysAssert(table_bool[which] != 0, AipsError);
		break;
	    case TpUChar:
		record_char[which].attachToRecord(inputBuffer, i);
		table_char[which] = new ScalarColumn<uChar>(outputTable,
							    colnames(inputMap(i)));
		AlwaysAssert(table_char[which] != 0, AipsError);
		break;
	    case TpShort:
		record_short[which].attachToRecord(inputBuffer, i);
		table_short[which] = new ScalarColumn<Short>(outputTable,
							     colnames(inputMap(i)));
		AlwaysAssert(table_short[which] != 0, AipsError);
		break;
	    case TpInt:
		record_int[which].attachToRecord(inputBuffer, i);
		table_int[which] = new ScalarColumn<Int>(outputTable,
							 colnames(inputMap(i)));
		AlwaysAssert(table_int[which] != 0, AipsError);
		break;
	    case TpFloat:
		record_float[which].attachToRecord(inputBuffer, i);
		table_float[which] = new ScalarColumn<Float>(outputTable,
							     colnames(inputMap(i)));
		AlwaysAssert(table_float[which] != 0, AipsError);
		break;
	    case TpDouble:
		record_double[which].attachToRecord(inputBuffer, i);
		table_double[which] = new ScalarColumn<Double>(outputTable,
							       colnames(inputMap(i)));
		AlwaysAssert(table_double[which] != 0, AipsError);
		break;
	    case TpComplex:
		record_complex[which].attachToRecord(inputBuffer, i);
		table_complex[which] = new ScalarColumn<Complex>(outputTable,
								 colnames(inputMap(i)));
		AlwaysAssert(table_complex[which] != 0, AipsError);
		break;
	    case TpDComplex:
		record_dcomplex[which].attachToRecord(inputBuffer, i);
		table_dcomplex[which] = new ScalarColumn<DComplex>(outputTable,
								   colnames(inputMap(i)));
		AlwaysAssert(table_dcomplex[which] != 0, AipsError);
		break;
	    case TpString:
		record_string[which].attachToRecord(inputBuffer, i);
		table_string[which] = new ScalarColumn<String>(outputTable,
							       colnames(inputMap(i)));
		AlwaysAssert(table_string[which] != 0, AipsError);
		break;
	    case TpArrayBool:
		record_array_bool[which].attachToRecord(inputBuffer, i);
		table_array_bool[which] = new ArrayColumn<Bool>(outputTable,
								colnames(inputMap(i)));
		AlwaysAssert(table_array_bool[which] != 0, AipsError);
		break;
	    case TpArrayUChar:
		record_array_char[which].attachToRecord(inputBuffer, i);
		table_array_char[which] = new ArrayColumn<uChar>(outputTable,
								 colnames(inputMap(i)));
		AlwaysAssert(table_array_char[which] != 0, AipsError);
		break;
	    case TpArrayShort:
		record_array_short[which].attachToRecord(inputBuffer, i);
		table_array_short[which] = new ArrayColumn<Short>(outputTable,
								  colnames(inputMap(i)));
		AlwaysAssert(table_array_short[which] != 0, AipsError);
		break;
	    case TpArrayInt:
		record_array_int[which].attachToRecord(inputBuffer, i);
		table_array_int[which] = new ArrayColumn<Int>(outputTable,
							      colnames(inputMap(i)));
		AlwaysAssert(table_array_int[which] != 0, AipsError);
		break;
	    case TpArrayFloat:
		record_array_float[which].attachToRecord(inputBuffer, i);
		table_array_float[which] = new ArrayColumn<Float>(outputTable,
								  colnames(inputMap(i)));
		AlwaysAssert(table_array_float[which] != 0, AipsError);
		break;
	    case TpArrayDouble:
		record_array_double[which].attachToRecord(inputBuffer, i);
		table_array_double[which] = new ArrayColumn<Double>(outputTable,
								    colnames(inputMap(i)));
		AlwaysAssert(table_array_double[which] != 0, AipsError);
		break;
	    case TpArrayComplex:
		record_array_complex[which].attachToRecord(inputBuffer, i);
		table_array_complex[which] = new ArrayColumn<Complex>(outputTable,
								      colnames(inputMap(i)));
		AlwaysAssert(table_array_complex[which] != 0, AipsError);
		break;
	    case TpArrayDComplex:
		record_array_dcomplex[which].attachToRecord(inputBuffer, i);
		table_array_dcomplex[which] = new ArrayColumn<DComplex>(outputTable,
									colnames(inputMap(i)));
		AlwaysAssert(table_array_dcomplex[which] != 0, AipsError);
		break;
	    case TpArrayString:
		record_array_string[which].attachToRecord(inputBuffer, i);
		table_array_string[which] = new ArrayColumn<String>(outputTable,
								    colnames(inputMap(i)));
		AlwaysAssert(table_array_string[which] != 0, AipsError);
		break;
	    default:
		throw(AipsError(
				"CopyRecordToTable::CopyRecordToTable - unknown type"));
	    }
	    where[inputBuffer.description().type(i)]++;
	}
    }
}

CopyRecordToTable::CopyRecordToTable(const CopyRecordToTable &other)
{
    *this = other;
}
 
CopyRecordToTable::~CopyRecordToTable()
{
    clearAll();
}

CopyRecordToTable &CopyRecordToTable::operator=(const CopyRecordToTable &other)
{
    if (this != &other) {
	clearAll();
	table_bool.resize(other.table_bool.nelements(), True);
	record_bool.resize(other.record_bool.nelements(), True);
	for (uInt i=0;i<table_bool.nelements();i++) {
	    table_bool[i] = new ScalarColumn<Bool>(*(other.table_bool[i]));
	    record_bool[i] = other.record_bool[i];
	    AlwaysAssert(table_bool[i], AipsError);
	}
	table_char.resize(other.table_char.nelements(), True);
	record_char.resize(other.record_char.nelements(), True);
	for (uInt i=0;i<table_char.nelements();i++) {
	    table_char[i] = new ScalarColumn<uChar>(*(other.table_char[i]));
	    record_char[i] = other.record_char[i];
	    AlwaysAssert(table_char[i], AipsError);
	}
	table_short.resize(other.table_short.nelements(), True);
	record_short.resize(other.record_short.nelements(), True);
	for (uInt i=0;i<table_short.nelements();i++) {
	    table_short[i] = new ScalarColumn<Short>(*(other.table_short[i]));
	    record_short[i] = other.record_short[i];
	    AlwaysAssert(table_short[i], AipsError);
	}
	table_int.resize(other.table_int.nelements(), True);
	record_int.resize(other.record_int.nelements(), True);
	for (uInt i=0;i<table_int.nelements();i++) {
	    table_int[i] = new ScalarColumn<Int>(*(other.table_int[i]));
	    record_int[i] = other.record_int[i];
	    AlwaysAssert(table_int[i], AipsError);
	}
	table_float.resize(other.table_float.nelements(), True);
	record_float.resize(other.record_float.nelements(), True);
	for (uInt i=0;i<table_float.nelements();i++) {
	    table_float[i] = new ScalarColumn<Float>(*(other.table_float[i]));
	    record_float[i] = other.record_float[i];
	    AlwaysAssert(table_float[i], AipsError);
	}
	table_double.resize(other.table_double.nelements(), True);
	record_double.resize(other.record_double.nelements(), True);
	for (uInt i=0;i<table_double.nelements();i++) {
	    table_double[i] = new ScalarColumn<Double>(*(other.table_double[i]));
	    record_double[i] = other.record_double[i];
	    AlwaysAssert(table_double[i], AipsError);
	}
	table_complex.resize(other.table_complex.nelements(), True);
	record_complex.resize(other.record_complex.nelements(), True);
	for (uInt i=0;i<table_complex.nelements();i++) {
	    table_complex[i] = new ScalarColumn<Complex>(*(other.table_complex[i]));
	    record_complex[i] = other.record_complex[i];
	    AlwaysAssert(table_complex[i], AipsError);
	}
	table_dcomplex.resize(other.table_dcomplex.nelements(), True);
	record_dcomplex.resize(other.record_dcomplex.nelements(), True);
	for (uInt i=0;i<table_dcomplex.nelements();i++) {
	    table_dcomplex[i] = new ScalarColumn<DComplex>(*(other.table_dcomplex[i]));
	    record_dcomplex[i] = other.record_dcomplex[i];
	    AlwaysAssert(table_dcomplex[i], AipsError);
	}
	table_string.resize(other.table_string.nelements(), True);
	record_string.resize(other.record_string.nelements(), True);
	for (uInt i=0;i<table_string.nelements();i++) {
	    table_string[i] = new ScalarColumn<String>(*(other.table_string[i]));
	    record_string[i] = other.record_string[i];
	    AlwaysAssert(table_string[i], AipsError);
	}
	table_array_bool.resize(other.table_array_bool.nelements(), True);
	record_array_bool.resize(other.record_array_bool.nelements(), True);
	for (uInt i=0;i<table_array_bool.nelements();i++) {
	    table_array_bool[i] = new ArrayColumn<Bool>(*(other.table_array_bool[i]));
	    record_array_bool[i] = other.record_array_bool[i];
	    AlwaysAssert(table_array_bool[i], AipsError);
	}
	table_array_char.resize(other.table_array_char.nelements(), True);
	record_array_char.resize(other.record_array_char.nelements(), True);
	for (uInt i=0;i<table_array_char.nelements();i++) {
	    table_array_char[i] = new ArrayColumn<uChar>(*(other.table_array_char[i]));
	    record_array_char[i] = other.record_array_char[i];
	    AlwaysAssert(table_array_char[i], AipsError);
	}
	table_array_short.resize(other.table_array_short.nelements(), True);
	record_array_short.resize(other.record_array_short.nelements(), True);
	for (uInt i=0;i<table_array_short.nelements();i++) {
	    table_array_short[i] = new ArrayColumn<Short>(*(other.table_array_short[i]));
	    record_array_short[i] = other.record_array_short[i];
	    AlwaysAssert(table_array_short[i], AipsError);
	}
	table_array_int.resize(other.table_array_int.nelements(), True);
	record_array_int.resize(other.record_array_int.nelements(), True);
	for (uInt i=0;i<table_array_int.nelements();i++) {
	    table_array_int[i] = new ArrayColumn<Int>(*(other.table_array_int[i]));
	    record_array_int[i] = other.record_array_int[i];
	    AlwaysAssert(table_array_int[i], AipsError);
	}
	table_array_float.resize(other.table_array_float.nelements(), True);
	record_array_float.resize(other.record_array_float.nelements(), True);
	for (uInt i=0;i<table_array_float.nelements();i++) {
	    table_array_float[i] = new ArrayColumn<Float>(*(other.table_array_float[i]));
	    record_array_float[i] = other.record_array_float[i];
	    AlwaysAssert(table_array_float[i], AipsError);
	}
	table_array_double.resize(other.table_array_double.nelements(), True);
	record_array_double.resize(other.record_array_double.nelements(), True);
	for (uInt i=0;i<table_array_double.nelements();i++) {
	    table_array_double[i] = new ArrayColumn<Double>(*(other.table_array_double[i]));
	    record_array_double[i] = other.record_array_double[i];
	    AlwaysAssert(table_array_double[i], AipsError);
	}
	table_array_complex.resize(other.table_array_complex.nelements(), True);
	record_array_complex.resize(other.record_array_complex.nelements(), True);
	for (uInt i=0;i<table_array_complex.nelements();i++) {
	    table_array_complex[i] = new ArrayColumn<Complex>(*(other.table_array_complex[i]));
	    record_array_complex[i] = other.record_array_complex[i];
	    AlwaysAssert(table_array_complex[i], AipsError);
	}
	table_array_dcomplex.resize(other.table_array_dcomplex.nelements(), True);
	record_array_dcomplex.resize(other.record_array_dcomplex.nelements(), True);
	for (uInt i=0;i<table_array_dcomplex.nelements();i++) {
	    table_array_dcomplex[i] = new ArrayColumn<DComplex>(*(other.table_array_dcomplex[i]));
	    record_array_dcomplex[i] = other.record_array_dcomplex[i];
	    AlwaysAssert(table_array_dcomplex[i], AipsError);
	}
	table_array_string.resize(other.table_array_string.nelements(), True);
	record_array_string.resize(other.record_array_string.nelements(), True);
	for (uInt i=0;i<table_array_string.nelements();i++) {
	    table_array_string[i] = new ArrayColumn<String>(*(other.table_array_string[i]));
	    record_array_string[i] = other.record_array_string[i];
	    AlwaysAssert(table_array_string[i], AipsError);
	}
    }
    return *this;
}

void CopyRecordToTable::copy(uInt rownr)
{
    uInt i;

    for (i=0; i < table_bool.nelements(); i++) {
        table_bool[i]->put(rownr, *(record_bool[i]));
    }
 
    for (i=0; i < table_char.nelements(); i++) {
        table_char[i]->put(rownr, *(record_char[i]));
    }
 
    for (i=0; i < table_short.nelements(); i++) {
       table_short[i]->put(rownr, *(record_short[i]));
    }
 
    for (i=0; i < table_int.nelements(); i++) {
        table_int[i]->put(rownr, *(record_int[i]));
    }
 
    for (i=0; i < table_float.nelements(); i++) {
        table_float[i]->put(rownr, *(record_float[i]));
    }
 
    for (i=0; i < table_double.nelements(); i++) {
        table_double[i]->put(rownr, *(record_double[i]));
    }
 
    for (i=0; i < table_complex.nelements(); i++) {
        table_complex[i]->put(rownr, *(record_complex[i]));
    }
 
    for (i=0; i < table_dcomplex.nelements(); i++) {
        table_dcomplex[i]->put(rownr, *(record_dcomplex[i]));
    }
 
    for (i=0; i < table_string.nelements(); i++) {
        table_string[i]->put(rownr, *(record_string[i]));
    }
  
    for (i=0; i < table_array_bool.nelements(); i++) {
        table_array_bool[i]->put(rownr, *(record_array_bool[i]));
    }
 
    for (i=0; i < table_array_char.nelements(); i++) {
        table_array_char[i]->put(rownr, *(record_array_char[i]));
    }
 
    for (i=0; i < table_array_short.nelements(); i++) {
        table_array_short[i]->put(rownr, *(record_array_short[i]));
   }
 
    for (i=0; i < table_array_int.nelements(); i++) {
        table_array_int[i]->put(rownr, *(record_array_int[i]));
    }
 
    for (i=0; i < table_array_float.nelements(); i++) {
        table_array_float[i]->put(rownr, *(record_array_float[i]));
    }
 
    for (i=0; i < table_array_double.nelements(); i++) {
        table_array_double[i]->put(rownr, *(record_array_double[i]));
    }
 
    for (i=0; i < table_array_complex.nelements(); i++) {
        table_array_complex[i]->put(rownr, *(record_array_complex[i]));
    }
 
    for (i=0; i < table_array_dcomplex.nelements(); i++) {
        table_array_dcomplex[i]->put(rownr, *(record_array_dcomplex[i]));
    }
 
    for (i=0; i < table_array_string.nelements(); i++) {
	table_array_string[i]->put(rownr, *(record_array_string[i]));
    }
}


void CopyRecordToTable::clearAll()
{
    uInt i;
 
    for (i=0; i < table_bool.nelements(); i++) {
        delete table_bool[i];
    }
    table_bool.set(static_cast<ScalarColumn<Bool>*>(0));
 
    for (i=0; i < table_char.nelements(); i++) {
        delete table_char[i];
    }
    table_char.set(static_cast<ScalarColumn<uChar>*>(0));
 
    for (i=0; i < table_short.nelements(); i++) {
        delete table_short[i];
    }
    table_short.set(static_cast<ScalarColumn<Short>*>(0));
 
    for (i=0; i < table_int.nelements(); i++) {
        delete table_int[i];
    }
    table_int.set(static_cast<ScalarColumn<Int>*>(0));
 
    for (i=0; i < table_float.nelements(); i++) {
        delete table_float[i];
    }
    table_float.set(static_cast<ScalarColumn<Float>*>(0));
 
    for (i=0; i < table_double.nelements(); i++) {
        delete table_double[i];
    }
    table_double.set(static_cast<ScalarColumn<Double>*>(0));
 
    for (i=0; i < table_complex.nelements(); i++) {
        delete table_complex[i];
    }
    table_complex.set(static_cast<ScalarColumn<Complex>*>(0));
 
    for (i=0; i < table_dcomplex.nelements(); i++) {
        delete table_dcomplex[i];
    }
    table_dcomplex.set(static_cast<ScalarColumn<DComplex>*>(0));
 
    for (i=0; i < table_string.nelements(); i++) {
        delete table_string[i];
    }
    table_string.set(static_cast<ScalarColumn<String>*>(0));
 
    for (i=0; i < table_array_bool.nelements(); i++) {
        delete table_array_bool[i];
    }
    table_array_bool.set(static_cast<ArrayColumn<Bool>*>(0));
 
    for (i=0; i < table_array_char.nelements(); i++) {
        delete table_array_char[i];
    }
    table_array_char.set(static_cast<ArrayColumn<uChar>*>(0));
 
    for (i=0; i < table_array_short.nelements(); i++) {
        delete table_array_short[i];
    }
    table_array_short.set(static_cast<ArrayColumn<Short>*>(0));
 
    for (i=0; i < table_array_int.nelements(); i++) {
        delete table_array_int[i];
    }
    table_array_int.set(static_cast<ArrayColumn<Int>*>(0));
 
    for (i=0; i < table_array_float.nelements(); i++) {
        delete table_array_float[i];
    }
    table_array_float.set(static_cast<ArrayColumn<Float>*>(0));
 
    for (i=0; i < table_array_double.nelements(); i++) {
        delete table_array_double[i];
    }
    table_array_double.set(static_cast<ArrayColumn<Double>*>(0));
 
    for (i=0; i < table_array_complex.nelements(); i++) {
        delete table_array_complex[i];
    }
    table_array_complex.set(static_cast<ArrayColumn<Complex>*>(0));
 
    for (i=0; i < table_array_dcomplex.nelements(); i++) {
        delete table_array_dcomplex[i];
    }
    table_array_dcomplex.set(static_cast<ArrayColumn<DComplex>*>(0));
 
    for (i=0; i < table_array_string.nelements(); i++) {
        delete table_array_string[i];
    }
    table_array_string.set(static_cast<ArrayColumn<String>*>(0));
}

void addRecordDesc(TableDesc &tableDescription,
                          const RecordDesc &recDesc,
                          const String &prefix)
{
    uInt n = recDesc.nfields();
    String fullPrefix = prefix;
    if (! prefix.empty()) {
	fullPrefix = prefix + "_";
    }
    // variable length arrays will likely be identified by a neg. shape
    // but for now, we fake it out with an IPosition involving zeros
//    IPosition varShape(1,-1);
    for (uInt i=0; i < n; i++) {
        String colname = fullPrefix + recDesc.name(i);
        if (recDesc.isScalar(i)) {
            switch(recDesc.type(i)) {
            case TpBool:
                tableDescription.addColumn(ScalarColumnDesc<Bool>(colname));
                break;
            case TpUChar:
                tableDescription.addColumn(ScalarColumnDesc<uChar>(colname));
                break;
            case TpShort:
                tableDescription.addColumn(ScalarColumnDesc<Short>(colname));
                break;
            case TpInt:
                tableDescription.addColumn(ScalarColumnDesc<Int>(colname));
                break;
            case TpFloat:
                tableDescription.addColumn(ScalarColumnDesc<Float>(colname));
                break;
            case TpDouble:
                tableDescription.addColumn(ScalarColumnDesc<Double>(colname));
                break;
            case TpComplex:
                tableDescription.addColumn(ScalarColumnDesc<Complex>(colname));
                break;
            case TpDComplex:
                tableDescription.addColumn(ScalarColumnDesc<DComplex>(colname));
                break;
            case TpString:
                tableDescription.addColumn(ScalarColumnDesc<String>(colname));
                break;
	    default:
                AlwaysAssertExit(0); // NOTREACHED
            }
        } else if (recDesc.isArray(i)) {
            int options = 0;
//            if (recDesc.shape(i) != varShape) options = ColumnDesc::Direct;
	    if (recDesc.shape(i).product() != 0) options = ColumnDesc::Direct;
            switch(recDesc.type(i)) {
            case TpArrayBool:
		if (options != 0) {
		    tableDescription.addColumn(ArrayColumnDesc<Bool>(colname,
                             recDesc.shape(i), options));
		} else {
		    tableDescription.addColumn(ArrayColumnDesc<Bool>(colname,
                             options));
		}
                break;
            case TpArrayUChar:
		if (options != 0) {
		    tableDescription.addColumn(ArrayColumnDesc<uChar>(colname,
                             recDesc.shape(i), options));
		} else {
		    tableDescription.addColumn(ArrayColumnDesc<uChar>(colname,
                             options));
		}
                break;
            case TpArrayShort:
		if (options != 0) {
		    tableDescription.addColumn(ArrayColumnDesc<Short>(colname,
                             recDesc.shape(i), options));
		} else {
		    tableDescription.addColumn(ArrayColumnDesc<Short>(colname,
                             options));
		}
                break;
            case TpArrayInt:
		if (options != 0) {
		    tableDescription.addColumn(ArrayColumnDesc<Int>(colname,
                             recDesc.shape(i), options));
		} else {
		    tableDescription.addColumn(ArrayColumnDesc<Int>(colname,
                             options));
		}
                break;
            case TpArrayFloat:
		if (options != 0) {
		    tableDescription.addColumn(ArrayColumnDesc<Float>(colname,
                             recDesc.shape(i), options));
		} else {
		    tableDescription.addColumn(ArrayColumnDesc<Float>(colname,
                             options));
		}
                break;
            case TpArrayDouble:
		if (options != 0) {
		    tableDescription.addColumn(ArrayColumnDesc<Double>(colname,
                             recDesc.shape(i), options));
		} else {
		    tableDescription.addColumn(ArrayColumnDesc<Double>(colname,
                             options));
		}
                break;
            case TpArrayComplex:
		if (options != 0) {
		    tableDescription.addColumn(ArrayColumnDesc<Complex>(colname,
                             recDesc.shape(i), options));
		} else {
		    tableDescription.addColumn(ArrayColumnDesc<Complex>(colname,
                             options));
		}
                break;
            case TpArrayDComplex:
		if (options != 0) {
		   tableDescription.addColumn(ArrayColumnDesc<DComplex>(colname,
                             recDesc.shape(i), options));
		} else {
		   tableDescription.addColumn(ArrayColumnDesc<DComplex>(colname,
                             options));
		}
		break;
            case TpArrayString:
		if (options != 0) {
		    tableDescription.addColumn(ArrayColumnDesc<String>(colname,
                             recDesc.shape(i), options));
		} else {
		    tableDescription.addColumn(ArrayColumnDesc<String>(colname,
                             options));
		}
                break;
            default:
                AlwaysAssertExit(0); // NOTREACHED
            }
        } else {
            AlwaysAssertExit(0); // NOTREACHED
        }
    }
}

CopyRecordToRecord::CopyRecordToRecord(RecordInterface &outputBuffer,
				       const RecordInterface &inputBuffer,
				       const Vector<Int> inputMap)
{
    Block<Int> counts(TpNumberOfTypes);
    counts.set(0);
    // Count how many fields of each type exist
    uInt n = inputBuffer.nfields();
    uInt i;
    for (i=0; i < n; i++) {
        if (inputMap(i) != -1) counts[inputBuffer.description().type(i)]++;
    }
    uInt total = 0;
    out_record_bool.resize(counts[TpBool]); in_record_bool.resize(counts[TpBool]);
    total += counts[TpBool];
 
    out_record_char.resize(counts[TpUChar]); in_record_char.resize(counts[TpUChar]);
    total += counts[TpUChar];
 
    out_record_short.resize(counts[TpShort]); in_record_short.resize(counts[TpShort]);
    total += counts[TpShort];
 
    out_record_int.resize(counts[TpInt]); in_record_int.resize(counts[TpInt]);
    total += counts[TpInt];
 
    out_record_float.resize(counts[TpFloat]); in_record_float.resize(counts[TpFloat]);
    total += counts[TpFloat];
 
    out_record_double.resize(counts[TpDouble]);
    in_record_double.resize(counts[TpDouble]);
    total += counts[TpDouble];
 
    out_record_complex.resize(counts[TpComplex]);
    in_record_complex.resize(counts[TpComplex]);
    total += counts[TpComplex];
 
    out_record_dcomplex.resize(counts[TpDComplex]);
    in_record_dcomplex.resize(counts[TpDComplex]);
    total += counts[TpDComplex];
 
    out_record_string.resize(counts[TpString]);
    in_record_string.resize(counts[TpString]);
    total += counts[TpString];
 
    out_record_array_bool.resize(counts[TpArrayBool]);
    in_record_array_bool.resize(counts[TpArrayBool]);
    total += counts[TpArrayBool];
 
    out_record_array_char.resize(counts[TpArrayUChar]);
    in_record_array_char.resize(counts[TpArrayUChar]);
    total += counts[TpArrayUChar];
 
    out_record_array_short.resize(counts[TpArrayShort]);
    in_record_array_short.resize(counts[TpArrayShort]);
    total += counts[TpArrayShort];
 
    out_record_array_int.resize(counts[TpArrayInt]);
    in_record_array_int.resize(counts[TpArrayInt]);
    total += counts[TpArrayInt];
 
    out_record_array_float.resize(counts[TpArrayFloat]);
    in_record_array_float.resize(counts[TpArrayFloat]);
    total += counts[TpArrayFloat];
 
    out_record_array_double.resize(counts[TpArrayDouble]);
    in_record_array_double.resize(counts[TpArrayDouble]);
    total += counts[TpArrayDouble];
 
    out_record_array_complex.resize(counts[TpArrayComplex]);
    in_record_array_complex.resize(counts[TpArrayComplex]);
    total += counts[TpArrayComplex];
 
    out_record_array_dcomplex.resize(counts[TpArrayDComplex]);
    in_record_array_dcomplex.resize(counts[TpArrayDComplex]);
    total += counts[TpArrayDComplex];
 
    out_record_array_string.resize(counts[TpArrayString]);
    in_record_array_string.resize(counts[TpArrayString]);
    total += counts[TpArrayString];
  
    // Keeps track of what index we're writing into for each block.
    Block<uInt> where(TpNumberOfTypes);
    where.set(0);
    for (i=0; i < inputMap.nelements(); i++) {
        if (inputMap(i) != -1) {
	    uInt which = where[inputBuffer.description().type(i)];
	    switch(inputBuffer.description().type(i)) {
	    case TpBool:
		in_record_bool[which].attachToRecord(inputBuffer, i);
		out_record_bool[which].attachToRecord(outputBuffer,
						      inputMap(i));
		break;
	    case TpUChar:
		in_record_char[which].attachToRecord(inputBuffer, i);
		out_record_char[which].attachToRecord(outputBuffer,
						      inputMap(i));
		break;
	    case TpShort:
		in_record_short[which].attachToRecord(inputBuffer, i);
		out_record_short[which].attachToRecord(outputBuffer,
						       inputMap(i));
		break;
	    case TpInt:
		in_record_int[which].attachToRecord(inputBuffer, i);
		out_record_int[which].attachToRecord(outputBuffer,
						     inputMap(i));
		break;
	    case TpFloat:
		in_record_float[which].attachToRecord(inputBuffer, i);
		out_record_float[which].attachToRecord(outputBuffer,
						       inputMap(i));
		break;
	    case TpDouble:
		in_record_double[which].attachToRecord(inputBuffer, i);
		out_record_double[which].attachToRecord(outputBuffer,
							inputMap(i));
		break;
	    case TpComplex:
		in_record_complex[which].attachToRecord(inputBuffer, i);
		out_record_complex[which].attachToRecord(outputBuffer,
							 inputMap(i));
		break;
	    case TpDComplex:
		in_record_dcomplex[which].attachToRecord(inputBuffer, i);
		out_record_dcomplex[which].attachToRecord(outputBuffer,
							  inputMap(i));
		break;
	    case TpString:
		in_record_string[which].attachToRecord(inputBuffer, i);
		out_record_string[which].attachToRecord(outputBuffer,
							inputMap(i));
		break;
	    case TpArrayBool:
		in_record_array_bool[which].attachToRecord(inputBuffer, i);
		out_record_array_bool[which].attachToRecord(outputBuffer,
							    inputMap(i));
		break;
	    case TpArrayUChar:
		in_record_array_char[which].attachToRecord(inputBuffer, i);
		out_record_array_char[which].attachToRecord(outputBuffer,
							    inputMap(i));
		break;
	    case TpArrayShort:
		in_record_array_short[which].attachToRecord(inputBuffer, i);
		out_record_array_short[which].attachToRecord(outputBuffer,
							     inputMap(i));
		break;
	    case TpArrayInt:
		in_record_array_int[which].attachToRecord(inputBuffer, i);
		out_record_array_int[which].attachToRecord(outputBuffer,
							   inputMap(i));
		break;
	    case TpArrayFloat:
		in_record_array_float[which].attachToRecord(inputBuffer, i);
		out_record_array_float[which].attachToRecord(outputBuffer,
							     inputMap(i));
		break;
	    case TpArrayDouble:
		in_record_array_double[which].attachToRecord(inputBuffer, i);
		out_record_array_double[which].attachToRecord(outputBuffer,
							      inputMap(i));
		break;
	    case TpArrayComplex:
		in_record_array_complex[which].attachToRecord(inputBuffer, i);
		out_record_array_complex[which].attachToRecord(outputBuffer,
							       inputMap(i));
		break;
	    case TpArrayDComplex:
		in_record_array_dcomplex[which].attachToRecord(inputBuffer, i);
		out_record_array_dcomplex[which].attachToRecord(outputBuffer,
								inputMap(i));
		break;
	    case TpArrayString:
		in_record_array_string[which].attachToRecord(inputBuffer, i);
		out_record_array_string[which].attachToRecord(outputBuffer,
							      inputMap(i));
		break;
	    default:
		throw(AipsError(
		    "CopyRecordToRecord::CopyRecordToRecord - unknown type"));
	    }
	    where[inputBuffer.description().type(i)]++;
	}
    }
}
 
CopyRecordToRecord::~CopyRecordToRecord()
{
    // nothing
}
 
void CopyRecordToRecord::copy()
{
    uInt i;

    for (i=0; i < out_record_bool.nelements(); i++) {
        *(out_record_bool[i]) = *(in_record_bool[i]);
    }
 
    for (i=0; i < out_record_char.nelements(); i++) {
        *(out_record_char[i]) = *(in_record_char[i]);
    }
 
    for (i=0; i < out_record_short.nelements(); i++) {
	*(out_record_short[i]) = *(in_record_short[i]);
    }
 
    for (i=0; i < out_record_int.nelements(); i++) {
        *(out_record_int[i]) = *(in_record_int[i]);
    }
 
    for (i=0; i < out_record_float.nelements(); i++) {
        *(out_record_float[i]) = *(in_record_float[i]);
    }
 
    for (i=0; i < out_record_double.nelements(); i++) {
        *(out_record_double[i]) = *(in_record_double[i]);
    }
 
    for (i=0; i < out_record_complex.nelements(); i++) {
        *(out_record_complex[i]) = *(in_record_complex[i]);
    }
 
    for (i=0; i < out_record_dcomplex.nelements(); i++) {
        *(out_record_dcomplex[i]) = *(in_record_dcomplex[i]);
    }
 
    for (i=0; i < out_record_string.nelements(); i++) {
        *(out_record_string[i]) = *(in_record_string[i]);
    }
  
    for (i=0; i < out_record_array_bool.nelements(); i++) {
        *(out_record_array_bool[i]) = *(in_record_array_bool[i]);
    }
 
    for (i=0; i < out_record_array_char.nelements(); i++) {
        *(out_record_array_char[i]) = *(in_record_array_char[i]);
    }
 
    for (i=0; i < out_record_array_short.nelements(); i++) {
        *(out_record_array_short[i]) = *(in_record_array_short[i]);
   }
 
    for (i=0; i < out_record_array_int.nelements(); i++) {
        *(out_record_array_int[i]) = *(in_record_array_int[i]);
    }
 
    for (i=0; i < out_record_array_float.nelements(); i++) {
        *(out_record_array_float[i]) = *(in_record_array_float[i]);
    }
 
    for (i=0; i < out_record_array_double.nelements(); i++) {
        *(out_record_array_double[i]) = *(in_record_array_double[i]);
    }
 
    for (i=0; i < out_record_array_complex.nelements(); i++) {
        *(out_record_array_complex[i]) = *(in_record_array_complex[i]);
    }
 
    for (i=0; i < out_record_array_dcomplex.nelements(); i++) {
        *(out_record_array_dcomplex[i]) = *(in_record_array_dcomplex[i]);
    }
 
    for (i=0; i < out_record_array_string.nelements(); i++) {
	*(out_record_array_string[i]) = *(in_record_array_string[i]);
    }
}

} //# NAMESPACE CASACORE - END

