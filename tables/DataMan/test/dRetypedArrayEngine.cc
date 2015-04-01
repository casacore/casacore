//# dRetypedArrayEngine.cc: Test program for class RetypedArrayEngine
//# Copyright (C) 1995,1996,1999,2000,2001,2002
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
//#include <casacore/tables/Tables/test/dRetypedArrayEngine.h>
#include "dRetypedArrayEngine.h"
#include <casacore/tables/DataMan/RetypedArrayEngine.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Containers/RecordDesc.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
void* RetypedArrayEx1::newCopyInfo (const TableRecord&, const IPosition&)
{ return 0; }
void RetypedArrayEx1::deleteCopyInfo (void*)
{}
void RetypedArrayEx1::set (void*, void* vout,
			   const Array<float>& in,
			   const IPosition& shape)
{
    Array<RetypedArrayEx1>& out = *(Array<RetypedArrayEx1>*)vout;
    if (shape.nelements() == 1  &&  shape(0) == 2) {
	retypedArrayEngineSet (out, in);
    }else{
	throw (DataManError ("RetypedArrayEx1::set"));
    }
}
void RetypedArrayEx1::get (void*, Array<float>& out,
			   const void* vin,
			   const IPosition& shape)
{
    const Array<RetypedArrayEx1>& in = *(const Array<RetypedArrayEx1>*)vin;
    if (shape.nelements() == 1  &&  shape(0) == 2) {
	retypedArrayEngineGet (out, in);
    }else{
	throw (DataManError ("RetypedArrayEx1::get"));
    }
}



void* RetypedArrayEx2::newCopyInfo (const TableRecord& record,
				    const IPosition& shape)
{
    return new CopyInfo (record, shape);
}
void RetypedArrayEx2::deleteCopyInfo (void* copyInfo)
{
    delete (CopyInfo*)copyInfo;
}

RetypedArrayEx2::CopyInfo::CopyInfo (const TableRecord& record,
				     const IPosition& shape)
: mask_p   (new Vector<Bool>),
  nrTrue_p (0)
{
    Int fieldnr = record.description().fieldNumber ("mask");
    if (fieldnr >= 0) {
	RORecordFieldPtr<Array<Bool> > field (record, fieldnr);
	*mask_p = *field;
	AlwaysAssert (mask_p->nelements() == 4, DataManError);
    }
    for (uInt i=0; i<mask_p->nelements(); i++) {
	if ((*mask_p)(i)) {
	    nrTrue_p++;
	}
    }
    // The shape must be 1-dimensionsal.
    AlwaysAssert (shape.nelements() == 1, DataManError);
    // When a mask is given, it must match the shape.
    if (nrTrue_p > 0) {
	AlwaysAssert (shape(0) == Int(nrTrue_p), DataManError);
    }
}

RetypedArrayEx2::CopyInfo::~CopyInfo()
{
    delete mask_p;
}

void RetypedArrayEx2::CopyInfo::set (void* vout,
				     const Array<DComplex>& in,
				     const IPosition& shape)
{
    Array<RetypedArrayEx2>& out = *(Array<RetypedArrayEx2>*)vout;
    AlwaysAssert (shape.nelements() == 1, DataManError);
    if (shape(0) == 4) {
	retypedArrayEngineSet (out, in);
    }else{
	AlwaysAssert (shape(0) == Int(nrTrue_p), DataManError);
	retypedArrayEngineSet (out, in, shape, (void*)mask_p);
    }
}
void RetypedArrayEx2::CopyInfo::get (Array<DComplex>& out,
				     const void* vin,
				     const IPosition& shape)
{
    const Array<RetypedArrayEx2>& in = *(const Array<RetypedArrayEx2>*)vin;
    AlwaysAssert (shape.nelements() == 1, DataManError);
    if (shape(0) == 4) {
	retypedArrayEngineGet (out, in);
    }else{
	retypedArrayEngineGet (out, in, shape, (void*)mask_p);
    }
}

void RetypedArrayEx2::setElem (const DComplex* data, const IPosition&,
			       const void* maskPtr)
{
    const Vector<Bool>& mask = *(const Vector<Bool>*)maskPtr;
    if (mask(0)) {
	I_p = *data++;
    }else{
	I_p = 0;
    }
    if (mask(1)) {
	Q_p = *data++;
    }else{
	Q_p = 0;
    }
    if (mask(2)) {
	U_p = *data++;
    }else{
	U_p = 0;
    }
    if (mask(3)) {
	V_p = *data;
    }else{
	V_p = 0;
    }
}
void RetypedArrayEx2::getElem (DComplex* data, const IPosition&,
			       const void* maskPtr) const
{
    const Vector<Bool>& mask = *(const Vector<Bool>*)maskPtr;
    if (mask(0)) {
	*data++ = I_p;
    }
    if (mask(1)) {
	*data++ = Q_p;
    }
    if (mask(2)) {
	*data++ = U_p;
    }
    if (mask(3)) {
	*data = V_p;
    }
}


// <summary> Test program for class RetypedArrayEngine </summary>

// This program tests the virtual column engine RetypedArrayEngine.
// It is using the example classes RetypedArrayEx* for that purpose.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.

void a(Bool doExcp);
void b();
void c();


int main (int argc, const char*[])
{
    try {
	a( (argc<2));
	b();
	c();
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}

// First build a description.
void a (Bool doExcp)
{
    // First register the virtual column engine.
    RetypedArrayEngine<RetypedArrayEx1,float>::registerClass();
    // Add ArrayColumnDesc<RetypedArrayEx1> to column type map.
    ArrayColumnDesc<RetypedArrayEx1>("x").registerClass();

    // Build the table description.
    TableDesc td("", "1", TableDesc::Scratch);
    td.comment() = "A test of class TableDesc";
    td.addColumn (ArrayColumnDesc<float> ("Data", IPosition(2,2,10),
					  ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<RetypedArrayEx1>
		    ("colA", "",
		     RetypedArrayEngine<RetypedArrayEx1,float>::className(),
		     "", IPosition(1,10), ColumnDesc::FixedShape));

    // Now create a new table from the description.
    SetupNewTable newtab("dRetypedArrayEngine_tmp.data", td, Table::New);
    // Create the virtual column engine with the target columns Data.
    RetypedArrayEngine<RetypedArrayEx1,float> engine ("colA", "Data");
    newtab.bindColumn ("colA", engine);
    Table tab(newtab, 50);

    // Fill the table via the virtual columns.
    ArrayColumn<RetypedArrayEx1> colA (tab, "colA");
    Vector<RetypedArrayEx1> vec(10);
    uInt i;
    for (i=0; i<tab.nrow(); i++) {
	for (uInt j=0; j<10; j++) {
	    vec(j) = RetypedArrayEx1(i*100+j, i*100+j+10000);
	}
	colA.put (i, vec);
    }

    //# Do an erronous thing.
    SetupNewTable newtab2("dRetypedArrayEngine_tmp.dat2", td, Table::Scratch);
    newtab2.bindColumn ("Data", engine);
    if (doExcp) {
///	try {
///	    Table tab2(newtab2, 50);           // bound to incorrect column
///	} catch (AipsError x) {
///	    cout << x.getMesg() << endl;
///	} 
    }
}

void b()
{
    // Read back the table.
    Table tab("dRetypedArrayEngine_tmp.data");
    ArrayColumn<float> colD (tab, "Data");
    ArrayColumn<RetypedArrayEx1> colA(tab, "colA");
    Matrix<float> valD;
    Vector<RetypedArrayEx1> valA, valA1;
    Matrix<float> resD(2,10);
    Vector<RetypedArrayEx1> resA(10);
    Slice slice(1,5,2);
    uInt i=0;
    i = 0;
    for (i=0; i<tab.nrow(); i++) {
	for (uInt j=0; j<10; j++) {
	    resD(0,j) = i*100+j;
	    resD(1,j) = resD(0,j) + 10000;
	    resA(j) = RetypedArrayEx1(resD(0,j), resD(1,j));
	}
	cout << "get row " << i << endl;
	colD.get (i, valD);
	colA.get (i, valA);
	colA.getSlice (i, slice, valA1);
	if (! allEQ (valD, resD)) {
	    cout << "Error in Data in row " << i << endl;
	}
	if (! allEQ (valA, resA)) {
	    cout << "Error in colA in row " << i << endl;
	}
	if (! allEQ (valA1, resA(slice))) {
	    cout << "Error in colA slice in row " << i << endl;
	}
    }
    Matrix<RetypedArrayEx1> matA = colA.getColumn();
    for (i=0; i<tab.nrow(); i++) {
	for (uInt j=0; j<10; j++) {
	    if (!(matA(j,i) == RetypedArrayEx1(i*100+j, i*100+j+10000))) {
		cout << "error in matA(" << j << "," << i << "): "
		     << matA(j,i).x() << " " << matA(j,i).y() << endl;
	    }
	}
    }
}


void c()
{
    // First register the virtual column engine.
    RetypedArrayEngine<RetypedArrayEx2,DComplex>::registerClass();
    // Add ArrayColumnDesc<RetypedArrayEx2> to column type map.
    ArrayColumnDesc<RetypedArrayEx2>("x").registerClass();

    // Build the table description.
    TableDesc td("", "1", TableDesc::Scratch);
    td.addColumn (ArrayColumnDesc<DComplex> ("Data"));
    td.addColumn (ArrayColumnDesc<RetypedArrayEx2> ("Stokes"));
  {
    // Now create a new table from the description.
    SetupNewTable newtab("dRetypedArrayEngine_tmp.data", td, Table::New);
    // Create the virtual column engine with the target columns Data.
    RetypedArrayEngine<RetypedArrayEx2,DComplex> engine ("Stokes", "Data");
    newtab.bindColumn ("Stokes", engine);
    Table tab(newtab, 50);

    // Fill the table via the virtual columns.
    ArrayColumn<RetypedArrayEx2> stokesColumn (tab, "Stokes");
    Vector<RetypedArrayEx2> vec(10);
    uInt i;
    for (i=0; i<tab.nrow(); i++) {
	for (uInt j=0; j<10; j++) {
	    uInt v = i*100 + j;
	    vec(j) = RetypedArrayEx2(v, v+10000, v+50000, v+90000);
	}
	stokesColumn.put (i, vec);
    }
  }
  {
    // Read back the table.
    Table tab("dRetypedArrayEngine_tmp.data");
    ArrayColumn<DComplex> colD (tab, "Data");
    ArrayColumn<RetypedArrayEx2> colA(tab, "Stokes");
    Matrix<DComplex> valD;
    Vector<RetypedArrayEx2> valA;
    Matrix<DComplex> resD(4,10);
    Vector<RetypedArrayEx2> resA(10);
    uInt i=0;
    i = 0;
    for (i=0; i<tab.nrow(); i++) {
	for (uInt j=0; j<10; j++) {
	    uInt v = i*100 + j;
	    resD(0,j) = DComplex(v);
	    resD(1,j) = DComplex(v + 10000);
	    resD(2,j) = DComplex(v + 50000);
	    resD(3,j) = DComplex(v + 90000);
	    resA(j) = RetypedArrayEx2(resD(0,j), resD(1,j),
				      resD(2,j), resD(3,j));
	}
	colD.get (i, valD);
	colA.get (i, valA);
	if (! allEQ (valD, resD)) {
	    cout << "Error in Data in row " << i << endl;
	}
	if (! allEQ (valA, resA)) {
	    cout << "Error in Stokes in row " << i << endl;
	}
    }
  }

  {
    // Now create a new table from the description.
    SetupNewTable newtab("dRetypedArrayEngine_tmp.data", td, Table::New);
    // Create the virtual column engine with the target columns Data.
    RecordDesc rdesc;
    rdesc.addField ("mask", TpArrayBool);
    TableRecord record (rdesc);
    RecordFieldPtr<Array<Bool> > field (record, 0);
    // Only the I and Q value are used, so the shape is [2].
    Vector<Bool> mask(4);
    mask = False;
    mask(0) = True;
    mask(1) = True;
    *field = mask;
    RetypedArrayEngine<RetypedArrayEx2,DComplex> engine ("Stokes", "Data",
							 IPosition(1,2),
							 record);
    newtab.bindColumn ("Stokes", engine);
    Table tab(newtab, 50);

    // Fill the table via the virtual columns.
    ArrayColumn<RetypedArrayEx2> stokesColumn (tab, "Stokes");
    Vector<RetypedArrayEx2> vec(10);
    uInt i;
    for (i=0; i<tab.nrow(); i++) {
	for (uInt j=0; j<10; j++) {
	    uInt v = i*100 + j;
	    vec(j) = RetypedArrayEx2(v, v+10000, v+50000, v+90000);
	}
	stokesColumn.put (i, vec);
    }
  }
  {
    // Read back the table.
    Table tab("dRetypedArrayEngine_tmp.data");
    ArrayColumn<DComplex> colD (tab, "Data");
    ArrayColumn<RetypedArrayEx2> colA(tab, "Stokes");
    Matrix<DComplex> valD;
    Vector<RetypedArrayEx2> valA;
    Matrix<DComplex> resD(2,10);
    Vector<RetypedArrayEx2> resA(10);
    uInt i=0;
    i = 0;
    for (i=0; i<tab.nrow(); i++) {
	for (uInt j=0; j<10; j++) {
	    uInt v = i*100 + j;
	    resD(0,j) = DComplex(v);
	    resD(1,j) = DComplex(v + 10000);
	    resA(j) = RetypedArrayEx2(resD(0,j), resD(1,j),
				      DComplex(0), DComplex(0));
	}
	colD.get (i, valD);
	colA.get (i, valA);
	if (! allEQ (valD, resD)) {
	    cout << "Error in Data in row " << i << endl;
	}
	if (! allEQ (valA, resA)) {
	    cout << "Error in Stokes in row " << i << endl;
	}
    }
  }
}
