//# dVirtColEng.cc: Demo of a virtual column engine
//# Copyright (C) 1994,1995,1996,1997,1999,2001
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

//# Define a main program to allow compalition and linking by the make system.
//# The variable is set in tVirtColEng.cc to skip it.
#if !defined(DVIRTCOLENG_MAIN)
int main()
{ return 0; }
#endif


//# Includes
#include "dVirtColEng.h"
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/IO/AipsIO.h>


#include <casacore/casa/namespace.h>
DummyVirtualEngine::DummyVirtualEngine()
: data1_p (this, 1.0),
  data2_p (this, 1.0)
{}

DummyVirtualEngine::DummyVirtualEngine (double scale1, double scale2)
: data1_p (this, scale1),
  data2_p (this, scale2)
{}

DummyVirtualEngine::~DummyVirtualEngine()
{}

// Clone the engine object.
DataManager* DummyVirtualEngine::clone() const
{
    DataManager* dmPtr = new DummyVirtualEngine (data1_p.scale(),
						 data2_p.scale());
    if (dmPtr == 0) {
	throw (AllocError ("DummyVirtualEngine::clone()", 1));
    }
    return dmPtr;
}


DataManagerColumn* DummyVirtualEngine::makeScalarColumn (const String&, int,
							 const String&)
{
    return &data1_p;
}

DataManagerColumn* DummyVirtualEngine::makeIndArrColumn (const String&, int,
							 const String&)
{
    return &data2_p;
}

Bool DummyVirtualEngine::flush (AipsIO& ios, Bool)
{
    data1_p.flush (ios);
    data2_p.flush (ios);
    return True;
}
void DummyVirtualEngine::create (uInt)
{}
void DummyVirtualEngine::open (uInt, AipsIO& ios)
{
    data1_p.open (ios);
    data2_p.open (ios);
}
void DummyVirtualEngine::prepare ()
{
    data1_p.prepare (table());
    data2_p.prepare (table());
}

DataManager* DummyVirtualEngine::makeObject (const String&, const Record&)
{
    DataManager* dmPtr = new DummyVirtualEngine();
    if (dmPtr == 0) {
	throw (AllocError ("DummyVirtualEngine::makeObject()", 1));
    }
    return dmPtr;
}
void DummyVirtualEngine::registerClass()
{
    DataManager::registerCtor ("DummyVirtualEngine",
			       DummyVirtualEngine::makeObject);
}
String DummyVirtualEngine::dataManagerType() const
{
    return "DummyVirtualEngine";
}



DummyVirtualScalar::DummyVirtualScalar (DummyVirtualEngine* dve, double scale)
: enginePtr_p(dve),
  scale_p    (scale),
  writable_p (0),
  column_p   (0)
{}

//# This copy constructor should only be called by VirtualColumnEngine::clone,
//# when writable_p and the column_p variables are not filled yet.
//# Check if that is indeed the case.
DummyVirtualScalar::DummyVirtualScalar (const DummyVirtualScalar& that)
: VirtualScalarColumn<Double>(),
  enginePtr_p(that.enginePtr_p),
  scale_p    (that.scale_p),
  writable_p (0),
  column_p   (0)
{
    if (that.writable_p  ||  that.column_p) {
	throw (DataManInternalError ("DummyVirtualScalar copy ctor"));
    }
}


DummyVirtualScalar::~DummyVirtualScalar()
{
    delete column_p;
}

void DummyVirtualScalar::prepare (const Table& table)
{
    //# Determine if the column is writable.
    writable_p = (table.isColumnWritable ("DATA1")  ?  1 : -1);
    column_p = new ScalarColumn<Int> (table, "DATA1");
}
void DummyVirtualScalar::open (AipsIO& ios)
{
    ios.getstart ("DummyVirtualScalar");
    ios >> scale_p;
    ios.getend();
}
void DummyVirtualScalar::flush (AipsIO& ios)
{
    ios.putstart ("DummyVirtualScalar", 1);      // class version 1
    ios << scale_p;
    ios.putend();
}

// The function create is called upon initialization of the virtual column.
// The initialization order of the columns is undetermined, which means
// that this function isWritable can be called before the column has been
// initialized.
// For example, suppose column A uses column B and A get initialized
// before B. Then A will call B's isWritable(), while B has not been
// initialized yet.
// This all means that isWritable must take care of the case
// where the writable_p flag is not set yet.
Bool DummyVirtualScalar::isWritable() const
{
    if (writable_p == 0) {
	return enginePtr_p->table().isColumnWritable ("DATA1");
    }
    return (writable_p > 0  ?  True : False);
}

void DummyVirtualScalar::get (uInt rownr, double& data)
{
    data = scale_p * (*column_p)(rownr);
}
void DummyVirtualScalar::getdoubleV (uInt rownr, double* dataPtr)
{
    *dataPtr = scale_p * (*column_p)(rownr);
}    

void DummyVirtualScalar::put (uInt rownr, const double& data)
{
    column_p->put (rownr, Int(data / scale_p));
}
void DummyVirtualScalar::putdoubleV (uInt rownr, const double* dataPtr)
{
    column_p->put (rownr, Int(*dataPtr / scale_p));
}




DummyVirtualArray::DummyVirtualArray (DummyVirtualEngine* dve, double scale)
: enginePtr_p(dve),
  scale_p    (scale),
  writable_p (0),
  column_p   (0)
{}

//# This copy constructor should only be called by VirtualColumnEngine::clone,
//# when writable_p and the column_p variables are not filled yet.
//# Check if that is indeed the case.
DummyVirtualArray::DummyVirtualArray (const DummyVirtualArray& that)
: VirtualArrayColumn<Double>(),
  enginePtr_p(that.enginePtr_p),
  scale_p    (that.scale_p),
  writable_p (0),
  column_p   (0)
{
    if (that.writable_p  ||  that.column_p) {
	throw (DataManInternalError ("DummyVirtualArray copy ctor"));
    }
}

DummyVirtualArray::~DummyVirtualArray()
{
    delete column_p;
}

void DummyVirtualArray::prepare (const Table& table)
{
    //# Determine if the column is writable.
    writable_p = (table.isColumnWritable ("DATA2")  ?  1 : -1);
    column_p = new ArrayColumn<Int> (table, "DATA2");
}
void DummyVirtualArray::open (AipsIO& ios)
{
    ios.getstart ("DummyVirtualArray");
    ios >> scale_p;
    ios.getend();
}
void DummyVirtualArray::flush (AipsIO& ios)
{
    ios.putstart ("DummyVirtualArray", 1);      // class version 1
    ios << scale_p;
    ios.putend();
}

Bool DummyVirtualArray::isWritable() const
{
    if (writable_p == 0) {
	return enginePtr_p->table().isColumnWritable ("DATA2");
    }
    return (writable_p > 0  ?  True : False);
}

void DummyVirtualArray::setShape (uInt rownr, const IPosition& shape)
{
    column_p->setShape (rownr, shape);
}
Bool DummyVirtualArray::isShapeDefined (uInt rownr)
{
    return column_p->isDefined (rownr);
}
uInt DummyVirtualArray::ndim (uInt rownr)
{
    return column_p->ndim (rownr);
}
IPosition DummyVirtualArray::shape (uInt rownr)
{
    return column_p->shape (rownr);
}

void DummyVirtualArray::getArray (uInt rownr, Array<double>& array)
{
    Array<Int> intern(array.shape());
    column_p->get (rownr, intern);
    Bool deleteIn, deleteOut;
    double* out = array.getStorage (deleteOut);
    double* op  = out;
    const Int* in = intern.getStorage (deleteIn);
    const Int* ip = in;
    const Int* last = ip + array.nelements();
    while (ip < last) {
	*op++ = *ip++ * scale_p;
    }
    intern.freeStorage (in, deleteIn);
    array.putStorage (out, deleteOut);
}
void DummyVirtualArray::putArray (uInt rownr, const Array<double>& array)
{
    Array<Int> intern(array.shape());
    Bool deleteIn, deleteOut;
    const double* in = array.getStorage (deleteIn);
    const double* ip = in;
    Int* out = intern.getStorage (deleteOut);
    Int* op  = out;
    const Int* last = op + array.nelements();
    while (op < last) {
	*op++ = Int (*ip++ / scale_p + 0.5);
    }
    array.freeStorage (in, deleteIn);
    intern.putStorage (out, deleteOut);
    column_p->put (rownr, intern);
}
