//# BaseColumn.cc: Abstract base class for a table column
//# Copyright (C) 1994,1995,1996,1998
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

#include <aips/Tables/BaseColumn.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Tables/TableError.h>

BaseColumn::BaseColumn (const BaseColumnDesc* cdp)
: colDescPtr_p(cdp),
    // The following cast is safe, because this class uses colDesc_p
    // only to give a const ColumnDesc&.
  colDesc_p   ((BaseColumnDesc*)cdp)
{}

BaseColumn::~BaseColumn()
{}


//# By default all functions throw an exception
//# to ensure they are called correctly.

void BaseColumn::setShape (uInt, const IPosition&)
    { throw (TableInvOper ("setShape() only valid for an array")); }

void BaseColumn::setShape (uInt, const IPosition&, const IPosition&)
    { throw (TableInvOper ("setShape() only valid for an array")); }

uInt BaseColumn::ndimColumn() const
{
    throw (TableInvOper ("ndimColumn() only valid for an array"));
    return 0;
}

IPosition BaseColumn::shapeColumn() const
{
    throw (TableInvOper ("shapeColumn() only valid for an array"));
    return IPosition(0);
}

uInt BaseColumn::ndim (uInt) const
{
    throw (TableInvOper ("ndim() only valid for an array"));
    return 0;
}

IPosition BaseColumn::shape (uInt) const
{
    throw (TableInvOper ("shape() only valid for an array"));
    return IPosition(0);
}


Bool BaseColumn::canChangeShape() const
{
    return False;                      // can not be changed
}

Bool BaseColumn::canAccessScalarColumn (Bool& reask) const
{
    reask = False;                     // By default an entire column
    return False;                      // can never be accessed
}
Bool BaseColumn::canAccessArrayColumn (Bool& reask) const
{
    reask = False;                     // By default an entire column
    return False;                      // can never be accessed
}
Bool BaseColumn::canAccessScalarColumnCells (Bool& reask) const
{
    reask = False;                     // By default cells in a column
    return False;                      // can never be accessed
}
Bool BaseColumn::canAccessArrayColumnCells (Bool& reask) const
{
    reask = False;                     // By default cells in a column
    return False;                      // can never be accessed
}
Bool BaseColumn::canAccessSlice (Bool& reask) const
{
    reask = False;                     // By default a cell slice
    return False;                      // can never be accessed
}
Bool BaseColumn::canAccessColumnSlice (Bool& reask) const
{
    reask = False;                     // By default a column slice
    return False;                      // can never be accessed
}


void BaseColumn::getSlice (uInt, const Slicer&, void*) const
    { throw (TableInvOper ("getSlice() only valid for an array")); }

void BaseColumn::getScalarColumn (void*) const
    { throw (TableInvOper ("getScalarColumn() not implemented")); }

void BaseColumn::getArrayColumn (void*) const
    { throw (TableInvOper ("getArrayColumn() not implemented")); }

void BaseColumn::getScalarColumnCells (const RefRows&, void*) const
    { throw (TableInvOper ("getScalarColumnCells() not implemented")); }

void BaseColumn::getArrayColumnCells (const RefRows&, void*) const
    { throw (TableInvOper ("getArrayColumnCells() not implemented")); }

void BaseColumn::getColumnSliceCells (const RefRows&,
				      const Slicer&, void*) const
    { throw (TableInvOper ("getColumnCells(Slicer&) only valid for an array")); }

void BaseColumn::getColumnSlice (const Slicer&, void*) const
    { throw (TableInvOper ("getColumn(Slicer&) only valid for an array")); }

void BaseColumn::putSlice (uInt, const Slicer&, const void*)
    { throw (TableInvOper ("putSlice() only valid for an array")); }

void BaseColumn::putScalarColumn (const void*)
    { throw (TableInvOper ("putScalarColumn() not implemented")); }

void BaseColumn::putArrayColumn (const void*)
    { throw (TableInvOper ("putArrayColumn() not implemented")); }

void BaseColumn::putScalarColumnCells (const RefRows&, const void*)
    { throw (TableInvOper ("putScalarColumnCells() not implemented")); }

void BaseColumn::putArrayColumnCells (const RefRows&, const void*)
    { throw (TableInvOper ("putArrayColumnCells() not implemented")); }

void BaseColumn::putColumnSlice (const Slicer&, const void*)
    { throw (TableInvOper ("putColumn(Slicer&) only valid for an array")); }

void BaseColumn::putColumnSliceCells (const RefRows&,
				      const Slicer&, const void*)
    { throw (TableInvOper ("putColumnCells(Slicer&) only valid for an array")); }


void BaseColumn::makeSortKey (Sort&, ObjCompareFunc*, Int, const void*&)
    { throw (TableInvOper ("makeSortKey only valid for a scalar")); }
void BaseColumn::makeRefSortKey (Sort&, ObjCompareFunc*, Int,
				 const Vector<uInt>&, const void*&)
    { throw (TableInvOper ("makeSortKey(rownrs) not valid")); }
void BaseColumn::freeSortKey (const void*&)
    { throw (TableInvOper ("freeSortKey only valid for a scalar")); }
void BaseColumn::allocIterBuf (void*&, void*&, ObjCompareFunc*&)
    { throw (TableInvOper ("allocIterBuf only valid for a scalar")); }
void BaseColumn::freeIterBuf (void*&, void*&)
    { throw (TableInvOper ("freeIterBuf only valid for a scalar")); }


void BaseColumn::getScalar (uInt rownr, Bool& value) const
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("getScalar only possible for scalars"));
    }
    switch (colDescPtr_p->dataType()) {
    case TpBool:
	get (rownr, &value);
	return;
    default:
	throw (TableInvDT ("invalid type promotion in getScalar(Bool)"));
    }
}

void BaseColumn::getScalar (uInt rownr, uChar& value) const
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("getScalar only possible for scalars"));
    }
    switch (colDescPtr_p->dataType()) {
    case TpUChar:
	get (rownr, &value);
	return;
    default:
	throw (TableInvDT ("invalid type promotion in getScalar(uChar)"));
    }
}

void BaseColumn::getScalar (uInt rownr, Short& value) const
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("getScalar only possible for scalars"));
    }
    switch (colDescPtr_p->dataType()) {
    case TpShort:
	get (rownr, &value);
	return;
    default:
	throw (TableInvDT ("invalid type promotion in getScalar(Short)"));
    }
}

void BaseColumn::getScalar (uInt rownr, uShort& value) const
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("getScalar only possible for scalars"));
    }
    switch (colDescPtr_p->dataType()) {
    case TpUChar:
	uChar valuc;
	get (rownr, &valuc);
	value = valuc;
	return;
    case TpUShort:
	get (rownr, &value);
	return;
    default:
	throw (TableInvDT ("invalid type promotion in getScalar(uShort)"));
    }
}

void BaseColumn::getScalar (uInt rownr, Int& value) const
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("getScalar only possible for scalars"));
    }
    switch (colDescPtr_p->dataType()) {
    case TpShort:
	Short vals;
	get (rownr, &vals);
	value = vals;
	return;
    case TpInt:
	get (rownr, &value);
	return;
    default:
	throw (TableInvDT ("invalid type promotion in getScalar(Int)"));
    }
}

void BaseColumn::getScalar (uInt rownr, uInt& value) const
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("getScalar only possible for scalars"));
    }
    switch (colDescPtr_p->dataType()) {
    case TpUChar:
	uChar valuc;
	get (rownr, &valuc);
	value = valuc;
	return;
    case TpUShort:
	uShort valus;
	get (rownr, &valus);
	value = valus;
	return;
    case TpUInt:
	get (rownr, &value);
	return;
    default:
	throw (TableInvDT ("invalid type promotion in getScalar(uInt)"));
    }
}

void BaseColumn::getScalar (uInt rownr, float& value) const
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("getScalar only possible for scalars"));
    }
    switch (colDescPtr_p->dataType()) {
    case TpUChar:
	uChar valuc;
	get (rownr, &valuc);
	value = valuc;
	return;
    case TpShort:
	Short vals;
	get (rownr, &vals);
	value = vals;
	return;
    case TpUShort:
	uShort valus;
	get (rownr, &valus);
	value = valus;
	return;
    case TpInt:
	Int vali;
	get (rownr, &vali);
	value = vali;
	return;
    case TpUInt:
	uInt valui;
	get (rownr, &valui);
	value = valui;
	return;
    case TpFloat:
	get (rownr, &value);
	return;
    case TpDouble:
	double vald;
	get (rownr, &vald);
	value = vald;
	return;
    default:
	throw (TableInvDT ("invalid type promotion in getScalar(float)"));
    }
}

void BaseColumn::getScalar (uInt rownr, double& value) const
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("getScalar only possible for scalars"));
    }
    switch (colDescPtr_p->dataType()) {
    case TpUChar:
	uChar valuc;
	get (rownr, &valuc);
	value = valuc;
	return;
    case TpShort:
	Short vals;
	get (rownr, &vals);
	value = vals;
	return;
    case TpUShort:
	uShort valus;
	get (rownr, &valus);
	value = valus;
	return;
    case TpInt:
	Int vali;
	get (rownr, &vali);
	value = vali;
	return;
    case TpUInt:
	uInt valui;
	get (rownr, &valui);
	value = valui;
	return;
    case TpFloat:
	float valf;
	get (rownr, &valf);
	value = valf;
	return;
    case TpDouble:
	get (rownr, &value);
	return;
    default:
	throw (TableInvDT ("invalid type promotion in getScalar(double)"));
    }
}

void BaseColumn::getScalar (uInt rownr, Complex& value) const
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("getScalar only possible for scalars"));
    }
    switch (colDescPtr_p->dataType()) {
    case TpUChar:
	uChar valuc;
	get (rownr, &valuc);
	value = Complex ((float) valuc);
	return;
    case TpShort:
	Short vals;
	get (rownr, &vals);
	value = Complex ((float) vals);
	return;
    case TpUShort:
	uShort valus;
	get (rownr, &valus);
	value = Complex ((float) valus);
	return;
    case TpInt:
	Int vali;
	get (rownr, &vali);
	value = Complex ((float) vali);
	return;
    case TpUInt:
	uInt valui;
	get (rownr, &valui);
	value = Complex ((float) valui);
	return;
    case TpFloat:
	float valf;
	get (rownr, &valf);
	value = Complex (valf);
	return;
    case TpDouble:
	double vald;
	get (rownr, &vald);
	value = Complex ((float) vald);
	return;
    case TpComplex:
	get (rownr, &value);
	return;
    case TpDComplex:
	{ DComplex valdc;
	  get (rownr, &valdc);
	  value = valdc; }
	return;
    default:
	throw (TableInvDT ("invalid type promotion in getScalar(Complex)"));
    }
}

void BaseColumn::getScalar (uInt rownr, DComplex& value) const
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("getScalar only possible for scalars"));
    }
    switch (colDescPtr_p->dataType()) {
    case TpUChar:
	uChar valuc;
	get (rownr, &valuc);
	value = DComplex ((double) valuc);
	return;
    case TpShort:
	Short vals;
	get (rownr, &vals);
	value = DComplex ((double) vals);
	return;
    case TpUShort:
	uShort valus;
	get (rownr, &valus);
	value = DComplex ((double) valus);
	return;
    case TpInt:
	Int vali;
	get (rownr, &vali);
	value = DComplex ((double) vali);
	return;
    case TpUInt:
	uInt valui;
	get (rownr, &valui);
	value = DComplex ((double) valui);
	return;
    case TpFloat:
	float valf;
	get (rownr, &valf);
	value = DComplex ((double) valf);
	return;
    case TpDouble:
	double vald;
	get (rownr, &vald);
	value = DComplex (vald);
	return;
    case TpComplex:
	{ Complex valc;
	  get (rownr, &valc);
	  value = valc; }
	return;
    case TpDComplex:
	get (rownr, &value);
	return;
    default:
	throw (TableInvDT ("invalid type promotion in getScalar(DComplex)"));
    }
}

void BaseColumn::getScalar (uInt rownr, String& value) const
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("getScalar only possible for scalars"));
    }
    switch (colDescPtr_p->dataType()) {
    case TpString:
	get (rownr, &value);
	return;
    default:
	throw (TableInvDT ("invalid type promotion in getScalar(String)"));
    }
}

void BaseColumn::getScalar (uInt rownr, void* value,
			    const String& dataTypeId) const
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("getScalar only possible for scalars"));
    }
    if (colDescPtr_p->dataType() != TpOther
    ||  colDescPtr_p->dataTypeId() != dataTypeId) {
	throw (TableInvDT ("invalid type promotion in getScalar(void*)"));
    }
    get (rownr, value);
}


void BaseColumn::putScalar (uInt rownr, const Bool& value)
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("putScalar only possible for scalars"));
    }
    switch (colDescPtr_p->dataType()) {
    case TpBool:
	put (rownr, &value);
	return;
    default:
	throw (TableInvDT ("invalid type promotion in putScalar(Bool)"));
    }
}

void BaseColumn::putScalar (uInt rownr, const uChar& value)
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("putScalar only possible for scalars"));
    }
    switch (colDescPtr_p->dataType()) {
    case TpUChar:
	put (rownr, &value);
	return;
    case TpUShort:
	uShort valus;
	valus = value;
	put (rownr, &valus);
	return;
    case TpUInt:
	uInt valui;
	valui = value;
	put (rownr, &valui);
	return;
    case TpFloat:
	float valf;
	valf = value;
	put (rownr, &valf);
	return;
    case TpDouble:
	double vald;
	vald = value;
	put (rownr, &vald);
	return;
    case TpComplex:
	{ Complex valc(value);
	  put (rownr, &valc); }
	return;
    case TpDComplex:
	{ DComplex valdc(value);
	  put (rownr, &valdc); }
	return;
    default:
	throw (TableInvDT ("invalid type promotion in putScalar(uChar)"));
    }
}

void BaseColumn::putScalar (uInt rownr, const Short& value)
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("putScalar only possible for scalars"));
    }
    switch (colDescPtr_p->dataType()) {
    case TpShort:
	put (rownr, &value);
	return;
    case TpInt:
	Int valui;
	valui = value;
	put (rownr, &valui);
	return;
    case TpFloat:
	float valf;
	valf = value;
	put (rownr, &valf);
	return;
    case TpDouble:
	double vald;
	vald = value;
	put (rownr, &vald);
	return;
    case TpComplex:
	{ Complex valc(value);
	  put (rownr, &valc); }
	return;
    case TpDComplex:
	{ DComplex valdc(value);
	  put (rownr, &valdc); }
	return;
    default:
	throw (TableInvDT ("invalid type promotion in putScalar(Short)"));
    }
}

void BaseColumn::putScalar (uInt rownr, const uShort& value)
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("putScalar only possible for scalars"));
    }
    switch (colDescPtr_p->dataType()) {
    case TpUShort:
	put (rownr, &value);
	return;
    case TpUInt:
	uInt valui;
	valui = value;
	put (rownr, &valui);
	return;
    case TpFloat:
	float valf;
	valf = value;
	put (rownr, &valf);
	return;
    case TpDouble:
	double vald;
	vald = value;
	put (rownr, &vald);
	return;
    case TpComplex:
	{ Complex valc(value);
	  put (rownr, &valc); }
	return;
    case TpDComplex:
	{ DComplex valdc(value);
	  put (rownr, &valdc); }
	return;
    default:
	throw (TableInvDT ("invalid type promotion in putScalar(uShort)"));
    }
}

void BaseColumn::putScalar (uInt rownr, const Int& value)
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("putScalar only possible for scalars"));
    }
    switch (colDescPtr_p->dataType()) {
    case TpInt:
	put (rownr, &value);
	return;
    case TpFloat:
	float valf;
	valf = value;
	put (rownr, &valf);
	return;
    case TpDouble:
	double vald;
	vald = value;
	put (rownr, &vald);
	return;
    case TpComplex:
	{ Complex valc(value);
	  put (rownr, &valc); }
	return;
    case TpDComplex:
	{ DComplex valdc(value);
	  put (rownr, &valdc); }
	return;
    default:
	throw (TableInvDT ("invalid type promotion in putScalar(Int)"));
    }
}

void BaseColumn::putScalar (uInt rownr, const uInt& value)
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("putScalar only possible for scalars"));
    }
    switch (colDescPtr_p->dataType()) {
    case TpUInt:
	put (rownr, &value);
	return;
    case TpFloat:
	float valf;
	valf = value;
	put (rownr, &valf);
	return;
    case TpDouble:
	double vald;
	vald = value;
	put (rownr, &vald);
	return;
    case TpComplex:
	{ Complex valc(value);
	  put (rownr, &valc); }
	return;
    case TpDComplex:
	{ DComplex valdc(value);
	  put (rownr, &valdc); }
	return;
    default:
	throw (TableInvDT ("invalid type promotion in putScalar(uInt)"));
    }
}

void BaseColumn::putScalar (uInt rownr, const float& value)
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("putScalar only possible for scalars"));
    }
    switch (colDescPtr_p->dataType()) {
    case TpFloat:
	put (rownr, &value);
	return;
    case TpDouble:
	double vald;
	vald = value;
	put (rownr, &vald);
	return;
    case TpComplex:
	{ Complex valc(value);
	  put (rownr, &valc); }
	return;
    case TpDComplex:
	{ DComplex valdc(value);
	  put (rownr, &valdc); }
	return;
    default:
	throw (TableInvDT ("invalid type promotion in putScalar(float)"));
    }
}

void BaseColumn::putScalar (uInt rownr, const double& value)
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("putScalar only possible for scalars"));
    }
    switch (colDescPtr_p->dataType()) {
    case TpFloat:
	float valf;
	valf = value;
	put (rownr, &valf);
	return;
    case TpDouble:
	put (rownr, &value);
	return;
    case TpComplex:
	{ Complex valc(value);
	  put (rownr, &valc); }
	return;
    case TpDComplex:
	{ DComplex valdc(value);
	  put (rownr, &valdc); }
	return;
    default:
	throw (TableInvDT ("invalid type promotion in putScalar(double)"));
    }
}

void BaseColumn::putScalar (uInt rownr, const Complex& value)
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("putScalar only possible for scalars"));
    }
    switch (colDescPtr_p->dataType()) {
    case TpComplex:
	put (rownr, &value);
	return;
    case TpDComplex:
	{ DComplex valdc(value.real(), value.imag());
	  put (rownr, &valdc); }
	return;
    default:
	throw (TableInvDT ("invalid type promotion in putScalar(Complex)"));
    }
}

void BaseColumn::putScalar (uInt rownr, const DComplex& value)
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("putScalar only possible for scalars"));
    }
    switch (colDescPtr_p->dataType()) {
    case TpComplex:
	{ Complex valc (value.real(), value.imag());
	  put (rownr, &valc); }
	return;
    case TpDComplex:
	put (rownr, &value);
	return;
    default:
	throw (TableInvDT ("invalid type promotion in putScalar(DComplex)"));
    }
}

void BaseColumn::putScalar (uInt rownr, const String& value)
{
    if (!colDescPtr_p->isScalar()) {
	throw (TableInvOper ("putScalar only possible for scalars"));
    }
    switch (colDescPtr_p->dataType()) {
    case TpString:
	put (rownr, &value);
	return;
    default:
	throw (TableInvDT ("invalid type promotion in putScalar(String)"));
    }
}
