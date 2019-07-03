//# BaseColumn.cc: Abstract base class for a table column
//# Copyright (C) 1994,1995,1996,1998,1999,2000
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

#include <casacore/tables/Tables/BaseColumn.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/tables/Tables/TableError.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

BaseColumn::BaseColumn (const BaseColumnDesc* cdp)
: colDescPtr_p(cdp)
{}

BaseColumn::~BaseColumn()
{}


//# By default all functions throw an exception
//# to ensure they are called correctly.

void BaseColumn::setShape (rownr_t, const IPosition&)
{
  throw (TableInvOper ("invalid setShape() for column " + colDescPtr_p->name() +
                       "; only valid for an array"));
}

void BaseColumn::setShape (rownr_t, const IPosition&, const IPosition&)
{
  throw (TableInvOper ("invalid setShape() for column " + colDescPtr_p->name() +
                       "; only valid for an array"));
}

uInt BaseColumn::ndimColumn() const
{
  throw (TableInvOper ("invalid ndimColumn() for column " + colDescPtr_p->name() +
                       "; only valid for an array"));
  return 0;
}

IPosition BaseColumn::shapeColumn() const
{
  throw (TableInvOper ("invalid shapeColumn() for column " + colDescPtr_p->name() +
                       "; only valid for an array"));
  return IPosition(0);
}

uInt BaseColumn::ndim (rownr_t) const
{
  throw (TableInvOper ("invalid ndim() for column " + colDescPtr_p->name() +
                       "; only valid for an array"));
  return 0;
}

IPosition BaseColumn::shape (rownr_t) const
{
  throw (TableInvOper ("invalid shape() for column " + colDescPtr_p->name() +
                       "; only valid for an array"));
  return IPosition(0);
}

IPosition BaseColumn::tileShape (rownr_t) const
{
  throw (TableInvOper ("invalid tileShape() for column " + colDescPtr_p->name() +
                       "; only valid for an array"));
}


Bool BaseColumn::canChangeShape() const
{
    return False;                      // can not be changed
}

void BaseColumn::get (rownr_t, void*) const
{
  throw (TableInvOper ("get() not implemented for column " +
                       colDesc_p.name() + "; only valid for a scalar"));
}

void BaseColumn::getArray (rownr_t, ArrayBase&) const
{
  throw (TableInvOper ("getArray() not implemented for column " +
                       colDesc_p.name() + "; only valid for an array"));
}

void BaseColumn::getSlice (rownr_t, const Slicer&, ArrayBase&) const
{
  throw (TableInvOper ("getSlice() not implemented for column " +
                       colDescPtr_p->name() + "; only valid for an array"));
}

void BaseColumn::getScalarColumn (ArrayBase&) const
{
  throw (TableInvOper ("getScalarColumn() not implemented for column " +
                       colDescPtr_p->name() + "; only valid for a scalar"));
}

void BaseColumn::getArrayColumn (ArrayBase&) const
{
  throw (TableInvOper ("getArrayColumn() not implemented for column " +
                       colDescPtr_p->name() + "; only valid for an array"));
}

void BaseColumn::getScalarColumnCells (const RefRows&, ArrayBase&) const
{
  throw (TableInvOper ("getScalarColumnCells() not implemented for column " +
                       colDescPtr_p->name() + "; only valid for a scalar"));
}

void BaseColumn::getArrayColumnCells (const RefRows&, ArrayBase&) const
{
  throw (TableInvOper ("getArrayColumnCells() not implemented for column " +
                       colDescPtr_p->name() + "; only valid for an array"));
}

void BaseColumn::getColumnSliceCells (const RefRows&,
				      const Slicer&, ArrayBase&) const
{
  throw (TableInvOper ("getColumnCells(Slicer&) not implemented for column " +
                       colDescPtr_p->name() + "; only valid for an array"));
}

void BaseColumn::getColumnSlice (const Slicer&, ArrayBase&) const
{
  throw (TableInvOper ("getColumn(Slicer&) not implemented for column " +
                       colDescPtr_p->name() + "; only valid for an array"));
}

void BaseColumn::put (rownr_t, const void*)
{
  throw (TableInvOper ("put() not implemented for column " +
                       colDesc_p.name() + "; only valid for a scalar"));
}

void BaseColumn::putArray (rownr_t, const ArrayBase&)
{
  throw (TableInvOper ("putArray() not implemented for column " +
                       colDesc_p.name() + "; only valid for an array"));
}

void BaseColumn::putSlice (rownr_t, const Slicer&, const ArrayBase&)
{
  throw (TableInvOper ("putSlice() not implemented for column " +
                       colDescPtr_p->name() + "; only valid for an array"));
}

void BaseColumn::putScalarColumn (const ArrayBase&)
{
  throw (TableInvOper ("putScalarColumn() not implemented for column " +
                       colDescPtr_p->name() + "; only valid for a scalar"));
}

void BaseColumn::putArrayColumn (const ArrayBase&)
{
  throw (TableInvOper ("putArrayColumn() not implemented for column " +
                       colDescPtr_p->name() + "; only valid for an array"));
}

void BaseColumn::putScalarColumnCells (const RefRows&, const ArrayBase&)
{
  throw (TableInvOper ("putScalarColumnCells() not implemented for column " +
                       colDescPtr_p->name() + "; only valid for a scalar"));
}

void BaseColumn::putArrayColumnCells (const RefRows&, const ArrayBase&)
{
  throw (TableInvOper ("putArrayColumnCells() not implemented for column " +
                       colDescPtr_p->name() + "; only valid for an array"));
}

void BaseColumn::putColumnSlice (const Slicer&, const ArrayBase&)
{
  throw (TableInvOper ("putColumn(Slicer&) not implemented for column " +
                       colDescPtr_p->name() + "; only valid for an array"));
}

void BaseColumn::putColumnSliceCells (const RefRows&,
				      const Slicer&, const ArrayBase&)
{
  throw (TableInvOper ("putColumnCells(Slicer&) not implemented for column " +
                       colDescPtr_p->name() + "; only valid for an array"));
}


void BaseColumn::makeSortKey (Sort&, CountedPtr<BaseCompare>&, Int,
                              CountedPtr<ArrayBase>&)
{
  throw (TableInvOper ("makeSortKey() for column " + colDescPtr_p->name() +
                       " is only valid for a scalar"));
}
void BaseColumn::makeRefSortKey (Sort&, CountedPtr<BaseCompare>&, Int,
				 const Vector<rownr_t>&, CountedPtr<ArrayBase>&)
{
  throw (TableInvOper ("makeSortKey(rownrs) for column " + colDescPtr_p->name() +
                       " is only valid for a scalar"));
}
void BaseColumn::allocIterBuf (void*&, void*&, CountedPtr<BaseCompare>&)
{
  throw (TableInvOper ("allocIterBuf() for column " + colDescPtr_p->name() +
                       " is only valid for a scalar"));
}
void BaseColumn::freeIterBuf (void*&, void*&)
{
  throw (TableInvOper ("freeIterBuf() for column " + colDescPtr_p->name() +
                       " is only valid for a scalar"));
}


void BaseColumn::getScalar (rownr_t rownr, Bool& value) const
{
    if (!colDescPtr_p->isScalar()) {
        throwGetScalar();
    }
    switch (colDescPtr_p->dataType()) {
    case TpBool:
	get (rownr, &value);
	return;
    default:
        throwGetType("Bool");
    }
}

void BaseColumn::getScalar (rownr_t rownr, uChar& value) const
{
    if (!colDescPtr_p->isScalar()) {
        throwGetScalar();
    }
    switch (colDescPtr_p->dataType()) {
    case TpUChar:
	get (rownr, &value);
	return;
    default:
        throwGetType("uChar");
    }
}

void BaseColumn::getScalar (rownr_t rownr, Short& value) const
{
    if (!colDescPtr_p->isScalar()) {
        throwGetScalar();
    }
    switch (colDescPtr_p->dataType()) {
    case TpShort:
	get (rownr, &value);
	return;
    default:
        throwGetType("Short");
    }
}

void BaseColumn::getScalar (rownr_t rownr, uShort& value) const
{
    if (!colDescPtr_p->isScalar()) {
        throwGetScalar();
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
        throwGetType("uShort");
    }
}

void BaseColumn::getScalar (rownr_t rownr, Int& value) const
{
    if (!colDescPtr_p->isScalar()) {
        throwGetScalar();
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
	get (rownr, &value);
	return;
    default:
        throwGetType("Int");
    }
}

void BaseColumn::getScalar (rownr_t rownr, uInt& value) const
{
    if (!colDescPtr_p->isScalar()) {
        throwGetScalar();
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
        throwGetType("uInt");
    }
}

void BaseColumn::getScalar (rownr_t rownr, Int64& value) const
{
    if (!colDescPtr_p->isScalar()) {
        throwGetScalar();
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
    case TpInt64:
	get (rownr, &value);
	return;
    default:
        throwGetType("Int64");
    }
}

void BaseColumn::getScalar (rownr_t rownr, float& value) const
{
    if (!colDescPtr_p->isScalar()) {
        throwGetScalar();
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
    case TpInt64:
	Int64 vali64;
	get (rownr, &vali64);
	value = vali64;
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
        throwGetType("float");
    }
}

void BaseColumn::getScalar (rownr_t rownr, double& value) const
{
    if (!colDescPtr_p->isScalar()) {
        throwGetScalar();
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
    case TpInt64:
	Int64 vali64;
	get (rownr, &vali64);
	value = vali64;
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
        throwGetType("double");
    }
}

void BaseColumn::getScalar (rownr_t rownr, Complex& value) const
{
    if (!colDescPtr_p->isScalar()) {
        throwGetScalar();
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
    case TpInt64:
	Int64 vali64;
	get (rownr, &vali64);
	value = vali64;
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
	  value = Complex(valdc.real(), valdc.imag());
	}
	return;
    default:
        throwGetType("Complex");
    }
}

void BaseColumn::getScalar (rownr_t rownr, DComplex& value) const
{
    if (!colDescPtr_p->isScalar()) {
        throwGetScalar();
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
    case TpInt64:
	Int64 vali64;
	get (rownr, &vali64);
	value = vali64;
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
        throwGetType("DComplex");
    }
}

void BaseColumn::getScalar (rownr_t rownr, String& value) const
{
    if (!colDescPtr_p->isScalar()) {
        throwGetScalar();
    }
    switch (colDescPtr_p->dataType()) {
    case TpString:
	get (rownr, &value);
	return;
    default:
        throwGetType("String");
    }
}

void BaseColumn::getScalar (rownr_t rownr, TableRecord& value) const
{
    if (!colDescPtr_p->isScalar()) {
        throwGetScalar();
    }
    switch (colDescPtr_p->dataType()) {
    case TpRecord:
	get (rownr, &value);
	return;
    default:
        throwGetType("TableRecord");
    }
}

void BaseColumn::getScalar (rownr_t rownr, void* value,
			    const String& dataTypeId) const
{
    if (!colDescPtr_p->isScalar()) {
        throwGetScalar();
    }
    if (colDescPtr_p->dataType() != TpOther
    ||  colDescPtr_p->dataTypeId() != dataTypeId) {
        throwGetType("void*");
    }
    get (rownr, value);
}


void BaseColumn::putScalar (rownr_t rownr, const Bool& value)
{
    if (!colDescPtr_p->isScalar()) {
        throwPutScalar();
    }
    switch (colDescPtr_p->dataType()) {
    case TpBool:
	put (rownr, &value);
	return;
    default:
        throwPutType("Bool");
    }
}

void BaseColumn::putScalar (rownr_t rownr, const uChar& value)
{
    if (!colDescPtr_p->isScalar()) {
        throwPutScalar();
    }
    switch (colDescPtr_p->dataType()) {
    case TpUChar:
	put (rownr, &value);
	return;
    case TpShort:
	Short vals;
	vals = value;
	put (rownr, &vals);
	return;
    case TpUShort:
	uShort valus;
	valus = value;
	put (rownr, &valus);
	return;
    case TpInt:
	Int vali;
	vali = value;
	put (rownr, &vali);
	return;
    case TpUInt:
	uInt valui;
	valui = value;
	put (rownr, &valui);
	return;
    case TpInt64:
	Int64 vali64;
	vali64 = value;
	put (rownr, &vali64);
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
        throwPutType("uChar");
    }
}

void BaseColumn::putScalar (rownr_t rownr, const Short& value)
{
    if (!colDescPtr_p->isScalar()) {
        throwPutScalar();
    }
    switch (colDescPtr_p->dataType()) {
    case TpShort:
	put (rownr, &value);
	return;
    case TpInt:
	Int vali;
	vali = value;
	put (rownr, &vali);
	return;
    case TpInt64:
	Int64 vali64;
	vali64 = value;
	put (rownr, &vali64);
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
        throwPutType("Short");
    }
}

void BaseColumn::putScalar (rownr_t rownr, const uShort& value)
{
    if (!colDescPtr_p->isScalar()) {
        throwPutScalar();
    }
    switch (colDescPtr_p->dataType()) {
    case TpUShort:
	put (rownr, &value);
	return;
    case TpInt:
	Int vali;
	vali = value;
	put (rownr, &vali);
	return;
    case TpUInt:
	uInt valui;
	valui = value;
	put (rownr, &valui);
	return;
    case TpInt64:
	Int64 vali64;
	vali64 = value;
	put (rownr, &vali64);
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
        throwPutType("uShort");
    }
}

void BaseColumn::putScalar (rownr_t rownr, const Int& value)
{
    if (!colDescPtr_p->isScalar()) {
        throwPutScalar();
    }
    switch (colDescPtr_p->dataType()) {
    case TpInt:
	put (rownr, &value);
	return;
    case TpInt64:
	Int64 vali64;
	vali64 = value;
	put (rownr, &vali64);
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
        throwPutType("Int");
    }
}

void BaseColumn::putScalar (rownr_t rownr, const uInt& value)
{
    if (!colDescPtr_p->isScalar()) {
        throwPutScalar();
    }
    switch (colDescPtr_p->dataType()) {
    case TpUInt:
	put (rownr, &value);
	return;
    case TpInt64:
	Int64 vali64;
	vali64 = value;
	put (rownr, &vali64);
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
        throwPutType("uInt");
    }
}

void BaseColumn::putScalar (rownr_t rownr, const Int64& value)
{
    if (!colDescPtr_p->isScalar()) {
        throwPutScalar();
    }
    switch (colDescPtr_p->dataType()) {
    case TpInt64:
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
        throwPutType("Int64");
    }
}

void BaseColumn::putScalar (rownr_t rownr, const float& value)
{
    if (!colDescPtr_p->isScalar()) {
        throwPutScalar();
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
        throwPutType("float");
    }
}

void BaseColumn::putScalar (rownr_t rownr, const double& value)
{
    if (!colDescPtr_p->isScalar()) {
        throwPutScalar();
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
        throwPutType("double");
    }
}

void BaseColumn::putScalar (rownr_t rownr, const Complex& value)
{
    if (!colDescPtr_p->isScalar()) {
        throwPutScalar();
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
        throwPutType("Complex");
    }
}

void BaseColumn::putScalar (rownr_t rownr, const DComplex& value)
{
    if (!colDescPtr_p->isScalar()) {
        throwPutScalar();
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
      throwPutType("DComplex");
    }
}

void BaseColumn::putScalar (rownr_t rownr, const String& value)
{
    if (!colDescPtr_p->isScalar()) {
        throwPutScalar();
    }
    switch (colDescPtr_p->dataType()) {
    case TpString:
	put (rownr, &value);
	return;
    default:
        throwPutType("String");
    }
}

void BaseColumn::putScalar (rownr_t rownr, const TableRecord& value)
{
    if (!colDescPtr_p->isScalar()) {
        throwPutScalar();
    }
    switch (colDescPtr_p->dataType()) {
    case TpRecord:
	put (rownr, &value);
	return;
    default:
        throwPutType("TableRecord");
    }
}

void BaseColumn::throwGetScalar() const
{
    throw (TableInvOper ("invalid getScalar() for column " + colDescPtr_p->name() +
                         "; only possible for a scalar"));
}

void BaseColumn::throwPutScalar() const
{
    throw (TableInvOper ("invalid putScalar() for column " + colDescPtr_p->name() +
                         "; only possible for a scalar"));
}

void BaseColumn::throwGetType (const String& type) const
{
    throw (TableInvDT ("invalid type promotion in getScalar(" + type +
                       ") for column " + colDescPtr_p->name() + " with type "
                       + ValType::getTypeStr(colDescPtr_p->dataType())));
}

void BaseColumn::throwPutType (const String& type) const
{
    throw (TableInvDT ("invalid type promotion in putScalar(" + type +
                       ") for column " + colDescPtr_p->name() + " with type "
                       + ValType::getTypeStr(colDescPtr_p->dataType())));
}

const ColumnDesc& BaseColumn::columnDesc() const
{
  colDesc_p = ColumnDesc(const_cast<BaseColumnDesc*>(colDescPtr_p));
  return colDesc_p;
}

} //# NAMESPACE CASACORE - END

