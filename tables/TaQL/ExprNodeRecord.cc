//# ExprNodeRecord.cc: Nodes representing fields in record select expression tree
//# Copyright (C) 2000
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


#include <casacore/tables/TaQL/ExprNodeRecord.h>
#include <casacore/tables/TaQL/TableExprData.h>
#include <casacore/casa/Containers/RecordInterface.h>
#include <casacore/casa/Containers/RecordDesc.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableExprNodeRecordField::TableExprNodeRecordField
                                            (DataType dtype,
					     const Block<Int>& fieldNumbers)
: TableExprNodeBinary (NTNumeric, VTScalar, OtField, Table()),
  fieldNrs_p  (fieldNumbers),
  lastEntry_p (fieldNumbers.nelements() - 1)
{
  //# Fill in the real data type.
  switch (dtype) {
  case TpBool:
    dtype_p = NTBool;
    break;
  case TpString:
    dtype_p = NTString;
    break;
  case TpComplex:
  case TpDComplex:
    dtype_p = NTComplex;
    break;
  case TpChar:
  case TpUChar:
  case TpShort:
  case TpUShort:
  case TpInt:
  case TpUInt:
  case TpInt64:
    dtype_p = NTInt;
    break;
  case TpFloat:
  case TpDouble:
    dtype_p = NTDouble;
    break;
  default:
    throw (AipsError ("TableExprNodeRecordField: invalid data type"));
  }
  exprtype_p = Variable;
  // Make sure the variable is not a constant for isDefined.
  ndim_p = -1;
}

TableExprNodeRecordField::~TableExprNodeRecordField()
{}

const IPosition& TableExprNodeRecordField::getShape (const TableExprId&)
{
    return shape_p;
}

Bool TableExprNodeRecordField::isDefined (const TableExprId& id)
{
  DataType dtype=TpOther;
  if (id.byData()) {
    dtype = id.data().dataType (fieldNrs_p);
  } else {
    const RecordInterface* recPtr = &(id.record());
    for (uInt i=0; i<lastEntry_p; i++) {
      RecordDesc desc = recPtr->description();
      if (fieldNrs_p[i] >= Int(desc.nfields())
      ||  !desc.isSubRecord(fieldNrs_p[i])) {
	return False;
      }
      recPtr = &(recPtr->asRecord (fieldNrs_p[i]));
    }
    RecordDesc desc = recPtr->description();
    if (fieldNrs_p[lastEntry_p] >= Int(desc.nfields())) {
      return False;
    }
    dtype = desc.type(fieldNrs_p[lastEntry_p]);
  }
  switch (dtype_p) {
  case NTBool:
    return dtype == TpBool;
  case NTInt:
    return dtype == TpUChar  ||  dtype == TpShort  ||  dtype == TpInt
       ||  dtype == TpUInt   ||  dtype == TpInt64;
  case NTDouble:
    return dtype == TpUChar  ||  dtype == TpShort  ||  dtype == TpInt
       ||  dtype == TpUInt   ||  dtype == TpInt64
       ||  dtype == TpFloat  ||  dtype == TpDouble;
  case NTComplex:
    return dtype == TpUChar  ||  dtype == TpShort  ||  dtype == TpInt
       ||  dtype == TpUInt   ||  dtype == TpInt64
       ||  dtype == TpFloat  ||  dtype == TpDouble
       ||  dtype == TpComplex  ||  dtype == TpDComplex;
  case NTString:
    return dtype == TpString;
  default:
    return False;
  }
  return False;
}

Bool     TableExprNodeRecordField::getBool     (const TableExprId& id)
{
  if (id.byData()) {
    return id.data().getBool (fieldNrs_p);
  }
  return getRecord(id).asBool (fieldNrs_p[lastEntry_p]);
}
Int64    TableExprNodeRecordField::getInt      (const TableExprId& id)
{
  if (id.byData()) {
    return id.data().getInt (fieldNrs_p);
  }
  return getRecord(id).asInt64 (fieldNrs_p[lastEntry_p]);
}
Double   TableExprNodeRecordField::getDouble   (const TableExprId& id)
{
  if (id.byData()) {
    return id.data().getDouble (fieldNrs_p);
  }
  return getRecord(id).asDouble (fieldNrs_p[lastEntry_p]);
}
DComplex TableExprNodeRecordField::getDComplex (const TableExprId& id)
{
  if (id.byData()) {
    return id.data().getDComplex (fieldNrs_p);
  }
  return getRecord(id).asDComplex (fieldNrs_p[lastEntry_p]);
}
String   TableExprNodeRecordField::getString   (const TableExprId& id)
{
  if (id.byData()) {
    return id.data().getString (fieldNrs_p);
  }
  return getRecord(id).asString (fieldNrs_p[lastEntry_p]);
}

const RecordInterface& TableExprNodeRecordField::getRecord
                                            (const TableExprId& id) const
{
  const RecordInterface* recPtr = &(id.record());
  for (uInt i=0; i<lastEntry_p; i++) {
    recPtr = &(recPtr->asRecord (fieldNrs_p[i]));
  }
  return *recPtr;
}




TableExprNodeRecordFieldArray::TableExprNodeRecordFieldArray
                                            (DataType dtype,
					     const Block<Int>& fieldNumbers)
: TableExprNodeArray (NTNumeric, OtField),
  fieldNrs_p  (fieldNumbers),
  lastEntry_p (fieldNumbers.nelements() - 1)
{
  //# Fill in the real data type.
  switch (dtype) {
  case TpArrayBool:
    dtype_p = NTBool;
    break;
  case TpArrayString:
    dtype_p = NTString;
    break;
  case TpArrayComplex:
  case TpArrayDComplex:
    dtype_p = NTComplex;
    break;
  case TpArrayUChar:
  case TpArrayShort:
  case TpArrayInt:
  case TpArrayUInt:
  case TpArrayInt64:
    dtype_p = NTInt;
    break;
  case TpArrayFloat:
  case TpArrayDouble:
    dtype_p = NTDouble;
    break;
  default:
    throw (AipsError ("TableExprNodeRecordFieldArray: invalid data type"));
  }
  exprtype_p = Variable;
}

TableExprNodeRecordFieldArray::~TableExprNodeRecordFieldArray()
{}

const IPosition& TableExprNodeRecordFieldArray::getShape
                                                   (const TableExprId& id)
{
    varShape_p.resize (0);
    if (id.byData()) {
      varShape_p = id.data().shape (fieldNrs_p);
    } else {
      varShape_p = getRecord(id).shape (fieldNrs_p[lastEntry_p]);
    }
    return varShape_p;
}

Bool TableExprNodeRecordFieldArray::isDefined (const TableExprId& id)
{
  DataType dtype=TpOther;
  if (id.byData()) {
    dtype = id.data().dataType (fieldNrs_p);
  } else {
    const RecordInterface* recPtr = &(id.record());
    for (uInt i=0; i<lastEntry_p; i++) {
      RecordDesc desc = recPtr->description();
      if (fieldNrs_p[i] >= Int(desc.nfields())
      ||  !desc.isSubRecord(fieldNrs_p[i])) {
	return False;
      }
      recPtr = &(recPtr->asRecord (fieldNrs_p[i]));
    }
    RecordDesc desc = recPtr->description();
    if (fieldNrs_p[lastEntry_p] >= Int(desc.nfields())) {
      return False;
    }
    dtype = desc.type(fieldNrs_p[lastEntry_p]);
  }
  switch (dtype_p) {
  case NTBool:
    return dtype == TpArrayBool;
  case NTInt:
    return dtype == TpArrayUChar  ||  dtype == TpArrayShort
       ||  dtype == TpArrayInt    ||  dtype == TpArrayUInt
       ||  dtype == TpArrayInt64;
  case NTDouble:
    return dtype == TpArrayUChar  ||  dtype == TpArrayShort
       ||  dtype == TpArrayInt    ||  dtype == TpArrayUInt
       ||  dtype == TpArrayInt64
       ||  dtype == TpArrayFloat  ||  dtype == TpArrayDouble;
  case NTComplex:
    return dtype == TpArrayUChar  ||  dtype == TpArrayShort
       ||  dtype == TpArrayInt    ||  dtype == TpArrayUInt
       ||  dtype == TpArrayInt64
       ||  dtype == TpArrayFloat  ||  dtype == TpArrayDouble
       ||  dtype == TpArrayComplex  ||  dtype == TpArrayDComplex;
  case NTString:
    return dtype == TpArrayString;
  default:
    return False;
  }
  return False;
}

Array<Bool> TableExprNodeRecordFieldArray::getArrayBool
                                                   (const TableExprId& id)
{
  if (id.byData()) {
    return id.data().getArrayBool (fieldNrs_p);
  }
  return getRecord(id).asArrayBool (fieldNrs_p[lastEntry_p]);
}

Array<Int64> TableExprNodeRecordFieldArray::getArrayInt
                                                   (const TableExprId& id)
{
  if (id.byData()) {
    return id.data().getArrayInt (fieldNrs_p);
  }
  return getRecord(id).toArrayInt64 (fieldNrs_p[lastEntry_p]);
}

Array<Double> TableExprNodeRecordFieldArray::getArrayDouble
                                                   (const TableExprId& id)
{
  if (id.byData()) {
    return id.data().getArrayDouble (fieldNrs_p);
  }
  return getRecord(id).toArrayDouble (fieldNrs_p[lastEntry_p]);
}

Array<DComplex> TableExprNodeRecordFieldArray::getArrayDComplex
                                                   (const TableExprId& id)
{
  if (id.byData()) {
    return id.data().getArrayDComplex (fieldNrs_p);
  }
  return getRecord(id).toArrayDComplex (fieldNrs_p[lastEntry_p]);
}

Array<String> TableExprNodeRecordFieldArray::getArrayString
                                                   (const TableExprId& id)
{
  if (id.byData()) {
    return id.data().getArrayString (fieldNrs_p);
  }
  return getRecord(id).asArrayString (fieldNrs_p[lastEntry_p]);
}

const RecordInterface& TableExprNodeRecordFieldArray::getRecord
                                            (const TableExprId& id) const
{
  const RecordInterface* recPtr = &(id.record());
  for (uInt i=0; i<lastEntry_p; i++) {
    recPtr = &(recPtr->asRecord (fieldNrs_p[i]));
  }
  return *recPtr;
}

} //# NAMESPACE CASACORE - END
