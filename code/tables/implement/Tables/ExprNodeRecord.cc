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


#include <aips/Tables/ExprNodeRecord.h>
#include <aips/Containers/RecordInterface.h>
#include <aips/Containers/RecordDesc.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>


TableExprNodeRecordField::TableExprNodeRecordField
                                            (DataType dtype,
					     const Block<Int>& fieldNumbers)
: TableExprNodeBinary (NTNumeric, VTScalar, OtField, 0),
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
  DataType dtype = desc.type(fieldNrs_p[lastEntry_p]);
  switch (dtype_p) {
  case NTBool:
    return dtype == TpBool;
  case NTDouble:
    return dtype == TpUChar  ||  dtype == TpShort  ||  dtype == TpInt
       ||  dtype == TpUInt   ||  dtype == TpFloat  ||  dtype == TpDouble;
  case NTComplex:
    return dtype == TpUChar  ||  dtype == TpShort  ||  dtype == TpInt
       ||  dtype == TpUInt   ||  dtype == TpFloat  ||  dtype == TpDouble
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
  return getRecord(id).asBool (fieldNrs_p[lastEntry_p]);
}
Double   TableExprNodeRecordField::getDouble   (const TableExprId& id)
{
  return getRecord(id).asDouble (fieldNrs_p[lastEntry_p]);
}
DComplex TableExprNodeRecordField::getDComplex (const TableExprId& id)
{
  return getRecord(id).asDComplex (fieldNrs_p[lastEntry_p]);
}
String   TableExprNodeRecordField::getString   (const TableExprId& id)
{
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
    varShape_p = getRecord(id).shape (fieldNrs_p[lastEntry_p]);
    return varShape_p;
}

Bool TableExprNodeRecordFieldArray::isDefined (const TableExprId& id)
{
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
  DataType dtype = desc.type(fieldNrs_p[lastEntry_p]);
  switch (dtype_p) {
  case NTBool:
    return dtype == TpArrayBool;
  case NTDouble:
    return dtype == TpArrayUChar  ||  dtype == TpArrayShort
       ||  dtype == TpArrayInt    ||  dtype == TpArrayUInt
       ||  dtype == TpArrayFloat  ||  dtype == TpArrayDouble;
  case NTComplex:
    return dtype == TpArrayUChar  ||  dtype == TpArrayShort
       ||  dtype == TpArrayInt    ||  dtype == TpArrayUInt
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
  return getRecord(id).asArrayBool (fieldNrs_p[lastEntry_p]);
}

Array<Double> TableExprNodeRecordFieldArray::getArrayDouble
                                                   (const TableExprId& id)
{
  const RecordInterface& record = getRecord(id);
  DataType dtype = record.type(fieldNrs_p[lastEntry_p]);
  if (dtype == TpArrayDouble) {
    return record.asArrayDouble (fieldNrs_p[lastEntry_p]);
  }
  Array<Double> result (record.shape(fieldNrs_p[lastEntry_p]));
  switch (dtype) {
  case TpArrayUChar:
    convertArray (result, record.asArrayuChar (fieldNrs_p[lastEntry_p]));
    break;
  case TpArrayShort:
    convertArray (result, record.asArrayShort (fieldNrs_p[lastEntry_p]));
    break;
  case TpArrayInt:
    convertArray (result, record.asArrayInt (fieldNrs_p[lastEntry_p]));
    break;
  case TpArrayUInt:
    convertArray (result, record.asArrayuInt (fieldNrs_p[lastEntry_p]));
    break;
  case TpArrayFloat:
    convertArray (result, record.asArrayFloat (fieldNrs_p[lastEntry_p]));
    break;
  default:
    throw (AipsError ("TableExprNodeRecordFieldArray::getArrayDouble"));
  }
  return result;
}

Array<DComplex> TableExprNodeRecordFieldArray::getArrayDComplex
                                                   (const TableExprId& id)
{
  const RecordInterface& record = getRecord(id);
  DataType dtype = record.type(fieldNrs_p[lastEntry_p]);
  if (dtype == TpArrayDComplex) {
    return getRecord(id).asArrayDComplex (fieldNrs_p[lastEntry_p]);
  }
  Array<DComplex> result (record.shape(fieldNrs_p[lastEntry_p]));
  switch (dtype) {
  case TpArrayComplex:
    convertArray (result, record.asArrayComplex (fieldNrs_p[lastEntry_p]));
    break;
  default:
    convertArray (result, getArrayDouble (id));
  }
  return result;
}

Array<String> TableExprNodeRecordFieldArray::getArrayString
                                                   (const TableExprId& id)
{
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
