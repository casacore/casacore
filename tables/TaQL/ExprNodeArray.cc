//# ExprNodeArray.cc: Classes representing an array in table select expression
//# Copyright (C) 1997,1999,2000,2001
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

#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeArray.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/tables/TaQL/ExprNodeUtil.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/tables/TaQL/MArrayMath.h>
#include <casacore/tables/TaQL/MArrayLogical.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableExprNodeArray::TableExprNodeArray (NodeDataType dtype, OperType otype)
: TableExprNodeBinary (dtype, VTArray, otype, Constant)
{
    ndim_p = -1;
}
TableExprNodeArray::TableExprNodeArray (const TableExprNodeRep& node,
                                        NodeDataType dtype, OperType otype)
: TableExprNodeBinary (dtype, node, otype)
{}
TableExprNodeArray::TableExprNodeArray (NodeDataType dtype, OperType otype,
                                        const IPosition& shape)
: TableExprNodeBinary (dtype, VTArray, otype, Constant)
{
    shape_p = shape;
    ndim_p  = shape.size();
    if (ndim_p == 0) {
        ndim_p = -1;
    }
}

TENShPtr TableExprNodeArray::makeConstantScalar()
{
  if (isConstant()) {
    switch (dataType()) {
    case NTBool:
      {
        MArray<Bool> arr = getArrayBool(0);
        if (arr.size() == 1) {
          return TENShPtr(new TableExprNodeConstBool (arr.array().data()[0]));
        }
      }
      break;
    case NTInt:
      {
        MArray<Int64> arr = getArrayInt(0);
        if (arr.size() == 1) {
          return TENShPtr(new TableExprNodeConstInt (arr.array().data()[0]));
        }
      }
      break;
    case NTDouble:
      {
        MArray<Double> arr = getArrayDouble(0);
        if (arr.size() == 1) {
          return TENShPtr(new TableExprNodeConstDouble (arr.array().data()[0]));
        }
      }
      break;
    case NTComplex:
      {
        MArray<DComplex> arr = getArrayDComplex(0);
        if (arr.size() == 1) {
          return TENShPtr(new TableExprNodeConstDComplex (arr.array().data()[0]));
        }
      }
      break;
    case NTString:
      {
        MArray<String> arr = getArrayString(0);
        if (arr.size() == 1) {
          return TENShPtr(new TableExprNodeConstString (arr.array().data()[0]));
        }
      }
      break;
    case NTDate:
      {
        MArray<MVTime> arr = getArrayDate(0);
        if (arr.size() == 1) {
          return TENShPtr(new TableExprNodeConstDate (arr.array().data()[0]));
        }
      }
      break;
    default:
      break;
    }
  }
  return 0;
}

IPosition TableExprNodeArray::validateIndex (const IPosition& index,
                                             const ArrayBase& arr) const
{
  if (index.size() != arr.ndim()) {
    throw TableInvExpr("index size does not match the array dimensionality");
  }
  IPosition inx(index);
  for (uInt i=0; i<inx.size(); ++i) {
    if (inx[i] < 0) {
      inx[i] += arr.shape()[i];
    }
  }
  arr.validateIndex (inx);
  return inx;
}

const IPosition& TableExprNodeArray::getShape (const TableExprId& id)
{
    varShape_p.resize (0);
    switch (dataType()) {
    case NTBool:
        varShape_p = getArrayBool(id).shape();
        break;
    case NTInt:
        varShape_p = getArrayInt(id).shape();
        break;
    case NTDouble:
        varShape_p = getArrayDouble(id).shape();
        break;
    case NTComplex:
        varShape_p = getArrayDComplex(id).shape();
        break;
    case NTString:
        varShape_p = getArrayString(id).shape();
        break;
    case NTDate:
        varShape_p = getArrayDate(id).shape();
        break;
    default:
        TableExprNode::throwInvDT ("TableExprNodeArray::getShape");
    }
    return varShape_p;
}

MArray<Double> TableExprNodeArray::getArrayDouble (const TableExprId& id)
{
    MArray<Int64> arr = getArrayInt (id);
    Array<Double> result (arr.shape());
    convertArray (result, arr.array());
    return MArray<Double> (result, arr.mask());
}

MArray<DComplex> TableExprNodeArray::getArrayDComplex (const TableExprId& id)
{
    MArray<Double> arr = getArrayDouble (id);
    Array<DComplex> result (arr.shape());
    convertArray (result, arr.array());
    return MArray<DComplex> (result, arr.mask());
}

Bool TableExprNodeArray::contains (const TableExprId& id, Bool value)
{
    return anyEQ (value, getArrayBool (id));
}
Bool TableExprNodeArray::contains (const TableExprId& id, Int64 value)
{
    return anyEQ (value, getArrayInt (id));
}
Bool TableExprNodeArray::contains (const TableExprId& id, Double value)
{
    return anyEQ (value, getArrayDouble (id));
}
Bool TableExprNodeArray::contains (const TableExprId& id, DComplex value)
{
    return anyEQ (value, getArrayDComplex (id));
}
Bool TableExprNodeArray::contains (const TableExprId& id, String value)
{
    return anyEQ (value, getArrayString (id));
}
Bool TableExprNodeArray::contains (const TableExprId& id, MVTime value)
{
    return anyEQ (value, getArrayDate (id));
}

MArray<Bool> TableExprNodeArray::contains (const TableExprId& id,
                                           const MArray<Bool>& value)
{
    MArray<Bool> set = getArrayBool (id);
    Array<Bool> result(value.shape());
    Bool deleteIn, deleteOut;
    const Bool* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    size_t nval = value.size();
    for (size_t i=0; i<nval; i++) {
        out[i] = anyEQ (in[i], set);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
}
MArray<Bool> TableExprNodeArray::contains (const TableExprId& id,
                                           const MArray<Int64>& value)
{
    MArray<Int64> set = getArrayInt (id);
    Array<Bool> result(value.shape());
    Bool deleteIn, deleteOut;
    const Int64* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    size_t nval = value.size();
    for (size_t i=0; i<nval; i++) {
        out[i] = anyEQ (in[i], set);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
}
MArray<Bool> TableExprNodeArray::contains (const TableExprId& id,
                                           const MArray<Double>& value)
{
    MArray<Double> set = getArrayDouble (id);
    Array<Bool> result(value.shape());
    Bool deleteIn, deleteOut;
    const Double* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    size_t nval = value.size();
    for (size_t i=0; i<nval; i++) {
        out[i] = anyEQ (in[i], set);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
}
MArray<Bool> TableExprNodeArray::contains (const TableExprId& id,
                                           const MArray<DComplex>& value)
{
    MArray<DComplex> set = getArrayDComplex (id);
    Array<Bool> result(value.shape());
    Bool deleteIn, deleteOut;
    const DComplex* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    size_t nval = value.size();
    for (size_t i=0; i<nval; i++) {
        out[i] = anyEQ (in[i], set);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
}
MArray<Bool> TableExprNodeArray::contains (const TableExprId& id,
                                           const MArray<String>& value)
{
    MArray<String> set = getArrayString (id);
    Array<Bool> result(value.shape());
    Bool deleteIn, deleteOut;
    const String* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    size_t nval = value.size();
    for (size_t i=0; i<nval; i++) {
        out[i] = anyEQ (in[i], set);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
}
MArray<Bool> TableExprNodeArray::contains (const TableExprId& id,
                                           const MArray<MVTime>& value)
{
    MArray<MVTime> set = getArrayDate (id);
    Array<Bool> result(value.shape());
    Bool deleteIn, deleteOut;
    const MVTime* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    size_t nval = value.size();
    for (size_t i=0; i<nval; i++) {
        out[i] = anyEQ (in[i], set);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
}

Bool TableExprNodeArray::getElemBool (const TableExprId& id,
                                      const Slicer& slicer)
{
    MArray<Bool> arr = getArrayBool (id);
    return arr.array()(validateIndex(slicer.start(), arr.array()));
}
Int64 TableExprNodeArray::getElemInt (const TableExprId& id,
                                      const Slicer& slicer)
{
    MArray<Int64> arr = getArrayInt (id);
    return arr.array()(validateIndex(slicer.start(), arr.array()));
}
Double TableExprNodeArray::getElemDouble (const TableExprId& id,
                                          const Slicer& slicer)
{
    MArray<Double> arr = getArrayDouble (id);
    return arr.array()(validateIndex(slicer.start(), arr.array()));
}
DComplex TableExprNodeArray::getElemDComplex (const TableExprId& id,
                                              const Slicer& slicer)
{
    MArray<DComplex> arr = getArrayDComplex (id);
    return arr.array()(validateIndex(slicer.start(), arr.array()));
}
String TableExprNodeArray::getElemString (const TableExprId& id,
                                          const Slicer& slicer)
{
    MArray<String> arr = getArrayString (id);
    return arr.array()(validateIndex(slicer.start(), arr.array()));
}
MVTime TableExprNodeArray::getElemDate (const TableExprId& id,
                                        const Slicer& slicer)
{
    MArray<MVTime> arr = getArrayDate (id);
    return arr.array()(validateIndex(slicer.start(), arr.array()));
}

MArray<Bool> TableExprNodeArray::getSliceBool (const TableExprId& id,
                                               const Slicer& slicer)
{
    MArray<Bool> arr = getArrayBool (id);
    if (arr.isNull()) {
      return arr;
    }
    IPosition start, end, incr;
    slicer.inferShapeFromSource (arr.array().shape(), start, end, incr);
    return arr(start, end, incr);
}
MArray<Int64> TableExprNodeArray::getSliceInt (const TableExprId& id,
                                               const Slicer& slicer)
{
    MArray<Int64> arr = getArrayInt (id);
    if (arr.isNull()) {
      return arr;
    }
    IPosition start, end, incr;
    slicer.inferShapeFromSource (arr.shape(), start, end, incr);
    return arr(start, end, incr);
}
MArray<Double> TableExprNodeArray::getSliceDouble (const TableExprId& id,
                                                   const Slicer& slicer)
{
    MArray<Double> arr = getArrayDouble (id);
    if (arr.isNull()) {
      return arr;
    }
    IPosition start, end, incr;
    slicer.inferShapeFromSource (arr.array().shape(), start, end, incr);
    return arr(start, end, incr);
}
MArray<DComplex> TableExprNodeArray::getSliceDComplex (const TableExprId& id,
                                                       const Slicer& slicer)
{
    MArray<DComplex> arr = getArrayDComplex (id);
    if (arr.isNull()) {
      return arr;
    }
    IPosition start, end, incr;
    slicer.inferShapeFromSource (arr.array().shape(), start, end, incr);
    return arr(start, end, incr);
}
MArray<String> TableExprNodeArray::getSliceString (const TableExprId& id,
                                                   const Slicer& slicer)
{
    MArray<String> arr = getArrayString (id);
    if (arr.isNull()) {
      return arr;
    }
    IPosition start, end, incr;
    slicer.inferShapeFromSource (arr.array().shape(), start, end, incr);
    return arr(start, end, incr);
}
MArray<MVTime> TableExprNodeArray::getSliceDate (const TableExprId& id,
                                                 const Slicer& slicer)
{
    MArray<MVTime> arr = getArrayDate (id);
    if (arr.isNull()) {
      return arr;
    }
    IPosition start, end, incr;
    slicer.inferShapeFromSource (arr.array().shape(), start, end, incr);
    return arr(start, end, incr);
}

Array<Bool>     TableExprNodeArray::getElemColumnBool (const Vector<rownr_t>&,
                                                       const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnBool(Slicer) not implemented)");
    return Array<Bool>();
}
Array<uChar>    TableExprNodeArray::getElemColumnuChar (const Vector<rownr_t>&,
                                                        const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnuChar(Slicer) not implemented)");
    return Array<uChar>();
}
Array<Short>    TableExprNodeArray::getElemColumnShort (const Vector<rownr_t>&,
                                                        const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnShort(Slicer) not implemented)");
    return Array<Short>();
}
Array<uShort>   TableExprNodeArray::getElemColumnuShort (const Vector<rownr_t>&,
                                                         const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnuShort(Slicer) not implemented)");
    return Array<uShort>();
}
Array<Int>      TableExprNodeArray::getElemColumnInt (const Vector<rownr_t>&,
                                                      const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnInt(Slicer) not implemented)");
    return Array<Int>();
}
Array<uInt>     TableExprNodeArray::getElemColumnuInt (const Vector<rownr_t>&,
                                                       const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnuInt(Slicer) not implemented)");
    return Array<uInt>();
}
Array<Int64>    TableExprNodeArray::getElemColumnInt64 (const Vector<rownr_t>&,
                                                        const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnInt64(Slicer) not implemented)");
    return Array<Int64>();
}
Array<Float>    TableExprNodeArray::getElemColumnFloat (const Vector<rownr_t>&,
                                                        const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnFloat(Slicer) not implemented)");
    return Array<Float>();
}
Array<Double>   TableExprNodeArray::getElemColumnDouble (const Vector<rownr_t>&,
                                                         const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnDouble(Slicer) not implemented)");
    return Array<Double>();
}
Array<Complex>  TableExprNodeArray::getElemColumnComplex (const Vector<rownr_t>&,
                                                          const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnComplex(Slicer) not implemented)");
    return Array<Complex>();
}
Array<DComplex> TableExprNodeArray::getElemColumnDComplex (const Vector<rownr_t>&,
                                                           const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnDComplex(Slicer) not implemented)");
    return Array<DComplex>();
}
Array<String>   TableExprNodeArray::getElemColumnString (const Vector<rownr_t>&,
                                                         const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnString(Slicer) not implemented)");
    return Array<String>();
}

MArray<Int64> TableExprNodeArray::makeArray (const IPosition& shape,
                                             Int64 value)
{
    Array<Int64> arr(shape);
    arr.set (value);
    return MArray<Int64>(arr);
}
MArray<Double> TableExprNodeArray::makeArray (const IPosition& shape,
                                             Double value)
{
    Array<Double> arr(shape);
    arr.set (value);
    return MArray<Double>(arr);
}
MArray<DComplex> TableExprNodeArray::makeArray (const IPosition& shape,
                                               const DComplex& value)
{
    Array<DComplex> arr(shape);
    arr.set (value);
    return MArray<DComplex>(arr);
}



// ----------------------------------
// TableExprNodeArrayColumn functions
// ----------------------------------

TableExprNodeArrayColumn::TableExprNodeArrayColumn
                                           (const TableColumn& tablecol,
                                            const TableExprInfo& tableInfo)
: TableExprNodeArray (NTNumeric, OtColumn),
  tableInfo_p        (tableInfo),
  tabCol_p           (tablecol),
  applySelection_p   (True)
{
    //# Fill in the real data type and the base table pointer.
    switch (tabCol_p.columnDesc().dataType()) {
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
        throw (TableInvExpr (tabCol_p.columnDesc().name(),
                             "unknown data type"));
    }
    exprtype_p = Variable;
    // Set the fixed shape and dimensionality (if known).
    ndim_p = tabCol_p.ndimColumn();
    if (ndim_p == 0) {
        ndim_p = -1;                         // unknown dimensionality
    }
    shape_p = tabCol_p.shapeColumn();
    setUnit (TableExprNodeColumn::getColumnUnit(tabCol_p));
}

TableExprInfo TableExprNodeArrayColumn::getTableInfo() const
{
    return tableInfo_p;
}

void TableExprNodeArrayColumn::disableApplySelection()
{
    applySelection_p = False;
}
  
void TableExprNodeArrayColumn::applySelection (const Vector<rownr_t>& rownrs)
{
    if (applySelection_p) {
        // Attach the column to the selection of the table.
        // Get column name before doing selection!!!!
        String name = tabCol_p.columnDesc().name();
        tableInfo_p.apply (rownrs);
        tabCol_p = TableColumn(tableInfo_p.table(), name);
        // Reset switch, because the column object can be used multiple times.
        // when a select expression is used as e.g. sort key.
        applySelection_p = False;
    }
}

const IPosition& TableExprNodeArrayColumn::getShape (const TableExprId& id)
{
    varShape_p.resize (0);
    if (tabCol_p.isDefined (id.rownr())) {
      varShape_p = tabCol_p.shape (id.rownr());
    }
    return varShape_p;
}

Bool TableExprNodeArrayColumn::isDefined (const TableExprId& id)
{
    return tabCol_p.isDefined (id.rownr());
}

Bool TableExprNodeArrayColumn::getColumnDataType (DataType& dt) const
{
    dt = tabCol_p.columnDesc().dataType();
    return True;
}



TableExprNodeArrayColumnBool::TableExprNodeArrayColumnBool
                                           (const TableColumn& col,
                                            const TableExprInfo& tableInfo)
: TableExprNodeArrayColumn (col, tableInfo),
  col_p                    (col)
{}

void TableExprNodeArrayColumnBool::applySelection (const Vector<rownr_t>& rownrs)
{
    TableExprNodeArrayColumn::applySelection (rownrs);
    col_p = ArrayColumn<Bool>(tabCol_p);
}

Bool TableExprNodeArrayColumnBool::getElemBool (const TableExprId& id,
                                                const Slicer& index)
{
    Array<Bool> arr = col_p.getSlice (id.rownr(), index);
    return *arr.data();
}
MArray<Bool> TableExprNodeArrayColumnBool::getArrayBool (const TableExprId& id)
{
  if (tabCol_p.isDefined (id.rownr())) {
    return MArray<Bool> (col_p(id.rownr()));
  }
  return MArray<Bool>();
}
MArray<Bool> TableExprNodeArrayColumnBool::getSliceBool (const TableExprId& id,
                                                         const Slicer& index)
{
  if (tabCol_p.isDefined (id.rownr())) {
    return MArray<Bool> (col_p.getSlice (id.rownr(), index));
  }
  return MArray<Bool>();
}
Array<Bool> TableExprNodeArrayColumnBool::getElemColumnBool
(const Vector<rownr_t>& rownrs, const Slicer& index)
{
    return col_p.getColumnCells (rownrs, index);
}

TableExprNodeArrayColumnuChar::TableExprNodeArrayColumnuChar
                                           (const TableColumn& col,
                                            const TableExprInfo& tableInfo)
: TableExprNodeArrayColumn (col, tableInfo),
  col_p                    (col)
{}

void TableExprNodeArrayColumnuChar::applySelection (const Vector<rownr_t>& rownrs)
{
    TableExprNodeArrayColumn::applySelection (rownrs);
    col_p = ArrayColumn<uChar>(tabCol_p);
}

Int64 TableExprNodeArrayColumnuChar::getElemInt (const TableExprId& id,
                                                 const Slicer& index)
{
    Array<uChar> arr = col_p.getSlice (id.rownr(), index);
    return *arr.data();
}
MArray<Int64> TableExprNodeArrayColumnuChar::getArrayInt
                                                    (const TableExprId& id)
{
  if (tabCol_p.isDefined (id.rownr())) {
    Array<uChar> arr = col_p (id.rownr());
    Array<Int64> out (arr.shape());
    convertArray (out, arr);
    return MArray<Int64> (out);
  }
  return MArray<Int64>();
}
MArray<Int64> TableExprNodeArrayColumnuChar::getSliceInt
                                                    (const TableExprId& id,
                                                     const Slicer& index)
{
  if (tabCol_p.isDefined (id.rownr())) {
    Array<uChar> arr = col_p.getSlice (id.rownr(), index);
    Array<Int64> out (arr.shape());
    convertArray (out, arr);
    return MArray<Int64> (out);
  }
  return MArray<Int64>();
}
Array<uChar> TableExprNodeArrayColumnuChar::getElemColumnuChar
(const Vector<rownr_t>& rownrs, const Slicer& index)
{
    return col_p.getColumnCells (rownrs, index);
}

TableExprNodeArrayColumnShort::TableExprNodeArrayColumnShort
                                           (const TableColumn& col,
                                            const TableExprInfo& tableInfo)
: TableExprNodeArrayColumn (col, tableInfo),
  col_p                    (col)
{}

void TableExprNodeArrayColumnShort::applySelection (const Vector<rownr_t>& rownrs)
{
    TableExprNodeArrayColumn::applySelection (rownrs);
    col_p = ArrayColumn<Short>(tabCol_p);
}

Int64 TableExprNodeArrayColumnShort::getElemInt (const TableExprId& id,
                                                 const Slicer& index)
{
    Array<Short> arr = col_p.getSlice (id.rownr(), index);
    return *arr.data();
}
MArray<Int64> TableExprNodeArrayColumnShort::getArrayInt
                                                    (const TableExprId& id)
{
  if (tabCol_p.isDefined (id.rownr())) {
    Array<Short> arr = col_p (id.rownr());
    Array<Int64> out (arr.shape());
    convertArray (out, arr);
    return MArray<Int64> (out);
  }
  return MArray<Int64>();
}
MArray<Int64> TableExprNodeArrayColumnShort::getSliceInt
                                                    (const TableExprId& id,
                                                     const Slicer& index)
{
  if (tabCol_p.isDefined (id.rownr())) {
    Array<Short> arr = col_p.getSlice (id.rownr(), index);
    Array<Int64> out (arr.shape());
    convertArray (out, arr);
    return MArray<Int64> (out);
  }
  return MArray<Int64>();
}
Array<Short> TableExprNodeArrayColumnShort::getElemColumnShort
(const Vector<rownr_t>& rownrs, const Slicer& index)
{
    return col_p.getColumnCells (rownrs, index);
}

TableExprNodeArrayColumnuShort::TableExprNodeArrayColumnuShort
                                           (const TableColumn& col,
                                            const TableExprInfo& tableInfo)
: TableExprNodeArrayColumn (col, tableInfo),
  col_p                    (col)
{}

void TableExprNodeArrayColumnuShort::applySelection (const Vector<rownr_t>& rownrs)
{
    TableExprNodeArrayColumn::applySelection (rownrs);
    col_p = ArrayColumn<uShort>(tabCol_p);
}

Int64 TableExprNodeArrayColumnuShort::getElemInt (const TableExprId& id,
                                                  const Slicer& index)
{
    Array<uShort> arr = col_p.getSlice (id.rownr(), index);
    return *arr.data();
}
MArray<Int64> TableExprNodeArrayColumnuShort::getArrayInt
                                                     (const TableExprId& id)
{
  if (tabCol_p.isDefined (id.rownr())) {
    Array<uShort> arr = col_p (id.rownr());
    Array<Int64> out (arr.shape());
    convertArray (out, arr);
    return MArray<Int64> (out);
  }
  return MArray<Int64>();
}
MArray<Int64> TableExprNodeArrayColumnuShort::getSliceInt
                                                     (const TableExprId& id,
                                                      const Slicer& index)
{
  if (tabCol_p.isDefined (id.rownr())) {
    Array<uShort> arr = col_p.getSlice (id.rownr(), index);
    Array<Int64> out (arr.shape());
    convertArray (out, arr);
    return MArray<Int64> (out);
  }
  return MArray<Int64>();
}
Array<uShort> TableExprNodeArrayColumnuShort::getElemColumnuShort
(const Vector<rownr_t>& rownrs, const Slicer& index)
{
    return col_p.getColumnCells (rownrs, index);
}

TableExprNodeArrayColumnInt::TableExprNodeArrayColumnInt
                                           (const TableColumn& col,
                                            const TableExprInfo& tableInfo)
: TableExprNodeArrayColumn (col, tableInfo),
  col_p                    (col)
{}

void TableExprNodeArrayColumnInt::applySelection (const Vector<rownr_t>& rownrs)
{
    TableExprNodeArrayColumn::applySelection (rownrs);
    col_p = ArrayColumn<Int>(tabCol_p);
}

Int64 TableExprNodeArrayColumnInt::getElemInt (const TableExprId& id,
                                               const Slicer& index)
{
    Array<Int> arr = col_p.getSlice (id.rownr(), index);
    return *arr.data();
}
MArray<Int64> TableExprNodeArrayColumnInt::getArrayInt
                                                  (const TableExprId& id)
{
  if (tabCol_p.isDefined (id.rownr())) {
    Array<Int> arr = col_p (id.rownr());
    Array<Int64> out (arr.shape());
    convertArray (out, arr);
    return MArray<Int64> (out);
  }
  return MArray<Int64>();
}
MArray<Int64> TableExprNodeArrayColumnInt::getSliceInt
                                                  (const TableExprId& id,
                                                   const Slicer& index)
{
  if (tabCol_p.isDefined (id.rownr())) {
    Array<Int> arr = col_p.getSlice (id.rownr(), index);
    Array<Int64> out (arr.shape());
    convertArray (out, arr);
    return MArray<Int64> (out);
  }
  return MArray<Int64>();
}
Array<Int> TableExprNodeArrayColumnInt::getElemColumnInt
(const Vector<rownr_t>& rownrs, const Slicer& index)
{
    return col_p.getColumnCells (rownrs, index);
}

TableExprNodeArrayColumnuInt::TableExprNodeArrayColumnuInt
                                           (const TableColumn& col,
                                            const TableExprInfo& tableInfo)
: TableExprNodeArrayColumn (col, tableInfo),
  col_p                    (col)
{}

void TableExprNodeArrayColumnuInt::applySelection (const Vector<rownr_t>& rownrs)
{
    TableExprNodeArrayColumn::applySelection (rownrs);
    col_p = ArrayColumn<uInt>(tabCol_p);
}

Int64 TableExprNodeArrayColumnuInt::getElemInt (const TableExprId& id,
                                                const Slicer& index)
{
    Array<uInt> arr = col_p.getSlice (id.rownr(), index);
    return *arr.data();
}
MArray<Int64> TableExprNodeArrayColumnuInt::getArrayInt
                                                   (const TableExprId& id)
{
  if (tabCol_p.isDefined (id.rownr())) {
    Array<uInt> arr = col_p (id.rownr());
    Array<Int64> out (arr.shape());
    convertArray (out, arr);
    return MArray<Int64> (out);
  }
  return MArray<Int64>();
}
MArray<Int64> TableExprNodeArrayColumnuInt::getSliceInt
                                                   (const TableExprId& id,
                                                    const Slicer& index)
{
  if (tabCol_p.isDefined (id.rownr())) {
    Array<uInt> arr = col_p.getSlice (id.rownr(), index);
    Array<Int64> out (arr.shape());
    convertArray (out, arr);
    return MArray<Int64> (out);
  }
  return MArray<Int64>();
}
Array<uInt> TableExprNodeArrayColumnuInt::getElemColumnuInt
(const Vector<rownr_t>& rownrs, const Slicer& index)
{
    return col_p.getColumnCells (rownrs, index);
}

TableExprNodeArrayColumnInt64::TableExprNodeArrayColumnInt64
                                           (const TableColumn& col,
                                            const TableExprInfo& tableInfo)
: TableExprNodeArrayColumn (col, tableInfo),
  col_p                    (col)
{}

void TableExprNodeArrayColumnInt64::applySelection (const Vector<rownr_t>& rownrs)
{
    TableExprNodeArrayColumn::applySelection (rownrs);
    col_p = ArrayColumn<Int64>(tabCol_p);
}

Int64 TableExprNodeArrayColumnInt64::getElemInt (const TableExprId& id,
                                                 const Slicer& index)
{
    Array<Int64> arr = col_p.getSlice (id.rownr(), index);
    return *arr.data();
}
MArray<Int64> TableExprNodeArrayColumnInt64::getArrayInt
                                                    (const TableExprId& id)
{
  if (tabCol_p.isDefined (id.rownr())) {
    return MArray<Int64> (col_p (id.rownr()));
  }
  return MArray<Int64>();
}
MArray<Int64> TableExprNodeArrayColumnInt64::getSliceInt
                                                    (const TableExprId& id,
                                                     const Slicer& index)
{
  if (tabCol_p.isDefined (id.rownr())) {
    return MArray<Int64> (col_p.getSlice (id.rownr(), index));
  }
  return MArray<Int64>();
}
Array<Int64> TableExprNodeArrayColumnInt64::getElemColumnInt64
(const Vector<rownr_t>& rownrs, const Slicer& index)
{
    return col_p.getColumnCells (rownrs, index);
}

TableExprNodeArrayColumnFloat::TableExprNodeArrayColumnFloat
                                           (const TableColumn& col,
                                            const TableExprInfo& tableInfo)
: TableExprNodeArrayColumn (col, tableInfo),
  col_p                    (col)
{}

void TableExprNodeArrayColumnFloat::applySelection (const Vector<rownr_t>& rownrs)
{
    TableExprNodeArrayColumn::applySelection (rownrs);
    col_p = ArrayColumn<Float>(tabCol_p);
}

Double TableExprNodeArrayColumnFloat::getElemDouble (const TableExprId& id,
                                                     const Slicer& index)
{
    Array<Float> arr = col_p.getSlice (id.rownr(), index);
    return *arr.data();
}
MArray<Double> TableExprNodeArrayColumnFloat::getArrayDouble
                                                    (const TableExprId& id)
{
  if (tabCol_p.isDefined (id.rownr())) {
    Array<Float> arr = col_p (id.rownr());
    Array<Double> out (arr.shape());
    convertArray (out, arr);
    return MArray<Double>(out);
  }
  return MArray<Double>();
}
MArray<Double> TableExprNodeArrayColumnFloat::getSliceDouble
                                                    (const TableExprId& id,
                                                     const Slicer& index)
{
  if (tabCol_p.isDefined (id.rownr())) {
    Array<Float> arr = col_p.getSlice (id.rownr(), index);
    Array<Double> out (arr.shape());
    convertArray (out, arr);
    return MArray<Double>(out);
  }
  return MArray<Double>();
}
Array<Float> TableExprNodeArrayColumnFloat::getElemColumnFloat
(const Vector<rownr_t>& rownrs, const Slicer& index)
{
    return col_p.getColumnCells (rownrs, index);
}

TableExprNodeArrayColumnDouble::TableExprNodeArrayColumnDouble
                                           (const TableColumn& col,
                                            const TableExprInfo& tableInfo)
: TableExprNodeArrayColumn (col, tableInfo),
  col_p                    (col)
{}

void TableExprNodeArrayColumnDouble::applySelection (const Vector<rownr_t>& rownrs)
{
    TableExprNodeArrayColumn::applySelection (rownrs);
    col_p = ArrayColumn<Double>(tabCol_p);
}

Double TableExprNodeArrayColumnDouble::getElemDouble (const TableExprId& id,
                                                      const Slicer& index)
{
    Array<Double> arr = col_p.getSlice (id.rownr(), index);
    return *arr.data();
}
MArray<Double> TableExprNodeArrayColumnDouble::getArrayDouble
                                                     (const TableExprId& id)
{
  if (tabCol_p.isDefined (id.rownr())) {
    return MArray<Double> (col_p (id.rownr()));
  }
  return MArray<Double>();
}
MArray<Double> TableExprNodeArrayColumnDouble::getSliceDouble
                                                     (const TableExprId& id,
                                                      const Slicer& index)
{
  if (tabCol_p.isDefined (id.rownr())) {
    return MArray<Double> (col_p.getSlice (id.rownr(), index));
  }
  return MArray<Double>();
}
Array<Double> TableExprNodeArrayColumnDouble::getElemColumnDouble
(const Vector<rownr_t>& rownrs, const Slicer& index)
{
    return col_p.getColumnCells (rownrs, index);
}

TableExprNodeArrayColumnComplex::TableExprNodeArrayColumnComplex
                                           (const TableColumn& col,
                                            const TableExprInfo& tableInfo)
: TableExprNodeArrayColumn (col, tableInfo),
  col_p                    (col)
{}

void TableExprNodeArrayColumnComplex::applySelection (const Vector<rownr_t>& rownrs)
{
    TableExprNodeArrayColumn::applySelection (rownrs);
    col_p = ArrayColumn<Complex>(tabCol_p);
}

DComplex TableExprNodeArrayColumnComplex::getElemDComplex
                                                     (const TableExprId& id,
                                                      const Slicer& index)
{
    Array<Complex> arr = col_p.getSlice (id.rownr(), index);
    return *arr.data();
}
MArray<DComplex> TableExprNodeArrayColumnComplex::getArrayDComplex
                                                     (const TableExprId& id)
{
  if (tabCol_p.isDefined (id.rownr())) {
    Array<Complex> arr = col_p (id.rownr());
    Array<DComplex> out (arr.shape());
    convertArray (out, arr);
    return MArray<DComplex> (out);
  }
  return MArray<DComplex>();
}
MArray<DComplex> TableExprNodeArrayColumnComplex::getSliceDComplex
                                                     (const TableExprId& id,
                                                      const Slicer& index)
{
  if (tabCol_p.isDefined (id.rownr())) {
    Array<Complex> arr = col_p.getSlice (id.rownr(), index);
    Array<DComplex> out (arr.shape());
    convertArray (out, arr);
    return MArray<DComplex> (out);
  }
  return MArray<DComplex>();
}
Array<Complex> TableExprNodeArrayColumnComplex::getElemColumnComplex
(const Vector<rownr_t>& rownrs, const Slicer& index)
{
    return col_p.getColumnCells (rownrs, index);
}

TableExprNodeArrayColumnDComplex::TableExprNodeArrayColumnDComplex
                                           (const TableColumn& col,
                                            const TableExprInfo& tableInfo)
: TableExprNodeArrayColumn (col, tableInfo),
  col_p                    (col)
{}

void TableExprNodeArrayColumnDComplex::applySelection (const Vector<rownr_t>& rownrs)
{
    TableExprNodeArrayColumn::applySelection (rownrs);
    col_p = ArrayColumn<DComplex>(tabCol_p);
}

DComplex TableExprNodeArrayColumnDComplex::getElemDComplex
                                                     (const TableExprId& id,
                                                      const Slicer& index)
{
    Array<DComplex> arr = col_p.getSlice (id.rownr(), index);
    return *arr.data();
}
MArray<DComplex> TableExprNodeArrayColumnDComplex::getArrayDComplex
                                                     (const TableExprId& id)
{
  if (tabCol_p.isDefined (id.rownr())) {
    return MArray<DComplex> (col_p (id.rownr()));
  }
  return MArray<DComplex>();
}
MArray<DComplex> TableExprNodeArrayColumnDComplex::getSliceDComplex
                                                     (const TableExprId& id,
                                                      const Slicer& index)
{
  if (tabCol_p.isDefined (id.rownr())) {
    return MArray<DComplex> (col_p.getSlice (id.rownr(), index));
  }
  return MArray<DComplex>();
}
Array<DComplex> TableExprNodeArrayColumnDComplex::getElemColumnDComplex
(const Vector<rownr_t>& rownrs, const Slicer& index)
{
    return col_p.getColumnCells (rownrs, index);
}

TableExprNodeArrayColumnString::TableExprNodeArrayColumnString
                                           (const TableColumn& col,
                                            const TableExprInfo& tableInfo)
: TableExprNodeArrayColumn (col, tableInfo),
  col_p                    (col)
{}

void TableExprNodeArrayColumnString::applySelection (const Vector<rownr_t>& rownrs)
{
    TableExprNodeArrayColumn::applySelection (rownrs);
    col_p = ArrayColumn<String>(tabCol_p);
}

String TableExprNodeArrayColumnString::getElemString (const TableExprId& id,
                                                      const Slicer& index)
{
    Array<String> arr = col_p.getSlice (id.rownr(), index);
    return *arr.data();
}
MArray<String> TableExprNodeArrayColumnString::getArrayString
                                                     (const TableExprId& id)
{
  if (tabCol_p.isDefined (id.rownr())) {
    return MArray<String> (col_p (id.rownr()));
  }
  return MArray<String>();
}
MArray<String> TableExprNodeArrayColumnString::getSliceString
                                                     (const TableExprId& id,
                                                      const Slicer& index)
{
  if (tabCol_p.isDefined (id.rownr())) {
    return MArray<String> (col_p.getSlice (id.rownr(), index));
  }
  return MArray<String>();
}
Array<String> TableExprNodeArrayColumnString::getElemColumnString
(const Vector<rownr_t>& rownrs, const Slicer& index)
{
    return col_p.getColumnCells (rownrs, index);
}




// ----------------------------
// TableExprNodeIndex functions
// ----------------------------

TableExprNodeIndex::TableExprNodeIndex (const TableExprNodeSet& indices,
                                        const TaQLStyle& style)
: TableExprNodeMulti (NTInt, VTIndex, OtColumn, indices),
  origin_p           (style.origin()),
  endMinus_p         (0),
  isCOrder_p         (style.isCOrder()),
  isSingle_p         (True)
{
    if (style.isEndExcl()) endMinus_p = 1;
    fillIndex (indices);
}

void TableExprNodeIndex::checkIndexValues (const TENShPtr& arrayNode)
{
    uInt i;
    Int ndim = arrayNode->ndim();
    uInt n = start_p.size();
    // Check against dimensionality (if fixed).
    if (ndim >= 0  &&  ndim != Int(n)) {
        throw (TableInvExpr ("#indices mismatches array dimensionality"));
    }
    // Check start and increment values.
    for (i=0; i<n; i++) {
        if (!varIndex_p[3*i + 2]) {
            if (incr_p(i) < 0) {
                throw (TableInvExpr ("index increment value is negative"));
            }
        }
    }
    // Check against array shape (if fixed).
    IPosition shape = arrayNode->shape();
    if (shape.size() > 0) {
        for (i=0; i<n; i++) {
            if (!varIndex_p[3*i]) {
                if (start_p(i) >= shape(i)) {
                    throw (TableInvExpr("index value exceeds array shape"));
                }
            }
            if (!varIndex_p[3*i + 1]) {
                if (end_p(i) >= shape(i)) {
                    throw (TableInvExpr("index end value exceeds array shape"));
                }
            }
        }
    }
}

void TableExprNodeIndex::fillSlicer (const TableExprId& id)
{
    uInt n = varIndex_p.size();
    uInt i = 0;
    uInt j = 0;
    while (j < n) {
        if (varIndex_p[j]) {
            Int64 val = operands_p[j]->getInt (id);
            if (val < 0) {
                start_p(i) = val;
            }else{
                start_p(i) = val - origin_p;
            }
        }
        j++;
        if (varIndex_p[j]) {
            if (operands_p[j] == 0) {
                end_p(i) = start_p(i);
            }else{
                Int64 val = operands_p[j]->getInt (id);
                if (val < 0) {
                    end_p(i) = val - endMinus_p;
                }else{
                    end_p(i) = val - origin_p - endMinus_p;
                }
            }
        }
        j++;
        if (varIndex_p[j]) {
            incr_p(i) = operands_p[j]->getInt(id);
        }
        j++;
        i++;
    }
    slicer_p = Slicer (start_p, end_p, incr_p, Slicer::endIsLast);
}

// Fill the children pointers of a node.
// Also reduce the tree if possible by combining constants.
void TableExprNodeIndex::fillIndex (const TableExprNodeSet& indices)
{
    // Check that the set elements have equal data types.
    indices.checkEqualDataTypes();
    // Check that the set contains discrete values.
    if (! indices.isDiscrete()) {
        throw (TableInvExpr ("Index values must be discrete (with possible :"));
    }
    TENShPtr rep;
    // Copy block of start, end, and increment.
    // Determine if single element subscripting is done.
    // That is true if all starts are given and no end and increment values.
    // Check if all indices have data type Int and are scalars.
    uInt n = indices.size();
    operands_p.resize (3 * n);
    uInt j = 0;
    for (uInt i=0; i<n; i++) {
        uInt inx = (isCOrder_p  ?  n-i-1 : i);
        rep = indices[inx]->start();
        if (rep) {
            operands_p[j] = rep;
        }else{
            isSingle_p = False;
        }
        j++;
        rep = indices[inx]->end();
        if (rep) {
            operands_p[j] = rep;
            isSingle_p = False;
        }
        j++;
        rep = indices[inx]->increment();
        if (rep) {
            operands_p[j] = rep;
            isSingle_p = False;
        }
        j++;
    }
    // Check if all indices have data type Int, are scalars, and don't
    // use aggregate functions.
    for (uInt i=0; i<j; i++) {
        if (operands_p[i] != 0) {
            if (operands_p[i]->dataType()  != NTInt
            ||  operands_p[i]->valueType() != VTScalar) {
                throw (TableInvExpr ("Index value must be an integer scalar"));
            }
            TableExprNodeUtil::checkAggrFuncs (operands_p[i].get());
        }
    }
    convertConstIndex();
    if (isConstant()) {
        slicer_p = Slicer (start_p, end_p, incr_p, Slicer::endIsLast);
    }
}

void TableExprNodeIndex::convertConstIndex()
{
    TENShPtr rep;
    uInt n = operands_p.size() / 3;
    start_p.resize (n);
    end_p.resize (n);
    incr_p.resize (n);
    varIndex_p.resize (3*n);
    varIndex_p.set (False);
    uInt j = 0;
    for (uInt i=0; i<n; i++) {
        // If no start value is given, it is 0.
        rep = operands_p[j];
        start_p(i) = 0;
        if (rep != 0) {
            if (rep->isConstant()) {
                Int64 val = rep->getInt(0);
                if (val < 0) {
                    start_p(i) = val;
                }else{
                    start_p(i) = val - origin_p;
                }
            }else{
                varIndex_p[j] = True;
            }
        }
        j++;
        // If no end value is given, it is initially set to the end.
        // If a start is given, it is set to start.
        // A negative end means till the end.
        rep = operands_p[j];
        end_p(i) = Slicer::MimicSource;
        if (rep != 0) {
            if (rep->isConstant()) {
                Int64 val = rep->getInt(0);
                if (val != Slicer::MimicSource) {
                    if (val < 0) {
                        end_p(i) = val - endMinus_p;
                    }else{
                        end_p(i) = val - origin_p - endMinus_p;
                    }
                }
            }else{
                varIndex_p[j] = True;
            }
        }else{
            if (operands_p[j-1] != 0) {
                end_p(i) = start_p(i);
                varIndex_p[j] = varIndex_p[j-1];
            }
        }
            
        j++;
        // If no increment value is given, it is 1.
        rep = operands_p[j];
        incr_p(i) = 1;
        if (rep != 0) {
            if (rep->isConstant()) {
              incr_p(i) = rep->getInt(0);
            }else{
                varIndex_p[j] = True;
            }
        }
        j++;
    }
}




// ----------------------
// TableExprNodeArrayPart
// ----------------------
TableExprNodeArrayPart::TableExprNodeArrayPart (const TENShPtr& arrayNode,
                                                const TENShPtr& indexNode)
: TableExprNodeArray (arrayNode->dataType(), OtSlice),
  colNode_p          (0)
{
    // Keep nodes and cast them to the array and index node.
    lnode_p = arrayNode;
    rnode_p = indexNode;
    arrNode_p = dynamic_cast<TableExprNodeArray*>(arrayNode.get());
    AlwaysAssert (arrNode_p, AipsError);
    inxNode_p = dynamic_cast<TableExprNodeIndex*>(indexNode.get());
    AlwaysAssert (inxNode_p, AipsError);
    // Check the index bounds as far as possible.
    inxNode_p->checkIndexValues (arrayNode);
    fillExprType  (indexNode.get());
    fillExprType  (arrayNode.get());
    // If indexing a single element, the result is a scalar.
    if (inxNode_p->isSingle()) {
        vtype_p = VTScalar;
        ndim_p  = 0;
    } else if (inxNode_p->isConstant()) {
        // Otherwise if the index node is constant, it may be possible
        // to determine the resulting shape.
        const Slicer& slicer = inxNode_p->getSlicer(0);
        // If all slicer lengths are defined, that is the resulting shape.
        if (slicer.isFixed()) {
            shape_p = slicer.length();
            ndim_p  = shape_p.size();
        }else{
            // If some are depending on array shape, the resulting
            // shape can be determined if the array shape is fixed.
            IPosition arrshp = arrayNode->shape();
            if (arrshp.size() > 0) {
                IPosition blc,trc,inc;
                shape_p = slicer.inferShapeFromSource (arrshp, blc, trc, inc);
                ndim_p  = shape_p.size();
            }
        }
    }
    if (inxNode_p->isConstant()) {
        // If the constant child is an ArrayColumn, things can be
        // improved in getColumnXXX.
        colNode_p = dynamic_cast<TableExprNodeArrayColumn*>(arrayNode.get());
    }
    setUnit (arrayNode->unit());
}



void TableExprNodeArrayPart::show (ostream& os, uInt indent) const
{
    TableExprNodeRep::show (os, indent);
    os << "array: ";
    arrNode_p->show (os, indent+2);
    os << "index: ";
    inxNode_p->show (os, indent+2);
}

Bool TableExprNodeArrayPart::getColumnDataType (DataType& dt) const
{
    //# Return data type of column if constant index.
    if (inxNode_p->isConstant()) {
        return arrNode_p->getColumnDataType (dt);
    }
    return False;
}

Bool TableExprNodeArrayPart::getBool (const TableExprId& id)
{
    DebugAssert (valueType() == VTScalar, AipsError);
    return arrNode_p->getElemBool (id, inxNode_p->getSlicer(id));
}
Int64 TableExprNodeArrayPart::getInt (const TableExprId& id)
{
    DebugAssert (valueType() == VTScalar, AipsError);
    return arrNode_p->getElemInt (id, inxNode_p->getSlicer(id));
}
Double TableExprNodeArrayPart::getDouble (const TableExprId& id)
{
    DebugAssert (valueType() == VTScalar, AipsError);
    return arrNode_p->getElemDouble (id, inxNode_p->getSlicer(id));
}
DComplex TableExprNodeArrayPart::getDComplex (const TableExprId& id)
{
    DebugAssert (valueType() == VTScalar, AipsError);
    return arrNode_p->getElemDComplex (id, inxNode_p->getSlicer(id));
}
String TableExprNodeArrayPart::getString (const TableExprId& id)
{
    DebugAssert (valueType() == VTScalar, AipsError);
    return arrNode_p->getElemString (id, inxNode_p->getSlicer(id));
}
MVTime TableExprNodeArrayPart::getDate (const TableExprId& id)
{
    DebugAssert (valueType() == VTScalar, AipsError);
    return arrNode_p->getElemDate (id, inxNode_p->getSlicer(id));
}

MArray<Bool> TableExprNodeArrayPart::getArrayBool (const TableExprId& id)
{
    DebugAssert (valueType() == VTArray, AipsError);
    return arrNode_p->getSliceBool (id, inxNode_p->getSlicer(id));
}
MArray<Int64> TableExprNodeArrayPart::getArrayInt (const TableExprId& id)
{
    DebugAssert (valueType() == VTArray, AipsError);
    return arrNode_p->getSliceInt (id, inxNode_p->getSlicer(id));
}
MArray<Double> TableExprNodeArrayPart::getArrayDouble (const TableExprId& id)
{
    DebugAssert (valueType() == VTArray, AipsError);
    return arrNode_p->getSliceDouble (id, inxNode_p->getSlicer(id));
}
MArray<DComplex> TableExprNodeArrayPart::getArrayDComplex
                                                     (const TableExprId& id)
{
    DebugAssert (valueType() == VTArray, AipsError);
    return arrNode_p->getSliceDComplex (id, inxNode_p->getSlicer(id));
}
MArray<String> TableExprNodeArrayPart::getArrayString (const TableExprId& id)
{
    DebugAssert (valueType() == VTArray, AipsError);
    return arrNode_p->getSliceString (id, inxNode_p->getSlicer(id));
}
MArray<MVTime> TableExprNodeArrayPart::getArrayDate (const TableExprId& id)
{
    DebugAssert (valueType() == VTArray, AipsError);
    return arrNode_p->getSliceDate (id, inxNode_p->getSlicer(id));
}

Array<Bool> TableExprNodeArrayPart::getColumnBool (const Vector<rownr_t>& rownrs)
{
    if (colNode_p == 0) {
        return TableExprNodeRep::getColumnBool (rownrs);
    }
    return colNode_p->getElemColumnBool (rownrs, inxNode_p->getSlicer(0));
}
Array<uChar>    TableExprNodeArrayPart::getColumnuChar (const Vector<rownr_t>& rownrs)
{
    if (colNode_p == 0) {
        return TableExprNodeRep::getColumnuChar (rownrs);
    }
    return colNode_p->getElemColumnuChar (rownrs, inxNode_p->getSlicer(0));
}
Array<Short>    TableExprNodeArrayPart::getColumnShort (const Vector<rownr_t>& rownrs)
{
    if (colNode_p == 0) {
        return TableExprNodeRep::getColumnShort (rownrs);
    }
    return colNode_p->getElemColumnShort (rownrs, inxNode_p->getSlicer(0));
}
Array<uShort>   TableExprNodeArrayPart::getColumnuShort (const Vector<rownr_t>& rownrs)
{
    if (colNode_p == 0) {
        return TableExprNodeRep::getColumnuShort (rownrs);
    }
    return colNode_p->getElemColumnuShort (rownrs, inxNode_p->getSlicer(0));
}
Array<Int>      TableExprNodeArrayPart::getColumnInt (const Vector<rownr_t>& rownrs)
{
    if (colNode_p == 0) {
        return TableExprNodeRep::getColumnInt (rownrs);
    }
    return colNode_p->getElemColumnInt (rownrs, inxNode_p->getSlicer(0));
}
Array<uInt>     TableExprNodeArrayPart::getColumnuInt (const Vector<rownr_t>& rownrs)
{
    if (colNode_p == 0) {
        return TableExprNodeRep::getColumnuInt (rownrs);
    }
    return colNode_p->getElemColumnuInt (rownrs, inxNode_p->getSlicer(0));
}
Array<Int64>    TableExprNodeArrayPart::getColumnInt64 (const Vector<rownr_t>& rownrs)
{
    if (colNode_p == 0) {
        return TableExprNodeRep::getColumnInt64 (rownrs);
    }
    return colNode_p->getElemColumnInt64 (rownrs, inxNode_p->getSlicer(0));
}
Array<Float>    TableExprNodeArrayPart::getColumnFloat (const Vector<rownr_t>& rownrs)
{
    if (colNode_p == 0) {
        return TableExprNodeRep::getColumnFloat (rownrs);
    }
    return colNode_p->getElemColumnFloat (rownrs, inxNode_p->getSlicer(0));
}
Array<Double>   TableExprNodeArrayPart::getColumnDouble (const Vector<rownr_t>& rownrs)
{
    if (colNode_p == 0) {
        return TableExprNodeRep::getColumnDouble (rownrs);
    }
    return colNode_p->getElemColumnDouble (rownrs, inxNode_p->getSlicer(0));
}
Array<Complex>  TableExprNodeArrayPart::getColumnComplex (const Vector<rownr_t>& rownrs)
{
    if (colNode_p == 0) {
        return TableExprNodeRep::getColumnComplex (rownrs);
    }
    return colNode_p->getElemColumnComplex (rownrs, inxNode_p->getSlicer(0));
}
Array<DComplex> TableExprNodeArrayPart::getColumnDComplex (const Vector<rownr_t>& rownrs)
{
    if (colNode_p == 0) {
        return TableExprNodeRep::getColumnDComplex (rownrs);
    }
    return colNode_p->getElemColumnDComplex (rownrs, inxNode_p->getSlicer(0));
}
Array<String>   TableExprNodeArrayPart::getColumnString (const Vector<rownr_t>& rownrs)
{
    if (colNode_p == 0) {
        return TableExprNodeRep::getColumnString (rownrs);
    }
    return colNode_p->getElemColumnString (rownrs, inxNode_p->getSlicer(0));
}

} //# NAMESPACE CASACORE - END

