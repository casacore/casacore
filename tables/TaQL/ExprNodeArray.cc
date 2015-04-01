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
//#
//# $Id$

#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeArray.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableExprNodeArray::TableExprNodeArray (NodeDataType dtype, OperType otype)
: TableExprNodeBinary (dtype, VTArray, otype, Table())
{
    ndim_p = -1;
}
TableExprNodeArray::TableExprNodeArray (const TableExprNodeRep& node,
					NodeDataType dtype, OperType otype)
: TableExprNodeBinary (dtype, node, otype)
{}
TableExprNodeArray::TableExprNodeArray (NodeDataType dtype, OperType otype,
					const IPosition& shape)
: TableExprNodeBinary (dtype, VTArray, otype, Table())
{
    shape_p = shape;
    ndim_p  = shape.nelements();
    if (ndim_p == 0) {
	ndim_p = -1;
    }
}

TableExprNodeArray::~TableExprNodeArray()
{}

TableExprNodeRep* TableExprNodeArray::makeConstantScalar()
{
  if (isConstant()) {
    switch (dataType()) {
    case NTBool:
      {
        Array<Bool> arr = getArrayBool(0);
        if (arr.size() == 1) {
          return new TableExprNodeConstBool (arr.data()[0]);
        }
      }
      break;
    case NTInt:
      {
        Array<Int64> arr = getArrayInt(0);
        if (arr.size() == 1) {
          return new TableExprNodeConstInt (arr.data()[0]);
        }
      }
      break;
    case NTDouble:
      {
        Array<Double> arr = getArrayDouble(0);
        if (arr.size() == 1) {
          return new TableExprNodeConstDouble (arr.data()[0]);
        }
      }
      break;
    case NTComplex:
      {
        Array<DComplex> arr = getArrayDComplex(0);
        if (arr.size() == 1) {
          return new TableExprNodeConstDComplex (arr.data()[0]);
        }
      }
      break;
    case NTString:
      {
        Array<String> arr = getArrayString(0);
        if (arr.size() == 1) {
          return new TableExprNodeConstString (arr.data()[0]);
        }
      }
      break;
    case NTDate:
      {
        Array<MVTime> arr = getArrayDate(0);
        if (arr.size() == 1) {
          return new TableExprNodeConstDate (arr.data()[0]);
        }
      }
      break;
    default:
      break;
    }
  }
  return 0;
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

Array<Double> TableExprNodeArray::getArrayDouble (const TableExprId& id)
{
    Array<Int64> arr = getArrayInt (id);
    Array<Double> result (arr.shape());
    convertArray (result, arr);
    return result;
}

Array<DComplex> TableExprNodeArray::getArrayDComplex (const TableExprId& id)
{
    Array<Double> arr = getArrayDouble (id);
    Array<DComplex> result (arr.shape());
    convertArray (result, arr);
    return result;
}

Bool TableExprNodeArray::hasBool     (const TableExprId& id, Bool value)
{
    return anyEQ (value, getArrayBool (id));
}
Bool TableExprNodeArray::hasInt      (const TableExprId& id, Int64 value)
{
    return anyEQ (value, getArrayInt (id));
}
Bool TableExprNodeArray::hasDouble   (const TableExprId& id, Double value)
{
    return anyEQ (value, getArrayDouble (id));
}
Bool TableExprNodeArray::hasDComplex (const TableExprId& id,
				      const DComplex& value)
{
    return anyEQ (value, getArrayDComplex (id));
}
Bool TableExprNodeArray::hasString   (const TableExprId& id,
				      const String& value)
{
    return anyEQ (value, getArrayString (id));
}
Bool TableExprNodeArray::hasDate     (const TableExprId& id,
				      const MVTime& value)
{
    return anyEQ (value, getArrayDate (id));
}

Array<Bool> TableExprNodeArray::hasArrayBool (const TableExprId& id,
					      const Array<Bool>& value)
{
    Array<Bool> set = getArrayBool (id);
    Array<Bool> result(value.shape());
    Bool deleteIn, deleteOut;
    const Bool* in = value.getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    uInt nval = value.nelements();
    for (uInt i=0; i<nval; i++) {
	out[i] = anyEQ (in[i], set);
    }
    value.freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return result;
}
Array<Bool> TableExprNodeArray::hasArrayInt (const TableExprId& id,
                                             const Array<Int64>& value)
{
    Array<Int64> set = getArrayInt (id);
    Array<Bool> result(value.shape());
    Bool deleteIn, deleteOut;
    const Int64* in = value.getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    uInt nval = value.nelements();
    for (uInt i=0; i<nval; i++) {
	out[i] = anyEQ (in[i], set);
    }
    value.freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return result;
}
Array<Bool> TableExprNodeArray::hasArrayDouble (const TableExprId& id,
						const Array<Double>& value)
{
    Array<Double> set = getArrayDouble (id);
    Array<Bool> result(value.shape());
    Bool deleteIn, deleteOut;
    const Double* in = value.getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    uInt nval = value.nelements();
    for (uInt i=0; i<nval; i++) {
	out[i] = anyEQ (in[i], set);
    }
    value.freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return result;
}
Array<Bool> TableExprNodeArray::hasArrayDComplex (const TableExprId& id,
						  const Array<DComplex>& value)
{
    Array<DComplex> set = getArrayDComplex (id);
    Array<Bool> result(value.shape());
    Bool deleteIn, deleteOut;
    const DComplex* in = value.getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    uInt nval = value.nelements();
    for (uInt i=0; i<nval; i++) {
	out[i] = anyEQ (in[i], set);
    }
    value.freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return result;
}
Array<Bool> TableExprNodeArray::hasArrayString (const TableExprId& id,
						const Array<String>& value)
{
    Array<String> set = getArrayString (id);
    Array<Bool> result(value.shape());
    Bool deleteIn, deleteOut;
    const String* in = value.getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    uInt nval = value.nelements();
    for (uInt i=0; i<nval; i++) {
	out[i] = anyEQ (in[i], set);
    }
    value.freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return result;
}
Array<Bool> TableExprNodeArray::hasArrayDate (const TableExprId& id,
					      const Array<MVTime>& value)
{
    Array<MVTime> set = getArrayDate (id);
    Array<Bool> result(value.shape());
    Bool deleteIn, deleteOut;
    const MVTime* in = value.getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    uInt nval = value.nelements();
    for (uInt i=0; i<nval; i++) {
	out[i] = anyEQ (in[i], set);
    }
    value.freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return result;
}

Bool TableExprNodeArray::getElemBool (const TableExprId& id,
				      const Slicer& slicer)
{
    Array<Bool> arr = getArrayBool (id);
    arr.validateIndex (slicer.start());
    return arr(slicer.start());
}
Int64 TableExprNodeArray::getElemInt (const TableExprId& id,
                                      const Slicer& slicer)
{
    Array<Int64> arr = getArrayInt (id);
    arr.validateIndex (slicer.start());
    return arr(slicer.start());
}
Double TableExprNodeArray::getElemDouble (const TableExprId& id,
					  const Slicer& slicer)
{
    Array<Double> arr = getArrayDouble (id);
    arr.validateIndex (slicer.start());
    return arr(slicer.start());
}
DComplex TableExprNodeArray::getElemDComplex (const TableExprId& id,
					      const Slicer& slicer)
{
    Array<DComplex> arr = getArrayDComplex (id);
    arr.validateIndex (slicer.start());
    return arr(slicer.start());
}
String TableExprNodeArray::getElemString (const TableExprId& id,
					  const Slicer& slicer)
{
    Array<String> arr = getArrayString (id);
    arr.validateIndex (slicer.start());
    return arr(slicer.start());
}
MVTime TableExprNodeArray::getElemDate (const TableExprId& id,
					const Slicer& slicer)
{
    Array<MVTime> arr = getArrayDate (id);
    arr.validateIndex (slicer.start());
    return arr(slicer.start());
}

Array<Bool> TableExprNodeArray::getSliceBool (const TableExprId& id,
					      const Slicer& slicer)
{
    Array<Bool> arr = getArrayBool (id);
    IPosition start, end, incr;
    slicer.inferShapeFromSource (arr.shape(), start, end, incr);
    return arr(start, end, incr);
}
Array<Int64> TableExprNodeArray::getSliceInt (const TableExprId& id,
                                              const Slicer& slicer)
{
    Array<Int64> arr = getArrayInt (id);
    IPosition start, end, incr;
    slicer.inferShapeFromSource (arr.shape(), start, end, incr);
    return arr(start, end, incr);
}
Array<Double> TableExprNodeArray::getSliceDouble (const TableExprId& id,
						  const Slicer& slicer)
{
    Array<Double> arr = getArrayDouble (id);
    IPosition start, end, incr;
    slicer.inferShapeFromSource (arr.shape(), start, end, incr);
    return arr(start, end, incr);
}
Array<DComplex> TableExprNodeArray::getSliceDComplex (const TableExprId& id,
						      const Slicer& slicer)
{
    Array<DComplex> arr = getArrayDComplex (id);
    IPosition start, end, incr;
    slicer.inferShapeFromSource (arr.shape(), start, end, incr);
    return arr(start, end, incr);
}
Array<String> TableExprNodeArray::getSliceString (const TableExprId& id,
						  const Slicer& slicer)
{
    Array<String> arr = getArrayString (id);
    IPosition start, end, incr;
    slicer.inferShapeFromSource (arr.shape(), start, end, incr);
    return arr(start, end, incr);
}
Array<MVTime> TableExprNodeArray::getSliceDate (const TableExprId& id,
						const Slicer& slicer)
{
    Array<MVTime> arr = getArrayDate (id);
    IPosition start, end, incr;
    slicer.inferShapeFromSource (arr.shape(), start, end, incr);
    return arr(start, end, incr);
}

Array<Bool>     TableExprNodeArray::getElemColumnBool (const Vector<uInt>&,
                                                       const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnBool(Slicer) not implemented)");
    return Array<Bool>();
}
Array<uChar>    TableExprNodeArray::getElemColumnuChar (const Vector<uInt>&,
                                                        const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnuChar(Slicer) not implemented)");
    return Array<uChar>();
}
Array<Short>    TableExprNodeArray::getElemColumnShort (const Vector<uInt>&,
                                                        const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnShort(Slicer) not implemented)");
    return Array<Short>();
}
Array<uShort>   TableExprNodeArray::getElemColumnuShort (const Vector<uInt>&,
                                                         const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnuShort(Slicer) not implemented)");
    return Array<uShort>();
}
Array<Int>      TableExprNodeArray::getElemColumnInt (const Vector<uInt>&,
                                                      const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnInt(Slicer) not implemented)");
    return Array<Int>();
}
Array<uInt>     TableExprNodeArray::getElemColumnuInt (const Vector<uInt>&,
                                                       const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnuInt(Slicer) not implemented)");
    return Array<uInt>();
}
Array<Float>    TableExprNodeArray::getElemColumnFloat (const Vector<uInt>&,
                                                        const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnFloat(Slicer) not implemented)");
    return Array<Float>();
}
Array<Double>   TableExprNodeArray::getElemColumnDouble (const Vector<uInt>&,
                                                         const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnDouble(Slicer) not implemented)");
    return Array<Double>();
}
Array<Complex>  TableExprNodeArray::getElemColumnComplex (const Vector<uInt>&,
                                                          const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnComplex(Slicer) not implemented)");
    return Array<Complex>();
}
Array<DComplex> TableExprNodeArray::getElemColumnDComplex (const Vector<uInt>&,
                                                           const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnDComplex(Slicer) not implemented)");
    return Array<DComplex>();
}
Array<String>   TableExprNodeArray::getElemColumnString (const Vector<uInt>&,
                                                         const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnString(Slicer) not implemented)");
    return Array<String>();
}

Array<Int64> TableExprNodeArray::makeArray (const IPosition& shape,
                                            Int64 value)
{
    Array<Int64> arr(shape);
    arr.set (value);
    return arr;
}
Array<Double> TableExprNodeArray::makeArray (const IPosition& shape,
					     Double value)
{
    Array<Double> arr(shape);
    arr.set (value);
    return arr;
}
Array<DComplex> TableExprNodeArray::makeArray (const IPosition& shape,
					       const DComplex& value)
{
    Array<DComplex> arr(shape);
    arr.set (value);
    return arr;
}



// ----------------------------------
// TableExprNodeArrayColumn functions
// ----------------------------------

TableExprNodeArrayColumn::TableExprNodeArrayColumn
                                           (const TableColumn& tablecol,
					    const Table& table)
: TableExprNodeArray (NTNumeric, OtColumn),
  selTable_p         (table),
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
    table_p = table;
    exprtype_p = Variable;
    // Set the fixed shape and dimensionality (if known).
    ndim_p = tabCol_p.ndimColumn();
    if (ndim_p == 0) {
	ndim_p = -1;                         // unknown dimensionality
    }
    shape_p = tabCol_p.shapeColumn();
    setUnit (TableExprNodeColumn::getColumnUnit(tabCol_p));
}

TableExprNodeArrayColumn::~TableExprNodeArrayColumn()
{}

void TableExprNodeArrayColumn::getColumnNodes (vector<TableExprNodeRep*>& cols)
{
    cols.push_back (this);
}

void TableExprNodeArrayColumn::disableApplySelection()
{
    applySelection_p = False;
}
  
void TableExprNodeArrayColumn::applySelection (const Vector<uInt>& rownrs)
{
    if (applySelection_p) {
        // Attach the column to the selection of the table.
        // Get column name before doing selection!!!!
        String name = tabCol_p.columnDesc().name();
        selTable_p = selTable_p(rownrs);
        tabCol_p = TableColumn(selTable_p, name);
        // Reset switch, because the column object can be used multiple times.
        // when a select expression is used as e.g. sort key.
        applySelection_p = False;
    }
}

const IPosition& TableExprNodeArrayColumn::getShape (const TableExprId& id)
{
    varShape_p.resize (0);
    varShape_p = tabCol_p.shape (id.rownr());
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
					    const Table& table)
: TableExprNodeArrayColumn (col, table),
  col_p                    (col)
{}
TableExprNodeArrayColumnBool::~TableExprNodeArrayColumnBool()
{}

void TableExprNodeArrayColumnBool::applySelection (const Vector<uInt>& rownrs)
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
Array<Bool> TableExprNodeArrayColumnBool::getArrayBool (const TableExprId& id)
{
    return col_p(id.rownr());
}
Array<Bool> TableExprNodeArrayColumnBool::getSliceBool (const TableExprId& id,
							const Slicer& index)
{
    return col_p.getSlice (id.rownr(), index);
}
Array<Bool> TableExprNodeArrayColumnBool::getElemColumnBool
(const Vector<uInt>& rownrs, const Slicer& index)
{
    return col_p.getColumnCells (rownrs, index);
}

TableExprNodeArrayColumnuChar::TableExprNodeArrayColumnuChar
                                           (const TableColumn& col,
					    const Table& table)
: TableExprNodeArrayColumn (col, table),
  col_p                    (col)
{}
TableExprNodeArrayColumnuChar::~TableExprNodeArrayColumnuChar()
{}

void TableExprNodeArrayColumnuChar::applySelection (const Vector<uInt>& rownrs)
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
Array<Int64> TableExprNodeArrayColumnuChar::getArrayInt
                                                    (const TableExprId& id)
{
    Array<uChar> arr = col_p (id.rownr());
    Array<Int64> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<Int64> TableExprNodeArrayColumnuChar::getSliceInt
                                                    (const TableExprId& id,
						     const Slicer& index)
{
    Array<uChar> arr = col_p.getSlice (id.rownr(), index);
    Array<Int64> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<uChar> TableExprNodeArrayColumnuChar::getElemColumnuChar
(const Vector<uInt>& rownrs, const Slicer& index)
{
    return col_p.getColumnCells (rownrs, index);
}

TableExprNodeArrayColumnShort::TableExprNodeArrayColumnShort
                                           (const TableColumn& col,
					    const Table& table)
: TableExprNodeArrayColumn (col, table),
  col_p                    (col)
{}
TableExprNodeArrayColumnShort::~TableExprNodeArrayColumnShort()
{}

void TableExprNodeArrayColumnShort::applySelection (const Vector<uInt>& rownrs)
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
Array<Int64> TableExprNodeArrayColumnShort::getArrayInt
                                                    (const TableExprId& id)
{
    Array<Short> arr = col_p (id.rownr());
    Array<Int64> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<Int64> TableExprNodeArrayColumnShort::getSliceInt
                                                    (const TableExprId& id,
						     const Slicer& index)
{
    Array<Short> arr = col_p.getSlice (id.rownr(), index);
    Array<Int64> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<Short> TableExprNodeArrayColumnShort::getElemColumnShort
(const Vector<uInt>& rownrs, const Slicer& index)
{
    return col_p.getColumnCells (rownrs, index);
}

TableExprNodeArrayColumnuShort::TableExprNodeArrayColumnuShort
                                           (const TableColumn& col,
					    const Table& table)
: TableExprNodeArrayColumn (col, table),
  col_p                    (col)
{}
TableExprNodeArrayColumnuShort::~TableExprNodeArrayColumnuShort()
{}

void TableExprNodeArrayColumnuShort::applySelection (const Vector<uInt>& rownrs)
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
Array<Int64> TableExprNodeArrayColumnuShort::getArrayInt
                                                     (const TableExprId& id)
{
    Array<uShort> arr = col_p (id.rownr());
    Array<Int64> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<Int64> TableExprNodeArrayColumnuShort::getSliceInt
                                                     (const TableExprId& id,
						      const Slicer& index)
{
    Array<uShort> arr = col_p.getSlice (id.rownr(), index);
    Array<Int64> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<uShort> TableExprNodeArrayColumnuShort::getElemColumnuShort
(const Vector<uInt>& rownrs, const Slicer& index)
{
    return col_p.getColumnCells (rownrs, index);
}

TableExprNodeArrayColumnInt::TableExprNodeArrayColumnInt
                                           (const TableColumn& col,
					    const Table& table)
: TableExprNodeArrayColumn (col, table),
  col_p                    (col)
{}
TableExprNodeArrayColumnInt::~TableExprNodeArrayColumnInt()
{}

void TableExprNodeArrayColumnInt::applySelection (const Vector<uInt>& rownrs)
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
Array<Int64> TableExprNodeArrayColumnInt::getArrayInt
                                                  (const TableExprId& id)
{
    Array<Int> arr = col_p (id.rownr());
    Array<Int64> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<Int64> TableExprNodeArrayColumnInt::getSliceInt
                                                  (const TableExprId& id,
						   const Slicer& index)
{
    Array<Int> arr = col_p.getSlice (id.rownr(), index);
    Array<Int64> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<Int> TableExprNodeArrayColumnInt::getElemColumnInt
(const Vector<uInt>& rownrs, const Slicer& index)
{
    return col_p.getColumnCells (rownrs, index);
}

TableExprNodeArrayColumnuInt::TableExprNodeArrayColumnuInt
                                           (const TableColumn& col,
					    const Table& table)
: TableExprNodeArrayColumn (col, table),
  col_p                    (col)
{}
TableExprNodeArrayColumnuInt::~TableExprNodeArrayColumnuInt()
{}

void TableExprNodeArrayColumnuInt::applySelection (const Vector<uInt>& rownrs)
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
Array<Int64> TableExprNodeArrayColumnuInt::getArrayInt
                                                   (const TableExprId& id)
{
    Array<uInt> arr = col_p (id.rownr());
    Array<Int64> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<Int64> TableExprNodeArrayColumnuInt::getSliceInt
                                                   (const TableExprId& id,
						    const Slicer& index)
{
    Array<uInt> arr = col_p.getSlice (id.rownr(), index);
    Array<Int64> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<uInt> TableExprNodeArrayColumnuInt::getElemColumnuInt
(const Vector<uInt>& rownrs, const Slicer& index)
{
    return col_p.getColumnCells (rownrs, index);
}

TableExprNodeArrayColumnFloat::TableExprNodeArrayColumnFloat
                                           (const TableColumn& col,
					    const Table& table)
: TableExprNodeArrayColumn (col, table),
  col_p                    (col)
{}
TableExprNodeArrayColumnFloat::~TableExprNodeArrayColumnFloat()
{}

void TableExprNodeArrayColumnFloat::applySelection (const Vector<uInt>& rownrs)
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
Array<Double> TableExprNodeArrayColumnFloat::getArrayDouble
                                                    (const TableExprId& id)
{
    Array<Float> arr = col_p (id.rownr());
    Array<Double> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<Double> TableExprNodeArrayColumnFloat::getSliceDouble
                                                    (const TableExprId& id,
						     const Slicer& index)
{
    Array<Float> arr = col_p.getSlice (id.rownr(), index);
    Array<Double> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<Float> TableExprNodeArrayColumnFloat::getElemColumnFloat
(const Vector<uInt>& rownrs, const Slicer& index)
{
    return col_p.getColumnCells (rownrs, index);
}

TableExprNodeArrayColumnDouble::TableExprNodeArrayColumnDouble
                                           (const TableColumn& col,
					    const Table& table)
: TableExprNodeArrayColumn (col, table),
  col_p                    (col)
{}
TableExprNodeArrayColumnDouble::~TableExprNodeArrayColumnDouble()
{}

void TableExprNodeArrayColumnDouble::applySelection (const Vector<uInt>& rownrs)
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
Array<Double> TableExprNodeArrayColumnDouble::getArrayDouble
                                                     (const TableExprId& id)
{
    return col_p (id.rownr());
}
Array<Double> TableExprNodeArrayColumnDouble::getSliceDouble
                                                     (const TableExprId& id,
						      const Slicer& index)
{
    return col_p.getSlice (id.rownr(), index);
}
Array<Double> TableExprNodeArrayColumnDouble::getElemColumnDouble
(const Vector<uInt>& rownrs, const Slicer& index)
{
    return col_p.getColumnCells (rownrs, index);
}

TableExprNodeArrayColumnComplex::TableExprNodeArrayColumnComplex
                                           (const TableColumn& col,
					    const Table& table)
: TableExprNodeArrayColumn (col, table),
  col_p                    (col)
{}
TableExprNodeArrayColumnComplex::~TableExprNodeArrayColumnComplex()
{}

void TableExprNodeArrayColumnComplex::applySelection (const Vector<uInt>& rownrs)
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
Array<DComplex> TableExprNodeArrayColumnComplex::getArrayDComplex
                                                     (const TableExprId& id)
{
    Array<Complex> arr = col_p (id.rownr());
    Array<DComplex> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<DComplex> TableExprNodeArrayColumnComplex::getSliceDComplex
                                                     (const TableExprId& id,
						      const Slicer& index)
{
    Array<Complex> arr = col_p.getSlice (id.rownr(), index);
    Array<DComplex> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<Complex> TableExprNodeArrayColumnComplex::getElemColumnComplex
(const Vector<uInt>& rownrs, const Slicer& index)
{
    return col_p.getColumnCells (rownrs, index);
}

TableExprNodeArrayColumnDComplex::TableExprNodeArrayColumnDComplex
                                           (const TableColumn& col,
					    const Table& table)
: TableExprNodeArrayColumn (col, table),
  col_p                    (col)
{}
TableExprNodeArrayColumnDComplex::~TableExprNodeArrayColumnDComplex()
{}

void TableExprNodeArrayColumnDComplex::applySelection (const Vector<uInt>& rownrs)
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
Array<DComplex> TableExprNodeArrayColumnDComplex::getArrayDComplex
                                                     (const TableExprId& id)
{
    return col_p (id.rownr());
}
Array<DComplex> TableExprNodeArrayColumnDComplex::getSliceDComplex
                                                     (const TableExprId& id,
						      const Slicer& index)
{
    return col_p.getSlice (id.rownr(), index);
}
Array<DComplex> TableExprNodeArrayColumnDComplex::getElemColumnDComplex
(const Vector<uInt>& rownrs, const Slicer& index)
{
    return col_p.getColumnCells (rownrs, index);
}

TableExprNodeArrayColumnString::TableExprNodeArrayColumnString
                                           (const TableColumn& col,
					    const Table& table)
: TableExprNodeArrayColumn (col, table),
  col_p                    (col)
{}
TableExprNodeArrayColumnString::~TableExprNodeArrayColumnString()
{}

void TableExprNodeArrayColumnString::applySelection (const Vector<uInt>& rownrs)
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
Array<String> TableExprNodeArrayColumnString::getArrayString
                                                     (const TableExprId& id)
{
    return col_p (id.rownr());
}
Array<String> TableExprNodeArrayColumnString::getSliceString
                                                     (const TableExprId& id,
						      const Slicer& index)
{
    return col_p.getSlice (id.rownr(), index);
}
Array<String> TableExprNodeArrayColumnString::getElemColumnString
(const Vector<uInt>& rownrs, const Slicer& index)
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
  endMinus_p         (style.origin()),
  isCOrder_p         (style.isCOrder()),
  isSingle_p         (True)
{
    if (style.isEndExcl()) ++endMinus_p;
    fillIndex (indices);
}

TableExprNodeIndex::~TableExprNodeIndex()
{}

void TableExprNodeIndex::checkIndexValues (const TableExprNodeRep* arrayNode)
{
    uInt i;
    Int ndim = arrayNode->ndim();
    uInt n = start_p.nelements();
    // Check against dimensionality (if fixed).
    if (ndim >= 0  &&  ndim != Int(n)) {
	throw (TableInvExpr ("#indices mismatches array dimensionality"));
    }
    // Check start and increment values.
    for (i=0; i<n; i++) {
	if (!varIndex_p[3*i]) {
	    if (start_p(i) < 0) {
		throw (TableInvExpr ("index value before array origin"));
	    }
	}
	if (!varIndex_p[3*i + 2]) {
	    if (incr_p(i) < 0) {
		throw (TableInvExpr ("index increment value is negative"));
	    }
	}
    }
    // Check against array shape (if fixed).
    IPosition shape = arrayNode->shape();
    if (shape.nelements() > 0) {
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
    uInt n = varIndex_p.nelements();
    uInt i = 0;
    uInt j = 0;
    while (j < n) {
	if (varIndex_p[j]) {
	    start_p(i) = operands_p[j]->getInt(id) - origin_p;
	}
	j++;
	if (varIndex_p[j]) {
	    if (operands_p[j] == 0) {
		end_p(i) = start_p(i);
	    }else{
		Int64 val = operands_p[j]->getInt (id);
		if (val < 0) {
		    end_p = Slicer::MimicSource;
		}else{
		    end_p(i) = val - endMinus_p;
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
    TableExprNodeRep* rep;
    // Copy block of start, end, and increment.
    // Determine if single element subscripting is done.
    // That is true if all starts are given and no end and increment values.
    // Check if all indices have data type Int and are scalars.
    uInt n = indices.nelements();
    operands_p.resize (3 * n);
    operands_p.set (static_cast<TableExprNodeRep*>(0));
    uInt j = 0;
    for (uInt i=0; i<n; i++) {
        uInt inx = (isCOrder_p  ?  n-i-1 : i);
	rep = const_cast<TableExprNodeRep*>(indices[inx].start());
	if (rep != 0) {
	    operands_p[j] = rep->link();
	}else{
	    isSingle_p = False;
	}
	j++;
	rep = const_cast<TableExprNodeRep*>(indices[inx].end());
	if (rep != 0) {
	    operands_p[j] = rep->link();
	    isSingle_p = False;
	}
	j++;
	rep = const_cast<TableExprNodeRep*>(indices[inx].increment());
	if (rep != 0) {
	    operands_p[j] = rep->link();
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
		throw (TableInvExpr ("Index value must an integer scalar"));
	    }
            TableExprNodeRep::checkAggrFuncs (operands_p[i]);
	}
    }
    convertConstIndex();
    if (isConstant()) {
	slicer_p = Slicer (start_p, end_p, incr_p, Slicer::endIsLast);
    }
}

void TableExprNodeIndex::convertConstIndex()
{
    TableExprNodeRep* rep;
    uInt n = operands_p.nelements() / 3;
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
		start_p(i) = rep->getInt(0) - origin_p;
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
		if (val < 0) {
		    end_p = Slicer::MimicSource;
		}else{
		    end_p(i) = val - endMinus_p;
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
TableExprNodeArrayPart::TableExprNodeArrayPart (TableExprNodeRep* arrayNode,
						TableExprNodeIndex* indexNode)
: TableExprNodeArray (arrayNode->dataType(), OtSlice),
  indexNode_p        (indexNode),
  colNode_p          (0)
{
    checkTablePtr (indexNode);
    checkTablePtr (arrayNode);
    fillExprType  (indexNode);
    fillExprType  (arrayNode);
    arrNode_p = dynamic_cast<TableExprNodeArray*>(arrayNode);
    AlwaysAssert (arrNode_p, AipsError);
    // If indexing a single element, the result is a scalar.
    if (indexNode->isSingle()) {
	vtype_p = VTScalar;
	ndim_p  = 0;
    } else if (indexNode->isConstant()) {
	// Otherwise if the index node is constant, it may be possible
	// to determine the resulting shape.
	const Slicer& slicer = indexNode->getSlicer(0);
	// If all slicer lengths are defined, that is the resulting shape.
	if (slicer.isFixed()) {
	    shape_p = slicer.length();
	    ndim_p  = shape_p.nelements();
	}else{
	    // If some are depending on array shape, the resulting
	    // shape can be determined if the array shape is fixed.
	    IPosition arrshp = arrayNode->shape();
	    if (arrshp.nelements() > 0) {
		IPosition blc,trc,inc;
		shape_p = slicer.inferShapeFromSource (arrshp, blc, trc, inc);
		ndim_p  = shape_p.nelements();
	    }
	}
    }
    if (indexNode->isConstant()) {
	// If the constant child is an ArrayColumn, things can be
	// improved in getColumnXXX.
	colNode_p = dynamic_cast<TableExprNodeArrayColumn*>(arrayNode);
    }
}

TableExprNodeArrayPart::~TableExprNodeArrayPart()
{}


void TableExprNodeArrayPart::show (ostream& os, uInt indent) const
{
    TableExprNodeRep::show (os, indent);
    os << "array: ";
    lnode_p->show (os, indent+2);
    os << "index: ";
    indexNode_p->show (os, indent+2);
}

Bool TableExprNodeArrayPart::getColumnDataType (DataType& dt) const
{
    //# Return data type of column if constant index.
    if (indexNode_p->isConstant()) {
	return lnode_p->getColumnDataType (dt);
    }
    return False;
}

//# Note that all following casts are perfectly safe.
Bool TableExprNodeArrayPart::getBool (const TableExprId& id)
{
    DebugAssert (valueType() == VTScalar, AipsError);
    return arrNode_p->getElemBool (id, indexNode_p->getSlicer(id));
}
Int64 TableExprNodeArrayPart::getInt (const TableExprId& id)
{
    DebugAssert (valueType() == VTScalar, AipsError);
    return arrNode_p->getElemInt (id, indexNode_p->getSlicer(id));
}
Double TableExprNodeArrayPart::getDouble (const TableExprId& id)
{
    DebugAssert (valueType() == VTScalar, AipsError);
    return arrNode_p->getElemDouble (id, indexNode_p->getSlicer(id));
}
DComplex TableExprNodeArrayPart::getDComplex (const TableExprId& id)
{
    DebugAssert (valueType() == VTScalar, AipsError);
    return arrNode_p->getElemDComplex (id, indexNode_p->getSlicer(id));
}
String TableExprNodeArrayPart::getString (const TableExprId& id)
{
    DebugAssert (valueType() == VTScalar, AipsError);
    return arrNode_p->getElemString (id, indexNode_p->getSlicer(id));
}
MVTime TableExprNodeArrayPart::getDate (const TableExprId& id)
{
    DebugAssert (valueType() == VTScalar, AipsError);
    return arrNode_p->getElemDate (id, indexNode_p->getSlicer(id));
}

Array<Bool> TableExprNodeArrayPart::getArrayBool (const TableExprId& id)
{
    DebugAssert (valueType() == VTArray, AipsError);
    return arrNode_p->getSliceBool (id, indexNode_p->getSlicer(id));
}
Array<Int64> TableExprNodeArrayPart::getArrayInt (const TableExprId& id)
{
    DebugAssert (valueType() == VTArray, AipsError);
    return arrNode_p->getSliceInt (id, indexNode_p->getSlicer(id));
}
Array<Double> TableExprNodeArrayPart::getArrayDouble (const TableExprId& id)
{
    DebugAssert (valueType() == VTArray, AipsError);
    return arrNode_p->getSliceDouble (id, indexNode_p->getSlicer(id));
}
Array<DComplex> TableExprNodeArrayPart::getArrayDComplex
                                                     (const TableExprId& id)
{
    DebugAssert (valueType() == VTArray, AipsError);
    return arrNode_p->getSliceDComplex (id, indexNode_p->getSlicer(id));
}
Array<String> TableExprNodeArrayPart::getArrayString (const TableExprId& id)
{
    DebugAssert (valueType() == VTArray, AipsError);
    return arrNode_p->getSliceString (id, indexNode_p->getSlicer(id));
}
Array<MVTime> TableExprNodeArrayPart::getArrayDate (const TableExprId& id)
{
    DebugAssert (valueType() == VTArray, AipsError);
    return arrNode_p->getSliceDate (id, indexNode_p->getSlicer(id));
}

Array<Bool> TableExprNodeArrayPart::getColumnBool (const Vector<uInt>& rownrs)
{
    if (colNode_p == 0) {
	return TableExprNodeRep::getColumnBool (rownrs);
    }
    return colNode_p->getElemColumnBool (rownrs, indexNode_p->getSlicer(0));
}
Array<uChar>    TableExprNodeArrayPart::getColumnuChar (const Vector<uInt>& rownrs)
{
    if (colNode_p == 0) {
	return TableExprNodeRep::getColumnuChar (rownrs);
    }
    return colNode_p->getElemColumnuChar (rownrs, indexNode_p->getSlicer(0));
}
Array<Short>    TableExprNodeArrayPart::getColumnShort (const Vector<uInt>& rownrs)
{
    if (colNode_p == 0) {
	return TableExprNodeRep::getColumnShort (rownrs);
    }
    return colNode_p->getElemColumnShort (rownrs, indexNode_p->getSlicer(0));
}
Array<uShort>   TableExprNodeArrayPart::getColumnuShort (const Vector<uInt>& rownrs)
{
    if (colNode_p == 0) {
	return TableExprNodeRep::getColumnuShort (rownrs);
    }
    return colNode_p->getElemColumnuShort (rownrs, indexNode_p->getSlicer(0));
}
Array<Int>      TableExprNodeArrayPart::getColumnInt (const Vector<uInt>& rownrs)
{
    if (colNode_p == 0) {
	return TableExprNodeRep::getColumnInt (rownrs);
    }
    return colNode_p->getElemColumnInt (rownrs, indexNode_p->getSlicer(0));
}
Array<uInt>     TableExprNodeArrayPart::getColumnuInt (const Vector<uInt>& rownrs)
{
    if (colNode_p == 0) {
	return TableExprNodeRep::getColumnuInt (rownrs);
    }
    return colNode_p->getElemColumnuInt (rownrs, indexNode_p->getSlicer(0));
}
Array<Float>    TableExprNodeArrayPart::getColumnFloat (const Vector<uInt>& rownrs)
{
    if (colNode_p == 0) {
	return TableExprNodeRep::getColumnFloat (rownrs);
    }
    return colNode_p->getElemColumnFloat (rownrs, indexNode_p->getSlicer(0));
}
Array<Double>   TableExprNodeArrayPart::getColumnDouble (const Vector<uInt>& rownrs)
{
    if (colNode_p == 0) {
	return TableExprNodeRep::getColumnDouble (rownrs);
    }
    return colNode_p->getElemColumnDouble (rownrs, indexNode_p->getSlicer(0));
}
Array<Complex>  TableExprNodeArrayPart::getColumnComplex (const Vector<uInt>& rownrs)
{
    if (colNode_p == 0) {
	return TableExprNodeRep::getColumnComplex (rownrs);
    }
    return colNode_p->getElemColumnComplex (rownrs, indexNode_p->getSlicer(0));
}
Array<DComplex> TableExprNodeArrayPart::getColumnDComplex (const Vector<uInt>& rownrs)
{
    if (colNode_p == 0) {
	return TableExprNodeRep::getColumnDComplex (rownrs);
    }
    return colNode_p->getElemColumnDComplex (rownrs, indexNode_p->getSlicer(0));
}
Array<String>   TableExprNodeArrayPart::getColumnString (const Vector<uInt>& rownrs)
{
    if (colNode_p == 0) {
	return TableExprNodeRep::getColumnString (rownrs);
    }
    return colNode_p->getElemColumnString (rownrs, indexNode_p->getSlicer(0));
}

} //# NAMESPACE CASACORE - END

