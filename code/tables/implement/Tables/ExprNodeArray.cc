//# ExprNodeArray.cc: Classes representing an array in table select expression
//# Copyright (C) 1997,1999,2000
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

#include <aips/Tables/ExprNode.h>
#include <aips/Tables/ExprNodeArray.h>
#include <aips/Tables/ExprNodeSet.h>
#include <aips/Tables/TableError.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/Slicer.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>


TableExprNodeArray::TableExprNodeArray (NodeDataType dtype, OperType otype)
: TableExprNodeBinary (dtype, VTArray, otype, 0)
{
    ndim_p = -1;
}
TableExprNodeArray::TableExprNodeArray (const TableExprNodeRep& node,
					NodeDataType dtype, OperType otype)
: TableExprNodeBinary (dtype, node, otype)
{}
TableExprNodeArray::TableExprNodeArray (NodeDataType dtype, OperType otype,
					const IPosition& shape)
: TableExprNodeBinary (dtype, VTArray, otype, 0)
{
    shape_p = shape;
    ndim_p  = shape.nelements();
    if (ndim_p == 0) {
	ndim_p = -1;
    }
}

TableExprNodeArray::~TableExprNodeArray()
{}

const IPosition& TableExprNodeArray::getShape (const TableExprId& id)
{
    varShape_p.resize (0);
    switch (dataType()) {
    case NTBool:
	varShape_p = getArrayBool(id).shape();
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

Array<DComplex> TableExprNodeArray::getArrayDComplex (const TableExprId& id)
{
    Array<Double> arr = getArrayDouble (id);
    Array<DComplex> result (arr.shape());
    convertArray (result, arr);
    return result;
}

Bool TableExprNodeArray::getElemBool (const TableExprId& id,
				      const Slicer& slicer)
{
    return getArrayBool (id) (slicer.start());
}
Double TableExprNodeArray::getElemDouble (const TableExprId& id,
					  const Slicer& slicer)
{
    return getArrayDouble (id) (slicer.start());
}
DComplex TableExprNodeArray::getElemDComplex (const TableExprId& id,
					      const Slicer& slicer)
{
    return getArrayDComplex (id) (slicer.start());
}
String TableExprNodeArray::getElemString (const TableExprId& id,
					  const Slicer& slicer)
{
    return getArrayString (id) (slicer.start());
}
MVTime TableExprNodeArray::getElemDate (const TableExprId& id,
					const Slicer& slicer)
{
    return getArrayDate (id) (slicer.start());
}

Array<Bool> TableExprNodeArray::getSliceBool (const TableExprId& id,
					      const Slicer& slicer)
{
    Array<Bool> arr = getArrayBool (id);
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

Array<Bool>     TableExprNodeArray::getElemColumnBool (const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnBool(Slicer) not implemented)");
    return Array<Bool>();
}
Array<uChar>    TableExprNodeArray::getElemColumnuChar (const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnuChar(Slicer) not implemented)");
    return Array<uChar>();
}
Array<Short>    TableExprNodeArray::getElemColumnShort (const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnShort(Slicer) not implemented)");
    return Array<Short>();
}
Array<uShort>   TableExprNodeArray::getElemColumnuShort (const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnuShort(Slicer) not implemented)");
    return Array<uShort>();
}
Array<Int>      TableExprNodeArray::getElemColumnInt (const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnInt(Slicer) not implemented)");
    return Array<Int>();
}
Array<uInt>     TableExprNodeArray::getElemColumnuInt (const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnuInt(Slicer) not implemented)");
    return Array<uInt>();
}
Array<Float>    TableExprNodeArray::getElemColumnFloat (const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnFloat(Slicer) not implemented)");
    return Array<Float>();
}
Array<Double>   TableExprNodeArray::getElemColumnDouble (const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnDouble(Slicer) not implemented)");
    return Array<Double>();
}
Array<Complex>  TableExprNodeArray::getElemColumnComplex (const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnComplex(Slicer) not implemented)");
    return Array<Complex>();
}
Array<DComplex> TableExprNodeArray::getElemColumnDComplex (const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnDComplex(Slicer) not implemented)");
    return Array<DComplex>();
}
Array<String>   TableExprNodeArray::getElemColumnString (const Slicer&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnString(Slicer) not implemented)");
    return Array<String>();
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
                                           (const ROTableColumn& tablecol,
					    const BaseTable* tabptr)
: TableExprNodeArray (NTNumeric, OtColumn),
  tabCol_p           (tablecol)
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
    case TpFloat:
    case TpDouble:
	dtype_p = NTDouble;
	break;
    default:
	throw (TableInvExpr (tabCol_p.columnDesc().name(),
			     "unknown data type"));
    }
    baseTabPtr_p = tabptr;
    exprtype_p = Variable;
    // Set the fixed shape and dimensionality (if known).
    ndim_p = tabCol_p.ndimColumn();
    if (ndim_p == 0) {
	ndim_p = -1;                         // unknown dimensionality
    }
    shape_p = tabCol_p.shapeColumn();
}

TableExprNodeArrayColumn::~TableExprNodeArrayColumn()
{}

void TableExprNodeArrayColumn::replaceTablePtr (const Table& table,
						const BaseTable* baseTablePtr)
{
    String name = tabCol_p.columnDesc().name();
    tabCol_p.reference (ROTableColumn (table, name));
    baseTabPtr_p = baseTablePtr;
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
                                           (const ROTableColumn& col,
					    const BaseTable* tabptr)
: TableExprNodeArrayColumn (col, tabptr),
  col_p                    (col)
{}
TableExprNodeArrayColumnBool::~TableExprNodeArrayColumnBool()
{}
void TableExprNodeArrayColumnBool::replaceTablePtr (const Table& table,
					       const BaseTable* baseTablePtr)
{
    // First replace in base class and use that ROTableColumn.
    TableExprNodeArrayColumn::replaceTablePtr (table, baseTablePtr);
    col_p.reference (ROArrayColumn<Bool> (tabCol_p));
}

Bool TableExprNodeArrayColumnBool::getElemBool (const TableExprId& id,
						const Slicer& index)
{
    Array<Bool> arr = col_p.getSlice (id.rownr(), index);
    Bool deleteIt;
    const Bool* f = arr.getStorage (deleteIt);
    Bool val = *f;
    arr.freeStorage (f, deleteIt);
    return val;
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
                                                       (const Slicer& index)
{
    return col_p.getColumn (index);
}

TableExprNodeArrayColumnuChar::TableExprNodeArrayColumnuChar
                                           (const ROTableColumn& col,
					    const BaseTable* tabptr)
: TableExprNodeArrayColumn (col, tabptr),
  col_p                    (col)
{}
TableExprNodeArrayColumnuChar::~TableExprNodeArrayColumnuChar()
{}
void TableExprNodeArrayColumnuChar::replaceTablePtr (const Table& table,
					       const BaseTable* baseTablePtr)
{
    // First replace in base class and use that ROTableColumn.
    TableExprNodeArrayColumn::replaceTablePtr (table, baseTablePtr);
    col_p.reference (ROArrayColumn<uChar> (tabCol_p));
}

Double TableExprNodeArrayColumnuChar::getElemDouble (const TableExprId& id,
						     const Slicer& index)
{
    Array<uChar> arr = col_p.getSlice (id.rownr(), index);
    Bool deleteIt;
    const uChar* f = arr.getStorage (deleteIt);
    uChar val = *f;
    arr.freeStorage (f, deleteIt);
    return val;
}
Array<Double> TableExprNodeArrayColumnuChar::getArrayDouble
                                                    (const TableExprId& id)
{
    Array<uChar> arr = col_p (id.rownr());
    Array<Double> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<Double> TableExprNodeArrayColumnuChar::getSliceDouble
                                                    (const TableExprId& id,
						     const Slicer& index)
{
    Array<uChar> arr = col_p.getSlice (id.rownr(), index);
    Array<Double> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<uChar> TableExprNodeArrayColumnuChar::getElemColumnuChar
                                                    (const Slicer& index)
{
    return col_p.getColumn (index);
}

TableExprNodeArrayColumnShort::TableExprNodeArrayColumnShort
                                           (const ROTableColumn& col,
					    const BaseTable* tabptr)
: TableExprNodeArrayColumn (col, tabptr),
  col_p                    (col)
{}
TableExprNodeArrayColumnShort::~TableExprNodeArrayColumnShort()
{}
void TableExprNodeArrayColumnShort::replaceTablePtr (const Table& table,
					       const BaseTable* baseTablePtr)
{
    // First replace in base class and use that ROTableColumn.
    TableExprNodeArrayColumn::replaceTablePtr (table, baseTablePtr);
    col_p.reference (ROArrayColumn<Short> (tabCol_p));
}

Double TableExprNodeArrayColumnShort::getElemDouble (const TableExprId& id,
						     const Slicer& index)
{
    Array<Short> arr = col_p.getSlice (id.rownr(), index);
    Bool deleteIt;
    const Short* f = arr.getStorage (deleteIt);
    Short val = *f;
    arr.freeStorage (f, deleteIt);
    return val;
}
Array<Double> TableExprNodeArrayColumnShort::getArrayDouble
                                                    (const TableExprId& id)
{
    Array<Short> arr = col_p (id.rownr());
    Array<Double> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<Double> TableExprNodeArrayColumnShort::getSliceDouble
                                                    (const TableExprId& id,
						     const Slicer& index)
{
    Array<Short> arr = col_p.getSlice (id.rownr(), index);
    Array<Double> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<Short> TableExprNodeArrayColumnShort::getElemColumnShort
                                                    (const Slicer& index)
{
    return col_p.getColumn (index);
}

TableExprNodeArrayColumnuShort::TableExprNodeArrayColumnuShort
                                           (const ROTableColumn& col,
					    const BaseTable* tabptr)
: TableExprNodeArrayColumn (col, tabptr),
  col_p                    (col)
{}
TableExprNodeArrayColumnuShort::~TableExprNodeArrayColumnuShort()
{}
void TableExprNodeArrayColumnuShort::replaceTablePtr
                                              (const Table& table,
					       const BaseTable* baseTablePtr)
{
    // First replace in base class and use that ROTableColumn.
    TableExprNodeArrayColumn::replaceTablePtr (table, baseTablePtr);
    col_p.reference (ROArrayColumn<uShort> (tabCol_p));
}

Double TableExprNodeArrayColumnuShort::getElemDouble (const TableExprId& id,
						      const Slicer& index)
{
    Array<uShort> arr = col_p.getSlice (id.rownr(), index);
    Bool deleteIt;
    const uShort* f = arr.getStorage (deleteIt);
    uShort val = *f;
    arr.freeStorage (f, deleteIt);
    return val;
}
Array<Double> TableExprNodeArrayColumnuShort::getArrayDouble
                                                     (const TableExprId& id)
{
    Array<uShort> arr = col_p (id.rownr());
    Array<Double> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<Double> TableExprNodeArrayColumnuShort::getSliceDouble
                                                     (const TableExprId& id,
						      const Slicer& index)
{
    Array<uShort> arr = col_p.getSlice (id.rownr(), index);
    Array<Double> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<uShort> TableExprNodeArrayColumnuShort::getElemColumnuShort
                                                     (const Slicer& index)
{
    return col_p.getColumn (index);
}

TableExprNodeArrayColumnInt::TableExprNodeArrayColumnInt
                                           (const ROTableColumn& col,
					    const BaseTable* tabptr)
: TableExprNodeArrayColumn (col, tabptr),
  col_p                    (col)
{}
TableExprNodeArrayColumnInt::~TableExprNodeArrayColumnInt()
{}
void TableExprNodeArrayColumnInt::replaceTablePtr
                                              (const Table& table,
					       const BaseTable* baseTablePtr)
{
    // First replace in base class and use that ROTableColumn.
    TableExprNodeArrayColumn::replaceTablePtr (table, baseTablePtr);
    col_p.reference (ROArrayColumn<Int> (tabCol_p));
}

Double TableExprNodeArrayColumnInt::getElemDouble (const TableExprId& id,
						   const Slicer& index)
{
    Array<Int> arr = col_p.getSlice (id.rownr(), index);
    Bool deleteIt;
    const Int* f = arr.getStorage (deleteIt);
    Int val = *f;
    arr.freeStorage (f, deleteIt);
    return val;
}
Array<Double> TableExprNodeArrayColumnInt::getArrayDouble
                                                  (const TableExprId& id)
{
    Array<Int> arr = col_p (id.rownr());
    Array<Double> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<Double> TableExprNodeArrayColumnInt::getSliceDouble
                                                  (const TableExprId& id,
						   const Slicer& index)
{
    Array<Int> arr = col_p.getSlice (id.rownr(), index);
    Array<Double> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<Int> TableExprNodeArrayColumnInt::getElemColumnInt (const Slicer& index)
{
    return col_p.getColumn (index);
}

TableExprNodeArrayColumnuInt::TableExprNodeArrayColumnuInt
                                           (const ROTableColumn& col,
					    const BaseTable* tabptr)
: TableExprNodeArrayColumn (col, tabptr),
  col_p                    (col)
{}
TableExprNodeArrayColumnuInt::~TableExprNodeArrayColumnuInt()
{}
void TableExprNodeArrayColumnuInt::replaceTablePtr
                                              (const Table& table,
					       const BaseTable* baseTablePtr)
{
    // First replace in base class and use that ROTableColumn.
    TableExprNodeArrayColumn::replaceTablePtr (table, baseTablePtr);
    col_p.reference (ROArrayColumn<uInt> (tabCol_p));
}

Double TableExprNodeArrayColumnuInt::getElemDouble (const TableExprId& id,
						    const Slicer& index)
{
    Array<uInt> arr = col_p.getSlice (id.rownr(), index);
    Bool deleteIt;
    const uInt* f = arr.getStorage (deleteIt);
    uInt val = *f;
    arr.freeStorage (f, deleteIt);
    return val;
}
Array<Double> TableExprNodeArrayColumnuInt::getArrayDouble
                                                   (const TableExprId& id)
{
    Array<uInt> arr = col_p (id.rownr());
    Array<Double> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<Double> TableExprNodeArrayColumnuInt::getSliceDouble
                                                   (const TableExprId& id,
						    const Slicer& index)
{
    Array<uInt> arr = col_p.getSlice (id.rownr(), index);
    Array<Double> out (arr.shape());
    convertArray (out, arr);
    return out;
}
Array<uInt> TableExprNodeArrayColumnuInt::getElemColumnuInt
                                                   (const Slicer& index)
{
    return col_p.getColumn (index);
}

TableExprNodeArrayColumnFloat::TableExprNodeArrayColumnFloat
                                           (const ROTableColumn& col,
					    const BaseTable* tabptr)
: TableExprNodeArrayColumn (col, tabptr),
  col_p                    (col)
{}
TableExprNodeArrayColumnFloat::~TableExprNodeArrayColumnFloat()
{}
void TableExprNodeArrayColumnFloat::replaceTablePtr
                                              (const Table& table,
					       const BaseTable* baseTablePtr)
{
    // First replace in base class and use that ROTableColumn.
    TableExprNodeArrayColumn::replaceTablePtr (table, baseTablePtr);
    col_p.reference (ROArrayColumn<Float> (tabCol_p));
}

Double TableExprNodeArrayColumnFloat::getElemDouble (const TableExprId& id,
						     const Slicer& index)
{
    Array<Float> arr = col_p.getSlice (id.rownr(), index);
    Bool deleteIt;
    const Float* f = arr.getStorage (deleteIt);
    Float val = *f;
    arr.freeStorage (f, deleteIt);
    return val;
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
                                                    (const Slicer& index)
{
    return col_p.getColumn (index);
}

TableExprNodeArrayColumnDouble::TableExprNodeArrayColumnDouble
                                           (const ROTableColumn& col,
					    const BaseTable* tabptr)
: TableExprNodeArrayColumn (col, tabptr),
  col_p                    (col)
{}
TableExprNodeArrayColumnDouble::~TableExprNodeArrayColumnDouble()
{}
void TableExprNodeArrayColumnDouble::replaceTablePtr
                                              (const Table& table,
					       const BaseTable* baseTablePtr)
{
    // First replace in base class and use that ROTableColumn.
    TableExprNodeArrayColumn::replaceTablePtr (table, baseTablePtr);
    col_p.reference (ROArrayColumn<Double> (tabCol_p));
}

Double TableExprNodeArrayColumnDouble::getElemDouble (const TableExprId& id,
						      const Slicer& index)
{
    Array<Double> arr = col_p.getSlice (id.rownr(), index);
    Bool deleteIt;
    const Double* f = arr.getStorage (deleteIt);
    Double val = *f;
    arr.freeStorage (f, deleteIt);
    return val;
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
                                                     (const Slicer& index)
{
    return col_p.getColumn (index);
}

TableExprNodeArrayColumnComplex::TableExprNodeArrayColumnComplex
                                           (const ROTableColumn& col,
					    const BaseTable* tabptr)
: TableExprNodeArrayColumn (col, tabptr),
  col_p                    (col)
{}
TableExprNodeArrayColumnComplex::~TableExprNodeArrayColumnComplex()
{}
void TableExprNodeArrayColumnComplex::replaceTablePtr
                                              (const Table& table,
					       const BaseTable* baseTablePtr)
{
    // First replace in base class and use that ROTableColumn.
    TableExprNodeArrayColumn::replaceTablePtr (table, baseTablePtr);
    col_p.reference (ROArrayColumn<Complex> (tabCol_p));
}

DComplex TableExprNodeArrayColumnComplex::getElemDComplex
                                                     (const TableExprId& id,
						      const Slicer& index)
{
    Array<Complex> arr = col_p.getSlice (id.rownr(), index);
    Bool deleteIt;
    const Complex* f = arr.getStorage (deleteIt);
    DComplex val;
    val = *f;
    arr.freeStorage (f, deleteIt);
    return val;
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
                                                     (const Slicer& index)
{
    return col_p.getColumn (index);
}

TableExprNodeArrayColumnDComplex::TableExprNodeArrayColumnDComplex
                                           (const ROTableColumn& col,
					    const BaseTable* tabptr)
: TableExprNodeArrayColumn (col, tabptr),
  col_p                    (col)
{}
TableExprNodeArrayColumnDComplex::~TableExprNodeArrayColumnDComplex()
{}
void TableExprNodeArrayColumnDComplex::replaceTablePtr
                                              (const Table& table,
		      			       const BaseTable* baseTablePtr)
{
    // First replace in base class and use that ROTableColumn.
    TableExprNodeArrayColumn::replaceTablePtr (table, baseTablePtr);
    col_p.reference (ROArrayColumn<DComplex> (tabCol_p));
}

DComplex TableExprNodeArrayColumnDComplex::getElemDComplex
                                                     (const TableExprId& id,
						      const Slicer& index)
{
    Array<DComplex> arr = col_p.getSlice (id.rownr(), index);
    Bool deleteIt;
    const DComplex* f = arr.getStorage (deleteIt);
    DComplex val = *f;
    arr.freeStorage (f, deleteIt);
    return val;
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
                                                      (const Slicer& index)
{
    return col_p.getColumn (index);
}

TableExprNodeArrayColumnString::TableExprNodeArrayColumnString
                                           (const ROTableColumn& col,
					    const BaseTable* tabptr)
: TableExprNodeArrayColumn (col, tabptr),
  col_p                    (col)
{}
TableExprNodeArrayColumnString::~TableExprNodeArrayColumnString()
{}
void TableExprNodeArrayColumnString::replaceTablePtr
                                              (const Table& table,
					       const BaseTable* baseTablePtr)
{
    // First replace in base class and use that ROTableColumn.
    TableExprNodeArrayColumn::replaceTablePtr (table, baseTablePtr);
    col_p.reference (ROArrayColumn<String> (tabCol_p));
}

String TableExprNodeArrayColumnString::getElemString (const TableExprId& id,
						      const Slicer& index)
{
    Array<String> arr = col_p.getSlice (id.rownr(), index);
    Bool deleteIt;
    const String* f = arr.getStorage (deleteIt);
    String val = *f;
    arr.freeStorage (f, deleteIt);
    return val;
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
                                                     (const Slicer& index)
{
    return col_p.getColumn (index);
}




// ----------------------------
// TableExprNodeIndex functions
// ----------------------------

TableExprNodeIndex::TableExprNodeIndex (const TableExprNodeSet& indices,
					uInt origin)
: TableExprNodeMulti (NTDouble, VTIndex, OtColumn, indices),
  origin_p           (origin),
  isSingle_p         (True)
{
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
	    start_p(i) = Int(operands_p[j]->getDouble(id) + 0.5) - origin_p;
	}
	j++;
	if (varIndex_p[j]) {
	    if (operands_p[j] == 0) {
		end_p(i) = start_p(i);
	    }else{
		Double val = operands_p[j]->getDouble (id);
		if (val < 0) {
		    end_p = Slicer::MimicSource;
		}else{
		    end_p(i) = Int(val + 0.5) - origin_p;
		}
	    }
	}
	j++;
	if (varIndex_p[j]) {
	    incr_p(i) = Int(operands_p[j]->getDouble(id) + 0.5);
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
    // Check that the set contains discrete values.
    if (! indices.isDiscrete()) {
	throw (TableInvExpr ("Index values must be discrete (with possible :"));
    }
    TableExprNodeRep* rep;
    // Copy block of start, end, and increment.
    // Determine if single element subscripting is done.
    // That is true if all starts are given and no end and increment values.
    // Check if all indices have data type Double and are scalars.
    uInt n = indices.nelements();
    operands_p.resize (3 * n);
    operands_p.set (static_cast<TableExprNodeRep*>(0));
    uInt i;
    uInt j = 0;
    for (i=0; i<n; i++) {
	rep = const_cast<TableExprNodeRep*>(indices[i].start());
	if (rep != 0) {
	    operands_p[j] = rep->link();
	}else{
	    isSingle_p = False;
	}
	j++;
	rep = const_cast<TableExprNodeRep*>(indices[i].end());
	if (rep != 0) {
	    operands_p[j] = rep->link();
	    isSingle_p = False;
	}
	j++;
	rep = const_cast<TableExprNodeRep*>(indices[i].increment());
	if (rep != 0) {
	    operands_p[j] = rep->link();
	    isSingle_p = False;
	}
	j++;
    }
    // Check if all indices have data type Double and are scalars.
    for (i=0; i<j; i++) {
	if (operands_p[i] != 0) {
	    if (operands_p[i]->dataType()  != NTDouble
	    ||  operands_p[i]->valueType() != VTScalar) {
		throw (TableInvExpr ("Index value must a numeric scalar"));
	    }
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
		start_p(i) = Int(rep->getDouble(0) + 0.5) - origin_p;
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
		Double val = rep->getDouble(0);
		if (val < 0) {
		    end_p = Slicer::MimicSource;
		}else{
		    end_p(i) = Int(val + 0.5) - origin_p;
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
		incr_p(i) = Int(rep->getDouble(0) + 0.5);
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
  indexNode_p        (indexNode)
{
    checkTablePtr (indexNode);
    checkTablePtr (arrayNode);
    fillExprType  (indexNode);
    fillExprType  (arrayNode);
    // When indexing a single element, the result is a scalar.
    if (indexNode->isSingle()) {
	vtype_p = VTScalar;
	ndim_p  = 0;
    } else if (indexNode->isConstant()) {
	// Otherwise when the index node is constant, it may be possible
	// to determine the resulting shape.
	const Slicer& slicer = indexNode->getSlicer(0);
	// When all slicer lengths are defined, that is the resulting shape.
	if (slicer.isFixed()) {
	    shape_p = slicer.length();
	    ndim_p  = shape_p.nelements();
	}else{
	    // When some are depending on array shape, the resulting
	    // shape can be determined if the array shape is fixed.
	    IPosition arrshp = arrayNode->shape();
	    if (arrshp.nelements() > 0) {
		IPosition blc,trc,inc;
		shape_p = slicer.inferShapeFromSource (arrshp, blc, trc, inc);
		ndim_p  = shape_p.nelements();
	    }
	}
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
    //# Return data type of column when constant index.
    if (indexNode_p->isConstant()) {
	return lnode_p->getColumnDataType (dt);
    }
    return False;
}

//# Note that all following casts are perfectly safe.
Bool TableExprNodeArrayPart::getBool (const TableExprId& id)
{
    DebugAssert (valueType() == VTScalar, AipsError);
    return dynamic_cast<TableExprNodeArray*>(lnode_p)->getElemBool
                                    (id, indexNode_p->getSlicer(id));
}
Double TableExprNodeArrayPart::getDouble (const TableExprId& id)
{
    DebugAssert (valueType() == VTScalar, AipsError);
    return dynamic_cast<TableExprNodeArray*>(lnode_p)->getElemDouble
                                    (id, indexNode_p->getSlicer(id));
}
DComplex TableExprNodeArrayPart::getDComplex (const TableExprId& id)
{
    DebugAssert (valueType() == VTScalar, AipsError);
    return dynamic_cast<TableExprNodeArray*>(lnode_p)->getElemDComplex
                                    (id, indexNode_p->getSlicer(id));
}
String TableExprNodeArrayPart::getString (const TableExprId& id)
{
    DebugAssert (valueType() == VTScalar, AipsError);
    return dynamic_cast<TableExprNodeArray*>(lnode_p)->getElemString
                                    (id, indexNode_p->getSlicer(id));
}
MVTime TableExprNodeArrayPart::getDate (const TableExprId& id)
{
    DebugAssert (valueType() == VTScalar, AipsError);
    return dynamic_cast<TableExprNodeArray*>(lnode_p)->getElemDate
                                    (id, indexNode_p->getSlicer(id));
}

Array<Bool> TableExprNodeArrayPart::getArrayBool (const TableExprId& id)
{
    DebugAssert (valueType() == VTArray, AipsError);
    return dynamic_cast<TableExprNodeArray*>(lnode_p)->getSliceBool
                                    (id, indexNode_p->getSlicer(id));
}
Array<Double> TableExprNodeArrayPart::getArrayDouble (const TableExprId& id)
{
    DebugAssert (valueType() == VTArray, AipsError);
    return dynamic_cast<TableExprNodeArray*>(lnode_p)->getSliceDouble
                                    (id, indexNode_p->getSlicer(id));
}
Array<DComplex> TableExprNodeArrayPart::getArrayDComplex
                                                     (const TableExprId& id)
{
    DebugAssert (valueType() == VTArray, AipsError);
    return dynamic_cast<TableExprNodeArray*>(lnode_p)->getSliceDComplex
                                    (id, indexNode_p->getSlicer(id));
}
Array<String> TableExprNodeArrayPart::getArrayString (const TableExprId& id)
{
    DebugAssert (valueType() == VTArray, AipsError);
    return dynamic_cast<TableExprNodeArray*>(lnode_p)->getSliceString
                                    (id, indexNode_p->getSlicer(id));
}
Array<MVTime> TableExprNodeArrayPart::getArrayDate (const TableExprId& id)
{
    DebugAssert (valueType() == VTArray, AipsError);
    return dynamic_cast<TableExprNodeArray*>(lnode_p)->getSliceDate
                                    (id, indexNode_p->getSlicer(id));
}

Array<Bool> TableExprNodeArrayPart::getColumnBool()
{
    if (! indexNode_p->isConstant()) {
	return TableExprNodeRep::getColumnBool();
    }
    return dynamic_cast<TableExprNodeArray*>(lnode_p)->getElemColumnBool
                                   (indexNode_p->getSlicer(0));
}
Array<uChar>    TableExprNodeArrayPart::getColumnuChar()
{
    if (! indexNode_p->isConstant()) {
	return TableExprNodeRep::getColumnuChar();
    }
    return dynamic_cast<TableExprNodeArray*>(lnode_p)->getElemColumnuChar
                                   (indexNode_p->getSlicer(0));
}
Array<Short>    TableExprNodeArrayPart::getColumnShort()
{
    if (! indexNode_p->isConstant()) {
	return TableExprNodeRep::getColumnShort();
    }
    return dynamic_cast<TableExprNodeArray*>(lnode_p)->getElemColumnShort
                                   (indexNode_p->getSlicer(0));
}
Array<uShort>   TableExprNodeArrayPart::getColumnuShort()
{
    if (! indexNode_p->isConstant()) {
	return TableExprNodeRep::getColumnuShort();
    }
    return dynamic_cast<TableExprNodeArray*>(lnode_p)->getElemColumnuShort
                                   (indexNode_p->getSlicer(0));
}
Array<Int>      TableExprNodeArrayPart::getColumnInt()
{
    if (! indexNode_p->isConstant()) {
	return TableExprNodeRep::getColumnInt();
    }
    return dynamic_cast<TableExprNodeArray*>(lnode_p)->getElemColumnInt
                                   (indexNode_p->getSlicer(0));
}
Array<uInt>     TableExprNodeArrayPart::getColumnuInt()
{
    if (! indexNode_p->isConstant()) {
	return TableExprNodeRep::getColumnuInt();
    }
    return dynamic_cast<TableExprNodeArray*>(lnode_p)->getElemColumnuInt
                                   (indexNode_p->getSlicer(0));
}
Array<Float>    TableExprNodeArrayPart::getColumnFloat()
{
    if (! indexNode_p->isConstant()) {
	return TableExprNodeRep::getColumnFloat();
    }
    return dynamic_cast<TableExprNodeArray*>(lnode_p)->getElemColumnFloat
                                   (indexNode_p->getSlicer(0));
}
Array<Double>   TableExprNodeArrayPart::getColumnDouble()
{
    if (! indexNode_p->isConstant()) {
	return TableExprNodeRep::getColumnDouble();
    }
    return dynamic_cast<TableExprNodeArray*>(lnode_p)->getElemColumnDouble
                                   (indexNode_p->getSlicer(0));
}
Array<Complex>  TableExprNodeArrayPart::getColumnComplex()
{
    if (! indexNode_p->isConstant()) {
	return TableExprNodeRep::getColumnComplex();
    }
    return dynamic_cast<TableExprNodeArray*>(lnode_p)->getElemColumnComplex
                                   (indexNode_p->getSlicer(0));
}
Array<DComplex> TableExprNodeArrayPart::getColumnDComplex()
{
    if (! indexNode_p->isConstant()) {
	return TableExprNodeRep::getColumnDComplex();
    }
    return dynamic_cast<TableExprNodeArray*>(lnode_p)->getElemColumnDComplex
                                   (indexNode_p->getSlicer(0));
}
Array<String>   TableExprNodeArrayPart::getColumnString()
{
    if (! indexNode_p->isConstant()) {
	return TableExprNodeRep::getColumnString();
    }
    return dynamic_cast<TableExprNodeArray*>(lnode_p)->getElemColumnString
                                   (indexNode_p->getSlicer(0));
}
