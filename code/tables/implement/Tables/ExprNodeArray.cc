//# ExprNodeArray.cc: Classes representing an array in table select expression
//# Copyright (C) 1997
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
#include <aips/Tables/TableError.h>
#include <aips/Lattices/Slicer.h>


TableExprNodeArray::TableExprNodeArray (const ROTableColumn& tablecol,
					const BaseTable* tabptr)
: TableExprNodeBinary (NTNumeric, OtArrCol),
  tabCol_p            (tablecol)
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
}

TableExprNodeArray::~TableExprNodeArray()
{}

uInt TableExprNodeArray::ndim() const
{
    return tabCol_p.ndimColumn();
}
IPosition TableExprNodeArray::shape() const
{
    return tabCol_p.shapeColumn();
}
Bool TableExprNodeArray::getColumnDataType (DataType& dt) const
{
    dt = tabCol_p.columnDesc().dataType();
    return True;
}

Bool TableExprNodeArray::getElemBool (uInt, const IPosition&)
{
    TableExprNode::throwInvDT 
                       ("(getElemBool(IPosition) not implemented)");
    return False;
}
double TableExprNodeArray::getElemDouble (uInt, const IPosition&)
{
    TableExprNode::throwInvDT
                       ("(getElemDouble(IPosition) not implemented)");
    return 0;
}
DComplex TableExprNodeArray::getElemDComplex (uInt, const IPosition&)
{
    TableExprNode::throwInvDT
                       ("(getElemDComplex(IPosition) not implemented)");
    return 0;
}
String TableExprNodeArray::getElemString (uInt, const IPosition&)
{
    TableExprNode::throwInvDT
                       ("(getElemString(IPosition) not implemented)");
    return "";
}
Array<Bool>     TableExprNodeArray::getElemColumnBool (const IPosition&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnBool(IPosition) not implemented)");
    return Array<Bool>();
}
Array<uChar>    TableExprNodeArray::getElemColumnuChar (const IPosition&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnuChar(IPosition) not implemented)");
    return Array<uChar>();
}
Array<Short>    TableExprNodeArray::getElemColumnShort (const IPosition&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnShort(IPosition) not implemented)");
    return Array<Short>();
}
Array<uShort>   TableExprNodeArray::getElemColumnuShort (const IPosition&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnuShort(IPosition) not implemented)");
    return Array<uShort>();
}
Array<Int>      TableExprNodeArray::getElemColumnInt (const IPosition&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnInt(IPosition) not implemented)");
    return Array<Int>();
}
Array<uInt>     TableExprNodeArray::getElemColumnuInt (const IPosition&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnuInt(IPosition) not implemented)");
    return Array<uInt>();
}
Array<Float>    TableExprNodeArray::getElemColumnFloat (const IPosition&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnFloat(IPosition) not implemented)");
    return Array<Float>();
}
Array<Double>   TableExprNodeArray::getElemColumnDouble (const IPosition&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnDouble(IPosition) not implemented)");
    return Array<Double>();
}
Array<Complex>  TableExprNodeArray::getElemColumnComplex (const IPosition&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnComplex(IPosition) not implemented)");
    return Array<Complex>();
}
Array<DComplex> TableExprNodeArray::getElemColumnDComplex (const IPosition&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnDComplex(IPosition) not implemented)");
    return Array<DComplex>();
}
Array<String>   TableExprNodeArray::getElemColumnString (const IPosition&)
{
    TableExprNode::throwInvDT
                       ("(getElemColumnString(IPosition) not implemented)");
    return Array<String>();
}


TableExprNodeArrayBool::TableExprNodeArrayBool (const ROTableColumn& col,
						const BaseTable* tabptr)
: TableExprNodeArray (col, tabptr),
  col_p              (col)
{}
TableExprNodeArrayBool::~TableExprNodeArrayBool()
{}
Bool TableExprNodeArrayBool::getElemBool (uInt rownr, const IPosition& index)
{
    Array<Bool> arr = col_p.getSlice (rownr, index);
    Bool deleteIt;
    Bool* f = arr.getStorage (deleteIt);
    Bool val = *f;
    arr.freeStorage (f, deleteIt);
    return val;
}
Array<Bool> TableExprNodeArrayBool::getElemColumnBool (const IPosition& index)
{
    return col_p.getColumn (Slicer(index));
}

TableExprNodeArrayuChar::TableExprNodeArrayuChar (const ROTableColumn& col,
						  const BaseTable* tabptr)
: TableExprNodeArray (col, tabptr),
  col_p              (col)
{}
TableExprNodeArrayuChar::~TableExprNodeArrayuChar()
{}
double TableExprNodeArrayuChar::getElemDouble (uInt rownr,
					       const IPosition& index)
{
    Array<uChar> arr = col_p.getSlice (rownr, index);
    Bool deleteIt;
    uChar* f = arr.getStorage (deleteIt);
    uChar val = *f;
    arr.freeStorage (f, deleteIt);
    return val;
}
Array<uChar> TableExprNodeArrayuChar::getElemColumnuChar
                                              (const IPosition& index)
{
    return col_p.getColumn (Slicer(index));
}

TableExprNodeArrayShort::TableExprNodeArrayShort (const ROTableColumn& col,
						  const BaseTable* tabptr)
: TableExprNodeArray (col, tabptr),
  col_p              (col)
{}
TableExprNodeArrayShort::~TableExprNodeArrayShort()
{}
double TableExprNodeArrayShort::getElemDouble (uInt rownr,
					       const IPosition& index)
{
    Array<Short> arr = col_p.getSlice (rownr, index);
    Bool deleteIt;
    Short* f = arr.getStorage (deleteIt);
    Short val = *f;
    arr.freeStorage (f, deleteIt);
    return val;
}
Array<Short> TableExprNodeArrayShort::getElemColumnShort
                                              (const IPosition& index)
{
    return col_p.getColumn (Slicer(index));
}

TableExprNodeArrayuShort::TableExprNodeArrayuShort (const ROTableColumn& col,
						    const BaseTable* tabptr)
: TableExprNodeArray (col, tabptr),
  col_p              (col)
{}
TableExprNodeArrayuShort::~TableExprNodeArrayuShort()
{}
double TableExprNodeArrayuShort::getElemDouble (uInt rownr,
						const IPosition& index)
{
    Array<uShort> arr = col_p.getSlice (rownr, index);
    Bool deleteIt;
    uShort* f = arr.getStorage (deleteIt);
    uShort val = *f;
    arr.freeStorage (f, deleteIt);
    return val;
}
Array<uShort> TableExprNodeArrayuShort::getElemColumnuShort
                                                (const IPosition& index)
{
    return col_p.getColumn (Slicer(index));
}

TableExprNodeArrayInt::TableExprNodeArrayInt (const ROTableColumn& col,
					      const BaseTable* tabptr)
: TableExprNodeArray (col, tabptr),
  col_p              (col)
{}
TableExprNodeArrayInt::~TableExprNodeArrayInt()
{}
double TableExprNodeArrayInt::getElemDouble (uInt rownr,
					     const IPosition& index)
{
    Array<Int> arr = col_p.getSlice (rownr, index);
    Bool deleteIt;
    Int* f = arr.getStorage (deleteIt);
    Int val = *f;
    arr.freeStorage (f, deleteIt);
    return val;
}
Array<Int> TableExprNodeArrayInt::getElemColumnInt (const IPosition& index)
{
    return col_p.getColumn (Slicer(index));
}

TableExprNodeArrayuInt::TableExprNodeArrayuInt (const ROTableColumn& col,
						const BaseTable* tabptr)
: TableExprNodeArray (col, tabptr),
  col_p              (col)
{}
TableExprNodeArrayuInt::~TableExprNodeArrayuInt()
{}
double TableExprNodeArrayuInt::getElemDouble (uInt rownr,
					      const IPosition& index)
{
    Array<uInt> arr = col_p.getSlice (rownr, index);
    Bool deleteIt;
    uInt* f = arr.getStorage (deleteIt);
    uInt val = *f;
    arr.freeStorage (f, deleteIt);
    return val;
}
Array<uInt> TableExprNodeArrayuInt::getElemColumnuInt (const IPosition& index)
{
    return col_p.getColumn (Slicer(index));
}

TableExprNodeArrayFloat::TableExprNodeArrayFloat (const ROTableColumn& col,
						  const BaseTable* tabptr)
: TableExprNodeArray (col, tabptr),
  col_p              (col)
{}
TableExprNodeArrayFloat::~TableExprNodeArrayFloat()
{}
double TableExprNodeArrayFloat::getElemDouble (uInt rownr,
					       const IPosition& index)
{
    Array<Float> arr = col_p.getSlice (rownr, index);
    Bool deleteIt;
    Float* f = arr.getStorage (deleteIt);
    Float val = *f;
    arr.freeStorage (f, deleteIt);
    return val;
}
Array<Float> TableExprNodeArrayFloat::getElemColumnFloat
                                              (const IPosition& index)
{
    return col_p.getColumn (Slicer(index));
}

TableExprNodeArrayDouble::TableExprNodeArrayDouble (const ROTableColumn& col,
						    const BaseTable* tabptr)
: TableExprNodeArray (col, tabptr),
  col_p              (col)
{}
TableExprNodeArrayDouble::~TableExprNodeArrayDouble()
{}
double TableExprNodeArrayDouble::getElemDouble (uInt rownr,
						const IPosition& index)
{
    Array<double> arr = col_p.getSlice (rownr, index);
    Bool deleteIt;
    double* f = arr.getStorage (deleteIt);
    double val = *f;
    arr.freeStorage (f, deleteIt);
    return val;
}
Array<Double> TableExprNodeArrayDouble::getElemColumnDouble
                                                (const IPosition& index)
{
    return col_p.getColumn (Slicer(index));
}

TableExprNodeArrayComplex::TableExprNodeArrayComplex (const ROTableColumn& col,
						      const BaseTable* tabptr)
: TableExprNodeArray (col, tabptr),
  col_p              (col)
{}
TableExprNodeArrayComplex::~TableExprNodeArrayComplex()
{}
DComplex TableExprNodeArrayComplex::getElemDComplex (uInt rownr,
						     const IPosition& index)
{
    Array<Complex> arr = col_p.getSlice (rownr, index);
    Bool deleteIt;
    Complex* f = arr.getStorage (deleteIt);
    DComplex val;
    val = *f;
    arr.freeStorage (f, deleteIt);
    return val;
}
Array<Complex> TableExprNodeArrayComplex::getElemColumnComplex
                                                (const IPosition& index)
{
    return col_p.getColumn (Slicer(index));
}

TableExprNodeArrayDComplex::TableExprNodeArrayDComplex
                                                    (const ROTableColumn& col,
						     const BaseTable* tabptr)
: TableExprNodeArray (col, tabptr),
  col_p              (col)
{}
TableExprNodeArrayDComplex::~TableExprNodeArrayDComplex()
{}
DComplex TableExprNodeArrayDComplex::getElemDComplex (uInt rownr,
						      const IPosition& index)
{
    Array<DComplex> arr = col_p.getSlice (rownr, index);
    Bool deleteIt;
    DComplex* f = arr.getStorage (deleteIt);
    DComplex val = *f;
    arr.freeStorage (f, deleteIt);
    return val;
}
Array<DComplex> TableExprNodeArrayDComplex::getElemColumnDComplex
                                                    (const IPosition& index)
{
    return col_p.getColumn (Slicer(index));
}

TableExprNodeArrayString::TableExprNodeArrayString (const ROTableColumn& col,
						    const BaseTable* tabptr)
: TableExprNodeArray (col, tabptr),
  col_p              (col)
{}
TableExprNodeArrayString::~TableExprNodeArrayString()
{}
String TableExprNodeArrayString::getElemString (uInt rownr,
						const IPosition& index)
{
    Array<String> arr = col_p.getSlice (rownr, index);
    Bool deleteIt;
    String* f = arr.getStorage (deleteIt);
    String val = *f;
    arr.freeStorage (f, deleteIt);
    return val;
}
Array<String> TableExprNodeArrayString::getElemColumnString
                                                (const IPosition& index)
{
    return col_p.getColumn (Slicer(index));
}




// ----------------------------
// TableExprNodeIndex functions
// ----------------------------

TableExprNodeIndex::TableExprNodeIndex (uInt origin)
: TableExprNodeMulti (NTDouble, OtIndex),
  origin_p           (origin),
  isConst_p          (False)
{}

TableExprNodeIndex::~TableExprNodeIndex()
{}

void TableExprNodeIndex::checkIndices
                                (const PtrBlock<TableExprNodeRep*>& indices)
{
    Block<Int> dtypeOper;
    checkDT (dtypeOper, NTDouble, NTDouble, indices);
}

void TableExprNodeIndex::checkIndexValues (const TableExprNodeRep* arrayNode)
{
    uInt i;
    uInt ndim = arrayNode->ndim();
    if (ndim > 0  &&  index_p.nelements() != ndim) {
	throw (TableInvExpr ("#indices mismatches array dimensionality"));
    }
    for (i=0; i<index_p.nelements(); i++) {
	if (!varAxes_p[i]) {
	    if (index_p(i) < 0) {
		throw (TableInvExpr ("index value before array origin"));
	    }
	}
    }
    IPosition shape = arrayNode->shape();
    for (i=0; i<shape.nelements(); i++) {
	if (!varAxes_p[i]) {
	    if (index_p(i) >= shape(i)) {
		throw (TableInvExpr ("index value exceeds array shape"));
	    }
	}
    }
}

void TableExprNodeIndex::fillIndex (uInt rownr)
{
    uInt n = varAxes_p.nelements();
    for (uInt i=0; i<n; i++) {
	if (varAxes_p[i]) {
	    index_p(i) = Int(operands_p[i]->getDouble (rownr) - origin_p);
	}
    }
}

// Fill the children pointers of a node.
// Also reduce the tree if possible by combining constants.
void TableExprNodeIndex::fillNode (const PtrBlock<TableExprNodeRep*>& indices)
{
    // Copy block of children
    operands_p.resize (indices.nelements());
    for (uInt i=0; i<indices.nelements(); i++) {
	operands_p[i] = indices[i]->link();
    }
    checkTable();
    convertConst();
}

void TableExprNodeIndex::convertConst()
{
    uInt n = operands_p.nelements();
    index_p.resize (n);
    varAxes_p.resize (n);
    varAxes_p.set (True);
    isConst_p = True;
    for (uInt i=0; i<n; i++) {
	if (operands_p[i]->operType() == OtConst) {
	    index_p(i) = Int(operands_p[i]->getDouble (0) - origin_p);
	    varAxes_p[i] = False;
	}else{
	    isConst_p = False;
	}
    }
}




// -------------------------
// TableExprNodeArrayElement
// -------------------------

TableExprNodeArrayElement::TableExprNodeArrayElement
                                       (TableExprNodeIndex* indexNode,
					NodeDataType tp)
: TableExprNodeBinary (tp, OtArray),
  indexNode_p         (indexNode)
{}

TableExprNodeArrayElement::~TableExprNodeArrayElement()
{}

Bool TableExprNodeArrayElement::getColumnDataType (DataType& dt) const
{
    //# Return data type of column when constant index.
    if (indexNode_p->baseTablePtr() != 0) {
	return False;
    }
    return lnode_p->getColumnDataType (dt);
}

//# Note that all following casts are perfectly safe.
Bool TableExprNodeArrayElement::getBool (uInt rownr)
{
    return ((TableExprNodeArray*)lnode_p)->getElemBool
                                    (rownr, indexNode_p->getIndex(rownr));
}
double TableExprNodeArrayElement::getDouble (uInt rownr)
{
    return ((TableExprNodeArray*)lnode_p)->getElemDouble
                                    (rownr, indexNode_p->getIndex(rownr));
}
DComplex TableExprNodeArrayElement::getDComplex (uInt rownr)
{
    return ((TableExprNodeArray*)lnode_p)->getElemDComplex
                                    (rownr, indexNode_p->getIndex(rownr));
}
String TableExprNodeArrayElement::getString (uInt rownr)
{
    return ((TableExprNodeArray*)lnode_p)->getElemString
                                    (rownr, indexNode_p->getIndex(rownr));
}

Array<Bool>     TableExprNodeArrayElement::getColumnBool()
{
    if (indexNode_p->baseTablePtr() != 0) {
	return TableExprNodeRep::getColumnBool();
    }
    return ((TableExprNodeArray*)lnode_p)->getElemColumnBool
                                              (indexNode_p->getIndex(0));
}
Array<uChar>    TableExprNodeArrayElement::getColumnuChar()
{
    if (indexNode_p->baseTablePtr() != 0) {
	return TableExprNodeRep::getColumnuChar();
    }
    return ((TableExprNodeArray*)lnode_p)->getElemColumnuChar
                                              (indexNode_p->getIndex(0));
}
Array<Short>    TableExprNodeArrayElement::getColumnShort()
{
    if (indexNode_p->baseTablePtr() != 0) {
	return TableExprNodeRep::getColumnShort();
    }
    return ((TableExprNodeArray*)lnode_p)->getElemColumnShort
                                              (indexNode_p->getIndex(0));
}
Array<uShort>   TableExprNodeArrayElement::getColumnuShort()
{
    if (indexNode_p->baseTablePtr() != 0) {
	return TableExprNodeRep::getColumnuShort();
    }
    return ((TableExprNodeArray*)lnode_p)->getElemColumnuShort
                                              (indexNode_p->getIndex(0));
}
Array<Int>      TableExprNodeArrayElement::getColumnInt()
{
    if (indexNode_p->baseTablePtr() != 0) {
	return TableExprNodeRep::getColumnInt();
    }
    return ((TableExprNodeArray*)lnode_p)->getElemColumnInt
                                              (indexNode_p->getIndex(0));
}
Array<uInt>     TableExprNodeArrayElement::getColumnuInt()
{
    if (indexNode_p->baseTablePtr() != 0) {
	return TableExprNodeRep::getColumnuInt();
    }
    return ((TableExprNodeArray*)lnode_p)->getElemColumnuInt
                                              (indexNode_p->getIndex(0));
}
Array<Float>    TableExprNodeArrayElement::getColumnFloat()
{
    if (indexNode_p->baseTablePtr() != 0) {
	return TableExprNodeRep::getColumnFloat();
    }
    return ((TableExprNodeArray*)lnode_p)->getElemColumnFloat
                                              (indexNode_p->getIndex(0));
}
Array<Double>   TableExprNodeArrayElement::getColumnDouble()
{
    if (indexNode_p->baseTablePtr() != 0) {
	return TableExprNodeRep::getColumnDouble();
    }
    return ((TableExprNodeArray*)lnode_p)->getElemColumnDouble
                                              (indexNode_p->getIndex(0));
}
Array<Complex>  TableExprNodeArrayElement::getColumnComplex()
{
    if (indexNode_p->baseTablePtr() != 0) {
	return TableExprNodeRep::getColumnComplex();
    }
    return ((TableExprNodeArray*)lnode_p)->getElemColumnComplex
                                              (indexNode_p->getIndex(0));
}
Array<DComplex> TableExprNodeArrayElement::getColumnDComplex()
{
    if (indexNode_p->baseTablePtr() != 0) {
	return TableExprNodeRep::getColumnDComplex();
    }
    return ((TableExprNodeArray*)lnode_p)->getElemColumnDComplex
                                              (indexNode_p->getIndex(0));
}
Array<String>   TableExprNodeArrayElement::getColumnString()
{
    if (indexNode_p->baseTablePtr() != 0) {
	return TableExprNodeRep::getColumnString();
    }
    return ((TableExprNodeArray*)lnode_p)->getElemColumnString
                                              (indexNode_p->getIndex(0));
}
