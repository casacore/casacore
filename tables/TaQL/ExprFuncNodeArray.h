//# ExprFuncNodeArray.h: Class representing an array function in table select expression
//# Copyright (C) 2001,2003
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

#ifndef TABLES_EXPRFUNCNODEARRAY_H
#define TABLES_EXPRFUNCNODEARRAY_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNodeArray.h>
#include <casacore/tables/TaQL/ExprFuncNode.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations


// <summary>
// Class representing an array function in table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TableExprFuncNode>TableExprFuncNode</linkto>
//   <li> <linkto class=TableExprNodeArray>TableExprNodeArray</linkto>
// </prerequisite>

// <synopsis> 
// This class can be seen as a specialization of TableExprFuncNode
// for functions returning arrays.
// However, it is derived from TableExprNodeArray to make it possible
// that the ExprNode classes use all array functionality offered by
// that base class.
// <br>Internally an TableExprFuncNode object is used.
// <p>
// When a TaQL function is used, TableExprFuncNode::checkOperands
// determines whether the result is a scalar or an array.
// Thereafter TableExprNode::newFunctionNode creates a TableExprFuncNode
// for scalars or a TableExprFuncNodeArray for arrays.
// </synopsis> 


class TableExprFuncNodeArray : public TableExprNodeArray
{
public:
    // Constructor
    TableExprFuncNodeArray (TableExprFuncNode::FunctionType,
                            NodeDataType, ValueType,
                            const TableExprNodeSet& source,
                            const vector<TENShPtr>& nodes,
                            const Block<Int>& dtypeOper,
                            const TaQLStyle&);

    // Destructor
    ~TableExprFuncNodeArray();

    // Get the nodes representing an aggregate function.
    virtual void getAggrNodes (std::vector<TableExprNodeRep*>& aggr);

    // Get the nodes representing a table column.
    virtual void getColumnNodes (std::vector<TableExprNodeRep*>& cols);
  
    // 'get' Functions to get the desired result of a function
    // <group>
    virtual MArray<Bool> getArrayBool (const TableExprId& id);
    virtual MArray<Int64> getArrayInt (const TableExprId& id);
    virtual MArray<Double> getArrayDouble (const TableExprId& id);
    virtual MArray<DComplex> getArrayDComplex (const TableExprId& id);
    virtual MArray<String> getArrayString (const TableExprId& id);
    virtual MArray<MVTime> getArrayDate (const TableExprId& id);
    // </group>

    // Get the function node.
    TableExprFuncNode* getChild()
      { return &node_p; }
    const TableExprFuncNode* getChild() const
      { return &node_p; }

protected:
    // Try if the function gives a constant result.
    // If so, set the expression type to Constant.
    // Get possible constant arguments like axes.
    void tryToConst();

    // Some functions to be used by TableExprNodeFuncArray.
    // <group>
    const std::vector<TENShPtr>& operands() const
        { return node_p.operands(); }
    std::vector<TENShPtr>& rwOperands()
        { return node_p.rwOperands(); }
    TableExprFuncNode::FunctionType funcType() const
        { return node_p.funcType(); }
    NodeDataType argDataType() const
        { return node_p.argDataType(); }
    // </group>

private:
    // Get the collapse axes for the partial functions.
    // It compares the values with the #dim and removes them if too high.
    // axarg gives the argument nr of the axes.
    IPosition getAxes (const TableExprId& id,
                       Int ndim, uInt axarg=1, Bool swapRemove=True);

    // Remove axes exceeding ndim.
    IPosition removeAxes (const IPosition& axes, Int ndim) const;

    // Get the shape for the array, boxed and running functions.
    // If an axis length < 0, the corresponding main shape axis (if present)
    // is used.
    // axarg gives the argument nr of the shape.
    const IPosition& getArrayShape (const TableExprId& id,
                                    uInt axarg=1);

    // Get the transpose order of the array axes.
    IPosition getOrder (const TableExprId& id, Int ndim);

    // Get the axes for the reverse function.
    IPosition getReverseAxes (const TableExprId& id, uInt ndim);

    // Get the arguments for the diagonals function.
    // They are checked and if needed adapted if the shape is not empty.
    const IPosition& getDiagonalArg (const TableExprId& id,
                                     const IPosition& shp);

    // Set the alternate value expandAlt_p for array expand and return it.
    const IPosition& getAlternate (const TableExprId& id);

    // Adjust the resize shape by replacing negative axes with the
    // original axis (if present) or 1.
    IPosition adjustShape (const IPosition& shape,
                           const IPosition& origShape) const;
  
    // Templated function to resize/expand an array.
    template<typename T>
    MArray<T> TEFResize (const MArray<T>& arr, const TableExprId& id);

    // The angular distance between each pair of the arguments.
    MArray<Double> angdistx (const MArray<Double>& a1,
                             const MArray<Double>& a2) const;


    //# Data members
    TableExprFuncNode node_p;
    Int               origin_p;        //# axes origin
    Bool              isCOrder_p;      //# axes order
    Bool              constAxes_p;     //# True = collapse axes are constant
    Bool              constAlt_p;      //# True = expandAlt_p is constant
    IPosition         ipos_p;          //# the (maybe constant) axes or shape
    IPosition         iposN_p;         //# the non-reversed axes or shape
    IPosition         expandAlt_p;     //# alternate for expand/resize
};




} //# NAMESPACE CASACORE - END

#endif
