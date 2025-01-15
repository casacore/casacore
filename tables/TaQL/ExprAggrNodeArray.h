//# ExprAggrNodeArray.h: TaQL node representing an array aggregate function
//# Copyright (C) 2013
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef TABLES_EXPRAGGRNODEARRAY_H
#define TABLES_EXPRAGGRNODEARRAY_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprFuncNodeArray.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward Declarations.
  class TableExprGroupFuncBase;
  class TableExprGroupFuncSet;

// <summary>
// TaQL node representing an array aggregate function
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>

// <synopsis>
// This class is similar to TableExprAggrNode, but its result is an array
// instead of a scalar value.
// There are few aggregate functions resulting in an array. An example
// is <src>gaggr</src>, which aggregates the non-empty arrays in a group
// into a single array. Other functions (like medians, runningmean, etc.)
// can be applied to its result making it quite versatile.
//
// Most array aggregate functions are lazy to avoid using too much memory.
// </synopsis> 

  class TableExprAggrNodeArray: public TableExprFuncNodeArray
  {
  public:
    // Constructor.
    TableExprAggrNodeArray (TableExprFuncNode::FunctionType,
                            NodeDataType, ValueType,
                            const TableExprNodeSet& source,
                            const vector<TENShPtr>& nodes,
                            const Block<Int>& dtypeOper,
                            const TaQLStyle& style);

    // This node does aggregation.
    virtual Bool isAggregate() const;
    
    // Get the operand node.
    TENShPtr operand()
      { return (operands().empty()  ?  TENShPtr() : operands()[0]); }

    // Create the correct aggregate function object.
    virtual std::shared_ptr<TableExprGroupFuncBase> makeGroupAggrFunc();

    // Is the array aggregate function lazy?
    virtual Bool isLazyAggregate() const;

    // Functions to get the result of an aggregate function.
    // <group>
    virtual MArray<Bool>     getArrayBool     (const TableExprId& id);
    virtual MArray<Int64>    getArrayInt      (const TableExprId& id);
    virtual MArray<Double>   getArrayDouble   (const TableExprId& id);
    virtual MArray<DComplex> getArrayDComplex (const TableExprId& id);
    virtual MArray<String>   getArrayString   (const TableExprId& id);
    virtual MArray<MVTime>   getArrayDate     (const TableExprId& id);
    // </group>

  private:
    // Create the correct aggregate function object.
    std::shared_ptr<TableExprGroupFuncBase> doMakeGroupAggrFunc();

    //# Data members.
    std::shared_ptr<TableExprGroupFuncBase> itsFunc;
  };


} //# NAMESPACE CASACORE - END

#endif
