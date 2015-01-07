//# ExprAggrNode.h: TaQL node representing a scalar aggregate function
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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id: TaQLNode.h 21051 2011-04-20 11:46:29Z gervandiepen $

#ifndef TABLES_EXPRAGGRNODE_H
#define TABLES_EXPRAGGRNODE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprFuncNode.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward Declarations.
  class TableExprGroupFuncBase;
  class TableExprGroupFuncSet;

// <summary>
// TaQL node representing a scalar aggregate function
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tTableGram">
// </reviewed>

// <synopsis>
// A TableExprAggrNode object is a special TableExprFuncNode object.
// Instead of operating on a single row, it operates on a group of table
// rows, usually formed by means of the GROUPBY clause.
// It aggregates the values in the rows in the group by means of an
// aggregation function derived from TableExprGroupFuncBase.
// Several standard aggregation functions (e.g., gmean, gmin, gsum) are
// defined in TaQL and implemented this way.
//
// There are two types of aggregate function implementations:
// <ul>
//  <li> Immediate aggregate functions calculate the results while the
//       groups are being formed. In this way they step sequentially through
//       the data.
//       This is only possible for functions that do not have to keep
//       to many data in memory.
//  <li> Lazy aggregate functions calculate the results after the groups are
//       formed using the vector of TableExprIds they get per group.
//       In this way only data for a single group might need to be kept in
//       memory. It is used, for instance, to calculate the median.
// </ul>
// Note that this class handles operands that are a scalar or array.
// If array, all values in the array are used as individual values.
// Class TableExprAggrNodeArray handles aggregate functions giving an
// array result (e.g., function <src>gaggr</src>).
//
// It is also possible to define an aggregate function in a UDF derived
// from class UDFBase. Such an aggregate function is instantiated as a
// TableExprUDFNode(Array) object, not as TabeExprAggrNode(Array).
// These functions are always lazy.
// </synopsis> 

  class TableExprAggrNode: public TableExprFuncNode
  {
  public:
    // Constructor.
    TableExprAggrNode (FunctionType, NodeDataType, ValueType,
		       const TableExprNodeSet& source);

    // Check the operands of the aggregate function and return the
    // result's data type.
    static NodeDataType checkOperands (Block<Int>& dtypeOper,
                                       ValueType& resVT, FunctionType ftype,
                                       PtrBlock<TableExprNodeRep*>& nodes);

    // Get the nodes representing an aggregate function.
    virtual void getAggrNodes (vector<TableExprNodeRep*>& aggr);

    // Get the operand node.
    TableExprNodeRep* operand()
      { return (operands().empty()  ?  0 : operands()[0]); }

    // Create the correct aggregate function object.
    // It is also kept in case it is a lazy aggregate function.
    virtual CountedPtr<TableExprGroupFuncBase> makeGroupAggrFunc();

    // Is the aggregate function a lazy or an immediate one?
    virtual Bool isLazyAggregate() const;

    // Functions to get the result of an aggregate function.
    // <group>
    virtual Bool      getBool     (const TableExprId& id);
    virtual Int64     getInt      (const TableExprId& id);
    virtual Double    getDouble   (const TableExprId& id);
    virtual DComplex  getDComplex (const TableExprId& id);
    virtual String    getString   (const TableExprId& id);
    virtual MVTime    getDate     (const TableExprId& id);
    // </group>

  private:
    // Do the actual creation of the correct aggregate function object.
    TableExprGroupFuncBase* doMakeGroupAggrFunc();

    //# Data members.
    CountedPtr<TableExprGroupFuncBase> itsFunc;
  };


} //# NAMESPACE CASACORE - END

#endif
