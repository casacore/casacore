//# ExprAggrNode.h: TaQL node representing an aggregate function
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
#include <tables/Tables/ExprFuncNode.h>


namespace casa { //# NAMESPACE CASA - BEGIN

  //# Forward Declarations.
  class TableExprGroupFunc;
  class TableExprGroupFuncSet;

// <summary>
// Envelope class for a node in the raw TaQL parse tree.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto group=TableGram.h#TableGramFunctions>TableGram</linkto>
//   <li> Note 199 describing
//        <a href="../notes/199.html">
//        TaQL</a>
// </prerequisite>

// <synopsis>
// The result of parsing a TaQL command is stored in TaQLNode objects.
// Each part of the command can have its own specialized
// <linkto class=TaQLNodeRep>TaQLNodeRep</linkto> object, which forms
// the letter in the TaQLNode envelope.
// <br>The actual scanning/parsing of the command is done using flex/bison
// as defined in the TableGram files.
// </synopsis> 

// <motivation>
// The letter-envelope idiom (counted pointer) makes if much easier
// to keep track of memory, especially in the case of exceptions.
// </motivation>


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
    virtual void getAggrNodes (vector<TableExprAggrNode*>& aggr);

    // Get the operand node.
    TableExprNodeRep* operand()
      { return operands()[0]; }

    // Create the correct aggregate function object.
    TableExprGroupFunc* makeGroupFunc();

    // Set the result of the aggregation.
    // The vector represents the rows in the resulting table.
    // The seqnr-th entry in each TableExprGroupFuncSet represents this node.
    void setResult (const vector<CountedPtr<TableExprGroupFuncSet> >&,
                    uInt funcnr);

    // Functions to get the result of an aggregate function.
    // <group>
    Bool      getBool     (const TableExprId& id);
    Int64     getInt      (const TableExprId& id);
    Double    getDouble   (const TableExprId& id);
    DComplex  getDComplex (const TableExprId& id);
    String    getString   (const TableExprId& id);
    MVTime    getDate     (const TableExprId& id);
    // </group>

  private:
    uInt itsFuncNr;
    const vector<CountedPtr<TableExprGroupFuncSet> >* itsResult;
  };


} //# NAMESPACE CASA - END

#endif
