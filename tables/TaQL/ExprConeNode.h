//# ExprConeNode.h: Class representing a cone search in table select expression
//# Copyright (C) 2005
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

#ifndef TABLES_EXPRCONENODE_H
#define TABLES_EXPRCONENODE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprFuncNode.h>
#include <casacore/tables/TaQL/ExprFuncNodeArray.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


// <summary>
// Class representing a cone search in table select expression
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TableExprFuncNode>TableExprFuncNode</linkto>
// </prerequisite>

// <synopsis> 
// The class represents a cone search.
// It is a specialization of the TableExprFuncNode class.
// Currently the implementation is straightforward, but in the future
// it can do smarter things.
// For instance:
// <ul>
//  <li> If the cone positions and radii are constant, one can use
//       an integer zone number (e.g. floor(dec)) to avoid the much
//       more expensive sine/cosine calculations. Each cone will get a
//       minzone and maxzone value (derived from cone position and radius).
//  <li> Multiple cones can be ordered on minzone and maxzone.
// </ul>
// </synopsis> 


class TableExprConeNode : public TableExprFuncNode
{
public:
  // Constructor
  TableExprConeNode (FunctionType, NodeDataType,
		     const TableExprNodeSet& source, uInt origin);

  // Destructor
  ~TableExprConeNode();

  // 'get' Functions to get the desired result of a function.
  // <group>
  Bool  getBool (const TableExprId& id);
  Int64 getInt  (const TableExprId& id);
  // </group>

  // Check the data and value types of the operands.
  // It sets the exptected data and value types of the operands.
  // Set the value type of the function result and returns
  // the data type of the function result.
  static NodeDataType checkOperands (Block<Int>& dtypeOper,
				     ValueType& resVT,
				     Block<Int>& vtypeOper,
				     FunctionType,
				     PtrBlock<TableExprNodeRep*>&);

  // Link the children to the node and convert the children
  // to constants if possible. Also convert the node to
  // constant if possible.
  static TableExprNodeRep* fillNode (TableExprConeNode* thisNode,
				     PtrBlock<TableExprNodeRep*>& nodes,
				     const Block<Int>& dtypeOper);

  // Link the children to the node and convert the children
  // to constants if possible.
  static void fillChildNodes (TableExprConeNode* thisNode,
			      PtrBlock<TableExprNodeRep*>& nodes,
			      const Block<Int>& dtypeOper);

private:
  // Try if the function gives a constant result.
  // If so, set the expression type to Constant.
  void tryToConst();

  // Find the number of elements in an argument.
  // It returns -1 if unknown.
  static Int findNelem (const TableExprNodeRep* node);


  uInt origin_p;
};




class TableExprConeNodeArray : public TableExprFuncNodeArray
{
public:
  // Constructor
  TableExprConeNodeArray (TableExprFuncNode::FunctionType, NodeDataType,
                          const TableExprNodeSet& source, uInt origin);

  // Destructor
  ~TableExprConeNodeArray();

  // 'get' Functions to get the desired result of a function.
  // <group>
  Array<Bool>  getArrayBool (const TableExprId& id);
  Array<Int64> getArrayInt  (const TableExprId& id);
  // </group>

  // Link the children to the node and convert the children
  // to constants if possible. Also convert the node to
  // constant if possible.
  static TableExprNodeRep* fillNode (TableExprConeNodeArray* thisNode,
				     PtrBlock<TableExprNodeRep*>& nodes,
				     const Block<Int>& dtypeOper);

  // Link the children to the node and convert the children
  // to constants if possible.
  static void fillChildNodes (TableExprConeNodeArray* thisNode,
			      PtrBlock<TableExprNodeRep*>& nodes,
			      const Block<Int>& dtypeOper);

private:
  // Try if the function gives a constant result.
  // If so, set the expression type to Constant.
  void tryToConst();


  uInt origin_p;
};


} //# NAMESPACE CASACORE - END

#endif
