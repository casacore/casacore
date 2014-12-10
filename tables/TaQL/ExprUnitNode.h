//# ExprUnitNode.h: Nodes representing unit handling in table select expression tree
//# Copyright (C) 2006
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

#ifndef TABLES_EXPRUNITNODE_H
#define TABLES_EXPRUNITNODE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNodeRep.h>
#include <casacore/tables/TaQL/ExprNodeArray.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN


// <summary>
// Unit for scalar values in a table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>
//
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> Unit
// </prerequisite>

// <synopsis> 
// This class represents a unit in a table select expression tree.
// It contains a unit conversion factor to convert the child to this unit.
// The factor is 1 if the child has no unit.
// </synopsis> 

class TableExprNodeUnit : public TableExprNodeBinary
{
public:
  // Constrcut from the given child node and unit.
  TableExprNodeUnit (TableExprNodeRep& child, const Unit& unit);

  ~TableExprNodeUnit();

  // Calculate the conversion factor and return it.
  // It is static to be useful for TableExprNodeArrayFunc as well.
  static Double set (TableExprNodeRep& parent,
		     const TableExprNodeRep& child,
		     const Unit& unit);

  // Create a new node if unit conversion is needed.
  // Otherwise return the current node.
  static TableExprNodeRep* useUnit (TableExprNodeRep* const node,
				    const Unit& unit);

  // Use <src>useUnit</src> to see if a conversion is needed.
  // If so, adapt the reference counts and replace the node.
  static void adaptUnit (TableExprNodeRep*& node, const Unit& unit);

  // Find the unit to be used and adapt the nodes to it.
  static Unit adaptUnits (TableExprNodeRep*& node1,
			  TableExprNodeRep*& node2,
			  TableExprNodeRep*& node3);

  // Get the unit factor.
  virtual Double getUnitFactor() const;

  virtual Double   getDouble   (const TableExprId& id);
  virtual DComplex getDComplex (const TableExprId& id);
private:
  Double factor_p;
};




// <summary>
// Unit for array values in a table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>
//
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> Unit
// </prerequisite>

// <synopsis> 
// This class represents a unit in a table select expression tree.
// It contains a unit conversion factor to convert the child to this unit.
// The factor is 1 if the child has no unit.
// </synopsis> 

class TableExprNodeArrayUnit : public TableExprNodeArray
{
public:
  TableExprNodeArrayUnit (TableExprNodeRep& child, const Unit& unit);
  ~TableExprNodeArrayUnit();
  virtual Double getUnitFactor() const;
  virtual Array<Double>   getArrayDouble   (const TableExprId& id);
  virtual Array<DComplex> getArrayDComplex (const TableExprId& id);
private:
  Double factor_p;
};



} //# NAMESPACE CASACORE - END

#endif
