//# ExprUnitNode.cc: Nodes representing unit handling in table select expression tree
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
//# $Id: ExprUnitNode.cc 21262 2012-09-07 12:38:36Z gervandiepen $

#include <casacore/tables/TaQL/ExprUnitNode.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/tables/TaQL/MArray.h>
#include <casacore/casa/Arrays/ArrayMath.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableExprNodeUnit::TableExprNodeUnit (TableExprNodeRep& child,
				      const Unit& unit)
: TableExprNodeBinary (child.dataType(), child, OtUndef)
{
    // Units imply conversion, thus result cannot be integer.
    if (dtype_p == NTInt) {
        dtype_p = NTDouble;
    }
    lnode_p  = child.link();
    factor_p = set (*this, child, unit);
}

TableExprNodeUnit::~TableExprNodeUnit()
{}

Double TableExprNodeUnit::set (TableExprNodeRep& parent,
			       const TableExprNodeRep& child,
			       const Unit& unit)
{
  Double factor = 1;
  if (unit.empty()) {
    parent.setUnit (child.unit());
  } else {
    if (! child.unit().empty()) {
      // Conversion is only possible between units of the same type
      // and between time/angle.
      UnitVal type1 = unit.getValue();
      UnitVal type2 = child.unit().getValue();
      if (! (type1 == type2  
             || (type1 == UnitVal::ANGLE  &&  type2 == UnitVal::TIME)
             || (type2 == UnitVal::ANGLE  &&  type1 == UnitVal::TIME))) {
	throw TableInvExpr ("Units " + unit.getName() + " and " +
			    child.unit().getName() + " do not conform");
      }
      // Get conversion factor.
      Quantity q(1., child.unit());
      factor = q.getValue (unit);
    }
    parent.setUnit (unit);
  }
  return factor;
}

TableExprNodeRep* TableExprNodeUnit::useUnit (TableExprNodeRep* const node,
					      const Unit& unit)
{
  // No conversion needed if a unit is empty.
  // However, always set the node's unit to the new unit.
  if (unit.empty()  ||  node->unit().empty()) {
    node->setUnit (unit);
    return node;
  }
  // A conversion might be needed.
  // A set has to create conversion nodes for its elements.
  if (node->valueType() == VTSet  ||  node->valueType() == VTSetElem) {
    node->adaptSetUnits(unit);
    return node;
  }
  // Create a unit conversion node for a scalar or array.
  TableExprNodeBinary* tsnptr;
  if (node->valueType() == VTScalar) {
    tsnptr = new TableExprNodeUnit (*node, unit);
  } else {
    tsnptr = new TableExprNodeArrayUnit (*node, unit);
  }
  if (tsnptr->getUnitFactor() == 1.) {
    // Units are the same, so no conversion needed.
    delete tsnptr;
    return node;
  }
  return tsnptr;
}

void TableExprNodeUnit::adaptUnit (TableExprNodeRep*& node,
				   const Unit& unit)
{
  // See if a conversion is needed.
  // If so, adapt the reference counts and replace it.
  TableExprNodeRep* nnode = useUnit (node, unit);
  if (nnode != node) {
    unlink (node);
    node = nnode->link();
  }
}

Unit TableExprNodeUnit::adaptUnits (TableExprNodeRep*& node1,
				    TableExprNodeRep*& node2,
				    TableExprNodeRep*& node3)
{
  // Find unit to be used.
  Unit unit;
  if (unit.empty()  &&  node1) unit = node1->unit();
  if (unit.empty()  &&  node2) unit = node2->unit();
  if (unit.empty()  &&  node3) unit = node3->unit();
  if (! unit.empty()) {
    if (node1) adaptUnit (node1, unit);
    if (node2) adaptUnit (node2, unit);
    if (node3) adaptUnit (node3, unit);
  }
  return unit;
}

Double TableExprNodeUnit::getUnitFactor() const
  { return factor_p; }

Double TableExprNodeUnit::getDouble (const TableExprId& id)
  { return factor_p * lnode_p->getDouble(id); }

DComplex TableExprNodeUnit::getDComplex (const TableExprId& id)
  { return factor_p * lnode_p->getDComplex(id); }






TableExprNodeArrayUnit::TableExprNodeArrayUnit (TableExprNodeRep& child,
						const Unit& unit)
: TableExprNodeArray (child, child.dataType(), OtUndef)
{
  // Units imply conversion, thus result cannot be integer.
  if (dtype_p == NTInt) {
    dtype_p = NTDouble;
  }
  lnode_p  = child.link();
  factor_p = TableExprNodeUnit::set (*this, child, unit);
}

TableExprNodeArrayUnit::~TableExprNodeArrayUnit()
{}

Double TableExprNodeArrayUnit::getUnitFactor() const
  { return factor_p; }

MArray<Double> TableExprNodeArrayUnit::getArrayDouble (const TableExprId& id)
{ 
  MArray<Double> arr = lnode_p->getArrayDouble(id);
  return MArray<Double> (factor_p * arr.array(), arr.mask());
}

MArray<DComplex> TableExprNodeArrayUnit::getArrayDComplex(const TableExprId& id)
{
  MArray<DComplex> arr = lnode_p->getArrayDComplex(id);
  return MArray<DComplex> (DComplex(factor_p) * arr.array(), arr.mask());
}



} //# NAMESPACE CASACORE - END
