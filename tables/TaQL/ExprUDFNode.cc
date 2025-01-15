//# ExprUDFNode.cc: Class representing a scalar UDF in select expression
//# Copyright (C) 2010
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

//# Includes
#include <casacore/tables/TaQL/ExprUDFNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/tables/TaQL/ExprGroup.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN
  
  TableExprUDFNode::TableExprUDFNode (const std::shared_ptr<UDFBase>& udf,
                                      const TableExprInfo& tabInfo,
                                      const TableExprNodeSet& source)
    : TableExprNodeMulti (udf->dataType(), VTScalar, OtFunc, source),
      itsTableInfo (tabInfo),
      itsUDF       (udf)
  {
    // The source may be empty which causes the expression type
    // to be made constant. Force it to be variable if needed.
    if (udf->isConstant()) {
      exprtype_p = Constant;
    } else {
      exprtype_p = Variable;
    }
    // Set the unit and attributes (is also fine if undefined).
    setUnit (Unit(udf->getUnit()));
    setAttributes (udf->getAttributes());
  }

  void TableExprUDFNode::flattenTree (vector<TableExprNodeRep*>& nodes)
  {
    nodes.push_back (this);
    itsUDF->flattenTree (nodes);
  }

  TableExprInfo TableExprUDFNode::getTableInfo() const
    { return itsTableInfo; }
  
  void TableExprUDFNode::disableApplySelection()
    { itsUDF->disableApplySelection(); }

  void TableExprUDFNode::applySelection (const Vector<rownr_t>& rownrs)
    { itsUDF->applySelection (rownrs); }

  std::shared_ptr<TableExprGroupFuncBase> TableExprUDFNode::makeGroupAggrFunc()
    { return std::make_shared<TableExprGroupNull>(this); }

  Bool      TableExprUDFNode::getBool     (const TableExprId& id)
    { return itsUDF->getBool (id); }
  Int64     TableExprUDFNode::getInt      (const TableExprId& id)
    { return itsUDF->getInt (id); }
  Double    TableExprUDFNode::getDouble   (const TableExprId& id)
    { return itsUDF->getDouble (id); }
  DComplex  TableExprUDFNode::getDComplex (const TableExprId& id)
    { return itsUDF->getDComplex (id); }
  String    TableExprUDFNode::getString   (const TableExprId& id)
    { return itsUDF->getString (id); }
  TaqlRegex TableExprUDFNode::getRegex    (const TableExprId& id)
    { return itsUDF->getRegex (id); }
  MVTime    TableExprUDFNode::getDate     (const TableExprId& id)
    { return itsUDF->getDate (id); }

} //# NAMESPACE CASACORE - END
