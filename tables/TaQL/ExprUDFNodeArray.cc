//# ExprUDFNodeArray.cc: Class representing an array UDF in select expression
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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

//# Includes
#include <casacore/tables/TaQL/ExprUDFNodeArray.h>
#include <casacore/tables/TaQL/ExprGroup.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN
  
  TableExprUDFNodeArray::TableExprUDFNodeArray (UDFBase* udf, const Table& tab,
                                                const TableExprNodeSet&)
    : TableExprNodeArray (udf->dataType(), OtFunc),
      itsUDF (udf)
  {
    // Set the table. This is needed for ExprNode::checkReplaceTable to work.
    table_p = tab;
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

  TableExprUDFNodeArray::~TableExprUDFNodeArray()
  {
    delete itsUDF;
  }

  void TableExprUDFNodeArray::getAggrNodes (vector<TableExprNodeRep*>& aggr)
  {
    uInt naggr = aggr.size();
    itsUDF->getAggrNodes (aggr);
    if (itsUDF->isAggregate()) {
      // If the UDF itself is an aggregate function, its operands should not
      // contain aggregate functions.
      if (naggr != aggr.size()) {
        throw TableInvExpr ("The argument of an aggregate function cannot use "
                            "an aggregate function");
      }
      aggr.push_back (this);
    }
  }

  void TableExprUDFNodeArray::getColumnNodes (vector<TableExprNodeRep*>& cols)
  {
    itsUDF->getColumnNodes (cols);
    cols.push_back (this);
  }

  void TableExprUDFNodeArray::disableApplySelection()
    { itsUDF->disableApplySelection(); }

  void TableExprUDFNodeArray::applySelection (const Vector<rownr_t>& rownrs)
    { itsUDF->applySelection (rownrs); }

  CountedPtr<TableExprGroupFuncBase> TableExprUDFNodeArray::makeGroupAggrFunc()
  {
    return new TableExprGroupNull(this);
  }

  MArray<Bool>     TableExprUDFNodeArray::getArrayBool    (const TableExprId& id)
    { return itsUDF->getArrayBool (id); }
  MArray<Int64>    TableExprUDFNodeArray::getArrayInt     (const TableExprId& id)
    { return itsUDF->getArrayInt (id); }
  MArray<Double>   TableExprUDFNodeArray::getArrayDouble  (const TableExprId& id)
    { return itsUDF->getArrayDouble (id); }
  MArray<DComplex> TableExprUDFNodeArray::getArrayDComplex(const TableExprId& id)
    { return itsUDF->getArrayDComplex (id); }
  MArray<String>   TableExprUDFNodeArray::getArrayString  (const TableExprId& id)
    { return itsUDF->getArrayString (id); }
  MArray<MVTime>   TableExprUDFNodeArray::getArrayDate    (const TableExprId& id)
    { return itsUDF->getArrayDate (id); }

} //# NAMESPACE CASACORE - END
