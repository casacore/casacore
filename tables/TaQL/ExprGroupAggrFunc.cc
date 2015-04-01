//# ExprGroupAggrFunc.cc: Classes for TaQL's GROUPBY clause
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

//# Includes
#include <casacore/tables/TaQL/ExprGroupAggrFunc.h>
#include <casacore/tables/TaQL/ExprAggrNode.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/tables/TaQL/ExprNodeArray.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Utilities/Sort.h>
#include <limits>


namespace casacore { //# NAMESPACE CASACORE - BEGIN


  TableExprGroupCountAll::TableExprGroupCountAll (TableExprNodeRep* node)
    : TableExprGroupFuncInt (node)
  {}
  TableExprGroupCountAll::~TableExprGroupCountAll()
  {}
  void TableExprGroupCountAll::apply (const TableExprId&)
  {
    itsValue++;
  }

  TableExprGroupCount::TableExprGroupCount (TableExprNodeRep* node)
    : TableExprGroupFuncInt (node),
      itsColumn (0)
  {
    // Get the TableColumn object from the argument of the gcount node.
    itsColumn = dynamic_cast<TableExprNodeArrayColumn*>(itsOperand);
    if (!itsColumn) {
      TableExprNodeColumn* col =
        dynamic_cast<TableExprNodeColumn*>(itsOperand);
      if (!col) {
        throw TableInvExpr("Argument of GCOUNT function must be a column");
      }
    }
  }
  TableExprGroupCount::~TableExprGroupCount()
  {}
  void TableExprGroupCount::apply (const TableExprId& id)
  {
    // Add if this row contains a value.
    if (!itsColumn  ||  itsColumn->isDefined(id.rownr())) {
      itsValue++;
    }
  }

  TableExprGroupAny::TableExprGroupAny (TableExprNodeRep* node)
    : TableExprGroupFuncBool (node, False)
  {}
  TableExprGroupAny::~TableExprGroupAny()
  {}
  void TableExprGroupAny::apply (const TableExprId& id)
  {
    Bool v = itsOperand->getBool(id);
    if (v) itsValue = True;
  }

  TableExprGroupAll::TableExprGroupAll (TableExprNodeRep* node)
    : TableExprGroupFuncBool (node, True)
  {}
  TableExprGroupAll::~TableExprGroupAll()
  {}
  void TableExprGroupAll::apply (const TableExprId& id)
  {
    Bool v = itsOperand->getBool(id);
    if (!v) itsValue = False;
  }

  TableExprGroupNTrue::TableExprGroupNTrue (TableExprNodeRep* node)
    : TableExprGroupFuncInt (node)
  {}
  TableExprGroupNTrue::~TableExprGroupNTrue()
  {}
  void TableExprGroupNTrue::apply (const TableExprId& id)
  {
    Bool v = itsOperand->getBool(id);
    if (v) itsValue++;
  }

  TableExprGroupNFalse::TableExprGroupNFalse (TableExprNodeRep* node)
    : TableExprGroupFuncInt (node)
  {}
  TableExprGroupNFalse::~TableExprGroupNFalse()
  {}
  void TableExprGroupNFalse::apply (const TableExprId& id)
  {
    Bool v = itsOperand->getBool(id);
    if (!v) itsValue++;
  }

  TableExprGroupMinInt::TableExprGroupMinInt (TableExprNodeRep* node)
    : TableExprGroupFuncInt (node, std::numeric_limits<Int64>::max())
  {}
  TableExprGroupMinInt::~TableExprGroupMinInt()
  {}
  void TableExprGroupMinInt::apply (const TableExprId& id)
  {
    Int64 v = itsOperand->getInt(id);
    if (v<itsValue) itsValue = v;
  }

  TableExprGroupMaxInt::TableExprGroupMaxInt (TableExprNodeRep* node)
    : TableExprGroupFuncInt (node, std::numeric_limits<Int64>::min())
  {}
  TableExprGroupMaxInt::~TableExprGroupMaxInt()
  {}
  void TableExprGroupMaxInt::apply (const TableExprId& id)
  {
    Int64 v = itsOperand->getInt(id);
    if (v>itsValue) itsValue = v;
  }

  TableExprGroupSumInt::TableExprGroupSumInt(TableExprNodeRep* node)
    : TableExprGroupFuncInt (node)
  {}
  TableExprGroupSumInt::~TableExprGroupSumInt()
  {}
  void TableExprGroupSumInt::apply (const TableExprId& id)
  {
    itsValue += itsOperand->getInt(id);
  }

  TableExprGroupProductInt::TableExprGroupProductInt(TableExprNodeRep* node)
    : TableExprGroupFuncInt (node, 1)
  {}
  TableExprGroupProductInt::~TableExprGroupProductInt()
  {}
  void TableExprGroupProductInt::apply (const TableExprId& id)
  {
    itsValue *= itsOperand->getInt(id);
  }

  TableExprGroupSumSqrInt::TableExprGroupSumSqrInt(TableExprNodeRep* node)
    : TableExprGroupFuncInt (node)
  {}
  TableExprGroupSumSqrInt::~TableExprGroupSumSqrInt()
  {}
  void TableExprGroupSumSqrInt::apply (const TableExprId& id)
  {
    Int64 v = itsOperand->getInt(id);
    itsValue += v*v;
  }


  TableExprGroupMinDouble::TableExprGroupMinDouble(TableExprNodeRep* node)
    : TableExprGroupFuncDouble (node, std::numeric_limits<Double>::max())
  {}
  TableExprGroupMinDouble::~TableExprGroupMinDouble()
  {}
  void TableExprGroupMinDouble::apply (const TableExprId& id)
  {
    Double v = itsOperand->getDouble(id);
    if (v<itsValue) itsValue = v;
  }

  TableExprGroupMaxDouble::TableExprGroupMaxDouble(TableExprNodeRep* node)
    : TableExprGroupFuncDouble (node, std::numeric_limits<Double>::min())
  {}
  TableExprGroupMaxDouble::~TableExprGroupMaxDouble()
  {}
  void TableExprGroupMaxDouble::apply (const TableExprId& id)
  {
    Double v = itsOperand->getDouble(id);
    if (v>itsValue) itsValue = v;
  }

  TableExprGroupSumDouble::TableExprGroupSumDouble(TableExprNodeRep* node)
    : TableExprGroupFuncDouble (node)
  {}
  TableExprGroupSumDouble::~TableExprGroupSumDouble()
  {}
  void TableExprGroupSumDouble::apply (const TableExprId& id)
  {
    itsValue += itsOperand->getDouble(id);
  }

  TableExprGroupProductDouble::TableExprGroupProductDouble(TableExprNodeRep* node)
    : TableExprGroupFuncDouble (node, 1)
  {}
  TableExprGroupProductDouble::~TableExprGroupProductDouble()
  {}
  void TableExprGroupProductDouble::apply (const TableExprId& id)
  {
    itsValue *= itsOperand->getDouble(id);
  }

  TableExprGroupSumSqrDouble::TableExprGroupSumSqrDouble(TableExprNodeRep* node)
    : TableExprGroupFuncDouble (node)
  {}
  TableExprGroupSumSqrDouble::~TableExprGroupSumSqrDouble()
  {}
  void TableExprGroupSumSqrDouble::apply (const TableExprId& id)
  {
    Double v = itsOperand->getDouble(id);
    itsValue += v*v;
  }

  TableExprGroupMeanDouble::TableExprGroupMeanDouble(TableExprNodeRep* node)
    : TableExprGroupFuncDouble (node),
      itsNr (0)
  {}
  TableExprGroupMeanDouble::~TableExprGroupMeanDouble()
  {}
  void TableExprGroupMeanDouble::apply (const TableExprId& id)
  {
    itsValue += itsOperand->getDouble(id);
    itsNr++;
  }
  void TableExprGroupMeanDouble::finish()
  {
    if (itsNr > 0) {
      itsValue /= itsNr;
    }
  }

  TableExprGroupVarianceDouble::TableExprGroupVarianceDouble(TableExprNodeRep* node)
    : TableExprGroupFuncDouble (node),
      itsNr (0),
      itsM2 (0)
  {}
  TableExprGroupVarianceDouble::~TableExprGroupVarianceDouble()
  {}
  void TableExprGroupVarianceDouble::apply (const TableExprId& id)
  {
    // Calculate mean and variance in a running way using a
    // numerically stable algorithm
    // See en.wikipedia.org/wiki/Algorithms_for_calculating_variance
    itsNr++;
    Double v = itsOperand->getDouble(id);
    Double delta = v - itsValue;   // itsValue contains the mean
    itsValue += delta/itsNr;
    itsM2    += delta*(v-itsValue);
  }
  void TableExprGroupVarianceDouble::finish()
  {
    if (itsNr > 1) {
      itsValue = itsM2 / (itsNr-1);
    } else {
      itsValue = 0;
    }
  }

  TableExprGroupStdDevDouble::TableExprGroupStdDevDouble(TableExprNodeRep* node)
    : TableExprGroupVarianceDouble (node)
  {}
  TableExprGroupStdDevDouble::~TableExprGroupStdDevDouble()
  {}
  void TableExprGroupStdDevDouble::finish()
  {
    TableExprGroupVarianceDouble::finish();
    itsValue = sqrt(itsValue);
  }

  TableExprGroupRmsDouble::TableExprGroupRmsDouble(TableExprNodeRep* node)
    : TableExprGroupFuncDouble (node),
      itsNr (0)
  {}
  TableExprGroupRmsDouble::~TableExprGroupRmsDouble()
  {}
  void TableExprGroupRmsDouble::apply (const TableExprId& id)
  {
    Double v = itsOperand->getDouble(id);
    itsValue += v*v;
    itsNr++;
  }
  void TableExprGroupRmsDouble::finish()
  {
    if (itsNr > 0) {
      itsValue = sqrt(itsValue / itsNr);
    }
  }

  TableExprGroupFractileDouble::TableExprGroupFractileDouble(TableExprNodeRep* node,
                                                             Double fraction)
    : TableExprGroupFuncDouble (node),
      itsFrac (fraction)
  {}
  TableExprGroupFractileDouble::~TableExprGroupFractileDouble()
  {}
  Bool TableExprGroupFractileDouble::isLazy() const
  {
    return True;
  }
  void TableExprGroupFractileDouble::apply (const TableExprId&)
  {}
  Double TableExprGroupFractileDouble::getDouble (const vector<TableExprId>& ids)
  {
    vector<Double> values;
    values.reserve (ids.size());
    for (uInt i=0; i<ids.size(); ++i) {
      values.push_back (itsOperand->getDouble (ids[i]));
    }
    if (! values.empty()) {
      return GenSort<Double>::kthLargest
        (&(values[0]), values.size(),
         static_cast<Int>((values.size() - 1)*itsFrac + 0.001));
    }
    return 0;
  }


  TableExprGroupSumDComplex::TableExprGroupSumDComplex(TableExprNodeRep* node)
    : TableExprGroupFuncDComplex (node)
  {}
  TableExprGroupSumDComplex::~TableExprGroupSumDComplex()
  {}
  void TableExprGroupSumDComplex::apply (const TableExprId& id)
  {
    itsValue += itsOperand->getDComplex(id);
  }

  TableExprGroupProductDComplex::TableExprGroupProductDComplex(TableExprNodeRep* node)
    : TableExprGroupFuncDComplex (node, DComplex(1,0))
  {}
  TableExprGroupProductDComplex::~TableExprGroupProductDComplex()
  {}
  void TableExprGroupProductDComplex::apply (const TableExprId& id)
  {
    itsValue *= itsOperand->getDComplex(id);
  }

  TableExprGroupSumSqrDComplex::TableExprGroupSumSqrDComplex(TableExprNodeRep* node)
    : TableExprGroupFuncDComplex (node)
  {}
  TableExprGroupSumSqrDComplex::~TableExprGroupSumSqrDComplex()
  {}
  void TableExprGroupSumSqrDComplex::apply (const TableExprId& id)
  {
    DComplex v = itsOperand->getDComplex(id);
    itsValue += v*v;
  }

  TableExprGroupMeanDComplex::TableExprGroupMeanDComplex(TableExprNodeRep* node)
    : TableExprGroupFuncDComplex (node),
      itsNr (0)
  {}
  TableExprGroupMeanDComplex::~TableExprGroupMeanDComplex()
  {}
  void TableExprGroupMeanDComplex::apply (const TableExprId& id)
  {
    itsValue += itsOperand->getDComplex(id);
    itsNr++;
  }
  void TableExprGroupMeanDComplex::finish()
  {
    if (itsNr > 0) {
      itsValue /= double(itsNr);
    }
  }


} //# NAMESPACE CASACORE - END
