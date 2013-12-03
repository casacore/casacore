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
#include <tables/Tables/ExprGroupAggrFunc.h>
#include <tables/Tables/ExprAggrNode.h>
#include <tables/Tables/ExprDerNode.h>
#include <tables/Tables/ExprNodeArray.h>
#include <tables/Tables/TableColumn.h>
#include <tables/Tables/TableError.h>
#include <casa/Utilities/Sort.h>
#include <limits>


namespace casa { //# NAMESPACE CASA - BEGIN


  TableExprGroupCountAll::TableExprGroupCountAll()
  {}
  TableExprGroupCountAll::~TableExprGroupCountAll()
  {}
  void TableExprGroupCountAll::apply (TableExprAggrNode&, const TableExprId&)
  {
    itsValue++;
  }

  TableExprGroupCount::TableExprGroupCount(TableExprAggrNode& node)
    : itsColumn (0)
  {
    // Get the TableColumn object from the argument of the gcount node.
    itsColumn = dynamic_cast<TableExprNodeArrayColumn*>(node.operand());
    if (!itsColumn) {
      TableExprNodeColumn* col =
        dynamic_cast<TableExprNodeColumn*>(node.operand());
      if (!col) {
        throw TableInvExpr("Argument of GCOUNT function must be a column");
      }
    }
  }
  TableExprGroupCount::~TableExprGroupCount()
  {}
  void TableExprGroupCount::apply (TableExprAggrNode&,
                                   const TableExprId& id)
  {
    // Add if this row contains a value.
    if (!itsColumn  ||  itsColumn->isDefined(id.rownr())) {
      itsValue++;
    }
  }

  TableExprGroupAny::TableExprGroupAny()
    : TableExprGroupFuncBool (False)
  {}
  TableExprGroupAny::~TableExprGroupAny()
  {}
  void TableExprGroupAny::apply (TableExprAggrNode& node,
                                 const TableExprId& id)
  {
    Bool v = node.operand()->getBool(id);
    if (v) itsValue = True;
  }

  TableExprGroupAll::TableExprGroupAll()
    : TableExprGroupFuncBool (True)
  {}
  TableExprGroupAll::~TableExprGroupAll()
  {}
  void TableExprGroupAll::apply (TableExprAggrNode& node,
                                 const TableExprId& id)
  {
    Bool v = node.operand()->getBool(id);
    if (!v) itsValue = False;
  }

  TableExprGroupNTrue::TableExprGroupNTrue()
  {}
  TableExprGroupNTrue::~TableExprGroupNTrue()
  {}
  void TableExprGroupNTrue::apply (TableExprAggrNode& node,
                                   const TableExprId& id)
  {
    Bool v = node.operand()->getBool(id);
    if (v) itsValue++;
  }

  TableExprGroupNFalse::TableExprGroupNFalse()
  {}
  TableExprGroupNFalse::~TableExprGroupNFalse()
  {}
  void TableExprGroupNFalse::apply (TableExprAggrNode& node,
                                    const TableExprId& id)
  {
    Bool v = node.operand()->getBool(id);
    if (!v) itsValue++;
  }

  TableExprGroupMinInt::TableExprGroupMinInt()
    : TableExprGroupFuncInt (std::numeric_limits<Int64>::max())
  {}
  TableExprGroupMinInt::~TableExprGroupMinInt()
  {}
  void TableExprGroupMinInt::apply (TableExprAggrNode& node,
                                    const TableExprId& id)
  {
    Int64 v = node.operand()->getInt(id);
    if (v<itsValue) itsValue = v;
  }

  TableExprGroupMaxInt::TableExprGroupMaxInt()
    : TableExprGroupFuncInt (std::numeric_limits<Int64>::min())
  {}
  TableExprGroupMaxInt::~TableExprGroupMaxInt()
  {}
  void TableExprGroupMaxInt::apply (TableExprAggrNode& node,
                                    const TableExprId& id)
  {
    Int64 v = node.operand()->getInt(id);
    if (v>itsValue) itsValue = v;
  }

  TableExprGroupSumInt::TableExprGroupSumInt()
  {}
  TableExprGroupSumInt::~TableExprGroupSumInt()
  {}
  void TableExprGroupSumInt::apply (TableExprAggrNode& node,
                                    const TableExprId& id)
  {
    itsValue += node.operand()->getInt(id);
  }

  TableExprGroupProductInt::TableExprGroupProductInt()
    : TableExprGroupFuncInt (1)
  {}
  TableExprGroupProductInt::~TableExprGroupProductInt()
  {}
  void TableExprGroupProductInt::apply (TableExprAggrNode& node,
                                        const TableExprId& id)
  {
    itsValue *= node.operand()->getInt(id);
  }

  TableExprGroupSumSqrInt::TableExprGroupSumSqrInt()
  {}
  TableExprGroupSumSqrInt::~TableExprGroupSumSqrInt()
  {}
  void TableExprGroupSumSqrInt::apply (TableExprAggrNode& node,
                                       const TableExprId& id)
  {
    Int64 v = node.operand()->getInt(id);
    itsValue += v*v;
  }


  TableExprGroupMinDouble::TableExprGroupMinDouble()
    : TableExprGroupFuncDouble (std::numeric_limits<Double>::max())
  {}
  TableExprGroupMinDouble::~TableExprGroupMinDouble()
  {}
  void TableExprGroupMinDouble::apply (TableExprAggrNode& node,
                                       const TableExprId& id)
  {
    Double v = node.operand()->getDouble(id);
    if (v<itsValue) itsValue = v;
  }

  TableExprGroupMaxDouble::TableExprGroupMaxDouble()
    : TableExprGroupFuncDouble (std::numeric_limits<Double>::min())
  {}
  TableExprGroupMaxDouble::~TableExprGroupMaxDouble()
  {}
  void TableExprGroupMaxDouble::apply (TableExprAggrNode& node,
                                       const TableExprId& id)
  {
    Double v = node.operand()->getDouble(id);
    if (v>itsValue) itsValue = v;
  }

  TableExprGroupSumDouble::TableExprGroupSumDouble()
  {}
  TableExprGroupSumDouble::~TableExprGroupSumDouble()
  {}
  void TableExprGroupSumDouble::apply (TableExprAggrNode& node,
                                       const TableExprId& id)
  {
    itsValue += node.operand()->getDouble(id);
  }

  TableExprGroupProductDouble::TableExprGroupProductDouble()
    : TableExprGroupFuncDouble (1)
  {}
  TableExprGroupProductDouble::~TableExprGroupProductDouble()
  {}
  void TableExprGroupProductDouble::apply (TableExprAggrNode& node,
                                           const TableExprId& id)
  {
    itsValue *= node.operand()->getDouble(id);
  }

  TableExprGroupSumSqrDouble::TableExprGroupSumSqrDouble()
  {}
  TableExprGroupSumSqrDouble::~TableExprGroupSumSqrDouble()
  {}
  void TableExprGroupSumSqrDouble::apply (TableExprAggrNode& node,
                                          const TableExprId& id)
  {
    Double v = node.operand()->getDouble(id);
    itsValue += v*v;
  }

  TableExprGroupMeanDouble::TableExprGroupMeanDouble()
    : itsNr (0)
  {}
  TableExprGroupMeanDouble::~TableExprGroupMeanDouble()
  {}
  void TableExprGroupMeanDouble::apply (TableExprAggrNode& node,
                                        const TableExprId& id)
  {
    itsValue += node.operand()->getDouble(id);
    itsNr++;
  }
  void TableExprGroupMeanDouble::finish()
  {
    if (itsNr > 0) {
      itsValue /= itsNr;
    }
  }

  TableExprGroupVarianceDouble::TableExprGroupVarianceDouble()
    : itsNr (0),
      itsM2 (0)
  {}
  TableExprGroupVarianceDouble::~TableExprGroupVarianceDouble()
  {}
  void TableExprGroupVarianceDouble::apply (TableExprAggrNode& node,
                                            const TableExprId& id)
  {
    // Calculate mean and variance in a running way using a
    // numerically stable algorithm
    // See en.wikipedia.org/wiki/Algorithms_for_calculating_variance
    itsNr++;
    Double v = node.operand()->getDouble(id);
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

  TableExprGroupStdDevDouble::TableExprGroupStdDevDouble()
  {}
  TableExprGroupStdDevDouble::~TableExprGroupStdDevDouble()
  {}
  void TableExprGroupStdDevDouble::finish()
  {
    TableExprGroupVarianceDouble::finish();
    itsValue = sqrt(itsValue);
  }

  TableExprGroupRmsDouble::TableExprGroupRmsDouble()
    : itsNr (0)
  {}
  TableExprGroupRmsDouble::~TableExprGroupRmsDouble()
  {}
  void TableExprGroupRmsDouble::apply (TableExprAggrNode& node,
                                       const TableExprId& id)
  {
    Double v = node.operand()->getDouble(id);
    itsValue += v*v;
    itsNr++;
  }
  void TableExprGroupRmsDouble::finish()
  {
    if (itsNr > 0) {
      itsValue = sqrt(itsValue / itsNr);
    }
  }

  TableExprGroupFractileDouble::TableExprGroupFractileDouble(Double fraction)
    : itsFrac (fraction)
  {}
  TableExprGroupFractileDouble::~TableExprGroupFractileDouble()
  {}
  void TableExprGroupFractileDouble::apply (TableExprAggrNode& node,
                                            const TableExprId& id)
  {
    itsValues.push_back (node.operand()->getDouble(id));
  }
  void TableExprGroupFractileDouble::finish()
  {
    if (! itsValues.empty()) {
      itsValue = GenSort<Double>::kthLargest
        (&(itsValues[0]), itsValues.size(),
         static_cast<Int>(itsValues.size()*itsFrac + 0.5));
    }
  }


  TableExprGroupSumDComplex::TableExprGroupSumDComplex()
  {}
  TableExprGroupSumDComplex::~TableExprGroupSumDComplex()
  {}
  void TableExprGroupSumDComplex::apply (TableExprAggrNode& node,
                                         const TableExprId& id)
  {
    itsValue += node.operand()->getDComplex(id);
  }

  TableExprGroupProductDComplex::TableExprGroupProductDComplex()
    : TableExprGroupFuncDComplex (DComplex(1,0))
  {}
  TableExprGroupProductDComplex::~TableExprGroupProductDComplex()
  {}
  void TableExprGroupProductDComplex::apply (TableExprAggrNode& node,
                                             const TableExprId& id)
  {
    itsValue *= node.operand()->getDComplex(id);
  }

  TableExprGroupSumSqrDComplex::TableExprGroupSumSqrDComplex()
  {}
  TableExprGroupSumSqrDComplex::~TableExprGroupSumSqrDComplex()
  {}
  void TableExprGroupSumSqrDComplex::apply (TableExprAggrNode& node,
                                            const TableExprId& id)
  {
    DComplex v = node.operand()->getDComplex(id);
    itsValue += v*v;
  }

  TableExprGroupMeanDComplex::TableExprGroupMeanDComplex()
    : itsNr (0)
  {}
  TableExprGroupMeanDComplex::~TableExprGroupMeanDComplex()
  {}
  void TableExprGroupMeanDComplex::apply (TableExprAggrNode& node,
                                          const TableExprId& id)
  {
    itsValue += node.operand()->getDComplex(id);
    itsNr++;
  }
  void TableExprGroupMeanDComplex::finish()
  {
    if (itsNr > 0) {
      itsValue /= double(itsNr);
    }
  }


} //# NAMESPACE CASA - END
