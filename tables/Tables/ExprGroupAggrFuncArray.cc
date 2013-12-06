//# ExprGroupAggrFuncArray.cc: The various array aggregation functions
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
#include <tables/Tables/ExprGroupAggrFuncArray.h>
#include <tables/Tables/ExprAggrNode.h>
#include <tables/Tables/ExprDerNode.h>
#include <tables/Tables/ExprNodeArray.h>
#include <tables/Tables/TableColumn.h>
#include <tables/Tables/TableError.h>
#include <casa/Utilities/Sort.h>
#include <limits>


namespace casa { //# NAMESPACE CASA - BEGIN


  TableExprGroupArrAny::TableExprGroupArrAny()
    : TableExprGroupFuncBool (False)
  {}
  TableExprGroupArrAny::~TableExprGroupArrAny()
  {}
  void TableExprGroupArrAny::apply (TableExprAggrNode& node,
                                    const TableExprId& id)
  {
    if (!itsValue) {
      Bool v = anyTrue (node.operand()->getArrayBool(id));
      if (v) itsValue = True;
    }
  }

  TableExprGroupArrAll::TableExprGroupArrAll()
    : TableExprGroupFuncBool (True)
  {}
  TableExprGroupArrAll::~TableExprGroupArrAll()
  {}
  void TableExprGroupArrAll::apply (TableExprAggrNode& node,
                                    const TableExprId& id)
  {
    if (itsValue) {
      Bool v = allTrue (node.operand()->getArrayBool(id));
      if (!v) itsValue = False;
    }
  }

  TableExprGroupArrNTrue::TableExprGroupArrNTrue()
  {}
  TableExprGroupArrNTrue::~TableExprGroupArrNTrue()
  {}
  void TableExprGroupArrNTrue::apply (TableExprAggrNode& node,
                                      const TableExprId& id)
  {
    itsValue += ntrue (node.operand()->getArrayBool(id));
  }

  TableExprGroupArrNFalse::TableExprGroupArrNFalse()
  {}
  TableExprGroupArrNFalse::~TableExprGroupArrNFalse()
  {}
  void TableExprGroupArrNFalse::apply (TableExprAggrNode& node,
                                       const TableExprId& id)
  {
    itsValue += nfalse (node.operand()->getArrayBool(id));
  }

  TableExprGroupMinArrInt::TableExprGroupMinArrInt()
    : TableExprGroupFuncInt (std::numeric_limits<Int64>::max())
  {}
  TableExprGroupMinArrInt::~TableExprGroupMinArrInt()
  {}
  void TableExprGroupMinArrInt::apply (TableExprAggrNode& node,
                                       const TableExprId& id)
  {
    Array<Int64> arr = node.operand()->getArrayInt(id);
    if (! arr.empty()) {
      Int64 v = min(arr);
      if (v<itsValue) itsValue = v;
    }
  }

  TableExprGroupMaxArrInt::TableExprGroupMaxArrInt()
    : TableExprGroupFuncInt (std::numeric_limits<Int64>::min())
  {}
  TableExprGroupMaxArrInt::~TableExprGroupMaxArrInt()
  {}
  void TableExprGroupMaxArrInt::apply (TableExprAggrNode& node,
                                    const TableExprId& id)
  {
    Array<Int64> arr = node.operand()->getArrayInt(id);
    if (! arr.empty()) {
      Int64 v = max(arr);
      if (v>itsValue) itsValue = v;
    }
  }

  TableExprGroupSumArrInt::TableExprGroupSumArrInt()
  {}
  TableExprGroupSumArrInt::~TableExprGroupSumArrInt()
  {}
  void TableExprGroupSumArrInt::apply (TableExprAggrNode& node,
                                    const TableExprId& id)
  {
    itsValue += sum(node.operand()->getArrayInt(id));
  }

  TableExprGroupProductArrInt::TableExprGroupProductArrInt()
    : TableExprGroupFuncInt (1)
  {}
  TableExprGroupProductArrInt::~TableExprGroupProductArrInt()
  {}
  void TableExprGroupProductArrInt::apply (TableExprAggrNode& node,
                                        const TableExprId& id)
  {
    Array<Int64> arr = node.operand()->getArrayInt(id);
    if (! arr.empty()) {
      itsValue *= product(arr);
    }
  }

  TableExprGroupSumSqrArrInt::TableExprGroupSumSqrArrInt()
  {}
  TableExprGroupSumSqrArrInt::~TableExprGroupSumSqrArrInt()
  {}
  void TableExprGroupSumSqrArrInt::apply (TableExprAggrNode& node,
                                       const TableExprId& id)
  {
    Array<Int64> arr = node.operand()->getArrayInt(id);
    itsValue += sum(arr*arr);
  }


  TableExprGroupMinArrDouble::TableExprGroupMinArrDouble()
    : TableExprGroupFuncDouble (std::numeric_limits<Double>::max())
  {}
  TableExprGroupMinArrDouble::~TableExprGroupMinArrDouble()
  {}
  void TableExprGroupMinArrDouble::apply (TableExprAggrNode& node,
                                          const TableExprId& id)
  {
    Array<Double> arr = node.operand()->getArrayDouble(id);
    if (! arr.empty()) {
      Double v = min(arr);
      if (v<itsValue) itsValue = v;
    }
  }

  TableExprGroupMaxArrDouble::TableExprGroupMaxArrDouble()
    : TableExprGroupFuncDouble (std::numeric_limits<Double>::min())
  {}
  TableExprGroupMaxArrDouble::~TableExprGroupMaxArrDouble()
  {}
  void TableExprGroupMaxArrDouble::apply (TableExprAggrNode& node,
                                          const TableExprId& id)
  {
    Array<Double> arr = node.operand()->getArrayDouble(id);
    if (! arr.empty()) {
      Double v = max(arr);
      if (v>itsValue) itsValue = v;
    }
  }

  TableExprGroupSumArrDouble::TableExprGroupSumArrDouble()
  {}
  TableExprGroupSumArrDouble::~TableExprGroupSumArrDouble()
  {}
  void TableExprGroupSumArrDouble::apply (TableExprAggrNode& node,
                                          const TableExprId& id)
  {
    itsValue += sum(node.operand()->getArrayDouble(id));
  }

  TableExprGroupProductArrDouble::TableExprGroupProductArrDouble()
    : TableExprGroupFuncDouble (1)
  {}
  TableExprGroupProductArrDouble::~TableExprGroupProductArrDouble()
  {}
  void TableExprGroupProductArrDouble::apply (TableExprAggrNode& node,
                                              const TableExprId& id)
  {
    Array<Double> arr = node.operand()->getArrayDouble(id);
    if (! arr.empty()) {
      itsValue *= product(arr);
    }
  }

  TableExprGroupSumSqrArrDouble::TableExprGroupSumSqrArrDouble()
  {}
  TableExprGroupSumSqrArrDouble::~TableExprGroupSumSqrArrDouble()
  {}
  void TableExprGroupSumSqrArrDouble::apply (TableExprAggrNode& node,
                                             const TableExprId& id)
  {
    Array<Double> arr = node.operand()->getArrayDouble(id);
    itsValue += sum(arr*arr);
  }

  TableExprGroupMeanArrDouble::TableExprGroupMeanArrDouble()
    : itsNr (0)
  {}
  TableExprGroupMeanArrDouble::~TableExprGroupMeanArrDouble()
  {}
  void TableExprGroupMeanArrDouble::apply (TableExprAggrNode& node,
                                           const TableExprId& id)
  {
    Array<Double> arr = node.operand()->getArrayDouble(id);
    itsValue += sum(arr);
    itsNr    += arr.size();
  }
  void TableExprGroupMeanArrDouble::finish()
  {
    if (itsNr > 0) {
      itsValue /= itsNr;
    }
  }

  TableExprGroupVarianceArrDouble::TableExprGroupVarianceArrDouble()
    : itsNr (0),
      itsM2 (0)
  {}
  TableExprGroupVarianceArrDouble::~TableExprGroupVarianceArrDouble()
  {}
  void TableExprGroupVarianceArrDouble::apply (TableExprAggrNode& node,
                                               const TableExprId& id)
  {
    // Calculate mean and variance in a running way using a
    // numerically stable algorithm
    // See en.wikipedia.org/wiki/Algorithms_for_calculating_variance
    Array<Double> arr = node.operand()->getArrayDouble(id);
    if (! arr.empty()) {
      Double meanv = mean(arr);
      Double m2    = 0;
      if (arr.size() > 1) {
        m2 = variance(arr, meanv) * (arr.size()-1);
      }
      Double delta = meanv - itsValue;   // itsValue contains the overall mean
      itsValue = (itsNr*itsValue + arr.size()*meanv) / (itsNr + arr.size());
      itsM2   += (m2 + delta*delta*itsNr*arr.size() / (itsNr + arr.size()));
      itsNr += arr.size();
    }
  }
  void TableExprGroupVarianceArrDouble::finish()
  {
    if (itsNr > 1) {
      itsValue = itsM2 / (itsNr-1);
    } else {
      itsValue = 0;
    }
  }

  TableExprGroupStdDevArrDouble::TableExprGroupStdDevArrDouble()
  {}
  TableExprGroupStdDevArrDouble::~TableExprGroupStdDevArrDouble()
  {}
  void TableExprGroupStdDevArrDouble::finish()
  {
    TableExprGroupVarianceArrDouble::finish();
    itsValue = sqrt(itsValue);
  }

  TableExprGroupRmsArrDouble::TableExprGroupRmsArrDouble()
    : itsNr (0)
  {}
  TableExprGroupRmsArrDouble::~TableExprGroupRmsArrDouble()
  {}
  void TableExprGroupRmsArrDouble::apply (TableExprAggrNode& node,
                                          const TableExprId& id)
  {
    Array<Double> arr = node.operand()->getArrayDouble(id);
    itsValue += sum(arr*arr);
    itsNr    += arr.size();
  }
  void TableExprGroupRmsArrDouble::finish()
  {
    if (itsNr > 0) {
      itsValue = sqrt(itsValue / itsNr);
    }
  }

  TableExprGroupFractileArrDouble::TableExprGroupFractileArrDouble(Double fraction)
    : itsFrac (fraction)
  {}
  TableExprGroupFractileArrDouble::~TableExprGroupFractileArrDouble()
  {}
  void TableExprGroupFractileArrDouble::apply (TableExprAggrNode& node,
                                               const TableExprId& id)
  {
    // Make contiguous if needed.
    Array<Double> arr = node.operand()->getArrayDouble(id);
    if (! arr.empty()) {
      if (arr.contiguousStorage()) {
        itsValues.push_back (arr);
      } else {
        Array<Double> tmp(arr.shape());
        tmp = arr;
        itsValues.push_back (tmp);
      }
    }
  }
  void TableExprGroupFractileArrDouble::finish()
  {
    // To get the fractile all arrays have to be combined.
    Int64 size = 0;
    for (vector<Array<Double> >::const_iterator iter = itsValues.begin();
         iter!=itsValues.end(); ++iter) {
      size += iter->size();
    }
    if (size > 0) {
      try {
        Vector<Double> vec(size);
        Double* data = vec.data();
        for (vector<Array<Double> >::const_iterator iter = itsValues.begin();
             iter!=itsValues.end(); ++iter) {
          objcopy (data, iter->data(), iter->size());
          data += iter->size();
        }
        itsValue = GenSort<Double>::kthLargest
          (vec.data(), size, static_cast<Int>((size-1)*itsFrac));
        // Remove all Arrays and keep the combined one (for possible ROLLUP).
        itsValues.resize (1);
        itsValues[0].reference (vec);
      } catch (std::exception& x) {
        throw TableInvExpr ("Cannot compute gfractile; "
                            "probably too many data - " + String(x.what()));
      }
    }
  }


  TableExprGroupSumArrDComplex::TableExprGroupSumArrDComplex()
  {}
  TableExprGroupSumArrDComplex::~TableExprGroupSumArrDComplex()
  {}
  void TableExprGroupSumArrDComplex::apply (TableExprAggrNode& node,
                                            const TableExprId& id)
  {
    itsValue += sum(node.operand()->getArrayDComplex(id));
  }

  TableExprGroupProductArrDComplex::TableExprGroupProductArrDComplex()
    : TableExprGroupFuncDComplex (DComplex(1,0))
  {}
  TableExprGroupProductArrDComplex::~TableExprGroupProductArrDComplex()
  {}
  void TableExprGroupProductArrDComplex::apply (TableExprAggrNode& node,
                                             const TableExprId& id)
  {
    Array<DComplex> arr = node.operand()->getArrayDComplex(id);
    if (! arr.empty()) {
      itsValue *= product(arr);
    }
  }

  TableExprGroupSumSqrArrDComplex::TableExprGroupSumSqrArrDComplex()
  {}
  TableExprGroupSumSqrArrDComplex::~TableExprGroupSumSqrArrDComplex()
  {}
  void TableExprGroupSumSqrArrDComplex::apply (TableExprAggrNode& node,
                                               const TableExprId& id)
  {
    Array<DComplex> arr = node.operand()->getArrayDComplex(id);
    itsValue += sum(arr*arr);
  }

  TableExprGroupMeanArrDComplex::TableExprGroupMeanArrDComplex()
    : itsNr (0)
  {}
  TableExprGroupMeanArrDComplex::~TableExprGroupMeanArrDComplex()
  {}
  void TableExprGroupMeanArrDComplex::apply (TableExprAggrNode& node,
                                             const TableExprId& id)
  {
    Array<DComplex> arr = node.operand()->getArrayDComplex(id);
    itsValue += sum(arr);
    itsNr    += arr.size();
  }
  void TableExprGroupMeanArrDComplex::finish()
  {
    if (itsNr > 0) {
      itsValue /= double(itsNr);
    }
  }


} //# NAMESPACE CASA - END
