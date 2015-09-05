//# ExprGroupAggrFuncArray.cc: The various array reduction aggregation functions
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
#include <casacore/tables/TaQL/ExprGroupAggrFuncArray.h>
#include <casacore/tables/TaQL/ExprAggrNode.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/tables/TaQL/ExprNodeArray.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Utilities/Sort.h>
#include <limits>


namespace casacore { //# NAMESPACE CASACORE - BEGIN


  TableExprGroupArrayAny::TableExprGroupArrayAny(TableExprNodeRep* node)
    : TableExprGroupFuncBool (node, False)
  {}
  TableExprGroupArrayAny::~TableExprGroupArrayAny()
  {}
  void TableExprGroupArrayAny::apply (const TableExprId& id)
  {
    if (!itsValue) {
      Bool v = anyTrue (itsOperand->getArrayBool(id));
      if (v) itsValue = True;
    }
  }

  TableExprGroupArrayAll::TableExprGroupArrayAll(TableExprNodeRep* node)
    : TableExprGroupFuncBool (node, True)
  {}
  TableExprGroupArrayAll::~TableExprGroupArrayAll()
  {}
  void TableExprGroupArrayAll::apply (const TableExprId& id)
  {
    if (itsValue) {
      Bool v = allTrue (itsOperand->getArrayBool(id));
      if (!v) itsValue = False;
    }
  }

  TableExprGroupArrayNTrue::TableExprGroupArrayNTrue(TableExprNodeRep* node)
    : TableExprGroupFuncInt (node)
  {}
  TableExprGroupArrayNTrue::~TableExprGroupArrayNTrue()
  {}
  void TableExprGroupArrayNTrue::apply (const TableExprId& id)
  {
    itsValue += ntrue (itsOperand->getArrayBool(id));
  }

  TableExprGroupArrayNFalse::TableExprGroupArrayNFalse(TableExprNodeRep* node)
    : TableExprGroupFuncInt (node)
  {}
  TableExprGroupArrayNFalse::~TableExprGroupArrayNFalse()
  {}
  void TableExprGroupArrayNFalse::apply (const TableExprId& id)
  {
    itsValue += nfalse (itsOperand->getArrayBool(id));
  }

  TableExprGroupMinArrayInt::TableExprGroupMinArrayInt(TableExprNodeRep* node)
    : TableExprGroupFuncInt (node, std::numeric_limits<Int64>::max())
  {}
  TableExprGroupMinArrayInt::~TableExprGroupMinArrayInt()
  {}
  void TableExprGroupMinArrayInt::apply (const TableExprId& id)
  {
    Array<Int64> arr = itsOperand->getArrayInt(id);
    if (! arr.empty()) {
      Int64 v = min(arr);
      if (v<itsValue) itsValue = v;
    }
  }

  TableExprGroupMaxArrayInt::TableExprGroupMaxArrayInt(TableExprNodeRep* node)
    : TableExprGroupFuncInt (node, std::numeric_limits<Int64>::min())
  {}
  TableExprGroupMaxArrayInt::~TableExprGroupMaxArrayInt()
  {}
  void TableExprGroupMaxArrayInt::apply (const TableExprId& id)
  {
    Array<Int64> arr = itsOperand->getArrayInt(id);
    if (! arr.empty()) {
      Int64 v = max(arr);
      if (v>itsValue) itsValue = v;
    }
  }

  TableExprGroupSumArrayInt::TableExprGroupSumArrayInt(TableExprNodeRep* node)
    : TableExprGroupFuncInt (node)
  {}
  TableExprGroupSumArrayInt::~TableExprGroupSumArrayInt()
  {}
  void TableExprGroupSumArrayInt::apply (const TableExprId& id)
  {
    itsValue += sum(itsOperand->getArrayInt(id));
  }

  TableExprGroupProductArrayInt::TableExprGroupProductArrayInt(TableExprNodeRep* node)
    : TableExprGroupFuncInt (node, 1)
  {}
  TableExprGroupProductArrayInt::~TableExprGroupProductArrayInt()
  {}
  void TableExprGroupProductArrayInt::apply (const TableExprId& id)
  {
    Array<Int64> arr = itsOperand->getArrayInt(id);
    if (! arr.empty()) {
      itsValue *= product(arr);
    }
  }

  TableExprGroupSumSqrArrayInt::TableExprGroupSumSqrArrayInt(TableExprNodeRep* node)
    : TableExprGroupFuncInt (node)
  {}
  TableExprGroupSumSqrArrayInt::~TableExprGroupSumSqrArrayInt()
  {}
  void TableExprGroupSumSqrArrayInt::apply (const TableExprId& id)
  {
    Array<Int64> arr = itsOperand->getArrayInt(id);
    itsValue += sum(arr*arr);
  }


  TableExprGroupMinArrayDouble::TableExprGroupMinArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncDouble (node, std::numeric_limits<Double>::max())
  {}
  TableExprGroupMinArrayDouble::~TableExprGroupMinArrayDouble()
  {}
  void TableExprGroupMinArrayDouble::apply (const TableExprId& id)
  {
    Array<Double> arr = itsOperand->getArrayDouble(id);
    if (! arr.empty()) {
      Double v = min(arr);
      if (v<itsValue) itsValue = v;
    }
  }

  TableExprGroupMaxArrayDouble::TableExprGroupMaxArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncDouble (node, std::numeric_limits<Double>::min())
  {}
  TableExprGroupMaxArrayDouble::~TableExprGroupMaxArrayDouble()
  {}
  void TableExprGroupMaxArrayDouble::apply (const TableExprId& id)
  {
    Array<Double> arr = itsOperand->getArrayDouble(id);
    if (! arr.empty()) {
      Double v = max(arr);
      if (v>itsValue) itsValue = v;
    }
  }

  TableExprGroupSumArrayDouble::TableExprGroupSumArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncDouble (node)
  {}
  TableExprGroupSumArrayDouble::~TableExprGroupSumArrayDouble()
  {}
  void TableExprGroupSumArrayDouble::apply (const TableExprId& id)
  {
    itsValue += sum(itsOperand->getArrayDouble(id));
  }

  TableExprGroupProductArrayDouble::TableExprGroupProductArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncDouble (node, 1)
  {}
  TableExprGroupProductArrayDouble::~TableExprGroupProductArrayDouble()
  {}
  void TableExprGroupProductArrayDouble::apply (const TableExprId& id)
  {
    Array<Double> arr = itsOperand->getArrayDouble(id);
    if (! arr.empty()) {
      itsValue *= product(arr);
    }
  }

  TableExprGroupSumSqrArrayDouble::TableExprGroupSumSqrArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncDouble (node)
  {}
  TableExprGroupSumSqrArrayDouble::~TableExprGroupSumSqrArrayDouble()
  {}
  void TableExprGroupSumSqrArrayDouble::apply (const TableExprId& id)
  {
    Array<Double> arr = itsOperand->getArrayDouble(id);
    itsValue += sum(arr*arr);
  }

  TableExprGroupMeanArrayDouble::TableExprGroupMeanArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncDouble (node),
      itsNr (0)
  {}
  TableExprGroupMeanArrayDouble::~TableExprGroupMeanArrayDouble()
  {}
  void TableExprGroupMeanArrayDouble::apply (const TableExprId& id)
  {
    Array<Double> arr = itsOperand->getArrayDouble(id);
    itsValue += sum(arr);
    itsNr    += arr.size();
  }
  void TableExprGroupMeanArrayDouble::finish()
  {
    if (itsNr > 0) {
      itsValue /= itsNr;
    }
  }

  TableExprGroupVarianceArrayDouble::TableExprGroupVarianceArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncDouble (node),
      itsNr (0),
      itsM2 (0)
  {}
  TableExprGroupVarianceArrayDouble::~TableExprGroupVarianceArrayDouble()
  {}
  void TableExprGroupVarianceArrayDouble::apply (const TableExprId& id)
  {
    // Calculate mean and variance in a running way using a
    // numerically stable algorithm
    // See en.wikipedia.org/wiki/Algorithms_for_calculating_variance
    Array<Double> arr = itsOperand->getArrayDouble(id);
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
  void TableExprGroupVarianceArrayDouble::finish()
  {
    if (itsNr > 1) {
      itsValue = itsM2 / (itsNr-1);
    } else {
      itsValue = 0;
    }
  }

  TableExprGroupStdDevArrayDouble::TableExprGroupStdDevArrayDouble(TableExprNodeRep* node)
    : TableExprGroupVarianceArrayDouble (node)
  {}
  TableExprGroupStdDevArrayDouble::~TableExprGroupStdDevArrayDouble()
  {}
  void TableExprGroupStdDevArrayDouble::finish()
  {
    TableExprGroupVarianceArrayDouble::finish();
    itsValue = sqrt(itsValue);
  }

  TableExprGroupRmsArrayDouble::TableExprGroupRmsArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncDouble (node),
      itsNr (0)
  {}
  TableExprGroupRmsArrayDouble::~TableExprGroupRmsArrayDouble()
  {}
  void TableExprGroupRmsArrayDouble::apply (const TableExprId& id)
  {
    Array<Double> arr = itsOperand->getArrayDouble(id);
    itsValue += sum(arr*arr);
    itsNr    += arr.size();
  }
  void TableExprGroupRmsArrayDouble::finish()
  {
    if (itsNr > 0) {
      itsValue = sqrt(itsValue / itsNr);
    }
  }

  TableExprGroupFractileArrayDouble::TableExprGroupFractileArrayDouble(TableExprNodeRep* node, Double fraction)
    : TableExprGroupFuncDouble (node),
      itsFrac (fraction)
  {}
  TableExprGroupFractileArrayDouble::~TableExprGroupFractileArrayDouble()
  {}
  Bool TableExprGroupFractileArrayDouble::isLazy() const
  {
    return True;
  }
  void TableExprGroupFractileArrayDouble::apply (const TableExprId&)
  {}
  Double TableExprGroupFractileArrayDouble::getDouble
  (const vector<TableExprId>& ids)
  {
    try {
      if (ids.empty()) {
        return 0;
      }
      // All arrays have to be combined in a single vector.
      // Get first array to estimate the total size.
      Array<Double> arr = itsOperand->getArrayDouble(ids[0]);
      vector<Double> values;
      values.reserve (ids.size() * arr.size());
      copyArray (arr, values);
      for (uInt i=1; i<ids.size(); ++i) {
        // Get value and make contiguous if needed.
        Array<Double> arr = itsOperand->getArrayDouble(ids[i]);
        copyArray (arr, values);
      }
      return GenSort<Double>::kthLargest
        (&(values[0]), values.size(),
         static_cast<Int>((values.size() - 1)*itsFrac + 0.001));
    } catch (const std::exception& x) {
      throw TableInvExpr ("Cannot compute gfractile; "
                          "probably too many data - " + String(x.what()));
    }
  }
  void TableExprGroupFractileArrayDouble::copyArray
  (const Array<Double>& arr, vector<Double>& buffer) const
  {
    // Array does not need to be contiguous, so use iterator.
    Array<Double>::const_iterator iterEnd = arr.end();
    for (Array<Double>::const_iterator iter = arr.begin();
         iter!=iterEnd; ++iter) {
      buffer.push_back (*iter);
    }
  }


  TableExprGroupSumArrayDComplex::TableExprGroupSumArrayDComplex(TableExprNodeRep* node)
    : TableExprGroupFuncDComplex (node)
  {}
  TableExprGroupSumArrayDComplex::~TableExprGroupSumArrayDComplex()
  {}
  void TableExprGroupSumArrayDComplex::apply (const TableExprId& id)
  {
    itsValue += sum(itsOperand->getArrayDComplex(id));
  }

  TableExprGroupProductArrayDComplex::TableExprGroupProductArrayDComplex(TableExprNodeRep* node)
    : TableExprGroupFuncDComplex (node, DComplex(1,0))
  {}
  TableExprGroupProductArrayDComplex::~TableExprGroupProductArrayDComplex()
  {}
  void TableExprGroupProductArrayDComplex::apply (const TableExprId& id)
  {
    Array<DComplex> arr = itsOperand->getArrayDComplex(id);
    if (! arr.empty()) {
      itsValue *= product(arr);
    }
  }

  TableExprGroupSumSqrArrayDComplex::TableExprGroupSumSqrArrayDComplex(TableExprNodeRep* node)
    : TableExprGroupFuncDComplex (node)
  {}
  TableExprGroupSumSqrArrayDComplex::~TableExprGroupSumSqrArrayDComplex()
  {}
  void TableExprGroupSumSqrArrayDComplex::apply (const TableExprId& id)
  {
    Array<DComplex> arr = itsOperand->getArrayDComplex(id);
    itsValue += sum(arr*arr);
  }

  TableExprGroupMeanArrayDComplex::TableExprGroupMeanArrayDComplex(TableExprNodeRep* node)
    : TableExprGroupFuncDComplex (node),
      itsNr (0)
  {}
  TableExprGroupMeanArrayDComplex::~TableExprGroupMeanArrayDComplex()
  {}
  void TableExprGroupMeanArrayDComplex::apply (const TableExprId& id)
  {
    Array<DComplex> arr = itsOperand->getArrayDComplex(id);
    itsValue += sum(arr);
    itsNr    += arr.size();
  }
  void TableExprGroupMeanArrayDComplex::finish()
  {
    if (itsNr > 0) {
      itsValue /= double(itsNr);
    }
  }


  TableExprGroupArrayAnys::TableExprGroupArrayAnys(TableExprNodeRep* node)
    : TableExprGroupFuncArrayBool (node)
  {}
  TableExprGroupArrayAnys::~TableExprGroupArrayAnys()
  {}
  void TableExprGroupArrayAnys::apply (const TableExprId& id)
  {
    Array<Bool> arr(itsOperand->getArrayBool(id));
    if (! arr.empty()) {
      if (checkShape (arr, "GANYS")) {
        itsValue = arr;
      } else {
        itsValue = itsValue || arr;
      }
    }
  }

  TableExprGroupArrayAlls::TableExprGroupArrayAlls(TableExprNodeRep* node)
    : TableExprGroupFuncArrayBool (node)
  {}
  TableExprGroupArrayAlls::~TableExprGroupArrayAlls()
  {}
  void TableExprGroupArrayAlls::apply (const TableExprId& id)
  {
    Array<Bool> arr(itsOperand->getArrayBool(id));
    if (! arr.empty()) {
      if (checkShape (arr, "GALLS")) {
        itsValue = arr;
      } else {
        itsValue = itsValue && arr;
      }
    }
  }

  TableExprGroupArrayNTrues::TableExprGroupArrayNTrues(TableExprNodeRep* node)
    : TableExprGroupFuncArrayInt (node)
  {}
  TableExprGroupArrayNTrues::~TableExprGroupArrayNTrues()
  {}
  void TableExprGroupArrayNTrues::apply (const TableExprId& id)
  {
    Array<Bool> arr(itsOperand->getArrayBool(id));
    if (! arr.empty()) {
      if (checkShape (arr, "GNTRUES")) {
        itsValue = 0;
      }
      Bool deleteIt;
      const Bool* inPtr = arr.getStorage (deleteIt);
      for (size_t i=0; i<arr.size(); ++i) {
        if (inPtr[i]) {
          itsValue.data()[i]++;
        }
      }
      arr.freeStorage (inPtr, deleteIt);
    }
  }

  TableExprGroupArrayNFalses::TableExprGroupArrayNFalses(TableExprNodeRep* node)
    : TableExprGroupFuncArrayInt (node)
  {}
  TableExprGroupArrayNFalses::~TableExprGroupArrayNFalses()
  {}
  void TableExprGroupArrayNFalses::apply (const TableExprId& id)
  {
    Array<Bool> arr(itsOperand->getArrayBool(id));
    if (! arr.empty()) {
      if (checkShape (arr, "GNFALSES")) {
        itsValue = 0;
      }
      Bool deleteIt;
      const Bool* inPtr = arr.getStorage (deleteIt);
      for (size_t i=0; i<arr.size(); ++i) {
        if (! inPtr[i]) {
          itsValue.data()[i]++;
        }
      }
      arr.freeStorage (inPtr, deleteIt);
    }
  }


  TableExprGroupMinsArrayInt::TableExprGroupMinsArrayInt(TableExprNodeRep* node)
    : TableExprGroupFuncArrayInt (node)
  {}
  TableExprGroupMinsArrayInt::~TableExprGroupMinsArrayInt()
  {}
  void TableExprGroupMinsArrayInt::apply (const TableExprId& id)
  {
    Array<Int64> arr = itsOperand->getArrayInt(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GMINS")) {
        itsValue = arr;
      } else {
        itsValue = min(itsValue, arr);
      }
    }
  }

  TableExprGroupMaxsArrayInt::TableExprGroupMaxsArrayInt(TableExprNodeRep* node)
    : TableExprGroupFuncArrayInt (node)
  {}
  TableExprGroupMaxsArrayInt::~TableExprGroupMaxsArrayInt()
  {}
  void TableExprGroupMaxsArrayInt::apply (const TableExprId& id)
  {
    Array<Int64> arr = itsOperand->getArrayInt(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GMAXS")) {
        itsValue = arr;
      } else {
        itsValue = max(itsValue, arr);
      }
    }
  }

  TableExprGroupSumsArrayInt::TableExprGroupSumsArrayInt(TableExprNodeRep* node)
    : TableExprGroupFuncArrayInt (node)
  {}
  TableExprGroupSumsArrayInt::~TableExprGroupSumsArrayInt()
  {}
  void TableExprGroupSumsArrayInt::apply (const TableExprId& id)
  {
    Array<Int64> arr = itsOperand->getArrayInt(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GSUMS")) {
        itsValue = arr;
      } else {
        itsValue += arr;
      }
    }
  }

  TableExprGroupProductsArrayInt::TableExprGroupProductsArrayInt(TableExprNodeRep* node)
    : TableExprGroupFuncArrayInt (node)
  {}
  TableExprGroupProductsArrayInt::~TableExprGroupProductsArrayInt()
  {}
  void TableExprGroupProductsArrayInt::apply (const TableExprId& id)
  {
    Array<Int64> arr = itsOperand->getArrayInt(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GPRODUCTS")) {
        itsValue = arr;
      } else {
        itsValue *= arr;
      }
    }
  }

  TableExprGroupSumSqrsArrayInt::TableExprGroupSumSqrsArrayInt(TableExprNodeRep* node)
    : TableExprGroupFuncArrayInt (node)
  {}
  TableExprGroupSumSqrsArrayInt::~TableExprGroupSumSqrsArrayInt()
  {}
  void TableExprGroupSumSqrsArrayInt::apply (const TableExprId& id)
  {
    Array<Int64> arr = itsOperand->getArrayInt(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GSUMSQRS")) {
        itsValue = arr*arr;
      } else {
        itsValue += arr*arr;
      }
    }
  }


  TableExprGroupMinsArrayDouble::TableExprGroupMinsArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncArrayDouble (node)
  {}
  TableExprGroupMinsArrayDouble::~TableExprGroupMinsArrayDouble()
  {}
  void TableExprGroupMinsArrayDouble::apply (const TableExprId& id)
  {
    Array<Double> arr = itsOperand->getArrayDouble(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GMINS")) {
        itsValue = arr;
      } else {
        itsValue = min(itsValue, arr);
      }
    }
  }

  TableExprGroupMaxsArrayDouble::TableExprGroupMaxsArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncArrayDouble (node)
  {}
  TableExprGroupMaxsArrayDouble::~TableExprGroupMaxsArrayDouble()
  {}
  void TableExprGroupMaxsArrayDouble::apply (const TableExprId& id)
  {
    Array<Double> arr = itsOperand->getArrayDouble(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GMAXS")) {
        itsValue = arr;
      } else {
        itsValue = max(itsValue, arr);
      }
    }
  }

  TableExprGroupSumsArrayDouble::TableExprGroupSumsArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncArrayDouble (node)
  {}
  TableExprGroupSumsArrayDouble::~TableExprGroupSumsArrayDouble()
  {}
  void TableExprGroupSumsArrayDouble::apply (const TableExprId& id)
  {
    Array<Double> arr = itsOperand->getArrayDouble(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GSUMS")) {
        itsValue = arr;
      } else {
        itsValue += arr;
      }
    }
  }

  TableExprGroupProductsArrayDouble::TableExprGroupProductsArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncArrayDouble (node)
  {}
  TableExprGroupProductsArrayDouble::~TableExprGroupProductsArrayDouble()
  {}
  void TableExprGroupProductsArrayDouble::apply (const TableExprId& id)
  {
    Array<Double> arr = itsOperand->getArrayDouble(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GPRODUCTS")) {
        itsValue = arr;
      } else {
        itsValue *= arr;
      }
    }
  }

  TableExprGroupSumSqrsArrayDouble::TableExprGroupSumSqrsArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncArrayDouble (node)
  {}
  TableExprGroupSumSqrsArrayDouble::~TableExprGroupSumSqrsArrayDouble()
  {}
  void TableExprGroupSumSqrsArrayDouble::apply (const TableExprId& id)
  {
    Array<Double> arr = itsOperand->getArrayDouble(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GSUMSQRS")) {
        itsValue = arr*arr;
      } else {
        itsValue += arr*arr;
      }
    }
  }

  TableExprGroupMeansArrayDouble::TableExprGroupMeansArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncArrayDouble (node)
  {}
  TableExprGroupMeansArrayDouble::~TableExprGroupMeansArrayDouble()
  {}
  void TableExprGroupMeansArrayDouble::apply (const TableExprId& id)
  {
    Array<Double> arr = itsOperand->getArrayDouble(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GMEANS")) {
        // Future: if arr is a masked array, only add where not masked
        itsValue = arr;
        itsNr.resize (arr.shape());
        itsNr = 1;
      } else {
        itsValue += arr;
        itsNr += (Int64)1;
      }
    }
  }
  void TableExprGroupMeansArrayDouble::finish()
  {
    DebugAssert (itsNr.contiguousStorage()  &&  itsValue.contiguousStorage(),
                 AipsError);
    Array<Double>::iterator itv = itsValue.begin();
    for (Array<Int64>::const_iterator itn = itsNr.begin();
         itn != itsNr.end(); ++itn, ++itv) {
      if (*itn > 0) {
        *itv /= *itn;
      }
    }
  }

  TableExprGroupVariancesArrayDouble::TableExprGroupVariancesArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncArrayDouble (node)
  {}
  TableExprGroupVariancesArrayDouble::~TableExprGroupVariancesArrayDouble()
  {}
  void TableExprGroupVariancesArrayDouble::apply (const TableExprId& id)
  {
    // Calculate mean and variance in a running way using a
    // numerically stable algorithm.
    // See en.wikipedia.org/wiki/Algorithms_for_calculating_variance
    Array<Double> arr = itsOperand->getArrayDouble(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GVARIANCES")) {
        // First time.
        itsNr.resize (arr.shape());
        itsMean.resize (arr.shape());
        itsNr    = 1;
        itsMean  = arr;
        itsValue = 0.;
      } else {
        itsNr += (Int64)1;
        Array<Double> delta(arr - itsMean);
        Array<Double> tmpd(delta.shape());
        std::transform (delta.begin(), delta.end(), itsNr.begin(),
                        tmpd.begin(), Divides<Double,Int64>());
        itsMean += tmpd;
        delta *= arr - itsMean;
        itsValue += delta;
      }
    }
  }
  void TableExprGroupVariancesArrayDouble::finish()
  {
    DebugAssert (itsNr.contiguousStorage()  &&  itsValue.contiguousStorage(),                 AipsError);
    Array<Double>::iterator itv = itsValue.begin();
    for (Array<Int64>::const_iterator itn = itsNr.begin();
         itn != itsNr.end(); ++itn, ++itv) {
      if (*itn > 1) {
        *itv /= *itn - 1;
      } else {
        *itv = 0;
      }
    }
  }

  TableExprGroupStdDevsArrayDouble::TableExprGroupStdDevsArrayDouble(TableExprNodeRep* node)
    : TableExprGroupVariancesArrayDouble (node)
  {}
  TableExprGroupStdDevsArrayDouble::~TableExprGroupStdDevsArrayDouble()
  {}
  void TableExprGroupStdDevsArrayDouble::finish()
  {
    TableExprGroupVariancesArrayDouble::finish();
    itsValue = sqrt(itsValue);
  }

  TableExprGroupRmssArrayDouble::TableExprGroupRmssArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncArrayDouble (node)
  {}
  TableExprGroupRmssArrayDouble::~TableExprGroupRmssArrayDouble()
  {}
  void TableExprGroupRmssArrayDouble::apply (const TableExprId& id)
  {
    Array<Double> arr = itsOperand->getArrayDouble(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GRMSS")) {
        itsValue = arr*arr;
        itsNr.resize (arr.shape());
        itsNr = 1;
      } else {
        itsValue += arr*arr;
        itsNr += (Int64)1;
      }
    }
  }
  void TableExprGroupRmssArrayDouble::finish()
  {
    DebugAssert (itsNr.contiguousStorage()  &&  itsValue.contiguousStorage(),
                 AipsError);
    Array<Double>::iterator itv = itsValue.begin();
    for (Array<Int64>::const_iterator itn = itsNr.begin();
         itn != itsNr.end(); ++itn, ++itv) {
      if (*itn > 0) {
        *itv = sqrt(*itv / *itn);
      } else {
        *itv = 0;
      }
    }
  }

  TableExprGroupSumsArrayDComplex::TableExprGroupSumsArrayDComplex(TableExprNodeRep* node)
    : TableExprGroupFuncArrayDComplex (node)
  {}
  TableExprGroupSumsArrayDComplex::~TableExprGroupSumsArrayDComplex()
  {}
  void TableExprGroupSumsArrayDComplex::apply (const TableExprId& id)
  {
    Array<DComplex> arr = itsOperand->getArrayDComplex(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GSUMS")) {
        itsValue = arr;
      } else {
        itsValue += arr;
      }
    }
  }

  TableExprGroupProductsArrayDComplex::TableExprGroupProductsArrayDComplex(TableExprNodeRep* node)
    : TableExprGroupFuncArrayDComplex (node)
  {}
  TableExprGroupProductsArrayDComplex::~TableExprGroupProductsArrayDComplex()
  {}
  void TableExprGroupProductsArrayDComplex::apply (const TableExprId& id)
  {
    Array<DComplex> arr = itsOperand->getArrayDComplex(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GPRODUCTS")) {
        itsValue = arr;
      } else {
        itsValue *= arr;
      }
    }
  }

  TableExprGroupSumSqrsArrayDComplex::TableExprGroupSumSqrsArrayDComplex(TableExprNodeRep* node)
    : TableExprGroupFuncArrayDComplex (node)
  {}
  TableExprGroupSumSqrsArrayDComplex::~TableExprGroupSumSqrsArrayDComplex()
  {}
  void TableExprGroupSumSqrsArrayDComplex::apply (const TableExprId& id)
  {
    Array<DComplex> arr = itsOperand->getArrayDComplex(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GSUMSQRS")) {
        itsValue = arr*arr;
      } else {
        itsValue += arr*arr;
      }
    }
  }

  TableExprGroupMeansArrayDComplex::TableExprGroupMeansArrayDComplex(TableExprNodeRep* node)
    : TableExprGroupFuncArrayDComplex (node)
  {}
  TableExprGroupMeansArrayDComplex::~TableExprGroupMeansArrayDComplex()
  {}
  void TableExprGroupMeansArrayDComplex::apply (const TableExprId& id)
  {
    Array<DComplex> arr = itsOperand->getArrayDComplex(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GMEANS")) {
        // Future: if arr is a masked array, only add where not masked
        itsValue = arr;
        itsNr.resize (arr.shape());
        itsNr = 1;
      } else {
        itsValue += arr;
        itsNr += (Int64)1;
      }
    }
  }
  void TableExprGroupMeansArrayDComplex::finish()
  {
    DebugAssert (itsNr.contiguousStorage()  &&  itsValue.contiguousStorage(),
                 AipsError);
    Array<DComplex>::iterator itv = itsValue.begin();
    for (Array<Int64>::const_iterator itn = itsNr.begin();
         itn != itsNr.end(); ++itn, ++itv) {
      if (*itn > 0) {
        *itv /= *itn;
      }
    }
  }


  TableExprGroupHistBase::TableExprGroupHistBase (TableExprNodeRep* node,
                                                  Int64 nbin,
                                                  Double start, Double end)
    : TableExprGroupFuncBase (node),
      itsHist  (nbin+2, 0),
      itsStart (start)
  {
    AlwaysAssert (nbin > 0  &&  end > start, AipsError);
    itsWidth = (end-start) / nbin;
  }
  TableExprGroupHistBase::~TableExprGroupHistBase()
  {}
  void TableExprGroupHistBase::add (Double val)
  {
    size_t bin = size_t(std::max(0., (val - itsStart) / itsWidth + 1.));
    if (bin >= itsHist.size()) {
      bin = itsHist.size() - 1;
    }
    itsHist[bin]++;
  }
  Array<Int64> TableExprGroupHistBase::getArrayInt (const vector<TableExprId>&)
  {
    return itsHist;
  }

  TableExprGroupHistScalar::TableExprGroupHistScalar (TableExprNodeRep* node,
                                                      Int64 nbin,
                                                      Double start, Double end)
    : TableExprGroupHistBase (node, nbin, start, end)
  {}
  TableExprGroupHistScalar::~TableExprGroupHistScalar()
  {}
  void TableExprGroupHistScalar::apply (const TableExprId& id)
  {
    add (itsOperand->getDouble (id));
  }

  TableExprGroupHistInt::TableExprGroupHistInt (TableExprNodeRep* node,
                                                Int64 nbin,
                                                Double start, Double end)
    : TableExprGroupHistBase (node, nbin, start, end)
  {}
  TableExprGroupHistInt::~TableExprGroupHistInt()
  {}
  void TableExprGroupHistInt::apply (const TableExprId& id)
  {
    Array<Int64> arr = itsOperand->getArrayInt (id);
    // Array does not need to be contiguous, so use iterator.
    Array<Int64>::const_iterator iterEnd = arr.end();
    for (Array<Int64>::const_iterator iter = arr.begin();
         iter!=iterEnd; ++iter) {
      add (*iter);
    }
  }

  TableExprGroupHistDouble::TableExprGroupHistDouble (TableExprNodeRep* node,
                                                      Int64 nbin,
                                                      Double start, Double end)
    : TableExprGroupHistBase (node, nbin, start, end)
  {}
  TableExprGroupHistDouble::~TableExprGroupHistDouble()
  {}
  void TableExprGroupHistDouble::apply (const TableExprId& id)
  {
    Array<Double> arr = itsOperand->getArrayDouble (id);
    // Array does not need to be contiguous, so use iterator.
    Array<Double>::const_iterator iterEnd = arr.end();
    for (Array<Double>::const_iterator iter = arr.begin();
         iter!=iterEnd; ++iter) {
      add (*iter);
    }
  }


} //# NAMESPACE CASACORE - END
