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
#include <casacore/tables/TaQL/MArrayMath.h>
#include <casacore/tables/TaQL/MArrayLogical.h>
#include <vector>
#include <limits>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // Clear value is masked off.
  template<typename T>
  void TEGClearMasked (MArray<T>& arr)
  {
    if (arr.hasMask()) {
      Array<Bool>::const_contiter m = arr.mask().cbegin();
      for (typename Array<T>::contiter p = arr.array().cbegin();
           p != arr.array().cend(); ++p, ++m) {
        if (*m) *p = T();
      }
    }
  }

  template<typename T>
  void TEGMin (const MArray<T>& src, MArray<T>& dst)
  {
    typename Array<T>::const_iterator in = src.array().begin();
    if (src.hasMask()) {
      typename Array<Bool>::const_iterator min = src.mask().begin();
      typename Array<Bool>::contiter mout = dst.wmask().cbegin();
      for (typename Array<T>::contiter out = dst.array().cbegin();
           out != dst.array().cend(); ++in, ++min, ++out, ++mout) {
        if (! *min) {
          *mout = False;
          if (*in < *out) *out = *in;
        }
      }
    } else {
      for (typename Array<T>::contiter out = dst.array().cbegin();
           out != dst.array().cend(); ++in, ++out) {
        if (*in < *out) *out = *in;
      }
    }
  }

  template<typename T>
  void TEGMax (const MArray<T>& src, MArray<T>& dst)
  {
    typename Array<T>::const_iterator in = src.array().begin();
    if (src.hasMask()) {
      typename Array<Bool>::const_iterator min = src.mask().begin();
      typename Array<Bool>::contiter mout = dst.wmask().cbegin();
      for (typename Array<T>::contiter out = dst.array().cbegin();
           out != dst.array().cend(); ++in, ++min, ++out, ++mout) {
        if (! *min) {
          *mout = False;
          if (*in > *out) *out = *in;
        }
      }
    } else {
      for (typename Array<T>::contiter out = dst.array().cbegin();
           out != dst.array().cend(); ++in, ++out) {
        if (*in > *out) *out = *in;
      }
    }
  }

  template<typename T>
  void TEGSum (const MArray<T>& src, MArray<T>& dst)
  {
    if (src.hasMask()) {
      typename Array<T>::const_iterator in = src.array().begin();
      typename Array<Bool>::const_iterator min = src.mask().begin();
      typename Array<Bool>::contiter mout = dst.wmask().cbegin();
      for (typename Array<T>::contiter out = dst.array().cbegin();
           out != dst.array().cend(); ++in, ++min, ++out, ++mout) {
        if (! *min) {
          *mout = False;
          *out += *in;
        }
      }
    } else {
      dst.array() += src.array();
    }
  }

  template<typename T>
  void TEGProduct (const MArray<T>& src, MArray<T>& dst)
  {
    if (src.hasMask()) {
      typename Array<T>::const_iterator in = src.array().begin();
      typename Array<Bool>::const_iterator min = src.mask().begin();
      typename Array<Bool>::contiter mout = dst.wmask().cbegin();
      for (typename Array<T>::contiter out = dst.array().cbegin();
           out != dst.array().cend(); ++in, ++min, ++out, ++mout) {
        if (! *min) {
          *mout = False;
          *out *= *in;
        }
      }
    } else {
      dst.array() *= src.array();
    }
  }

  template<typename T>
  void TEGSumSqr (const MArray<T>& src, MArray<T>& dst)
  {
    if (src.hasMask()) {
      typename Array<T>::const_iterator in = src.array().begin();
      typename Array<Bool>::const_iterator min = src.mask().begin();
      typename Array<Bool>::contiter mout = dst.wmask().cbegin();
      for (typename Array<T>::contiter out = dst.array().cbegin();
           out != dst.array().cend(); ++in, ++min, ++out, ++mout) {
        if (! *min) {
          *mout = False;
          *out += *in * *in;
        }
      }
    } else {
      typename Array<T>::const_iterator in = src.array().begin();
      for (typename Array<T>::contiter out = dst.array().cbegin();
           out != dst.array().cend(); ++in, ++out) {
        *out += *in * *in;
      }
    }
  }

  template<typename T>
  void TEGMeanAdd (const MArray<T>& src, Array<T>& dst, Array<Int64>& nr)
  {
    typename Array<Int64>::contiter itn = nr.cbegin();
    if (src.hasMask()) {
      typename Array<T>::const_iterator in = src.array().begin();
      typename Array<Bool>::const_iterator min = src.mask().begin();
      for (typename Array<T>::contiter out = dst.cbegin();
           out != dst.cend(); ++in, ++min, ++out, ++itn) {
        if (! *min) {
          *out += *in;
          (*itn)++;
        }
      }
    } else {
      typename Array<T>::const_iterator in = src.array().begin();
      for (typename Array<T>::contiter out = dst.cbegin();
           out != dst.cend(); ++in, ++out, ++itn) {
        *out += *in;
        (*itn)++;
      }
    }
  }

  template<typename T>
  void TEGMeanFinish (MArray<T>& val, const Array<Int64>& nr)
  {
    DebugAssert (nr.contiguousStorage()  &&  val.array().contiguousStorage(),
                 AipsError);
    typename Array<T>::contiter itv = val.array().cbegin();
    typename Array<Bool>::contiter itm = val.wmask().cbegin();
    for (Array<Int64>::const_contiter itn = nr.cbegin();
         itn != nr.cend(); ++itn, ++itv, ++itm) {
      if (*itn > 0) {
        *itv /= *itn;
      } else if (val.hasMask()) {
        *itm = True;
      }
    }
  }


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
    MArray<Int64> arr = itsOperand->getArrayInt(id);
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
    MArray<Int64> arr = itsOperand->getArrayInt(id);
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
    MArray<Int64> arr = itsOperand->getArrayInt(id);
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
    MArray<Int64> arr = itsOperand->getArrayInt(id);
    itsValue += sum(arr*arr);
  }


  TableExprGroupMinArrayDouble::TableExprGroupMinArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncDouble (node, std::numeric_limits<Double>::max())
  {}
  TableExprGroupMinArrayDouble::~TableExprGroupMinArrayDouble()
  {}
  void TableExprGroupMinArrayDouble::apply (const TableExprId& id)
  {
    MArray<Double> arr = itsOperand->getArrayDouble(id);
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
    MArray<Double> arr = itsOperand->getArrayDouble(id);
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
    MArray<Double> arr = itsOperand->getArrayDouble(id);
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
    MArray<Double> arr = itsOperand->getArrayDouble(id);
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
    MArray<Double> arr = itsOperand->getArrayDouble(id);
    itsValue += sum(arr);
    if (arr.hasMask()) {
      itsNr += nfalse(arr.mask());
    } else {
      itsNr += arr.size();
    }
  }
  void TableExprGroupMeanArrayDouble::finish()
  {
    if (itsNr > 0) {
      itsValue /= itsNr;
    }
  }

  TableExprGroupVarianceArrayDouble::TableExprGroupVarianceArrayDouble(TableExprNodeRep* node, uInt ddof)
    : TableExprGroupFuncDouble (node),
      itsDdof    (ddof),
      itsNr      (0),
      itsCurMean (0)
  {}
  TableExprGroupVarianceArrayDouble::~TableExprGroupVarianceArrayDouble()
  {}
  void TableExprGroupVarianceArrayDouble::apply (const TableExprId& id)
  {
    // Calculate mean and variance in a running way using a
    // numerically stable algorithm
    // See en.wikipedia.org/wiki/Algorithms_for_calculating_variance
    MArray<Double> arr = itsOperand->getArrayDouble(id);
    if (! arr.empty()) {
      Array<Double>::const_iterator in = arr.array().begin();
      if (arr.hasMask()) {
        Array<Bool>::const_iterator min = arr.mask().begin();
        for (size_t i=0; i<arr.size(); ++i, ++in, ++min) {
          if (! *min) {
            itsNr++;
            Double delta = *in - itsCurMean;
            itsCurMean += delta / itsNr;
            Double d = *in - itsCurMean;
            itsValue += d*delta;
          }
        }
      } else {
        for (size_t i=0; i<arr.size(); ++i, ++in) {
          itsNr++;
          Double delta = *in - itsCurMean;
          itsCurMean += delta / itsNr;
          Double d = *in - itsCurMean;
          itsValue += d*delta;
        }
      }
    }
  }
  void TableExprGroupVarianceArrayDouble::finish()
  {
    if (itsNr > itsDdof) {
      itsValue /= itsNr-itsDdof;
    } else {
      itsValue = 0;
    }
  }

  TableExprGroupStdDevArrayDouble::TableExprGroupStdDevArrayDouble(TableExprNodeRep* node, uInt ddof)
    : TableExprGroupVarianceArrayDouble (node, ddof)
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
    MArray<Double> arr = itsOperand->getArrayDouble(id);
    itsValue += sum(arr*arr);
    if (arr.hasMask()) {
      itsNr += nfalse(arr.mask());
    } else  {
      itsNr += arr.size();
    }
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
      size_t nr = 0;
      MArray<Double> arr0 = itsOperand->getArrayDouble(ids[0]);
      std::vector<Double> values(ids.size() * arr0.size());
      nr += arr0.flatten (&(values[0]), values.size());
      for (uInt i=1; i<ids.size(); ++i) {
        // Get value and make contiguous if needed.
        MArray<Double> arr = itsOperand->getArrayDouble(ids[i]);
        if (arr.size() > values.size()-nr) {
          values.resize (values.size() + arr.size());
        }
        nr += arr.flatten (&(values[0]) + nr, values.size()-nr);
      }
      return GenSort<Double>::kthLargest
        (&(values[0]), nr,
         static_cast<Int64>((nr - 1)*itsFrac + 0.001));
    } catch (const std::exception& x) {
      throw TableInvExpr ("Cannot compute gfractile; "
                          "probably too many data - " + String(x.what()));
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
    MArray<DComplex> arr = itsOperand->getArrayDComplex(id);
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
    MArray<DComplex> arr = itsOperand->getArrayDComplex(id);
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
    MArray<DComplex> arr = itsOperand->getArrayDComplex(id);
    itsValue += sum(arr);
    if (arr.hasMask()) {
      itsNr += nfalse(arr.mask());
    } else {
      itsNr += arr.size();
    }
  }
  void TableExprGroupMeanArrayDComplex::finish()
  {
    if (itsNr > 0) {
      itsValue /= double(itsNr);
    }
  }

  TableExprGroupVarianceArrayDComplex::TableExprGroupVarianceArrayDComplex(TableExprNodeRep* node, uInt ddof)
    : TableExprGroupFuncDouble (node),
      itsDdof (ddof),
      itsNr   (0)
  {}
  TableExprGroupVarianceArrayDComplex::~TableExprGroupVarianceArrayDComplex()
  {}
  void TableExprGroupVarianceArrayDComplex::apply (const TableExprId& id)
  {
    // Calculate mean and variance in a running way using a
    // numerically stable algorithm
    // See en.wikipedia.org/wiki/Algorithms_for_calculating_variance
    MArray<DComplex> arr = itsOperand->getArrayDComplex(id);
    if (! arr.empty()) {
      Array<DComplex>::const_iterator in = arr.array().begin();
      if (arr.hasMask()) {
        Array<Bool>::const_iterator min = arr.mask().begin();
        for (size_t i=0; i<arr.size(); ++i, ++in, ++min) {
          if (! *min) {
            itsNr++;
            DComplex delta = *in - itsCurMean;
            itsCurMean += delta / Double(itsNr);
            DComplex d = *in - itsCurMean;
            itsValue += real(delta)*real(d) + imag(delta)*imag(d);
          }
        }
      } else {
        for (size_t i=0; i<arr.size(); ++i, ++in) {
          itsNr++;
          DComplex delta = *in - itsCurMean;
          itsCurMean += delta / Double(itsNr);
          DComplex d = *in - itsCurMean;
          itsValue += real(delta)*real(d) + imag(delta)*imag(d);
        }
      }
    }
  }
  void TableExprGroupVarianceArrayDComplex::finish()
  {
    if (itsNr > itsDdof) {
      itsValue /= itsNr-itsDdof;
    } else {
      itsValue = 0;
    }
  }

  TableExprGroupStdDevArrayDComplex::TableExprGroupStdDevArrayDComplex(TableExprNodeRep* node, uInt ddof)
    : TableExprGroupVarianceArrayDComplex (node, ddof)
  {}
  TableExprGroupStdDevArrayDComplex::~TableExprGroupStdDevArrayDComplex()
  {}
  void TableExprGroupStdDevArrayDComplex::finish()
  {
    TableExprGroupVarianceArrayDComplex::finish();
    itsValue = sqrt(itsValue);
  }


  TableExprGroupArrayAnys::TableExprGroupArrayAnys(TableExprNodeRep* node)
    : TableExprGroupFuncArrayBool (node)
  {}
  TableExprGroupArrayAnys::~TableExprGroupArrayAnys()
  {}
  void TableExprGroupArrayAnys::apply (const TableExprId& id)
  {
    MArray<Bool> arr(itsOperand->getArrayBool(id));
    if (! arr.empty()) {
      if (checkShape (arr, "GANYS")) {
        itsValue.array() = arr.array();
        itsValue.wmask() = arr.mask();
      } else if (arr.hasMask()) {
        Array<Bool>::const_iterator in = arr.array().begin();
        Array<Bool>::const_iterator min = arr.mask().begin();
        Array<Bool>::contiter mout = itsValue.wmask().cbegin();
        for (Array<Bool>::contiter out = itsValue.array().cbegin();
             out != itsValue.array().cend(); ++in, ++min, ++out, ++mout) {
          if (! *min) {
            *mout = False;
            *out = *out || *in;
          }
        }
      } else {
        Array<Bool>::const_iterator in = arr.array().begin();
        for (Array<Bool>::contiter out = itsValue.array().cbegin();
             out != itsValue.array().cend(); ++in, ++out) {
          *out = *out || *in;
        }
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
    MArray<Bool> arr(itsOperand->getArrayBool(id));
    if (! arr.empty()) {
      if (checkShape (arr, "GALLS")) {
        itsValue.array() = arr.array();
        itsValue.wmask() = arr.mask();
      } else if (arr.hasMask()) {
        Array<Bool>::const_iterator in = arr.array().begin();
        Array<Bool>::const_iterator min = arr.mask().begin();
        Array<Bool>::contiter mout = itsValue.wmask().cbegin();
        for (Array<Bool>::contiter out = itsValue.array().cbegin();
             out != itsValue.array().cend(); ++in, ++min, ++out, ++mout) {
          if (! *min) {
            *mout = False;
            *out = *out && *in;
          }
        }
      } else {
        Array<Bool>::const_iterator in = arr.array().begin();
        for (Array<Bool>::contiter out = itsValue.array().cbegin();
             out != itsValue.array().cend(); ++in, ++out) {
          *out = *out && *in;
        }
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
    MArray<Bool> arr(itsOperand->getArrayBool(id));
    if (! arr.empty()) {
      if (checkShape (arr, "GNTRUES")) {
        itsValue.array() = 0;
        itsValue.wmask() = True;
      }
      if (arr.hasMask()) {
        Array<Bool>::const_iterator in = arr.array().begin();
        Array<Bool>::const_iterator min = arr.mask().begin();
        Array<Bool>::contiter mout = itsValue.wmask().cbegin();
        for (Array<Int64>::contiter out = itsValue.array().cbegin();
             out != itsValue.array().cend(); ++in, ++min, ++out, ++mout) {
          if (! *min) {
            *mout = False;
            if (*in) {
              (*out)++;
            }
          }
        }
      } else {
        Array<Bool>::const_iterator in = arr.array().begin();
        for (Array<Int64>::contiter out = itsValue.array().cbegin();
             out != itsValue.array().cend(); ++in, ++out) {
          if (*in) {
            (*out)++;
          }
        }
      }
    }
  }

  TableExprGroupArrayNFalses::TableExprGroupArrayNFalses(TableExprNodeRep* node)
    : TableExprGroupFuncArrayInt (node)
  {}
  TableExprGroupArrayNFalses::~TableExprGroupArrayNFalses()
  {}
  void TableExprGroupArrayNFalses::apply (const TableExprId& id)
  {
    MArray<Bool> arr(itsOperand->getArrayBool(id));
    if (! arr.empty()) {
      if (checkShape (arr, "GNFALSES")) {
        itsValue.array() = 0;
        itsValue.wmask() = True;
      }
      if (arr.hasMask()) {
        Array<Bool>::const_iterator in = arr.array().begin();
        Array<Bool>::const_iterator min = arr.mask().begin();
        Array<Bool>::contiter mout = itsValue.wmask().cbegin();
        for (Array<Int64>::contiter out = itsValue.array().cbegin();
             out != itsValue.array().cend(); ++in, ++min, ++out, ++mout) {
          if (! *min) {
            *mout = False;
            if (! *in) {
              (*out)++;
            }
          }
        }
      } else {
        Array<Bool>::const_iterator in = arr.array().begin();
        for (Array<Int64>::contiter out = itsValue.array().cbegin();
             out != itsValue.array().cend(); ++in, ++out) {
          if (! *in) {
            (*out)++;
          }
        }
      }
    }
  }


  TableExprGroupMinsArrayInt::TableExprGroupMinsArrayInt(TableExprNodeRep* node)
    : TableExprGroupFuncArrayInt (node)
  {}
  TableExprGroupMinsArrayInt::~TableExprGroupMinsArrayInt()
  {}
  void TableExprGroupMinsArrayInt::apply (const TableExprId& id)
  {
    MArray<Int64> arr(itsOperand->getArrayInt(id));
    if (! arr.empty()) {
      if (checkShape (arr, "GMINS")) {
        itsValue.array() = std::numeric_limits<Int64>::max();
        itsValue.wmask() = True;
      }
      TEGMin (arr, itsValue);
    }
  }
  void TableExprGroupMinsArrayInt::finish()
  {
    TEGClearMasked (itsValue);
  }

  TableExprGroupMaxsArrayInt::TableExprGroupMaxsArrayInt(TableExprNodeRep* node)
    : TableExprGroupFuncArrayInt (node)
  {}
  TableExprGroupMaxsArrayInt::~TableExprGroupMaxsArrayInt()
  {}
  void TableExprGroupMaxsArrayInt::apply (const TableExprId& id)
  {
    MArray<Int64> arr(itsOperand->getArrayInt(id));
    if (! arr.empty()) {
      if (checkShape (arr, "GMAXS")) {
        itsValue.array() = std::numeric_limits<Int64>::min();
        itsValue.wmask() = True;
      }
      TEGMax (arr, itsValue);
    }
  }
  void TableExprGroupMaxsArrayInt::finish()
  {
    TEGClearMasked (itsValue);
  }

  TableExprGroupSumsArrayInt::TableExprGroupSumsArrayInt(TableExprNodeRep* node)
    : TableExprGroupFuncArrayInt (node)
  {}
  TableExprGroupSumsArrayInt::~TableExprGroupSumsArrayInt()
  {}
  void TableExprGroupSumsArrayInt::apply (const TableExprId& id)
  {
    MArray<Int64> arr = itsOperand->getArrayInt(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GSUMS")) {
        itsValue.array() = 0;
        itsValue.wmask() = True;
      }
      TEGSum (arr, itsValue);
    }
  }

  TableExprGroupProductsArrayInt::TableExprGroupProductsArrayInt(TableExprNodeRep* node)
    : TableExprGroupFuncArrayInt (node)
  {}
  TableExprGroupProductsArrayInt::~TableExprGroupProductsArrayInt()
  {}
  void TableExprGroupProductsArrayInt::apply (const TableExprId& id)
  {
    MArray<Int64> arr = itsOperand->getArrayInt(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GPRODUCTS")) {
        itsValue.array() = 1;
        itsValue.wmask() = True;
      }
      TEGProduct (arr, itsValue);
    }
  }
  void TableExprGroupProductsArrayInt::finish()
  {
    TEGClearMasked (itsValue);
  }

  TableExprGroupSumSqrsArrayInt::TableExprGroupSumSqrsArrayInt(TableExprNodeRep* node)
    : TableExprGroupFuncArrayInt (node)
  {}
  TableExprGroupSumSqrsArrayInt::~TableExprGroupSumSqrsArrayInt()
  {}
  void TableExprGroupSumSqrsArrayInt::apply (const TableExprId& id)
  {
    MArray<Int64> arr = itsOperand->getArrayInt(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GSUMSQRS")) {
        itsValue.array() = 0;
        itsValue.wmask() = True;
      }
      TEGSumSqr (arr, itsValue);
    }
  }


  TableExprGroupMinsArrayDouble::TableExprGroupMinsArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncArrayDouble (node)
  {}
  TableExprGroupMinsArrayDouble::~TableExprGroupMinsArrayDouble()
  {}
  void TableExprGroupMinsArrayDouble::apply (const TableExprId& id)
  {
    MArray<Double> arr(itsOperand->getArrayDouble(id));
    if (! arr.empty()) {
      if (checkShape (arr, "GMINS")) {
        itsValue.array() = std::numeric_limits<Double>::max();
        itsValue.wmask() = True;
      }
      TEGMin (arr, itsValue);
    }
  }
  void TableExprGroupMinsArrayDouble::finish()
  {
    TEGClearMasked (itsValue);
  }

  TableExprGroupMaxsArrayDouble::TableExprGroupMaxsArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncArrayDouble (node)
  {}
  TableExprGroupMaxsArrayDouble::~TableExprGroupMaxsArrayDouble()
  {}
  void TableExprGroupMaxsArrayDouble::apply (const TableExprId& id)
  {
    MArray<Double> arr = itsOperand->getArrayDouble(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GMAXS")) {
        itsValue.array() = std::numeric_limits<Double>::min();
        itsValue.wmask() = True;
      }
      TEGMax (arr, itsValue);
    }
  }
  void TableExprGroupMaxsArrayDouble::finish()
  {
    TEGClearMasked (itsValue);
  }

  TableExprGroupSumsArrayDouble::TableExprGroupSumsArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncArrayDouble (node)
  {}
  TableExprGroupSumsArrayDouble::~TableExprGroupSumsArrayDouble()
  {}
  void TableExprGroupSumsArrayDouble::apply (const TableExprId& id)
  {
    MArray<Double> arr = itsOperand->getArrayDouble(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GSUMS")) {
        itsValue.array() = 0;
        itsValue.wmask() = True;
      }
      TEGSum (arr, itsValue);
    }
  }

  TableExprGroupProductsArrayDouble::TableExprGroupProductsArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncArrayDouble (node)
  {}
  TableExprGroupProductsArrayDouble::~TableExprGroupProductsArrayDouble()
  {}
  void TableExprGroupProductsArrayDouble::apply (const TableExprId& id)
  {
    MArray<Double> arr = itsOperand->getArrayDouble(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GPRODUCTS")) {
        itsValue.array() = 1;
        itsValue.wmask() = True;
      }
      TEGProduct (arr, itsValue);
    }
  }
  void TableExprGroupProductsArrayDouble::finish()
  {
    TEGClearMasked (itsValue);
  }

  TableExprGroupSumSqrsArrayDouble::TableExprGroupSumSqrsArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncArrayDouble (node)
  {}
  TableExprGroupSumSqrsArrayDouble::~TableExprGroupSumSqrsArrayDouble()
  {}
  void TableExprGroupSumSqrsArrayDouble::apply (const TableExprId& id)
  {
    MArray<Double> arr = itsOperand->getArrayDouble(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GSUMSQRS")) {
        itsValue.array() = 0;
        itsValue.wmask() = True;
      }
      TEGSumSqr (arr, itsValue);
    }
  }

  TableExprGroupMeansArrayDouble::TableExprGroupMeansArrayDouble(TableExprNodeRep* node)
    : TableExprGroupFuncArrayDouble (node)
  {}
  TableExprGroupMeansArrayDouble::~TableExprGroupMeansArrayDouble()
  {}
  void TableExprGroupMeansArrayDouble::apply (const TableExprId& id)
  {
    MArray<Double> arr = itsOperand->getArrayDouble(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GMEANS")) {
        itsValue.array() = 0;
        itsValue.wmask() = False;
        itsNr.resize (arr.shape());
        itsNr = 0;
      }
      TEGMeanAdd (arr, itsValue.array(), itsNr);
    }
  }
  void TableExprGroupMeansArrayDouble::finish()
  {
    TEGMeanFinish (itsValue, itsNr);
  }

  TableExprGroupVariancesArrayDouble::TableExprGroupVariancesArrayDouble(TableExprNodeRep* node, uInt ddof)
    : TableExprGroupFuncArrayDouble (node),
      itsDdof (ddof)
  {}
  TableExprGroupVariancesArrayDouble::~TableExprGroupVariancesArrayDouble()
  {}
  void TableExprGroupVariancesArrayDouble::apply (const TableExprId& id)
  {
    // Calculate mean and variance in a running way using a
    // numerically stable algorithm.
    // See en.wikipedia.org/wiki/Algorithms_for_calculating_variance
    MArray<Double> arr = itsOperand->getArrayDouble(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GVARIANCES")) {
        itsValue.array() = 0;
        itsValue.wmask() = False;
        itsCurMean.resize (arr.shape());
        itsCurMean = 0;
        itsNr.resize (arr.shape());
        itsNr = 0;
      }
      Array<Double>::contiter itm = itsCurMean.cbegin();
      Array<Int64>::contiter itn = itsNr.cbegin();
      Array<Double>::const_iterator in = arr.array().begin();
      if (arr.hasMask()) {
        Array<Bool>::const_iterator min = arr.mask().begin();
        for (Array<Double>::contiter out = itsValue.array().cbegin();
             out != itsValue.array().cend(); ++in, ++min, ++out, ++itm, ++itn) {
          if (! *min) {
            (*itn)++;
            Double delta = *in - *itm;
            *itm += delta / *itn;
            delta *= *in - *itm;
            *out += delta;
          }
        }
      } else {
        for (Array<Double>::contiter out = itsValue.array().cbegin();
             out != itsValue.array().cend(); ++in, ++out, ++itm, ++itn) {
          (*itn)++;
          Double delta = *in - *itm;
          *itm += delta / *itn;
          delta *= *in - *itm;
          *out += delta;
        }
      }
    }
  }
  void TableExprGroupVariancesArrayDouble::finish()
  {
    DebugAssert (itsNr.contiguousStorage()  &&  itsValue.contiguousStorage(),
                 AipsError);
    Array<Double>::contiter itv = itsValue.array().cbegin();
    Array<Bool>::contiter itm = itsValue.wmask().cbegin();
    for (Array<Int64>::const_contiter itn = itsNr.cbegin();
         itn != itsNr.cend(); ++itn, ++itv, ++itm) {
      if (*itn > itsDdof) {
        *itv /= *itn - itsDdof;
      } else {
        *itv = 0;
        *itm = True;
      }
    }
  }

  TableExprGroupStdDevsArrayDouble::TableExprGroupStdDevsArrayDouble(TableExprNodeRep* node, uInt ddof)
    : TableExprGroupVariancesArrayDouble (node, ddof)
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
    MArray<Double> arr = itsOperand->getArrayDouble(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GRMSS")) {
        itsValue.array() = 0;
        itsValue.wmask() = False;
        itsNr.resize (arr.shape());
        itsNr = 0;
      }
      Array<Int64>::contiter itn = itsNr.cbegin();
      Array<Double>::const_iterator in = arr.array().begin();
      if (arr.hasMask()) {
        Array<Bool>::const_iterator min = arr.mask().begin();
        for (Array<Double>::contiter out = itsValue.array().cbegin();
             out != itsValue.array().cend(); ++in, ++min, ++out, ++itn) {
          if (! *min) {
            *out += *in * *in;
            (*itn)++;
          }
        }
      } else {
        for (Array<Double>::contiter out = itsValue.array().cbegin();
             out != itsValue.array().cend(); ++in, ++out, ++itn) {
          *out += *in * *in;
          (*itn)++;
        }
      }
    }
  }
  void TableExprGroupRmssArrayDouble::finish()
  {
    DebugAssert (itsNr.contiguousStorage()  &&  itsValue.contiguousStorage(),
                 AipsError);
    Array<Double>::contiter itv = itsValue.array().cbegin();
    Array<Bool>::contiter itm = itsValue.wmask().cbegin();
    for (Array<Int64>::const_contiter itn = itsNr.cbegin();
         itn != itsNr.cend(); ++itn, ++itv, ++itm) {
      if (*itn > 0) {
        *itv = sqrt(*itv / *itn);
      } else if (itsValue.hasMask()) {
        *itm = True;
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
    MArray<DComplex> arr = itsOperand->getArrayDComplex(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GSUMS")) {
        itsValue.array() = DComplex();
        itsValue.wmask() = True;
      }
      TEGSum (arr, itsValue);
    }
  }

  TableExprGroupProductsArrayDComplex::TableExprGroupProductsArrayDComplex(TableExprNodeRep* node)
    : TableExprGroupFuncArrayDComplex (node)
  {}
  TableExprGroupProductsArrayDComplex::~TableExprGroupProductsArrayDComplex()
  {}
  void TableExprGroupProductsArrayDComplex::apply (const TableExprId& id)
  {
    MArray<DComplex> arr = itsOperand->getArrayDComplex(id);
    if (checkShape (arr, "GPRODUCTS")) {
      itsValue.array() = DComplex(1,0);
      itsValue.wmask() = True;
    }
    TEGProduct (arr, itsValue);
  }
  void TableExprGroupProductsArrayDComplex::finish()
  {
    TEGClearMasked (itsValue);
  }

  TableExprGroupSumSqrsArrayDComplex::TableExprGroupSumSqrsArrayDComplex(TableExprNodeRep* node)
    : TableExprGroupFuncArrayDComplex (node)
  {}
  TableExprGroupSumSqrsArrayDComplex::~TableExprGroupSumSqrsArrayDComplex()
  {}
  void TableExprGroupSumSqrsArrayDComplex::apply (const TableExprId& id)
  {
    MArray<DComplex> arr = itsOperand->getArrayDComplex(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GSUMSQRS")) {
        itsValue.array() = DComplex();
        itsValue.wmask() = True;
      }
      TEGSumSqr (arr, itsValue);
    }
  }

  TableExprGroupMeansArrayDComplex::TableExprGroupMeansArrayDComplex(TableExprNodeRep* node)
    : TableExprGroupFuncArrayDComplex (node)
  {}
  TableExprGroupMeansArrayDComplex::~TableExprGroupMeansArrayDComplex()
  {}
  void TableExprGroupMeansArrayDComplex::apply (const TableExprId& id)
  {
    MArray<DComplex> arr = itsOperand->getArrayDComplex(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GMEANS")) {
        itsValue.array() = DComplex();
        itsValue.wmask() = False;
        itsNr.resize (arr.shape());
        itsNr = 0;
      }
      TEGMeanAdd (arr, itsValue.array(), itsNr);
    }
  }
  void TableExprGroupMeansArrayDComplex::finish()
  {
    TEGMeanFinish (itsValue, itsNr);
  }

  TableExprGroupVariancesArrayDComplex::TableExprGroupVariancesArrayDComplex(TableExprNodeRep* node, uInt ddof)
    : TableExprGroupFuncArrayDouble (node),
      itsDdof (ddof)
  {}
  TableExprGroupVariancesArrayDComplex::~TableExprGroupVariancesArrayDComplex()
  {}
  void TableExprGroupVariancesArrayDComplex::apply (const TableExprId& id)
  {
    // Calculate mean and variance in a running way using a
    // numerically stable algorithm.
    // See en.wikipedia.org/wiki/Algorithms_for_calculating_variance
    MArray<DComplex> arr = itsOperand->getArrayDComplex(id);
    if (! arr.empty()) {
      if (checkShape (arr, "GVARIANCES")) {
        itsValue.array() = 0;
        itsValue.wmask() = False;
        itsCurMean.resize (arr.shape());
        itsNr.resize (arr.shape());
        itsNr = 0;
      }
      Array<DComplex>::contiter itm = itsCurMean.cbegin();
      Array<Int64>::contiter itn = itsNr.cbegin();
      Array<DComplex>::const_iterator in = arr.array().begin();
      if (arr.hasMask()) {
        Array<Bool>::const_iterator min = arr.mask().begin();
        for (Array<Double>::contiter out = itsValue.array().cbegin();
             out != itsValue.array().cend(); ++in, ++min, ++out, ++itm, ++itn) {
          if (! *min) {
            (*itn)++;
            DComplex delta = *in - *itm;
            *itm += delta / Double(*itn);
            DComplex d = *in - *itm;
            *out += real(d)*real(delta) + imag(d)*imag(delta);
          }
        }
      } else {
        for (Array<Double>::contiter out = itsValue.array().cbegin();
             out != itsValue.array().cend(); ++in, ++out, ++itm, ++itn) {
          (*itn)++;
          DComplex delta = *in - *itm;
          *itm += delta / Double(*itn);
          DComplex d = *in - *itm;
          *out += real(d)*real(delta) + imag(d)*imag(delta);
        }
      }
    }
  }
  void TableExprGroupVariancesArrayDComplex::finish()
  {
    DebugAssert (itsNr.contiguousStorage()  &&  itsValue.contiguousStorage(),
                 AipsError);
    Array<Double>::contiter itv = itsValue.array().cbegin();
    Array<Bool>::contiter itm = itsValue.wmask().cbegin();
    for (Array<Int64>::const_contiter itn = itsNr.cbegin();
         itn != itsNr.cend(); ++itn, ++itv, ++itm) {
      if (*itn > itsDdof) {
        *itv /= *itn - itsDdof;
      } else {
        *itv = 0;
        *itm = True;
      }
    }
  }

  TableExprGroupStdDevsArrayDComplex::TableExprGroupStdDevsArrayDComplex(TableExprNodeRep* node, uInt ddof)
    : TableExprGroupVariancesArrayDComplex (node, ddof)
  {}
  TableExprGroupStdDevsArrayDComplex::~TableExprGroupStdDevsArrayDComplex()
  {}
  void TableExprGroupStdDevsArrayDComplex::finish()
  {
    TableExprGroupVariancesArrayDComplex::finish();
    itsValue = sqrt(itsValue);
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
  MArray<Int64> TableExprGroupHistBase::getArrayInt (const vector<TableExprId>&)
  {
    return MArray<Int64>(itsHist);
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
    MArray<Int64> arr = itsOperand->getArrayInt (id);
    // Array does not need to be contiguous, so use iterator.
    if (! arr.hasMask()) {
      Array<Int64>::const_iterator iterEnd = arr.array().end();
      for (Array<Int64>::const_iterator iter = arr.array().begin();
           iter!=iterEnd; ++iter) {
        add (*iter);
      }
    } else {
      Array<Int64>::const_iterator iterEnd = arr.array().end();
      Array<Bool>::const_iterator miter = arr.mask().begin();
      for (Array<Int64>::const_iterator iter = arr.array().begin();
           iter!=iterEnd; ++iter, ++miter) {
        if (!*miter) add (*iter);
      }
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
    MArray<Double> arr = itsOperand->getArrayDouble (id);
    // Array does not need to be contiguous, so use iterator.
    if (! arr.hasMask()) {
      Array<Double>::const_iterator iterEnd = arr.array().end();
      for (Array<Double>::const_iterator iter = arr.array().begin();
           iter!=iterEnd; ++iter) {
        add (*iter);
      }
    } else {
      Array<Double>::const_iterator iterEnd = arr.array().end();
      Array<Bool>::const_iterator miter = arr.mask().begin();
      for (Array<Double>::const_iterator iter = arr.array().begin();
           iter!=iterEnd; ++iter, ++miter) {
        if (!*miter) add (*iter);
      }
    }
  }


} //# NAMESPACE CASACORE - END
