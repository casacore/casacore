//# ExprNodeSetElem.cc: Classes representing a set element in table select expression
//# Copyright (C) 1997,1999,2000,2001,2003
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

#include <casacore/tables/TaQL/ExprNodeSetElem.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprUnitNode.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/tables/TaQL/ExprDerNodeArray.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  TableExprNodeSetElemBase::TableExprNodeSetElemBase (NodeDataType dt)
    : TableExprNodeRep (dt, VTSetElem, OtUndef, Constant)
  {}

  bool TableExprNodeSetElemBase::isDiscrete() const
    { return false; }

  bool TableExprNodeSetElemBase::isSingle() const
    { return false; }

  bool TableExprNodeSetElemBase::isLeftClosed() const
    { return false; }

  bool TableExprNodeSetElemBase::isRightClosed() const
    { return false; }

  bool TableExprNodeSetElemBase::isMidWidth() const
    { return false; }
  
  void TableExprNodeSetElemBase::adaptSetUnits (const Unit& unit)
  {
    if (! unit.empty()) {
      if (itsStart) TableExprNodeUnit::adaptUnit (itsStart, unit);
      if (itsEnd)   TableExprNodeUnit::adaptUnit (itsEnd,   unit);
      if (itsIncr)  TableExprNodeUnit::adaptUnit (itsIncr,  unit);
      setUnit (unit);
    }
  }

  void TableExprNodeSetElemBase::show (ostream& os, uint32_t indent) const
  {
    TableExprNodeRep::show (os, indent);
    if (itsStart) {
      os << "start: ";
      itsStart->show (os, indent+2);
    }
    if (itsEnd) {
      os << "end:   ";
      itsEnd->show (os, indent+2);
    }
    if (itsIncr) {
      os << "incr:  ";
      itsIncr->show (os, indent+2);
    }
  }

  void TableExprNodeSetElemBase::flattenTree (std::vector<TableExprNodeRep*>& nodes)
  {
    nodes.push_back (this);
    if (itsStart) {
      itsStart->flattenTree (nodes);
    }
    if (itsEnd) {
      itsEnd->flattenTree (nodes);
    }
    if (itsIncr) {
      itsIncr->flattenTree (nodes);
    }
  }
  
  void TableExprNodeSetElemBase::fillVector (Vector<bool>&, int64_t&,
                                             const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::fillVector<bool>"); }
  void TableExprNodeSetElemBase::fillVector (Vector<int64_t>&, int64_t&,
                                             const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::fillVector<int64_t>"); }
  void TableExprNodeSetElemBase::fillVector (Vector<double>&, int64_t&,
                                             const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::fillVector<double>"); }
  void TableExprNodeSetElemBase::fillVector (Vector<DComplex>&, int64_t&,
                                             const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::fillVector<DComplex>"); }
  void TableExprNodeSetElemBase::fillVector (Vector<String>&, int64_t&,
                                             const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::fillVector<String>"); }
  void TableExprNodeSetElemBase::fillVector (Vector<MVTime>&, int64_t&,
                                             const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::fillVector<MVTime>"); }

  void TableExprNodeSetElemBase::matchBool     (bool*, const bool*, size_t,
                                                const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::matchBool"); }
  void TableExprNodeSetElemBase::matchInt      (bool*, const int64_t*, size_t,
                                                const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::matchInt"); }
  void TableExprNodeSetElemBase::matchDouble   (bool*, const double*, size_t,
                                                const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::matchDouble"); }
  void TableExprNodeSetElemBase::matchDComplex (bool*, const DComplex*, size_t,
                                                const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::matchDComplex"); }
  void TableExprNodeSetElemBase::matchString   (bool*, const String*, size_t,
                                                const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::matchString"); }
  void TableExprNodeSetElemBase::matchDate     (bool*, const MVTime*, size_t,
                                                const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::matchDate"); }

  void TableExprNodeSetElemBase::setExprType()
  {
    exprtype_p = Constant;
    fillExprType (itsStart.get());
    fillExprType (itsEnd.get());
    fillExprType (itsIncr.get());
  }

  TENShPtr TableExprNodeSetElemBase::evalExpr (const TENShPtr& expr,
                                               const TableExprId& id) const
  {
    if (!expr) {
      return TENShPtr();
    }
    TENShPtr res;
    switch (dataType()) {
    case NTBool:
      res = new TableExprNodeConstBool (expr->getBool (id));
      break;
    case NTInt:
      res = new TableExprNodeConstInt (expr->getInt (id));
      break;
    case NTDouble:
      res = new TableExprNodeConstDouble (expr->getDouble (id));
      break;
    case NTComplex:
      res = new TableExprNodeConstDComplex (expr->getDComplex (id));
      break;
    case NTString:
      res = new TableExprNodeConstString (expr->getString (id));
      break;
    case NTDate:
      // Note that the increment or width for a DateTime is double.
      if (expr->dataType() == NTDate) {
        res = new TableExprNodeConstDate (expr->getDate (id));
      } else {
        res = new TableExprNodeConstDouble (expr->getDouble (id));
      }
      break;
    default:
      TableExprNode::throwInvDT ("TableExprNodeSetElem::evaluate");
    }
    res->setUnit (expr->unit());
    return res;
  }

  void TableExprNodeSetElemBase::getStart (const TableExprId& id, double& v) const
  {
    if (itsStart->dataType() == NTDate) {
      v = itsStart->getDate (id);   // gets converted to days
    } else {
      v = itsStart->getDouble (id);
    }
  }

  void TableExprNodeSetElemBase::getEnd (const TableExprId& id, double& v) const
  {
    if (itsEnd->dataType() == NTDate) {
      v = itsEnd->getDate (id);   // gets converted to days
    } else {
      v = itsEnd->getDouble (id);
    }
  }

  void TableExprNodeSetElemBase::getStart (const TableExprId& id, String& v) const
  {
    v = itsStart->getString (id);
  }

  void TableExprNodeSetElemBase::getEnd (const TableExprId& id, String& v) const
  {
    v = itsEnd->getString (id);
  }



  TableExprNodeSetElemSingle::TableExprNodeSetElemSingle (const TableExprNode& value)
    : TableExprNodeSetElemBase()
  {
    itsStart = value.getRep();
    dtype_p = itsStart->dataType();
    setUnit (itsStart->unit());
    setExprType();
    ndim_p  = value.getNodeRep()->ndim();
    shape_p = value.getNodeRep()->shape();
  }

  TableExprNodeSetElemSingle::TableExprNodeSetElemSingle
  (const TableExprNodeSetElemSingle& that, const TENShPtr& value)
    : TableExprNodeSetElemBase(that.dataType())
  {
    itsStart = value;
    setUnit (itsStart->unit());
  }

  TENSEBShPtr TableExprNodeSetElemSingle::evaluate
  (const TableExprId& id) const
  {
    return TENSEBShPtr(new TableExprNodeSetElemSingle
                       (*this, evalExpr(itsStart, id)));
  }

  bool TableExprNodeSetElemSingle::isDiscrete() const
    { return true; }

  bool TableExprNodeSetElemSingle::isSingle() const
    { return true; }

  void TableExprNodeSetElemSingle::fillVector (Vector<bool>& vec, int64_t& cnt,
                                               const TableExprId& id) const
  {
    int64_t n = vec.size();
    if (n < cnt+1) {
      vec.resize (cnt+64, true);
    }
    vec(cnt++) = itsStart->getBool (id);
  }
  void TableExprNodeSetElemSingle::fillVector (Vector<int64_t>& vec, int64_t& cnt,
                                               const TableExprId& id) const
  {
    int64_t n = vec.size();
    if (n < cnt+1) {
      vec.resize (cnt+64, true);
    }
    vec(cnt++) = itsStart->getInt (id);
  }
  void TableExprNodeSetElemSingle::fillVector (Vector<double>& vec, int64_t& cnt,
                                               const TableExprId& id) const
  {
    int64_t n = vec.size();
    if (n < cnt+1) {
      vec.resize (cnt+64, true);
    }
    vec(cnt++) = itsStart->getDouble (id);
  }
  void TableExprNodeSetElemSingle::fillVector (Vector<DComplex>& vec, int64_t& cnt,
                                               const TableExprId& id) const
  {
    int64_t n = vec.size();
    if (n < cnt+1) {
      vec.resize (cnt+64, true);
    }
    vec(cnt++) = itsStart->getDComplex (id);
  }
  void TableExprNodeSetElemSingle::fillVector (Vector<String>& vec, int64_t& cnt,
                                               const TableExprId& id) const
  {
    int64_t n = vec.size();
    if (n < cnt+1) {
      vec.resize (cnt+64, true);
    }
    vec(cnt++) = itsStart->getString (id);
  }
  void TableExprNodeSetElemSingle::fillVector (Vector<MVTime>& vec, int64_t& cnt,
                                               const TableExprId& id) const
  {
    int64_t n = vec.size();
    if (n < cnt+1) {
      vec.resize (cnt+64, true);
    }
    vec(cnt++) = itsStart->getDate (id);
  }

  void TableExprNodeSetElemSingle::matchBool (bool* match, const bool* value,
                                              size_t nval,
                                              const TableExprId& id) const
  {
    bool* lastVal = match + nval;
    if (itsStart->valueType() == VTArray) {
      TableExprNodeArrayConstBool start (itsStart->getArrayBool(id));
      while (match < lastVal) {
        if (start.contains (id, *value)) {
          *match = true;
        }
        value++;
        match++;
      }
    } else {
      bool start = itsStart->getBool (id);
      while (match < lastVal) {
        if (*value == start) {
          *match = true;
        }
        value++;
        match++;
      }
    }
  }
  void TableExprNodeSetElemSingle::matchInt (bool* match, const int64_t* value,
                                             size_t nval,
                                             const TableExprId& id) const
  {
    bool* lastVal = match + nval;
    if (itsStart->valueType() == VTArray) {
      TableExprNodeArrayConstInt start (itsStart->getArrayInt(id));
      while (match < lastVal) {
        if (start.contains (id, *value)) {
          *match = true;
        }
        value++;
        match++;
      }
    } else {
      int64_t start = itsStart->getInt (id);
      while (match < lastVal) {
        if (*value == start) {
          *match = true;
        }
        value++;
        match++;
      }
    }
  }
  void TableExprNodeSetElemSingle::matchDouble (bool* match, const double* value,
                                                size_t nval,
                                                const TableExprId& id) const
  {
    bool* lastVal = match + nval;
    if (itsStart->valueType() == VTArray) {
      TableExprNodeArrayConstDouble start (itsStart->getArrayDouble(id));
      while (match < lastVal) {
        if (start.contains (id, *value)) {
          *match = true;
        }
        value++;
        match++;
      }
    } else {
      double start = itsStart->getDouble (id);
      while (match < lastVal) {
        if (*value == start) {
          *match = true;
        }
        value++;
        match++;
      }
    }
  }
  void TableExprNodeSetElemSingle::matchDComplex (bool* match, const DComplex* value,
                                                  size_t nval,
                                                  const TableExprId& id) const
  {
    bool* lastVal = match + nval;
    if (itsStart->valueType() == VTArray) {
      TableExprNodeArrayConstDComplex start (itsStart->getArrayDComplex(id));
      while (match < lastVal) {
        if (start.contains (id, *value)) {
          *match = true;
        }
        value++;
        match++;
      }
    } else {
      DComplex start = itsStart->getDComplex (id);
      while (match < lastVal) {
        if (*value == start) {
          *match = true;
        }
        value++;
        match++;
      }
    }
  }
  void TableExprNodeSetElemSingle::matchString (bool* match, const String* value,
                                                size_t nval,
                                                const TableExprId& id) const
  {
    bool* lastVal = match + nval;
    if (itsStart->valueType() == VTArray) {
      TableExprNodeArrayConstString start (itsStart->getArrayString(id));
      while (match < lastVal) {
        if (start.contains (id, *value)) {
          *match = true;
        }
        value++;
        match++;
      }
    } else {
      String start = itsStart->getString (id);
      while (match < lastVal) {
        if (*value == start) {
          *match = true;
        }
        value++;
        match++;
      }
    }
  }
  void TableExprNodeSetElemSingle::matchDate (bool* match, const MVTime* value,
                                              size_t nval,
                                              const TableExprId& id) const
  {
    bool* lastVal = match + nval;
    if (itsStart->valueType() == VTArray) {
      TableExprNodeArrayConstDate start (itsStart->getArrayDate(id));
      while (match < lastVal) {
        if (start.contains (id, *value)) {
          *match = true;
        }
        value++;
        match++;
      }
    } else {
      MVTime start = itsStart->getDate (id);
      while (match < lastVal) {
        if (*value == start) {
          *match = true;
        }
        value++;
        match++;
      }
    }
  }


  
  TableExprNodeSetElemDiscrete::TableExprNodeSetElemDiscrete
  (const TableExprNode& start,
   const TableExprNode& end,
   const TableExprNode& incr,
   bool isEndExcl)
    : TableExprNodeSetElemBase(),
      itsEndExcl (isEndExcl)
  {
    // Start, end and increment are all optional.
    // Get the overall data type and test if they are scalar.
    bool isScalar = true;
    NodeDataType dts = NTInt;
    if (! start.isNull()) {
      itsStart = start.getRep();
      dts = itsStart->dataType();
      isScalar = isScalar && start.isScalar();
    }
    NodeDataType dte = dts;
    if (! end.isNull()) {
      itsEnd = end.getRep();
      dte = itsEnd->dataType();
      isScalar = isScalar && end.isScalar();
    }
    NodeDataType dti = NTInt;
    if (! incr.isNull()) {
      itsIncr = incr.getRep();
      dti = itsIncr->dataType();
      isScalar = isScalar && incr.isScalar();
    }
    if (!isScalar) {
      throw TableInvExpr("Scalar values must be used in start:incr:end");
    }
    if (dts == NTInt  &&  (dte == NTDouble || dti == NTDouble)) dts = NTDouble;
    if (dte == NTInt  &&  (dts == NTDouble || dti == NTDouble)) dte = NTDouble;
    if ((dts != NTInt  &&  dts != NTDouble  &&  dts != NTDate)
        ||  dte != dts  ||  (dti != NTInt  &&  dti != NTDouble)) {
      throw TableInvExpr("start:end should have equal data types (int32_t, double"
                         " or Date) and incr should have int32_t or double");
    }
    // Find unit and adapt units if needed.
    setUnit (TableExprNodeUnit::adaptUnits (itsStart, itsEnd, itsIncr));
    dtype_p = dts;
    setExprType();
  }

  TableExprNodeSetElemDiscrete::TableExprNodeSetElemDiscrete
  (const TableExprNodeSetElemDiscrete& that, const TENShPtr& start,
   const TENShPtr& end, const TENShPtr& incr)
    : TableExprNodeSetElemBase(that.dataType()),
      itsEndExcl (that.itsEndExcl)
  {
    itsStart = start;
    itsEnd = end;
    itsIncr = incr;
    setUnit (that.unit());
  }

  TENSEBShPtr TableExprNodeSetElemDiscrete::evaluate
  (const TableExprId& id) const
  {
    return TENSEBShPtr(new TableExprNodeSetElemDiscrete
                       (*this, evalExpr(itsStart, id),
                        evalExpr(itsEnd, id), evalExpr(itsIncr, id)));
  }

  bool TableExprNodeSetElemDiscrete::isDiscrete() const
    { return true; }

  void TableExprNodeSetElemDiscrete::fillVector (Vector<int64_t>& vec, int64_t& cnt,
                                                 const TableExprId& id) const
  {
    int64_t start = !itsStart  ?  0 : itsStart->getInt (id);
    int64_t end   = !itsEnd  ?  start : itsEnd->getInt (id);
    int64_t incr  = !itsIncr  ?  1 : itsIncr->getInt (id);
    if (incr == 0) {
      throw TableInvExpr("Increment in a range must be non-zero");
    }
    int64_t nval = std::max(int64_t(0), 1 + (end - start) / incr);
    if (itsEndExcl  &&  nval > 0) {
      int64_t rngend = start + (nval-1)*incr;
      if (rngend == end) {
        nval -= 1;
      }
    }
    int64_t n = vec.size();
    if (n < cnt+nval) {
      vec.resize (cnt+max(64,nval), true);
    }
    for (int64_t i=0; i<nval; i++) {
      vec(cnt++) = start;
      start += incr;
    }
  }
  void TableExprNodeSetElemDiscrete::fillVector (Vector<double>& vec, int64_t& cnt,
                                                 const TableExprId& id) const
  {
    double start = !itsStart  ?  0 : itsStart->getDouble (id);
    double end   = !itsEnd  ?  start : itsEnd->getDouble (id);
    double incr  = !itsIncr  ?  1 : itsIncr->getDouble (id);
    if (incr == 0) {
      throw TableInvExpr("Increment in a range must be non-zero");
    }
    int64_t nval = std::max(int64_t(0), int64_t(1 + (end - start) / incr + 1e-10));
    if (itsEndExcl  &&  nval > 0) {
      double rngend = start + (nval-1)*incr;
      if (near(rngend, end)  ||  (end == 0  &&  nearAbs(rngend, end))) {
        nval -= 1;
      }
    }
    int64_t n = vec.size();
    if (n < cnt+nval) {
      vec.resize (cnt+max(64,nval), true);
    }
    for (int64_t i=0; i<nval; i++) {
      vec(cnt++) = start;
      start += incr;
    }
  }
  void TableExprNodeSetElemDiscrete::fillVector (Vector<MVTime>& vec, int64_t& cnt,
                                                 const TableExprId& id) const
  {
    double start = !itsStart  ?  0 : double(itsStart->getDate (id));
    double end   = !itsEnd  ?  start : double(itsEnd->getDate (id));
    double incr  = !itsIncr  ?  1 : itsIncr->getDouble (id);
    if (incr == 0) {
      throw TableInvExpr("Increment in a range must be non-zero");
    }
    int64_t nval = std::max(int64_t(0), int64_t(1 + (end - start) / incr + 1e-10));
    if (itsEndExcl  &&  nval > 0) {
      double rngend = start + (nval-1)*incr;
      if (near(rngend, end)  ||  (end == 0  &&  nearAbs(rngend, end))) {
        nval -= 1;
      }
    }
    int64_t n = vec.size();
    if (n < cnt+nval) {
      vec.resize (cnt+max(64,nval), true);
    }
    for (int64_t i=0; i<nval; i++) {
      vec(cnt++) = start;
      start += incr;
    }
  }

  void TableExprNodeSetElemDiscrete::matchInt (bool* match, const int64_t* value,
                                               size_t nval,
                                               const TableExprId& id) const
  {
    int64_t start = !itsStart  ?  0 : itsStart->getInt (id);
    int64_t end   = !itsEnd  ?  start : itsEnd->getInt (id);
    int64_t incr  = !itsIncr  ?  1 : itsIncr->getInt (id);
    if (incr == 0) {
      throw TableInvExpr("Increment in a range must be non-zero");
    }
    bool* lastVal = match + nval;
    end -= start;
    if (itsEndExcl) {
      end -= 1;
    }
    while (match < lastVal) {
      int64_t tmp = *value - start;
      if (incr > 0) {
        if (tmp >= 0  &&  (!itsEnd  ||  tmp <= end)) {
          if (tmp%incr == 0) {
            *match = true;
          }
        }
      } else {
        if (tmp <= 0  &&  (!itsEnd  ||  tmp >= end)) {
          if (tmp%incr == 0) {
            *match = true;
          }
        }
      }
      value++;
      match++;
    }
  }
  void TableExprNodeSetElemDiscrete::matchDouble (bool* match, const double* value,
                                                  size_t nval,
                                                  const TableExprId& id) const
  {
    double start = !itsStart  ?  0 : itsStart->getDouble (id);
    double end   = !itsEnd  ?  start : itsEnd->getDouble (id);
    double incr  = !itsIncr  ?  1 : itsIncr->getDouble (id);
    if (incr == 0) {
      throw TableInvExpr("Increment in a range must be non-zero");
    }
    bool* lastVal = match + nval;
    end -= start;
    while (match < lastVal) {
      double tmp = *value - start;
      if (incr > 0) {
        if (tmp >= 0  &&  (!itsEnd  ||  tmp < end  ||
                           (!itsEndExcl && tmp==end))) {
          if (near(tmp, incr*int64_t(tmp/incr + 0.5))) {
            *match = true;
          }
        }
      } else {
        if (tmp <= 0  &&  (!itsEnd  ||  tmp > end  ||
                           (!itsEndExcl && tmp==end))) {
          if (near(tmp, incr*int64_t(tmp/incr + 0.5))) {
            *match = true;
          }
        }
      }
      value++;
      match++;
    }
  }
  void TableExprNodeSetElemDiscrete::matchDate (bool* match, const MVTime* value,
                                                size_t nval,
                                                const TableExprId& id) const
  {
    double start = !itsStart  ?  0 : double(itsStart->getDate (id));
    double end   = !itsEnd  ?  start : double(itsEnd->getDate (id));
    double incr  = !itsIncr  ?  1 : itsIncr->getDouble (id);
    if (incr == 0) {
      throw TableInvExpr("Increment in a range must be non-zero");
    }
    bool* lastVal = match + nval;
    end -= start;
    while (match < lastVal) {
      double tmp = double(*value) - start;
      if (incr > 0) {
        if (tmp >= 0  &&  (!itsEnd  ||  tmp < end  ||
                           (!itsEndExcl && tmp==end))) {
          if (near(tmp, incr*int64_t(tmp/incr + 0.5))) {
            *match = true;
          }
        }
      } else {
        if (tmp <= 0  &&  (!itsEnd  ||  tmp > end  ||
                           (!itsEndExcl && tmp==end))) {
          if (near(tmp, incr*int64_t(tmp/incr + 0.5))) {
            *match = true;
          }
        }
      }
      value++;
      match++;
    }
  }



  TableExprNodeSetElemCont::TableExprNodeSetElemCont (bool isLeftClosed,
                                                      const TableExprNode& start,
                                                      const TableExprNode& end,
                                                      bool isRightClosed)
  {
    setup (isLeftClosed, &start, &end, isRightClosed);
  }

  TableExprNodeSetElemCont::TableExprNodeSetElemCont (bool isLeftClosed,
                                                      const TableExprNode& start)
  {
    setup (isLeftClosed, &start, 0, false);
  }

  TableExprNodeSetElemCont::TableExprNodeSetElemCont (const TableExprNode& end,
                                                      bool isRightClosed)
    : TableExprNodeSetElemBase()
  {
    setup (false, 0, &end, isRightClosed);
  }

  TableExprNodeSetElemCont::TableExprNodeSetElemCont (const TableExprNode& mid,
                                                      const TableExprNode& width)
    : TableExprNodeSetElemBase()
  {
    AlwaysAssert (!mid.isNull(), AipsError);
    AlwaysAssert (!width.isNull(), AipsError);
    itsStart = mid.getRep();
    itsEnd   = width.getRep();
    itsLeftClosed  = true;
    itsRightClosed = true;
  }
  
  TableExprNodeSetElemCont::TableExprNodeSetElemCont
  (const TableExprNodeSetElemCont& that, const TENShPtr& start,
   const TENShPtr& end)
    : TableExprNodeSetElemBase(that.dataType()),
      itsLeftClosed  (that.itsLeftClosed),
      itsRightClosed (that.itsRightClosed)
  {
    itsStart = start;
    itsEnd = end;
    setUnit (that.unit());
  }

  void TableExprNodeSetElemCont::setup (bool isLeftClosed,
                                        const TableExprNode* start,
                                        const TableExprNode* end,
                                        bool isRightClosed)
  {
    // Setup for a continuous interval given as start,end.
    // Start or end are optional.
    itsLeftClosed  = isLeftClosed;
    itsRightClosed = isRightClosed;
    bool isScalar  = true;
    if (start) {
      itsStart = start->getRep();
      isScalar = isScalar && start->isScalar();
      // Get data type.
      dtype_p = itsStart->dataType();
      // Integer is handled as double.
      if (dtype_p == NTInt) {
        dtype_p = NTDouble;
      }
    }
    if (end) {
      itsEnd = end->getRep();
      isScalar = isScalar && end->isScalar();
      NodeDataType etype = itsEnd->dataType();
      if (etype == NTInt) {
        etype = NTDouble;
      }
      if (start  &&  etype != dtype_p) {
        throw TableInvExpr ("start=:=end must have equal data types");
      }
      dtype_p = etype;
    }
    if (!isScalar) {
      throw TableInvExpr("Scalar values must be used in start=:=end");
    }
    NodeDataType dt = dataType();
    if (dt != NTDouble  &&  dt != NTString  &&  dt != NTDate) {
      throw TableInvExpr ("start=:=end only valid for "
                          "Int, double, String or Date");
    }
    // Find unit and adapt units if needed.
    setUnit (TableExprNodeUnit::adaptUnits (itsStart, itsEnd, itsIncr));
    setExprType();
  }

  TENSEBShPtr TableExprNodeSetElemCont::evaluate
  (const TableExprId& id) const
  {
    return TENSEBShPtr(new TableExprNodeSetElemCont
                       (*this, evalExpr(itsStart, id),
                        evalExpr(itsEnd, id)));
  }

  bool TableExprNodeSetElemCont::isLeftClosed() const
    { return itsLeftClosed; }

  bool TableExprNodeSetElemCont::isRightClosed() const
    { return itsRightClosed; }

  void TableExprNodeSetElemCont::matchDouble (bool* match, const double* value,
                                              size_t nval,
                                              const TableExprId& id) const
  {
    double start = !itsStart  ?  0 : itsStart->getDouble (id);
    double end   = !itsEnd  ?  start : itsEnd->getDouble (id);
    bool* lastVal = match + nval;
    while (match < lastVal) {
      double tmp = *value;
      if ((!itsStart
           ||  tmp > start  ||  (itsLeftClosed  &&  tmp == start))
          &&  (!itsEnd
               ||  tmp < end  ||  (itsRightClosed  &&  tmp == end))) {
        *match = true;
      }
      value++;
      match++;
    }
  }

  void TableExprNodeSetElemCont::matchString (bool* match, const String* value,
                                              size_t nval,
                                              const TableExprId& id) const
  {
    String start;
    if (itsStart) {
      start = itsStart->getString (id);
    }
    String end;
    if (itsEnd) {
      end = itsEnd->getString (id);
    }
    bool* lastVal = match + nval;
    while (match < lastVal) {
      if ((!itsStart
           ||  *value > start  ||  (itsLeftClosed  &&  *value == start))
          &&  (!itsEnd
               ||  *value < end  ||  (itsRightClosed  &&  *value == end))) {
        *match = true;
      }
      value++;
      match++;
    }
  }

  void TableExprNodeSetElemCont::matchDate (bool* match, const MVTime* value,
                                            size_t nval,
                                            const TableExprId& id) const
  {
    double start = !itsStart  ?  0 : double(itsStart->getDate (id));
    double end   = !itsEnd  ?  start : double(itsEnd->getDate (id));
    bool* lastVal = match + nval;
    while (match < lastVal) {
      double tmp = *value;
      if ((!itsStart
           ||  tmp > start  ||  (itsLeftClosed  &&  tmp == start))
          &&  (!itsEnd
               ||  tmp < end  ||  (itsRightClosed  &&  tmp == end))) {
        *match = true;
      }
      value++;
      match++;
    }
  }



  TableExprNodeSetElemMidWidth::TableExprNodeSetElemMidWidth (const TableExprNode& mid,
                                                              const TableExprNode& width)
    : TableExprNodeSetElemCont (mid, width)
  {
    bool isScalar = mid.isScalar() && width.isScalar();
    // Get data type.
    dtype_p = itsStart->dataType();
    // Integer is handled as double.
    if (dtype_p == NTInt) {
      dtype_p = NTDouble;
    }
    if (dtype_p != NTDouble  &&  dtype_p != NTDate) {
      throw TableInvExpr ("mid<:>width must have an int32_t, double or Datetime mid value");
    }
    if (itsEnd->dataType() != NTInt  &&  itsEnd->dataType() != NTDouble) {
      throw TableInvExpr ("mid<:>width must have an int32_t or double width value");
    }
    if (!isScalar) {
      throw TableInvExpr("Scalar values must be used in mid<:>width");
    }
    // Find unit and adapt units if needed.
    NodeDataType dt = dataType();
    if (dt == NTDouble) {
      setUnit (TableExprNodeUnit::adaptUnits (itsStart, itsEnd, itsIncr));
    } else if (dt == NTDate) {
      TableExprNodeUnit::adaptUnit (itsEnd, "d");
    } else {
      throw TableInvExpr ("mid<:>width only valid for int32_t, double or Date");
    }
    setExprType();
  }

  TENSEBShPtr TableExprNodeSetElemMidWidth::evaluate
  (const TableExprId& id) const
  {
    double start, end, mid, width;
    getEnd(id, width);
    if (width == 0) {
      start = std::numeric_limits<double>::lowest();
      end   = std::numeric_limits<double>::max();
    } else {
      getStart(id, mid);
      start = mid - width*0.5;
      end   = mid + width*0.5;
    }
    TENShPtr startp;
    TENShPtr endp;
    if (dataType() == NTDouble) {
      startp.reset (new TableExprNodeConstDouble(start));
      endp.reset   (new TableExprNodeConstDouble(end));
    } else{
      startp.reset (new TableExprNodeConstDate(MVTime(start)));
      endp.reset   (new TableExprNodeConstDate(MVTime(end)));
    }
    // Create a closed-closed interval.
    return TENSEBShPtr(new TableExprNodeSetElemCont (*this, startp, endp));
  }

  bool TableExprNodeSetElemMidWidth::isMidWidth() const
    { return true; }

  void TableExprNodeSetElemMidWidth::matchDouble (bool* match, const double* value,
                                                  size_t nval,
                                                  const TableExprId& id) const
  {
    double width = itsEnd->getDouble (id);
    double start, end;
    if (width == 0) {
      start = std::numeric_limits<double>::lowest();
      end   = std::numeric_limits<double>::max();
    } else {
      double mid   = itsStart->getDouble (id);
      start = mid - width*0.5;
      end   = mid + width*0.5;
    }
    bool* lastVal = match + nval;
    while (match < lastVal) {
      double tmp = *value;
      if (tmp >= start  &&  tmp <= end) {
        *match = true;
      }
      value++;
      match++;
    }
  }

  void TableExprNodeSetElemMidWidth::matchDate (bool* match, const MVTime* value,
                                                size_t nval,
                                                const TableExprId& id) const
  {
    double mid   = double(itsStart->getDate (id));
    double width = double(itsEnd->getDouble (id));
    double start = mid - width*0.5;
    double end   = mid + width*0.5;
    bool* lastVal = match + nval;
    while (match < lastVal) {
      double tmp = *value;
      if (tmp >= start  &&  tmp <= end) {
        *match = true;
      }
      value++;
      match++;
    }
  }


  
  TableExprNodeSetElem::TableExprNodeSetElem (const TableExprNode& node)
    : itsElem (new TableExprNodeSetElemSingle (node))
  {}

  TableExprNodeSetElem::TableExprNodeSetElem (const TableExprNode* start,
                                              const TableExprNode* end,
                                              const TableExprNode* incr,
                                              bool isEndExcl)
  {
    TableExprNode s (start ? *start : TableExprNode());
    TableExprNode e (end ? *end : TableExprNode());
    TableExprNode i (incr ? *incr : TableExprNode());
    itsElem.reset (new TableExprNodeSetElemDiscrete (s, e, i, isEndExcl));
  }

  TableExprNodeSetElem::TableExprNodeSetElem (bool isLeftClosed,
                                              const TableExprNode& start,
                                              const TableExprNode& end,
                                              bool isRightClosed)
    : itsElem (new TableExprNodeSetElemCont (isLeftClosed, start,
                                             end, isRightClosed))
  {}
  
  TableExprNodeSetElem::TableExprNodeSetElem (bool isLeftClosed,
                                              const TableExprNode& start)
    : itsElem (new TableExprNodeSetElemCont (isLeftClosed, start))
  {}


  TableExprNodeSetElem::TableExprNodeSetElem (const TableExprNode& end,
                                              bool isRightClosed)
    : itsElem (new TableExprNodeSetElemCont (end, isRightClosed))
  {}

  TableExprNodeSetElem::TableExprNodeSetElem (const TableExprNode& mid,
                                              const TableExprNode& width)
    : itsElem (new TableExprNodeSetElemMidWidth (mid, width))
  {}

  
} //# NAMESPACE CASACORE - END
