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

  Bool TableExprNodeSetElemBase::isDiscrete() const
    { return False; }

  Bool TableExprNodeSetElemBase::isSingle() const
    { return False; }

  Bool TableExprNodeSetElemBase::isLeftClosed() const
    { return False; }

  Bool TableExprNodeSetElemBase::isRightClosed() const
    { return False; }

  Bool TableExprNodeSetElemBase::isMidWidth() const
    { return False; }
  
  void TableExprNodeSetElemBase::adaptSetUnits (const Unit& unit)
  {
    if (! unit.empty()) {
      if (itsStart) TableExprNodeUnit::adaptUnit (itsStart, unit);
      if (itsEnd)   TableExprNodeUnit::adaptUnit (itsEnd,   unit);
      if (itsIncr)  TableExprNodeUnit::adaptUnit (itsIncr,  unit);
      setUnit (unit);
    }
  }

  void TableExprNodeSetElemBase::show (ostream& os, uInt indent) const
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
  
  void TableExprNodeSetElemBase::fillVector (Vector<Bool>&, Int64&,
                                             const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::fillVector<Bool>"); }
  void TableExprNodeSetElemBase::fillVector (Vector<Int64>&, Int64&,
                                             const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::fillVector<Int64>"); }
  void TableExprNodeSetElemBase::fillVector (Vector<Double>&, Int64&,
                                             const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::fillVector<Double>"); }
  void TableExprNodeSetElemBase::fillVector (Vector<DComplex>&, Int64&,
                                             const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::fillVector<DComplex>"); }
  void TableExprNodeSetElemBase::fillVector (Vector<String>&, Int64&,
                                             const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::fillVector<String>"); }
  void TableExprNodeSetElemBase::fillVector (Vector<MVTime>&, Int64&,
                                             const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::fillVector<MVTime>"); }

  void TableExprNodeSetElemBase::matchBool     (Bool*, const Bool*, size_t,
                                                const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::matchBool"); }
  void TableExprNodeSetElemBase::matchInt      (Bool*, const Int64*, size_t,
                                                const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::matchInt"); }
  void TableExprNodeSetElemBase::matchDouble   (Bool*, const Double*, size_t,
                                                const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::matchDouble"); }
  void TableExprNodeSetElemBase::matchDComplex (Bool*, const DComplex*, size_t,
                                                const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::matchDComplex"); }
  void TableExprNodeSetElemBase::matchString   (Bool*, const String*, size_t,
                                                const TableExprId&) const
     { throw TableInvExpr ("TableExprNodeSetElem::matchString"); }
  void TableExprNodeSetElemBase::matchDate     (Bool*, const MVTime*, size_t,
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

  void TableExprNodeSetElemBase::getStart (const TableExprId& id, Double& v) const
  {
    if (itsStart->dataType() == NTDate) {
      v = itsStart->getDate (id);   // gets converted to days
    } else {
      v = itsStart->getDouble (id);
    }
  }

  void TableExprNodeSetElemBase::getEnd (const TableExprId& id, Double& v) const
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

  Bool TableExprNodeSetElemSingle::isDiscrete() const
    { return True; }

  Bool TableExprNodeSetElemSingle::isSingle() const
    { return True; }

  void TableExprNodeSetElemSingle::fillVector (Vector<Bool>& vec, Int64& cnt,
                                               const TableExprId& id) const
  {
    Int64 n = vec.size();
    if (n < cnt+1) {
      vec.resize (cnt+64, True);
    }
    vec(cnt++) = itsStart->getBool (id);
  }
  void TableExprNodeSetElemSingle::fillVector (Vector<Int64>& vec, Int64& cnt,
                                               const TableExprId& id) const
  {
    Int64 n = vec.size();
    if (n < cnt+1) {
      vec.resize (cnt+64, True);
    }
    vec(cnt++) = itsStart->getInt (id);
  }
  void TableExprNodeSetElemSingle::fillVector (Vector<Double>& vec, Int64& cnt,
                                               const TableExprId& id) const
  {
    Int64 n = vec.size();
    if (n < cnt+1) {
      vec.resize (cnt+64, True);
    }
    vec(cnt++) = itsStart->getDouble (id);
  }
  void TableExprNodeSetElemSingle::fillVector (Vector<DComplex>& vec, Int64& cnt,
                                               const TableExprId& id) const
  {
    Int64 n = vec.size();
    if (n < cnt+1) {
      vec.resize (cnt+64, True);
    }
    vec(cnt++) = itsStart->getDComplex (id);
  }
  void TableExprNodeSetElemSingle::fillVector (Vector<String>& vec, Int64& cnt,
                                               const TableExprId& id) const
  {
    Int64 n = vec.size();
    if (n < cnt+1) {
      vec.resize (cnt+64, True);
    }
    vec(cnt++) = itsStart->getString (id);
  }
  void TableExprNodeSetElemSingle::fillVector (Vector<MVTime>& vec, Int64& cnt,
                                               const TableExprId& id) const
  {
    Int64 n = vec.size();
    if (n < cnt+1) {
      vec.resize (cnt+64, True);
    }
    vec(cnt++) = itsStart->getDate (id);
  }

  void TableExprNodeSetElemSingle::matchBool (Bool* match, const Bool* value,
                                              size_t nval,
                                              const TableExprId& id) const
  {
    Bool* lastVal = match + nval;
    if (itsStart->valueType() == VTArray) {
      TableExprNodeArrayConstBool start (itsStart->getArrayBool(id));
      while (match < lastVal) {
        if (start.contains (id, *value)) {
          *match = True;
        }
        value++;
        match++;
      }
    } else {
      Bool start = itsStart->getBool (id);
      while (match < lastVal) {
        if (*value == start) {
          *match = True;
        }
        value++;
        match++;
      }
    }
  }
  void TableExprNodeSetElemSingle::matchInt (Bool* match, const Int64* value,
                                             size_t nval,
                                             const TableExprId& id) const
  {
    Bool* lastVal = match + nval;
    if (itsStart->valueType() == VTArray) {
      TableExprNodeArrayConstInt start (itsStart->getArrayInt(id));
      while (match < lastVal) {
        if (start.contains (id, *value)) {
          *match = True;
        }
        value++;
        match++;
      }
    } else {
      Int64 start = itsStart->getInt (id);
      while (match < lastVal) {
        if (*value == start) {
          *match = True;
        }
        value++;
        match++;
      }
    }
  }
  void TableExprNodeSetElemSingle::matchDouble (Bool* match, const Double* value,
                                                size_t nval,
                                                const TableExprId& id) const
  {
    Bool* lastVal = match + nval;
    if (itsStart->valueType() == VTArray) {
      TableExprNodeArrayConstDouble start (itsStart->getArrayDouble(id));
      while (match < lastVal) {
        if (start.contains (id, *value)) {
          *match = True;
        }
        value++;
        match++;
      }
    } else {
      Double start = itsStart->getDouble (id);
      while (match < lastVal) {
        if (*value == start) {
          *match = True;
        }
        value++;
        match++;
      }
    }
  }
  void TableExprNodeSetElemSingle::matchDComplex (Bool* match, const DComplex* value,
                                                  size_t nval,
                                                  const TableExprId& id) const
  {
    Bool* lastVal = match + nval;
    if (itsStart->valueType() == VTArray) {
      TableExprNodeArrayConstDComplex start (itsStart->getArrayDComplex(id));
      while (match < lastVal) {
        if (start.contains (id, *value)) {
          *match = True;
        }
        value++;
        match++;
      }
    } else {
      DComplex start = itsStart->getDComplex (id);
      while (match < lastVal) {
        if (*value == start) {
          *match = True;
        }
        value++;
        match++;
      }
    }
  }
  void TableExprNodeSetElemSingle::matchString (Bool* match, const String* value,
                                                size_t nval,
                                                const TableExprId& id) const
  {
    Bool* lastVal = match + nval;
    if (itsStart->valueType() == VTArray) {
      TableExprNodeArrayConstString start (itsStart->getArrayString(id));
      while (match < lastVal) {
        if (start.contains (id, *value)) {
          *match = True;
        }
        value++;
        match++;
      }
    } else {
      String start = itsStart->getString (id);
      while (match < lastVal) {
        if (*value == start) {
          *match = True;
        }
        value++;
        match++;
      }
    }
  }
  void TableExprNodeSetElemSingle::matchDate (Bool* match, const MVTime* value,
                                              size_t nval,
                                              const TableExprId& id) const
  {
    Bool* lastVal = match + nval;
    if (itsStart->valueType() == VTArray) {
      TableExprNodeArrayConstDate start (itsStart->getArrayDate(id));
      while (match < lastVal) {
        if (start.contains (id, *value)) {
          *match = True;
        }
        value++;
        match++;
      }
    } else {
      MVTime start = itsStart->getDate (id);
      while (match < lastVal) {
        if (*value == start) {
          *match = True;
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
   Bool isEndExcl)
    : TableExprNodeSetElemBase(),
      itsEndExcl (isEndExcl)
  {
    // Start, end and increment are all optional.
    // Get the overall data type and test if they are scalar.
    Bool isScalar = True;
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
      throw TableInvExpr("start:end should have equal data types (Int, Double"
                         " or Date) and incr should have Int or Double");
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

  Bool TableExprNodeSetElemDiscrete::isDiscrete() const
    { return True; }

  void TableExprNodeSetElemDiscrete::fillVector (Vector<Int64>& vec, Int64& cnt,
                                                 const TableExprId& id) const
  {
    DebugAssert (itsDiscrete, AipsError);
    Int64 start = !itsStart  ?  0 : itsStart->getInt (id);
    Int64 end   = !itsEnd  ?  start : itsEnd->getInt (id);
    Int64 incr  = !itsIncr  ?  1 : itsIncr->getInt (id);
    if (incr == 0) {
      throw TableInvExpr("Increment in a range must be non-zero");
    }
    Int64 nval = std::max(Int64(0), 1 + (end - start) / incr);
    if (itsEndExcl  &&  nval > 0) {
      Int64 rngend = start + (nval-1)*incr;
      if (rngend == end) {
        nval -= 1;
      }
    }
    Int64 n = vec.size();
    if (n < cnt+nval) {
      vec.resize (cnt+max(64,nval), True);
    }
    for (Int64 i=0; i<nval; i++) {
      vec(cnt++) = start;
      start += incr;
    }
  }
  void TableExprNodeSetElemDiscrete::fillVector (Vector<Double>& vec, Int64& cnt,
                                                 const TableExprId& id) const
  {
    DebugAssert (itsDiscrete, AipsError);
    Double start = !itsStart  ?  0 : itsStart->getDouble (id);
    Double end   = !itsEnd  ?  start : itsEnd->getDouble (id);
    Double incr  = !itsIncr  ?  1 : itsIncr->getDouble (id);
    if (incr == 0) {
      throw TableInvExpr("Increment in a range must be non-zero");
    }
    Int64 nval = std::max(Int64(0), Int64(1 + (end - start) / incr + 1e-10));
    if (itsEndExcl  &&  nval > 0) {
      Double rngend = start + (nval-1)*incr;
      if (near(rngend, end)  ||  (end == 0  &&  nearAbs(rngend, end))) {
        nval -= 1;
      }
    }
    Int64 n = vec.size();
    if (n < cnt+nval) {
      vec.resize (cnt+max(64,nval), True);
    }
    for (Int64 i=0; i<nval; i++) {
      vec(cnt++) = start;
      start += incr;
    }
  }
  void TableExprNodeSetElemDiscrete::fillVector (Vector<MVTime>& vec, Int64& cnt,
                                                 const TableExprId& id) const
  {
    DebugAssert (itsDiscrete, AipsError);
    Double start = !itsStart  ?  0 : Double(itsStart->getDate (id));
    Double end   = !itsEnd  ?  start : Double(itsEnd->getDate (id));
    Double incr  = !itsIncr  ?  1 : itsIncr->getDouble (id);
    if (incr == 0) {
      throw TableInvExpr("Increment in a range must be non-zero");
    }
    Int64 nval = std::max(Int64(0), Int64(1 + (end - start) / incr + 1e-10));
    if (itsEndExcl  &&  nval > 0) {
      Double rngend = start + (nval-1)*incr;
      if (near(rngend, end)  ||  (end == 0  &&  nearAbs(rngend, end))) {
        nval -= 1;
      }
    }
    Int64 n = vec.size();
    if (n < cnt+nval) {
      vec.resize (cnt+max(64,nval), True);
    }
    for (Int64 i=0; i<nval; i++) {
      vec(cnt++) = start;
      start += incr;
    }
  }

  void TableExprNodeSetElemDiscrete::matchInt (Bool* match, const Int64* value,
                                               size_t nval,
                                               const TableExprId& id) const
  {
    Int64 start = !itsStart  ?  0 : itsStart->getInt (id);
    Int64 end   = !itsEnd  ?  start : itsEnd->getInt (id);
    Int64 incr  = !itsIncr  ?  1 : itsIncr->getInt (id);
    if (incr == 0) {
      throw TableInvExpr("Increment in a range must be non-zero");
    }
    Bool* lastVal = match + nval;
    end -= start;
    if (itsEndExcl) {
      end -= 1;
    }
    while (match < lastVal) {
      Int64 tmp = *value - start;
      if (incr > 0) {
        if (tmp >= 0  &&  (!itsEnd  ||  tmp <= end)) {
          if (tmp%incr == 0) {
            *match = True;
          }
        }
      } else {
        if (tmp <= 0  &&  (!itsEnd  ||  tmp >= end)) {
          if (tmp%incr == 0) {
            *match = True;
          }
        }
      }
      value++;
      match++;
    }
  }
  void TableExprNodeSetElemDiscrete::matchDouble (Bool* match, const Double* value,
                                                  size_t nval,
                                                  const TableExprId& id) const
  {
    Double start = !itsStart  ?  0 : itsStart->getDouble (id);
    Double end   = !itsEnd  ?  start : itsEnd->getDouble (id);
    Double incr  = !itsIncr  ?  1 : itsIncr->getDouble (id);
    if (incr == 0) {
      throw TableInvExpr("Increment in a range must be non-zero");
    }
    Bool* lastVal = match + nval;
    end -= start;
    while (match < lastVal) {
      Double tmp = *value - start;
      if (incr > 0) {
        if (tmp >= 0  &&  (!itsEnd  ||  tmp < end  ||
                           (!itsEndExcl && tmp==end))) {
          if (near(tmp, incr*Int64(tmp/incr + 0.5))) {
            *match = True;
          }
        }
      } else {
        if (tmp <= 0  &&  (!itsEnd  ||  tmp > end  ||
                           (!itsEndExcl && tmp==end))) {
          if (near(tmp, incr*Int64(tmp/incr + 0.5))) {
            *match = True;
          }
        }
      }
      value++;
      match++;
    }
  }
  void TableExprNodeSetElemDiscrete::matchDate (Bool* match, const MVTime* value,
                                                size_t nval,
                                                const TableExprId& id) const
  {
    Double start = !itsStart  ?  0 : Double(itsStart->getDate (id));
    Double end   = !itsEnd  ?  start : Double(itsEnd->getDate (id));
    Double incr  = !itsIncr  ?  1 : itsIncr->getDouble (id);
    if (incr == 0) {
      throw TableInvExpr("Increment in a range must be non-zero");
    }
    Bool* lastVal = match + nval;
    end -= start;
    while (match < lastVal) {
      Double tmp = Double(*value) - start;
      if (incr > 0) {
        if (tmp >= 0  &&  (!itsEnd  ||  tmp < end  ||
                           (!itsEndExcl && tmp==end))) {
          if (near(tmp, incr*Int64(tmp/incr + 0.5))) {
            *match = True;
          }
        }
      } else {
        if (tmp <= 0  &&  (!itsEnd  ||  tmp > end  ||
                           (!itsEndExcl && tmp==end))) {
          if (near(tmp, incr*Int64(tmp/incr + 0.5))) {
            *match = True;
          }
        }
      }
      value++;
      match++;
    }
  }



  TableExprNodeSetElemCont::TableExprNodeSetElemCont (Bool isLeftClosed,
                                                      const TableExprNode& start,
                                                      const TableExprNode& end,
                                                      Bool isRightClosed)
  {
    setup (isLeftClosed, &start, &end, isRightClosed);
  }

  TableExprNodeSetElemCont::TableExprNodeSetElemCont (Bool isLeftClosed,
                                                      const TableExprNode& start)
  {
    setup (isLeftClosed, &start, 0, False);
  }

  TableExprNodeSetElemCont::TableExprNodeSetElemCont (const TableExprNode& end,
                                                      Bool isRightClosed)
    : TableExprNodeSetElemBase()
  {
    setup (False, 0, &end, isRightClosed);
  }

  TableExprNodeSetElemCont::TableExprNodeSetElemCont (const TableExprNode& mid,
                                                      const TableExprNode& width)
    : TableExprNodeSetElemBase()
  {
    AlwaysAssert (!mid.isNull(), AipsError);
    AlwaysAssert (!width.isNull(), AipsError);
    itsStart = mid.getRep();
    itsEnd   = width.getRep();
    itsLeftClosed  = True;
    itsRightClosed = True;
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

  void TableExprNodeSetElemCont::setup (Bool isLeftClosed,
                                        const TableExprNode* start,
                                        const TableExprNode* end,
                                        Bool isRightClosed)
  {
    // Setup for a continuous interval given as start,end.
    // Start or end are optional.
    itsLeftClosed  = isLeftClosed;
    itsRightClosed = isRightClosed;
    Bool isScalar  = True;
    if (start) {
      itsStart = start->getRep();
      isScalar = isScalar && start->isScalar();
      // Get data type.
      dtype_p = itsStart->dataType();
      // Integer is handled as Double.
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
                          "Int, Double, String or Date");
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

  Bool TableExprNodeSetElemCont::isLeftClosed() const
    { return itsLeftClosed; }

  Bool TableExprNodeSetElemCont::isRightClosed() const
    { return itsRightClosed; }

  void TableExprNodeSetElemCont::matchDouble (Bool* match, const Double* value,
                                              size_t nval,
                                              const TableExprId& id) const
  {
    Double start = !itsStart  ?  0 : itsStart->getDouble (id);
    Double end   = !itsEnd  ?  start : itsEnd->getDouble (id);
    Bool* lastVal = match + nval;
    while (match < lastVal) {
      Double tmp = *value;
      if ((!itsStart
           ||  tmp > start  ||  (itsLeftClosed  &&  tmp == start))
          &&  (!itsEnd
               ||  tmp < end  ||  (itsRightClosed  &&  tmp == end))) {
        *match = True;
      }
      value++;
      match++;
    }
  }

  void TableExprNodeSetElemCont::matchString (Bool* match, const String* value,
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
    Bool* lastVal = match + nval;
    while (match < lastVal) {
      if ((!itsStart
           ||  *value > start  ||  (itsLeftClosed  &&  *value == start))
          &&  (!itsEnd
               ||  *value < end  ||  (itsRightClosed  &&  *value == end))) {
        *match = True;
      }
      value++;
      match++;
    }
  }

  void TableExprNodeSetElemCont::matchDate (Bool* match, const MVTime* value,
                                            size_t nval,
                                            const TableExprId& id) const
  {
    Double start = !itsStart  ?  0 : Double(itsStart->getDate (id));
    Double end   = !itsEnd  ?  start : Double(itsEnd->getDate (id));
    Bool* lastVal = match + nval;
    while (match < lastVal) {
      Double tmp = *value;
      if ((!itsStart
           ||  tmp > start  ||  (itsLeftClosed  &&  tmp == start))
          &&  (!itsEnd
               ||  tmp < end  ||  (itsRightClosed  &&  tmp == end))) {
        *match = True;
      }
      value++;
      match++;
    }
  }



  TableExprNodeSetElemMidWidth::TableExprNodeSetElemMidWidth (const TableExprNode& mid,
                                                              const TableExprNode& width)
    : TableExprNodeSetElemCont (mid, width)
  {
    Bool isScalar = mid.isScalar() && width.isScalar();
    // Get data type.
    dtype_p = itsStart->dataType();
    // Integer is handled as Double.
    if (dtype_p == NTInt) {
      dtype_p = NTDouble;
    }
    if (dtype_p != NTDouble  &&  dtype_p != NTDate) {
      throw TableInvExpr ("mid<:>width must have an Int, Double or Datetime mid value");
    }
    if (itsEnd->dataType() != NTInt  &&  itsEnd->dataType() != NTDouble) {
      throw TableInvExpr ("mid<:>width must have an Int or Double width value");
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
      throw TableInvExpr ("mid<:>width only valid for Int, Double or Date");
    }
    setExprType();
  }

  TENSEBShPtr TableExprNodeSetElemMidWidth::evaluate
  (const TableExprId& id) const
  {
    Double start, end, mid, width;
    getEnd(id, width);
    if (width == 0) {
      start = std::numeric_limits<Double>::lowest();
      end   = std::numeric_limits<Double>::max();
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

  Bool TableExprNodeSetElemMidWidth::isMidWidth() const
    { return True; }

  void TableExprNodeSetElemMidWidth::matchDouble (Bool* match, const Double* value,
                                                  size_t nval,
                                                  const TableExprId& id) const
  {
    Double width = itsEnd->getDouble (id);
    Double start, end;
    if (width == 0) {
      start = std::numeric_limits<Double>::lowest();
      end   = std::numeric_limits<Double>::max();
    } else {
      Double mid   = itsStart->getDouble (id);
      start = mid - width*0.5;
      end   = mid + width*0.5;
    }
    Bool* lastVal = match + nval;
    while (match < lastVal) {
      Double tmp = *value;
      if (tmp >= start  &&  tmp <= end) {
        *match = True;
      }
      value++;
      match++;
    }
  }

  void TableExprNodeSetElemMidWidth::matchDate (Bool* match, const MVTime* value,
                                                size_t nval,
                                                const TableExprId& id) const
  {
    Double mid   = Double(itsStart->getDate (id));
    Double width = Double(itsEnd->getDouble (id));
    Double start = mid - width*0.5;
    Double end   = mid + width*0.5;
    Bool* lastVal = match + nval;
    while (match < lastVal) {
      Double tmp = *value;
      if (tmp >= start  &&  tmp <= end) {
        *match = True;
      }
      value++;
      match++;
    }
  }


  
  TableExprNodeSetElem::TableExprNodeSetElem (const TENSEBShPtr& elem)
    : TableExprNodeRep (NTBool, VTSetElem, OtUndef, Constant),
      itsElem (elem)
  {
    init();
  }

  TableExprNodeSetElem::TableExprNodeSetElem (const TableExprNode& node)
    : TableExprNodeRep (NTBool, VTSetElem, OtUndef, Constant),
      itsElem (new TableExprNodeSetElemSingle (node))
  {
    init();
  }

  TableExprNodeSetElem::TableExprNodeSetElem (const TableExprNode* start,
                                              const TableExprNode* end,
                                              const TableExprNode* incr,
                                              Bool isEndExcl)
    : TableExprNodeRep (NTBool, VTSetElem, OtUndef, Constant)
  {
    TableExprNode s (start ? *start : TableExprNode());
    TableExprNode e (end ? *end : TableExprNode());
    TableExprNode i (incr ? *incr : TableExprNode());
    itsElem.reset (new TableExprNodeSetElemDiscrete (s, e, i, isEndExcl));
    init();
  }

  TableExprNodeSetElem::TableExprNodeSetElem (Bool isLeftClosed,
                                              const TableExprNode& start,
                                              const TableExprNode& end,
                                              Bool isRightClosed)
    : TableExprNodeRep (NTBool, VTSetElem, OtUndef, Constant),
      itsElem (new TableExprNodeSetElemCont (isLeftClosed, start,
                                             end, isRightClosed))
  {
    init();
  }
  
  TableExprNodeSetElem::TableExprNodeSetElem (Bool isLeftClosed,
                                              const TableExprNode& start)
    : TableExprNodeRep (NTBool, VTSetElem, OtUndef, Constant),
      itsElem (new TableExprNodeSetElemCont (isLeftClosed, start))
  {
    init();
  }

  TableExprNodeSetElem::TableExprNodeSetElem (const TableExprNode& end,
                                              Bool isRightClosed)
    : TableExprNodeRep (NTBool, VTSetElem, OtUndef, Constant),
      itsElem (new TableExprNodeSetElemCont (end, isRightClosed))
  {
    init();
  }

  TableExprNodeSetElem::TableExprNodeSetElem (const TableExprNode& mid,
                                              const TableExprNode& width)
    : TableExprNodeRep (NTBool, VTSetElem, OtUndef, Constant),
      itsElem (new TableExprNodeSetElemMidWidth (mid, width))
  {
    init();
  }

  void TableExprNodeSetElem::init()
  {
    dtype_p    = itsElem->dataType();
    exprtype_p = itsElem->exprType();
  }
  
} //# NAMESPACE CASACORE - END
