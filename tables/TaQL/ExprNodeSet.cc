//# ExprNodeSet.cc: Classes representing a set in table select expression
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
//#
//# $Id: ExprNodeSet.cc 21262 2012-09-07 12:38:36Z gervandiepen $

#include <casacore/tables/TaQL/ExprNodeSet.h>
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

TableExprNodeSetElem::TableExprNodeSetElem (const TableExprNode& value)
: TableExprNodeRep (NTDouble, VTSetElem, OtUndef, Table()),
  itsStart       (0),
  itsEnd         (0),
  itsIncr        (0),
  itsEndExcl     (False),
  itsLeftClosed  (True),
  itsRightClosed (True),
  itsDiscrete    (True),
  itsSingle      (True)
{
    itsStart = value.getRep();
    dtype_p = itsStart->dataType();
    setUnit (itsStart->unit());
    checkTable();
}

TableExprNodeSetElem::TableExprNodeSetElem (const TableExprNode* start,
                                            const TableExprNode* end,
                                            const TableExprNode* incr,
                                            Bool isEndExcl)
: TableExprNodeRep (NTDouble, VTSetElem, OtUndef, Table()),
  itsStart       (0),
  itsEnd         (0),
  itsIncr        (0),
  itsEndExcl     (isEndExcl),
  itsLeftClosed  (True),
  itsRightClosed (True),
  itsDiscrete    (True),
  itsSingle      (False)
{
    // Link to the nodes and determine the data types.
    //# Note that the TableExprNode copy ctor is needed to get rid of const.
    Bool isScalar = True;
    NodeDataType dts = NTInt;
    if (start) {
        itsStart = start->getRep();
        dts = itsStart->dataType();
        isScalar = isScalar && start->isScalar();
    }
    NodeDataType dte = dts;
    if (end) {
      itsEnd = end->getRep();
        dte = itsEnd->dataType();
        isScalar = isScalar && end->isScalar();
    }
    NodeDataType dti = NTInt;
    if (incr) {
        itsIncr = incr->getRep();
        dti = itsIncr->dataType();
        isScalar = isScalar && incr->isScalar();
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
    checkTable();
}

TableExprNodeSetElem::TableExprNodeSetElem (Bool isLeftClosed,
                                            const TableExprNode& start,
                                            const TableExprNode& end,
                                            Bool isRightClosed)
: TableExprNodeRep (NTDouble, VTSetElem, OtUndef, Table())
{
    setup (isLeftClosed, &start, &end, isRightClosed);
}

TableExprNodeSetElem::TableExprNodeSetElem (Bool isLeftClosed,
                                            const TableExprNode& start)
: TableExprNodeRep (NTDouble, VTSetElem, OtUndef, Table())
{
    setup (isLeftClosed, &start, 0, False);
}

TableExprNodeSetElem::TableExprNodeSetElem (const TableExprNode& end,
                                            Bool isRightClosed)
: TableExprNodeRep (NTDouble, VTSetElem, OtUndef, Table())
{
    setup (False, 0, &end, isRightClosed);
}

TableExprNodeSetElem::TableExprNodeSetElem (const TableExprNodeSetElem& that)
: TableExprNodeRep (that),
  itsStart         (that.itsStart),
  itsEnd           (that.itsEnd),
  itsIncr          (that.itsIncr),
  itsEndExcl       (that.itsEndExcl),
  itsLeftClosed    (that.itsLeftClosed),
  itsRightClosed   (that.itsRightClosed),
  itsDiscrete      (that.itsDiscrete),
  itsSingle        (that.itsSingle)
{}

TableExprNodeSetElem::TableExprNodeSetElem (const TableExprNodeSetElem& that,
                                            const TENShPtr& start,
                                            const TENShPtr& end,
                                            const TENShPtr& incr)
: TableExprNodeRep (that.dataType(), VTSetElem, OtUndef, Table()),
  itsStart         (start),
  itsEnd           (end),
  itsIncr          (incr),
  itsEndExcl       (that.itsEndExcl),
  itsLeftClosed    (that.itsLeftClosed),
  itsRightClosed   (that.itsRightClosed),
  itsDiscrete      (that.itsDiscrete),
  itsSingle        (that.itsSingle)
{
    TableExprNodeUnit::adaptUnits (itsStart, itsEnd, itsIncr);
}

TableExprNodeSetElem::~TableExprNodeSetElem()
{}

void TableExprNodeSetElem::setup (Bool isLeftClosed,
                                  const TableExprNode* start,
                                  const TableExprNode* end,
                                  Bool isRightClosed)
{
    itsStart       = 0;
    itsEnd         = 0;
    itsIncr        = 0;
    itsEndExcl     = False;
    itsLeftClosed  = isLeftClosed;
    itsRightClosed = isRightClosed;
    itsDiscrete    = False;
    itsSingle      = False;
    Bool isScalar  = True;
    //# Note that the TableExprNode copy ctor is needed to get rid of const.
    if (start) {
        itsStart = start->getRep();
        isScalar = isScalar && start->isScalar();
        // Get data type; integer continuous interval is always double
        dtype_p = itsStart->dataType();
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
            throw (TableInvExpr ("start=:=end must have equal data types"));
        }
        dtype_p = etype;
    }
    if (!isScalar) {
        throw TableInvExpr("Scalar values must be used in start:=:end");
    }
    NodeDataType dt = dataType();
    if (dt != NTInt  &&  dt != NTDouble  &&  dt != NTString
        &&  dt != NTDate) {
      throw (TableInvExpr ("start:=:end only valid for Int, Double,"
                             " String or Date"));
    }
    // Find unit and adapt units if needed.
    setUnit (TableExprNodeUnit::adaptUnits (itsStart, itsEnd, itsIncr));
    checkTable();
}

void TableExprNodeSetElem::adaptSetUnits (const Unit& unit)
{
    if (! unit.empty()) {
        if (itsStart) TableExprNodeUnit::adaptUnit (itsStart, unit);
        if (itsEnd)   TableExprNodeUnit::adaptUnit (itsEnd,   unit);
        if (itsIncr)  TableExprNodeUnit::adaptUnit (itsIncr,  unit);
        setUnit (unit);
    }
}

void TableExprNodeSetElem::show (ostream& os, uInt indent) const
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

void TableExprNodeSetElem::getAggrNodes (vector<TableExprNodeRep*>& aggr)
{
    if (itsStart) {
        itsStart->getAggrNodes (aggr);
    }
    if (itsEnd) {
        itsEnd->getAggrNodes (aggr);
    }
    if (itsIncr) {
        itsIncr->getAggrNodes (aggr);
    }
}
  
void TableExprNodeSetElem::getColumnNodes (vector<TableExprNodeRep*>& cols)
{
    if (itsStart) {
        itsStart->getColumnNodes (cols);
    }
    if (itsEnd) {
        itsEnd->getColumnNodes (cols);
    }
    if (itsIncr) {
        itsIncr->getColumnNodes (cols);
    }
}
  
void TableExprNodeSetElem::checkTable()
{
    table_p = Table();
    checkTablePtr (itsStart);
    checkTablePtr (itsEnd);
    checkTablePtr (itsIncr);
    exprtype_p = Constant;
    fillExprType (itsStart);
    fillExprType (itsEnd);
    fillExprType (itsIncr);
}

TableExprNodeSetElem* TableExprNodeSetElem::evaluate
                                           (const TableExprId& id) const
{
    TENShPtr start;
    TENShPtr end;
    TENShPtr incr;
    switch (dataType()) {
    case NTBool:
        if (itsStart) {
            start = new TableExprNodeConstBool (itsStart->getBool (id));
        }
        break;
    case NTInt:
        if (itsStart) {
            start = new TableExprNodeConstInt (itsStart->getInt (id));
        }
        if (itsEnd) {
            end = new TableExprNodeConstInt (itsEnd->getInt (id));
        }
        if (itsIncr) {
            incr = new TableExprNodeConstInt (itsIncr->getInt (id));
        }
        break;
    case NTDouble:
        if (itsStart) {
            start = new TableExprNodeConstDouble (itsStart->getDouble (id));
        }
        if (itsEnd) {
            end = new TableExprNodeConstDouble (itsEnd->getDouble (id));
        }
        if (itsIncr) {
            incr = new TableExprNodeConstDouble (itsIncr->getDouble (id));
        }
        break;
    case NTComplex:
        if (itsStart) {
            start = new TableExprNodeConstDComplex
                                            (itsStart->getDComplex (id));
        }
        break;
    case NTString:
        if (itsStart) {
            start = new TableExprNodeConstString (itsStart->getString (id));
        }
        if (itsEnd) {
            end = new TableExprNodeConstString (itsEnd->getString (id));
        }
        break;
    case NTDate:
        if (itsStart) {
            start = new TableExprNodeConstDate (itsStart->getDate (id));
        }
        if (itsEnd) {
            end = new TableExprNodeConstDate (itsEnd->getDate (id));
        }
        if (itsIncr) {
            incr = new TableExprNodeConstDouble (itsIncr->getDouble (id));
        }
        break;
    default:
        TableExprNode::throwInvDT ("TableExprNodeSetElem::evaluate");
    }
    return new TableExprNodeSetElem (*this, start, end, incr);
}

void TableExprNodeSetElem::fillVector (Vector<Bool>& vec, Int64& cnt,
                                       const TableExprId& id) const
{
    DebugAssert (itsSingle, AipsError);
    Int64 n = vec.size();
    if (n < cnt+1) {
        vec.resize (cnt+64, True);
    }
    vec(cnt++) = itsStart->getBool (id);
}
void TableExprNodeSetElem::fillVector (Vector<Int64>& vec, Int64& cnt,
                                       const TableExprId& id) const
{
    DebugAssert (itsDiscrete, AipsError);
    Int64 start = itsStart==0  ?  0 : itsStart->getInt (id);
    Int64 end   = itsEnd==0  ?  start : itsEnd->getInt (id);
    Int64 incr  = itsIncr==0  ?  1 : itsIncr->getInt (id);
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
void TableExprNodeSetElem::fillVector (Vector<Double>& vec, Int64& cnt,
                                       const TableExprId& id) const
{
    DebugAssert (itsDiscrete, AipsError);
    Double start = itsStart==0  ?  0 : itsStart->getDouble (id);
    Double end   = itsEnd==0  ?  start : itsEnd->getDouble (id);
    Double incr  = itsIncr==0  ?  1 : itsIncr->getDouble (id);
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
void TableExprNodeSetElem::fillVector (Vector<DComplex>& vec, Int64& cnt,
                                       const TableExprId& id) const
{
    DebugAssert (itsSingle, AipsError);
    Int64 n = vec.size();
    if (n < cnt+1) {
        vec.resize (cnt+64, True);
    }
    vec(cnt++) = itsStart->getDComplex (id);
}
void TableExprNodeSetElem::fillVector (Vector<String>& vec, Int64& cnt,
                                       const TableExprId& id) const
{
    DebugAssert (itsSingle, AipsError);
    Int64 n = vec.size();
    if (n < cnt+1) {
        vec.resize (cnt+64, True);
    }
    vec(cnt++) = itsStart->getString (id);
}
void TableExprNodeSetElem::fillVector (Vector<MVTime>& vec, Int64& cnt,
                                       const TableExprId& id) const
{
    DebugAssert (itsDiscrete, AipsError);
    Double start = itsStart==0  ?  0 : Double(itsStart->getDate (id));
    Double end   = itsEnd==0  ?  start : Double(itsEnd->getDate (id));
    Double incr  = itsIncr==0  ?  1 : itsIncr->getDouble (id);
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

void TableExprNodeSetElem::matchBool (Bool* match, const Bool* value,
                                      size_t nval,
                                      const TableExprId& id) const
{
    DebugAssert (itsSingle, AipsError);
    Bool start = itsStart->getBool (id);
    Bool* lastVal = match + nval;
    while (match < lastVal) {
        if (*value == start) {
            *match = True;
        }
        value++;
        match++;
    }
}
void TableExprNodeSetElem::matchInt (Bool* match, const Int64* value,
                                     size_t nval,
                                     const TableExprId& id) const
{
    Int64 start = itsStart==0  ?  0 : itsStart->getInt (id);
    Int64 end   = itsEnd==0  ?  start : itsEnd->getInt (id);
    Int64 incr  = itsIncr==0  ?  1 : itsIncr->getInt (id);
    if (incr == 0) {
      throw TableInvExpr("Increment in a range must be non-zero");
    }
    Bool* lastVal = match + nval;
    if (itsSingle) {
        while (match < lastVal) {
            if (*value == start) {
                *match = True;
            }
            value++;
            match++;
        }
    } else if (itsDiscrete) {
        end -= start;
        if (itsEndExcl) {
            end -= 1;
        }
        while (match < lastVal) {
            Int64 tmp = *value - start;
            if (incr > 0) {
                if (tmp >= 0  &&  (itsEnd == 0  ||  tmp <= end)) {
                    if (tmp%incr == 0) {
                        *match = True;
                    }
                }
            } else {
                if (tmp <= 0  &&  (itsEnd == 0  ||  tmp >= end)) {
                    if (tmp%incr == 0) {
                        *match = True;
                    }
                }
            }
            value++;
            match++;
        }
    }else{
        while (match < lastVal) {
            Int64 tmp = *value;
            if ((itsStart == 0
             ||  tmp > start  ||  (itsLeftClosed  &&  tmp == start))
            &&  (itsEnd == 0
             ||  tmp < end  ||  (itsRightClosed  &&  tmp == end))) {
                *match = True;
            }
            value++;
            match++;
        }
    }
}
void TableExprNodeSetElem::matchDouble (Bool* match, const Double* value,
                                        size_t nval,
                                        const TableExprId& id) const
{
    Double start = itsStart==0  ?  0 : itsStart->getDouble (id);
    Double end   = itsEnd==0  ?  start : itsEnd->getDouble (id);
    Double incr  = itsIncr==0  ?  1 : itsIncr->getDouble (id);
    if (incr == 0) {
      throw TableInvExpr("Increment in a range must be non-zero");
    }
    Bool* lastVal = match + nval;
    if (itsSingle) {
        while (match < lastVal) {
            if (*value == start) {
                *match = True;
            }
            value++;
            match++;
        }
    } else if (itsDiscrete) {
        end -= start;
        while (match < lastVal) {
            Double tmp = *value - start;
            if (incr > 0) {
                if (tmp >= 0  &&  (itsEnd == 0  ||  tmp < end  ||
                                   (!itsEndExcl && tmp==end))) {
                    if (near(tmp, incr*Int64(tmp/incr + 0.5))) {
                        *match = True;
                    }
                }
            } else {
                if (tmp <= 0  &&  (itsEnd == 0  ||  tmp > end  ||
                                   (!itsEndExcl && tmp==end))) {
                    if (near(tmp, incr*Int64(tmp/incr + 0.5))) {
                        *match = True;
                    }
                }
            }
            value++;
            match++;
        }
    }else{
        while (match < lastVal) {
            Double tmp = *value;
            if ((itsStart == 0
             ||  tmp > start  ||  (itsLeftClosed  &&  tmp == start))
            &&  (itsEnd == 0
             ||  tmp < end  ||  (itsRightClosed  &&  tmp == end))) {
                *match = True;
            }
            value++;
            match++;
        }
    }
}
void TableExprNodeSetElem::matchDComplex (Bool* match, const DComplex* value,
                                          size_t nval,
                                          const TableExprId& id) const
{
    DebugAssert (itsSingle, AipsError);
    DComplex start = itsStart->getDComplex (id);
    Bool* lastVal = match + nval;
    while (match < lastVal) {
        if (*value == start) {
            *match = True;
        }
        value++;
        match++;
    }
}
void TableExprNodeSetElem::matchString (Bool* match, const String* value,
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
    if (itsDiscrete) {
        DebugAssert (itsSingle, AipsError);
        while (match < lastVal) {
            if (*value == start) {
                *match = True;
            }
            value++;
            match++;
        }
    }else{
        while (match < lastVal) {
            if ((itsStart == 0
             ||  *value > start  ||  (itsLeftClosed  &&  *value == start))
            &&  (itsEnd == 0
             ||  *value < end  ||  (itsRightClosed  &&  *value == end))) {
                *match = True;
            }
            value++;
            match++;
        }
    }
}
void TableExprNodeSetElem::matchDate (Bool* match, const MVTime* value,
                                      size_t nval,
                                      const TableExprId& id) const
{
    Double start = itsStart==0  ?  0 : Double(itsStart->getDate (id));
    Double end   = itsEnd==0  ?  start : Double(itsEnd->getDate (id));
    Double incr  = itsIncr==0  ?  1 : itsIncr->getDouble (id);
    if (incr == 0) {
      throw TableInvExpr("Increment in a range must be non-zero");
    }
    Bool* lastVal = match + nval;
    if (itsSingle) {
        while (match < lastVal) {
            if (Double(*value) == start) {
                *match = True;
            }
            value++;
            match++;
        }
    } else if (itsDiscrete) {
        end -= start;
        while (match < lastVal) {
            Double tmp = Double(*value) - start;
            if (incr > 0) {
                if (tmp >= 0  &&  (itsEnd == 0  ||  tmp < end  ||
                                   (!itsEndExcl && tmp==end))) {
                    if (near(tmp, incr*Int64(tmp/incr + 0.5))) {
                        *match = True;
                    }
                }
            } else {
                if (tmp <= 0  &&  (itsEnd == 0  ||  tmp > end  ||
                                   (!itsEndExcl && tmp==end))) {
                    if (near(tmp, incr*Int64(tmp/incr + 0.5))) {
                        *match = True;
                    }
                }
            }
            value++;
            match++;
        }
    }else{
        while (match < lastVal) {
            Double tmp = *value;
            if ((itsStart == 0
             ||  tmp > start  ||  (itsLeftClosed  &&  tmp == start))
            &&  (itsEnd == 0
             ||  tmp < end  ||  (itsRightClosed  &&  tmp == end))) {
                *match = True;
            }
            value++;
            match++;
        }
    }
}



TableExprNodeSet::TableExprNodeSet()
: TableExprNodeRep (NTNumeric, VTSet, OtUndef, Table()),
  itsSingle        (True),
  itsDiscrete      (True),
  itsBounded       (True),
  itsCheckTypes    (True),
  itsAllIntervals  (False),
  itsFindFunc      (0)
{}

TableExprNodeSet::TableExprNodeSet (const IPosition& indices)
: TableExprNodeRep (NTInt, VTSet, OtUndef, Table()),
  itsSingle        (True),
  itsDiscrete      (True),
  itsBounded       (True),
  itsCheckTypes    (False),
  itsAllIntervals  (False),
  itsFindFunc      (0)
{
    uInt n = indices.size();
    itsElems.resize (n);
    for (uInt i=0; i<n; i++) {
      itsElems[i] = new TableExprNodeSetElem (TableExprNode (Int64(indices(i))));
    }
}

TableExprNodeSet::TableExprNodeSet (const Slicer& indices)
: TableExprNodeRep (NTInt, VTSet, OtUndef, Table()),
  itsSingle        (False),
  itsDiscrete      (True),
  itsBounded       (True),
  itsCheckTypes    (False),
  itsAllIntervals  (False),
  itsFindFunc      (0)
{
    TableExprNode start;
    TableExprNode end;
    TableExprNode* startp;
    TableExprNode* endp;
    uInt n = indices.ndim();
    itsElems.resize (n);
    for (uInt i=0; i<n; i++) {
        startp = endp = 0;
        if (indices.start()(i) != Slicer::MimicSource) {
          start = TableExprNode (Int64(indices.start()(i)));
            startp = &start;
        }
        if (indices.end()(i) != Slicer::MimicSource) {
          end = TableExprNode (Int64(indices.end()(i)));
            endp = &end;
        }
        TableExprNode incr (Int64(indices.stride()(i)));
        itsElems[i] = new TableExprNodeSetElem (startp, endp, &incr);
    }
}

TableExprNodeSet::TableExprNodeSet (const Vector<rownr_t>& rownrs,
                                    const TableExprNodeSet& set)
: TableExprNodeRep (set.dataType(), VTSet, OtUndef, Table()),
  itsElems         (rownrs.size() * set.size()),
  itsSingle        (set.isSingle()),
  itsDiscrete      (set.isDiscrete()),
  itsBounded       (set.isBounded()),
  itsCheckTypes    (False),
  itsAllIntervals  (False),
  itsFindFunc      (0)
{
    // Fill in all values.
    size_t nrel = set.size();
    for (rownr_t i=0; i<rownrs.size(); i++) {
        for (size_t j=0; j<nrel; j++) {
            itsElems[j+i*nrel] = set[j].evaluate (rownrs[i]);
        }
    }
    // Try to combine multiple intervals; it can improve performance a lot.
    if (rownrs.size() > 1  &&  !isSingle()  &&  !isDiscrete()) {
        if (set.dataType() == NTInt) {
            combineIntIntervals();
        } else if (set.dataType() == NTDouble) {
            combineDoubleIntervals();
        } else if (set.dataType() == NTDate) {
            combineDateIntervals();
        }
    }
    setUnit (set.unit());
}

TableExprNodeSet::TableExprNodeSet (const TableExprNodeSet& that)
: TableExprNodeRep (that),
  itsSingle        (that.itsSingle),
  itsDiscrete      (that.itsDiscrete),
  itsBounded       (that.itsBounded),
  itsCheckTypes    (that.itsCheckTypes),
  itsAllIntervals  (that.itsAllIntervals),
  itsStart         (that.itsStart),
  itsEnd           (that.itsEnd),
  itsFindFunc      (that.itsFindFunc)
{
  size_t n = that.itsElems.size();
  itsElems.resize (n);
  for (size_t i=0; i<n; i++) {
    itsElems[i] = new TableExprNodeSetElem (*castSetElem(that.itsElems[i]));
  }
}

TableExprNodeSet::~TableExprNodeSet()
{}

void TableExprNodeSet::combineIntIntervals()
{
  DebugAssert (itsElems.size() > 0, AipsError);
  // Make an id (with an arbitrary row number) for the gets.
  TableExprId id(0);
  std::vector<TENShPtr> elems(1);
  TableExprNodeSetElem& elem = *(castItsElem(0));
  if (elem.start() == 0) {
    // No start value, so only the highest end value is relevant.
    // Make a single interval with the used open/closed-ness.
    Int64 val = elem.end()->getInt(id);
    for (size_t i=1; i<itsElems.size(); i++) {
      Int64 valn = castItsElem(i)->end()->getInt(id);
      if (valn > val) {
        val = valn;
      }
    }
    elems[0] = new TableExprNodeSetElem(TableExprNode(val),
                                        elem.isRightClosed());
  } else if (elem.end() == 0) {
    // No end value, so only the lowest start value is relevant.
    Int64 val = elem.start()->getInt(id);
    for (size_t i=1; i<itsElems.size(); i++) {
      Int64 valn = castItsElem(i)->start()->getInt(id);
      if (valn < val) {
        val = valn;
      }
    }
    elems[0] = new TableExprNodeSetElem(elem.isLeftClosed(),
                                        TableExprNode(val));
  } else {
    // The intervals contain both a start and an end value.
    // Make the block large enough for all possible intervals.
    elems.resize (itsElems.size());
    size_t nelem = 0;
    // Get all start values and sort them (indirectly) in ascending order.
    Block<Int64> vals(itsElems.size());
    for (size_t i=0; i<itsElems.size(); i++) {
      vals[i] = castItsElem(i)->start()->getInt(id);
    }
    Vector<uInt> index;
    GenSortIndirect<Int64,uInt>::sort (index, vals, vals.size());
    // Get the start and end value of first interval in sorted list.
    Int64 stval  = vals[index[0]];
    Int64 endval = castItsElem(index[0])->end()->getInt(id);
    // Loop through the next intervals and combine if possible.
    for (uInt i=1; i<index.size(); i++) {
      Int inx = index[i];
      Int64 st2 = vals[inx];
      Int64 end2 = castItsElem(inx)->end()->getInt(id);
      // Combine intervals if they overlap.
      // They do if the next interval starts before end of this one
      // or if starting at the end and one side of the interval is closed.
      if (st2 < endval  ||
          (st2 == endval  &&
           (elem.isLeftClosed() || elem.isRightClosed()))) {
        // Overlap; update end if higher.
        if (end2 > endval) {
          endval = end2;
        }
      } else {
        // No overlap, so create the interval found and start a new one.
        elems[nelem++] = new TableExprNodeSetElem(elem.isLeftClosed(),
                                                  stval,
                                                  endval,
                                                  elem.isRightClosed());
        stval  = st2;
        endval = end2;
      }
    }
    // Create the last interval and resize the array to #intervals found.
    elems[nelem++] = new TableExprNodeSetElem(elem.isLeftClosed(),
                                              stval,
                                              endval,
                                              elem.isRightClosed());
    elems.resize (nelem);
    // Store the values in a start and an end array.
    itsStart.resize (nelem);
    itsEnd.resize (nelem);
    for (size_t i=0; i<nelem; i++) {
      itsStart[i] = castSetElem(elems[i])->start()->getInt(id);
      itsEnd[i]   = castSetElem(elems[i])->end()->getInt(id);
    }
    setFindFunc (elem.isLeftClosed(), elem.isRightClosed());
    itsAllIntervals = True;
  }
  // Replace by new ones.
  itsElems = elems;
}

void TableExprNodeSet::combineDoubleIntervals()
{
  DebugAssert (itsElems.size() > 0, AipsError);
  // Make an id (with an arbitrary row number) for the gets.
  TableExprId id(0);
  std::vector<TENShPtr> elems(1);
  TableExprNodeSetElem& elem = *(castItsElem(0));
  if (elem.start() == 0) {
    // No start value, so only the highest end value is relevant.
    // Make a single interval with the used open/closed-ness.
    Double val = elem.end()->getDouble(id);
    for (size_t i=1; i<itsElems.size(); i++) {
      Double valn = castItsElem(i)->end()->getDouble(id);
      if (valn > val) {
        val = valn;
      }
    }
    elems[0] = new TableExprNodeSetElem(TableExprNode(val),
                                        elem.isRightClosed());
  } else if (elem.end() == 0) {
    // No end value, so only the lowest start value is relevant.
    Double val = elem.start()->getDouble(id);
    for (size_t i=1; i<itsElems.size(); i++) {
      Double valn = castItsElem(i)->start()->getDouble(id);
      if (valn < val) {
        val = valn;
      }
    }
    elems[0] = new TableExprNodeSetElem(elem.isLeftClosed(),
                                        TableExprNode(val));
  } else {
    // The intervals contain both a start and an end value.
    // Make the block large enough for all possible intervals.
    elems.resize (itsElems.size());
    size_t nelem = 0;
    // Get all start values and sort them (indirectly) in ascending order.
    Block<Double> vals(itsElems.size());
    for (size_t i=0; i<itsElems.size(); i++) {
      vals[i] = castItsElem(i)->start()->getDouble(id);
    }
    Vector<uInt> index;
    GenSortIndirect<Double,uInt>::sort (index, vals, vals.size());
    // Get the start and end value of first interval in sorted list.
    Double stval  = vals[index[0]];
    Double endval = castItsElem(index[0])->end()->getDouble(id);
    // Loop through the next intervals and combine if possible.
    for (uInt i=1; i<index.size(); i++) {
      Int inx = index[i];
      Double st2 = vals[inx];
      Double end2 = castItsElem(inx)->end()->getDouble(id);
      // Combine intervals if they overlap.
      // They do if the next interval starts before end of this one
      // or if starting at the end and one side of the interval is closed.
      if (st2 < endval  ||
          (st2 == endval  &&
           (elem.isLeftClosed() || elem.isRightClosed()))) {
        // Overlap; update end if higher.
        if (end2 > endval) {
          endval = end2;
        }
      } else {
        // No overlap, so create the interval found and start a new one.
        elems[nelem++] = new TableExprNodeSetElem(elem.isLeftClosed(),
                                                  stval,
                                                  endval,
                                                  elem.isRightClosed());
        stval  = st2;
        endval = end2;
      }
    }
    // Create the last interval and resize the array to #intervals found.
    elems[nelem++] = new TableExprNodeSetElem(elem.isLeftClosed(),
                                              stval,
                                              endval,
                                              elem.isRightClosed());
    elems.resize (nelem);
    // Store the values in a start and an end array.
    itsStart.resize (nelem);
    itsEnd.resize (nelem);
    for (size_t i=0; i<nelem; i++) {
      itsStart[i] = castSetElem(elems[i])->start()->getDouble(id);
      itsEnd[i]   = castSetElem(elems[i])->end()->getDouble(id);
    }
    setFindFunc (elem.isLeftClosed(), elem.isRightClosed());
    itsAllIntervals = True;
  }
  // Replace by new ones.
  itsElems = elems;
}

void TableExprNodeSet::combineDateIntervals()
{
  DebugAssert (itsElems.size() > 0, AipsError);
  // Make an id (with an arbitrary row number) for the gets.
  // Note that this function uses the automatic Double<->MVTime conversions.
  TableExprId id(0);
  std::vector<TENShPtr> elems(1);
  TableExprNodeSetElem& elem = *(castItsElem(0));
  if (elem.start() == 0) {
    // No start value, so only the highest end value is relevant.
    // Make a single interval with the used open/closed-ness.
    Double val = elem.end()->getDate(id);
    for (size_t i=1; i<itsElems.size(); i++) {
      Double valn = castItsElem(i)->end()->getDate(id);
      if (valn > val) {
        val = valn;
      }
    }
    elems[0] = new TableExprNodeSetElem
                    (TableExprNode(new TableExprNodeConstDate(val)),
                     elem.isRightClosed());
  } else if (elem.end() == 0) {
    // No end value, so only the lowest start value is relevant.
    Double val = elem.start()->getDate(id);
    for (size_t i=1; i<itsElems.size(); i++) {
      Double valn = castItsElem(i)->start()->getDate(id);
      if (valn < val) {
        val = valn;
      }
    }
    elems[0] = new TableExprNodeSetElem
                    (elem.isLeftClosed(),
                     TableExprNode(new TableExprNodeConstDate(val)));
  } else {
    // The intervals contain both a start and an end value.
    // Make the block large enough for all possible intervals.
    elems.resize (itsElems.size());
    size_t nelem = 0;
    // Get all start values and sort them (indirectly) in ascending order.
    Block<Double> vals(itsElems.size());
    for (size_t i=0; i<itsElems.size(); i++) {
      vals[i] = castItsElem(i)->start()->getDate(id);
    }
    Vector<uInt> index;
    GenSortIndirect<Double,uInt>::sort (index, vals, vals.size());
    // Get the start and end value of first interval in sorted list.
    Double stval  = vals[index[0]];
    Double endval = castItsElem(index[0])->end()->getDate(id);
    // Loop through the next intervals and combine if possible.
    for (uInt i=1; i<index.size(); i++) {
      Int inx = index[i];
      Double st2 = vals[inx];
      Double end2 = castItsElem(inx)->end()->getDate(id);
      // Combine intervals if they overlap.
      // They do if the next interval starts before end of this one
      // or if starting at the end and one side of the interval is closed.
      if (st2 < endval  ||
          (st2 == endval  &&
           (elem.isLeftClosed() || elem.isRightClosed()))) {
        // Overlap; update end if higher.
        if (end2 > endval) {
          endval = end2;
        }
      } else {
        // No overlap, so create the interval found and start a new one.
        elems[nelem++] = new TableExprNodeSetElem
                                       (elem.isLeftClosed(),
                                        new TableExprNodeConstDate(stval),
                                        new TableExprNodeConstDate(endval),
                                        elem.isRightClosed());
        stval  = st2;
        endval = end2;
      }
    }
    // Create the last interval and resize the array to #intervals found.
    elems[nelem++] = new TableExprNodeSetElem
                                       (elem.isLeftClosed(),
                                        new TableExprNodeConstDate(stval),
                                        new TableExprNodeConstDate(endval),
                                        elem.isRightClosed());
    elems.resize (nelem);
    // Store the values in a start and an end array.
    itsStart.resize (nelem);
    itsEnd.resize (nelem);
    for (size_t i=0; i<nelem; i++) {
      itsStart[i] = castSetElem(elems[i])->start()->getDate(id);
      itsEnd[i]   = castSetElem(elems[i])->end()->getDate(id);
    }
    setFindFunc (elem.isLeftClosed(), elem.isRightClosed());
    itsAllIntervals = True;
  }
  // Replace by new ones.
  itsElems = elems;
}

void TableExprNodeSet::setFindFunc (Bool isLeftClosed, Bool isRightClosed)
{
  if (isLeftClosed) {
    if (isRightClosed) {
      itsFindFunc = &TableExprNodeSet::findClosedClosed;
    } else {
      itsFindFunc = &TableExprNodeSet::findClosedOpen;
    }
  } else {
    if (isRightClosed) {
      itsFindFunc = &TableExprNodeSet::findOpenClosed;
    } else {
      itsFindFunc = &TableExprNodeSet::findOpenOpen;
    }
  }
}

void TableExprNodeSet::add (const TableExprNodeSetElem& elem,
                            Bool adaptType)
{
    size_t n = itsElems.size();
    itsElems.resize (n+1);
    itsElems[n] = new TableExprNodeSetElem (elem);
    // Set and adapt unit as needed.
    if (unit().empty()) {
        setUnit (elem.unit());
    }
    // See if the set properties change.
    if (! elem.isSingle()) {
        itsSingle = False;
        if (! elem.isDiscrete()) {
            itsDiscrete = False;
            itsBounded  = False;
        } else {
            if (elem.end() == 0) {
                // Note that an undefined start defaults to 0, this is bounded.
                itsBounded = False;
            }
        }
    }
    if (n == 0) {
        dtype_p = elem.dataType();
    } else if (adaptType) {
        // Determine the highest data type.
        // Note: using OtEQ works well for all types (including dates).
        dtype_p = TableExprNodeBinary::getDT (dtype_p, elem.dataType(),
                                              OtEQ);
    }
    checkTablePtr (itsElems[n]);
    fillExprType  (itsElems[n]);
}

void TableExprNodeSet::adaptSetUnits (const Unit& unit)
{
    if (! unit.empty()) {
        for (size_t i=0; i<itsElems.size(); i++) {
            itsElems[i]->adaptSetUnits (unit);
        }
        setUnit (unit);
    }
}

void TableExprNodeSet::checkEqualDataTypes() const
{
    if (itsCheckTypes) {
        for (size_t i=0; i<itsElems.size(); i++) {
            if (itsElems[i]->dataType() != dtype_p) {
                throw TableInvExpr ("Set elements must have equal data types");
            }
        }
    }
}

void TableExprNodeSet::show (ostream& os, uInt indent) const
{
    TableExprNodeRep::show (os, indent);
    for (size_t j=0; j<itsElems.size(); j++) {
        itsElems[j]->show (os, indent+2);
    }
}

void TableExprNodeSet::getAggrNodes (vector<TableExprNodeRep*>& aggr)
{
    for (size_t j=0; j<itsElems.size(); j++) {
        itsElems[j]->getAggrNodes (aggr);
    }
}

void TableExprNodeSet::getColumnNodes (vector<TableExprNodeRep*>& cols)
{
    for (size_t j=0; j<itsElems.size(); j++) {
        itsElems[j]->getColumnNodes (cols);
    }
}

Bool TableExprNodeSet::hasArrays() const
{
    //# Check if a value is an array?
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        const TableExprNodeSetElem& elem = *(castItsElem(i));
        if (elem.start()  &&  elem.start()->valueType() == VTArray) {
            return True;
        }
        if (elem.end()  &&  elem.end()->valueType() == VTArray) {
            return True;
        }
        if (elem.increment()  &&  elem.increment()->valueType() == VTArray) {
            return True;
        }
    }
    return False;
}

TENShPtr TableExprNodeSet::setOrArray() const
{
    // A set where elements have different unit types cannot be turned
    // into an array.
    if (! unit().empty()) {
        Quantity q(1., unit());
        size_t n = size();
        for (size_t i=0; i<n; i++) {
              if (! itsElems[i]->unit().empty()) {
                  if (! q.isConform (itsElems[i]->unit())) {
                      return new TableExprNodeSet (*this);
                  }
              }
        }
        // No different units, so adapt elements to first unit.
        for (size_t i=0; i<n; i++) {
            itsElems[i]->adaptSetUnits (unit());
        }
    }
    // If discrete, all start values should be filled in.
    if (itsDiscrete) {
        size_t n = size();
        for (size_t i=0; i<n; i++) {
            if (castItsElem(i)->start() == 0) {
                throw (TableInvExpr ("no start value in discrete interval"));
            }
        }
    }
    // If the set is bounded, it can be converted to an array.
    if (itsBounded) {
        // If it is const, that can be done immediately.
        if (isConstant()) {
            return toConstArray();
        }
    }
    TableExprNodeSet* set = new TableExprNodeSet (*this);
    if (itsBounded) {
        // Set the type to VTArray; the getArray functions
        // will convert the set to an array for each row.
        set->setValueType (VTArray);
        if (itsSingle  &&  !hasArrays()) {
            set->ndim_p = 1;
            set->shape_p = IPosition (1, size());
        }
    }
    return set;
}

TENShPtr TableExprNodeSet::toConstArray() const
{
    // Construct the correct const array object.
    TENShPtr tsnptr;
    switch (dataType()) {
    case NTBool:
      tsnptr = new TableExprNodeArrayConstBool (toArray<Bool>(0));
      break;
    case NTInt:
      tsnptr = new TableExprNodeArrayConstInt (toArray<Int64>(0));
      break;
    case NTDouble:
      tsnptr = new TableExprNodeArrayConstDouble (toArray<Double>(0));
      break;
    case NTComplex:
      tsnptr = new TableExprNodeArrayConstDComplex (toArray<DComplex>(0));
      break;
    case NTString:
      tsnptr = new TableExprNodeArrayConstString (toArray<String>(0));
      break;
    case NTDate:
      tsnptr = new TableExprNodeArrayConstDate (toArray<MVTime>(0));
      break;
    default:
      TableExprNode::throwInvDT ("TableExprNodeSet::toConstArray");
    }
    tsnptr->setUnit (unit());
    return tsnptr;
}

MArray<Bool> TableExprNodeSet::getArrayBool (const TableExprId& id)
{
  return toArray<Bool> (id);
}
MArray<Int64> TableExprNodeSet::getArrayInt (const TableExprId& id)
{
  return toArray<Int64> (id);
}
MArray<Double> TableExprNodeSet::getArrayDouble (const TableExprId& id)
{
  return toArray<Double> (id);
}
MArray<DComplex> TableExprNodeSet::getArrayDComplex (const TableExprId& id)
{
  return toArray<DComplex> (id);
}
MArray<String> TableExprNodeSet::getArrayString (const TableExprId& id)
{
  return toArray<String> (id);
}
MArray<MVTime> TableExprNodeSet::getArrayDate (const TableExprId& id)
{
  return toArray<MVTime> (id);
}

Bool TableExprNodeSet::findOpenOpen (Double value)
{
    size_t n = itsElems.size();
    if (value >= itsEnd[n-1]) {
        return False;
    }
    for (size_t i=0; i<n; i++) {
        if (value <= itsStart[i]) {
            return False;
        }
        if (value < itsEnd[i]) {
            return True;
        }
    }
    return False;
}
Bool TableExprNodeSet::findOpenClosed (Double value)
{
    size_t n = itsElems.size();
    if (value > itsEnd[n-1]) {
        return False;
    }
    for (size_t i=0; i<n; i++) {
        if (value <= itsStart[i]) {
            return False;
        }
        if (value <= itsEnd[i]) {
            return True;
        }
    }
    return False;
}
Bool TableExprNodeSet::findClosedOpen (Double value)
{
    size_t n = itsElems.size();
    if (value >= itsEnd[n-1]) {
        return False;
    }
    for (size_t i=0; i<n; i++) {
        if (value < itsStart[i]) {
            return False;
        }
        if (value < itsEnd[i]) {
            return True;
        }
    }
    return False;
}
Bool TableExprNodeSet::findClosedClosed (Double value)
{
    size_t n = itsElems.size();
    if (value > itsEnd[n-1]) {
        return False;
    }
    for (size_t i=0; i<n; i++) {
        if (value < itsStart[i]) {
            return False;
        }
        if (value <= itsEnd[i]) {
            return True;
        }
    }
    return False;
}

Bool TableExprNodeSet::hasBool (const TableExprId& id, Bool value)
{
    Bool result = False;
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        castItsElem(i)->matchBool (&result, &value, 1, id);
    }
    return result;
}
Bool TableExprNodeSet::hasInt (const TableExprId& id, Int64 value)
{
    if (itsAllIntervals) {
        return (this->*itsFindFunc) (value);
    }
    Bool result = False;
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        castItsElem(i)->matchInt (&result, &value, 1, id);
    }
    return result;
}
Bool TableExprNodeSet::hasDouble (const TableExprId& id, Double value)
{
    if (itsAllIntervals) {
        return (this->*itsFindFunc) (value);
    }
    Bool result = False;
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        castItsElem(i)->matchDouble (&result, &value, 1, id);
    }
    return result;
}
Bool TableExprNodeSet::hasDComplex (const TableExprId& id,
                                    const DComplex& value)
{
    Bool result = False;
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        castItsElem(i)->matchDComplex (&result, &value, 1, id);
    }
    return result;
}
Bool TableExprNodeSet::hasString (const TableExprId& id, const String& value)
{
    Bool result = False;
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        castItsElem(i)->matchString (&result, &value, 1, id);
    }
    return result;
}
Bool TableExprNodeSet::hasDate (const TableExprId& id, const MVTime& value)
{
    if (itsAllIntervals) {
        return (this->*itsFindFunc) (value);
    }
    Bool result = False;
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        castItsElem(i)->matchDate (&result, &value, 1, id);
    }
    return result;
}
MArray<Bool> TableExprNodeSet::hasArrayBool (const TableExprId& id,
                                             const MArray<Bool>& value)
{
    Array<Bool> result(value.shape());
    result.set (False);
    Bool deleteIn, deleteOut;
    const Bool* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    size_t nval = value.size();
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        castItsElem(i)->matchBool (out, in, nval, id);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
}
MArray<Bool> TableExprNodeSet::hasArrayInt (const TableExprId& id,
                                            const MArray<Int64>& value)
{
    Array<Bool> result(value.shape());
    result.set (False);
    Bool deleteIn, deleteOut;
    const Int64* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    size_t nval = value.size();
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        castItsElem(i)->matchInt (out, in, nval, id);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
}
MArray<Bool> TableExprNodeSet::hasArrayDouble (const TableExprId& id,
                                               const MArray<Double>& value)
{
    Array<Bool> result(value.shape());
    result.set (False);
    Bool deleteIn, deleteOut;
    const Double* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    size_t nval = value.size();
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        castItsElem(i)->matchDouble (out, in, nval, id);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
}
MArray<Bool> TableExprNodeSet::hasArrayDComplex (const TableExprId& id,
                                                 const MArray<DComplex>& value)
{
    Array<Bool> result(value.shape());
    result.set (False);
    Bool deleteIn, deleteOut;
    const DComplex* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    size_t nval = value.size();
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        castItsElem(i)->matchDComplex (out, in, nval, id);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
}
MArray<Bool> TableExprNodeSet::hasArrayString (const TableExprId& id,
                                               const MArray<String>& value)
{
    Array<Bool> result(value.shape());
    result.set (False);
    Bool deleteIn, deleteOut;
    const String* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    size_t nval = value.size();
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        castItsElem(i)->matchString (out, in, nval, id);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
}
MArray<Bool> TableExprNodeSet::hasArrayDate (const TableExprId& id,
                                             const MArray<MVTime>& value)
{
    Array<Bool> result(value.shape());
    result.set (False);
    Bool deleteIn, deleteOut;
    const MVTime* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    size_t nval = value.size();
    size_t n = itsElems.size();
    for (size_t i=0; i<n; i++) {
        castItsElem(i)->matchDate (out, in, nval, id);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
}

} //# NAMESPACE CASACORE - END

