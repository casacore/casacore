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
//# $Id$

#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprUnitNode.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/tables/TaQL/ExprDerNodeArray.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIO.h>
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
    //# Note that the TableExprNode copy ctor is needed to get rid of const.
    TableExprNode tmp(value);
    itsStart = getRep(tmp)->link();
    dtype_p = itsStart->dataType();
    try {
      setUnit (itsStart->unit());
      checkTable();
    } catch (const std::exception&) {
      // Unlink, because destructor is not called if constructor
      // throws an exception.
      unlink (itsStart);
      throw;
    }
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
    NodeDataType dts = NTInt;
    if (start != 0) {
	TableExprNode tmp(*start);
	itsStart = getRep(tmp)->link();
	dts = itsStart->dataType();
    }
    NodeDataType dte = dts;
    if (end != 0) {
	TableExprNode tmp(*end);
	itsEnd = getRep(tmp)->link();
	dte = itsEnd->dataType();
    }
    NodeDataType dti = NTInt;
    if (incr != 0) {
	TableExprNode tmp(*incr);
	itsIncr = getRep(tmp)->link();
	dti = itsIncr->dataType();
    }
    if (dts == NTInt  &&  (dte == NTDouble || dti == NTDouble)) dts = NTDouble;
    if (dte == NTInt  &&  (dts == NTDouble || dti == NTDouble)) dte = NTDouble;
    try {
      if ((dts != NTInt  &&  dts != NTDouble  &&  dts != NTDate)
       ||  dte != dts  ||  (dti != NTInt  &&  dti != NTDouble)) {
	throw TableInvExpr("start:end should have equal data types (Int, Double"
			     " or Date) and incr should have Int or Double");
      }
      // Find unit and adapt units if needed.
      setUnit (TableExprNodeUnit::adaptUnits (itsStart, itsEnd, itsIncr));
      dtype_p = dts;
      checkTable();
    } catch (const std::exception&) {
      // Unlink, because destructor is not called if constructor
      // throws an exception.
      unlink (itsStart);
      unlink (itsEnd);
      unlink (itsIncr);
      throw;
    }
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
{
    if (itsStart != 0) {
	itsStart->link();
    }
    if (itsEnd != 0) {
	itsEnd->link();
    }
    if (itsIncr != 0) {
	itsIncr->link();
    }
}

TableExprNodeSetElem::TableExprNodeSetElem (const TableExprNodeSetElem& that,
					    TableExprNodeRep* start,
					    TableExprNodeRep* end,
					    TableExprNodeRep* incr)
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
    if (itsStart != 0) {
	itsStart->link();
    }
    if (itsEnd != 0) {
	itsEnd->link();
    }
    if (itsIncr != 0) {
	itsIncr->link();
    }
    try {
      TableExprNodeUnit::adaptUnits (itsStart, itsEnd, itsIncr);
    } catch (const std::exception&) {
      // Unlink, because destructor is not called if constructor
      // throws an exception.
      unlink (itsStart);
      unlink (itsEnd);
      unlink (itsIncr);
      throw;
    }
}

TableExprNodeSetElem::~TableExprNodeSetElem()
{
    unlink (itsStart);
    unlink (itsEnd);
    unlink (itsIncr);
}

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
    //# Note that the TableExprNode copy ctor is needed to get rid of const.
    if (start != 0) {
	TableExprNode tmp(*start);
	itsStart = getRep(tmp)->link();
        // Get data type; integer continuous interval is always double
	dtype_p = itsStart->dataType();
        if (dtype_p == NTInt) {
            dtype_p = NTDouble;
        }
    }
    try {
      if (end != 0) {
	TableExprNode tmp(*end);
	itsEnd = getRep(tmp)->link();
        NodeDataType etype = itsEnd->dataType();
        if (etype == NTInt) {
            etype = NTDouble;
        }
	if (start != 0  &&  etype != dtype_p) {
	    throw (TableInvExpr ("start=:=end must have equal data types"));
	}
	dtype_p = etype;
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
    } catch (const std::exception&) {
      // Unlink, because destructor is not called if constructor
      // throws an exception.
      unlink (itsStart);
      unlink (itsEnd);
      unlink (itsIncr);
      throw;
    }
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
    if (itsStart != 0) {
	os << "start: ";
	itsStart->show (os, indent+2);
    }
    if (itsEnd != 0) {
	os << "end:   ";
	itsEnd->show (os, indent+2);
    }
    if (itsIncr != 0) {
	os << "incr:  ";
	itsIncr->show (os, indent+2);
    }
}

void TableExprNodeSetElem::getAggrNodes (vector<TableExprNodeRep*>& aggr)
{
    if (itsStart != 0) {
        itsStart->getAggrNodes (aggr);
    }
    if (itsEnd != 0) {
        itsEnd->getAggrNodes (aggr);
    }
    if (itsIncr != 0) {
        itsIncr->getAggrNodes (aggr);
    }
}
  
void TableExprNodeSetElem::getColumnNodes (vector<TableExprNodeRep*>& cols)
{
    if (itsStart != 0) {
        itsStart->getColumnNodes (cols);
    }
    if (itsEnd != 0) {
        itsEnd->getColumnNodes (cols);
    }
    if (itsIncr != 0) {
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
    TableExprNodeRep* start = 0;
    TableExprNodeRep* end = 0;
    TableExprNodeRep* incr = 0;
    switch (dataType()) {
    case NTBool:
	if (itsStart != 0) {
	    start = new TableExprNodeConstBool (itsStart->getBool (id));
	}
	break;
    case NTInt:
	if (itsStart != 0) {
	    start = new TableExprNodeConstInt (itsStart->getInt (id));
	}
	if (itsEnd != 0) {
            end = new TableExprNodeConstInt (itsEnd->getInt (id));
	}
	if (itsIncr != 0) {
	    incr = new TableExprNodeConstInt (itsIncr->getInt (id));
	}
	break;
    case NTDouble:
	if (itsStart != 0) {
	    start = new TableExprNodeConstDouble (itsStart->getDouble (id));
	}
	if (itsEnd != 0) {
            end = new TableExprNodeConstDouble (itsEnd->getDouble (id));
	}
	if (itsIncr != 0) {
	    incr = new TableExprNodeConstDouble (itsIncr->getDouble (id));
	}
	break;
    case NTComplex:
	if (itsStart != 0) {
	    start = new TableExprNodeConstDComplex
                                            (itsStart->getDComplex (id));
	}
	break;
    case NTString:
	if (itsStart != 0) {
	    start = new TableExprNodeConstString (itsStart->getString (id));
	}
	if (itsEnd != 0) {
	    end = new TableExprNodeConstString (itsEnd->getString (id));
	}
	break;
    case NTDate:
	if (itsStart != 0) {
	    start = new TableExprNodeConstDate (itsStart->getDate (id));
	}
	if (itsEnd != 0) {
            end = new TableExprNodeConstDate (itsEnd->getDate (id));
	}
	if (itsIncr != 0) {
	    incr = new TableExprNodeConstDouble (itsIncr->getDouble (id));
	}
	break;
    default:
	TableExprNode::throwInvDT ("TableExprNodeSetElem::evaluate");
    }
    return new TableExprNodeSetElem (*this, start, end, incr);
}

void TableExprNodeSetElem::fillVector (Vector<Bool>& vec, uInt& cnt,
				       const TableExprId& id) const
{
    DebugAssert (itsSingle, AipsError);
    uInt n = vec.nelements();
    if (n < cnt+1) {
	vec.resize (cnt+1, True);
    }
    vec(cnt++) = itsStart->getBool (id);
}
void TableExprNodeSetElem::fillVector (Vector<Int64>& vec, uInt& cnt,
				       const TableExprId& id) const
{
    DebugAssert (itsDiscrete, AipsError);
    Int64 start = itsStart==0  ?  0 : itsStart->getInt (id);
    Int64 end   = itsEnd==0  ?  start : itsEnd->getInt (id);
    Int64 incr  = itsIncr==0  ?  1 : itsIncr->getInt (id);
    if (start > end) {
	return;
    }
    uInt nval = 1 + uInt((end - start) / incr);
    uInt n = vec.nelements();
    if (n < cnt+nval) {
	vec.resize (cnt+nval, True);
    }
    for (uInt i=0; i<nval; i++) {
	vec(cnt++) = start;
	start += incr;
        if (itsEndExcl  &&  start >= end) {
            break;
        }
    }
}
void TableExprNodeSetElem::fillVector (Vector<Double>& vec, uInt& cnt,
				       const TableExprId& id) const
{
    DebugAssert (itsDiscrete, AipsError);
    Double start = itsStart==0  ?  0 : itsStart->getDouble (id);
    Double end   = itsEnd==0  ?  start : itsEnd->getDouble (id);
    Double incr  = itsIncr==0  ?  1 : itsIncr->getDouble (id);
    if (start > end) {
	return;
    }
    uInt nval = 1 + uInt((end - start) / incr);
    uInt n = vec.nelements();
    if (n < cnt+nval) {
	vec.resize (cnt+nval, True);
    }
    for (uInt i=0; i<nval; i++) {
	vec(cnt++) = start;
	start += incr;
        if (itsEndExcl  &&  start >= end) {
            break;
        }
    }
}
void TableExprNodeSetElem::fillVector (Vector<DComplex>& vec, uInt& cnt,
				       const TableExprId& id) const
{
    DebugAssert (itsSingle, AipsError);
    uInt n = vec.nelements();
    if (n < cnt+1) {
	vec.resize (cnt+1, True);
    }
    vec(cnt++) = itsStart->getDComplex (id);
}
void TableExprNodeSetElem::fillVector (Vector<String>& vec, uInt& cnt,
				       const TableExprId& id) const
{
    DebugAssert (itsSingle, AipsError);
    uInt n = vec.nelements();
    if (n < cnt+1) {
	vec.resize (cnt+1, True);
    }
    vec(cnt++) = itsStart->getString (id);
}
void TableExprNodeSetElem::fillVector (Vector<MVTime>& vec, uInt& cnt,
				       const TableExprId& id) const
{
    DebugAssert (itsDiscrete, AipsError);
    Double start = itsStart==0  ?  0 : Double(itsStart->getDate (id));
    Double end   = itsEnd==0  ?  start : Double(itsEnd->getDate (id));
    Double incr  = itsIncr==0  ?  1 : itsIncr->getDouble (id);
    if (start > end) {
	return;
    }
    uInt nval = 1 + uInt((end - start) / incr);
    uInt n = vec.nelements();
    if (n < cnt+nval) {
	vec.resize (cnt+nval, True);
    }
    for (uInt i=0; i<nval; i++) {
	vec(cnt++) = start;
	start += incr;
        if (itsEndExcl  &&  start >= end) {
            break;
        }
    }
}

void TableExprNodeSetElem::matchBool (Bool* match, const Bool* value,
				      uInt nval,
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
                                     uInt nval,
                                     const TableExprId& id) const
{
    Int64 start = itsStart==0  ?  0 : itsStart->getInt (id);
    Int64 end   = itsEnd==0  ?  start : itsEnd->getInt (id);
    Int64 incr  = itsIncr==0  ?  1 : itsIncr->getInt (id);
    if (start > end) {
	return;
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
	    if (tmp >= 0  &&  (itsEnd == 0  ||  tmp <= end)) {
                if (tmp%incr == 0) {
		    *match = True;
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
					uInt nval,
					const TableExprId& id) const
{
    Double start = itsStart==0  ?  0 : itsStart->getDouble (id);
    Double end   = itsEnd==0  ?  start : itsEnd->getDouble (id);
    Double incr  = itsIncr==0  ?  1 : itsIncr->getDouble (id);
    if (start > end) {
	return;
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
	    if (tmp >= 0  &&  (itsEnd == 0  ||  tmp < end  ||
                               (!itsEndExcl && tmp==end))) {
                if (near(tmp, incr*Int64(tmp/incr + 0.5))) {
		    *match = True;
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
					  uInt nval,
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
					uInt nval,
					const TableExprId& id) const
{
    String start;
    if (itsStart != 0) {
	start = itsStart->getString (id);
    }
    String end;
    if (itsEnd != 0) {
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
				      uInt nval,
				      const TableExprId& id) const
{
    Double start = itsStart==0  ?  0 : Double(itsStart->getDate (id));
    Double end   = itsEnd==0  ?  start : Double(itsEnd->getDate (id));
    Double incr  = itsIncr==0  ?  1 : itsIncr->getDouble (id);
    if (start > end) {
	return;
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
	    if (tmp >= 0  &&  (itsEnd == 0  ||  tmp < end  ||
                               (!itsEndExcl && tmp==end))) {
		Double div = tmp/incr;
		if (int(div) == div) {
		    *match = True;
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
    uInt n = indices.nelements();
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

TableExprNodeSet::TableExprNodeSet (const Vector<uInt>& rownrs,
                                    const TableExprNodeSet& set)
: TableExprNodeRep (set.dataType(), VTSet, OtUndef, Table()),
  itsElems         (rownrs.size() * set.nelements()),
  itsSingle        (set.isSingle()),
  itsDiscrete      (set.isDiscrete()),
  itsBounded       (set.isBounded()),
  itsCheckTypes    (False),
  itsAllIntervals  (False),
  itsFindFunc      (0)
{
    // Fill in all values.
    uInt nrel = set.nelements();
    for (uInt i=0; i<rownrs.size(); i++) {
        for (uInt j=0; j<nrel; j++) {
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
    uInt n = that.itsElems.nelements();
    itsElems.resize (n);
    for (uInt i=0; i<n; i++) {
	itsElems[i] = new TableExprNodeSetElem (*(that.itsElems[i]));
    }
}

TableExprNodeSet::~TableExprNodeSet()
{
    deleteElems();
}

void TableExprNodeSet::deleteElems()
{
    uInt n = itsElems.nelements();
    for (uInt i=0; i<n; i++) {
	delete itsElems[i];
    }
}

void TableExprNodeSet::combineIntIntervals()
{
  DebugAssert (itsElems.nelements() > 0, AipsError);
  // Make an id (with an arbitrary row number) for the gets.
  TableExprId id(0);
  PtrBlock<TableExprNodeSetElem*> elems(1);
  TableExprNodeSetElem& elem = *(itsElems[0]);
  if (elem.start() == 0) {
    // No start value, so only the highest end value is relevant.
    // Make a single interval with the used open/closed-ness.
    Int64 val = elem.end()->getInt(id);
    for (uInt i=1; i<itsElems.nelements(); i++) {
      Int64 valn = itsElems[i]->end()->getInt(id);
      if (valn > val) {
	val = valn;
      }
    }
    elems[0] = new TableExprNodeSetElem(TableExprNode(val),
					elem.isRightClosed());
  } else if (elem.end() == 0) {
    // No end value, so only the lowest start value is relevant.
    Int64 val = elem.start()->getInt(id);
    for (uInt i=1; i<itsElems.nelements(); i++) {
      Int64 valn = itsElems[i]->start()->getInt(id);
      if (valn < val) {
	val = valn;
      }
    }
    elems[0] = new TableExprNodeSetElem(elem.isLeftClosed(),
					TableExprNode(val));
  } else {
    // The intervals contain both a start and an end value.
    // Make the block large enough for all possible intervals.
    elems.resize (itsElems.nelements());
    uInt nelem = 0;
    // Get all start values and sort them (indirectly) in ascending order.
    Block<Int64> vals(itsElems.nelements());
    for (uInt i=0; i<itsElems.nelements(); i++) {
      vals[i] = itsElems[i]->start()->getInt(id);
    }
    Vector<uInt> index;
    GenSortIndirect<Int64>::sort (index, vals, vals.nelements());
    // Get the start and end value of first interval in sorted list.
    Int64 stval  = vals[index[0]];
    Int64 endval = itsElems[index[0]]->end()->getInt(id);
    // Loop through the next intervals and combine if possible.
    for (uInt i=1; i<index.nelements(); i++) {
      Int inx = index[i];
      Int64 st2 = vals[inx];
      Int64 end2 = itsElems[inx]->end()->getInt(id);
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
    elems.resize (nelem, True, True);
    // Store the values in a start and an end array.
    itsStart.resize (nelem);
    itsEnd.resize (nelem);
    for (uInt i=0; i<nelem; i++) {
      itsStart[i] = elems[i]->start()->getInt(id);
      itsEnd[i]   = elems[i]->end()->getInt(id);
    }
    setFindFunc (elem.isLeftClosed(), elem.isRightClosed());
    itsAllIntervals = True;
  }
  // Delete all existing intervals and replace by new ones.
  deleteElems();
  itsElems = elems;
}

void TableExprNodeSet::combineDoubleIntervals()
{
  DebugAssert (itsElems.nelements() > 0, AipsError);
  // Make an id (with an arbitrary row number) for the gets.
  TableExprId id(0);
  PtrBlock<TableExprNodeSetElem*> elems(1);
  TableExprNodeSetElem& elem = *(itsElems[0]);
  if (elem.start() == 0) {
    // No start value, so only the highest end value is relevant.
    // Make a single interval with the used open/closed-ness.
    Double val = elem.end()->getDouble(id);
    for (uInt i=1; i<itsElems.nelements(); i++) {
      Double valn = itsElems[i]->end()->getDouble(id);
      if (valn > val) {
	val = valn;
      }
    }
    elems[0] = new TableExprNodeSetElem(TableExprNode(val),
					elem.isRightClosed());
  } else if (elem.end() == 0) {
    // No end value, so only the lowest start value is relevant.
    Double val = elem.start()->getDouble(id);
    for (uInt i=1; i<itsElems.nelements(); i++) {
      Double valn = itsElems[i]->start()->getDouble(id);
      if (valn < val) {
	val = valn;
      }
    }
    elems[0] = new TableExprNodeSetElem(elem.isLeftClosed(),
					TableExprNode(val));
  } else {
    // The intervals contain both a start and an end value.
    // Make the block large enough for all possible intervals.
    elems.resize (itsElems.nelements());
    uInt nelem = 0;
    // Get all start values and sort them (indirectly) in ascending order.
    Block<Double> vals(itsElems.nelements());
    for (uInt i=0; i<itsElems.nelements(); i++) {
      vals[i] = itsElems[i]->start()->getDouble(id);
    }
    Vector<uInt> index;
    GenSortIndirect<Double>::sort (index, vals, vals.nelements());
    // Get the start and end value of first interval in sorted list.
    Double stval  = vals[index[0]];
    Double endval = itsElems[index[0]]->end()->getDouble(id);
    // Loop through the next intervals and combine if possible.
    for (uInt i=1; i<index.nelements(); i++) {
      Int inx = index[i];
      Double st2 = vals[inx];
      Double end2 = itsElems[inx]->end()->getDouble(id);
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
    elems.resize (nelem, True, True);
    // Store the values in a start and an end array.
    itsStart.resize (nelem);
    itsEnd.resize (nelem);
    for (uInt i=0; i<nelem; i++) {
      itsStart[i] = elems[i]->start()->getDouble(id);
      itsEnd[i]   = elems[i]->end()->getDouble(id);
    }
    setFindFunc (elem.isLeftClosed(), elem.isRightClosed());
    itsAllIntervals = True;
  }
  // Delete all existing intervals and replace by new ones.
  deleteElems();
  itsElems = elems;
}

void TableExprNodeSet::combineDateIntervals()
{
  DebugAssert (itsElems.nelements() > 0, AipsError);
  // Make an id (with an arbitrary row number) for the gets.
  // Note that this function uses the automatic Double<->MVTime conversions.
  TableExprId id(0);
  PtrBlock<TableExprNodeSetElem*> elems(1);
  TableExprNodeSetElem& elem = *(itsElems[0]);
  if (elem.start() == 0) {
    // No start value, so only the highest end value is relevant.
    // Make a single interval with the used open/closed-ness.
    Double val = elem.end()->getDate(id);
    for (uInt i=1; i<itsElems.nelements(); i++) {
      Double valn = itsElems[i]->end()->getDate(id);
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
    for (uInt i=1; i<itsElems.nelements(); i++) {
      Double valn = itsElems[i]->start()->getDate(id);
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
    elems.resize (itsElems.nelements());
    uInt nelem = 0;
    // Get all start values and sort them (indirectly) in ascending order.
    Block<Double> vals(itsElems.nelements());
    for (uInt i=0; i<itsElems.nelements(); i++) {
      vals[i] = itsElems[i]->start()->getDate(id);
    }
    Vector<uInt> index;
    GenSortIndirect<Double>::sort (index, vals, vals.nelements());
    // Get the start and end value of first interval in sorted list.
    Double stval  = vals[index[0]];
    Double endval = itsElems[index[0]]->end()->getDate(id);
    // Loop through the next intervals and combine if possible.
    for (uInt i=1; i<index.nelements(); i++) {
      Int inx = index[i];
      Double st2 = vals[inx];
      Double end2 = itsElems[inx]->end()->getDate(id);
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
    elems.resize (nelem, True, True);
    // Store the values in a start and an end array.
    itsStart.resize (nelem);
    itsEnd.resize (nelem);
    for (uInt i=0; i<nelem; i++) {
      itsStart[i] = elems[i]->start()->getDate(id);
      itsEnd[i]   = elems[i]->end()->getDate(id);
    }
    setFindFunc (elem.isLeftClosed(), elem.isRightClosed());
    itsAllIntervals = True;
  }
  // Delete all existing intervals and replace by new ones.
  deleteElems();
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

void TableExprNodeSet::add (const TableExprNodeSetElem& elem)
{
    uInt n = itsElems.nelements();
    itsElems.resize (n+1);
    itsElems[n] = new TableExprNodeSetElem (elem);
    // Set and adapt unit as needed.
    if (unit().empty()) {
        setUnit (elem.unit());
        ///   } else {
        ///        itsElems[n]->adaptSetUnits (unit());
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
    }
    checkTablePtr (itsElems[n]);
    fillExprType  (itsElems[n]);
}

void TableExprNodeSet::adaptSetUnits (const Unit& unit)
{
    if (! unit.empty()) {
        for (uInt i=0; i<itsElems.nelements(); i++) {
	    itsElems[i]->adaptSetUnits (unit);
	}
	setUnit (unit);
    }
}

void TableExprNodeSet::checkEqualDataTypes() const
{
    if (itsCheckTypes) {
        for (uInt i=0; i<itsElems.nelements(); i++) {
	    if (itsElems[i]->dataType() != dtype_p) {
	        throw TableInvExpr ("Set elements must have equal data types");
	    }
	}
    }
}

void TableExprNodeSet::show (ostream& os, uInt indent) const
{
    TableExprNodeRep::show (os, indent);
    for (uInt j=0; j<itsElems.nelements(); j++) {
	itsElems[j]->show (os, indent+2);
    }
}

void TableExprNodeSet::getAggrNodes (vector<TableExprNodeRep*>& aggr)
{
    for (uInt j=0; j<itsElems.nelements(); j++) {
        itsElems[j]->getAggrNodes (aggr);
    }
}

void TableExprNodeSet::getColumnNodes (vector<TableExprNodeRep*>& cols)
{
    for (uInt j=0; j<itsElems.nelements(); j++) {
        itsElems[j]->getColumnNodes (cols);
    }
}

Bool TableExprNodeSet::hasArrays() const
{
    //# Check if a value is an array?
    uInt n = itsElems.nelements();
    for (uInt i=0; i<n; i++) {
	const TableExprNodeSetElem& elem = *(itsElems[i]);
	if (elem.start() != 0
        &&  elem.start()->valueType() == VTArray) {
	    return True;
	}
	if (elem.end() != 0
        &&  elem.end()->valueType() == VTArray) {
	    return True;
	}
	if (elem.increment() != 0
        &&  elem.increment()->valueType() == VTArray) {
	    return True;
	}
    }
    return False;
}

TableExprNodeRep* TableExprNodeSet::setOrArray() const
{
    // The set should not contain array elements.
    if (hasArrays()) {
        return new TableExprNodeSet (*this);
///	throw a(TableInvExpr ("A set cannot contain elements having arrays"));
    }
    // A set where elements have different units cannot be turned into an array.
    if (! unit().empty()) {
        Quantity q(1., unit());
	uInt n = nelements();
	for (uInt i=0; i<n; i++) {
              if (! itsElems[i]->unit().empty()) {
                  if (! q.isConform (itsElems[i]->unit())) {
                      return new TableExprNodeSet (*this);
                  }
              }
	}
        // No different units, so adapt elements to known one.
	for (uInt i=0; i<n; i++) {
            itsElems[i]->adaptSetUnits (unit());
        }
    }
    // When discrete, all start values should be filled in.
    if (itsDiscrete) {
	uInt n = nelements();
	for (uInt i=0; i<n; i++) {
	    if (itsElems[i]->start() == 0) {
		throw (TableInvExpr ("no start value in discrete interval"));
	    }
	}
    }
    // When the set is bounded, it can be converted to an array.
    if (itsBounded) {
	// When it is const, that can be done immediately.
	if (isConstant()) {
	    return toArray();
	}
    }
    TableExprNodeSet* set = new TableExprNodeSet (*this);
    if (itsBounded) {
	// Set the type to VTArray and the getArray
	// functions convert the set to an array for each row.
	set->setValueType (VTArray);
	if (itsSingle) {
	    set->ndim_p = 1;
	    set->shape_p = IPosition (1, nelements());
	}
    }
    return set;
}

TableExprNodeRep* TableExprNodeSet::toArray() const
{
    // Construct the correct const array object.
    TableExprNodeRep* tsnptr=0;
    switch (dataType()) {
    case NTBool:
	tsnptr = new TableExprNodeArrayConstBool (toArrayBool(0));
	break;
    case NTInt:
	tsnptr = new TableExprNodeArrayConstInt (toArrayInt(0));
	break;
    case NTDouble:
	tsnptr = new TableExprNodeArrayConstDouble (toArrayDouble(0));
	break;
    case NTComplex:
	tsnptr = new TableExprNodeArrayConstDComplex (toArrayDComplex(0));
	break;
    case NTString:
	tsnptr = new TableExprNodeArrayConstString (toArrayString(0));
	break;
    case NTDate:
	tsnptr = new TableExprNodeArrayConstDate (toArrayDate(0));
	break;
    default:
	TableExprNode::throwInvDT ("TableExprNodeSet::toArray");
    }
    tsnptr->setUnit (unit());
    return tsnptr;
}

Array<Bool> TableExprNodeSet::toArrayBool (const TableExprId& id) const
{
    DebugAssert (itsBounded, AipsError);
    // First determine (roughly) the number of values needed in
    // the resulting vector. This number is correct in case
    // single values are given (which is usually the case).
    // The fillVector functions will resize when needed.
    // At the end the vector is also resized in case it was too long.
    uInt n = nelements();
    uInt cnt = 0;
    Vector<Bool> result (n);
    for (uInt i=0; i<n; i++) {
	itsElems[i]->fillVector (result, cnt, id);
    }
    result.resize (cnt, True);
    return result;
}
Array<Int64> TableExprNodeSet::toArrayInt (const TableExprId& id) const
{
    DebugAssert (itsBounded, AipsError);
    uInt n = nelements();
    uInt cnt = 0;
    Vector<Int64> result (n);
    for (uInt i=0; i<n; i++) {
	itsElems[i]->fillVector (result, cnt, id);
    }
    result.resize (cnt, True);
    return result;
}
Array<Double> TableExprNodeSet::toArrayDouble (const TableExprId& id) const
{
    DebugAssert (itsBounded, AipsError);
    uInt n = nelements();
    uInt cnt = 0;
    Vector<Double> result (n);
    for (uInt i=0; i<n; i++) {
	itsElems[i]->fillVector (result, cnt, id);
    }
    result.resize (cnt, True);
    return result;
}
Array<DComplex> TableExprNodeSet::toArrayDComplex (const TableExprId& id) const
{
    DebugAssert (itsBounded, AipsError);
    uInt n = nelements();
    uInt cnt = 0;
    Vector<DComplex> result (n);
    for (uInt i=0; i<n; i++) {
	itsElems[i]->fillVector (result, cnt, id);
    }
    result.resize (cnt, True);
    return result;
}
Array<String> TableExprNodeSet::toArrayString (const TableExprId& id) const
{
    DebugAssert (itsBounded, AipsError);
    uInt n = nelements();
    uInt cnt = 0;
    Vector<String> result (n);
    for (uInt i=0; i<n; i++) {
	itsElems[i]->fillVector (result, cnt, id);
    }
    result.resize (cnt, True);
    return result;
}
Array<MVTime> TableExprNodeSet::toArrayDate (const TableExprId& id) const
{
    DebugAssert (itsBounded, AipsError);
    uInt n = nelements();
    uInt cnt = 0;
    Vector<MVTime> result (n);
    for (uInt i=0; i<n; i++) {
	itsElems[i]->fillVector (result, cnt, id);
    }
    result.resize (cnt, True);
    return result;
}

Array<Bool> TableExprNodeSet::getArrayBool (const TableExprId& id)
{
    return toArrayBool (id);
}
Array<Int64> TableExprNodeSet::getArrayInt (const TableExprId& id)
{
    return toArrayInt (id);
}
Array<Double> TableExprNodeSet::getArrayDouble (const TableExprId& id)
{
    return toArrayDouble (id);
}
Array<DComplex> TableExprNodeSet::getArrayDComplex (const TableExprId& id)
{
    return toArrayDComplex (id);
}
Array<String> TableExprNodeSet::getArrayString (const TableExprId& id)
{
    return toArrayString (id);
}
Array<MVTime> TableExprNodeSet::getArrayDate (const TableExprId& id)
{
    return toArrayDate (id);
}

Bool TableExprNodeSet::findOpenOpen (Double value)
{
    uInt n = itsElems.nelements();
    if (value >= itsEnd[n-1]) {
        return False;
    }
    for (uInt i=0; i<n; i++) {
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
    uInt n = itsElems.nelements();
    if (value > itsEnd[n-1]) {
        return False;
    }
    for (uInt i=0; i<n; i++) {
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
    uInt n = itsElems.nelements();
    if (value >= itsEnd[n-1]) {
        return False;
    }
    for (uInt i=0; i<n; i++) {
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
    uInt n = itsElems.nelements();
    if (value > itsEnd[n-1]) {
        return False;
    }
    for (uInt i=0; i<n; i++) {
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
    uInt n = itsElems.nelements();
    for (uInt i=0; i<n; i++) {
	itsElems[i]->matchBool (&result, &value, 1, id);
    }
    return result;
}
Bool TableExprNodeSet::hasInt (const TableExprId& id, Int64 value)
{
    if (itsAllIntervals) {
        return (this->*itsFindFunc) (value);
    }
    Bool result = False;
    uInt n = itsElems.nelements();
    for (uInt i=0; i<n; i++) {
        itsElems[i]->matchInt (&result, &value, 1, id);
    }
    return result;
}
Bool TableExprNodeSet::hasDouble (const TableExprId& id, Double value)
{
    if (itsAllIntervals) {
        return (this->*itsFindFunc) (value);
    }
    Bool result = False;
    uInt n = itsElems.nelements();
    for (uInt i=0; i<n; i++) {
        itsElems[i]->matchDouble (&result, &value, 1, id);
    }
    return result;
}
Bool TableExprNodeSet::hasDComplex (const TableExprId& id,
				    const DComplex& value)
{
    Bool result = False;
    uInt n = itsElems.nelements();
    for (uInt i=0; i<n; i++) {
	itsElems[i]->matchDComplex (&result, &value, 1, id);
    }
    return result;
}
Bool TableExprNodeSet::hasString (const TableExprId& id, const String& value)
{
    Bool result = False;
    uInt n = itsElems.nelements();
    for (uInt i=0; i<n; i++) {
	itsElems[i]->matchString (&result, &value, 1, id);
    }
    return result;
}
Bool TableExprNodeSet::hasDate (const TableExprId& id, const MVTime& value)
{
    if (itsAllIntervals) {
        return (this->*itsFindFunc) (value);
    }
    Bool result = False;
    uInt n = itsElems.nelements();
    for (uInt i=0; i<n; i++) {
	itsElems[i]->matchDate (&result, &value, 1, id);
    }
    return result;
}
Array<Bool> TableExprNodeSet::hasArrayBool (const TableExprId& id,
					    const Array<Bool>& value)
{
    Array<Bool> set = getArrayBool (id);
    Array<Bool> result(value.shape());
    result.set (False);
    Bool deleteIn, deleteOut;
    const Bool* in = value.getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    uInt nval = value.nelements();
    uInt n = itsElems.nelements();
    for (uInt i=0; i<n; i++) {
	itsElems[i]->matchBool (out, in, nval, id);
    }
    value.freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return result;
}
Array<Bool> TableExprNodeSet::hasArrayInt (const TableExprId& id,
                                           const Array<Int64>& value)
{
    Array<Int64> set = getArrayInt (id);
    Array<Bool> result(value.shape());
    result.set (False);
    Bool deleteIn, deleteOut;
    const Int64* in = value.getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    uInt nval = value.nelements();
    uInt n = itsElems.nelements();
    for (uInt i=0; i<n; i++) {
	itsElems[i]->matchInt (out, in, nval, id);
    }
    value.freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return result;
}
Array<Bool> TableExprNodeSet::hasArrayDouble (const TableExprId& id,
					      const Array<Double>& value)
{
    Array<Double> set = getArrayDouble (id);
    Array<Bool> result(value.shape());
    result.set (False);
    Bool deleteIn, deleteOut;
    const Double* in = value.getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    uInt nval = value.nelements();
    uInt n = itsElems.nelements();
    for (uInt i=0; i<n; i++) {
	itsElems[i]->matchDouble (out, in, nval, id);
    }
    value.freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return result;
}
Array<Bool> TableExprNodeSet::hasArrayDComplex (const TableExprId& id,
						const Array<DComplex>& value)
{
    Array<DComplex> set = getArrayDComplex (id);
    Array<Bool> result(value.shape());
    result.set (False);
    Bool deleteIn, deleteOut;
    const DComplex* in = value.getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    uInt nval = value.nelements();
    uInt n = itsElems.nelements();
    for (uInt i=0; i<n; i++) {
	itsElems[i]->matchDComplex (out, in, nval, id);
    }
    value.freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return result;
}
Array<Bool> TableExprNodeSet::hasArrayString (const TableExprId& id,
					      const Array<String>& value)
{
    Array<String> set = getArrayString (id);
    Array<Bool> result(value.shape());
    result.set (False);
    Bool deleteIn, deleteOut;
    const String* in = value.getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    uInt nval = value.nelements();
    uInt n = itsElems.nelements();
    for (uInt i=0; i<n; i++) {
	itsElems[i]->matchString (out, in, nval, id);
    }
    value.freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return result;
}
Array<Bool> TableExprNodeSet::hasArrayDate (const TableExprId& id,
					    const Array<MVTime>& value)
{
    Array<MVTime> set = getArrayDate (id);
    Array<Bool> result(value.shape());
    result.set (False);
    Bool deleteIn, deleteOut;
    const MVTime* in = value.getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    uInt nval = value.nelements();
    uInt n = itsElems.nelements();
    for (uInt i=0; i<n; i++) {
	itsElems[i]->matchDate (out, in, nval, id);
    }
    value.freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return result;
}

} //# NAMESPACE CASACORE - END

