// # MSFieldParse.cc: Classes to hold results from field grammar parser
// # Copyright (C) 1994,1995,1997,1998,1999,2000,2001,2003
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This library is free software; you can redistribute it and/or modify it
// # under the terms of the GNU Library General Public License as published by
// # the Free Software Foundation; either version 2 of the License, or (at your
// # option) any later version.
// #
// # This library is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
// # License for more details.
// #
// # You should have received a copy of the GNU Library General Public License
// # along with this library; if not, write to the Free Software Foundation,
// # Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

#include <casacore/ms/MSSel/MSFieldParse.h>
#include <casacore/ms/MSSel/MSFieldIndex.h>
#include <casacore/ms/MSSel/MSSourceIndex.h>
#include <casacore/ms/MSSel/MSSelectionTools.h>
#include <casacore/casa/Logging/LogIO.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

MSFieldParse* MSFieldParse::thisMSFParser = nullptr;  // Global pointer to the parser object
TableExprNode* MSFieldParse::node_p = nullptr;
TableExprNode MSFieldParse::columnAsTEN_p;
Vector<int> MSFieldParse::idList;

// # Constructor
MSFieldParse::MSFieldParse() : MSParse(), colName(MS::columnName(MS::FIELD_ID)) { reset(); }

// # Constructor with given ms name.
MSFieldParse::MSFieldParse(const MeasurementSet* ms)
    : MSParse(ms, "Field"), colName(MS::columnName(MS::FIELD_ID)), msFieldSubTable_p(ms->field()) {
  reset();
}

MSFieldParse::MSFieldParse(const MSField& msFieldSubTable, const TableExprNode& colAsTEN)
    : MSParse(), colName(MS::columnName(MS::FIELD_ID)), msFieldSubTable_p(msFieldSubTable) {
  reset();
  columnAsTEN_p = colAsTEN;
}

void MSFieldParse::reset() {
  if (MSFieldParse::node_p != nullptr) delete MSFieldParse::node_p;
  MSFieldParse::node_p = nullptr;
  if (node_p) delete node_p;
  node_p = new TableExprNode();
  idList.resize(0);
  //    setMS(ms);
}

const TableExprNode* MSFieldParse::selectFieldIds(const Vector<int>& fieldIds) {
  {
    Vector<int> tmp(set_union(fieldIds, idList));
    idList.resize(tmp.nelements());
    idList = tmp;
  }
  TableExprNode condition;
  //    TableExprNode condition = (msInterface()->asMS()->col(colName).in(fieldIds));
  //    condition = (msInterface()->col(colName).in(fieldIds));
  //    condition = ms()->col(colName).in(fieldIds);
  condition = columnAsTEN_p.in(fieldIds);
  // condition = ms()->col(colName);

  addCondition(*node_p, condition);

  // if(node_p->isNull())
  //   *node_p = condition.in(fieldIds);
  // else
  //   *node_p = *node_p || condition.in(fieldIds);

  return node_p;
}

const TableExprNode* MSFieldParse::node() { return node_p; }

}  // namespace casacore
