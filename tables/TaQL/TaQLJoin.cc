//# TaQLJoin.cc: Class handling the condition of the join clause
//# Copyright (C) 2022
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

#include <casacore/tables/TaQL/TaQLJoin.h>
#include <casacore/tables/TaQL/TableParseJoin.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  
  Int64 TaQLJoinRow::findRow (const TableExprId&)
  {
    return itsRow;
  }
  


  TaQLJoin::TaQLJoin (const TENShPtr& mainNode, const TENShPtr& joinNode,
                      const std::vector<std::shared_ptr<TaQLJoinBase>>& children)
    : itsMainNode (mainNode),
      itsJoinNode (joinNode),
      itsChildren (children)
  {
    itsOptSet = dynamic_cast<TableExprNodeSetOptBase*>(itsJoinNode.get());
    AlwaysAssert (itsOptSet, AipsError);
  }
    
  Int64 TaQLJoin::findRow (const TableExprId& id)
  {
    Int64 index;
    switch (itsMainNode->dataType()) {
    case TableExprNodeRep::NTInt:
      index = itsOptSet->find (itsMainNode->getInt(id));
      break;
    case TableExprNodeRep::NTDouble:
      index = itsOptSet->find (itsMainNode->getDouble(id));
      break;
    case TableExprNodeRep::NTString:
      index = itsOptSet->find (itsMainNode->getString(id));
      break;
    default:
      index = -1;
    }
    if (index < 0) {
      return -1;
    }
    return itsChildren[index]->findRow (id);
  }

  template<typename T>
  std::shared_ptr<TaQLJoinBase> TaQLJoin::makeOptDiscrete
  (TableExprNodeRep& node,
   const std::vector<TableExprNode>& mainNodes,
   const std::vector<TableExprNode>& joinNodes,
   const std::vector<rownr_t>& rows,
   size_t level)
  {
    Vector<T> vec (rows.size());
    for (size_t i=0; i<rows.size(); ++i) {
      node.get (rows[i], vec[i]);
    }
    std::vector<std::shared_ptr<TaQLJoinBase>> children;
    Vector<T> newVec(vec);
    if (level == mainNodes.size() - 1) {
      // For the lowest level, the children are TaQLJoinRow objects
      // containing the row number only.
      for (size_t j=0; j<rows.size(); ++j) {
        children.push_back (std::shared_ptr<TaQLJoinBase>(new TaQLJoinRow(rows[j])));
      }
    } else {
      // For other levels a TaQLJoin object is created for each unique value.
      // It contains all row numbers per value.
      // Sort the values and get the unique ones.
      Vector<Int64> index;
      GenSortIndirect<T,Int64>::sort (index, vec.data(), vec.size());
      std::vector<T> vals;
      T val = vec[index[0]];
      std::vector<rownr_t> srows;
      srows.push_back (index[0]);
      for (size_t j=1; j<rows.size(); ++j) {
        Int64 row = index[j];
        T val2 = vec[row];
        if (val2 == val) {
          srows.push_back (row);
        } else {
          vals.push_back (val);
          children.push_back (TaQLJoin::createRecursive
                              (mainNodes, joinNodes, srows, level+1));
          val = val2;
          srows.resize(0);
          srows.push_back (index[j]);
        }
      }
      vals.push_back (val);
      children.push_back (TaQLJoin::createRecursive
                          (mainNodes, joinNodes, srows, level+1));
      newVec.reference (Vector<T>(vals));
    }
    TENShPtr optSet (new TableExprNodeSetOptUSet<T> (node, newVec));
    return std::shared_ptr<TaQLJoinBase>
      (new TaQLJoin (mainNodes[level].getRep(), optSet, children));
  }

  template<typename T>
  std::shared_ptr<TaQLJoinBase> TaQLJoin::makeOptInterval
  (const TableExprNodeSet& set,
   const std::vector<TableExprNode>& mainNodes,
   const std::vector<TableExprNode>& joinNodes,
   const std::vector<rownr_t>& rows,
   size_t level)
  {
    const TableExprNodeSetElemCont& elem =
      dynamic_cast<const TableExprNodeSetElemCont&>(*(set[0].get()));
    // Get all start-end interval values.
    Block<T> stvals(rows.size());
    Block<T> endvals(rows.size());
    for (size_t i=0; i<rows.size(); ++i) {
      // Use evaluate to ensure that mid-width is converted to start-end.
      TENSEBShPtr newElem = elem.evaluate (rows[i]);
      newElem->getStart (0, stvals[i]);
      newElem->getEnd (0, endvals[i]);
    }
    // Sort the intervals in order of start value.
    Vector<Int64> index;
    GenSortIndirect<T,Int64>::sort (index, stvals, stvals.size());
    std::vector<T> starts;
    std::vector<T> ends;
    std::vector<std::shared_ptr<TaQLJoinBase>> children;
    if (level == mainNodes.size() - 1) {
      // For the lowest level, the children are TaQLJoinRow objects
      // containing the row number only.
      for (size_t j=0; j<rows.size(); ++j) {
        Int64 row = rows[index[j]];
        starts.push_back (stvals[index[j]]);
        ends.push_back (endvals[index[j]]);
        children.push_back (std::shared_ptr<TaQLJoinBase>(new TaQLJoinRow(row)));
      }
    } else {
      // For other levels a TaQLJoin object is created for each unique interval.
      // It contains all row numbers for per interval.
      T st = stvals[index[0]];
      T end = endvals[index[0]];
      std::vector<rownr_t> srows;
      srows.push_back (index[0]);
      for (size_t j=1; j<rows.size(); ++j) {
        Int64 row = rows[index[j]];
        T st2 = stvals[row];
        T end2 = endvals[row];
        if (st2 == st  &&  end2 == end) {
          srows.push_back (row);
        } else {
          starts.push_back (st);
          ends.push_back (end);
          children.push_back (TaQLJoin::createRecursive
                              (mainNodes, joinNodes, srows, level+1));
          st = st2;
          end = end2;
          srows.resize(0);
          srows.push_back (index[j]);
        }
      }
      starts.push_back (st);
      ends.push_back (end);
      children.push_back (TaQLJoin::createRecursive
                          (mainNodes, joinNodes, srows, level+1));
    }
    // The intervals are kept in the optimized ContSet object for
    // speedy interval lookup.
    TENShPtr optSet (TableExprNodeSetOptContSetBase<T>::createOptSet
                     (set, starts, ends,
                      std::vector<Bool>({elem.isLeftClosed()}),
                      std::vector<Bool>({elem.isRightClosed()})));
    return std::shared_ptr<TaQLJoinBase>
      (new TaQLJoin (mainNodes[level].getRep(), optSet, children));
  }

  std::shared_ptr<TaQLJoinBase> TaQLJoin::createRecursive
  (const std::vector<TableExprNode>& mainNodes,
   const std::vector<TableExprNode>& joinNodes,
   const std::vector<rownr_t>& rows,
   size_t level)
  {
    // TaQLJoin objects are created in a recursive way and stored as a tree.
    // Each level represents an EQ or IN comparison in the join condition.
    // The top level contains one object.
    // The next level contains an object per unique value or interval.
    // In that way finding the matching row for multiple comparisons means
    // finding the right value at each level.
    // First see if a value or an interval is given.
    TableExprNodeRep* node = joinNodes[level].getRep().get();
    TableExprNodeRep* mainNode = mainNodes[level].getRep().get();
    const TableExprNodeSet* set  = dynamic_cast<TableExprNodeSet*>(node);
    std::shared_ptr<TaQLJoinBase> joinTree;
    if (!set) {
      AlwaysAssert (node->valueType() == TableExprNodeRep::VTScalar, AipsError);
      if (node->dataType() == TableExprNodeRep::NTInt  &&
          mainNode->dataType() == TableExprNodeRep::NTInt) {
        joinTree = makeOptDiscrete<Int64> (*node, mainNodes, joinNodes, rows, level);
      } else if (node->dataType() == TableExprNodeRep::NTString  &&
                 mainNode->dataType() == TableExprNodeRep::NTString) {
        joinTree = makeOptDiscrete<String> (*node, mainNodes, joinNodes, rows, level);
      } else {
        throw TableInvExpr ("In a equality join condition only Int and String "
                            "data types are possible");
      }
    } else {
      // Check if a correct interval is given in the set.
      // The data type checking is superfluous because the IN operator does that.
      // But you never know.
      AlwaysAssert (set->size() == 1, AipsError);
      AlwaysAssert (!set->isDiscrete()  &&  !set->hasArrays(), AipsError);
      const TENSEBShPtr& elem = (*set)[0];
      if ((elem->dataType() == TableExprNodeRep::NTInt  ||
           elem->dataType() == TableExprNodeRep::NTDouble  ||
           elem->dataType() == TableExprNodeRep::NTDate)  &&
          (mainNode->dataType() == TableExprNodeRep::NTInt  ||
           mainNode->dataType() == TableExprNodeRep::NTDouble  ||
           mainNode->dataType() == TableExprNodeRep::NTDate)) {
        joinTree = makeOptInterval<Double> (*set, mainNodes, joinNodes,
                                            rows, level);
      } else if (elem->dataType() == TableExprNodeRep::NTString  &&
                 mainNode->dataType() == TableExprNodeRep::NTString) {
        joinTree = makeOptInterval<String> (*set, mainNodes, joinNodes,
                                            rows, level);
      } else {
        throw TableInvExpr ("In an interval join condition only Int, Double, Date "
                            "and String data types are possible");
      }
    }
    return joinTree;
  }


  
  TaQLJoinColumn::TaQLJoinColumn (const TENShPtr& columnNode,
                                  const TableParseJoin& join)
    : TableExprNodeRep (*columnNode),
      itsColumn (columnNode),
      itsJoin   (join)
  {
    setUnit (columnNode->unit());
  }

  // Get the table info for this column.
  TableExprInfo TaQLJoinColumn::getTableInfo() const
  {
    return itsColumn->getTableInfo();
  }
  
  void TaQLJoinColumn::clear()
  {}
  
  MArray<Bool> TaQLJoinColumn::getArrayBool (const TableExprId& id)
  {
    Int64 rownr = itsJoin.findRow(id);
    if (rownr < 0) {
      return MArray<Bool>();
    }
    return itsColumn->getArrayBool (rownr);
  }
  
  MArray<Int64> TaQLJoinColumn::getArrayInt (const TableExprId& id)
  {
    Int64 rownr = itsJoin.findRow(id);
    if (rownr < 0) {
      return MArray<Int64>();
    }
    return itsColumn->getArrayInt (rownr);
  }
  
  MArray<Double> TaQLJoinColumn::getArrayDouble (const TableExprId& id)
  {
    Int64 rownr = itsJoin.findRow(id);
    if (rownr < 0) {
      return MArray<Double>();
    }
    return itsColumn->getArrayDouble (rownr);
  }
  
  MArray<DComplex> TaQLJoinColumn::getArrayDComplex (const TableExprId& id)
  {
    Int64 rownr = itsJoin.findRow(id);
    if (rownr < 0) {
      return MArray<DComplex>();
    }
    return itsColumn->getArrayDComplex (rownr);
  }
  
  MArray<String> TaQLJoinColumn::getArrayString (const TableExprId& id)
  {
    Int64 rownr = itsJoin.findRow(id);
    if (rownr < 0) {
      return MArray<String>();
    }
    return itsColumn->getArrayString (rownr);
  }

  MArray<MVTime> TaQLJoinColumn::getArrayDate (const TableExprId& id)
  {
    Int64 rownr = itsJoin.findRow(id);
    if (rownr < 0) {
      return MArray<MVTime>();
    }
    return itsColumn->getArrayDate (rownr);
  }

  TableExprNode TaQLJoinColumn::makeColumnNode (const TENShPtr& columnNode,
                                                const TableParseJoin& join)
  {
    DebugAssert (columnNode->operType() == OtColumn, AipsError);
    if (columnNode->valueType() == VTScalar) {
      switch (columnNode->dataType()) {
      case NTBool:
        return new TaQLJoinColumnBool (columnNode, join);
      case NTInt:
        return new TaQLJoinColumnInt (columnNode, join);
      case NTDouble:
        return new TaQLJoinColumnDouble (columnNode, join);
      case NTComplex:
        return new TaQLJoinColumnDComplex (columnNode, join);
      case NTString:
        return new TaQLJoinColumnString (columnNode, join);
      case NTDate:
        return new TaQLJoinColumnDate (columnNode, join);
      default:
        throw TableInvDT();
      }
    }
    return new TaQLJoinColumn (columnNode, join); 
  }
    

  TaQLJoinColumnBool::TaQLJoinColumnBool (const TENShPtr& columnNode,
                                          const TableParseJoin& join)
    : TaQLJoinColumn (columnNode, join)
  {
    rownr_t nrow = itsColumn->getTableInfo().table().nrow();
    itsData.resize (nrow);
    for (rownr_t row=0; row<nrow; ++row) {
      itsData[row] = itsColumn->getBool(row);
    }
  }
  Bool TaQLJoinColumnBool::getBool (const TableExprId& id)
  {
    Int64 rownr = itsJoin.findRow(id);
    if (rownr < 0) {
      return False;
    }
    DebugAssert (rownr < itsData.size(), AipsError);
    return itsData[rownr];
  }
  void TaQLJoinColumnBool::clear()
    { itsData.resize(); }

  TaQLJoinColumnInt::TaQLJoinColumnInt (const TENShPtr& columnNode,
                                        const TableParseJoin& join)
    : TaQLJoinColumn (columnNode, join)
  {
    rownr_t nrow = itsColumn->getTableInfo().table().nrow();
    itsData.resize (nrow);
    for (rownr_t row=0; row<nrow; ++row) {
      itsData[row] = itsColumn->getInt(row);
    }
  }
  Int64 TaQLJoinColumnInt::getInt (const TableExprId& id)
  {
    Int64 rownr = itsJoin.findRow(id);
    if (rownr < 0) {
      return std::numeric_limits<Int64>::max();
    }
    DebugAssert (rownr < itsData.size(), AipsError);
    return itsData[rownr];
  }
  void TaQLJoinColumnInt::clear()
    { itsData.resize(); }


  TaQLJoinColumnDouble::TaQLJoinColumnDouble (const TENShPtr& columnNode,
                                              const TableParseJoin& join)
    : TaQLJoinColumn (columnNode, join)
  {
    rownr_t nrow = itsColumn->getTableInfo().table().nrow();
    itsData.resize (nrow);
    for (rownr_t row=0; row<nrow; ++row) {
      itsData[row] = itsColumn->getDouble(row);
    }
  }
  Double TaQLJoinColumnDouble::getDouble (const TableExprId& id)
  {
    Int64 rownr = itsJoin.findRow(id);
    if (rownr < 0) {
      return std::numeric_limits<Double>::quiet_NaN();
    }
    DebugAssert (rownr < itsData.size(), AipsError);
    return itsData[rownr];
  }
  void TaQLJoinColumnDouble::clear()
    { itsData.resize(); }


  TaQLJoinColumnDComplex::TaQLJoinColumnDComplex (const TENShPtr& columnNode,
                                                  const TableParseJoin& join)
    : TaQLJoinColumn (columnNode, join)
  {
    rownr_t nrow = itsColumn->getTableInfo().table().nrow();
    itsData.resize (nrow);
    for (rownr_t row=0; row<nrow; ++row) {
      itsData[row] = itsColumn->getDComplex(row);
    }
  }
  DComplex TaQLJoinColumnDComplex::getDComplex (const TableExprId& id)
  {
    Int64 rownr = itsJoin.findRow(id);
    if (rownr < 0) {
      return DComplex(std::numeric_limits<Double>::quiet_NaN(),
                      std::numeric_limits<Double>::quiet_NaN());
    }
    DebugAssert (rownr < itsData.size(), AipsError);
    return itsData[rownr];
  }
  void TaQLJoinColumnDComplex::clear()
    { itsData.resize(); }


  TaQLJoinColumnString::TaQLJoinColumnString (const TENShPtr& columnNode,
                                              const TableParseJoin& join)
    : TaQLJoinColumn (columnNode, join)
  {
    rownr_t nrow = itsColumn->getTableInfo().table().nrow();
    itsData.resize (nrow);
    for (rownr_t row=0; row<nrow; ++row) {
      itsData[row] = itsColumn->getString(row);
    }
  }
  String TaQLJoinColumnString::getString (const TableExprId& id)
  {
    Int64 rownr = itsJoin.findRow(id);
    if (rownr < 0) {
      return "none";
    }
    DebugAssert (rownr < itsData.size(), AipsError);
    return itsData[rownr];
  }
  void TaQLJoinColumnString::clear()
    { itsData.resize(); }

  TaQLJoinColumnDate::TaQLJoinColumnDate (const TENShPtr& columnNode,
                                          const TableParseJoin& join)
    : TaQLJoinColumn (columnNode, join)
  {
    rownr_t nrow = itsColumn->getTableInfo().table().nrow();
    itsData.resize (nrow);
    for (rownr_t row=0; row<nrow; ++row) {
      itsData[row] = itsColumn->getDate(row);
    }
  }
  MVTime TaQLJoinColumnDate::getDate (const TableExprId& id)
  {
    Int64 rownr = itsJoin.findRow(id);
    if (rownr < 0) {
      return MVTime();
    }
    DebugAssert (rownr < itsData.size(), AipsError);
    return itsData[rownr];
  }
  void TaQLJoinColumnDate::clear()
    { itsData.resize(); }



  TaQLJoinRowid::TaQLJoinRowid (const TableExprInfo& tabInfo,
                                const TableParseJoin& join)
    : TableExprNodeRep (NTInt, VTScalar, OtRownr, Variable),
      itsTabInfo(tabInfo),
      itsJoin   (join)
  {}
  TableExprInfo TaQLJoinRowid::getTableInfo() const
  {
    return itsTabInfo;
  }
  Int64 TaQLJoinRowid::getInt (const TableExprId& id)
  {
    return itsJoin.findRow(id);
  }


} //# NAMESPACE CASACORE - END
