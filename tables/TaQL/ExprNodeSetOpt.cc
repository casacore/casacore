//# ExprNodeSetOpt.cc: Classes representing an optimized set in table select expression
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

//# Includes
#include <casacore/tables/TaQL/ExprNodeSetOpt.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  TableExprNodeSetOptBase::TableExprNodeSetOptBase
  (const TableExprNodeRep& orig)
    : TableExprNodeRep (orig)
  {}
  Bool TableExprNodeSetOptBase::contains (const TableExprId&, Int64 value)
    { return (find(value) >= 0); }
  Bool TableExprNodeSetOptBase::contains (const TableExprId&, Double value)
    { return (find(value) >= 0); }
  Bool TableExprNodeSetOptBase::contains (const TableExprId&, String value)
    { return (find(value) >= 0); }
  MArray<Bool> TableExprNodeSetOptBase::contains (const TableExprId&,
                                                  const MArray<Int64>& value)
  {
    Array<Bool> result(value.shape());
    Bool deleteIn, deleteOut;
    const Int64* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    for (size_t i=0; i<value.size(); ++i) {
      out[i] = (find(in[i]) >= 0);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
  }
  MArray<Bool> TableExprNodeSetOptBase::contains (const TableExprId&,
                                                  const MArray<Double>& value)
  {
    Array<Bool> result(value.shape());
    Bool deleteIn, deleteOut;
    const Double* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    for (size_t i=0; i<value.size(); ++i) {
      out[i] = (find(in[i]) >= 0);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
  }
  MArray<Bool> TableExprNodeSetOptBase::contains (const TableExprId&,
                                                  const MArray<String>& value)
  {
    Array<Bool> result(value.shape());
    Bool deleteIn, deleteOut;
    const String* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    for (size_t i=0; i<value.size(); ++i) {
      out[i] = (find(in[i]) >= 0);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
  }
  Int64 TableExprNodeSetOptBase::find (Int64) const
    { return -1; }
  Int64 TableExprNodeSetOptBase::find (Double) const
    { return -1; }
  Int64 TableExprNodeSetOptBase::find (String) const
    { return -1; }



  template<typename T>
  TableExprNodeSetOptUSet<T>::TableExprNodeSetOptUSet (const TableExprNodeRep& orig,
                                                       const Array<T>& arr)
    : TableExprNodeSetOptBase (orig)
  {
    itsMap.clear();
    auto iter = arr.begin();
    for (size_t i=0; i<arr.size(); ++i) {
      itsMap.insert (std::make_pair (*iter, i));
      ++iter;
    }
  }
  
  template<typename T>
  void TableExprNodeSetOptUSet<T>::show (ostream& os, uInt indent) const
  {
    TableExprNodeRep::show (os, indent);
    os << "Int set as std::unordered_map<T>" << endl;
  }
  
  template<typename T>
  Int64 TableExprNodeSetOptUSet<T>::find (T value) const
  {
    auto iter = itsMap.find(value);
    if (iter == itsMap.end()) {
      return -1;
    }
    return iter->second;
  }



  TableExprNodeSetOptContSetBase::TableExprNodeSetOptContSetBase
  (const TableExprNodeSet& orig)
    : TableExprNodeSetOptBase (orig)
  {}


  template<typename T>
  TableExprNodeSetOptContSet<T>::TableExprNodeSetOptContSet
  (const TableExprNodeSet& orig,
   const std::vector<T>& starts,
   const std::vector<T>& ends,
   const std::vector<Bool>& leftC,
   const std::vector<Bool>& rightC)
    : TableExprNodeSetOptContSetBase (orig),
      itsStarts (starts),
      itsEnds   (ends),
      itsLeftC  (leftC),
      itsRightC (rightC)
  {
    AlwaysAssert (starts.size() == ends.size(), AipsError);
    AlwaysAssert (starts.size() == leftC.size(), AipsError);
    AlwaysAssert (starts.size() == rightC.size(), AipsError);
  }

  template<typename T>
  TableExprNodeSetOptContSet<T>::TableExprNodeSetOptContSet
  (const TableExprNodeSet& orig,
   const std::vector<T>& starts,
   const std::vector<T>& ends)
    : TableExprNodeSetOptContSetBase (orig),
      itsStarts (starts),
      itsEnds   (ends)
  {
    AlwaysAssert (starts.size() == ends.size(), AipsError);
  }

  template<typename T>
  void TableExprNodeSetOptContSet<T>::show (ostream& os, uInt indent) const
  {
    TableExprNodeRep::show (os, indent);
    os << "TableExprNodeSetOptContSet with " << itsStarts.size()
       << " intervals" << endl
       << " start = " << itsStarts << "  leftC = " << itsLeftC << endl
       << "   end = " << itsEnds << "  rightC = " << itsRightC << endl;
  }
  
  template<typename T>
  Int64 TableExprNodeSetOptContSet<T>::find (T value) const
  {
    for (size_t i=0; i<itsStarts.size(); ++i) {
      if ((value > itsStarts[i]  &&  value < itsEnds[i])  ||
          (value == itsStarts[i]  &&  itsLeftC[i])  ||
          (value == itsEnds[i]  &&  itsRightC[i])) {
        return i;
      }
    }
    return -1;
  }

  template<typename T>
  TENShPtr TableExprNodeSetOptContSet<T>::transform (const TableExprNodeSet& set,
                                                     Bool combine)
  {
    DebugAssert (set.size() > 0, AipsError);
    // Get all start values and sort them (indirectly) in ascending order.
    // Use lowest value if no start given.
    // Similar for all end values.
    Block<T> stvals (set.size(), std::numeric_limits<T>::lowest());
    Block<T> endvals(set.size(), std::numeric_limits<T>::max());
    // Make an id for the gets.
    // The values are constant, hence use an use arbitrary row number.
    TableExprId id(0);
    for (size_t i=0; i<set.size(); ++i) {
      if (set[i]->start()) {
        set[i]->getStart (id, stvals[i]);
      }
      if (set[i]->end()) {
        set[i]->getEnd (id, endvals[i]);
      }
    }
    // Sort the start values indirectly.
    Vector<Int64> index;
    GenSortIndirect<T,Int64>::sort (index, stvals, stvals.size());
    std::vector<T> newStart;
    std::vector<T> newEnd;
    std::vector<Bool> newLeftC;
    std::vector<Bool> newRightC;
    std::vector<rownr_t> rowNrs;
    if (!combine) {
      for (size_t i=0; i<index.size(); ++i) {
        Int64 inx = index[i];
        newStart.push_back (stvals[inx]);
        newEnd.push_back (endvals[inx]);
        newLeftC.push_back (set[inx]->isLeftClosed());
        newRightC.push_back (set[inx]->isRightClosed());
        rowNrs.push_back (inx);
      }
    } else {
      // Get the start and end value of first interval in sorted list.
      T stval  = stvals[index[0]];
      T endval = endvals[index[0]];
      Bool leftC  = set[index[0]]->isLeftClosed();
      Bool rightC = set[index[0]]->isRightClosed();
      rownr_t rownr = index[0];
      // Loop through the next intervals and combine if possible.
      for (size_t i=1; i<index.size(); i++) {
        Int64 inx = index[i];
        T st2 = stvals[inx];
        T end2 = endvals[inx];
        // Combine intervals if they overlap.
        // They do if the next interval starts before end of this one
        // or if starting at the end and one side of the interval is closed.
        if (st2 < endval  ||
            (st2 == endval  &&
             (set[inx]->isLeftClosed() || set[index[i-1]]->isRightClosed()))) {
          // Overlap; update end if higher.
          if (end2 > endval) {
            endval = end2;
            rightC = set[inx]->isRightClosed();
          }
        } else {
          // No overlap, so create the interval found and start a new one.
          newStart.push_back (stval);
          newEnd.push_back (endval);
          newLeftC.push_back (leftC);
          newRightC.push_back (rightC);
          rowNrs.push_back (rownr);
          stval  = st2;
          endval = end2;
          leftC  = set[inx]->isLeftClosed();
          rightC = set[inx]->isRightClosed();
          rownr  = inx;
        }
      }
      // Create the last interval.
      newStart.push_back (stval);
      newEnd.push_back (endval);
      newLeftC.push_back (leftC);
      newRightC.push_back (rightC);
      rowNrs.push_back (rownr);
    }
    // Now create the correct object.
    return createOptSet (set, newStart, newEnd, newLeftC, newRightC);
  }

  template<typename T>
  TENShPtr TableExprNodeSetOptContSet<T>::createOptSet
  (const TableExprNodeSet& set,
   const std::vector<T>& start, const std::vector<T>& end, 
   const std::vector<Bool>& leftC, const std::vector<Bool>& rightC)
   {
    // See if all intervals have the same left and right closedness.
    // If so, a better version can be used that does not need to test on it
    // for each compare.
    Bool same = True;
    for (size_t i=1; i<leftC.size(); ++i) {
      if (leftC[i] != leftC[0]  ||  rightC[i] != rightC[0]) {
        same = False;
        break;
      }
    }
    // Create the appropriate object.
    TableExprNodeSetOptContSetBase* optSet;
    if (same) {
      if (leftC[0]) {
        if (rightC[0]) {
          optSet = new TableExprNodeSetOptContSetCC<T> (set, start, end);
        } else {
          optSet = new TableExprNodeSetOptContSetCO<T> (set, start, end);
        }
      } else {
        if (rightC[0]) {
          optSet = new TableExprNodeSetOptContSetOC<T> (set, start, end);
        } else {
          optSet = new TableExprNodeSetOptContSetOO<T> (set, start, end);
        }
      }
    } else {
      optSet = new TableExprNodeSetOptContSet<T> (set, start, end,
                                                  leftC, rightC);
    }
    TENShPtr ptr(optSet);
    ///optSet->setRows (rowNrs);
    return ptr;
  }


  template<typename T>
  TableExprNodeSetOptContSetCC<T>::TableExprNodeSetOptContSetCC
  (const TableExprNodeSet& orig,
   const std::vector<T>& starts,
   const std::vector<T>& ends)
    : TableExprNodeSetOptContSet<T> (orig, starts, ends)
  {}

  template<typename T>
  void TableExprNodeSetOptContSetCC<T>::show (ostream& os, uInt indent) const
  {
    TableExprNodeSetOptContSet<T>::show (os, indent);
    os << " as TableExprNodeSetOptContSetCC" << endl;
  }
  
  template<typename T>
  Int64 TableExprNodeSetOptContSetCC<T>::find (T value) const
  {
    // Use lambda function to use <= instead of <.
    auto iter = std::upper_bound (this->itsEnds.begin(), this->itsEnds.end(), value,
                                  [](T v1, T v2){return v1<=v2;});
    if (iter != this->itsEnds.end()) {
      size_t index = std::distance (this->itsEnds.begin(), iter);
      if (value >= this->itsStarts[index]) {
        return index;
      }
    }
    return -1;
  }
  

  template<typename T>
  TableExprNodeSetOptContSetCO<T>::TableExprNodeSetOptContSetCO
  (const TableExprNodeSet& orig,
   const std::vector<T>& starts,
   const std::vector<T>& ends)
    : TableExprNodeSetOptContSet<T> (orig, starts, ends)
  {}

  template<typename T>
  void TableExprNodeSetOptContSetCO<T>::show (ostream& os, uInt indent) const
  {
    TableExprNodeSetOptContSet<T>::show (os, indent);
    os << " as TableExprNodeSetOptContSetCO" << endl;
  }
  
  template<typename T>
  Int64 TableExprNodeSetOptContSetCO<T>::find (T value) const
  {
    auto iter = std::upper_bound (this->itsEnds.begin(), this->itsEnds.end(), value);
    if (iter != this->itsEnds.end()) {
      size_t index = std::distance (this->itsEnds.begin(), iter);
      if (value >= this->itsStarts[index]) {
        return index;
      }
    }
    return -1;
  }


  template<typename T>
  TableExprNodeSetOptContSetOC<T>::TableExprNodeSetOptContSetOC
  (const TableExprNodeSet& orig,
   const std::vector<T>& starts,
   const std::vector<T>& ends)
    : TableExprNodeSetOptContSet<T> (orig, starts, ends)
  {}

  template<typename T>
  void TableExprNodeSetOptContSetOC<T>::show (ostream& os, uInt indent) const
  {
    TableExprNodeSetOptContSet<T>::show (os, indent);
    os << " as TableExprNodeSetOptContSetOC" << endl;
  }
  
  template<typename T>
  Int64 TableExprNodeSetOptContSetOC<T>::find (T value) const
  {
    // Use lambda function to use <= instead of <.
    auto iter = std::upper_bound (this->itsEnds.begin(), this->itsEnds.end(), value,
                                  [](T v1, T v2){return v1<=v2;});
    if (iter != this->itsEnds.end()) {
      size_t index = std::distance (this->itsEnds.begin(), iter);
      if (value > this->itsStarts[index]) {
        return index;
      }
    }
    return -1;
  }


  template<typename T>
  TableExprNodeSetOptContSetOO<T>::TableExprNodeSetOptContSetOO
  (const TableExprNodeSet& orig,
   const std::vector<T>& starts,
   const std::vector<T>& ends)
    : TableExprNodeSetOptContSet<T> (orig, starts, ends)
  {}

  template<typename T>
  void TableExprNodeSetOptContSetOO<T>::show (ostream& os, uInt indent) const
  {
    TableExprNodeSetOptContSet<T>::show (os, indent);
    os << " as TableExprNodeSetOptContSetOO" << endl;
  }
  
  template<typename T>
  Int64 TableExprNodeSetOptContSetOO<T>::find (T value) const
  {
    auto iter = std::upper_bound (this->itsEnds.begin(), this->itsEnds.end(), value);
    if (iter != this->itsEnds.end()) {
      size_t index = std::distance (this->itsEnds.begin(), iter);
      if (value > this->itsStarts[index]) {
        return index;
      }
    }
    return -1;
  }


  // Instantiate for Double and String.
  template class TableExprNodeSetOptUSet<Int64>;
  template class TableExprNodeSetOptUSet<String>;
  template class TableExprNodeSetOptContSet<Double>;
  template class TableExprNodeSetOptContSetCC<Double>;
  template class TableExprNodeSetOptContSetCO<Double>;
  template class TableExprNodeSetOptContSetOC<Double>;
  template class TableExprNodeSetOptContSetOO<Double>;
  template class TableExprNodeSetOptContSet<String>;
  template class TableExprNodeSetOptContSetCC<String>;
  template class TableExprNodeSetOptContSetCO<String>;
  template class TableExprNodeSetOptContSetOC<String>;
  template class TableExprNodeSetOptContSetOO<String>;
  
} //# NAMESPACE CASACORE - END
