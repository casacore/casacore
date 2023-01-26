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
  bool TableExprNodeSetOptBase::contains (const TableExprId&, int64_t value)
    { return (find(value) >= 0); }
  bool TableExprNodeSetOptBase::contains (const TableExprId&, double value)
    { return (find(value) >= 0); }
  bool TableExprNodeSetOptBase::contains (const TableExprId&, String value)
    { return (find(value) >= 0); }
  MArray<bool> TableExprNodeSetOptBase::contains (const TableExprId&,
                                                  const MArray<int64_t>& value)
  {
    Array<bool> result(value.shape());
    bool deleteIn, deleteOut;
    const int64_t* in = value.array().getStorage (deleteIn);
    bool* out = result.getStorage (deleteOut);
    for (size_t i=0; i<value.size(); ++i) {
      out[i] = (find(in[i]) >= 0);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<bool> (result, value.mask());
  }
  MArray<bool> TableExprNodeSetOptBase::contains (const TableExprId&,
                                                  const MArray<double>& value)
  {
    Array<bool> result(value.shape());
    bool deleteIn, deleteOut;
    const double* in = value.array().getStorage (deleteIn);
    bool* out = result.getStorage (deleteOut);
    for (size_t i=0; i<value.size(); ++i) {
      out[i] = (find(in[i]) >= 0);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<bool> (result, value.mask());
  }
  MArray<bool> TableExprNodeSetOptBase::contains (const TableExprId&,
                                                  const MArray<String>& value)
  {
    Array<bool> result(value.shape());
    bool deleteIn, deleteOut;
    const String* in = value.array().getStorage (deleteIn);
    bool* out = result.getStorage (deleteOut);
    for (size_t i=0; i<value.size(); ++i) {
      out[i] = (find(in[i]) >= 0);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<bool> (result, value.mask());
  }
  int64_t TableExprNodeSetOptBase::find (int64_t) const
    { return -1; }
  int64_t TableExprNodeSetOptBase::find (double) const
    { return -1; }
  int64_t TableExprNodeSetOptBase::find (String) const
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
  void TableExprNodeSetOptUSet<T>::show (ostream& os, uint32_t indent) const
  {
    TableExprNodeRep::show (os, indent);
    os << "Int set as std::unordered_map<T>" << endl;
  }
  
  template<typename T>
  int64_t TableExprNodeSetOptUSet<T>::find (T value) const
  {
    auto iter = itsMap.find(value);
    if (iter == itsMap.end()) {
      return -1;
    }
    return iter->second;
  }



  template<typename T>
  TableExprNodeSetOptContSetBase<T>::TableExprNodeSetOptContSetBase
  (const TableExprNodeSet& orig,
   const std::vector<T>& starts,
   const std::vector<T>& ends)
    : TableExprNodeSetOptBase (orig),
      itsStarts (starts),
      itsEnds   (ends)
  {
    AlwaysAssert (starts.size() == ends.size(), AipsError);
  }

  template<typename T>
  void TableExprNodeSetOptContSetBase<T>::show (ostream& os, uint32_t indent) const
  {
    TableExprNodeRep::show (os, indent);
    os << "  TableExprNodeSetOptContSet with " << itsStarts.size()
       << " intervals" << endl
       << "    start = " << itsStarts << endl
       << "      end = " << itsEnds << endl;
  }
  
  template<typename T>
  TENShPtr TableExprNodeSetOptContSetBase<T>::transform (const TableExprNodeSet& set,
                                                         bool combine)
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
    Vector<int64_t> index;
    GenSortIndirect<T,int64_t>::sort (index, stvals, stvals.size());
    std::vector<T> newStart;
    std::vector<T> newEnd;
    std::vector<bool> newLeftC;
    std::vector<bool> newRightC;
    if (!combine) {
      for (size_t i=0; i<index.size(); ++i) {
        int64_t inx = index[i];
        newStart.push_back (stvals[inx]);
        newEnd.push_back (endvals[inx]);
        newLeftC.push_back (set[inx]->isLeftClosed());
        newRightC.push_back (set[inx]->isRightClosed());
      }
    } else {
      // Get the start and end value of first interval in sorted list.
      T stval  = stvals[index[0]];
      T endval = endvals[index[0]];
      bool leftC  = set[index[0]]->isLeftClosed();
      bool rightC = set[index[0]]->isRightClosed();
      // Loop through the next intervals and combine if possible.
      for (size_t i=1; i<index.size(); i++) {
        int64_t inx = index[i];
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
          stval  = st2;
          endval = end2;
          leftC  = set[inx]->isLeftClosed();
          rightC = set[inx]->isRightClosed();
        }
      }
      // Create the last interval.
      newStart.push_back (stval);
      newEnd.push_back (endval);
      newLeftC.push_back (leftC);
      newRightC.push_back (rightC);
    }
    // Now create the correct object.
    return createOptSet (set, newStart, newEnd, newLeftC, newRightC);
  }

  template<typename T>
  TENShPtr TableExprNodeSetOptContSetBase<T>::createOptSet
  (const TableExprNodeSet& set,
   const std::vector<T>& start, const std::vector<T>& end, 
   const std::vector<bool>& leftC, const std::vector<bool>& rightC)
   {
     AlwaysAssert (start.size() == end.size(), AipsError);
     AlwaysAssert (leftC.size() == rightC.size(), AipsError);
    // See if all intervals have the same left and right closedness.
    // If so, a better version can be used that does not need to test on it
    // for each compare.
    bool same = true;
    for (size_t i=1; i<leftC.size(); ++i) {
      if (leftC[i] != leftC[0]  ||  rightC[i] != rightC[0]) {
        same = false;
        break;
      }
    }
    // Create the appropriate object.
    // If all open/closed sides are the same, a more optimized class can be used,
    // where a closed side requires std::less_equal and an open side std::less.
    // Otherwise the more generic OptContSet class must be used.
    TableExprNodeSetOptContSetBase<T>* optSet;
    if (same) {
      if (leftC[0]) {
        if (rightC[0]) {
          optSet = new TableExprNodeSetOptContSet<T,std::less_equal<T>,std::less_equal<T>>
            (set, start, end, std::less_equal<T>(), std::less_equal<T>(), "CC");
        } else {
          optSet = new TableExprNodeSetOptContSet<T,std::less_equal<T>,std::less<T>>
            (set, start, end, std::less_equal<T>(), std::less<T>(), "CO");
        }
      } else {
        if (rightC[0]) {
          optSet = new TableExprNodeSetOptContSet<T,std::less<T>,std::less_equal<T>>
            (set, start, end, std::less<T>(), std::less_equal<T>(), "OC");
        } else {
          optSet = new TableExprNodeSetOptContSet<T,std::less<T>,std::less<T>>
            (set, start, end, std::less<T>(), std::less<T>(), "OO");
        }
      }
    } else {
      optSet = new TableExprNodeSetOptContSetMixOC<T> (set, start, end,
                                                       leftC, rightC);
    }
    return TENShPtr(optSet);
  }

  

  template<typename T>
  TableExprNodeSetOptContSetMixOC<T>::TableExprNodeSetOptContSetMixOC
  (const TableExprNodeSet& orig,
   const std::vector<T>& starts,
   const std::vector<T>& ends,
   const std::vector<bool>& leftC,
   const std::vector<bool>& rightC)
    : TableExprNodeSetOptContSetBase<T> (orig, starts, ends),
      itsLeftC  (leftC),
      itsRightC (rightC)
  {
    AlwaysAssert (starts.size() == leftC.size(), AipsError);
    AlwaysAssert (starts.size() == rightC.size(), AipsError);
  }

  template<typename T>
  void TableExprNodeSetOptContSetMixOC<T>::show (ostream& os, uint32_t indent) const
  {
    TableExprNodeSetOptContSetBase<T>::show (os, indent);
    os << "    leftC = " << itsLeftC << endl
       << "   rightC = " << itsRightC << endl;
  }
  
  template<typename T>
  int64_t TableExprNodeSetOptContSetMixOC<T>::find (T value) const
  {
    for (size_t i=0; i<this->itsStarts.size(); ++i) {
      if ((value > this->itsStarts[i]  &&  value < this->itsEnds[i])  ||
          (value == this->itsStarts[i]  &&  this->itsLeftC[i])  ||
          (value == this->itsEnds[i]  &&  this->itsRightC[i])) {
        return i;
      }
    }
    return -1;
  }



  template <typename T, typename LeftComp, typename RightComp>
  TableExprNodeSetOptContSet<T,LeftComp,RightComp>::TableExprNodeSetOptContSet
  (const TableExprNodeSet& orig,
   const std::vector<T>& starts, const std::vector<T>& ends,
   LeftComp leftCmp, RightComp rightCmp,
   const String& cmpType)
    : TableExprNodeSetOptContSetBase<T> (orig, starts, ends),
      itsLeftCmp   (leftCmp),
      itsRightCmp  (rightCmp),
      itsCmpType   (cmpType)
  {}

  template <typename T, typename LeftComp, typename RightComp>
  void TableExprNodeSetOptContSet<T,LeftComp,RightComp>::show
  (ostream& os, uint32_t indent) const
  {
    TableExprNodeSetOptContSetBase<T>::show (os, indent);
    os << "   as TableExprNodeSetOptContSet" << itsCmpType << endl;
  }

  template <typename T, typename LeftComp, typename RightComp>
  int64_t TableExprNodeSetOptContSet<T,LeftComp,RightComp>::find (T value) const
  {
    auto iter = std::upper_bound (this->itsEnds.begin(), this->itsEnds.end(),
                                  value, itsRightCmp);
    if (iter != this->itsEnds.end()) {
      size_t index = std::distance (this->itsEnds.begin(), iter);
      if (itsLeftCmp (this->itsStarts[index], value)) {
        return index;
      }
    }
    return -1;
  }


  // Instantiate as needed for int64_t, double and String.
  // Only the instantiated types are used in the TaQL code for the
  // IN and join operator. Datetime is handled as double.
  // bool, DComplex and Regex are not used in these operators.
  // std::less is for an open interval side, std::less_equal for a closed side.
  template class TableExprNodeSetOptUSet<int64_t>;
  template class TableExprNodeSetOptUSet<String>;
  template class TableExprNodeSetOptContSetBase<double>;
  template class TableExprNodeSetOptContSetBase<String>;
  template class TableExprNodeSetOptContSetMixOC<double>;
  template class TableExprNodeSetOptContSetMixOC<String>;
  template class TableExprNodeSetOptContSet<double,std::less_equal<double>,std::less_equal<double>>;
  template class TableExprNodeSetOptContSet<double,std::less_equal<double>,std::less<double>>;
  template class TableExprNodeSetOptContSet<double,std::less<double>,std::less_equal<double>>;
  template class TableExprNodeSetOptContSet<double,std::less<double>,std::less<double>>;
  template class TableExprNodeSetOptContSet<String,std::less_equal<String>,std::less_equal<String>>;
  template class TableExprNodeSetOptContSet<String,std::less_equal<String>,std::less<String>>;
  template class TableExprNodeSetOptContSet<String,std::less<String>,std::less_equal<String>>;
  template class TableExprNodeSetOptContSet<String,std::less<String>,std::less<String>>;


} //# NAMESPACE CASACORE - END
