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

  TableExprNodeSetOptIntUSet::TableExprNodeSetOptIntUSet (const TableExprNodeRep& orig,
                                                          const Array<Int64>& arr)
    : TableExprNodeRep (orig)
  {
    itsSet.clear();
    itsSet.insert(arr.begin(), arr.end());
  }
  
  void TableExprNodeSetOptIntUSet::show (ostream& os, uInt indent) const
  {
    TableExprNodeRep::show (os, indent);
    os << "Int set as std::unordered_set<Int64>" << endl;
  }
  
  Bool TableExprNodeSetOptIntUSet::contains (const TableExprId&, Int64 value)
  {
    return itsSet.find(value) != itsSet.end();
  }

  MArray<Bool> TableExprNodeSetOptIntUSet::contains (const TableExprId&,
                                                     const MArray<Int64>& value)
  {
    Array<Bool> result(value.shape());
    Bool deleteIn, deleteOut;
    const Int64* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    for (size_t i=0; i<value.size(); ++i) {
      out[i] = (itsSet.find(in[i]) != itsSet.end());
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
  }


  TableExprNodeSetOptStringUSet::TableExprNodeSetOptStringUSet (const TableExprNodeRep& orig,
                                                                const Array<String>& arr)
    : TableExprNodeRep (orig)
  {
    itsSet.clear();
    itsSet.insert(arr.begin(), arr.end());
  }
  
  void TableExprNodeSetOptStringUSet::show (ostream& os, uInt indent) const
  {
    TableExprNodeRep::show (os, indent);
    os << "String set as std::unordered_set<String>" << endl;
  }
  
  Bool TableExprNodeSetOptStringUSet::contains (const TableExprId&,
                                                String value)
  {
    return itsSet.find(value) != itsSet.end();
  }

  MArray<Bool> TableExprNodeSetOptStringUSet::contains (const TableExprId&,
                                                        const MArray<String>& value)
  {
    Array<Bool> result(value.shape());
    Bool deleteIn, deleteOut;
    const String* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    for (size_t i=0; i<value.size(); ++i) {
      out[i] = (itsSet.find(in[i]) != itsSet.end());
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
  }


  template<typename T>
  TableExprNodeSetOptContSet<T>::TableExprNodeSetOptContSet
  (const TableExprNodeSet& orig,
   const std::vector<T>& starts,
   const std::vector<T>& ends,
   const std::vector<Bool>& leftC,
   const std::vector<Bool>& rightC)
    : TableExprNodeRep (orig),
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
    : TableExprNodeRep (orig),
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
  Bool TableExprNodeSetOptContSet<T>::contains (const TableExprId&,
                                                T value)
  {
    for (size_t i=0; i<itsStarts.size(); ++i) {
      if ((value > itsStarts[i]  &&  value < itsEnds[i])  ||
          (value == itsStarts[i]  &&  itsLeftC[i])  ||
          (value == itsEnds[i]  &&  itsRightC[i])) {
        return True;
      }
    }
    return False;
  }

  template<typename T>
  MArray<Bool> TableExprNodeSetOptContSet<T>::contains (const TableExprId& id,
                                                        const MArray<T>& value)
  {
    Array<Bool> result(value.shape());
    Bool deleteIn, deleteOut;
    const T* in = value.array().getStorage (deleteIn);
    Bool* out = result.getStorage (deleteOut);
    for (size_t i=0; i<value.size(); ++i) {
      out[i] = contains(id, in[i]);
    }
    value.array().freeStorage (in, deleteIn);
    result.putStorage (out, deleteOut);
    return MArray<Bool> (result, value.mask());
  }

  template<typename T>
  TENShPtr TableExprNodeSetOptContSet<T>::transform (const TableExprNodeSet& set)
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
        stvals[i] = set[i]->getStart(id);
      }
      if (set[i]->end()) {
        endvals[i] = set[i]->getEnd(id);
      }
    }
    // Sort the start values indirectly.
    Vector<Int64> index;
    GenSortIndirect<T,Int64>::sort (index, stvals, stvals.size());
    std::vector<T> newStart;
    std::vector<T> newEnd;
    std::vector<Bool> newLeftC;
    std::vector<Bool> newRightC;
    // Get the start and end value of first interval in sorted list.
    T stval  = stvals[index[0]];
    T endval = endvals[index[0]];
    Bool leftC  = set[index[0]]->isLeftClosed();
    Bool rightC = set[index[0]]->isRightClosed();
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
           (set[index[i]]->isLeftClosed() || set[index[i-1]]->isRightClosed()))) {
        // Overlap; update end if higher.
        if (end2 > endval) {
          endval = end2;
          rightC = set[index[i]]->isRightClosed();
        }
      } else {
        // No overlap, so create the interval found and start a new one.
        newStart.push_back (stval);
        newEnd.push_back (endval);
        newLeftC.push_back (leftC);
        newRightC.push_back (rightC);
        stval  = st2;
        endval = end2;
        leftC  = set[index[i]]->isLeftClosed();
        rightC = set[index[i]]->isRightClosed();
      }
    }
    // Create the last interval.
    newStart.push_back (stval);
    newEnd.push_back (endval);
    newLeftC.push_back (leftC);
    newRightC.push_back (rightC);
    // See if all intervals have the same left and right closedness.
    // If so, a better version can be used that does not need to test on it
    // for each compare.
    Bool same = True;
    for (size_t i=1; i<newLeftC.size(); ++i) {
      if (newLeftC[i] != newLeftC[0]  ||  newRightC[i] != newRightC[0]) {
        same = False;
        break;
      }
    }
    // Now create the correct object.
    TENShPtr ptr;
    if (same) {
      if (newLeftC[0]) {
        if (newRightC[0]) {
          ptr.reset (new TableExprNodeSetOptContSetCC<T> (set, newStart, newEnd));
        } else {
          ptr.reset (new TableExprNodeSetOptContSetCO<T> (set, newStart, newEnd));
        }
      } else {
        if (newRightC[0]) {
          ptr.reset (new TableExprNodeSetOptContSetOC<T> (set, newStart, newEnd));
        } else {
          ptr.reset (new TableExprNodeSetOptContSetOO<T> (set, newStart, newEnd));
        }
      }
    } else {
      ptr.reset (new TableExprNodeSetOptContSet<T> (set, newStart, newEnd,
                                                    newLeftC, newRightC));
    }
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
  Bool TableExprNodeSetOptContSetCC<T>::contains (const TableExprId&,
                                                  T value)
  {
    // Use lambda function to use <= instead of <.
    auto iter = std::upper_bound (this->itsEnds.begin(), this->itsEnds.end(), value,
                                  [](T v1, T v2){return v1<=v2;});
    if (iter != this->itsEnds.end()) {
      size_t index = std::distance (this->itsEnds.begin(), iter);
      return (value >= this->itsStarts[index]);
    }
    return False;
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
  Bool TableExprNodeSetOptContSetCO<T>::contains (const TableExprId&,
                                                  T value)
  {
    auto iter = std::upper_bound (this->itsEnds.begin(), this->itsEnds.end(), value);
    if (iter != this->itsEnds.end()) {
      size_t index = std::distance (this->itsEnds.begin(), iter);
      return (value >= this->itsStarts[index]);
    }
    return False;
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
  Bool TableExprNodeSetOptContSetOC<T>::contains (const TableExprId&,
                                                  T value)
  {
    // Use lambda function to use <= instead of <.
    auto iter = std::upper_bound (this->itsEnds.begin(), this->itsEnds.end(), value,
                                  [](T v1, T v2){return v1<=v2;});
    if (iter != this->itsEnds.end()) {
      size_t index = std::distance (this->itsEnds.begin(), iter);
      return (value > this->itsStarts[index]);
    }
    return False;
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
  Bool TableExprNodeSetOptContSetOO<T>::contains (const TableExprId&,
                                                  T value)
  {
    auto iter = std::upper_bound (this->itsEnds.begin(), this->itsEnds.end(), value);
    if (iter != this->itsEnds.end()) {
      size_t index = std::distance (this->itsEnds.begin(), iter);
      return (value > this->itsStarts[index]);
    }
    return False;
  }


  // Instantiate for Double and String.
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
