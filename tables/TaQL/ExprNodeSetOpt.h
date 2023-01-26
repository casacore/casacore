//# ExprNodeSetOpt.h: Classes representing an optimized set in table select expression
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

#ifndef TABLES_EXPRNODESETOPT_H
#define TABLES_EXPRNODESETOPT_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNodeRep.h>
#include <unordered_map>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // Forward Declarations
  class TableExprNodeSet;

  
  // <summary>
  // Abstract base class for optimized set representations
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="Mordante" date="2022/11/08" tests="tExprNodeSetOpt.cc">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> TableExprNodeSet
  // </prerequisite>

  // <synopsis>
  // This class is the abstract base class for the optimized representation of
  // constant value or interval sets used by the IN operator or join operator.
  //
  // The <src>find</src> function can operate on integer, double and string values.
  // Note that datetimes are handled as doubles. It returns the index of the
  // value or interval matching the value searched for.
  // </synopsis>

  class TableExprNodeSetOptBase : public TableExprNodeRep
  {
  public:
    explicit TableExprNodeSetOptBase (const TableExprNodeRep& orig);
    // Does the set contain the given value?
    // They call the <src>find</src> function.
    // <group>
    bool contains (const TableExprId& id, int64_t value) override;
    bool contains (const TableExprId& id, double value) override;
    bool contains (const TableExprId& id, String value) override;
    // </group>
    // Tell for each array value if the set contains that value.
    // It calls the scalar <src>contains</src> function for each value.
    // <group>
    MArray<bool> contains (const TableExprId& id,
                           const MArray<int64_t>& value) override;
    MArray<bool> contains (const TableExprId& id,
                           const MArray<double>& value) override;
    MArray<bool> contains (const TableExprId& id,
                           const MArray<String>& value) override;
    // </group>
    // Tell which key matches a value. -1 = no match.
    // The default implementations throw a 'not implemented' exception.
    //# The String version is passed by value to use the same mechanism
    //# as used for the other types to make templates possible.
    // <group>
    virtual int64_t find (int64_t value) const;
    virtual int64_t find (double value) const;
    virtual int64_t find (String value) const;
    // </group>
  };


  // <summary>
  // An optimized representation of a discrete selection set.
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="Mordante" date="2022/11/08" tests="tExprNodeSetOpt.cc">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> TableExprNodeSet
  // </prerequisite>

  // <synopsis>
  // This templated class is an optimized representation of an constant
  // integer or string array set used by the IN operator.
  // If applicable, TableExprLogicNode instantiates an object of this class.
  // <br>The representation is a std::unordered_map containing the array values
  // and the index in the array.
  // <br>Note that a std::unordered_map is used instead of std::map because its
  // hashing mechanism makes it faster.
  // </synopsis>

  template <typename T>
  class TableExprNodeSetOptUSet: public TableExprNodeSetOptBase
  {
  public:
    // Construct an empty set.
    TableExprNodeSetOptUSet (const TableExprNodeRep& orig, const Array<T>&);

    // Show the node.
    void show (ostream& os, uint32_t indent) const override;

    // Where does a value occur in the set? -1 is no match.
    int64_t find (T value) const override;

  private:
    std::unordered_map<T,int64_t> itsMap;
  };


  // <summary>
  // An optimized representation of a selection set with continuous intervals.
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="Mordante" date="2022/11/08" tests="tExprNodeSetOpt.cc">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> TableExprNodeSet
  // </prerequisite>

  // <synopsis>
  // This class is the base class for the optimized representations of a
  // constant selection set with continuous intervals.
  // It holds the start and end values of the intervals.
  // </synopsis>

  template<typename T>
  class TableExprNodeSetOptContSetBase: public TableExprNodeSetOptBase
  {
  public:
    // Construct from the original set and the start and end values of the
    // intervals. The vectors must have the same length.
    explicit TableExprNodeSetOptContSetBase (const TableExprNodeSet& orig,
                                             const std::vector<T>& starts,
                                             const std::vector<T>& ends);
    // Get the size (nr of intervals).
    size_t size() const
      { return itsStarts.size(); }
    // Show the node.
    void show (ostream& os, uint32_t indent) const override;
    // Transform a set into an optimized one by ordering the intervals
    // and optionally combining adjacent intervals.
    // If not possible, an empty TENShPtr is returned.
    static TENShPtr transform (const TableExprNodeSet& set,
                               bool combine=true);
    // Create the appropriate optimized OptContSet object.
    // Note that leftC and rightC do not need to have the same length as start/end.
    // If it is known that all intervals have the same leftC/rightC,
    // a single value suffices.
    static TENShPtr createOptSet (const TableExprNodeSet& set,
                                  const std::vector<T>& start,
                                  const std::vector<T>& end, 
                                  const std::vector<bool>& leftC,
                                  const std::vector<bool>& rightC);
  protected:
    std::vector<T> itsStarts;
    std::vector<T> itsEnds;
  };


  // <summary>
  // An optimized representation of a selection set with continuous intervals.
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="Mordante" date="2022/11/08" tests="tExprNodeSetOpt.cc">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> TableExprNodeSet
  // </prerequisite>

  // <synopsis>
  // This class is an optimized representation of a constant selection set
  // with continuous intervals using a mix of open and closed start and end.
  // If applicable, TableExprLogicNode instantiates an object of this class.
  // <br>The representation has std::vector objects containing the start
  // and end values. A lookup using std::upper_bound on the end values is done
  // to determine if a value is contained in one of the intervals.
  // <br>This templated class is instantiated for double and String.
  // </synopsis>

  template <typename T>
  class TableExprNodeSetOptContSetMixOC: public TableExprNodeSetOptContSetBase<T>
  {
  public:
    TableExprNodeSetOptContSetMixOC (const TableExprNodeSet& orig,
                                     const std::vector<T>& starts,
                                     const std::vector<T>& ends,
                                     const std::vector<bool>& leftC,
                                     const std::vector<bool>& rightC);
    // Show the node.
    void show (ostream& os, uint32_t indent) const override;
    // Tell which interval contains a value. -1 = no match.
    int64_t find (T value) const override;
  protected:
    std::vector<bool> itsLeftC;
    std::vector<bool> itsRightC;
  };


  // <summary>
  // An optimized representation of a selection set with similar intervals.
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="Mordante" date="2022/11/08" tests="tExprNodeSetOpt.cc">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> TableExprNodeSetOptContSet
  // </prerequisite>

  // <synopsis>
  // This class is a further optimized version of TableExprNodeSetOptContSet
  // for continuous intervals all using the same open/closed interval types.
  // It reduces the number of comparisons required.
  // The left and right comparison functors tell if an interval side is
  // open (uses std::less) or closed (uses std::less_equal).
  // </synopsis>

  template <typename T, typename LeftComp, typename RightComp>
  class TableExprNodeSetOptContSet: public TableExprNodeSetOptContSetBase<T>
  {
  public:
    TableExprNodeSetOptContSet (const TableExprNodeSet& orig,
                                const std::vector<T>& starts,
                                const std::vector<T>& ends,
                                LeftComp leftCmp, RightComp rightCmp,
                                const String& cmpType);
    // Show the node.
    void show (ostream& os, uint32_t indent) const override;
    // Tell which interval contains a value. -1 = no match.
    int64_t find (T value) const override;
  private:
    LeftComp  itsLeftCmp;
    RightComp itsRightCmp;
    String    itsCmpType;
  };


} //# NAMESPACE CASACORE - END

#endif
