//# ExprNodeSet.h: Classes representing a set in table select expression
//# Copyright (C) 1997,2000,2001,2002,2003
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef TABLES_EXPRNODESET_H
#define TABLES_EXPRNODESET_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNodeSetElem.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <vector>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableExprNode;
class IPosition;
class Slicer;


// <summary>
// Class to hold multiple table expression nodes.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
//   <li> TableExprNodeBinary
// </prerequisite>

// <synopsis> 
// This class is used to assemble several table expression nodes.
// It is used for 3 purposes:
// <ol>
// <li> To hold the arguments of a function.
//      All set elements must be single.
// <li> To hold the variables of an index for an array slice.
//      All set elements must be of type int scalar and they must
//      represent a discrete interval (which includes single).
// <li> To hold the elements of a set used with the IN operator.
//      All set elements must be scalars of any type.
// </ol>
// The type of all set elements has to be the same.
// The set consists of
// <linkto class=TableExprNodeSetElem>TableExprNodeSetElem</linkto>
// elements. The <src>add</src> function has to be used to
// add an element to the set.
// <p>
// It is possible to construct the object directly from an
// <linkto class=IPosition>IPosition</linkto> object.
// In that case all elements are single.
// Furthermore it is possible to construct it directly from a
// <linkto class=Slicer>Slicer</linkto> object.
// In that case all elements represent a discrete interval.
// </synopsis> 

class TableExprNodeSet : public TableExprNodeRep
{
public:
    // Construct an empty set.
    TableExprNodeSet();

    // Construct from an <src>IPosition</src>.
    // The number of elements in the set is the number of elements
    // in the <src>IPosition</src>. All set elements are single values.
    TableExprNodeSet (const IPosition&);

    // Construct from a <src>Slicer</src>.
    // The number of elements in the set is the dimensionality
    // of the <src>Slicer</src>. All set elements are discrete intervals.
    // Their start and/or end is undefined if it is was not defined
    // (i.e. Slicer::MimicSource used) in the <src>Slicer</src> object.
    TableExprNodeSet (const Slicer&);

    // Construct a set with n*set.size() elements where n is the number
    // of rows.
    // Element i is constructed by evaluating the input element
    // for row rownr[i].
    TableExprNodeSet (const Vector<rownr_t>& rownrs, const TableExprNodeSet&);

    TableExprNodeSet(const TableExprNodeSet&);

    ~TableExprNodeSet();
    
    // A copy of a TableExprNodeSet cannot be assigned.
    TableExprNodeSet& operator= (const TableExprNodeSet&) = delete;

    // Add an element to the set.
    // If adaptType=True, the data type is the highest of the elements added.
    // Otherwise it is that of the first element.
    // True is meant for a set of values, False for function arguments.
    // <br>A constant mid-width interval is added as a normal interval.
    // In this way constant intervals can never be mid-width which makes
    // optimization easier.
    void add (const TENSEBShPtr&, Bool adaptType=False);
    void add (const TableExprNodeSetElem& elem, Bool adaptType=False)
      { add (elem.getElem(), adaptType); }

    // Show the node.
    void show (ostream& os, uInt indent) const override;

    // Flatten the node tree by adding the node and its children to the vector.
    virtual void flattenTree (std::vector<TableExprNodeRep*>&) override;
  
    // Check if the data type of the set elements are the same.
    // If not, an exception is thrown.
    //# Note that if itsCheckTypes is set, the data types are already
    //# known to be equal.
    void checkEqualDataTypes() const;

    // Contains the set only single elements?
    // Single means that only single values are given (thus no end nor incr).
    Bool isSingle() const;

    // Contains the set only discrete elements?
    // Discrete means that no continuous ranges are given, but discrete
    // ranges (using :) are possible.
    Bool isDiscrete() const;

    // Is the set fully bounded (discrete and no undefined end values)?
    Bool isBounded() const;

    // Get the number of elements.
    size_t size() const;
    // For backward compatibility.
    size_t nelements() const {return size();}

    // Get the i-th element.
    const TENSEBShPtr& operator[] (size_t index) const;

    // Contains the set array values?
    Bool hasArrays() const;

    // Try to convert the set to an array.
    // If not possible, a copy of the set is returned.
    TENShPtr setOrArray() const;

    template<typename T>
    MArray<T> toArray (const TableExprId& id) const;

    // Get an array value for this bounded set in the given row.
    // <group>
    MArray<Bool> getArrayBool         (const TableExprId& id) override;
    MArray<Int64> getArrayInt         (const TableExprId& id) override;
    MArray<Double> getArrayDouble     (const TableExprId& id) override;
    MArray<DComplex> getArrayDComplex (const TableExprId& id) override;
    MArray<String> getArrayString     (const TableExprId& id) override;
    MArray<MVTime> getArrayDate       (const TableExprId& id) override;
    // </group>

    // Does a value occur in the set?
    // <group>
    Bool contains (const TableExprId& id, Bool value) override;
    Bool contains (const TableExprId& id, Int64 value) override;
    Bool contains (const TableExprId& id, Double value) override;
    Bool contains (const TableExprId& id, DComplex value) override;
    Bool contains (const TableExprId& id, String value) override;
    Bool contains (const TableExprId& id, MVTime value) override;
    MArray<Bool> contains (const TableExprId& id,
                                   const MArray<Bool>& value) override;
    MArray<Bool> contains (const TableExprId& id,
                                   const MArray<Int64>& value) override;
    MArray<Bool> contains (const TableExprId& id,
                                   const MArray<Double>& value) override;
    MArray<Bool> contains (const TableExprId& id,
                                   const MArray<DComplex>& value) override;
    MArray<Bool> contains (const TableExprId& id,
                                   const MArray<String>& value) override;
    MArray<Bool> contains (const TableExprId& id,
                                   const MArray<MVTime>& value) override;
    // </group>

    // Useful to make overloading clearer (mainly for test programs).
    Bool contains (const TableExprId& id, int value)
      { return contains (id, Int64(value)); }
    Bool contains (const TableExprId& id, const char* value)
      { return contains (id, String(value)); }

    // Let a set node convert itself to the given unit.
    void adaptSetUnits (const Unit&) override;

    // Try to set the set's shape for a bounded set with single elements.
    void setShape();

private:
    // Convert the const set to an array.
    TENShPtr toConstArray() const;

    // Get the array in a templated way.
    // <group>
    void getArray (MArray<Bool>& marr, const TENShPtr& node,
                   const TableExprId& id) const
      { marr.reference (node->getArrayBool (id)); }
    void getArray (MArray<Int64>& marr, const TENShPtr& node,
                   const TableExprId& id) const
      { marr.reference (node->getArrayInt (id)); }
    void getArray (MArray<Double>& marr, const TENShPtr& node,
                   const TableExprId& id) const
      { marr.reference (node->getArrayDouble (id)); }
    void getArray (MArray<DComplex>& marr, const TENShPtr& node,
                   const TableExprId& id) const
      { marr.reference (node->getArrayDComplex (id)); }
    void getArray (MArray<String>& marr, const TENShPtr& node,
                   const TableExprId& id) const
      { marr.reference (node->getArrayString (id)); }
    void getArray (MArray<MVTime>& marr, const TENShPtr& node,
                   const TableExprId& id) const
      { marr.reference (node->getArrayDate (id)); }
    // </group>

    //# Data members
    std::vector<TENSEBShPtr> itsElems;
    Bool itsSingle;
    Bool itsDiscrete;
    Bool itsBounded;       //# Set is discrete and all starts/ends are defined
    Bool itsCheckTypes;    //# True = checking data types is not needed
};


inline Bool TableExprNodeSet::isSingle() const
{
    return itsSingle;
}
inline Bool TableExprNodeSet::isDiscrete() const
{
    return itsDiscrete;
}
inline Bool TableExprNodeSet::isBounded() const
{
    return itsBounded;
}
inline size_t TableExprNodeSet::size() const
{
    return itsElems.size();
}
inline const TENSEBShPtr& TableExprNodeSet::operator[] (size_t index) const
{
    return itsElems[index];
}


template<typename T>
MArray<T> TableExprNodeSet::toArray (const TableExprId& id) const
{
  /// TODO: align possible units
    DebugAssert (itsBounded, AipsError);
    Int64 n = size();
    if (hasArrays()) {
      if (itsElems[0]->start()->valueType() != VTArray) {
        throw TableInvExpr("scalar value cannot be given in a nested array");
      }
      // Handle a nested array; this is done recursively.
      MArray<T> marr;
      getArray (marr, itsElems[0]->start(), id);
      if (marr.isNull()) {
        return marr;
      }
      Array<T> result (marr.array());
      Array<Bool> mask (marr.mask());
      IPosition shp = result.shape();
      uInt naxes = shp.size();
      shp.append (IPosition(1,n));
      IPosition maskShp(shp);
      maskShp[maskShp.size()-1] = 1;
      result.resize (shp, True);
      if (! mask.empty()) {
        mask.resize (shp, True);
      }
      // Iterate through the remaining arrays.
      ArrayIterator<T> iter(result, shp.size()-1);
      IPosition s(shp.size(), 0);
      IPosition e(shp-1);
      e[naxes] = 0;
      for (Int64 i=1; i<n; i++) {
        if (itsElems[i]->start()->valueType() != VTArray) {
          throw TableInvExpr("scalar value cannot be given in a nested array");
        }
        iter.next();
        s[naxes]++;
        e[naxes]++;
        MArray<T> marr;
        getArray (marr, itsElems[i]->start(), id);
        if (marr.isNull()) {
          return marr;
        }
        if (! marr.shape().isEqual (iter.array().shape())) {
          throw TableInvExpr("Shapes of nested arrays do not match");
        }
        iter.array() = marr.array();
        if (marr.hasMask()) {
          if (mask.empty()) {
            // The first time a mask was found, so create the resulting mask.
            mask.resize (shp);
            mask = False;
          }
          mask(s,e) = marr.mask().reform(maskShp);
        } else if (! mask.empty()) {
          // This array has no mask, so set to False in resulting mask.
          mask(s,e) = False;
        }
      }
      return MArray<T>(result, mask);
    } else {
      // Combine scalars.
      Int64 n = size();
      Int64 cnt = 0;
      Vector<T> result (n);
      for (Int64 i=0; i<n; i++) {
        itsElems[i]->fillVector (result, cnt, id);
      }
      result.resize (cnt, True);
      return MArray<T>(result);
    }
}



} //# NAMESPACE CASACORE - END

#endif
