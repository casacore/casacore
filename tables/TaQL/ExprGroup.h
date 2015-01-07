//# ExprGroup.h: Classes handling TaQL's GROUPBY functionality
//# Copyright (C) 2013
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
//# $Id: TaQLNode.h 21051 2011-04-20 11:46:29Z gervandiepen $

#ifndef TABLES_EXPRGROUP_H
#define TABLES_EXPRGROUP_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/tables/TaQL/ExprAggrNode.h>
#include <vector>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary>
  // Class representing a key in the groupby clause.
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tTableGram">
  // </reviewed>
  // <synopsis>
  // The GROUPBY clause consists of one or more keys, each being a scalar
  // TaQL expression with an arbitrary data type.
  // This class contains the value of a key for a particular table row.
  // It is part of a TableExprGroupKeySet object.
  // </synopsis> 
  class TableExprGroupKey
  {
  public:
    // Construct for a given data type.
    explicit TableExprGroupKey (TableExprNodeRep::NodeDataType dtype)
      : itsDT (dtype)
    {}

    // Get the data type.
    TableExprNodeRep::NodeDataType dataType() const
      { return itsDT; }
    
    // Set the key's value.
    // <group>
    void set (Bool v)
      { itsBool = v; }
    void set (Int64 v)
      { itsInt64 = v; }
    void set (Double v)
      { itsDouble = v; }
    void set (const String& v)
      { itsString = v; }
    // </group>

    // Compare this and that key.
    // <group>
    bool operator== (const TableExprGroupKey&) const;
    bool operator<  (const TableExprGroupKey&) const;
    // </group>

  private:
    TableExprNodeRep::NodeDataType itsDT;
    Bool   itsBool;
    Int64  itsInt64;
    Double itsDouble;
    String itsString;
  };


  // <summary>
  // Class representing all keys in the groupby clause.
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tTableGram">
  // </reviewed>
  // <synopsis>
  // The GROUPBY clause consists of one or more keys, each being a scalar
  // TaQL expression with an arbitrary data type.
  // This class contains a set of TableExprGroupKey objects, each containing
  // the value of a key for a particular table row.
  // <br>It contains comparison functions to make it possible to use them
  // in a std::map object to map the groupby keyset to a group.
  // </synopsis> 
  class TableExprGroupKeySet
  {
  public:
    // Form the object from the given groupby nodes.
    TableExprGroupKeySet (const vector<TableExprNode>& nodes);

    // Add a key to end the set.
    void addKey (TableExprNodeRep::NodeDataType dtype)
      { itsKeys.push_back (TableExprGroupKey(dtype)); }

    // Fill the keys with the values from the nodes for this rowid.
    void fill (const vector<TableExprNode>& nodes, const TableExprId& id);

    // Compare all keys in the set.
    // The keyset is compared in order of key, thus the first key defines
    // the major ordering.
    bool operator== (const TableExprGroupKeySet&) const;
    bool operator<  (const TableExprGroupKeySet&) const;

  private:
    vector<TableExprGroupKey> itsKeys;
  };


  // <summary>
  // Class holding the results of groupby and aggregation
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tTableGram">
  // </reviewed>
  // <synopsis>
  // The SELECT (and HAVING) clause can contain aggregate functions
  // of which the results can be grouped using the GROUPBY clause.
  // This class holds the results of the (immediate) aggregate functions
  // and, if needed, the TableExprId ids of all rows belonging to each group.
  // These ids are used to evaluate the lazy aggregate functions.
  // <br>An object of this class is part of the TableExprIdAggr object
  // used to get the aggregated values of each group.
  // </synopsis> 
  class TableExprGroupResult
  {
  public:
    // Create from the possible set of immediate aggregate functions.
    // No immediate functions were used, thus no TableExprIds needed.
    explicit TableExprGroupResult
    (const vector<CountedPtr<TableExprGroupFuncSet> >& funcSets);
    // Create from the possible set of immediate aggregate functions
    // and the set of TableExprIds per group for lazy aggregate functions.
    TableExprGroupResult
    (const vector<CountedPtr<TableExprGroupFuncSet> >& funcSets,
     const vector<CountedPtr<vector<TableExprId> > >& ids);
    // Get the nr of groups.
    uInt ngroup() const
      { return itsFuncSets.size(); }
    // Get the set of functions (and their results) for the given group.
    TableExprGroupFuncSet& funcSet (uInt group) const
      { return *itsFuncSets[group]; }
    // Get the set of TableExprIds for the given group.
    const vector<TableExprId>& ids (uInt group) const
      { return *itsIds[group]; }
  private:
    vector<CountedPtr<TableExprGroupFuncSet> > itsFuncSets;
    vector<CountedPtr<vector<TableExprId> > >  itsIds;
  };


  // <summary>
  // Abstract base class for classes calculating an aggregated group result.
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // The GROUPBY clause divides a table into groups for which aggregated
  // results can be calculated like the mean or minimum. These results are
  // calculated in classes derived from this abstract base class.
  // <br>There is one such function object per aggregation per group. All
  // aggregation objects of a group are combined in a std::vector.
  // This vector is mapped to a TableExprGroupKeySet object to keep track
  // of all groups and aggregations.
  // <br> There are two types of aggregation function classes.
  // <ul>
  //  <li> Immediate classes implement the 'apply' function to immediately
  //       apply the operand's value in the aggregation.
  //       Such classes do not keep the operand's values.
  //  <li> Lazy classes do not need the 'apply' function. Instead they
  //       read all values of the group in the 'getXXX' function and do the
  //       aggregation. Such classes are meant for aggregation functions
  //       like 'median' that need to keep all values. When applying it
  //       immediately, all groups need to keep their values which might need
  //       too much memory. Lazy classes need the values of only one group
  //       at a time, but have the disadvantage that reading the values from
  //       the table might be done in a non-sequential order.
  // </ul>
  // Most derived classes are immediate classes.
  // </synopsis> 
  class TableExprGroupFuncBase
  {
  public:
    // Construct from the TaQL aggregation node. It keeps the operand
    // of the aggregation node.
    explicit TableExprGroupFuncBase (TableExprNodeRep* node);
    virtual ~TableExprGroupFuncBase();
    // Does the aggregate function use lazy semantics?
    // The default implementation returns False.
    virtual Bool isLazy() const;
    // Get the function's sequence nr.
    uInt seqnr() const
      { return itsSeqnr; }
    // Set the function's sequence nr.
    void setSeqnr (uInt seqnr)
      { itsSeqnr = seqnr; }
    // Get the operand's value for the given row and apply it to the aggregation.
    // This function should not be called for lazy classes.
    virtual void apply (const TableExprId& id) = 0;
    // If needed, finish the aggregation.
    // By default nothing is done.
    virtual void finish();
    // Get the assembled TableExprIds of a group. It is specifically meant
    // for TableExprGroupExprId used for lazy aggregation.
    virtual CountedPtr<vector<TableExprId> > getIds() const;
    // Get the aggregated value.
    // Immediate classes can return the already calculated value, while
    // lazy classes will get the values of all rows given by the TableExprIds
    // and do the aggregation.
    // <group>
    virtual Bool getBool (const vector<TableExprId>& = vector<TableExprId>());
    virtual Int64 getInt (const vector<TableExprId>& = vector<TableExprId>());
    virtual Double getDouble (const vector<TableExprId>& = vector<TableExprId>());
    virtual DComplex getDComplex (const vector<TableExprId>& = vector<TableExprId>());
    virtual MVTime getDate (const vector<TableExprId>& = vector<TableExprId>());
    virtual String getString (const vector<TableExprId>& = vector<TableExprId>());
    virtual Array<Bool> getArrayBool (const vector<TableExprId>& = vector<TableExprId>());
    virtual Array<Int64> getArrayInt (const vector<TableExprId>& = vector<TableExprId>());
    virtual Array<Double> getArrayDouble (const vector<TableExprId>& = vector<TableExprId>());
    virtual Array<DComplex> getArrayDComplex (const vector<TableExprId>& = vector<TableExprId>());
    virtual Array<MVTime> getArrayDate (const vector<TableExprId>& = vector<TableExprId>());
    virtual Array<String> getArrayString (const vector<TableExprId>& = vector<TableExprId>());
    // <group>
  private:
    // Copying is not needed, thus not allowed.
    TableExprGroupFuncBase (const TableExprGroupFuncBase&);
    TableExprGroupFuncBase& operator= (const TableExprGroupFuncBase&);
  protected:
    //# Data member
    TableExprNodeRep* itsNode;
    TableExprNodeRep* itsOperand;
    uInt              itsSeqnr;
  };


  // <summary>
  // Class derived from TableExprGroupFuncBase representing a no function
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // This class represents a null aggregate function which is meant for
  // possible aggregate functionality in UDFs.
  // </synopsis>
  class TableExprGroupNull: public TableExprGroupFuncBase
  {
  public:
    explicit TableExprGroupNull (TableExprNodeRep* node);
    virtual ~TableExprGroupNull();
    virtual Bool isLazy() const;
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Class derived from TableExprGroupFuncBase for the first value in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // This class keeps the TableExprId of the first value in a group.
  // The 'getXXX' functions get the value for that TableExprId.
  // </synopsis>
  class TableExprGroupFirst: public TableExprGroupFuncBase
  {
  public:
    explicit TableExprGroupFirst (TableExprNodeRep* node);
    virtual ~TableExprGroupFirst();
    virtual void apply (const TableExprId& id);
    virtual Bool getBool (const vector<TableExprId>&);
    virtual Int64 getInt (const vector<TableExprId>&);
    virtual Double getDouble (const vector<TableExprId>&);
    virtual DComplex getDComplex (const vector<TableExprId>&);
    virtual MVTime getDate (const vector<TableExprId>&);
    virtual String getString (const vector<TableExprId>&);
    virtual Array<Bool> getArrayBool (const vector<TableExprId>&);
    virtual Array<Int64> getArrayInt (const vector<TableExprId>&);
    virtual Array<Double> getArrayDouble (const vector<TableExprId>&);
    virtual Array<DComplex> getArrayDComplex (const vector<TableExprId>&);
    virtual Array<MVTime> getArrayDate (const vector<TableExprId>&);
    virtual Array<String> getArrayString (const vector<TableExprId>&);
  protected:
    TableExprId itsId;
  };

  // <summary>
  // Class derived from TableExprGroupFuncBase for the first value in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // This class keeps the TableExprId of the last value in a group.
  // The 'getXXX' functions get the value for that TableExprId.
  // <br>For ease of use this class is derived from TableExprGroupFirst.
  // </synopsis>
  class TableExprGroupLast: public TableExprGroupFirst
  {
  public:
    explicit TableExprGroupLast (TableExprNodeRep* node);
    virtual ~TableExprGroupLast();
    virtual void apply (const TableExprId& id);
  };

  // <summary>
  // Class derived from TableExprGroupFuncBase collecting the ids in a group
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // This class keeps all TableExprIds in a group.
  // It is meant for lazy aggregation classes which use the collected
  // TableExprIds in their 'getXXX' functions.
  // </synopsis>
  class TableExprGroupExprId: public TableExprGroupFuncBase
  {
  public:
    explicit TableExprGroupExprId (TableExprNodeRep* node);
    virtual ~TableExprGroupExprId();
    virtual Bool isLazy() const;
    virtual void apply (const TableExprId& id);
    virtual CountedPtr<vector<TableExprId> > getIds() const;
  private:
    CountedPtr<vector<TableExprId> > itsIds;
  };

  // <summary>
  // Class collecting the rowids of entries in a group.
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // This class collects the row numbers of the rows in a group.
  // </synopsis>
  class TableExprGroupRowid: public TableExprGroupFuncBase
  {
  public:
    explicit TableExprGroupRowid (TableExprNodeRep* node);
    virtual ~TableExprGroupRowid();
    virtual Bool isLazy() const;
    virtual void apply (const TableExprId& id);
    virtual Array<Int64> getArrayInt (const vector<TableExprId>&);
  };

  // <summary>
  // Class collecting the arrays in a group.
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // This class collects the non-empty arrays in a group into an array with
  // one more axis. All arrays (if not empty) must have the same shape.
  // </synopsis>
  class TableExprGroupAggr: public TableExprGroupFuncBase
  {
  public:
    explicit TableExprGroupAggr (TableExprNodeRep* node);
    virtual ~TableExprGroupAggr();
    virtual Bool isLazy() const;
    virtual void apply (const TableExprId& id);
    virtual Array<Bool> getArrayBool (const vector<TableExprId>&);
    virtual Array<Int64> getArrayInt (const vector<TableExprId>&);
    virtual Array<Double> getArrayDouble (const vector<TableExprId>&);
    virtual Array<DComplex> getArrayDComplex (const vector<TableExprId>&);
    virtual Array<MVTime> getArrayDate (const vector<TableExprId>&);
    virtual Array<String> getArrayString (const vector<TableExprId>&);
  protected:
    template<typename T>
    Array<T> getArray (const vector<TableExprId>& ids)
    {
      // Return scalar values as a Vector.
      if (itsOperand->valueType() == TableExprNodeRep::VTScalar) {
        Vector<T> result(ids.size());
        for (size_t i=0; i<ids.size(); ++i) {
          itsOperand->get (ids[i], result[i]);
        }
        return result;
      }
      // Array values are returned as an array with one more axis.
      // Get the first value to determine the shape.
      Array<T> arr;
      itsOperand->get (ids[0], arr);
      IPosition shp = arr.shape();
      shp.append (IPosition (1, ids.size()));
      Array<T> result(shp);
      ArrayIterator<T> iter (result, arr.ndim());
      iter.array() = arr;
      iter.next();
      int i=1;
      while (! iter.pastEnd()) {
        itsOperand->get (ids[i], iter.array());
        iter.next();
        i++;
      }
      return result;
    }
  };


  // <summary>
  // Abstract base class for aggregate functions giving a bool scalar.
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // This class is derived from TableExprGroupFuncBase and act as the
  // abstract base class for aggregate functions resulting in a bool scalar.
  // <br>Derived classes can use <src>itsValue</src> to contain the
  // aggregated value. It that case they do not need to implement the
  // <src>get</src> function.
  // </synopsis>
  class TableExprGroupFuncBool: public TableExprGroupFuncBase
  {
  public:
    explicit TableExprGroupFuncBool (TableExprNodeRep* node)
      : TableExprGroupFuncBase (node)
    {}
    TableExprGroupFuncBool (TableExprNodeRep* node, Bool initValue)
      : TableExprGroupFuncBase (node),
        itsValue (initValue)
    {}
    virtual ~TableExprGroupFuncBool();
    virtual Bool getBool (const vector<TableExprId>&);
  protected:
    Bool itsValue;
  };

  // <summary>
  // Abstract base class for aggregate functions giving an integer scalar.
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // This class is derived from TableExprGroupFuncBase and act as the
  // abstract base class for aggregate functions resulting in an integer scalar.
  // <br>Derived classes can use <src>itsValue</src> to contain the
  // aggregated value. It that case they do not need to implement the
  // <src>get</src> function.
  // </synopsis>
  class TableExprGroupFuncInt: public TableExprGroupFuncBase
  {
  public:
    explicit TableExprGroupFuncInt (TableExprNodeRep* node, Int64 initValue=0)
      : TableExprGroupFuncBase (node),
        itsValue (initValue)
    {}
    virtual ~TableExprGroupFuncInt();
    virtual Int64  getInt (const vector<TableExprId>&);
    virtual Double getDouble (const vector<TableExprId>&);
  protected:
    Int64 itsValue;
  };

  // <summary>
  // Abstract base class for aggregate functions giving a double scalar.
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // This class is derived from TableExprGroupFuncBase and act as the
  // abstract base class for aggregate functions resulting in a double scalar.
  // <br>Derived classes can use <src>itsValue</src> to contain the
  // aggregated value. It that case they do not need to implement the
  // <src>get</src> function.
  // </synopsis>
  class TableExprGroupFuncDouble: public TableExprGroupFuncBase
  {
  public:
    explicit TableExprGroupFuncDouble (TableExprNodeRep* node,
                                       Double initValue = 0)
      : TableExprGroupFuncBase (node),
        itsValue (initValue)
    {}
    virtual ~TableExprGroupFuncDouble();
    virtual Double getDouble (const vector<TableExprId>&);
  protected:
    Double itsValue;
  };

  // <summary>
  // Abstract base class for aggregate functions giving a dcomplex scalar.
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // This class is derived from TableExprGroupFuncBase and act as the
  // abstract base class for aggregate functions resulting in a dcomplex scalar.
  // <br>Derived classes can use <src>itsValue</src> to contain the
  // aggregated value. It that case they do not need to implement the
  // <src>get</src> function.
  // </synopsis>
  class TableExprGroupFuncDComplex: public TableExprGroupFuncBase
  {
  public:
    explicit TableExprGroupFuncDComplex (TableExprNodeRep* node,
                                         const DComplex& initValue = DComplex())
      : TableExprGroupFuncBase (node),
        itsValue (initValue)
    {}
    virtual ~TableExprGroupFuncDComplex();
    virtual DComplex getDComplex (const vector<TableExprId>&);
  protected:
    DComplex itsValue;
  };

  // <summary>
  // Abstract base class for aggregate functions giving a date/time scalar.
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // This class is derived from TableExprGroupFuncBase and act as the
  // abstract base class for aggregate functions resulting in a date/time scalar.
  // <br>Derived classes can use <src>itsValue</src> to contain the
  // aggregated value. It that case they do not need to implement the
  // <src>get</src> function.
  // </synopsis>
  class TableExprGroupFuncDate: public TableExprGroupFuncBase
  {
  public:
    explicit TableExprGroupFuncDate (TableExprNodeRep* node,
                                     const MVTime& initValue = MVTime())
      : TableExprGroupFuncBase (node),
        itsValue (initValue)
    {}
    virtual ~TableExprGroupFuncDate();
    virtual MVTime getDate (const vector<TableExprId>&);
  protected:
    MVTime itsValue;
  };

  // <summary>
  // Abstract base class for aggregate functions giving a string scalar.
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // This class is derived from TableExprGroupFuncBase and act as the
  // abstract base class for aggregate functions resulting in a string scalar.
  // <br>Derived classes can use <src>itsValue</src> to contain the
  // aggregated value. It that case they do not need to implement the
  // <src>get</src> function.
  // </synopsis>
  class TableExprGroupFuncString: public TableExprGroupFuncBase
  {
  public:
    explicit TableExprGroupFuncString (TableExprNodeRep* node,
                                       const String& initValue = String())
      : TableExprGroupFuncBase (node),
        itsValue (initValue)
    {}
    virtual ~TableExprGroupFuncString();
    virtual String getString (const vector<TableExprId>&);
  protected:
    String itsValue;
  };

  // <summary>
  // Abstract base class for aggregate functions giving a bool array.
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // This class is derived from TableExprGroupFuncBase and act as the
  // abstract base class for aggregate functions resulting in a bool array.
  // <br>Derived classes can use <src>itsValue</src> to contain the
  // aggregated value. It that case they do not need to implement the
  // <src>get</src> function.
  // </synopsis>
  class TableExprGroupFuncArrayBool: public TableExprGroupFuncBase
  {
  public:
    explicit TableExprGroupFuncArrayBool (TableExprNodeRep* node)
      : TableExprGroupFuncBase (node)
    {}
    virtual ~TableExprGroupFuncArrayBool();
    virtual Array<Bool> getArrayBool (const vector<TableExprId>&);
  protected:
    // If not empty, check if the shape matches that of <src>itsValue</src>.
    // If <src>itsValue</src> is still empty, it is sized.
    Bool checkShape (const ArrayBase& arr, const String& func);
    Array<Bool> itsValue;
  };

  // <summary>
  // Abstract base class for aggregate functions giving an integer array.
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // This class is derived from TableExprGroupFuncBase and act as the
  // abstract base class for aggregate functions resulting in an integer array.
  // <br>Derived classes can use <src>itsValue</src> to contain the
  // aggregated value. It that case they do not need to implement the
  // <src>get</src> function.
  // </synopsis>
  class TableExprGroupFuncArrayInt: public TableExprGroupFuncBase
  {
  public:
    explicit TableExprGroupFuncArrayInt (TableExprNodeRep* node)
      : TableExprGroupFuncBase (node)
    {}
    virtual ~TableExprGroupFuncArrayInt();
    virtual Array<Int64> getArrayInt (const vector<TableExprId>&);
  protected:
    // If not empty, check if the shape matches that of <src>itsValue</src>.
    // If <src>itsValue</src> is still empty, it is sized.
    Bool checkShape (const ArrayBase& arr, const String& func);
    Array<Int64> itsValue;
  };

  // <summary>
  // Abstract base class for aggregate functions giving a double array.
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // This class is derived from TableExprGroupFuncBase and act as the
  // abstract base class for aggregate functions resulting in a double array.
  // <br>Derived classes can use <src>itsValue</src> to contain the
  // aggregated value. It that case they do not need to implement the
  // <src>get</src> function.
  // </synopsis>
  class TableExprGroupFuncArrayDouble: public TableExprGroupFuncBase
  {
  public:
    explicit TableExprGroupFuncArrayDouble (TableExprNodeRep* node)
      : TableExprGroupFuncBase (node)
    {}
    virtual ~TableExprGroupFuncArrayDouble();
    virtual Array<Double> getArrayDouble (const vector<TableExprId>&);
  protected:
    // If not empty, check if the shape matches that of <src>itsValue</src>.
    // If <src>itsValue</src> is still empty, it is sized.
    Bool checkShape (const ArrayBase& arr, const String& func);
    Array<Double> itsValue;
  };

  // <summary>
  // Abstract base class for aggregate functions giving a dcomplex array.
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // This class is derived from TableExprGroupFuncBase and act as the
  // abstract base class for aggregate functions resulting in a dcomplex array.
  // <br>Derived classes can use <src>itsValue</src> to contain the
  // aggregated value. It that case they do not need to implement the
  // <src>get</src> function.
  // </synopsis>
  class TableExprGroupFuncArrayDComplex: public TableExprGroupFuncBase
  {
  public:
    explicit TableExprGroupFuncArrayDComplex (TableExprNodeRep* node)
      : TableExprGroupFuncBase (node)
    {}
    virtual ~TableExprGroupFuncArrayDComplex();
    virtual Array<DComplex> getArrayDComplex (const vector<TableExprId>&);
  protected:
    // If not empty, check if the shape matches that of <src>itsValue</src>.
    // If <src>itsValue</src> is still empty, it is sized.
    Bool checkShape (const ArrayBase& arr, const String& func);
    Array<DComplex> itsValue;
  };

  // <summary>
  // Abstract base class for aggregate functions giving a date/time array.
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // This class is derived from TableExprGroupFuncBase and act as the
  // abstract base class for aggregate functions resulting in a date/time array.
  // <br>Derived classes can use <src>itsValue</src> to contain the
  // aggregated value. It that case they do not need to implement the
  // <src>get</src> function.
  // </synopsis>
  class TableExprGroupFuncArrayDate: public TableExprGroupFuncBase
  {
  public:
    explicit TableExprGroupFuncArrayDate (TableExprNodeRep* node)
      : TableExprGroupFuncBase (node)
    {}
    virtual ~TableExprGroupFuncArrayDate();
    virtual Array<MVTime> getArrayDate (const vector<TableExprId>&);
  protected:
    // If not empty, check if the shape matches that of <src>itsValue</src>.
    // If <src>itsValue</src> is still empty, it is sized.
    Bool checkShape (const ArrayBase& arr, const String& func);
    Array<MVTime> itsValue;
  };

  // <summary>
  // Abstract base class for aggregate functions giving a string array.
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // This class is derived from TableExprGroupFuncBase and act as the
  // abstract base class for aggregate functions resulting in a string array.
  // <br>Derived classes can use <src>itsValue</src> to contain the
  // aggregated value. It that case they do not need to implement the
  // <src>get</src> function.
  // </synopsis>
  class TableExprGroupFuncArrayString: public TableExprGroupFuncBase
  {
  public:
    explicit TableExprGroupFuncArrayString (TableExprNodeRep* node)
      : TableExprGroupFuncBase (node)
    {}
    virtual ~TableExprGroupFuncArrayString();
    virtual Array<String> getArrayString (const vector<TableExprId>&);
  protected:
    // If not empty, check if the shape matches that of <src>itsValue</src>.
    // If <src>itsValue</src> is still empty, it is sized.
    Bool checkShape (const ArrayBase& arr, const String& func);
    Array<String> itsValue;
  };


  // <summary>
  // Class containing the results of aggregated values in a group.
  // </summary>
  // <use visibility=local>
  // <reviewed reviewer="" date="" tests="tExprGroup">
  // </reviewed>
  // <synopsis>
  // This class contains the set of aggregate function objects containing
  // all aggregate results of a particular GROUPBY group.
  // It also contains the TableExprId of the last row in the group.
  // It is used for possible non-aggregate expressions.
  // </synopsis>
  class TableExprGroupFuncSet
  {
  public:
    TableExprGroupFuncSet()
      : itsId (0)
    {}

    // Let the aggregate node objects construct the function set.
    TableExprGroupFuncSet (const vector<TableExprNodeRep*>& aggrNodes);

    // Add a function object.
    void add (const CountedPtr<TableExprGroupFuncBase>& func);

    // Apply the functions to the given row.
    void apply (const TableExprId& id);

    // Get the vector of functions.
    const vector<CountedPtr<TableExprGroupFuncBase> >& getFuncs() const
      { return itsFuncs; }

    // Get the TableExprId.
    const TableExprId& getId() const
      { return itsId; }

  private:
    // Copying is not needed, thus not allowed.
    TableExprGroupFuncSet (const TableExprGroupFuncSet&);
    TableExprGroupFuncSet& operator= (const TableExprGroupFuncSet&);

    //# Data members.
    vector<CountedPtr<TableExprGroupFuncBase> > itsFuncs;
    TableExprId itsId;      //# row containing the non-aggregate variables
  };

} //# NAMESPACE CASACORE - END

#endif
