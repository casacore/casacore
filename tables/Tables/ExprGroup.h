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
#include <casa/BasicSL/String.h>
#include <tables/Tables/ExprAggrNode.h>
#include <vector>


namespace casa { //# NAMESPACE CASA - BEGIN

// <summary>
// Envelope class for a node in the raw TaQL parse tree.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto group=TableGram.h#TableGramFunctions>TableGram</linkto>
//   <li> Note 199 describing
//        <a href="../notes/199.html">
//        TaQL</a>
// </prerequisite>

// <synopsis>
// The result of parsing a TaQL command is stored in TaQLNode objects.
// Each part of the command can have its own specialized
// <linkto class=TaQLNodeRep>TaQLNodeRep</linkto> object, which forms
// the letter in the TaQLNode envelope.
// <br>The actual scanning/parsing of the command is done using flex/bison
// as defined in the TableGram files.
// </synopsis> 

// <motivation>
// The letter-envelope idiom (counted pointer) makes if much easier
// to keep track of memory, especially in the case of exceptions.
// </motivation>

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
    bool operator== (const TableExprGroupKey&) const;
    bool operator<  (const TableExprGroupKey&) const;

  private:
    TableExprNodeRep::NodeDataType itsDT;
    Bool   itsBool;
    Int64  itsInt64;
    Double itsDouble;
    String itsString;
  };


// <summary>
// Envelope class for a node containing a constant value.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <synopsis>
// This is a specialization of the envelope class
// <linkto class=TaQLNode>TaQLNode</linkto> for a node containing
// a constant value.
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


  // The abstract base class for classes holding the intermediate results
  // for an aggregate function.
  // One object per keyset is used.
  class TableExprGroupFunc
  {
  public:
    TableExprGroupFunc()
    {}
    virtual ~TableExprGroupFunc();
    virtual void apply (TableExprAggrNode& node, const TableExprId& id) = 0;
    virtual void finish();
    virtual Bool getBool();
    virtual Int64 getInt();
    virtual Double getDouble();
    virtual DComplex getDComplex();
    virtual MVTime getDate();
    virtual String getString();
  private:
    // Copying is not needed, thus not allowed.
    TableExprGroupFunc (const TableExprGroupFunc&);
    TableExprGroupFunc& operator= (const TableExprGroupFunc&);
  };

  // The abstract base class for bool aggregate functions.
  class TableExprGroupFuncBool: public TableExprGroupFunc
  {
  public:
    explicit TableExprGroupFuncBool (Bool initValue)
      : itsValue (initValue)
    {}
    virtual ~TableExprGroupFuncBool();
    virtual Bool getBool();
  protected:
    Bool itsValue;
  };

  // The abstract base class for integer aggregate functions.
  class TableExprGroupFuncInt: public TableExprGroupFunc
  {
  public:
    explicit TableExprGroupFuncInt (Int64 initValue=0)
      : itsValue (initValue)
    {}
    virtual ~TableExprGroupFuncInt();
    virtual Int64  getInt();
    virtual Double getDouble();
  protected:
    Int64 itsValue;
  };

  // The abstract base class for double aggregate functions.
  class TableExprGroupFuncDouble: public TableExprGroupFunc
  {
  public:
    explicit TableExprGroupFuncDouble (Double initValue = 0)
      : itsValue (initValue)
    {}
    virtual ~TableExprGroupFuncDouble();
    virtual Double getDouble();
  protected:
    Double itsValue;
  };

  // The abstract base class for dcomplex aggregate functions.
  class TableExprGroupFuncDComplex: public TableExprGroupFunc
  {
  public:
    explicit TableExprGroupFuncDComplex (const DComplex& initValue = DComplex())
      : itsValue (initValue)
    {}
    virtual ~TableExprGroupFuncDComplex();
    virtual DComplex getDComplex();
  protected:
    DComplex itsValue;
  };

  // The abstract base class for string aggregate functions.
  class TableExprGroupFuncString: public TableExprGroupFunc
  {
  public:
    explicit TableExprGroupFuncString (const String& initValue = String())
      : itsValue (initValue)
    {}
    virtual ~TableExprGroupFuncString();
    virtual String getString();
  protected:
    String itsValue;
  };


  // This class contains the set of aggregate function objects which contain
  // the result of a particular GROUPBY group.
  // It also contains the row number of a row containing the non-aggregate
  // values for that group.
  class TableExprGroupFuncSet
  {
  public:
    TableExprGroupFuncSet()
      : itsId (0)
    {}

    // Let the aggregate node objects construct the function set.
    TableExprGroupFuncSet (const vector<TableExprAggrNode*>& aggrNodes);

    // Add a function object.
    void add (const CountedPtr<TableExprGroupFunc>& func)
      { itsFuncs.push_back (func); }

    // Apply the functions to the given row.
    void apply (const vector<TableExprAggrNode*>& nodes, const TableExprId& id);

    // Get the vector of functions.
    const vector<CountedPtr<TableExprGroupFunc> >& getFuncs() const
      { return itsFuncs; }

    // Get the TableExprId.
    const TableExprId& getId() const
      { return itsId; }

  private:
    // Copying is not needed, thus not allowed.
    TableExprGroupFuncSet (const TableExprGroupFuncSet&);
    TableExprGroupFuncSet& operator= (const TableExprGroupFuncSet&);

    vector<CountedPtr<TableExprGroupFunc> > itsFuncs;
    TableExprId itsId;      //# row containing the non-aggregate variables
  };


} //# NAMESPACE CASA - END

#endif
