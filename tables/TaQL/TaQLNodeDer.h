//# TaQLNodeDer.h: Specialized nodes in the raw TaQL parse tree
//# Copyright (C) 2005
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

#ifndef TABLES_TAQLNODEDER_H
#define TABLES_TAQLNODEDER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/TaQLNode.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/Containers/Block.h>
#include <vector>
#include <iostream>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


// <summary>
// Raw TaQL parse tree node defining a constant value.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding a constant expression or a table name.
// The types supported are Bool, Int, Double, DComplex, String, and MVTime.
// Note that a keyword or column name is represented by TaQLKeyColNodeRep.
// </synopsis> 

class TaQLConstNodeRep: public TaQLNodeRep
{
public:
  // Do not change the values of this enum, as objects might be persistent.
  enum Type {CTBool   =0,
	     CTInt    =1,
	     CTReal   =2,
	     CTComplex=3,
	     CTString =4,
	     CTTime   =5};
  explicit TaQLConstNodeRep (Bool value)
    : TaQLNodeRep (TaQLNode_Const),
      itsType(CTBool), itsIsTableName(False), itsBValue(value) {}
  explicit TaQLConstNodeRep (Int64 value, Bool isTableName=False)
    : TaQLNodeRep (TaQLNode_Const),
      itsType(CTInt), itsIsTableName(isTableName), itsIValue(value),
      itsRValue(value), itsCValue(value,0.) {}
  explicit TaQLConstNodeRep (Double value)
    : TaQLNodeRep (TaQLNode_Const),
      itsType(CTReal), itsIsTableName(False), itsRValue(value),
      itsCValue(value,0.) {}
  explicit TaQLConstNodeRep (Double value, const String& unit)
    : TaQLNodeRep (TaQLNode_Const),
      itsType(CTReal), itsIsTableName(False), itsRValue(value),
      itsCValue(value,0.), itsUnit(unit) {}
  explicit TaQLConstNodeRep (DComplex value)
    : TaQLNodeRep (TaQLNode_Const),
      itsType(CTComplex), itsIsTableName(False), itsCValue(value) {}
  explicit TaQLConstNodeRep (const String& value, Bool isTableName=False)
    : TaQLNodeRep (TaQLNode_Const),
      itsType(CTString), itsIsTableName(isTableName), itsSValue(value) {}
  explicit TaQLConstNodeRep (const MVTime& value)
    : TaQLNodeRep (TaQLNode_Const),
      itsType(CTTime), itsIsTableName(False),
      itsRValue(value), itsCValue(value,0.), itsTValue(value) {}
  virtual ~TaQLConstNodeRep();
  void setIsTableName()
    { itsIsTableName = True; }
  const String& getString() const;
  const String& getUnit() const
    { return itsUnit; }
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLConstNodeRep* restore (AipsIO& aio);

  Type     itsType;
  Bool     itsIsTableName;
  Bool     itsBValue;
  Int64    itsIValue;
  Double   itsRValue;
  DComplex itsCValue;
  String   itsSValue;
  MVTime   itsTValue;
  String   itsUnit;
};


// <summary>
// Raw TaQL parse tree node defining a constant regex value.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding a constant regex/pattern value.
// Part of the regex are the delimiters (like p//).
// It also holds if the regex is case-insensitive and if a match or no match
// operator is given.
// </synopsis> 

class TaQLRegexNodeRep: public TaQLNodeRep
{
public:
  explicit TaQLRegexNodeRep (const String& value);
  TaQLRegexNodeRep (const String& value, Bool caseInsensitive, Bool negate,
                    Bool ignoreBlanks, Int maxDistance)
    : TaQLNodeRep (TaQLNode_Regex),
      itsValue(value), itsCaseInsensitive(caseInsensitive), itsNegate(negate),
      itsIgnoreBlanks(ignoreBlanks), itsMaxDistance(maxDistance) {}
  virtual ~TaQLRegexNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLRegexNodeRep* restore (AipsIO& aio);

  String itsValue;
  Bool   itsCaseInsensitive;
  Bool   itsNegate;             //# True means !~
  //# The following members are only used for distance.
  Bool   itsIgnoreBlanks;
  Int    itsMaxDistance;
};


// <summary>
// Raw TaQL parse tree node defining a unary operator.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding a unary operator and operand.
// The operators supported are -, ~, NOT, EXISTS, and NOT EXISTS.
// Note the unary operator + is superfluous and is ignored by the parser.
// </synopsis> 

class TaQLUnaryNodeRep: public TaQLNodeRep
{
public:
  // Do not change the values of this enum, as objects might be persistent.
  enum Type {U_MINUS    =0,
	     U_NOT      =1,
	     U_EXISTS   =2,
	     U_NOTEXISTS=3,
             U_BITNOT   =4};
  TaQLUnaryNodeRep (Type type, const TaQLNode& child)
    : TaQLNodeRep (TaQLNode_Unary),
      itsType(type), itsChild(child) {}
  virtual ~TaQLUnaryNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLUnaryNodeRep* restore (AipsIO& aio);

  Type     itsType;
  TaQLNode itsChild;
};


// <summary>
// Raw TaQL parse tree node defining a binary operator.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding a binary operator and operands.
// All standard mathematical (including % and ^), relational, bit, and logical
// operators are supported. Furthermore operator IN and the INDEX operator
// (for indexing in an array) are supported.
// </synopsis> 

class TaQLBinaryNodeRep: public TaQLNodeRep
{
public:
  // Do not change the values of this enum, as objects might be persistent.
  enum Type {B_PLUS  =0,
	     B_MINUS =1,
	     B_TIMES =2,
	     B_DIVIDE=3,
	     B_MODULO=4,
	     B_POWER =5,
	     B_EQ    =6,
	     B_NE    =7,
	     B_GT    =8,
	     B_GE    =9,
	     B_LT    =10,
	     B_LE    =11,
	     B_OR    =12,
	     B_AND   =13,
	     B_IN    =14,
	     B_INDEX =15,
	     B_DIVIDETRUNC=16,
	     B_EQREGEX    =17,
	     B_NEREGEX    =18,
	     B_BITAND     =19,
             B_BITXOR     =20,
             B_BITOR      =21};
  TaQLBinaryNodeRep (Type type, const TaQLNode& left, const TaQLNode& right)
    : TaQLNodeRep (TaQLNode_Binary),
      itsType(type), itsLeft(left), itsRight(right) {}
  virtual ~TaQLBinaryNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLBinaryNodeRep* restore (AipsIO& aio);
  // Handle a comparison wih a regex. The operator (~ or !~) is extracted
  // from the regex.
  static TaQLBinaryNodeRep* handleRegex (const TaQLNode& left,
					 const TaQLRegexNode& regex);

  Type     itsType;
  TaQLNode itsLeft;
  TaQLNode itsRight;
};


// <summary>
// Raw TaQL parse tree node defining a list of nodes.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding a list of heterogeneous nodes.
// </synopsis> 

class TaQLMultiNodeRep: public TaQLNodeRep
{
public:
  explicit TaQLMultiNodeRep (Bool isSetOrArray=False)
    : TaQLNodeRep (TaQLNode_Multi), itsIsSetOrArray(isSetOrArray) {}
  TaQLMultiNodeRep(const String& prefix, const String& postfix,
		   Bool isSetOrArray=False)
    : TaQLNodeRep (TaQLNode_Multi),
      itsIsSetOrArray(isSetOrArray),
      itsPrefix(prefix), itsPostfix(postfix) {}
  virtual ~TaQLMultiNodeRep();
  void setIsSetOrArray()
    { itsIsSetOrArray = True; }
  void setPPFix (const String& prefix, const String& postfix)
    { itsPrefix = prefix; itsPostfix = postfix; }
  void add (const TaQLNode& node)
    { itsNodes.push_back (node); }
  const std::vector<TaQLNode>& getNodes() const
    { return itsNodes; }
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLMultiNodeRep* restore (AipsIO& aio);

  std::vector<TaQLNode> itsNodes;
  Bool   itsIsSetOrArray;
  String itsPrefix;
  String itsPostfix;
};


// <summary>
// Raw TaQL parse tree node defining a function.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding a function name and its arguments.
// </synopsis> 

class TaQLFuncNodeRep: public TaQLNodeRep
{
public:
  TaQLFuncNodeRep (const String& name)
    : TaQLNodeRep (TaQLNode_Func),
      itsName(name), itsArgs(False) {}
  TaQLFuncNodeRep (const String& name, const TaQLMultiNode& args)
    : TaQLNodeRep (TaQLNode_Func),
      itsName(name), itsArgs(args) {}
  virtual ~TaQLFuncNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLFuncNodeRep* restore (AipsIO& aio);

  String        itsName;
  TaQLMultiNode itsArgs;
};


// <summary>
// Raw TaQL parse tree node defining a range.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the optional start and end values
// of a range (i.e. an interval) and flags if the range is open or closed.
// </synopsis> 

class TaQLRangeNodeRep: public TaQLNodeRep
{
public:
  TaQLRangeNodeRep (Bool leftClosed, TaQLNode start,
		    const TaQLNode& end, Bool rightClosed)
    : TaQLNodeRep (TaQLNode_Range),
      itsLeftClosed(leftClosed), itsStart(start),
      itsEnd(end), itsRightClosed(rightClosed) {}
  TaQLRangeNodeRep (Bool leftClosed, const TaQLNode& start)
    : TaQLNodeRep (TaQLNode_Range),
      itsLeftClosed(leftClosed), itsStart(start),
      itsEnd(), itsRightClosed(False) {}
  TaQLRangeNodeRep (const TaQLNode& end, Bool rightClosed)
    : TaQLNodeRep (TaQLNode_Range),
      itsLeftClosed(False), itsStart(),
      itsEnd(end), itsRightClosed(rightClosed) {}
  virtual ~TaQLRangeNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLRangeNodeRep* restore (AipsIO& aio);

  Bool     itsLeftClosed;
  TaQLNode itsStart;
  TaQLNode itsEnd;
  Bool     itsRightClosed;
};


// <summary>
// Raw TaQL parse tree node defining an index in a array.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the optional start, end, and incr
// values of an index in an array.
// </synopsis> 

class TaQLIndexNodeRep: public TaQLNodeRep
{
public:
  TaQLIndexNodeRep (const TaQLNode& start, const TaQLNode& end,
		    const TaQLNode& incr)
    : TaQLNodeRep (TaQLNode_Index),
      itsStart(start), itsEnd(end), itsIncr(incr) {}
  virtual ~TaQLIndexNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLIndexNodeRep* restore (AipsIO& aio);

  TaQLNode itsStart;
  TaQLNode itsEnd;
  TaQLNode itsIncr;
};


// <summary>
// Raw TaQL parse tree node defining a join operation.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the expressions of a join operation.
// This is, however, a placeholder and not implemented yet.
// </synopsis> 

class TaQLJoinNodeRep: public TaQLNodeRep
{
public:
  TaQLJoinNodeRep (const TaQLMultiNode& tables, const TaQLNode& condition)
    : TaQLNodeRep (TaQLNode_Join),
      itsTables(tables), itsCondition(condition) {}
  virtual ~TaQLJoinNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLJoinNodeRep* restore (AipsIO& aio);

  TaQLMultiNode itsTables;
  TaQLNode      itsCondition;
};


// <summary>
// Raw TaQL parse tree node defining a keyword or column name.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the name of a keyword or column.
// The name can contain . and :: delimiters for scoping.
// </synopsis> 

class TaQLKeyColNodeRep: public TaQLNodeRep
{
public:
  TaQLKeyColNodeRep (const String& name)
    : TaQLNodeRep (TaQLNode_KeyCol),
      itsName(name) {}
  virtual ~TaQLKeyColNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLKeyColNodeRep* restore (AipsIO& aio);

  String itsName;
};


// <summary>
// Raw TaQL parse tree node defining a table.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the info defining a table.
// It can be a constant value holding a name or it can be a subquery.
// Furthermore the alias of the table is defined (which can be empty).
// </synopsis> 

class TaQLTableNodeRep: public TaQLNodeRep
{
public:
  TaQLTableNodeRep (const TaQLNode& table, const String& alias)
    : TaQLNodeRep (TaQLNode_Table),
      itsTable(table), itsAlias(alias) {}
  virtual ~TaQLTableNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLTableNodeRep* restore (AipsIO& aio);

  TaQLNode itsTable;
  String   itsAlias;
};


// <summary>
// Raw TaQL parse tree node defining a select column expression.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding a column expression in the
// column list of the select clause.
// A new column name and data type can be defined for the column (expression).
// The expression can be a wildcarded column name (a regex) preceeded by
// ~ or !~ (meaning include or exclude).
// </synopsis> 

class TaQLColNodeRep: public TaQLNodeRep
{
public:
  TaQLColNodeRep (const TaQLNode& expr, const String& name,
		  const String& dtype)
    : TaQLNodeRep (TaQLNode_Col),
      itsExpr(expr), itsName(name), itsDtype(checkDataType(dtype)) {}
  virtual ~TaQLColNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLColNodeRep* restore (AipsIO& aio);

  TaQLNode itsExpr;
  String   itsName;
  String   itsDtype;
};


// <summary>
// Raw TaQL parse tree node defining a select column list.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding a select column list.
// It also defines if the result must be distinct (unique)
// </synopsis> 

class TaQLColumnsNodeRep: public TaQLNodeRep
{
public:
  TaQLColumnsNodeRep (Bool distinct, const TaQLMultiNode& nodes)
    : TaQLNodeRep (TaQLNode_Columns),
      itsDistinct(distinct), itsNodes(nodes) {}
  virtual ~TaQLColumnsNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLColumnsNodeRep* restore (AipsIO& aio);

  Bool          itsDistinct;
  TaQLMultiNode itsNodes;
};


// <summary>
// Raw TaQL parse tree node defining a groupby list.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding a groupby list with the optional
// ROLLUP qualifier.
// </synopsis> 

class TaQLGroupNodeRep: public TaQLNodeRep
{
public:
  // Do not change the values of this enum, as objects might be persistent.
  enum Type {Normal=0,
	     Rollup=1};  //# in the future type Cube could be added
  TaQLGroupNodeRep (Type type, const TaQLMultiNode& nodes)
    : TaQLNodeRep (TaQLNode_Groupby),
      itsType(type), itsNodes(nodes) {}
  virtual ~TaQLGroupNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLGroupNodeRep* restore (AipsIO& aio);

  Type          itsType;
  TaQLMultiNode itsNodes;
};


// <summary>
// Raw TaQL parse tree node defining a sort key.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding a sort key and the optional order
// in which this key must be sorted.
// </synopsis> 

class TaQLSortKeyNodeRep: public TaQLNodeRep
{
public:
  // Do not change the values of this enum, as objects might be persistent.
  enum Type {Ascending =0,
	     Descending=1,
	     None      =2};
  TaQLSortKeyNodeRep (Type type, const TaQLNode& child)
    : TaQLNodeRep (TaQLNode_SortKey),
      itsType(type), itsChild(child) {}
  virtual ~TaQLSortKeyNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLSortKeyNodeRep* restore (AipsIO& aio);

  Type     itsType;
  TaQLNode itsChild;
};


// <summary>
// Raw TaQL parse tree node defining a sort list.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding a sort list and the default order
// for each individual sort key.
// </synopsis> 

class TaQLSortNodeRep: public TaQLNodeRep
{
public:
  // Do not change the values of this enum, as objects might be persistent.
  enum Type {Ascending =0,
	     Descending=1};
  TaQLSortNodeRep (Bool unique, Type type, const TaQLMultiNode& keys)
    : TaQLNodeRep (TaQLNode_Sort),
      itsUnique(unique), itsType(type), itsKeys(keys) {}
  virtual ~TaQLSortNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLSortNodeRep* restore (AipsIO& aio);

  Bool          itsUnique;
  Type          itsType;
  TaQLMultiNode itsKeys;
};


// <summary>
// Raw TaQL parse tree node defining a limit/offset expression.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the optional expressions for the
// LIMIT and OFFSET clause.
// </synopsis> 

class TaQLLimitOffNodeRep: public TaQLNodeRep
{
public:
  TaQLLimitOffNodeRep (const TaQLNode& limit, const TaQLNode& offset)
    : TaQLNodeRep (TaQLNode_LimitOff),
      itsLimit(limit), itsOffset(offset) {}
  virtual ~TaQLLimitOffNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLLimitOffNodeRep* restore (AipsIO& aio);

  TaQLNode itsLimit;
  TaQLNode itsOffset;
};


// <summary>
// Raw TaQL parse tree node defining a giving expression list.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the values for a GIVING clause.
// The value can be a table name or a list of expressions.
// </synopsis> 

class TaQLGivingNodeRep: public TaQLNodeRep
{
public:
  explicit TaQLGivingNodeRep (const String& name, const String& type);
  explicit TaQLGivingNodeRep (const TaQLMultiNode& exprlist)
    : TaQLNodeRep (TaQLNode_Giving),
      itsType     (-1),
      itsExprList (exprlist) {}
  virtual ~TaQLGivingNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLGivingNodeRep* restore (AipsIO& aio);
  // Constructor for restore.
  TaQLGivingNodeRep (const String& name, Int type)
    : TaQLNodeRep (TaQLNode_Giving),
      itsName     (name),
      itsType     (type) {}

  String        itsName;
  Int           itsType;    // -1=exprlist 0=undefined, 1=memory, 2=scratch
                            //  3=plain, 4=plain_big, 5=plain_little,
                            //  6=plain_local
  TaQLMultiNode itsExprList;
};


// <summary>
// Raw TaQL parse tree node defining a column update expression.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the values for an update expression.
// It defines the column name and the expression for the new value.
// Optionally an index can be defined in case the column contains array
// values for which only some values need to be updated.
// </synopsis> 

class TaQLUpdExprNodeRep: public TaQLNodeRep
{
public:
  explicit TaQLUpdExprNodeRep (const String& name,
			       const TaQLMultiNode& indices,
			       const TaQLNode& expr)
    : TaQLNodeRep (TaQLNode_UpdExpr),
      itsName(name), itsIndices(indices), itsExpr(expr) {}
  virtual ~TaQLUpdExprNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLUpdExprNodeRep* restore (AipsIO& aio);

  String        itsName;
  TaQLMultiNode itsIndices;
  TaQLNode      itsExpr;
};


// <summary>
// Raw TaQL parse tree node defining a selection command.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is an abstract TaQLNodeRep for a selection command that can
// also be used as a subquery.
// It holds flags telling if and how the select command must be
// executed when the node is visited for TaQLNodeHandler.
// </synopsis> 

class TaQLQueryNodeRep: public TaQLNodeRep
{
public:
  TaQLQueryNodeRep (int nodeType);
  virtual ~TaQLQueryNodeRep();
  void setBrackets()
    { itsBrackets = True; }
  void setNoExecute()
    { itsNoExecute = True; }
  void setFromExecute()
    { itsFromExecute = True; }
  Bool getBrackets() const
    { return itsBrackets; }
  Bool getNoExecute() const
    { return itsNoExecute; }
  Bool getFromExecute() const
    { return itsFromExecute; }
  virtual void show (std::ostream& os) const;
protected:
  virtual void saveSuper (AipsIO& aio) const;
  virtual void restoreSuper (AipsIO& aio);
private:
  virtual void showDerived (std::ostream& os) const = 0;
  Bool itsBrackets;
  Bool itsNoExecute;    //# no execute in EXISTS operator
  Bool itsFromExecute;  //# special execute in FROM
};


// <summary>
// Raw TaQL parse tree node defining a select command.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the different parts of a
// select expression.
// It also holds flags telling if and how the select command must be
// executed when the node is visited for TaQLNodeHandler.
// </synopsis> 

class TaQLSelectNodeRep: public TaQLQueryNodeRep
{
public:
  TaQLSelectNodeRep (const TaQLNode& columns, const TaQLMultiNode& tables,
		     const TaQLNode& join, const TaQLNode& where,
		     const TaQLNode& groupby, const TaQLNode& having,
		     const TaQLNode& sort, const TaQLNode& limitoff,
		     const TaQLNode& giving);
  virtual ~TaQLSelectNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void showDerived (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLSelectNodeRep* restore (AipsIO& aio);

  TaQLNode      itsColumns;
  TaQLMultiNode itsTables;
  TaQLNode      itsJoin;
  TaQLNode      itsWhere;
  TaQLNode      itsGroupby;
  TaQLNode      itsHaving;
  TaQLNode      itsSort;
  TaQLNode      itsLimitOff;
  TaQLNode      itsGiving;
};


// <summary>
// Raw TaQL parse tree node defining a count command.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the parts for a count command.
// </synopsis> 

class TaQLCountNodeRep: public TaQLQueryNodeRep
{
public:
  TaQLCountNodeRep (const TaQLNode& columns, const TaQLMultiNode& tables,
                    const TaQLNode& where);
  virtual ~TaQLCountNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void showDerived (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLCountNodeRep* restore (AipsIO& aio);

  TaQLNode      itsColumns;
  TaQLMultiNode itsTables;
  TaQLNode      itsWhere;
};


// <summary>
// Raw TaQL parse tree node defining an update command.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the parts for an update command.
// The tables to be used can be defined in two parts: the main one in
// the UPDATE clause, possible other ones in the FROM command.
// </synopsis> 

class TaQLUpdateNodeRep: public TaQLNodeRep
{
public:
  TaQLUpdateNodeRep (const TaQLMultiNode& tables, const TaQLMultiNode& update,
		     const TaQLMultiNode& from, const TaQLNode& where,
		     const TaQLNode& sort, const TaQLNode& limitoff)
    : TaQLNodeRep (TaQLNode_Update),
      itsTables(tables), itsUpdate(update), itsFrom(from),
      itsWhere(where), itsSort(sort), itsLimitOff(limitoff) {}
  virtual ~TaQLUpdateNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLUpdateNodeRep* restore (AipsIO& aio);

  TaQLMultiNode itsTables;
  TaQLMultiNode itsUpdate;
  TaQLMultiNode itsFrom;
  TaQLNode      itsWhere;
  TaQLNode      itsSort;
  TaQLNode      itsLimitOff;
};


// <summary>
// Raw TaQL parse tree node defining an insert command.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the parts for an insert command.
// The values cvan be a list of expressions or a subquery.
// </synopsis> 

class TaQLInsertNodeRep: public TaQLNodeRep
{
public:
  TaQLInsertNodeRep (const TaQLMultiNode& tables, const TaQLMultiNode& columns,
		     const TaQLNode& values)
    : TaQLNodeRep (TaQLNode_Insert),
      itsTables(tables), itsColumns(columns), itsValues(values) {}
  TaQLInsertNodeRep (const TaQLMultiNode& tables, const TaQLMultiNode& insert);
  virtual ~TaQLInsertNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLInsertNodeRep* restore (AipsIO& aio);

  TaQLMultiNode itsTables;
  TaQLMultiNode itsColumns;
  TaQLNode      itsValues;
};


// <summary>
// Raw TaQL parse tree node defining a delete command.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the parts for a delete command.
// </synopsis> 

class TaQLDeleteNodeRep: public TaQLNodeRep
{
public:
  TaQLDeleteNodeRep (const TaQLMultiNode& tables, const TaQLNode& where,
		     const TaQLNode& sort, const TaQLNode& limitoff)
    : TaQLNodeRep (TaQLNode_Delete),
      itsTables(tables), itsWhere(where),
      itsSort(sort), itsLimitOff(limitoff) {}
  virtual ~TaQLDeleteNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLDeleteNodeRep* restore (AipsIO& aio);

  TaQLMultiNode itsTables;
  TaQLNode      itsWhere;
  TaQLNode      itsSort;
  TaQLNode      itsLimitOff;
};


// <summary>
// Raw TaQL parse tree node defining a calc command.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the parts of the calc command.
// </synopsis> 

class TaQLCalcNodeRep: public TaQLNodeRep
{
public:
  TaQLCalcNodeRep (const TaQLMultiNode& tables, const TaQLNode& expr,
                   const TaQLNode& where,
                   const TaQLNode& sort, const TaQLNode& limitoff)
    : TaQLNodeRep (TaQLNode_Calc),
      itsTables(tables), itsExpr(expr),
      itsWhere(where), itsSort(sort), itsLimitOff(limitoff) {}
  virtual ~TaQLCalcNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLCalcNodeRep* restore (AipsIO& aio);

  TaQLMultiNode itsTables;
  TaQLNode      itsExpr;
  TaQLNode      itsWhere;
  TaQLNode      itsSort;
  TaQLNode      itsLimitOff;
};


// <summary>
// Raw TaQL parse tree node defining a create table command.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the parts of the create table command.
// </synopsis> 

class TaQLCreTabNodeRep: public TaQLNodeRep
{
public:
  TaQLCreTabNodeRep (const String& name, const TaQLMultiNode& cols,
		     const TaQLMultiNode& dataMans)
    : TaQLNodeRep (TaQLNode_CreTab),
      itsName(name), itsColumns(cols), itsDataMans(dataMans) {}
  virtual ~TaQLCreTabNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLCreTabNodeRep* restore (AipsIO& aio);

  String        itsName;
  TaQLMultiNode itsColumns;
  TaQLMultiNode itsDataMans;
};


// <summary>
// Raw TaQL parse tree node defining a create column specification.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the parts of a column specification
// in the create table command.
// </synopsis> 

class TaQLColSpecNodeRep: public TaQLNodeRep
{
public:
  TaQLColSpecNodeRep (const String& name, const String& dtype,
		      const TaQLMultiNode& spec)
    : TaQLNodeRep (TaQLNode_ColSpec),
      itsName(name), itsDtype(checkDataType(dtype)), itsSpec(spec) {}
  virtual ~TaQLColSpecNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLColSpecNodeRep* restore (AipsIO& aio);

  String        itsName;
  String        itsDtype;
  TaQLMultiNode itsSpec;
};


// <summary>
// Raw TaQL parse tree node defining a record field.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the parts of a record field.
// </synopsis> 

class TaQLRecFldNodeRep: public TaQLNodeRep
{
public:
  TaQLRecFldNodeRep (const String& name, const TaQLNode& values)
    : TaQLNodeRep (TaQLNode_RecFld),
      itsName(name), itsValues(values) {}
  virtual ~TaQLRecFldNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLRecFldNodeRep* restore (AipsIO& aio);

  String   itsName;
  TaQLNode itsValues;
};


// <summary>
// Raw TaQL parse tree node defining a unit.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the parts of a record field.
// </synopsis> 

class TaQLUnitNodeRep: public TaQLNodeRep
{
public:
  TaQLUnitNodeRep (const String& unit, const TaQLNode& child)
    : TaQLNodeRep (TaQLNode_Unit),
      itsUnit(unit), itsChild(child) {}
  virtual ~TaQLUnitNodeRep();
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLUnitNodeRep* restore (AipsIO& aio);

  String   itsUnit;
  TaQLNode itsChild;
};


} //# NAMESPACE CASACORE - END

#endif
