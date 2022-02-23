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
  explicit TaQLConstNodeRep (Bool value);
  explicit TaQLConstNodeRep (Int64 value);
  explicit TaQLConstNodeRep (Double value);
  explicit TaQLConstNodeRep (Double value, const String& unit);
  explicit TaQLConstNodeRep (DComplex value);
  explicit TaQLConstNodeRep (const String& value, Bool isTableName=False);
  explicit TaQLConstNodeRep (const MVTime& value);
  explicit TaQLConstNodeRep (Int64 value, const String& subTableName);
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
                    Bool ignoreBlanks, Int maxDistance);
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
  TaQLUnaryNodeRep (Type type, const TaQLNode& child);
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
  TaQLBinaryNodeRep (Type type, const TaQLNode& left, const TaQLNode& right);
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
  explicit TaQLMultiNodeRep (Bool isSetOrArray=False);
  TaQLMultiNodeRep(const String& prefix, const String& postfix,
                   Bool isSetOrArray=False);
  void setIsSetOrArray()
    { itsIsSetOrArray = True; }
  void setPPFix (const String& prefix, const String& postfix)
    { itsPrefix = prefix; itsPostfix = postfix; }
  void setSeparator (const String& sep)
    { itsSep = sep; }
  void setSeparator (uInt incr, const String& sep)
  { itsIncr = incr; itsSep2 = sep; }
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
  String itsSep;
  String itsSep2;
  uInt   itsIncr;
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
  TaQLFuncNodeRep (const String& name);
  TaQLFuncNodeRep (const String& name, const TaQLMultiNode& args);
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
                    const TaQLNode& end, Bool rightClosed);
  TaQLRangeNodeRep (Bool leftClosed, const TaQLNode& start);
  TaQLRangeNodeRep (const TaQLNode& end, Bool rightClosed);
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
                    const TaQLNode& incr);
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
  TaQLJoinNodeRep (const TaQLMultiNode& tables, const TaQLNode& condition);
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
  TaQLKeyColNodeRep (const String& name, const String& nameMask = String());
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLKeyColNodeRep* restore (AipsIO& aio);

  String itsName;
  String itsNameMask;
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
  TaQLTableNodeRep (const TaQLNode& table, const String& alias);
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
                  const String& nameMask, const String& dtype);
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLColNodeRep* restore (AipsIO& aio);

  TaQLNode itsExpr;
  String   itsName;
  String   itsNameMask;
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
  TaQLColumnsNodeRep (Bool distinct, const TaQLMultiNode& nodes);
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
  TaQLGroupNodeRep (Type type, const TaQLMultiNode& nodes);
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
  TaQLSortKeyNodeRep (Type type, const TaQLNode& child);
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
  TaQLSortNodeRep (Bool unique, Type type, const TaQLMultiNode& keys);
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
  TaQLLimitOffNodeRep (const TaQLNode& limit, const TaQLNode& offset);
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
  explicit TaQLGivingNodeRep (const String& name, const TaQLMultiNode& type);
  explicit TaQLGivingNodeRep (const TaQLMultiNode& exprlist);
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLGivingNodeRep* restore (AipsIO& aio);

  String        itsName;
  TaQLMultiNode itsType;
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
  TaQLUpdExprNodeRep (const String& name, const String& nameMask,
                      const TaQLNode& expr);
  TaQLUpdExprNodeRep (const String& name, const String& nameMask,
                      const TaQLMultiNode& indices,
                      const TaQLNode& expr);
  TaQLUpdExprNodeRep (const String& name, const String& nameMask,
                      const TaQLMultiNode& indices1,
                      const TaQLMultiNode& indices2,
                      const TaQLNode& expr);
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLUpdExprNodeRep* restore (AipsIO& aio);

  String        itsName;
  String        itsNameMask;
  TaQLMultiNode itsIndices1;     //# indices or mask
  TaQLMultiNode itsIndices2;     //# mask or indices
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
  void saveSuper (AipsIO& aio) const;
  void restoreSuper (AipsIO& aio);
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
  TaQLSelectNodeRep (const TaQLNode& columns,
                     const TaQLMultiNode& withTables, const TaQLNode& where,
                     const TaQLNode& groupby, const TaQLNode& having,
                     const TaQLNode& sort, const TaQLNode& limitoff,
                     const TaQLNode& giving, const TaQLMultiNode& dminfo);
  TaQLSelectNodeRep (const TaQLNode& columns,
                     const TaQLMultiNode& withTables, const TaQLMultiNode& fromTables,
                     const TaQLNode& join, const TaQLNode& where,
                     const TaQLNode& groupby, const TaQLNode& having,
                     const TaQLNode& sort, const TaQLNode& limitoff,
                     const TaQLNode& giving, const TaQLMultiNode& dminfo);
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void showDerived (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLSelectNodeRep* restore (AipsIO& aio);

  TaQLNode      itsColumns;
  TaQLMultiNode itsWith;
  TaQLMultiNode itsTables;
  TaQLNode      itsJoin;
  TaQLNode      itsWhere;
  TaQLNode      itsGroupby;
  TaQLNode      itsHaving;
  TaQLNode      itsSort;
  TaQLNode      itsLimitOff;
  TaQLNode      itsGiving;
  TaQLMultiNode itsDMInfo;
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
  TaQLCountNodeRep (const TaQLMultiNode& with, const TaQLNode& columns,
                    const TaQLMultiNode& tables, const TaQLNode& where);
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void showDerived (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLCountNodeRep* restore (AipsIO& aio);

  TaQLMultiNode itsWith;
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
  TaQLUpdateNodeRep (const TaQLMultiNode& with,
                     const TaQLMultiNode& tables, const TaQLMultiNode& update,
                     const TaQLMultiNode& from, const TaQLNode& where,
                     const TaQLNode& sort, const TaQLNode& limitoff);
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLUpdateNodeRep* restore (AipsIO& aio);

  TaQLMultiNode itsWith;
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
  TaQLInsertNodeRep (const TaQLMultiNode& with, const TaQLMultiNode& tables,
                     const TaQLMultiNode& columns,
                     const TaQLNode& values, const TaQLNode& limit);
  TaQLInsertNodeRep (const TaQLMultiNode& with, const TaQLMultiNode& tables,
                     const TaQLMultiNode& insert);
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLInsertNodeRep* restore (AipsIO& aio);

  TaQLMultiNode itsWith;
  TaQLMultiNode itsTables;
  TaQLMultiNode itsColumns;
  TaQLNode      itsValues;
  TaQLNode      itsLimit;
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
  TaQLDeleteNodeRep (const TaQLMultiNode& with, const TaQLMultiNode& tables,
                     const TaQLNode& where,
                     const TaQLNode& sort, const TaQLNode& limitoff);
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLDeleteNodeRep* restore (AipsIO& aio);

  TaQLMultiNode itsWith;
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
  TaQLCalcNodeRep (const TaQLMultiNode& withTables, const TaQLMultiNode& fromTables,
                   const TaQLNode& expr, const TaQLNode& where,
                   const TaQLNode& sort, const TaQLNode& limitoff);
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLCalcNodeRep* restore (AipsIO& aio);

  TaQLMultiNode itsWith;
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

class TaQLCreTabNodeRep: public TaQLQueryNodeRep
{
public:
  TaQLCreTabNodeRep (const TaQLMultiNode& with,
                     const TaQLNode& giving, const TaQLMultiNode& likeDrop,
                     const TaQLMultiNode& cols,
                     const TaQLNode& limit, const TaQLMultiNode& dminfo);
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void showDerived (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLCreTabNodeRep* restore (AipsIO& aio);

  TaQLMultiNode itsWith;
  TaQLNode      itsGiving;
  TaQLMultiNode itsLikeDrop;
  TaQLMultiNode itsColumns;
  TaQLNode      itsLimit;
  TaQLMultiNode itsDMInfo;
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
  TaQLColSpecNodeRep (const String& name, const String& likeCol,
                      const String& dtype, const TaQLMultiNode& spec);
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLColSpecNodeRep* restore (AipsIO& aio);

  String        itsName;
  String        itsLikeCol;
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
  TaQLRecFldNodeRep (const String& name,
                     const TaQLNode& values, const String& dtype);
  TaQLRecFldNodeRep (const String& name, const TaQLRecFldNodeRep&);
  TaQLRecFldNodeRep (const String& name, const String& fromName,
                     const String& dtype);
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLRecFldNodeRep* restore (AipsIO& aio);

  String   itsName;
  String   itsFromName;
  String   itsDtype;
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
  TaQLUnitNodeRep (const String& unit, const TaQLNode& child);
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLUnitNodeRep* restore (AipsIO& aio);

  String   itsUnit;
  TaQLNode itsChild;
};


// <summary>
// Raw TaQL parse tree node defining an alter table command.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the parts of the alter table command.
// </synopsis> 

class TaQLAltTabNodeRep: public TaQLQueryNodeRep
{
public:
  TaQLAltTabNodeRep (const TaQLMultiNode& with, const TaQLNode& table,
                     const TaQLMultiNode& from, const TaQLMultiNode& commands);
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void showDerived (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLAltTabNodeRep* restore (AipsIO& aio);

  TaQLMultiNode itsWith;
  TaQLNode      itsTable;
  TaQLMultiNode itsFrom;
  TaQLMultiNode itsCommands;
};


// <summary>
// Raw TaQL parse tree node defining an alter table add column command.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the parts of the add column subcommand.
// </synopsis> 

class TaQLAddColNodeRep: public TaQLNodeRep
{
public:
  TaQLAddColNodeRep (const TaQLMultiNode& cols, const TaQLMultiNode& dminfo);
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLAddColNodeRep* restore (AipsIO& aio);

  TaQLMultiNode itsColumns;
  TaQLMultiNode itsDMInfo;
};


// <summary>
// Raw TaQL parse tree node defining an alter table rename or drop command.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the parts of the rename or drop subcommand.
// </synopsis> 

class TaQLRenDropNodeRep: public TaQLNodeRep
{
public:
  TaQLRenDropNodeRep (Int type, const TaQLMultiNode& cols);
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLRenDropNodeRep* restore (AipsIO& aio);

  Int           itsType;
  TaQLMultiNode itsNames;
};


// <summary>
// Raw TaQL parse tree node defining an alter table set keyword command.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the parts of the set keyword subcommand.
// </synopsis> 

class TaQLSetKeyNodeRep: public TaQLNodeRep
{
public:
  TaQLSetKeyNodeRep (const TaQLMultiNode& keyvals);
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLSetKeyNodeRep* restore (AipsIO& aio);

  TaQLMultiNode itsKeyVals;
};


// <summary>
// Raw TaQL parse tree node defining an alter table add rows command.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the parts of the add rows subcommand.
// </synopsis> 

class TaQLAddRowNodeRep: public TaQLNodeRep
{
public:
  TaQLAddRowNodeRep (const TaQLNode& nrow);
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLAddRowNodeRep* restore (AipsIO& aio);

  TaQLNode itsNRow;
};


// <summary>
// Raw TaQL parse tree node defining an alter table command.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the parts of the alter table command.
// </synopsis> 

class TaQLConcTabNodeRep: public TaQLQueryNodeRep
{
public:
  TaQLConcTabNodeRep (const String& tableName,
                      const TaQLMultiNode& tables,
                      const TaQLMultiNode& subtableNames);
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void showDerived (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLConcTabNodeRep* restore (AipsIO& aio);

  String        itsTableName;
  TaQLMultiNode itsTables;
  TaQLMultiNode itsSubTables;
};


// <summary>
// Raw TaQL parse tree node defining a show command.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the parts of the show command.
// </synopsis> 

class TaQLShowNodeRep: public TaQLNodeRep
{
public:
  TaQLShowNodeRep (const TaQLMultiNode& names);
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const;
  virtual void show (std::ostream& os) const;
  virtual void save (AipsIO& aio) const;
  static TaQLShowNodeRep* restore (AipsIO& aio);

  TaQLMultiNode itsNames;
};


// <summary>
// Raw TaQL parse tree node defining an alter table copy column command.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the parts of the copy column subcommand.
// </synopsis> 

class TaQLCopyColNodeRep: public TaQLNodeRep
{
public:
  TaQLCopyColNodeRep (const TaQLMultiNode& names, const TaQLMultiNode& dminfo);
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const override;
  virtual void show (std::ostream& os) const override;
  virtual void save (AipsIO& aio) const override;
  static TaQLCopyColNodeRep* restore (AipsIO& aio);

  TaQLMultiNode itsNames;
  TaQLMultiNode itsDMInfo;
};


// <summary>
// Raw TaQL parse tree node defining a DROP TABLE command.
// </summary>
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TaQLNodeRep>TaQLNodeRep</linkto>
// </prerequisite>
// <synopsis> 
// This class is a TaQLNodeRep holding the tables of a drop table command.
// </synopsis> 

class TaQLDropTabNodeRep: public TaQLNodeRep
{
public:
  TaQLDropTabNodeRep (const TaQLMultiNode& with, const TaQLMultiNode& tables);
  virtual TaQLNodeResult visit (TaQLNodeVisitor&) const override;
  virtual void show (std::ostream& os) const override;
  virtual void save (AipsIO& aio) const override;
  static TaQLDropTabNodeRep* restore (AipsIO& aio);

  TaQLMultiNode itsWith;
  TaQLMultiNode itsTables;
};


} //# NAMESPACE CASACORE - END

#endif
