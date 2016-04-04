//# TaQLNodeDer.cc: Representation of entities in the TaQL parse tree
//# Copyright (C) 2005
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundatcd ion; either version 2 of the License, or (at your
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

//# Includes
#include <casacore/tables/TaQL/TaQLNodeDer.h>
#include <casacore/tables/TaQL/TaQLNodeVisitor.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/tables/Tables/TableError.h>
#include <iomanip>
#include <sstream>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


TaQLConstNodeRep::TaQLConstNodeRep (Bool value)
  : TaQLNodeRep (TaQLNode_Const),
    itsType        (CTBool),
    itsIsTableName (False),
    itsBValue      (value)
{}
TaQLConstNodeRep::TaQLConstNodeRep (Int64 value, Bool isTableName)
  : TaQLNodeRep (TaQLNode_Const),
    itsType        (CTInt),
    itsIsTableName (isTableName),
    itsIValue      (value),
    itsRValue      (value),
    itsCValue      (value, 0.) 
{}
TaQLConstNodeRep::TaQLConstNodeRep (Double value)
  : TaQLNodeRep (TaQLNode_Const),
    itsType        (CTReal),
    itsIsTableName (False),
    itsRValue      (value),
    itsCValue      (value, 0.)
{}
TaQLConstNodeRep::TaQLConstNodeRep (Double value, const String& unit)
  : TaQLNodeRep (TaQLNode_Const),
    itsType        (CTReal),
    itsIsTableName (False),
    itsRValue      (value),
    itsCValue      (value, 0.),
    itsUnit        (unit)
{}
TaQLConstNodeRep::TaQLConstNodeRep (DComplex value)
  : TaQLNodeRep (TaQLNode_Const),
    itsType        (CTComplex),
    itsIsTableName (False),
    itsCValue      (value)
{}
TaQLConstNodeRep::TaQLConstNodeRep (const String& value,
                                    Bool isTableName)
  : TaQLNodeRep (TaQLNode_Const),
    itsType        (CTString), 
    itsIsTableName (isTableName),
    itsSValue      (value)
{}
TaQLConstNodeRep::TaQLConstNodeRep (const MVTime& value)
  : TaQLNodeRep (TaQLNode_Const),
    itsType        (CTTime),
    itsIsTableName (False),
    itsRValue      (value),
    itsCValue      (value, 0.),
    itsTValue      (value)
{}
TaQLConstNodeRep::~TaQLConstNodeRep()
{}
const String& TaQLConstNodeRep::getString() const
{
  AlwaysAssert (itsType == CTString, AipsError);
  return itsSValue;
}
TaQLNodeResult TaQLConstNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitConstNode (*this);
}
void TaQLConstNodeRep::show (std::ostream& os) const
{
  // Output the possible unit in the same way as TaQLUnitNodeRep is doing.
  if (! itsUnit.empty()) {
    os << '(';
  }
  switch (itsType) {
  case CTBool:
    if (itsBValue) {
      os << 'T';
    } else {
      os << 'F';
    }
    break;
  case CTInt:
    if (itsIsTableName) {
      os << '$';
    }
    os << itsIValue;
    break;
  case CTReal:
    os << std::setprecision(16) << itsRValue;
    break;
  case CTComplex:
    if (itsCValue.real() != 0) {
      os << std::setprecision(16) << itsCValue.real() << '+';
    }
    os << std::setprecision(16) << itsCValue.imag() << 'i';
    break;
  case CTString:
    if (itsIsTableName) {
      /// NOTE: possible special characters in the string should be handled.
      os << itsSValue;
    } else {
      /// NOTE: possible quotes in the string should be handled.
      os << "'" << itsSValue << "'";
    }
    break;
  case CTTime:
    // 10 digits precision in the time
    os << MVTime::Format(MVTime::YMD, 10) << itsTValue;
    break;
  }
  if (! itsUnit.empty()) {
    os << ")'" << itsUnit << "'";
  }
}
void TaQLConstNodeRep::save (AipsIO& aio) const
{
  aio << char(itsType) << itsIsTableName << itsUnit;
  switch (itsType) {
  case CTBool:
    aio << itsBValue;
    break;
  case CTInt:
    aio << itsIValue;
    break;
  case CTReal:
    aio << itsRValue;
    break;
  case CTComplex:
    aio << itsCValue;
    break;
  case CTString:
    aio << itsSValue;
    break;
  case CTTime:
    aio << (double)itsTValue;
    break;
  }
}
TaQLConstNodeRep* TaQLConstNodeRep::restore (AipsIO& aio)
{
  char type;
  Bool isTableName;
  String unit;
  aio >> type >> isTableName >> unit;
  switch (type) {
  case CTBool:
    {
      Bool value;
      aio >> value;
      return new TaQLConstNodeRep (value);
    }
  case CTInt:
    {
      Int64 value;
      aio >> value;
      return new TaQLConstNodeRep (value, isTableName);
    }
  case CTReal:
    {
      Double value;
      aio >> value;
      return new TaQLConstNodeRep (value, unit);
    }
  case CTComplex:
    {
      DComplex value;
      aio >> value;
      return new TaQLConstNodeRep (value);
    }
  case CTString:
    {
      String value;
      aio >> value;
      return new TaQLConstNodeRep (value, isTableName);
    }
  case CTTime:
    {
      double v;
      aio >> v;
      return new TaQLConstNodeRep (MVTime(v));
    }
  }
  return 0;
}

TaQLRegexNodeRep::TaQLRegexNodeRep (const String& regex)
  : TaQLNodeRep (TaQLNode_Regex),
    itsCaseInsensitive (False),
    itsNegate          (False),
    itsIgnoreBlanks    (False),
    itsMaxDistance     (-1)
{
  Int sz = regex.size();
  AlwaysAssert (sz >= 4  &&  regex[sz-1] != ' ', AipsError);
  Int inx = 0;
  if (regex[0] == '!') {
    itsNegate = True;
    ++inx;
  }
  AlwaysAssert (regex[inx] == '~', AipsError);
  // Skip blanks.
  while (regex[++inx] == ' ') {}
  // Find regex qualifiers.
  while (--sz > inx) {
    if (regex[sz] == 'i') {
      itsCaseInsensitive = True;
    } else if (regex[sz] == 'b') {
      itsIgnoreBlanks = True;
    } else if (isdigit(regex[sz])) {
      int numend = sz;
      while (isdigit(regex[--sz])) {}
      ++sz;
      istringstream istr(regex.substr(sz, numend));
      istr >> itsMaxDistance;
    } else {
      break;
    }
  }
  ++sz;
  AlwaysAssert (sz-inx >= 3, AipsError);
  itsValue = regex.substr(inx, sz-inx);
  if (itsCaseInsensitive) {
    itsValue.downcase();
  }
}
TaQLRegexNodeRep::TaQLRegexNodeRep (const String& value,
                                    Bool caseInsensitive, Bool negate,
                                    Bool ignoreBlanks, Int maxDistance)
  : TaQLNodeRep (TaQLNode_Regex),
    itsValue           (value),
    itsCaseInsensitive (caseInsensitive),
    itsNegate          (negate),
    itsIgnoreBlanks    (ignoreBlanks),
    itsMaxDistance     (maxDistance)
{}
TaQLRegexNodeRep::~TaQLRegexNodeRep()
{}
TaQLNodeResult TaQLRegexNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitRegexNode (*this);
}
void TaQLRegexNodeRep::show (std::ostream& os) const
{
  if (itsNegate) {
    os << '!';
  }
  os << '~';
  os << itsValue;
  if (itsCaseInsensitive) {
    os << 'i';
  }
  if (itsIgnoreBlanks) {
    os << 'b';
  }
  if (itsMaxDistance >= 0) {
    os << itsMaxDistance;
  }
}
void TaQLRegexNodeRep::save (AipsIO& aio) const
{
  aio << itsValue << itsCaseInsensitive << itsNegate << itsIgnoreBlanks
      << itsMaxDistance;
}
TaQLRegexNodeRep* TaQLRegexNodeRep::restore (AipsIO& aio)
{
  String value;
  Bool caseInsensitive, negate, ignoreBlanks;
  Int maxDistance;
  aio >> value >> caseInsensitive >> negate >> ignoreBlanks
      >> maxDistance;
  return new TaQLRegexNodeRep (value, caseInsensitive, negate, ignoreBlanks,
                               maxDistance);
}


TaQLUnaryNodeRep::TaQLUnaryNodeRep (Type type, const TaQLNode& child)
  : TaQLNodeRep (TaQLNode_Unary),
    itsType  (type),
    itsChild (child)
{}
TaQLUnaryNodeRep::~TaQLUnaryNodeRep()
{}
TaQLNodeResult TaQLUnaryNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitUnaryNode (*this);
}
void TaQLUnaryNodeRep::show (std::ostream& os) const
{
  switch (itsType) {
  case U_MINUS:
    os << "-(";
    itsChild.show(os);
    os << ')';
    break;
  case U_NOT:
    os << "NOT(";
    itsChild.show(os);
    os << ')';
    break;
  case U_EXISTS:
    os << "EXISTS ";
    itsChild.show(os);
    break;
  case U_NOTEXISTS:
    os << "NOT EXISTS ";
    itsChild.show(os);
    break;
  case U_BITNOT:
    os << "~(";
    itsChild.show(os);
    os << ')';
    break;
  }
}
void TaQLUnaryNodeRep::save (AipsIO& aio) const
{
  aio << char(itsType);
  itsChild.saveNode (aio);
}
TaQLUnaryNodeRep* TaQLUnaryNodeRep::restore (AipsIO& aio)
{
  char ctype;
  aio >> ctype;
  TaQLUnaryNodeRep::Type type = (TaQLUnaryNodeRep::Type)ctype;
  TaQLNode node = TaQLNode::restoreNode (aio);
  return new TaQLUnaryNodeRep (type, node);
}

TaQLBinaryNodeRep::TaQLBinaryNodeRep (Type type, const TaQLNode& left,
                                      const TaQLNode& right)
  : TaQLNodeRep (TaQLNode_Binary),
    itsType  (type),
    itsLeft  (left),
    itsRight (right)
{}
TaQLBinaryNodeRep::~TaQLBinaryNodeRep()
{}
TaQLBinaryNodeRep* TaQLBinaryNodeRep::handleRegex (const TaQLNode& left,
						   const TaQLRegexNode& right)
{
  Type oper;
  if (right.negate()) {
    oper = B_NEREGEX;
  } else {
    oper = B_EQREGEX;
  }
  return new TaQLBinaryNodeRep (oper, left, right); 
}
TaQLNodeResult TaQLBinaryNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitBinaryNode (*this);
}
void TaQLBinaryNodeRep::show (std::ostream& os) const
{
  os << '(';
  itsLeft.show (os);
  os << ')';
  Bool paren = True;
  switch (itsType) {
  case B_PLUS:
    os << '+';
    break;
  case B_MINUS:
    os << '-';
    break;
  case B_TIMES:
    os << '*';
    break;
  case B_DIVIDE:
    os << '/';
    break;
  case B_DIVIDETRUNC:
    os << "//";
    break;
  case B_MODULO:
    os << '%';
    break;
  case B_POWER:
    os << "**";
    break;
  case B_OR:
    os << "||";
    break;
  case B_AND:
    os << "&&";
    break;
  case B_EQ:
    os << '=';
    break;
  case B_NE:
    os << "<>";
    break;
  case B_GT:
    os << '>';
    break;
  case B_GE:
    os << ">=";
    break;
  case B_LT:
    os << '<';
    break;
  case B_LE:
    os << "<=";
    break;
  case B_IN:
    paren = False;
    os << " IN ";
    break;
  case B_INDEX:
    paren = False;
    break;
  case B_EQREGEX:
  case B_NEREGEX:
    paren = False;
    break;
  case B_BITAND:
    os << '&';
    break;
  case B_BITXOR:
    os << '^';
    break;
  case B_BITOR:
    os << '|';
    break;
  }
  if (paren) {
    os << '(';
    itsRight.show (os);
    os << ')';
  } else {
    itsRight.show (os);
  }
}
void TaQLBinaryNodeRep::save (AipsIO& aio) const
{
  aio << char(itsType);
  itsLeft.saveNode (aio);
  itsRight.saveNode (aio);
}
TaQLBinaryNodeRep* TaQLBinaryNodeRep::restore (AipsIO& aio)
{
  char ctype;
  aio >> ctype;
  TaQLBinaryNodeRep::Type type = (TaQLBinaryNodeRep::Type)ctype;
  TaQLNode left = TaQLNode::restoreNode (aio);
  TaQLNode right = TaQLNode::restoreNode (aio);
  return new TaQLBinaryNodeRep (type, left, right);
}

TaQLMultiNodeRep::TaQLMultiNodeRep (Bool isSetOrArray)
  : TaQLNodeRep (TaQLNode_Multi),
    itsIsSetOrArray (isSetOrArray),
    itsSep          (","),
    itsIncr         (1)
{}
TaQLMultiNodeRep::TaQLMultiNodeRep(const String& prefix,
                                   const String& postfix,
                                   Bool isSetOrArray)
  : TaQLNodeRep (TaQLNode_Multi),
    itsIsSetOrArray (isSetOrArray),
    itsPrefix       (prefix),
    itsPostfix      (postfix),
    itsSep          (","),
    itsIncr         (1)
{}
TaQLMultiNodeRep::~TaQLMultiNodeRep()
{}
TaQLNodeResult TaQLMultiNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitMultiNode (*this);
}
void TaQLMultiNodeRep::show (std::ostream& os) const
{
  os << itsPrefix;
  for (uInt i=0; i<itsNodes.size(); ++i) {
    if (i != 0) {
      os << (i%itsIncr == 0  ?  itsSep : itsSep2);
    }
    itsNodes[i].show (os);
  }
  os << itsPostfix;
}
void TaQLMultiNodeRep::save (AipsIO& aio) const
{
  aio << itsIsSetOrArray << itsPrefix << itsPostfix
      << itsSep << itsSep2 << itsIncr;
  aio << uInt(itsNodes.size());
  for (uInt i=0; i<itsNodes.size(); ++i) {
    itsNodes[i].saveNode (aio);
  }
}
TaQLMultiNodeRep* TaQLMultiNodeRep::restore (AipsIO& aio)
{
  uInt size, incr;
  Bool isSetOrArray;
  String prefix, postfix, sep, sep2;
  aio >> isSetOrArray >> prefix >> postfix
      >> sep >> sep2 >> incr;
  aio >> size;
  TaQLMultiNodeRep* node = new TaQLMultiNodeRep(prefix, postfix, isSetOrArray);
  node->setSeparator (sep);
  node->setSeparator (incr, sep2);
  for (uInt i=0; i<size; ++i) {
    node->add (TaQLNode::restoreNode (aio));
  }
  return node;
}

TaQLFuncNodeRep::TaQLFuncNodeRep (const String& name)
  : TaQLNodeRep (TaQLNode_Func),
    itsName (name),
    itsArgs (False)
{}
TaQLFuncNodeRep::TaQLFuncNodeRep (const String& name,
                                  const TaQLMultiNode& args)
  : TaQLNodeRep (TaQLNode_Func),
    itsName (name),
    itsArgs (args)
{}
TaQLFuncNodeRep::~TaQLFuncNodeRep()
{}
TaQLNodeResult TaQLFuncNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitFuncNode (*this);
}
void TaQLFuncNodeRep::show (std::ostream& os) const
{
  os << itsName << '(';
  itsArgs.show (os);
  os << ')';
}
void TaQLFuncNodeRep::save (AipsIO& aio) const
{
  aio << itsName;
  itsArgs.saveNode (aio);
}
TaQLFuncNodeRep* TaQLFuncNodeRep::restore (AipsIO& aio)
{
  String name;
  aio >> name;
  return new TaQLFuncNodeRep (name, TaQLNode::restoreMultiNode (aio));
}

TaQLRangeNodeRep::TaQLRangeNodeRep (Bool leftClosed, TaQLNode start,
                                    const TaQLNode& end, Bool rightClosed)
  : TaQLNodeRep (TaQLNode_Range),
    itsLeftClosed (leftClosed),
    itsStart      (start),
    itsEnd        (end),
    itsRightClosed(rightClosed)
{}
TaQLRangeNodeRep::TaQLRangeNodeRep (Bool leftClosed, const TaQLNode& start)
  : TaQLNodeRep (TaQLNode_Range),
    itsLeftClosed (leftClosed),
    itsStart      (start),
    itsEnd        (),
    itsRightClosed(False)
{}
TaQLRangeNodeRep::TaQLRangeNodeRep (const TaQLNode& end, Bool rightClosed)
  : TaQLNodeRep (TaQLNode_Range),
    itsLeftClosed (False),
    itsStart      (),
    itsEnd        (end),
    itsRightClosed(rightClosed)
{}
TaQLRangeNodeRep::~TaQLRangeNodeRep()
{}
TaQLNodeResult TaQLRangeNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitRangeNode (*this);
}
void TaQLRangeNodeRep::show (std::ostream& os) const
{
  if (itsLeftClosed) {
    os << '{';
  } else {
    os << '<';
  }
  itsStart.show (os);
  os << ',';
  itsEnd.show (os);
  if (itsRightClosed) {
    os << '}';
  } else {
    os << '>';
  }
}
void TaQLRangeNodeRep::save (AipsIO& aio) const
{
  aio << itsLeftClosed << itsRightClosed;
  itsStart.saveNode (aio);
  itsEnd.saveNode (aio);
}
TaQLRangeNodeRep* TaQLRangeNodeRep::restore (AipsIO& aio)
{
  Bool leftClosed, rightClosed;
  aio >> leftClosed >> rightClosed;
  TaQLNode start = TaQLNode::restoreNode (aio);
  TaQLNode end = TaQLNode::restoreNode (aio);
  return new TaQLRangeNodeRep (leftClosed, start, end, rightClosed);
}

TaQLIndexNodeRep::TaQLIndexNodeRep (const TaQLNode& start,
                                    const TaQLNode& end,
                                    const TaQLNode& incr)
  : TaQLNodeRep (TaQLNode_Index),
    itsStart (start),
    itsEnd   (end),
    itsIncr  (incr)
{}
TaQLIndexNodeRep::~TaQLIndexNodeRep()
{}
TaQLNodeResult TaQLIndexNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitIndexNode (*this);
}
void TaQLIndexNodeRep::show (std::ostream& os) const
{
  itsStart.show (os);
  if (itsEnd.isValid()) {
    os << ':';
    itsEnd.show (os);
  } else if (itsIncr.isValid()) {
    os << ':';
  }
  if (itsIncr.isValid()) {
    os << ':';
    itsIncr.show (os);
  }    
}
void TaQLIndexNodeRep::save (AipsIO& aio) const
{
  itsStart.saveNode (aio);
  itsEnd.saveNode (aio);
  itsIncr.saveNode (aio);
}
TaQLIndexNodeRep* TaQLIndexNodeRep::restore (AipsIO& aio)
{
  TaQLNode start = TaQLNode::restoreNode (aio);
  TaQLNode end = TaQLNode::restoreNode (aio);
  TaQLNode incr = TaQLNode::restoreNode (aio);
  return new TaQLIndexNodeRep (start, end, incr);
}

TaQLJoinNodeRep::TaQLJoinNodeRep (const TaQLMultiNode& tables,
                                  const TaQLNode& condition)
  : TaQLNodeRep (TaQLNode_Join),
    itsTables    (tables),
    itsCondition (condition)
{}
TaQLJoinNodeRep::~TaQLJoinNodeRep()
{}
TaQLNodeResult TaQLJoinNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitJoinNode (*this);
}
void TaQLJoinNodeRep::show (std::ostream& os) const
{
  os << "JOIN ";
  if (itsTables.isValid()) {
    itsTables.show (os);
    os << ' ';
  }
  os << "ON CONDITION ";
  itsCondition.show (os);
}
void TaQLJoinNodeRep::save (AipsIO& aio) const
{
  itsTables.saveNode (aio);
  itsCondition.saveNode (aio);
}
TaQLJoinNodeRep* TaQLJoinNodeRep::restore (AipsIO& aio)
{
  TaQLMultiNode tables = TaQLNode::restoreMultiNode (aio);
  TaQLNode condition = TaQLNode::restoreNode (aio);
  return new TaQLJoinNodeRep (tables, condition);
}

TaQLKeyColNodeRep::TaQLKeyColNodeRep (const String& name,
                                      const String& nameMask)
: TaQLNodeRep (TaQLNode_KeyCol),
  itsName     (name),
  itsNameMask (nameMask)
{}
TaQLKeyColNodeRep::~TaQLKeyColNodeRep()
{}
TaQLNodeResult TaQLKeyColNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitKeyColNode (*this);
}
void TaQLKeyColNodeRep::show (std::ostream& os) const
{
  if (itsNameMask.empty()) {
    os << itsName;
  } else {
    os << '(' << itsName << ',' << itsNameMask << ')';
  }
}
void TaQLKeyColNodeRep::save (AipsIO& aio) const
{
  aio << itsName << itsNameMask;
}
TaQLKeyColNodeRep* TaQLKeyColNodeRep::restore (AipsIO& aio)
{
  String name, nameMask;
  aio >> name >> nameMask;
  return new TaQLKeyColNodeRep (name, nameMask);
}

TaQLTableNodeRep::TaQLTableNodeRep (const TaQLNode& table,
                                    const String& alias)
  : TaQLNodeRep (TaQLNode_Table),
    itsTable (table),
    itsAlias (alias)
{}
TaQLTableNodeRep::~TaQLTableNodeRep()
{}
TaQLNodeResult TaQLTableNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitTableNode (*this);
}
void TaQLTableNodeRep::show (std::ostream& os) const
{
  itsTable.show (os);
  if (! itsAlias.empty()) {
    os << " AS " << itsAlias;
  }
}
void TaQLTableNodeRep::save (AipsIO& aio) const
{
  aio << itsAlias;
  itsTable.saveNode (aio);
}
TaQLTableNodeRep* TaQLTableNodeRep::restore (AipsIO& aio)
{
  String alias;
  aio >> alias;
  return new TaQLTableNodeRep (TaQLNode::restoreNode(aio), alias);
}

TaQLColNodeRep::TaQLColNodeRep (const TaQLNode& expr, const String& name,
                                const String& nameMask, const String& dtype)
  : TaQLNodeRep (TaQLNode_Col),
    itsExpr     (expr),
    itsName     (name),
    itsNameMask (nameMask),
    itsDtype    (checkDataType(dtype))
{}
TaQLColNodeRep::~TaQLColNodeRep()
{}
TaQLNodeResult TaQLColNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitColNode (*this);
}
void TaQLColNodeRep::show (std::ostream& os) const
{
  itsExpr.show (os);
  if (! itsName.empty()) {
    os << " AS " ;
    if (itsNameMask.empty()) {
      os << itsName;
    } else {
      os << '(' << itsName << ',' << itsNameMask << ')';
    }
    if (! itsDtype.empty()) {
      os << ' ' << itsDtype;
    }
  }
}
void TaQLColNodeRep::save (AipsIO& aio) const
{
  aio << itsName << itsNameMask << itsDtype;
  itsExpr.saveNode (aio);
}
TaQLColNodeRep* TaQLColNodeRep::restore (AipsIO& aio)
{
  String name, nameMask, dtype;
  aio >> name >> nameMask >> dtype;
  TaQLColNodeRep* node = new TaQLColNodeRep (TaQLNode::restoreNode(aio),
					     name, nameMask, dtype);
  return node;
}

TaQLColumnsNodeRep::TaQLColumnsNodeRep (Bool distinct,
                                        const TaQLMultiNode& nodes)
  : TaQLNodeRep (TaQLNode_Columns),
    itsDistinct (distinct),
    itsNodes    (nodes)
{}
TaQLColumnsNodeRep::~TaQLColumnsNodeRep()
{}
TaQLNodeResult TaQLColumnsNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitColumnsNode (*this);
}
void TaQLColumnsNodeRep::show (std::ostream& os) const
{
  if (itsDistinct) {
    os << " DISTINCT";
  }
  if (itsNodes.isValid()) {
    os << ' ';
    itsNodes.show (os);
  }
}
void TaQLColumnsNodeRep::save (AipsIO& aio) const
{
  aio << itsDistinct;
  itsNodes.saveNode (aio);
}
TaQLColumnsNodeRep* TaQLColumnsNodeRep::restore (AipsIO& aio)
{
  Bool distinct;
  aio >> distinct;
  return new TaQLColumnsNodeRep (distinct, TaQLNode::restoreMultiNode(aio));
}

TaQLGroupNodeRep::TaQLGroupNodeRep (Type type, const TaQLMultiNode& nodes)
  : TaQLNodeRep (TaQLNode_Groupby),
    itsType  (type),
    itsNodes (nodes)
{}
TaQLGroupNodeRep::~TaQLGroupNodeRep()
{}
TaQLNodeResult TaQLGroupNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitGroupNode (*this);
}
void TaQLGroupNodeRep::show (std::ostream& os) const
{
  os << " GROUPBY";
  if (itsType == Rollup) {
    os << " ROLLUP";
  }
  os << ' ';
  itsNodes.show (os);
}
void TaQLGroupNodeRep::save (AipsIO& aio) const
{
  aio << char(itsType);
  itsNodes.saveNode (aio);
}
TaQLGroupNodeRep* TaQLGroupNodeRep::restore (AipsIO& aio)
{
  char ctype;
  aio >> ctype;
  TaQLGroupNodeRep::Type type = (TaQLGroupNodeRep::Type)ctype;
  return new TaQLGroupNodeRep (type, TaQLNode::restoreMultiNode(aio));
}

TaQLSortKeyNodeRep::TaQLSortKeyNodeRep (Type type, const TaQLNode& child)
  : TaQLNodeRep (TaQLNode_SortKey),
    itsType  (type),
    itsChild (child)
{}
TaQLSortKeyNodeRep::~TaQLSortKeyNodeRep()
{}
TaQLNodeResult TaQLSortKeyNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitSortKeyNode (*this);
}
void TaQLSortKeyNodeRep::show (std::ostream& os) const
{
  itsChild.show (os);
  switch (itsType) {
  case Ascending:
    os << " ASC";
    break;
  case Descending:
    os << " DESC";
    break;
  case None:
    break;
  }
}
void TaQLSortKeyNodeRep::save (AipsIO& aio) const
{
  aio << char(itsType);
  itsChild.saveNode (aio);
}
TaQLSortKeyNodeRep* TaQLSortKeyNodeRep::restore (AipsIO& aio)
{
  char ctype;
  aio >> ctype;
  TaQLSortKeyNodeRep::Type type = (TaQLSortKeyNodeRep::Type)ctype;
  return new TaQLSortKeyNodeRep (type, TaQLNode::restoreNode(aio));
}

TaQLSortNodeRep::TaQLSortNodeRep (Bool unique, Type type,
                                  const TaQLMultiNode& keys)
  : TaQLNodeRep (TaQLNode_Sort),
    itsUnique (unique),
    itsType   (type),
    itsKeys   (keys)
{}
TaQLSortNodeRep::~TaQLSortNodeRep()
{}
TaQLNodeResult TaQLSortNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitSortNode (*this);
}
void TaQLSortNodeRep::show (std::ostream& os) const
{
  os << " ORDERBY";
  if (itsUnique) {
    os << " UNIQUE";
  }
  if (itsType == Descending) {
    os << " DESC";
  }
  os << ' ';
  itsKeys.show (os);
}
void TaQLSortNodeRep::save (AipsIO& aio) const
{
  aio << itsUnique << char(itsType);
  itsKeys.saveNode (aio);
}
TaQLSortNodeRep* TaQLSortNodeRep::restore (AipsIO& aio)
{
  Bool unique;
  char ctype;
  aio >> unique >> ctype;
  TaQLSortNodeRep::Type type = (TaQLSortNodeRep::Type)ctype;
  return new TaQLSortNodeRep (unique, type, TaQLNode::restoreMultiNode(aio));
}

TaQLLimitOffNodeRep::TaQLLimitOffNodeRep (const TaQLNode& limit,
                                          const TaQLNode& offset)
  : TaQLNodeRep (TaQLNode_LimitOff),
    itsLimit (limit),
    itsOffset (offset)
{}
TaQLLimitOffNodeRep::~TaQLLimitOffNodeRep()
{}
TaQLNodeResult TaQLLimitOffNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitLimitOffNode (*this);
}
void TaQLLimitOffNodeRep::show (std::ostream& os) const
{
  if (itsLimit.isValid()) {
    os << " LIMIT ";
    itsLimit.show (os);
  }
  if (itsOffset.isValid()) {
    os << " OFFSET ";
    itsOffset.show (os);
  }
}
void TaQLLimitOffNodeRep::save (AipsIO& aio) const
{
  itsLimit.saveNode (aio);
  itsOffset.saveNode (aio);
}
TaQLLimitOffNodeRep* TaQLLimitOffNodeRep::restore (AipsIO& aio)
{
  TaQLNode limit = TaQLNode::restoreNode (aio);
  TaQLNode offset = TaQLNode::restoreNode (aio);
  return new TaQLLimitOffNodeRep (limit, offset);
}

TaQLGivingNodeRep::TaQLGivingNodeRep (const String& name,
                                      const TaQLMultiNode& type)
  : TaQLNodeRep (TaQLNode_Giving),
    itsName     (name),
    itsType     (type)
{}
TaQLGivingNodeRep::TaQLGivingNodeRep (const TaQLMultiNode& exprlist)
  : TaQLNodeRep (TaQLNode_Giving),
    itsExprList (exprlist)
{}
TaQLGivingNodeRep::~TaQLGivingNodeRep()
{}
TaQLNodeResult TaQLGivingNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitGivingNode (*this);
}
void TaQLGivingNodeRep::show (std::ostream& os) const
{
  if (itsExprList.isValid()) {
    itsExprList.show (os);
  } else {
    os << itsName;
    if (itsType.isValid()) {
      os << " AS ";
      itsType.show (os);
    }
  }
}
void TaQLGivingNodeRep::save (AipsIO& aio) const
{
  itsExprList.saveNode (aio);
  if (! itsExprList.isValid()) {
    aio << itsName;
    itsType.saveNode (aio);
  }
}
TaQLGivingNodeRep* TaQLGivingNodeRep::restore (AipsIO& aio)
{
  TaQLMultiNode node = TaQLNode::restoreMultiNode(aio);
  if (node.isValid()) {
    return new TaQLGivingNodeRep (node);
  }
  String name;
  aio >> name;
  TaQLMultiNode type = TaQLNode::restoreMultiNode (aio);
  return new TaQLGivingNodeRep (name, type);
}

TaQLUpdExprNodeRep::TaQLUpdExprNodeRep (const String& name,
                                        const String& nameMask,
                                        const TaQLNode& expr)
  : TaQLNodeRep (TaQLNode_UpdExpr),
    itsName     (name),
    itsNameMask (nameMask),
    itsExpr     (expr)
{}
TaQLUpdExprNodeRep::TaQLUpdExprNodeRep (const String& name,
                                        const String& nameMask,
                                        const TaQLMultiNode& indices,
                                        const TaQLNode& expr)
  : TaQLNodeRep (TaQLNode_UpdExpr),
    itsName     (name),
    itsNameMask (nameMask),
    itsIndices1 (indices),
    itsExpr     (expr)
{}
TaQLUpdExprNodeRep::TaQLUpdExprNodeRep (const String& name,
                                        const String& nameMask,
                                        const TaQLMultiNode& indices1,
                                        const TaQLMultiNode& indices2,
                                        const TaQLNode& expr)
  : TaQLNodeRep (TaQLNode_UpdExpr),
    itsName     (name),
    itsNameMask (nameMask),
    itsIndices1 (indices1),
    itsIndices2 (indices2),
    itsExpr     (expr)
{}
TaQLUpdExprNodeRep::~TaQLUpdExprNodeRep()
{}
TaQLNodeResult TaQLUpdExprNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitUpdExprNode (*this);
}
void TaQLUpdExprNodeRep::show (std::ostream& os) const
{
  if (itsNameMask.empty()) {
    os << itsName;
  } else {
    os << '(' << itsName << ',' << itsNameMask << ')';
  }
  itsIndices1.show (os);
  itsIndices2.show (os);
  os << '=';
  itsExpr.show (os);
}
void TaQLUpdExprNodeRep::save (AipsIO& aio) const
{
  aio << itsName << itsNameMask;
  itsIndices1.saveNode (aio);
  itsIndices2.saveNode (aio);
  itsExpr.saveNode (aio);
}
TaQLUpdExprNodeRep* TaQLUpdExprNodeRep::restore (AipsIO& aio)
{
  String name, nameMask;
  aio >> name >> nameMask;
  TaQLMultiNode indices1 = TaQLNode::restoreMultiNode (aio);
  TaQLMultiNode indices2 = TaQLNode::restoreMultiNode (aio);
  TaQLNode expr = TaQLNode::restoreNode (aio);
  return new TaQLUpdExprNodeRep (name, nameMask, indices1, indices2, expr);
}

TaQLQueryNodeRep::TaQLQueryNodeRep (int nodeType)
  : TaQLNodeRep    (nodeType),
    itsBrackets    (False),
    itsNoExecute   (False),
    itsFromExecute (False)
{}
TaQLQueryNodeRep::~TaQLQueryNodeRep()
{}
void TaQLQueryNodeRep::show (std::ostream& os) const
{
  if (itsBrackets) {
    os << '[';
  }
  showDerived (os);
  if (itsBrackets) {
    os << ']';
  }
}
void TaQLQueryNodeRep::saveSuper (AipsIO& aio) const
{
  aio << itsBrackets << itsNoExecute << itsFromExecute;
}
void TaQLQueryNodeRep::restoreSuper (AipsIO& aio)
{
  aio >> itsBrackets >> itsNoExecute >> itsFromExecute;
}

TaQLSelectNodeRep::TaQLSelectNodeRep (const TaQLNode& columns,
                                      const TaQLNode& where,
                                      const TaQLNode& groupby,
                                      const TaQLNode& having,
                                      const TaQLNode& sort,
                                      const TaQLNode& limitoff,
                                      const TaQLNode& giving,
                                      const TaQLMultiNode& dminfo)
  : TaQLQueryNodeRep (TaQLNode_Select),
    itsColumns(columns),
    itsWhere(where), itsGroupby(groupby), itsHaving(having),
    itsSort(sort), itsLimitOff(limitoff), itsGiving(giving),
    itsDMInfo(dminfo)
{} 
TaQLSelectNodeRep::TaQLSelectNodeRep (const TaQLNode& columns,
                                      const TaQLMultiNode& tables,
                                      const TaQLNode& join,
                                      const TaQLNode& where,
                                      const TaQLNode& groupby,
                                      const TaQLNode& having,
                                      const TaQLNode& sort,
                                      const TaQLNode& limitoff,
                                      const TaQLNode& giving,
                                      const TaQLMultiNode& dminfo)
  : TaQLQueryNodeRep (TaQLNode_Select),
    itsColumns(columns), itsTables(tables), itsJoin(join),
    itsWhere(where), itsGroupby(groupby), itsHaving(having),
    itsSort(sort), itsLimitOff(limitoff), itsGiving(giving),
    itsDMInfo(dminfo)
{}
TaQLSelectNodeRep::~TaQLSelectNodeRep()
{}
TaQLNodeResult TaQLSelectNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitSelectNode (*this);
}
void TaQLSelectNodeRep::showDerived (std::ostream& os) const
{
  os << "SELECT";
  itsColumns.show (os);
  if (itsTables.isValid()) {
    os << " FROM ";
    itsTables.show (os);
  }
  itsJoin.show (os);
  if (itsWhere.isValid()) {
    os << " WHERE ";
    itsWhere.show (os);
  }
  if (itsGroupby.isValid()) {
    itsGroupby.show (os);
  }
  if (itsHaving.isValid()) {
    os << " HAVING ";
    itsHaving.show (os);
  }
  itsSort.show (os);
  itsLimitOff.show (os);
  if (itsGiving.isValid()) {
    os << " GIVING ";
    itsGiving.show (os);
  }
  if (itsDMInfo.isValid()) {
    os << " DMINFO ";
    itsDMInfo.show (os);
  }
}
void TaQLSelectNodeRep::save (AipsIO& aio) const
{
  itsColumns.saveNode (aio);
  itsTables.saveNode (aio);
  itsJoin.saveNode (aio);
  itsWhere.saveNode (aio);
  itsGroupby.saveNode (aio);
  itsHaving.saveNode (aio);
  itsSort.saveNode (aio);
  itsLimitOff.saveNode (aio);
  itsGiving.saveNode (aio);
  itsDMInfo.saveNode (aio);
  saveSuper (aio);
}
TaQLSelectNodeRep* TaQLSelectNodeRep::restore (AipsIO& aio)
{
  TaQLNode columns = TaQLNode::restoreNode (aio);
  TaQLMultiNode tables = TaQLNode::restoreMultiNode (aio);
  TaQLNode join = TaQLNode::restoreMultiNode (aio);
  TaQLNode where = TaQLNode::restoreNode (aio);
  TaQLNode groupby = TaQLNode::restoreNode (aio);
  TaQLNode having = TaQLNode::restoreNode (aio);
  TaQLNode sort = TaQLNode::restoreNode (aio);
  TaQLNode limitoff = TaQLNode::restoreNode (aio);
  TaQLNode giving = TaQLNode::restoreNode (aio);
  TaQLMultiNode dminfo = TaQLNode::restoreMultiNode (aio);
  TaQLSelectNodeRep* node = new TaQLSelectNodeRep (columns, tables, join,
						   where, groupby, having,
						   sort, limitoff, giving,
                                                   dminfo);
  node->restoreSuper (aio);
  return node;
}

TaQLCountNodeRep::TaQLCountNodeRep (const TaQLNode& columns,
                                    const TaQLMultiNode& tables,
                                    const TaQLNode& where)
  : TaQLQueryNodeRep (TaQLNode_Count),
    itsColumns(columns), itsTables(tables), itsWhere(where)
{}
TaQLCountNodeRep::~TaQLCountNodeRep()
{}
TaQLNodeResult TaQLCountNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitCountNode (*this);
}
void TaQLCountNodeRep::showDerived (std::ostream& os) const
{
  os << "COUNT ";
  itsColumns.show (os);
  os << " FROM ";
  itsTables.show (os);
  if (itsWhere.isValid()) {
    os << " WHERE ";
    itsWhere.show (os);
  }
}
void TaQLCountNodeRep::save (AipsIO& aio) const
{
  itsColumns.saveNode (aio);
  itsTables.saveNode (aio);
  itsWhere.saveNode (aio);
  saveSuper (aio);
}
TaQLCountNodeRep* TaQLCountNodeRep::restore (AipsIO& aio)
{
  TaQLNode columns = TaQLNode::restoreNode (aio);
  TaQLMultiNode tables = TaQLNode::restoreMultiNode (aio);
  TaQLNode where = TaQLNode::restoreNode (aio);
  TaQLCountNodeRep* node = new TaQLCountNodeRep (columns, tables, where);
  node->restoreSuper (aio);
  return node;
}

TaQLUpdateNodeRep::TaQLUpdateNodeRep (const TaQLMultiNode& tables,
                                      const TaQLMultiNode& update,
                                      const TaQLMultiNode& from,
                                      const TaQLNode& where,
                                      const TaQLNode& sort,
                                      const TaQLNode& limitoff)
  : TaQLNodeRep (TaQLNode_Update),
    itsTables   (tables),
    itsUpdate   (update),
    itsFrom     (from),
    itsWhere    (where),
    itsSort     (sort),
    itsLimitOff (limitoff)
{}
TaQLUpdateNodeRep::~TaQLUpdateNodeRep()
{}
TaQLNodeResult TaQLUpdateNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitUpdateNode (*this);
}
void TaQLUpdateNodeRep::show (std::ostream& os) const
{
  os << "UPDATE ";
  itsTables.show (os);
  os << " SET ";
  itsUpdate.show (os);
  if (itsFrom.isValid()) {
    os << " FROM ";
    itsFrom.show (os);
  }
  if (itsWhere.isValid()) {
    os << " WHERE ";
    itsWhere.show (os);
  }
  itsSort.show (os);
  itsLimitOff.show (os);
}
void TaQLUpdateNodeRep::save (AipsIO& aio) const
{
  itsTables.saveNode (aio);
  itsUpdate.saveNode (aio);
  itsFrom.saveNode (aio);
  itsWhere.saveNode (aio);
  itsSort.saveNode (aio);
  itsLimitOff.saveNode (aio);
}
TaQLUpdateNodeRep* TaQLUpdateNodeRep::restore (AipsIO& aio)
{
  TaQLMultiNode tables = TaQLNode::restoreMultiNode (aio);
  TaQLMultiNode update = TaQLNode::restoreMultiNode (aio);
  TaQLMultiNode from = TaQLNode::restoreMultiNode (aio);
  TaQLNode where = TaQLNode::restoreNode (aio);
  TaQLNode sort = TaQLNode::restoreNode (aio);
  TaQLNode limitoff = TaQLNode::restoreNode (aio);
  return new TaQLUpdateNodeRep (tables, update, from, where, sort, limitoff);
}

TaQLInsertNodeRep::TaQLInsertNodeRep (const TaQLMultiNode& tables,
                                      const TaQLMultiNode& columns,
                                      const TaQLNode& values,
                                      const TaQLNode& limit)
  : TaQLNodeRep (TaQLNode_Insert),
    itsTables  (tables),
    itsColumns (columns),
    itsValues  (values),
    itsLimit   (limit)
{}
TaQLInsertNodeRep::TaQLInsertNodeRep (const TaQLMultiNode& tables,
                                      const TaQLMultiNode& insert)
  : TaQLNodeRep (TaQLNode_Insert),
    itsTables  (tables),
    itsColumns (False)
{
  // Convert the list of column=value expressions like
  //        SET col1=val1, col2=val2
  // to a list of columns and a list of values like
  //        [col1,col2] VALUES [val1,val2].
  TaQLMultiNode values(False);
  values.setPPFix ("VALUES [", "]");
  // The nodes in the list are of type TaQLUpdExprNodeRep.
  const std::vector<TaQLNode>& nodes = insert.getMultiRep()->getNodes();
  for (uInt i=0; i<nodes.size(); ++i) {
    const TaQLUpdExprNodeRep* rep = dynamic_cast<const TaQLUpdExprNodeRep*>
      (nodes[i].getRep());
    AlwaysAssert (rep, AipsError);
    if (rep->itsIndices1.isValid()) {
      throw TableInvExpr ("Column indices cannot be given in an "
                          "INSERT command");
    }
    // Add the column name and value expression.
    itsColumns.add (new TaQLKeyColNodeRep (rep->itsName));
    values.add (rep->itsExpr);
  }
  itsValues = values;
}
TaQLInsertNodeRep::~TaQLInsertNodeRep()
{}
TaQLNodeResult TaQLInsertNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitInsertNode (*this);
}
void TaQLInsertNodeRep::show (std::ostream& os) const
{
  os << "INSERT";
  if (itsLimit.isValid()) {
    os << " LIMIT ";
    itsLimit.show (os);
  }
  os << " INTO ";
  itsTables.show (os);
  if (itsColumns.isValid()) {
    os << " [";
    itsColumns.show (os);
    os << ']';
  }
  os << ' ';
  itsValues.show (os);
}
void TaQLInsertNodeRep::save (AipsIO& aio) const
{
  itsTables.saveNode (aio);
  itsColumns.saveNode (aio);
  itsValues.saveNode (aio);
  itsLimit.saveNode (aio);
}
TaQLInsertNodeRep* TaQLInsertNodeRep::restore (AipsIO& aio)
{
  TaQLMultiNode tables = TaQLNode::restoreMultiNode (aio);
  TaQLMultiNode columns = TaQLNode::restoreMultiNode (aio);
  TaQLNode values = TaQLNode::restoreNode (aio);
  TaQLNode limit  = TaQLNode::restoreNode (aio);
  return new TaQLInsertNodeRep (tables, columns, values, limit);
}

TaQLDeleteNodeRep::TaQLDeleteNodeRep (const TaQLMultiNode& tables,
                                      const TaQLNode& where,
                                      const TaQLNode& sort,
                                      const TaQLNode& limitoff)
  : TaQLNodeRep (TaQLNode_Delete),
    itsTables   (tables),
    itsWhere    (where),
    itsSort     (sort),
    itsLimitOff (limitoff)
{}
TaQLDeleteNodeRep::~TaQLDeleteNodeRep()
{}
TaQLNodeResult TaQLDeleteNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitDeleteNode (*this);
}
void TaQLDeleteNodeRep::show (std::ostream& os) const
{
  os << "DELETE FROM ";
  itsTables.show (os);
  if (itsWhere.isValid()) {
    os << " WHERE ";
    itsWhere.show (os);
  }
  itsSort.show (os);
  itsLimitOff.show (os);
}
void TaQLDeleteNodeRep::save (AipsIO& aio) const
{
  itsTables.saveNode (aio);
  itsWhere.saveNode (aio);
  itsSort.saveNode (aio);
  itsLimitOff.saveNode (aio);
}
TaQLDeleteNodeRep* TaQLDeleteNodeRep::restore (AipsIO& aio)
{
  TaQLMultiNode tables = TaQLNode::restoreMultiNode (aio);
  TaQLNode where = TaQLNode::restoreNode (aio);
  TaQLNode sort = TaQLNode::restoreNode (aio);
  TaQLNode limitoff = TaQLNode::restoreNode (aio);
  return new TaQLDeleteNodeRep (tables, where, sort, limitoff);
}

TaQLCalcNodeRep::TaQLCalcNodeRep (const TaQLMultiNode& tables,
                                  const TaQLNode& expr,
                                  const TaQLNode& where,
                                  const TaQLNode& sort,
                                  const TaQLNode& limitoff)
  : TaQLNodeRep (TaQLNode_Calc),
    itsTables   (tables),
    itsExpr     (expr),
    itsWhere    (where),
    itsSort     (sort),
    itsLimitOff (limitoff)
{}
TaQLCalcNodeRep::~TaQLCalcNodeRep()
{}
TaQLNodeResult TaQLCalcNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitCalcNode (*this);
}
void TaQLCalcNodeRep::show (std::ostream& os) const
{
  os << "CALC ";
  itsExpr.show (os);
  if (itsTables.isValid()) {
    os << " FROM ";
    itsTables.show (os);
  }
  if (itsWhere.isValid()) {
    os << " WHERE ";
    itsWhere.show (os);
  }
  itsSort.show (os);
  itsLimitOff.show (os);
}
void TaQLCalcNodeRep::save (AipsIO& aio) const
{
  itsTables.saveNode (aio);
  itsExpr.saveNode (aio);
  itsWhere.saveNode (aio);
  itsSort.saveNode (aio);
  itsLimitOff.saveNode (aio);
}
TaQLCalcNodeRep* TaQLCalcNodeRep::restore (AipsIO& aio)
{
  TaQLMultiNode tables = TaQLNode::restoreMultiNode (aio);
  TaQLNode expr = TaQLNode::restoreNode (aio);
  TaQLNode where = TaQLNode::restoreNode (aio);
  TaQLNode sort = TaQLNode::restoreNode (aio);
  TaQLNode limitoff = TaQLNode::restoreNode (aio);
  return new TaQLCalcNodeRep (tables, expr, where, sort, limitoff);
}

TaQLCreTabNodeRep::TaQLCreTabNodeRep (const TaQLNode& giving,
                                      const TaQLMultiNode& cols,
                                      const TaQLNode& limit,
                                      const TaQLMultiNode& dminfo)
  : TaQLQueryNodeRep (TaQLNode_CreTab),
    itsGiving  (giving),
    itsColumns (cols),
    itsLimit   (limit),
    itsDMInfo  (dminfo)
{}
TaQLCreTabNodeRep::~TaQLCreTabNodeRep()
{}
TaQLNodeResult TaQLCreTabNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitCreTabNode (*this);
}
void TaQLCreTabNodeRep::showDerived (std::ostream& os) const
{
  os << "CREATE TABLE ";
  itsGiving.show (os);
  os << ' ';
  itsColumns.show (os);
  if (itsLimit.isValid()) {
    os << " LIMIT ";
    itsLimit.show (os);
  }
  if (itsDMInfo.isValid()) {
    os << " DMINFO ";
    itsDMInfo.show (os);
  }
}
void TaQLCreTabNodeRep::save (AipsIO& aio) const
{
  itsGiving.saveNode (aio);
  itsColumns.saveNode (aio);
  itsLimit.saveNode (aio);
  itsDMInfo.saveNode (aio);
  saveSuper (aio);
}
TaQLCreTabNodeRep* TaQLCreTabNodeRep::restore (AipsIO& aio)
{
  TaQLNode giving = TaQLNode::restoreNode (aio);
  TaQLMultiNode columns = TaQLNode::restoreMultiNode (aio);
  TaQLNode limit = TaQLNode::restoreNode (aio);
  TaQLMultiNode dminfo = TaQLNode::restoreMultiNode (aio);
  TaQLCreTabNodeRep* node = new TaQLCreTabNodeRep (giving, columns,
                                                   limit, dminfo);
  node->restoreSuper (aio);
  return node;
}

TaQLColSpecNodeRep::TaQLColSpecNodeRep (const String& name,
                                        const String& dtype,
                                        const TaQLMultiNode& spec)
  : TaQLNodeRep (TaQLNode_ColSpec),
    itsName  (name),
    itsDtype (checkDataType(dtype)),
    itsSpec  (spec)
{}
TaQLColSpecNodeRep::~TaQLColSpecNodeRep()
{}
TaQLNodeResult TaQLColSpecNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitColSpecNode (*this);
}
void TaQLColSpecNodeRep::show (std::ostream& os) const
{
  os << itsName;
  if (! itsDtype.empty()) {
    os << ' ' << itsDtype;
  }
  if (itsSpec.isValid()) {
    os << ' ';
    itsSpec.show (os);
  }
}
void TaQLColSpecNodeRep::save (AipsIO& aio) const
{
  aio << itsName << itsDtype;
  itsSpec.saveNode (aio);
}
TaQLColSpecNodeRep* TaQLColSpecNodeRep::restore (AipsIO& aio)
{
  String name, dtype;
  aio >> name >> dtype;
  TaQLMultiNode spec = TaQLNode::restoreMultiNode (aio);
  return new TaQLColSpecNodeRep (name, dtype, spec);
}

TaQLRecFldNodeRep::TaQLRecFldNodeRep (const String& name,
                                      const TaQLNode& values,
                                      const String& dtype)
  : TaQLNodeRep (TaQLNode_RecFld),
    itsName  (name),
    itsDtype (checkDataType(dtype)),
    itsValues(values)
{}
TaQLRecFldNodeRep::TaQLRecFldNodeRep (const String& name,
                                      const TaQLRecFldNodeRep& node)
  : TaQLNodeRep (TaQLNode_RecFld),
    itsName  (name),
    itsDtype (node.itsDtype),
    itsValues(node.itsValues)
{}
TaQLRecFldNodeRep::TaQLRecFldNodeRep (const String& name,
                                      const String& fromName,
                                      const String& dtype)
  : TaQLNodeRep (TaQLNode_RecFld),
    itsName     (name),
    itsFromName (fromName),
    itsDtype    (checkDataType(dtype))
{}
TaQLRecFldNodeRep::~TaQLRecFldNodeRep()
{}
TaQLNodeResult TaQLRecFldNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitRecFldNode (*this);
}
void TaQLRecFldNodeRep::show (std::ostream& os) const
{
  if (! itsName.empty()) {
    os << itsName << '=';
  }
  if (! itsFromName.empty()) {
    os << itsFromName;
  } else if (itsValues.isValid()) {
    if (itsValues.nodeType() == TaQLNode_Multi  &&
        ((const TaQLMultiNodeRep*)(itsValues.getRep()))->itsNodes.empty()) {
      os << "[=]";
    } else {
      itsValues.show (os);
    }
  } else {
    os << "[]";
  }
  if (! itsDtype.empty()) {
    os << " AS " << itsDtype;
  }
}
void TaQLRecFldNodeRep::save (AipsIO& aio) const
{
  aio << itsName << itsFromName << itsDtype;
  itsValues.saveNode (aio);
}
TaQLRecFldNodeRep* TaQLRecFldNodeRep::restore (AipsIO& aio)
{
  String name, fromName, dtype;
  aio >> name >> fromName >> dtype;
  TaQLNode values = TaQLNode::restoreNode (aio);
  if (fromName.empty()) {
    return new TaQLRecFldNodeRep (name, values, dtype);
  } else {
    return new TaQLRecFldNodeRep (name, fromName, dtype);
  }
}

TaQLUnitNodeRep::TaQLUnitNodeRep (const String& unit,
                                  const TaQLNode& child)
  : TaQLNodeRep (TaQLNode_Unit),
    itsUnit  (unit),
    itsChild (child)
{}
TaQLUnitNodeRep::~TaQLUnitNodeRep()
{}
TaQLNodeResult TaQLUnitNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitUnitNode (*this);
}
void TaQLUnitNodeRep::show (std::ostream& os) const
{
  os << '(';
  itsChild.show(os);
  os << ")'" << itsUnit << "'";
}
void TaQLUnitNodeRep::save (AipsIO& aio) const
{
  aio << itsUnit;
  itsChild.saveNode (aio);
}
TaQLUnitNodeRep* TaQLUnitNodeRep::restore (AipsIO& aio)
{
  String unit;
  aio >> unit;
  TaQLNode node = TaQLNode::restoreNode (aio);
  return new TaQLUnitNodeRep (unit, node);
}

TaQLAltTabNodeRep::TaQLAltTabNodeRep (const TaQLNode& table,
                                      const TaQLMultiNode& from,
                                      const TaQLMultiNode& commands)
  : TaQLQueryNodeRep (TaQLNode_AltTab),
    itsTable    (table),
    itsFrom     (from),
    itsCommands (commands)
{}
TaQLAltTabNodeRep::~TaQLAltTabNodeRep()
{}
TaQLNodeResult TaQLAltTabNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitAltTabNode (*this);
}
void TaQLAltTabNodeRep::showDerived (std::ostream& os) const
{
  os << "ALTER TABLE ";
  itsTable.show (os);
  if (itsFrom.isValid()) {
    os << " FROM ";
    itsFrom.show (os);
  }
  os << ' ';
  itsCommands.show (os);
}
void TaQLAltTabNodeRep::save (AipsIO& aio) const
{
  itsTable.saveNode (aio);
  itsFrom.saveNode (aio);
  itsCommands.saveNode (aio);
}
TaQLAltTabNodeRep* TaQLAltTabNodeRep::restore (AipsIO& aio)
{
  TaQLNode table         = TaQLNode::restoreNode (aio);
  TaQLMultiNode from     = TaQLNode::restoreMultiNode (aio);
  TaQLMultiNode commands = TaQLNode::restoreMultiNode (aio);
  return new TaQLAltTabNodeRep (table, from, commands);
}

TaQLAddColNodeRep::TaQLAddColNodeRep (const TaQLMultiNode& cols,
                                      const TaQLMultiNode& dminfo)
  : TaQLNodeRep (TaQLNode_AddCol),
    itsColumns (cols),
    itsDMInfo  (dminfo)
{}
TaQLAddColNodeRep::~TaQLAddColNodeRep()
{}
TaQLNodeResult TaQLAddColNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitAddColNode (*this);
}
void TaQLAddColNodeRep::show (std::ostream& os) const
{
  os << "ADD COLUMN ";
  itsColumns.show (os);
  if (itsDMInfo.isValid()) {
    os << " DMINFO ";
    itsDMInfo.show (os);
  }
}
void TaQLAddColNodeRep::save (AipsIO& aio) const
{
  itsColumns.saveNode(aio);
  itsDMInfo.saveNode(aio);
}
TaQLAddColNodeRep* TaQLAddColNodeRep::restore (AipsIO& aio)
{
  TaQLMultiNode cols   = TaQLNode::restoreMultiNode (aio);
  TaQLMultiNode dminfo = TaQLNode::restoreMultiNode (aio);
  return new TaQLAddColNodeRep (cols, dminfo);
}

TaQLRenDropNodeRep::TaQLRenDropNodeRep (Int type, const TaQLMultiNode& names)
  : TaQLNodeRep (TaQLNode_RenDrop),
    itsType  (type),
    itsNames (names)
{}
TaQLRenDropNodeRep::~TaQLRenDropNodeRep()
{}
TaQLNodeResult TaQLRenDropNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitRenDropNode (*this);
}
void TaQLRenDropNodeRep::show (std::ostream& os) const
{
  if (itsType == 0) {
    os << "RENAME COLUMN ";
  } else if (itsType == 1) {
    os << "DROP COLUMN ";
  } else if (itsType == 2) {
    os << "RENAME KEYWORD ";
  } else {
    os << "DROP KEYWORD ";
  }
  itsNames.show (os);
}
void TaQLRenDropNodeRep::save (AipsIO& aio) const
{
  aio << itsType;
  itsNames.saveNode (aio);
}
TaQLRenDropNodeRep* TaQLRenDropNodeRep::restore (AipsIO& aio)
{
  Int type;
  aio >> type;
  TaQLMultiNode names = TaQLNode::restoreMultiNode (aio);
  return new TaQLRenDropNodeRep (type, names);
}

TaQLSetKeyNodeRep::TaQLSetKeyNodeRep (const TaQLMultiNode& keyvals)
  : TaQLNodeRep (TaQLNode_SetKey),
    itsKeyVals (keyvals)
{}
TaQLSetKeyNodeRep::~TaQLSetKeyNodeRep()
{}
TaQLNodeResult TaQLSetKeyNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitSetKeyNode (*this);
}
void TaQLSetKeyNodeRep::show (std::ostream& os) const
{
  os << "SET KEYWORD ";
  itsKeyVals.show (os);
}
void TaQLSetKeyNodeRep::save (AipsIO& aio) const
{
  itsKeyVals.saveNode (aio);
}
TaQLSetKeyNodeRep* TaQLSetKeyNodeRep::restore (AipsIO& aio)
{
  TaQLMultiNode keyvals = TaQLNode::restoreMultiNode (aio);
  return new TaQLSetKeyNodeRep (keyvals);
}

TaQLAddRowNodeRep::TaQLAddRowNodeRep (const TaQLNode& nrow)
  : TaQLNodeRep (TaQLNode_AddRow),
    itsNRow(nrow)
{}
TaQLAddRowNodeRep::~TaQLAddRowNodeRep()
{}
TaQLNodeResult TaQLAddRowNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitAddRowNode (*this);
}
void TaQLAddRowNodeRep::show (std::ostream& os) const
{
  os << "ADD ROW ";
  itsNRow.show (os);
}
void TaQLAddRowNodeRep::save (AipsIO& aio) const
{
  itsNRow.saveNode (aio);
}
TaQLAddRowNodeRep* TaQLAddRowNodeRep::restore (AipsIO& aio)
{
  TaQLNode nrow = TaQLNode::restoreNode (aio);
  return new TaQLAddRowNodeRep (nrow);
}

TaQLConcTabNodeRep::TaQLConcTabNodeRep (const String& tableName,
                                        const TaQLMultiNode& tables,
                                        const TaQLMultiNode& subtableNames)
  : TaQLQueryNodeRep (TaQLNode_ConcTab),
    itsTableName (tableName),
    itsTables    (tables),
    itsSubTables (subtableNames)
{}
TaQLConcTabNodeRep::~TaQLConcTabNodeRep()
{}
TaQLNodeResult TaQLConcTabNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitConcTabNode (*this);
}
void TaQLConcTabNodeRep::showDerived (std::ostream& os) const
{
  os << "[";
  itsTables.show (os);
  if (itsSubTables.isValid()) {
    os << " SUBTABLES ";
    itsSubTables.show (os);
  }
  if (! itsTableName.empty()) {
    os << " GIVING " << itsTableName;
  }
  os << ']';
}
void TaQLConcTabNodeRep::save (AipsIO& aio) const
{
  aio << itsTableName;
  itsTables.saveNode (aio);
  itsSubTables.saveNode (aio);
}
TaQLConcTabNodeRep* TaQLConcTabNodeRep::restore (AipsIO& aio)
{
  String tableName;
  aio >> tableName;
  TaQLMultiNode tables = TaQLNode::restoreMultiNode (aio);
  TaQLMultiNode subtables = TaQLNode::restoreMultiNode (aio);
  return new TaQLConcTabNodeRep (tableName, tables, subtables);
}

TaQLShowNodeRep::TaQLShowNodeRep (const TaQLMultiNode& names)
  : TaQLNodeRep (TaQLNode_Show),
    itsNames (names)
{}
TaQLShowNodeRep::~TaQLShowNodeRep()
{}
TaQLNodeResult TaQLShowNodeRep::visit (TaQLNodeVisitor& visitor) const
{
  return visitor.visitShowNode (*this);
}
void TaQLShowNodeRep::show (std::ostream& os) const
{
  if (itsNames.isValid()) {
    itsNames.show (os);
  }
}
void TaQLShowNodeRep::save (AipsIO& aio) const
{
  itsNames.saveNode (aio);
}
TaQLShowNodeRep* TaQLShowNodeRep::restore (AipsIO& aio)
{
  TaQLMultiNode names = TaQLNode::restoreMultiNode (aio);
  return new TaQLShowNodeRep (names);
}


} //# NAMESPACE CASACORE - END
