//# ExprMathNode.h: Nodes representing scalar mathematical operators in table select expression tree
//# Copyright (C) 1994,1995,1996,1997,1999,2000
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

#ifndef TABLES_EXPRMATHNODE_H
#define TABLES_EXPRMATHNODE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNodeRep.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# This file defines classes derived from TableExprNode representing
//# the data type and operator in a table expression.
//#
//# Data types Bool, Int64, Double, DComplex and String are used.
//# Char, uChar, Short, uShort, Int, and uInt are converted to Int64,
//# Float to Double, and Complex to DComplex.
//# Binary operators +, -, *, /, ==, >=, >, <, <= and != are recognized.
//# Also &&, ||, parentheses and unary +, - and ! are recognized.



// <summary>
// Addition in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis>
// This abstract class represents an addition in a table expression tree.
// </synopsis>

class TableExprNodePlus : public TableExprNodeBinary
{
public:
    TableExprNodePlus (NodeDataType, const TableExprNodeRep&);
    ~TableExprNodePlus();
};


// <summary>
// Int addition in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis> 
// This class represents an addition in a table select expression tree.
// Strings can also be added (ie. concatenated).
// Numeric data types will be promoted if possible, so for instance
// an addition of Int and Complex is possible.
// </synopsis> 

class TableExprNodePlusInt : public TableExprNodePlus
{
public:
    TableExprNodePlusInt (const TableExprNodeRep&);
    ~TableExprNodePlusInt();
    Int64    getInt      (const TableExprId& id);
    Double   getDouble   (const TableExprId& id);
    DComplex getDComplex (const TableExprId& id);
};


// <summary>
// Double addition in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis> 
// This class represents an addition in a table select expression tree.
// Strings can also be added (ie. concatenated).
// Numeric data types will be promoted if possible, so for instance
// an addition of Int and Complex is possible.
// </synopsis> 

class TableExprNodePlusDouble : public TableExprNodePlus
{
public:
    TableExprNodePlusDouble (const TableExprNodeRep&);
    ~TableExprNodePlusDouble();
    Double   getDouble   (const TableExprId& id);
    DComplex getDComplex (const TableExprId& id);
};


// <summary>
// DComplex addition in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis> 
// This class represents an addition in a table select expression tree.
// Strings can also be added (ie. concatenated).
// Numeric data types will be promoted if possible, so for instance
// an addition of Int and Complex is possible.
// </synopsis> 

class TableExprNodePlusDComplex : public TableExprNodePlus
{
public:
    TableExprNodePlusDComplex (const TableExprNodeRep&);
    ~TableExprNodePlusDComplex();
    DComplex getDComplex (const TableExprId& id);
};


// <summary>
// String addition in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis> 
// This class represents an addition in a table select expression tree.
// Strings can also be added (ie. concatenated).
// Numeric data types will be promoted if possible, so for instance
// an addition of Int and Complex is possible.
// </synopsis> 

class TableExprNodePlusString : public TableExprNodePlus
{
public:
    TableExprNodePlusString (const TableExprNodeRep&);
    ~TableExprNodePlusString();
    String getString (const TableExprId& id);
};


// <summary>
// Date addition in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis> 
// This class represents an addition in a table select expression tree.
// Strings can also be added (ie. concatenated).
// Numeric data types will be promoted if possible, so for instance
// an addition of Int and Complex is possible.
// </synopsis> 

class TableExprNodePlusDate : public TableExprNodePlus
{
public:
    TableExprNodePlusDate (const TableExprNodeRep&);
    ~TableExprNodePlusDate();
    virtual void handleUnits();
    Double getDouble (const TableExprId& id);
    MVTime getDate (const TableExprId& id);
};



// <summary>
// Subtraction in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis>
// This abstract class represents a subtraction in a table expression tree.
// </synopsis>

class TableExprNodeMinus : public TableExprNodeBinary
{
public:
    TableExprNodeMinus (NodeDataType, const TableExprNodeRep&);
    ~TableExprNodeMinus();
};


// <summary>
// Int subtraction in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis> 
// This class represents a subtraction in a table select expression tree.
// Numeric data types will be promoted if possible, so for instance
// a subtraction of Int and Complex is possible.
// </synopsis> 

class TableExprNodeMinusInt : public TableExprNodeMinus
{
public:
    TableExprNodeMinusInt (const TableExprNodeRep&);
    ~TableExprNodeMinusInt();
    virtual void handleUnits();
    Int64    getInt      (const TableExprId& id);
    Double   getDouble   (const TableExprId& id);
    DComplex getDComplex (const TableExprId& id);
};


// <summary>
// Double subtraction in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis> 
// This class represents a subtraction in a table select expression tree.
// Numeric data types will be promoted if possible, so for instance
// a subtraction of Int and Complex is possible.
// </synopsis> 

class TableExprNodeMinusDouble : public TableExprNodeMinus
{
public:
    TableExprNodeMinusDouble (const TableExprNodeRep&);
    ~TableExprNodeMinusDouble();
    virtual void handleUnits();
    Double   getDouble   (const TableExprId& id);
    DComplex getDComplex (const TableExprId& id);
};


// <summary>
// DComplex subtraction in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis> 
// This class represents a subtraction in a table select expression tree.
// Numeric data types will be promoted if possible, so for instance
// a subtraction of Int and Complex is possible.
// </synopsis> 

class TableExprNodeMinusDComplex : public TableExprNodeMinus
{
public:
    TableExprNodeMinusDComplex (const TableExprNodeRep&);
    ~TableExprNodeMinusDComplex();
    DComplex getDComplex (const TableExprId& id);
};


// <summary>
// Date subtraction in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis> 
// This class represents a subtraction in a table select expression tree.
// Numeric data types will be promoted if possible, so for instance
// a subtraction of Int and Complex is possible.
// </synopsis> 

class TableExprNodeMinusDate : public TableExprNodeMinus
{
public:
    TableExprNodeMinusDate (const TableExprNodeRep&);
    ~TableExprNodeMinusDate();
    virtual void handleUnits();
    MVTime getDate   (const TableExprId& id);
    Double getDouble (const TableExprId& id);
};



// <summary>
// Multiplication in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis>
// This abstract class represents a multiplication in a table expression tree.
// </synopsis>

class TableExprNodeTimes : public TableExprNodeBinary
{
public:
    TableExprNodeTimes (NodeDataType, const TableExprNodeRep&);
    ~TableExprNodeTimes();
    virtual void handleUnits();
};


// <summary>
// Int multiplication in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis> 
// This class represents a multiplication in a table select expression tree.
// Numeric data types will be promoted if possible, so for instance
// a multiplication of Int and Complex is possible.
// </synopsis> 

class TableExprNodeTimesInt : public TableExprNodeTimes
{
public:
    TableExprNodeTimesInt (const TableExprNodeRep&);
    ~TableExprNodeTimesInt();
    Int64    getInt      (const TableExprId& id);
    Double   getDouble   (const TableExprId& id);
    DComplex getDComplex (const TableExprId& id);
};


// <summary>
// Double multiplication in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis> 
// This class represents a multiplication in a table select expression tree.
// Numeric data types will be promoted if possible, so for instance
// a multiplication of Int and Complex is possible.
// </synopsis> 

class TableExprNodeTimesDouble : public TableExprNodeTimes
{
public:
    TableExprNodeTimesDouble (const TableExprNodeRep&);
    ~TableExprNodeTimesDouble();
    Double   getDouble   (const TableExprId& id);
    DComplex getDComplex (const TableExprId& id);
};


// <summary>
// DComplex multiplication in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis> 
// This class represents a multiplication in a table select expression tree.
// Numeric data types will be promoted if possible, so for instance
// a multiplication of Int and Complex is possible.
// </synopsis> 

class TableExprNodeTimesDComplex : public TableExprNodeTimes
{
public:
    TableExprNodeTimesDComplex (const TableExprNodeRep&);
    ~TableExprNodeTimesDComplex();
    DComplex getDComplex (const TableExprId& id);
};



// <summary>
// Division in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis>
// This abstract class represents a division in a table expression tree.
// </synopsis>

class TableExprNodeDivide : public TableExprNodeBinary
{
public:
    TableExprNodeDivide (NodeDataType, const TableExprNodeRep&);
    ~TableExprNodeDivide();
    virtual void handleUnits();
};


// <summary>
// Double division in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis> 
// This class represents a division in a table select expression tree.
// Numeric data types will be promoted if possible, so for instance
// a division of Int and Complex is possible.
// </synopsis> 

class TableExprNodeDivideDouble : public TableExprNodeDivide
{
public:
    TableExprNodeDivideDouble (const TableExprNodeRep&);
    ~TableExprNodeDivideDouble();
    Double   getDouble   (const TableExprId& id);
    DComplex getDComplex (const TableExprId& id);
};


// <summary>
// DComplex division in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis> 
// This class represents a division in a table select expression tree.
// Numeric data types will be promoted if possible, so for instance
// a division of Int and Complex is possible.
// </synopsis> 

class TableExprNodeDivideDComplex : public TableExprNodeDivide
{
public:
    TableExprNodeDivideDComplex (const TableExprNodeRep&);
    ~TableExprNodeDivideDComplex();
    DComplex getDComplex (const TableExprId& id);
};


// <summary>
// Modulo in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis>
// This abstract class represents a modulo in a table expression tree.
// </synopsis>

class TableExprNodeModulo : public TableExprNodeBinary
{
public:
    TableExprNodeModulo (NodeDataType, const TableExprNodeRep&);
    ~TableExprNodeModulo();
    virtual void handleUnits();
};


// <summary>
// Int modulo in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis> 
// This class represents a modulo operation in a table select expression tree.
// It is only possible for datatype Int.
// </synopsis> 

class TableExprNodeModuloInt : public TableExprNodeModulo
{
public:
    TableExprNodeModuloInt (const TableExprNodeRep&);
    ~TableExprNodeModuloInt();
    Int64    getInt      (const TableExprId& id);
    Double   getDouble   (const TableExprId& id);
    DComplex getDComplex (const TableExprId& id);
};


// <summary>
// Double modulo in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis> 
// This class represents a modulo operation in a table select expression tree.
// It is only possible for datatype Double.
// </synopsis> 

class TableExprNodeModuloDouble : public TableExprNodeModulo
{
public:
    TableExprNodeModuloDouble (const TableExprNodeRep&);
    ~TableExprNodeModuloDouble();
    Double   getDouble   (const TableExprId& id);
    DComplex getDComplex (const TableExprId& id);
};


// <summary>
// Bitwise and in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis> 
// This class represents a bitwise and operation in a table select expression
// tree. It is only possible for datatype Int.
// </synopsis> 

class TableExprNodeBitAndInt : public TableExprNodeBinary
{
public:
    TableExprNodeBitAndInt (const TableExprNodeRep&);
    ~TableExprNodeBitAndInt();
    Int64    getInt      (const TableExprId& id);
    Double   getDouble   (const TableExprId& id);
    DComplex getDComplex (const TableExprId& id);
};


// <summary>
// Bitwise or in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis> 
// This class represents a bitwise or operation in a table select expression
// tree. It is only possible for datatype Int.
// </synopsis> 

class TableExprNodeBitOrInt : public TableExprNodeBinary
{
public:
    TableExprNodeBitOrInt (const TableExprNodeRep&);
    ~TableExprNodeBitOrInt();
    Int64    getInt      (const TableExprId& id);
    Double   getDouble   (const TableExprId& id);
    DComplex getDComplex (const TableExprId& id);
};


// <summary>
// Bitwise xor in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
//   <li> TableExprNodeRep
// </prerequisite>

// <synopsis> 
// This class represents a bitwise xor operation in a table select expression
// tree. It is only possible for datatype Int.
// </synopsis> 

class TableExprNodeBitXorInt : public TableExprNodeBinary
{
public:
    TableExprNodeBitXorInt (const TableExprNodeRep&);
    ~TableExprNodeBitXorInt();
    Int64    getInt      (const TableExprId& id);
    Double   getDouble   (const TableExprId& id);
    DComplex getDComplex (const TableExprId& id);
};



// <summary>
// Unary minus in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
// </prerequisite>

// <synopsis> 
// This class represents a unary minus in a table select expression tree.
// This is defined for numeric data types only.
// </synopsis> 

class TableExprNodeMIN : public TableExprNodeBinary
{
public:
    TableExprNodeMIN (const TableExprNodeRep&);
    ~TableExprNodeMIN();
    Int64    getInt      (const TableExprId& id);
    Double   getDouble   (const TableExprId& id);
    DComplex getDComplex (const TableExprId& id);
};


// <summary>
// Bitwise negate in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
// </prerequisite>

// <synopsis> 
// This class represents a bitwise negate in a table select expression tree.
// This is defined for integer data types only.
// </synopsis> 

class TableExprNodeBitNegate : public TableExprNodeBinary
{
public:
    TableExprNodeBitNegate (const TableExprNodeRep&);
    ~TableExprNodeBitNegate();
    Int64    getInt      (const TableExprId& id);
    Double   getDouble   (const TableExprId& id);
    DComplex getDComplex (const TableExprId& id);
};


} //# NAMESPACE CASACORE - END

#endif
