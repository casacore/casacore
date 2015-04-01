//# ExprMathArrayNode.h: Nodes representing mathematical array operators in table select expression tree
//# Copyright (C) 1997,1999,2000
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

#ifndef TABLES_EXPRMATHNODEARRAY_H
#define TABLES_EXPRMATHNODEARRAY_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNodeArray.h>
#include <casacore/casa/Arrays/Array.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

//# This file defines classes derived from TableExprNode representing
//# the data type and operator in a table expression.
//#
//# Data types Bool, Int64, Double, DComplex and String are used.
//# Char, uChar, Short, uShort, Int, and uInt are converted to Int64,
//# Float to Double, and Complex to DComplex.
//# Binary operators +, -, *, /, and % are recognized.
//# Also unary + and - are recognized.



// <summary>
// Array addition in table select expression tree
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

class TableExprNodeArrayPlus : public TableExprNodeArray
{
public:
    TableExprNodeArrayPlus (NodeDataType, const TableExprNodeRep&);
    ~TableExprNodeArrayPlus();
};


// <summary>
// Int Array addition in table select expression tree
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

class TableExprNodeArrayPlusInt : public TableExprNodeArrayPlus
{
public:
    TableExprNodeArrayPlusInt (const TableExprNodeRep&);
    ~TableExprNodeArrayPlusInt();
    Array<Int64> getArrayInt (const TableExprId& id);
};


// <summary>
// Double Array addition in table select expression tree
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

class TableExprNodeArrayPlusDouble : public TableExprNodeArrayPlus
{
public:
    TableExprNodeArrayPlusDouble (const TableExprNodeRep&);
    ~TableExprNodeArrayPlusDouble();
    Array<Double> getArrayDouble (const TableExprId& id);
};


// <summary>
// DComplex Array addition in table select expression tree
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

class TableExprNodeArrayPlusDComplex : public TableExprNodeArrayPlus
{
public:
    TableExprNodeArrayPlusDComplex (const TableExprNodeRep&);
    ~TableExprNodeArrayPlusDComplex();
    Array<DComplex> getArrayDComplex (const TableExprId& id);
};


// <summary>
// String Array addition in table select expression tree
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

class TableExprNodeArrayPlusString : public TableExprNodeArrayPlus
{
public:
    TableExprNodeArrayPlusString (const TableExprNodeRep&);
    ~TableExprNodeArrayPlusString();
    Array<String> getArrayString (const TableExprId& id);
private:
    // Concatenate <src>nr</src> arrays of strings.
    // The increment is 0 for a scalar value. Otherwise it is 1.
    void concString (String* to, const String* left, Int incrLeft,
		     const String* right, Int incrRight, uInt nr) const;
};


// <summary>
// Date Array addition in table select expression tree
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

class TableExprNodeArrayPlusDate : public TableExprNodeArrayPlus
{
public:
    TableExprNodeArrayPlusDate (const TableExprNodeRep&);
    ~TableExprNodeArrayPlusDate();
    virtual void handleUnits();
    Array<Double> getArrayDouble (const TableExprId& id);
    Array<MVTime> getArrayDate   (const TableExprId& id);
};



// <summary>
// Array addition in table select expression tree
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

class TableExprNodeArrayMinus : public TableExprNodeArray
{
public:
    TableExprNodeArrayMinus (NodeDataType, const TableExprNodeRep&);
    ~TableExprNodeArrayMinus();
};


// <summary>
// Int Array subtraction in table select expression tree
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

class TableExprNodeArrayMinusInt : public TableExprNodeArrayMinus
{
public:
    TableExprNodeArrayMinusInt (const TableExprNodeRep&);
    ~TableExprNodeArrayMinusInt();
    Array<Int64> getArrayInt (const TableExprId& id);
};


// <summary>
// Double Array subtraction in table select expression tree
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

class TableExprNodeArrayMinusDouble : public TableExprNodeArrayMinus
{
public:
    TableExprNodeArrayMinusDouble (const TableExprNodeRep&);
    ~TableExprNodeArrayMinusDouble();
    Array<Double> getArrayDouble (const TableExprId& id);
};


// <summary>
// DComplex Array subtraction in table select expression tree
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

class TableExprNodeArrayMinusDComplex : public TableExprNodeArrayMinus
{
public:
    TableExprNodeArrayMinusDComplex (const TableExprNodeRep&);
    ~TableExprNodeArrayMinusDComplex();
    Array<DComplex> getArrayDComplex (const TableExprId& id);
};


// <summary>
// Date Array subtraction in table select expression tree
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

class TableExprNodeArrayMinusDate : public TableExprNodeArrayMinus
{
public:
    TableExprNodeArrayMinusDate (const TableExprNodeRep&);
    ~TableExprNodeArrayMinusDate();
    virtual void handleUnits();
    Array<Double> getArrayDouble (const TableExprId& id);
    Array<MVTime> getArrayDate   (const TableExprId& id);
};



// <summary>
// Array addition in table select expression tree
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

class TableExprNodeArrayTimes : public TableExprNodeArray
{
public:
    TableExprNodeArrayTimes (NodeDataType, const TableExprNodeRep&);
    ~TableExprNodeArrayTimes();
    virtual void handleUnits();
};


// <summary>
// Int Array multiplication in table select expression tree
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

class TableExprNodeArrayTimesInt : public TableExprNodeArrayTimes
{
public:
    TableExprNodeArrayTimesInt (const TableExprNodeRep&);
    ~TableExprNodeArrayTimesInt();
    Array<Int64> getArrayInt (const TableExprId& id);
};


// <summary>
// Double Array multiplication in table select expression tree
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

class TableExprNodeArrayTimesDouble : public TableExprNodeArrayTimes
{
public:
    TableExprNodeArrayTimesDouble (const TableExprNodeRep&);
    ~TableExprNodeArrayTimesDouble();
    Array<Double> getArrayDouble (const TableExprId& id);
};


// <summary>
// DComplex Array multiplication in table select expression tree
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

class TableExprNodeArrayTimesDComplex : public TableExprNodeArrayTimes
{
public:
    TableExprNodeArrayTimesDComplex (const TableExprNodeRep&);
    ~TableExprNodeArrayTimesDComplex();
    Array<DComplex> getArrayDComplex (const TableExprId& id);
};



// <summary>
// Array addition in table select expression tree
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

class TableExprNodeArrayDivide : public TableExprNodeArray
{
public:
    TableExprNodeArrayDivide (NodeDataType, const TableExprNodeRep&);
    ~TableExprNodeArrayDivide();
    virtual void handleUnits();
};


// <summary>
// Double Array division in table select expression tree
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

class TableExprNodeArrayDivideDouble : public TableExprNodeArrayDivide
{
public:
    TableExprNodeArrayDivideDouble (const TableExprNodeRep&);
    ~TableExprNodeArrayDivideDouble();
    Array<Double> getArrayDouble (const TableExprId& id);
};


// <summary>
// DComplex Array division in table select expression tree
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

class TableExprNodeArrayDivideDComplex : public TableExprNodeArrayDivide
{
public:
    TableExprNodeArrayDivideDComplex (const TableExprNodeRep&);
    ~TableExprNodeArrayDivideDComplex();
    Array<DComplex> getArrayDComplex (const TableExprId& id);
};



// <summary>
// Array addition in table select expression tree
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

class TableExprNodeArrayModulo : public TableExprNodeArray
{
public:
    TableExprNodeArrayModulo (NodeDataType, const TableExprNodeRep&);
    ~TableExprNodeArrayModulo();
    virtual void handleUnits();
};


// <summary>
// Int Array modulo in table select expression tree
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

class TableExprNodeArrayModuloInt : public TableExprNodeArrayModulo
{
public:
    TableExprNodeArrayModuloInt (const TableExprNodeRep&);
    ~TableExprNodeArrayModuloInt();
    Array<Int64> getArrayInt (const TableExprId& id);
};


// <summary>
// Double Array modulo in table select expression tree
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

class TableExprNodeArrayModuloDouble : public TableExprNodeArrayModulo
{
public:
    TableExprNodeArrayModuloDouble (const TableExprNodeRep&);
    ~TableExprNodeArrayModuloDouble();
    Array<Double> getArrayDouble (const TableExprId& id);
};



// <summary>
// Int Array bitwise and in table select expression tree
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
// This class represents a bitwise and  operation in a table select expression
// tree. It is only possible for datatype Int.
// </synopsis> 

class TableExprNodeArrayBitAndInt : public TableExprNodeArray
{
public:
    TableExprNodeArrayBitAndInt (const TableExprNodeRep&);
    ~TableExprNodeArrayBitAndInt();
    Array<Int64> getArrayInt (const TableExprId& id);
};


// <summary>
// Int Array bitwise or in table select expression tree
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
// This class represents a bitwise or  operation in a table select expression
// tree. It is only possible for datatype Int.
// </synopsis> 

class TableExprNodeArrayBitOrInt : public TableExprNodeArray
{
public:
    TableExprNodeArrayBitOrInt (const TableExprNodeRep&);
    ~TableExprNodeArrayBitOrInt();
    Array<Int64> getArrayInt (const TableExprId& id);
};


// <summary>
// Int Array bitwise xor in table select expression tree
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
// This class represents a bitwise xor  operation in a table select expression
// tree. It is only possible for datatype Int.
// </synopsis> 

class TableExprNodeArrayBitXorInt : public TableExprNodeArray
{
public:
    TableExprNodeArrayBitXorInt (const TableExprNodeRep&);
    ~TableExprNodeArrayBitXorInt();
    Array<Int64> getArrayInt (const TableExprId& id);
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

class TableExprNodeArrayMIN : public TableExprNodeArray
{
public:
    TableExprNodeArrayMIN (const TableExprNodeRep&);
    ~TableExprNodeArrayMIN();
    Array<Int64>    getArrayInt      (const TableExprId& id);
    Array<Double>   getArrayDouble   (const TableExprId& id);
    Array<DComplex> getArrayDComplex (const TableExprId& id);
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
// This is defined for Int data types only.
// </synopsis> 

class TableExprNodeArrayBitNegate : public TableExprNodeArray
{
public:
    TableExprNodeArrayBitNegate (const TableExprNodeRep&);
    ~TableExprNodeArrayBitNegate();
    Array<Int64> getArrayInt (const TableExprId& id);
};


} //# NAMESPACE CASACORE - END

#endif
