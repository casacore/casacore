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

#if !defined(AIPS_EXPRMATHNODEARRAY_H)
#define AIPS_EXPRMATHNODEARRAY_H

//# Includes
#include <aips/aips.h>
#include <aips/Tables/ExprNodeArray.h>
#include <aips/Arrays/Array.h>

//# Forward Declarations

//# This file defines classes derived from TableExprNode representing
//# the data type and operator in a table expression.
//#
//# Data types Bool, Double, DComplex and String are used.
//# Char, uChar, Short, uShort, Int, uInt and float are converted 
//# to Double, and Complex to DComplex.
//# Binary operators +, -, *, /, and % are recognized.
//# Also unary + and - are recognized.



// <summary>
// Double Array addition in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
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

class TableExprNodeArrayPlusDouble : public TableExprNodeArray
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

// <reviewed reviewer="" date="" tests="">
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

class TableExprNodeArrayPlusDComplex : public TableExprNodeArray
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

// <reviewed reviewer="" date="" tests="">
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

class TableExprNodeArrayPlusString : public TableExprNodeArray
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
// Double Array subtraction in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
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

class TableExprNodeArrayMinusDouble : public TableExprNodeArray
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

// <reviewed reviewer="" date="" tests="">
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

class TableExprNodeArrayMinusDComplex : public TableExprNodeArray
{
public:
    TableExprNodeArrayMinusDComplex (const TableExprNodeRep&);
    ~TableExprNodeArrayMinusDComplex();
    Array<DComplex> getArrayDComplex (const TableExprId& id);
};



// <summary>
// Double Array multiplication in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
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

class TableExprNodeArrayTimesDouble : public TableExprNodeArray
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

// <reviewed reviewer="" date="" tests="">
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

class TableExprNodeArrayTimesDComplex : public TableExprNodeArray
{
public:
    TableExprNodeArrayTimesDComplex (const TableExprNodeRep&);
    ~TableExprNodeArrayTimesDComplex();
    Array<DComplex> getArrayDComplex (const TableExprId& id);
};



// <summary>
// Double Array division in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
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

class TableExprNodeArrayDivideDouble : public TableExprNodeArray
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

// <reviewed reviewer="" date="" tests="">
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

class TableExprNodeArrayDivideDComplex : public TableExprNodeArray
{
public:
    TableExprNodeArrayDivideDComplex (const TableExprNodeRep&);
    ~TableExprNodeArrayDivideDComplex();
    Array<DComplex> getArrayDComplex (const TableExprId& id);
};



// <summary>
// Double Array modulo in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
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

class TableExprNodeArrayModuloDouble : public TableExprNodeArray
{
public:
    TableExprNodeArrayModuloDouble (const TableExprNodeRep&);
    ~TableExprNodeArrayModuloDouble();
    Array<Double> getArrayDouble (const TableExprId& id);
};



// <summary>
// Unary minus in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
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
    Array<Double>   getArrayDouble   (const TableExprId& id);
    Array<DComplex> getArrayDComplex (const TableExprId& id);
};



#endif
