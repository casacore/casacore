//# ExprDerArrayNode.h: Nodes representing array operators in table select expression tree
//# Copyright (C) 1997
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

#if !defined(AIPS_EXPRDERNODEARRAY_H)
#define AIPS_EXPRDERNODEARRAY_H

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
//# Binary operators +, -, *, /, ==, >=, >, <, <= and != are recognized.
//# Also &&, ||, parentheses and unary +, - and ! are recognized.



// <summary>
// Addition in table select expression tree
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
    Array<Double> getArrayDouble (uInt rownr);
};

class TableExprNodeArrayPlusDComplex : public TableExprNodeArray
{
public:
    TableExprNodeArrayPlusDComplex (const TableExprNodeRep&);
    ~TableExprNodeArrayPlusDComplex();
    Array<DComplex> getArrayDComplex (uInt rownr);
};

class TableExprNodeArrayPlusString : public TableExprNodeArray
{
public:
    TableExprNodeArrayPlusString (const TableExprNodeRep&);
    ~TableExprNodeArrayPlusString();
    Array<String> getArrayString (uInt rownr);
private:
    // Concatenate <src>nr</src> arrays of strings.
    // The increment is 0 for a scalar value. Otherwise it is 1.
    void concString (String* to, const String* left, Int incrLeft,
		     const String* right, Int incrRight, uInt nr) const;
};


// <summary>
// Subtraction in table select expression tree
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
    Array<Double> getArrayDouble (uInt rownr);
};

class TableExprNodeArrayMinusDComplex : public TableExprNodeArray
{
public:
    TableExprNodeArrayMinusDComplex (const TableExprNodeRep&);
    ~TableExprNodeArrayMinusDComplex();
    Array<DComplex> getArrayDComplex (uInt rownr);
};


// <summary>
// Multiplication in table select expression tree
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
    Array<Double> getArrayDouble (uInt rownr);
};

class TableExprNodeArrayTimesDComplex : public TableExprNodeArray
{
public:
    TableExprNodeArrayTimesDComplex (const TableExprNodeRep&);
    ~TableExprNodeArrayTimesDComplex();
    Array<DComplex> getArrayDComplex (uInt rownr);
};


// <summary>
// Division in table select expression tree
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
    Array<Double> getArrayDouble (uInt rownr);
};

class TableExprNodeArrayDivideDComplex : public TableExprNodeArray
{
public:
    TableExprNodeArrayDivideDComplex (const TableExprNodeRep&);
    ~TableExprNodeArrayDivideDComplex();
    Array<DComplex> getArrayDComplex (uInt rownr);
};


// <summary>
// Modulo in table select expression tree
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
    Array<Double> getArrayDouble (uInt rownr);
};



// <summary>
// Comparison == in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
// </prerequisite>

// <synopsis> 
// This class represents an == comparison in a table select expression tree.
// This is defined for all data types.
// Only the Bool get function is defined, because the result of a
// compare is always a Bool.
// </synopsis> 

class TableExprNodeArrayEQBool : public TableExprNodeArray
{
public:
    TableExprNodeArrayEQBool (const TableExprNodeRep&);
    ~TableExprNodeArrayEQBool();
    Array<Bool> getArrayBool (uInt rownr);
};

class TableExprNodeArrayEQDouble : public TableExprNodeArray
{
public:
    TableExprNodeArrayEQDouble (const TableExprNodeRep&);
    ~TableExprNodeArrayEQDouble();
    Array<Bool> getArrayBool (uInt rownr);
};

class TableExprNodeArrayEQDComplex : public TableExprNodeArray
{
public:
    TableExprNodeArrayEQDComplex (const TableExprNodeRep&);
    ~TableExprNodeArrayEQDComplex();
    Array<Bool> getArrayBool (uInt rownr);
};

class TableExprNodeArrayEQString : public TableExprNodeArray
{
public:
    TableExprNodeArrayEQString (const TableExprNodeRep&);
    ~TableExprNodeArrayEQString();
    Array<Bool> getArrayBool (uInt rownr);
};

class TableExprNodeArrayEQRegex : public TableExprNodeArray
{
public:
    TableExprNodeArrayEQRegex (const TableExprNodeRep&);
    ~TableExprNodeArrayEQRegex();
    Array<Bool> getArrayBool (uInt rownr);
};

class TableExprNodeArrayEQDate : public TableExprNodeArray
{
public:
    TableExprNodeArrayEQDate (const TableExprNodeRep&);
    ~TableExprNodeArrayEQDate();
    Array<Bool> getArrayBool (uInt rownr);
};


// <summary>
// Comparison != in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
// </prerequisite>

// <synopsis> 
// This class represents an != comparison in a table select expression tree.
// This is defined for all data types.
// Only the Bool get function is defined, because the result of a
// compare is always a Bool.
// </synopsis> 

class TableExprNodeArrayNEBool : public TableExprNodeArray
{
public:
    TableExprNodeArrayNEBool (const TableExprNodeRep&);
    ~TableExprNodeArrayNEBool();
    Array<Bool> getArrayBool (uInt rownr);
};

class TableExprNodeArrayNEDouble : public TableExprNodeArray
{
public:
    TableExprNodeArrayNEDouble (const TableExprNodeRep&);
    ~TableExprNodeArrayNEDouble();
    Array<Bool> getArrayBool (uInt rownr);
};

class TableExprNodeArrayNEDComplex : public TableExprNodeArray
{
public:
    TableExprNodeArrayNEDComplex (const TableExprNodeRep&);
    ~TableExprNodeArrayNEDComplex();
    Array<Bool> getArrayBool (uInt rownr);
};

class TableExprNodeArrayNEString : public TableExprNodeArray
{
public:
    TableExprNodeArrayNEString (const TableExprNodeRep&);
    ~TableExprNodeArrayNEString();
    Array<Bool> getArrayBool (uInt rownr);
};

class TableExprNodeArrayNERegex : public TableExprNodeArray
{
public:
    TableExprNodeArrayNERegex (const TableExprNodeRep&);
    ~TableExprNodeArrayNERegex();
    Array<Bool> getArrayBool (uInt rownr);
};

class TableExprNodeArrayNEDate : public TableExprNodeArray
{
public:
    TableExprNodeArrayNEDate (const TableExprNodeRep&);
    ~TableExprNodeArrayNEDate();
    Array<Bool> getArrayBool (uInt rownr);
};


// <summary>
// Comparison > in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
// </prerequisite>

// <synopsis> 
// This class represents an > comparison in a table select expression tree.
// This is defined for all data types.
// Only the Bool get function is defined, because the result of a
// compare is always a Bool.
// </synopsis> 

class TableExprNodeArrayGTDouble : public TableExprNodeArray
{
public:
    TableExprNodeArrayGTDouble (const TableExprNodeRep&);
    ~TableExprNodeArrayGTDouble();
    Array<Bool> getArrayBool (uInt rownr);
};

class TableExprNodeArrayGTDComplex : public TableExprNodeArray
{
public:
    TableExprNodeArrayGTDComplex (const TableExprNodeRep&);
    ~TableExprNodeArrayGTDComplex();
    Array<Bool> getArrayBool (uInt rownr);
};

class TableExprNodeArrayGTString : public TableExprNodeArray
{
public:
    TableExprNodeArrayGTString (const TableExprNodeRep&);
    ~TableExprNodeArrayGTString();
    Array<Bool> getArrayBool (uInt rownr);
};

class TableExprNodeArrayGTDate : public TableExprNodeArray
{
public:
    TableExprNodeArrayGTDate (const TableExprNodeRep&);
    ~TableExprNodeArrayGTDate();
    Array<Bool> getArrayBool (uInt rownr);
};


// <summary>
// Comparison >= in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
// </prerequisite>

// <synopsis> 
// This class represents an >= comparison in a table select expression tree.
// This is defined for all data types.
// Only the Bool get function is defined, because the result of a
// compare is always a Bool.
// </synopsis> 

class TableExprNodeArrayGEDouble : public TableExprNodeArray
{
public:
    TableExprNodeArrayGEDouble (const TableExprNodeRep&);
    ~TableExprNodeArrayGEDouble();
    Array<Bool> getArrayBool (uInt rownr);
};

class TableExprNodeArrayGEDComplex : public TableExprNodeArray
{
public:
    TableExprNodeArrayGEDComplex (const TableExprNodeRep&);
    ~TableExprNodeArrayGEDComplex();
    Array<Bool> getArrayBool (uInt rownr);
};

class TableExprNodeArrayGEString : public TableExprNodeArray
{
public:
    TableExprNodeArrayGEString (const TableExprNodeRep&);
    ~TableExprNodeArrayGEString();
    Array<Bool> getArrayBool (uInt rownr);
};

class TableExprNodeArrayGEDate : public TableExprNodeArray
{
public:
    TableExprNodeArrayGEDate (const TableExprNodeRep&);
    ~TableExprNodeArrayGEDate();
    Array<Bool> getArrayBool (uInt rownr);
};


// <summary>
// Comparison IN in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
// </prerequisite>

// <synopsis> 
// This class represents an IN comparison in a table select expression tree.
// This is defined for all data types.
// Only the Bool get function is defined, because the result of a
// compare is always a Bool.
// </synopsis> 

class TableExprNodeArrayINDouble : public TableExprNodeArray
{
public:
    TableExprNodeArrayINDouble (const TableExprNodeRep&);
    ~TableExprNodeArrayINDouble();
    Array<Bool> getArrayBool (uInt rownr);
};

class TableExprNodeArrayINDComplex : public TableExprNodeArray
{
public:
    TableExprNodeArrayINDComplex (const TableExprNodeRep&);
    ~TableExprNodeArrayINDComplex();
    Array<Bool> getArrayBool (uInt rownr);
};

class TableExprNodeArrayINString : public TableExprNodeArray
{
public:
    TableExprNodeArrayINString (const TableExprNodeRep&);
    ~TableExprNodeArrayINString();
    Array<Bool> getArrayBool (uInt rownr);
};

class TableExprNodeArrayINDate : public TableExprNodeArray
{
public:
    TableExprNodeArrayINDate (const TableExprNodeRep&);
    ~TableExprNodeArrayINDate();
    Array<Bool> getArrayBool (uInt rownr);
};



// <summary>
// Logical or in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
// </prerequisite>

// <synopsis> 
// This class represents a logical or in a table select expression tree.
// This is defined for Bool only.
// </synopsis> 

class TableExprNodeArrayOR : public TableExprNodeArray
{
public:
    TableExprNodeArrayOR (const TableExprNodeRep&);
    ~TableExprNodeArrayOR();
    Array<Bool> getArrayBool (uInt rownr);
};


// <summary>
// Logical and in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
// </prerequisite>

// <synopsis> 
// This class represents a logical and in a table select expression tree.
// This is defined for Bool only.
// </synopsis> 

class TableExprNodeArrayAND: public TableExprNodeArray
{
public:
    TableExprNodeArrayAND (const TableExprNodeRep&);
    ~TableExprNodeArrayAND();
    Array<Bool> getArrayBool (uInt rownr);
};


// <summary>
// Logical not in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNodeArray
// </prerequisite>

// <synopsis> 
// This class represents a logical not in a table select expression tree.
// This is defined for Bool only.
// </synopsis> 

class TableExprNodeArrayNOT: public TableExprNodeArray
{
public:
    TableExprNodeArrayNOT (const TableExprNodeRep&);
    ~TableExprNodeArrayNOT();
    Array<Bool> getArrayBool (uInt rownr);
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
    Array<Double>   getArrayDouble   (uInt rownr);
    Array<DComplex> getArrayDComplex (uInt rownr);
};



// <summary>
// Constant in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNodeArray
// </prerequisite>

// <synopsis> 
// This class represents a constant in a table select expression tree.
// This is also used to hold the value of a table keyword, which is
// constant over the entire table.
// </synopsis> 

class TableExprNodeArrayConstBool : public TableExprNodeArray
{
public:
    TableExprNodeArrayConstBool (const Array<Bool>& value);
    ~TableExprNodeArrayConstBool();
    Array<Bool> getArrayBool (uInt rownr);
private:
    Array<Bool> value_p;
};

class TableExprNodeArrayConstDouble : public TableExprNodeArray
{
public:
    TableExprNodeArrayConstDouble (const Array<Double>& value);
    TableExprNodeArrayConstDouble (const Array<Float>& value);
    TableExprNodeArrayConstDouble (const Array<uInt>& value);
    TableExprNodeArrayConstDouble (const Array<Int>& value);
    TableExprNodeArrayConstDouble (const Array<uShort>& value);
    TableExprNodeArrayConstDouble (const Array<Short>& value);
    TableExprNodeArrayConstDouble (const Array<uChar>& value);
    ~TableExprNodeArrayConstDouble();
    Array<Double>   getArrayDouble   (uInt rownr);
    Array<DComplex> getArrayDComplex (uInt rownr);
private:
    Array<Double> value_p;
};

class TableExprNodeArrayConstDComplex : public TableExprNodeArray
{
public:
    TableExprNodeArrayConstDComplex (const Array<DComplex>& value);
    TableExprNodeArrayConstDComplex (const Array<Complex>& value);
    TableExprNodeArrayConstDComplex (const Array<Double>& value);
    ~TableExprNodeArrayConstDComplex();
    Array<DComplex> getArrayDComplex (uInt rownr);
private:
    Array<DComplex> value_p;
};

class TableExprNodeArrayConstString : public TableExprNodeArray
{
public:
    TableExprNodeArrayConstString (const Array<String>& value);
    ~TableExprNodeArrayConstString();
    Array<String> getArrayString (uInt rownr);
private:
    Array<String> value_p;
};

class TableExprNodeArrayConstDate : public TableExprNodeArray
{
public:
    TableExprNodeArrayConstDate (const Array<MVTime>& value);
    ~TableExprNodeArrayConstDate();
    Array<Double> getArrayDouble(uInt rownr);
    Array<MVTime> getArrayDate  (uInt rownr);
private:
    Array<MVTime> value_p;
};



#endif
