//# ExprDerNode.h: Nodes representing scalar operators in table select expression tree
//# Copyright (C) 1994,1995,1996,1997
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

#if !defined(AIPS_EXPRDERNODE_H)
#define AIPS_EXPRDERNODE_H

//# Includes
#include <aips/aips.h>
#include <aips/Tables/ExprNodeRep.h>
#include <aips/Mathematics/Random.h>

//# Forward Declarations
class ROTableColumn;
class Table;

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

class TableExprNodePlusDouble : public TableExprNodeBinary
{
public:
    TableExprNodePlusDouble (const TableExprNodeRep&);
    ~TableExprNodePlusDouble();
    Double   getDouble   (uInt rownr);
    DComplex getDComplex (uInt rownr);
};

class TableExprNodePlusDComplex : public TableExprNodeBinary
{
public:
    TableExprNodePlusDComplex (const TableExprNodeRep&);
    ~TableExprNodePlusDComplex();
    DComplex getDComplex (uInt rownr);
};

class TableExprNodePlusString : public TableExprNodeBinary
{
public:
    TableExprNodePlusString (const TableExprNodeRep&);
    ~TableExprNodePlusString();
    String getString (uInt rownr);
};

class TableExprNodePlusDate : public TableExprNodeBinary
{
public:
    TableExprNodePlusDate (const TableExprNodeRep&);
    ~TableExprNodePlusDate();
    Double getDouble (uInt rownr);
    MVTime getDate (uInt rownr);
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

class TableExprNodeMinusDouble : public TableExprNodeBinary
{
public:
    TableExprNodeMinusDouble (const TableExprNodeRep&);
    ~TableExprNodeMinusDouble();
    Double   getDouble   (uInt rownr);
    DComplex getDComplex (uInt rownr);
};

class TableExprNodeMinusDComplex : public TableExprNodeBinary
{
public:
    TableExprNodeMinusDComplex (const TableExprNodeRep&);
    ~TableExprNodeMinusDComplex();
    DComplex getDComplex (uInt rownr);
};

class TableExprNodeMinusDate : public TableExprNodeBinary
{
public:
    TableExprNodeMinusDate (const TableExprNodeRep&);
    ~TableExprNodeMinusDate();
    MVTime getDate   (uInt rownr);
    Double getDouble (uInt rownr);
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

class TableExprNodeTimesDouble : public TableExprNodeBinary
{
public:
    TableExprNodeTimesDouble (const TableExprNodeRep&);
    ~TableExprNodeTimesDouble();
    Double   getDouble   (uInt rownr);
    DComplex getDComplex (uInt rownr);
};

class TableExprNodeTimesDComplex : public TableExprNodeBinary
{
public:
    TableExprNodeTimesDComplex (const TableExprNodeRep&);
    ~TableExprNodeTimesDComplex();
    DComplex getDComplex (uInt rownr);
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

class TableExprNodeDivideDouble : public TableExprNodeBinary
{
public:
    TableExprNodeDivideDouble (const TableExprNodeRep&);
    ~TableExprNodeDivideDouble();
    Double   getDouble   (uInt rownr);
    DComplex getDComplex (uInt rownr);
};

class TableExprNodeDivideDComplex : public TableExprNodeBinary
{
public:
    TableExprNodeDivideDComplex (const TableExprNodeRep&);
    ~TableExprNodeDivideDComplex();
    DComplex getDComplex (uInt rownr);
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

class TableExprNodeModuloDouble : public TableExprNodeBinary
{
public:
    TableExprNodeModuloDouble (const TableExprNodeRep&);
    ~TableExprNodeModuloDouble();
    Double   getDouble   (uInt rownr);
    DComplex getDComplex (uInt rownr);
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

class TableExprNodeEQBool : public TableExprNodeBinary
{
public:
    TableExprNodeEQBool (const TableExprNodeRep&);
    ~TableExprNodeEQBool();
    Bool getBool (uInt rownr);
};

class TableExprNodeEQDouble : public TableExprNodeBinary
{
public:
    TableExprNodeEQDouble (const TableExprNodeRep&);
    ~TableExprNodeEQDouble();
    Bool getBool (uInt rownr);
    void ranges (Block<TableExprRange>&);
};

class TableExprNodeEQDComplex : public TableExprNodeBinary
{
public:
    TableExprNodeEQDComplex (const TableExprNodeRep&);
    ~TableExprNodeEQDComplex();
    Bool getBool (uInt rownr);
};

class TableExprNodeEQString : public TableExprNodeBinary
{
public:
    TableExprNodeEQString (const TableExprNodeRep&);
    ~TableExprNodeEQString();
    Bool getBool (uInt rownr);
};

class TableExprNodeEQRegex : public TableExprNodeBinary
{
public:
    TableExprNodeEQRegex (const TableExprNodeRep&);
    ~TableExprNodeEQRegex();
    Bool getBool (uInt rownr);
};

class TableExprNodeEQDate : public TableExprNodeBinary
{
public:
    TableExprNodeEQDate (const TableExprNodeRep&);
    ~TableExprNodeEQDate();
    Bool getBool (uInt rownr);
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

class TableExprNodeNEBool : public TableExprNodeBinary
{
public:
    TableExprNodeNEBool (const TableExprNodeRep&);
    ~TableExprNodeNEBool();
    Bool getBool (uInt rownr);
};

class TableExprNodeNEDouble : public TableExprNodeBinary
{
public:
    TableExprNodeNEDouble (const TableExprNodeRep&);
    ~TableExprNodeNEDouble();
    Bool getBool (uInt rownr);
};

class TableExprNodeNEDComplex : public TableExprNodeBinary
{
public:
    TableExprNodeNEDComplex (const TableExprNodeRep&);
    ~TableExprNodeNEDComplex();
    Bool getBool (uInt rownr);
};

class TableExprNodeNEString : public TableExprNodeBinary
{
public:
    TableExprNodeNEString (const TableExprNodeRep&);
    ~TableExprNodeNEString();
    Bool getBool (uInt rownr);
};

class TableExprNodeNERegex : public TableExprNodeBinary
{
public:
    TableExprNodeNERegex (const TableExprNodeRep&);
    ~TableExprNodeNERegex();
    Bool getBool (uInt rownr);
};

class TableExprNodeNEDate : public TableExprNodeBinary
{
public:
    TableExprNodeNEDate (const TableExprNodeRep&);
    ~TableExprNodeNEDate();
    Bool getBool (uInt rownr);
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

class TableExprNodeGTDouble : public TableExprNodeBinary
{
public:
    TableExprNodeGTDouble (const TableExprNodeRep&);
    ~TableExprNodeGTDouble();
    Bool getBool (uInt rownr);
    void ranges (Block<TableExprRange>&);
};

class TableExprNodeGTDComplex : public TableExprNodeBinary
{
public:
    TableExprNodeGTDComplex (const TableExprNodeRep&);
    ~TableExprNodeGTDComplex();
    Bool getBool (uInt rownr);
};

class TableExprNodeGTString : public TableExprNodeBinary
{
public:
    TableExprNodeGTString (const TableExprNodeRep&);
    ~TableExprNodeGTString();
    Bool getBool (uInt rownr);
};

class TableExprNodeGTDate : public TableExprNodeBinary
{
public:
    TableExprNodeGTDate (const TableExprNodeRep&);
    ~TableExprNodeGTDate();
    Bool getBool (uInt rownr);
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

class TableExprNodeGEDouble : public TableExprNodeBinary
{
public:
    TableExprNodeGEDouble (const TableExprNodeRep&);
    ~TableExprNodeGEDouble();
    Bool getBool (uInt rownr);
    void ranges (Block<TableExprRange>&);
};

class TableExprNodeGEDComplex : public TableExprNodeBinary
{
public:
    TableExprNodeGEDComplex (const TableExprNodeRep&);
    ~TableExprNodeGEDComplex();
    Bool getBool (uInt rownr);
};

class TableExprNodeGEString : public TableExprNodeBinary
{
public:
    TableExprNodeGEString (const TableExprNodeRep&);
    ~TableExprNodeGEString();
    Bool getBool (uInt rownr);
};

class TableExprNodeGEDate : public TableExprNodeBinary
{
public:
    TableExprNodeGEDate (const TableExprNodeRep&);
    ~TableExprNodeGEDate();
    Bool getBool (uInt rownr);
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

class TableExprNodeINDouble : public TableExprNodeBinary
{
public:
    TableExprNodeINDouble (const TableExprNodeRep&);
    ~TableExprNodeINDouble();
    Bool getBool (uInt rownr);
};

class TableExprNodeINDComplex : public TableExprNodeBinary
{
public:
    TableExprNodeINDComplex (const TableExprNodeRep&);
    ~TableExprNodeINDComplex();
    Bool getBool (uInt rownr);
};

class TableExprNodeINString : public TableExprNodeBinary
{
public:
    TableExprNodeINString (const TableExprNodeRep&);
    ~TableExprNodeINString();
    Bool getBool (uInt rownr);
};

class TableExprNodeINDate : public TableExprNodeBinary
{
public:
    TableExprNodeINDate (const TableExprNodeRep&);
    ~TableExprNodeINDate();
    Bool getBool (uInt rownr);
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

class TableExprNodeOR : public TableExprNodeBinary
{
public:
    TableExprNodeOR (const TableExprNodeRep&);
    ~TableExprNodeOR();
    Bool getBool (uInt rownr);
    void ranges (Block<TableExprRange>&);
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

class TableExprNodeAND: public TableExprNodeBinary
{
public:
    TableExprNodeAND (const TableExprNodeRep&);
    ~TableExprNodeAND();
    Bool getBool (uInt rownr);
    void ranges (Block<TableExprRange>&);
};


// <summary>
// Logical not in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
// </prerequisite>

// <synopsis> 
// This class represents a logical not in a table select expression tree.
// This is defined for Bool only.
// </synopsis> 

class TableExprNodeNOT: public TableExprNodeBinary
{
public:
    TableExprNodeNOT (const TableExprNodeRep&);
    ~TableExprNodeNOT();
    Bool getBool (uInt rownr);
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

class TableExprNodeMIN : public TableExprNodeBinary
{
public:
    TableExprNodeMIN (const TableExprNodeRep&);
    ~TableExprNodeMIN();
    Double   getDouble   (uInt rownr);
    DComplex getDComplex (uInt rownr);
};



// <summary>
// Constant in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
// </prerequisite>

// <synopsis> 
// This class represents a constant in a table select expression tree.
// This is also used to hold the value of a table keyword, which is
// constant over the entire table.
// </synopsis> 

class TableExprNodeConstBool : public TableExprNodeBinary
{
public:
    TableExprNodeConstBool (const Bool& value);
    ~TableExprNodeConstBool();
    Bool getBool (uInt rownr);
private:
    Bool value_p;
};

class TableExprNodeConstDouble : public TableExprNodeBinary
{
public:
    TableExprNodeConstDouble (const Double& value);
    ~TableExprNodeConstDouble();
    Double   getDouble   (uInt rownr);
    DComplex getDComplex (uInt rownr);
private:
    Double value_p;
};

class TableExprNodeConstDComplex : public TableExprNodeBinary
{
public:
    TableExprNodeConstDComplex (const DComplex& value);
    ~TableExprNodeConstDComplex();
    DComplex getDComplex (uInt rownr);
private:
    DComplex value_p;
};

class TableExprNodeConstString : public TableExprNodeBinary
{
public:
    TableExprNodeConstString (const String& value);
    ~TableExprNodeConstString();
    String getString (uInt rownr);
private:
    String value_p;
};

class TableExprNodeConstRegex : public TableExprNodeBinary
{
public:
    TableExprNodeConstRegex (const Regex& value);
    ~TableExprNodeConstRegex();
    Regex getRegex (uInt rownr);
private:
    Regex value_p;
};

class TableExprNodeConstDate : public TableExprNodeBinary
{
public:
    TableExprNodeConstDate (const MVTime& value);
    ~TableExprNodeConstDate();
    Double getDouble(uInt rownr);
    MVTime getDate  (uInt rownr);
private:
    MVTime value_p;
};



// <summary>
// Scalar column in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>
//
// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
// </prerequisite>

// <synopsis> 
// This class represents a scalar column in a table select expression tree.
// When the select expression gets evaluated, the value of the
// given row in the column is used.
// </synopsis> 


class TableExprNodeColumn : public TableExprNodeBinary
{
public:
    TableExprNodeColumn (const Table&, const BaseTable*,
			 const String& columnName);
    ~TableExprNodeColumn();

    // Replace the Table pointer in this node.
    virtual void replaceTablePtr (const Table&, const BaseTable*);

    Bool     getBool     (uInt rownr);
    Double   getDouble   (uInt rownr);
    DComplex getDComplex (uInt rownr);
    String   getString   (uInt rownr);
    const ROTableColumn& getColumn() const;

    // Get the data type of this scalar column.
    Bool getColumnDataType (DataType&) const;

    Array<Bool>     getColumnBool();
    Array<uChar>    getColumnuChar();
    Array<Short>    getColumnShort();
    Array<uShort>   getColumnuShort();
    Array<Int>      getColumnInt();
    Array<uInt>     getColumnuInt();
    Array<Float>    getColumnFloat();
    Array<Double>   getColumnDouble();
    Array<Complex>  getColumnComplex();
    Array<DComplex> getColumnDComplex();
    Array<String>   getColumnString();

protected:
    ROTableColumn* tabColPtr_p;                //# pointer to table column
};



// <summary>
// Rownumber in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
// </prerequisite>

// <synopsis>
// This class represents the rownumber() function in a table
// select expression tree.
// The origin is stored to indicate whether the first rownumber
// should be zero (in C++)  or an other value (1 in TaQL) 
// </synopsis> 

class TableExprNodeRownr : public TableExprNodeBinary
{
public:
    TableExprNodeRownr (const BaseTable*, uInt origin);
    ~TableExprNodeRownr();
    Double getDouble (uInt rownr);
private:
    uInt origin_p;
};



// <summary>
// Random number in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
// </prerequisite>

// <synopsis>
// This class represents the rand() function in a table
// select expression tree.
// </synopsis> 

class TableExprNodeRandom : public TableExprNodeBinary
{
public:
    TableExprNodeRandom (const BaseTable*);
    ~TableExprNodeRandom();
    Double getDouble (uInt rownr);
private:
    MLCG    generator_p;
    Uniform random_p;
};



#endif
