//# ExprLogicNode.h: Nodes representing scalar logical operators in table select expression tree
//# Copyright (C) 1994,1995,1996,1997,1999
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

#if !defined(AIPS_EXPRLOGICNODE_H)
#define AIPS_EXPRLOGICNODE_H

//# Includes
#include <aips/aips.h>
#include <aips/Tables/ExprNodeRep.h>


//# This file defines classes derived from TableExprNode representing
//# the data type and operator in a table expression.
//#
//# Data types Bool, Double, DComplex and String are used.
//# Char, uChar, Short, uShort, Int, uInt and float are converted 
//# to Double, and Complex to DComplex.
//# Binary operators +, -, *, /, ==, >=, >, <, <= and != are recognized.
//# Also &&, ||, parentheses and unary +, - and ! are recognized.



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



#endif
