//# ExprLogicNode.h: Nodes representing scalar logical operators in table select expression tree
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

#ifndef TABLES_EXPRLOGICNODE_H
#define TABLES_EXPRLOGICNODE_H

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
//# Binary operators ==, >=, >, <, <=, !=, and IN are recognized.
//# Also &&, ||, and unary ! are recognized.



// <summary>
// Bool comparison == in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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
    Bool getBool (const TableExprId& id);
};


// <summary>
// Int comparison == in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeEQInt : public TableExprNodeBinary
{
public:
    TableExprNodeEQInt (const TableExprNodeRep&);
    ~TableExprNodeEQInt();
    Bool getBool (const TableExprId& id);
};


// <summary>
// Double comparison == in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeEQDouble : public TableExprNodeBinary
{
public:
    TableExprNodeEQDouble (const TableExprNodeRep&);
    ~TableExprNodeEQDouble();
    Bool getBool (const TableExprId& id);
    void ranges (Block<TableExprRange>&);
};


// <summary>
// DComplex comparison == in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeEQDComplex : public TableExprNodeBinary
{
public:
    TableExprNodeEQDComplex (const TableExprNodeRep&);
    ~TableExprNodeEQDComplex();
    Bool getBool (const TableExprId& id);
};


// <summary>
// String comparison == in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeEQString : public TableExprNodeBinary
{
public:
    TableExprNodeEQString (const TableExprNodeRep&);
    ~TableExprNodeEQString();
    Bool getBool (const TableExprId& id);
};


// <summary>
// Regex comparison == in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeEQRegex : public TableExprNodeBinary
{
public:
    TableExprNodeEQRegex (const TableExprNodeRep&);
    ~TableExprNodeEQRegex();
    Bool getBool (const TableExprId& id);
};


// <summary>
// Date comparison == in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeEQDate : public TableExprNodeBinary
{
public:
    TableExprNodeEQDate (const TableExprNodeRep&);
    ~TableExprNodeEQDate();
    Bool getBool (const TableExprId& id);
};



// <summary>
// Bool comparison != in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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
    Bool getBool (const TableExprId& id);
};


// <summary>
// Int comparison != in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeNEInt : public TableExprNodeBinary
{
public:
    TableExprNodeNEInt (const TableExprNodeRep&);
    ~TableExprNodeNEInt();
    Bool getBool (const TableExprId& id);
};


// <summary>
// Double comparison != in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeNEDouble : public TableExprNodeBinary
{
public:
    TableExprNodeNEDouble (const TableExprNodeRep&);
    ~TableExprNodeNEDouble();
    Bool getBool (const TableExprId& id);
};


// <summary>
// DComplex comparison != in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeNEDComplex : public TableExprNodeBinary
{
public:
    TableExprNodeNEDComplex (const TableExprNodeRep&);
    ~TableExprNodeNEDComplex();
    Bool getBool (const TableExprId& id);
};


// <summary>
// String comparison != in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeNEString : public TableExprNodeBinary
{
public:
    TableExprNodeNEString (const TableExprNodeRep&);
    ~TableExprNodeNEString();
    Bool getBool (const TableExprId& id);
};


// <summary>
// Regex comparison != in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeNERegex : public TableExprNodeBinary
{
public:
    TableExprNodeNERegex (const TableExprNodeRep&);
    ~TableExprNodeNERegex();
    Bool getBool (const TableExprId& id);
};


// <summary>
// Date comparison != in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeNEDate : public TableExprNodeBinary
{
public:
    TableExprNodeNEDate (const TableExprNodeRep&);
    ~TableExprNodeNEDate();
    Bool getBool (const TableExprId& id);
};



// <summary>
// Int comparison > in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeGTInt : public TableExprNodeBinary
{
public:
    TableExprNodeGTInt (const TableExprNodeRep&);
    ~TableExprNodeGTInt();
    Bool getBool (const TableExprId& id);
};


// <summary>
// Double comparison > in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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
    Bool getBool (const TableExprId& id);
    void ranges (Block<TableExprRange>&);
};


// <summary>
// DComplex comparison > in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeGTDComplex : public TableExprNodeBinary
{
public:
    TableExprNodeGTDComplex (const TableExprNodeRep&);
    ~TableExprNodeGTDComplex();
    Bool getBool (const TableExprId& id);
};


// <summary>
// String comparison > in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeGTString : public TableExprNodeBinary
{
public:
    TableExprNodeGTString (const TableExprNodeRep&);
    ~TableExprNodeGTString();
    Bool getBool (const TableExprId& id);
};


// <summary>
// Date comparison > in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeGTDate : public TableExprNodeBinary
{
public:
    TableExprNodeGTDate (const TableExprNodeRep&);
    ~TableExprNodeGTDate();
    Bool getBool (const TableExprId& id);
};



// <summary>
// Int comparison >= in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeGEInt : public TableExprNodeBinary
{
public:
    TableExprNodeGEInt (const TableExprNodeRep&);
    ~TableExprNodeGEInt();
    Bool getBool (const TableExprId& id);
};


// <summary>
// Double comparison >= in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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
    Bool getBool (const TableExprId& id);
    void ranges (Block<TableExprRange>&);
};


// <summary>
// DComplex comparison >= in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeGEDComplex : public TableExprNodeBinary
{
public:
    TableExprNodeGEDComplex (const TableExprNodeRep&);
    ~TableExprNodeGEDComplex();
    Bool getBool (const TableExprId& id);
};


// <summary>
// String comparison >= in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeGEString : public TableExprNodeBinary
{
public:
    TableExprNodeGEString (const TableExprNodeRep&);
    ~TableExprNodeGEString();
    Bool getBool (const TableExprId& id);
};


// <summary>
// Date comparison >= in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeGEDate : public TableExprNodeBinary
{
public:
    TableExprNodeGEDate (const TableExprNodeRep&);
    ~TableExprNodeGEDate();
    Bool getBool (const TableExprId& id);
};



// <summary>
// Int comparison IN in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeINInt : public TableExprNodeBinary
{
public:
    TableExprNodeINInt (const TableExprNodeRep&, Bool doTracing=False);
    virtual ~TableExprNodeINInt();
    virtual void convertConstChild();
    virtual Bool getBool (const TableExprId& id);
private:
    Bool        itsDoTracing;
    //# If the right node is constant and its range is sufficiently small,
    //# it is turned into a Bool index for linear lookup time.
    Block<Bool> itsIndex;
    Int64       itsMin;
    Int64       itsMax;
};


// <summary>
// Double comparison IN in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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
    Bool getBool (const TableExprId& id);
};


// <summary>
// DComplex comparison IN in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeINDComplex : public TableExprNodeBinary
{
public:
    TableExprNodeINDComplex (const TableExprNodeRep&);
    ~TableExprNodeINDComplex();
    Bool getBool (const TableExprId& id);
};


// <summary>
// String comparison IN in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeINString : public TableExprNodeBinary
{
public:
    TableExprNodeINString (const TableExprNodeRep&);
    ~TableExprNodeINString();
    Bool getBool (const TableExprId& id);
};


// <summary>
// Date comparison IN in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeINDate : public TableExprNodeBinary
{
public:
    TableExprNodeINDate (const TableExprNodeRep&);
    ~TableExprNodeINDate();
    Bool getBool (const TableExprId& id);
};



// <summary>
// Logical or in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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
    Bool getBool (const TableExprId& id);
    void ranges (Block<TableExprRange>&);
};


// <summary>
// Logical and in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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
    Bool getBool (const TableExprId& id);
    void ranges (Block<TableExprRange>&);
};


// <summary>
// Logical not in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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
    Bool getBool (const TableExprId& id);
};




} //# NAMESPACE CASACORE - END

#endif
