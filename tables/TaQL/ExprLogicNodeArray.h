//# ExprLogicArrayNode.h: Nodes representing logical array operators in table select expression tree
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

#ifndef TABLES_EXPRLOGICNODEARRAY_H
#define TABLES_EXPRLOGICNODEARRAY_H

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
//# Binary operators ==, >=, >, <, <= and != are recognized.
//# Also &&, ||, and unary ! are recognized.



// <summary>
// Bool Array comparison == in table select expression tree
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

class TableExprNodeArrayEQBool : public TableExprNodeArray
{
public:
    TableExprNodeArrayEQBool (const TableExprNodeRep&);
    ~TableExprNodeArrayEQBool();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// Int Array comparison == in table select expression tree
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

class TableExprNodeArrayEQInt : public TableExprNodeArray
{
public:
    TableExprNodeArrayEQInt (const TableExprNodeRep&);
    ~TableExprNodeArrayEQInt();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// Double Array comparison == in table select expression tree
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

class TableExprNodeArrayEQDouble : public TableExprNodeArray
{
public:
    TableExprNodeArrayEQDouble (const TableExprNodeRep&);
    ~TableExprNodeArrayEQDouble();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// DComplex Array comparison == in table select expression tree
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

class TableExprNodeArrayEQDComplex : public TableExprNodeArray
{
public:
    TableExprNodeArrayEQDComplex (const TableExprNodeRep&);
    ~TableExprNodeArrayEQDComplex();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// String Array comparison == in table select expression tree
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

class TableExprNodeArrayEQString : public TableExprNodeArray
{
public:
    TableExprNodeArrayEQString (const TableExprNodeRep&);
    ~TableExprNodeArrayEQString();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// Regex Array comparison == in table select expression tree
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

class TableExprNodeArrayEQRegex : public TableExprNodeArray
{
public:
    TableExprNodeArrayEQRegex (const TableExprNodeRep&);
    ~TableExprNodeArrayEQRegex();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// Date Array comparison == in table select expression tree
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

class TableExprNodeArrayEQDate : public TableExprNodeArray
{
public:
    TableExprNodeArrayEQDate (const TableExprNodeRep&);
    ~TableExprNodeArrayEQDate();
    Array<Bool> getArrayBool (const TableExprId& id);
};



// <summary>
// Bool Array comparison != in table select expression tree
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

class TableExprNodeArrayNEBool : public TableExprNodeArray
{
public:
    TableExprNodeArrayNEBool (const TableExprNodeRep&);
    ~TableExprNodeArrayNEBool();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// Int Array comparison != in table select expression tree
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

class TableExprNodeArrayNEInt : public TableExprNodeArray
{
public:
    TableExprNodeArrayNEInt (const TableExprNodeRep&);
    ~TableExprNodeArrayNEInt();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// Double Array comparison != in table select expression tree
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

class TableExprNodeArrayNEDouble : public TableExprNodeArray
{
public:
    TableExprNodeArrayNEDouble (const TableExprNodeRep&);
    ~TableExprNodeArrayNEDouble();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// DComplex Array comparison != in table select expression tree
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

class TableExprNodeArrayNEDComplex : public TableExprNodeArray
{
public:
    TableExprNodeArrayNEDComplex (const TableExprNodeRep&);
    ~TableExprNodeArrayNEDComplex();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// String Array comparison != in table select expression tree
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

class TableExprNodeArrayNEString : public TableExprNodeArray
{
public:
    TableExprNodeArrayNEString (const TableExprNodeRep&);
    ~TableExprNodeArrayNEString();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// Regex Array comparison != in table select expression tree
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

class TableExprNodeArrayNERegex : public TableExprNodeArray
{
public:
    TableExprNodeArrayNERegex (const TableExprNodeRep&);
    ~TableExprNodeArrayNERegex();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// Date Array comparison != in table select expression tree
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

class TableExprNodeArrayNEDate : public TableExprNodeArray
{
public:
    TableExprNodeArrayNEDate (const TableExprNodeRep&);
    ~TableExprNodeArrayNEDate();
    Array<Bool> getArrayBool (const TableExprId& id);
};



// <summary>
// Int Array comparison > in table select expression tree
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

class TableExprNodeArrayGTInt : public TableExprNodeArray
{
public:
    TableExprNodeArrayGTInt (const TableExprNodeRep&);
    ~TableExprNodeArrayGTInt();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// Double Array comparison > in table select expression tree
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

class TableExprNodeArrayGTDouble : public TableExprNodeArray
{
public:
    TableExprNodeArrayGTDouble (const TableExprNodeRep&);
    ~TableExprNodeArrayGTDouble();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// DComplex Array comparison > in table select expression tree
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

class TableExprNodeArrayGTDComplex : public TableExprNodeArray
{
public:
    TableExprNodeArrayGTDComplex (const TableExprNodeRep&);
    ~TableExprNodeArrayGTDComplex();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// String Array comparison > in table select expression tree
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

class TableExprNodeArrayGTString : public TableExprNodeArray
{
public:
    TableExprNodeArrayGTString (const TableExprNodeRep&);
    ~TableExprNodeArrayGTString();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// Date Array comparison > in table select expression tree
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

class TableExprNodeArrayGTDate : public TableExprNodeArray
{
public:
    TableExprNodeArrayGTDate (const TableExprNodeRep&);
    ~TableExprNodeArrayGTDate();
    Array<Bool> getArrayBool (const TableExprId& id);
};



// <summary>
// Int Array comparison >= in table select expression tree
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

class TableExprNodeArrayGEInt : public TableExprNodeArray
{
public:
    TableExprNodeArrayGEInt (const TableExprNodeRep&);
    ~TableExprNodeArrayGEInt();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// Double Array comparison >= in table select expression tree
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

class TableExprNodeArrayGEDouble : public TableExprNodeArray
{
public:
    TableExprNodeArrayGEDouble (const TableExprNodeRep&);
    ~TableExprNodeArrayGEDouble();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// DComplex Array comparison >= in table select expression tree
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

class TableExprNodeArrayGEDComplex : public TableExprNodeArray
{
public:
    TableExprNodeArrayGEDComplex (const TableExprNodeRep&);
    ~TableExprNodeArrayGEDComplex();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// String Array comparison >= in table select expression tree
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

class TableExprNodeArrayGEString : public TableExprNodeArray
{
public:
    TableExprNodeArrayGEString (const TableExprNodeRep&);
    ~TableExprNodeArrayGEString();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// Date Array comparison >= in table select expression tree
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

class TableExprNodeArrayGEDate : public TableExprNodeArray
{
public:
    TableExprNodeArrayGEDate (const TableExprNodeRep&);
    ~TableExprNodeArrayGEDate();
    Array<Bool> getArrayBool (const TableExprId& id);
};



// <summary>
// Int Array comparison IN in table select expression tree
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

class TableExprNodeArrayINInt : public TableExprNodeArray
{
public:
    TableExprNodeArrayINInt (const TableExprNodeRep&);
    ~TableExprNodeArrayINInt();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// Double Array comparison IN in table select expression tree
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

class TableExprNodeArrayINDouble : public TableExprNodeArray
{
public:
    TableExprNodeArrayINDouble (const TableExprNodeRep&);
    ~TableExprNodeArrayINDouble();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// DComplex Array comparison IN in table select expression tree
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

class TableExprNodeArrayINDComplex : public TableExprNodeArray
{
public:
    TableExprNodeArrayINDComplex (const TableExprNodeRep&);
    ~TableExprNodeArrayINDComplex();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// String Array comparison IN in table select expression tree
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

class TableExprNodeArrayINString : public TableExprNodeArray
{
public:
    TableExprNodeArrayINString (const TableExprNodeRep&);
    ~TableExprNodeArrayINString();
    Array<Bool> getArrayBool (const TableExprId& id);
};


// <summary>
// Date Array comparison IN in table select expression tree
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

class TableExprNodeArrayINDate : public TableExprNodeArray
{
public:
    TableExprNodeArrayINDate (const TableExprNodeRep&);
    ~TableExprNodeArrayINDate();
    Array<Bool> getArrayBool (const TableExprId& id);
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

class TableExprNodeArrayOR : public TableExprNodeArray
{
public:
    TableExprNodeArrayOR (const TableExprNodeRep&);
    ~TableExprNodeArrayOR();
    Array<Bool> getArrayBool (const TableExprId& id);
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

class TableExprNodeArrayAND: public TableExprNodeArray
{
public:
    TableExprNodeArrayAND (const TableExprNodeRep&);
    ~TableExprNodeArrayAND();
    Array<Bool> getArrayBool (const TableExprId& id);
};



// <summary>
// Logical not in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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
    Array<Bool> getArrayBool (const TableExprId& id);
};




} //# NAMESPACE CASACORE - END

#endif
