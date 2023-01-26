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

#ifndef TABLES_EXPRLOGICNODE_H
#define TABLES_EXPRLOGICNODE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNodeRep.h>
#include <set>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# This file defines classes derived from TableExprNode representing
//# the data type and operator in a table expression.
//#
//# Data types bool, int64_t, double, DComplex and String are used.
//# char, unsigned char, int16_t, uint16_t, int32_t, and uint32_t are converted to int64_t,
//# float to double, and Complex to DComplex.
//# Binary operators ==, >=, >, <, <=, !=, and IN are recognized.
//# Also &&, ||, and unary ! are recognized.



// <summary>
// bool comparison == in table select expression tree
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeEQBool : public TableExprNodeBinary
{
public:
    TableExprNodeEQBool (const TableExprNodeRep&);
    ~TableExprNodeEQBool() = default;
    bool getBool (const TableExprId& id) override;
};


// <summary>
// int32_t comparison == in table select expression tree
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeEQInt : public TableExprNodeBinary
{
public:
    TableExprNodeEQInt (const TableExprNodeRep&);
    ~TableExprNodeEQInt() = default;
    bool getBool (const TableExprId& id) override;
};


// <summary>
// double comparison == in table select expression tree
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeEQDouble : public TableExprNodeBinary
{
public:
    TableExprNodeEQDouble (const TableExprNodeRep&);
    ~TableExprNodeEQDouble() = default;
    bool getBool (const TableExprId& id) override;
    void ranges (Block<TableExprRange>&) override;
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeEQDComplex : public TableExprNodeBinary
{
public:
    TableExprNodeEQDComplex (const TableExprNodeRep&);
    ~TableExprNodeEQDComplex() = default;
    bool getBool (const TableExprId& id) override;
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeEQString : public TableExprNodeBinary
{
public:
    TableExprNodeEQString (const TableExprNodeRep&);
    ~TableExprNodeEQString() = default;
    bool getBool (const TableExprId& id) override;
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeEQRegex : public TableExprNodeBinary
{
public:
    TableExprNodeEQRegex (const TableExprNodeRep&);
    ~TableExprNodeEQRegex() = default;
    bool getBool (const TableExprId& id) override;
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeEQDate : public TableExprNodeBinary
{
public:
    TableExprNodeEQDate (const TableExprNodeRep&);
    ~TableExprNodeEQDate() = default;
    bool getBool (const TableExprId& id) override;
};



// <summary>
// bool comparison != in table select expression tree
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeNEBool : public TableExprNodeBinary
{
public:
    TableExprNodeNEBool (const TableExprNodeRep&);
    ~TableExprNodeNEBool() = default;
    bool getBool (const TableExprId& id) override;
};


// <summary>
// int32_t comparison != in table select expression tree
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeNEInt : public TableExprNodeBinary
{
public:
    TableExprNodeNEInt (const TableExprNodeRep&);
    ~TableExprNodeNEInt() = default;
    bool getBool (const TableExprId& id) override;
};


// <summary>
// double comparison != in table select expression tree
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeNEDouble : public TableExprNodeBinary
{
public:
    TableExprNodeNEDouble (const TableExprNodeRep&);
    ~TableExprNodeNEDouble() = default;
    bool getBool (const TableExprId& id) override;
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeNEDComplex : public TableExprNodeBinary
{
public:
    TableExprNodeNEDComplex (const TableExprNodeRep&);
    ~TableExprNodeNEDComplex() = default;
    bool getBool (const TableExprId& id) override;
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeNEString : public TableExprNodeBinary
{
public:
    TableExprNodeNEString (const TableExprNodeRep&);
    ~TableExprNodeNEString() = default;
    bool getBool (const TableExprId& id) override;
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeNERegex : public TableExprNodeBinary
{
public:
    TableExprNodeNERegex (const TableExprNodeRep&);
    ~TableExprNodeNERegex() = default;
    bool getBool (const TableExprId& id) override;
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeNEDate : public TableExprNodeBinary
{
public:
    TableExprNodeNEDate (const TableExprNodeRep&);
    ~TableExprNodeNEDate() = default;
    bool getBool (const TableExprId& id) override;
};



// <summary>
// int32_t comparison > in table select expression tree
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeGTInt : public TableExprNodeBinary
{
public:
    TableExprNodeGTInt (const TableExprNodeRep&);
    ~TableExprNodeGTInt() = default;
    bool getBool (const TableExprId& id) override;
};


// <summary>
// double comparison > in table select expression tree
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeGTDouble : public TableExprNodeBinary
{
public:
    TableExprNodeGTDouble (const TableExprNodeRep&);
    ~TableExprNodeGTDouble() = default;
    bool getBool (const TableExprId& id) override;
    void ranges (Block<TableExprRange>&) override;
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeGTDComplex : public TableExprNodeBinary
{
public:
    TableExprNodeGTDComplex (const TableExprNodeRep&);
    ~TableExprNodeGTDComplex() = default;
    bool getBool (const TableExprId& id) override;
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeGTString : public TableExprNodeBinary
{
public:
    TableExprNodeGTString (const TableExprNodeRep&);
    ~TableExprNodeGTString() = default;
    bool getBool (const TableExprId& id) override;
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeGTDate : public TableExprNodeBinary
{
public:
    TableExprNodeGTDate (const TableExprNodeRep&);
    ~TableExprNodeGTDate() = default;
    bool getBool (const TableExprId& id) override;
};



// <summary>
// int32_t comparison >= in table select expression tree
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeGEInt : public TableExprNodeBinary
{
public:
    TableExprNodeGEInt (const TableExprNodeRep&);
    ~TableExprNodeGEInt() = default;
    bool getBool (const TableExprId& id) override;
};


// <summary>
// double comparison >= in table select expression tree
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeGEDouble : public TableExprNodeBinary
{
public:
    TableExprNodeGEDouble (const TableExprNodeRep&);
    ~TableExprNodeGEDouble() = default;
    bool getBool (const TableExprId& id) override;
    void ranges (Block<TableExprRange>&) override;
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeGEDComplex : public TableExprNodeBinary
{
public:
    TableExprNodeGEDComplex (const TableExprNodeRep&);
    ~TableExprNodeGEDComplex() = default;
    bool getBool (const TableExprId& id) override;
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeGEString : public TableExprNodeBinary
{
public:
    TableExprNodeGEString (const TableExprNodeRep&);
    ~TableExprNodeGEString() = default;
    bool getBool (const TableExprId& id) override;
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeGEDate : public TableExprNodeBinary
{
public:
    TableExprNodeGEDate (const TableExprNodeRep&);
    ~TableExprNodeGEDate() = default;
    bool getBool (const TableExprId& id) override;
};



// <summary>
// int32_t comparison IN in table select expression tree
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// The right hand side can be optimized if it contains a constant array which
// can be replaced by an std::unordered_set<int64_t> or a Block<bool>.
// </synopsis> 

class TableExprNodeINInt : public TableExprNodeBinary
{
public:
    // <src>doTracing</src> is not used.
    TableExprNodeINInt (const TableExprNodeRep&, bool doTracing=false);
    ~TableExprNodeINInt() = default;
    void optimize() override;
    static void doOptimize (TENShPtr& rnode);
    bool getBool (const TableExprId& id) override;
private:
};


// <summary>
// double comparison IN in table select expression tree
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// The right hand side can be optimized if it contains a constant set with
// bounded intervals.
// </synopsis> 

class TableExprNodeINDouble : public TableExprNodeBinary
{
public:
    TableExprNodeINDouble (const TableExprNodeRep&);
    ~TableExprNodeINDouble() = default;
    void optimize() override;
    static void doOptimize (TENShPtr& rnode);
    bool getBool (const TableExprId& id) override;
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeINDComplex : public TableExprNodeBinary
{
public:
    TableExprNodeINDComplex (const TableExprNodeRep&);
    ~TableExprNodeINDComplex() = default;
    bool getBool (const TableExprId& id) override;
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// The right hand side can be optimized if it contains a constant array which
// can be replaced by an std::unordered_set<String>
// </synopsis> 

class TableExprNodeINString : public TableExprNodeBinary
{
public:
    TableExprNodeINString (const TableExprNodeRep&);
    ~TableExprNodeINString() = default;
    void optimize() override;
    static void doOptimize (TENShPtr& rnode);
    bool getBool (const TableExprId& id) override;
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
// Only the bool get function is defined, because the result of a
// compare is always a bool.
// </synopsis> 

class TableExprNodeINDate : public TableExprNodeBinary
{
public:
    TableExprNodeINDate (const TableExprNodeRep&);
    ~TableExprNodeINDate() = default;
    void optimize() override;
    static void doOptimize (TENShPtr& rnode);
    bool getBool (const TableExprId& id) override;
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
// This is defined for bool only.
// </synopsis> 

class TableExprNodeOR : public TableExprNodeBinary
{
public:
    TableExprNodeOR (const TableExprNodeRep&);
    ~TableExprNodeOR() = default;
    bool getBool (const TableExprId& id) override;
    void ranges (Block<TableExprRange>&) override;
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
// This is defined for bool only.
// </synopsis> 

class TableExprNodeAND: public TableExprNodeBinary
{
public:
    TableExprNodeAND (const TableExprNodeRep&);
    ~TableExprNodeAND() = default;
    bool getBool (const TableExprId& id) override;
    void ranges (Block<TableExprRange>&) override;
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
// This is defined for bool only.
// </synopsis> 

class TableExprNodeNOT: public TableExprNodeBinary
{
public:
    TableExprNodeNOT (const TableExprNodeRep&);
    ~TableExprNodeNOT() = default;
    bool getBool (const TableExprId& id) override;
};




} //# NAMESPACE CASACORE - END

#endif
