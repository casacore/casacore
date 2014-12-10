//# ExprDerNode.h: Nodes representing scalars in table select expression tree
//# Copyright (C) 1994,1995,1996,1997,1999,2000,2001
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

#ifndef TABLES_EXPRDERNODE_H
#define TABLES_EXPRDERNODE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNodeRep.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicMath/Random.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableColumn;
class Table;

//# This file defines classes derived from TableExprNode representing
//# the data type and operator in a table expression.
//#
//# Data types Bool, Int64, Double, DComplex and String are used.
//# Char, uChar, Short, uShort, Int, and uInt are converted to Int64,
//# Float to Double, and Complex to DComplex.
//# Binary operators +, -, *, /, ==, >=, >, <, <= and != are recognized.
//# Also &&, ||, parentheses and unary +, - and ! are recognized.



// <summary>
// Constant Bool in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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
    Bool getBool (const TableExprId& id);
private:
    Bool value_p;
};


// <summary>
// Constant Int64 in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeConstInt : public TableExprNodeBinary
{
public:
    TableExprNodeConstInt (const Int64& value);
    ~TableExprNodeConstInt();
    Int64    getInt      (const TableExprId& id);
    Double   getDouble   (const TableExprId& id);
    DComplex getDComplex (const TableExprId& id);
private:
    Int64 value_p;
};


// <summary>
// Constant Double in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeConstDouble : public TableExprNodeBinary
{
public:
    TableExprNodeConstDouble (const Double& value);
    ~TableExprNodeConstDouble();
    Double   getDouble   (const TableExprId& id);
    DComplex getDComplex (const TableExprId& id);
private:
    Double value_p;
};


// <summary>
// Constant DComplex in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeConstDComplex : public TableExprNodeBinary
{
public:
    TableExprNodeConstDComplex (const DComplex& value);
    ~TableExprNodeConstDComplex();
    DComplex getDComplex (const TableExprId& id);
private:
    DComplex value_p;
};


// <summary>
// Constant String in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeConstString : public TableExprNodeBinary
{
public:
    TableExprNodeConstString (const String& value);
    ~TableExprNodeConstString();
    String getString (const TableExprId& id);
private:
    String value_p;
};


// <summary>
// Constant Regex or StringDistance in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeConstRegex : public TableExprNodeBinary
{
public:
    TableExprNodeConstRegex (const TaqlRegex& value);
    ~TableExprNodeConstRegex();
    TaqlRegex getRegex (const TableExprId& id);
private:
    TaqlRegex      value_p;
    StringDistance dist_p;
};


// <summary>
// Constant Date in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeConstDate : public TableExprNodeBinary
{
public:
    TableExprNodeConstDate (const MVTime& value);
    ~TableExprNodeConstDate();
    Double getDouble(const TableExprId& id);
    MVTime getDate  (const TableExprId& id);
private:
    MVTime value_p;
};



// <summary>
// Scalar column in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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
    TableExprNodeColumn (const Table&, const String& columnName);
    ~TableExprNodeColumn();

    // This node represents a table column.
    virtual void getColumnNodes (vector<TableExprNodeRep*>& cols);
  
    // Do not apply the selection.
    virtual void disableApplySelection();

    // Re-create the column object for a selection of rows.
    virtual void applySelection (const Vector<uInt>& rownrs);

    // Get the data type of this scalar column.
    Bool getColumnDataType (DataType&) const;

    // Get the data for the given id.
    Bool     getBool     (const TableExprId& id);
    Int64    getInt      (const TableExprId& id);
    Double   getDouble   (const TableExprId& id);
    DComplex getDComplex (const TableExprId& id);
    String   getString   (const TableExprId& id);
    const TableColumn& getColumn() const;

    // Get the data for the given rows.
    Array<Bool>     getColumnBool (const Vector<uInt>& rownrs);
    Array<uChar>    getColumnuChar (const Vector<uInt>& rownrs);
    Array<Short>    getColumnShort (const Vector<uInt>& rownrs);
    Array<uShort>   getColumnuShort (const Vector<uInt>& rownrs);
    Array<Int>      getColumnInt (const Vector<uInt>& rownrs);
    Array<uInt>     getColumnuInt (const Vector<uInt>& rownrs);
    Array<Float>    getColumnFloat (const Vector<uInt>& rownrs);
    Array<Double>   getColumnDouble (const Vector<uInt>& rownrs);
    Array<Complex>  getColumnComplex (const Vector<uInt>& rownrs);
    Array<DComplex> getColumnDComplex (const Vector<uInt>& rownrs);
    Array<String>   getColumnString (const Vector<uInt>& rownrs);

    // Get the column unit (can be empty).
    static Unit getColumnUnit (const TableColumn&);

protected:
    Table       selTable_p;
    TableColumn tabCol_p;
    Bool        applySelection_p;
};



// <summary>
// Rownumber in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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
    TableExprNodeRownr (const Table&, uInt origin);
    ~TableExprNodeRownr();
    Int64  getInt (const TableExprId& id);
private:
    uInt origin_p;
};



// <summary>
// Rowid in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableExprNode
// </prerequisite>

// <synopsis>
// This class represents the rowid() function in a table
// select expression tree.
// It is meant to get the original row number in a GIVING clause,
// but, of course, it can also be used in the SELECT clause.
// The row number returned is 0-based.
// </synopsis> 

class TableExprNodeRowid : public TableExprNodeBinary
{
public:
    TableExprNodeRowid (const Table&);
    ~TableExprNodeRowid();
    virtual void applySelection (const Vector<uInt>& rownrs);
    Int64 getInt (const TableExprId& id);
private:
    Vector<uInt> rownrs_p;
};



// <summary>
// Random number in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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
    TableExprNodeRandom (const Table&);
    ~TableExprNodeRandom();
    Double getDouble (const TableExprId& id);
private:
    MLCG    generator_p;
    Uniform random_p;
};



} //# NAMESPACE CASACORE - END

#endif
