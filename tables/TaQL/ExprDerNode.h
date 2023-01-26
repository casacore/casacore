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
//# Data types bool, int64_t, double, DComplex and String are used.
//# char, unsigned char, int16_t, uint16_t, int32_t, and uint32_t are converted to int64_t,
//# float to double, and Complex to DComplex.
//# Binary operators +, -, *, /, ==, >=, >, <, <= and != are recognized.
//# Also &&, ||, parentheses and unary +, - and ! are recognized.



// <summary>
// Constant bool in table select expression tree
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
    TableExprNodeConstBool (const bool& value);
    ~TableExprNodeConstBool() override = default;
    bool getBool (const TableExprId& id) override;
private:
    bool value_p;
};


// <summary>
// Constant int64_t in table select expression tree
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
    TableExprNodeConstInt (const int64_t& value);
    ~TableExprNodeConstInt() override = default;
    int64_t    getInt      (const TableExprId& id) override;
    double   getDouble   (const TableExprId& id) override;
    DComplex getDComplex (const TableExprId& id) override;
private:
    int64_t value_p;
};


// <summary>
// Constant double in table select expression tree
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
    TableExprNodeConstDouble (const double& value);
    ~TableExprNodeConstDouble() override = default;
    double   getDouble   (const TableExprId& id) override;
    DComplex getDComplex (const TableExprId& id) override;
private:
    double value_p;
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
    ~TableExprNodeConstDComplex() override = default;
    DComplex getDComplex (const TableExprId& id) override;
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
    ~TableExprNodeConstString() override = default;
    String getString (const TableExprId& id) override;
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
    ~TableExprNodeConstRegex() override = default;
    TaqlRegex getRegex (const TableExprId& id) override;
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
    ~TableExprNodeConstDate() override = default;
    double getDouble(const TableExprId& id) override;
    MVTime getDate  (const TableExprId& id) override;
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
    TableExprNodeColumn (const TableExprInfo&, const String& columnName);
    ~TableExprNodeColumn() override = default;

    // Get the table info for this column.
    TableExprInfo getTableInfo() const override;

    // Do not apply the selection.
    void disableApplySelection() override;

    // Re-create the column object for a selection of rows.
    void applySelection (const Vector<rownr_t>& rownrs) override;

    // Get the data type of this scalar column.
    bool getColumnDataType (DataType&) const override;

    // Get the data for the given id.
    bool     getBool     (const TableExprId& id) override;
    int64_t    getInt      (const TableExprId& id) override;
    double   getDouble   (const TableExprId& id) override;
    DComplex getDComplex (const TableExprId& id) override;
    String   getString   (const TableExprId& id) override;
    const TableColumn& getColumn() const;

    // Get the data for the given rows.
    Array<bool>     getColumnBool (const Vector<rownr_t>& rownrs) override;
    Array<unsigned char>    getColumnuChar (const Vector<rownr_t>& rownrs) override;
    Array<int16_t>    getColumnShort (const Vector<rownr_t>& rownrs) override;
    Array<uint16_t>   getColumnuShort (const Vector<rownr_t>& rownrs) override;
    Array<int32_t>      getColumnInt (const Vector<rownr_t>& rownrs) override;
    Array<uint32_t>     getColumnuInt (const Vector<rownr_t>& rownrs) override;
    Array<int64_t>    getColumnInt64 (const Vector<rownr_t>& rownrs) override;
    Array<float>    getColumnFloat (const Vector<rownr_t>& rownrs) override;
    Array<double>   getColumnDouble (const Vector<rownr_t>& rownrs) override;
    Array<Complex>  getColumnComplex (const Vector<rownr_t>& rownrs) override;
    Array<DComplex> getColumnDComplex (const Vector<rownr_t>& rownrs) override;
    Array<String>   getColumnString (const Vector<rownr_t>& rownrs) override;

    // Get the column unit (can be empty).
    static Unit getColumnUnit (const TableColumn&);

protected:
    TableExprInfo tableInfo_p;
    TableColumn   tabCol_p;
    bool          applySelection_p;
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
// should be zero (in C++)  or another value (1 in TaQL).
// </synopsis> 

class TableExprNodeRownr : public TableExprNodeBinary
{
public:
    TableExprNodeRownr (const TableExprInfo&, uint32_t origin);
    ~TableExprNodeRownr() override = default;
    TableExprInfo getTableInfo() const override;
    int64_t  getInt (const TableExprId& id) override;
private:
    TableExprInfo tableInfo_p;
    uint32_t          origin_p;
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
    TableExprNodeRowid (const TableExprInfo&);
    ~TableExprNodeRowid() override = default;
    TableExprInfo getTableInfo() const override;
    void applySelection (const Vector<rownr_t>& rownrs) override;
    int64_t getInt (const TableExprId& id) override;
private:
    TableExprInfo   tableInfo_p;
    Vector<rownr_t> rownrs_p;
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
    TableExprNodeRandom (const TableExprInfo&);
    ~TableExprNodeRandom() override = default;
    TableExprInfo getTableInfo() const override;
    double getDouble (const TableExprId& id) override;
private:
    TableExprInfo tableInfo_p;
    MLCG          generator_p;
    Uniform       random_p;
};



} //# NAMESPACE CASACORE - END

#endif
