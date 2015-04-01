//# ExprDerArrayNode.h: Nodes representing constant arrays in table select expression tree
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

#ifndef TABLES_EXPRDERNODEARRAY_H
#define TABLES_EXPRDERNODEARRAY_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNodeArray.h>
#include <casacore/casa/Arrays/Array.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations


// <summary>
// Bool Array constant in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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
    Array<Bool> getArrayBool (const TableExprId& id);
private:
    Array<Bool> value_p;
};


// <summary>
// Int Array constant in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeArrayConstInt : public TableExprNodeArray
{
public:
    TableExprNodeArrayConstInt (const Array<Int64>& value);
    TableExprNodeArrayConstInt (const Array<uInt>& value);
    TableExprNodeArrayConstInt (const Array<Int>& value);
    TableExprNodeArrayConstInt (const Array<uShort>& value);
    TableExprNodeArrayConstInt (const Array<Short>& value);
    TableExprNodeArrayConstInt (const Array<uChar>& value);
    ~TableExprNodeArrayConstInt();
    Array<Int64>    getArrayInt      (const TableExprId& id);
    Array<Double>   getArrayDouble   (const TableExprId& id);
    Array<DComplex> getArrayDComplex (const TableExprId& id);
private:
    Array<Int64> value_p;
};


// <summary>
// Double Array constant in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeArrayConstDouble : public TableExprNodeArray
{
public:
    TableExprNodeArrayConstDouble (const Array<Double>& value);
    TableExprNodeArrayConstDouble (const Array<Float>& value);
    TableExprNodeArrayConstDouble (const Array<Int64>& value);
    ~TableExprNodeArrayConstDouble();
    Array<Double>   getArrayDouble   (const TableExprId& id);
    Array<DComplex> getArrayDComplex (const TableExprId& id);
private:
    Array<Double> value_p;
};


// <summary>
// DComplex Array constant in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeArrayConstDComplex : public TableExprNodeArray
{
public:
    TableExprNodeArrayConstDComplex (const Array<DComplex>& value);
    TableExprNodeArrayConstDComplex (const Array<Complex>& value);
    TableExprNodeArrayConstDComplex (const Array<Double>& value);
    TableExprNodeArrayConstDComplex (const Array<Int64>& value);
    ~TableExprNodeArrayConstDComplex();
    Array<DComplex> getArrayDComplex (const TableExprId& id);
private:
    Array<DComplex> value_p;
};


// <summary>
// String Array constant in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeArrayConstString : public TableExprNodeArray
{
public:
    TableExprNodeArrayConstString (const Array<String>& value);
    ~TableExprNodeArrayConstString();
    Array<String> getArrayString (const TableExprId& id);
private:
    Array<String> value_p;
};


// <summary>
// Date Array constant in table select expression tree
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
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

class TableExprNodeArrayConstDate : public TableExprNodeArray
{
public:
    TableExprNodeArrayConstDate (const Array<MVTime>& value);
    ~TableExprNodeArrayConstDate();
    Array<Double> getArrayDouble(const TableExprId& id);
    Array<MVTime> getArrayDate  (const TableExprId& id);
private:
    Array<MVTime> value_p;
};




} //# NAMESPACE CASACORE - END

#endif
