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

#ifndef TABLES_EXPRDERNODEARRAY_H
#define TABLES_EXPRDERNODEARRAY_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/ExprNodeArray.h>
#include <casacore/casa/Arrays/Array.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations


// <summary>
// bool Array constant in table select expression tree
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
    TableExprNodeArrayConstBool (const Array<bool>& value);
    TableExprNodeArrayConstBool (const MArray<bool>& value);
    ~TableExprNodeArrayConstBool();
    MArray<bool> getArrayBool (const TableExprId& id);
private:
    MArray<bool> value_p;
};


// <summary>
// int32_t Array constant in table select expression tree
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
    TableExprNodeArrayConstInt (const Array<uint64_t>& value);
    TableExprNodeArrayConstInt (const Array<int64_t>& value);
    TableExprNodeArrayConstInt (const Array<uint32_t>& value);
    TableExprNodeArrayConstInt (const Array<int32_t>& value);
    TableExprNodeArrayConstInt (const Array<uint16_t>& value);
    TableExprNodeArrayConstInt (const Array<int16_t>& value);
    TableExprNodeArrayConstInt (const Array<unsigned char>& value);
    TableExprNodeArrayConstInt (const MArray<uint64_t>& value);
    TableExprNodeArrayConstInt (const MArray<int64_t>& value);
    TableExprNodeArrayConstInt (const MArray<uint32_t>& value);
    TableExprNodeArrayConstInt (const MArray<int32_t>& value);
    TableExprNodeArrayConstInt (const MArray<uint16_t>& value);
    TableExprNodeArrayConstInt (const MArray<int16_t>& value);
    TableExprNodeArrayConstInt (const MArray<unsigned char>& value);
    ~TableExprNodeArrayConstInt();
    MArray<int64_t>    getArrayInt      (const TableExprId& id);
    MArray<double>   getArrayDouble   (const TableExprId& id);
    MArray<DComplex> getArrayDComplex (const TableExprId& id);
private:
    MArray<int64_t> value_p;
};


// <summary>
// double Array constant in table select expression tree
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
    TableExprNodeArrayConstDouble (const Array<double>& value);
    TableExprNodeArrayConstDouble (const Array<float>& value);
    TableExprNodeArrayConstDouble (const Array<int64_t>& value);
    TableExprNodeArrayConstDouble (const MArray<double>& value);
    TableExprNodeArrayConstDouble (const MArray<float>& value);
    TableExprNodeArrayConstDouble (const MArray<int64_t>& value);
    ~TableExprNodeArrayConstDouble();
    MArray<double>   getArrayDouble   (const TableExprId& id);
    MArray<DComplex> getArrayDComplex (const TableExprId& id);
private:
    MArray<double> value_p;
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
    TableExprNodeArrayConstDComplex (const Array<double>& value);
    TableExprNodeArrayConstDComplex (const Array<int64_t>& value);
    TableExprNodeArrayConstDComplex (const MArray<DComplex>& value);
    TableExprNodeArrayConstDComplex (const MArray<Complex>& value);
    TableExprNodeArrayConstDComplex (const MArray<double>& value);
    TableExprNodeArrayConstDComplex (const MArray<int64_t>& value);
    ~TableExprNodeArrayConstDComplex();
    MArray<DComplex> getArrayDComplex (const TableExprId& id);
private:
    MArray<DComplex> value_p;
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
    TableExprNodeArrayConstString (const MArray<String>& value);
    ~TableExprNodeArrayConstString();
    MArray<String> getArrayString (const TableExprId& id);
private:
    MArray<String> value_p;
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
    TableExprNodeArrayConstDate (const MArray<MVTime>& value);
    ~TableExprNodeArrayConstDate();
    MArray<double> getArrayDouble(const TableExprId& id);
    MArray<MVTime> getArrayDate  (const TableExprId& id);
private:
    MArray<MVTime> value_p;
};




} //# NAMESPACE CASACORE - END

#endif
