//# ExprNodeRecord.h: Nodes representing fields in record select expression tree
//# Copyright (C) 2000
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

#ifndef TABLES_EXPRNODERECORD_H
#define TABLES_EXPRNODERECORD_H

//# Includes
#include <casacore/tables/TaQL/ExprNodeRep.h>
#include <casacore/tables/TaQL/ExprNodeArray.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Utilities/DataType.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class RecordDesc;
class RecordInterface;


//# This file defines classes derived from TableExprNode representing
//# fields in a record select expression.
//#
//# Data types bool, double, DComplex and String are used.
//# char, unsigned char, int16_t, uint16_t, int32_t and uint32_t are converted to int64_t,
//# float to double, and Complex to DComplex.
//# Binary operators +, -, *, /, ==, >=, >, <, <= and != are recognized.
//# Also &&, ||, parentheses and unary +, - and ! are recognized.



// <summary>
// Scalar field in record select expression tree
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


class TableExprNodeRecordField : public TableExprNodeBinary
{
public:
    TableExprNodeRecordField (DataType dtype,
                              const Block<int32_t>& fieldNumbers);
    ~TableExprNodeRecordField();

    virtual const IPosition& getShape (const TableExprId& id);
    virtual bool isDefined (const TableExprId& id);

    virtual bool     getBool     (const TableExprId& id);
    virtual int64_t    getInt      (const TableExprId& id);
    virtual double   getDouble   (const TableExprId& id);
    virtual DComplex getDComplex (const TableExprId& id);
    virtual String   getString   (const TableExprId& id);

protected:
    Block<int32_t> fieldNrs_p;
    uint32_t       lastEntry_p;

    // Get the record for the last field number, thus going through
    // all subrecords for the other field numbers.
    const RecordInterface& getRecord (const TableExprId& id) const;
};



// <summary>
// Array field in record select expression tree
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


class TableExprNodeRecordFieldArray : public TableExprNodeArray
{
public:
    TableExprNodeRecordFieldArray (DataType dtype,
                                   const Block<int32_t>& fieldNumbers);
    ~TableExprNodeRecordFieldArray();

    virtual bool isDefined (const TableExprId& id);
    virtual const IPosition& getShape (const TableExprId& id);

    virtual MArray<bool>     getArrayBool     (const TableExprId& id);
    virtual MArray<int64_t>    getArrayInt      (const TableExprId& id);
    virtual MArray<double>   getArrayDouble   (const TableExprId& id);
    virtual MArray<DComplex> getArrayDComplex (const TableExprId& id);
    virtual MArray<String>   getArrayString   (const TableExprId& id);

protected:
    Block<int32_t> fieldNrs_p;
    uint32_t       lastEntry_p;

    // Get the record for the last field number, thus going through
    // all subrecords for the other field numbers.
    const RecordInterface& getRecord (const TableExprId& id) const;
};




} //# NAMESPACE CASACORE - END

#endif
