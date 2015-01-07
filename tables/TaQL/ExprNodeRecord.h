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
//#
//# $Id$

#ifndef TABLES_EXPRNODERECORD_H
#define TABLES_EXPRNODERECORD_H

//# Includes
#include <casacore/casa/aips.h>
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
//# Data types Bool, Double, DComplex and String are used.
//# Char, uChar, Short, uShort, Int, uInt and float are converted 
//# to Double, and Complex to DComplex.
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
			      const Block<Int>& fieldNumbers);
    ~TableExprNodeRecordField();

    virtual const IPosition& getShape (const TableExprId& id);
    virtual Bool isDefined (const TableExprId& id);

    virtual Bool     getBool     (const TableExprId& id);
    virtual Int64    getInt      (const TableExprId& id);
    virtual Double   getDouble   (const TableExprId& id);
    virtual DComplex getDComplex (const TableExprId& id);
    virtual String   getString   (const TableExprId& id);

protected:
    Block<Int> fieldNrs_p;
    uInt       lastEntry_p;

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
				   const Block<Int>& fieldNumbers);
    ~TableExprNodeRecordFieldArray();

    virtual Bool isDefined (const TableExprId& id);
    virtual const IPosition& getShape (const TableExprId& id);

    virtual Array<Bool>     getArrayBool     (const TableExprId& id);
    virtual Array<Int64>    getArrayInt      (const TableExprId& id);
    virtual Array<Double>   getArrayDouble   (const TableExprId& id);
    virtual Array<DComplex> getArrayDComplex (const TableExprId& id);
    virtual Array<String>   getArrayString   (const TableExprId& id);

protected:
    Block<Int> fieldNrs_p;
    uInt       lastEntry_p;

    // Get the record for the last field number, thus going through
    // all subrecords for the other field numbers.
    const RecordInterface& getRecord (const TableExprId& id) const;
};




} //# NAMESPACE CASACORE - END

#endif
