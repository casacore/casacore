//# ExprMathNode.cc: Nodes representing scalar mathematical operators in table select expression tree
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000
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

#include <aips/Tables/ExprMathNode.h>
#include <aips/Quanta/MVTime.h>


// Implement the arithmetic operators for each data type.

TableExprNodePlusDouble::TableExprNodePlusDouble (const TableExprNodeRep& node)
: TableExprNodeBinary (NTDouble, node, OtPlus)
{}
TableExprNodePlusDouble::~TableExprNodePlusDouble()
{}
Double TableExprNodePlusDouble::getDouble (const TableExprId& id)
    { return lnode_p->getDouble(id) + rnode_p->getDouble(id); }
DComplex TableExprNodePlusDouble::getDComplex (const TableExprId& id)
    { return lnode_p->getDouble(id) + rnode_p->getDouble(id); }

TableExprNodePlusDComplex::TableExprNodePlusDComplex (const TableExprNodeRep& node)
: TableExprNodeBinary (NTComplex, node, OtPlus)
{}
TableExprNodePlusDComplex::~TableExprNodePlusDComplex()
{}
DComplex TableExprNodePlusDComplex::getDComplex (const TableExprId& id)
    { return lnode_p->getDComplex(id) + rnode_p->getDComplex(id); }

TableExprNodePlusString::TableExprNodePlusString (const TableExprNodeRep& node)
: TableExprNodeBinary (NTString, node, OtPlus)
{}
TableExprNodePlusString::~TableExprNodePlusString()
{}
String TableExprNodePlusString::getString (const TableExprId& id)
    { return lnode_p->getString(id) + rnode_p->getString(id); }

TableExprNodePlusDate::TableExprNodePlusDate (const TableExprNodeRep& node)
: TableExprNodeBinary (NTDate, node, OtPlus)
{}
TableExprNodePlusDate::~TableExprNodePlusDate()
{}
MVTime TableExprNodePlusDate::getDate(const TableExprId& id)
{ return lnode_p->getDouble(id) + rnode_p->getDouble(id); }
Double TableExprNodePlusDate::getDouble(const TableExprId& id)
{ return lnode_p->getDouble(id) + rnode_p->getDouble(id); }


TableExprNodeMinusDouble::TableExprNodeMinusDouble (const TableExprNodeRep& node)
: TableExprNodeBinary (NTDouble, node, OtMinus)
{}
TableExprNodeMinusDouble::~TableExprNodeMinusDouble()
{}
Double TableExprNodeMinusDouble::getDouble (const TableExprId& id)
    { return lnode_p->getDouble(id) - rnode_p->getDouble(id); }
DComplex TableExprNodeMinusDouble::getDComplex (const TableExprId& id)
    { return lnode_p->getDouble(id) - rnode_p->getDouble(id); }

TableExprNodeMinusDComplex::TableExprNodeMinusDComplex (const TableExprNodeRep& node)
: TableExprNodeBinary (NTComplex, node, OtMinus)
{}
TableExprNodeMinusDComplex::~TableExprNodeMinusDComplex()
{}
DComplex TableExprNodeMinusDComplex::getDComplex (const TableExprId& id)
    { return lnode_p->getDComplex(id) - rnode_p->getDComplex(id); }

TableExprNodeMinusDate::TableExprNodeMinusDate (const TableExprNodeRep& node)
: TableExprNodeBinary (NTDate, node, OtMinus)
{}
TableExprNodeMinusDate::~TableExprNodeMinusDate()
{}
MVTime TableExprNodeMinusDate::getDate(const TableExprId& id)
    { return lnode_p->getDouble(id) - rnode_p->getDouble(id); }
Double TableExprNodeMinusDate::getDouble(const TableExprId& id)
    { return lnode_p->getDouble(id) - rnode_p->getDouble(id); }


TableExprNodeTimesDouble::TableExprNodeTimesDouble (const TableExprNodeRep& node)
: TableExprNodeBinary (NTDouble, node, OtTimes)
{}
TableExprNodeTimesDouble::~TableExprNodeTimesDouble()
{}
Double TableExprNodeTimesDouble::getDouble (const TableExprId& id)
    { return lnode_p->getDouble(id) * rnode_p->getDouble(id); }
DComplex TableExprNodeTimesDouble::getDComplex (const TableExprId& id)
    { return lnode_p->getDouble(id) * rnode_p->getDouble(id); }

TableExprNodeTimesDComplex::TableExprNodeTimesDComplex (const TableExprNodeRep& node)
: TableExprNodeBinary (NTComplex, node, OtTimes)
{}
TableExprNodeTimesDComplex::~TableExprNodeTimesDComplex()
{}
DComplex TableExprNodeTimesDComplex::getDComplex (const TableExprId& id)
    { return lnode_p->getDComplex(id) * rnode_p->getDComplex(id); }


TableExprNodeDivideDouble::TableExprNodeDivideDouble (const TableExprNodeRep& node)
: TableExprNodeBinary (NTDouble, node, OtDivide)
{}
TableExprNodeDivideDouble::~TableExprNodeDivideDouble()
{}
Double TableExprNodeDivideDouble::getDouble (const TableExprId& id)
    { return lnode_p->getDouble(id) / rnode_p->getDouble(id); }
DComplex TableExprNodeDivideDouble::getDComplex (const TableExprId& id)
    { return lnode_p->getDouble(id) / rnode_p->getDouble(id); }

TableExprNodeDivideDComplex::TableExprNodeDivideDComplex (const TableExprNodeRep& node)
: TableExprNodeBinary (NTComplex, node, OtDivide)
{}
TableExprNodeDivideDComplex::~TableExprNodeDivideDComplex()
{}
DComplex TableExprNodeDivideDComplex::getDComplex (const TableExprId& id)
    { return lnode_p->getDComplex(id) / rnode_p->getDComplex(id); }


TableExprNodeModuloDouble::TableExprNodeModuloDouble (const TableExprNodeRep& node)
: TableExprNodeBinary (NTDouble, node, OtModulo)
{}
TableExprNodeModuloDouble::~TableExprNodeModuloDouble()
{}
Double TableExprNodeModuloDouble::getDouble (const TableExprId& id)
    { return fmod (lnode_p->getDouble(id), rnode_p->getDouble(id)); }
DComplex TableExprNodeModuloDouble::getDComplex (const TableExprId& id)
    { return fmod (lnode_p->getDouble(id), rnode_p->getDouble(id)); }


TableExprNodeMIN::TableExprNodeMIN (const TableExprNodeRep& node)
: TableExprNodeBinary (node.dataType(), node, OtMIN)
{}
TableExprNodeMIN::~TableExprNodeMIN()
{}
Double TableExprNodeMIN::getDouble (const TableExprId& id)
    { return -(lnode_p->getDouble(id)); }
DComplex TableExprNodeMIN::getDComplex (const TableExprId& id)
    { return -(lnode_p->getDComplex(id)); }
