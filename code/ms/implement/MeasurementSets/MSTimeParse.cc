//# MSTimeParse.cc: Classes to hold results from time grammar parser
//# Copyright (C) 1994,1995,1997,1998,1999,2000,2001,2003
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

#include <ms/MeasurementSets/MSTimeParse.h>
#include <ms/MeasurementSets/MSMainColumns.h>
#include <casa/Quanta/MVTime.h>

namespace casa { //# NAMESPACE CASA - BEGIN

TableExprNode* MSTimeParse::node_p = 0x0;

//# Constructor
MSTimeParse::MSTimeParse ()
: MSParse(), colName(MS::columnName(MS::TIME))
{
}

//# Constructor with given ms name.
MSTimeParse::MSTimeParse (const MeasurementSet& ms)
: MSParse(ms, "Time"), colName(MS::columnName(MS::TIME))
{
    if(node_p) delete node_p;
    node_p = new TableExprNode();
}

const TableExprNode *MSTimeParse::selectStartTime(const MEpoch& startTime)
{
    MVTime mvStart(startTime.getValue());

    TableExprNode condition = (ms()->col(colName) >= mvStart);

    if(node_p->isNull())
        *node_p = condition;
    else
        *node_p = *node_p && condition;

    return node_p;
}

const TableExprNode *MSTimeParse::selectEndTime(const MEpoch& endTime)
{
    MVTime mvEnd(endTime.getValue());
 
    TableExprNode condition = (ms()->col(colName) <= mvEnd);

    if(node_p->isNull())
        *node_p = condition;
    else
        *node_p = *node_p && condition;

    return node_p;
}

const TableExprNode *MSTimeParse::selectRange(const MEpoch& startTime,
                                              const MEpoch& endTime)
{
    MVTime mvStart(startTime.getValue());
    MVTime mvEnd(endTime.getValue());

    TableExprNode condition = (ms()->col(colName) >= mvStart) &&
                              (ms()->col(colName) <= mvEnd);

    if(node_p->isNull())
        *node_p = condition;
    else
        *node_p = *node_p && condition;

    return node_p;
}

const TableExprNode* MSTimeParse::node()
{
    return node_p;
}

} //# NAMESPACE CASA - END
