//# MSSpwParse.cc: Classes to hold results from spw grammar parser
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

#include <ms/MeasurementSets/MSSpwParse.h>
#include <ms/MeasurementSets/MSDataDescIndex.h>
#include <ms/MeasurementSets/MSSpWindowIndex.h>

namespace casa { //# NAMESPACE CASA - BEGIN

TableExprNode MSSpwParse::node_p;

//# Constructor
MSSpwParse::MSSpwParse ()
: MSParse()
{
}

//# Constructor with given ms name.
MSSpwParse::MSSpwParse (const MeasurementSet& ms)
: MSParse(ms, "SPW")
{
    node_p = TableExprNode();
}

TableExprNode *MSSpwParse::selectSpwIds(const Vector<Int>& spwIds)
{
    // Look-up in DATA_DESC sub-table
    MSDataDescIndex msDDI(ms().dataDescription());
    String colName = MS::columnName(MS::DATA_DESC_ID);

   TableExprNode condition =
       (ms().col(colName).in(msDDI.matchSpwId(spwIds)));

    if(node().isNull())
        node() = condition;
    else
        node() = node() && condition;

    return &node();
}

TableExprNode *MSSpwParse::selectSpwName(const String& name)
{
    const String colName = MS::columnName(MS::DATA_DESC_ID);
    bool selectName;

    ROMSSpWindowColumns msSWC(ms().spectralWindow());
    ROScalarColumn<String> names(msSWC.name());

    for (uInt i = 0; i < names.getColumn().nelements(); i++)
    {
        if(strcmp(names(i).chars(), name.chars())==0)
            selectName = True;
    }

    TableExprNode condition;
    if(selectName)
    {
        MSSpWindowIndex msSWI(ms().spectralWindow());
        condition = 0;
    }
    else
        condition = 0;


    if(node().isNull())
        node() = condition;
    else
        node() = node() && condition;

    return &node();
}

TableExprNode& MSSpwParse::node()
{
    return node_p;
}

} //# NAMESPACE CASA - END
