//# MSAntennaParse.cc: Classes to hold results from antenna grammar parser
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

#include <ms/MeasurementSets/MSAntennaParse.h>
#include <ms/MeasurementSets/MSAntennaIndex.h>

namespace casa { //# NAMESPACE CASA - BEGIN

TableExprNode MSAntennaParse::node_p;

//# Constructor
MSAntennaParse::MSAntennaParse ()
: MSParse(),
  colName1(MS::columnName(MS::ANTENNA1)),
  colName2(MS::columnName(MS::ANTENNA2))
{
}

//# Constructor with given ms name.
MSAntennaParse::MSAntennaParse (const MeasurementSet& ms)
: MSParse(ms, "Antenna"),
  colName1(MS::columnName(MS::ANTENNA1)),
  colName2(MS::columnName(MS::ANTENNA2))
{
    node_p = TableExprNode();
}

TableExprNode* MSAntennaParse::selectAntennaIds(const Vector<Int>& antennaIds)
{
    TableExprNode condition = ms()->col(colName1).in(antennaIds) ||
                              ms()->col(colName2).in(antennaIds);

    if(node().isNull())
        node() = condition;
    else
        node() = node() && condition;

    return &node();
}

TableExprNode* MSAntennaParse::selectAntennaName(const Vector<String>& antennaNames)
{
    MSAntennaIndex msAI(ms()->antenna());

    TableExprNode condition =
        ms()->col(colName1).in(msAI.matchAntennaName(antennaNames)) ||
        ms()->col(colName2).in(msAI.matchAntennaName(antennaNames));

    if(node().isNull())
        node() = condition;
    else
        node() = node() && condition;

    return &node();
}

TableExprNode& MSAntennaParse::node()
{
    return node_p;
}

} //# NAMESPACE CASA - END
