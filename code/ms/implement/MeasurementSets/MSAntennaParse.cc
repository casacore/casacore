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
#include <casa/Logging/LogIO.h>

namespace casa { //# NAMESPACE CASA - BEGIN

TableExprNode* MSAntennaParse::node_p = 0x0;

//# Constructor
MSAntennaParse::MSAntennaParse ()
: MSParse(),
  colName1(MS::columnName(MS::ANTENNA1)),
  colName2(MS::columnName(MS::ANTENNA2))
{
}

//# Constructor with given ms name.
MSAntennaParse::MSAntennaParse (const MeasurementSet* ms)
: MSParse(ms, "Antenna"),
  colName1(MS::columnName(MS::ANTENNA1)),
  colName2(MS::columnName(MS::ANTENNA2))
{
    if(node_p) delete node_p;
    node_p = new TableExprNode();
}

const TableExprNode* MSAntennaParse::selectAntennaIds(const Vector<Int>& antennaIds)
{
  //  LogIO os(LogOrigin("MSSpwParse", "selectAntennaIds()", WHERE));
  TableExprNode condition = ms()->col(colName1).in(antennaIds) ||
    ms()->col(colName2).in(antennaIds);
  
  if(node_p->isNull())
    *node_p = condition;
  else
    *node_p = *node_p || condition;
  
  return node();
}

const TableExprNode* MSAntennaParse::selectNameOrStation(const Vector<String>& antennaNames)
{
  MSAntennaIndex msAI(ms()->antenna());
  TableExprNode condition = ms()->col(colName1).in(msAI.matchAntennaName(antennaNames)) || ms()->col(colName2).in(msAI.matchAntennaName(antennaNames));
  if(node_p->isNull())
    *node_p = condition;
  else {
    *node_p = *node_p || condition;
  }
  return node();
}

const TableExprNode* MSAntennaParse::selectNameOrStation(const String& identifier)
{
  Vector<Int> antennaIdsFromStation ;
  //  Bool searchStation = True;
  TableExprNode condition;
  MSAntennaIndex msAI(ms()->antenna());
  antennaIdsFromStation = msAI.matchAntennaStation(identifier);
  //select from stations
  if(antennaIdsFromStation.nelements() > 0) {
    condition = ms()->col(colName1).in(antennaIdsFromStation) ||
      ms()->col(colName2).in(antennaIdsFromStation);
    //    searchStation = False;
  } else {
    //select from names
    condition = ms()->col(colName1).in(msAI.matchAntennaName(identifier)) ||
      ms()->col(colName2).in(msAI.matchAntennaName(identifier));
  }
  
  if(node_p->isNull())
    *node_p = condition;
  else {
    *node_p = *node_p || condition;
  }
  
  return node();
}

const TableExprNode* MSAntennaParse::selectFromIdsAndCPs(const Int index, const String& cp)
{
  LogIO os(LogOrigin("MSSpwParse", "selectFromIdsAndCPs()", WHERE));
  os << " selectFromIdsAndCPs is not available "  <<LogIO::POST;
  exit(0);
  
  TableExprNode condition;
  
  if(node_p->isNull())
    *node_p = condition;
  else
    *node_p = *node_p || condition;
  
  return node();
}

const TableExprNode* MSAntennaParse::selectFromIdsAndCPs(const Int firstIndex, const String& firstcp, const Int secondIndex, const String& secondcp)
{
  LogIO os(LogOrigin("MSSpwParse", "selectFromIdsAndCPs()", WHERE));
  os << " selectFromIdsAndCPs is not available "  <<LogIO::POST;
  TableExprNode condition;
 
  if(node_p->isNull())
    *node_p = condition;
  else
    *node_p = *node_p || condition;
  
  return node();
}

const TableExprNode* MSAntennaParse::node()
{
    return node_p;
}

} //# NAMESPACE CASA - END
