//# TaQLNodeRep.cc: Representation of entities in the TaQL parse tree
//# Copyright (C) 2005
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

#include <casacore/tables/TaQL/TaQLNodeRep.h>
#include <casacore/tables/TaQL/TaQLNode.h>
#include <casacore/tables/Tables/TableError.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

TaQLNodeRep::TaQLNodeRep (int nodeType)
: itsCount    (0),
  itsNodeType (nodeType),
  itsStyle    (TaQLNode::theirStyle)
{}

TaQLNodeRep::~TaQLNodeRep()
{}

String TaQLNodeRep::checkDataType (const String& dtype)
{
  String dtstr(dtype);
  if (! dtstr.empty()) {
    dtstr.upcase();
    if (dtstr == "B"  ||  dtstr == "BOOL"  ||  dtstr == "BOOLEAN") {
      dtstr = "B";
    } else if (dtstr == "U1"  ||  dtstr == "UC"
           ||  dtstr == "UCHAR"  ||  dtstr == "BYTE") {
      dtstr = "U1";
    } else if (dtstr == "I2"  ||  dtstr == "SHORT"  ||  dtstr == "SMALLINT") {
      dtstr = "I2";
    } else if (dtstr == "U2"  ||  dtstr == "UI2"
	   ||  dtstr == "USHORT"  ||  dtstr == "USMALLINT") {
      dtstr = "U2";
    } else if (dtstr == "I4"  ||  dtstr == "INT"  ||  dtstr == "INTEGER") {
      dtstr = "I4";
    } else if (dtstr == "U4" ||  dtstr == "UI4"
	   ||  dtstr == "UINT"  ||  dtstr == "UINTEGER") {
      dtstr = "U4";
    } else if (dtstr == "FLT"  ||  dtstr == "R4"  ||  dtstr == "FLOAT") {
      dtstr = "R4";
    } else if (dtstr == "DBL"  ||  dtstr == "R8"  ||  dtstr == "DOUBLE") {
      dtstr = "R8";
    } else if (dtstr == "FC"  ||  dtstr == "C4"
           ||  dtstr == "FCOMPLEX"  ||  dtstr == "COMPLEX") {
      dtstr = "C4";
    } else if (dtstr == "DC"  ||  dtstr == "C8"  ||  dtstr == "DCOMPLEX") {
      dtstr = "C8";
    } else if (dtstr == "S"  ||  dtstr == "STRING") {
      dtstr = "S";
    } else {
      throw TableError ("Datatype '" + dtype + "' is invalid");
    }
  }
  return dtstr;
}

} //# NAMESPACE CASACORE - END
