//# LogFilterExpr.cc: Class to deal with a TaQL expression to filter messages
//# Copyright (C) 2000
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <casacore/tables/LogTables/LogFilterExpr.h>
#include <casacore/casa/Logging/LogMessage.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/RecordGram.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LogFilterExpr::LogFilterExpr (const String& expr)
: itsExpr (0)
{
  // Make a description for the parser.
  RecordDesc desc;
  desc.addField ("TIME", TpDouble);
  desc.addField ("PRIORITY", TpString);
  desc.addField ("MESSAGE", TpString);
  desc.addField ("LOCATION", TpString);
  desc.addField ("OBJECT_ID", TpString);
  itsExpr = new TableExprNode (RecordGram::parse (Record(desc), expr));
}

LogFilterExpr::LogFilterExpr (const LogFilterExpr& that)
: TableExprData(),
  itsExpr (0)
{
  if (that.itsExpr != 0) {
    itsExpr = new TableExprNode (*that.itsExpr);
  }
}

LogFilterExpr::~LogFilterExpr()
{
  delete itsExpr;
}

LogFilterExpr& LogFilterExpr::operator= (const LogFilterExpr& that)
{
  if (this != &that) {
    delete itsExpr;
    itsExpr = 0;
    if (that.itsExpr != 0) {
      itsExpr = new TableExprNode (*that.itsExpr);
    }
  }
  return *this;
}

Bool LogFilterExpr::matches (const LogMessage& message)
{
  // Evaluate the expression for this message.
  itsMessage = &message;
  Bool valb;
  // This class contains the functions to get the values.
  itsExpr->get (*this, valb);
  return valb;
}

Double LogFilterExpr::getDouble (const Block<Int>& fieldNrs) const
{
  switch (fieldNrs[0]) {
  case 0:
    return itsMessage->messageTime().modifiedJulianDay()*24.0*3600.0;
  default:
    throw (AipsError("LogFilterExpr::getDouble"));
  }
}

String LogFilterExpr::getString (const Block<Int>& fieldNrs) const
{
  switch (fieldNrs[0]) {
  case 1:
    return itsMessage->LogMessage::toString(itsMessage->priority());
  case 2:
    return itsMessage->message();
  case 3:
    return itsMessage->origin().location();
  case 4:
    {
      String tmp;
      itsMessage->origin().objectID().toString(tmp);
      return tmp;
    }
  default:
    throw (AipsError("LogFilterExpr::getString"));
  }
}

DataType LogFilterExpr::dataType (const Block<Int>& fieldNrs) const
{
  switch (fieldNrs[0]) {
  case 0:
    return TpDouble;
  case 1:
  case 2:
  case 3:
  case 4:
    return TpString;
  default:
    throw (AipsError("LogFilterExpr::dataType"));
  }
}

} //# NAMESPACE CASACORE - END

