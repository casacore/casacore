//# LogFilterTaql.h: Filter LogMessages using a TaQL expression
//# Copyright (C) 1996,2000,2003
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
//#
//# $Id$

#ifndef TABLES_LOGFILTERTAQL_H
#define TABLES_LOGFILTERTAQL_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Logging/LogFilterInterface.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class LogFilterExpr;


// <summary>
// Filter LogMessages using a TaQL expression.
// </summary>

// <use visibility=export>

// <reviewed reviewer="wbrouw" date="1996/08/21" tests="" demos="dLogging.cc" tests="tLogging.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="LogMessage">LogMessage</linkto>
// </prerequisite>
//
// <etymology>
// Log[Message] Filter.
// </etymology>
//
// <synopsis>
// The <src>LogFilter</src> class is used by the various log sink classes,
// typically accessed through <linkto class="LogSink">LogSink</linkto>, to
// decide whether a particular <linkto class="LogMessage">LogMessage</linkto>
// should be accepted or rejected.
//
// Simple filtering is based on the messages priority.
// In particular, you typically will choose to only pass messages greater
// than or equal to  <src>NORMAL</src> in priority, but you might choose
// <src>DEBUGGING</src> to see all messages, or <src>SEVERE</src> to only see
// messages that report serious problems.
// </synopsis>
//
// <example>
// Suppose we wanted to change the global sink so that it prints all messages,
// including debugging messages:
// <srcblock>
// LogFilter all(LogMessage::DEBUGGING);
// LogSink::globalSink().filter(all);
// </srcblock>
// </example>
//
// <motivation>
// </motivation>
//
//# <todo asof="1996/07/23">
//# </todo>

class LogFilterTaql : public LogFilterInterface
{
public:
  // Construct a filter from a TaQL expression. Only messages matching
  // the expression pass the filter.
  // The field names that can be used in the expression are:
  // TIME, PRIORITY, MESSAGE, LOCATION, OBJECT_ID.
  // All fields are strings, expect for TIME which is a double (MJD in sec.).
  explicit LogFilterTaql (const String& expr);

  // Copy <src>other</src> to <src>this</src>.
  // <group>
  LogFilterTaql (const LogFilterTaql& other);
  LogFilterTaql& operator= (const LogFilterTaql& other);
  // </group>

  virtual ~LogFilterTaql();

  // Clone the object.
  virtual LogFilterTaql* clone() const;

  // Return True if <src>message</src> passes this filter.
  virtual Bool pass (const LogMessage& message) const;

private:
  LogFilterExpr* expr_p;
};



} //# NAMESPACE CASACORE - END

#endif
