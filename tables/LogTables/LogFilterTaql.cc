//# LogFilterTaql.cc: Filter LogMessages using a TaQL expression
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
//# $Id$

#include <casacore/tables/LogTables/LogFilterTaql.h>
#include <casacore/tables/LogTables/LogFilterExpr.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LogFilterTaql::LogFilterTaql (const String& expr)
: expr_p  (0)
{
  expr_p = new LogFilterExpr(expr);
}

LogFilterTaql::LogFilterTaql (const LogFilterTaql& other)
: LogFilterInterface(),
  expr_p  (0)
{
  if (other.expr_p != 0) {
    expr_p = new LogFilterExpr (*other.expr_p);
  }
}

LogFilterTaql& LogFilterTaql::operator= (const LogFilterTaql& other)
{
  if (this != &other) {
    delete expr_p;
    expr_p = 0;
    if (other.expr_p != 0) {
      expr_p = new LogFilterExpr (*other.expr_p);
    }
  }
  return *this;
}

LogFilterTaql::~LogFilterTaql()
{
  delete expr_p;
}

LogFilterTaql* LogFilterTaql::clone() const
{
  return new LogFilterTaql(*this);
}

Bool LogFilterTaql::pass (const LogMessage& message) const
{
  return expr_p->matches (message);
}

} //# NAMESPACE CASACORE - END

