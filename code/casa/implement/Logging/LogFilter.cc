//# LogFilter.cc: Filter LogMessages on priority
//# Copyright (C) 1996,2000
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

#include <aips/Logging/LogFilter.h>
#include <aips/Logging/LogFilterExpr.h>


LogFilter::LogFilter (LogMessage::Priority lowest)
: lowest_p(lowest),
  expr_p  (0)
{}

LogFilter::LogFilter (const String& expr)
: lowest_p(LogMessage::NORMAL),
  expr_p  (0)
{
  expr_p = new LogFilterExpr(expr);
}

LogFilter::LogFilter (const LogFilter& other)
: lowest_p(other.lowest_p),
  expr_p  (0)
{
  if (other.expr_p != 0) {
    expr_p = new LogFilterExpr (*other.expr_p);
  }
}

LogFilter& LogFilter::operator= (const LogFilter& other)
{
  if (this != &other) {
    lowest_p = other.lowest_p;
    delete expr_p;
    expr_p = 0;
    if (other.expr_p != 0) {
      expr_p = new LogFilterExpr (*other.expr_p);
    }
  }
  return *this;
}

LogFilter::~LogFilter()
{
  delete expr_p;
}

Bool LogFilter::passExpr (const LogMessage& message) const
{
  return expr_p->matches (message);
}
