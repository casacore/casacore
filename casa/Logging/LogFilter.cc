//# LogFilter.cc: Filter LogMessages on message priority
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

#include <casacore/casa/Logging/LogFilter.h>


namespace casa { //# NAMESPACE CASA - BEGIN

LogFilter::LogFilter (LogMessage::Priority lowest)
: lowest_p(lowest)
{}

LogFilter::LogFilter (const LogFilter& other)
: LogFilterInterface(),
  lowest_p(other.lowest_p)
{}

LogFilter& LogFilter::operator= (const LogFilter& other)
{
  if (this != &other) {
    lowest_p = other.lowest_p;
  }
  return *this;
}

LogFilter::~LogFilter()
{}

LogFilter* LogFilter::clone() const
{
  return new LogFilter(*this);
}

Bool LogFilter::pass (const LogMessage& message) const
{
  return message.priority() >= lowest_p;
}



} //# NAMESPACE CASA - END
