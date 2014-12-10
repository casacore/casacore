//# LogSinkInterface.cc: Accepts LogMessages and posts them to some destination
//# Copyright (C) 1996,2000,2001,2003
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

#include <casacore/casa/Logging/LogSinkInterface.h>
#include <casacore/casa/Logging/LogFilter.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

String LogSinkInterface::localId( )
{
    return String( "LogSinkInterface" );
}

LogSinkInterface::LogSinkInterface()
: filter_p (new LogFilter())
{
    // Nothing
}

LogSinkInterface::LogSinkInterface(const LogFilterInterface &filter)
: filter_p(filter.clone())
{
  // Nothing
}

LogSinkInterface::LogSinkInterface(const LogSinkInterface &other)
: filter_p(other.filter_p->clone())
{
  // Nothing
}

LogSinkInterface &LogSinkInterface::operator=(const LogSinkInterface &other)
{
  if (this != &other) {
    delete filter_p;
    filter_p = other.filter_p->clone();
  }
  return *this;
}

LogSinkInterface::~LogSinkInterface()
{
  flush();
  delete filter_p;
}

uInt LogSinkInterface::nelements() const
{
  return 0;
}

Double LogSinkInterface::getTime (uInt) const
{
  throw AipsError ("LogSinkInterface::getTime - no such message");
  return 0;
}
String LogSinkInterface::getPriority (uInt) const
{
  throw AipsError ("LogSinkInterface::getPriority - no such message");
  return "";
}
String LogSinkInterface::getMessage (uInt) const
{
  throw AipsError ("LogSinkInterface::getMessage - no such message");
  return "";
}
String LogSinkInterface::getLocation (uInt) const
{
  throw AipsError ("LogSinkInterface::getLocation - no such message");
  return "";
}
String LogSinkInterface::getObjectID (uInt) const
{
  throw AipsError ("LogSinkInterface::getObjectID - no such message");
  return "";
}

const LogFilterInterface &LogSinkInterface::filter() const
{
    return *filter_p;
}

LogSinkInterface &LogSinkInterface::filter(const LogFilterInterface &filter)
{
    delete filter_p;
    filter_p = filter.clone();;
    return *this;
}

void LogSinkInterface::flush(Bool)
{
    // Defult implementation is to do nothing.
}

void LogSinkInterface::cerrToo(Bool)
{
    // Defult implementation is to do nothing.
}

void LogSinkInterface::writeLocally (Double,
				     const String&,
				     const String&,
				     const String&,
				     const String&)
{}

void LogSinkInterface::clearLocally ()
{}

} //# NAMESPACE CASACORE - END

