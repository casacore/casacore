//# LogSinkInterface.cc: Accepts LogMessages and posts them to some destination
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
//#
//# $Id$

#include <aips/Logging/LogSinkInterface.h>


LogSinkInterface::LogSinkInterface() : filter_p(LogMessage::NORMAL)
{
    // Nothing
}

LogSinkInterface::LogSinkInterface(const LogFilter &filter)
  : filter_p(filter)
{
    // Nothing
}

LogSinkInterface::LogSinkInterface(const LogSinkInterface &other)
  : filter_p(other.filter_p)
{
    // Nothing
}

LogSinkInterface &LogSinkInterface::operator=(const LogSinkInterface &other)
{
    if (this != &other) {
        filter_p = other.filter_p;
    }
    return *this;
}

LogSinkInterface::~LogSinkInterface()
{
    flush();
}

const LogFilter &LogSinkInterface::filter() const
{
    return filter_p;
}

LogSinkInterface &LogSinkInterface::filter(const LogFilter &filter)
{
    filter_p = filter;
    return *this;
}

void LogSinkInterface::flush()
{
    // Defult implementation is to do nothing.
}

Bool LogSinkInterface::isTableLogSink() const
{
    return False;
}
