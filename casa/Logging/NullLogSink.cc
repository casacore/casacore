//# NullLogSink.cc: Throw away all messages.
//# Copyright (C) 1996,2003
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

#include <casacore/casa/Logging/NullLogSink.h>
#include <casacore/casa/Logging/LogFilter.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

String NullLogSink::localId( ) {
    return String("NullLogSink");
}

String NullLogSink::id( ) const {
    return String("NullLogSink");
}

NullLogSink::NullLogSink()
{
    // Nothing
}

NullLogSink::NullLogSink(LogMessage::Priority filter)
: LogSinkInterface(LogFilter(filter))
{
    // Nothing
}

NullLogSink::NullLogSink(const LogFilterInterface &filter)
: LogSinkInterface(filter)
{
    // Nothing
}

NullLogSink::NullLogSink(const NullLogSink &other) : LogSinkInterface(other)
{
    // Nothing
}

NullLogSink &NullLogSink::operator=(const NullLogSink &other)
{
    if (this != &other) {
        // Copy the base class part
        LogSinkInterface &This = *this;
	This = other;
    }
    return *this;
}

NullLogSink::~NullLogSink()
{
    // Nothing
}

Bool NullLogSink::postLocally(const LogMessage &message) 
{
    return filter().pass(message);
}


} //# NAMESPACE CASACORE - END

