//# MemoryLogSink.h: save log messages in memory
//# Copyright (C) 2001,2003
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

#include <casacore/casa/Logging/MemoryLogSink.h>
#include <casacore/casa/Logging/LogFilter.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

String MemoryLogSink::localId( ) {
    return String("MemoryLogSink");
}

String MemoryLogSink::id( ) const {
    return String("MemoryLogSink");
}

MemoryLogSink::MemoryLogSink()
: LogSinkInterface(),
  nmsg_p (0)
{}

MemoryLogSink::MemoryLogSink (LogMessage::Priority filter)
: LogSinkInterface(LogFilter(filter)),
  nmsg_p (0)
{}

MemoryLogSink::MemoryLogSink (const LogFilterInterface& filter)
: LogSinkInterface(filter),
  nmsg_p (0)
{}

MemoryLogSink::MemoryLogSink (const MemoryLogSink& other)
: LogSinkInterface()
{
  copy_other (other);
}

MemoryLogSink& MemoryLogSink::operator= (const MemoryLogSink& other)
{
  if (this != &other) {
    copy_other (other);
  }
  return *this;
}

void MemoryLogSink::copy_other (const MemoryLogSink& other)
{
  LogSinkInterface::operator= (other);
  nmsg_p = other.nmsg_p;
  time_p = other.time_p;
  priority_p = other.priority_p;
  message_p = other.message_p;
  location_p = other.location_p;
  objectID_p = other.objectID_p;
}

MemoryLogSink::~MemoryLogSink()
{}

uInt MemoryLogSink::nelements() const
{
  return nmsg_p;
}

Double MemoryLogSink::getTime (uInt i) const
{
  AlwaysAssert (i < nmsg_p, AipsError);
  return time_p[i];
}
String MemoryLogSink::getPriority (uInt i) const
{
  AlwaysAssert (i < nmsg_p, AipsError);
  return priority_p[i];
}
String MemoryLogSink::getMessage (uInt i) const
{
  AlwaysAssert (i < nmsg_p, AipsError);
  return message_p[i];
}
String MemoryLogSink::getLocation (uInt i) const
{
  AlwaysAssert (i < nmsg_p, AipsError);
  return location_p[i];
}
String MemoryLogSink::getObjectID (uInt i) const
{
  AlwaysAssert (i < nmsg_p, AipsError);
  return objectID_p[i];
}

Bool MemoryLogSink::postLocally (const LogMessage& message)
{
  Bool posted = False;
  if (filter().pass(message)) {
    posted = True;
    if (nmsg_p >= time_p.nelements()) {
      resize (nmsg_p+1);
    }
    time_p[nmsg_p] = message.messageTime().modifiedJulianDay()*24.0*3600.0;
    priority_p[nmsg_p] = LogMessage::toString(message.priority());
    message_p[nmsg_p]  = message.message();
    location_p[nmsg_p] = message.origin().location();
    String tmp;
    message.origin().objectID().toString(tmp);
    objectID_p[nmsg_p] = tmp;
    nmsg_p++;
  }
  return posted;
}

void MemoryLogSink::writeLocally (Double time,
				  const String& message,
				  const String& priority,
				  const String& location,
				  const String& objectID)
{
  if (nmsg_p >= time_p.nelements()) {
    resize (nmsg_p+1);
  }
  time_p[nmsg_p]     = time;
  message_p[nmsg_p]  = message;
  priority_p[nmsg_p] = priority;
  location_p[nmsg_p] = location;
  objectID_p[nmsg_p] = objectID;
  nmsg_p++;
}

void MemoryLogSink::clearLocally()
{
  // Resize the block to 0 elements.
  time_p.resize     (0, True, True);
  priority_p.resize (0, True, True);
  message_p.resize  (0, True, True);
  location_p.resize (0, True, True);
  objectID_p.resize (0, True, True);
  nmsg_p = 0;
}

void MemoryLogSink::resize (uInt nrnew)
{
  // Increase with at least 64 elements.
  if (nrnew < time_p.nelements()+64) {
    nrnew = time_p.nelements()+64;
  }
  time_p.resize     (nrnew);
  priority_p.resize (nrnew);
  message_p.resize  (nrnew);
  location_p.resize (nrnew);
  objectID_p.resize (nrnew);
}

} //# NAMESPACE CASACORE - END

