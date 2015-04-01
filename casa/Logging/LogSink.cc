//# LogSink.cc: Distribute LogMessages to their destination(s)
//# Copyright (C) 1996,1999,2001,2003
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

#include <casacore/casa/Logging/LogSink.h>
#include <casacore/casa/Logging/LogMessage.h>
#include <casacore/casa/Logging/LogFilter.h>

#include <casacore/casa/Logging/NullLogSink.h>
#include <casacore/casa/Logging/MemoryLogSink.h>
#include <casacore/casa/Logging/StreamLogSink.h>

#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

CountedPtr<LogSink::LsiIntermediate> *LogSink::global_sink_p = 0;
Mutex LogSink::theirMutex;


String LogSink::localId( ) {
    return String("LogSink");
}

String LogSink::id( ) const {
    return String("LogSink");
}

LogSink::LogSink(LogMessage::Priority filter, Bool nullSink)
  : LogSinkInterface(LogFilter(filter)),
    useGlobalSink_p (True)
{
    if ( ! LogSink::global_sink_p ) {
        createGlobalSink();
    }
    local_ref_to_global_p = *LogSink::global_sink_p;

    if (nullSink) {
        local_sink_p = new NullLogSink(LogFilter(LogMessage::DEBUGGING));
    } else {
        local_sink_p = new MemoryLogSink(LogFilter(LogMessage::DEBUGGING));
    }
    AlwaysAssert(! local_sink_p.null(), AipsError);
}

LogSink::LogSink(const LogFilterInterface &filter, Bool nullSink)
  : LogSinkInterface(filter),
    useGlobalSink_p (True)
{
    if ( ! LogSink::global_sink_p ) {
        createGlobalSink();
    }
    local_ref_to_global_p = *LogSink::global_sink_p;

    if (nullSink) {
        local_sink_p = new NullLogSink(LogFilter(LogMessage::DEBUGGING));
    } else {
        local_sink_p = new MemoryLogSink(LogFilter(LogMessage::DEBUGGING));
    }
    AlwaysAssert(! local_sink_p.null(), AipsError);
}

LogSink::LogSink(LogMessage::Priority filter, ostream *os,
                 Bool useGlobalSink)
  : LogSinkInterface(LogFilter(filter)), 
    local_sink_p(new StreamLogSink(LogFilter(LogMessage::DEBUGGING), os)),
    useGlobalSink_p (useGlobalSink)
{
    if ( ! LogSink::global_sink_p ) {
        createGlobalSink();
    }
    local_ref_to_global_p = *LogSink::global_sink_p;

    AlwaysAssert(! local_sink_p.null(), AipsError);
}

LogSink::LogSink(const LogFilterInterface &filter, ostream *os,
                 Bool useGlobalSink)
  : LogSinkInterface(filter), 
    local_sink_p(new StreamLogSink(LogFilter(LogMessage::DEBUGGING), os)),
    useGlobalSink_p (useGlobalSink)
{
    if ( ! LogSink::global_sink_p ) {
        createGlobalSink();
    }
    local_ref_to_global_p = *LogSink::global_sink_p;

    AlwaysAssert(! local_sink_p.null(), AipsError);
}

LogSink::LogSink (const LogFilterInterface &filter,
		  const CountedPtr<LogSinkInterface>& sink)
  : LogSinkInterface(filter),
    local_sink_p(sink),
    useGlobalSink_p (True)
{
    if ( ! LogSink::global_sink_p ) {
        createGlobalSink();
    }
    local_ref_to_global_p = *LogSink::global_sink_p;
}

LogSink::LogSink(const LogSink &other) 
  : LogSinkInterface(other), local_sink_p(other.local_sink_p),
    useGlobalSink_p (other.useGlobalSink_p)
{
    if ( ! LogSink::global_sink_p ) {
        createGlobalSink();
    }
    local_ref_to_global_p = *LogSink::global_sink_p;
}

LogSink &LogSink::operator=(const LogSink &other)
{
    if (this != &other) {
	local_ref_to_global_p = other.local_ref_to_global_p;
        local_sink_p = other.local_sink_p;
        useGlobalSink_p = other.useGlobalSink_p;
	LogSinkInterface &This = *this;
	This = other;
    }
    return *this;
}

LogSink::~LogSink()
{
    flush();
}

Bool LogSink::post(const LogMessage &message) 
{
    Bool postedLocally  =  postLocally(message);
    Bool postedGlobally =  False;
    if (useGlobalSink_p) {
      postedGlobally = postGlobally(message);
    }
    return (postedLocally || postedGlobally);
}

Bool LogSink::postGlobally(const LogMessage &message)
{
    Bool posted = False;
    AlwaysAssert(!(*global_sink_p).null(), AipsError);
    if ((**global_sink_p)->filter().pass(message)) {
        posted = globalSink().postLocally(message);
    }
    return posted;
}

void LogSink::preparePostThenThrow(const LogMessage &message,
                                   const AipsError& x) 
{
    // Try not to copy message since a severe error might be caused by
    // out of memory
    if (message.priority() == LogMessage::SEVERE) {
        post(message);
	flush();
        x.setMessage (message.toString());
    } else {
        LogMessage messageCopy(message);
	messageCopy.priority(LogMessage::SEVERE);
	post(messageCopy);
        x.setMessage (messageCopy.toString());
    }
}

void LogSink::postGloballyThenThrow(const LogMessage &message)
{
    // Try not to copy message since a severe error might be caused by
    // out of memory
    if (message.priority() == LogMessage::SEVERE) {
        postGlobally(message);
	globalSink().flush();
	throw(AipsError(message.toString()));
    } else {
        LogMessage messageCopy(message);
	messageCopy.priority(LogMessage::SEVERE);
	postGlobally(messageCopy);
	globalSink().flush();
	throw(AipsError(messageCopy.toString()));
    }
}

uInt LogSink::nelements() const
{
  return local_sink_p->nelements();
}

Double LogSink::getTime (uInt i) const
{
  return local_sink_p->getTime(i);
}
String LogSink::getPriority (uInt i) const
{
  return local_sink_p->getPriority(i);
}
String LogSink::getMessage (uInt i) const
{
  return local_sink_p->getMessage(i);
}
String LogSink::getLocation (uInt i) const
{
  return local_sink_p->getLocation(i);
}
String LogSink::getObjectID (uInt i) const
{
  return local_sink_p->getObjectID(i);
}

const LogFilterInterface &LogSink::filter() const
{
    return this->LogSinkInterface::filter();
}

LogSinkInterface &LogSink::filter(const LogFilterInterface &thefilter)
{
    return this->LogSinkInterface::filter(thefilter);
}

const LogSinkInterface &LogSink::localSink() const
{
    return *(local_sink_p);
}

LogSinkInterface &LogSink::localSink()
{
    return *(local_sink_p);
}

LogSink &LogSink::localSink(LogSinkInterface *&fromNew)
{
    local_sink_p = fromNew;
    fromNew = 0;
    AlwaysAssert(!local_sink_p.null(), AipsError);
    return *this;
}

Bool LogSink::nullGlobalSink( )
{
    return ! global_sink_p ? True : (*global_sink_p).null( ) ? True : False;
}

LogSinkInterface &LogSink::globalSink()
{
    if ( ! LogSink::global_sink_p ) {
        createGlobalSink();
    }
    return ***global_sink_p;
}

void LogSink::globalSink(LogSinkInterface *&fromNew)
{
    if ( ! LogSink::global_sink_p ) {
        createGlobalSink();
    }
    (* global_sink_p)->replace(fromNew);
    fromNew = 0;
    AlwaysAssert(!(*global_sink_p).null(), AipsError);
}

Bool LogSink::postLocally(const LogMessage &message) 
{
    if (filter().pass(message)) {
        return local_sink_p->postLocally(message);
    } else {
        return False;
    }
}

void LogSink::writeLocally (Double time, const String& message,
			    const String& priority, const String& location,
			    const String& objectID)
{
    local_sink_p->writeLocally (time, message, priority, location, objectID);
}

void LogSink::clearLocally()
{
    local_sink_p->clearLocally();
}

void LogSink::flush (Bool global)
{
    if (!local_sink_p.null()) {
        local_sink_p->flush(False);
    }
    if (global  &&  !(*global_sink_p).null()) {
        (**global_sink_p)->flush(False);
    }
}

void LogSink::createGlobalSink()
{
    ScopedMutexLock lock(theirMutex);
    if ( ! LogSink::global_sink_p ) {
        LogSink::global_sink_p = new CountedPtr<LsiIntermediate> ();
        (* global_sink_p) = new LsiIntermediate (new StreamLogSink(LogMessage::NORMAL, &cerr));
    }
}

} //# NAMESPACE CASACORE - END

