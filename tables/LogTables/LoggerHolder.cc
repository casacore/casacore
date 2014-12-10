//# LoggerHolder.cc: Class to handle a hierarchy of loggers
//# Copyright (C) 2001,2002,2003
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


#include <casacore/tables/LogTables/LoggerHolder.h>
#include <casacore/casa/Logging/MemoryLogSink.h>
#include <casacore/tables/LogTables/TableLogSink.h>
#include <casacore/casa/Logging/LogFilter.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LoggerHolder::LoggerHolder (Bool nullSink)
: itsRep (new LoggerHolderRep (nullSink))
{}

LoggerHolder::LoggerHolder (const String& logTableName, Bool isWritable)
: itsRep (new LoggerHolderRep (logTableName, isWritable))
{}

LoggerHolder::LoggerHolder (const LoggerHolder& that)
: itsRep (that.itsRep)
{}

LoggerHolder::~LoggerHolder()
{
  // Close the possible log table to avoid waste in case the logger
  // is not really used anymore.
  itsRep->tempClose();
}

LoggerHolder& LoggerHolder::operator= (const LoggerHolder& that)
{
  if (this != &that) {
    itsRep = that.itsRep;
  }
  return *this;
}

void LoggerHolder::append (const LoggerHolder& other)
{
  itsRep->append (other);
}

void LoggerHolder::reopenRW()
{
  itsRep->reopenRW();
}

void LoggerHolder::addParent (const LoggerHolder& logger)
{
  itsRep->addParent (logger);
}

void LoggerHolder::tempClose (Bool closeParents) const
{
  itsRep->tempClose (closeParents);
}

void LoggerHolder::unlock()
{
  itsRep->unlock();
}

void LoggerHolder::flush()
{
  itsRep->flush();
}

void LoggerHolder::resync()
{
  itsRep->resync();
}

void LoggerHolder::removeParents()
{
  itsRep->removeParents();
}

void LoggerHolder::clear()
{
  itsRep->clear();
}



LoggerHolderRep::LoggerHolderRep (Bool nullSink)
: itsSink       (LogFilter(), nullSink),
  itsTablePtr   (0),
  itsIsWritable (True),
  itsIsClosed   (False)
{
  itsLogger = LogIO(itsSink);
}

LoggerHolderRep::LoggerHolderRep (const String& logTableName, Bool isWritable)
: itsTableName  (logTableName),
  itsTablePtr   (0),
  itsIsWritable (isWritable),
  itsIsClosed   (True)
{
  // Open the log table.
  doReopen();
}

LoggerHolderRep::LoggerHolderRep (const LoggerHolderRep& that)
: itsParents    (that.itsParents),
  itsSink       (that.itsSink),
  itsLogger     (that.itsLogger),
  itsTableName  (that.itsTableName),
  itsTablePtr   (that.itsTablePtr),
  itsIsWritable (that.itsIsWritable),
  itsIsClosed   (that.itsIsClosed)
{}

LoggerHolderRep::~LoggerHolderRep()
{
  removeParents();
}

LoggerHolderRep& LoggerHolderRep::operator= (const LoggerHolderRep& that)
{
  if (this != &that) {
    itsParents    = that.itsParents;
    itsSink       = that.itsSink;
    itsLogger     = that.itsLogger;
    itsTableName  = that.itsTableName;
    itsTablePtr   = that.itsTablePtr;
    itsIsWritable = that.itsIsWritable;
    itsIsClosed   = that.itsIsClosed;
  }
  return *this;
}

void LoggerHolderRep::append (const LoggerHolder& other)
{
  reopenRW();
  LogSinkInterface& logsink = sink().localSink();
  for (LoggerHolder::const_iterator iter = other.begin();
       iter != other.end();
       iter++) {
    logsink.writeLocally (iter->time(), iter->message(), iter->priority(),
			  iter->location(), iter->objectID());
  }
}

void LoggerHolderRep::reopenRW()
{
  // Only needed if a table is used and if not already open for rw.
  if (!itsTableName.empty()) {
    if (itsTablePtr == 0  ||  !itsIsWritable) {
    // Temporarily close table possibly opened for readonly.
      tempClose (False);
      // Reopen temporarily closed table for rw (if possible).
      if (!itsIsWritable) {
	itsIsWritable = Table::isWritable (itsTableName);
      }
      reopen();
    }
  }
}

void LoggerHolderRep::doReopen()
{
  if (itsIsClosed  &&  itsTablePtr == 0  &&  !itsTableName.empty()) {
    if (itsIsWritable) {
      itsTablePtr = new TableLogSink(LogFilter(), itsTableName);
    } else {
      itsTablePtr = new TableLogSink(itsTableName);
    }
    // Make it the local sink. Because that clears the pointer,
    // we pass it a copy.
    LogSinkInterface* ptr = itsTablePtr;
    itsSink.localSink (ptr);
    itsLogger   = LogIO(itsSink);
    itsIsClosed = False;
  }
}

void LoggerHolderRep::addParent (const LoggerHolder& logger)
{
  uInt nr = itsParents.nelements();
  itsParents.resize (nr + 1);
  itsParents[nr] = logger;
}

void LoggerHolderRep::tempClose (Bool closeParents)
{
  if (itsTablePtr != 0) {
    itsTablePtr->table().unlock();
    itsSink     = LogSink();
    itsLogger   = LogIO();
    itsTablePtr = 0;
    itsIsClosed = True;
  }
  if (closeParents) {
    for (uInt i=0; i<itsParents.nelements(); i++) {
      itsParents[i].tempClose (closeParents);
    }
  }
}

void LoggerHolderRep::unlock()
{
  if (itsTablePtr != 0) {
    itsTablePtr->table().unlock();
  }
}

void LoggerHolderRep::flush()
{
  if (itsTablePtr != 0) {
    itsTablePtr->table().flush();
  }
}

void LoggerHolderRep::resync()
{
  if (itsTablePtr != 0
      &&  !itsTablePtr->table().hasLock (FileLocker::Read)) {
    itsTablePtr->table().unlock();
  }
}

LogIO& LoggerHolderRep::logio()
{
  reopenRW();
  return itsLogger;
}

LogSink& LoggerHolderRep::sink()
{
  if (itsIsClosed) {
    reopen();
  }
  return itsSink;
}

void LoggerHolderRep::removeParents()
{
  itsParents.resize (0, True, True);
}

void LoggerHolderRep::clear()
{
  reopenRW();
  removeParents();
  itsSink.clearLocally();
}



LogHolderIter::LogHolderIter (const LoggerHolder* logger)
: itsLogger     (logger),
  itsTempClosed (logger->isTempClosed()),
  itsParentIter (0),
  itsCounter    (0)
{
  const Block<LoggerHolder>& par = itsLogger->parents();
  if (par.nelements() > 0) {
    itsParentIter = new LogHolderIter (&par[0]);
    itsCounter++;
  }
}

LogHolderIter::~LogHolderIter()
{
  delete itsParentIter;
  if (itsTempClosed) {
    itsLogger->tempClose();
  }
}

Bool LogHolderIter::next()
{
  while (itsParentIter != 0  &&  !itsParentIter->next()) {
    delete itsParentIter;
    itsParentIter = 0;
    const Block<LoggerHolder>& par = itsLogger->parents();
    if (par.nelements() > itsCounter) {
      itsParentIter = new LogHolderIter (&par[itsCounter]);
      itsCounter++;
    } else {
      itsCounter = 0;
    }
  }
  if (itsParentIter != 0) {
    itsEntry = itsParentIter->getEntry();
  } else {
    const LogSink& sink = itsLogger->sink();
    if (itsCounter < sink.nelements()) {
      itsEntry = LogHolderIterEntry (&sink, itsCounter);
      itsCounter++;
    } else {
      return False;
    }
  }
  return True;
}

LoggerHolderIterator::LoggerHolderIterator (const LoggerHolder* logger)
: itsIter (0)
{
  itsIter = new LogHolderIter (logger);
  next();
}

LoggerHolderIterator::LoggerHolderIterator (const LoggerHolderIterator& that)
: itsIter (0)
{
  if (that.itsIter != 0) {
    itsIter = new LogHolderIter (&(that.logger()));
    next();
  }
}

LoggerHolderIterator& LoggerHolderIterator::operator=
                                     (const LoggerHolderIterator& that)
{
  if (this != &that) {
    delete itsIter;
    itsIter = 0;
    if (that.itsIter != 0) {
      itsIter = new LogHolderIter (&(that.logger()));
      next();
    }
  }
  return *this;
}

} //# NAMESPACE CASACORE - END

