//# ImageLogger.cc: Class to handle all image logging
//# Copyright (C) 2001
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


#include <trial/Images/ImageLogger.h>
#include <aips/Logging/MemoryLogSink.h>
#include <aips/Logging/TableLogSink.h>


ImageLogger::ImageLogger (Bool nullSink)
: itsSink       (LogFilter(), nullSink),
  itsTablePtr   (0),
  itsIsWritable (True),
  itsIsClosed   (False)
{
  itsLogger = LogIO(itsSink);
}

ImageLogger::ImageLogger (const String& logTableName, Bool isWritable)
: itsTableName  (logTableName),
  itsTablePtr   (0),
  itsIsWritable (isWritable),
  itsIsClosed   (True)
{
  // Open the log table.
  doReopen();
}

ImageLogger::ImageLogger (const ImageLogger& that)
: itsSink       (that.itsSink),
  itsLogger     (that.itsLogger),
  itsTableName  (that.itsTableName),
  itsTablePtr   (that.itsTablePtr),
  itsIsWritable (that.itsIsWritable),
  itsIsClosed   (that.itsIsClosed)
{}

ImageLogger::~ImageLogger()
{
  removeParents();
}

ImageLogger& ImageLogger::operator= (const ImageLogger& that)
{
  if (this != &that) {
    removeParents();
    itsSink       = that.itsSink;
    itsLogger     = that.itsLogger;
    itsTableName  = that.itsTableName;
    itsTablePtr   = that.itsTablePtr;
    itsIsWritable = that.itsIsWritable;
    itsIsClosed   = that.itsIsClosed;
  }
  return *this;
}

void ImageLogger::append (const ImageLogger& other)
{
  reopenRW();
  LogSinkInterface& logsink = sink().localSink();
  for (ImageLogger::const_iterator iter = other.begin();
       iter != other.end();
       iter++) {
    logsink.writeLocally (iter->time(), iter->message(), iter->priority(),
			  iter->location(), iter->objectID());
  }
}

void ImageLogger::reopenRW()
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

void ImageLogger::doReopen()
{
  if (itsIsClosed  &&  itsTablePtr == 0  &&  !itsTableName.empty()) {
    if (itsIsWritable) {
      itsTablePtr = new TableLogSink(LogFilter(), itsTableName, False);
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

void ImageLogger::addParent (const ImageLogger* logger)
{
  uInt nr = itsParents.nelements();
  itsParents.resize (nr + 1);
  itsParents[nr] = const_cast<ImageLogger*>(logger);
}

void ImageLogger::tempClose (Bool closeParents)
{
  if (itsTablePtr != 0) {
    itsSink     = LogSink();
    itsLogger   = LogIO();
    itsTablePtr = 0;
    itsIsClosed = True;
  }
  if (closeParents) {
    for (uInt i=0; i<itsParents.nelements(); i++) {
      itsParents[i]->tempClose (closeParents);
    }
  }
}

void ImageLogger::close()
{
  itsSink   = LogSink();
  itsLogger = LogIO();
  itsIsClosed = False;
  itsTablePtr = 0;
}

void ImageLogger::unlock()
{
  if (itsTablePtr != 0) {
    itsTablePtr->table().unlock();
  }
}

void ImageLogger::flush()
{
  if (itsTablePtr != 0) {
    itsTablePtr->table().flush();
  }
}

void ImageLogger::resync()
{
  if (itsTablePtr != 0
      &&  !itsTablePtr->table().hasLock (FileLocker::Read)) {
    itsTablePtr->table().unlock();
  }
}

void ImageLogger::removeParents()
{
  itsParents.resize (0, True, True);
}

void ImageLogger::clear()
{
  reopenRW();
  removeParents();
}


ImageLogIter::ImageLogIter (const ImageLogger* logger)
: itsLogger     (logger),
  itsTempClosed (logger->isTempClosed()),
  itsParentIter (0),
  itsCounter    (0)
{
  const PtrBlock<ImageLogger*> par = itsLogger->parents();
  if (par.nelements() > 0) {
    itsParentIter = new ImageLogIter (par[0]);
    itsCounter++;
  }
}

ImageLogIter::~ImageLogIter()
{
  delete itsParentIter;
  if (itsTempClosed) {
    const_cast<ImageLogger*>(itsLogger)->tempClose();
  }
}

Bool ImageLogIter::next()
{
  while (itsParentIter != 0  &&  !itsParentIter->next()) {
    delete itsParentIter;
    itsParentIter = 0;
    const PtrBlock<ImageLogger*> par = itsLogger->parents();
    if (par.nelements() > itsCounter) {
      itsParentIter = new ImageLogIter (par[itsCounter]);
      itsCounter++;
    } else {
      itsCounter = 0;
    }
  }
  if (itsParentIter != 0) {
    itsEntry = itsParentIter->getEntry();
  } else {
    const LogSink& sink = const_cast<ImageLogger*>(itsLogger)->sink();
    if (itsCounter < sink.nelements()) {
      itsEntry = ImageLogIterEntry (&sink, itsCounter);
      itsCounter++;
    } else {
      return False;
    }
  }
  return True;
}

ImageLoggerIterator::ImageLoggerIterator (const ImageLogger* logger)
: itsIter (0)
{
  itsIter = new ImageLogIter (logger);
  next();
}

ImageLoggerIterator::ImageLoggerIterator (const ImageLoggerIterator& that)
: itsIter (0)
{
  if (that.itsIter != 0) {
    itsIter = new ImageLogIter (&(that.logger()));
    next();
  }
}

ImageLoggerIterator& ImageLoggerIterator::operator=
                                     (const ImageLoggerIterator& that)
{
  if (this != &that) {
    delete itsIter;
    itsIter = 0;
    if (that.itsIter != 0) {
      itsIter = new ImageLogIter (&(that.logger()));
      next();
    }
  }
  return *this;
}
