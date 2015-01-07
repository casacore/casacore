//# LoggerHolder.h: Class holding a hierarchy of loggers
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
//#
//# $Id$

#ifndef TABLES_LOGGERHOLDER_H
#define TABLES_LOGGERHOLDER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Utilities/CountedPtr.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class LoggerHolderRep;
class LoggerHolderIterator;
class TableLogSink;

// <summary>
// Class holding a hierarchy of loggers.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tLoggerHolder.cc" demos="">
// </reviewed>

// <prerequisite>
//  <li> <linkto class="LogIO">LogIO</linkto> <li>
// </prerequisite>

// <synopsis>
// The LoggerHolder class implements a hierarchy of loggers.
// It has a log sink of its own and can have multiple parent LoggerHolder
// objects representing the log info of parent objects.
// It is used by class
// <linkto class=ImageInterface>ImageInterface</linkto>, but could also
// be used elsewhere.
//
// The sink of a LoggerHolder can be different depending on the type of image.
// E.g. for a transient image it can be a
// <linkto class=MemoryLogSink>MemoryLogSink</linkto>, while for a persistent
// image it will be a <linkto class=TableLogSink>TableLogSink</linkto>.
// <br>An important feature is that an LoggerHolder can have zero or more
// parent LoggerHolder objects. In that way the log of the parent object
// of an image object can be made part of the log of the image object itself,
// without having to copy the log.
//
// To iterate through all messages in a LoggerHolder (including all parents),
// the <linkto class=LoggerHolderIterator>LoggerHolderIterator</linkto> can
// be used. This is an STL-style const_iterator object.
//
// LoggerHolder uses reference counting
// (of class <linkto class=LoggerHolderRep>LoggerHolderRep</linkto>)
// to be able to retain
// the object after the (ImageInterface) object containing it is gone.
// Otherwise classes like SubImage would lose their log info.
// </synopsis>

// <example>
// <srcblock>
//  LoggerHolder logger ("tLoggerHolder_tmp.log", True);
//  logger.logio() << "test1" << LogIO::POST;
//  logger.logio() << "test2" << LogIO::POST;
//  for (LoggerHolder::const_iterator iter = logger.begin();
//       iter != logger.end();
//       iter++) {
//    cout << iter->time() << ' ' << iter->message() << endl;
//  }
// </srcblock>
// This example shows the construction of an LoggerHolder with a
// TableLogSink sink. Thereafter some messages are written.
// The latter part shows how to iterate through all messages.
//
// <srcblock>
//  LoggerHolder logger (False);
//  logger.addParent (parent.logger());
//  logger.logio() << "test1" << LogIO::POST;
//  logger.logio() << "test2" << LogIO::POST;
// </srcblock>
// This example shows the construction of an LoggerHolder with a
// MemoryLogSink sink (e.g. for a SubImage). Thereafter the logger of
// the parent image is added to it.
// Finally some messages are written.
// </example>

// <motivation>
// This class simplifies and unifies all Image logging activities.
// </motivation>

//# <todo asof="2001/06/14">
//# </todo>

class LoggerHolder
{
public:
  // Create with a NullSink or MemoryLogSink (default).
  explicit LoggerHolder (Bool nullSink = False);

  // Create with a TableLogSink.
  LoggerHolder (const String& logTableName, Bool isWritable);

  // Copy constructor (reference sematics).
  LoggerHolder (const LoggerHolder&);

  ~LoggerHolder();

  // Assignment (reference semantics).
  LoggerHolder& operator= (const LoggerHolder&);

  // Add a logger from a parent.
  void addParent (const LoggerHolder&);

  // Append the entries of the other logger to this one.
  void append (const LoggerHolder& other);

  // Reopen a readonly logtable for read/write (if needed).
  void reopenRW();

  // Reopen the log table if needed (after a tempClose).
  void reopen();

  // Temporarily close all log tables.
  // By default the possible parent log tables are also closed.
  void tempClose (Bool closeParents = True) const;

  // Unlock the log table.
  void unlock();

  // Flush the log table.
  void flush();

  // Resync the log table (if needed).
  void resync();

  // Is the log table temporarily closed?
  Bool isTempClosed() const;

  // Get access to the logger.
  // It assumes that it will be used to post a message, so it reopens
  // the log table for read/write if needed).
  LogIO& logio();

  // Get access to the log sink (reopen the log table if needed).
  // It is not assumed you want to write. If you want to do that,
  // you should first call reopenRW() to ensure you can write.
  // <group>
  LogSink& sink();
  const LogSink& sink() const;
  // </group>

  // Clear the log.
  // It removes the parents and removes all messages from the sink.
  void clear();

  // Remove all parents.
  void removeParents();

  // Return the block of parents.
  const Block<LoggerHolder>& parents() const;

  // Define the STL-style iterators.
  // Only a const forward iterator is available.
  // It makes it possible to iterate through all messages in the logger.
  // <srcblock>
  //  LoggerHolder logger("log.name", False)
  //  for (LoggerHolder::const_iterator iter=arr.begin();
  //       iter!=arr.end(); iter++) {
  //    cout << iter.message() << endl;
  //  }
  // </srcblock>
  // <group name=STL-iterator>
  // STL-style typedefs.
  typedef LoggerHolderIterator const_iterator;
  // Get the begin and end iterator object.
  const_iterator begin() const;
  const_iterator end() const;
  // </group>


private:
  CountedPtr<LoggerHolderRep> itsRep;
};




// <summary>
// Representation of the class holding a hierarchy of loggers.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tLoggerHolder.cc" demos="">
// </reviewed>

// <prerequisite>
//  <li> <linkto class="LogIO">LogIO</linkto> <li>
// </prerequisite>

// <synopsis>
// The LoggerHolderRep class is the reference counted implementation
// of <linkto class=LoggerHolder>LoggerHolder</linkto>.
// See that class for more information.
// </synopsis>

// <motivation>
// Reference counting was needed to be able to keep a LoggerHolder
// object after the (ImageInterface) object containing it is gone.
// </motivation>

//# <todo asof="2001/06/14">
//# </todo>

class LoggerHolderRep
{
public:
  // Create with a NullSink or MemoryLogSink (default).
  LoggerHolderRep (Bool nullSink);

  // Create with a TableLogSink.
  LoggerHolderRep (const String& logTableName, Bool isWritable);

  // Copy constructor.
  LoggerHolderRep (const LoggerHolderRep&);

  ~LoggerHolderRep();

  // Assignment.
  // It removes the current parents.
  LoggerHolderRep& operator= (const LoggerHolderRep&);

  // Add a logger from a parent.
  void addParent (const LoggerHolder&);

  // Append the entries of the other logger to this one.
  void append (const LoggerHolder& other);

  // Reopen a readonly logtable for read/write (if needed).
  void reopenRW();

  // Reopen the log table if needed (after a tempClose).
  void reopen()
    { if (itsIsClosed) doReopen(); }

  // Temporarily close all log tables.
  // By default the possible parent log tables are also closed.
  void tempClose (Bool closeParents = True);

  // Unlock the log table.
  void unlock();

  // Flush the log table.
  void flush();

  // Resync the log table (if needed).
  void resync();

  // Is the log table temporarily closed?
  Bool isTempClosed() const
    { return itsIsClosed; }

  // Get access to the logger.
  // It assumes that it will be used to post a message, so it reopens
  // the log table for read/write if needed).
  LogIO& logio();

  // Get access to the log sink (reopen the log table if needed).
  // It is not assumed you want to write. If you want to do that,
  // you should first call reopenRW() to ensure you can write.
  LogSink& sink();

  // Clear the log.
  // It removes the parents and removes all messages from the sink.
  void clear();

  // Remove all parents.
  void removeParents();

  // Return the block of parents.
  const Block<LoggerHolder>& parents() const
    { return itsParents; }

  // Define the STL-style iterators.
  // Only a const forward iterator is available.
  // It makes it possible to iterate through all messages in the logger.
  // <srcblock>
  //  LoggerHolder logger("log.name", False)
  //  for (LoggerHolder::const_iterator iter=arr.begin();
  //       iter!=arr.end(); iter++) {
  //    cout << iter.message() << endl;
  //  }
  // </srcblock>
  // <group name=STL-iterator>
  // STL-style typedefs.
  typedef LoggerHolderIterator const_iterator;
  // Get the begin and end iterator object.
  const_iterator begin() const;
  const_iterator end() const;
  // </group>


private:
  // Do the actual reopen.
  void doReopen();


  Block<LoggerHolder> itsParents;
  LogSink             itsSink;
  LogIO               itsLogger;
  String              itsTableName;
  TableLogSink*       itsTablePtr;
  Bool                itsIsWritable;
  Bool                itsIsClosed;
};




// <summary>
// Class representing an entry in a LoggerHolder.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tLoggerHolder.cc" demos="">
// </reviewed>

// <prerequisite>
//  <li> <linkto class="LoggerHolder">LoggerHolder</linkto> <li>
// </prerequisite>

// <synopsis>
// This class makes it possible to use the iterator in the STL-style.
// It only contains a 'pointer' to the current entry in the current logger.
// Function like <src>time()</src> can be used to retrieve the message parts.
// </synopsis>

class LogHolderIterEntry
{
public:
  LogHolderIterEntry()
    : itsSink(0), itsIndex(0) {}

  LogHolderIterEntry (const LogSink* sink, uInt index)
    : itsSink(sink), itsIndex(index) {}

  LogHolderIterEntry (const LogHolderIterEntry& that)
    : itsSink(that.itsSink), itsIndex(that.itsIndex) {}

  ~LogHolderIterEntry()
    {}

  LogHolderIterEntry& operator= (const LogHolderIterEntry& that)
    { itsSink=that.itsSink; itsIndex=that.itsIndex; return *this; }

  // Get the message parts.
  // <group>
  Double time() const
    { return itsSink->getTime(itsIndex); }
  String message() const
    { return itsSink->getMessage(itsIndex); }
  String priority() const
    { return itsSink->getPriority(itsIndex); }
  String location() const
    { return itsSink->getLocation(itsIndex); }
  String objectID() const
    { return itsSink->getObjectID(itsIndex); }
  // </group>

private:
  const LogSink* itsSink;
  uInt           itsIndex;
};




// <summary>
// Class doing the actual iteration through an LoggerHolder.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tLoggerHolder.cc" demos="">
// </reviewed>

// <prerequisite>
//  <li> <linkto class="LoggerHolder">LoggerHolder</linkto> <li>
// </prerequisite>

// <synopsis>
// This class makes it possible to use the iterator in the STL-style.
// It is used by
//<linkto class=LoggerHolderIterator>LoggerHolderIterator</linkto>
// which is the class as seen by the user.
// LogHolderIter makes it easier to make the first entry available on
// construction of an LoggerHolderIterator.
// </synopsis>

class LogHolderIter
{
public:
  // Construct the iterator on the given LoggerHolderRep.
  LogHolderIter (const LoggerHolder*);

  ~LogHolderIter();

  // Increment to next message.
  // Returns False if at the end.
  Bool next();

  // Get the entry.
  const LogHolderIterEntry& getEntry() const
    { return itsEntry; }

  const LoggerHolder& logger() const
    { return *itsLogger; }

private:
  // Copy constructor is not needed, thus forbidden.
  LogHolderIter (const LogHolderIter&);

  // Assignment is not needed, thus forbidden.
  LogHolderIter& operator= (const LogHolderIter&);


  const LoggerHolder* itsLogger;
  Bool                itsTempClosed;
  LogHolderIter*      itsParentIter;
  uInt                itsCounter;
  LogHolderIterEntry  itsEntry;
};



// <summary>
// Class to iterate through an LoggerHolder.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tLoggerHolder.cc" demos="">
// </reviewed>

// <prerequisite>
//  <li> <linkto class="LoggerHolder">LoggerHolder</linkto> <li>
// </prerequisite>

// <synopsis>
// This class makes it possible to iterate in the STL-style through all
// entries of an LoggerHolder object. If the logger has parent LoggerHolder
// objects, it first iterates through all parents (recursively) and
// finally through all entries in the LoggerHolder object itself.
// </synopsis>

// <example>
// <srcblock>
//  LoggerHolder logger ("tLoggerHolder_tmp.log", True);
//  logger.logio() << "test1" << LogIO::POST;
//  logger.logio() << "test2" << LogIO::POST;
//  for (LoggerHolder::const_iterator iter = logger.begin();
//       iter != logger.end();
//       iter++) {
//    cout << iter->time() << ' ' << iter->message() << endl;
//  }
// </srcblock>
// </example>

class LoggerHolderIterator
{
public:
  LoggerHolderIterator()
    : itsIter(0), itsNotAtEnd(False) {}

  LoggerHolderIterator (const LoggerHolder*);

  LoggerHolderIterator (const LoggerHolderIterator&);

  ~LoggerHolderIterator()
    { delete itsIter; }

  LoggerHolderIterator& operator= (const LoggerHolderIterator&);

  // Increment to next message.
  // <group>
  void operator++()
    { next(); }
  void operator++ (int)
    { next(); }
  // </group>

  // Is the iterator not at the end yet?
  Bool operator!= (const LoggerHolderIterator&)
    { return itsNotAtEnd; }

  // Get the entry.
  // <group>
  const LogHolderIterEntry& operator*() const
    { return itsIter->getEntry(); }
  const LogHolderIterEntry* operator->() const
    { return &(itsIter->getEntry()); }
  // </group>

  const LoggerHolder& logger() const
    { return itsIter->logger(); }

private:
  // Get the next entry (if available).
  void next()
    { itsNotAtEnd = itsIter->next(); }


  LogHolderIter* itsIter;
  Bool           itsNotAtEnd;
};



inline void LoggerHolder::reopen()
{
  itsRep->reopen();
}
inline Bool LoggerHolder::isTempClosed() const
{
  return itsRep->isTempClosed();
}
inline LogIO& LoggerHolder::logio()
{
  return itsRep->logio();
}
inline LogSink& LoggerHolder::sink()
{
  return itsRep->sink();
}
inline const LogSink& LoggerHolder::sink() const
{
  return itsRep->sink();
}
inline const Block<LoggerHolder>& LoggerHolder::parents() const
{
  return itsRep->parents();
}
inline LoggerHolder::const_iterator LoggerHolder::begin() const
{
  return LoggerHolderIterator (this);
}
inline LoggerHolder::const_iterator LoggerHolder::end() const
{
  return LoggerHolderIterator();
}




} //# NAMESPACE CASACORE - END

#endif
