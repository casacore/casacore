//# ImageLogger.h: Class to handle all image logging
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

#if !defined(AIPS_IMAGELOGGER_H)
#define AIPS_IMAGELOGGER_H

//# Includes
#include <aips/Logging/LogIO.h>
#include <aips/Containers/Block.h>

//# Forward Declarations
class ImageLoggerIterator;

// <summary>
// Class to handle all image logging.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tImageLogger.cc" demos="">
// </reviewed>

// <prerequisite>
//  <li> <linkto class="LogIO">LogIO</linkto> <li>
// </prerequisite>

// <synopsis>
// The ImageLogger class is the destination for all logging in the Image
// classes (i.e. classes derived from
// <linkto class=ImageInterface>ImageInterface</linkto>).
//
// The sink of a ImageLogger can be different depending on the type of image.
// E.g. for an ImageExpr object it can be a
// <linkto class=MemoryLogSink>MemoryLogSink</linkto>, while for a PagedImage
// it will be a <linkto class=TableLogSink>TableLogSink</linkto>.
// <br>An important feature is that an ImageLogger can have zero or more
// parent ImageLogger objects. In that way the log of the parent image
// of e.g. a SubImage can be made part of the log of the SubImage itself,
// without having to copy the log.
//
// To iterate through all messages in an ImageLogger (including all parents),
// the <linkto class=ImageLoggerIterator>ImageLoggerIterator</linkto> can
// be used. This is an STL-style const_iterator object.
// </synopsis>

// <example>
// <srcblock>
//  ImageLogger logger ("tImageLogger_tmp.log", True);
//  logger.logio() << "test1" << LogIO::POST;
//  logger.logio() << "test2" << LogIO::POST;
//  for (ImageLogger::const_iterator iter = logger.begin();
//       iter != logger.end();
//       iter++) {
//    cout << iter->time() << ' ' << iter->message() << endl;
//  }
// </srcblock>
// This example shows the construction of an ImageLogger with a
// TableLogSink sink. Thereafter some messages are written.
// The latter part shows how to iterate through all messages.
//
// <srcblock>
//  ImageLogger logger (False);
//  logger.addParent (parentImage.logger());
//  logger.logio() << "test1" << LogIO::POST;
//  logger.logio() << "test2" << LogIO::POST;
// </srcblock>
// This example shows the construction of an ImageLogger with a
// MemoryLogSink sink (e.g. for a SubImage). Thereafter the logger of
// the parent image is added to it.
// Finally some messages are written.
// </example>

// <motivation>
// This class simplifies and unifies all Image logging activities.
// </motivation>

//# <todo asof="2001/06/14">
//# </todo>

class ImageLogger
{
public:
  // Create with a NullSink or MemoryLogSink (default).
  ImageLogger (Bool nullSink = False);

  // Create with a TableLogSink.
  ImageLogger (const String& logTableName, Bool isWritable);

  // Copy constructor.
  ImageLogger (const ImageLogger&);

  ~ImageLogger();

  // Assignment.
  ImageLogger& operator= (const ImageLogger&);

  // Add a logger from a parent image.
  void addParent (const ImageLogger*);

  // Append the entries of the other logger to this one.
  void append (const ImageLogger& other);

  // Reopen a readonly logtable for read/write.
  void reopenRW();

  // Reopen the log table if needed (after a tempClose).
  void reopen()
    { if (itsIsClosed) doReopen(); }

  // Temporarily close all log tables.
  // By default the possible parent log tables are also closed.
  void tempClose (Bool closeParents = True);

  // Close this log table permanently.
  void close();

  // Unlock the log table.
  void unlock();

  // Flush the log table.
  void flush();

  // Resync the log table (if needed).
  void resync();

  // Is the log table temporarily closed?
  Bool isTempClosed() const
    { return itsIsClosed; }

  // Get access to the logger (reopen the log table if needed).
  LogIO& logio()
    {
      if (itsIsClosed) reopen();
      return itsLogger;
    }

  // Get access to the log sink (reopen the log table if needed).
  LogSink& sink()
    {
      if (itsIsClosed) reopen();
      return itsSink;
    }

  // Return the block of parents.
  const PtrBlock<ImageLogger*> parents() const
    { return itsParents; }

  // Define the STL-style iterators.
  // Only a const forward iterator is available.
  // It makes it possible to iterate through all messages in the logger.
  // <srcblock>
  //  ImageLogger logger("log.name", False)
  //  for (ImageLogger::const_iterator iter=arr.begin();
  //       iter!=arr.end(); iter++) {
  //    cout << iter.message() << endl;
  //  }
  // </srcblock>
  // <group name=STL-iterator>
  // STL-style typedefs.
  typedef ImageLoggerIterator const_iterator;
  // Get the begin and end iterator object.
  const_iterator begin() const;
  const_iterator end() const;
  // </group>


private:
  void doReopen();


  PtrBlock<ImageLogger*> itsParents;
  LogSink                itsSink;
  LogIO                  itsLogger;
  String                 itsTableName;
  TableLogSink*          itsTablePtr;
  Bool                   itsIsWritable;
  Bool                   itsIsClosed;
};




// <summary>
// Class representing an entry in a logger.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tImageLogger.cc" demos="">
// </reviewed>

// <prerequisite>
//  <li> <linkto class="ImageLogger">ImageLogger</linkto> <li>
// </prerequisite>

// <synopsis>
// This class makes it possible to use the iterator in the STL-style.
// It only contains a 'pointer' to the current entry in the current logger.
// Function like <src>time()</src> can be used to retrieve the message parts.
// </synopsis>

class ImageLogIterEntry
{
public:
  ImageLogIterEntry()
    : itsSink(0), itsIndex(0) {}

  ImageLogIterEntry (const LogSink* sink, uInt index)
    : itsSink(sink), itsIndex(index) {}

  ImageLogIterEntry (const ImageLogIterEntry& that)
    : itsSink(that.itsSink), itsIndex(that.itsIndex) {}

  ~ImageLogIterEntry()
    {}

  ImageLogIterEntry& operator= (const ImageLogIterEntry& that)
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
// Class doing the actual iteration through an ImageLogger.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tImageLogger.cc" demos="">
// </reviewed>

// <prerequisite>
//  <li> <linkto class="ImageLogger">ImageLogger</linkto> <li>
// </prerequisite>

// <synopsis>
// This class makes it possible to use the iterator in the STL-style.
// It is used by <linkto class=ImageLoggerIterator>ImageLoggerIterator</linkto>
// which is the class as seen by the user.
// ImageLogIter makes it easier to make the first entry available on
// construction of an ImageLoggerIterator.
// </synopsis>

class ImageLogIter
{
public:
  // Construct the iterator on the given ImageLogger.
  ImageLogIter (const ImageLogger*);

  ~ImageLogIter();

  // Increment to next message.
  // Returns False if at the end.
  Bool next();

  // Get the entry.
  const ImageLogIterEntry& getEntry() const
    { return itsEntry; }

  const ImageLogger& logger() const
    { return *itsLogger; }

private:
  // Copy constructor is not needed, thus forbidden.
  ImageLogIter (const ImageLogIter&);

  // Assignment is not needed, thus forbidden.
  ImageLogIter& operator= (const ImageLogIter&);


  const ImageLogger* itsLogger;
  Bool               itsTempClosed;
  ImageLogIter*      itsParentIter;
  uInt               itsCounter;
  ImageLogIterEntry  itsEntry;
};



// <summary>
// Class to iterate through an ImageLogger.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tImageLogger.cc" demos="">
// </reviewed>

// <prerequisite>
//  <li> <linkto class="ImageLogger">ImageLogger</linkto> <li>
// </prerequisite>

// <synopsis>
// This class makes it possible to iterate in the STL-style through all
// entries of an ImageLogger object. If the logger has parent ImageLogger
// objects, it first iterates through all parents (recursively) and
// finally through all entries in the ImageLogger object itself.
// </synopsis>

// <example>
// <srcblock>
//  ImageLogger logger ("tImageLogger_tmp.log", True);
//  logger.logio() << "test1" << LogIO::POST;
//  logger.logio() << "test2" << LogIO::POST;
//  for (ImageLogger::const_iterator iter = logger.begin();
//       iter != logger.end();
//       iter++) {
//    cout << iter->time() << ' ' << iter->message() << endl;
//  }
// </srcblock>
// </example>

class ImageLoggerIterator
{
public:
  ImageLoggerIterator()
    : itsIter(0), itsNotAtEnd(False) {}

  ImageLoggerIterator (const ImageLogger*);

  ImageLoggerIterator (const ImageLoggerIterator&);

  ~ImageLoggerIterator()
    { delete itsIter; }

  ImageLoggerIterator& operator= (const ImageLoggerIterator&);

  // Increment to next message.
  // <group>
  void operator++()
    { next(); }
  void operator++ (int)
    { next(); }
  // </group>

  // Is the iterator not at the end yet?
  Bool operator!= (const ImageLoggerIterator& that)
    { return itsNotAtEnd; }

  // Get the entry.
  // <group>
  const ImageLogIterEntry& operator*() const
    { return itsIter->getEntry(); }
  const ImageLogIterEntry* operator->() const
    { return &(itsIter->getEntry()); }
  // </group>

  const ImageLogger& logger() const
    { return itsIter->logger(); }

private:
  // Get the next entry (if available).
  void next()
    { itsNotAtEnd = itsIter->next(); }


  ImageLogIter* itsIter;
  Bool          itsNotAtEnd;
};



inline ImageLogger::const_iterator ImageLogger::begin() const
{
  return ImageLoggerIterator (this);
}
inline ImageLogger::const_iterator ImageLogger::end() const
{
  return ImageLoggerIterator();
}


#endif
