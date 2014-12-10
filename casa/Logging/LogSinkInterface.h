//# LogSinkInterface.h: Accepts LogMessages and posts them to some destination
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

#ifndef CASA_LOGSINKINTERFACE_H
#define CASA_LOGSINKINTERFACE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Logging/LogFilterInterface.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
//Accepts LogMessages and posts them to some destination
// </summary>

// <use visibility=local>

// <reviewed reviewer="wbrouw" date="1996/08/21" tests="tLogging.cc"  demos="dLogging.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LogMessage>LogMessage</linkto>
//   <li> <linkto class=LogFilterInterface>LogFilterInterface</linkto>
// </prerequisite>
//
// <etymology>
// Log as in "Log Book." Sink from its common usage ("source/sink") as a thing
// which can accept some substance or energy. Interface because this is an
// abstract, not concrete, class.
// </etymology>
//
// <synopsis>
// This abstract base class is not intended for applications programmers. 
// Instead they should look at <linkto class=LogSink>LogSink</linkto>.
//
// This class defines a minimal "posting" interface for all objects which accept
// log messages. The fundamental model of a <src>LogSinkInterface</src> is:
// <ol>
// <li> That it contains a
//      <linkto class=LogFilterInterface>LogFilterInterface</linkto> that is
//      used to accept or reject messages; and
// <li> That it has a post message that takes a log message; and, if it passes
//      the filter, does something with it (prints it to a stream, saves it to
//      a table, ...).
// </ol>
// There is no notion of local vs global sinks - that is imposed by
// <src>LogSink</src>.
// </synopsis>
//
// <example>
// <srcblock>
// LogSinkInterface &ref = ...;
// LogMessage message(...);
// ref.postLocally(message);
// if (ref.filter().lowestPriority() != LogMessage::DEBUGGING) {
//    ref.filter(LogMessage::DEBUGGING);
// }
// </srcblock>
// For a more complete example see <linkto file="Logging.h">Logging.h</linkto>.
// </example>
//
// <motivation>
// Make it straightforward to extend the number of places a message may be
// in the future through derivation.
// </motivation>
//
// <todo asof="1996/07/24">
//   <li> Nothing known.
// </todo>


class LogSinkInterface
{
public:
  // Create with a <src>NORMAL</src> filter.
  LogSinkInterface();
  // Create with the supplied <src>filter</src>.
  LogSinkInterface(const LogFilterInterface &filter);

  // Copy semantics - copy the filter from <src>other</src> to <src>this</src>
  // <group>
  LogSinkInterface(const LogSinkInterface &other);
  LogSinkInterface &operator=(const LogSinkInterface &);
  // </group>

  virtual ~LogSinkInterface();

  // Get/set the filter.
  // <group>
  virtual const LogFilterInterface &filter() const;
  virtual LogSinkInterface &filter(const LogFilterInterface &filter);
  // </group>

  // Get number of messages in sink.
  virtual uInt nelements() const;

  // Get given part of the i-th message from the sink.
  // <group>
  virtual Double getTime (uInt i) const;
  virtual String getPriority (uInt i) const;
  virtual String getMessage (uInt i) const;
  virtual String getLocation (uInt i) const;
  virtual String getObjectID (uInt i) const;
  // </group>

  // This function must be over-ridden in derived classes. If the filter
  // passes the message, do what is necessary with the message and return
  // <src>True</src>.
  virtual Bool postLocally(const LogMessage &message)= 0;

  // Write any pending output.
  virtual void flush (Bool global=True);

  // Write a message (usually from another logsink) into the local one.
  // The default implementation does nothing.
  virtual void writeLocally (Double time, const String& message,
			     const String& priority, const String& location,
			     const String& objectID);

  // Clear the local sink (i.e. remove all messages from it).
  // The default implementation does nothing.
  virtual void clearLocally();

  // Returns the id for this class...
  static String localId( );
  // Returns the id of the LogSink in use...
  virtual String id( ) const = 0;
  // Write to cerr too
  virtual void cerrToo(bool cerr2);
  void setTaskName(const String &theTask){taskName=theTask;}
private:
  LogFilterInterface* filter_p;
protected:
  String taskName;
};



} //# NAMESPACE CASACORE - END

#endif
