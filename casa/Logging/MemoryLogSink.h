//# MemoryLogSink.h: Save log messages in memory
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

#ifndef CASA_MEMORYLOGSINK_H
#define CASA_MEMORYLOGSINK_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Logging/LogSinkInterface.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

// <summary>
// Save log messages in memory.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tLogging.cc" demos="dLogging.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LogSinkInterface>LogSinkInterface</linkto>
// </prerequisite>
//
// <synopsis>
// This class posts messages which pass the filter to 
// memory.
// </synopsis>
//
// <example>
// See <linkto file="Logging.h">Logging.h</linkto>.
// </example>
//
// <motivation>
// For temporary images log messages must be held in memory temporarily.
// </motivation>
//
//# <todo asof="2001/06/12">
//# </todo>

class MemoryLogSink : public LogSinkInterface
{
public:
  // Create an empty sink without a filter.
  MemoryLogSink();

  // Create an empty sink with the given filter.
  // <group>
  explicit MemoryLogSink (LogMessage::Priority filter);
  explicit MemoryLogSink (const LogFilterInterface& filter);
  // </group>

  // Copy constructor (copy semantics).
  MemoryLogSink (const MemoryLogSink& other);

  // Assignment (copy semantics).
  MemoryLogSink& operator= (const MemoryLogSink& other);
  
  virtual ~MemoryLogSink();

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

  // If the message passes the filter, write it to memory
  virtual Bool postLocally (const LogMessage& message);

  // Write a message (usually from another logsink) into the local one.
  virtual void writeLocally (Double time, const String& message,
			     const String& priority, const String& location,
			     const String& objectID);

  // Clear the local sink (i.e. remove all messages from it).
  virtual void clearLocally();

  // Returns the id for this class...
  static String localId( );
  // Returns the id of the LogSink in use...
  String id( ) const;

private:
  // Avoid duplicating code in copy ctor and assignment operator
  void copy_other (const MemoryLogSink& other);

  // Rezize the blocks to the given size, but at least 64 elements
  // more than the current size.
  void resize (uInt nrnew);

  uInt          nmsg_p;
  Block<Double> time_p;
  Block<String> priority_p;
  Block<String> message_p;
  Block<String> location_p;
  Block<String> objectID_p;
};



} //# NAMESPACE CASACORE - END

#endif
