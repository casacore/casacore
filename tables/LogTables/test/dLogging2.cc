//# dLogging.cc: This program demonstrates the logging system.
//# Copyright (C) 1996,2001,2003
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

//#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//#!!
//#!! If you modify this file, change casa/Logging.h to reflect the changes.
//#!!
//#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

//##### This file is documented in casa/Logging.h.

#include <casacore/casa/Logging.h>
#include <casacore/tables/LogTables/TableLogSink.h>
#include <casacore/casa/Arrays.h>

#include <casacore/casa/sstream.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
class DataClass
{
public:
  DataClass(const IPosition &shape, const LogSink &sink);  // 1
  void set(Int toWhat);                                    // 2
  LogSink &sink() {return log_sink_p;}                     // 3
  Array<Int> &data() {return data_p;}                      // 4
  const Array<Int> &data() const {return data_p;}          // 5
private:                                                   // 6
  Vector<Int> data_p;                                      // 7
  LogSink log_sink_p;                                      // 8
};

DataClass::DataClass(const IPosition &shape, const LogSink &sink)
  : log_sink_p(sink)                                                     // 1
{                                                                        // 2
  LogOrigin  where("DataClass",                                          // 3
	       "DataClass(const IPosition &shape, const LogSink &sink)", // 4
	       WHERE);                                                   // 5
  LogMessage logMessage(where);                                          // 6
                                                                         // 7
  if (shape.nelements() != 1) {                                          // 8
    logMessage.priority(LogMessage::SEVERE).line(__LINE__).message(      // 9
	     "Illegal Shape! Must be one dimensional.");                 // 10
    log_sink_p.postThenThrow(logMessage, AipsError());                   // 11
  }                                                                      // 12
                                                                         // 13
  data_p.resize(shape(0));                                               // 14
  ostringstream buffer;                                                     // 15
  buffer << "Inital shape " << shape << "and value 2";                   // 16
  log_sink_p.post(                                                       // 17
  logMessage.priority(LogMessage::NORMAL).line(__LINE__).                // 19
     message(buffer));                                                   // 19
                                                                         // 20
  set(2);                                                                // 21
}


void DataClass::set(Int toWhat)
{
  LogOrigin  where("DataClass", "set(Int toWhat)", WHERE);                // 1
  LogMessage logMessage(where);                                           // 2
  ostringstream buffer;                                                   // 3
  buffer << "Setting data values to " << toWhat;                          // 4
  log_sink_p.post(logMessage.priority(LogMessage::NORMAL).line(__LINE__). // 5
		  message(buffer));                                       // 6
  uInt n = data_p.nelements();                                            // 7
  for (uInt i=0; i < n; i++) {                                            // 8
#ifdef AIPS_DEBUG                                                         // 9
    ostringstream buffer;                                                 // 10
    buffer << "Setting element  " << i << " to " << toWhat;               // 11
    logMessage.priority(LogMessage::DEBUGGING).line(__LINE__).            // 12
               message(buffer);                                           // 13
    log_sink_p.post(logMessage);                                          // 14
#endif                                                                    // 15
    data_p(i) = toWhat;                                                   // 16
  }
}

void square(DataClass &object)
{
  object.sink().post(LogMessage("Squaring object.data elements",         // 1
				LogOrigin("square(DataClass &object)",   // 2
					  WHERE)));                      // 3
  object.data() *= object.data();                                        // 4
}

float sum(const DataClass &object)
{
  float theSum = sum(object.data());                                 // 1
  ostringstream buffer;                                              // 2
  buffer << "Sum of object is: " << theSum;                          // 3
  LogSink::postGlobally(LogMessage(buffer,                           // 4
	   LogOrigin("sum(const DataClass &object)", WHERE)));       // 5
  return theSum;                                                     // 6
}


int main()
{
    LogSink::globalSink().filter(LogFilter(LogMessage::DEBUGGING));// 1
    LogSink logger = TableLogSink::makeSink
                                ("dLogging2_tmp_messages");        // 2
                                                                   // 3
    IPosition legalShape(1, 10);                                   // 4
    DataClass dc(legalShape, logger);                              // 5
			                                           // 6
    square(dc);                                                    // 7
                                                                   // 8
    Float total = sum(dc);                                         // 9
    if (total != 40) {                                             // 10
      cout << "sum is incorrect" << endl;                          // 11
      return 1;                                                    // 12
    }                                                              // 13
    return 0;                                                      // 14
}
