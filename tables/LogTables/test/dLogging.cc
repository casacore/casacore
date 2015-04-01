//# dLogging.cc: This program demonstrates the logging system.
//# Copyright (C) 1996,1997,2001,2003
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
  LogIO   &sink() {return os_p;}                           // 3
  Array<Int> &data() {return data_p;}                      // 4
  const Array<Int> &data() const {return data_p;}          // 5
private:                                                   // 6
  Vector<Int> data_p;                                      // 7
  LogSink log_sink_p;                                      // 8
  LogIO os_p;                                              // 9
};

DataClass::DataClass(const IPosition &shape, const LogSink &sink)
  : log_sink_p(sink), os_p(log_sink_p)                                     // 1
{                                                                          // 2
  os_p << LogOrigin("DataClass",                                           // 3
               "DataClass(const IPosition &shape, const LogSink &sink)");  // 4
                                                                           // 5
  if (shape.nelements() != 1) {                                            // 6
    os_p << LogIO::SEVERE << WHERE <<                                      // 7
      "Illegal Shape! Must be one dimensional." << LogIO::EXCEPTION;       // 8
  }                                                                        // 9
                                                                           // 10
  data_p.resize(shape(0));                                                 // 11
  os_p << "Inital shape " << shape << "and value 2" <<                     // 12
      LogIO::NORMAL << LogIO::POST;                                        // 13
                                                                           // 14
  set(2);                                                                  // 15
}


void DataClass::set(Int toWhat)
{
  os_p << LogIO::NORMAL << LogOrigin("DataClass", "set(Int toWhat)");      // 1
  os_p << "Setting data values to " << toWhat << WHERE << LogIO::POST;     // 2
  uInt n = data_p.nelements();                                             // 3
  for (uInt i=0; i < n; i++) {                                             // 4
#ifdef AIPS_DEBUG                                                          // 5
    os_p << LogIO::DEBUGGING << WHERE <<                                   // 6
      "Setting element  " << i << " to " << toWhat << LogIO::POST;         // 7
#endif                                                                     // 8
    data_p(i) = toWhat;                                                    // 9
  }
}

void square(DataClass &object)
{
  object.sink() << LogIO::NORMAL << WHERE <<                              // 1
    LogOrigin("square(DataClass &object)") << "Squaring data elements"    // 2
        << LogIO::POST;                                                   // 3
  object.data() *= object.data();                                         // 4
}

float sum(const DataClass &object)
{
  LogIO global(LogOrigin("sum(const DataClass &object)"));        // 1
  float theSum = sum(object.data());                              // 2
  global << WHERE << "Sum of object is: " << theSum;              // 3 
  return theSum;                                                  // 4
}


int main()
{
    LogSink::globalSink().filter(LogFilter(LogMessage::DEBUGGING));// 1
    LogSink logger = TableLogSink::makeSink
                                ("dLogging_tmp_messages");         // 2
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
