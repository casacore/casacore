//# TableLogSink.h: Save log messages in an AIPS++ Table
//# Copyright (C) 1996,1997
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

#if !defined(AIPS_TABLE_LOG_SINK_H)
#define AIPS_TABLE_LOG_SINK_H

#include <aips/aips.h>
#include <aips/Logging/LogSink.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>

class TableDesc;

// <summary>
// Save log messages in an AIPS++ Table
// </summary>

// <use visibility=export>

// <reviewed reviewer="wbrouw" date="1996/08/21" tests="tLogging.cc" demos="dLogging.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LogSinkInterface>LogSinkInterface</linkto>
//   <li> <linkto module=Tables>Tables</linkto>
// </prerequisite>
//
// <etymology>
// Log to an AIPS++ Table.
// </etymology>
//
// <synopsis>
// Unlike the other classes derived from 
// <linkto class=LogSinkInterface>LogSinkInterface</linkto>, there are utility
// functions in this class which might be of some modest interest. In
// particular, the member functions which define the structure of the table
// and define the column names might be of interest.
//
// This class posts messages which pass the filter to an AIPS++
// <linkto class=Table>Table</linkto>. It puts ever field of the
// <linkto class=LogMessage>LogMessage</linkto> into its own column.
// </synopsis>
//
// <example>
// See <linkto file="Logging.h">Logging.h</linkto>.
// </example>
//
// <motivation>
// "Persistent" log messages must be stored in a Table.
// </motivation>
//
// <todo asof="1996/07/24">
//   <li> Review the default storage managers, or allow them to be over-ridden,
//        at some point.
//   <li> Allow a subset of the columns to be written? e.g., only time, 
//        message, and priority.
// </todo>

class TableLogSink : public LogSinkInterface
{
public:
    // If <src>fileName</src> exists, attach and append to it, otherwise create
    // a file with that name. If the table exists, it must have all the required
    // columns defined by <src>logTableDescription()</src>.
    TableLogSink(const LogFilter &filter, const String &fileName);

    // After copying, both sinks will write to the same <src>Table</src>.
    // <group>
    TableLogSink(const TableLogSink &other);
    TableLogSink& operator=(const TableLogSink &other);
    // </group>

    ~TableLogSink();

    // If the message passes the filter, write it to the log table.
    virtual Bool postLocally(const LogMessage &message);

    // Return the table which is being written to.
    // <group>
    const Table &table() const;
    Table &table();
    // </group>
  
    // Defines the minimal set of columns in the table (more may exist, but
    // are ignored.
    enum Columns { 
      // MJD in seconds. (Double.)
      TIME, 
      // Message importance. (String).
      PRIORITY,
      // Informational message. (String).
      MESSAGE, 
      // Class name of message source. (String).
      CLASS, 
      // Function name of message source. (String).
      FUNCTION, 
      // File name of message source. (String).
      FILE, 
      // Line number of message source. (Int).
      LINE,
      // ObjectID of distributed object that created the message (Int[4]).
      OBJECT_ID };

    // Turn the <src>Columns</src> enum into a String which is the actual
    // column name in the <src>Table</src>.
    static String columnName(Columns which);
    // Description of the log table. You can use this if, e.g., you do not
    // want to use the storage managers that this class creates by default
    // (currently Miriad).
    static TableDesc logTableDescription();

    // Write out any pending output to the table.
    virtual void flush();

    // This will non longer be needed when all compilers have "real"
    // exceptions.
    virtual void cleanup();
private:
    // Undefined and inaccessible
    TableLogSink();
    // Avoid duplicating code in copy ctor and assignment operator
    void copy_other(const TableLogSink &other);

    Table log_table_p;
    // Message
    ScalarColumn<Double>   time_p;
    ScalarColumn<String>  priority_p;
    ScalarColumn<String>  message_p;
    // Origin
    ScalarColumn<String>  class_p;
    ScalarColumn<String>  function_p;
    ScalarColumn<String>  file_p;
    ScalarColumn<Int>     line_p;
    ArrayColumn<Int>      object_id_p;
};

//# Inlines
inline const Table &TableLogSink::table() const {return log_table_p;}
inline Table &TableLogSink::table() {return log_table_p;}

#endif
