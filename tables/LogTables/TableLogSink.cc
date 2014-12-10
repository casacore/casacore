//# TableLogSink.h: save log messages in a Casacore Table
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2003
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
//# $Id$

#include <casacore/casa/Arrays/Vector.h>
#include <casacore/tables/LogTables/TableLogSink.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/DataMan/StandardStMan.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicSL/Constants.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

String TableLogSink::localId( ) {
    return String("TableLogSink");
}

String TableLogSink::id( ) const {
    return String("TableLogSink");
}

TableLogSink::TableLogSink (LogMessage::Priority filter,
			    const String& fileName)
: LogSinkInterface(LogFilter(filter))
{
    init (fileName);
}

TableLogSink::TableLogSink (const LogFilterInterface& filter,
			    const String& fileName)
: LogSinkInterface(filter)
{
    init (fileName);
}

void TableLogSink::init (const String& fileName)
{
    LogMessage logMessage(LogOrigin("TableLogSink", "TableLogSink", WHERE));
    if (fileName.empty()) {
        // Create temporary table
        logMessage.priority(LogMessage::DEBUGGING).line(__LINE__).
                            message("Creating temporary log table");
	LogSink::postGlobally(logMessage);
        SetupNewTable setup (fileName, logTableDescription(), Table::Scratch);
	makeTable (setup);
    } else if (Table::isWritable(fileName)) {
	log_table_p = Table(fileName, Table::Update);
	logMessage.priority(LogMessage::DEBUGGING).line(__LINE__).message(
	  String("Opening existing file ") + fileName);
	LogSink::postGlobally(logMessage);
    } else if (Table::isReadable(fileName)) {
        // We can read it, but not write it!
        logMessage.priority(LogMessage::SEVERE).line(__LINE__).message(
	   fileName + " exists, but is not writable");
	LogSink::postGloballyThenThrow(logMessage);
    } else {
        // Table does not exist - create
        logMessage.priority(LogMessage::DEBUGGING).line(__LINE__).
                            message("Creating " + fileName);
	LogSink::postGlobally(logMessage);
        SetupNewTable setup (fileName, logTableDescription(), Table::New);
        makeTable (setup);
    }
    attachCols();
}

TableLogSink::TableLogSink (const String& fileName)
: LogSinkInterface()
{
    LogMessage logMessage(LogOrigin("TableLogSink", "TableLogSink", WHERE));
    if (! Table::isReadable (fileName)) {
        // Table does not exist.
        logMessage.priority(LogMessage::SEVERE).line(__LINE__).message(
	   fileName + " does not exist or is not readable");
	LogSink::postGloballyThenThrow(logMessage);
    } else {
	log_table_p = Table(fileName);
	logMessage.priority(LogMessage::DEBUGGING).line(__LINE__).message(
	  String("Opening readonly ") + fileName);
	LogSink::postGlobally(logMessage);
    }
    attachCols();
}

TableLogSink::TableLogSink (const TableLogSink& other)
: LogSinkInterface()
{
    copy_other (other);
}

TableLogSink& TableLogSink::operator= (const TableLogSink& other)
{
    if (this != &other) {
        copy_other(other);
    }
    return *this;
}

void TableLogSink::copy_other (const TableLogSink& other)
{
    LogSinkInterface::operator= (other);
    log_table_p = other.log_table_p;
    time_p.reference     (other.time_p);
    priority_p.reference (other.priority_p);
    message_p.reference  (other.message_p);
    location_p.reference (other.location_p);
    id_p.reference       (other.id_p);
}

TableLogSink::~TableLogSink()
{
    flush();
}

void TableLogSink::makeTable (SetupNewTable& setup)
{
    // Bind all to the SSM.
    StandardStMan stman("SSM", 32768);
    setup.bindAll(stman);
    log_table_p = Table(setup);
    log_table_p.tableInfo() = TableInfo(TableInfo::LOG);
    log_table_p.tableInfo().
	  readmeAddLine("Repository for software-generated logging messages");
}

void TableLogSink::attachCols()
{
    time_p.attach     (log_table_p, columnName(TIME));
    priority_p.attach (log_table_p, columnName(PRIORITY));
    message_p.attach  (log_table_p, columnName(MESSAGE));
    location_p.attach (log_table_p, columnName(LOCATION));
    id_p.attach       (log_table_p, columnName(OBJECT_ID));
    // If writable, define the time keywords when not defined yet.
    // In this way the table browser can interpret the times.
    if (log_table_p.isWritable()) {
        TableRecord& keySet = time_p.rwKeywordSet();
        if (! keySet.isDefined ("UNIT")) {
          keySet.define ("UNIT", "s");
          keySet.define ("MEASURE_TYPE", "EPOCH");
          keySet.define ("MEASURE_REFERENCE", "UTC");
        }
    }
}

void TableLogSink::reopenRW (const LogFilterInterface& aFilter)
{
    log_table_p.reopenRW();
    attachCols();
    filter (aFilter);
}

Bool TableLogSink::postLocally (const LogMessage& message)
{
    if (log_table_p.isWritable()) {
        log_table_p.reopenRW();
        attachCols();
    }
    Bool posted = False;
    if (filter().pass(message)) {
	String tmp;
	message.origin().objectID().toString(tmp);
        writeLocally (message.messageTime().modifiedJulianDay()*C::day,
                      message.message(),
                      LogMessage::toString(message.priority()),
                      message.origin().location(),
                      tmp);
        posted = True;
    }
    return posted;
}

uInt TableLogSink::nelements() const
{
  return table().nrow();
}

Double TableLogSink::getTime (uInt i) const
{
  AlwaysAssert (i < table().nrow(), AipsError);
  return roTime()(i);
}
String TableLogSink::getPriority (uInt i) const
{
  AlwaysAssert (i < table().nrow(), AipsError);
  return roPriority()(i);
}
String TableLogSink::getMessage (uInt i) const
{
  AlwaysAssert (i < table().nrow(), AipsError);
  return roMessage()(i);
}
String TableLogSink::getLocation (uInt i) const
{
  AlwaysAssert (i < table().nrow(), AipsError);
  return roLocation()(i);
}
String TableLogSink::getObjectID (uInt i) const
{
  AlwaysAssert (i < table().nrow(), AipsError);
  return roObjectID()(i);
}

String TableLogSink::columnName (TableLogSink::Columns which)
{
  switch (which) {
  case TIME:
    return "TIME";
  case PRIORITY:
    return "PRIORITY";
  case MESSAGE:
    return "MESSAGE";
  case LOCATION:
    return "LOCATION";
  case OBJECT_ID:
    return "OBJECT_ID";
  default:
    AlwaysAssert(! "REACHED", AipsError);
  }
  return "";
}

TableDesc TableLogSink::logTableDescription()
{
  TableDesc desc;
  desc.comment() = "Log message table";

  desc.addColumn (ScalarColumnDesc<Double>(columnName(TIME),
					   "MJD in seconds"));
  ScalarColumnDesc<String> pdesc (columnName(PRIORITY));
  pdesc.setMaxLength (9);   // Longest is DEBUGGING
  desc.addColumn (pdesc);
  desc.addColumn (ScalarColumnDesc<String>(columnName(MESSAGE)));
  desc.addColumn (ScalarColumnDesc<String>(columnName(LOCATION)));
  desc.addColumn (ScalarColumnDesc<String>(columnName(OBJECT_ID)));
  return desc;
}

void TableLogSink::flush(Bool)
{
  log_table_p.flush();
}

void TableLogSink::writeLocally (Double mtime,
				 const String& mmessage,
				 const String& mpriority,
				 const String& mlocation,
				 const String& mobjectID)
{
  uInt offset = table().nrow();
  // cout << "writing " << mmessage << " at row " << offset << endl;
  table().addRow(1);
  time().put     (offset, mtime);
  message().put  (offset, mmessage);
  priority().put (offset, mpriority);
  location().put (offset, mlocation);
  objectID().put (offset, mobjectID);
}

void TableLogSink::clearLocally()
{
  String fileName = log_table_p.tableName();
  // Delete current log table.
  log_table_p.markForDelete();
  log_table_p = Table();
  // Create new log table.
  SetupNewTable setup (fileName, logTableDescription(), Table::New);
  makeTable (setup);
  attachCols();
}


LogSink TableLogSink::makeSink (const LogFilterInterface &filter,
				const String &fileName)
{
  LogSinkInterface* sink = new TableLogSink (LogFilter(LogMessage::DEBUGGING),
					     fileName);
  return LogSink (filter, CountedPtr<LogSinkInterface>(sink));
}

} //# NAMESPACE CASACORE - END

