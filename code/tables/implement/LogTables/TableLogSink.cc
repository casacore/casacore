//# TableLogSink.h: save log messages in an AIPS++ Table
//# Copyright (C) 1996,1997,1998
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

#include <aips/Arrays/Vector.h>
#include <aips/Logging/TableLogSink.h>

#include <aips/Tables/TableDesc.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/IncrementalStMan.h>
#include <aips/Tables/StManAipsIO.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/ArrColDesc.h>

#include <aips/Exceptions/Error.h>
#include <aips/Utilities/Assert.h>

TableLogSink::TableLogSink(const LogFilter &filter, const String &fileName)
  : LogSinkInterface(filter)
{
    LogMessage logMessage(
	      LogOrigin("TableLogSink", "TableLogSink", WHERE));

    if (Table::isWritable(fileName)) {
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
	logMessage.priority(LogMessage::DEBUGGING).line(__LINE__).message(
	  String("Creating ") + fileName);
	LogSink::postGlobally(logMessage);
        SetupNewTable setup(fileName, logTableDescription(), Table::New);
// 	// Bind all to the ISM. When we have a standard storage manager we
// 	// should bind the MESSAGE column to it.
// 	IncrementalStMan stman("ISM");
	StManAipsIO stman;   // Use AipsIO until bug gone from ISM
	setup.bindAll(stman);
	log_table_p = Table(setup);
	log_table_p.tableInfo() = TableInfo(TableInfo::LOG);
	log_table_p.tableInfo().
	  readmeAddLine("Repository for software-generated logging messages");
    }

    // Avoid double deletion by Cleanup.
    log_table_p.makePermanent();

    // Attach the columns
    time_p.attach(log_table_p, columnName(TIME));
    priority_p.attach(log_table_p, columnName(PRIORITY));
    message_p.attach(log_table_p, columnName(MESSAGE));
    location_p.attach(log_table_p, columnName(LOCATION));
    id_p.attach(log_table_p, columnName(OBJECT_ID));
    roTime_p.attach(log_table_p, columnName(TIME));
    roPriority_p.attach(log_table_p, columnName(PRIORITY));
    roMessage_p.attach(log_table_p, columnName(MESSAGE));
    roLocation_p.attach(log_table_p, columnName(LOCATION));
    roId_p.attach(log_table_p, columnName(OBJECT_ID));
}

TableLogSink::TableLogSink(const String &fileName)
  : LogSinkInterface(LogFilter())
{
    LogMessage logMessage(
	      LogOrigin("TableLogSink", "TableLogSink", WHERE));

    if (! Table::isReadable(fileName)) {
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

    // Avoid double deletion by Cleanup.
    log_table_p.makePermanent();

    // Attach the columns
    roTime_p.attach(log_table_p, columnName(TIME));
    roPriority_p.attach(log_table_p, columnName(PRIORITY));
    roMessage_p.attach(log_table_p, columnName(MESSAGE));
    roLocation_p.attach(log_table_p, columnName(LOCATION));
    roId_p.attach(log_table_p, columnName(OBJECT_ID));
}

TableLogSink::TableLogSink(const TableLogSink &other)
{
    copy_other(other);
}

TableLogSink &TableLogSink::operator=(const TableLogSink &other)
{
    if (this != &other) {
        copy_other(other);
    }
    return *this;
}

void TableLogSink::copy_other(const TableLogSink &other)
{
    log_table_p = other.log_table_p;
    time_p.reference(other.time_p);
    priority_p.reference(other.priority_p);
    message_p.reference(other.message_p);
    location_p.reference(other.location_p);
    id_p.reference(other.id_p);
    roTime_p.reference(other.roTime_p);
    roPriority_p.reference(other.roPriority_p);
    roMessage_p.reference(other.roMessage_p);
    roLocation_p.reference(other.roLocation_p);
    roId_p.reference(other.roId_p);

    // Avoid double deletion by Cleanup.
    log_table_p.makePermanent();
}

TableLogSink::~TableLogSink()
{
    flush();
}

void TableLogSink::reopenRW (const LogFilter& aFilter)
{
    log_table_p.reopenRW();
    filter (aFilter);
}

Bool TableLogSink::postLocally(const LogMessage &message)
{
    Bool posted = False;
    if (filter().pass(message)) {
        posted = True;
        // Adding a row at a time might be too inefficient?
        uInt n = log_table_p.nrow();
	log_table_p.addRow();
	time_p.put(n, message.messageTime().modifiedJulianDay()*24.0*3600.0);
	priority_p.put(n, LogMessage::toString(message.priority()));
	message_p.put(n, message.message());
	location_p.put(n, message.origin().location());
	String tmp;
	message.origin().objectID().toString(tmp);
	id_p.put(n, tmp);
    }
    return posted;
}

String TableLogSink::columnName(TableLogSink::Columns which)
{
    switch(which) {
    case TIME: {return "TIME";}
    case PRIORITY: {return "PRIORITY";}
    case MESSAGE: {return "MESSAGE";}
    case LOCATION: {return "LOCATION";}
    case OBJECT_ID: {return "OBJECT_ID";}
    default:
        AlwaysAssert(! "REACHED", AipsError);
    }
    return "TableLogSink::columnName - error: not reached";
}

TableDesc TableLogSink::logTableDescription()
{
    TableDesc desc;
    desc.comment() = "Log message table";

    desc.addColumn(ScalarColumnDesc<Double>(columnName(TIME),
					    "MJD in seconds"));
    desc.addColumn(ScalarColumnDesc<String>(columnName(PRIORITY)));
    desc.addColumn(ScalarColumnDesc<String>(columnName(MESSAGE)));
    desc.addColumn(ScalarColumnDesc<String>(columnName(LOCATION)));
    desc.addColumn(ScalarColumnDesc<String>(columnName(OBJECT_ID)));
    return desc;
}

void TableLogSink::flush()
{
    log_table_p.flush();
}

Bool TableLogSink::isTableLogSink() const
{
    return True;
}

void TableLogSink::concatenate(const TableLogSink &other)
{
    const uInt offset = table().nrow();
    const uInt n = other.table().nrow();
    table().addRow(n);

    for (uInt i=0; i<n; i++) {
	time().put(offset+i, other.roTime()(i));
	priority().put(offset+i, other.roPriority()(i));
	message().put(offset+i, other.roMessage()(i));
	location().put(offset+i, other.roLocation()(i));
	objectID().put(offset+i, other.roObjectID()(i));
    }
}

void TableLogSink::cleanup()
{
    this->TableLogSink::~TableLogSink();
}
