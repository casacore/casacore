//# TableLogSink.h: save log messages in an AIPS++ Table
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
	// Seems to be a problem with removing rows
	//	IncrementalStMan stman ("ISM");
	StManAipsIO stman;
	setup.bindAll(stman);
	log_table_p = Table(setup);
	log_table_p.tableInfo() = TableInfo(TableInfo::LOG);
	log_table_p.tableInfo().
	  readmeAddLine("Repository for software-generated logging messages");
    }

    // Attach the columns
    time_p.attach(log_table_p, columnName(TIME));
    priority_p.attach(log_table_p, columnName(PRIORITY));
    message_p.attach(log_table_p, columnName(MESSAGE));
    class_p.attach(log_table_p, columnName(CLASS));
    function_p.attach(log_table_p, columnName(FUNCTION));
    file_p.attach(log_table_p, columnName(FILE));
    line_p.attach(log_table_p, columnName(LINE));
    object_id_p.attach(log_table_p, columnName(OBJECT_ID));
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
    class_p.reference(other.class_p);
    function_p.reference(other.function_p);
    file_p.reference(other.file_p);
    line_p.reference(other.line_p);
    object_id_p.reference(other.object_id_p);
}

TableLogSink::~TableLogSink()
{
    flush();
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
	class_p.put(n, message.origin().className());
	function_p.put(n, message.origin().functionName());
	file_p.put(n, message.origin().fileName());
	uInt linenum = message.line();
	// Set invalid line numbers to -1 to make it clear it isn't set
	linenum > 0 ? line_p.put(n, message.line()) : line_p.put(n, -1);
	const Array<Int> &ai = message.origin().objectID().toVector();
	object_id_p.put(n, ai);
    }
    return posted;
}

String TableLogSink::columnName(TableLogSink::Columns which)
{
    switch(which) {
    case TIME: {return "TIME";}
    case PRIORITY: {return "PRIORITY";}
    case MESSAGE: {return "MESSAGE";}
    case CLASS: {return "CLASS";}
    case FUNCTION: {return "FUNCTION";}
    case FILE: {return "FILE";}
    case LINE: {return "LINE";}
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
    desc.addColumn(ScalarColumnDesc<String>(columnName(CLASS)));
    desc.addColumn(ScalarColumnDesc<String>(columnName(FUNCTION)));
    desc.addColumn(ScalarColumnDesc<String>(columnName(FILE)));
    desc.addColumn(ScalarColumnDesc<Int>(columnName(LINE)));
    desc.addColumn(ArrayColumnDesc<Int>(columnName(OBJECT_ID),
			"Sequence Number, PID, time (sec. from 1970), HostID",
				IPosition(1,4),
				ColumnDesc::Direct|ColumnDesc::FixedShape));
    return desc;
}

void TableLogSink::flush()
{
    log_table_p.flush();
}


void TableLogSink::cleanup()
{
    this->TableLogSink::~TableLogSink();
}
