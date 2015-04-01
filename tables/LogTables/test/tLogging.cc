//# tLogging.cc: Test the logging classes
//# Copyright (C) 1996,1997,1998,2000,2001,2003
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


#include <casacore/casa/Logging/LogFilter.h>
#include <casacore/tables/LogTables/LogFilterTaql.h>
#include <casacore/casa/Logging/LogMessage.h>
#include <casacore/casa/Logging/LogOrigin.h>
#include <casacore/casa/Logging/LogSink.h>
#include <casacore/casa/Logging/NullLogSink.h>
#include <casacore/casa/Logging/MemoryLogSink.h>
#include <casacore/casa/Logging/StreamLogSink.h>
#include <casacore/tables/LogTables/TableLogSink.h>
#include <casacore/casa/Logging/LogIO.h>

#include <casacore/tables/Tables.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/OS/Directory.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/sstream.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
void testLogFilter()
{
    // LogFilter(LogMessage::Priority lowest=LogMessage::DEBUGGING);
    LogFilter low(LogMessage::DEBUGGING);
    LogFilter normal(LogMessage::NORMAL);
    LogFilter warn(LogMessage::WARN);
    LogFilter severe(LogMessage::SEVERE);
    LogFilter tmp;
    LogMessage message;

    // LogMessage::Priority lowestPriority() const;
    AlwaysAssertExit(low.lowestPriority() == LogMessage::DEBUGGING);
    AlwaysAssertExit(normal.lowestPriority() == LogMessage::NORMAL);
    AlwaysAssertExit(severe.lowestPriority() == LogMessage::SEVERE);
    AlwaysAssertExit(warn.lowestPriority() == LogMessage::WARN);
    AlwaysAssertExit(tmp.lowestPriority() == LogMessage::NORMAL);

    // LogFilter(const LogFilter &other);
    // LogFilter &operator=(const LogFilter &other);
    tmp = severe;
    LogFilter copy(severe);
    AlwaysAssertExit((tmp.lowestPriority() == copy.lowestPriority()) &&
		     (tmp.lowestPriority() == LogMessage::SEVERE));
    
    // Bool pass(const LogMessage &message) const;
    message.priority(LogMessage::DEBUGGING);
    AlwaysAssertExit(low.pass(message) && !normal.pass(message) &&
		     !warn.pass(message) && !severe.pass(message));
    message.priority(LogMessage::NORMAL);
    AlwaysAssertExit(low.pass(message) && normal.pass(message) &&
		     !warn.pass(message) && !severe.pass(message));
    message.priority(LogMessage::SEVERE);
    AlwaysAssertExit(low.pass(message) && normal.pass(message) &&
		     warn.pass(message) && severe.pass(message));
    message.priority(LogMessage::WARN);
    AlwaysAssertExit(low.pass(message) && normal.pass(message) &&
		     warn.pass(message) && !severe.pass(message));

    // LogFilter(const String& expr);
    LogFilterTaql expr1
               ("LOCATION=='::abc' && !(PRIORITY in ['INFO','DEBUGGING'])");
    LogFilterTaql expr2
               ("LOCATION=='::abc' && PRIORITY in ['INFO','DEBUGGING']");
    LogFilterTaql expr(expr2);
    expr = expr1;
    AlwaysAssertExit (!expr.pass(LogMessage(String("abc"),
					    LogMessage::NORMAL)));
    AlwaysAssertExit (!expr.pass(LogMessage(String("abc"),
					    LogMessage::DEBUGGING)));
    AlwaysAssertExit (expr.pass(LogMessage(String("abc"),
					   LogMessage::WARN)));
    AlwaysAssertExit (expr.pass(LogMessage(String("abc"),
					   LogMessage::SEVERE)));
    AlwaysAssertExit (!expr.pass(LogMessage(String("abcd"),
					    LogMessage::WARN)));
    AlwaysAssertExit (!expr.pass(LogMessage(String("abcd"),
					    LogMessage::SEVERE)));
    // ~LogFilter() at end of block
}

void testLogMessage()
{
    // LogMessage(Priority=NORMAL); 
    // Priority priority() const;
    // const String &message() const;
    {
        LogMessage m1;
	AlwaysAssertExit(m1.priority() == LogMessage::NORMAL);
        LogMessage m2(LogMessage::SEVERE);
	AlwaysAssertExit(m2.priority() == LogMessage::SEVERE);
	AlwaysAssertExit(m2.message() == "");
    }
	
    // LogMessage(const LogOrigin &sourceLocation, Priority=NORMAL);
    // LogMessage(const String &message, const LogOrigin &sourceLocation, 
    //            Priority=NORMAL);
    // const LogOrigin &origin() const;
    // LogMessage(const LogMessage &other);
    // LogMessage &operator=(const LogMessage &other);
    {
        LogMessage m1(LogOrigin("test"), LogMessage::SEVERE);
	AlwaysAssertExit(m1.priority() == LogMessage::SEVERE &&
			 m1.message()=="");
	AlwaysAssertExit(m1.origin().functionName() == "test");
	LogMessage m2("message", LogOrigin("test"), LogMessage::SEVERE);
	AlwaysAssertExit(m2.priority() == LogMessage::SEVERE &&
		     m2.message()=="message" &&
		     m2.origin().functionName() == "test");
	LogMessage m3(m2);
	AlwaysAssertExit(m3.priority() == m2.priority() &&
		     m3.message() == m2.message() &&
		     m3.origin().functionName() == m2.origin().functionName());
	LogMessage m4; m4 = m2;
	AlwaysAssertExit(m4.priority() == m2.priority() &&
		     m4.message() == m2.message() &&
		     m4.origin().functionName() == m2.origin().functionName());
    }


    // LogMessage &message(const String &message, Bool keepLastTime = False);
    // const Time &messageTime() const;
    // uInt line() const;
    // LogMessage &line(uInt which);
    // LogMessage &origin(const LogOrigin &origin);
    // LogMessage &priority(Priority which);
    // static const String &toString(Priority which);
    // String toString() const;
    // global operator<<
    {
        LogMessage m;
	m.message("hello");
	AlwaysAssertExit(m.message() == "hello");
	Time now;
	// 100 msec should be bigger than clock tick?
	AlwaysAssertExit(now - m.messageTime() >= 0 &&
		     now - m.messageTime() < 0.1); 
	m.line(100);
	AlwaysAssertExit(m.line() == 100);
	m.origin(LogOrigin("test"));
	AlwaysAssertExit(m.origin().functionName() == "test");
	m.priority(LogMessage::SEVERE);
	AlwaysAssertExit(m.priority() == LogMessage::SEVERE);
	AlwaysAssertExit(LogMessage::toString(m.priority()) == "SEVERE");
	String message = m.toString();
	AlwaysAssertExit(message.contains(String("hello")) &&
			 message.contains(String("test")) &&
			 message.contains(String("SEVERE")));
	ostringstream os;
	os << m;
	String cached(os);
	cached = cached(0, cached.length()-1); // get rid of trailing nl
	AlwaysAssertExit(cached == message);
    }

    // ~LogMessage(); - implicit at end of blocks
}

void testLogOrigin()
{
    // LogOrigin();
    // const String &functionName() const;
    // const String &className() const;
    // const ObjectID &objectID() const;
    // uInt line() const;
    // LogOrigin &fileName(const String &fileName);
    {
        LogOrigin empty;
	AlwaysAssertExit(empty.functionName() == "" &&
			 empty.className() == "" &&
			 empty.objectID().isNull() &&
			 empty.line() == 0 &&
			 empty.fileName() == "");
    }

    // Takes the place of WHERE, which would be used in user code
    SourceLocation location; location.fileName = "file"; 
    location.lineNumber = 10;

    // LogOrigin(const String &globalFunctionName, const char *fileName=0,
    //           uInt lineNumber = 0);
    {
        LogOrigin global("global", &location);
	AlwaysAssertExit(global.functionName() == "global" &&
			 global.className() == "" &&
			 global.objectID().isNull() &&
			 global.line() == 10 &&
			 global.fileName() == "file");
    }

    // LogOrigin(const String &className, const String &memberFuncName,
    // 	      const char *fileName=0, uInt lineNumber = 0);
    {
        LogOrigin member("class", "member", &location);
	AlwaysAssertExit(member.functionName() == "member" &&
			 member.className() == "class" &&
			 member.objectID().isNull() &&
			 member.line() == 10 &&
			 member.fileName() == "file");
    }

    // LogOrigin(const String &className, const String &memberFuncName,
    //         const ObjectID &id, const char *fileName=0, uInt lineNumber = 0);
    // LogOrigin(const LogOrigin &other);
    // LogOrigin &operator=(const LogOrigin &other);
    // String fullName() const;
    // LogOrigin &functionName(const String &funcName);
    // LogOrigin &className(const String &className);
    // LogOrigin &objectID(const ObjectID &id);
    // LogOrigin &line(uInt which);
    // const String &fileName() const;
    // String toString() const;
    // global ostream &operator<<(ostream &os, const LogOrigin &origin);
    {
        ObjectID id;
	LogOrigin distributed("class", "member", id, &location);
	AlwaysAssertExit(distributed.functionName() == "member" &&
			 distributed.className() == "class" &&
			 distributed.objectID() == id &&
			 distributed.line() == 10 &&
			 distributed.fileName() == "file" &&
			 distributed.fullName() == "class::member");
	LogOrigin t1(distributed), t2;
	t2 = distributed;
	AlwaysAssertExit(distributed.functionName() == t1.functionName() &&
			 distributed.className() == t1.className() &&
			 distributed.objectID() == t1.objectID() &&
			 distributed.line() == t1.line() &&
			 distributed.fileName() == t1.fileName());
	AlwaysAssertExit(distributed.functionName() == t2.functionName() &&
			 distributed.className() == t2.className() &&
			 distributed.objectID() == t2.objectID() &&
			 distributed.line() == t2.line() &&
			 distributed.fileName() == t2.fileName());
	LogOrigin t3;
	t3.functionName(t1.functionName()).className(t1.className()).
	  objectID(t1.objectID()).line(t1.line()).fileName(t1.fileName());
	AlwaysAssertExit(distributed.functionName() == t3.functionName() &&
			 distributed.className() == t3.className() &&
			 distributed.objectID() == t3.objectID() &&
			 distributed.line() == t3.line() &&
			 distributed.fileName() == t3.fileName());
	String s = t3.toString();
	AlwaysAssertExit(s.contains(String("class")) &&
			 s.contains(String("member")) &&
			 s.contains(String("file")));
	ostringstream buffer;
	buffer << t3;
	String s2(buffer);
	AlwaysAssertExit(s2 == s);
    }

    // ~LogOrigin(); - implicit at end of blocks
}

const char *tableNames[] = { "tLogging_tmp", "tLogging_tmp2", 0};

void cleanup()
{
    int i = 0;
    while (tableNames[i]) {
	Directory deleteme(tableNames[i]);
	if (deleteme.exists()) {
	    deleteme.removeRecursive();
	}
	i++;
    }
}

void testLogSink()
{
    cleanup();

    // LogSink(const LogFilter &filter);
    LogSink sink1(LogMessage::SEVERE);
    ostringstream os;
    // LogSink(const LogFilter &filter, ostream *os);
    LogSink sink2(LogMessage::SEVERE, &os);
    // LogSink(const LogFilter &filter, const String &fileName);
    LogSink sink3 = TableLogSink::makeSink (LogMessage::SEVERE, tableNames[0]);
    LogSinkInterface *newGlobal = new TableLogSink(LogMessage::SEVERE, 
    						   tableNames[1]);
    LogSinkInterface *copy = newGlobal;
    AlwaysAssertExit(newGlobal);
    // static void globalSink(LogSinkInterface *&fromNew);
    LogSink::globalSink(newGlobal);
    AlwaysAssertExit(!newGlobal);
    // static LogSinkInterface &globalSink();
    AlwaysAssertExit(copy = &LogSink::globalSink());
    LogMessage message;
    message.message("test");
    // Bool post(const LogMessage &message);
    AlwaysAssertExit(! sink1.post(message) &&
		     ! sink2.post(message) &&
		     ! sink3.post(message));
    message.priority(LogMessage::SEVERE);
    AlwaysAssertExit(sink1.post(message) &&
		     sink2.post(message) &&
		     sink3.post(message));
	
    String fromos(os);
    AlwaysAssertExit(fromos.contains("test"));
    
    Table logTable(tableNames[0]);
    Table logTable2(tableNames[1]);
    ScalarColumn<String> messageColumn(logTable,
			 TableLogSink::columnName(TableLogSink::MESSAGE));
    ScalarColumn<String> messageColumn2(logTable2,
			  TableLogSink::columnName(TableLogSink::MESSAGE));
    AlwaysAssertExit(messageColumn(0) == "test");
    // LogSink(const LogSink &other);
    LogSink sink4(sink3);
    // LogSink &operator=(const LogSink &other);
    LogSink sink5(LogMessage::SEVERE); sink5 = sink3;
    sink4.post(message);
    sink5.post(message);
    AlwaysAssertExit(logTable.nrow() == 3 &&
		     messageColumn(1) == "test" &&
		     messageColumn(2) == "test");
    AlwaysAssertExit(logTable2.nrow() == 5 &&
		     messageColumn2(4) == "test");
    // static Bool postGlobally(const LogMessage &message);
    sink5.postGlobally(message);
    AlwaysAssertExit(logTable2.nrow() == 6);
    // virtual Bool postLocally(const LogMessage &message);
    sink5.postLocally(message);
    AlwaysAssertExit(logTable.nrow() == 4);
    // const LogSinkInterface &localSink() const;
    AlwaysAssertExit(&sink3.localSink() == &sink4.localSink());
    // virtual const LogFilter &filter() const;
    AlwaysAssertExit(dynamic_cast<const LogFilter&>(sink3.filter()).
		     lowestPriority() == LogMessage::SEVERE);
    // virtual LogSinkInterface &filter(const LogFilter &filter);
    sink3.filter(LogFilter(LogMessage::NORMAL));
    AlwaysAssertExit(dynamic_cast<const LogFilter&>(sink3.filter()).
		     lowestPriority() == LogMessage::NORMAL);

    // void postThenThrow(const LogMessage &message);
    Bool caught = False;
    try {
        sink5.postThenThrow(message, AipsError());
    } catch (AipsError x) {
        caught = True;
	AlwaysAssertExit(x.getMesg().contains("test"));
	AlwaysAssertExit(logTable.nrow() == 5 && logTable2.nrow() == 7);
    } 
    AlwaysAssertExit(caught);

    // static void postGloballyThenThrow(const LogMessage &message);
    caught = False;
    try {
        sink5.postGloballyThenThrow(message);
    } catch (AipsError x) {
        caught = True;
	AlwaysAssertExit(x.getMesg().contains("test"));
	AlwaysAssertExit(logTable.nrow() == 5 && logTable2.nrow() == 8);
    } 
    AlwaysAssertExit(caught);

    // LogSink &localSink(LogSinkInterface *&fromNew);
    LogSinkInterface *newLocal = new NullLogSink(LogMessage::SEVERE);
    AlwaysAssertExit(newLocal);
    copy = newLocal;
    sink5.localSink(newLocal);
    AlwaysAssertExit(!newLocal);
    AlwaysAssertExit(copy == &sink5.localSink());
    AlwaysAssertExit(&sink5.localSink() != &sink4.localSink());

    // LogSink &localSink(LogSinkInterface *&fromNew);
    LogSinkInterface *newLocalm = new MemoryLogSink(LogMessage::SEVERE);
    AlwaysAssertExit(newLocalm);
    copy = newLocalm;
    sink5.localSink(newLocalm);
    AlwaysAssertExit(!newLocalm);
    AlwaysAssertExit(copy == &sink5.localSink());
    AlwaysAssertExit(&sink5.localSink() != &sink4.localSink());

    // ~LogSink(); - implicit at end of block
}

void testLogIO()
{
    {
	ostringstream ostr;
	LogSink sls(LogMessage::NORMAL, &ostr);
	//     LogIO(LogSink &sink);
	LogIO os(sls);
	//     ostream& output();
	//     void post();
	os << "This SHOULD post" << LogIO::POST;
	String s(ostr);
	AlwaysAssert(s.contains("SHOULD"), AipsError);
	//     ~LogIO();
    }
    {
	ostringstream ostr;
	LogSink sls(LogMessage::NORMAL, &ostr);
	//     LogIO(const LogOrigin &or, LogSink &sink);
	LogIO os(LogOrigin("HELLO"), sls);
	os << "This SHOULD post" << LogIO::POST;
	String s(ostr);
	AlwaysAssert(s.contains("HELLO"), AipsError);
    }
    {
	ostringstream ostr;
	LogSink sls(LogMessage::NORMAL, &ostr);
	LogIO os(sls);
	//     void priority(LogMessage::Priority which);
	os << LogIO::DEBUGGING << "This SHOULD NOT post" << LogIO::POST;
	String s(ostr);
	AlwaysAssert(s == "", AipsError);
    }
    {
	ostringstream ostr;
	LogSinkInterface *sls = new StreamLogSink(LogMessage::NORMAL, &ostr);
	LogSink::globalSink(sls);
	//     LogIO();
	LogIO os;
	os << "This SHOULD post" << LogIO::POST;
	String s(ostr);
	AlwaysAssert(s.contains("SHOULD"), AipsError);
	// Put back the null log sink since ostr is now frozen!
	sls = new NullLogSink;
	LogSink::globalSink(sls);
    }
    {
	ostringstream ostr;
	LogSink sls(LogMessage::NORMAL, &ostr);
	LogIO os(sls);
	Bool caught = False;
	try {
	    //     void postThenThrow();
	    os << "This SHOULD post" << LogIO::EXCEPTION;
	} catch (AipsError x) {
	    caught = True;
	} 
	AlwaysAssert(caught, AipsError);
	String s(ostr);
	AlwaysAssert(s.contains("SHOULD"), AipsError);
	//     ~LogIO();
    }
    {
	ostringstream ostr;
	LogSink sls(LogMessage::NORMAL, &ostr);
	LogIO os(sls);
	Bool caught = False;
	try {
	    //     void postThenThrow();
            os << "This SHOULD post";
            os.postThenThrow (DuplError("duplicate"));
	} catch (DuplError& x) {
	    caught = True;
	} 
	AlwaysAssert(caught, AipsError);
	String s(ostr);
	AlwaysAssert(s.contains("SHOULD"), AipsError);
	AlwaysAssert(s.contains("duplicate"), AipsError);
	//     ~LogIO();
    }
}

void testLogAny (LogSink& sink)
{
  LogIO lio(sink);
  lio << "test " << LogIO::WARN << "message" << LogIO::SEVERE
      << LogIO::POST;
  AlwaysAssertExit (sink.nelements() == 1);
  lio << "test2 message" << LogIO::POST;
  AlwaysAssertExit (sink.nelements() == 2);
  AlwaysAssertExit (lio.localSink().nelements() == 2);
  AlwaysAssertExit (sink.getMessage(0) == "test message");
  AlwaysAssertExit (sink.getPriority(0) == "SEVERE");
  AlwaysAssertExit (sink.getMessage(1) == "test2 message");
  AlwaysAssertExit (sink.getPriority(1) == "INFO");
  sink.localSink().writeLocally (sink.getTime(0), sink.getMessage(1),
				 sink.getPriority(0), sink.getLocation(0),
				 sink.getObjectID(0));
  AlwaysAssertExit (sink.nelements() == 3);
  AlwaysAssertExit (sink.getMessage(2) == "test2 message");
  AlwaysAssertExit (sink.getPriority(2) == "SEVERE");
  sink.clearLocally();
  AlwaysAssertExit (sink.nelements() == 0);
  lio << "test " << LogIO::WARN << "message" << LogIO::SEVERE
      << LogIO::POST;
  AlwaysAssertExit (sink.nelements() == 1);
  AlwaysAssertExit (sink.getMessage(0) == "test message");
  AlwaysAssertExit (sink.getPriority(0) == "SEVERE");
}

void testLogMemory()
{
  LogFilter tmp;
  LogSink sink(tmp, False);
  testLogAny (sink);
}

void testLogTable()
{
  LogFilter tmp;
  cleanup();
  LogSink sink = TableLogSink::makeSink (tmp, tableNames[0]);
  testLogAny (sink);
}

int main()
{
    try {
        testLogFilter();
	testLogMessage();
	testLogOrigin();
	// Also tests other classes derived from LogSinkInterface
	testLogSink();
	testLogIO();
	testLogMemory();
	testLogTable();
    } catch (AipsError x) {
        cout << "Caught an exception : " << x.getMesg() << endl;
	exit(1);
    } 

    cerr << "OK (nothing else should have been printed by this program)\n";
    return 0;
}
