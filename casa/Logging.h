//# Logging.h: Send, record, and filter informational messages
//# Copyright (C) 1996,1997,2004
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

#ifndef CASA_LOGGING_H
#define CASA_LOGGING_H

#include <casacore/casa/aips.h>

#include <casacore/casa/Logging/LogMessage.h>
#include <casacore/casa/Logging/LogOrigin.h>
#include <casacore/casa/Logging/LogSink.h>
#include <casacore/casa/Logging/LogFilter.h>
//#include <aips/LogTables/TableLogSink.h>
#include <casacore/casa/Logging/LogIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module> 
//
// <summary> 
// Send, record, and filter informational messages.
// </summary>

// <prerequisite>
//   <li> General Casacore utility classes, such as String.
// </prerequisite>

// <reviewed reviewer="wbrouw" date="1996/08/21" demos="dLogging.cc" tests="tLogging.cc">
// </reviewed>

// <etymology>
// Logging, as in "log book", or "processing log."
// </etymology>
//
// <synopsis> 
// The classes in the logging module have two essential purposes:
// <ol>
// <li> To attach processing logs to datasets to retain a permanent history of
//      informational messages that describe how the dataset arrived at its
//      present state; and
// <li> To inform the user about the progress and decisions made by various
//      algorithms, such as those used in the Measures system.
// </ol>
//
// The two fundamental classes in the Logging module are the
// <linkto class="LogMessage">LogMessage</linkto> and
// <linkto class="LogSink">LogSink</linkto> classes.
// However, the class which is most of interest to application programmers is
// the <linkto class=LogIO>LogIO</linkto> class since it forms the usual
// interface to logging.
//
// A <src>LogMessage</src> consists of an informational message tagged with the
// time, a priority (<src>DEBUGGING, NORMAL,</src>, <src>WARN</src>, or
// <src>SEVERE</src>) and the source code location of the origin of the message
// (for use in debugging primarily).
//
// The <src>LogSink</src> is used to send the <src>LogMessage</src> to its
// destinations. Usually the message will be sent to both a global sink,
// intended for user information (e.g., a GUI window), and to a sink associated
// with the dataset(s) which are being modified. In practice, the application
// programmer does not need to worry about where the global messages go, that
// policy is implemented by the Tasking system. In practice, global messages
// will be sent to Glish, where they will appear in a GUI, unless Glish is
// not available, in which case SEVERE messsages (only) will be sent to
// stdout. However, the principle is that "ordinary" application programmers
// shouldn't worry about the details of the global sink - they should just
// use it.
//
// A <linkto class="LogFilter">LogFilter</linkto> can be used to filter
// messages (based on priority only at the moment) before they are sent to
// their appropriate sink(s).
//
// The <linkto class="LogIO">LogIO</linkto> class puts an ostream like
// interface on top of the loggins system. Basically, the application
// programmer just has to create messages using and <src>os << items</src>
// type interface.
//
// The first issue that the application programmer has to decide is whether
// to use logging at all, or if instead he should put his messages into a
// <linkto class="String">String</linkto> or an <src>ostream</src>. It is
// never wrong to use log messages, however it is reasonable for low level
// classes to use <src>String</src>s or <src>ostream</src>s, since the
// caller of that class will have the opportunity to put the text in a log
// message if he decides that's the most appropriate thing to do. Note that
// it is always wrong to write directly to <src>cout</src> or
// <src>cerr</src> (other
// then for debugging) - use an <src>ostream</src>, so the caller can replace
// it with, for example, an <src>ostringstream</src>.
//
// Once you decide to use logging, the application programmer only has
// to decide at every location he wants to log:
// <ol>
//     <li> What content do you want the message to have; and
//     <li> what priority does the message have (DEBUGGING, NORMAL, WARN, SEVERE).
// </ol>
// Schematically, application programmers would use the logging system as
// follows:
// <srcBlock>
// #include <casacore/casa/Logging.h>
// ...
// void MyClass:myFunction(LogIO &os)
// {
//    os << LogIO::NORMAL << LogOrigin("MyClass", "myFunction()", WHERE);   // 1
//    ...
//    os << WHERE << "An informative message") << LogIO::POST;              // 2
//    if (error()) {
//        os << WHERE << LogIO::SEVERE << "Error!" << LogIO::POST;          // 3
//        sink.post(msg);
//        ...
//    }
// }
// </srcBlock>
// <ol>
//    <li> Set up the location where log messages come from. WHERE will expand
//         into the file name and line number (useful for debugging). Set the
//         priority to NORMAL (this is the default, but you don't know what
//         the state of <src>os</src> is when it is passed in to this function).
//    <li> Set the message and the new line number (optional but encouraged) and
//         post it.
//    <li> Change the priority to SEVERE and post an error message. 
// </ol>
//
// When a dataset is created from several other datasets, their input
// "histories" should be merged if possible. This can be done if the
// local log sink is in fact a Table. The way you do this is by interrogating
// the local sink to find out if it is in fact a TableLogSink. If it is, you
// can use a concatenate method of TableLogSink. Schematically this would be
// implemented as follows in some DataSet class that has a logSink method that
// returns a LogIO reference:
// <srcBlock>
// void merge(DataSet &out, const DataSet &in1, const DataSet &in2) {
//     ... copy the data from in1 and in2 to out
//     if (out.logSink().localSink().isTableLogSink()) { // can write to out
//         if (in1.logSink().localSink().isTableLogSink()) {
//             out.logSink().localSink().castToTableLogSink().concatenate(
//                  in1.logSink().localSink().castToTableLogSink());
//     }
//     if (... the same for in2 ...)
// }
// </srcBlock>
// Of course, DataSet might provide some convenience function for merging
// histories. However the point is that given a sink, you can safely determing
// whether or not it is in fact a TableLogSink, and if it is you can call
// its concatenate function, which takes another TableLogSink.
// </synopsis> 
//
// <example>
// The following example code is checked into the system as
// <src>dLogging.cc</src>. It is found in the Logging test directory.
//
// <srcblock>
// class DataClass
// {
// public:
//   DataClass(const IPosition &shape, const LogSink &sink);  // 1
//   void set(Int toWhat);                                    // 2
//   LogIO   &sink() return os_p;}                            // 3
//   Array<Int> &data() {return data_p;}                      // 4
//   const Array<Int> &data() const {return data_p;}          // 5
// private:                                                   // 6
//   Vector<Int> data_p;                                      // 7
//   LogSink log_sink_p;                                      // 8
//   LogIO os_p;                                              // 9
// };
// </srcblock>
//
// This toy class is meant to represent one which is to have "attached" logging
// information. Generally, these classes would be fairly high level
// astronomical classes, e.g. <src>Image</src>, not <src>Array</src>. Note that
// only operations which change the data should be logged in the internal log.
// Operations which only read the data should be logged either globally, or in
// the class that is taking the results and modifying its own data.
//
// <dl compact>
// <dt>1.
// <dd> Depending on the application, the LogSink to be used might
// either be handed in to the class, as is the case here, or it might
// be created by the class.  For example, a <src>MeasurementSet</src>
// will have a processing log table with a known name.
// <dt> 2.
// <dd> A sample function that changes the state of the class. Here,
//      it just sets all the elements of the internal array to 
//      <src>toWhat</src>.
// <dt> 3.
// <dd> Return the LogIO that is used by this object. A member function like this
//      should be provided for use by global functions which manipulate the object.
//      Note that it is non-const --- the internal sink should be modified only
//      by functions which CHANGE the object, otherwise the global sink should be
//      used.
// <dt> 4.
// <dd> Return the internal data. Arguably this should be logged at at least
//      DEBUGGING level.
// <dt> 5.
// <dd> Non-const version of the above. Note that it should not be logged since
//      the state cannot be changed with this function.
// <dt> 7.
// <dd> The internal data member.
// <dt> 8.
// <dd> The location to which log mesages are sent.
// <dt> 9.
// <dd> The LogIO object that will be the actual interface to the logging 
//      system.
// </dl>
//
// <srcblock>
// DataClass::DataClass(const IPosition &shape, const LogSink &sink)
//   : log_sink_p(sink), os_p(log_sink_p)                                     // 1
// {                                                                          // 2
//   os_p << LogOrigin("DataClass",                                           // 3
//                "DataClass(const IPosition &shape, const LogSink &sink)");  // 4
//                                                                            // 5
//   if (shape.nelements() != 1) {                                            // 6
//     os_p << LogIO::SEVERE << WHERE <<                                      // 7
//       "Illegal Shape! Must be one dimensional." << LogIO::EXCEPTION;       // 8
//   }                                                                        // 9
//                                                                            // 10
//   data_p.resize(shape(0));                                                 // 11
//   os_p << "Inital shape " << shape << "and value 2" <<                     // 12
//       LogIO::NORMAL << LogIO::POST;                                        // 13
//                                                                            // 14
//   set(2);                                                                  // 15
// }
// </srcblock>
// <dl compact>
// <dt> 1.
// <dd> The private <src>LogSink</src> data member is initialized with one that 
//      the caller provides. Note that LogSink uses reference semantics, so
//      that if another "copy" of the sink is made then all the log messages
//      will go to the same place. For example:
//      <srcblock>
//          LogSink a("mylogtable");
//          LogSink b(a);
//          LogSink c;
//          c = a;
//          ...
//          c.post(...);  // ends up in mylogtable
//          ...
//          b.post(...);  // as does this
//      </srcblock>
//      This can be useful if several classes might be modifying the same data, 
//      or if a data is spread over several objects.
//
//      Also, os_p is intialized from the sink.
// <dt> 3.
// <dd> For a member function, the first argument to LogOrigin is the class name.
// <dt> 4.
// <dd> The next argument is the function name. You should use the full name with
//      arguments so that you can use the argument name in your messages. Leave
//      off the return type. Cutting and pasting is easier than typing!
// <dt> 7.
// <dd> WHERE is a predefined macro that gives the file name and line number.
// <dt> 8.
// <dd> Create a SEVERE level error message, post it and throw an exception.
// <dt> 11.
// <dd> This will post the message locally and globally, and then throw
// an exception. Another possibility would be to call
// <src>postGloballyThenThrow()</src> if you only wanted to send the
// message to the global sink (for example, if the object is hopelessly
// corrupted, or if the problem occurs in a read-only operation). The
// thrown exception is an <src>AipsError</src>. The
// <src>post*Throw()</src> functions will always set the priority to
// <src>SEVERE</src>, however it doesn't hurt to show your intentions
// <dt> 12.
// <dd> Create and send a NORMAL priority message.
// <dt> 15.
// <dd> Call <src>set()</src> from the constructor to give the data values 
//      an initial value.
// </dl>
//
// <srcblock>
// void DataClass::set(Int toWhat)
// {
//   os_p << LogIO::NORMAL << LogOrigin("DataClass", "set(Int toWhat)");      // 1
//   os_p << "Setting data values to " << toWhat << WHERE << LogIO::POST;     // 2
//   uInt n = data_p.nelements();                                             // 3
//   for (uInt i=0; i < n; i++) {                                             // 4
// #ifdef AIPS_DEBUG                                                          // 5
//     os_p << LogIO::DEBUGGING << WHERE <<                                   // 6
//       "Setting element  " << i << " to " << toWhat << LogIO::POST;         // 7
// #endif                                                                     // 8
//     data_p(i) = toWhat;                                                    // 9
//   }
// }
// </srcblock>
//
// <dl compact>
// <dt> 2.
// <dd> This and the previous line set up and send a normal priority log message
//      much as we did previously.
// <dt> 7.
// <dd> LogMessages are relatively expensive to produces and consume. Use of 
//      them in a very tight loop should either be <src>ifdef</src>'d out as 
//      in this example, or like:
//      <srcblock>
//      if (aips_debug_on) {
//      ... set up and send log message ...
//      }
//      </srcblock>
//      The advantage of this code is that it's always available - so, for 
//      example, you can turn it on and off by manipulating the global variable
//      <src>aips_debug_on</src>. However very tight loops cannot even afford 
//      this extra <src>if</src>, and should prefer the <src>ifdef</src>.
//
//      Normally the <src>DEBUGGING</src> messages are "boring but low-volume",
//      and you should just send them normally.
// </dl>
//
// <srcblock>
// void square(DataClass &object)
// {
//   object.sink() << LogIO::NORMAL << WHERE <<                              // 1
//     LogOrigin("square(DataClass &object)") << "Squaring data elements"    // 2
// 	<< LogIO::POST;                                                    // 3
//   object.data() *= object.data();                                         // 4
// }
// </srcblock>
//
// This function shows how a global function that modifies an object can send
// log messages to that objects <src>LogSink</src> using a function of that
// object to get access to its sink.
//
// <srcblock>
// float sum(const DataClass &object)
// {
//   LogIO global(LogOrigin("sum(const DataClass &object)"));        // 1
//   float theSum = sum(object.data());                              // 2
//   global << WHERE << "Sum of object is: " << theSum;              // 3 
//   return theSum;                                                  // 4
// }
// </srcblock>
// This is an example of a global function that only reads -- does not change --
// an object.
// <dl>
// <dt> 3.
// <dd> Since we are not changing the data object, we only post the message
//      globally, we don't write it to the data object's log sink. The caller
//      of <src>sum()</src> might log the message somewhere else if the return
//      value is used to modify data in some other object. Instead we send it
//      to the global sink. Here we don't POST the message ourselves, we rely
//      on the LogIO destructor to do it for us.
// </dl>
//
// <srcblock>
// int main()
// {
//     LogSink::globalSink().filter(LogMessage::DEBUGGING);           // 1
//     LogSink logger(LogMessage::NORMAL, "dLogging_messages_tmp");   // 2
//                                                                    // 3
//     IPosition legalShape(1, 10);                                   // 4
//     DataClass dc(legalShape, logger);                              // 5
//                                                                    // 6
//     square(dc);                                                    // 7
//                                                                    // 8
//     Float total = sum(dc);                                         // 9
//                                                                    // 10
//     return 0;                                                      // 11
// }
// </srcblock>
// <dl compact>
// <dt> 1.
// <dd> Change the priority of messages to display on the global sink's 
//      filter to
//      <src>DEBUGGING</src> from the default <src>NORMAL</src>. The default
//      global sink logs to cerr. The global sink can be replaced with
//      <src>LogSink::globalSink()</src>.
// <dt> 2.
// <dd> Create the sink that we are going to use. This constructor will use
//      a <linkto class="Table">Table</linkto>. If the table doesn't exist
//      it will be created. If it does exist, new log messages will be appended
//      to the end.
// <dt> 5.
// <dd> Create an object with the provided sink. The alternative strategy, which
//      will be used with classes like
//      <linkto class="MeasurementSet">MeasurementSet</linkto> is for the object
//      to make it's own <src>LogSink</src> if it knows where it wants its 
//      messages to go.
// <dt> 7.
// <dd> Changes the data - log messages go to its local sink.
// <dt> 9.
// <dd> Reads the data - log messages go only to the global sink.
// </dl>

// </example>
//
// <motivation>
// <ol>
// <li> Attaching informational messages to datasets to describe their processing
// history.
// <li> Informational messages to inform the user about the progress and 
//      parameters of algorithms - for example those used for reference frame
//      conversions in the Measures module.
// </ol>
// </motivation>

// <todo asof="1997/01/19">
//   <li> More filtering options?
// </todo>

// </module>


} //# NAMESPACE CASACORE - END

#endif
