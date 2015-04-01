//# LogOrigin.h: The source code location of the originator of a LogMessageLogOrig
//# Copyright (C) 1996,1999,2000,2001
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

#ifndef CASA_LOGORIGIN_H
#define CASA_LOGORIGIN_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/System/ObjectID.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

struct SourceLocation;

// <summary> 
// LogOrigin: The source code location of the originator of a LogMessage.
// </summary>

// <use visibility=export>

// <reviewed reviewer="wbrouw" date="1996/08/21" tests="tLogging.cc" demos="dLogging.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="ObjectID">ObjectID</linkto> if you are interested in
//        logging from Distributed Objects (don't worry if you don't know what
//        that means - in that case ignore it!).
// </prerequisite>
//
// <etymology>
// Log[message] Origin[ation point].
// </etymology>
//
// <synopsis> 
// The <src>LogOriging</src> class is used to record the location at which a
// <linkto class="LogMessage">LogMessage</linkto> originates. It consists of:
// <ul>
// <li> A class name, which is blank in the case of a global function.
// <li> A function name - global or member. You should "cut and paste" not only
//      the function name, but also the arguments (but not the return type) to
//      ensure that the function name is unique in the face of overloading.
// <li> A source file name, usually filled in with the <src>WHERE</src>
//      predefined macro.
// <li> A line number, usually filled in with the <src>__LINE__</src> or
//      <src>WHERE</src> macros.
// <li> An <linkto class="ObjectID">ObjectID</linkto> if the log message comes
//      from a distributed object (if you don't know what this means, 
//      you don't need to worry about it).
// <li> Eventually we may want to canonicalize the path in the filename.
// </ul>
// </synopsis> 
//
// <example>

// </example>
// See the examples for <linkto class="LogMessage">LogMessage</linkto> and in
// <linkto file="Logging.h">Logging.h</linkto>.
//
// <motivation>
// It can be very useful for debugging if you know where a message is coming
// from.
// </motivation>
//
// <todo asof="1996/07/23">
//   <li> Nothing known
// </todo>

class LogOrigin 
{
public:

    // The default constructor sets a null class name, function name, object id,
    // source file name, and sets the line number to zero.
    LogOrigin();

    // Use this constructor if the log message origination is from a
    // global function. Normally <src>where</src> is provided using
    // the <src>WHERE</src> macro.
    LogOrigin(const String &globalFunctionName, const SourceLocation *where = 0);
  
    // Use this constructor if the log message origination is from a
    // class member function. Normally <src>where</src> is provided using
    // the <src>WHERE</src> macro.
    LogOrigin(const String &className, const String &memberFuncName,
	      const SourceLocation *where = 0);

    // Use this constructor if the log message origination is from a
    // distributed object (don't worry if you don't know what this
    // means). Normally <src>where</src> is provided using the
    // <src>WHERE</src> macro.
    LogOrigin(const String &className, const String &memberFuncName,
	      const ObjectID &id, const SourceLocation *where = 0);

    // Make <src>this</src> LogOrigin a copy of <src>other</src>.
    // <group>
    LogOrigin(const LogOrigin &other);
    LogOrigin &operator=(const LogOrigin &other);
    // </group>

    ~LogOrigin();

    // Get or set the corresponding element of the source location. Note that
    // the "set" functions can be strung together:
    // <srcBlock>
    // LogOrigin where;
    // ...
    // where.function("anotherFunc").line(__LINE__);
    // </srcBlock>
    // <group>
    const String &taskName() const;
    LogOrigin &taskName(const String &funcName);

    const String &functionName() const;
    LogOrigin &functionName(const String &funcName);

    const String &className() const;
    LogOrigin &className(const String &className);

    const ObjectID &objectID() const;
    LogOrigin &objectID(const ObjectID &id);

    uInt line() const;
    LogOrigin &line(uInt which);

    const String &fileName() const;
    LogOrigin &fileName(const String &fileName);
    // </group>

    // Set the file name and line number at the same time. Normally
    // <src>where</src> will be defined with the <src>WHERE</src> macro.
    LogOrigin &sourceLocation(const SourceLocation *where);

    // Returns <src>class\::function</src> for a member function, or
    // <src>\::function</src> for a global function.
    String fullName() const;

    // Turn the entire origin into a String.
    String toString() const;

    // Turns the entire origin except for the ObjectID into a String. The
    // ObjectID can be turned into a string vie ObjectID::toString.
    String location() const;

    // Return true if the line number and file name are not set.
    Bool isUnset() const;

private:
    String task_p;
    String function_p;
    String class_p;
    ObjectID id_p;
    uInt line_p;
    String file_p;
    String node_p;

    // Return a String with the MPI rank
    String getNode();

    // Provide common implementation for copy constructor and
    // assignment operator.
    void copy_other(const LogOrigin &other);
};

// <summary>
// Write a LogOrigin to an ostream.
// </summary>
// Write a LogOrigin as a string to an ostream. Merely calls
// <src>LogOrigin::toString()</src>
// <group name=LogOrigin_ostream>  
ostream &operator<<(ostream &os, const LogOrigin &origin);
// </group>

// <summary>
// Helper struct to get the source line.
// </summary>
// The user should only use the <src>WHERE</src> macro.
// <group name=SourceLocation>
struct SourceLocation
{
    const char *fileName;
    Int lineNumber;
    static const SourceLocation *canonicalize(const char *file, Int line);
};

#define WHERE casacore::SourceLocation::canonicalize(__FILE__, __LINE__)

// </group>


} //# NAMESPACE CASACORE - END

#endif
