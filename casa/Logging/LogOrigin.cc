//# LogOrigin.cc: The source code location of the originator of a LogMessage.
//# Copyright (C) 1996,1997,2001,2003
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/casa/Logging/LogOrigin.h>
#include <casacore/casa/OS/EnvVar.h>
#include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

LogOrigin::LogOrigin()
: node_p(getNode())
{}

LogOrigin::LogOrigin(const String &globalFunctionName)
  : function_p(globalFunctionName),
    node_p(getNode())
{}

LogOrigin::LogOrigin(const String &globalFunctionName, const SourceLocation &where)
  : function_p(globalFunctionName),
    line_p(where.lineNumber),
    file_p(where.fileName),
    node_p(getNode())
{}

LogOrigin::LogOrigin(const String &className, const String &memberFuncName)
  : function_p(memberFuncName),
    class_p(className),
    node_p(getNode())
{}

LogOrigin::LogOrigin(const String &className, const String &memberFuncName,
		     const SourceLocation &where)
  : function_p(memberFuncName),
    class_p(className),
    line_p(where.lineNumber),
    file_p(where.fileName),
    node_p(getNode())
{}

LogOrigin::LogOrigin(const String &className, const String &memberFuncName,
	  const ObjectID &id)
: task_p(""), function_p(memberFuncName), class_p(className),
  id_p(id),
  node_p(getNode())
{}

LogOrigin::LogOrigin(const String &className, const String &memberFuncName,
	  const ObjectID &id, const SourceLocation &where)
: task_p(""), function_p(memberFuncName), class_p(className), 
  id_p(id), 
  line_p(where.lineNumber),
  file_p(where.fileName),
  node_p(getNode())
{}

LogOrigin::LogOrigin(const LogOrigin &other) = default;

LogOrigin &LogOrigin::operator=(const LogOrigin &other) = default;

LogOrigin::~LogOrigin() = default;

const String &LogOrigin::taskName() const
{
    return task_p;
}

LogOrigin &LogOrigin::taskName(const String &funcName)
{
    task_p = funcName;
    return *this;
}


const String &LogOrigin::functionName() const
{
    return function_p;
}

LogOrigin &LogOrigin::functionName(const String &funcName)
{
    function_p = funcName;
    return *this;
}

const String &LogOrigin::className() const
{
    return class_p;
}

LogOrigin &LogOrigin::className(const String &cn)
{
    class_p = cn;
    return *this;
}

const ObjectID &LogOrigin::objectID() const
{
    return id_p;
}

LogOrigin &LogOrigin::objectID(const ObjectID &id)
{
    id_p = id;
    return *this;
}

uInt LogOrigin::line() const
{
    return line_p;
}

LogOrigin &LogOrigin::line(uInt which)
{
    line_p = which;
    return *this;
}

const String &LogOrigin::fileName() const
{
    return file_p;
}

LogOrigin &LogOrigin::fileName(const String &fn)
{
    file_p = fn;
    return *this;
}

LogOrigin &LogOrigin::sourceLocation(const SourceLocation *where)
{
    if (where) {
	line_p = where->lineNumber;
	if (file_p != where->fileName) {
	    file_p = where->fileName;
	}
    } else {
        line_p = 0;
	file_p = "";
    }
    return *this;
}

String LogOrigin::fullName() const
{
    String nameTag("");
    if(task_p.length())
	    nameTag = task_p + "::";
    nameTag += className() + "::" + functionName();
    if(node_p.length())
        nameTag +=  "::" + node_p;
    return nameTag;
}

String LogOrigin::location() const
{
    ostringstream os;
    String nullString;
    os << fullName();
    if (fileName() != nullString) {
        os << " (file " << fileName();
	if (line() > 0) {
	    os << ", line " << line();
	}
	os << ")";
    }
    return String(os);
}

String LogOrigin::toString() const
{
    String retval = location();
    if (! objectID().isNull()) {
	ostringstream os;
        os << " ObjectID=" << objectID();
	retval += String(os);
    }
    return retval;
}

Bool LogOrigin::isUnset() const
{
  return (function_p == "" && class_p == "" && id_p.isNull() && 
		line_p == 0 && file_p == "" && task_p == "");
}

ostream &operator<<(ostream &os, const LogOrigin &origin)
{
	os << origin.toString();
    return os;
}

SourceLocation SourceLocation::canonicalize(const char *file, Int line)
{
  SourceLocation permanent;
  permanent.fileName = file;
  permanent.lineNumber = line;
  return permanent;
}

// Get the OpenMPI rank from the current process.
// This assumes that the MPI library implementation is openMPI.
// If a program is started without mpirun, LogOrigin will print nothing
// for node_p. If it is started with mpirun, each instance
// running on an MPI server will print its rank at the LogOrigin
String LogOrigin::getNode()
{
    // Note, MPI rank 0 means the MPIClient from mpi4py is running, not a server,
    // therefore it should print nothing
    String rank = EnvironmentVariable::get("OMPI_COMM_WORLD_RANK");
    if (! rank.empty()) {
      if (rank == "0") {
        rank = String();
      } else {
        rank = "MPIServer-" + rank;
      }
    }
    return rank;
}

} //# NAMESPACE CASACORE - END

