//# NewFile.cc: Constrain a string to be a new (non-existent) file
//# Copyright (C) 1996,1997,1999,2000
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

#include <trial/Tasking/NewFile.h>
#include <trial/Tasking/ApplicationEnvironment.h>

#include <aips/OS/File.h>
#include <aips/OS/RegularFile.h>
#include <aips/OS/Directory.h>
#include <aips/OS/SymLink.h>
#include <aips/Arrays/Vector.h>
#include <aips/Tables/Table.h>

#include <aips/Logging/LogMessage.h>
#include <aips/Logging/LogSink.h>
#include <aips/Exceptions/Error.h>

NewFile::NewFile(Bool deleteIfExists) : delete_p(deleteIfExists)
{
    // Nothing
}

NewFile::NewFile(const NewFile &other) : delete_p(other.delete_p)
{
    // Nothing
}

NewFile &NewFile::operator=(const NewFile &other)
{
    if (this != &other) {
	delete_p = other.delete_p;
    }
    return *this;
}

NewFile::~NewFile()
{
    // Nothing
}

Bool NewFile::valueOK(const String &value, String &error) const
{
    LogOrigin or("NewFile", 
		 "valueOK(const String &value, String &error) const",
			WHERE);
    LogMessage msg(or);

    error = "";
    Bool retval = False;
//
    if (value.empty()) {
       error = "File string is empty";
       return False;
    }
//
    File thefile(value);
    if (thefile.exists()) {
	String text = String("File '") + value + "' already exists. Remove it?";
	  
	Vector<String> choices(2);
	choices(0) = "no";
	choices(1) = "yes";
	String remove = ApplicationEnvironment::choice(text, choices);
	if (remove == "yes") {
	    Bool removed = False;
	    String extra_error = "";
	    try {
		if (thefile.isRegular()) {
		    RegularFile rfile = thefile;
		    rfile.remove();
		    removed = True;
		} else if (thefile.isDirectory()) {
		    // Assume that directories are tables.
		    if (! Table::isWritable(value)) {
			removed = False;
			extra_error = "Table is not writable!";
		    } else {
			removed = False;
			if (Table::canDeleteTable(extra_error, value)) {
			    try {
				Table::deleteTable(value);
				removed = True;
			    } catch (AipsError xxx) {
				removed = False;
				extra_error = String("Error deleting table ")
				    + value + ":" + xxx.getMesg();
			    } 
			}
		    }
		} else if (thefile.isSymLink()) {
		    SymLink sfile = thefile;
		    sfile.remove();
		    removed = True;
		}
	    } catch (AipsError x) {
		extra_error = x.getMesg();
		removed = False;
	    } 
	    if (!removed) {
		retval = False;
		error = String("Could not remove file ") + value;
		if (extra_error != "") {
		    error += String("(") + extra_error + ")";
		}
		error += ".";
	    } else {
		retval = True;
		msg.message(String("Removed file ") + value + 
	            " at users request").line(__LINE__).
		    priority(LogMessage::NORMAL);
		LogSink::postGlobally(msg);
	    }
	} else {
	    retval = False;
	    error = String("File ") + value + 
		" exists, and the user does not want to remove it.";
	}
    } else {
	retval = True;
    }

    return retval;
}

ParameterConstraint<String> *NewFile::clone() const
{
    return new NewFile(*this);
}
