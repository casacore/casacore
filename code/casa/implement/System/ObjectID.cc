//# <ClassFileName.h>: this defines <ClassName>, which ...
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

#include <aips/Tasking/ObjectID.h>
#include <aips/OS/HostInfo.h>
#include <aips/Utilities/Regex.h>
#include <aips/Utilities/Assert.h>
#include <aips/Containers/RecordInterface.h>

#include <strstream.h>
#include <iostream.h>
#include <stdio.h>                  // needed for sprintf


ObjectID::ObjectID(Bool makeNull)
  : sequence_number_p(0), process_id_p(0), creation_time_p(0), hostname_p("")
{
    if (! makeNull) {
        sequence_number_p = sequence_number();
	process_id_p = HostInfo::processID();
	creation_time_p = Int(HostInfo::secondsFrom1970() + 0.499);
	hostname_p = HostInfo::hostName();
    }
}

ObjectID::ObjectID(Int sequence, Int pid, Int time, const String &hostname)
  : sequence_number_p(sequence), process_id_p(pid), creation_time_p(time),
    hostname_p(hostname)
{
    // Nothing
}

ObjectID::ObjectID(const ObjectID &other)
  : sequence_number_p(other.sequence_number_p), 
    process_id_p(other.process_id_p),
    creation_time_p(other.creation_time_p), hostname_p(other.hostname_p)
{
    // Nothing
}

ObjectID &ObjectID::operator=(const ObjectID &other)
{
    if (this != &other) {
      sequence_number_p = other.sequence_number_p;
      process_id_p = other.process_id_p;
      creation_time_p = other.creation_time_p;
      hostname_p = other.hostname_p;
    }
    return *this;
}

Bool ObjectID::isNull() const 
{
    return ToBool(sequence_number_p == 0 &&
		  process_id_p == 0 &&
		  creation_time_p == 0 &&
		  hostname_p == "");
}

Bool ObjectID::operator==(const ObjectID &other) const
{
    return ToBool(sequence_number_p == other.sequence_number_p &&
		  process_id_p == other.process_id_p &&
		  creation_time_p == other.creation_time_p &&
		  hostname_p == other.hostname_p);
}

Bool ObjectID::operator!=(const ObjectID &other) const
{
    return ToBool(! (*this == other));
}

Int ObjectID::sequence_number()
{
    static int seqno = -1;
    seqno++;
    return seqno;
}

void ObjectID::toString(String &out) const
{
    out = "";
    if (isNull()) {
	return;
    }

    ostrstream os;
    os << "sequence=" << sequence() << " host=" << hostName() <<
	" pid=" << pid() << " time=" << creationTime();
    out = os;
}

static Bool toInt(Int &val, String &error, const String &in)
{
    error = "";
    val = 0;
    Int len = in.length();
    if (len == 0) {
	error = "No digits in number.";
	return False;
    }
    for (Int i=0; i<len; i++) {
	char digit = in[i];
	Int diff = digit - '0';
	if (diff < 0 || diff > 9) {
	    error = String("Illegal character (") + digit + ") in number";
	    return False;
	}
	val = 10*val + diff;
    }
    return True;
}

Bool ObjectID::fromString(String &error, const String &in)
{
    error = "";
    *this = ObjectID(True);
    if (in == "") {
	return True; // Null string is the null object!
    }

    // Allow for extra fields.
    String parsed[8]; // keyword=value for each String
    Int found = split(in, parsed, sizeof(parsed)/sizeof(String),
		      Regex("[ \t,]+"));
    if (found <= 0) {
	error = String("Could not parse string: ") + in;
	return False;
    }

    Bool foundSeq = False, foundHost = False, foundPid = False, 
	foundTime = False;
    String host;
    Int seq, pid, time;
    Bool ok = True;

    
    String splitup[2];
    
    String &key = splitup[0];
    String &val = splitup[1];
    for (Int i=0; ok && i<found; i++) {
	key = ""; val = "";
	split(parsed[i], splitup, 2, "=");
	val.gsub(" ", "");
	if (key == "sequence") {
	    if (foundSeq || !toInt(seq, error, val)) {
		ok = False;
		error = String("Error parsing 'sequence': ") + error;
	    } else {
		foundSeq = True;
	    }
	} else if (key == "host") {
	    if (!foundHost && val != "") {
		host = val;
		foundHost = True;
	    } else {
		ok = False;
		error = String("Illegal host field in: ") + in;
	    }
	} else if (key == "pid") {
	    if (foundPid || !toInt(pid, error, val)) {
		ok = False;
		error = String("Error parsing 'pid': ") + error;
	    } else {
		foundPid = True;
	    }
	} else if (key == "time") {
	    if (foundTime || !toInt(time, error, val)) {
		ok = False;
		error = String("Error parsing 'time': ") + error;
	    } else {
		foundTime = True;
	    }
	}
    }

    if (ok) {
	if (foundSeq && foundPid && foundHost && foundTime) {
	    *this = ObjectID(seq, pid, time, host);
	} else {
	    ok = False;
	    error = "Could not find all of sequence, host, pid, and time";
	}
    }
    return ok;
}

ostream &operator<<(ostream &os, const ObjectID &id)
{
    String tmp;
    id.toString(tmp);
    os << "[" << tmp << "]";
    return os;
}


String ObjectID::extractIDs (Block<ObjectID>& objectIDs,
			     const String& command)
{
    objectIDs.resize (0, True, True);
    String error;
    String result;
    String str = command;
    // Extract object-id from the command, convert it to an
    // ObjectID in the block, and put its index into the command.
    Int index = str.index ("'ObjectID=[");
    while (index >= 0) {
        result += str.before(index);
	index += 11;
	Int pos = str.index ("]'", index);
	ObjectID oid;
	// Convert to ObjectID.
	// If not succesfull, put original back.
	if (! oid.fromString (error, str(index, pos-index))) {
	    result += str(index-11, pos-index+13);
	} else {
	    uInt n = objectIDs.nelements() + 1;
	    objectIDs.resize (n);
	    objectIDs[n-1] = oid;
	    char buf[8];
	    sprintf (buf, "$%i", n);
	    result += buf;
	    str = str.after(pos+1);
	}
	index = str.index ("'ObjectID=[");
    }
    result += str;
    return result;
}
