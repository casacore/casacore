//# <ClassFileName.h>: this defines <ClassName>, which ...
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

#include <casacore/casa/System/ObjectID.h>
#include <casacore/casa/OS/HostInfo.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/sstream.h>
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ObjectID::ObjectID(bool makeNull)
  : sequence_number_p(0), process_id_p(0), creation_time_p(0), hostname_p("")
{
    if (! makeNull) {
        sequence_number_p = sequence_number();
	process_id_p = HostInfo::processID();
	creation_time_p = int32_t(HostInfo::secondsFrom1970() + 0.499);
	hostname_p = HostInfo::hostName();
    }
}

ObjectID::ObjectID(int32_t sequence, int32_t pid, int32_t time, const String &hostname)
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

bool ObjectID::isNull() const 
{
    return (sequence_number_p == 0 &&
		  process_id_p == 0 &&
		  creation_time_p == 0 &&
		  hostname_p == "");
}

bool ObjectID::operator==(const ObjectID &other) const
{
    return (sequence_number_p == other.sequence_number_p &&
		  process_id_p == other.process_id_p &&
		  creation_time_p == other.creation_time_p &&
		  hostname_p == other.hostname_p);
}

bool ObjectID::operator!=(const ObjectID &other) const
{
    return (! (*this == other));
}

int32_t ObjectID::sequence_number()
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

    ostringstream os;
    os << "sequence=" << sequence() << " host=" << hostName() <<
	" pid=" << pid() << " time=" << creationTime();
    out = os.str();
}

static bool toInt(int32_t &val, String &error, const String &in)
{
    error = "";
    val = 0;
    int32_t len = in.length();
    if (len == 0) {
	error = "No digits in number.";
	return false;
    }
    for (int32_t i=0; i<len; i++) {
	char digit = in[i];
	int32_t diff = digit - '0';
	if (diff < 0 || diff > 9) {
	    error = String("Illegal character (") + digit + ") in number";
	    return false;
	}
	val = 10*val + diff;
    }
    return true;
}

bool ObjectID::fromString(String &error, const String &in)
{
    error = "";
    *this = ObjectID(true);
    if (in == "") {
	return true; // Null string is the null object!
    }

    // Allow for extra fields.
    String parsed[8]; // keyword=value for each String
    int32_t found = split(in, parsed, sizeof(parsed)/sizeof(String),
		      Regex("[ \t,]+"));
    if (found <= 0) {
	error = String("Could not parse string: ") + in;
	return false;
    }

    bool foundSeq = false, foundHost = false, foundPid = false, 
	foundTime = false;
    String host;
    int32_t seq, pid, time;
    bool ok = true;

    
    String splitup[2];
    
    String &key = splitup[0];
    String &val = splitup[1];
    for (int32_t i=0; ok && i<found; i++) {
	key = ""; val = "";
	split(parsed[i], splitup, 2, "=");
	val.gsub(" ", "");
	if (key == "sequence") {
	    if (foundSeq || !toInt(seq, error, val)) {
		ok = false;
		error = String("Error parsing 'sequence': ") + error;
	    } else {
		foundSeq = true;
	    }
	} else if (key == "host") {
	    if (!foundHost && val != "") {
		host = val;
		foundHost = true;
	    } else {
		ok = false;
		error = String("Illegal host field in: ") + in;
	    }
	} else if (key == "pid") {
	    if (foundPid || !toInt(pid, error, val)) {
		ok = false;
		error = String("Error parsing 'pid': ") + error;
	    } else {
		foundPid = true;
	    }
	} else if (key == "time") {
	    if (foundTime || !toInt(time, error, val)) {
		ok = false;
		error = String("Error parsing 'time': ") + error;
	    } else {
		foundTime = true;
	    }
	}
    }

    if (ok) {
	if (foundSeq && foundPid && foundHost && foundTime) {
	    *this = ObjectID(seq, pid, time, host);
	} else {
	    ok = false;
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

} //# NAMESPACE CASACORE - END

