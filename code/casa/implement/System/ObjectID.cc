//# <ClassFileName.h>: this defines <ClassName>, which ...
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

#include <aips/Tasking/ObjectID.h>
#include <aips/Arrays/Vector.h>
#include <aips/OS/Time.h>
#include <aips/Utilities/Assert.h>

#include <unistd.h>
#include <iostream.h>

#if defined(AIPS_SOLARIS)
#include <sys/systeminfo.h>
#include <stdio.h>
#endif


ObjectID::ObjectID(Bool makeNull)
  : sequence_number_p(0), process_id_p(0), creation_time_p(0), hostid_p(0)
{
    if (! makeNull) {
        sequence_number_p = sequence_number();
	process_id_p = process_id();
	creation_time_p = time();
	hostid_p = hostid();
    }
}

ObjectID::ObjectID(Int sequence, Int pid, Int time, Int hostid)
  : sequence_number_p(sequence), process_id_p(pid), creation_time_p(time),
    hostid_p(hostid)
{
    // Nothing
}

ObjectID::ObjectID(const ObjectID &other)
  : sequence_number_p(other.sequence_number_p), process_id_p(other.process_id_p),
    creation_time_p(other.creation_time_p), hostid_p(other.hostid_p)
{
    // Nothing
}

ObjectID &ObjectID::operator=(const ObjectID &other)
{
    if (this != &other) {
      sequence_number_p = other.sequence_number_p;
      process_id_p = other.process_id_p;
      creation_time_p = other.creation_time_p;
      hostid_p = other.hostid_p;
    }
    return *this;
}

Bool ObjectID::isNull() const 
{
    return ToBool(sequence_number_p == 0 &&
		  process_id_p == 0 &&
		  creation_time_p == 0 &&
		  hostid_p == 0);
}

Bool ObjectID::operator==(const ObjectID &other) const
{
    return ToBool(sequence_number_p == other.sequence_number_p &&
		  process_id_p == other.process_id_p &&
		  creation_time_p == other.creation_time_p &&
		  hostid_p == other.hostid_p);
}

Bool ObjectID::operator!=(const ObjectID &other) const
{
    return ToBool(! (*this == other));
}

// Move this out of the object since it can cause problems for our
// exception emulation.
    static Vector<Int> state(4);
const Vector<Int> &ObjectID::toVector() const
{
    state(0) = sequence_number_p;
    state(1) = process_id_p;
    state(2) = creation_time_p;
    state(3) = hostid_p;
    return state;
}

void ObjectID::fromVector(const Vector<Int> &vec)
{
    AlwaysAssert(vec.nelements() >= 4, AipsError);
    sequence_number_p = vec(0);
    process_id_p = vec(1);
    creation_time_p = vec(2);
    hostid_p = vec(3);
}

Int ObjectID::sequence_number()
{
    static int seqno = -1;
    seqno++;
    return seqno;
}

Int ObjectID::process_id()
{
  // Should move out into OS/ package.
    return getpid();
}

// Move out of func in case it causes our exception emulation problems.
Int ObjectID::time()
{
    Time epoch(1990, 1, 1);
    Time now;
    return Int(now - epoch);
}

Int ObjectID::host_id()
{
  // Not very portable yet
#if defined (AIPS_SOLARIS)
  char hostid[32];
  Int  hostidcount = 32;
  Int resultcount = sysinfo( SI_HW_SERIAL, hostid, hostidcount );
  Int id;
  sscanf( hostid, "%i", &id );
#else
  Int id = (Int)gethostid();
#endif
  return id;
}

ostream &operator<<(ostream &os, const ObjectID &id)
{
    os << "[pid=" << id.pid() << " sequence=" << id.sequence() << 
      " creationTime=" << id.creationTime() << " hostid" <<
      id.hostid() << "]";
    return os;
}

