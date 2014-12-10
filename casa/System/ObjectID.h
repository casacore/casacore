//# ObjectID.h: A unique identifier for distributed and other objects
//# Copyright (C) 1996,1998,1999,2000,2001,2003
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


#ifndef CASA_OBJECTID_H
#define CASA_OBJECTID_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
template<class T> class Block;

// <summary> 
// ObjectID:  A unique identifier for distributed and other objects.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tObjectID.cc" demos="">

// <prerequisite>
//   <li> none
// </prerequisite>
//
// <etymology>
// The ObjectID class name reflects its role as the single identifier for 
// distributed and other user-level objects.
// </etymology>
//
// <synopsis> 
// The ObjectID class is used to give a unique identifier to ``high-level''
// objects in the system. Internally the ObjectID consists of a sequence number
// (unique within the creating process), a process id, a creation time, and a
// host id. Pragmatically the ObjectID should be unique with no dangers of
// collisions.
//
// A special ``Null'' ObjectID is available. 
// </synopsis> 
//
// <motivation>
// The fundamental purpose for an ObjectID is to provide a unique identifier
// for persistent objects, or for objects that might be accessed outside the
// creating processes address space.
// </motivation>
//
// <todo asof="1997/09/23">
//   <li> Nothing (hostid -> hostname on this date).
// </todo>

class ObjectID
{
public:
    // If <src>makeNull</src> is True, make the null ObjectID, otherwise create
    // a unique ObjectID.
    ObjectID(Bool makeNull = False);
    // Create explicitly from the provided constituents.
    ObjectID(Int sequence, Int pid, Int time, const String &hostname);

    // Copy <src>other</src>. Note that if the ObjectID is embedded inside an
    // object, the enclosing object probably does not want to copy the ObjectID
    // since generally speaking the identity of the enclosing object should be
    // immutable.
    // <group>
    ObjectID(const ObjectID &other);
    ObjectID &operator=(const ObjectID &other);
    // </group>
    
    // Is this ObjectID set?
    Bool isNull() const;

    // Compare two ObjectID's for (in)equality.
    // <group>
    Bool operator==(const ObjectID &other) const;
    Bool operator!=(const ObjectID &other) const;
    // </group>

    // It is useful to interconvert between strings and ObjecID's, e.g. when
    // saving to FITS or writing to a table. The form of the string is:
    // <srcblock>
    // sequence=123 host=hostname pid=pid time=time
    // </srcblock>
    // with an optional comma between the fields.
    // However, in general user code should not depend on the exact form of
    // the string.
    // <group>
    // If this fails, an error message is set and the ObjectID is the null
    // ObjectID.
    Bool fromString(String &error, const String &in);
    // Note that <src>out</src> is zero'd before it is set.
    void toString(String &out) const;
    // </group>

    // Ordinarily the user does not need to get at the exact state of the,
    // ObjectID, however it is available for those times when it is necessary.
    // <group>
    Int sequence() const;
    Int pid() const;
    Int creationTime() const;
    const String &hostName() const;
    // </group>

    // Extract objectID strings (as set by glish script substitute.g) from
    // a command, convert them to ObjectID objects, store those in the
    // Block, and replace the strings by their Block indices as
    // <src>$OBJ#n#O</src> where n is the index.
    static String extractIDs (Block<ObjectID>& objectIDs,
			      const String& command);

private:
    Int sequence_number_p;
    Int process_id_p;
    Int creation_time_p;
    String hostname_p;

    // Make a unique sequence number, returns 0 on first call, 1 on next, ...
    static Int sequence_number();
};

uInt hashFunc(const ObjectID &);

ostream &operator<<(ostream &os, const ObjectID &id);

//# Inlines

inline Int ObjectID::sequence() const 
{
    return sequence_number_p;
}

inline Int ObjectID::pid() const 
{
    return process_id_p;
}

inline Int ObjectID::creationTime() const
{
    return creation_time_p;
}

inline const String &ObjectID::hostName() const
{
    return hostname_p;
}


} //# NAMESPACE CASACORE - END

#endif
