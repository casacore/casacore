//# ObjectID.h: A unique identifier for distributed and other objects
//# Copyright (C) 1996
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


#if !defined(AIPS_OBJECT_ID_H)
#define AIPS_OBJECT_ID_H

#include <aips/aips.h>

template<class T> class Vector;
#if defined(AIPS_STDLIB)
#include <iosfwd.h>
#else
imported class ostream;
#endif

// <summary> 
// ObjectID:  A unique identifier for distributed and other objects.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tObjectID.cc" demos="">

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
// <todo asof="1996March18">
//   <li> Hostname would be more useful than hostid.
// </todo>

class ObjectID
{
public:
    // If <src>makeNull</src> is True, make the null ObjectID, otherwise create
    // a unique ObjectID.
    ObjectID(Bool makeNull = False);
    // Create explicitly from the provided constituents.
    ObjectID(Int sequence, Int pid, Int time, Int hostid);

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

    // ObjectID's must be handed across the network, etc., so it must be
    // straightforward to save/restore an object from a canonical data type.
    // <group>
    const Vector<Int> &toVector() const;
    void fromVector(const Vector<Int> &vec);
    // </group>

    // Ordinarily the user does not need to get at the exact state of the,
    // ObjectID, however it is available for those times when it is necessary.
    // <group>
    Int sequence() const;
    Int pid() const;
    Int creationTime() const;
    Int hostid() const;
    // </group>
private:
    Int sequence_number_p;
    Int process_id_p;
    Int creation_time_p;
    Int hostid_p;

    // Break into functions to isolate portability problems
    static Int sequence_number();
    static Int process_id();
    static Int time();
    static Int host_id();
};

ostream &operator<<(ostream &os, const ObjectID &);

uInt hashFunc(const ObjectID &);

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

inline Int ObjectID::hostid() const
{
    return hostid_p;
}

#endif

