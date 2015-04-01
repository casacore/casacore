//# ObjectID2.cc: Hash related OjectID functions. Prevent link coupling.
//# Copyright (C) 1996,1999,2001,2003
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


#include <casacore/casa/System/ObjectID.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/stdio.h>                  // needed for sprintf


namespace casacore { //# NAMESPACE CASACORE - BEGIN

uInt hashFunc(const ObjectID &key)
{
    // We should check to see if this hash is any good
    uInt result = 0;
    char c;
    c = (char) key.sequence();
    result = result || c;
    c = (char) key.pid();
    result = result || (c<<8);
    c = (char)key.creationTime();
    result = result || (c<<16);
    c = (char)key.hostName()[0];
    result = result || (c<<24);
    return result;
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
	    char buf[16];
	    sprintf (buf, "$OBJ#%i#O", n);
	    result += buf;
	    str = str.after(pos+1);
	}
	index = str.index ("'ObjectID=[");
    }
    result += str;
    return result;
}

} //# NAMESPACE CASACORE - END

