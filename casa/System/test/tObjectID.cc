//# tObjectID.cc: This program tests the ObjectID class
//# Copyright (C) 1994,1995,1996,2000,2001,2003
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/System/ObjectID.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

void assert_hash(const ObjectID &id)
{
	if (!id.isNull()) {
		// Hash bytes are: seq number, pid, creation time and hostname
		// All except the first will always have at least one bit set
		auto hash = hashFunc(id);
		AlwaysAssertExit(((hash >> 8) & 0xff) != 0);
		AlwaysAssertExit(((hash >> 16) & 0xff) != 0);
		AlwaysAssertExit(((hash >> 24) & 0xff) != 0);
	}
}

int main()
{
  try{

    // default ctor
    ObjectID ID1; 
    ObjectID ID2; 
    assert_hash(ID1);
    assert_hash(ID2);

    // inequality operator
    AlwaysAssertExit(ID1!=ID2);

    // assignment operator
    ID1 = ID2;
    // equality operator, too!
    AlwaysAssertExit(ID1 == ID2);
    assert_hash(ID1);
    assert_hash(ID2);

    // copy ctor
    ObjectID ID1copy(ID1);
    AlwaysAssertExit(ID1 == ID1copy);
    assert_hash(ID1copy);

    ObjectID null(True);
    AlwaysAssertExit(null.isNull());

    String copied;
    ID1.toString(copied);
    ID1 = ObjectID(True);
    String error;
    AlwaysAssertExit(ID1.fromString(error, copied));
    AlwaysAssertExit(ID1 == ID1copy);
    assert_hash(ID1);
    ID1.toString(copied);

  } catch (std::exception& x) {
    cerr << x.what() << endl;
    return 1;
  } 

  cout << "OK" << endl;
  return 0;
}






