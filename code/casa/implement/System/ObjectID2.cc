//# ObjectID2.cc: Hash related OjectID functions. Prevent link coupling.
//# Copyright (C) 1996,1999
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

#include <aips/Tasking/ObjectID.h>
#include <aips/Arrays/Vector.h>
#include <aips/OS/Time.h>

#include <unistd.h>
#include <iostream.h>

#if defined(AIPS_SOLARIS)
#include <sys/systeminfo.h>
#include <stdio.h>
#endif

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
