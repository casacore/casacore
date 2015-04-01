//# tTSMShape.cc: Test program for class TSMShape
//# Copyright (C) 1994,1995,1996,2001
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

#include <casacore/tables/DataMan/TSMShape.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test program for class tTSMShape.
// </summary>


void check (const IPosition& shape, const IPosition& offsetIncr,
	    const IPosition& stride, const char* message)
{
    TSMShape tsmShape (shape);
    IPosition position(4);
    uInt checkOffset = 0;
    for (position(3)=0; position(3)<stride(3); position(3)++) {
	for (position(2)=0; position(2)<stride(2); position(2)++) {
	    for (position(1)=0; position(1)<stride(1); position(1)++) {
		for (position(0)=0; position(0)<stride(0); position(0)++) {
		    uInt offset = tsmShape.offset (position);
		    if (offset != checkOffset) {
			cout << message << "invalid offset: " << offset
			     << " iso " << checkOffset << endl;
		    }
		    IPosition pos = tsmShape.position (checkOffset);
		    if (pos != position) {
			cout << message << "invalid position " << pos
			     << " at offset " << checkOffset << endl;
		    }
		    checkOffset += offsetIncr(0);
		}
		checkOffset += offsetIncr(1);
	    }
	    checkOffset += offsetIncr(2);
	}
	checkOffset += offsetIncr(3);
    }
}

int main()
{
  {
    IPosition shape (3,4,5,6);
    IPosition origin(3,2,5,3);
    TSMShape tsmShape (shape);
    uInt checkOffset = 0;
    IPosition position(3);
    for (position(2)=0; position(2)<shape(2); position(2)++) {
	for (position(1)=0; position(1)<shape(1); position(1)++) {
	    for (position(0)=0; position(0)<shape(0); position(0)++) {
		uInt offset = tsmShape.offset (position+origin, origin);
		if (offset != checkOffset) {
		    cout << "invalid orig offset: " << offset << " iso "
			 << checkOffset << endl;
		}
		IPosition pos = tsmShape.position (checkOffset, origin);
		if (pos != position+origin) {
		    cout << "invalid position " << pos << " at offset "
			 << checkOffset << endl;
		}
		checkOffset++;
	    }
	}
    }
  }

  {
    IPosition shape (4,4,5,6,7);
    IPosition incr(4);
    TSMShape tsmShape (shape);
    incr = tsmShape.offsetIncrement (IPosition(4,4,5,6,7));
    if (incr(0) != 1  &&  incr(1) != 0  &&  incr(2) != 0  &&  incr(3) != 0) {
	cout << "Invalid incr(4,5,6,7): " << incr(0) << "," << incr(1)
	     << "," << incr(2) << "," << incr(3) << endl;
    }
    check (shape, incr, IPosition(4,4,5,6,7), "(4,5,6,7) ");
    incr = tsmShape.offsetIncrement (IPosition(4,1,1,1,1));
    check (shape, incr, IPosition(4,1,1,1,1), "(1,1,1,1) ");
    incr = tsmShape.offsetIncrement (IPosition(4,3,2,6,4));
    check (shape, incr, IPosition(4,1,1,1,1), "(1,3,2,6,4) ");
  }

  cout << "OK" << endl;
  return 0;
}
