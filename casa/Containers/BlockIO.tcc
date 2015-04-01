//# BlockIO.cc: Functions to perform IO for the Block class
//# Copyright (C) 1993,1994,1995,1999,2001,2005
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

#ifndef CASA_BLOCKIO_TCC
#define CASA_BLOCKIO_TCC

#include <casacore/casa/Containers/BlockIO.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/IO/AipsIOCarray.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> void putBlock (AipsIO& ios, const Block<T>& blk, Int nr)
{
    if (nr < 0) {
	nr = 0;
    } else if (nr > Int(blk.nelements())) {
	nr = blk.nelements();
    }
    ios.putstart("Block", 1);
    putAipsIO(ios, (uInt)nr, blk.storage());
    ios.putend();
}


template<class T> void getBlock (AipsIO& ios, Block<T>& blk)
{
    ios.getstart("Block");
    uInt nr;
    ios >> nr;
    blk.resize(nr,True);
    getAipsIO(ios, (uInt)nr, blk.storage());
    ios.getend();
}


template<class T> void showBlock (ostream& ios, const Block<T>& blk, Int nr)
{
    if (nr < 0) {
	nr = 0;
    } else if (nr > Int(blk.nelements())) {
	nr = blk.nelements();
    }
    ios << "[";
    for (Int i=0; i<nr; i++) {
        if (i > 0) {
	    ios << ", ";
	}
	ios << blk[i];
    }
    ios << "]";
}

} //# NAMESPACE CASACORE - END


#endif
