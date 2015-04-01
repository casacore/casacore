//# TiledDataStManAccessor.cc: Gives access to some TiledDataStMan functions
//# Copyright (C) 1994,1995,1996,1997
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

//# Includes
#include <casacore/tables/DataMan/TiledDataStManAccessor.h>
#include <casacore/tables/DataMan/TiledDataStMan.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/BasicSL/String.h>



namespace casacore { //# NAMESPACE CASACORE - BEGIN

TiledDataStManAccessor::TiledDataStManAccessor (const Table& table,
						const String& dataManagerName)
: ROTiledStManAccessor (table, dataManagerName),
  tiledDataManPtr_p (0)
{
    DataManager* dmptr = getDataManager();
    TiledDataStMan dataMan;
    if (dmptr->dataManagerType() != dataMan.dataManagerType()) {
	throw (DataManError ("Data manager " + dataManagerName + " has type "
			     + dmptr->dataManagerType() + "; expected "
			     + dataMan.dataManagerType()));
    }
    if (! table.isWritable()) {
	throw (DataManError ("TiledDataStManAccessor: table is not writable"));
    }
    // The types match, so it is now safe to cast.
    tiledDataManPtr_p = (TiledDataStMan*)dmptr;
}

TiledDataStManAccessor::TiledDataStManAccessor ()
{
  // dummy constructor
}

TiledDataStManAccessor::~TiledDataStManAccessor()
{}

TiledDataStManAccessor::TiledDataStManAccessor
                               (const TiledDataStManAccessor& that)
: ROTiledStManAccessor (that),
  tiledDataManPtr_p (that.tiledDataManPtr_p)
{}

TiledDataStManAccessor& TiledDataStManAccessor::operator=
	                       (const TiledDataStManAccessor& that)
{
    ROTiledStManAccessor::operator= (that);
    tiledDataManPtr_p = that.tiledDataManPtr_p;
    return *this;
}

void TiledDataStManAccessor::addHypercube (const IPosition& cubeShape,
					   const IPosition& tileShape,
					   const Record& values)
{
    tiledDataManPtr_p->addHypercube (cubeShape, tileShape, values);
}

void TiledDataStManAccessor::extendHypercube (uInt incrInLastDim,
					      const Record& values)
{
    tiledDataManPtr_p->extendHypercube (incrInLastDim, values);
}

} //# NAMESPACE CASACORE - END

