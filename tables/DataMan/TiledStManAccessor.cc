//# TiledStManAccessor.cc: Gives access to some TiledStMan functions
//# Copyright (C) 1994,1995,1996,1997,1999,2000
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
#include <casacore/tables/DataMan/TiledStManAccessor.h>
#include <casacore/tables/DataMan/TiledStMan.h>
#include <casacore/tables/DataMan/TSMCube.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/BasicSL/String.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ROTiledStManAccessor::ROTiledStManAccessor (const Table& table,
					    const String& name,
                                            Bool byColumn)
  : RODataManAccessor (table, name, byColumn),
    dataManPtr_p (0)
{
    dataManPtr_p = dynamic_cast<TiledStMan*>(baseDataManager());
    if (dataManPtr_p == 0) {
	throw (DataManError ("ROTiledStManAccessor " + name +
                             " constructed for data manager type "
			     + baseDataManager()->dataManagerType() +
			     "; expected Tiled*StMan"));
    }
}

ROTiledStManAccessor::ROTiledStManAccessor()
{}

ROTiledStManAccessor::~ROTiledStManAccessor()
{}

ROTiledStManAccessor::ROTiledStManAccessor
                               (const ROTiledStManAccessor& that)
: RODataManAccessor(that),
  dataManPtr_p (that.dataManPtr_p)
{}

ROTiledStManAccessor& ROTiledStManAccessor::operator=
	                       (const ROTiledStManAccessor& that)
{
    if (this != &that) {
        RODataManAccessor::operator= (that);
        dataManPtr_p = that.dataManPtr_p;
    }
    return *this;
}


DataManager* ROTiledStManAccessor::getDataManager() const
{
    return dataManPtr_p;
}

void ROTiledStManAccessor::setMaximumCacheSize (uInt size)
{
    dataManPtr_p->setMaximumCacheSize (size);
}
uInt ROTiledStManAccessor::maximumCacheSize() const
{
    return dataManPtr_p->maximumCacheSize();
}

uInt ROTiledStManAccessor::cacheSize (uInt rownr) const
{
    return dataManPtr_p->cacheSize (rownr);
}

const IPosition& ROTiledStManAccessor::hypercubeShape (uInt rownr) const
{
    return dataManPtr_p->hypercubeShape (rownr);
}

const IPosition& ROTiledStManAccessor::tileShape (uInt rownr) const
{
    return dataManPtr_p->tileShape (rownr);
}

uInt ROTiledStManAccessor::bucketSize (uInt rownr) const
{
    return dataManPtr_p->bucketSize (rownr);
}

const Record& ROTiledStManAccessor::valueRecord (uInt rownr) const
{
    return dataManPtr_p->getHypercube(rownr)->valueRecord();
}

uInt ROTiledStManAccessor::nhypercubes() const
{
    return dataManPtr_p->nhypercubes();
}

uInt ROTiledStManAccessor::getCacheSize (uInt hypercube) const
{
    return dataManPtr_p->getTSMCube(hypercube)->cacheSize();
}

const IPosition& ROTiledStManAccessor::getHypercubeShape (uInt hypercube) const
{
    return dataManPtr_p->getTSMCube(hypercube)->cubeShape();
}

const IPosition& ROTiledStManAccessor::getTileShape (uInt hypercube) const
{
    return dataManPtr_p->getTSMCube(hypercube)->tileShape();
}

uInt ROTiledStManAccessor::getBucketSize (uInt hypercube) const
{
    return dataManPtr_p->getTSMCube(hypercube)->bucketSize();
}

const Record& ROTiledStManAccessor::getValueRecord (uInt hypercube) const
{
    return dataManPtr_p->getTSMCube(hypercube)->valueRecord();
}

uInt ROTiledStManAccessor::calcCacheSize (uInt rownr,
					  const IPosition& sliceShape,
					  const IPosition& axisPath) const
{
    return dataManPtr_p->calcCacheSize (rownr, sliceShape, IPosition(),
					IPosition(), axisPath);
}
uInt ROTiledStManAccessor::calcCacheSize (uInt rownr,
					  const IPosition& sliceShape,
					  const IPosition& windowStart,
					  const IPosition& windowLength,
					  const IPosition& axisPath) const
{
    return dataManPtr_p->calcCacheSize (rownr, sliceShape, windowStart,
					windowLength, axisPath);
}

void ROTiledStManAccessor::setCacheSize (uInt rownr,
					 const IPosition& sliceShape,
					 const IPosition& axisPath,
					 Bool forceSmaller)
{
    dataManPtr_p->setCacheSize (rownr, sliceShape, IPosition(),
				IPosition(), axisPath,
				forceSmaller);
}
void ROTiledStManAccessor::setCacheSize (uInt rownr,
					 const IPosition& sliceShape,
					 const IPosition& windowStart,
					 const IPosition& windowLength,
					 const IPosition& axisPath,
					 Bool forceSmaller)
{
    dataManPtr_p->setCacheSize (rownr, sliceShape, windowStart,
				windowLength, axisPath,
				forceSmaller);
}

void ROTiledStManAccessor::setCacheSize (uInt rownr, uInt nbuckets,
					 Bool forceSmaller)
{
    dataManPtr_p->setCacheSize (rownr, nbuckets, forceSmaller);
}

void ROTiledStManAccessor::clearCaches()
{
    dataManPtr_p->emptyCaches();
}

} //# NAMESPACE CASACORE - END

