//# IncrStManAccessor.cc: Gives access to some IncrementalStMan functions
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
//# $Id$

//# Includes
#include <casacore/tables/DataMan/IncrStManAccessor.h>
#include <casacore/tables/DataMan/ISMBase.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/BasicSL/String.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ROIncrementalStManAccessor::ROIncrementalStManAccessor (const Table& table,
                                                        const String& name,
                                                        Bool byColumn)
  : RODataManAccessor (table, name, byColumn),
    dataManPtr_p (0)
{
    dataManPtr_p = dynamic_cast<ISMBase*>(baseDataManager());
    if (dataManPtr_p == 0) {
	throw (DataManError ("ROIncrementalStManAccessor " + name +
                             " constructed for data manager type "
			     + baseDataManager()->dataManagerType() +
			     "; expected IncrementalStMan"));
    }
}

ROIncrementalStManAccessor::~ROIncrementalStManAccessor()
{}

ROIncrementalStManAccessor::ROIncrementalStManAccessor
                               (const ROIncrementalStManAccessor& that)
: RODataManAccessor(that),
  dataManPtr_p (that.dataManPtr_p)
{}

ROIncrementalStManAccessor& ROIncrementalStManAccessor::operator=
	                       (const ROIncrementalStManAccessor& that)
{
    dataManPtr_p = that.dataManPtr_p;
    return *this;
}


void ROIncrementalStManAccessor::setCacheSize (uInt size,
                                               Bool canExceedNrBuckets)
{
    dataManPtr_p->setCacheSize (size, canExceedNrBuckets);
}
uInt ROIncrementalStManAccessor::cacheSize() const
{
    return dataManPtr_p->cacheSize();
}

void ROIncrementalStManAccessor::clearCache()
{
    dataManPtr_p->clearCache();
}

void ROIncrementalStManAccessor::showIndexStatistics (ostream& os) const
{
    dataManPtr_p->showIndexStatistics (os);
}

void ROIncrementalStManAccessor::showBucketLayout (ostream& os) const
{
    dataManPtr_p->showBucketLayout (os);
}

Bool ROIncrementalStManAccessor::checkBucketLayout (uInt &offendingCursor,
                                                    uInt &offendingBucketStartRow,
                                                    uInt &offendingBucketNrow,
                                                    uInt &offendingBucketNr,
                                                    uInt &offendingCol,
                                                    uInt &offendingIndex,
                                                    uInt &offendingRow,
                                                    uInt &offendingPrevRow) const
{
  Bool ok;
  ok = dataManPtr_p->checkBucketLayout (offendingCursor,
                                        offendingBucketStartRow,
                                        offendingBucketNrow,
                                        offendingBucketNr,
                                        offendingCol,
                                        offendingIndex,
                                        offendingRow,
                                        offendingPrevRow);
  return ok;
}

} //# NAMESPACE CASACORE - END

