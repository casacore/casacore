//# StandardStManAccessor.cc: Gives access to some StandardStMan functions
//# Copyright (C) 2000
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
#include <aips/Tables/StandardStManAccessor.h>
#include <aips/Tables/SSMBase.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/DataManError.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>

ROStandardStManAccessor::ROStandardStManAccessor (const Table& aTable,
					    const String& aDataManagerName)
: itsSSMPtr (0)
{
    DataManager* aDmPtr = findDataManager (aTable, aDataManagerName);
    SSMBase aDataMan;
    if (aDmPtr->dataManagerType() != aDataMan.dataManagerType()) {
	throw (DataManError ("Data manager " + aDataManagerName + " has type "
			     + aDmPtr->dataManagerType() + "; expected "
			     + aDataMan.dataManagerType()));
    }
    // The types match, so it is now safe to cast.
    itsSSMPtr = dynamic_cast<SSMBase*>(aDmPtr);
    AlwaysAssert(itsSSMPtr != 0, AipsError);
}

ROStandardStManAccessor::~ROStandardStManAccessor()
{}

ROStandardStManAccessor::ROStandardStManAccessor
                               (const ROStandardStManAccessor& that)
: itsSSMPtr (that.itsSSMPtr)
{}

ROStandardStManAccessor& ROStandardStManAccessor::operator=
	                       (const ROStandardStManAccessor& that)
{
    itsSSMPtr = that.itsSSMPtr;
    return *this;
}


void ROStandardStManAccessor::setCacheSize (uInt aSize)
{
    itsSSMPtr->setCacheSize (aSize);
}

uInt ROStandardStManAccessor::getCacheSize() const
{
    return itsSSMPtr->getCacheSize();
}

void ROStandardStManAccessor::clearCache()
{
    itsSSMPtr->clearCache();
}

void ROStandardStManAccessor::showCacheStatistics (ostream& anOs) const
{
    itsSSMPtr->showCacheStatistics (anOs);
}

void ROStandardStManAccessor::showBaseStatistics (ostream& anOs) const
{
    itsSSMPtr->showBaseStatistics (anOs);
}

void ROStandardStManAccessor::showIndexStatistics (ostream& anOs) const
{
    itsSSMPtr->showIndexStatistics (anOs);
}
