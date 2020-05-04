//# TSMIdColumn.cc: Tiled Hypercube Storage Manager for id columns
//# Copyright (C) 1995,1996,1997,1999
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
#include <casacore/tables/DataMan/TSMIdColumn.h>
#include <casacore/tables/DataMan/TiledStMan.h>
#include <casacore/tables/DataMan/TSMCube.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TSMIdColumn::TSMIdColumn (const TSMColumn& column)
: TSMColumn (column)
{}

TSMIdColumn::~TSMIdColumn()
{}

void TSMIdColumn::getfloat (rownr_t rownr, float* dataPtr)
{
    // Get the hypercube the row is in.
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr);
    hypercube->valueRecord().get (columnName(), *dataPtr);
}
void TSMIdColumn::putfloat (rownr_t rownr, const float* dataPtr)
{
    float value;
    TSMIdColumn::getfloat (rownr, &value);
    if (value != *dataPtr) {
	throw TSMError ("TSMIdColumn::put: new value mismatches existing "
                        "in id column " + columnName());
    }
}



#define TSMIDCOLUMN_GETPUT(T) \
void TSMIdColumn::aips_name2(get,T) (rownr_t rownr, T* dataPtr) \
{ \
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr); \
    hypercube->valueRecord().get (columnName(), *dataPtr); \
} \
void TSMIdColumn::aips_name2(put,T) (rownr_t rownr, const T* dataPtr) \
{ \
    T value; \
    TSMIdColumn::aips_name2(get,T) (rownr, &value); \
    if (value != *dataPtr) { \
	throw TSMError ("TSMIdColumn::put: new value mismatches existing" \
                        " in id column " + columnName()); \
    } \
}

TSMIDCOLUMN_GETPUT(Bool)
TSMIDCOLUMN_GETPUT(Int)
TSMIDCOLUMN_GETPUT(uInt)
TSMIDCOLUMN_GETPUT(Int64)
//#TSMIDCOLUMN_GETPUT(float)
TSMIDCOLUMN_GETPUT(double)
TSMIDCOLUMN_GETPUT(Complex)
TSMIDCOLUMN_GETPUT(DComplex)
TSMIDCOLUMN_GETPUT(String)

} //# NAMESPACE CASACORE - END

