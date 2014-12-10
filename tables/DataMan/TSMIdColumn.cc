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

void TSMIdColumn::getfloatV (uInt rownr, float* dataPtr)
{
    // Get the hypercube the row is in.
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr);
    hypercube->valueRecord().get (columnName(), *dataPtr);
}
void TSMIdColumn::putfloatV (uInt rownr, const float* dataPtr)
{
    float value;
    TSMIdColumn::getfloatV (rownr, &value);
    if (value != *dataPtr) {
	throw (TSMError ("TSMIdColumn::put: new value mismatches existing"));
    }
}



#define TSMIDCOLUMN_GETPUT(T,NM) \
void TSMIdColumn::aips_name2(get,NM) (uInt rownr, T* dataPtr) \
{ \
    TSMCube* hypercube = stmanPtr_p->getHypercube (rownr); \
    hypercube->valueRecord().get (columnName(), *dataPtr); \
} \
void TSMIdColumn::aips_name2(put,NM) (uInt rownr, const T* dataPtr) \
{ \
    T value; \
    TSMIdColumn::aips_name2(get,NM) (rownr, &value); \
    if (value != *dataPtr) { \
	throw (TSMError ("TSMIdColumn::put: new value mismatches existing")); \
    } \
}

TSMIDCOLUMN_GETPUT(Bool,BoolV)
TSMIDCOLUMN_GETPUT(Int,IntV)
TSMIDCOLUMN_GETPUT(uInt,uIntV)
//#TSMIDCOLUMN_GETPUT(float,floatV)
TSMIDCOLUMN_GETPUT(double,doubleV)
TSMIDCOLUMN_GETPUT(Complex,ComplexV)
TSMIDCOLUMN_GETPUT(DComplex,DComplexV)
TSMIDCOLUMN_GETPUT(String,StringV)

} //# NAMESPACE CASACORE - END

