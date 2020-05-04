//# StManColumnBase.cc: Base storage manager column class
//# Copyright (C) 2019
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
#include <casacore/tables/DataMan/StManColumnBase.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/tables/DataMan/DataManError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

StManColumnBase::StManColumnBase (int dataType)
  : dtype_p (static_cast<DataType>(dataType))
{
    elemSize_p = ValType::getTypeSize (dtype_p);
}

StManColumnBase::~StManColumnBase()
{}

int StManColumnBase::dataType() const
    { return dtype_p; }

Bool StManColumnBase::isNativeDataType (int dtype)
{
    switch (dtype) {
    case TpBool:
    case TpUChar:
    case TpShort:
    case TpUShort:
    case TpInt:
    case TpUInt:
    case TpInt64:
    case TpFloat:
    case TpDouble:
    case TpComplex:
    case TpDComplex:
    case TpString:
    case TpArrayBool:
    case TpArrayUChar:
    case TpArrayShort:
    case TpArrayUShort:
    case TpArrayInt:
    case TpArrayUInt:
    case TpArrayInt64:
    case TpArrayFloat:
    case TpArrayDouble:
    case TpArrayComplex:
    case TpArrayDComplex:
    case TpArrayString:
	return True;
    }
    return False;
}


} //# NAMESPACE CASACORE - END
