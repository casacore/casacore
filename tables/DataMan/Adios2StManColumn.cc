//# Adios2StManColumn.cc: The Column of the ADIOS2 Storage Manager
//# Copyright (C) 2018
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

#include "Adios2StManColumn.h"

namespace casacore
{

Adios2StManColumn::Adios2StManColumn(
        Adios2StMan *aParent,
        int aDataType,
        String aColName,
        std::shared_ptr<adios2::IO> aAdiosIO)
    :StManColumn(aDataType),
    itsStManPtr(aParent),
    itsColumnName(aColName),
    itsAdiosIO(aAdiosIO)
{
}

String Adios2StManColumn::getColumnName()
{
    return itsColumnName;
}

void Adios2StManColumn::setShapeColumn(const IPosition &aShape)
{
    isShapeFixed = true;
    itsCasaShape = aShape;
    itsAdiosShape.resize(aShape.size() + 1);
    itsAdiosStart.resize(aShape.size() + 1);
    itsAdiosCount.resize(aShape.size() + 1);
    for (size_t i = 1; i < itsCasaShape.size() + 1; ++i)
    {
        itsAdiosShape[i] = itsCasaShape[i - 1];
        itsAdiosCount[i] = itsCasaShape[i - 1];
        itsAdiosStart[i] = 0;
    }
}

IPosition Adios2StManColumn::shape(uInt aRowNr)
{
    if(isShapeFixed)
    {
        return itsCasaShape;
    }
    else
    {
        auto shape = itsCasaShapes.find(aRowNr);
        if(shape != itsCasaShapes.end())
        {
            return shape->second;
        }
        else
        {
            /*
            throw(std::runtime_error("Shape not defined for Column "
                        + static_cast<std::string>(itsColumnName)
                        + " Row " + std::to_string(aRowNr)));
                        */
            return IPosition();
        }
    }
}

Bool Adios2StManColumn::canChangeShape() const
{
    return !isShapeFixed;
}

void Adios2StManColumn::setShape (uInt aRowNr, const IPosition& aShape)
{
    itsCasaShapes[aRowNr] = aShape;
}

void Adios2StManColumn::putBoolV(uInt rownr, const Bool *dataPtr)
{
    putScalarV(rownr, dataPtr);
}
void Adios2StManColumn::putuCharV(uInt rownr, const uChar *dataPtr)
{
    putScalarV(rownr, dataPtr);
}
void Adios2StManColumn::putShortV(uInt rownr, const Short *dataPtr)
{
    putScalarV(rownr, dataPtr);
}
void Adios2StManColumn::putuShortV(uInt rownr, const uShort *dataPtr)
{
    putScalarV(rownr, dataPtr);
}
void Adios2StManColumn::putIntV(uInt rownr, const Int *dataPtr)
{
    putScalarV(rownr, dataPtr);
}
void Adios2StManColumn::putuIntV(uInt rownr, const uInt *dataPtr)
{
    putScalarV(rownr, dataPtr);
}
void Adios2StManColumn::putfloatV(uInt rownr, const Float *dataPtr)
{
    putScalarV(rownr, dataPtr);
}
void Adios2StManColumn::putdoubleV(uInt rownr, const Double *dataPtr)
{
    putScalarV(rownr, dataPtr);
}
void Adios2StManColumn::putComplexV(uInt rownr, const Complex *dataPtr)
{
    putScalarV(rownr, dataPtr);
}
void Adios2StManColumn::putDComplexV(uInt rownr, const DComplex *dataPtr)
{
    putScalarV(rownr, dataPtr);
}
void Adios2StManColumn::putStringV(uInt rownr, const String *dataPtr)
{
    putScalarV(rownr, dataPtr);
}

void Adios2StManColumn::getBoolV(uInt rownr, Bool *dataPtr)
{
    getScalarV(rownr, dataPtr);
}
void Adios2StManColumn::getuCharV(uInt rownr, uChar *dataPtr)
{
    getScalarV(rownr, dataPtr);
}
void Adios2StManColumn::getShortV(uInt rownr, Short *dataPtr)
{
    getScalarV(rownr, dataPtr);
}
void Adios2StManColumn::getuShortV(uInt rownr, uShort *dataPtr)
{
    getScalarV(rownr, dataPtr);
}
void Adios2StManColumn::getIntV(uInt rownr, Int *dataPtr)
{
    getScalarV(rownr, dataPtr);
}
void Adios2StManColumn::getuIntV(uInt rownr, uInt *dataPtr)
{
    getScalarV(rownr, dataPtr);
}
void Adios2StManColumn::getfloatV(uInt rownr, Float *dataPtr)
{
    getScalarV(rownr, dataPtr);
}
void Adios2StManColumn::getdoubleV(uInt rownr, Double *dataPtr)
{
    getScalarV(rownr, dataPtr);
}
void Adios2StManColumn::getComplexV(uInt rownr, Complex *dataPtr)
{
    getScalarV(rownr, dataPtr);
}
void Adios2StManColumn::getDComplexV(uInt rownr, DComplex *dataPtr)
{
    getScalarV(rownr, dataPtr);
}
void Adios2StManColumn::getStringV(uInt rownr, String *dataPtr)
{
    getScalarV(rownr, dataPtr);
}

}
