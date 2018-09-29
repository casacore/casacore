//# Adios2StManColumn.h: A Column in the ADIOS2 Storage Manager
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

#ifndef ADIOS2STMANCOLUMN_H
#define ADIOS2STMANCOLUMN_H

#include "Adios2StMan.h"

#include <casacore/casa/Arrays/Array.h>
#include <casacore/tables/DataMan/StManColumn.h>

namespace casacore
{

class Adios2StManColumn : public StManColumn
{
public:
    Adios2StManColumn(Adios2StMan *aParent, int aDataType, String aColName, std::shared_ptr<adios2::IO> aAdiosIO);

    virtual void create(uInt aNrRows,
                        std::shared_ptr<adios2::Engine> aAdiosEngine,
                        char aOpenMode) = 0;
    virtual void setShapeColumn(const IPosition &aShape);
    virtual IPosition shape(uInt aRowNr);

    int getDataTypeSize();
    int getDataType();
    String getColumnName();

    virtual void putScalarV(uInt aRowNr, const void *aDataPtr) = 0;
    virtual void getScalarV(uInt aRowNr, void *aDataPtr) = 0;

    virtual void putBoolV(uInt aRowNr, const Bool *aDataPtr);
    virtual void putuCharV(uInt aRowNr, const uChar *aDataPtr);
    virtual void putShortV(uInt aRowNr, const Short *aDataPtr);
    virtual void putuShortV(uInt aRowNr, const uShort *aDataPtr);
    virtual void putIntV(uInt aRowNr, const Int *aDataPtr);
    virtual void putuIntV(uInt aRowNr, const uInt *aDataPtr);
    virtual void putfloatV(uInt aRowNr, const Float *aDataPtr);
    virtual void putdoubleV(uInt aRowNr, const Double *aDataPtr);
    virtual void putComplexV(uInt aRowNr, const Complex *aDataPtr);
    virtual void putDComplexV(uInt aRowNr, const DComplex *aDataPtr);
    virtual void putStringV(uInt aRowNr, const String *aDataPtr);

    virtual void getBoolV(uInt aRowNr, Bool *aDataPtr);
    virtual void getuCharV(uInt aRowNr, uChar *aDataPtr);
    virtual void getShortV(uInt aRowNr, Short *aDataPtr);
    virtual void getuShortV(uInt aRowNr, uShort *aDataPtr);
    virtual void getIntV(uInt aRowNr, Int *aDataPtr);
    virtual void getuIntV(uInt aRowNr, uInt *aDataPtr);
    virtual void getfloatV(uInt aRowNr, Float *aDataPtr);
    virtual void getdoubleV(uInt aRowNr, Double *aDataPtr);
    virtual void getComplexV(uInt aRowNr, Complex *aDataPtr);
    virtual void getDComplexV(uInt aRowNr, DComplex *aDataPtr);
    virtual void getStringV(uInt aRowNr, String *aDataPtr);


protected:
    void getArrayWrapper(uint64_t rowStart, uint64_t nrRows, const Slicer &ns,
                         void *dataPtr);

    Adios2StMan *itsStManPtr;

    String itsColumnName;
    char itsColumnType; // 's'-scalar, 'd'-direct array, 'i'-indirect array
    IPosition itsCasaShape;
    int itsDataTypeSize;
    int itsCasaDataType;

    std::shared_ptr<adios2::IO> itsAdiosIO;
    std::shared_ptr<adios2::Engine> itsAdiosEngine;
    std::string itsAdiosDataType;
    adios2::Dims itsAdiosShape;
    adios2::Dims itsAdiosStart;
    adios2::Dims itsAdiosCount;
}; // class Adios2StManColumn


template <class T>
class Adios2StManColumnT : public Adios2StManColumn
{
public:
    Adios2StManColumnT(Adios2StMan *aParent, int aDataType, String aColName, std::shared_ptr<adios2::IO> aAdiosIO)
    : Adios2StManColumn(aParent, aDataType, aColName, aAdiosIO)
    {
    }
    void create(uInt aNrRows, std::shared_ptr<adios2::Engine> aAdiosEngine,
                char aOpenMode)
    {
        itsAdiosShape[0] = aNrRows;
        itsAdiosEngine = aAdiosEngine;
        itsAdiosVariable = itsAdiosIO->InquireVariable<T>(itsColumnName);
        if (!itsAdiosVariable && aOpenMode == 'w')
        {
            itsAdiosVariable = itsAdiosIO->DefineVariable<T>(
                itsColumnName, itsAdiosShape, itsAdiosStart,
                itsAdiosCount);
        }
    }
    virtual void putArrayV(uInt rownr, const void *dataPtr)
    {
        Bool deleteIt;
        itsAdiosStart[0] = rownr;
        itsAdiosVariable.SetSelection(
            {itsAdiosStart, itsAdiosCount});
        const T *data =
            (reinterpret_cast<const Array<T> *>(dataPtr))->getStorage(deleteIt);
        itsAdiosEngine->Put(itsAdiosVariable, data);
        (reinterpret_cast<const Array<T> *>(dataPtr))
            ->freeStorage(reinterpret_cast<const T *&>(data), deleteIt);
    }
    virtual void putScalarV(uInt rownr, const void *dataPtr)
    {
        itsAdiosStart[0] = rownr;
        itsAdiosVariable.SetSelection(
            {itsAdiosStart, itsAdiosCount});
        itsAdiosEngine->Put(itsAdiosVariable,
                            reinterpret_cast<const T *>(dataPtr));
    }
    virtual void getArrayV(uInt aRowNr, void *dataPtr)
    {
        itsAdiosStart[0] = aRowNr;
        itsAdiosCount[0] = 1;
        for (size_t i = 1; i < itsAdiosShape.size(); ++i)
        {
            itsAdiosStart[i] = 0;
            itsAdiosCount[i] = itsAdiosShape[i];
        }
        itsAdiosVariable.SetSelection({itsAdiosStart, itsAdiosCount});
        Bool deleteIt;
        T *data = (reinterpret_cast<Array<T>*>(dataPtr))->getStorage(deleteIt);
        itsAdiosEngine->Get<T>(itsAdiosVariable, data,adios2::Mode::Sync);
        reinterpret_cast<Array<T>*>(dataPtr)->putStorage(reinterpret_cast<T *&>(data), deleteIt);
    }
    virtual void getSliceV(uInt aRowNr, const Slicer &ns, void *dataPtr)
    {
        itsAdiosStart[0] = aRowNr;
        itsAdiosCount[0] = 1;
        for (size_t i = 1; i < itsAdiosShape.size(); ++i)
        {
            itsAdiosStart[i] = ns.start()(i - 1);
            itsAdiosCount[i] = ns.length()(i - 1);
        }
        itsAdiosVariable.SetSelection({itsAdiosStart, itsAdiosCount});
        Bool deleteIt;
        T *data = (reinterpret_cast<Array<T>*>(dataPtr))->getStorage(deleteIt);
        itsAdiosEngine->Get<T>(itsAdiosVariable, data,adios2::Mode::Sync);
        reinterpret_cast<Array<T>*>(dataPtr)->putStorage(reinterpret_cast<T *&>(data), deleteIt);
    }
    virtual void getArrayColumnV(void *dataPtr)
    {
        for(auto &i:itsAdiosStart){
            i=0;
        }
        itsAdiosVariable.SetSelection({itsAdiosStart, itsAdiosShape});
        Bool deleteIt;
        T *data = (reinterpret_cast<Array<T>*>(dataPtr))->getStorage(deleteIt);
        itsAdiosEngine->Get<T>(itsAdiosVariable, data,adios2::Mode::Sync);
        reinterpret_cast<Array<T>*>(dataPtr)->putStorage(reinterpret_cast<T *&>(data), deleteIt);
    }
    virtual void getColumnSliceV(const Slicer &ns, void *dataPtr)
    {
        itsAdiosStart[0] = 0;
        itsAdiosCount[0] = itsAdiosShape[0];
        for (size_t i = 1; i < itsAdiosShape.size(); ++i)
        {
            itsAdiosStart[i] = ns.start()(i - 1);
            itsAdiosCount[i] = ns.length()(i - 1);
        }
        itsAdiosVariable.SetSelection({itsAdiosStart, itsAdiosCount});
        Bool deleteIt;
        T *data = (reinterpret_cast<Array<T>*>(dataPtr))->getStorage(deleteIt);
        itsAdiosEngine->Get<T>(itsAdiosVariable, data,adios2::Mode::Sync);
        reinterpret_cast<Array<T>*>(dataPtr)->putStorage(reinterpret_cast<T *&>(data), deleteIt);
    }
    virtual void getScalarV(uInt aRowNr, void *data)
    {
        itsAdiosStart[0] = aRowNr;
        itsAdiosCount[0] = 1;
        itsAdiosVariable.SetSelection({itsAdiosStart, itsAdiosCount});
        itsAdiosEngine->Get<T>(itsAdiosVariable, reinterpret_cast<T *>(data),
                               adios2::Mode::Sync);
    }

private:
    adios2::Variable<T> itsAdiosVariable;
}; // class Adios2StManColumnT

} // namespace casacore

#endif // ADIOS2STMANCOLUMN_H
