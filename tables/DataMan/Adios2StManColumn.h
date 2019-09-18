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

#include <unordered_map>
#include <numeric>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/tables/DataMan/StManColumn.h>
#include <casacore/tables/Tables/RefRows.h>

#include "Adios2StManImpl.h"


namespace casacore
{

class Adios2StManColumn : public StManColumn
{
public:
    Adios2StManColumn(Adios2StMan::impl *aParent, int aDataType, String aColName, std::shared_ptr<adios2::IO> aAdiosIO);

    virtual Bool canAccessSlice (Bool& reask) const { reask = false; return true; };
    virtual Bool canAccessColumnSlice (Bool& reask) const { reask = false; return true; };

    virtual void create(std::shared_ptr<adios2::Engine> aAdiosEngine,
                        char aOpenMode) = 0;
    virtual void setShapeColumn(const IPosition &aShape);
    virtual IPosition shape(uInt aRowNr);
    Bool canChangeShape() const;
    void setShape (uInt aRowNr, const IPosition& aShape);

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
    virtual void putInt64V(uInt aRowNr, const Int64 *aDataPtr);
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
    virtual void getInt64V(uInt aRowNr, Int64 *aDataPtr);
    virtual void getfloatV(uInt aRowNr, Float *aDataPtr);
    virtual void getdoubleV(uInt aRowNr, Double *aDataPtr);
    virtual void getComplexV(uInt aRowNr, Complex *aDataPtr);
    virtual void getDComplexV(uInt aRowNr, DComplex *aDataPtr);
    virtual void getStringV(uInt aRowNr, String *aDataPtr);

    virtual void putSliceBoolV(uInt rownr, const Slicer& ns, const Array<Bool>* dataPtr);
    virtual void putSliceuCharV(uInt rownr, const Slicer& ns, const Array<uChar>* dataPtr);
    virtual void putSliceShortV(uInt rownr, const Slicer& ns, const Array<Short>* dataPtr);
    virtual void putSliceuShortV(uInt rownr, const Slicer& ns, const Array<uShort>* dataPtr);
    virtual void putSliceIntV(uInt rownr, const Slicer& ns, const Array<Int>* dataPtr);
    virtual void putSliceuIntV(uInt rownr, const Slicer& ns, const Array<uInt>* dataPtr);
    virtual void putSlicefloatV(uInt rownr, const Slicer& ns, const Array<float>* dataPtr);
    virtual void putSlicedoubleV(uInt rownr, const Slicer& ns, const Array<double>* dataPtr);
    virtual void putSliceComplexV(uInt rownr, const Slicer& ns, const Array<Complex>* dataPtr);
    virtual void putSliceDComplexV(uInt rownr, const Slicer& ns, const Array<DComplex>* dataPtr);
    virtual void putSliceStringV(uInt rownr, const Slicer& ns, const Array<String>* dataPtr);

    virtual void getSliceBoolV(uInt rownr, const Slicer& ns, Array<Bool>* dataPtr);
    virtual void getSliceuCharV(uInt rownr, const Slicer& ns, Array<uChar>* dataPtr);
    virtual void getSliceShortV(uInt rownr, const Slicer& ns, Array<Short>* dataPtr);
    virtual void getSliceuShortV(uInt rownr, const Slicer& ns, Array<uShort>* dataPtr);
    virtual void getSliceIntV(uInt rownr, const Slicer& ns, Array<Int>* dataPtr);
    virtual void getSliceuIntV(uInt rownr, const Slicer& ns, Array<uInt>* dataPtr);
    virtual void getSlicefloatV(uInt rownr, const Slicer& ns, Array<float>* dataPtr);
    virtual void getSlicedoubleV(uInt rownr, const Slicer& ns, Array<double>* dataPtr);
    virtual void getSliceComplexV(uInt rownr, const Slicer& ns, Array<Complex>* dataPtr);
    virtual void getSliceDComplexV(uInt rownr, const Slicer& ns, Array<DComplex>* dataPtr);
    virtual void getSliceStringV(uInt rownr, const Slicer& ns, Array<String>* dataPtr);


protected:
    void scalarVToSelection(uInt rownr);
    void arrayVToSelection(uInt rownr);
    void sliceVToSelection(uInt rownr, const Slicer &ns);
    void columnSliceVToSelection(const Slicer &ns);
    void columnSliceCellsVToSelection(const RefRows &rows, const Slicer &ns);
    void columnSliceCellsVToSelection(uInt row_start, uInt row_end, const Slicer &ns);

    Adios2StMan::impl *itsStManPtr;

    String itsColumnName;
    IPosition itsCasaShape;
    std::unordered_map<uInt, IPosition> itsCasaShapes;
    Bool isShapeFixed = false;

    std::shared_ptr<adios2::IO> itsAdiosIO;
    std::shared_ptr<adios2::Engine> itsAdiosEngine;
    char itsAdiosOpenMode;
    std::string itsAdiosDataType;
    adios2::Dims itsAdiosShape = {std::numeric_limits<uInt>::max()};
    adios2::Dims itsAdiosStart = {0};
    adios2::Dims itsAdiosCount = {1};
}; // class Adios2StManColumn


template <class T>
class Adios2StManColumnT : public Adios2StManColumn
{
public:

    Adios2StManColumnT(
            Adios2StMan::impl *aParent,
            int aDataType,
            String aColName,
            std::shared_ptr<adios2::IO> aAdiosIO)
    : Adios2StManColumn(aParent, aDataType, aColName, aAdiosIO)
    {
    }

    void create(std::shared_ptr<adios2::Engine> aAdiosEngine, char aOpenMode)
    {
        itsAdiosEngine = aAdiosEngine;
        itsAdiosOpenMode = aOpenMode;
        itsAdiosVariable = itsAdiosIO->InquireVariable<T>(itsColumnName);
        if(aOpenMode == 'w')
        {
            if (!itsAdiosVariable)
            {
                itsAdiosVariable = itsAdiosIO->DefineVariable<T>(
                        itsColumnName,
                        itsAdiosShape,
                        itsAdiosStart,
                        itsAdiosCount);
            }
        }
        else if(aOpenMode == 'r')
        {
            size_t cacheSize = std::accumulate(
                    itsAdiosShape.begin() + 1,
                    itsAdiosShape.end(),
                    itsReadCacheMaxRows,
                    std::multiplies<size_t>());
            itsReadCache.resize(cacheSize);
        }
        itsArraySize = std::accumulate(
                itsAdiosShape.begin() + 1,
                itsAdiosShape.end(),
                1,
                std::multiplies<size_t>());
    }

    virtual void putArrayV(uInt rownr, const void *dataPtr)
    {
        arrayVToSelection(rownr);
        toAdios(dataPtr);
    }

    virtual void getArrayV(uInt rownr, void *dataPtr)
    {
        arrayVToSelection(rownr);
        if(itsReadCacheMaxRows > 0)
        {
            if(itsAdiosStart[0] < itsReadCacheStartRow or itsAdiosStart[0] >= itsReadCacheStartRow + itsReadCacheRows)
            {
                itsAdiosCount[0] = itsReadCacheMaxRows;
                itsReadCacheRows = itsReadCacheMaxRows;
                fromAdios(itsReadCache.data());
            }
            Bool deleteIt;
            auto *arrayPtr = asArrayPtr(dataPtr);
            T *data = arrayPtr->getStorage(deleteIt);
            size_t index = itsArraySize * (itsAdiosStart[0] - itsReadCacheStartRow);
            size_t length = sizeof(T) * itsArraySize;
            std::memcpy(data, itsReadCache.data() + index, length);
            arrayPtr->putStorage(data, deleteIt);
        }
        else
        {
            fromAdios(dataPtr);
        }
    }

    virtual void putScalarV(uInt rownr, const void *dataPtr)
    {
        scalarVToSelection(rownr);
        toAdios(reinterpret_cast<const T *>(dataPtr));
    }

    virtual void getScalarV(uInt aRowNr, void *data)
    {
        scalarVToSelection(aRowNr);
        fromAdios(reinterpret_cast<T *>(data));
    }

    virtual void putArrayColumnCellsV (const RefRows& rownrs, const void* dataPtr)
    {
        if(rownrs.isSliced())
        {
            rownrs.convert();
        }
        Bool deleteIt;
        auto *arrayPtr = asArrayPtr(dataPtr);
        const T *data = arrayPtr->getStorage(deleteIt);
        itsAdiosCount[0] = 1;
        for (size_t i = 1; i < itsAdiosShape.size(); ++i)
        {
            itsAdiosStart[i] = 0;
            itsAdiosCount[i] = itsAdiosShape[i];
        }
        for(uInt i = 0; i < rownrs.rowVector().size(); ++i)
        {
            itsAdiosStart[0] = rownrs.rowVector()[i];
            toAdios(data + i * itsCasaShape.nelements());
        }
        arrayPtr->freeStorage(data, deleteIt);
    }

    virtual void getArrayColumnCellsV (const RefRows& rownrs, void* dataPtr)
    {
        if(rownrs.isSliced())
        {
            rownrs.convert();
        }
        Bool deleteIt;
        auto *arrayPtr = asArrayPtr(dataPtr);
        T *data = arrayPtr->getStorage(deleteIt);
        itsAdiosCount[0] = 1;
        for (size_t i = 1; i < itsAdiosShape.size(); ++i)
        {
            itsAdiosStart[i] = 0;
            itsAdiosCount[i] = itsAdiosShape[i];
        }
        for(uInt i = 0; i < rownrs.rowVector().size(); ++i)
        {
            itsAdiosStart[0] = rownrs.rowVector()[i];
            fromAdios(data + i * itsCasaShape.nelements());
        }
        arrayPtr->putStorage(data, deleteIt);
    }

    virtual void getSliceV(uInt aRowNr, const Slicer &ns, void *dataPtr)
    {
        sliceVToSelection(aRowNr, ns);
        fromAdios(dataPtr);
    }

    virtual void putSliceV(uInt aRowNr, const Slicer &ns, const void *dataPtr)
    {
        sliceVToSelection(aRowNr, ns);
        toAdios(dataPtr);
    }

    virtual void getArrayColumnV(void *dataPtr)
    {
        for(auto &i:itsAdiosStart){
            i=0;
        }
        fromAdios(dataPtr);
    }

    virtual void putColumnSliceV(const Slicer &ns, const void *dataPtr)
    {
        columnSliceVToSelection(ns);
        toAdios(dataPtr);
    }

    virtual void getColumnSliceV(const Slicer &ns, void *dataPtr)
    {
        columnSliceVToSelection(ns);
        fromAdios(dataPtr);
    }

    virtual void getColumnSliceCellsV(const RefRows& rownrs,
                                      const Slicer& slicer, void* dataPtr)
    {
        columnSliceCellsVToSelection(rownrs, slicer);
        fromAdios(dataPtr);
    }

    virtual void putColumnSliceCellsV (const RefRows& rownrs,
                                       const Slicer& slicer, const void* dataPtr)
    {
        columnSliceCellsVToSelection(rownrs, slicer);
        toAdios(dataPtr);
    }

private:
    const String itsStringArrayBarrier = "ADIOS2BARRIER";
    adios2::Variable<T> itsAdiosVariable;
    size_t itsReadCacheStartRow = 0;
    size_t itsReadCacheRows = 0;
    size_t itsReadCacheMaxRows = 1000;
    size_t itsArraySize;
    std::vector<T> itsReadCache;

    Array<T> *asArrayPtr(void *dataPtr) const
    {
        return reinterpret_cast<Array<T>*>(dataPtr);
    }

    const Array<T> *asArrayPtr(const void *dataPtr) const
    {
        return reinterpret_cast<const Array<T>*>(dataPtr);
    }

    void toAdios(const T *data)
    {
        itsAdiosVariable.SetSelection({itsAdiosStart, itsAdiosCount});
        itsAdiosEngine->Put<T>(itsAdiosVariable, data, adios2::Mode::Sync);
    }

    void fromAdios(T *data)
    {
        itsAdiosVariable.SetSelection({itsAdiosStart, itsAdiosCount});
        itsAdiosEngine->Get<T>(itsAdiosVariable, data, adios2::Mode::Sync);
    }

    void toAdios(const void *dataPtr)
    {
        Bool deleteIt;
        auto *arrayPtr = asArrayPtr(dataPtr);
        const T *data = arrayPtr->getStorage(deleteIt);
        toAdios(data);
        arrayPtr->freeStorage (data, deleteIt);
    }

    void fromAdios(void *dataPtr)
    {
        Bool deleteIt;
        auto *arrayPtr = asArrayPtr(dataPtr);
        T *data = arrayPtr->getStorage(deleteIt);
        fromAdios(data);
        arrayPtr->putStorage(data, deleteIt);
    }

}; // class Adios2StManColumnT

} // namespace casacore

#endif // ADIOS2STMANCOLUMN_H
