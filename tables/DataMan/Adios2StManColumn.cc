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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include "Adios2StManColumn.h"

namespace casacore
{

Adios2StManColumn::Adios2StManColumn(
        Adios2StMan::impl *aParent,
        int aDataType,
        String aColName,
        std::shared_ptr<adios2::IO> aAdiosIO)
    :StManColumnBase(aDataType),
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
    for (size_t i = 0; i < aShape.size(); ++i)
    {
        itsAdiosShape[i + 1] = aShape[aShape.size() - i - 1];
        itsAdiosCount[i + 1] = aShape[aShape.size() - i - 1];
        itsAdiosStart[i + 1] = 0;
    }
}

IPosition Adios2StManColumn::shape(rownr_t aRowNr)
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
            return IPosition();
        }
    }
}

Bool Adios2StManColumn::canChangeShape() const
{
    return !isShapeFixed;
}

void Adios2StManColumn::setShape (rownr_t aRowNr, const IPosition& aShape)
{
    itsCasaShapes[aRowNr] = aShape;
}

void Adios2StManColumn::scalarToSelection(rownr_t rownr)
{
    itsAdiosStart[0] = rownr;
    itsAdiosCount[0] = 1;
}

void Adios2StManColumn::scalarColumnVToSelection()
{
    itsAdiosStart[0] = 0;
    itsAdiosCount[0] = itsStManPtr->getNrRows();
}

void Adios2StManColumn::arrayVToSelection(rownr_t rownr)
{
    if(isShapeFixed)
    {
        itsAdiosStart[0] = rownr;
        itsAdiosCount[0] = 1;
        for (size_t i = 1; i < itsAdiosShape.size(); ++i)
        {
            itsAdiosStart[i] = 0;
            itsAdiosCount[i] = itsAdiosShape[i];
        }
    }
    else
    {
        auto casaShape = itsCasaShapes.find(rownr);
        if(casaShape != itsCasaShapes.end())
        {
            itsAdiosShape.resize(casaShape->second.size() + 1);
            itsAdiosStart.resize(casaShape->second.size() + 1);
            itsAdiosCount.resize(casaShape->second.size() + 1);
            itsAdiosStart[0] = rownr;
            itsAdiosCount[0] = 1;
            for (size_t i = 0; i < casaShape->second.size(); ++i)
            {
                itsAdiosShape[i + 1] = casaShape->second[casaShape->second.size() - i - 1];
                itsAdiosCount[i + 1] = casaShape->second[casaShape->second.size() - i - 1];
                itsAdiosStart[i + 1] = 0;
            }
        }
        else
        {
            cerr << "Shape of Row " << rownr << " has not been set" << endl;
        }
    }
}

void Adios2StManColumn::arrayColumnVToSelection()
{
    itsAdiosStart[0] = 0;
    itsAdiosCount[0] = itsStManPtr->getNrRows();
    for (size_t i = 1; i < itsAdiosShape.size(); ++i)
    {
        itsAdiosStart[i] = 0;
        itsAdiosCount[i] = itsAdiosShape[i];
    }
}

void Adios2StManColumn::scalarColumnCellsVToSelection(const RefRows &rows)
{
    RefRowsSliceIter iter(rows);
    auto row_start = iter.sliceStart();
    auto row_end = iter.sliceEnd();
    iter.next();
    if (!iter.pastEnd()) {
        throw std::runtime_error("Adios2StManColumn::scalarColumnCellsVToSelection supports single slices");
    }
    itsAdiosStart[0] = row_start;
    itsAdiosCount[0] = row_end - row_start + 1;
    for (size_t i = 1; i < itsAdiosShape.size(); ++i)
    {
        itsAdiosStart[i] = 0;
        itsAdiosCount[i] = itsAdiosShape[i];
    }
}

void Adios2StManColumn::sliceVToSelection(rownr_t rownr, const Slicer &ns)
{
    columnSliceCellsVToSelection(rownr, 1, ns);
}

void Adios2StManColumn::columnSliceVToSelection(const Slicer &ns)
{
    columnSliceCellsVToSelection(0, itsStManPtr->getNrRows(), ns);
}

void Adios2StManColumn::columnSliceCellsVToSelection(const RefRows &rows, const Slicer &ns)
{
    RefRowsSliceIter iter(rows);
    auto row_start = iter.sliceStart();
    auto row_end = iter.sliceEnd();
    iter.next();
    if (!iter.pastEnd()) {
        throw std::runtime_error("Adios2StManColumn::columnSliceCellsVToSelection supports single slices");
    }
    columnSliceCellsVToSelection(row_start, row_end - row_start + 1, ns);
}

void Adios2StManColumn::columnSliceCellsVToSelection(rownr_t row_start, rownr_t row_count, const Slicer &ns)
{
    itsAdiosStart[0] = row_start;
    itsAdiosCount[0] = row_count;
    for (size_t i = 1; i < itsAdiosShape.size(); ++i)
    {
        itsAdiosStart[i] = ns.start()(ns.ndim() - i);
        itsAdiosCount[i] = ns.length()(ns.ndim() - i);
    }
}

void Adios2StManColumn::putArrayV(rownr_t rownr, const ArrayBase& data)
{
    arrayVToSelection(rownr);
    toAdios(&data);
}

void Adios2StManColumn::getArrayV(rownr_t rownr, ArrayBase& data)
{
    arrayVToSelection(rownr);
    fromAdios(&data);
}

void Adios2StManColumn::putScalar(rownr_t rownr, const void *dataPtr)
{
    scalarToSelection(rownr);
    toAdios(dataPtr);
}

void Adios2StManColumn::getScalar(rownr_t rownr, void *data)
{
    scalarToSelection(rownr);
    fromAdios(data);
}

void Adios2StManColumn::putScalarColumnV(const ArrayBase &data)
{
    scalarColumnVToSelection();
    toAdios(&data);
}

void Adios2StManColumn::getScalarColumnV(ArrayBase &data)
{
    scalarColumnVToSelection();
    fromAdios(&data);
}

void Adios2StManColumn::getScalarColumnCellsV(const RefRows &rownrs, ArrayBase& data)
{
    scalarColumnCellsVToSelection(rownrs);
    fromAdios(&data);
}

void Adios2StManColumn::putScalarColumnCellsV(const RefRows &rownrs, const ArrayBase& data)
{
    scalarColumnCellsVToSelection(rownrs);
    toAdios(&data);
}

Adios2StManColumn::ArrayColumnCellsVIter::ArrayColumnCellsVIter(Adios2StManColumn &column, const RefRows &rownrs)
 : itsColumn(column),
   itsRowsSliceIter(rownrs)
{
    column.itsAdiosStart[0] = 0;
    column.itsAdiosCount[0] = 0;
    for (size_t i = 1; i < column.itsAdiosShape.size(); ++i) {
        column.itsAdiosStart[i] = 0;
        column.itsAdiosCount[i] = column.itsAdiosShape[i];
    }
}

bool Adios2StManColumn::ArrayColumnCellsVIter::finished() const
{
    return itsRowsSliceIter.pastEnd();
}

std::size_t Adios2StManColumn::ArrayColumnCellsVIter::next_offset()
{
    if (itsRowsSliceIter.pastEnd()) {
        throw std::runtime_error("Adios2StManColumn::ArrayColumnCellsViter");
    }
    // adjust user offset _before_ we adjust itsAdiosCount in the column
    itsOffset += itsColumn.itsAdiosCount[0] * itsColumn.itsCasaShape.product();
    auto row_start = itsRowsSliceIter.sliceStart();
    auto row_end = itsRowsSliceIter.sliceEnd();
    itsColumn.itsAdiosStart[0] = row_start;
    itsColumn.itsAdiosCount[0] = row_end - row_start + 1;
    itsRowsSliceIter.next();
    return itsOffset;
}

void Adios2StManColumn::putArrayColumnCellsV (const RefRows& rownrs, const ArrayBase& data)
{
    Bool deleteIt;
    const void *dataPtr = data.getVStorage(deleteIt);
    ArrayColumnCellsVIter iter(*this, rownrs);
    while (!iter.finished()) {
        toAdios(dataPtr, iter.next_offset());
    }
    data.freeVStorage(dataPtr, deleteIt);
}

void Adios2StManColumn::getArrayColumnCellsV (const RefRows& rownrs, ArrayBase &data)
{
    Bool deleteIt;
    void *dataPtr = data.getVStorage(deleteIt);
    ArrayColumnCellsVIter iter(*this, rownrs);
    while (!iter.finished()) {
        fromAdios(dataPtr, iter.next_offset());
    }
    data.putVStorage(dataPtr, deleteIt);
}

void Adios2StManColumn::getSliceV(rownr_t aRowNr, const Slicer &ns, ArrayBase& data)
{
    sliceVToSelection(aRowNr, ns);
    fromAdios(&data);
}

void Adios2StManColumn::putSliceV(rownr_t aRowNr, const Slicer &ns, const ArrayBase& data)
{
    sliceVToSelection(aRowNr, ns);
    toAdios(&data);
}

void Adios2StManColumn::getArrayColumnV(ArrayBase& data)
{
    arrayColumnVToSelection();
    fromAdios(&data);
}

void Adios2StManColumn::putArrayColumnV(const ArrayBase& data)
{
	arrayColumnVToSelection();
    toAdios(&data);
}

void Adios2StManColumn::putColumnSliceV(const Slicer &ns, const ArrayBase& data)
{
    columnSliceVToSelection(ns);
    toAdios(&data);
}

void Adios2StManColumn::getColumnSliceV(const Slicer &ns, ArrayBase& data)
{
    columnSliceVToSelection(ns);
    fromAdios(&data);
}

void Adios2StManColumn::getColumnSliceCellsV(const RefRows& rownrs,
                                  const Slicer& slicer, ArrayBase& data)
{
    columnSliceCellsVToSelection(rownrs, slicer);
    fromAdios(&data);
}

void Adios2StManColumn::putColumnSliceCellsV(const RefRows& rownrs,
                                   const Slicer& slicer, const ArrayBase& data)
{
    columnSliceCellsVToSelection(rownrs, slicer);
    toAdios(&data);
}


#define DEFINE_GETPUT(T) \
void Adios2StManColumn::put ## T(rownr_t rownr, const T *dataPtr) \
{ \
    putScalar(rownr, dataPtr); \
} \
\
void Adios2StManColumn::get ## T(rownr_t rownr, T *dataPtr) \
{ \
    getScalar(rownr, dataPtr); \
}


DEFINE_GETPUT(Bool)
DEFINE_GETPUT(uChar)
DEFINE_GETPUT(Short)
DEFINE_GETPUT(uShort)
DEFINE_GETPUT(Int)
DEFINE_GETPUT(uInt)
DEFINE_GETPUT(float)
DEFINE_GETPUT(double)
DEFINE_GETPUT(Complex)
DEFINE_GETPUT(DComplex)
DEFINE_GETPUT(Int64)
#undef DEFINE_GETPUT


// string

template<>
void Adios2StManColumnT<std::string>::create(std::shared_ptr<adios2::Engine> aAdiosEngine, char /*aOpenMode*/)
{
    itsAdiosEngine = aAdiosEngine;
}

void Adios2StManColumn::putString(rownr_t rownr, const String *dataPtr)
{
    std::string variableName = static_cast<std::string>(itsColumnName) + std::to_string(rownr);
    adios2::Variable<std::string> v = itsAdiosIO->InquireVariable<std::string>(variableName);
    if (!v)
    {
        v = itsAdiosIO->DefineVariable<std::string>(variableName);
    }
    itsAdiosEngine->Put(v, reinterpret_cast<const std::string *>(dataPtr), adios2::Mode::Sync);
}

void Adios2StManColumn::getString(rownr_t rownr, String *dataPtr)
{
    std::string variableName = static_cast<std::string>(itsColumnName) + std::to_string(rownr);
    adios2::Variable<std::string> v = itsAdiosIO->InquireVariable<std::string>(variableName);
    if (v)
    {
        itsAdiosEngine->Get(v, reinterpret_cast<std::string *>(dataPtr), adios2::Mode::Sync);
    }
}

void Adios2StManColumnString::putArrayV(rownr_t rownr, const ArrayBase& data)
{
    String combined;
    Bool deleteIt;
    auto *arrayPtr = reinterpret_cast<const Array<String> *>(&data);
    const String *dataPtr = arrayPtr->getStorage(deleteIt);
    for(auto &i : *arrayPtr)
    {
        combined = combined + i + itsStringArrayBarrier;
    }
    arrayPtr->freeStorage(dataPtr, deleteIt);
    putString(rownr, &combined);
}

void Adios2StManColumnString::getArrayV(rownr_t rownr, ArrayBase& data)
{
    String combined;
    getString(rownr, &combined);
    Bool deleteIt;
    auto *arrayPtr = reinterpret_cast<Array<String> *>(&data);
    String *dataPtr = arrayPtr->getStorage(deleteIt);
    size_t pos = 0;
    for(auto &i : *arrayPtr)
    {
        size_t found = combined.find(itsStringArrayBarrier, pos);
        if(found != std::string::npos)
        {
            i = combined.substr(pos, found - pos);
            pos = found + itsStringArrayBarrier.length();
        }
    }
    arrayPtr->putStorage(dataPtr, deleteIt);
}

void Adios2StManColumnString::getSliceV(rownr_t /*aRowNr*/, const Slicer &/*ns*/, ArrayBase &/*data*/)
{
    throw std::runtime_error("Not implemented yet");
}

void Adios2StManColumnString::putSliceV(rownr_t /*aRowNr*/, const Slicer &/*ns*/, const ArrayBase &/*data*/)
{
    throw std::runtime_error("Not implemented yet");
}

void Adios2StManColumnString::getColumnSliceV(const Slicer &/*ns*/, ArrayBase &/*data*/)
{
    throw std::runtime_error("Not implemented yet");
}

void Adios2StManColumnString::putColumnSliceV(const Slicer &/*ns*/, const ArrayBase &/*data*/)
{
    throw std::runtime_error("Not implemented yet");
}

void Adios2StManColumnString::getColumnSliceCellsV(const RefRows& /*rownrs*/, const Slicer& /*slicer*/, ArrayBase& /*data*/)
{
    throw std::runtime_error("Not implemented yet");
}

void Adios2StManColumnString::putColumnSliceCellsV(const RefRows& /*rownrs*/, const Slicer& /*slicer*/, const ArrayBase& /*data*/)
{
    throw std::runtime_error("Not implemented yet");
}

} // namespace casacore
