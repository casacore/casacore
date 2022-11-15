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

#ifndef ADIOS2STMANCOLUMN_H
#define ADIOS2STMANCOLUMN_H

#include <unordered_map>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/tables/DataMan/StManColumnBase.h>
#include <casacore/tables/Tables/RefRows.h>

#include "Adios2StManImpl.h"


namespace casacore
{

class Adios2StManColumn : public StManColumnBase
{
public:
    Adios2StManColumn(Adios2StMan::impl *aParent, int aDataType, String aColName, std::shared_ptr<adios2::IO> aAdiosIO);

    virtual void create(std::shared_ptr<adios2::Engine> aAdiosEngine,
                        char aOpenMode) = 0;
    virtual void setShapeColumn(const IPosition &aShape) override;
    virtual IPosition shape(rownr_t aRowNr) override;
    Bool canChangeShape() const override;
    void setShape (rownr_t aRowNr, const IPosition& aShape) override;

    int getDataTypeSize();
    int getDataType();
    String getColumnName();

protected:

    // scalar get/put
    virtual void getBool(rownr_t aRowNr, Bool *aDataPtr) override;
    virtual void getuChar(rownr_t aRowNr, uChar *aDataPtr) override;
    virtual void getShort(rownr_t aRowNr, Short *aDataPtr) override;
    virtual void getuShort(rownr_t aRowNr, uShort *aDataPtr) override;
    virtual void getInt(rownr_t aRowNr, Int *aDataPtr) override;
    virtual void getuInt(rownr_t aRowNr, uInt *aDataPtr) override;
    virtual void getInt64(rownr_t aRowNr, Int64 *aDataPtr) override;
    virtual void getfloat(rownr_t aRowNr, Float *aDataPtr) override;
    virtual void getdouble(rownr_t aRowNr, Double *aDataPtr) override;
    virtual void getComplex(rownr_t aRowNr, Complex *aDataPtr) override;
    virtual void getDComplex(rownr_t aRowNr, DComplex *aDataPtr) override;
    virtual void getString(rownr_t aRowNr, String *aDataPtr) override;

    virtual void putBool(rownr_t aRowNr, const Bool *aDataPtr) override;
    virtual void putuChar(rownr_t aRowNr, const uChar *aDataPtr) override;
    virtual void putShort(rownr_t aRowNr, const Short *aDataPtr) override;
    virtual void putuShort(rownr_t aRowNr, const uShort *aDataPtr) override;
    virtual void putInt(rownr_t aRowNr, const Int *aDataPtr) override;
    virtual void putuInt(rownr_t aRowNr, const uInt *aDataPtr) override;
    virtual void putInt64(rownr_t aRowNr, const Int64 *aDataPtr) override;
    virtual void putfloat(rownr_t aRowNr, const Float *aDataPtr) override;
    virtual void putdouble(rownr_t aRowNr, const Double *aDataPtr) override;
    virtual void putComplex(rownr_t aRowNr, const Complex *aDataPtr) override;
    virtual void putDComplex(rownr_t aRowNr, const DComplex *aDataPtr) override;
    virtual void putString(rownr_t aRowNr, const String *aDataPtr) override;

    // The rest of the get and put functions
    virtual void getScalarColumnV (ArrayBase& dataPtr) override;
    virtual void putScalarColumnV (const ArrayBase& dataPtr) override;
    virtual void getScalarColumnCellsV (const RefRows& rownrs,
                        ArrayBase& dataPtr) override;
    virtual void putScalarColumnCellsV (const RefRows& rownrs,
                        const ArrayBase& dataPtr) override;
    virtual void getArrayV (rownr_t rownr, ArrayBase& dataPtr) override;
    virtual void putArrayV (rownr_t rownr, const ArrayBase& data) override;
    virtual void getArrayColumnV (ArrayBase& data) override;
    virtual void putArrayColumnV (const ArrayBase& data) override;
    virtual void getArrayColumnCellsV (const RefRows& rownrs,
                    ArrayBase& data) override;
    virtual void putArrayColumnCellsV (const RefRows& rownrs,
                       const ArrayBase& data) override;
    virtual void getSliceV (rownr_t rownr, const Slicer& slicer, ArrayBase& data) override;
    virtual void putSliceV (rownr_t rownr, const Slicer& slicer,
                       const ArrayBase& data) override;
    virtual void getColumnSliceV (const Slicer& slicer, ArrayBase& data) override;
    virtual void putColumnSliceV (const Slicer& slicer, const ArrayBase& data) override;
    virtual void getColumnSliceCellsV (const RefRows& rownrs,
                       const Slicer& slicer, ArrayBase& data) override;
    virtual void putColumnSliceCellsV (const RefRows& rownrs,
                       const Slicer& slicer,
                       const ArrayBase& data) override;


private:
    void putScalar(rownr_t rownr, const void *dataPtr);
    void getScalar(rownr_t rownr, void *dataPtr);
    virtual void toAdios(const ArrayBase *arrayPtr) = 0;
    virtual void fromAdios(ArrayBase *arrayPtr) = 0;
    virtual void toAdios(const void *dataPtr, std::size_t offset=0) = 0;
    virtual void fromAdios(void *dataPtr, std::size_t offset=0) = 0;


protected:
    void scalarToSelection(rownr_t rownr);
    void scalarColumnVToSelection();
    void scalarColumnCellsVToSelection(const RefRows &rownrs);
    void arrayVToSelection(rownr_t rownr);
    void arrayColumnVToSelection();
    void sliceVToSelection(rownr_t rownr, const Slicer &ns);
    void columnSliceVToSelection(const Slicer &ns);
    void columnSliceCellsVToSelection(const RefRows &rows, const Slicer &ns);
    void columnSliceCellsVToSelection(rownr_t row_start, rownr_t row_end, const Slicer &ns);

    Adios2StMan::impl *itsStManPtr;

    String itsColumnName;
    IPosition itsCasaShape;
    std::unordered_map<rownr_t, IPosition> itsCasaShapes;
    Bool isShapeFixed = false;

    std::shared_ptr<adios2::IO> itsAdiosIO;
    std::shared_ptr<adios2::Engine> itsAdiosEngine;
    std::string itsAdiosDataType;
    adios2::Dims itsAdiosShape = {std::numeric_limits<rownr_t>::max()};
    adios2::Dims itsAdiosStart = {0};
    adios2::Dims itsAdiosCount = {1};
}; // class Adios2StManColumn


template <class T>
class Adios2StManColumnT : public Adios2StManColumn
{
public:

    using Adios2StManColumn::Adios2StManColumn;

    void create(std::shared_ptr<adios2::Engine> aAdiosEngine, char aOpenMode)
    {
        itsAdiosEngine = aAdiosEngine;
        itsAdiosVariable = itsAdiosIO->InquireVariable<T>(itsColumnName);
        if (!itsAdiosVariable && aOpenMode == 'w')
        {
            itsAdiosVariable = itsAdiosIO->DefineVariable<T>(
                itsColumnName,
                itsAdiosShape,
                itsAdiosStart,
                itsAdiosCount);
        }
    }

private:
    adios2::Variable<T> itsAdiosVariable;

    void toAdios(const void *data, std::size_t offset)
    {
        const T *tData = static_cast<const T *>(data);
        if(!isShapeFixed)
            itsAdiosVariable.SetShape(itsAdiosShape);
        itsAdiosVariable.SetSelection({itsAdiosStart, itsAdiosCount});
        itsAdiosEngine->Put<T>(itsAdiosVariable, tData + offset, adios2::Mode::Sync);
    }

    void fromAdios(void *data, std::size_t offset)
    {
        T *tData = static_cast<T *>(data);
        itsAdiosVariable.SetSelection({itsAdiosStart, itsAdiosCount});
        itsAdiosEngine->Get<T>(itsAdiosVariable, tData + offset, adios2::Mode::Sync);
    }

    void toAdios(const ArrayBase *arrayPtr)
    {
        Bool deleteIt;
        const void *data = arrayPtr->getVStorage(deleteIt);
        toAdios(data, 0);
        arrayPtr->freeVStorage (data, deleteIt);
    }

    void fromAdios(ArrayBase *arrayPtr)
    {
        Bool deleteIt;
        void *data = arrayPtr->getVStorage(deleteIt);
        fromAdios(data, 0);
        arrayPtr->putVStorage(data, deleteIt);
    }

}; // class Adios2StManColumnT

class Adios2StManColumnString : public Adios2StManColumnT<std::string>
{
public:
	using Adios2StManColumnT::Adios2StManColumnT;

protected:
	void putArrayV(rownr_t rownr, const ArrayBase& data);
	void getArrayV(rownr_t rownr, ArrayBase& data);
	void getSliceV(rownr_t /*aRowNr*/, const Slicer &/*ns*/, ArrayBase &/*data*/);
	void putSliceV(rownr_t /*aRowNr*/, const Slicer &/*ns*/, const ArrayBase &/*data*/);
	void getColumnSliceV(const Slicer &/*ns*/, ArrayBase &/*data*/);
	void putColumnSliceV(const Slicer &/*ns*/, const ArrayBase &/*data*/);
	void getColumnSliceCellsV(const RefRows& /*rownrs*/, const Slicer& /*slicer*/, ArrayBase& /*data*/);
	void putColumnSliceCellsV(const RefRows& /*rownrs*/, const Slicer& /*slicer*/, const ArrayBase& /*data*/);

private:
    const String itsStringArrayBarrier = "ADIOS2BARRIER";
}; // class Adios2StManColumnString

} // namespace casacore

#endif // ADIOS2STMANCOLUMN_H
