//# LofarColumn.h: A Column in the LOFAR Storage Manager
//# Copyright (C) 2009
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

#include <tables/Tables/LofarColumn.h>
#include <tables/Tables/DataManError.h>
#include <casa/Arrays/Array.h>

namespace casa { //# NAMESPACE CASA - BEGIN

  Bool LofarColumn::isWritable() const
  {
    return False;
  }
  void LofarColumn::setShapeColumn (const IPosition&)
  {}


  void Ant1Column::getIntV (uInt rownr, Int* dataPtr)
  {
    // Fill ColumnCache object.
    const Block<Int>& ants = itsParent->ant1();
    columnCache().setIncrement (1);
    uInt strow = rownr / ants.size() * ants.size();
    columnCache().setIncrement (1);
    columnCache().set (strow, strow + ants.size() - 1, ants.storage());
    *dataPtr = ants[rownr-strow];
  }

  void Ant2Column::getIntV (uInt rownr, Int* dataPtr)
  {
    // Fill ColumnCache object.
    const Block<Int>& ants = itsParent->ant2();
    uInt strow = rownr / ants.size() * ants.size();
    columnCache().setIncrement (1);
    columnCache().set (strow, strow + ants.size() - 1, ants.storage());
    *dataPtr = ants[rownr-strow];
  }

  void TimeColumn::getdoubleV (uInt rownr, Double* dataPtr)
  {
    // Get time of the block containing this row.
    uInt nrbasel = itsParent->ant1().size();
    uInt blnr = rownr / nrbasel;
    itsValue = itsParent->time (blnr);
    // Fill ColumnCache object.
    uInt strow = blnr * nrbasel;
    columnCache().setIncrement (0);
    columnCache().set (strow, strow + nrbasel - 1, &itsValue);
    *dataPtr = itsValue;
  }

  void IntervalColumn::getdoubleV (uInt rownr, Double* dataPtr)
  {
    itsValue = itsParent->interval();
    columnCache().setIncrement (0);
    columnCache().set (0, itsParent->getNRow()-1, &itsValue);
    *dataPtr = itsValue;
  }

  void ZeroColumn::getIntV (uInt rownr, Int* dataPtr)
  {
    itsValue = 0;
    columnCache().setIncrement (0);
    columnCache().set (0, itsParent->getNRow()-1, &itsValue);
    *dataPtr = 0;
  }

  void FalseColumn::getBoolV (uInt rownr, Bool* dataPtr)
  {
    itsValue = False;
    columnCache().setIncrement (0);
    columnCache().set (0, itsParent->getNRow()-1, &itsValue);
    *dataPtr = 0;
  }

  IPosition UvwColumn::shape (uInt)
  {
    return IPosition(1,3);
  }
  void UvwColumn::getArraydoubleV (uInt rownr, Array<Double>* dataPtr)
  {
    Array<Double>::iterator iterend = dataPtr->end();
    for (Array<Double>::iterator iter=dataPtr->begin();
         iter != iterend; ++iter) {
      *iter = 0.;
    }
  }

  Bool DataColumn::isWritable() const
  {
    return True;
  }
  IPosition DataColumn::shape (uInt)
  {
    return IPosition(2, itsParent->npol(), itsParent->nchan());
  }
  void DataColumn::getArrayComplexV (uInt rownr, Array<Complex>* dataPtr)
  {
    Bool deleteIt;
    Complex* data = dataPtr->getStorage(deleteIt);
    itsParent->getData (rownr, data);
    dataPtr->putStorage (data, deleteIt);
  }
  void DataColumn::putArrayComplexV (uInt rownr, const Array<Complex>* dataPtr)
  {
    Bool deleteIt;
    const Complex* data = dataPtr->getStorage(deleteIt);
    itsParent->putData (rownr, data);
    dataPtr->freeStorage (data, deleteIt);
  }

  IPosition FlagColumn::shape (uInt)
  {
    return IPosition(2, itsParent->npol(), itsParent->nchan());
  }
  void FlagColumn::getArrayBoolV (uInt rownr, Array<Bool>* dataPtr)
  {
    uInt npol = itsParent->npol();
    const uShort* data = itsParent->getNSample (rownr, False);
    const uShort* dataEnd = data + itsParent->nchan();
    if (dataPtr->contiguousStorage()) {
      for (Array<Bool>::contiter iter=dataPtr->cbegin();
           data<dataEnd; ++data) {
        Bool flagged = (*data == 0);
        for (uInt i=0; i<npol; ++i, ++iter) {
          *iter = flagged;
        }
      }
    } else {
      for (Array<Bool>::iterator iter=dataPtr->begin();
           data<dataEnd; ++data, ++iter) {
        Bool flagged = (*data == 0);
        for (uInt i=0; i<npol; ++i, ++iter) {
          *iter = flagged;
        }
      }
    }
  }

  IPosition WeightColumn::shape (uInt)
  {
    return IPosition(1, itsParent->nchan());
  }
  void WeightColumn::getArrayfloatV (uInt rownr, Array<Float>* dataPtr)
  {
    Double maxn = itsParent->maxnSample();
    const uShort* data = itsParent->getNSample (rownr, True);
    const uShort* dataEnd = data + dataPtr->size();
    if (dataPtr->contiguousStorage()) {
      for (Array<Float>::contiter iter=dataPtr->cbegin();
           data<dataEnd; ++data, ++iter) {
        *iter = *data / maxn;
      }
    } else {
      for (Array<Float>::iterator iter=dataPtr->begin();
           data<dataEnd; ++data, ++iter) {
        *iter = *data / maxn;
      }
    }
  }

  IPosition SigmaColumn::shape (uInt)
  {
    return IPosition(1, itsParent->nchan());
  }
  void SigmaColumn::getArrayfloatV (uInt rownr, Array<Float>* dataPtr)
  {
    Array<Float>::iterator iterend = dataPtr->end();
    for (Array<Float>::iterator iter=dataPtr->begin();
         iter != iterend; ++iter) {
      *iter = 0.;
    }
  }

  Bool FlagCatColumn::isShapeDefined (uInt)
  {
    return False;
  }
  IPosition FlagCatColumn::shape (uInt)
  {
    throw DataManError ("LofarStMan: no data in column FLAG_CATEGORY");
  }

} //# NAMESPACE CASA - END
