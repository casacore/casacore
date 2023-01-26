//# SSMDirColumn.cc: a Direct Array Column of the Standard Storage Manager
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

#include <casacore/tables/DataMan/SSMDirColumn.h>
#include <casacore/tables/DataMan/SSMStringHandler.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Utilities/ValType.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

SSMDirColumn::SSMDirColumn (SSMBase* aParent, int aDataType, uint32_t aColNr)
: SSMColumn   (aParent,aDataType,aColNr)
{
}

SSMDirColumn::~SSMDirColumn()
{
}

void SSMDirColumn::setMaxLength (uint32_t)
{}

void SSMDirColumn::deleteRow(rownr_t aRowNr)
{
  char* aValue;
  rownr_t  aSRow;
  rownr_t  anERow;
  aValue = itsSSMPtr->find (aRowNr, itsColNr, aSRow, anERow, columnName());
  
  if (aRowNr < anERow) {
    // remove from bucket
    // first check if type is a bool
    
    if (dataType() == TpBool) {

      uint64_t anOffr = (aRowNr-aSRow+1) * itsNrCopy;
      uint64_t anOfto = (aRowNr-aSRow) * itsNrCopy;
      uint64_t nr = (anERow-aRowNr) * itsNrCopy;
      Block<bool> tmp(nr);
      Conversion::bitToBool (tmp.storage(), aValue + anOffr/8,
			     anOffr%8, nr);
      Conversion::boolToBit (aValue + anOfto/8, tmp.storage(),
			     anOfto%8, nr);
    } else {
      // remove from bucket
      shiftRows(aValue,aRowNr,aSRow,anERow);
    }
    itsSSMPtr->setBucketDirty();
  }
}

void SSMDirColumn::getArrayV (rownr_t aRowNr, ArrayBase& aDataPtr)
{
  bool deleteIt;
  if (dtype() == TpBool) {
    // Bools need to be converted from bits.
    rownr_t aStartRow;
    rownr_t anEndRow;
    char*   aValue;
    Array<bool>& arr = static_cast<Array<bool>&>(aDataPtr);
    bool* data = arr.getStorage (deleteIt);
    aValue = itsSSMPtr->find (aRowNr, itsColNr, aStartRow, anEndRow,
                              columnName());
    uint64_t anOff = (aRowNr-aStartRow) * itsNrCopy;
    Conversion::bitToBool(data, aValue+ anOff/8, anOff%8, itsNrCopy);
    arr.putStorage (data, deleteIt);
  } else if (dtype() == TpString) {
    // Strings are stored indirectly.
    int32_t buf[3];
    getRowValue(buf, aRowNr);
    Array<String>& arr = static_cast<Array<String>&>(aDataPtr);
    itsSSMPtr->getStringHandler()->get(arr, buf[0], buf[1], buf[2], false);
  } else {
    // Other types can be handled directly.
    void* data = aDataPtr.getVStorage (deleteIt);
    getValue (aRowNr, data);
    aDataPtr.putVStorage (data, deleteIt);
  }
}

void SSMDirColumn::getValue(rownr_t aRowNr, void* data)
{
  rownr_t aStartRow;
  rownr_t anEndRow;
  char*   aValue;
  aValue = itsSSMPtr->find (aRowNr, itsColNr, aStartRow, anEndRow,
                            columnName());
  itsReadFunc (data, aValue+(aRowNr-aStartRow)*itsExternalSizeBytes,
	       itsNrCopy);
}

void SSMDirColumn::putArrayV (rownr_t aRowNr, const ArrayBase& aDataPtr)
{
  bool deleteIt;
  if (dtype() == TpBool) {
    // Bools need to be converted from bits.
    rownr_t aStartRow;
    rownr_t anEndRow;
    char*   aValue;
    aValue = itsSSMPtr->find (aRowNr, itsColNr, aStartRow, anEndRow,
                              columnName());
    uint64_t anOff = (aRowNr-aStartRow) * itsNrCopy;
    const Array<bool>& arr = static_cast<const Array<bool>&>(aDataPtr);
    const bool* data = arr.getStorage (deleteIt);
    Conversion::boolToBit (aValue + anOff/8, data, anOff%8, itsNrCopy);
    arr.freeStorage (data, deleteIt);
  } else if (dtype() == TpString) {
    // Strings are stored indirectly.
    int32_t buf[3];
    getRowValue(buf, aRowNr);
    const Array<String>& arr = static_cast<const Array<String>&>(aDataPtr);
    itsSSMPtr->getStringHandler()->put(buf[0], buf[1], buf[2], arr, false);
    putValue(aRowNr, buf);
  } else {
    // Other types can be handled directly.
    const void* data = aDataPtr.getVStorage (deleteIt);
    putValue (aRowNr, data);
    aDataPtr.freeVStorage (data, deleteIt);
  }
  itsSSMPtr->setBucketDirty();
}


} //# NAMESPACE CASACORE - END

