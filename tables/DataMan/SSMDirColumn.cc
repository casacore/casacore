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

SSMDirColumn::SSMDirColumn (SSMBase* aParent, int aDataType, uInt aColNr)
: SSMColumn   (aParent,aDataType,aColNr)
{
}

SSMDirColumn::~SSMDirColumn()
{
}

void SSMDirColumn::setMaxLength (uInt)
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

      uInt64 anOffr = (aRowNr-aSRow+1) * itsNrCopy;
      uInt64 anOfto = (aRowNr-aSRow) * itsNrCopy;
      uInt64 nr = (anERow-aRowNr) * itsNrCopy;
      Block<Bool> tmp(nr);
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
  Bool deleteIt;
  if (dtype() == TpBool) {
    // Bools need to be converted from bits.
    rownr_t aStartRow;
    rownr_t anEndRow;
    char*   aValue;
    Array<Bool>& arr = static_cast<Array<Bool>&>(aDataPtr);
    Bool* data = arr.getStorage (deleteIt);
    aValue = itsSSMPtr->find (aRowNr, itsColNr, aStartRow, anEndRow,
                              columnName());
    uInt64 anOff = (aRowNr-aStartRow) * itsNrCopy;
    Conversion::bitToBool(data, aValue+ anOff/8, anOff%8, itsNrCopy);
    arr.putStorage (data, deleteIt);
  } else if (dtype() == TpString) {
    // Strings are stored indirectly.
    Int buf[3];
    getRowValue(buf, aRowNr);
    Array<String>& arr = static_cast<Array<String>&>(aDataPtr);
    itsSSMPtr->getStringHandler()->get(arr, buf[0], buf[1], buf[2], False);
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
  Bool deleteIt;
  if (dtype() == TpBool) {
    // Bools need to be converted from bits.
    rownr_t aStartRow;
    rownr_t anEndRow;
    char*   aValue;
    aValue = itsSSMPtr->find (aRowNr, itsColNr, aStartRow, anEndRow,
                              columnName());
    uInt64 anOff = (aRowNr-aStartRow) * itsNrCopy;
    const Array<Bool>& arr = static_cast<const Array<Bool>&>(aDataPtr);
    const Bool* data = arr.getStorage (deleteIt);
    Conversion::boolToBit (aValue + anOff/8, data, anOff%8, itsNrCopy);
    arr.freeStorage (data, deleteIt);
  } else if (dtype() == TpString) {
    // Strings are stored indirectly.
    Int buf[3];
    getRowValue(buf, aRowNr);
    const Array<String>& arr = static_cast<const Array<String>&>(aDataPtr);
    itsSSMPtr->getStringHandler()->put(buf[0], buf[1], buf[2], arr, False);
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

