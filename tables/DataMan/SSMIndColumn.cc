//# SSMIndColumn.cc: Column of Standard storage manager for indirect arrays
//# Copyright (C) 2000,2001,2002
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

//# Includes
#include <casacore/tables/DataMan/SSMIndColumn.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/OS/CanonicalConversion.h>
#include <casacore/casa/OS/LECanonicalConversion.h>
#include <casacore/tables/DataMan/DataManError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

SSMIndColumn::SSMIndColumn (SSMBase* aParent, int aDataType, uint32_t aColNr)
: SSMColumn    (aParent, aDataType, aColNr),
  isShapeFixed (false),
  itsIosFile   (0),
  itsIndArray  (0)
{
    init();
}

SSMIndColumn::~SSMIndColumn()
{}

void SSMIndColumn::setMaxLength (uint32_t)
{}

void SSMIndColumn::doCreate (rownr_t aNrRows)
{
    // Initialize and create new file.
    itsIosFile = itsSSMPtr->openArrayFile (ByteIO::New);
    addRow(aNrRows,0,false);
}

void SSMIndColumn::getFile (rownr_t)
{
    // Initialize and open existing file.
    itsIosFile = itsSSMPtr->openArrayFile (itsSSMPtr->fileOption());
}

void SSMIndColumn::addRow (rownr_t aNewNrRows, rownr_t anOldNrRows, bool doInit)
{
  // init the buckets to zero of needed
  if (doInit) {
    rownr_t aRowNr=0;
    rownr_t aNrRows=aNewNrRows;

    while (aNrRows > 0) {
      rownr_t aStartRow;
      rownr_t anEndRow;
      char*   aValPtr;
      aValPtr = itsSSMPtr->find (aRowNr, itsColNr, aStartRow, anEndRow,
                                 columnName());
      aRowNr = anEndRow+1;
      rownr_t aNr = anEndRow-aStartRow+1;
      aNrRows -= aNr;
      memset(aValPtr, 0, aNr * itsExternalSizeBytes);
      itsSSMPtr->setBucketDirty();
    }
  }
  // If the shape is fixed and if the first row is added, define
  // an array to have an array for all rows.
  // Later rows get the value of a previous row, so we don't have to
  // do anything for them.
  if (isShapeFixed) {
    for (; anOldNrRows<aNewNrRows;anOldNrRows++) {
      setShape(anOldNrRows,itsFixedShape);
    }
  }
}

void SSMIndColumn::setShapeColumn (const IPosition& aShape)
{
    itsFixedShape  = aShape;
    isShapeFixed   = true;
}

void SSMIndColumn::setShape (rownr_t aRowNr, const IPosition& aShape)
{
  // Get the current entry. If none, make empty one.
  StIndArray* aPtr = getArrayPtr (aRowNr);
  if (aPtr == 0) {
    itsIndArray = StIndArray(0);
  } else {
    // Note that getArrayPtr sets itsIndArray (which is equal to aPtr).
    aPtr->getShape (*itsIosFile);
  }
  // put the new shape (if changed)
  // when changed put the file offset
  if (itsIndArray.setShape (*itsIosFile, dataType(), aShape)) {
    int64_t anOffset = itsIndArray.fileOffset();
    putValue (aRowNr, &anOffset);
  }
}

StIndArray* SSMIndColumn::getArrayPtr (rownr_t aRowNr)
{
  int64_t   anOffset;
  rownr_t aStartRow;
  rownr_t anEndRow;
  char*   aValue;

  aValue = itsSSMPtr->find (aRowNr, itsColNr, aStartRow, anEndRow,
                            columnName());
  itsReadFunc (&anOffset, aValue+(aRowNr-aStartRow)*itsExternalSizeBytes,
	       itsNrCopy);


  if (anOffset != 0) {
    itsIndArray = StIndArray (anOffset);
    return &itsIndArray;
  }else{
    return 0;
  }
}

//# Get the shape for the array (if any) in the given row.
//# Read shape if not read yet.
StIndArray* SSMIndColumn::getShape (rownr_t aRowNr)
{
    StIndArray* aPtr = getArrayPtr (aRowNr);
    if (aPtr == 0) {
      throw DataManInvOper ("SSMIndColumn::getShape: no array in row "+
                            String::toString(aRowNr) + " in column "
                            + columnName()
                            + " of table " + itsSSMPtr->table().tableName());
    }
    aPtr->getShape (*itsIosFile);
    return aPtr;
}

bool SSMIndColumn::isShapeDefined (rownr_t aRowNr)
    { return (getArrayPtr(aRowNr) == 0  ?  false : true); }

uint32_t SSMIndColumn::ndim (rownr_t aRowNr)
    { return getShape(aRowNr)->shape().nelements(); }

IPosition SSMIndColumn::shape (rownr_t aRowNr)
    { return getShape(aRowNr)->shape(); }

bool SSMIndColumn::canChangeShape() const
    { return (isShapeFixed  ?  false : true); }


void SSMIndColumn::deleteRow(rownr_t aRowNr)
{
  char*   aValue;
  rownr_t aSRow;
  rownr_t anERow;
  aValue = itsSSMPtr->find (aRowNr, itsColNr, aSRow, anERow, columnName());
  
  if (aRowNr < anERow) {
    // remove from bucket
    shiftRows(aValue,aRowNr,aSRow,anERow);
    itsSSMPtr->setBucketDirty();
  }
}

void SSMIndColumn::getArrayV (rownr_t aRowNr, ArrayBase& arr)
{
  getShape(aRowNr)->getArrayV (*itsIosFile, arr, dtype());
}

void SSMIndColumn::putArrayV (rownr_t aRowNr, const ArrayBase& arr)
{
  getShape(aRowNr)->putArrayV (*itsIosFile, arr, dtype());
}

void SSMIndColumn::getSliceV (rownr_t aRowNr, const Slicer& ns,
                              ArrayBase& arr)
{
  getShape(aRowNr)->getSliceV (*itsIosFile, ns, arr, dtype());
}

void SSMIndColumn::putSliceV (rownr_t aRowNr, const Slicer& ns,
                              const ArrayBase& arr)
{
  getShape(aRowNr)->putSliceV (*itsIosFile, ns, arr, dtype());
}
    

void SSMIndColumn::init()
{
  DebugAssert (itsNrElem==1, AipsError);
  if (itsSSMPtr->asBigEndian()) {
    itsReadFunc =
            CanonicalConversion::getToLocal(static_cast<int64_t*>(0));
    itsWriteFunc =
            CanonicalConversion::getFromLocal(static_cast<int64_t*>(0));
    itsExternalSizeBytes =
            CanonicalConversion::canonicalSize(static_cast<int64_t*>(0));
  }else{
    itsReadFunc =
            LECanonicalConversion::getToLocal(static_cast<int64_t*>(0));
    itsWriteFunc =
            LECanonicalConversion::getFromLocal(static_cast<int64_t*>(0));
    itsExternalSizeBytes =
            LECanonicalConversion::canonicalSize(static_cast<int64_t*>(0));
  }
  itsNrCopy = 1;
  itsExternalSizeBits = 8*itsExternalSizeBytes;
}

} //# NAMESPACE CASACORE - END

