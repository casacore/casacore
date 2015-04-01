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
//#
//# $Id$

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

SSMIndColumn::SSMIndColumn (SSMBase* aParent, int aDataType, uInt aColNr)
: SSMColumn    (aParent, aDataType, aColNr),
  isShapeFixed (False),
  itsIosFile   (0),
  itsIndArray  (0)
{
    init();
}

SSMIndColumn::~SSMIndColumn()
{}

void SSMIndColumn::setMaxLength (uInt)
{}

void SSMIndColumn::doCreate (uInt aNrRows)
{
    // Initialize and create new file.
    itsIosFile = itsSSMPtr->openArrayFile (ByteIO::New);
    addRow(aNrRows,0,False);
}

void SSMIndColumn::getFile (uInt)
{
    // Initialize and open existing file.
    itsIosFile = itsSSMPtr->openArrayFile (itsSSMPtr->fileOption());
}

void SSMIndColumn::addRow (uInt aNewNrRows, uInt anOldNrRows, Bool doInit)
{
  // init the buckets to zero of needed
  if (doInit) {
    uInt aRowNr=0;
    uInt aNrRows=aNewNrRows;

    while (aNrRows > 0) {
      uInt  aStartRow;
      uInt  anEndRow;
      char* aValPtr;
      aValPtr = itsSSMPtr->find (aRowNr, itsColNr, aStartRow, anEndRow);
      aRowNr = anEndRow+1;
      uInt aNr = anEndRow-aStartRow+1;
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
    isShapeFixed   = True;
}

void SSMIndColumn::setShape (uInt aRowNr, const IPosition& aShape)
{
  // Get the current entry. If none, make empty one.
  if (getArrayPtr (aRowNr) == 0) {
    itsIndArray = StIndArray(0);
  }
  // put the new shape (if changed)
  // when changed put the file offset
  if (itsIndArray.setShape (*itsIosFile, dataType(), aShape)) {
    Int64 anOffset = itsIndArray.fileOffset();
    putValue (aRowNr, &anOffset);
  }
}

StIndArray* SSMIndColumn::getArrayPtr (uInt aRowNr)
{
  Int64 anOffset;
  uInt  aStartRow;
  uInt  anEndRow;
  char* aValue;

  aValue = itsSSMPtr->find (aRowNr, itsColNr, aStartRow, anEndRow);
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
StIndArray* SSMIndColumn::getShape (uInt aRowNr)
{
    StIndArray* aPtr = getArrayPtr (aRowNr);
    if (aPtr == 0) {
      throw (DataManInvOper ("SSMIndColumn::getShape: no array in row "+
                             String::toString(aRowNr) + " of column "
                             + columnName()
                             + " in table " + itsSSMPtr->table().tableName()));
    }
    aPtr->getShape (*itsIosFile);
    return aPtr;
}

Bool SSMIndColumn::isShapeDefined (uInt aRowNr)
    { return (getArrayPtr(aRowNr) == 0  ?  False : True); }

uInt SSMIndColumn::ndim (uInt aRowNr)
    { return getShape(aRowNr)->shape().nelements(); }

IPosition SSMIndColumn::shape (uInt aRowNr)
    { return getShape(aRowNr)->shape(); }

Bool SSMIndColumn::canChangeShape() const
    { return (isShapeFixed  ?  False : True); }


Bool SSMIndColumn::canAccessSlice (Bool& reask) const
{
    reask = False;
    return True;
}


void SSMIndColumn::deleteRow(uInt aRowNr)
{
  char* aValue;
  uInt  aSRow;
  uInt  anERow;
  aValue = itsSSMPtr->find (aRowNr, itsColNr, aSRow, anERow);
  
  if (aRowNr < anERow) {
    // remove from bucket
    shiftRows(aValue,aRowNr,aSRow,anERow);
    itsSSMPtr->setBucketDirty();
  }
}

void SSMIndColumn::getArrayStringV (uInt aRowNr, Array<String>* arr)
    { getShape(aRowNr)->getArrayStringV (*itsIosFile, arr); }

void SSMIndColumn::putArrayStringV (uInt aRowNr, const Array<String>* arr)
    { getShape(aRowNr)->putArrayStringV (*itsIosFile, arr); }

void SSMIndColumn::getSliceStringV (uInt aRowNr, const Slicer& ns,
				   Array<String>* arr)
    { getShape(aRowNr)->getSliceStringV (*itsIosFile, ns, arr); }

void SSMIndColumn::putSliceStringV (uInt aRowNr, const Slicer& ns,
				   const Array<String>* arr)
    { getShape(aRowNr)->putSliceStringV (*itsIosFile, ns, arr); }
    

#define SSMIndColumn_GETPUT(T,NM) \
void SSMIndColumn::aips_name2(getArray,NM) (uInt aRowNr, Array<T>* arr) \
    { getShape(aRowNr)->aips_name2(getArray,NM) (*itsIosFile, arr); } \
void SSMIndColumn::aips_name2(putArray,NM) (uInt aRowNr, const Array<T>* arr) \
    { getShape(aRowNr)->aips_name2(putArray,NM) \
	                                                (*itsIosFile, arr); } \
void SSMIndColumn::aips_name2(getSlice,NM) \
                             (uInt aRowNr, const Slicer& ns, Array<T>* arr) \
    { getShape(aRowNr)->aips_name2(getSlice,NM) (*itsIosFile, ns, arr); } \
void SSMIndColumn::aips_name2(putSlice,NM) \
                        (uInt aRowNr, const Slicer& ns, const Array<T>* arr) \
    { getShape(aRowNr)->aips_name2(putSlice,NM) (*itsIosFile, ns, arr); }

SSMIndColumn_GETPUT(Bool,BoolV)
SSMIndColumn_GETPUT(uChar,uCharV)
SSMIndColumn_GETPUT(Short,ShortV)
SSMIndColumn_GETPUT(uShort,uShortV)
SSMIndColumn_GETPUT(Int,IntV)
SSMIndColumn_GETPUT(uInt,uIntV)
SSMIndColumn_GETPUT(float,floatV)
SSMIndColumn_GETPUT(double,doubleV)
SSMIndColumn_GETPUT(Complex,ComplexV)
SSMIndColumn_GETPUT(DComplex,DComplexV)


void SSMIndColumn::init()
{
  DebugAssert (itsNrElem==1, AipsError);
  if (itsSSMPtr->asBigEndian()) {
    itsReadFunc =
            CanonicalConversion::getToLocal(static_cast<Int64*>(0));
    itsWriteFunc =
            CanonicalConversion::getFromLocal(static_cast<Int64*>(0));
    itsExternalSizeBytes =
            CanonicalConversion::canonicalSize(static_cast<Int64*>(0));
  }else{
    itsReadFunc =
            LECanonicalConversion::getToLocal(static_cast<Int64*>(0));
    itsWriteFunc =
            LECanonicalConversion::getFromLocal(static_cast<Int64*>(0));
    itsExternalSizeBytes =
            LECanonicalConversion::canonicalSize(static_cast<Int64*>(0));
  }
  itsNrCopy = 1;
  itsExternalSizeBits = 8*itsExternalSizeBytes;
}

} //# NAMESPACE CASACORE - END

