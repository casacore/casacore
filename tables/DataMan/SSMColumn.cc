//# SSMColumn.cc: The Column of the Standard Storage Manager
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

#include <casacore/tables/DataMan/SSMColumn.h>
#include <casacore/tables/DataMan/SSMBase.h>
#include <casacore/tables/DataMan/SSMStringHandler.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/Copy.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/OS/CanonicalConversion.h>
#include <casacore/casa/OS/LECanonicalConversion.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

SSMColumn::SSMColumn (SSMBase* aParent, int aDataType, uInt aColNr)
: StManColumn    (aDataType),
  itsSSMPtr      (aParent),
  itsExternalSizeBytes(0),
  itsExternalSizeBits (0),
  itsColNr       (aColNr),
  itsMaxLen      (0),
  itsNrElem      (1),
  itsNrCopy      (0),
  itsData        (0)
{
  init();
}

SSMColumn::~SSMColumn()
{
  delete [] static_cast<char*>(itsData);
}

void SSMColumn::setShapeColumn (const IPosition& aShape)
{
    itsNrElem = aShape.product();
    itsShape  = aShape;
    init();
}

void SSMColumn::setMaxLength (uInt maxLength)
{
    itsMaxLen = maxLength;
    init();
}

uInt SSMColumn::ndim (uInt)
{
    return itsShape.nelements();
}

IPosition SSMColumn::shape (uInt)
{
    return itsShape;
}

void SSMColumn::doCreate(uInt)
{
}

void SSMColumn::getFile(uInt)
{
}

void SSMColumn::addRow (uInt aNewNrRows, uInt, Bool doInit)
{
  if (doInit  &&  dataType() == TpString) {
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
}

void SSMColumn::deleteRow(uInt aRowNr)
{
  char* aValue;
  uInt  aSRow;
  uInt  anERow;
  int aDT = dataType();

  if (aDT == TpString  &&  itsMaxLen == 0) {
    Int buf[3];
    getRowValue(buf, aRowNr);
    if (buf[2] > 8 ) {
      itsSSMPtr->getStringHandler()->remove(buf[0], buf[1], buf[2]);
      aValue = itsSSMPtr->find (aRowNr, itsColNr, aSRow, anERow);
      shiftRows(aValue,aRowNr,aSRow,anERow);
      itsSSMPtr->setBucketDirty();
      return;
    }
  }

  aValue = itsSSMPtr->find (aRowNr, itsColNr, aSRow, anERow);

  // For bools be sure that cache is actual
  Bool isBool = (aDT == TpBool);

  if (isBool  && aRowNr < anERow) {
    Bool aVal;
    getBoolV(aRowNr,&aVal);
  }

  // first check if aRowNr is in cache, if not, fill cache
  // In both cases remove row from cache
  uInt  aStartRow = columnCache().start();
  uInt  anEndRow  = columnCache().end();

  // Remove from cache if needed
  if (aRowNr >= aStartRow  &&  aRowNr <= anEndRow) {
    //  remove the row in itsData if not last
    if (aRowNr < anEndRow) {
      char* aToPtr  = getDataPtr() + (aRowNr-aStartRow)
	* itsLocalSize;
      char* aFromPtr = getDataPtr() + (aRowNr+1-aStartRow)
	* itsLocalSize;

      // decrement anEndrow
      uInt aLength = (anEndRow - aRowNr) * itsLocalSize;
      memmove(aToPtr,aFromPtr,aLength);
    }
    // Fill cache again with actual itsData.
    if (aStartRow == anEndRow) {
      columnCache().invalidate();
    } else {
      columnCache().set (aStartRow, anEndRow-1, getDataPtr());
    }
  }
  
  if (aRowNr < anERow) {
    // remove from bucket

    // first check if type is a bool
    if (isBool) {
      itsWriteFunc (aValue,itsData, (anERow-aSRow) * itsNrCopy);
    } else {
      shiftRows(aValue,aRowNr,aSRow,anERow);
    }
    itsSSMPtr->setBucketDirty();
  }
}

void SSMColumn::shiftRows(char* aValue, uInt aRowNr, uInt aSRow, uInt anERow)
{
  // Shift from aRrowNr on 1 to the left.
  char* aToPtr = aValue + (aRowNr-aSRow) * itsExternalSizeBytes;
  char* aFromPtr = aToPtr + itsExternalSizeBytes;
  uInt aLength = (anERow - aRowNr) * itsExternalSizeBytes;
  memmove(aToPtr,aFromPtr,aLength);
  // Clear last entry (so a putString on a new row finds zeroes).
  memset (aToPtr + aLength, 0, itsExternalSizeBytes);
}


void SSMColumn::getBoolV (uInt aRowNr, Bool* aValue)
{
  getValue(aRowNr);
  *aValue = static_cast<Bool*>(itsData)[aRowNr-columnCache().start()];
}
void SSMColumn::getuCharV (uInt aRowNr, uChar* aValue)
{
  getValue(aRowNr);
  *aValue = static_cast<uChar*>(itsData)[aRowNr-columnCache().start()];
}
void SSMColumn::getShortV (uInt aRowNr, Short* aValue)
{
  getValue(aRowNr);
  *aValue = static_cast<Short*>(itsData)[aRowNr-columnCache().start()];
}
void SSMColumn::getuShortV (uInt aRowNr, uShort* aValue)
{
  getValue(aRowNr);
  *aValue = static_cast<uShort*>(itsData)[aRowNr-columnCache().start()];
}
void SSMColumn::getIntV (uInt aRowNr, Int* aValue)
{
  getValue(aRowNr);
  *aValue = static_cast<Int*>(itsData)[aRowNr-columnCache().start()];
}
void SSMColumn::getuIntV (uInt aRowNr, uInt* aValue)
{
  getValue(aRowNr);
  *aValue = static_cast<uInt*>(itsData)[aRowNr-columnCache().start()];
}
void SSMColumn::getfloatV (uInt aRowNr, float* aValue)
{
  getValue(aRowNr);
  *aValue = static_cast<float*>(itsData)[aRowNr-columnCache().start()];
}
void SSMColumn::getdoubleV (uInt aRowNr, double* aValue)
{
  getValue(aRowNr);
  *aValue = static_cast<double*>(itsData)[aRowNr-columnCache().start()];
}
void SSMColumn::getComplexV (uInt aRowNr, Complex* aValue)
{
  getValue(aRowNr);
  *aValue = static_cast<Complex*>(itsData)[aRowNr-columnCache().start()];
}

void SSMColumn::getDComplexV (uInt aRowNr,DComplex* aValue)
{
  getValue(aRowNr);
  *aValue = static_cast<DComplex*>(itsData)[aRowNr-columnCache().start()];
}

void SSMColumn::getStringV (uInt aRowNr, String* aValue)
{
  if (itsMaxLen > 0) {
    // Allocate the maximum number of characters needed
    // The +1 is to correct for the incorrect use of the chars() function
    // Should be changed to use real Char*
    aValue->alloc(itsMaxLen+1);
    char* sp = const_cast<char*>(aValue->chars());
    uInt  aStartRow;
    uInt  anEndRow;
    char* buf = itsSSMPtr->find (aRowNr, itsColNr, aStartRow, anEndRow);
    itsReadFunc (sp, buf+(aRowNr-aStartRow)*itsExternalSizeBytes,
		 itsNrCopy);
    // Append a trailing zero (in case needed).
    // Note that if shorter, the string already contains a trailing zero.
    // Set the string to its actual length.
    sp[itsMaxLen] = '\0';
    uInt len = 0;
    while (*sp++ != '\0') {
      len++;
    }
    aValue->alloc(len);
  } else {

    // The string is probably stored indirectly in a string bucket.
    // Get bucketnr, offset, and length.
    Int buf[3];
    char* strbuf = getRowValue(buf, aRowNr);

    // if length <= 8 chars the string can be found in de data bucket
    // instead of the string bucket.

    if (buf[2] <= 8) {
      aValue->resize (buf[2]);       // resize storage which adds trailing 0
      char* sp = &((*aValue)[0]);    // get actual string
      memcpy (sp, strbuf, buf[2]);
#ifdef USE_OLD_STRING
      sp[buf[2]] = '\0';
#endif
    } else {
      itsSSMPtr->getStringHandler()->get(*aValue, buf[0], buf[1], buf[2]);
    }
  }
}

Char* SSMColumn::getRowValue(Int* data, uInt aRowNr)
{
  uInt  aStartRow;
  uInt  anEndRow;
  char* aValue;
  aValue = itsSSMPtr->find (aRowNr, itsColNr, aStartRow, anEndRow);
  itsReadFunc (data, aValue+(aRowNr-aStartRow)*itsExternalSizeBytes,
	       itsNrCopy);
  return aValue+(aRowNr-aStartRow)*itsExternalSizeBytes;
}

void SSMColumn::getValue(uInt aRowNr)
{
  if (aRowNr < columnCache().start()  ||  aRowNr > columnCache().end()) {
    uInt  aStartRow;
    uInt  anEndRow;
    char* aValue;
    aValue = itsSSMPtr->find (aRowNr, itsColNr, aStartRow, anEndRow);
    itsReadFunc (getDataPtr(), aValue, (anEndRow-aStartRow+1) * itsNrCopy);
    columnCache().set (aStartRow, anEndRow, getDataPtr());
  }
}

void SSMColumn::putBoolV (uInt aRowNr, const Bool* aValue)
{
  uInt  aStartRow;
  uInt  anEndRow;
  char* aDummy;

  aDummy = itsSSMPtr->find (aRowNr, itsColNr, aStartRow, anEndRow);

  uInt anOff    = aRowNr-aStartRow;

  Conversion::boolToBit(aDummy+(anOff/8),
			aValue,anOff%8,1);
  itsSSMPtr->setBucketDirty();

  if (aRowNr >= columnCache().start()  &&  aRowNr <= columnCache().end()) {
    getDataPtr()[aRowNr-columnCache().start()] = 
      *aValue;
  }
}
void SSMColumn::putuCharV (uInt aRowNr, const uChar* aValue)
{
  putValue(aRowNr,aValue);
  if (aRowNr >= columnCache().start()  &&  aRowNr <= columnCache().end()) {
    static_cast<uChar*>(itsData)[aRowNr-columnCache().start()] = 
      *aValue;
  }
}
void SSMColumn::putShortV (uInt aRowNr, const Short* aValue)
{
  putValue(aRowNr,aValue);
  if (aRowNr >= columnCache().start()  &&  aRowNr <= columnCache().end()) {
    static_cast<Short*>(itsData)[aRowNr-columnCache().start()] = 
      *aValue;
  }
}
void SSMColumn::putuShortV (uInt aRowNr, const uShort* aValue)
{
  putValue(aRowNr,aValue);
  if (aRowNr >= columnCache().start()  &&  aRowNr <= columnCache().end()) {
    static_cast<uShort*>(itsData)[aRowNr-columnCache().start()] = 
      *aValue;
  }
}
void SSMColumn::putIntV (uInt aRowNr, const Int* aValue)
{
  putValue(aRowNr,aValue);
  if (aRowNr >= columnCache().start()  &&  aRowNr <= columnCache().end()) {
    static_cast<Int*>(itsData)[aRowNr-columnCache().start()] = 
      *aValue;
  }
}
void SSMColumn::putuIntV (uInt aRowNr, const uInt* aValue)
{
  putValue(aRowNr,aValue);
  if (aRowNr >= columnCache().start()  &&  aRowNr <= columnCache().end()) {
    static_cast<uInt*>(itsData)[aRowNr-columnCache().start()] = 
      *aValue;
  }
}
void SSMColumn::putfloatV (uInt aRowNr, const float* aValue)
{
  putValue(aRowNr,aValue);
  if (aRowNr >= columnCache().start()  &&  aRowNr <= columnCache().end()) {
    static_cast<float*>(itsData)[aRowNr-columnCache().start()] = 
      *aValue;
  }
}
void SSMColumn::putdoubleV (uInt aRowNr, const double* aValue)
{
  putValue(aRowNr,aValue);
  if (aRowNr >= columnCache().start()  &&  aRowNr <= columnCache().end()) {
    static_cast<double*>(itsData)[aRowNr-columnCache().start()] = 
      *aValue;
  }
}
void SSMColumn::putComplexV (uInt aRowNr, const Complex* aValue)
{
  putValue(aRowNr,aValue);
  if (aRowNr >= columnCache().start()  &&  aRowNr <= columnCache().end()) {
    static_cast<Complex*>(itsData)[aRowNr-columnCache().start()] = 
      *aValue;
  }
}

void SSMColumn::putDComplexV (uInt aRowNr, const DComplex* aValue)
{
  putValue(aRowNr,aValue);
  if (aRowNr >= columnCache().start()  &&  aRowNr <= columnCache().end()) {
    static_cast<DComplex*>(itsData)[aRowNr-columnCache().start()] = 
      *aValue;
  }
}

void SSMColumn::putStringV (uInt aRowNr, const String* aValue)
{
  // Fixed length strings are written directly.
  if (itsMaxLen > 0) {
    uInt  aStartRow;
    uInt  anEndRow;
    char* aDummy = itsSSMPtr->find (aRowNr, itsColNr, aStartRow, anEndRow);
    itsWriteFunc (aDummy+(aRowNr-aStartRow)*itsExternalSizeBytes,
		  aValue->chars(), min(itsMaxLen, aValue->length()+1));
    itsSSMPtr->setBucketDirty();
  } else { 

    Int buf[3];
    // Try to find out if this value was filled before, in that case we use
    // an overwrite.
    getRowValue(buf, aRowNr);

    // if String <= 8 chars it is written into the data bucket
    // instead of the string bucket.
    if (aValue->length() <= 8) {
      // if string was written before, but longer then 8 chars it has to 
      // be removed from the stringbucket
      if (buf[2] > 8 ) {
	itsSSMPtr->getStringHandler()->remove(buf[0], buf[1], buf[2]);
      }
      buf[2] = aValue->length();
      putValueShortString (aRowNr, buf, *aValue);
    } else {
      // Maybe it was there earlier, but smaller.
      if (buf[2] <= 8) {
	buf[0] = 0;
	buf[1] = 0;
	buf[2] = 0;
      }
      itsSSMPtr->getStringHandler()->put (buf[0], buf[1], buf[2], *aValue);
      putValue (aRowNr, buf);
    }
  }
}

void SSMColumn::putValue(uInt aRowNr, const void* aValue)
{
  uInt  aStartRow;
  uInt  anEndRow;
  char* aDummy = itsSSMPtr->find (aRowNr, itsColNr, aStartRow, anEndRow);
  itsWriteFunc (aDummy+(aRowNr-aStartRow)*itsExternalSizeBytes,
  		aValue, itsNrCopy);
  itsSSMPtr->setBucketDirty();
}

void SSMColumn::putValueShortString(uInt aRowNr, const void* aValue,
				    const String& string)
{
  uInt  aStartRow;
  uInt  anEndRow;
  char* aDummy;

  aDummy = itsSSMPtr->find (aRowNr, itsColNr, aStartRow, anEndRow);

  itsWriteFunc (aDummy+(aRowNr-aStartRow)*itsExternalSizeBytes,
  		aValue, itsNrCopy);
  memcpy (aDummy+(aRowNr-aStartRow)*itsExternalSizeBytes, string.chars(),
	  string.length());
  itsSSMPtr->setBucketDirty();
}

void SSMColumn::getScalarColumnBoolV     (Vector<Bool>* aDataPtr)
{
  Bool deleteIt;
  Bool* anArray=aDataPtr->getStorage(deleteIt);
  getColumnValue(anArray,aDataPtr->nelements());
  aDataPtr->putStorage(anArray,deleteIt);
}

void SSMColumn::getScalarColumnuCharV    (Vector<uChar>* aDataPtr)
{
  Bool deleteIt;
  uChar* anArray=aDataPtr->getStorage(deleteIt);
  getColumnValue(anArray,aDataPtr->nelements());
  aDataPtr->putStorage(anArray,deleteIt);
}

void SSMColumn::getScalarColumnShortV    (Vector<Short>* aDataPtr)
{
  Bool deleteIt;
  Short* anArray=aDataPtr->getStorage(deleteIt);
  getColumnValue(anArray,aDataPtr->nelements());
  aDataPtr->putStorage(anArray,deleteIt);

}

void SSMColumn::getScalarColumnuShortV   (Vector<uShort>* aDataPtr)
{
  Bool deleteIt;
  uShort* anArray=aDataPtr->getStorage(deleteIt);
  getColumnValue(anArray,aDataPtr->nelements());
  aDataPtr->putStorage(anArray,deleteIt);
}

void SSMColumn::getScalarColumnIntV      (Vector<Int>* aDataPtr)
{
  Bool deleteIt;
  Int* anArray=aDataPtr->getStorage(deleteIt);
  getColumnValue(anArray,aDataPtr->nelements());
  aDataPtr->putStorage(anArray,deleteIt);
}

void SSMColumn::getScalarColumnuIntV     (Vector<uInt>* aDataPtr)
{
  Bool deleteIt;
  uInt* anArray=aDataPtr->getStorage(deleteIt);
  getColumnValue(anArray,aDataPtr->nelements());
  aDataPtr->putStorage(anArray,deleteIt);
}

void SSMColumn::getScalarColumnfloatV    (Vector<float>* aDataPtr)
{
  Bool deleteIt;
  float* anArray=aDataPtr->getStorage(deleteIt);
  getColumnValue(anArray,aDataPtr->nelements());
  aDataPtr->putStorage(anArray,deleteIt);
}

void SSMColumn::getScalarColumndoubleV   (Vector<double>* aDataPtr)
{
  Bool deleteIt;
  double* anArray=aDataPtr->getStorage(deleteIt);
  getColumnValue(anArray,aDataPtr->nelements());
  aDataPtr->putStorage(anArray,deleteIt);
}

void SSMColumn::getScalarColumnComplexV  (Vector<Complex>* aDataPtr)
{
  Bool deleteIt;
  Complex* anArray=aDataPtr->getStorage(deleteIt);
  getColumnValue(anArray,aDataPtr->nelements());
  aDataPtr->putStorage(anArray,deleteIt);
}

void SSMColumn::getScalarColumnDComplexV (Vector<DComplex>* aDataPtr)
{
  Bool deleteIt;
  DComplex* anArray=aDataPtr->getStorage(deleteIt);
  getColumnValue(anArray,aDataPtr->nelements());
  aDataPtr->putStorage(anArray,deleteIt);
}

void SSMColumn::getScalarColumnStringV (Vector<String>* aDataPtr)
{
  for (uInt i=0;i<aDataPtr->nelements(); i++) {
    getStringV(i,&(*aDataPtr)(i));
  }
}

void SSMColumn::getColumnValue(void* anArray,uInt aNrRows)
{
  char* aDataPtr = static_cast<char*>(anArray);
  uInt aRowNr=0;
  Int rowsToDo = aNrRows;
  
  while (rowsToDo > 0) {
    uInt  aStartRow;
    uInt  anEndRow;
    char* aValue;
    aValue = itsSSMPtr->find (aRowNr, itsColNr, aStartRow, anEndRow);
    aRowNr = anEndRow+1;
    uInt aNr = anEndRow-aStartRow+1;
    rowsToDo -= aNr;
    itsReadFunc (aDataPtr, aValue, aNr * itsNrCopy);
    aDataPtr += aNr * itsLocalSize;
  }
}

void SSMColumn::putScalarColumnBoolV     (const Vector<Bool>* aDataPtr)
{
  Bool deleteIt;
  const Bool* anArray=aDataPtr->getStorage(deleteIt);
  putColumnValue(anArray,aDataPtr->nelements());
  aDataPtr->freeStorage(anArray,deleteIt);
}

void SSMColumn::putScalarColumnuCharV    (const Vector<uChar>* aDataPtr)
{
  Bool deleteIt;
  const uChar* anArray=aDataPtr->getStorage(deleteIt);
  putColumnValue(anArray,aDataPtr->nelements());
  aDataPtr->freeStorage(anArray,deleteIt);
}

void SSMColumn::putScalarColumnShortV    (const Vector<Short>* aDataPtr)
{
  Bool deleteIt;
  const Short* anArray=aDataPtr->getStorage(deleteIt);
  putColumnValue(anArray,aDataPtr->nelements());
  aDataPtr->freeStorage(anArray,deleteIt);
}

void SSMColumn::putScalarColumnuShortV   (const Vector<uShort>* aDataPtr)
{
  Bool deleteIt;
  const uShort* anArray=aDataPtr->getStorage(deleteIt);
  putColumnValue(anArray,aDataPtr->nelements());
  aDataPtr->freeStorage(anArray,deleteIt);
}

void SSMColumn::putScalarColumnIntV      (const Vector<Int>* aDataPtr)
{
  Bool deleteIt;
  const Int* anArray=aDataPtr->getStorage(deleteIt);
  putColumnValue(anArray,aDataPtr->nelements());
  aDataPtr->freeStorage(anArray,deleteIt);
}

void SSMColumn::putScalarColumnuIntV     (const Vector<uInt>* aDataPtr)
{
  Bool deleteIt;
  const uInt* anArray=aDataPtr->getStorage(deleteIt);
  putColumnValue(anArray,aDataPtr->nelements());
  aDataPtr->freeStorage(anArray,deleteIt);
}

void SSMColumn::putScalarColumnfloatV    (const Vector<float>* aDataPtr)
{
  Bool deleteIt;
  const float* anArray=aDataPtr->getStorage(deleteIt);
  putColumnValue(anArray,aDataPtr->nelements());
  aDataPtr->freeStorage(anArray,deleteIt);
}

void SSMColumn::putScalarColumndoubleV   (const Vector<double>* aDataPtr)
{
  Bool deleteIt;
  const double* anArray=aDataPtr->getStorage(deleteIt);
  putColumnValue(anArray,aDataPtr->nelements());
  aDataPtr->freeStorage(anArray,deleteIt);
}

void SSMColumn::putScalarColumnComplexV  (const Vector<Complex>* aDataPtr)
{
  Bool deleteIt;
  const Complex* anArray=aDataPtr->getStorage(deleteIt);
  putColumnValue(anArray,aDataPtr->nelements());
  aDataPtr->freeStorage(anArray,deleteIt);
}

void SSMColumn::putScalarColumnDComplexV (const Vector<DComplex>* aDataPtr)
{
  Bool deleteIt;
  const DComplex* anArray=aDataPtr->getStorage(deleteIt);
  putColumnValue(anArray,aDataPtr->nelements());
  aDataPtr->freeStorage(anArray,deleteIt);
}

void SSMColumn::putScalarColumnStringV (const Vector<String>* aDataPtr)
{
  for (uInt i=0;i<aDataPtr->nelements(); i++) {
    putStringV(i,&(*aDataPtr)(i));
  }
}

void SSMColumn::putColumnValue(const void* anArray,uInt aNrRows)
{
  const char* aDataPtr = static_cast<const char*>(anArray);
  uInt aRowNr=0;
  Int rowsToDo=aNrRows;

  while (rowsToDo > 0) {
    uInt  aStartRow;
    uInt  anEndRow;
    char* aValPtr;
    aValPtr = itsSSMPtr->find (aRowNr, itsColNr, aStartRow, anEndRow);
    aRowNr = anEndRow+1;
    uInt aNr = anEndRow-aStartRow+1;
    rowsToDo -= aNr;
    itsWriteFunc (aValPtr, aDataPtr, aNr * itsNrCopy);
    aDataPtr += aNr * itsLocalSize;
    itsSSMPtr->setBucketDirty();
  }

  // Be sure cache will be emptied
  columnCache().invalidate();
}

void SSMColumn::removeColumn()
{
  if (dataType() == TpString  &&  itsMaxLen == 0) {
    Int buf[3];
    for (uInt i=0;i<itsSSMPtr->getNRow();i++) {
      getRowValue(buf, i);
      if (buf[2] > 8 ) {
	itsSSMPtr->getStringHandler()->remove(buf[0], buf[1], buf[2]);
      }
    }
  }
}
  
void SSMColumn::init()
{
  DataType aDT = static_cast<DataType>(dataType());
  itsLocalSize = ValType::getTypeSize(aDT);
  Bool asBigEndian = itsSSMPtr->asBigEndian();
  itsNrCopy = itsNrElem;
  if (aDT == TpString) {
    // Fixed length strings are written directly.
    if (itsMaxLen > 0) {
      itsNrCopy = itsMaxLen;
      itsLocalSize = itsNrCopy;
      itsExternalSizeBytes = itsNrCopy;
      itsReadFunc = itsWriteFunc = Conversion::valueCopy;
    } else {
      // Variable length strings are written indirectly.
      // They have 3 Ints (bucketnr, offset, length) telling where
      // the strings are.
      itsNrCopy=1;
      itsLocalSize = ValType::getTypeSize(TpInt);
      itsExternalSizeBytes = ValType::getCanonicalSize (TpInt, asBigEndian);
      uInt aNRel;
      ValType::getCanonicalFunc (TpInt, itsReadFunc, itsWriteFunc, aNRel,
				 asBigEndian);
      itsNrCopy *= aNRel;
      itsExternalSizeBytes *= 3;
      itsLocalSize         *= 3;
      itsNrCopy            *= 3;
    }
    itsExternalSizeBits   = 8*itsExternalSizeBytes;
  } else if (aDT == TpBool) {
    itsExternalSizeBytes = (itsNrElem + 7) / 8;
    itsExternalSizeBits  = itsNrElem;
    itsReadFunc  = &Conversion::bitToBool;
    itsWriteFunc = &Conversion::boolToBit;
  } else {
    itsExternalSizeBytes = ValType::getCanonicalSize (aDT, asBigEndian);
    uInt aNRel;
    ValType::getCanonicalFunc (aDT, itsReadFunc, itsWriteFunc, aNRel,
			       asBigEndian);
    itsNrCopy *= aNRel;
    itsExternalSizeBytes *= itsNrElem;
    itsLocalSize         *= itsNrElem;
    itsExternalSizeBits   = 8*itsExternalSizeBytes;
  }
}

void SSMColumn::resync (uInt)
{
    // Invalidate the last value read.
    columnCache().invalidate();
}

} //# NAMESPACE CASACORE - END

