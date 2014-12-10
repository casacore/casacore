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
//#
//# $Id$

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

void SSMDirColumn::deleteRow(uInt aRowNr)
{
  char* aValue;
  uInt  aSRow;
  uInt  anERow;
  aValue = itsSSMPtr->find (aRowNr, itsColNr, aSRow, anERow);
  
  if (aRowNr < anERow) {
    // remove from bucket
    // first check if type is a bool
    
    if (dataType() == TpBool) {

      uInt anOffr = (aRowNr-aSRow+1) * itsNrCopy;
      uInt anOfto = (aRowNr-aSRow) * itsNrCopy;
      uInt nr = (anERow-aRowNr) * itsNrCopy;
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

void SSMDirColumn::getArrayBoolV     (uInt aRowNr, 
				      Array<Bool>* aDataPtr)
{
  Bool deleteIt;
  uInt  aStartRow;
  uInt  anEndRow;
  char* aValue;

  Bool* data = aDataPtr->getStorage (deleteIt);

  aValue = itsSSMPtr->find (aRowNr, itsColNr, aStartRow, anEndRow);

  uInt anOff = (aRowNr-aStartRow) * itsNrCopy;

  Conversion::bitToBool(data, aValue+ anOff/8, anOff%8, itsNrCopy);

  aDataPtr->putStorage (data, deleteIt);
}

void SSMDirColumn::getArrayuCharV    (uInt aRowNr, 
				      Array<uChar>* aDataPtr)
{
    Bool deleteIt;
    uChar* data = aDataPtr->getStorage (deleteIt);
    getValue (aRowNr, data);
    aDataPtr->putStorage (data, deleteIt);
}

void SSMDirColumn::getArrayShortV    (uInt aRowNr,
				      Array<Short>* aDataPtr)
{
    Bool deleteIt;
    Short* data = aDataPtr->getStorage (deleteIt);
    getValue (aRowNr, data);
    aDataPtr->putStorage (data, deleteIt);
}

void SSMDirColumn::getArrayuShortV   (uInt aRowNr,
				      Array<uShort>* aDataPtr)
{
    Bool deleteIt;
    uShort* data = aDataPtr->getStorage (deleteIt);
    getValue (aRowNr, data);
    aDataPtr->putStorage (data, deleteIt);
}

void SSMDirColumn::getArrayIntV      (uInt aRowNr,
				      Array<Int>* aDataPtr)
{
    Bool deleteIt;
    Int* data = aDataPtr->getStorage (deleteIt);
    getValue (aRowNr, data);
    aDataPtr->putStorage (data, deleteIt);
}

void SSMDirColumn::getArrayuIntV     (uInt aRowNr,
				      Array<uInt>* aDataPtr)
{
    Bool deleteIt;
    uInt* data = aDataPtr->getStorage (deleteIt);
    getValue (aRowNr, data);
    aDataPtr->putStorage (data, deleteIt);
}

void SSMDirColumn::getArrayfloatV    (uInt aRowNr,
				      Array<float>* aDataPtr)
{
    Bool deleteIt;
    float* data = aDataPtr->getStorage (deleteIt);
    getValue (aRowNr, data);
    aDataPtr->putStorage (data, deleteIt);
}

void SSMDirColumn::getArraydoubleV   (uInt aRowNr,
				      Array<double>* aDataPtr)
{
    Bool deleteIt;
    double* data = aDataPtr->getStorage (deleteIt);
    getValue (aRowNr, data);
    aDataPtr->putStorage (data, deleteIt);
}

void SSMDirColumn::getArrayComplexV  (uInt aRowNr,
				      Array<Complex>* aDataPtr)
{
    Bool deleteIt;
    Complex* data = aDataPtr->getStorage (deleteIt);
    getValue (aRowNr, data);
    aDataPtr->putStorage (data, deleteIt);
}

void SSMDirColumn::getArrayDComplexV (uInt aRowNr,
				      Array<DComplex>* aDataPtr)
{
    Bool deleteIt;
    DComplex* data = aDataPtr->getStorage (deleteIt);
    getValue (aRowNr, data);
    aDataPtr->putStorage (data, deleteIt);
}

void SSMDirColumn::getArrayStringV (uInt aRowNr,
				    Array<String>* aDataPtr)
{
  Int buf[3];
  getRowValue(buf, aRowNr);
  itsSSMPtr->getStringHandler()->get(*aDataPtr, buf[0], buf[1], buf[2],False);
}

void SSMDirColumn::getValue(uInt aRowNr, void* data)
{
  uInt  aStartRow;
  uInt  anEndRow;
  char* aValue;
  aValue = itsSSMPtr->find (aRowNr, itsColNr, aStartRow, anEndRow);
  itsReadFunc (data, aValue+(aRowNr-aStartRow)*itsExternalSizeBytes,
	       itsNrCopy);
}

void SSMDirColumn::putArrayBoolV     (uInt aRowNr,
				      const Array<Bool>* aDataPtr)
{
  Bool deleteIt;
  const Bool* data = aDataPtr->getStorage (deleteIt);

  uInt  aStartRow;
  uInt  anEndRow;
  char* aValue;

  aValue = itsSSMPtr->find (aRowNr, itsColNr, aStartRow, anEndRow);

  uInt anOff = (aRowNr-aStartRow) * itsNrCopy;

  Conversion::boolToBit (aValue + anOff/8, data, anOff%8, itsNrCopy);
  itsSSMPtr->setBucketDirty();
  aDataPtr->freeStorage (data, deleteIt);
}

void SSMDirColumn::putArrayuCharV    (uInt aRowNr,
				      const Array<uChar>* aDataPtr)
{
    Bool deleteIt;
    const uChar* data = aDataPtr->getStorage (deleteIt);
    putValue (aRowNr, data);
    aDataPtr->freeStorage (data, deleteIt);
}

void SSMDirColumn::putArrayShortV    (uInt aRowNr,
				      const Array<Short>* aDataPtr)
{
    Bool deleteIt;
    const Short* data = aDataPtr->getStorage (deleteIt);
    putValue (aRowNr, data);
    aDataPtr->freeStorage (data, deleteIt);
}

void SSMDirColumn::putArrayuShortV   (uInt aRowNr,
				      const Array<uShort>* aDataPtr)
{
    Bool deleteIt;
    const uShort* data = aDataPtr->getStorage (deleteIt);
    putValue (aRowNr, data);
    aDataPtr->freeStorage (data, deleteIt);
}

void SSMDirColumn::putArrayIntV      (uInt aRowNr,
				      const Array<Int>* aDataPtr)
{
    Bool deleteIt;
    const Int* data = aDataPtr->getStorage (deleteIt);
    putValue (aRowNr, data);
    aDataPtr->freeStorage (data, deleteIt);
}

void SSMDirColumn::putArrayuIntV     (uInt aRowNr,
				      const Array<uInt>* aDataPtr)
{
    Bool deleteIt;
    const uInt* data = aDataPtr->getStorage (deleteIt);
    putValue (aRowNr, data);
    aDataPtr->freeStorage (data, deleteIt);
}

void SSMDirColumn::putArrayfloatV    (uInt aRowNr,
				      const Array<float>* aDataPtr)
{
    Bool deleteIt;
    const float* data = aDataPtr->getStorage (deleteIt);
    putValue (aRowNr, data);
    aDataPtr->freeStorage (data, deleteIt);
}

void SSMDirColumn::putArraydoubleV   (uInt aRowNr,
				      const Array<double>* aDataPtr)
{
    Bool deleteIt;
    const double* data = aDataPtr->getStorage (deleteIt);
    putValue (aRowNr, data);
    aDataPtr->freeStorage (data, deleteIt);
}

void SSMDirColumn::putArrayComplexV  (uInt aRowNr,
				      const Array<Complex>* aDataPtr)
{
    Bool deleteIt;
    const Complex* data = aDataPtr->getStorage (deleteIt);
    putValue (aRowNr, data);
    aDataPtr->freeStorage (data, deleteIt);
}

void SSMDirColumn::putArrayDComplexV (uInt aRowNr,
				      const Array<DComplex>* aDataPtr)
{
    Bool deleteIt;
    const DComplex* data = aDataPtr->getStorage (deleteIt);
    putValue (aRowNr, data);
    aDataPtr->freeStorage (data, deleteIt);
}

void SSMDirColumn::putArrayStringV (uInt aRowNr,
				    const Array<String>* aDataPtr)
{
  Int buf[3];
  // Try to find out if this value was filled before, in that case we use
  // an overwrite.
  getRowValue(buf, aRowNr);
  itsSSMPtr->getStringHandler()->put(buf[0], buf[1], buf[2], *aDataPtr,False);
  putValue(aRowNr, buf);

}

} //# NAMESPACE CASACORE - END

