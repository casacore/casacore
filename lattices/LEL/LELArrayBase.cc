//# LELArrayBase.cc: Base class for LELArray holding the mask
//# Copyright (C) 1999
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


#include <casacore/lattices/LEL/LELArrayBase.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LELArrayBase::LELArrayBase (const LELArrayBase& other)
: itsMaskPtr (0)
{
    operator= (other);
}

LELArrayBase::~LELArrayBase()
{
    delete itsMaskPtr;
}

LELArrayBase& LELArrayBase::operator= (const LELArrayBase& other) 
{
    if (this != &other) {
	delete itsMaskPtr;
	itsMaskPtr = 0;
	if (other.itsMaskPtr != 0) {
	    itsMaskPtr = new Array<Bool> (*other.itsMaskPtr);
	}
    }
    return *this;
}

void LELArrayBase::removeMask()
{
    delete itsMaskPtr;
    itsMaskPtr = 0;
}

void LELArrayBase::setMask (const Array<Bool>& mask)
{
    delete itsMaskPtr;
    itsMaskPtr = new Array<Bool> (mask);
}

void LELArrayBase::setMask (Array<Bool>& mask)
{
    delete itsMaskPtr;
    itsMaskPtr = new Array<Bool> (mask);
}

void LELArrayBase::setMask (const LELArrayBase& other)
{
    removeMask();
    if (other.isMasked()) {
	itsMaskPtr = new Array<Bool> (other.mask());
    }
}

void LELArrayBase::combineMask (const Array<Bool>& mask)
{
   if (!isMasked()) {
      itsMaskPtr = new Array<Bool> (mask);
   } else {
      Bool del1, del2;
      Bool* m1 = itsMaskPtr->getStorage (del1);
      const Bool* m2 = mask.getStorage (del2);
      uInt nr = itsMaskPtr->nelements();
      for (uInt i=0; i<nr; i++) {
	 if (!m2[i]) {
	    m1[i] = False;
	 }
      }
      itsMaskPtr->putStorage (m1, del1);
      mask.freeStorage (m2, del2);
   }
}

void LELArrayBase::combineOrAnd (Bool desiredValue, const Array<Bool>& value)
{
   // Combine the mask for an array and a scalar with a false mask.
   Bool deleteValue, deleteMask;
   const Bool* val = value.getStorage (deleteValue);
   uInt nr = value.nelements();
   if (itsMaskPtr == 0) {
      // Entire mask is true, so create one.
      itsMaskPtr = new Array<Bool> (value.shape());
      *itsMaskPtr = True;
   }
   // If value is unequal desiredValue, mask should also be false
   // (because  False || Unknown == Unknown  and  True && Unknown == Unknown).
   Bool* m = itsMaskPtr->getStorage (deleteMask);
   uInt ntrue = 0;
   for (uInt i=0; i<nr; i++) {
      if (val[i] != desiredValue) {
	 m[i] = False;
      } else if (m[i]) {
	 ntrue++;
      }
   }
   itsMaskPtr->putStorage (m, deleteMask);
   if (ntrue == nr) {
      removeMask();
   }
   value.freeStorage (val, deleteValue);
}

void LELArrayBase::combineOrAnd (Bool desiredValue, Array<Bool>& value,
				 const Array<Bool>& temp)
{
   Bool deleteValue, deleteTemp, deleteMask;
   Bool* val = value.getStorage (deleteValue);
   const Bool* tmp = temp.getStorage (deleteTemp);
   uInt nr = value.nelements();
   if (itsMaskPtr == 0) {
      for (uInt i=0; i<nr; i++) {
	 if (tmp[i] == desiredValue) {
	    val[i] = desiredValue;
	 }
      }
   } else {
      Bool* m = itsMaskPtr->getStorage (deleteMask);
      uInt ntrue = 0;
      for (uInt i=0; i<nr; i++) {
	 if (tmp[i] == desiredValue) {
	    val[i] = desiredValue;
	    m[i] = True;
	    ntrue++;
	 } else if (m[i]) {
	    ntrue++;
	 }
      }
      itsMaskPtr->putStorage (m, deleteMask);
      if (ntrue == nr) {
	 removeMask();
      }
   }
   value.putStorage (val, deleteValue);
   temp.freeStorage (tmp, deleteTemp);
}

void LELArrayBase::combineOrAnd (Bool desiredValue, Array<Bool>& value,
				 const Array<Bool>& temp,
				 const Array<Bool>& tempMask)
{
   Bool deleteValue, deleteTemp, deleteMask, deleteTempMask;
   Bool* val = value.getStorage (deleteValue);
   const Bool* tmp = temp.getStorage (deleteTemp);
   const Bool* tm = tempMask.getStorage (deleteTempMask);
   uInt nr = value.nelements();
   if (itsMaskPtr == 0) {
      itsMaskPtr = new Array<Bool>(value.shape());
      *itsMaskPtr = True;
   }
   Bool* m = itsMaskPtr->getStorage (deleteMask);
   uInt ntrue = 0;
   for (uInt i=0; i<nr; i++) {
      if (m[i]  &&  val[i] == desiredValue) {
	 ntrue++;
      } else if (tm[i]  &&  tmp[i] == desiredValue) {
	 val[i] = desiredValue;
	 m[i] = True;
	 ntrue++;
      } else {
         if (val[i] != desiredValue) {
	    val[i] = tmp[i];
         }
         if (m[i]) {
	    if (tm[i]) {
	       ntrue++;
	    } else {
	       m[i] = False;
	    }
         }
      }
   }
   itsMaskPtr->putStorage (m, deleteMask);
   if (ntrue == nr) {
       removeMask();
   }
   value.putStorage (val, deleteValue);
   temp.freeStorage (tmp, deleteTemp);
   tempMask.freeStorage (tm, deleteTempMask);
}

} //# NAMESPACE CASACORE - END

