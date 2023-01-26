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
	    itsMaskPtr = new Array<bool> (*other.itsMaskPtr);
	}
    }
    return *this;
}

void LELArrayBase::removeMask()
{
    delete itsMaskPtr;
    itsMaskPtr = 0;
}

void LELArrayBase::setMask (const Array<bool>& mask)
{
    delete itsMaskPtr;
    itsMaskPtr = new Array<bool> (mask);
}

void LELArrayBase::setMask (Array<bool>& mask)
{
    delete itsMaskPtr;
    itsMaskPtr = new Array<bool> (mask);
}

void LELArrayBase::setMask (const LELArrayBase& other)
{
    removeMask();
    if (other.isMasked()) {
	itsMaskPtr = new Array<bool> (other.mask());
    }
}

void LELArrayBase::combineMask (const Array<bool>& mask)
{
   if (!isMasked()) {
      itsMaskPtr = new Array<bool> (mask);
   } else {
      bool del1, del2;
      bool* m1 = itsMaskPtr->getStorage (del1);
      const bool* m2 = mask.getStorage (del2);
      uint32_t nr = itsMaskPtr->nelements();
      for (uint32_t i=0; i<nr; i++) {
	 if (!m2[i]) {
	    m1[i] = false;
	 }
      }
      itsMaskPtr->putStorage (m1, del1);
      mask.freeStorage (m2, del2);
   }
}

void LELArrayBase::combineOrAnd (bool desiredValue, const Array<bool>& value)
{
   // Combine the mask for an array and a scalar with a false mask.
   bool deleteValue, deleteMask;
   const bool* val = value.getStorage (deleteValue);
   uint32_t nr = value.nelements();
   if (itsMaskPtr == 0) {
      // Entire mask is true, so create one.
      itsMaskPtr = new Array<bool> (value.shape());
      *itsMaskPtr = true;
   }
   // If value is unequal desiredValue, mask should also be false
   // (because  false || Unknown == Unknown  and  true && Unknown == Unknown).
   bool* m = itsMaskPtr->getStorage (deleteMask);
   uint32_t ntrue = 0;
   for (uint32_t i=0; i<nr; i++) {
      if (val[i] != desiredValue) {
	 m[i] = false;
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

void LELArrayBase::combineOrAnd (bool desiredValue, Array<bool>& value,
				 const Array<bool>& temp)
{
   bool deleteValue, deleteTemp, deleteMask;
   bool* val = value.getStorage (deleteValue);
   const bool* tmp = temp.getStorage (deleteTemp);
   uint32_t nr = value.nelements();
   if (itsMaskPtr == 0) {
      for (uint32_t i=0; i<nr; i++) {
	 if (tmp[i] == desiredValue) {
	    val[i] = desiredValue;
	 }
      }
   } else {
      bool* m = itsMaskPtr->getStorage (deleteMask);
      uint32_t ntrue = 0;
      for (uint32_t i=0; i<nr; i++) {
	 if (tmp[i] == desiredValue) {
	    val[i] = desiredValue;
	    m[i] = true;
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

void LELArrayBase::combineOrAnd (bool desiredValue, Array<bool>& value,
				 const Array<bool>& temp,
				 const Array<bool>& tempMask)
{
   bool deleteValue, deleteTemp, deleteMask, deleteTempMask;
   bool* val = value.getStorage (deleteValue);
   const bool* tmp = temp.getStorage (deleteTemp);
   const bool* tm = tempMask.getStorage (deleteTempMask);
   uint32_t nr = value.nelements();
   if (itsMaskPtr == 0) {
      itsMaskPtr = new Array<bool>(value.shape());
      *itsMaskPtr = true;
   }
   bool* m = itsMaskPtr->getStorage (deleteMask);
   uint32_t ntrue = 0;
   for (uint32_t i=0; i<nr; i++) {
      if (m[i]  &&  val[i] == desiredValue) {
	 ntrue++;
      } else if (tm[i]  &&  tmp[i] == desiredValue) {
	 val[i] = desiredValue;
	 m[i] = true;
	 ntrue++;
      } else {
         if (val[i] != desiredValue) {
	    val[i] = tmp[i];
         }
         if (m[i]) {
	    if (tm[i]) {
	       ntrue++;
	    } else {
	       m[i] = false;
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

