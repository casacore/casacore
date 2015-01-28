//# LELArrayBase.h: Base class for LELArray holding the mask
//# Copyright (C) 1999,2001
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

#ifndef LATTICES_LELARRAYBASE_H
#define LATTICES_LELARRAYBASE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Base class for LELArray holding the mask.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <synopsis>
// This LEL class holds an array with a mask.
// The mask can be a single Bool valid for all elements of the array.
// Otherwise it is a full mask with the same shape as the array.
// </synopsis>

// <motivation>
// It maskes it possible to handle an array with its mask as a single object.
// </motivation>

// <todo asof="1998/01/20">
// </todo>
 

class LELArrayBase
{
public: 
// Default constructor sets to mask all true.
   LELArrayBase()
       : itsMaskPtr(0) {}

// Constructor takes mask.
   LELArrayBase (const Array<Bool>& mask)
       : itsMaskPtr(new Array<Bool>(mask)) {}

// Copy constructor (reference semantics).
   LELArrayBase (const LELArrayBase& other);

   ~LELArrayBase();

// Assignment (reference semantics).
   LELArrayBase& operator= (const LELArrayBase& other);

// Does the value have a mask?
   Bool isMasked() const
      { return  (itsMaskPtr != 0); }

// Get mask.
// <group>
   const Array<Bool>& mask() const
      { return *itsMaskPtr; }
   Array<Bool>& mask()
      { return *itsMaskPtr; }
// </group>

// Remove the mask.
   void removeMask();

// Set the mask from given array (takes reference).
   void setMask (const Array<Bool>& other);

// Set the mask from the mask of the other value.
   void setMask (const LELArrayBase& other);

// Set the mask from given array (takes reference).
   void setMask (Array<Bool>& other);

// Set the mask by combining the masks of both values.
   void setMask (const LELArrayBase& left, const LELArrayBase& right)
      { setMask (left); combineMask (right); }

// Combine the mask of this and the other value (by anding them).
// <group>
   void combineMask (const LELArrayBase& other)
    { if (other.isMasked()) combineMask (other.mask()); }
   void combineMask (const Array<Bool>& mask);
// </group>

// Combine the mask with the given value in case of an OR or AND.
// It means the mask is set to true if value is desiredValue
// (which should be True for OR and False for AND).
// <group>
// Combine with a single scalar value for which the mask is false.
   void combineOrAnd (Bool desiredValue, const Array<Bool>& value);

// Combine for two arrays taking the true/false array values into account.
// The mask and value are set to desiredValue if the temp value is desiredValue.
   void combineOrAnd (Bool desiredValue, Array<Bool>& value,
		      const Array<Bool>& temp);

// Combine for two arrays taking the true/false array values and mask
// into account.
// The mask and value are set to desiredValue if the temp value is desiredValue
// and its temp mask it true.
// The mask is set to false if the temp mask is False.
   void combineOrAnd (Bool desiredValue, Array<Bool>& value,
		      const Array<Bool>& temp, const Array<Bool>& tempMask);
// </group>

private:
   Array<Bool>* itsMaskPtr;
};



} //# NAMESPACE CASACORE - END

#endif
