//# RecordTransformable.h:
//# Copyright (C) 1998
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
//#
//# $Id$

#if !defined(AIPS_RECORDTRANSFORMABLE_H)
#define AIPS_RECORDTRANSFORMABLE_H

#include <aips/aips.h>

class String;
class RecordInterface;

// <summary>Interface class for converting to/from records</summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="RecordInterface">RecordInterface</linkto>
// </prerequisite>
//
// <etymology>
// This class defines the interface that classes should use if the can be
// transformed into a record representation.
// </etymology>
//
// <synopsis>
// This abstract base class is intended to be publicly inherited by classes
// that contain functions which can represent the object as a record. A class
// may contain these functions if:
// <ul> 
// <li> It needs to represent itself as a glish record (using a 
//      <linkto class="GlishRecord">GlishRecord</linkto>)
// <li> It needs to save itself to a Table (using a 
//      <linkto class="TableRecord">TableRecord</linkto>)
//
// This interface defines two functions that convert between a RecordInterface
// and the class that inherits these functions. 

// These functions are often used to parse input that is beyond the programs
// control (ie user input from glish or Table records that may have been
// generated elsewhere). Hence exceptions should not thrown be thrown by these
// functions. Instead the function should return False and append an error
// message to the supplied String when the transformation cannot be
// accomplished.

// </synopsis>
//
// <example>
//#! One or two concise (~10-20 lines) examples, with a modest amount of
//#! text to support code fragments.  Use <srcblock> and </srcblock> to
//#! delimit example code.
// </example>
//
// <motivation>
// This class was designed to standardise the function interface for converting
// between an object and its Glish representation.
// </motivation>
//
// <todo asof="1998/03/30">
//   <li> Nothing I hope!
// </todo>

class RecordTransformable
{
public:
  // The destructor must be virtual so that the descructor of derived classes
  // is actually used. 
  virtual ~RecordTransformable();
  
  // Convert the class to an Record representation. The input record may
  // already contain fields and these fields may be silently overridden. New
  // fields may be added to the input Record. If the transformation succeeds
  // then the error String is unchanged and the function returns
  // True. Otherwise the function returns False and appends an error message to
  // the supplied String giving the reason why the conversion failed.
  virtual Bool toRecord(String & error, RecordInterface & inRecord) const = 0;

  // Initialise the class from a Record representation. The input record should
  // contain the fields that are required by the class. Other fields will be
  // ignored. If the transformation succeeds then the error String is unchanged
  // and the function returns True. Otherwise the function returns False and
  // appends an error message to the supplied String giving the reason why the
  // conversion failed.
  virtual Bool fromRecord(String & error,
			  const RecordInterface & outRecord) = 0;
};

#endif
