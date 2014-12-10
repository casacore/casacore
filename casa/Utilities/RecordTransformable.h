//# RecordTransformable.h: Interface class for converting to/from records
//# Copyright (C) 1998,1999,2003
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

#ifndef CASA_RECORDTRANSFORMABLE_H
#define CASA_RECORDTRANSFORMABLE_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class String;
class RecordInterface;

// <summary>Interface class for converting to/from records</summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tRecordTransformable">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="RecordInterface">RecordInterface</linkto>
// </prerequisite>
//
// <etymology>
// This class defines the interface that a class should use if the can be
// transformed into a record representation.
// </etymology>
//
// <synopsis>
// This abstract base class is intended to be publicly inherited by classes
// that contain functions which can represent the object as a record (these
// functions should be called <src>toRecord</src> and
// <src>fromRecord</src>). Examples of records are: 
// <ul> 
// <li> <linkto class="Record">Record</linkto>
// <li> <linkto class="TableRecord">TableRecord</linkto>
// </ul> 
//
// This interface defines two functions that convert between a RecordInterface
// and the class that inherits these functions.  These functions are often used
// to parse input that is beyond the programs control e.g. user input from
// glish or Table records that may have been generated elsewhere. Hence
// exceptions should not thrown be thrown by these functions. Instead the
// function should return False and append an error message to the supplied
// String when the transformation cannot be accomplished.
//
// <note role=warning>
// Converting to/from a GlishRecord requires an extra step.
// First a Record should be used which can thereafter be converted to/from
// a GlishRecord using the appropriate GlishRecord functions.
// </note>
// </synopsis>
//
// <example>
// The following example prints out a class using its record representation.
// This example is in the file tRecordTransformable.cc
// <srcblock>
// void printAsRecord(const RecordTransformable & myClass) {
//   String errorMessage;
//   Record rec;
//   if (!myClass.toRecord(errorMessage, rec)) {
//     cout << "Cannot convert class to a Record. The reason is:" << endl; 
//     cout << errorMessage << endl;
//   } else {
//     cout << rec.ndefined() << endl;
//   }
// }
// </srcblock>
// </example>
//
// <motivation>
// This class was designed to standardise the function interface for converting
// between an object and its record representation.
// </motivation>
//
// <todo asof="1998/03/30">
//   <li> Nothing I hope!
// </todo>

class RecordTransformable
{
public:
  // The destructor must be virtual so that the destructor of derived classes
  // is actually used. 
  virtual ~RecordTransformable();
  
  // Convert the class to an Record representation. The input record may
  // already contain fields and these fields may be silently overridden. New
  // fields may be added to the input Record. If the transformation succeeds
  // then the error String is unchanged and the function returns
  // True. Otherwise the function returns False and appends an error message to
  // the supplied String giving the reason why the conversion failed.
  virtual Bool toRecord(String & error, RecordInterface & outRecord) const = 0;

  // Initialise the class from a Record representation. The input record should
  // contain the fields that are required by the class. Other fields will be
  // ignored. If the transformation succeeds then the error String is unchanged
  // and the function returns True. Otherwise the function returns False and
  // appends an error message to the supplied String giving the reason why the
  // conversion failed.
  virtual Bool fromRecord(String & error, const RecordInterface & inRecord) =0;

  // Initialise the class from a String representation. A string cannot
  // contain enough information for many objects. Hence the default
  // implementation of this class returns False, indicating that the class
  // could not be initialised and an error message is appended to the supplied
  // string. If the class can be initialised from a string then this function
  // should be overridden.
  virtual Bool fromString(String & error, const String & inString);

  // Specify the identification of the record (e.g. 'meas', 'quant'). The
  // default implementation returns a empty string.
  virtual const String &ident() const;
};


} //# NAMESPACE CASACORE - END

#endif
