//# RecordTransformable.h: Interface class for converting to/from records
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
class GlishValue;
class GlishRecord;

// <summary>Interface class for converting to/from records</summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tRecordTransformable">
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
// <li> <linkto class="GlishRecord">GlishRecord</linkto>
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
// At the moment only Records & TableRecords are derived from
// RecordInterface. In future all records (like GlishRecord) will be derived
// from this one base. Till then separate to/fromRecord() methods should be
// provided for the different record types. Implementation of these could be in
// a separate file to make sure they are only included when
// needed. Alternatively you could use the supplied to/fromGlishRecord
// functions. As the implementation of these functions is in a seperate file
// (Record2Transformable.cc) they are only linked in when necessary.
// </note>
// </synopsis>
//
// <example>
// The following example prints out a class using its record representation.
// This example is in the file tRecordTransformable.cc
// <srcblock>
// void printAsRecord(const RecordTransformable & myClass) {
//   String errorMessage;
//   GlishRecord rec;
//   if (!myClass.toGlishRecord(errorMessage, rec)) {
//     cout << "Cannot convert class to a Record. The reason is:" << endl; 
//     cout << errorMessage << endl;
//   } else {
//     cout << rec.format() << endl;
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
  // <group>
  virtual Bool toRecord(String & error, RecordInterface & outRecord) const = 0;
  Bool toGlishRecord(String & error, GlishRecord & outRecord) const;
  // </group>

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

  // Initialise the class from a Record or a String representation. The input
  // GlishValue should either be a record or a string and this function will
  // call the appropriate virtual function to do the conversion. This function
  // returns False if the input GlishValue is not a record or a string. It also
  // returns False if it could not initialise the object from the String or
  // Record. Whenever it reurns False an error message is appended to the
  // supplied string. It returns True if the conversion succeeded.
  Bool fromGlishRecord(String & error, const GlishValue & inValue);

};

#endif
