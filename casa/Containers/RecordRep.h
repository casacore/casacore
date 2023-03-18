//# RecordRep.h: The representation of a Record
//# Copyright (C) 1996,1997,2000,2001,2005
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


#ifndef CASA_RECORDREP_H
#define CASA_RECORDREP_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/RecordDesc.h>
#include <casacore/casa/Containers/RecordData.h>
#include <casacore/casa/Containers/RecordInterface.h>
#include <vector>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class AipsIO;
class IPosition;
class String;


// <summary>
// The representation of a  Record
// </summary>

// <use visibility=local>
// <reviewed reviewer="Mark Wieringa" date="1996/04/15" tests="tRecord">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Record">Record</linkto>.
// </prerequisite>
//
// <etymology>
// RecordRep is the REPresentation of a Record.
// </etymology>
//
// <synopsis>
// RecordRep is the actual implementation of a Record object.
// It contains the description and the data. The data is stored as
// a collection of void* pointers to the actual data. By storing
// it in this indirect way, it is easier to extend the data block.
// It also means that RecordFieldPtr objects always have the correct
// pointer and do not need to be adjusted when the data block is extended.
// <p>
// Despite the fact that the data pointers have type void*, the
// functions are completely type safe. This is done by passing the
// type around using the DataType enumeration. The downpart is that
// only types from that enumeration are supported (but that is also
// required by the RecordDesc mechanics).
// <p>
// Note that RecordRep does not know anything about RecordFieldPtr
// objects pointing to its data. Only its mother class Record
// knows about them and handles all cases where the RecordFieldPtr's
// have to be notified.
// </synopsis>
//
// <example>
// RecordRep mirrors all functions in Record.
// </example>
//
// <motivation>
// Having a separate RecordRep class makes copy-on-write possible.
// It also allows derivation of other RecordRep classes (like TableRecordRep),
// while their mother classes are not derived from each other.
// </motivation>

class RecordRep
{
public:
    // Create a record with no fields.
    RecordRep();

    // Create a record with the given description. If it is not possible to 
    // create all fields (for example, if a field of an unsupported type is
    // requested), an exception is thrown.
    // All fields are checked by the field checking function (if defined).
    RecordRep (const RecordDesc& description);

    // Create a copy of other using copy semantics.
    RecordRep (const RecordRep& other);

    // Copy all the data over.
    RecordRep& operator= (const RecordRep& other);
    
    // Delete all data.
    virtual ~RecordRep() = default;

    // Describes the current structure of this Record.
    const RecordDesc& description() const
      { return desc_p; }

    // Get the field number for a given name.
    Int fieldNumber (const String& name) const
      { return desc_p.fieldNumber (name); }

    // Get the comment for this field.
    const String& comment (Int whichField) const
      { return desc_p.comment (whichField); }

    // Set the comment for this field.
    void setComment (Int whichField, const String& comment)
      { desc_p.setComment (whichField, comment); }

    // Change the structure of this Record to contain the fields in
    // newDescription.
    // After calling restructure, <src>description() ==newDescription</src>.
    void restructure (const RecordDesc& newDescription, Bool recursive);

    // Returns True if this and other have the same RecordDesc, other
    // than different names for the fields. That is, the number, type and the
    // order of the fields must be identical (recursively for fixed
    // structured sub-Records in this).
    // <note role=caution>
    // <src>thisRecord.conform(thatRecord) == True</src> does not imply
    // <br><src>thatRecord.conform(thisRecord) == True</src>, because
    // a variable record in one conforms a fixed record in that, but
    // not vice-versa.
    // </note>
    Bool conform (const RecordRep& other) const;

    // Copy all data of the Record.
    void copyData (const RecordRep& other);

    // Remove a field from the record.
    void removeField (Int whichField);

    // Rename the given field.
    void renameField (const String& newName, Int whichField)
      { desc_p.renameField (newName, whichField); }

    // Add a field with the given name and value to the record.
    // The data type of the field is determined by the data type of the value.
    // For arrays it is possible to define if the shape is fixed.
    // <group>
    void addDataField (const String& name, DataType type,
		       const IPosition& shape, Bool fixedShape,
		       const void* data);
    void addField (const String& name, const Record& value,
		   RecordInterface::RecordType type);
    // </group>

    // Define a value for the given field.
    // Array conformance rules will not be applied for variable shaped arrays.
    // If the field and value data type mismatch, type promotion
    // of scalars will be done if possible. If not possible, an exception
    // is thrown.
    void defineDataField (Int whichField, DataType type, const void* value);

    // Put the description and data of the Record.
    // It also puts the fixedFlag attribute (of the mother Record).
    void putRecord (AipsIO& os, int recordType) const;

    // Get the description and data of the Record.
    // It also gets the fixedFlag attribute (of the mother Record).
    void getRecord (AipsIO& os, Int& recordType);

    // Put the data of a record.
    // This is used to write a subrecord, whose description has
    // already been written.
    void putData (AipsIO& os) const;

    // Read the data of a record.
    // This is used to read a subrecord, whose description has
    // already been read.
    void getData (AipsIO& os, uInt version);

    // Used by the RecordFieldPtr classes to attach in a type-safe way to the
    // correct field.
    // <group>
    virtual void* get_pointer (Int whichField, DataType type) const;
    virtual void* get_pointer (Int whichField, DataType type,
                               const String& recordType) const;
    // </group>

    // Merge a field from another record into this record.
    void mergeField (const RecordRep& other, Int whichFieldFromOther,
		     RecordInterface::DuplicatesFlag);

    // Merge all fields from the other record into this record.
    void merge (const RecordRep& other, RecordInterface::DuplicatesFlag);
    
    // Print a record.
    // Print the contents of the record.
    // Only the first <src>maxNrValues</src> of an array will be printed.
    // A value < 0 means the entire array.
    void print (std::ostream&,
		Int maxNrValues = 25,
		const String& indent="") const;
  
protected:
    // Do the actual merging of a field.
    virtual void doMergeField (DataType type, const void* otherPtr,
                               const IPosition& shape);

    // Utility functions to avoid code duplication in the public member 
    // functions.
    void copy_other (const RecordRep& other);

    // Check that the value's shape matches the given shape.
    // If not, an exception is thrown.
    void checkShape (const IPosition& shape, const void* value,
                     const String& fieldName);
  
    // Get a Scalar/ArrayKeywordSet object as a Record.
    // (type 0 = ScalarKeywordSet;  type 1 = ArrayKeywordSet).
    void getKeySet (AipsIO& os, uInt version, uInt type);

    // Get the description of a keyword set as a RecordDesc.
    void getKeyDesc (AipsIO& os, RecordDesc& desc);

    // Get the scalar values of a keyword set.
    void getScalarKeys (AipsIO& os);

    // Get the array values of a keyword set.
    void getArrayKeys (AipsIO& os);

    //# Data members
    // Holds the structure of this Record.
    RecordDesc desc_p;
    // Pointers to data values.
    // A pointer is used, so the RecordData address is not changed when
    // the vector is resized.
    std::vector<std::unique_ptr<RecordDataBase>> data_p;
};


} //# NAMESPACE CASACORE - END

#endif
