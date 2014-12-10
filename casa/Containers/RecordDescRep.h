//# RecordDescRep.h: Representation of a RecordDesc
//# Copyright (C) 1996,1997,1998,1999,2000,2001
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


#ifndef CASA_RECORDDESCREP_H
#define CASA_RECORDDESCREP_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Containers/SimOrdMap.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class RecordDesc;
class AipsIO;


// <summary>
// Representation of a RecordDesc
// </summary>

// <use visibility=local>
// <reviewed reviewer="Mark Wieringa" date="1996/04/15" tests="tRecordDesc">
// </reviewed>

// <prerequisite>
//   <li> <linkto group="DataType.h#DataType">DataType</linkto>
//   <li> <linkto class="RecordDesc">RecordDesc</linkto>
// </prerequisite>

// <etymology>
// Rep is an often used abbreviation for representation.
// Thus RecordDescRep is the representation of a RecordDesc.
// </etymology>

// <synopsis>
// RecordDescRep is used by RecordDesc
// to implement its copy-on-write semantics. RecordDesc is the interface
// to the user, while RecordDescRep contains the actual implementation.
// See <linkto class=RecordDesc>RecordDesc</linkto> for a more detailed
// description of a record description.
// </synopsis>

// <example>
// See the example in the description of the 
// <linkto class="Record:example">Record</linkto> class.
// </example>

// <motivation>
// RecordDescRep is needed to make copy-on-write semantics possible in
// class RecordDesc.
// </motivation>

// <todo asof="1995/06/01">
//   <li> Should the strategy wrt. field names be changed (not used in
//        field description equality, must be unique at a given level?).
//   <li> Perhaps we should be able to more conveniently change the description
//        of an existing field.
// </todo>

class RecordDescRep
{
public:
    // Create a description with no fields.
    RecordDescRep();

    // Create a description which is a copy of other.
    RecordDescRep (const RecordDescRep& other);

    // Replace this description with other.
    RecordDescRep& operator= (const RecordDescRep& other);

    virtual ~RecordDescRep();

    // Add scalar or array field. If of array type, the shape is set to [-1],
    // which indicates a variable sized array. Returns the number of fields in
    // the description.
    uInt addField (const String& fieldName, DataType scalarOrArrayType);

    // Add an array field of the indicated type. The DataType is promoted
    // from a scalar type to an array type if necessary, e.g., 
    // <src>TpInt ->TpArrayInt</src>.  Returns the number of fields in
    // the description.
    uInt addArray (const String& fieldName, DataType scalarOrArrayType,
		   const IPosition& shape);

    // Add a Record field to the description. This allows hierarchical
    // descriptions to be developed. Returns the number of fields in the
    // description.
    uInt addRecord (const String& fieldName, const RecordDesc& subDesc);

    // Add a Table field to the description. The Table description has the
    // given name. Returns the number of fields in the description.
    uInt addTable (const String& fieldName, const String& tableDescName);

    // Get the comment for this field.
    const String& comment (Int whichField) const;

    // Set the comment for this field.
    void setComment (Int whichField, const String& comment);

    // Set the shape for this field.
    // An exception will be thrown if the field is no array.
    void setShape (Int whichField, const IPosition& shape);

    // Merge a single field from other.  If allowDuplicates is True, silently
    // throw away fields if one with the same name and type already exists,
    // otherwise an exception is thrown.  Conflicting types always cause an
    // exception. Returns the number of fields in the description.
    uInt mergeField (const RecordDescRep& other, Int whichFieldFromOther,
		     int duplicateAction);

    // Add all the fields from another RecordDescRep to the current objects.
    uInt merge (const RecordDescRep& other, int duplicateAction);
    
    // Remove the given field from the description.
    virtual uInt removeField (Int whichField);

    // Rename the given field.
    virtual void renameField (const String& newName, Int whichField);

    // Returns the index of the field named fieldName. Returns -1 if fieldName
    // does not exist.
    Int fieldNumber (const String& fieldName) const;

    // Number of fields in the description.
    uInt nfields() const;

    // What is the type of the given field. Returns TpRecord if the field is
    // a sub-Record.
    DataType type (Int whichField) const;

    // What is the name of the given field.
    const String& name (Int whichField) const;

    // Create a name for a field defined by index as *i (similar to glish).
    // It takes care that the resulting name is unique by adding a suffix _j
    // when needed.
    String makeName (Int whichField) const;

    // Make the given name unique by adding a suffix _j when needed.
    // j is the minimal number needed to make it unique.
    String uniqueName (const String& name) const;

    // Returns True if whichField is an array.
    Bool isArray (Int whichField) const;

    // Returns True if whichField is a scalar.
    Bool isScalar (Int whichField) const;

    // Returns True if whichField is a sub-record.
    Bool isSubRecord (Int whichField) const;

    // Returns True if whichField is a table.
    Bool isTable (Int whichField) const;

    // What is the shape of the given field. Returns [1] if the field is a
    // scalar, table or, sub-record, [-1] if it is a variable length array,
    // and the actual shape for a fixed length array.
    const IPosition& shape (Int whichField) const;

    // What is the name of the table description associated with a table.
    const String& tableDescName (Int whichField) const;

    // If whichField is a sub-record with a description,
    // return its description. Otherwise an exception is thrown.
    // <group>
    const RecordDesc& subRecord (Int whichField) const;
    RecordDesc& subRecord (Int whichField);
    // </group>

    // <group>
    // This and other compare equal if the field types and shapes are identical
    // (recursively if there are described sub-records or tables).
    // The field names are not used.
    Bool operator== (const RecordDescRep& other) const;
    Bool operator!= (const RecordDescRep& other) const;
    // </group>

    // Test if this description conforms the other.
    // It is similar to operator==. However, a subrecord in that description
    // always conforms an arbitrary (i.e. empty) subrecord in this
    // description.
    // <br>This is used by Record, to see if another record can be assigned
    // to this record.
    Bool conform (const RecordDescRep& other) const;

    // Test if this description equals another one.
    // It is equal if the number of fields is equal and all field names in
    // this description occur in the other too. The order of the fields
    // is not important.
    // <br>The flag equalDataTypes is set to True if the data types
    // of all fields match.
    // <br>Use function operator== if order and types are important,
    // but names are not.
    Bool isEqual (const RecordDescRep& other, Bool& equalDataTypes) const;

    // Test if this description is a subset of another one.
    // It is similar to isEqual above.
    Bool isSubset (const RecordDescRep& other, Bool& equalDataTypes) const;

    // Test if this description is a strict subset of another one, thus
    // if it is a subset and not equal.
    Bool isStrictSubset (const RecordDescRep& other,
			 Bool& equalDataTypes) const;

    // Test if the set of field names in this and other record description
    // is disjoint (i.e. if they do not share names).
    Bool isDisjoint (const RecordDescRep& other) const;

protected:
    // Add a field name and its type.
    // It checks if the name is unique and it extends the various blocks
    // using increment_length.
    void addFieldName (const String& fieldName, DataType type);

    // Add a field from another Record description.
    // This is used by the merge functions.
    virtual void addRepField (const RecordDescRep& other,
			      const String& newName, Int whichField);

    // Add the field info. These are helper functions for the add functions
    // and can be used in derived classes too.
    // <group>
    void addFieldAny (DataType scalarOrArrayType);
    void addFieldArray (DataType scalarOrArrayType, const IPosition& shape);
    // </group>

    // Set the shape (for a derived class).
    void setShape (const IPosition& shape, Int whichField);

    // Helper functions
    // <group>
    virtual void increment_length();
    void copy_other (const RecordDescRep& other);
    // </group>

private:
    // Test if all fields are part of the other description.
    // The flag equalDataTypes is set to True if the data types of the
    // fields in both descriptions are the same.
    Bool allExist (const RecordDescRep&, Bool& equalDataTypes) const;

    // Number of fields in the description.
    uInt n_p;
    // The DataType of each field.
    Block<Int> types_p;
    // The name of each field.
    Block<String> names_p;
    // The description of the subrecords. Null if the field is not a subrecord.
    // This isn't the most efficient representation. If this is ever an issue
    // we could calculate these, or store them in one Block, or implement
    // copy-on-write semantics.
    PtrBlock<RecordDesc*> sub_records_p;
    // The shape of the field [1] for scalars and sub-records.
    Block<IPosition> shapes_p;
    // True if the corresponding field is an array.
    Block<Bool> is_array_p;
    // Table description name for table fields.
    Block<String> tableDescNames_p;
    // Comments for each field.
    Block<String> comments_p;
    // Mapping of field name to field number.
    SimpleOrderedMap<String,Int> name_map_p;
};

inline uInt RecordDescRep::nfields() const
{
    return n_p;
}

inline DataType RecordDescRep::type (Int whichField) const
{
    return DataType(types_p[whichField]);
}

inline const String& RecordDescRep::name (Int whichField) const
{
    return names_p[whichField];
}

inline const IPosition& RecordDescRep::shape (Int whichField) const
{
    return shapes_p[whichField];
}

inline Bool RecordDescRep::isArray (Int whichField) const
{
    return is_array_p[whichField];
}

inline Bool RecordDescRep::isScalar (Int whichField) const
{
    return isScalarFun (DataType(types_p[whichField]));
}

inline Bool RecordDescRep::isSubRecord (Int whichField) const
{
    return  (types_p[whichField] == TpRecord);
}

inline Bool RecordDescRep::isTable (Int whichField) const
{
    return  (types_p[whichField] == TpTable);
}

inline const RecordDesc& RecordDescRep::subRecord (Int whichField) const
{
    //# The cast to non-const is completely safe.
    return ((RecordDescRep*)this)->subRecord (whichField);
}

inline const String& RecordDescRep::tableDescName (Int whichField) const
{
    return tableDescNames_p[whichField];
}



} //# NAMESPACE CASACORE - END

#endif
