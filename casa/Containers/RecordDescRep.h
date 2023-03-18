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


#ifndef CASA_RECORDDESCREP_H
#define CASA_RECORDDESCREP_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/iosfwd.h>
#include <vector>
#include <map>

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
// <linkto class="Record:example1">Record</linkto> class.
// </example>

// <motivation>
// RecordDescRep is needed to make copy-on-write semantics possible in
// class RecordDesc.
// </motivation>

class RecordDescRep
{
public:
  // The data members of each entry in the description.
    struct Data
    {
      Data()
        : type_p(TpOther), is_array_p(False)
      {}
      Data(const String& name, Int dtype)
        : type_p(dtype), name_p(name), shape_p(1,1), is_array_p(False)
      {}
      // Copy constructor and assignment have copy semantics.
      Data (const Data& that)
        { copy (that); }
      Data& operator= (const Data& that)
        { if (this != &that) copy(that); return *this; }
      void copy (const Data&);
      void setArrayShape (const IPosition& shape)
        { shape_p = shape; is_array_p = True; }
      // The DataType of each field.
      Int type_p;
      // The name of each field.
      String name_p;
      // The description of the sub-records. Null if the field is not a subrecord.
      std::unique_ptr<RecordDesc> sub_record_p;
      // The shape of the field; [1] for scalars and sub-records.
      IPosition shape_p;
      // True if the corresponding field is an array.
      Bool is_array_p;
      // Table description name for table fields.
      String tableDescName_p;
      // Comments for each field.
      String comment_p;
    };


    virtual ~RecordDescRep() = default;

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
    // It checks if the name is unique. and it extends the various blocks
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

private:
    // Test if all fields are part of the other description.
    // The flag equalDataTypes is set to True if the data types of the
    // fields in both descriptions are the same.
    Bool allExist (const RecordDescRep&, Bool& equalDataTypes) const;

    // Copy the given desc to this.
    void copy_other (const RecordDescRep&);
  
    // The Data of each field.
    std::vector<Data> data_p;
    // Mapping of field name to field number.
    std::map<String,Int> name_map_p;
};

inline uInt RecordDescRep::nfields() const
{
  return data_p.size();
}

inline DataType RecordDescRep::type (Int whichField) const
{
    return DataType(data_p[whichField].type_p);
}

inline const String& RecordDescRep::name (Int whichField) const
{
    return data_p[whichField].name_p;
}

inline const IPosition& RecordDescRep::shape (Int whichField) const
{
    return data_p[whichField].shape_p;
}

inline Bool RecordDescRep::isArray (Int whichField) const
{
    return data_p[whichField].is_array_p;
}

inline Bool RecordDescRep::isScalar (Int whichField) const
{
    return isScalarFun (DataType(data_p[whichField].type_p));
}

inline Bool RecordDescRep::isSubRecord (Int whichField) const
{
    return  (data_p[whichField].type_p == TpRecord);
}

inline Bool RecordDescRep::isTable (Int whichField) const
{
    return  (data_p[whichField].type_p == TpTable);
}

inline const RecordDesc& RecordDescRep::subRecord (Int whichField) const
{
    //# The cast to non-const is completely safe.
    return const_cast<RecordDescRep*>(this)->subRecord (whichField);
}

inline const String& RecordDescRep::tableDescName (Int whichField) const
{
    return data_p[whichField].tableDescName_p;
}



} //# NAMESPACE CASACORE - END

#endif
