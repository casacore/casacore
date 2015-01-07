//# RecordDesc.h: Description of the fields in a record object
//# Copyright (C) 1995,1996,1998,2000,2001
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


#ifndef CASA_RECORDDESC_H
#define CASA_RECORDDESC_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/RecordDescRep.h>
#include <casacore/casa/Containers/RecordInterface.h>
#include <casacore/casa/Utilities/COWPtr.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class AipsIO;


// <summary>
// Description of the fields in a record object
// </summary>

// <use visibility=export>
// <reviewed reviewer="Mark Wieringa" date="1996/04/15" tests="tRecordDesc">
// </reviewed>

// <prerequisite>
//   <li> <linkto group="DataType.h#DataType">DataType</linkto>
//   <li> <linkto class="Record">Record</linkto>
// </prerequisite>
//
// <etymology>
// RecordStructure would perhaps have been the clearest possible name. However
// it was decided to name it ``RecordDesc'' to use a compatible naming
// convention with other classes in the system, such as TableDesc. This class
// <em>Desc</em>ribes the structure of a Record.
// </etymology>
//
// <synopsis>
// RecordDesc describes the structure of <linkto class="Record">Record</linkto>
// objects. A Record consists of a number of fields. A RecordDesc describes 
// those fields by assigning to each one:
// <ul>
//    <li> A name for the field.
//    <li> A type from the <linkto group="DataType.h#DataType">DataType</linkto>
//         enum.
//    <li> A shape if the field is an array.
//    <li> A RecordDesc if the field is itself a Record (the Record is an
//         hierarchical structure).
// </ul>
// Only one field with a given name is allowed (although fields in subrecords
// may have the same name as a field in a parent or child Record).
//
// Field indices are zero relative, i.e. they range from 0 to 
// <src>nfields()-1</src>.
// </synopsis>
//
// <example>
// See the example in the description of the 
// <linkto class="Record:example">Record</linkto> class.
// </example>
//
// <motivation>
// It is useful to be able to create many new objects with the same structure
// as some other, without necessarily cloning it by copying all the values.
// A ``Description'' type is necessary to do this (e.g., shape for an Array).
// </motivation>
//
//
// <todo asof="1995/06/01">
//   <li> Should the strategy wrt. field names be changed (not used in
//        field description equality, must be unique at a given level?).
//   <li> Perhaps we should be able to more conveniently change the description
//        of an existing field.
// </todo>

class RecordDesc
{
public:
    // Writes/reads the RecordDesc to/from an output stream.
    // <group name=io>
    friend ostream& operator<< (ostream& os, const RecordDesc& desc);
    friend AipsIO& operator<< (AipsIO& os, const RecordDesc& desc);
    friend AipsIO& operator>> (AipsIO& os, RecordDesc& desc);
    // </group>

    // Create a description with no fields.
    RecordDesc();

    // Create a description which is a copy of other.
    RecordDesc (const RecordDesc& other);

    // Replace this description with other.
    RecordDesc& operator= (const RecordDesc& other);

    ~RecordDesc();

    // Add scalar, array, sub-record, or table field.
    // If of array type, the shape is set to [-1], which indicates a
    // variable sized array.
    // If of sub-record type, the sub-record is free format.
    // Returns the number of fields in the description.
    uInt addField (const String& fieldName, DataType dataType);

    // Add an array field of the indicated type. The DataType is promoted
    // from a scalar type to an array type if necessary, e.g., 
    // <src>TpInt ->TpArrayInt</src>.  Returns the number of fields in
    // the description.
    // A shape of [-1] indicates a variable shape.
    uInt addField (const String& fieldName, DataType scalarOrArrayType,
		   const IPosition& shape);

    // Add a Record field to the description. This allows hierarchical
    // descriptions to be developed. Returns the number of fields in the
    // description.
    uInt addField (const String& fieldName, const RecordDesc& subDesc);

    // Add a Table field to the description. The Table description has the
    // given name. Returns the number of fields in the description.
    // <br>
    // When a table is put in a record field, it is checked if the name
    // of its description matches this name. If this name is empty, it
    // matches any table description.
    // <note role=warning>
    // Note that not all record types are able to instantiate a table field.
    // E.g. <linkto class=TableRecord>TableRecord</linkto> can instantiate
    // it, while <linkto class=Record>Record</linkto> cannot and throws an
    // exception when a record description containing a table field is used.
    // </note>
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
    uInt mergeField (const RecordDesc& other, Int whichFieldFromOther,
		     RecordInterface::DuplicatesFlag DuplicateAction
		                       = RecordInterface::ThrowOnDuplicates);

    // Add all the fields from another RecordDesc to the current objects.
    // It returns the new number of fields.
    uInt merge (const RecordDesc& other, 
		RecordInterface::DuplicatesFlag DuplicateAction
		                       = RecordInterface::ThrowOnDuplicates);
    
    // Remove the given field from the description.
    // It returns the new number of fields.
    uInt removeField (Int whichField);

    // Rename the given field.
    void renameField (const String& newName, Int whichField);

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

    // What is the name of the table description.
    // Returns an empty string when the field is no table.
    const String& tableDescName (Int whichField) const;

    // If whichField is a sub-record return its description.
    // Otherwise an exception is thrown.
    // The non-const version is named differently to prevent accidental
    // use of the non-const version.
    // <group>
    const RecordDesc& subRecord (Int whichField) const;
    RecordDesc& rwSubRecord (Int whichField);
    // </group>

    // This and other compare equal if the field types and shapes are identical
    // (recursively if there are described sub-records).
    // The field names are not used.
    // <br>Use function isEqual if names are important, but order is not.
    // <group>
    Bool operator== (const RecordDesc& other) const;
    Bool operator!= (const RecordDesc& other) const;
    // </group>

    // Test if this description conforms the other.
    // It is NOT doing it recursively, thus is does not check if
    // sub-records are conforming.
    // <br>This is used by Record, to see if another record can be assigned
    // to this record.
    Bool conform (const RecordDesc& other) const;

    // Test if this description equals another one.
    // It is equal if the number of fields is equal and all field names in
    // this description occur in the other too. The order of the fields
    // is not important.
    // <br>The flag equalDataTypes is set to True if the data types
    // of all fields match.
    // <br>Use function operator== if order and types are important,
    // but names are not.
    Bool isEqual (const RecordDesc& other, Bool& equalDataTypes) const;

    // Test if this description is a subset of another one.
    // It is similar to isEqual above.
    Bool isSubset (const RecordDesc& other, Bool& equalDataTypes) const;

    // Test if this description is a strict subset of another one, thus
    // if it is a subset and not equal.
    Bool isStrictSubset (const RecordDesc& other, Bool& equalDataTypes) const;

    // Test if this description is a superset of another one.
    Bool isSuperset (const RecordDesc& other, Bool& equalDataTypes) const;

    // Test if this description is a strict superset of another one, thus
    // if it is a superset and not equal.
    Bool isStrictSuperset (const RecordDesc& other,
			   Bool& equalDataTypes) const;

    // Test if the set of field names in this and other record description
    // is disjoint (i.e. if they do not share names).
    Bool isDisjoint (const RecordDesc& other) const;


private:
    // Writes/reads the RecordDesc to/from an output stream.
    // <group>
    ostream& put (ostream& os) const;
    AipsIO& put (AipsIO& os) const;
    AipsIO& get (AipsIO& os);
    // </group>

    // Use a copy-on-write pointer to the RecordDescRep.
    COWPtr<RecordDescRep> desc_p;
};



inline RecordDesc::RecordDesc()
: desc_p (new RecordDescRep)
{}

inline RecordDesc::RecordDesc (const RecordDesc& other)
: desc_p (other.desc_p)
{}

inline RecordDesc& RecordDesc::operator= (const RecordDesc& other)
{
    if (this != &other) {
	desc_p = other.desc_p;
    }
    return *this;
}

inline RecordDesc::~RecordDesc()
{}

inline uInt RecordDesc::addField (const String& fieldName, DataType dataType)
{
    return desc_p.rwRef().addField (fieldName, dataType);
}

inline uInt RecordDesc::addField (const String& fieldName,
				  DataType scalarOrArrayType,
				  const IPosition& shape)
{
    return desc_p.rwRef().addArray (fieldName, scalarOrArrayType, shape);
}

inline uInt RecordDesc::addField (const String& fieldName,
				  const RecordDesc& subDesc)
{
    return desc_p.rwRef().addRecord (fieldName, subDesc);
}

inline uInt RecordDesc::addTable (const String& fieldName,
				  const String& tableDescName)
{
    return desc_p.rwRef().addTable (fieldName, tableDescName);
}

inline const String& RecordDesc::comment (Int whichField) const
{
    return desc_p.ref().comment (whichField);
}

inline void RecordDesc::setComment (Int whichField, const String& comment)
{
    desc_p.rwRef().setComment (whichField, comment);
}

inline void RecordDesc::setShape (Int whichField, const IPosition& shape)
{
    desc_p.rwRef().setShape (whichField, shape);
}

inline uInt RecordDesc::mergeField (const RecordDesc& other,
			       Int whichFieldFromOther,
			       RecordInterface::DuplicatesFlag duplicateAction)
{
    return desc_p.rwRef().mergeField (other.desc_p.ref(), whichFieldFromOther,
				      duplicateAction);
}

inline uInt RecordDesc::merge (const RecordDesc& other,
			       RecordInterface::DuplicatesFlag duplicateAction)
{
    return desc_p.rwRef().merge (other.desc_p.ref(), duplicateAction);
}
    
inline uInt RecordDesc::removeField (Int whichField)
{
    return desc_p.rwRef().removeField (whichField);
}

inline void RecordDesc::renameField (const String& newName, Int whichField)
{
    desc_p.rwRef().renameField (newName, whichField);
}
 
inline Int RecordDesc::fieldNumber (const String& fieldName) const
{
    return desc_p.ref().fieldNumber (fieldName);
}

inline uInt RecordDesc::nfields() const
{
    return desc_p.ref().nfields();
}

inline DataType RecordDesc::type (Int whichField) const
{
    return desc_p.ref().type (whichField);
}

inline String RecordDesc::uniqueName (const String& name) const
{
    return desc_p.ref().uniqueName (name);
}

inline String RecordDesc::makeName (Int whichField) const
{
    return desc_p.ref().makeName (whichField);
}

inline const String& RecordDesc::name (Int whichField) const
{
    return desc_p.ref().name (whichField);
}

inline Bool RecordDesc::isArray (Int whichField) const
{
    return desc_p.ref().isArray (whichField);
}

inline Bool RecordDesc::isScalar (Int whichField) const
{
    return desc_p.ref().isScalar (whichField);
}

inline Bool RecordDesc::isSubRecord (Int whichField) const
{
    return desc_p.ref().isSubRecord (whichField);
}

inline Bool RecordDesc::isTable (Int whichField) const
{
    return desc_p.ref().isTable (whichField);
}

inline const IPosition& RecordDesc::shape (Int whichField) const
{
    return desc_p.ref().shape (whichField);
}

inline const String& RecordDesc::tableDescName (Int whichField) const
{
    return desc_p.ref().tableDescName (whichField);
}

inline const RecordDesc& RecordDesc::subRecord (Int whichField) const
{
    return desc_p.ref().subRecord (whichField);
}

inline RecordDesc& RecordDesc::rwSubRecord (Int whichField)
{
    return desc_p.rwRef().subRecord (whichField);
}

inline Bool RecordDesc::operator== (const RecordDesc& other) const
{
    return desc_p.ref() == other.desc_p.ref();
}

inline Bool RecordDesc::operator!= (const RecordDesc& other) const
{
    return desc_p.ref() != other.desc_p.ref();
}
inline Bool RecordDesc::conform (const RecordDesc& other) const
{
    return desc_p.ref().conform (other.desc_p.ref());
}

inline Bool RecordDesc::isEqual (const RecordDesc& other,
				 Bool& equalDataTypes) const
{
    return desc_p.ref().isEqual (other.desc_p.ref(), equalDataTypes);
}
inline Bool RecordDesc::isSubset (const RecordDesc& other,
				  Bool& equalDataTypes) const
{
    return desc_p.ref().isSubset (other.desc_p.ref(), equalDataTypes);
}
inline Bool RecordDesc::isStrictSubset (const RecordDesc& other,
					Bool& equalDataTypes) const
{
    return desc_p.ref().isStrictSubset (other.desc_p.ref(), equalDataTypes);
}
inline Bool RecordDesc::isSuperset (const RecordDesc& other,
				    Bool& equalDataTypes) const
{
    return other.desc_p.ref().isSubset (desc_p.ref(), equalDataTypes);
}
inline Bool RecordDesc::isStrictSuperset (const RecordDesc& other,
					  Bool& equalDataTypes) const
{
    return other.desc_p.ref().isStrictSubset (desc_p.ref(), equalDataTypes);
}
inline Bool RecordDesc::isDisjoint (const RecordDesc& other) const
{
    return desc_p.ref().isDisjoint (other.desc_p.ref());
}


inline ostream& operator<< (ostream& os, const RecordDesc& desc)
{
    return desc.put (os);
}
inline AipsIO& operator<< (AipsIO& os, const RecordDesc& desc)
{
    return desc.put (os);
}
inline AipsIO& operator>> (AipsIO& os, RecordDesc& desc)
{
    return desc.get (os);
}





} //# NAMESPACE CASACORE - END

#endif
