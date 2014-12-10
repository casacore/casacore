//# Record.h: A hierarchical collection of named fields of various types
//# Copyright (C) 1995,1996,1997,1998,2000,2001
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


#ifndef CASA_RECORD_H
#define CASA_RECORD_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/RecordInterface.h>
#include <casacore/casa/Containers/RecordRep.h>
#include <casacore/casa/Containers/RecordDesc.h>
#include <casacore/casa/Utilities/COWPtr.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template<class T> class Array;
class IPosition;
class AipsIO;
class ValueHolder;


// <summary>
// A hierarchical collection of named fields of various types
// </summary>

// <use visibility=export>
// <reviewed reviewer="Mark Wieringa" date="1996/04/15" tests="tRecord">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="RecordDesc">RecordDesc</linkto>.
//   <li> <linkto class="RecordInterface">RecordInterface</linkto>.
//   <li> <linkto class="RecordFieldPtr">RecordFieldPtr</linkto>.
// </prerequisite>

// <etymology>
// ``Record'' is a widely used term in both programming languages and data
// structures to denote an imhogeneous set of fields. An alternative would
// have been to name it <em>struct</em>ure, which would have perhaps been
// a clearer name for C++ programmers.
// </etymology>

// <synopsis>
// Class <linkto class=RecordInterface>RecordInterface</linkto> decribes
// the fundamental properties of records.
// <br>
// The Record class is a particular type of a record class.
// The fields in Record may be of scalar type, array type, or a Record.
// The types are chosen to be compatible with the native
// types of the Table system, viz: Bool, uChar, Short, Int, uInt, float,
// double, Complex, DComplex, String.
// Arrays of all these types are also available.
// Note that a Record is not a space-efficient way of storing small objects.
// <p>
// The structure of a Record is defined by the <linkto class="RecordDesc">
// RecordDesc</linkto> class. The structure of the Record can be defined at
// construction time. It can thereafter be restructured. This has the
// effect, however, that any existing RecordFieldPtr objects become
// invalid (using the <linkto file="Notice.h">Notice</linkto> classes).
// <br>
// It is possible to add or remove fields once a Record is constructed.
// However, this is not possible when the Record is constructed with a
// fixed structure (i.e. with the fixedStructure flag set).
// <p>
// A Record is an hierarchical structure, because it can have fields
// containing Record's (as layed out in the RecordDesc). A subrecord
// has a variable structure, when its RecordDesc is empty (i.e. contains
// no fields). It is fixed when its RecordDesc contains fields.
// <p>
// A Record may be assigned to another only if they conform; that is if their
// fields have the identical type in the identical order.
// The field names do not need to be identical however, only the types.
// That is, the structure needs to be identical, but
// not the labels. Note that field order is significant, 
// <src>[ifield(type=Int),ffield(type=float)]</src>
// is not the same as <src>[ffield(type=float),ifield(type=Int)]</src>
// <br>
// Conformance is checked recursively for fixed subrecords. That is, a
// variable structured subrecord is not checked, because any record
// can be assigned to it. A fixed structured subrecord has to
// conform the corresponding subrecord in the source.
// <p>
// Record uses copy-on-write semantics. This means that when a Record
// is copied, only the pointer to the underlying RecordRep object is copied.
// Only when the Record gets changed (i.e. when a non-const Record member
// function is called), the RecordRep object is copied.
// This results in a cheap copy behaviour.
// </synopsis>

// <example>
// Suppose we wanted to create a records that describe the favorite example
// of the OO world - an employee:
// <srcBlock>
// RecordDesc employeeDesc;
// employeeDesc.addField ("name", TpString);
// employeeDesc.addField ("salary", TpDouble);
// </srcBlock>
// The above creates the description (structure) for some record objects.
// <srcBlock>
// Record employeeA(employeeDesc);
// Record employeeB(employeeDesc, False);
// </srcBlock>
// And these two lines create Record objects which share this common structure.
// The first Record has a fixed structure, the 2nd variable.
// <srcBlock>
// RecordFieldPtr<String> nameA(employeeA, 0);
// RecordFieldPtr<String> nameB(employeeB, 0);
// RecordFieldPtr<double> salaryA(employeeA, 1);
// RecordFieldPtr<double> salaryB(employeeB, "salary");
// </srcBlock>
// This shows how we can get access to the individual fields. The fields are
// fundamentally identified by number, but the number can be looked up through
// the use of the fieldNumber member function.
// <srcBlock>
// nameA.define ("Tim");
// nameB.define ("Brian");
// salaryA.define (1.0e+8);
// salaryB.define (1.0 / *salaryA);
// </srcBlock>
// Once obtained, the fields are readily  manipulated, as shown above. Note
// that the field values are obtained through the dereference (<src>*</src>)
// operator. This is to identify that the field objects are <em>pointers</em>
// to the values in the underlying Record; that is
// <srcBlock>
// salaryA = salaryB;
// *salaryA = *salaryB;
// </srcBlock>
// Do very different things; the first line is a pointer copy; salaryA and 
// salaryB now point to the same field in salaryB. The second line is a value
// copy.
//
// Whole records can be copied as long as their structures are compatible, so
// that <src> employeeA = employeeB </src> is a legal statement. However, if
// the structure is changed, assignment is no longer possible, and all of the
// field pointers are invalidated:
// <srcBlock>
//    employeeB.define ("age", (Int)40);
//    employeeA = employeeB;                // exception - no longer conformant
// </srcBlock>
// </example>

// <motivation>
// Collections of data with different types are frequently needed.
// Record makes it possible to hold such data in a flexible way.
// </motivation>

// <todo asof="1996/03/12">
//   <li> A record reference class, which contains some fields from another
//        record, would likely be useful. This would be analagous to a
//        subarray sliced from an existing array.
// </todo>


class Record : public RecordInterface
{
friend class RecordRep;

public:
    // Create a record with no fields.
    // The record has a variable structure.
    Record();

    // Create a record with no fields.
    // The type determines if the record has a fixed or variable structure.
    // The callback function is called when a field is added to the Record.
    // That function can check the name and of data type of the new field
    // (for instance, the Table system uses it to ensure that table columns
    // and keywords have different names).
    explicit Record (RecordType type,
		     CheckFieldFunction* = 0, const void* checkArgument = 0);

    // Create a record with the given description. If it is not possible to 
    // create all fields (for example, if a field with an unsupported data
    // type is requested), an exception is thrown.
    // The type determines if the record has a fixed or variable structure.
    // All fields are checked by the field checking function (if defined)
    // (for instance, the Table system uses it to ensure that table columns
    // and keywords have different names).
    explicit Record (const RecordDesc& description, RecordType type = Fixed,
		     CheckFieldFunction* = 0, const void* checkArgument = 0);

    // Create a copy of other using copy semantics.
    Record (const Record& other);

    // Create a Record from another type of record using copy semantics.
    // Subrecords are also converted to a Record.
    Record (const RecordInterface& other);

    // Copy the data in the other record to this record.
    // It can operate in 2 ways depending on the Record structure flag.
    // <ul>
    // <li> For variable structured records the existing fields are
    //      thrown away and replaced by the new fields.
    //      This means that RecordFieldPtr's using this record get invalidated.
    //      Because copy-on-write semantics are used, this kind of
    //      assignment is a very efficient operation.
    // <li> For fixed structured records the existing values are replaced
    //      by the new values. This means that RecordFieldPtr's using this
    //      record remain valid.
    //      The structure of the other record has to conform this record
    //      or this record has to be empty, otherwise an exception is thrown.
    //      This assignment is less efficient, because it has to check the
    //      conformance and because each value has to be copied.
    // </ul>
    // <note role=warning>
    // Attributes like fixed structure flag and check function will not
    // be copied.
    // </note>
    Record& operator= (const Record& other);
    
    // Release resources associated with this object.
    ~Record();

    // Make a copy of this object.
    virtual RecordInterface* clone() const;

    // Assign that RecordInterface object to this one.
    // Unlike <src>operator=</src> it copies all data in the derived
    // class.
    virtual void assign (const RecordInterface& that);

    // Get the comment for this field.
    virtual const String& comment (const RecordFieldId&) const;

    // Set the comment for this field.
    virtual void setComment (const RecordFieldId&, const String& comment);

    // Describes the current structure of this Record.
    const RecordDesc& description() const;

    // Change the structure of this Record to contain the fields in
    // newDescription. After calling restructure, <src>description() ==
    // newDescription</src>. Any existing RecordFieldPtr objects are
    // invalidated (their <src>isAttached()</src> members return False) after
    // this call.
    // <br>When the new description contains subrecords, those subrecords
    // will be restructured if <src>recursive=True</src> is given.
    // Otherwise the subrecord is a variable empty record.
    // Subrecords will be variable if their description is empty (i.e. does
    // not contain any field), otherwise they are fixed. The 2nd form of
    // the <src>restructure</src> function will overwrite those implicit
    // record types with the given record type. The new type will also
    // be given to this top record.
    // <br>Restructuring is not possible and an exception is thrown
    // if the Record has a fixed structure.
    virtual void restructure (const RecordDesc& newDescription,
			      Bool recursive = True);

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
    Bool conform (const Record& other) const;

    // How many fields does this structure have? A convenient synonym for
    // <src>description().nfields()</src>.
    virtual uInt nfields() const;

    // Get the field number from the field name.
    // -1 is returned if the field name is unknown.
    virtual Int fieldNumber (const String& fieldName) const;

    // Get the data type of this field.
    virtual DataType type (Int whichField) const;

    // Remove a field from the record.
    // <note role=caution>
    // Removing a field means that the field number of the fields following
    // it will be decremented. Only the RecordFieldPtr's
    // pointing to the removed field will be invalidated.
    // </note>
    void removeField (const RecordFieldId&);

    // Rename the given field.
    void renameField (const String& newName, const RecordFieldId&);

    // Define a value for the given field containing a subrecord.
    // When the field is unknown, it will be added to the record.
    // The second version is meant for any type of record (e.g. Record,
    // TableRecord, GlishRecord). It is converted to a Record using the
    // Record constructor taking a RecordInterface object.
    // <group>
    void defineRecord (const RecordFieldId&, const Record& value,
		       RecordType type = Variable);
    virtual void defineRecord (const RecordFieldId&,
			       const RecordInterface& value,
			       RecordType = Variable);
    // </group>

    // Get the subrecord from the given field.
    // <note>
    // The non-const version has a different name to prevent that the
    // copy-on-write mechanism makes a copy when not necessary.
    // </note>
    // <group>
    const Record& subRecord (const RecordFieldId&) const;
    Record& rwSubRecord (const RecordFieldId&);
    virtual const RecordInterface& asRecord (const RecordFieldId&) const;
    virtual RecordInterface& asrwRecord (const RecordFieldId&);
    // </group>

    // Get or define the value as a ValueHolder.
    // This is useful to pass around a value of any supported type.
    // <group>
    ValueHolder asValueHolder (const RecordFieldId&) const;
    void defineFromValueHolder (const RecordFieldId&, const ValueHolder&);
    // </group>

    // Merge a field from another record into this record.
    // The DuplicatesFlag (as described in
    // <linkto class=RecordInterface>RecordInterface</linkto>) determines
    // what will be done in case the field name already exists.
    void mergeField (const Record& other, const RecordFieldId&,
		     DuplicatesFlag = ThrowOnDuplicates);

    // Merge all fields from the other record into this record.
    // The DuplicatesFlag (as described in
    // <linkto class=RecordInterface>RecordInterface</linkto>) determines
    // what will be done in case a field name already exists.
    // An exception will be thrown if other is the same as this
    // (i.e. if merging the record itself).
    void merge (const Record& other, DuplicatesFlag = ThrowOnDuplicates);
    
    // Write the Record to an output stream.
    friend AipsIO& operator<< (AipsIO& os, const Record& rec);
    
    // Read the Record from an input stream.
    friend AipsIO& operator>> (AipsIO& os, Record& rec);

    // Write the Record to an output stream.
    // This is used to write a subrecord, whose description has
    // not been written.
    void putRecord (AipsIO& os) const;
    
    // Read the Record from an input stream.
    // This is used to read a subrecord, whose description has
    // not been read.
    void getRecord (AipsIO& os);

    // Put the data of a record.
    // This is used to write a subrecord, whose description has
    // already been written.
    void putData (AipsIO& os) const;

    // Read the data of a record.
    // This is used to read a subrecord, whose description has
    // already been read.
    void getData (AipsIO& os, uInt version);

    // Make a unique record representation
    // (to do copy-on-write in RecordFieldPtr).
    virtual void makeUnique();

    // Print the contents of the record.
    // Only the first <src>maxNrValues</src> of an array will be printed.
    // A value < 0 means the entire array.
    virtual void print (std::ostream&,
			Int maxNrValues = 25,
			const String& indent="") const;


protected:
    // Used by the RecordField classes to attach in a type-safe way to the
    // correct field.
    // <group>
    virtual void* get_pointer (Int whichField, DataType type) const;
    virtual void* get_pointer (Int whichField, DataType type,
			       const String& recordType) const;
    // </group>

    // Return a const reference to the underlying RecordRep.
    const RecordRep& ref() const;

    // Return a non-const reference to the underlying RecordRep.
    // When needed, the RecordRep will be copied and all RecordField
    // objects will be notified.
    RecordRep& rwRef();

    // Add a field to the record.
    virtual void addDataField (const String& name, DataType type,
			       const IPosition& shape, Bool fixedShape,
			       const void* value);

    // Define a value in the given field.
    virtual void defineDataField (Int whichField, DataType type,
				  const void* value);

private:
    // Get the description of this record.
    virtual RecordDesc getDescription() const;

    // Create Record as a subrecord.
    // When the description is empty, the record has a variable structure.
    // Otherwise it is fixed.
    // <group>
    Record (RecordRep* parent, const RecordDesc& description);
    Record (RecordRep* parent, RecordType type);
    // </group>

    // The Record representation.
    COWPtr<RecordRep> rep_p;
    // The parent Record.
    RecordRep* parent_p;
};



inline const RecordRep& Record::ref() const
{
    return rep_p.ref();
}
inline const RecordDesc& Record::description() const
{
    return ref().description();
}

inline Bool Record::conform (const Record& other) const
{
    return ref().conform (other.ref());
}

inline AipsIO& operator<< (AipsIO& os, const Record& rec)
{
    rec.putRecord (os);
    return os;
}
inline void Record::putData (AipsIO& os) const
{
    ref().putData (os);
}

inline AipsIO& operator>> (AipsIO& os, Record& rec)
{
    rec.getRecord (os);
    return os;
}
inline void Record::getData (AipsIO& os, uInt version)
{
    rwRef().getData (os, version);
}




} //# NAMESPACE CASACORE - END

#endif
