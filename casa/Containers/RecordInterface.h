//# RecordInterface.h: Abstract base class for Record classes
//# Copyright (C) 1996,1997,1998,1999,2001
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


#ifndef CASA_RECORDINTERFACE_H
#define CASA_RECORDINTERFACE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Containers/RecordFieldId.h>
#include <casacore/casa/Arrays/Array.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class RecordDesc;
class ValueHolder;
class IPosition;


// <summary>
// Abstract base class for Record classes
// </summary>

// <use visibility=export>
// <reviewed reviewer="Mark Wieringa" date="1996/04/15" tests="tRecord">
// </reviewed>

//# <prerequisite>
//# </prerequisite>

// <etymology>
// ``Record'' is a widely used term in both programming languages and data
// structures to denote an imhogeneous set of fields. An alternative would
// have been to name it <em>struct</em>ure, which would have perhaps been
// a clearer name for C++ programmers.
// <br>
// RecordInterface denotes that this class defines the common interface to
// possible Record classes.
// </etymology>

// <synopsis>
// A Record is an heterogeneous, hierarchical, collection of named fields. The
// fields may be of scalar type, array type, a Table or a Record. This latter
// feature is what makes the Record a (potentially) hierarchical type.
// <p>
// RecordInterface is the abstract base class for various Record classes.
// At the moment three Record classes exist:
// <ul>
// <li> <linkto class=Record>Record</linkto>
// <li> <linkto class=TableRecord>TableRecord</linkto>
// </ul>
// Presently, the scalar types are chosen to be compatible with the native
// types of the Table system, viz: bool, unsigned char, int16_t, int32_t, uint32_t, int64_t,
// float, double, Complex, DComplex, String.
// Arrays of all these types are also available.
// It is fairly straightforward to extend this set if necessary, although it
// will result in more template instantiations with the current implementation.
// <p>
// Each field has an integral index, which ranges between 0 and 
// <src>nfields() - 1</src>. The values of a field can be manipulated
// in two ways:
// <ol>
//  <li> Through the get and put functions in this class.
//       They are easy to use and support type promotion.
//       However, they are a bit less efficient than the second way.
//  <li> Through the class
//       <linkto class="RecordFieldPtr">RecordFieldPtr</linkto>.
//       This is a bit less convenient. However, it is more efficient if
//       the same field is accessed multiple times.
// </ol>
// The structure of a record can be fixed or variable.
// If fixed, it is not possible to change the structure once the
// record has been instantiated. If variable, the record can be
// restructured or fields can be added/removed.
// <br>
// When a field gets added, it is possible to check if its name and
// type are valid by means of the CheckFunction callback. This is
// for instance used by the table system to assure that keywords
// and columns in a table do not have the same name.
// <p>
// Arrays in a record description can be fixed or variable shaped.
// If fixed shaped, only arrays with that shape can be stored
// in that field in the record. If variable shaped, any array
// can be stored.
// <br> However, note there is a difference between assign and define.
// Assign invokes the array assignment operator which checks for
// conformance. Thus even for variable shaped arrays, the new array
// must conform the exisitng one when using assign. Define simply replaces
// the array, thus for variable shaped arrays ay array shape will do.
// <p>
// RecordFieldPtr objects attached to a Record have to be notified when
// the Record is deleted or changed. 
// The RecordInterface class provides the hooks for this via the
// Notice system. It is derived from
// <linkto class=NoticeSource> NoticeSource</linkto>. The class
// <linkto class=RecordNotice>RecordNotice</linkto> is for the messages.
// </synopsis>

// <motivation>
// This common base class provides a common interface to the various
// Record classes.
// Furthermore it is needed for the class RecordFieldPtr.
// Finally it provides the hooks for the notification in case the
// record structure changes.
// </motivation>
//
// <todo asof="1996/03/10">
//   <li> A record reference class, which contains some fields from another
//        record, would likely be useful. This would be analagous to a
//        subarray sliced from an existing array.
// </todo>


class RecordInterface
{
public:
    // Define the flag telling if a Record has a fixed or
    // variable structure.
    enum RecordType {
	// Record has a fixed structure; that is, no fields can
	// be added or removed once the Record is created.
	Fixed,
	// Record has a variable structure; after Record creation
	// fields can be added or removed at will.
	Variable};

    // Define the Duplicates flag for the function merge in the various
    // record classes.
    // This function merges the fields from that record (description)
    // into this one.
    // DuplicatesFlag determines what to do if a field already exists.
    enum DuplicatesFlag {
	// Rename a name from the other set to name_n,
	// where n is the first positive number making the name unique.
	RenameDuplicates,
	// Skip duplicate names from the other set.
	SkipDuplicates,
	// Overwrite the value of a duplicate keyword
	// This will also happen if their types differ.
	OverwriteDuplicates,
	// Throw an exception.
	ThrowOnDuplicates};

    // Define the signature of the add callback function.
    // This function is called when a field is added to the record
    // (thus also when a Record is constructed from a RecordDesc).
    // The function can check if the name and/or data type are valid.
    // The extra argument is the argument given to the Record constructor
    // which can be used to pass non-Record information.
    // The function should return false if name or data type is invalid.
    // In that case it can fill the message string, which will be added
    // to the message in the thrown exception.
    typedef bool CheckFieldFunction (const String& fieldName,
				     DataType dataType,
				     const void* extraArgument,
				     String& message);

    // The default constructor creates an empty record with a variable
    // structure.
    RecordInterface();

    // Create a record with no fields.
    // The callback function is called when a field is added to the Record.
    // That function can check the name and of data type of the new field
    // (for instance, the Table system uses it to ensure that table columns
    // and keywords have different names).
    RecordInterface (RecordType type, CheckFieldFunction* funcPtr,
		     const void* checkArgument);

    // Copy constructor (copy semantics).
    RecordInterface (const RecordInterface& other);

    // Assignment (copy semantics).
    // This only assigns the RecordInterface object itself,
    // thus not the data in a derived class.
    // To do that the function <src>assign</src> below can be used.
    RecordInterface& operator= (const RecordInterface& other);

    // Destruct the record.
    // All attached RecordFieldPtr objects are notified to detach themselves.
    virtual ~RecordInterface();

    // Make a copy of this object.
    virtual RecordInterface* clone() const = 0;

    // Assign that RecordInterface object to this one.
    // Unlike <src>operator=</src> it copies all data in the derived
    // class.
    virtual void assign (const RecordInterface& that) = 0;

    // Is the Record structure fixed (i.e. impossible to restructure or
    // to add or remove fields)?
    bool isFixed() const;

    // How many fields does this structure have?
    // <group>
    virtual uint32_t nfields() const = 0;
    uint32_t size() const
        { return nfields(); }
    // </group>

    // Is the record empty?
    bool empty() const
        { return size() == 0; }

    // Get the field number from the field name.
    // -1 is returned if the field name is unknown.
    virtual int32_t fieldNumber (const String& fieldName) const = 0;

    // Get the field number for the given field id.
    // It throws an exception if id is unrecognized (e.g. an unknown name).
    int32_t idToNumber (const RecordFieldId&) const;

    // Test if a field name exists.
    //# Is here for backward compatibility with KeywordSet.
    bool isDefined (const String& fieldName) const;

    // Get the data type of this field (as defined in DataType.h).
    // <group>
    virtual DataType type (int32_t whichField) const = 0;
    DataType dataType (const RecordFieldId&) const;
    // </group>

    // Get the name of this field.
    String name (const RecordFieldId&) const;

    // Get the comment for this field.
    virtual const String& comment (const RecordFieldId&) const = 0;

    // Set the comment for this field.
    virtual void setComment (const RecordFieldId&, const String& comment) = 0;

    // Get the actual shape of this field.
    // It returns [1] for non-array fields.
    IPosition shape (const RecordFieldId&) const;

    // Get the description of this record.
    RecordDesc description() const;

    // Change the structure of this Record to contain the fields in
    // newDescription. After calling restructure, <src>description() ==
    // newDescription</src>. Any existing RecordFieldPtr objects are
    // invalidated (their <src>isAttached()</src> members return false) after
    // this call.
    // <br>If the new description contains subrecords, those subrecords
    // will be restructured if <src>recursive=true</src> is given.
    // Otherwise the subrecord is a variable empty record.
    // Subrecords will be variable if their description is empty (i.e. does
    // not contain any field), otherwise they are fixed.
    // <br>Restructuring is not possible and an exception is thrown
    // if the Record has a fixed structure.
    virtual void restructure (const RecordDesc& newDescription,
			      bool recursive=true) = 0;

    // Remove a field from the record.
    // <note role=caution>
    // Removing a field means that the field number of the fields following
    // it will be decremented. It will invalidate RecordFieldPtr's
    // pointing to the removed field, but no other RecordFieldPtr's.
    // </note>
    virtual void removeField (const RecordFieldId&) = 0;

    // Get or define the value as a ValueHolder.
    // This is useful to pass around a value of any supported type.
    // <group>
    virtual ValueHolder asValueHolder (const RecordFieldId&) const;
    virtual void defineFromValueHolder (const RecordFieldId&,
                                        const ValueHolder&);
    // </group>

    // Define a value for the given field.
    // Array conformance rules will not be applied for variable shaped arrays.
    // If the field and value data type mismatch, type promotion
    // of scalars will be done if possible. If not possible, an exception
    // is thrown.
    // <br>
    // If the field does not exist, it will be added to the record.
    // This results in an exception for fixed structured records.
    // The field is checked by a possible field checking function
    // before it gets added.
    // <group>
    void define (const RecordFieldId&, bool value);
    void define (const RecordFieldId&, unsigned char value);
    void define (const RecordFieldId&, int16_t value);
    void define (const RecordFieldId&, int32_t value);
    void define (const RecordFieldId&, uint32_t value);
    void define (const RecordFieldId&, int64_t value);
    void define (const RecordFieldId&, float value);
    void define (const RecordFieldId&, double value);
    void define (const RecordFieldId&, const Complex& value);
    void define (const RecordFieldId&, const DComplex& value);
    void define (const RecordFieldId&, const char* value);
    void define (const RecordFieldId&, const String& value);
    void define (const RecordFieldId&, const Array<bool>& value,
		 bool FixedShape = false);
    void define (const RecordFieldId&, const Array<unsigned char>& value,
		 bool FixedShape = false);
    void define (const RecordFieldId&, const Array<int16_t>& value,
		 bool FixedShape = false);
    void define (const RecordFieldId&, const Array<int32_t>& value,
		 bool FixedShape = false);
    void define (const RecordFieldId&, const Array<uint32_t>& value,
		 bool FixedShape = false);
    void define (const RecordFieldId&, const Array<int64_t>& value,
		 bool FixedShape = false);
    void define (const RecordFieldId&, const Array<float>& value,
		 bool FixedShape = false);
    void define (const RecordFieldId&, const Array<double>& value,
		 bool FixedShape = false);
    void define (const RecordFieldId&, const Array<Complex>& value,
		 bool FixedShape = false);
    void define (const RecordFieldId&, const Array<DComplex>& value,
		 bool FixedShape = false);
    void define (const RecordFieldId&, const Array<String>& value,
		 bool FixedShape = false);
    virtual void defineRecord (const RecordFieldId&,
			       const RecordInterface& value,
			       RecordType = Variable) = 0;
    // </group>

    // Get the value of the given field.
    // If the field and value data type mismatch, type promotion
    // will be done if possible. If not possible, an exception
    // is thrown.
    // If the value argument is an array, it will be reshaped if needed.
    // <group>
    void get (const RecordFieldId&, bool& value) const;
    void get (const RecordFieldId&, unsigned char& value) const;
    void get (const RecordFieldId&, int16_t& value) const;
    void get (const RecordFieldId&, int32_t& value) const;
    void get (const RecordFieldId&, uint32_t& value) const;
    void get (const RecordFieldId&, int64_t& value) const;
    void get (const RecordFieldId&, float& value) const;
    void get (const RecordFieldId&, double& value) const;
    void get (const RecordFieldId&, Complex& value) const;
    void get (const RecordFieldId&, DComplex& value) const;
    void get (const RecordFieldId&, String& value) const;
    void get (const RecordFieldId&, Array<bool>& value) const;
    void get (const RecordFieldId&, Array<unsigned char>& value) const;
    void get (const RecordFieldId&, Array<int16_t>& value) const;
    void get (const RecordFieldId&, Array<int32_t>& value) const;
    void get (const RecordFieldId&, Array<uint32_t>& value) const;
    void get (const RecordFieldId&, Array<int64_t>& value) const;
    void get (const RecordFieldId&, Array<float>& value) const;
    void get (const RecordFieldId&, Array<double>& value) const;
    void get (const RecordFieldId&, Array<Complex>& value) const;
    void get (const RecordFieldId&, Array<DComplex>& value) const;
    void get (const RecordFieldId&, Array<String>& value) const;
    // </group>

    // The following functions get the value based on field name or number.
    // The scalar functions promote the data type if needed. It also supports
    // conversion of int32_t to bool.
    // <br>The array functions throw an exception if the data type mismatches.
    // The toArrayX function can be used for array type promotion.
    // <group>
    bool            asBool    (const RecordFieldId&) const;
    unsigned char           asuChar   (const RecordFieldId&) const;
    int16_t           asShort   (const RecordFieldId&) const;
    int32_t             asInt     (const RecordFieldId&) const;
    uint32_t            asuInt    (const RecordFieldId&) const;
    int64_t           asInt64   (const RecordFieldId&) const;
    float           asFloat   (const RecordFieldId&) const;
    double          asDouble  (const RecordFieldId&) const;
    Complex         asComplex (const RecordFieldId&) const;
    DComplex        asDComplex(const RecordFieldId&) const;
    const String&   asString  (const RecordFieldId&) const;
    const Array<bool>&     asArrayBool    (const RecordFieldId&) const;
    const Array<unsigned char>&    asArrayuChar   (const RecordFieldId&) const;
    const Array<int16_t>&    asArrayShort   (const RecordFieldId&) const;
    const Array<int32_t>&      asArrayInt     (const RecordFieldId&) const;
    const Array<uint32_t>&     asArrayuInt    (const RecordFieldId&) const;
    const Array<int64_t>&    asArrayInt64   (const RecordFieldId&) const;
    const Array<float>&    asArrayFloat   (const RecordFieldId&) const;
    const Array<double>&   asArrayDouble  (const RecordFieldId&) const;
    const Array<Complex>&  asArrayComplex (const RecordFieldId&) const; 
    const Array<DComplex>& asArrayDComplex(const RecordFieldId&) const;
    const Array<String>&   asArrayString  (const RecordFieldId&) const;
    virtual const RecordInterface& asRecord (const RecordFieldId&) const = 0;
    virtual RecordInterface& asrwRecord (const RecordFieldId&) = 0;
    // </group>

    // Get an array while promoting the data as needed.
    // int32_t values can be converted to bool.
    // A scalar value is also converted to an array.
    // These functions are slower than <src>asX</src>, but more general.
    // <group>
    Array<bool>     toArrayBool    (const RecordFieldId&) const;
    Array<unsigned char>    toArrayuChar   (const RecordFieldId&) const;
    Array<int16_t>    toArrayShort   (const RecordFieldId&) const;
    Array<int32_t>      toArrayInt     (const RecordFieldId&) const;
    Array<uint32_t>     toArrayuInt    (const RecordFieldId&) const;
    Array<int64_t>    toArrayInt64   (const RecordFieldId&) const;
    Array<float>    toArrayFloat   (const RecordFieldId&) const;
    Array<double>   toArrayDouble  (const RecordFieldId&) const;
    Array<Complex>  toArrayComplex (const RecordFieldId&) const; 
    Array<DComplex> toArrayDComplex(const RecordFieldId&) const;
    Array<String>   toArrayString  (const RecordFieldId&) const;
    void toArray (const RecordFieldId& id, Array<bool>& array) const
      { array.reference (toArrayBool (id)); }
    void toArray (const RecordFieldId& id, Array<unsigned char>& array) const
      { array.reference (toArrayuChar (id)); }
    void toArray (const RecordFieldId& id, Array<int16_t>& array) const
      { array.reference (toArrayShort (id)); }
    void toArray (const RecordFieldId& id, Array<int32_t>& array) const
      { array.reference (toArrayInt (id)); }
    void toArray (const RecordFieldId& id, Array<uint32_t>& array) const
      { array.reference (toArrayuInt (id)); }
    void toArray (const RecordFieldId& id, Array<int64_t>& array) const
      { array.reference (toArrayInt64 (id)); }
    void toArray (const RecordFieldId& id, Array<float>& array) const
      { array.reference (toArrayFloat (id)); }
    void toArray (const RecordFieldId& id, Array<double>& array) const
      { array.reference (toArrayDouble (id)); }
    void toArray (const RecordFieldId& id, Array<Complex>& array) const
      { array.reference (toArrayComplex (id)); }
    void toArray (const RecordFieldId& id, Array<DComplex>& array) const
      { array.reference (toArrayDComplex (id)); }
    void toArray (const RecordFieldId& id, Array<String>& array) const
      { array.reference (toArrayString (id)); }
    // </group>

    // Get value based on field name or number.
    // They are here for backward compatibility with the old KeywordSet
    // classes and will be removed in the future.
    // <group>
    float           asfloat   (const RecordFieldId&) const;
    double          asdouble  (const RecordFieldId&) const;
    const Array<float>&    asArrayfloat   (const RecordFieldId&) const;
    const Array<double>&   asArraydouble  (const RecordFieldId&) const;
    // </group>

    // Make a unique record representation
    // (for copy-on-write in RecordFieldPtr).
    virtual void makeUnique() = 0;

    // Define a data field (for RecordFieldPtr).
    //# This function has to be public for the global defineRecordFieldPtr
    //# functions in RecordField.h.
    virtual void defineDataField (int32_t whichField, DataType type,
				  const void* value) = 0;

    // Used by the RecordFieldPtr classes to attach to the correct field.
    //# This function has to be public for the global attachRecordFieldPtr
    //# functions in RecordField.h.
    // The latter function is used to attach to a Record-type field
    // checking if the correct Record type is used.
    // <group>
    virtual void* get_pointer (int32_t whichField, DataType type) const = 0;
    virtual void* get_pointer (int32_t whichField, DataType type,
			       const String& recordType) const = 0;
    // </group>

    // Print the contents of the record.
    // Only the first <src>maxNrValues</src> of an array will be printed.
    // A value < 0 means the entire array.
    // <group>
    friend inline std::ostream& operator<< (std::ostream& os,
					    const RecordInterface& rec)
      { rec.print (os, 25, "  "); return os; }
    virtual void print (std::ostream&,
			int32_t maxNrValues = 25,
			const String& indent="") const = 0;
    // </group>


protected:
    // Let the derived class add an array field with the given type, shape,
    // and value.
    virtual void addDataField (const String& name, DataType type,
			       const IPosition& shape, bool fixedShape,
			       const void* value) = 0;

    // Check if the Record has a non-fixed structure.
    // If it is fixed, it throws an exception.
    // This can be used by other functions (like define).
    void throwIfFixed() const;

    // Check if the new field name is correct.
    // This is done by calling the checkFunction (if defined).
    // If incorrect, an exception is thrown.
    void checkName (const String& fieldName, DataType type) const;

    // Give access to the RecordType flag (write-access is needed when
    // a record is read back).
    // <group>
    RecordType& recordType();
    RecordType recordType() const;
    // </group>

    // Get the field number for the given field id.
    // It returns -1 if an unknown name was given.
    int32_t newIdToNumber (const RecordFieldId&) const;

    // Add a scalar field with the given type and value.
    // An exception is thrown if the record structure is fixed
    // or if the name is invalid.
    void defineField (const RecordFieldId&, DataType type, const void* value);

    // Add an array field with the given type, shape and value.
    // An exception is thrown if the record structure is fixed
    // or if the name is invalid.
    void defineField (const RecordFieldId&, DataType type,
		      const IPosition& shape, bool fixedShape,
		      const void* value);


private:
    // Get the description of this record.
    virtual RecordDesc getDescription() const = 0;

    // Holds the callback function plus argument.
    CheckFieldFunction* checkFunction_p;
    const void*         checkArgument_p;

    // Defines if the Record has a fixed structure.
    RecordType type_p;
};


inline bool RecordInterface::isFixed() const
{
    return  (type_p == Fixed);
}
inline bool RecordInterface::isDefined (const String& fieldName) const
{
    return  (fieldNumber(fieldName) >= 0);
}
inline RecordInterface::RecordType& RecordInterface::recordType()
{
    return type_p;
}
inline RecordInterface::RecordType RecordInterface::recordType() const
{
    return type_p;
}
inline DataType RecordInterface::dataType (const RecordFieldId& id) const
{
    return type (idToNumber(id));
}
inline void RecordInterface::define (const RecordFieldId& id, const char* value)
{
    define (id, String(value));
}
inline float RecordInterface::asfloat (const RecordFieldId& id) const
{
    return asFloat (id);
}
inline double RecordInterface::asdouble (const RecordFieldId& id) const
{
    return asDouble (id);
}
inline const Array<float>& RecordInterface::asArrayfloat
                                         (const RecordFieldId& id) const
{
    return asArrayFloat (id);
}
inline const Array<double>& RecordInterface::asArraydouble
                                         (const RecordFieldId& id) const
{
    return asArrayDouble (id);
}

} //# NAMESPACE CASACORE - END

#endif
