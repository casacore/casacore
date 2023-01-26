//# RecordField.h: Access to an individual field in a record
//# Copyright (C) 1995,1996,1997
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


#ifndef CASA_RECORDFIELD_H
#define CASA_RECORDFIELD_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Record.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableRecord;
class Table;


// <summary>
// Access to an individual field in a record.
// </summary>

// <use visibility=export>
// <reviewed reviewer="Mark Wieringa" date="1996/04/15" tests="tRecord">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="RecordInterface">RecordInterface</linkto>.
// </prerequisite>

// <etymology>
// RecordFieldPtr indicates that an object of this type is
// pointing to a field in a record.
// </etymology>

// <synopsis>
// RecordFieldPtr allows access to the fields in a record object.
// A record object is an object of a class derived from
// <linkto class=RecordInterface>RecordInterface</linkto>.
// <src>RecordFieldPtr<T></src> objects can only be instantiated for types `T'
// which are valid fields of a record object (e.g. int32_t, float, String,
// Record, TableRecord). It can, however, NOT be instantiated for
// a Table field, because Table fields are accessed indirectly via a
// TableKeyword object. Table fields have to be accessed directly
// through the <linkto class=TableRecord>TableRecord</linkto> interface.
// <p>
// Internally, a RecordFieldPtr stores a Record pointer and
// field number. Therefore, if the order of fields in a Record is modified,
// or if the Record is restructured, a RecordFieldPtr is invalidated and should
// no longer be used.
// <p>
// The RecordFieldPtr is pointer-like in the sense that it points to an
// object that is physically inside of another object (the enclosing
// record object).
// Access to the value is obtained via the dereference operator
// (<src>operator*()</src>) to emphasize the pointer like nature of these
// classes.
// <br>
// An alternative way to get access to the values is using the
// functions define and get. Note that in
// <srcblock>
//    RecordFieldPtr<Array<int32_t> > field (record, fieldNumber);
//    Array<int32_t> value;
//    *field = value;
//    field.define (value);
// </srcblock>
// the assignment (in line 3) and define (in line 4) are not equivalent.
// The assignment uses the normal Array assignment, thus it takes the
// Array conformance rules into account (an assign is only possible when
// the new array value conforms the current array value or when the current
// array value is empty).
// On the other hand, define does not take the current array value into
// account. Thus an array value can always be redefined.
// <br>
// However, note that if the field is defined with a non-fixed shape in
// the record description, a value must always conform that shape (in
// case of assignment as well as in case of define).
// </synopsis>

// <example>
// See the example in the <linkto class="Record">Record</linkto> class.
// </example>

// <motivation>
// RecordFieldPtr provides a fast way to access the data in a record.
// </motivation>

template<class T> class RecordFieldPtr
{
public:
    // This object does not point to any field, i.e. 
    // <src>this->isAttached() == false;</src>
    RecordFieldPtr();

    // Attach this field pointer to the given field. If it does not exist
    // an exception is thrown.
    // <group>
    RecordFieldPtr (RecordInterface& record, int32_t whichField);
    RecordFieldPtr (RecordInterface& record, const RecordFieldId&);
    // </group>

    // Change our pointer to the supplied field. If it doesn't exist an
    // exception is thrown.
    // <group>
    void attachToRecord (RecordInterface& record, int32_t whichField);
    void attachToRecord (RecordInterface& record, const RecordFieldId&);
    // </group>

    // Point to no field in any Record.
    void detach();

    // Provide access to the field's value.
    // <note>
    // To be sure a const function is called, it is best to use get().
    // For a non-const object, a non-const function is called, even if
    // used as an rvalue.
    // </note>
    // <group>
    T& operator*();
    const T& operator*() const { return get(); }
    const T& get() const { return *get_typed_ptr(parent_p, fieldNumber_p); }
    // </group>

    // Store a value in the field using redefinition.
    // Define differs from assignment w.r.t. arrays.
    // For define a variable shaped array is deleted first with the
    // effect that array conformance rules are not applied for them.
    void define (const T& value);

    // Get the comment of this field.
    const String& comment() const;

    // Set the comment for this field.
    void setComment (const String& comment);

    // Return the fieldnumber of this field.
    int32_t fieldNumber() const
	{return fieldNumber_p;}

    // Return the name of the field.
    String name() const
        {return parent_p->name (fieldNumber_p);}

    // Is this field pointer attached to a valid record? Operations which
    // might cause it to become detached are:
    // <ol>
    //     <li> Destruction of the Record
    //     <li> Restructuring of the record.
    //     <li> Explicit call of the detach() member.
    // </ol>
    //# This inherited function is shown for documentation purposes.
    bool isAttached() const
	{return parent_p;}

private:
    static const T* get_typed_ptr(RecordInterface* record, int32_t fieldNumber);
  
    RecordInterface* parent_p;
    int32_t              fieldNumber_p;
};


// <summary>
//  Read-Only access to an individual field from a Record.
// </summary>

// <use visibility=export>
// <reviewed reviewer="Mark Wieringa" date="1996/04/15" tests="tRecord">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="RecordFieldPtr">RecordRecordFieldPtr</linkto>.
// </prerequisite>
//
// <synopsis>
// This class is entirely like <linkto class="RecordFieldPtr">
// RecordFieldPtr</linkto>, except that it only allows Read-Only
// access to fields in a Record. The documentation for that class should
// be consulted.
// <p>
// Note that RecordFieldPtr is not inherited from RORecordFieldPtr,
// because that would give problems with the function attachToRecord.
// It would allow RecordFieldPtr to attach to a const RecordInterface object.
// </synopsis>

template<class T> class RORecordFieldPtr
{
public:
    RORecordFieldPtr() {}
    RORecordFieldPtr (const RecordInterface& record, int32_t whichField)
	: fieldPtr_p((RecordInterface&)record, whichField) {}
    RORecordFieldPtr (const RecordInterface& record, const RecordFieldId& id)
	: fieldPtr_p((RecordInterface&)record, id) {}
    RORecordFieldPtr (const RecordFieldPtr<T>& other)
	: fieldPtr_p(other) {}
    RORecordFieldPtr (const RORecordFieldPtr<T>& other)
	: fieldPtr_p(other.fieldPtr_p) {}
    RORecordFieldPtr<T>& operator= (const RORecordFieldPtr<T>& other)
	{ fieldPtr_p = other.fieldPtr_p; return *this;}

    ~RORecordFieldPtr() {}

    void attachToRecord (const RecordInterface& record, int32_t whichField)
        { fieldPtr_p.attachToRecord ((RecordInterface&)record, whichField); }
    void attachToRecord (const RecordInterface& record, const RecordFieldId& id)
        { fieldPtr_p.attachToRecord ((RecordInterface&)record, id); }
    
    const T& operator*() const  {return *fieldPtr_p;}
    const T& get() const  {return fieldPtr_p.get();}

    const String& comment() const  {return fieldPtr_p.comment();}

    int32_t fieldNumber() const
	{return fieldPtr_p.fieldNumber();}

    void detach() {fieldPtr_p.detach(); }
    bool isAttached() const {return fieldPtr_p.isAttached(); }

private:
    RecordFieldPtr<T> fieldPtr_p;
};

} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Containers/RecordField.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
