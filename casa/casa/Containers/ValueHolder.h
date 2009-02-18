//# ValueHolder.h: A holder object for the standard AIPS++ data types
//# Copyright (C) 2005
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


#ifndef CASA_VALUEHOLDER_H
#define CASA_VALUEHOLDER_H

//# Includes
#include <casa/Containers/ValueHolderRep.h>
#include <casa/Arrays/Array.h>

namespace casa { //# NAMESPACE CASA - BEGIN


// <summary>
// A holder for a value of any basic AIPS++ data type.
// </summary>

// <use visibility=export>
// <reviewed reviewer="" date="" tests="tValueHolder">
// </reviewed>

// <synopsis>
// Class ValueHolder is meant to be used for holding a single AIPS++ value.
// The value can be  scalar or an array of any basic type (including complex
// and string). Also a Record value is possible.
// In this way varying typed data (e.g. the result of getCell in the table DO)
// can be packed in a strongly typed variable.
// <br>All unsigned integer type values are kept as signed 32-bit integers
// because scripting languages usually only support those types.
//
// ValueHolder is an envelope class that holds a counted-referenced letter
// object <linkto class=ValueHolderRep>ValueHolderRep</linkto>.
// </synopsis>

// <motivation>
// This class comes handy in passing arbitrary values from a DO to
// its environment.
// </motivation>

class ValueHolder
{
public:
  // Construct a null object.
  ValueHolder()
    : itsRep(0)
    {}

  // Create the object for the given value.
  // <group>
  explicit ValueHolder (Bool value);
  explicit ValueHolder (uChar value);
  explicit ValueHolder (Short value);
  explicit ValueHolder (uShort value);
  explicit ValueHolder (Int value);
  explicit ValueHolder (uInt value);
  explicit ValueHolder (Float value);
  explicit ValueHolder (Double value);
  explicit ValueHolder (const Complex& value);
  explicit ValueHolder (const DComplex& value);
  explicit ValueHolder (const Char* value);
  explicit ValueHolder (const String& value);
  explicit ValueHolder (const Array<Bool>& value);
  explicit ValueHolder (const Array<uChar>& value);
  explicit ValueHolder (const Array<Short>& value);
  explicit ValueHolder (const Array<uShort>& value);
  explicit ValueHolder (const Array<Int>& value);
  explicit ValueHolder (const Array<uInt>& value);
  explicit ValueHolder (const Array<Float>& value);
  explicit ValueHolder (const Array<Double>& value);
  explicit ValueHolder (const Array<Complex>& value);
  explicit ValueHolder (const Array<DComplex>& value);
  explicit ValueHolder (const Array<String>& value);
  explicit ValueHolder (const Record& value);
  // </group>

  // Create an empty N-dim array.
  ValueHolder (uInt ndim, Bool dummy);

  // Create a ValueHolder from a ValueHolderRep.
  // It takes over the pointer and deletes it in the destructor.
  explicit ValueHolder (ValueHolderRep* rep)
    : itsRep (rep)
    { if (itsRep) itsRep->link(); }

  // Copy constructor (reference semantics).
  ValueHolder (const ValueHolder&);

  // Destructor.
  ~ValueHolder()
    { ValueHolderRep::unlink (itsRep); }

  // Assignment (reference semantics).
  ValueHolder& operator= (const ValueHolder&);

  // Is this a null object?
  Bool isNull() const
    { return itsRep == 0; }

  // Get the data type (as defined in DataType.h).
  DataType dataType() const;
    
  // Get the value.
  // It throws an exception if the data type is incorrect.
  // <group>
  Bool                  asBool    () const;
  uChar                 asuChar   () const;
  Short                 asShort   () const;
  uShort                asuShort  () const;
  Int                   asInt     () const;
  uInt                  asuInt    () const;
  Float                 asFloat   () const;
  Double                asDouble  () const;
  Complex               asComplex () const;
  DComplex              asDComplex() const;
  const String&         asString  () const;
  const Array<Bool>     asArrayBool    () const;
  const Array<uChar>    asArrayuChar   () const;
  const Array<Short>    asArrayShort   () const;
  const Array<uShort>   asArrayuShort  () const;
  const Array<Int>      asArrayInt     () const;
  const Array<uInt>     asArrayuInt    () const;
  const Array<Float>    asArrayFloat   () const;
  const Array<Double>   asArrayDouble  () const;
  const Array<Complex>  asArrayComplex () const; 
  const Array<DComplex> asArrayDComplex() const;
  const Array<String>   asArrayString  () const;
  const Record&         asRecord       () const;
  // </group>

  // Get the data in a way useful for templates.
  // <group>
  void getValue (Bool& value) const            { value = asBool(); }
  void getValue (uChar& value) const           { value = asuChar(); }
  void getValue (Short& value) const           { value = asShort(); }
  void getValue (uShort& value) const          { value = asuShort(); }
  void getValue (Int& value) const             { value = asInt(); }
  void getValue (uInt& value) const            { value = asuInt(); }
  void getValue (Float& value) const           { value = asFloat(); }
  void getValue (Double& value) const          { value = asDouble(); }
  void getValue (Complex& value) const         { value = asComplex(); }
  void getValue (DComplex& value) const        { value = asDComplex(); }
  void getValue (String& value) const          { value = asString(); }
  void getValue (Array<Bool>& value) const
    { value.reference(asArrayBool()); }
  void getValue (Array<uChar>& value) const
    { value.reference(asArrayuChar()); }
  void getValue (Array<Short>& value) const
    { value.reference(asArrayShort()); }
  void getValue (Array<uShort>& value) const
    { value.reference(asArrayuShort()); }
  void getValue (Array<Int>& value) const
    { value.reference(asArrayInt()); }
  void getValue (Array<uInt>& value) const
    { value.reference(asArrayuInt()); }
  void getValue (Array<Float>& value) const
    { value.reference(asArrayFloat()); }
  void getValue (Array<Double>& value) const
    { value.reference(asArrayDouble()); }
  void getValue (Array<Complex>& value) const
    { value.reference(asArrayComplex()); }
  void getValue (Array<DComplex>& value) const
    { value.reference(asArrayDComplex()); }
  void getValue (Array<String>& value) const
    { value.reference(asArrayString()); }
  // </group>

  // Put the value as a field in a record.
  void toRecord (Record&, const RecordFieldId&) const;

  // Construct the object from the value in a record.
  static ValueHolder fromRecord (const Record&, const RecordFieldId&);

  // Write the ValueHolder to an output stream.
  // Arrays are written as normal arrays using ArrayIO.h. 
  friend std::ostream& operator<< (std::ostream& os, const ValueHolder& vh)
    { return vh.itsRep->write (os); }

private:
  ValueHolderRep* itsRep;
};


inline DataType ValueHolder::dataType() const
  { return itsRep->dataType(); }
inline void ValueHolder::toRecord (Record& rec, const RecordFieldId& id) const
  { return itsRep->toRecord (rec, id); }
inline ValueHolder ValueHolder::fromRecord (const Record& rec,
					    const RecordFieldId& id)
  { return ValueHolder (ValueHolderRep::fromRecord (rec, id)); }
inline Bool ValueHolder::asBool() const
  { return itsRep->asBool(); }
inline uChar ValueHolder::asuChar() const
  { return itsRep->asuChar(); }
inline Short ValueHolder::asShort() const
  { return itsRep->asShort(); }
inline uShort ValueHolder::asuShort() const
  { return itsRep->asuShort(); }
inline Int ValueHolder::asInt() const
  { return itsRep->asInt(); }
inline uInt ValueHolder::asuInt() const
  { return itsRep->asuInt(); }
inline Float ValueHolder::asFloat() const
  { return itsRep->asFloat(); }
inline Double ValueHolder::asDouble() const
  { return itsRep->asDouble(); }
inline Complex ValueHolder::asComplex() const
  { return itsRep->asComplex(); }
inline DComplex ValueHolder::asDComplex() const
  { return itsRep->asDComplex(); }
inline const String& ValueHolder::asString() const
  { return itsRep->asString(); }
inline const Array<Bool> ValueHolder::asArrayBool() const
  { return itsRep->asArrayBool(); }
inline const Array<uChar> ValueHolder::asArrayuChar() const
  { return itsRep->asArrayuChar(); }
inline const Array<Short> ValueHolder::asArrayShort() const
  { return itsRep->asArrayShort(); }
inline const Array<uShort> ValueHolder::asArrayuShort() const
  { return itsRep->asArrayuShort(); }
inline const Array<Int> ValueHolder::asArrayInt() const
  { return itsRep->asArrayInt(); }
inline const Array<uInt> ValueHolder::asArrayuInt() const
  { return itsRep->asArrayuInt(); }
inline const Array<Float> ValueHolder::asArrayFloat() const
  { return itsRep->asArrayFloat(); }
inline const Array<Double> ValueHolder::asArrayDouble() const
  { return itsRep->asArrayDouble(); }
inline const Array<Complex> ValueHolder::asArrayComplex() const
  { return itsRep->asArrayComplex(); }
inline const Array<DComplex> ValueHolder::asArrayDComplex() const
  { return itsRep->asArrayDComplex(); }
inline const Array<String> ValueHolder::asArrayString() const
  { return itsRep->asArrayString(); }
inline const Record& ValueHolder::asRecord() const
  { return itsRep->asRecord(); }


} //# NAMESPACE CASA - END

#endif
