//# ValueHolder.h: A holder object for the standard Casacore data types
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


#ifndef CASA_VALUEHOLDER_H
#define CASA_VALUEHOLDER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/ValueHolderRep.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Utilities/CountedPtr.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


// <summary>
// A holder for a value of any basic Casacore data type.
// </summary>

// <use visibility=export>
// <reviewed reviewer="" date="" tests="tValueHolder">
// </reviewed>

// <synopsis>
// Class ValueHolder is meant to be used for holding a single Casacore value.
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
    {}

  // Create the object for the given value.
  // <group>
  explicit ValueHolder (bool value);
  explicit ValueHolder (unsigned char value);
  explicit ValueHolder (int16_t value);
  explicit ValueHolder (uint16_t value);
  explicit ValueHolder (int32_t value);
  explicit ValueHolder (uint32_t value);
  explicit ValueHolder (int64_t value);
  explicit ValueHolder (float value);
  explicit ValueHolder (double value);
  explicit ValueHolder (const Complex& value);
  explicit ValueHolder (const DComplex& value);
  explicit ValueHolder (const char* value);
  explicit ValueHolder (const String& value);
  explicit ValueHolder (const Array<bool>& value);
  explicit ValueHolder (const Array<unsigned char>& value);
  explicit ValueHolder (const Array<int16_t>& value);
  explicit ValueHolder (const Array<uint16_t>& value);
  explicit ValueHolder (const Array<int32_t>& value);
  explicit ValueHolder (const Array<uint32_t>& value);
  explicit ValueHolder (const Array<int64_t>& value);
  explicit ValueHolder (const Array<float>& value);
  explicit ValueHolder (const Array<double>& value);
  explicit ValueHolder (const Array<Complex>& value);
  explicit ValueHolder (const Array<DComplex>& value);
  explicit ValueHolder (const Array<String>& value);
  explicit ValueHolder (const Record& value);
  // </group>

  // Create an empty N-dim array (gets type TpOther).
  ValueHolder (uint32_t ndim, bool dummy);

  // Create a ValueHolder from a ValueHolderRep.
  // It takes over the pointer and deletes it in the destructor.
  explicit ValueHolder (ValueHolderRep* rep)
    : itsRep (rep)
    {}

  // Copy constructor (reference semantics).
  ValueHolder (const ValueHolder&);

  // Destructor.
  ~ValueHolder()
  {}

  // Assignment (reference semantics).
  ValueHolder& operator= (const ValueHolder&);

  // Is this a null object?
  bool isNull() const
    { return itsRep.null(); }

  // Get the data type (as defined in DataType.h).
  // Note that TpOther is returned for an empty untyped array.
  DataType dataType() const;
    
  // Get the value.
  // If possible, it converts the data as needed.
  // <group>
  bool                  asBool    () const;
  unsigned char                 asuChar   () const;
  int16_t                 asShort   () const;
  uint16_t                asuShort  () const;
  int32_t                   asInt     () const;
  uint32_t                  asuInt    () const;
  int64_t                 asInt64   () const;
  float                 asFloat   () const;
  double                asDouble  () const;
  Complex               asComplex () const;
  DComplex              asDComplex() const;
  const String&         asString  () const;
  const Array<bool>     asArrayBool    () const;
  const Array<unsigned char>    asArrayuChar   () const;
  const Array<int16_t>    asArrayShort   () const;
  const Array<uint16_t>   asArrayuShort  () const;
  const Array<int32_t>      asArrayInt     () const;
  const Array<uint32_t>     asArrayuInt    () const;
  const Array<int64_t>    asArrayInt64   () const;
  const Array<float>    asArrayFloat   () const;
  const Array<double>   asArrayDouble  () const;
  const Array<Complex>  asArrayComplex () const; 
  const Array<DComplex> asArrayDComplex() const;
  const Array<String>   asArrayString  () const;
  const Record&         asRecord       () const;
  // </group>

  // Get the data in a way useful for templates.
  // If possible, it converts the the data as needed.
  // <group>
  void getValue (bool& value) const            { value = asBool(); }
  void getValue (unsigned char& value) const           { value = asuChar(); }
  void getValue (int16_t& value) const           { value = asShort(); }
  void getValue (uint16_t& value) const          { value = asuShort(); }
  void getValue (int32_t& value) const             { value = asInt(); }
  void getValue (uint32_t& value) const            { value = asuInt(); }
  void getValue (int64_t& value) const           { value = asInt64(); }
  void getValue (float& value) const           { value = asFloat(); }
  void getValue (double& value) const          { value = asDouble(); }
  void getValue (Complex& value) const         { value = asComplex(); }
  void getValue (DComplex& value) const        { value = asDComplex(); }
  void getValue (String& value) const          { value = asString(); }
  void getValue (Array<bool>& value) const
    { value.reference(asArrayBool()); }
  void getValue (Array<unsigned char>& value) const
    { value.reference(asArrayuChar()); }
  void getValue (Array<int16_t>& value) const
    { value.reference(asArrayShort()); }
  void getValue (Array<uint16_t>& value) const
    { value.reference(asArrayuShort()); }
  void getValue (Array<int32_t>& value) const
    { value.reference(asArrayInt()); }
  void getValue (Array<uint32_t>& value) const
    { value.reference(asArrayuInt()); }
  void getValue (Array<int64_t>& value) const
    { value.reference(asArrayInt64()); }
  void getValue (Array<float>& value) const
    { value.reference(asArrayFloat()); }
  void getValue (Array<double>& value) const
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

  // Compare two ValueHolder objects.
  // They must have the same data type.
  bool operator< (const ValueHolder& right) const
    { return itsRep->operator< (*right.itsRep); }

  // Write the ValueHolder to an output stream.
  // Arrays are written as normal arrays using ArrayIO.h. 
  friend std::ostream& operator<< (std::ostream& os, const ValueHolder& vh)
    { return vh.itsRep->write (os); }

private:

  CountedPtr<ValueHolderRep> itsRep;
};


inline DataType ValueHolder::dataType() const
  { return itsRep->dataType(); }
inline void ValueHolder::toRecord (Record& rec, const RecordFieldId& id) const
  { return itsRep->toRecord (rec, id); }
inline ValueHolder ValueHolder::fromRecord (const Record& rec,
					    const RecordFieldId& id)
  { return ValueHolder (ValueHolderRep::fromRecord (rec, id)); }
inline bool ValueHolder::asBool() const
  { return itsRep->asBool(); }
inline unsigned char ValueHolder::asuChar() const
  { return itsRep->asuChar(); }
inline int16_t ValueHolder::asShort() const
  { return itsRep->asShort(); }
inline uint16_t ValueHolder::asuShort() const
  { return itsRep->asuShort(); }
inline int32_t ValueHolder::asInt() const
  { return itsRep->asInt(); }
inline uint32_t ValueHolder::asuInt() const
  { return itsRep->asuInt(); }
inline int64_t ValueHolder::asInt64() const
  { return itsRep->asInt64(); }
inline float ValueHolder::asFloat() const
  { return itsRep->asFloat(); }
inline double ValueHolder::asDouble() const
  { return itsRep->asDouble(); }
inline Complex ValueHolder::asComplex() const
  { return itsRep->asComplex(); }
inline DComplex ValueHolder::asDComplex() const
  { return itsRep->asDComplex(); }
inline const String& ValueHolder::asString() const
  { return itsRep->asString(); }
inline const Array<bool> ValueHolder::asArrayBool() const
  { return itsRep->asArrayBool(); }
inline const Array<unsigned char> ValueHolder::asArrayuChar() const
  { return itsRep->asArrayuChar(); }
inline const Array<int16_t> ValueHolder::asArrayShort() const
  { return itsRep->asArrayShort(); }
inline const Array<uint16_t> ValueHolder::asArrayuShort() const
  { return itsRep->asArrayuShort(); }
inline const Array<int32_t> ValueHolder::asArrayInt() const
  { return itsRep->asArrayInt(); }
inline const Array<uint32_t> ValueHolder::asArrayuInt() const
  { return itsRep->asArrayuInt(); }
inline const Array<int64_t> ValueHolder::asArrayInt64() const
  { return itsRep->asArrayInt64(); }
inline const Array<float> ValueHolder::asArrayFloat() const
  { return itsRep->asArrayFloat(); }
inline const Array<double> ValueHolder::asArrayDouble() const
  { return itsRep->asArrayDouble(); }
inline const Array<Complex> ValueHolder::asArrayComplex() const
  { return itsRep->asArrayComplex(); }
inline const Array<DComplex> ValueHolder::asArrayDComplex() const
  { return itsRep->asArrayDComplex(); }
inline const Array<String> ValueHolder::asArrayString() const
  { return itsRep->asArrayString(); }
inline const Record& ValueHolder::asRecord() const
  { return itsRep->asRecord(); }


} //# NAMESPACE CASACORE - END

#endif
