//# ValueHolderRep.h: A holder object for the standard AIPS++ data
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


#ifndef CASA_VALUEHOLDERREP_H
#define CASA_VALUEHOLDERREP_H

//# Includes
#include <casa/aips.h>
#include <casa/Utilities/DataType.h>
#include <iosfwd>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations
class AipsIO;
template<class T> class Array;
class Record;
class RecordFieldId;


// <summary>
// A holder for a value of any basic type.
// </summary>

// <use visibility=local>
// <reviewed reviewer="" date="" tests="tValueHolder">
// </reviewed>

// <synopsis>
// Class ValueHolderRep is the letter class for the envelope class ValueHolder.
// See <linkto class=ValueHolder>that class</linkto> for more information.
// </synopsis>

// <motivation>
// Copying ValueHolders should be as cheap as possible, so a counted
// referenced letter class is used.
// </motivation>

class ValueHolderRep
{
public:
  // Create the object for the given value.
  // <group>
  explicit ValueHolderRep (Bool value);
  explicit ValueHolderRep (uChar value);
  explicit ValueHolderRep (Short value);
  explicit ValueHolderRep (uShort value);
  explicit ValueHolderRep (Int value);
  explicit ValueHolderRep (uInt value);
  explicit ValueHolderRep (Float value);
  explicit ValueHolderRep (Double value);
  explicit ValueHolderRep (const Complex& value);
  explicit ValueHolderRep (const DComplex& value);
  explicit ValueHolderRep (const Char* value);
  explicit ValueHolderRep (const String& value);
  explicit ValueHolderRep (const Array<Bool>& value);
  explicit ValueHolderRep (const Array<uChar>& value);
  explicit ValueHolderRep (const Array<Short>& value);
  explicit ValueHolderRep (const Array<uShort>& value);
  explicit ValueHolderRep (const Array<Int>& value);
  explicit ValueHolderRep (const Array<uInt>& value);
  explicit ValueHolderRep (const Array<Float>& value);
  explicit ValueHolderRep (const Array<Double>& value);
  explicit ValueHolderRep (const Array<Complex>& value);
  explicit ValueHolderRep (const Array<DComplex>& value);
  explicit ValueHolderRep (const Array<String>& value);
  explicit ValueHolderRep (const Record& value);
  // </group>

  // Create an empty N-dim array.
  ValueHolderRep (uInt ndim, Bool dummy);

  // Destructor.
  ~ValueHolderRep();

  void link()
    { itsCount++; }

  static void unlink (ValueHolderRep* rep)
    { if (rep != 0 && --rep->itsCount == 0) delete rep; }

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

  // Put the value as a field in a record.
  void toRecord (Record&, const RecordFieldId&) const;

  // Construct the object from the value in a record.
  static ValueHolderRep* fromRecord (const Record& rec, const RecordFieldId&);

  // Write the ValueHolderRep to an output stream.
  // Arrays are written as normal arrays using ArrayIO.h. 
  std::ostream& write (std::ostream& os) const;

private:
  // Forbid copy ctor and assignment.
  //# There is no fundamental reason to forbid them, but it saves
  //# implementation work as long as they are not needed.
  // <group>
  ValueHolderRep (const ValueHolderRep&);
  ValueHolderRep& operator= (const ValueHolderRep&);
  // </group>


  Int      itsCount;
  uInt     itsNdim;
  DataType itsType;
  union {
    Bool   itsBool;
    uChar  itsUChar;
    Short  itsShort;
    Int    itsInt;
    Float  itsFloat;
    Double itsDouble;
    void*  itsPtr;
  };
};


inline DataType ValueHolderRep::dataType() const
{
  return itsType;
}


} //# NAMESPACE CASA - END

#endif
