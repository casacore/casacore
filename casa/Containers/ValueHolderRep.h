//# ValueHolderRep.h: A holder object for the standard CASACORE data
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


#ifndef CASA_VALUEHOLDERREP_H
#define CASA_VALUEHOLDERREP_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/casa/Utilities/DataType.h>
#include <iosfwd>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class AipsIO;
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
  explicit ValueHolderRep (bool value);
  explicit ValueHolderRep (unsigned char value);
  explicit ValueHolderRep (int16_t value);
  explicit ValueHolderRep (uint16_t value);
  explicit ValueHolderRep (int32_t value);
  explicit ValueHolderRep (uint32_t value);
  explicit ValueHolderRep (int64_t value);
  explicit ValueHolderRep (float value);
  explicit ValueHolderRep (double value);
  explicit ValueHolderRep (const Complex& value);
  explicit ValueHolderRep (const DComplex& value);
  explicit ValueHolderRep (const char* value);
  explicit ValueHolderRep (const String& value);
  explicit ValueHolderRep (const Array<bool>& value);
  explicit ValueHolderRep (const Array<unsigned char>& value);
  explicit ValueHolderRep (const Array<int16_t>& value);
  explicit ValueHolderRep (const Array<uint16_t>& value);
  explicit ValueHolderRep (const Array<int32_t>& value);
  explicit ValueHolderRep (const Array<uint32_t>& value);
  explicit ValueHolderRep (const Array<int64_t>& value);
  explicit ValueHolderRep (const Array<float>& value);
  explicit ValueHolderRep (const Array<double>& value);
  explicit ValueHolderRep (const Array<Complex>& value);
  explicit ValueHolderRep (const Array<DComplex>& value);
  explicit ValueHolderRep (const Array<String>& value);
  explicit ValueHolderRep (const Record& value);
  // </group>

  // Create an empty N-dim array.
  ValueHolderRep (uint32_t ndim, bool dummy);

  // Destructor.
  ~ValueHolderRep();

  // Get the data type (as defined in DataType.h).
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

  // Put the value as a field in a record.
  void toRecord (Record&, const RecordFieldId&) const;

  // Construct the object from the value in a record.
  static ValueHolderRep* fromRecord (const Record& rec, const RecordFieldId&);

  // Write the ValueHolderRep to an output stream.
  // Arrays are written as normal arrays using ArrayIO.h. 
  std::ostream& write (std::ostream& os) const;

  // Compare two ValueHolder objects.
  // They must have the same data type.
  bool operator< (const ValueHolderRep& right) const;
  /*
  bool operator== (const ValueHolderRep& right) const;
  bool near (const ValueHolderRep& right, tolerance=1e-5) const;
  bool nearAbs (const ValueHolderRep& right, double tolerance=1e-5) const;
  */

private:
  // Forbid copy ctor and assignment.
  //# There is no fundamental reason to forbid them, but it saves
  //# implementation work as long as they are not needed.
  // <group>
  ValueHolderRep (const ValueHolderRep&);
  ValueHolderRep& operator= (const ValueHolderRep&);
  // </group>


  uint32_t     itsNdim;
  DataType itsType;
  union {
    bool   itsBool;
    int64_t  itsInt64;
    float  itsFloat;
    double itsDouble;
    void*  itsPtr;
  };
};


inline DataType ValueHolderRep::dataType() const
{
  return itsType;
}


} //# NAMESPACE CASACORE - END

#endif
