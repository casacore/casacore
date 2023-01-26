//# ValueHolder.cc: A holder object for the standard Casacore data
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


#include <casacore/casa/Containers/ValueHolder.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


ValueHolder::ValueHolder (bool value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (unsigned char value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (int16_t value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (uint16_t value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (int32_t value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (uint32_t value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (int64_t value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (float value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (double value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Complex& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const DComplex& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const char* value)
  : itsRep (new ValueHolderRep(String(value)))
{}
ValueHolder::ValueHolder (const String& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Array<bool>& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Array<unsigned char>& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Array<int16_t>& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Array<uint16_t>& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Array<int32_t>& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Array<uint32_t>& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Array<int64_t>& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Array<float>& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Array<double>& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Array<Complex>& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Array<DComplex>& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Array<String>& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Record& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (uint32_t ndim, bool dummy)
  : itsRep (new ValueHolderRep(ndim, dummy))
{}

ValueHolder::ValueHolder (const ValueHolder& that)
  : itsRep (that.itsRep)
{}

ValueHolder& ValueHolder::operator= (const ValueHolder& that)
{
  if (this != &that) {
    itsRep = that.itsRep;
  }
  return *this;
}


} //# NAMESPACE CASACORE - END
