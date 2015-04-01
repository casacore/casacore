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
//#
//#
//# $Id$


#include <casacore/casa/Containers/ValueHolder.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


ValueHolder::ValueHolder (Bool value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (uChar value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (Short value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (uShort value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (Int value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (uInt value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (Int64 value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (Float value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (Double value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Complex& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const DComplex& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Char* value)
  : itsRep (new ValueHolderRep(String(value)))
{}
ValueHolder::ValueHolder (const String& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Array<Bool>& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Array<uChar>& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Array<Short>& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Array<uShort>& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Array<Int>& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Array<uInt>& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Array<Int64>& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Array<Float>& value)
  : itsRep (new ValueHolderRep(value))
{}
ValueHolder::ValueHolder (const Array<Double>& value)
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
ValueHolder::ValueHolder (uInt ndim, Bool dummy)
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
