//# ValueHolder.cc: A holder object for the standard AIPS++ data
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


#include <casa/Containers/ValueHolder.h>
#include <casa/Containers/Record.h>
#include <casa/Arrays/Array.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Utilities/Assert.h>
#include <casa/Exceptions/Error.h>

namespace casa { //# NAMESPACE CASA - BEGIN


ValueHolder::ValueHolder (Bool value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (uChar value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (Short value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (uShort value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (Int value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (uInt value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (Float value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (Double value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (const Complex& value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (const DComplex& value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (const Char* value)
  : itsRep (new ValueHolderRep(String(value)))
{
  itsRep->link();
}

ValueHolder::ValueHolder (const String& value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (const Array<Bool>& value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (const Array<uChar>& value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (const Array<Short>& value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (const Array<uShort>& value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (const Array<Int>& value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (const Array<uInt>& value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (const Array<Float>& value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (const Array<Double>& value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (const Array<Complex>& value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (const Array<DComplex>& value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (const Array<String>& value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (const Record& value)
  : itsRep (new ValueHolderRep(value))
{
  itsRep->link();
}

ValueHolder::ValueHolder (uInt ndim, Bool dummy)
  : itsRep (new ValueHolderRep(ndim, dummy))
{
  itsRep->link();
}


ValueHolder::ValueHolder (const ValueHolder& that)
  : itsRep (that.itsRep)
{
  if (itsRep) itsRep->link();
}

ValueHolder& ValueHolder::operator= (const ValueHolder& that)
{
  if (this != &that) {
    ValueHolderRep::unlink (itsRep);
    itsRep = that.itsRep;
    if (itsRep) itsRep->link();
  }
  return *this;
}


} //# NAMESPACE CASA - END
