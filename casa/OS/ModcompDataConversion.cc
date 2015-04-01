//# ModcompDataConversion.cc: A DataConversion class to convert Modcomp format.
//# Copyright (C) 1999,2001
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
//# $Id$

#include <casacore/casa/OS/ModcompDataConversion.h>
#include <casacore/casa/OS/ModcompConversion.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

ModcompDataConversion::~ModcompDataConversion(){
}

size_t ModcompDataConversion::toLocal (Char& to, const void* from) const {
  return ModcompConversion::toLocal (to, from);
}

size_t ModcompDataConversion::toLocal (uChar& to, const void* from) const {
  return ModcompConversion::toLocal (to, from);
}

size_t ModcompDataConversion::toLocal (Short& to, const void* from) const {
  return ModcompConversion::toLocal (to, from);
}

size_t ModcompDataConversion::toLocal (uShort& to, const void* from) const {
  return ModcompConversion::toLocal (to, from);
}

size_t ModcompDataConversion::toLocal (Int& to, const void* from) const {
  return ModcompConversion::toLocal (to, from);
}

size_t ModcompDataConversion::toLocal (uInt& to, const void* from) const {
  return ModcompConversion::toLocal (to, from);
}

size_t ModcompDataConversion::toLocal (Int64& to, const void* from) const {
  return ModcompConversion::toLocal (to, from);
}

size_t ModcompDataConversion::toLocal (uInt64& to, const void* from) const {
  return ModcompConversion::toLocal (to, from);
}

size_t ModcompDataConversion::toLocal (Float& to, const void* from) const {
  return ModcompConversion::toLocal (to, from);
}

size_t ModcompDataConversion::toLocal (Double& to, const void* from) const {
  return ModcompConversion::toLocal (to, from);
}

size_t ModcompDataConversion::toLocal (Char* to, const void* from,
                                       size_t nr) const {
  return ModcompConversion::toLocal (to, from, nr);
}

size_t ModcompDataConversion::toLocal (uChar* to, const void* from,
                                       size_t nr) const {
  return ModcompConversion::toLocal (to, from, nr);
}

size_t ModcompDataConversion::toLocal (Short* to, const void* from,
                                       size_t nr) const {
  return ModcompConversion::toLocal (to, from, nr);
}

size_t ModcompDataConversion::toLocal (uShort* to, const void* from,
                                       size_t nr) const {
  return ModcompConversion::toLocal (to, from, nr);
}

size_t ModcompDataConversion::toLocal (Int* to, const void* from,
                                       size_t nr) const {
  return ModcompConversion::toLocal (to, from, nr);
}

size_t ModcompDataConversion::toLocal (uInt* to, const void* from,
                                       size_t nr) const {
  return ModcompConversion::toLocal (to, from, nr);
}

size_t ModcompDataConversion::toLocal (Int64* to, const void* from,
                                       size_t nr) const {
  return ModcompConversion::toLocal (to, from, nr);
}

size_t ModcompDataConversion::toLocal (uInt64* to, const void* from,
                                       size_t nr) const {
  return ModcompConversion::toLocal (to, from, nr);
}

size_t ModcompDataConversion::toLocal (Float* to, const void* from,
                                       size_t nr) const {
  return ModcompConversion::toLocal (to, from, nr);
}

size_t ModcompDataConversion::toLocal (Double* to, const void* from, 
                                       size_t nr) const {
  return ModcompConversion::toLocal (to, from, nr);
}

size_t ModcompDataConversion::fromLocal (void* to, Char from) const {
  return ModcompConversion::fromLocal (to, from);
}

size_t ModcompDataConversion::fromLocal (void* to, uChar from) const {
  return ModcompConversion::fromLocal (to, from);
}

size_t ModcompDataConversion::fromLocal (void* to, Short from) const {
  return ModcompConversion::fromLocal (to, from);
}

size_t ModcompDataConversion::fromLocal (void* to, uShort from) const {
  return ModcompConversion::fromLocal (to, from);
}

size_t ModcompDataConversion::fromLocal (void* to, Int from) const {
  return ModcompConversion::fromLocal (to, from);
}

size_t ModcompDataConversion::fromLocal (void* to, uInt from) const {
  return ModcompConversion::fromLocal (to, from);
}

size_t ModcompDataConversion::fromLocal (void* to, Int64 from) const {
  return ModcompConversion::fromLocal (to, from);
}

size_t ModcompDataConversion::fromLocal (void* to, uInt64 from) const {
  return ModcompConversion::fromLocal (to, from);
}

size_t ModcompDataConversion::fromLocal (void* to, Float from) const {
    return ModcompConversion::fromLocal (to, from);
}

size_t ModcompDataConversion::fromLocal (void* to, Double from) const {
  return ModcompConversion::fromLocal (to, from);
}

size_t ModcompDataConversion::fromLocal (void* to, const Char* from,
                                         size_t nr) const {
  return ModcompConversion::fromLocal (to, from, nr);
}

size_t ModcompDataConversion::fromLocal (void* to, const uChar* from,
                                         size_t nr) const {
  return ModcompConversion::fromLocal (to, from, nr);
}

size_t ModcompDataConversion::fromLocal (void* to, const Short* from,
                                         size_t nr) const {
  return ModcompConversion::fromLocal (to, from, nr);
}

size_t ModcompDataConversion::fromLocal (void* to, const uShort* from,
                                         size_t nr) const {
  return ModcompConversion::fromLocal (to, from, nr);
}

size_t ModcompDataConversion::fromLocal (void* to, const Int* from,
                                         size_t nr) const {
  return ModcompConversion::fromLocal (to, from, nr);
}

size_t ModcompDataConversion::fromLocal (void* to, const uInt* from,
                                         size_t nr) const {
  return ModcompConversion::fromLocal (to, from, nr);
}

size_t ModcompDataConversion::fromLocal (void* to, const Int64* from,
                                         size_t nr) const {
  return ModcompConversion::fromLocal (to, from, nr);
}

size_t ModcompDataConversion::fromLocal (void* to, const uInt64* from,
                                         size_t nr) const {
  return ModcompConversion::fromLocal (to, from, nr);
}

size_t ModcompDataConversion::fromLocal (void* to, const Float* from,
                                         size_t nr) const {
  return ModcompConversion::fromLocal (to, from, nr);
}

size_t ModcompDataConversion::fromLocal (void* to, const Double* from,
                                         size_t nr) const {
  return ModcompConversion::fromLocal (to, from, nr);
}

Bool ModcompDataConversion::canCopy (const Char*) const {
  return (CONVERT_MODCOMP_CHAR == 0);
}

Bool ModcompDataConversion::canCopy (const uChar*) const {
  return (CONVERT_MODCOMP_UCHAR == 0);
}

Bool ModcompDataConversion::canCopy (const Short*) const {
  return (CONVERT_MODCOMP_SHORT == 0);
}

Bool ModcompDataConversion::canCopy (const uShort*) const {
  return (CONVERT_MODCOMP_USHORT == 0);
}

Bool ModcompDataConversion::canCopy (const Int*) const {
  return (CONVERT_MODCOMP_INT == 0);
}

Bool ModcompDataConversion::canCopy (const uInt*) const {
  return (CONVERT_MODCOMP_UINT == 0);
}

Bool ModcompDataConversion::canCopy (const Int64*) const {
  return (CONVERT_MODCOMP_INT64 == 0);
}

Bool ModcompDataConversion::canCopy (const uInt64*) const {
  return (CONVERT_MODCOMP_UINT64 == 0);
}

Bool ModcompDataConversion::canCopy (const Float*) const {
  return (CONVERT_MODCOMP_FLOAT == 0);
}

Bool ModcompDataConversion::canCopy (const Double*) const {
  return (CONVERT_MODCOMP_DOUBLE == 0);
}

uInt ModcompDataConversion::externalSize (const Char*) const {
  return SIZE_MODCOMP_CHAR;
}

uInt ModcompDataConversion::externalSize (const uChar*) const {
  return SIZE_MODCOMP_UCHAR;
}

uInt ModcompDataConversion::externalSize (const Short*) const {
  return SIZE_MODCOMP_SHORT;
}

uInt ModcompDataConversion::externalSize (const uShort*) const {
  return SIZE_MODCOMP_USHORT;
}

uInt ModcompDataConversion::externalSize (const Int*) const {
  return SIZE_MODCOMP_INT;
}

uInt ModcompDataConversion::externalSize (const uInt*) const {
  return SIZE_MODCOMP_UINT;
}

uInt ModcompDataConversion::externalSize (const Int64*) const {
  return SIZE_MODCOMP_INT64;
}

uInt ModcompDataConversion::externalSize (const uInt64*) const {
  return SIZE_MODCOMP_UINT64;
}

uInt ModcompDataConversion::externalSize (const Float*) const {
  return SIZE_MODCOMP_FLOAT;
}

uInt ModcompDataConversion::externalSize (const Double*) const {
  return SIZE_MODCOMP_DOUBLE;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 ModcompDataConversion"
// End: 

} //# NAMESPACE CASACORE - END

