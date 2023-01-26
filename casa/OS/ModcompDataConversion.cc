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

#include <casacore/casa/OS/ModcompDataConversion.h>
#include <casacore/casa/OS/ModcompConversion.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

ModcompDataConversion::~ModcompDataConversion(){
}

size_t ModcompDataConversion::toLocal (char& to, const void* from) const {
  return ModcompConversion::toLocal (to, from);
}

size_t ModcompDataConversion::toLocal (unsigned char& to, const void* from) const {
  return ModcompConversion::toLocal (to, from);
}

size_t ModcompDataConversion::toLocal (int16_t& to, const void* from) const {
  return ModcompConversion::toLocal (to, from);
}

size_t ModcompDataConversion::toLocal (uint16_t& to, const void* from) const {
  return ModcompConversion::toLocal (to, from);
}

size_t ModcompDataConversion::toLocal (int32_t& to, const void* from) const {
  return ModcompConversion::toLocal (to, from);
}

size_t ModcompDataConversion::toLocal (uint32_t& to, const void* from) const {
  return ModcompConversion::toLocal (to, from);
}

size_t ModcompDataConversion::toLocal (int64_t& to, const void* from) const {
  return ModcompConversion::toLocal (to, from);
}

size_t ModcompDataConversion::toLocal (uint64_t& to, const void* from) const {
  return ModcompConversion::toLocal (to, from);
}

size_t ModcompDataConversion::toLocal (float& to, const void* from) const {
  return ModcompConversion::toLocal (to, from);
}

size_t ModcompDataConversion::toLocal (double& to, const void* from) const {
  return ModcompConversion::toLocal (to, from);
}

size_t ModcompDataConversion::toLocal (char* to, const void* from,
                                       size_t nr) const {
  return ModcompConversion::toLocal (to, from, nr);
}

size_t ModcompDataConversion::toLocal (unsigned char* to, const void* from,
                                       size_t nr) const {
  return ModcompConversion::toLocal (to, from, nr);
}

size_t ModcompDataConversion::toLocal (int16_t* to, const void* from,
                                       size_t nr) const {
  return ModcompConversion::toLocal (to, from, nr);
}

size_t ModcompDataConversion::toLocal (uint16_t* to, const void* from,
                                       size_t nr) const {
  return ModcompConversion::toLocal (to, from, nr);
}

size_t ModcompDataConversion::toLocal (int32_t* to, const void* from,
                                       size_t nr) const {
  return ModcompConversion::toLocal (to, from, nr);
}

size_t ModcompDataConversion::toLocal (uint32_t* to, const void* from,
                                       size_t nr) const {
  return ModcompConversion::toLocal (to, from, nr);
}

size_t ModcompDataConversion::toLocal (int64_t* to, const void* from,
                                       size_t nr) const {
  return ModcompConversion::toLocal (to, from, nr);
}

size_t ModcompDataConversion::toLocal (uint64_t* to, const void* from,
                                       size_t nr) const {
  return ModcompConversion::toLocal (to, from, nr);
}

size_t ModcompDataConversion::toLocal (float* to, const void* from,
                                       size_t nr) const {
  return ModcompConversion::toLocal (to, from, nr);
}

size_t ModcompDataConversion::toLocal (double* to, const void* from, 
                                       size_t nr) const {
  return ModcompConversion::toLocal (to, from, nr);
}

size_t ModcompDataConversion::fromLocal (void* to, char from) const {
  return ModcompConversion::fromLocal (to, from);
}

size_t ModcompDataConversion::fromLocal (void* to, unsigned char from) const {
  return ModcompConversion::fromLocal (to, from);
}

size_t ModcompDataConversion::fromLocal (void* to, int16_t from) const {
  return ModcompConversion::fromLocal (to, from);
}

size_t ModcompDataConversion::fromLocal (void* to, uint16_t from) const {
  return ModcompConversion::fromLocal (to, from);
}

size_t ModcompDataConversion::fromLocal (void* to, int32_t from) const {
  return ModcompConversion::fromLocal (to, from);
}

size_t ModcompDataConversion::fromLocal (void* to, uint32_t from) const {
  return ModcompConversion::fromLocal (to, from);
}

size_t ModcompDataConversion::fromLocal (void* to, int64_t from) const {
  return ModcompConversion::fromLocal (to, from);
}

size_t ModcompDataConversion::fromLocal (void* to, uint64_t from) const {
  return ModcompConversion::fromLocal (to, from);
}

size_t ModcompDataConversion::fromLocal (void* to, float from) const {
    return ModcompConversion::fromLocal (to, from);
}

size_t ModcompDataConversion::fromLocal (void* to, double from) const {
  return ModcompConversion::fromLocal (to, from);
}

size_t ModcompDataConversion::fromLocal (void* to, const char* from,
                                         size_t nr) const {
  return ModcompConversion::fromLocal (to, from, nr);
}

size_t ModcompDataConversion::fromLocal (void* to, const unsigned char* from,
                                         size_t nr) const {
  return ModcompConversion::fromLocal (to, from, nr);
}

size_t ModcompDataConversion::fromLocal (void* to, const int16_t* from,
                                         size_t nr) const {
  return ModcompConversion::fromLocal (to, from, nr);
}

size_t ModcompDataConversion::fromLocal (void* to, const uint16_t* from,
                                         size_t nr) const {
  return ModcompConversion::fromLocal (to, from, nr);
}

size_t ModcompDataConversion::fromLocal (void* to, const int32_t* from,
                                         size_t nr) const {
  return ModcompConversion::fromLocal (to, from, nr);
}

size_t ModcompDataConversion::fromLocal (void* to, const uint32_t* from,
                                         size_t nr) const {
  return ModcompConversion::fromLocal (to, from, nr);
}

size_t ModcompDataConversion::fromLocal (void* to, const int64_t* from,
                                         size_t nr) const {
  return ModcompConversion::fromLocal (to, from, nr);
}

size_t ModcompDataConversion::fromLocal (void* to, const uint64_t* from,
                                         size_t nr) const {
  return ModcompConversion::fromLocal (to, from, nr);
}

size_t ModcompDataConversion::fromLocal (void* to, const float* from,
                                         size_t nr) const {
  return ModcompConversion::fromLocal (to, from, nr);
}

size_t ModcompDataConversion::fromLocal (void* to, const double* from,
                                         size_t nr) const {
  return ModcompConversion::fromLocal (to, from, nr);
}

bool ModcompDataConversion::canCopy (const char*) const {
  return (CONVERT_MODCOMP_CHAR == 0);
}

bool ModcompDataConversion::canCopy (const unsigned char*) const {
  return (CONVERT_MODCOMP_UCHAR == 0);
}

bool ModcompDataConversion::canCopy (const int16_t*) const {
  return (CONVERT_MODCOMP_SHORT == 0);
}

bool ModcompDataConversion::canCopy (const uint16_t*) const {
  return (CONVERT_MODCOMP_USHORT == 0);
}

bool ModcompDataConversion::canCopy (const int32_t*) const {
  return (CONVERT_MODCOMP_INT == 0);
}

bool ModcompDataConversion::canCopy (const uint32_t*) const {
  return (CONVERT_MODCOMP_UINT == 0);
}

bool ModcompDataConversion::canCopy (const int64_t*) const {
  return (CONVERT_MODCOMP_INT64 == 0);
}

bool ModcompDataConversion::canCopy (const uint64_t*) const {
  return (CONVERT_MODCOMP_UINT64 == 0);
}

bool ModcompDataConversion::canCopy (const float*) const {
  return (CONVERT_MODCOMP_FLOAT == 0);
}

bool ModcompDataConversion::canCopy (const double*) const {
  return (CONVERT_MODCOMP_DOUBLE == 0);
}

uint32_t ModcompDataConversion::externalSize (const char*) const {
  return SIZE_MODCOMP_CHAR;
}

uint32_t ModcompDataConversion::externalSize (const unsigned char*) const {
  return SIZE_MODCOMP_UCHAR;
}

uint32_t ModcompDataConversion::externalSize (const int16_t*) const {
  return SIZE_MODCOMP_SHORT;
}

uint32_t ModcompDataConversion::externalSize (const uint16_t*) const {
  return SIZE_MODCOMP_USHORT;
}

uint32_t ModcompDataConversion::externalSize (const int32_t*) const {
  return SIZE_MODCOMP_INT;
}

uint32_t ModcompDataConversion::externalSize (const uint32_t*) const {
  return SIZE_MODCOMP_UINT;
}

uint32_t ModcompDataConversion::externalSize (const int64_t*) const {
  return SIZE_MODCOMP_INT64;
}

uint32_t ModcompDataConversion::externalSize (const uint64_t*) const {
  return SIZE_MODCOMP_UINT64;
}

uint32_t ModcompDataConversion::externalSize (const float*) const {
  return SIZE_MODCOMP_FLOAT;
}

uint32_t ModcompDataConversion::externalSize (const double*) const {
  return SIZE_MODCOMP_DOUBLE;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 ModcompDataConversion"
// End: 

} //# NAMESPACE CASACORE - END

