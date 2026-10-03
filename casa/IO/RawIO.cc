// # RawIO.cc: Class for IO in local format
// # Copyright (C) 1996,1999,2001
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This library is free software; you can redistribute it and/or modify it
// # under the terms of the GNU Library General Public License as published by
// # the Free Software Foundation; either version 2 of the License, or (at your
// # option) any later version.
// #
// # This library is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
// # License for more details.
// #
// # You should have received a copy of the GNU Library General Public License
// # along with this library; if not, write to the Free Software Foundation,
// # Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

#include <casacore/casa/IO/RawIO.h>
#include <casacore/casa/IO/ByteIO.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

RawIO::RawIO(const std::shared_ptr<ByteIO>& byteIO) : TypeIO(byteIO) {}

RawIO::RawIO(const RawIO& that) : TypeIO(that) {}

RawIO& RawIO::operator=(const RawIO& that) {
  if (this != &that) {
    TypeIO::operator=(that);
  }
  return *this;
}

RawIO::~RawIO() {}

size_t RawIO::write(size_t nvalues, const bool* value) { return TypeIO::write(nvalues, value); }

size_t RawIO::write(size_t nvalues, const char* value) {
  itsByteIO->write(nvalues * sizeof(char), (void*)value);
  return nvalues * sizeof(char);
}

size_t RawIO::write(size_t nvalues, const unsigned char* value) {
  itsByteIO->write(nvalues * sizeof(unsigned char), (void*)value);
  return nvalues * sizeof(unsigned char);
}

size_t RawIO::write(size_t nvalues, const short* value) {
  itsByteIO->write(nvalues * sizeof(short), (void*)value);
  return nvalues * sizeof(short);
}

size_t RawIO::write(size_t nvalues, const unsigned short* value) {
  itsByteIO->write(nvalues * sizeof(unsigned short), (void*)value);
  return nvalues * sizeof(unsigned short);
}

size_t RawIO::write(size_t nvalues, const int* value) {
  itsByteIO->write(nvalues * sizeof(int), (void*)value);
  return nvalues * sizeof(int);
}

size_t RawIO::write(size_t nvalues, const unsigned int* value) {
  itsByteIO->write(nvalues * sizeof(unsigned int), (void*)value);
  return nvalues * sizeof(unsigned int);
}

size_t RawIO::write(size_t nvalues, const Int64* value) {
  itsByteIO->write(nvalues * sizeof(Int64), (void*)value);
  return nvalues * sizeof(Int64);
}

size_t RawIO::write(size_t nvalues, const uInt64* value) {
  itsByteIO->write(nvalues * sizeof(uInt64), (void*)value);
  return nvalues * sizeof(uInt64);
}

size_t RawIO::write(size_t nvalues, const float* value) {
  itsByteIO->write(nvalues * sizeof(float), (void*)value);
  return nvalues * sizeof(float);
}

size_t RawIO::write(size_t nvalues, const double* value) {
  itsByteIO->write(nvalues * sizeof(double), (void*)value);
  return nvalues * sizeof(double);
}

size_t RawIO::write(size_t nvalues, const Complex* value) { return TypeIO::write(nvalues, value); }

size_t RawIO::write(size_t nvalues, const DComplex* value) { return TypeIO::write(nvalues, value); }

size_t RawIO::write(size_t nvalues, const String* value) { return TypeIO::write(nvalues, value); }

size_t RawIO::read(size_t nvalues, bool* value) { return TypeIO::read(nvalues, value); }

size_t RawIO::read(size_t nvalues, char* value) {
  itsByteIO->read(nvalues * sizeof(char), value);
  return nvalues * sizeof(char);
}

size_t RawIO::read(size_t nvalues, unsigned char* value) {
  itsByteIO->read(nvalues * sizeof(unsigned char), value);
  return nvalues * sizeof(unsigned char);
}

size_t RawIO::read(size_t nvalues, short* value) {
  itsByteIO->read(nvalues * sizeof(short), value);
  return nvalues * sizeof(short);
}

size_t RawIO::read(size_t nvalues, unsigned short* value) {
  itsByteIO->read(nvalues * sizeof(unsigned short), value);
  return nvalues * sizeof(unsigned short);
}

size_t RawIO::read(size_t nvalues, int* value) {
  itsByteIO->read(nvalues * sizeof(int), value);
  return nvalues * sizeof(int);
}

size_t RawIO::read(size_t nvalues, unsigned int* value) {
  itsByteIO->read(nvalues * sizeof(unsigned int), value);
  return nvalues * sizeof(unsigned int);
}

size_t RawIO::read(size_t nvalues, Int64* value) {
  itsByteIO->read(nvalues * sizeof(Int64), value);
  return nvalues * sizeof(Int64);
}

size_t RawIO::read(size_t nvalues, uInt64* value) {
  itsByteIO->read(nvalues * sizeof(uInt64), value);
  return nvalues * sizeof(uInt64);
}

size_t RawIO::read(size_t nvalues, float* value) {
  itsByteIO->read(nvalues * sizeof(float), value);
  return nvalues * sizeof(float);
}

size_t RawIO::read(size_t nvalues, double* value) {
  itsByteIO->read(nvalues * sizeof(double), value);
  return nvalues * sizeof(double);
}

size_t RawIO::read(size_t nvalues, Complex* value) { return TypeIO::read(nvalues, value); }

size_t RawIO::read(size_t nvalues, DComplex* value) { return TypeIO::read(nvalues, value); }

size_t RawIO::read(size_t nvalues, String* value) { return TypeIO::read(nvalues, value); }

}  // namespace casacore
