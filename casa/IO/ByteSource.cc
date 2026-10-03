// # ByteSource.cc: Class for read-only access to data in a given format
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

#include <casacore/casa/IO/ByteSource.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/IO/TypeIO.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

ByteSource::ByteSource() {}

ByteSource::ByteSource(const std::shared_ptr<TypeIO>& typeIO) : BaseSinkSource(typeIO) {
  if (!isReadable()) {
    throw(AipsError("ByteSource is not readable"));
  }
}

ByteSource::ByteSource(const ByteSource& source) : BaseSinkSource(source) {}

ByteSource& ByteSource::operator=(const ByteSource& source) {
  BaseSinkSource::operator=(source);
  return *this;
}

ByteSource::~ByteSource() {}

ByteSource& ByteSource::operator>>(bool& value) {
  itsTypeIO->read(1, &value);
  return *this;
}

ByteSource& ByteSource::operator>>(char& value) {
  itsTypeIO->read(1, &value);
  return *this;
}

ByteSource& ByteSource::operator>>(unsigned char& value) {
  itsTypeIO->read(1, &value);
  return *this;
}

ByteSource& ByteSource::operator>>(short& value) {
  itsTypeIO->read(1, &value);
  return *this;
}

ByteSource& ByteSource::operator>>(unsigned short& value) {
  itsTypeIO->read(1, &value);
  return *this;
}

ByteSource& ByteSource::operator>>(int& value) {
  itsTypeIO->read(1, &value);
  return *this;
}

ByteSource& ByteSource::operator>>(unsigned int& value) {
  itsTypeIO->read(1, &value);
  return *this;
}

ByteSource& ByteSource::operator>>(Int64& value) {
  itsTypeIO->read(1, &value);
  return *this;
}

ByteSource& ByteSource::operator>>(uInt64& value) {
  itsTypeIO->read(1, &value);
  return *this;
}

ByteSource& ByteSource::operator>>(float& value) {
  itsTypeIO->read(1, &value);
  return *this;
}

ByteSource& ByteSource::operator>>(double& value) {
  itsTypeIO->read(1, &value);
  return *this;
}

ByteSource& ByteSource::operator>>(Complex& value) {
  itsTypeIO->read(1, &value);
  return *this;
}

ByteSource& ByteSource::operator>>(DComplex& value) {
  itsTypeIO->read(1, &value);
  return *this;
}

ByteSource& ByteSource::operator>>(String& value) {
  itsTypeIO->read(1, &value);
  return *this;
}

void ByteSource::read(size_t nvalues, bool* value) { itsTypeIO->read(nvalues, value); }

void ByteSource::read(size_t nvalues, char* value) { itsTypeIO->read(nvalues, value); }

void ByteSource::read(size_t nvalues, unsigned char* value) { itsTypeIO->read(nvalues, value); }

void ByteSource::read(size_t nvalues, short* value) { itsTypeIO->read(nvalues, value); }

void ByteSource::read(size_t nvalues, unsigned short* value) { itsTypeIO->read(nvalues, value); }

void ByteSource::read(size_t nvalues, int* value) { itsTypeIO->read(nvalues, value); }

void ByteSource::read(size_t nvalues, unsigned int* value) { itsTypeIO->read(nvalues, value); }

void ByteSource::read(size_t nvalues, Int64* value) { itsTypeIO->read(nvalues, value); }

void ByteSource::read(size_t nvalues, uInt64* value) { itsTypeIO->read(nvalues, value); }

void ByteSource::read(size_t nvalues, float* value) { itsTypeIO->read(nvalues, value); }

void ByteSource::read(size_t nvalues, double* value) { itsTypeIO->read(nvalues, value); }

void ByteSource::read(size_t nvalues, Complex* value) { itsTypeIO->read(nvalues, value); }

void ByteSource::read(size_t nvalues, DComplex* value) { itsTypeIO->read(nvalues, value); }

void ByteSource::read(size_t nvalues, String* value) { itsTypeIO->read(nvalues, value); }

}  // namespace casacore
