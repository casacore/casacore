// # MSValidIds.cc:  this defines MSValidIds.
// # Copyright (C) 2000
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

// # Includes

#include <casacore/ms/MSOper/MSValidIds.h>

#include <casacore/casa/Exceptions/Error.h>
#include <casacore/ms/MeasurementSets/MSColumns.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

MSValidIds::MSValidIds() : romsCols_p(nullptr), hasDoppler_p(false), hasSource_p(false) { ; }

MSValidIds::MSValidIds(const MeasurementSet &ms)
    : romsCols_p(nullptr), hasDoppler_p(false), hasSource_p(false) {
  attach(ms);
}

MSValidIds::MSValidIds(const MSValidIds &other)
    : romsCols_p(nullptr), hasDoppler_p(false), hasSource_p(false) {
  *this = other;
}

MSValidIds::~MSValidIds() { clear(); }

MSValidIds &MSValidIds::operator=(const MSValidIds &other) {
  if (this != &other) {
    attach(other.ms_p);
  }
  return *this;
}

void MSValidIds::attach(const MeasurementSet &ms) {
  clear();

  ms_p = ms;

  romsCols_p = new MSColumns(ms_p);
  AlwaysAssert(romsCols_p, AipsError);

  // check on existance of optional sub-tables
  hasDoppler_p = ms_p.keywordSet().isDefined("DOPPLER");
  hasSource_p = ms_p.keywordSet().isDefined("SOURCE");
}

int MSValidIds::antenna1(rownr_t rownr) const {
  int result = -1;
  if (checkRow(rownr) && romsCols_p) {
    result = checkResult(romsCols_p->antenna1()(rownr), ms_p.antenna());
  }
  return result;
}

int MSValidIds::antenna2(rownr_t rownr) const {
  int result = -1;
  if (checkRow(rownr) && romsCols_p) {
    result = checkResult(romsCols_p->antenna2()(rownr), ms_p.antenna());
  }
  return result;
}

int MSValidIds::dataDescId(rownr_t rownr) const {
  int result = -1;
  if (checkRow(rownr) && romsCols_p) {
    result = checkResult(romsCols_p->dataDescId()(rownr), ms_p.dataDescription());
  }
  return result;
}

int MSValidIds::fieldId(rownr_t rownr) const {
  int result = -1;
  if (checkRow(rownr) && romsCols_p) {
    result = checkResult(romsCols_p->fieldId()(rownr), ms_p.field());
  }
  return result;
}

int MSValidIds::observationId(rownr_t rownr) const {
  int result = -1;
  if (checkRow(rownr) && romsCols_p) {
    result = checkResult(romsCols_p->observationId()(rownr), ms_p.observation());
  }
  return result;
}

int MSValidIds::processorId(rownr_t rownr) const {
  int result = -1;
  if (checkRow(rownr) && romsCols_p) {
    result = checkResult(romsCols_p->processorId()(rownr), ms_p.processor());
  }
  return result;
}

int MSValidIds::stateId(rownr_t rownr) const {
  int result = -1;
  if (checkRow(rownr) && romsCols_p) {
    result = checkResult(romsCols_p->stateId()(rownr), ms_p.state());
  }
  return result;
}

int MSValidIds::polarizationId(rownr_t rownr) const {
  int result = dataDescId(rownr);
  if (result >= 0) {
    result =
        checkResult(romsCols_p->dataDescription().polarizationId()(result), ms_p.polarization());
  }
  return result;
}

int MSValidIds::spectralWindowId(rownr_t rownr) const {
  int result = dataDescId(rownr);
  if (result >= 0) {
    result = checkResult(romsCols_p->dataDescription().spectralWindowId()(result),
                         ms_p.spectralWindow());
  }
  return result;
}

int MSValidIds::dopplerId(rownr_t rownr) const {
  int result = hasDoppler_p ? spectralWindowId(rownr) : -1;
  if (result >= 0) {
    result = romsCols_p->spectralWindow().dopplerId().isNull()
                 ? -1
                 : romsCols_p->spectralWindow().dopplerId()(result);
  }
  return result;
}

int MSValidIds::sourceId(rownr_t rownr) const {
  int result = hasSource_p ? fieldId(rownr) : -1;
  if (result >= 0) {
    result = romsCols_p->field().sourceId()(result);
  }
  return result;
}

void MSValidIds::clear() {
  delete romsCols_p;
  romsCols_p = nullptr;

  hasDoppler_p = hasSource_p = false;
}

}  // namespace casacore
