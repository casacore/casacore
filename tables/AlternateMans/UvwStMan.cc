
#include "UvwStMan.h"

#include "UvwStManColumn.h"

namespace casacore {

UvwStMan::UvwStMan(const casacore::String &, const casacore::Record &) : DataManager() { }

UvwStMan::UvwStMan(const UvwStMan &source)
    : DataManager(),
      name_(source.name_) {}

UvwStMan::~UvwStMan() noexcept = default;

void UvwStMan::create64(casacore::rownr_t /*nRow*/) {
  file_ = UvwFile::CreateNew(fileName());
}

casacore::rownr_t UvwStMan::open64(casacore::rownr_t /*n_row*/, casacore::AipsIO &) {
  file_ = UvwFile::OpenExisting(fileName());
  return file_.NRows();
}

casacore::DataManagerColumn *UvwStMan::makeDirArrColumn(
    const casacore::String & /*name*/, int dataType,
    const casacore::String & /*dataTypeID*/) {

  if (dataType == casacore::TpDouble) {
    column_ = std::make_unique<UvwStManColumn>(file_, table());
    return column_.get();
  } else {
    throw std::runtime_error(
        "Trying to create a Uvw column with wrong type");
  }
}

void UvwStMan::deleteManager() { unlink(fileName().c_str()); }

void UvwStMan::addRow64(casacore::rownr_t) { }

void UvwStMan::removeRow64(casacore::rownr_t) {
  throw std::runtime_error(
      "Can't remove rows from a UvwStMan");
}

void UvwStMan::addColumn(casacore::DataManagerColumn*) {
    throw std::runtime_error(
        "Can't add generic columns to UvwStMan");
}

void UvwStMan::removeColumn(casacore::DataManagerColumn *column) {
  if(column_.get() == column) {
    column_.reset();
  }
  throw std::runtime_error(
      "Trying to remove column that was not part of the storage manager");
}

}  // namespace casacore

