
#include "UvwStMan.h"

#include "UvwStManColumn.h"

namespace casacore {

UvwStMan::UvwStMan(const String &, const Record &) : DataManager() { }

UvwStMan::UvwStMan(const UvwStMan &source)
    : DataManager(),
      name_(source.name_) {}

UvwStMan::~UvwStMan() noexcept = default;

void UvwStMan::create64(rownr_t /*nRow*/) {
  file_ = UvwFile::CreateNew(fileName());
}

rownr_t UvwStMan::open64(rownr_t /*n_row*/, AipsIO &) {
  file_ = UvwFile::OpenExisting(fileName());
  return file_.NRows();
}

DataManagerColumn *UvwStMan::makeDirArrColumn(
    const String & /*name*/, int dataType,
    const String & /*dataTypeID*/) {

  if (dataType == TpDouble) {
    column_ = std::make_unique<UvwStManColumn>(file_);
    return column_.get();
  } else {
    throw std::runtime_error(
        "Trying to create a Uvw column with wrong type");
  }
}

void UvwStMan::deleteManager() { unlink(fileName().c_str()); }

void UvwStMan::prepare() {
  if(column_)
    column_->Prepare(table());
}

void UvwStMan::addRow64(rownr_t) { }

void UvwStMan::removeRow64(rownr_t) {
  throw std::runtime_error(
      "Can't remove rows from a UvwStMan");
}

void UvwStMan::addColumn(DataManagerColumn*) {
    throw std::runtime_error(
        "Can't add generic columns to UvwStMan");
}

void UvwStMan::removeColumn(DataManagerColumn *column) {
  if(column_.get() == column) {
    column_.reset();
  }
  throw std::runtime_error(
      "Trying to remove column that was not part of the storage manager");
}

}  // namespace casacore

