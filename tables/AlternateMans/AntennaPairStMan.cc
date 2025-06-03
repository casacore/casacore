
#include "AntennaPairStMan.h"

#include "AntennaPairStManColumn.h"

namespace casacore {

AntennaPairStMan::AntennaPairStMan(const String &, const Record &) : DataManager() { }

AntennaPairStMan::AntennaPairStMan(const AntennaPairStMan &source)
    : DataManager(),
      name_(source.name_) {}

AntennaPairStMan::~AntennaPairStMan() noexcept = default;

void AntennaPairStMan::create64(rownr_t /*nRow*/) {
  file_ = AntennaPairFile::CreateNew(fileName());
}

rownr_t AntennaPairStMan::open64(rownr_t n_row, AipsIO &) {
  file_ = AntennaPairFile::OpenExisting(fileName());
  return n_row;
}

DataManagerColumn *AntennaPairStMan::makeScalarColumn(
    const String &name, int dataType,
    const String & /*dataTypeID*/) {

  if (dataType == TpInt) {
    if(name == "ANTENNA1") {
      columns_[0] = std::make_unique<AntennaPairStManColumn>(file_, false);
       return columns_[0].get();
    } if(name == "ANTENNA2") {
      columns_[1] = std::make_unique<AntennaPairStManColumn>(file_, true);
      return columns_[1].get();
    } else {
      throw std::runtime_error("Trying to create a column with AntennaPairStMan that is named '" + name + "': only ANTENNA1 or ANTENNA2 is supported");
    }
  } else {
    throw std::runtime_error(
        "Trying to create an ANTENNA column (" + name + ") with wrong type");
  }
}

void AntennaPairStMan::deleteManager() { unlink(fileName().c_str()); }

void AntennaPairStMan::addRow64(rownr_t) { }

void AntennaPairStMan::removeRow64(rownr_t) {
  throw std::runtime_error(
      "Can't remove rows from a AntennaPairStMan");
}

void AntennaPairStMan::removeColumn(DataManagerColumn *column) {
  if(columns_[0].get() == column) {
    columns_[0].reset();
  }
  else if(columns_[1].get() == column) {
    columns_[1].reset();
  }
  else {
    throw std::runtime_error(
        "Trying to remove column that was not part of the storage manager");
  }
}

}  // namespace casacore

