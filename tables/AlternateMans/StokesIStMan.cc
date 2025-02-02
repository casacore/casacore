#include "StokesIStMan.h"

#include "StokesIStManColumn.h"

void register_stokesistman() { casacore::StokesIStMan::registerClass(); }

namespace casacore {
namespace {
  /**
   * Create an object with given name and spec.
   * This methods gets registered in the StokesIStMan "constructor" map.
   * The caller has to delete the object.
   */
  static casacore::DataManager *Make(const casacore::String &name,
                                           const casacore::Record &spec) {
    return new StokesIStMan(name, spec);
  }
}

StokesIStMan::StokesIStMan(const casacore::String &/*name*/,
                           const casacore::Record &/*spec*/)
    : DataManager() {
}

StokesIStMan::StokesIStMan(const StokesIStMan &source)
    : DataManager(),
      name_(source.name_) {}

StokesIStMan::~StokesIStMan() noexcept = default;

casacore::Record StokesIStMan::dataManagerSpec() const {
  return casacore::Record();
}

void StokesIStMan::registerClass() {
  DataManager::registerCtor("StokesIStMan", Make);
}

void StokesIStMan::create64(casacore::rownr_t nRow) {
  file_ = BufferedColumnarFile::CreateNew(fileName(), kHeaderSize, CalculateAndUpdateStride());
  unsigned char data[kHeaderSize];
  std::copy_n(kMagicHeaderTag, 8, data);
  file_.WriteHeader(data);
  file_.AddRows(nRow);
}

casacore::rownr_t StokesIStMan::open64(casacore::rownr_t /*n_row*/, casacore::AipsIO &) {
  file_ = BufferedColumnarFile::OpenExisting(fileName(), kHeaderSize);
  unsigned char data[kHeaderSize];
  file_.ReadHeader(data);
  if (!std::equal(data, data + 8, kMagicHeaderTag)) {
    throw std::runtime_error(
        "The Stokes I columnar file header does not have the expected tag for Stokes I "
        "columns: the measurement set may be damaged");
  }
  return file_.NRows();
}

casacore::DataManagerColumn *StokesIStMan::makeScalarColumn(
    const casacore::String & /*name*/, int dataType,
    const casacore::String &dataTypeID) {
  std::ostringstream s;
  s << "Can not create scalar columns with StokesIStMan! (requested datatype: '"
    << dataTypeID << "' (" << dataType << ")";
  throw std::runtime_error(s.str());
}

casacore::DataManagerColumn *StokesIStMan::makeDirArrColumn(
    const casacore::String & /*name*/, int dataType,
    const casacore::String & /*dataTypeID*/) {

  if (dataType == casacore::TpFloat || dataType == casacore::TpComplex || dataType == casacore::TpBool)
    return columns_.emplace_back(std::make_unique<StokesIStManColumn>(*this, file_, static_cast<DataType>(dataType))).get();
  else
    throw std::runtime_error(
        "Trying to create a Stokes I column with wrong type");
}

casacore::DataManagerColumn *StokesIStMan::makeIndArrColumn(
    const casacore::String & /*name*/, int /*dataType*/,
    const casacore::String & /*dataTypeID*/) {
  throw std::runtime_error(
      "makeIndArrColumn() called on StokesIStMan. StokesIStMan can only create "
      "direct columns!\nUse casacore::ColumnDesc::Direct as option in the "
      "column desc constructor");
}

casacore::rownr_t StokesIStMan::resync64(casacore::rownr_t nRow)
{
  return nRow;
}

void StokesIStMan::deleteManager() { unlink(fileName().c_str()); }

void StokesIStMan::prepare() {}

void StokesIStMan::reopenRW() {}

void StokesIStMan::addRow64(casacore::rownr_t nrrow) { file_.AddRows(nrrow); }

void StokesIStMan::removeRow64(casacore::rownr_t rowNr) {
  if (rowNr != file_.NRows() - 1)
    throw std::runtime_error(
        "Trying to remove a row in the middle of the file: "
        "the StokesIStMan does not support this");
  file_.DeleteRow();
}

void StokesIStMan::addColumn(casacore::DataManagerColumn * /*column*/) {
    throw std::runtime_error(
        "Can't add generic columns to StokesIStMan");
}

void StokesIStMan::removeColumn(casacore::DataManagerColumn *column) {
  for (std::vector<std::unique_ptr<StokesIStManColumn>>::iterator i =
           columns_.begin();
       i != columns_.end(); ++i) {
    if (i->get() == column) {
      columns_.erase(i);
      file_.SetStride(CalculateAndUpdateStride());
      return;
    }
  }
  throw std::runtime_error(
      "Trying to remove column that was not part of the storage manager");
}

uint64_t StokesIStMan::CalculateAndUpdateStride() {
  uint64_t offset = 0;
  for (std::unique_ptr<StokesIStManColumn>& column : columns_) {
    column->setOffset(offset);
    offset += column->getStoredSizeInBytes();
  }
  return offset;
}

}  // namespace casacore

