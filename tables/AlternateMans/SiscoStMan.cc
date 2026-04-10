#include "SiscoStMan.h"

#include "SiscoStManColumn.h"

void register_SiscoStMan() { casacore::SiscoStMan::registerClass(); }

namespace casacore {
namespace {
  /**
   * Create an object with given name and spec.
   * This methods gets registered in the SiscoStMan "constructor" map.
   * The caller has to delete the object.
   */
  static casacore::DataManager *Make(const casacore::String &name,
                                           const casacore::Record &spec) {
    return new SiscoStMan(name, spec);
  }
}

SiscoStMan::SiscoStMan(const casacore::String &/*name*/,
                       const casacore::Record &spec)
    : DataManager() {
  const std::string kDeflateLevelKey = "deflate_level";
  const std::string kPredictLevelKey = "predict_level";
  const std::string kStokesIKey = "stokes_i";
  const std::string kDiagonalKey = "diagonal";

  if (spec.isDefined(kDeflateLevelKey)) {
    deflate_level_ = spec.asInt(kDeflateLevelKey);
    if (deflate_level_ <= 0)
      throw std::runtime_error("Invalid value for " + kDeflateLevelKey);
  }
  if (spec.isDefined(kPredictLevelKey)) {
    predict_level_ = spec.asInt(kPredictLevelKey);
    if (predict_level_ < -1)
      throw std::runtime_error("Invalid value for " + kPredictLevelKey);
  }
  bool stokes_i = false;
  bool diagonal = false;
  if (spec.isDefined(kStokesIKey)) {
    stokes_i = spec.asBool(kStokesIKey);
  }
  if(spec.isDefined(kDiagonalKey)) {
    diagonal = spec.asBool(kDiagonalKey);
  }
  if(stokes_i && diagonal)
    throw std::runtime_error("Can't combine Stokes I writing with Diagonal writing");
  if(stokes_i)
    store_mode_ = SiscoStoreMode::StokesI;
  else if(diagonal)
    store_mode_ = SiscoStoreMode::Diagonal;
  else
    store_mode_ = SiscoStoreMode::Original;
}

SiscoStMan::SiscoStMan(const SiscoStMan &source)
    : DataManager(),
      name_(source.name_), deflate_level_(source.deflate_level_), predict_level_(source.predict_level_), store_mode_(source.store_mode_) {}

SiscoStMan::~SiscoStMan() noexcept = default;

casacore::Record SiscoStMan::dataManagerSpec() const {
  casacore::Record result;
  result.define("deflate_level", deflate_level_);
  result.define("predict_level", predict_level_);
  result.define("stokes_i", store_mode_ == SiscoStoreMode::StokesI);
  result.define("diagonal", store_mode_ == SiscoStoreMode::Diagonal);
  return result;
}

void SiscoStMan::registerClass() {
  DataManager::registerCtor("SiscoStMan", Make);
}

void SiscoStMan::create64(casacore::rownr_t) {
}

casacore::rownr_t SiscoStMan::open64(casacore::rownr_t n_row, casacore::AipsIO &) {
  return n_row;
}

casacore::DataManagerColumn *SiscoStMan::makeScalarColumn(
    const casacore::String & /*name*/, int dataType,
    const casacore::String &dataTypeID) {
  std::ostringstream s;
  s << "Can not create scalar columns with SiscoStMan! (requested datatype: '"
    << dataTypeID << "' (" << dataType << ")";
  throw std::runtime_error(s.str());
}

casacore::DataManagerColumn *SiscoStMan::makeDirArrColumn(
    const casacore::String& name, int dataType,
    const casacore::String& dataTypeID) {
  return makeIndArrColumn(name, dataType, dataTypeID);
}

casacore::DataManagerColumn *SiscoStMan::makeIndArrColumn(
    [[maybe_unused]] const casacore::String &name, int dataType,
    [[maybe_unused]] const casacore::String &dataTypeID) {
  column_ = std::make_unique<SiscoStManColumn>(*this, static_cast<DataType>(dataType), store_mode_);
  return column_.get();
}

casacore::rownr_t SiscoStMan::resync64(casacore::rownr_t nRow)
{
  return nRow;
}

void SiscoStMan::deleteManager() { unlink(fileName().c_str()); }

void SiscoStMan::prepare() {
  if(column_)
    column_->Prepare();
}

void SiscoStMan::reopenRW() {}

void SiscoStMan::addRow64(casacore::rownr_t) { }

void SiscoStMan::removeRow64(casacore::rownr_t) {
}

void SiscoStMan::addColumn(casacore::DataManagerColumn*) {
    throw std::runtime_error(
        "Can't add generic columns to SiscoStMan");
}

void SiscoStMan::removeColumn(casacore::DataManagerColumn *column) {
  if (column_.get() == column) {
    column_.reset();
    return;
  } else {
    throw std::runtime_error(
        "Trying to remove column that was not part of the storage manager");
  }
}

}  // namespace casacore

