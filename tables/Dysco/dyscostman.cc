#include "dyscostman.h"
#include "dyscodatacolumn.h"
#include "dyscostmancol.h"
#include "dyscostmanerror.h"
#include "dyscoweightcolumn.h"

#include "header.h"

void register_dyscostman() { dyscostman::DyscoStMan::registerClass(); }

namespace dyscostman {

const unsigned short DyscoStMan::VERSION_MAJOR = 1,
                     DyscoStMan::VERSION_MINOR = 0;

DyscoStMan::DyscoStMan(unsigned dataBitCount, unsigned weightBitCount,
                       const casacore::String &name)
    : DataManager(),
      _nRow(0),
      _nBlocksInFile(0),
      _rowsPerBlock(0),
      _antennaCount(0),
      _blockSize(0),
      _headerSize(0),
      _name(name),
      _dataBitCount(dataBitCount),
      _weightBitCount(weightBitCount),
      _distribution(TruncatedGaussianDistribution),
      _normalization(Normalization::kAF),
      _studentTNu(0.0),
      _distributionTruncation(2.5),
      _staticSeed(false) {}

DyscoStMan::DyscoStMan(const casacore::String &name,
                       const casacore::Record &spec)
    : DataManager(),
      _nRow(0),
      _nBlocksInFile(0),
      _rowsPerBlock(0),
      _antennaCount(0),
      _blockSize(0),
      _headerSize(0),
      _name(name),
      _dataBitCount(0),
      _weightBitCount(0),
      _distribution(GaussianDistribution),
      _normalization(Normalization::kAF),
      _studentTNu(0.0),
      _distributionTruncation(0.0),
      _staticSeed(false) {
  setFromSpec(spec);
}

DyscoStMan::DyscoStMan(const DyscoStMan &source)
    : DataManager(),
      _nRow(0),
      _nBlocksInFile(0),
      _rowsPerBlock(0),
      _antennaCount(0),
      _blockSize(0),
      _headerSize(0),
      _name(source._name),
      _dataBitCount(source._dataBitCount),
      _weightBitCount(source._weightBitCount),
      _distribution(source._distribution),
      _normalization(source._normalization),
      _studentTNu(source._studentTNu),
      _distributionTruncation(source._distributionTruncation),
      _staticSeed(source._staticSeed) {}

void DyscoStMan::setFromSpec(const casacore::Record &spec) {
  // Here we need to load from _spec
  int i = spec.description().fieldNumber("dataBitCount");
  if (i >= 0) {
    _dataBitCount = spec.asInt("dataBitCount");
    if (_dataBitCount == 0)
      throw DyscoStManError("Invalid error for data bit rate");

    _weightBitCount = spec.asInt("weightBitCount");
    if (_weightBitCount == 0)
      throw DyscoStManError("Invalid error for weight bit rate");

    std::string str = spec.asString("distribution");
    if (str == "Uniform")
      _distribution = UniformDistribution;
    else if (str == "Gaussian")
      _distribution = GaussianDistribution;
    else if (str == "StudentT")
      _distribution = StudentsTDistribution;
    else if (str == "TruncatedGaussian")
      _distribution = TruncatedGaussianDistribution;
    else
      throw DyscoStManError("Unsupported distribution specified");
    str = spec.asString("normalization");
    if (str == "RF")
      _normalization = Normalization::kRF;
    else if (str == "AF")
      _normalization = Normalization::kAF;
    else if (str == "Row")
      _normalization = Normalization::kRow;
    else
      throw DyscoStManError("Unsupported normalization specified");
    if (spec.description().fieldNumber("studentTNu") >= 0)
      _studentTNu = spec.asDouble("studentTNu");
    else
      _studentTNu = 0.0;
    _distributionTruncation = spec.asDouble("distributionTruncation");
  }
}

void DyscoStMan::makeEmpty() {
  for (std::unique_ptr<DyscoStManColumn> &col : _columns) {
    col->shutdown();
  }
  _columns.clear();
}

DyscoStMan::~DyscoStMan() { makeEmpty(); }

casacore::Record DyscoStMan::dataManagerSpec() const {
  casacore::Record spec;
  spec.define("dataBitCount", _dataBitCount);
  spec.define("weightBitCount", _weightBitCount);
  std::string distStr;
  switch (_distribution) {
    case GaussianDistribution:
      distStr = "Gaussian";
      break;
    case UniformDistribution:
      distStr = "Uniform";
      break;
    case StudentsTDistribution:
      distStr = "StudentsT";
      break;
    case TruncatedGaussianDistribution:
      distStr = "TruncatedGaussian";
      break;
  }
  spec.define("distribution", distStr);
  std::string normStr;
  switch (_normalization) {
    case Normalization::kAF:
      normStr = "AF";
      break;
    case Normalization::kRF:
      normStr = "RF";
      break;
    case Normalization::kRow:
      normStr = "Row";
      break;
  }
  spec.define("normalization", normStr);
  spec.define("studentTNu", _studentTNu);
  spec.define("distributionTruncation", _distributionTruncation);
  return spec;
}

void DyscoStMan::registerClass() {
  DataManager::registerCtor("DyscoStMan", makeObject);
}

casacore::Bool DyscoStMan::flush(casacore::AipsIO &,
                                 casacore::Bool /*doFsync*/) {
  return false;
}

void DyscoStMan::create(casacore::uInt nRow) {
  _nRow = nRow;
  _fStream.reset(new std::fstream(
      fileName().c_str(),
      std::ios_base::in | std::ios_base::out | std::ios_base::trunc));
  if (_fStream->fail())
    throw DyscoStManError("I/O error: could not create new file '" +
                          fileName() + "'");
  _nBlocksInFile = 0;
}

void DyscoStMan::writeHeader() {
  _fStream->seekp(0, std::ios_base::beg);
  Header header;
  header.columnCount = _columns.size();
  header.storageManagerName = _name;
  header.rowsPerBlock = _rowsPerBlock;
  header.antennaCount = _antennaCount;
  header.blockSize = _blockSize;
  header.versionMajor = VERSION_MAJOR;
  header.versionMinor = VERSION_MINOR;
  header.dataBitCount = _dataBitCount;
  header.weightBitCount = _weightBitCount;
  header.distribution = _distribution;
  header.normalization = static_cast<uint8_t>(_normalization);
  header.studentTNu = _studentTNu;
  header.distributionTruncation = _distributionTruncation;

  header.columnHeaderOffset = header.calculateColumnHeaderOffset();
  _headerSize = header.columnHeaderOffset;
  for (std::unique_ptr<DyscoStManColumn> &col : _columns)
    _headerSize += sizeof(GenericColumnHeader) + col->ExtraHeaderSize();
  header.headerSize = _headerSize;

  header.Serialize(*_fStream);

  for (std::unique_ptr<DyscoStManColumn> &col : _columns) {
    GenericColumnHeader cHeader;
    cHeader.columnHeaderSize = cHeader.calculateSize() + col->ExtraHeaderSize();
    cHeader.Serialize(*_fStream);
    col->SerializeExtraHeader(*_fStream);
  }
  if (_fStream->fail())
    throw DyscoStManError("I/O error: could not write to file");
}

void DyscoStMan::readHeader() {
  Header header;
  _fStream->seekg(0, std::ios_base::beg);
  header.Unserialize(*_fStream);
  if (_fStream->fail())
    throw DyscoStManError("I/O error: could not read file '" + fileName() +
                          "' -- is the file corrupted?");
  _headerSize = header.headerSize;
  size_t curColumnHeaderOffset = header.columnHeaderOffset;
  size_t columnCount = header.columnCount;
  _name = header.storageManagerName;
  _dataBitCount = header.dataBitCount;
  _weightBitCount = header.weightBitCount;
  _distribution = (enum DyscoDistribution)header.distribution;
  _normalization = (enum Normalization)header.normalization;
  _studentTNu = header.studentTNu;
  _distributionTruncation = header.distributionTruncation;
  _rowsPerBlock = header.rowsPerBlock;
  _antennaCount = header.antennaCount;
  _blockSize = header.blockSize;

  if (header.versionMajor != 1 || header.versionMinor != 0) {
    std::stringstream s;
    s << "The compressed file has file format version " << header.versionMajor
      << "." << header.versionMinor
      << ", but this version of Dysco can only open file format version 1.0. "
         "Upgrade Dysco.\n";
    throw DyscoStManError(s.str());
  }

  if (columnCount != _columns.size()) {
    std::stringstream s;
    s << "The column count in the DyscoStMan file (" << columnCount
      << ") does not match with the measurement set (" << _columns.size()
      << ")";
    throw DyscoStManError(s.str());
  }

  for (size_t i = 0; i != _columns.size(); ++i) {
    DyscoStManColumn &col = *_columns[i];
    GenericColumnHeader cHeader;
    _fStream->seekg(curColumnHeaderOffset, std::ios_base::beg);
    cHeader.Unserialize(*_fStream);
    col.UnserializeExtraHeader(*_fStream);
    curColumnHeaderOffset += cHeader.columnHeaderSize;
  }
}

void DyscoStMan::initializeRowsPerBlock(size_t rowsPerBlock,
                                        size_t antennaCount,
                                        bool writeToHeader) {
  if (areOffsetsInitialized() &&
      (rowsPerBlock != _rowsPerBlock || antennaCount != _antennaCount))
    throw DyscoStManError(
        "initializeRowsPerBlock() called with two different "
        "values; something is wrong");

  _rowsPerBlock = rowsPerBlock;
  _antennaCount = antennaCount;
  _blockSize = 0;
  for (std::unique_ptr<DyscoStManColumn> &col : _columns) {
    size_t columnBlockSize =
        col->CalculateBlockSize(rowsPerBlock, antennaCount);
    col->SetOffsetInBlock(_blockSize);
    _blockSize += columnBlockSize;

    col->InitializeAfterNRowsPerBlockIsKnown();
  }
  if (writeToHeader) writeHeader();
}

void DyscoStMan::open(casacore::uInt nRow, casacore::AipsIO &) {
  _nRow = nRow;
  _fStream.reset(new std::fstream(fileName().c_str(),
                                  std::ios_base::in | std::ios_base::out));
  if (_fStream->fail()) {
    _fStream.reset(new std::fstream(fileName().c_str(), std::ios_base::in));
    if (_fStream->fail())
      throw DyscoStManError("I/O error: could not open file '" + fileName() +
                            "', which should be an existing file");
  }

  readHeader();

  _fStream->seekg(0, std::ios_base::end);
  if (_fStream->fail())
    throw DyscoStManError("I/O error: error reading file '" + fileName());
  std::streampos size = _fStream->tellg();
  if (size > _headerSize)
    _nBlocksInFile = (size_t(size) - _headerSize) / _blockSize;
  else
    _nBlocksInFile = 0;
}

casacore::DataManagerColumn *DyscoStMan::makeScalarColumn(
    const casacore::String & /*name*/, int dataType,
    const casacore::String &dataTypeID) {
  std::ostringstream s;
  s << "Can not create scalar columns with DyscoStMan! (requested datatype: '"
    << dataTypeID << "' (" << dataType << ")";
  throw DyscoStManError(s.str());
}

casacore::DataManagerColumn *DyscoStMan::makeDirArrColumn(
    const casacore::String &name, int dataType,
    const casacore::String & /*dataTypeID*/) {
  std::unique_ptr<DyscoStManColumn> col;

  if (name == "WEIGHT_SPECTRUM") {
    if (dataType == casacore::TpFloat)
      col.reset(new DyscoWeightColumn(this, dataType));
    else
      throw DyscoStManError(
          "Trying to create a Dysco weight column with wrong type");
  } else if (dataType == casacore::TpComplex) {
    col.reset(new DyscoDataColumn(this, dataType));
    if (_staticSeed)
      static_cast<DyscoDataColumn &>(*col).SetStaticRandomizationSeed();
  } else
    throw DyscoStManError(
        "Trying to create a Dysco data column with wrong type");
  _columns.push_back(std::move(col));
  return _columns.back().get();
}

casacore::DataManagerColumn *DyscoStMan::makeIndArrColumn(
    const casacore::String & /*name*/, int /*dataType*/,
    const casacore::String & /*dataTypeID*/) {
  throw DyscoStManError(
      "makeIndArrColumn() called on DyscoStMan. DyscoStMan can only created "
      "direct columns!\nUse casacore::ColumnDesc::Direct as option in your "
      "column desc constructor");
}

void DyscoStMan::resync(casacore::uInt /*nRow*/) {}

void DyscoStMan::deleteManager() { unlink(fileName().c_str()); }

void DyscoStMan::prepare() {
  std::lock_guard<std::mutex> lock(_mutex);

  if (_dataBitCount == 0 || _weightBitCount == 0)
    throw DyscoStManError(
        "One of the required parameters of the DyscoStMan was not "
        "set!\nDyscoStMan was not correctly initialized by your program.");

  for (std::unique_ptr<DyscoStManColumn> &col : _columns) {
    DyscoDataColumn *dataCol = dynamic_cast<DyscoDataColumn *>(col.get());
    if (dataCol)
      dataCol->SetBitsPerSymbol(_dataBitCount);
    else {
      DyscoWeightColumn *wghtCol = dynamic_cast<DyscoWeightColumn *>(col.get());
      if (wghtCol) wghtCol->SetBitsPerSymbol(_weightBitCount);
    }
    col->Prepare(_distribution, _normalization, _studentTNu,
                 _distributionTruncation);
  }

  // In case this is a new measurement set, we do not know the rowsPerBlock yet
  // If this measurement set is opened, we do know it, and we have to call
  // initializeRowsPerBlock() to let the columns know this value.
  if (areOffsetsInitialized())
    initializeRowsPerBlock(_rowsPerBlock, _antennaCount, false);
}

void DyscoStMan::reopenRW() {}

void DyscoStMan::addRow(casacore::uInt nrrow) { _nRow += nrrow; }

void DyscoStMan::removeRow(casacore::uInt rowNr) {
  if (rowNr != _nRow - 1)
    throw DyscoStManError(
        "Trying to remove a row in the middle of the file: "
        "the DyscoStMan does not support this");
  _nRow--;
}

void DyscoStMan::addColumn(casacore::DataManagerColumn * /*column*/) {
  if (_nBlocksInFile != 0)
    throw DyscoStManError(
        "Can't add columns while data has been committed to table");

  prepare();
  writeHeader();
}

void DyscoStMan::removeColumn(casacore::DataManagerColumn *column) {
  for (std::vector<std::unique_ptr<DyscoStManColumn>>::iterator i =
           _columns.begin();
       i != _columns.end(); ++i) {
    if (i->get() == column) {
      _columns.erase(i);
      writeHeader();
      return;
    }
  }
  throw DyscoStManError(
      "Trying to remove column that was not part of the storage manager");
}

void DyscoStMan::readCompressedData(size_t blockIndex,
                                    const DyscoStManColumn *column,
                                    unsigned char *dest, size_t size) {
  std::lock_guard<std::mutex> lock(_mutex);
  size_t fileOffset = getFileOffset(blockIndex);

  size_t columnOffset = column->OffsetInBlock();
  _fStream->seekg(fileOffset + columnOffset, std::ios_base::beg);
  _fStream->read(reinterpret_cast<char *>(dest), size);
  if (_fStream->fail()) {
    // This can be sort of ok ; row exists because other columns have written
    // here, but no data had been written yet for this column
    if (blockIndex + 1 != _nBlocksInFile)
      throw DyscoStManError("I/O error: error while reading file '" +
                            fileName() + "'");
    _fStream->clear();  // reset fail bit
  }
}

void DyscoStMan::writeCompressedData(size_t blockIndex,
                                     const DyscoStManColumn *column,
                                     const unsigned char *data, size_t size) {
  std::lock_guard<std::mutex> lock(_mutex);
  if (_nBlocksInFile <= blockIndex) {
    _nBlocksInFile = blockIndex + 1;
  }
  size_t fileOffset = getFileOffset(blockIndex);
  _fStream->seekp(fileOffset + column->OffsetInBlock(), std::ios_base::beg);
  _fStream->write(reinterpret_cast<const char *>(data), size);
  if (_fStream->fail())
    throw DyscoStManError("I/O error: error while writing file '" + fileName() +
                          "'");
}

}  // namespace dyscostman
