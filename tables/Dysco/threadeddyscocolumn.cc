#include "threadeddyscocolumn.h"

#include "dyscostman.h"
#include "dyscostmanerror.h"

#include "bytepacker.h"
#include "threadgroup.h"

#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/tables/Tables/ScalarColumn.h>

#include <algorithm>
#include <limits>

namespace dyscostman {

template <typename DataType>
ThreadedDyscoColumn<DataType>::ThreadedDyscoColumn(DyscoStMan *parent,
                                                   int dtype)
    : DyscoStManColumn(parent, dtype), _bitsPerSymbol(0), _ant1Col(),
      _ant2Col(), _fieldCol(), _packedBlockReadBuffer(),
      _unpackedSymbolReadBuffer(), _stopThreads(false),
      _currentBlock(std::numeric_limits<size_t>::max()),
      _isCurrentBlockChanged(false), _blockSize(0), _antennaCount(0),
      _timeBlockBuffer() {}

// prepare the class for destruction when the derived class is destructed.
// this is necessary because the virtual function of the derived class might get
// called to empty the cache.
template <typename DataType> void ThreadedDyscoColumn<DataType>::shutdown() {
  if (_isCurrentBlockChanged)
    storeBlock();

  stopThreads();
}

template <typename DataType>
ThreadedDyscoColumn<DataType>::~ThreadedDyscoColumn() {
  shutdown();
}

template <typename DataType> void ThreadedDyscoColumn<DataType>::stopThreads() {
  std::unique_lock<std::mutex> lock(_mutex);

  if (_threadGroup.empty()) {
    if (!_cache.empty())
      throw DyscoStManError(
          "DyscoStMan is flushed before at least two timeblocks were stored. "
          "DyscoStMan can not handle this situation.");
  } else {
    // Don't stop threads before cache is empty
    while (!_cache.empty())
      _cacheChangedCondition.wait(lock);

    // Signal threads to stop
    _stopThreads = true;
    _cacheChangedCondition.notify_all();

    // Wait for threads to end
    lock.unlock();
    _threadGroup.join_all();
  }
}

template <typename DataType>
void ThreadedDyscoColumn<DataType>::setShapeColumn(
    const casacore::IPosition &shape) {
  _shape = shape;
}

template <typename DataType>
void ThreadedDyscoColumn<DataType>::loadBlock(size_t blockIndex) {
  if (blockIndex < nBlocksInFile()) {
    readCompressedData(blockIndex, _packedBlockReadBuffer.data(), _blockSize);
    const size_t nPolarizations = _shape[0], nChannels = _shape[1],
                 nRows = nRowsInBlock(),
                 nMetaFloats = metaDataFloatCount(nRows, nPolarizations,
                                                  nChannels, _antennaCount);
    unsigned char *symbolStart =
        _packedBlockReadBuffer.data() + nMetaFloats * sizeof(float);
    BytePacker::unpack(_bitsPerSymbol, _unpackedSymbolReadBuffer.data(),
                       symbolStart,
                       symbolCount(nRows, nPolarizations, nChannels));
    float *metaData = reinterpret_cast<float *>(_packedBlockReadBuffer.data());
    initializeDecode(_timeBlockBuffer.get(), metaData, nRows, _antennaCount);
    uint64_t startRow = getRowIndex(blockIndex);
    _timeBlockBuffer->resize(nRows);
    for (size_t blockRow = 0; blockRow != nRows; ++blockRow) {
      int a1 = (*_ant1Col)(startRow + blockRow),
          a2 = (*_ant2Col)(startRow + blockRow);
      decode(_timeBlockBuffer.get(), _unpackedSymbolReadBuffer.data(), blockRow,
             a1, a2);
    }
  }
  _currentBlock = blockIndex;
  _isCurrentBlockChanged = false;
}

template <typename DataType>
void ThreadedDyscoColumn<DataType>::getValues(
    casacore::uInt rowNr, casacore::Array<DataType> *dataPtr) {
  if (!areOffsetsInitialized()) {
    // Trying to read before first block was written -- return zero
    // TODO if a few rows were written of the first block, those are
    // incorrectly returned. This is a rare case but can be fixed.
    for (typename casacore::Array<DataType>::contiter i = dataPtr->cbegin();
         i != dataPtr->cend(); ++i)
      *i = DataType();
  } else {
    size_t blockIndex = getBlockIndex(rowNr);
    if (blockIndex >= nBlocksInFile()) {
      // Trying to read a row that was not stored yet -- return zero
      for (typename casacore::Array<DataType>::contiter i = dataPtr->cbegin();
           i != dataPtr->cend(); ++i)
        *i = DataType();
    } else {
      std::unique_lock<std::mutex> lock(_mutex);
      // Wait until the block to be read is not in the write cache
      typename cache_t::const_iterator cacheItemPtr = _cache.find(blockIndex);
      while (cacheItemPtr != _cache.end()) {
        _cacheChangedCondition.wait(lock);
        cacheItemPtr = _cache.find(blockIndex);
      }
      lock.unlock();

      if (_currentBlock != blockIndex) {
        if (_isCurrentBlockChanged)
          storeBlock();
        loadBlock(blockIndex);
      }

      // The time block encoder is now initialized and contains the unpacked
      // block.
      _timeBlockBuffer->GetData(getRowWithinBlock(rowNr), dataPtr->data());
    }
  }
}

template <typename DataType> void ThreadedDyscoColumn<DataType>::storeBlock() {
  // Put the data of the current block into the cache so that the parallell
  // threads can write them
  std::unique_lock<std::mutex> lock(_mutex);
  CacheItem *item = new CacheItem(std::move(_timeBlockBuffer));
  // Wait until there is space available AND the row to be written is not in the
  // cache
  typename cache_t::iterator cacheItemPtr = _cache.find(_currentBlock);
  while (_cache.size() >= maxCacheSize() || cacheItemPtr != _cache.end()) {
    _cacheChangedCondition.wait(lock);
    cacheItemPtr = _cache.find(_currentBlock);
  }
  _cache.insert(typename cache_t::value_type(_currentBlock, item));
  _cacheChangedCondition.notify_all();
  lock.unlock();

  _isCurrentBlockChanged = false;
  const size_t nPolarizations = _shape[0], nChannels = _shape[1];
  _timeBlockBuffer.reset(
      new TimeBlockBuffer<data_t>(nPolarizations, nChannels));
  //_timeBlockBuffer->SetNAntennae(_antennaCount);
}

template <typename DataType>
void ThreadedDyscoColumn<DataType>::putValues(
    casacore::uInt rowNr, const casacore::Array<DataType> *dataPtr) {
  if (!areOffsetsInitialized()) {
    // If the manager did not initialize its offsets yet, then it is determined
    // from the first "time block" (a block with the same time, field and spw)
    // that is written into the measurement set.
    //
    // This only happens when a new measurement
    // set was created; if an existing measurement set is opened with at least
    // one block, then the offsets will be read from the headers.
    //
    // A consequence of this is that the first blocks in a new measurement set
    // are required to be written consecutively.
    double time = (*_timeCol)(rowNr);
    int fieldId = (*_fieldCol)(rowNr);
    int dataDescId = (*_dataDescIdCol)(rowNr);
    if (_timeBlockBuffer->Empty()) {
      // This is the first row written
      _currentBlock = 0;
      _lastWrittenTime = time;
      _lastWrittenField = fieldId;
      _lastWrittenDataDescId = dataDescId;
    } else if (time != _lastWrittenTime || fieldId != _lastWrittenField ||
               dataDescId != _lastWrittenDataDescId) {
      initializeRowsPerBlock(rowNr, _timeBlockBuffer->MaxAntennaIndex() + 1);
    }
  }

  const int ant1 = (*_ant1Col)(rowNr), ant2 = (*_ant2Col)(rowNr);
  if (areOffsetsInitialized()) {
    const size_t blockIndex = getBlockIndex(rowNr),
                 blockRow = getRowWithinBlock(rowNr);

    // Is this the first row of a new block?
    if (blockIndex != _currentBlock) {
      if (_isCurrentBlockChanged)
        storeBlock();

      // Load new block
      loadBlock(blockIndex);
    }
    _timeBlockBuffer->SetData(blockRow, ant1, ant2, dataPtr->data());
  } else {
    _timeBlockBuffer->SetData(rowNr, ant1, ant2, dataPtr->data());
  }
  _isCurrentBlockChanged = true;
}

template <typename DataType>
void ThreadedDyscoColumn<DataType>::Prepare(DyscoDistribution,
                                            Normalization,
                                            double /*studentsTNu*/,
                                            double /*distributionTruncation*/) {
  stopThreads();
  casacore::Table &table = storageManager().table();
  _ant1Col.reset(new casacore::ScalarColumn<int>(table, "ANTENNA1"));
  _ant2Col.reset(new casacore::ScalarColumn<int>(table, "ANTENNA2"));
  _fieldCol.reset(new casacore::ScalarColumn<int>(table, "FIELD_ID"));
  _dataDescIdCol.reset(new casacore::ScalarColumn<int>(table, "DATA_DESC_ID"));
  _timeCol.reset(new casacore::ScalarColumn<double>(table, "TIME"));

  size_t nPolarizations = _shape[0], nChannels = _shape[1];
  _timeBlockBuffer.reset(
      new TimeBlockBuffer<data_t>(nPolarizations, nChannels));
  if (_antennaCount != 0) {
    // TODO _timeBlockEncoder->SetNAntennae(_antennaCount);
  }
  _currentBlock = std::numeric_limits<size_t>::max();
}

template <typename DataType>
size_t ThreadedDyscoColumn<DataType>::defaultThreadCount() const {
  // Don't spawn more than 8 threads; it causes problems in NDPPP
  return std::min(8l, sysconf(_SC_NPROCESSORS_ONLN));
}

template <typename DataType>
void ThreadedDyscoColumn<DataType>::InitializeAfterNRowsPerBlockIsKnown() {
  stopThreads();
  if (_bitsPerSymbol == 0)
    throw DyscoStManError(
        "bitsPerSymbol not initialized in ThreadedDyscoColumn");

  _antennaCount = nAntennae();
  _blockSize = CalculateBlockSize(nRowsInBlock(), _antennaCount);
  _packedBlockReadBuffer.resize(_blockSize);
  const size_t nPolarizations = _shape[0], nChannels = _shape[1];
  _unpackedSymbolReadBuffer.resize(
      symbolCount(nRowsInBlock(), nPolarizations, nChannels));
  // TODO _timeBlockEncoder->SetNAntennae(_antennaCount);

  // start the threads
  size_t threadCount = defaultThreadCount();
  EncodingThreadFunctor functor;
  functor.parent = this;
  _stopThreads = false;
  for (size_t i = 0; i != threadCount; ++i)
    _threadGroup.create_thread(functor);
}

template <typename DataType>
void ThreadedDyscoColumn<DataType>::encodeAndWrite(
    size_t blockIndex, const CacheItem &item, unsigned char *packedSymbolBuffer,
    unsigned int *unpackedSymbolBuffer, ThreadDataBase *threadUserData) {
  const size_t nPolarizations = _shape[0], nChannels = _shape[1];
  const size_t metaDataSize =
      sizeof(float) * metaDataFloatCount(nRowsInBlock(), nPolarizations,
                                         nChannels, _antennaCount);
  const size_t nSymbols =
      symbolCount(nRowsInBlock(), nPolarizations, nChannels);

  float *metaBuffer = reinterpret_cast<float *>(packedSymbolBuffer);
  unsigned char *binaryBuffer = packedSymbolBuffer + metaDataSize;

  encode(threadUserData, item.encoder.get(), metaBuffer, unpackedSymbolBuffer,
         _antennaCount);

  BytePacker::pack(_bitsPerSymbol, binaryBuffer, unpackedSymbolBuffer,
                   nSymbols);

  const size_t binarySize = BytePacker::bufferSize(nSymbols, _bitsPerSymbol);
  writeCompressedData(blockIndex, packedSymbolBuffer,
                      metaDataSize + binarySize);
}

// Continuously write items from the cache into the measurement
// set untill asked to quit.
template <typename DataType>
void ThreadedDyscoColumn<DataType>::EncodingThreadFunctor::operator()() {
  const size_t nPolarizations = parent->_shape[0],
               nChannels = parent->_shape[1];
  const size_t nSymbols =
      parent->symbolCount(parent->nRowsInBlock(), nPolarizations, nChannels);

  std::unique_lock<std::mutex> lock(parent->_mutex);
  ao::uvector<unsigned char> packedSymbolBuffer(parent->_blockSize);
  ao::uvector<unsigned> unpackedSymbolBuffer(nSymbols);
  cache_t &cache = parent->_cache;

  std::unique_ptr<ThreadDataBase> threadUserData =
      parent->initializeEncodeThread();

  while (!parent->_stopThreads) {
    typename cache_t::iterator i;
    bool isItemAvailable = parent->isWriteItemAvailable(i);
    while (!isItemAvailable && !parent->_stopThreads) {
      parent->_cacheChangedCondition.wait(lock);
      isItemAvailable = parent->isWriteItemAvailable(i);
    }

    if (isItemAvailable) {
      size_t blockIndex = i->first;
      CacheItem &item = *i->second;
      item.isBeingWritten = true;

      lock.unlock();
      parent->encodeAndWrite(blockIndex, item, &packedSymbolBuffer[0],
                             &unpackedSymbolBuffer[0], threadUserData.get());

      lock.lock();
      delete &item;
      cache.erase(i);
      parent->_cacheChangedCondition.notify_all();
    }
  }
}

// This function should only be called with a locked mutex
template <typename DataType>
bool ThreadedDyscoColumn<DataType>::isWriteItemAvailable(
    typename cache_t::iterator &i) {
  i = _cache.begin();
  while (i != _cache.end() && i->second->isBeingWritten)
    ++i;
  return (i != _cache.end());
}

template <typename DataType>
size_t
ThreadedDyscoColumn<DataType>::CalculateBlockSize(size_t nRowsInBlock,
                                                  size_t nAntennae) const {
  size_t nPolarizations = _shape[0], nChannels = _shape[1];
  const size_t metaDataSize =
      sizeof(float) *
      metaDataFloatCount(nRowsInBlock, nPolarizations, nChannels, nAntennae);
  const size_t nSymbols = symbolCount(nRowsInBlock, nPolarizations, nChannels);
  const size_t binarySize = BytePacker::bufferSize(nSymbols, _bitsPerSymbol);
  return metaDataSize + binarySize;
}

template <typename DataType>
void ThreadedDyscoColumn<DataType>::SerializeExtraHeader(
    std::ostream &stream) const {
  Header header;
  header.antennaCount = _antennaCount;
  header.blockSize = _blockSize;
  header.Serialize(stream);
}

template <typename DataType>
void ThreadedDyscoColumn<DataType>::UnserializeExtraHeader(
    std::istream &stream) {
  Header header;
  header.Unserialize(stream);
  _antennaCount = header.antennaCount;
  _blockSize = header.blockSize;
}

template class ThreadedDyscoColumn<std::complex<float>>;
template class ThreadedDyscoColumn<float>;

} // namespace dyscostman
