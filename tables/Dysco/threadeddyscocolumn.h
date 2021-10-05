#ifndef DYSCO_THREADED_DYSCO_COLUMN_H
#define DYSCO_THREADED_DYSCO_COLUMN_H

#include <casacore/tables/DataMan/DataManError.h>

#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/tables/Tables/ScalarColumn.h>

#include <condition_variable>
#include <cstdint>
#include <map>
#include <memory>
#include <mutex>
#include <random>

#include "dyscostmancol.h"
#include "serializable.h"
#include "stochasticencoder.h"
#include "threadgroup.h"
#include "timeblockbuffer.h"

namespace dyscostman {

class DyscoStMan;

/**
 * A column for storing compressed values in a threaded way, tailered for the
 * data and weight columns that use a threaded approach for encoding.
 * @author André Offringa
 */
template <typename DataType>
class ThreadedDyscoColumn : public DyscoStManColumn {
 public:
  typedef DataType data_t;

  /**
   * Create a new column. Internally called by DyscoStMan when creating a
   * new column.
   */
  ThreadedDyscoColumn(DyscoStMan *parent, int dtype);

  ThreadedDyscoColumn(const ThreadedDyscoColumn &source) = delete;

  void operator=(const ThreadedDyscoColumn &source) = delete;

  /** Destructor. */
  virtual ~ThreadedDyscoColumn();

  /** Set the dimensions of values in this column. */
  virtual void setShapeColumn(const casacore::IPosition &shape) override;

  /** Get the dimensions of the values in a particular row.
   * @param rownr The row to get the shape for. */
  virtual casacore::IPosition shape(casacore::uInt /*rownr*/) override {
    return _shape;
  }

  /**
   * Read the values for a particular row. This will read the required
   * data and decode it.
   * @param rowNr The row number to get the values for.
   * @param dataPtr The array of values, which should be a contiguous array.
   */
  virtual void getArrayComplexV(
      casacore::uInt rowNr,
      casacore::Array<casacore::Complex> *dataPtr) override {
    // Note that this method is specialized for std::complex<float> -- the
    // generic method won't do anything
    return DyscoStManColumn::getArrayComplexV(rowNr, dataPtr);
  }
  virtual void getArrayfloatV(casacore::uInt rowNr,
                              casacore::Array<float> *dataPtr) override {
    // Note that this method is specialized for float -- the generic method
    // won't do anything
    return DyscoStManColumn::getArrayfloatV(rowNr, dataPtr);
  }

  /**
   * Write values into a particular row. This will add the values into the cache
   * and returns immediately afterwards. A pool of threads will encode the items
   * in the cache and write them to disk.
   * @param rowNr The row number to write the values to.
   * @param dataPtr The data pointer, which should be a contiguous array.
   */
  virtual void putArrayComplexV(
      casacore::uInt rowNr,
      const casacore::Array<casacore::Complex> *dataPtr) override {
    // Note that this method is specialized for std::complex<float> -- the
    // generic method won't do anything
    return DyscoStManColumn::putArrayComplexV(rowNr, dataPtr);
  }
  virtual void putArrayfloatV(casacore::uInt rowNr,
                              const casacore::Array<float> *dataPtr) override {
    // Note that this method is specialized for float -- the generic method
    // won't do anything
    return DyscoStManColumn::putArrayfloatV(rowNr, dataPtr);
  }

  virtual void Prepare(DyscoDistribution distribution,
                       Normalization normalization, double studentsTNu,
                       double distributionTruncation) override;

  /**
   * Prepare this column for reading/writing. Used internally by the stman.
   */
  virtual void InitializeAfterNRowsPerBlockIsKnown() override;

  /**
   * Set the bits per symbol. Should only be called by DyscoStMan.
   * @param bitsPerSymbol New number of bits per symbol.
   */
  void SetBitsPerSymbol(unsigned bitsPerSymbol) {
    _bitsPerSymbol = bitsPerSymbol;
  }

  virtual size_t CalculateBlockSize(size_t nRowsInBlock,
                                    size_t nAntennae) const final override;

  virtual size_t ExtraHeaderSize() const override { return Header::Size(); }

  virtual void SerializeExtraHeader(std::ostream &stream) const final override;

  virtual void UnserializeExtraHeader(std::istream &stream) final override;

 protected:
  class ThreadDataBase {
   public:
    virtual ~ThreadDataBase(){};
  };

  typedef typename TimeBlockBuffer<data_t>::symbol_t symbol_t;

  virtual void initializeDecode(TimeBlockBuffer<data_t> *buffer,
                                const float *metaBuffer, size_t nRow,
                                size_t nAntennae) = 0;

  virtual void decode(TimeBlockBuffer<data_t> *buffer, const symbol_t *data,
                      size_t blockRow, size_t a1, size_t a2) = 0;

  virtual std::unique_ptr<ThreadDataBase> initializeEncodeThread() = 0;

  virtual void encode(ThreadDataBase *threadData,
                      TimeBlockBuffer<data_t> *buffer, float *metaBuffer,
                      symbol_t *symbolBuffer, size_t nAntennae) = 0;

  virtual size_t metaDataFloatCount(size_t nRow, size_t nPolarizations,
                                    size_t nChannels,
                                    size_t nAntennae) const = 0;

  virtual size_t symbolCount(size_t nRowsInBlock, size_t nPolarizations,
                             size_t nChannels) const = 0;

  virtual void shutdown() override final;

  virtual size_t defaultThreadCount() const;

  size_t getBitsPerSymbol() const { return _bitsPerSymbol; }

  const casacore::IPosition &shape() const { return _shape; }

 private:
  struct CacheItem {
    CacheItem(std::unique_ptr<TimeBlockBuffer<data_t>> &&encoder_)
        : encoder(std::move(encoder_)), isBeingWritten(false) {}

    std::unique_ptr<TimeBlockBuffer<data_t>> encoder;
    bool isBeingWritten;
  };

  struct EncodingThreadFunctor {
    void operator()();
    ThreadedDyscoColumn *parent;
  };
  struct Header : public Serializable {
    uint32_t blockSize;
    uint32_t antennaCount;

    static uint32_t Size() { return 8; }

    virtual void Serialize(std::ostream &stream) const override {
      SerializeToUInt32(stream, blockSize);
      SerializeToUInt32(stream, antennaCount);
    }

    virtual void Unserialize(std::istream &stream) override {
      blockSize = UnserializeUInt32(stream);
      antennaCount = UnserializeUInt32(stream);
    }
  };

  typedef std::map<size_t, CacheItem *> cache_t;

  void getValues(casacore::uInt rowNr, casacore::Array<data_t> *dataPtr);
  void putValues(casacore::uInt rowNr, const casacore::Array<data_t> *dataPtr);

  void stopThreads();
  void encodeAndWrite(size_t blockIndex, const CacheItem &item,
                      unsigned char *packedSymbolBuffer,
                      unsigned int *unpackedSymbolBuffer,
                      ThreadDataBase *threadUserData);
  bool isWriteItemAvailable(typename cache_t::iterator &i);
  void loadBlock(size_t blockIndex);
  void storeBlock();
  size_t maxCacheSize() const {
    return ThreadedDyscoColumn::defaultThreadCount() * 12 / 10 + 1;
  }

  unsigned _bitsPerSymbol;
  casacore::IPosition _shape;
  std::unique_ptr<casacore::ScalarColumn<int>> _ant1Col, _ant2Col, _fieldCol,
      _dataDescIdCol;
  std::unique_ptr<casacore::ScalarColumn<double>> _timeCol;
  double _lastWrittenTime;
  int _lastWrittenField, _lastWrittenDataDescId;
  ao::uvector<unsigned char> _packedBlockReadBuffer;
  ao::uvector<unsigned int> _unpackedSymbolReadBuffer;
  cache_t _cache;
  bool _stopThreads;
  std::mutex _mutex;
  threadgroup _threadGroup;
  std::condition_variable _cacheChangedCondition;
  size_t _currentBlock;
  bool _isCurrentBlockChanged;
  size_t _blockSize;
  size_t _antennaCount;

  std::unique_ptr<TimeBlockBuffer<data_t>> _timeBlockBuffer;
};

template <>
inline void ThreadedDyscoColumn<std::complex<float>>::getArrayComplexV(
    casacore::uInt rowNr, casacore::Array<casacore::Complex> *dataPtr) {
  getValues(rowNr, dataPtr);
}
template <>
inline void ThreadedDyscoColumn<std::complex<float>>::putArrayComplexV(
    casacore::uInt rowNr, const casacore::Array<casacore::Complex> *dataPtr) {
  putValues(rowNr, dataPtr);
}
template <>
inline void ThreadedDyscoColumn<float>::getArrayfloatV(
    casacore::uInt rowNr, casacore::Array<float> *dataPtr) {
  getValues(rowNr, dataPtr);
}
template <>
inline void ThreadedDyscoColumn<float>::putArrayfloatV(
    casacore::uInt rowNr, const casacore::Array<float> *dataPtr) {
  putValues(rowNr, dataPtr);
}

extern template class ThreadedDyscoColumn<std::complex<float>>;
extern template class ThreadedDyscoColumn<float>;

}  // namespace dyscostman

#endif
