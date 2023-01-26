#ifndef DYSCO_STORAGE_MAN_COLUMN_H
#define DYSCO_STORAGE_MAN_COLUMN_H

#include "dyscodistribution.h"
#include "dysconormalization.h"

#include <casacore/tables/DataMan/StManColumnBase.h>

#include <casa/Arrays/IPosition.h>

#include <cstdint>
#include <map>

namespace dyscostman {

class DyscoStMan;

/**
 * Base class for columns of the DyscoStMan.
 * @author André Dysco
 */
class DyscoStManColumn : public casacore::StManColumnBase {
 public:
  /**
   * Constructor, to be overloaded by subclass.
   * @param parent The parent stman to which this column belongs.
   * @param dtype The column's type as defined by Casacore.
   */
  explicit DyscoStManColumn(DyscoStMan *parent, int dtype)
      : casacore::StManColumnBase(dtype),
        _offsetInBlock(0),
        _storageManager(parent) {}

  /** Destructor */
  virtual ~DyscoStManColumn() {}

  /** To be called before destructing the class. */
  virtual void shutdown() = 0;

  /**
   * Whether this column is writable
   * @returns @c true
   */
  virtual bool isWritable() const override { return true; }

  virtual void Prepare(DyscoDistribution distribution,
                       Normalization normalization, double studentsTNu,
                       double distributionTruncation) = 0;

  virtual void InitializeAfterNRowsPerBlockIsKnown() = 0;

  virtual size_t CalculateBlockSize(size_t nRowsInBlock,
                                    size_t nAntennae) const = 0;

  /**
   * Get number of bytes needed for column header of this column. This is
   * excluding the generic column header.
   * @returns Size of column header
   */
  virtual size_t ExtraHeaderSize() const { return 0; }

  virtual void SerializeExtraHeader(std::ostream &stream) const = 0;

  virtual void UnserializeExtraHeader(std::istream &stream) = 0;

  size_t OffsetInBlock() const { return _offsetInBlock; }

  void SetOffsetInBlock(size_t offsetInBlock) {
    _offsetInBlock = offsetInBlock;
  }

 protected:
  /** Get the storage manager for this column */
  DyscoStMan &storageManager() const { return *_storageManager; }

  /**
   * Read a row of compressed data from the stman file.
   * @param blockIndex The block index of the row to read.
   * @param dest The destination buffer, should be at least of size Stride().
   * @param size The nr of bytes to be read.
   */
  void readCompressedData(size_t blockIndex, unsigned char *dest, size_t size);

  /**
   * Write a row of compressed data to the stman file.
   * @param blockIndex The block index of the row to write.
   * @param data The data buffer containing Stride() bytes.
   * @param size The nr of bytes to be written.
   */
  void writeCompressedData(size_t blockIndex, const unsigned char *data,
                           size_t size);

  /**
   * Get the actual number of blocks in the file.
   */
  uint64_t nBlocksInFile() const;

  size_t getBlockIndex(uint64_t row) const;

  size_t getRowWithinBlock(uint64_t row) const;

  size_t nRowsInBlock() const;

  size_t nAntennae() const;

  uint64_t getRowIndex(size_t block) const;

  bool areOffsetsInitialized() const;

  void initializeRowsPerBlock(size_t rowsPerBlock, size_t antennaCount);

 private:
  DyscoStManColumn(const DyscoStManColumn &source) = delete;
  void operator=(const DyscoStManColumn &source) = delete;

  size_t _offsetInBlock;
  DyscoStMan *_storageManager;
};

}  // namespace dyscostman

#include "dyscostman.h"

namespace dyscostman {

inline void DyscoStManColumn::readCompressedData(size_t blockIndex,
                                                 unsigned char *dest,
                                                 size_t size) {
  _storageManager->readCompressedData(blockIndex, this, dest, size);
}

inline void DyscoStManColumn::writeCompressedData(size_t blockIndex,
                                                  const unsigned char *data,
                                                  size_t size) {
  _storageManager->writeCompressedData(blockIndex, this, data, size);
}

inline uint64_t DyscoStManColumn::nBlocksInFile() const {
  return _storageManager->nBlocksInFile();
}

inline size_t DyscoStManColumn::getBlockIndex(uint64_t row) const {
  return _storageManager->getBlockIndex(row);
}

inline size_t DyscoStManColumn::nRowsInBlock() const {
  return _storageManager->nRowsInBlock();
}

inline size_t DyscoStManColumn::nAntennae() const {
  return _storageManager->nAntennae();
}

inline uint64_t DyscoStManColumn::getRowIndex(size_t block) const {
  return _storageManager->getRowIndex(block);
}

inline size_t DyscoStManColumn::getRowWithinBlock(uint64_t rowIndex) const {
  return _storageManager->getRowWithinBlock(rowIndex);
}

inline bool DyscoStManColumn::areOffsetsInitialized() const {
  return _storageManager->areOffsetsInitialized();
}

inline void DyscoStManColumn::initializeRowsPerBlock(size_t rowsPerBlock,
                                                     size_t antennaCount) {
  _storageManager->initializeRowsPerBlock(rowsPerBlock, antennaCount, true);
}

}  // namespace dyscostman

#endif
