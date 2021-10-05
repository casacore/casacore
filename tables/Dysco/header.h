#ifndef DYSCO_HEADER_H
#define DYSCO_HEADER_H

#include "serializable.h"

#include <stdint.h>

namespace dyscostman {

#ifndef DOXYGEN_SHOULD_SKIP_THIS
struct Header : public Serializable {
  /** Size of the total header, including column subheaders */
  uint32_t headerSize;
  /** Start offset of the column headers */
  uint32_t columnHeaderOffset;
  /** Number of columns and column headers */
  uint32_t columnCount;

  std::string storageManagerName;

  uint32_t rowsPerBlock;
  uint32_t antennaCount;
  uint32_t blockSize;

  /** File version number */
  uint16_t versionMajor, versionMinor;

  uint8_t dataBitCount;
  uint8_t weightBitCount;
  uint8_t distribution;
  uint8_t normalization;
  double studentTNu, distributionTruncation;

  uint32_t calculateColumnHeaderOffset() const {
    return 7 * 4 +                              // 6 x uint32 + string length
           storageManagerName.size() + 2 * 2 +  // 2 x uint16
           4 * 1 +                              // 4 x uint8
           2 * 8;                               // 2 x double
  }

  virtual void Serialize(std::ostream &stream) const final override {
    SerializeToUInt32(stream, headerSize);
    SerializeToUInt32(stream, columnHeaderOffset);
    SerializeToUInt32(stream, columnCount);
    SerializeTo32bString(stream, storageManagerName);
    SerializeToUInt32(stream, rowsPerBlock);
    SerializeToUInt32(stream, antennaCount);
    SerializeToUInt32(stream, blockSize);
    SerializeToUInt16(stream, versionMajor);
    SerializeToUInt16(stream, versionMinor);
    SerializeToUInt8(stream, dataBitCount);
    SerializeToUInt8(stream, weightBitCount);
    SerializeToUInt8(stream, distribution);
    SerializeToUInt8(stream, normalization);
    SerializeToDouble(stream, studentTNu);
    SerializeToDouble(stream, distributionTruncation);
  }

  virtual void Unserialize(std::istream &stream) final override {
    headerSize = UnserializeUInt32(stream);
    columnHeaderOffset = UnserializeUInt32(stream);
    columnCount = UnserializeUInt32(stream);

    Unserialize32bString(stream, storageManagerName);

    rowsPerBlock = UnserializeUInt32(stream);
    antennaCount = UnserializeUInt32(stream);
    blockSize = UnserializeUInt32(stream);

    /** File version number */
    versionMajor = UnserializeUInt16(stream);
    versionMinor = UnserializeUInt16(stream);

    dataBitCount = UnserializeUInt8(stream);
    weightBitCount = UnserializeUInt8(stream);
    distribution = UnserializeUInt8(stream);
    normalization = UnserializeUInt8(stream);
    studentTNu = UnserializeDouble(stream);
    distributionTruncation = UnserializeDouble(stream);
  }

  // the column headers start here (first generic header, then column specific
  // header)
};

struct GenericColumnHeader : public Serializable {
  /** size of generic header + column specific header */
  uint32_t columnHeaderSize;

  virtual void Serialize(std::ostream &stream) const override {
    SerializeToUInt32(stream, columnHeaderSize);
  }

  virtual void Unserialize(std::istream &stream) override {
    columnHeaderSize = UnserializeUInt32(stream);
  }

  virtual uint32_t calculateSize() const { return 4; }
};
}

#endif

#endif
