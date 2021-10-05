#ifndef DYSCO_BYTE_PACKER_H
#define DYSCO_BYTE_PACKER_H

#include <stdexcept>

namespace dyscostman {

/**
 * Class for bit packing of values into bytes.
 *
 * Contains several methods that can pack and unpack an array of unsigned
 * values into a bit-packed array, using as few bytes as possible.
 * The number of bits used is fixed for all values in the array. All methods
 * assume that the specified output array has at least enough space to store
 * the packed / unpacked data. When packing, the amount of space needed is
 * ceil(symbolCount / bitCount).
 *
 * The @ref pack() and @ref unpack() methods can call the method with given
 * bitcount at runtime. If the bitcount is known at compile time, one of the
 * other methods can be used. For each of these calls, the input symbols are
 * assumed to occupy at most the given number of bits. The number of bytes
 * written during pack operations is ceil(symbolCount * bitCount / 8).
 * unpack operations will write symbolCount symbols into the output buffer.
 */
class BytePacker {
 public:
  /**
   * Call a pack..() function for a given bit count. Will forward the pack
   * operation to the one for the given bit count.
   * @param bitCount the number of bits to use per symbol
   * @param dest output buffer (see class desc for size)
   * @param symbolBuffer the input buffer
   * @param symbolCount number of symbols in @p symbolBuffer.
   */
  static void pack(unsigned bitCount, unsigned char *dest,
                   const unsigned *symbolBuffer, size_t symbolCount);

  /**
   * Call an unpack..() function for a given bit count. Will forward the unpack
   * operation to the one for the given bit count.
   * @param bitCount the number of bits used per symbol
   * @param symbolBuffer output buffer
   * @param packedBuffer the input buffer with the packed symbols
   * @param symbolCount number of symbols that will be unpacked into @p
   * symbolBuffer.
   */
  static void unpack(unsigned bitCount, unsigned *symbolBuffer,
                     unsigned char *packedBuffer, size_t symbolCount);

  /**
   * Pack the symbols from symbolBuffer into the destination array using
   * bitCount=2.
   */
  static void pack2(unsigned char *dest, const unsigned *symbolBuffer,
                    size_t symbolCount);
  /**
   * Reverse of pack2(). Will write symbolCount items into the symbolBuffer.
   */
  static void unpack2(unsigned *symbolBuffer, unsigned char *packedBuffer,
                      size_t symbolCount);

  /**
   * Pack the symbols from symbolBuffer into the destination array using
   * bitCount=3.
   */
  static void pack3(unsigned char *dest, const unsigned *symbolBuffer,
                    size_t symbolCount);
  /**
   * Reverse of pack3(). Will write symbolCount items into the symbolBuffer.
   */
  static void unpack3(unsigned *symbolBuffer, unsigned char *packedBuffer,
                      size_t symbolCount);

  /**
   * Pack the symbols from symbolBuffer into the destination array using
   * bitCount=4.
   */
  static void pack4(unsigned char *dest, const unsigned *symbolBuffer,
                    size_t symbolCount);
  /**
   * Reverse of pack4(). Will write symbolCount items into the symbolBuffer.
   */
  static void unpack4(unsigned *symbolBuffer, unsigned char *packedBuffer,
                      size_t symbolCount);

  /**
   * Pack the symbols from symbolBuffer into the destination array using
   * bitCount=6.
   */
  static void pack6(unsigned char *dest, const unsigned *symbolBuffer,
                    size_t symbolCount);

  /**
   * Reverse of pack6(). Will write symbolCount items into the symbolBuffer.
   */
  static void unpack6(unsigned *symbolBuffer, unsigned char *packedBuffer,
                      size_t symbolCount);

  /**
   * Pack the symbols from symbolBuffer into the destination array using
   * bitCount=8.
   */
  static void pack8(unsigned char *dest, const unsigned *symbolBuffer,
                    size_t symbolCount);
  /**
   * Reverse of pack8(). Will write symbolCount items into the symbolBuffer.
   */
  static void unpack8(unsigned *symbolBuffer, unsigned char *packedBuffer,
                      size_t symbolCount);

  /**
   * Pack the symbols from symbolBuffer into the destination array using
   * bitCount=10.
   */
  static void pack10(unsigned char *dest, const unsigned *symbolBuffer,
                     size_t symbolCount);
  /**
   * Reverse of pack10(). Will write symbolCount items into the symbolBuffer.
   */
  static void unpack10(unsigned *symbolBuffer, unsigned char *packedBuffer,
                       size_t symbolCount);

  /**
   * Pack the symbols from symbolBuffer into the destination array using
   * bitCount=12.
   */
  static void pack12(unsigned char *dest, const unsigned *symbolBuffer,
                     size_t symbolCount);
  /**
   * Reverse of pack12(). Will write symbolCount items into the symbolBuffer.
   */
  static void unpack12(unsigned *symbolBuffer, unsigned char *packedBuffer,
                       size_t symbolCount);

  /**
   * Pack the symbols from symbolBuffer into the destination array using
   * bitCount=16.
   */
  static void pack16(unsigned char *dest, const unsigned *symbolBuffer,
                     size_t symbolCount);
  /**
   * Reverse of pack16(). Will write symbolCount items into the symbolBuffer.
   */
  static void unpack16(unsigned *symbolBuffer, unsigned char *packedBuffer,
                       size_t symbolCount);

  static size_t bufferSize(size_t nSymbols, size_t nBits) {
    return (nSymbols * nBits + 7) / 8;
  }
};

inline void BytePacker::pack(unsigned int bitCount, unsigned char *dest,
                             const unsigned int *symbolBuffer,
                             size_t symbolCount) {
  switch (bitCount) {
    case 2:
      pack2(dest, symbolBuffer, symbolCount);
      break;
    case 3:
      pack3(dest, symbolBuffer, symbolCount);
      break;
    case 4:
      pack4(dest, symbolBuffer, symbolCount);
      break;
    case 6:
      pack6(dest, symbolBuffer, symbolCount);
      break;
    case 8:
      pack8(dest, symbolBuffer, symbolCount);
      break;
    case 10:
      pack10(dest, symbolBuffer, symbolCount);
      break;
    case 12:
      pack12(dest, symbolBuffer, symbolCount);
      break;
    case 16:
      pack16(dest, symbolBuffer, symbolCount);
      break;
    default:
      throw std::runtime_error("Unsupported packing size");
  }
}

inline void BytePacker::unpack(unsigned int bitCount,
                               unsigned int *symbolBuffer,
                               unsigned char *packedBuffer,
                               size_t symbolCount) {
  switch (bitCount) {
    case 2:
      unpack2(symbolBuffer, packedBuffer, symbolCount);
      break;
    case 3:
      unpack3(symbolBuffer, packedBuffer, symbolCount);
      break;
    case 4:
      unpack4(symbolBuffer, packedBuffer, symbolCount);
      break;
    case 6:
      unpack6(symbolBuffer, packedBuffer, symbolCount);
      break;
    case 8:
      unpack8(symbolBuffer, packedBuffer, symbolCount);
      break;
    case 10:
      unpack10(symbolBuffer, packedBuffer, symbolCount);
      break;
    case 12:
      unpack12(symbolBuffer, packedBuffer, symbolCount);
      break;
    case 16:
      unpack16(symbolBuffer, packedBuffer, symbolCount);
      break;
    default:
      throw std::runtime_error("Unsupported unpacking size");
  }
}

inline void BytePacker::pack2(unsigned char *dest,
                              const unsigned int *symbolBuffer,
                              size_t symbolCount) {
  const size_t limit = symbolCount / 4;
  for (size_t i = 0; i != limit; i++) {
    *dest = (*symbolBuffer);  // bits 1-2 into 1-2
    ++symbolBuffer;
    *dest |= (*symbolBuffer) << 2;  // bits 1-2 into 3-4
    ++symbolBuffer;
    *dest |= (*symbolBuffer) << 4;  // bits 1-2 into 5-6
    ++symbolBuffer;
    *dest |= (*symbolBuffer) << 6;  // bits 1-2 into 7-8
    ++symbolBuffer;
    ++dest;
  }
  size_t pos = limit * 4;
  if (pos != symbolCount) {
    *dest = (*symbolBuffer);  // bits 1-2 into 1-2
    ++pos;

    if (pos != symbolCount) {
      ++symbolBuffer;
      *dest |= (*symbolBuffer) << 2;  // bits 1-2 into 3-4
      ++pos;

      if (pos != symbolCount) {
        ++symbolBuffer;
        *dest |= (*symbolBuffer) << 4;  // bits 1-2 into 5-6
      }
    }
  }
}

inline void BytePacker::unpack2(unsigned *symbolBuffer,
                                unsigned char *packedBuffer,
                                size_t symbolCount) {
  const size_t limit = symbolCount / 4;
  for (size_t i = 0; i != limit; i++) {
    *symbolBuffer = *packedBuffer & 0x03;  // bits 1-2 into 1-2
    ++symbolBuffer;
    *symbolBuffer = ((*packedBuffer) & 0x0C) >> 2;  // bits 3-4 into 1-2
    ++symbolBuffer;
    *symbolBuffer = ((*packedBuffer) & 0x30) >> 4;  // bits 5-6 into 1-2
    ++symbolBuffer;
    *symbolBuffer = ((*packedBuffer) & 0xC0) >> 6;  // bits 7-8 into 1-2
    ++symbolBuffer;
    ++packedBuffer;
  }
  size_t pos = limit * 4;
  if (pos != symbolCount) {
    *symbolBuffer = *packedBuffer & 0x03;  // bits 1-2 into 1-2
    ++pos;

    if (pos != symbolCount) {
      ++symbolBuffer;
      *symbolBuffer = ((*packedBuffer) & 0x0C) >> 2;  // bits 3-4 into 1-2
      ++pos;

      if (pos != symbolCount) {
        ++symbolBuffer;
        *symbolBuffer = ((*packedBuffer) & 0x30) >> 4;  // bits 5-6 into 1-2
      }
    }
  }
}

inline void BytePacker::pack3(unsigned char *dest, const unsigned *symbolBuffer,
                              size_t symbolCount) {
  const size_t limit = symbolCount / 8;
  for (size_t i = 0; i != limit; i++) {
    *dest = *symbolBuffer;  // 1. Bit 1-3 into 1-3
    ++symbolBuffer;
    *dest |= (*symbolBuffer) << 3;  // 2. Bits 1-3 into 4-6
    ++symbolBuffer;
    *dest |= ((*symbolBuffer) & 0x3) << 6;  // 3. Bits 1-2 into 7-8
    ++dest;

    *dest = ((*symbolBuffer) & 0x4) >> 2;  // 3b. Bit 3 into 1
    ++symbolBuffer;
    *dest |= (*symbolBuffer) << 1;  // 4. Bits 1-3 into 2-4
    ++symbolBuffer;
    *dest |= (*symbolBuffer) << 4;  // 5. Bits 1-3 into 5-7
    ++symbolBuffer;
    *dest |= ((*symbolBuffer) & 0x1) << 7;  // 6. Bit 1 into 8
    ++dest;

    *dest = ((*symbolBuffer) & 0x6) >> 1;  // 6b. Bits 2-3 into 1-2
    ++symbolBuffer;
    *dest |= (*symbolBuffer) << 2;  // 7. Bits 1-3 into bits 3-5
    ++symbolBuffer;
    *dest |= (*symbolBuffer) << 5;  // 8. Bits 1-3 into bits 6-8
    ++symbolBuffer;
    ++dest;
  }

  size_t pos = limit * 8;
  if (pos != symbolCount) {
    *dest = *symbolBuffer;  // 1. Bit 1-3 into 1-3
    ++pos;
    if (pos != symbolCount) {
      ++symbolBuffer;
      *dest |= (*symbolBuffer) << 3;  // 2. Bits 1-3 into 4-6
      ++pos;
      if (pos != symbolCount) {
        ++symbolBuffer;
        *dest |= ((*symbolBuffer) & 0x3) << 6;  // 3. Bits 1-2 into 7-8
        ++dest;
        *dest = ((*symbolBuffer) & 0x4) >> 2;  // Bit 3 into 1
        ++pos;
        if (pos != symbolCount) {
          ++symbolBuffer;
          *dest |= (*symbolBuffer) << 1;  // 4. Bits 1-3 into 2-4
          ++pos;
          if (pos != symbolCount) {
            ++symbolBuffer;
            *dest |= (*symbolBuffer) << 4;  // 5. Bits 1-3 into 5-7
            ++pos;
            if (pos != symbolCount) {
              ++symbolBuffer;
              *dest |= ((*symbolBuffer) & 0x1) << 7;  // 6. Bit 1 into 8
              ++dest;
              *dest = ((*symbolBuffer) & 0x6) >> 1;  // Bits 2-3 into 1-2
              ++pos;
              if (pos != symbolCount) {
                ++symbolBuffer;
                *dest |= (*symbolBuffer) << 2;  // 7. Bits 1-3 into bits 3-5
              }
            }
          }
        }
      }
    }
  }
}

inline void BytePacker::unpack3(unsigned *symbolBuffer,
                                unsigned char *packedBuffer,
                                size_t symbolCount) {
  const size_t limit = symbolCount / 8;
  for (size_t i = 0; i != limit; i++) {
    *symbolBuffer = (*packedBuffer) & 0x07;  // 1. Bits 1-3 into 1-3
    ++symbolBuffer;
    *symbolBuffer = ((*packedBuffer) & 0x38) >> 3;  // 2. Bits 4-6 into 1-3
    ++symbolBuffer;
    *symbolBuffer = ((*packedBuffer) & 0xC0) >> 6;  // 3. Bits 7-8 into 1-2
    ++packedBuffer;

    *symbolBuffer |= ((*packedBuffer) & 0x01) << 2;  // 3b. Bit 1 into 3
    ++symbolBuffer;
    *symbolBuffer = ((*packedBuffer) & 0x0e) >> 1;  // 4. Bit 2-4 into 1-3
    ++symbolBuffer;
    *symbolBuffer = ((*packedBuffer) & 0x70) >> 4;  // 5. Bit 5-7 into 1-3
    ++symbolBuffer;
    *symbolBuffer = ((*packedBuffer) & 0x80) >> 7;  // 6. Bit 8 into 1
    ++packedBuffer;

    *symbolBuffer |= ((*packedBuffer) & 0x03) << 1;  // 6b. Bit 1-2 into 2-3
    ++symbolBuffer;
    *symbolBuffer = ((*packedBuffer) & 0x1c) >> 2;  // 7. Bit 3-5 into 1-3
    ++symbolBuffer;
    *symbolBuffer = ((*packedBuffer) & 0xe0) >> 5;  // 8. Bit 6-8 into 1-3
    ++symbolBuffer;
    ++packedBuffer;
  }
  size_t pos = limit * 8;
  if (pos != symbolCount) {
    *symbolBuffer = (*packedBuffer) & 0x07;  // 1. Bits 1-3 into 1-3
    ++pos;

    if (pos != symbolCount) {
      ++symbolBuffer;
      *symbolBuffer = ((*packedBuffer) & 0x38) >> 3;  // 2. Bits 4-6 into 1-3
      ++pos;

      if (pos != symbolCount) {
        ++symbolBuffer;
        *symbolBuffer = ((*packedBuffer) & 0xC0) >> 6;  // 3. Bits 7-8 into 1-2
        ++packedBuffer;
        *symbolBuffer |= ((*packedBuffer) & 0x01) << 2;  // 3b. Bit 1 into 3
        ++pos;

        if (pos != symbolCount) {
          ++symbolBuffer;
          *symbolBuffer = ((*packedBuffer) & 0x0e) >> 1;  // 4. Bit 2-4 into 1-3
          ++pos;

          if (pos != symbolCount) {
            ++symbolBuffer;
            *symbolBuffer =
                ((*packedBuffer) & 0x70) >> 4;  // 5. Bit 5-7 into 1-3
            ++pos;

            if (pos != symbolCount) {
              ++symbolBuffer;
              *symbolBuffer = ((*packedBuffer) & 0x80) >> 7;  // 6. Bit 8 into 1
              ++packedBuffer;
              *symbolBuffer |= ((*packedBuffer) & 0x03)
                               << 1;  // 6b. Bit 1-2 into 2-3
              ++pos;

              if (pos != symbolCount) {
                ++symbolBuffer;
                *symbolBuffer =
                    ((*packedBuffer) & 0x1c) >> 2;  // 7. Bit 3-5 into 1-3
              }
            }
          }
        }
      }
    }
  }
}

inline void BytePacker::pack4(unsigned char *dest,
                              const unsigned int *symbolBuffer,
                              size_t symbolCount) {
  const size_t limit = symbolCount / 2;
  for (size_t i = 0; i != limit; i++) {
    *dest = (*symbolBuffer);  // bits 1-4 into 1-4
    ++symbolBuffer;
    *dest |= (*symbolBuffer) << 4;  // bits 1-4 into 5-8
    ++symbolBuffer;
    ++dest;
  }
  if (limit * 2 != symbolCount) *dest = (*symbolBuffer);  // bits 1-4 into 1-4
}

inline void BytePacker::unpack4(unsigned *symbolBuffer,
                                unsigned char *packedBuffer,
                                size_t symbolCount) {
  const size_t limit = symbolCount / 2;
  for (size_t i = 0; i != limit; i++) {
    *symbolBuffer = *packedBuffer & 0x0F;  // bits 1-4 into 1-4
    ++symbolBuffer;
    *symbolBuffer = ((*packedBuffer) & 0xF0) >> 4;  // bits 5-8 into 1-4
    ++symbolBuffer;
    ++packedBuffer;
  }
  if (limit * 2 != symbolCount)
    *symbolBuffer = *packedBuffer & 0x0F;  // bits 1-4 into 1-4
}

inline void BytePacker::pack6(unsigned char *dest, const unsigned *symbolBuffer,
                              size_t symbolCount) {
  const size_t limit = symbolCount / 4;
  for (size_t i = 0; i != limit; i++) {
    *dest = *symbolBuffer;  // Bit 1-6 into 1-6
    ++symbolBuffer;
    *dest |= (*symbolBuffer & 3) << 6;  // Bits 1-2 into 7-8
    ++dest;

    *dest = (*symbolBuffer & 60) >> 2;  // Bits 3-6 into 1-4
    ++symbolBuffer;
    *dest |= (*symbolBuffer & 15) << 4;  // Bits 1-4 into 5-8
    ++dest;

    *dest = (*symbolBuffer & 48) >> 4;  // Bits 5-6 into 1-2
    ++symbolBuffer;
    *dest |= *symbolBuffer << 2;  // Bits 1-6 into bits 3-8
    ++symbolBuffer;
    ++dest;
  }

  size_t pos = limit * 4;
  if (pos != symbolCount) {
    *dest = *symbolBuffer;  // Bit 1-6 into 1-6
    ++pos;

    if (pos != symbolCount) {
      ++symbolBuffer;

      *dest |= (*symbolBuffer & 3) << 6;  // Bits 1-2 into 7-8
      ++dest;

      *dest = (*symbolBuffer & 60) >> 2;  // Bits 3-6 into 1-4
      ++pos;

      if (pos != symbolCount) {
        ++symbolBuffer;

        *dest |= (*symbolBuffer & 15) << 4;  // Bits 1-4 into 5-8
        ++dest;

        *dest = (*symbolBuffer & 48) >> 4;  // Bits 5-6 into 1-2
                                            //++symbolBuffer; ++pos;
      }
    }
  }
}

inline void BytePacker::unpack6(unsigned *symbolBuffer,
                                unsigned char *packedBuffer,
                                size_t symbolCount) {
  const size_t limit = symbolCount / 4;
  for (size_t i = 0; i != limit; i++) {
    *symbolBuffer = (*packedBuffer) & 63;  // Bits 1-6 into 1-6
    ++symbolBuffer;
    *symbolBuffer = ((*packedBuffer) & 192) >> 6;  // Bits 7-8 into 1-2
    ++packedBuffer;

    *symbolBuffer |= ((*packedBuffer) & 15) << 2;  // Bits 1-4 into 3-6
    ++symbolBuffer;
    *symbolBuffer = ((*packedBuffer) & 240) >> 4;  // Bits 5-8 into 1-4
    ++packedBuffer;

    *symbolBuffer |= ((*packedBuffer) & 3) << 4;  // Bits 1-2 into 5-6
    ++symbolBuffer;
    *symbolBuffer = ((*packedBuffer) & 252) >> 2;  // Bits 3-8 into 1-6
    ++packedBuffer;
    ++symbolBuffer;
  }
  size_t pos = limit * 4;
  if (pos != symbolCount) {
    *symbolBuffer = (*packedBuffer) & 63;  // Bits 1-6 into 1-6
    ++pos;

    if (pos != symbolCount) {
      ++symbolBuffer;

      *symbolBuffer = ((*packedBuffer) & 192) >> 6;  // Bits 7-8 into 1-2
      ++packedBuffer;

      *symbolBuffer |= ((*packedBuffer) & 15) << 2;  // Bits 1-4 into 3-6
      ++pos;

      if (pos != symbolCount) {
        ++symbolBuffer;
        *symbolBuffer = ((*packedBuffer) & 240) >> 4;  // Bits 5-8 into 1-4
        ++packedBuffer;

        *symbolBuffer |= ((*packedBuffer) & 3) << 4;  // Bits 1-2 into 5-6
      }
    }
  }
}

inline void BytePacker::pack8(unsigned char *dest, const unsigned *symbolBuffer,
                              size_t symbolCount) {
  for (size_t i = 0; i != symbolCount; ++i) dest[i] = symbolBuffer[i];
}

inline void BytePacker::unpack8(unsigned *symbolBuffer,
                                unsigned char *packedBuffer,
                                size_t symbolCount) {
  for (size_t i = 0; i != symbolCount; ++i) symbolBuffer[i] = packedBuffer[i];
}

inline void BytePacker::pack10(unsigned char *dest,
                               const unsigned int *symbolBuffer,
                               size_t symbolCount) {
  const size_t limit = symbolCount / 4;
  for (size_t i = 0; i != limit; i++) {
    *dest = (*symbolBuffer & 0x0FF);  // Bit 1-8 into 1-8
    ++dest;
    *dest = (*symbolBuffer & 0x300) >> 8;  // Bits 9-10 into 1-2
    ++symbolBuffer;
    *dest |= (*symbolBuffer & 0x03F) << 2;  // Bits 1-6 into 3-8
    ++dest;
    *dest = (*symbolBuffer & 0x3C0) >> 6;  // Bits 7-10 into 1-4
    ++symbolBuffer;

    *dest |= (*symbolBuffer & 0x00F) << 4;  // Bits 1-4 into 5-8
    ++dest;
    *dest = (*symbolBuffer & 0x3F0) >> 4;  // Bits 5-10 into bits 1-6
    ++symbolBuffer;
    *dest |= (*symbolBuffer & 0x003) << 6;  // Bits 1-2 into 7-8
    ++dest;
    *dest = (*symbolBuffer & 0x3FC) >> 2;  // Bits 3-10 into bits 1-8
    ++symbolBuffer;
    ++dest;
  }

  size_t pos = limit * 4;
  if (pos != symbolCount) {
    *dest = (*symbolBuffer & 0x0FF);  // Bit 1-8 into 1-8
    ++dest;
    *dest = (*symbolBuffer & 0x300) >> 8;  // Bits 9-10 into 1-2
    ++pos;

    if (pos != symbolCount) {
      ++symbolBuffer;

      *dest |= (*symbolBuffer & 0x03F) << 2;  // Bits 1-6 into 3-8
      ++dest;
      *dest = (*symbolBuffer & 0x3C0) >> 6;  // Bits 7-10 into 1-4
      ++pos;

      if (pos != symbolCount) {
        ++symbolBuffer;

        *dest |= (*symbolBuffer & 0x00F) << 4;  // Bits 1-4 into 5-8
        ++dest;
        *dest = (*symbolBuffer & 0x3F0) >> 4;  // Bits 5-10 into bits 1-6
                                               //++symbolBuffer; ++pos;
      }
    }
  }
}

inline void BytePacker::unpack10(unsigned int *symbolBuffer,
                                 unsigned char *packedBuffer,
                                 size_t symbolCount) {
  const size_t limit = symbolCount / 4;
  for (size_t i = 0; i != limit; i++) {
    *symbolBuffer = *packedBuffer;  // Bits 1-8 into 1-8
    ++packedBuffer;
    *symbolBuffer |= ((*packedBuffer) & 0x03) << 8;  // Bits 1-2 into 9-10
    ++symbolBuffer;

    *symbolBuffer = ((*packedBuffer) & 0xFC) >> 2;  // Bits 3-8 into 1-6
    ++packedBuffer;
    *symbolBuffer |= ((*packedBuffer) & 0x0F) << 6;  // Bits 1-4 into 7-10
    ++symbolBuffer;

    *symbolBuffer = ((*packedBuffer) & 0xF0) >> 4;  // Bits 5-8 into 1-4
    ++packedBuffer;
    *symbolBuffer |= ((*packedBuffer) & 0x3F) << 4;  // Bits 1-6 into 5-10
    ++symbolBuffer;

    *symbolBuffer = ((*packedBuffer) & 0xC0) >> 6;  // Bits 7-8 into 1-2
    ++packedBuffer;
    *symbolBuffer |= (*packedBuffer) << 2;  // Bits 1-8 into 3-10
    ++packedBuffer;
    ++symbolBuffer;
  }
  size_t pos = limit * 4;
  if (pos != symbolCount) {
    *symbolBuffer = *packedBuffer;  // Bits 1-8 into 1-8
    ++packedBuffer;
    *symbolBuffer |= ((*packedBuffer) & 0x03) << 8;  // Bits 1-2 into 9-10
    ++pos;

    if (pos != symbolCount) {
      ++symbolBuffer;

      *symbolBuffer = ((*packedBuffer) & 0xFC) >> 2;  // Bits 3-8 into 1-6
      ++packedBuffer;
      *symbolBuffer |= ((*packedBuffer) & 0x0F) << 6;  // Bits 1-4 into 7-10
      ++pos;

      if (pos != symbolCount) {
        ++symbolBuffer;
        *symbolBuffer = ((*packedBuffer) & 0xF0) >> 4;  // Bits 5-8 into 1-4
        ++packedBuffer;
        *symbolBuffer |= ((*packedBuffer) & 0x3F) << 4;  // Bits 1-6 into 5-10
      }
    }
  }
}

inline void BytePacker::pack12(unsigned char *dest,
                               const unsigned int *symbolBuffer,
                               size_t symbolCount) {
  const size_t limit = symbolCount / 2;
  for (size_t i = 0; i != limit; i++) {
    *dest = (*symbolBuffer) & 0x0FF;  // bits 1-8 into 1-8
    ++dest;

    *dest = ((*symbolBuffer) & 0xF00) >> 8;  // bits 9-12 into 1-4
    ++symbolBuffer;
    *dest |= ((*symbolBuffer) & 0x00F) << 4;  // bits 1-4 into 5-8
    ++dest;

    *dest = ((*symbolBuffer) & 0xFF0) >> 4;  // bits 5-12 into 1-8
    ++symbolBuffer;
    ++dest;
  }
  if (limit * 2 != symbolCount) {
    *dest = (*symbolBuffer) & 0x0FF;  // bits 1-8 into 1-8
    ++dest;

    *dest = ((*symbolBuffer) & 0xF00) >> 8;  // bits 9-12 into 1-4
  }
}

inline void BytePacker::unpack12(unsigned int *symbolBuffer,
                                 unsigned char *packedBuffer,
                                 size_t symbolCount) {
  const size_t limit = symbolCount / 2;
  for (size_t i = 0; i != limit; i++) {
    *symbolBuffer = *packedBuffer;  // bits 1-8 into 1-8
    ++packedBuffer;
    *symbolBuffer |= ((*packedBuffer) & 0x0F) << 8;  // bits 1-4 into 9-12
    ++symbolBuffer;

    *symbolBuffer = ((*packedBuffer) & 0xF0) >> 4;  // bits 5-8 into 1-4
    ++packedBuffer;
    *symbolBuffer |= ((*packedBuffer) & 0xFF) << 4;  // bits 1-8 into 5-12
    ++packedBuffer;
    ++symbolBuffer;
  }
  if (limit * 2 != symbolCount) {
    *symbolBuffer = *packedBuffer;  // bits 1-8 into 1-8
    ++packedBuffer;
    *symbolBuffer |= ((*packedBuffer) & 0x0F) << 8;  // bits 1-4 into 9-12
  }
}

inline void BytePacker::pack16(unsigned char *dest,
                               const unsigned *symbolBuffer,
                               size_t symbolCount) {
  for (size_t i = 0; i != symbolCount; ++i)
    reinterpret_cast<uint16_t *>(dest)[i] = symbolBuffer[i];
}

inline void BytePacker::unpack16(unsigned *symbolBuffer,
                                 unsigned char *packedBuffer,
                                 size_t symbolCount) {
  for (size_t i = 0; i != symbolCount; ++i)
    symbolBuffer[i] = reinterpret_cast<uint16_t *>(packedBuffer)[i];
}

}  // namespace dyscostman

#endif
