#ifndef CASACORE_TABLES_BITPACKING_H_
#define CASACORE_TABLES_BITPACKING_H_

#include <cstring>

inline void PackBoolArray(unsigned char* packed_buffer, const bool* input,
                          size_t n) {
  const size_t limit = n / 8;
  const bool* end = input + n;
  for (size_t i = 0; i != limit; ++i) {
    *packed_buffer = 0;
    for (size_t b = 0; b != 8; ++b) {
      *packed_buffer |= (*input) << b;
      ++input;
    }
    ++packed_buffer;
  }
  size_t b = 0;
  if (input != end) {
    *packed_buffer = 0;
    do {
      *packed_buffer |= (*input) << b;
      ++input;
      ++b;
    } while (input != end);
  }
}

inline void UnpackBoolArray(bool* output, const unsigned char* packed_input,
                            size_t n) {
  bool* end = output + n;
  const size_t limit = n / 8;
  for (size_t i = 0; i != limit; i++) {
    for (size_t b = 0; b != 8; ++b) {
      *output = (*packed_input >> b) & 0x1;
      ++output;
    }
    ++packed_input;
  }
  size_t b = 0;
  while (output != end) {
    *output = (*packed_input >> b) & 0x1;
    ++output;
    ++b;
  }
}

#endif
