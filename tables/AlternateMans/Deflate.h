#ifndef SISCO_DEFLATE_H_
#define SISCO_DEFLATE_H_

#include "libdeflate.h"

#include <cstddef>
#include <span>
#include <stdexcept>

namespace casacore::deflate {

class Compressor {
 public:
  Compressor(int compression_level)
      : compressor_(libdeflate_alloc_compressor(compression_level)) {
    if (!compressor_)
      throw std::runtime_error(
          "Could not create compressor: bad settings or out of memory");
  }

  Compressor(int compression_level, const libdeflate_options& options)
      : compressor_(
            libdeflate_alloc_compressor_ex(compression_level, &options)) {
    if (!compressor_)
      throw std::runtime_error(
          "Could not create compressor: bad settings or out of memory");
  }

  ~Compressor() { libdeflate_free_compressor(compressor_); }

  /**
   * Compresses the input into output. If the compressed data does not fit into
   * output, a value of zero is returned. Otherwise, the number of output bytes
   * written is returned.
   */
  size_t Compress(std::span<const std::byte> input,
                  std::span<std::byte> output) {
    return libdeflate_deflate_compress(compressor_, input.data(), input.size(),
                                       output.data(), output.size());
  }

  /**
   * Worst-case upper bound on the number of bytes of compressed data that may
   * be produced by compressing any buffer of length less than or equal to @p
   * input_size.
   */
  size_t CompressBound(size_t input_size) {
    return libdeflate_deflate_compress_bound(compressor_, input_size);
  }

 private:
  struct libdeflate_compressor* compressor_;
};

class Decompressor {
 public:
  Decompressor() : decompressor_(libdeflate_alloc_decompressor()) {
    if (!decompressor_)
      throw std::runtime_error(
          "Could not create decompressor: bad settings or out of memory");
  }

  Decompressor(const libdeflate_options& options)
      : decompressor_(libdeflate_alloc_decompressor_ex(&options)) {
    if (!decompressor_)
      throw std::runtime_error(
          "Could not create decompressor: bad settings or out of memory");
  }

  ~Decompressor() { libdeflate_free_decompressor(decompressor_); }

  size_t Decompress(std::span<const std::byte> input,
                    std::span<std::byte> output) {
    size_t actual_out_nbytes_ret;
    const libdeflate_result result = libdeflate_deflate_decompress(
        decompressor_, input.data(), input.size(), output.data(), output.size(),
        &actual_out_nbytes_ret);
    switch (result) {
      case LIBDEFLATE_SUCCESS:
        break;
      case LIBDEFLATE_BAD_DATA:
        throw std::runtime_error("Could not decompress data: bad data");
      case LIBDEFLATE_SHORT_OUTPUT:
        throw std::runtime_error(
            "Could not decompress data: data shorter than expected");
      case LIBDEFLATE_INSUFFICIENT_SPACE:
        throw std::runtime_error(
            "Could not decompress data: insufficient space in output buffer");
    }
    return actual_out_nbytes_ret;
  }

 private:
  struct libdeflate_decompressor* decompressor_;
};

}  // namespace casacore::deflate

#endif
