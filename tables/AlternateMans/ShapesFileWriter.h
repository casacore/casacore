#ifndef CASACORE_SHAPES_FILE_WRITER_H_
#define CASACORE_SHAPES_FILE_WRITER_H_

#include <fstream>

#include <casacore/casa/Arrays/IPosition.h>

#include "Deflate.h"

namespace casacore {

class ShapesFileWriter {
 public:
  ShapesFileWriter(const std::string& filename)
      : compressor_(9), file_(filename) {}

  ~ShapesFileWriter() {
    if (!buffer_.empty()) Flush();
  }

  void Write(const IPosition& position) {
    buffer_.emplace_back(position.size());
    if (buffer_.size() == kMaxBufferSize) Flush();
    for (ssize_t value : position) {
      buffer_.emplace_back(value);
      if (buffer_.size() == kMaxBufferSize) Flush();
    }
  }

 private:
  void Flush() {
    const uint32_t uncompressed_size = buffer_.size() * sizeof(uint64_t);
    compressed_buffer_.resize(compressor_.CompressBound(uncompressed_size));
    std::span input(reinterpret_cast<const std::byte*>(buffer_.data()),
                    uncompressed_size);
    const uint32_t compressed_size =
        compressor_.Compress(input, compressed_buffer_);
    file_.write(reinterpret_cast<const char*>(&uncompressed_size),
                sizeof(uint32_t));
    file_.write(reinterpret_cast<const char*>(&compressed_size),
                sizeof(uint32_t));
    file_.write(reinterpret_cast<const char*>(compressed_buffer_.data()),
                compressed_size);
    buffer_.clear();
  }

  static constexpr size_t kMaxBufferSize = 1024 * 128;
  std::vector<uint64_t> buffer_;
  std::vector<std::byte> compressed_buffer_;
  deflate::Compressor compressor_;
  std::ofstream file_;
};

}  // namespace casacore

#endif
