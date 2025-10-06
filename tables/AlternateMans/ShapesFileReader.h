#ifndef CASACORE_SHAPES_FILE_READER_H_
#define CASACORE_SHAPES_FILE_READER_H_

#include <fstream>

#include <casacore/casa/Arrays/IPosition.h>

#include "Deflate.h"

namespace casacore {

class ShapesFileReader {
 public:
  ShapesFileReader(const std::string& filename)
      : buffer_position_(buffer_.end()), file_(filename) {
    if (file_.fail()) throw std::runtime_error("Error opening shapes file");
  }

  IPosition Read() {
    if (buffer_position_ == buffer_.end()) FillCache();
    size_t size = *buffer_position_;
    ++buffer_position_;
    IPosition result(size);
    for (ssize_t& value : result) {
      if (buffer_position_ == buffer_.end()) FillCache();
      value = *buffer_position_;
      ++buffer_position_;
    }
    return result;
  }

  bool Eof() const { return file_.eof(); }

 private:
  void FillCache() {
    uint32_t uncompressed_size;
    if (!file_.eof()) {
      file_.read(reinterpret_cast<char*>(&uncompressed_size), sizeof(uint32_t));
    }
    if (file_.eof()) {
      buffer_.assign(1, 0);
    } else {
      uint32_t compressed_size;
      file_.read(reinterpret_cast<char*>(&compressed_size), sizeof(uint32_t));
      compressed_buffer_.resize(compressed_size);
      file_.read(reinterpret_cast<char*>(compressed_buffer_.data()),
                 compressed_size);
      buffer_.resize(uncompressed_size / sizeof(uint64_t));
      std::span output(reinterpret_cast<std::byte*>(buffer_.data()),
                       uncompressed_size);
      decompressor_.Decompress(compressed_buffer_, output);
      if (file_.fail())
        throw std::runtime_error("Error reading from shapes file");
    }
    buffer_position_ = buffer_.begin();
  }

  std::vector<uint64_t> buffer_;
  std::vector<uint64_t>::const_iterator buffer_position_;
  std::vector<std::byte> compressed_buffer_;
  deflate::Decompressor decompressor_;
  std::ifstream file_;
};

}  // namespace casacore

#endif
