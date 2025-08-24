#ifndef SISCO_SISCO_READER_H_
#define SISCO_SISCO_READER_H_

#include <complex>
#include <fstream>
#include <map>
#include <span>
#include <string>

#include "Deflate.h"
#include "Sisco.h"

/**
 * File interface for data stored in the generated model compression (Sisco)
 * format.
 */
class SiscoReader {
 public:
  SiscoReader(const std::string& filename);
  SiscoReader(SiscoReader&&) = default;
  ~SiscoReader() = default;
  SiscoReader& operator=(SiscoReader&&) = default;

  void Open(std::span<std::byte> header_data);
  void Read(size_t baseline_index, std::span<std::complex<float>> data);

 private:
  int predict_level_;
  std::vector<std::byte> read_buffer_;
  std::vector<std::byte> chunk_data_;
  size_t chunk_item_position_ = 0;
  std::vector<float> real_data_;
  std::vector<float> imaginary_data_;

  struct BaselineData {
    CompressorState real_state_;
    CompressorState imaginary_state_;
  };

  // Returns the decompressed size
  size_t ReadChunk(std::vector<std::byte>& buffer);

  // Indexed by baseline_index.
  std::map<size_t, BaselineData> baseline_data_;
  std::string filename_;
  std::ifstream file_;
  deflate::Decompressor deflate_decompressor;
};

#endif
