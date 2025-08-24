#include "SiscoReader.h"

#include <cassert>

SiscoReader::SiscoReader(const std::string& filename) : filename_(filename)
{
}

void SiscoReader::Open(std::span<std::byte> header_data)
{
  file_ = std::ifstream(filename_);
  if(!file_.good())
    throw std::runtime_error("Could not open file " + filename_);
  
  file_.read(reinterpret_cast<char*>(header_data.data()), header_data.size());
  
  signed char predict_level_char;
  file_.read(reinterpret_cast<char*>(&predict_level_char), 1);
  predict_level_ = predict_level_char;
  
  if(!file_.good())
    throw std::runtime_error("Failed to read header from " + filename_);
}

void SiscoReader::Read(size_t baseline_index, std::span<std::complex<float>> data)
{
  assert(file_.is_open());
  if(chunk_item_position_ * 2 * (kCompressedMantissaSize + kCompressedExponentSize) >= chunk_data_.size())
  {
    const size_t decompressed_size = ReadChunk(read_buffer_);
    chunk_data_.resize(decompressed_size);
    deflate_decompressor.Decompress(read_buffer_, chunk_data_);
    chunk_item_position_ = 0;
  }
  real_data_.resize(data.size());
  imaginary_data_.resize(data.size());

  BaselineData& baseline = baseline_data_[baseline_index];
  const size_t exponents_start = (chunk_data_.size() / (kCompressedMantissaSize + kCompressedExponentSize)) * kCompressedMantissaSize;

  const size_t mantissas_offset = chunk_item_position_ * kCompressedMantissaSize * 2;
  const size_t mantissas_size = kCompressedMantissaSize  * data.size();
  const size_t exponents_offset = exponents_start + chunk_item_position_ * kCompressedExponentSize * 2;
  const size_t exponents_size = kCompressedExponentSize  * data.size();
  std::span real_mantissas(&chunk_data_[mantissas_offset], mantissas_size);
  std::span real_exponents(&chunk_data_[exponents_offset], exponents_size);
  Decompress2D(predict_level_, baseline.real_state_, real_mantissas, real_exponents, real_data_);

  std::span imaginary_mantissas(&chunk_data_[mantissas_offset + mantissas_size], mantissas_size);
  std::span imaginary_exponents(&chunk_data_[exponents_offset + exponents_size], exponents_size);
  Decompress2D(predict_level_, baseline.imaginary_state_, imaginary_mantissas, imaginary_exponents, imaginary_data_);
  for(size_t i=0; i!=data.size(); ++i) {
    data[i] = std::complex<float>(real_data_[i], imaginary_data_[i]);
  }

  chunk_item_position_ += data.size();
}

size_t SiscoReader::ReadChunk(std::vector<std::byte>& buffer)
{
  uint64_t decompressed_size;
  uint64_t compressed_size;
  file_.read(reinterpret_cast<char*>(&decompressed_size), sizeof(decompressed_size));
  file_.read(reinterpret_cast<char*>(&compressed_size), sizeof(compressed_size));
  buffer.resize(compressed_size);
  file_.read(reinterpret_cast<char*>(buffer.data()), compressed_size);
  if(!file_.good())
    throw std::runtime_error("Could not read from file " + filename_);
  return decompressed_size;
}
