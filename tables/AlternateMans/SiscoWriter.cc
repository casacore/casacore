#include "SiscoWriter.h"

#include <cassert>
#include <set>
#include <thread>
#include <vector>

namespace {
/// Concatenate mantissas and exponents into one buffer
void Concatenate(std::vector<std::byte>& destination, const std::vector<std::byte>& mantissas, const std::vector<std::byte>& exponents, size_t n_elements) {
  destination.reserve(n_elements * 2 * (kCompressedMantissaSize + kCompressedExponentSize));
  destination.assign(mantissas.begin(), mantissas.begin() + n_elements * 2 * kCompressedMantissaSize);
  destination.insert(destination.end(), exponents.begin(), exponents.begin() + n_elements * 2 * kCompressedExponentSize);
}
} // namespace

SiscoWriter::SiscoWriter(const std::string& filename, int predict_level, int deflate_level) :
  filename_(filename), predict_level_(predict_level), deflate_level_(deflate_level)
{
}

void SiscoWriter::Open(std::span<const std::byte> header_data) {
  file_ = std::ofstream(filename_, std::ios::trunc | std::ios::binary | std::ios::in | std::ios::out);
  if(!file_.good())
    throw std::runtime_error("Could not open file '" + filename_ + "'");
  file_.write(reinterpret_cast<const char*>(header_data.data()), header_data.size());
  
  signed char predict_level_char = predict_level_;
  file_.write(reinterpret_cast<const char*>(&predict_level_char), 1);
  preprocess_thread_ = std::thread([&](){ PreprocessLoop(); });
  for(std::thread& worker : deflate_threads_)
    worker = std::thread([&]() { DeflateWorker(); });
  write_thread_ = std::thread([&](){ WriteLoop(); });
}

void SiscoWriter::Close() {
  preprocessing_tasks_.write_end();
  preprocess_thread_.join();
  for(std::thread& worker : deflate_threads_)
    worker.join();
  write_tasks_.write_end();
  write_thread_.join();
  file_.close();
}

void SiscoWriter::Write(size_t baseline_index, std::span<const std::complex<float>> data)
{
  assert(file_.is_open());
  CompressionTask new_task;
  new_task.real_data.resize(data.size());
  new_task.imaginary_data.resize(data.size());
  for(size_t i=0; i!=data.size(); ++i) {
    new_task.real_data[i] = data[i].real();
    new_task.imaginary_data[i] = data[i].imag();
  }
  new_task.baseline_index = baseline_index;
  preprocessing_tasks_.write(std::move(new_task));
}

void SiscoWriter::PreprocessLoop() {
  CompressionTask task;
  size_t n_chunks = 0;
  size_t chunk_position = 0;
  size_t chunk_size = kDefaultChunkSize;
  // Times 2 because it is complex data
  std::vector<std::byte> mantisas_data(chunk_size * 2 * kCompressedMantissaSize);
  std::byte* mantisa_position = mantisas_data.data();
  std::vector<std::byte> exponent_data(chunk_size * 2 * kCompressedExponentSize);
  std::byte* exponent_position = exponent_data.data();

  while(preprocessing_tasks_.read(task)) {
    BaselineData& baseline = baseline_data_[task.baseline_index];
    const size_t row_size = task.real_data.size();

    // A chunk should contain at least one row, so if this row is
    // larger than the chunk size, enlarge chunk.
    if(row_size > chunk_size) [[unlikely]] {
      chunk_size = row_size;
      // resize operation may invalidate pointers, so store offsets...
      const size_t m_offset = mantisa_position - mantisas_data.data();
      const size_t e_offset = exponent_position - exponent_data.data();
      mantisas_data.resize(chunk_size * 2 * kCompressedMantissaSize);
      exponent_data.resize(chunk_size * 2 * kCompressedExponentSize);
      mantisa_position = mantisas_data.data() + m_offset;
      exponent_position = exponent_data.data() + e_offset;
    }

    // Is chunk full?
    if(chunk_position + row_size > chunk_size) {
      DeflateTask deflate_task;
      deflate_task.index = n_chunks;
      Concatenate(deflate_task.data, mantisas_data, exponent_data, chunk_position);
      deflate_tasks_.write(std::move(deflate_task));
      mantisa_position = mantisas_data.data();
      exponent_position = exponent_data.data();
      chunk_position = 0;
      ++n_chunks;
    }

    chunk_position += row_size;
    const size_t mantissa_row_size = row_size * kCompressedMantissaSize;
    const size_t exponent_row_size = row_size * kCompressedExponentSize;

    std::span real_mantissa(mantisa_position, mantissa_row_size);
    std::span real_exponent(exponent_position, exponent_row_size);
    Compress2D(predict_level_, baseline.real_state_, task.real_data, real_mantissa, real_exponent);
    mantisa_position += mantissa_row_size;
    exponent_position += exponent_row_size;

    std::span imaginary_mantissa(mantisa_position, mantissa_row_size);
    std::span imaginary_exponent(exponent_position, exponent_row_size);
    Compress2D(predict_level_, baseline.imaginary_state_, task.imaginary_data, imaginary_mantissa, imaginary_exponent);
    mantisa_position += mantissa_row_size;
    exponent_position += exponent_row_size;
  }

  if(chunk_position != 0) {
    DeflateTask deflate_task;
    deflate_task.index = n_chunks;
    Concatenate(deflate_task.data, mantisas_data, exponent_data, chunk_position);
    deflate_tasks_.write(std::move(deflate_task));
  }

  deflate_tasks_.write_end();
}

void SiscoWriter::DeflateWorker() {
  DeflateTask task;
  deflate::Compressor deflate_compressor{deflate_level_};
  while(deflate_tasks_.read(task)) {
    const size_t size = task.data.size();
    const size_t max_size = deflate_compressor.CompressBound(size);
    std::vector<std::byte> output_buffer(max_size);
    const size_t compressed_size = deflate_compressor.Compress(task.data, output_buffer);
    output_buffer.resize(compressed_size);
    WriteTask write_task;
    write_task.index = task.index;
    write_task.uncompressed_size = size;
    write_task.data = std::move(output_buffer);
    write_tasks_.write(std::move(write_task));
  }
}

void SiscoWriter::WriteLoop()
{
  auto Compare = [](const WriteTask& lhs, const WriteTask& rhs){ return lhs.index < rhs.index; };
  std::set<WriteTask, decltype(Compare)> scheduled_tasks;
  WriteTask task;
  size_t sequence_index = 0;
  while(write_tasks_.read(task)) {
    if(task.index != sequence_index) {
      scheduled_tasks.insert(std::move(task));
      continue;
    }

    WriteChunk(task.uncompressed_size, task.data);
    ++sequence_index;

    while(!scheduled_tasks.empty() && scheduled_tasks.begin()->index == sequence_index) {
      task = std::move(scheduled_tasks.extract(scheduled_tasks.begin()).value());

      WriteChunk(task.uncompressed_size, task.data);
      ++sequence_index;
    }
  }
}

void SiscoWriter::WriteChunk(uint64_t uncompressed_size, std::span<const std::byte> data) {
  file_.write(reinterpret_cast<const char*>(&uncompressed_size), sizeof(uncompressed_size));
  const uint64_t compressed_size = data.size();
  file_.write(reinterpret_cast<const char*>(&compressed_size), sizeof(compressed_size));
  file_.write(reinterpret_cast<const char*>(data.data()), data.size());
}
