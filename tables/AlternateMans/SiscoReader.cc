#include "SiscoReader.h"

#include <cassert>

namespace casacore::sisco {
namespace {
void Ungroup(const std::byte* values, size_t n_values, std::byte* output) {
  for(size_t bytepos=0; bytepos!=4; ++bytepos) {
    const std::byte* input_start = &values[bytepos * n_values];
    for(size_t i=0; i!=n_values; ++i) {
      output[i*4+bytepos] = input_start[i];
    }
  }
}

void UngroupWithoutPrediction(const std::byte* values, size_t n_values, std::byte* output) {
  for(size_t bytepos=0; bytepos!=2; ++bytepos) {
    const std::byte* input_start = &values[bytepos * n_values];
    for(size_t i=0; i!=n_values; ++i) {
      output[i*4+bytepos] = input_start[i];
    }
  }
  const std::byte* mantissa2_start = &values[2 * n_values];
  for(size_t i=0; i!=n_values; ++i) {
    // The 24th bit of the input contains the sign.
    // The 24th bit of the mantissa is expected to be set by Bitfloat class,
    // and the 32th bit is expected to hold the sign.
    output[i*4+2] = mantissa2_start[i] | std::byte(0x80);
    output[i*4+3] = mantissa2_start[i] & std::byte(0x80);
  }
}

void CopyResult(const std::vector<float>& real, const std::vector<float>& imaginary, std::span<std::complex<float>> data) {
  assert(data.size() == real.size());
  assert(data.size() == imaginary.size());
  for(size_t i=0; i!=data.size(); ++i) {
    data[i] = std::complex<float>(real[i], imaginary[i]);
  }
}

} // namespace  

SiscoReader::SiscoReader(const std::string& filename) : filename_(filename)
{
}

SiscoReader::~SiscoReader() {
  if(open_)
    Close();
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
  
  const size_t n_threads = std::max<size_t>(2, DefaultThreadCount()) - 1;
  // See comment inside ResultLoop()  about lane size.
  decompress_lane_.resize(n_threads*2);
  
  read_thread_ = std::thread([&](){ ReadLoop(); });
  for(size_t i=0; i!=n_threads; ++i)
    result_workers_.emplace_back([&](){ ResultLoop(); });
  
  // Make the first chunk available
  results_in_chunk_counter_ = 0;
  DecompressChunk();
  current_chunk_ = &chunks_.emplace_back();
  GetNextChunk(*current_chunk_);
  chunk_item_position_ = 0;
  
  open_ = true;
}

void SiscoReader::Close() {
  {
    std::unique_lock lock(mutex_);
    request_queue_.Finish(lock);
  }
  read_lane_.write_end();
  read_thread_.join();
  result_lane_.write_end();

  for(std::thread& worker: result_workers_) {
    worker.join();
  }
  open_ = false;
}

void SiscoReader::ReadLoop() {
  size_t chunk_index = 0;
  assert(file_.is_open());
  Chunk chunk;
  chunk.decompressed_size = ReadChunk(chunk.read_buffer);
  while(chunk.decompressed_size != 0 && !read_lane_.is_end()) {
    chunk.chunk_index = chunk_index;
    read_lane_.write(std::move(chunk));
    
    ++chunk_index;
    chunk.decompressed_size = ReadChunk(chunk.read_buffer);
  }
  read_lane_.write_end();
}

void SiscoReader::DecompressChunk() {
  const size_t stored_mantissa_size = (predict_level_ == -1) ? 3 : kCompressedMantissaSize;
  Chunk chunk;
  deflate::Decompressor deflate_decompressor;
  if(read_lane_.read(chunk)) {
    chunk.decompress_buffer.resize(chunk.decompressed_size);
    deflate_decompressor.Decompress(chunk.read_buffer, chunk.decompress_buffer);
    
    const size_t n_values = chunk.decompress_buffer.size() / (stored_mantissa_size + kCompressedExponentSize);
    // The read buffer is reused now to store the ungrouped data.
    chunk.read_buffer.resize(n_values * kCompressedMantissaSize);
    if(predict_level_ == -1) {
      UngroupWithoutPrediction(chunk.decompress_buffer.data(), n_values, chunk.read_buffer.data());
    } else {
      Ungroup(chunk.decompress_buffer.data(), n_values, chunk.read_buffer.data());
    }
    
    decompress_lane_.write(std::move(chunk));
  }
}

void SiscoReader::GetNextChunk(Chunk& chunk) {
  if(!decompressed_queue.empty() && decompressed_queue.begin()->chunk_index == chunk_sequence) {
    chunk = std::move(decompressed_queue.extract(decompressed_queue.begin()).value());
  } else {
    decompress_lane_.read(chunk);
    while(chunk.chunk_index != chunk_sequence) {
      decompressed_queue.insert(std::move(chunk));
      decompress_lane_.read(chunk);
    }
  }
  ++chunk_sequence;
}

void SiscoReader::Request(size_t baseline_index, size_t n_values) {
  const size_t stored_mantissa_size = (predict_level_ == -1) ? 3 : kCompressedMantissaSize;
  if(chunk_item_position_ * 2 * (stored_mantissa_size + kCompressedExponentSize) >= current_chunk_->decompress_buffer.size())
  {
    // Sent a special request with zero size to make sure chunk gets cleaned.
    std::unique_lock lock(mutex_);
    current_chunk_->n_results_in_chunk = results_in_chunk_counter_ + 1;
    RequestData request;
    request.chunk = current_chunk_;
    request.n_values = 0;
    request_queue_.Push(std::move(request), lock);
    lock.unlock();
    
    results_in_chunk_counter_ = 0;
    current_chunk_ = &chunks_.emplace_back();
    GetNextChunk(*current_chunk_);
    chunk_item_position_ = 0;
  }
    
  const size_t exponents_start = (current_chunk_->decompress_buffer.size() / (stored_mantissa_size + kCompressedExponentSize)) * stored_mantissa_size;

  RequestData request;
  request.mantissas_offset = chunk_item_position_ * kCompressedMantissaSize * 2;
  request.exponents_offset = exponents_start + chunk_item_position_ * kCompressedExponentSize * 2;
  request.baseline_index = baseline_index;
  request.n_values = n_values;
  request.index = result_counter_;
  request.chunk = current_chunk_;
  ++result_counter_;
  chunk_item_position_ += n_values;
  results_in_chunk_counter_ ++;
    
  std::unique_lock lock(mutex_);
  request_queue_.Push(std::move(request), lock);
}

void SiscoReader::ResultLoop() {
  RequestData request;
  const auto condition = [this](const RequestData& request) {
    return request.n_values == 0 || !busy_baselines_.contains(request.baseline_index);
  };
  
  std::unique_lock lock(mutex_);
  while(request_queue_.PopIf(request, lock, condition)) {
    
    if(request.n_values != 0) {
      assert(!busy_baselines_.contains(request.baseline_index));
      busy_baselines_.insert(request.baseline_index);
      BaselineData& baseline = baseline_data_[request.baseline_index];
      lock.unlock();
      
      Result result;
      result.real_data.resize(request.n_values);
      result.imaginary_data.resize(request.n_values);

      const size_t mantissas_size = kCompressedMantissaSize * request.n_values;
      const size_t exponents_size = kCompressedExponentSize * request.n_values;
      
      std::span real_mantissas(&request.chunk->read_buffer[request.mantissas_offset], mantissas_size);
      std::span real_exponents(&request.chunk->decompress_buffer[request.exponents_offset], exponents_size);
      Decompress2D(predict_level_, baseline.real_state_, real_mantissas, real_exponents, result.real_data);
      
      std::span imaginary_mantissas(&request.chunk->read_buffer[request.mantissas_offset + mantissas_size], mantissas_size);
      std::span imaginary_exponents(&request.chunk->decompress_buffer[request.exponents_offset + exponents_size], exponents_size);
      Decompress2D(predict_level_, baseline.imaginary_state_, imaginary_mantissas, imaginary_exponents, result.imaginary_data);
      
      result.result_index = request.index;
      result_lane_.write(std::move(result));
      
      // We do this before the lock, because size() has its own lock, and holding two
      // locks make deadlocking easier.
      const size_t decompression_lane_size = decompress_lane_.size();
      lock.lock();
      busy_baselines_.erase(request.baseline_index);
      
      // If the nr of decompressed chunks gets low, use this thread to decompress a chunk. This way, threads
      // are balanced between decompression and unpacking. This condition isn't "atomic", i.e. all threads
      // may simultaneously find that decompression is required. In that case, the call will block and
      // threads may become idle for some time; hence there is some balance required between filling the
      // queue, the nr of threads and the size of the lane.
      if(decompression_lane_size < (decompress_lane_.capacity() + 3) / 4) {
        lock.unlock();
        DecompressChunk();
        lock.lock();
      }
    }
    
    // The following needs to happen 'atomically'; otherwise the chunk gets removed and another thread
    // may still access the chunk. This is achieved by doing it while holding the lock.
    request.chunk->n_results_processed++;
    // n_results_in_chunk may still be zero when not all chunks are processed, but
    // n_results_processed will never be zero so it will never delete too early
    if(request.chunk->n_results_processed == request.chunk->n_results_in_chunk) {
      const Chunk* chunk = request.chunk;
      [[maybe_unused]] const bool is_removed = chunks_.remove_if([chunk](const Chunk& value) { return &value == chunk; });
      assert(is_removed);
    }
  }
}

void SiscoReader::GetNextResult(std::span<std::complex<float>> data) {
  if(!result_queue.empty() && result_queue.begin()->result_index == result_sequence_) {
    CopyResult(result_queue.begin()->real_data, result_queue.begin()->imaginary_data, data);
    result_queue.erase(result_queue.begin());
  }
  else {  
    Result result;
    result_lane_.read(result);
    
    while(result.result_index != result_sequence_) {
      result_queue.emplace(std::move(result));
      result_lane_.read(result);
    }
    CopyResult(result.real_data, result.imaginary_data, data);
  }
  ++result_sequence_;
}

size_t SiscoReader::ReadChunk(std::vector<std::byte>& buffer)
{
  uint64_t decompressed_size;
  uint64_t compressed_size;
  file_.read(reinterpret_cast<char*>(&decompressed_size), sizeof(decompressed_size));
  if(file_.eof())
    return 0;
  file_.read(reinterpret_cast<char*>(&compressed_size), sizeof(compressed_size));
  if(!file_.good())
    throw std::runtime_error("Could not read from file " + filename_);
  buffer.resize(compressed_size);
  file_.read(reinterpret_cast<char*>(buffer.data()), compressed_size);
  if(!file_.good())
    throw std::runtime_error("Could not read from file " + filename_);
  return decompressed_size;
}

} // namespace casacore::sisco
