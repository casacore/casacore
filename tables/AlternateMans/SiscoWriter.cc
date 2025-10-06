#include "SiscoWriter.h"

#include <cassert>
#include <set>
#include <thread>
#include <vector>

namespace casacore::sisco {
namespace {

constexpr size_t kMaxChunks = 32;

/// Concatenate mantissas and exponents into one buffer
void Concatenate(std::vector<std::byte>& destination, const std::vector<std::byte>& mantissas, const std::vector<std::byte>& exponents, size_t n_elements) {
  destination.reserve(n_elements * 2 * (kCompressedMantissaSize + kCompressedExponentSize));
  destination.assign(mantissas.begin(), mantissas.begin() + n_elements * 2 * kCompressedMantissaSize);
  destination.insert(destination.end(), exponents.begin(), exponents.begin() + n_elements * 2 * kCompressedExponentSize);
  
  // To test exponent compression:
  //destination.assign(exponents.begin(),exponents.begin() + n_elements * 2 * kCompressedExponentSize);

  destination.clear();
  for(size_t bytepos = 0; bytepos!=4; ++bytepos) {
    for(size_t i=0; i!=n_elements * 2; ++i) {
      destination.emplace_back(mantissas[i*4+bytepos]);
    }
  }
  destination.insert(destination.end(), exponents.begin(), exponents.begin() + n_elements * 2 * kCompressedExponentSize);
}

void ConcatenateWithoutPrediction(std::vector<std::byte>& destination, const std::vector<std::byte>& mantissas, const std::vector<std::byte>& exponents, size_t n_elements) {
  // In the special case of no prediction, we know the mantissa is limited to 23 bits. Therefore, the
  // 24th bit is used for the sign, and the leading byte is removed. Although this doesn't change the
  // information in the stored data, it has a significant effect on the compression.
  destination.clear();
  const size_t n_values = n_elements * 2;
  destination.reserve(n_values * (3 + kCompressedExponentSize));
  for(size_t bytepos = 0; bytepos!=2; ++bytepos) {
    for(size_t i=0; i!=n_values; ++i) {
      destination.emplace_back(mantissas[i*4+bytepos]);
    }
  }
  for(size_t i=0; i!=n_values; ++i) {
    std::byte value_with_sign = mantissas[i*4+2] | mantissas[i*4+3];
    if(mantissas[i*4+3] == std::byte(0))
      value_with_sign = value_with_sign & std::byte(0x7F);
    destination.emplace_back(value_with_sign);
  }
  destination.insert(destination.end(), exponents.begin(), exponents.begin() + n_values * kCompressedExponentSize);
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
  const size_t n_threads = DefaultThreadCount();

  std::unique_lock lock(mutex_);
  NewChunk(lock); // will unlock
  for(size_t i=0; i!=n_threads; ++i)
    compression_threads_.emplace_back([&](){ PreprocessLoop(); });
  write_thread_ = std::thread([&](){ WriteLoop(); });
}

void SiscoWriter::Close() {
  {
    std::unique_lock lock(mutex_);
    preprocessing_tasks_.Finish(lock);
  }
  for(std::thread& thread : compression_threads_)
    thread.join();

  // Finish the last chunk if necessary
  if(chunk_write_position_ != 0) {
    write_chunk_->chunk_size = chunk_write_position_;
    DeflateChunk(*write_chunk_);
  }

  write_tasks_.write_end();
  write_thread_.join();
  file_.close();
}

void SiscoWriter::Write(size_t baseline_index, std::span<const std::complex<float>> data)
{
  const size_t row_size = data.size();

  if(chunk_write_position_ + row_size > write_chunk_->allocated_chunk_size) {
    // A chunk should contain at least one row, so if this row is
    // larger than the chunk size, enlarge chunk.
    if(chunk_write_position_ == 0) {
      // It's fine to resize the data because the position is still zero, and thus
      // no thread has been allocated space from this chunk yet.
      write_chunk_->allocated_chunk_size = row_size;
      write_chunk_->mantisas_data.resize(write_chunk_->allocated_chunk_size * 2 * kCompressedMantissaSize);
      write_chunk_->exponent_data.resize(write_chunk_->allocated_chunk_size * 2 * kCompressedExponentSize);
      // resize operation may invalidate pointers, so refresh them
      mantisa_position_ = write_chunk_->mantisas_data.data();
      exponent_position_ = write_chunk_->exponent_data.data();
    } else {
      // Chunk is full
      // To make sure there is always a thread that finishes the chunk, a
      // special null task is sent with mantisa_position=nullptr. If the
      // chunk is ready to be finished, the receiving of that task will finish
      // the chunk. If it is not yet ready to be finished, another thread
      // will notice that the finished_chunk_size is reached and will finish it.
      PreprocessingTask null_task;
      null_task.mantisa_position = nullptr;
      null_task.exponent_position = nullptr;
      null_task.baseline_index = 0;
      null_task.chunk = write_chunk_;

      std::unique_lock lock(mutex_);
      write_chunk_->chunk_size = chunk_write_position_;
      write_chunk_->n_allocated_items = write_chunk_->item_counter + 1;
      preprocessing_tasks_.Push(std::move(null_task), lock);
      NewChunk(lock);
    }
  }
  PreprocessingTask new_task;
  new_task.mantisa_position = mantisa_position_;
  new_task.exponent_position = exponent_position_;
  new_task.chunk = write_chunk_;
  mantisa_position_ += 2 * row_size * kCompressedMantissaSize;
  exponent_position_ += 2 * row_size * kCompressedExponentSize;

  chunk_write_position_ += row_size;

  assert(file_.is_open());
  new_task.real_data.resize(data.size());
  new_task.imaginary_data.resize(data.size());
  for(size_t i=0; i!=data.size(); ++i) {
    new_task.real_data[i] = data[i].real();
    new_task.imaginary_data[i] = data[i].imag();
  }
  new_task.baseline_index = baseline_index;
  std::unique_lock lock(mutex_);
  write_chunk_->item_counter++;
  preprocessing_tasks_.Push(std::move(new_task), lock);
}

void SiscoWriter::NewChunk(std::unique_lock<std::mutex>& lock) {
  while(chunks_.size() >= kMaxChunks)
    chunk_finished_condition_.wait(lock);
  write_chunk_ = &chunks_.emplace_front();
  lock.unlock();

  // Times 2 because it is complex data
  write_chunk_->mantisas_data.resize(write_chunk_->allocated_chunk_size * 2 * kCompressedMantissaSize);
  write_chunk_->exponent_data.resize(write_chunk_->allocated_chunk_size * 2 * kCompressedExponentSize);
  write_chunk_->index = n_chunks_;
  ++n_chunks_;
  mantisa_position_ = write_chunk_->mantisas_data.data();
  exponent_position_ = write_chunk_->exponent_data.data();
  chunk_write_position_ = 0;
}

void SiscoWriter::RemoveChunk(const Chunk* chunk) {
  std::scoped_lock lock(mutex_);
  [[maybe_unused]] const bool is_removed = chunks_.remove_if([chunk](const Chunk& value) { return &value == chunk; });
  assert(is_removed);
  chunk_finished_condition_.notify_one();
}

void SiscoWriter::Preprocess(SiscoWriter::PreprocessingTask& task, std::unique_lock<std::mutex>& lock) {
  size_t row_size = 0;
  if(task.mantisa_position) {
    BaselineData& baseline = baseline_data_[task.baseline_index];
    busy_baselines_.emplace(task.baseline_index);
    lock.unlock();

    row_size = task.real_data.size();

    const size_t mantissa_row_size = row_size * kCompressedMantissaSize;
    const size_t exponent_row_size = row_size * kCompressedExponentSize;

    std::span real_mantissa(task.mantisa_position, mantissa_row_size);
    std::span real_exponent(task.exponent_position, exponent_row_size);
    Compress2D(predict_level_, baseline.real_state_, task.real_data, real_mantissa, real_exponent);

    std::byte* const mantisa_position = task.mantisa_position + mantissa_row_size;
    std::byte* const exponent_position = task.exponent_position + exponent_row_size;
    std::span imaginary_mantissa(mantisa_position, mantissa_row_size);
    std::span imaginary_exponent(exponent_position, exponent_row_size);
    Compress2D(predict_level_, baseline.imaginary_state_, task.imaginary_data, imaginary_mantissa, imaginary_exponent);

    lock.lock();
    busy_baselines_.erase(task.baseline_index);
    preprocessing_tasks_.NotifyOneChange();
  }

  // NB: Lock is and must be still owned.
  task.chunk->n_finished_items++;
  // n_allocated_items may be zero if the chunk is still being filled, but in that case
  // it can't be the last item.
  if(task.chunk->n_allocated_items == task.chunk->n_finished_items) {
    lock.unlock();
    DeflateChunk(*task.chunk);
    lock.lock();
  }
}

void SiscoWriter::PreprocessLoop() {
  PreprocessingTask task;

  std::unique_lock lock(mutex_);
  while(preprocessing_tasks_.PopIf(task, lock, [this](PreprocessingTask& task){
    const size_t baseline = task.baseline_index;
    return /* TODO !task.mantisa_position ||*/ !busy_baselines_.contains(baseline);
  })) {
    Preprocess(task, lock);
  }
}

void SiscoWriter::DeflateChunk(Chunk& chunk) {
  std::vector<std::byte> data;
  const size_t index = chunk.index;
  if(predict_level_ == -1)
    ConcatenateWithoutPrediction(data, chunk.mantisas_data, chunk.exponent_data, chunk.chunk_size);
  else
    Concatenate(data, chunk.mantisas_data, chunk.exponent_data, chunk.chunk_size);
  RemoveChunk(&chunk);

  const size_t size = data.size();
  deflate::Compressor deflate_compressor{deflate_level_};
  const size_t max_size = deflate_compressor.CompressBound(size);
  std::vector<std::byte> output_buffer(max_size);
  const size_t compressed_size = deflate_compressor.Compress(data, output_buffer);
  output_buffer.resize(compressed_size);
  WriteTask write_task;
  write_task.index = index;
  write_task.uncompressed_size = size;
  write_task.data = std::move(output_buffer);
  write_tasks_.write(std::move(write_task));
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
  assert(scheduled_tasks.empty());
}

void SiscoWriter::WriteChunk(uint64_t uncompressed_size, std::span<const std::byte> data) {
  file_.write(reinterpret_cast<const char*>(&uncompressed_size), sizeof(uncompressed_size));
  const uint64_t compressed_size = data.size();
  file_.write(reinterpret_cast<const char*>(&compressed_size), sizeof(compressed_size));
  file_.write(reinterpret_cast<const char*>(data.data()), data.size());
  if(!file_.good()) {
    // Print to cerr because error msg might be lost due to multithreading.
    constexpr const char* message = "Error writing file!\n";
    throw std::runtime_error(message);
  }
}

} // namespace casacore::sisco
