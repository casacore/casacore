#ifndef SISCO_SISCO_WRITER_H_
#define SISCO_SISCO_WRITER_H_

#include <complex>
#include <fstream>
#include <list>
#include <map>
#include <set>
#include <span>
#include <string>
#include <vector>

#include "ConditionalQueue.h"
#include "Deflate.h"
#include "Lane.h"
#include "Sisco.h"

namespace casacore::sisco {

/**
 * File interface for data stored in the generated model compression (Sisco)
 * format.
 */
class SiscoWriter {
 public:
  SiscoWriter(const std::string& filename, int predict_level,
              int deflate_level);
  SiscoWriter(SiscoWriter&&) = delete;
  ~SiscoWriter() {
    if (file_.is_open()) Close();
  }
  SiscoWriter& operator=(SiscoWriter&&) = delete;

  void Open(std::span<const std::byte> header_data);
  void Write(size_t baseline_index, std::span<const std::complex<float>> data);
  void Close();

 private:
  struct Chunk {
    size_t index;
    std::vector<std::byte> mantisas_data;
    std::vector<std::byte> exponent_data;
    size_t allocated_chunk_size = kDefaultChunkSize;
    // Whenever a thread finishes preprocessing, it will update this value. If
    // the value is high enough, the thread knows the chunk is ready for
    // deflate.
    size_t n_allocated_items = 0;
    size_t n_finished_items = 0;
    size_t item_counter = 0;
    size_t chunk_size = 0;
  };
  struct PreprocessingTask {
    std::vector<float> real_data;
    std::vector<float> imaginary_data;
    size_t baseline_index;
    std::byte* mantisa_position;
    std::byte* exponent_position;
    Chunk* chunk;
  };
  struct WriteTask {
    size_t index;
    size_t uncompressed_size;
    std::vector<std::byte> data;
  };
  struct BaselineData {
    CompressorState real_state_;
    CompressorState imaginary_state_;
    size_t n_sequences_ = 0;
  };
  struct ThreadData {};

  void NewChunk(std::unique_lock<std::mutex>& lock);
  void RemoveChunk(const Chunk* chunk);
  void PreprocessLoop();
  void WriteLoop();
  void Preprocess(SiscoWriter::PreprocessingTask& task,
                  std::unique_lock<std::mutex>& lock);
  void WriteChunk(size_t uncompressed_size, std::span<const std::byte> data);
  void DeflateChunk(Chunk& chunk);

  // Chunk data
  constexpr static size_t kDefaultChunkSize = 1024 * 1024;
  size_t chunk_write_position_ = 0;
  std::byte* mantisa_position_;
  std::byte* exponent_position_;
  size_t n_chunks_ = 0;
  // A list is used because we do not want to invalidate pointers when
  // adding/revoming members. Access requires holding the mutex.
  std::list<Chunk> chunks_;
  Chunk* write_chunk_;

  // Indexed by baseline_index.
  std::map<size_t, BaselineData> baseline_data_;
  std::ofstream file_;
  std::string filename_;
  int predict_level_ = 2;
  int deflate_level_ = 9;
  ConditionalQueue<PreprocessingTask> preprocessing_tasks_{4096};
  aocommon::Lane<WriteTask> write_tasks_{10};

  std::mutex mutex_;
  std::condition_variable chunk_finished_condition_;
  /// Holds a list of baseline ids currently being preprocessed. Access
  /// must be synchronized.
  std::set<size_t> busy_baselines_;
  std::vector<std::thread> compression_threads_;
  std::thread write_thread_;
};

}  // namespace casacore::sisco

#endif
