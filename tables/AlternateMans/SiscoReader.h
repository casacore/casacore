#ifndef SISCO_SISCO_READER_H_
#define SISCO_SISCO_READER_H_

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
class SiscoReader {
  struct Chunk {
    size_t n_results_in_chunk = 0;
    size_t n_results_processed = 0;
    size_t decompressed_size;
    size_t chunk_index;
    std::vector<std::byte> read_buffer;
    std::vector<std::byte> decompress_buffer;

    bool operator<(const Chunk& rhs) const {
      return chunk_index < rhs.chunk_index;
    }
  };

 private:
  struct RequestData {
    size_t baseline_index;
    size_t n_values;
    size_t mantissas_offset;
    size_t exponents_offset;
    size_t index;
    Chunk* chunk;
  };

 public:
  SiscoReader(const std::string& filename);
  SiscoReader(SiscoReader&&) = default;
  ~SiscoReader();
  SiscoReader& operator=(SiscoReader&&) = default;

  void Open(std::span<std::byte> header_data);

  size_t GetRequestBufferSize() const { return kRequestBufferSize; }

  /**
   * This is an interface that allow parallelism over the decompression. To
   * use it, one thread should issue the requests (as fast as possible), while
   * the main thread should obtain the values using @ref GetNextResult().
   * The @ref Request() method is blocking when the internal buffer of results
   * is full or when all worker threads are busy.
   *
   * This method is necessary because the compressed files don't store the
   * nr of values per row or baseline_index.
   */
  void Request(size_t baseline_index, size_t n_values);

  void GetNextResult(std::span<std::complex<float>> data);

  int PredictLevel() const { return predict_level_; }

 private:
  void Close();

  void ResultLoop();

  struct Result {
    std::vector<float> real_data;
    std::vector<float> imaginary_data;
    size_t result_index = 0;
    bool operator<(const Result& rhs) const {
      return result_index < rhs.result_index;
    }
  };
  void ReadLoop();
  void DecompressChunk();
  void GetNextChunk(Chunk& chunk);
  /// Read current chunk and returns the decompressed size
  size_t ReadChunk(std::vector<std::byte>& buffer);

  static constexpr size_t kRequestBufferSize = 4096;
  std::set<Chunk> decompressed_queue;

  Chunk* current_chunk_;
  std::list<Chunk> chunks_;

  int predict_level_;
  size_t chunk_sequence = 0;
  size_t chunk_item_position_ = 0;
  size_t result_counter_ = 0;
  size_t result_sequence_ = 0;
  size_t results_in_chunk_counter_ = 0;

  bool open_ = false;

  aocommon::Lane<Chunk> read_lane_{4};
  aocommon::Lane<Chunk> decompress_lane_;
  aocommon::Lane<Result> result_lane_{kRequestBufferSize};
  ConditionalQueue<RequestData> request_queue_{kRequestBufferSize};
  std::set<Result> result_queue;

  std::thread read_thread_;
  std::vector<std::thread> result_workers_;
  std::mutex mutex_;
  std::set<size_t> busy_baselines_;

  struct BaselineData {
    CompressorState real_state_;
    CompressorState imaginary_state_;
  };

  // Indexed by baseline_index.
  std::map<size_t, BaselineData> baseline_data_;
  std::string filename_;
  std::ifstream file_;
};

}  // namespace casacore::sisco

#endif
