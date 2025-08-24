#ifndef SISCO_SISCO_WRITER_H_
#define SISCO_SISCO_WRITER_H_

#include <complex>
#include <fstream>
#include <map>
#include <span>
#include <string>

#include "Deflate.h"
#include "Lane.h"
#include "Sisco.h"

/**
 * File interface for data stored in the generated model compression (Sisco)
 * format.
 */
class SiscoWriter {
 public:
  SiscoWriter(const std::string& filename, int predict_level = 2,
              int deflate_level = 9);
  SiscoWriter(SiscoWriter&&) = default;
  ~SiscoWriter() {
    if (file_.is_open()) Close();
  }
  SiscoWriter& operator=(SiscoWriter&&) = default;

  void Open(std::span<const std::byte> header_data);
  void Write(size_t baseline_index, std::span<const std::complex<float>> data);
  void Close();

 private:
  struct CompressionTask {
    std::vector<float> real_data;
    std::vector<float> imaginary_data;
    size_t baseline_index;
  };
  struct DeflateTask {
    size_t index;
    std::vector<std::byte> data;
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

  void PreprocessLoop();
  void DeflateWorker();
  void WriteLoop();
  void WriteChunk(size_t uncompressed_size, std::span<const std::byte> data);

  constexpr static size_t kDefaultChunkSize = 1024 * 1024;

  // Indexed by baseline_index.
  std::map<size_t, BaselineData> baseline_data_;
  std::ofstream file_;
  std::string filename_;
  std::vector<std::thread> compression_threads_;
  int predict_level_ = 2;
  int deflate_level_ = 9;
  aocommon::Lane<CompressionTask> preprocessing_tasks_{4096};
  aocommon::Lane<DeflateTask> deflate_tasks_{10};
  aocommon::Lane<WriteTask> write_tasks_{10};

  std::thread preprocess_thread_;
  std::array<std::thread, 5> deflate_threads_;
  std::thread write_thread_;
};

#endif
