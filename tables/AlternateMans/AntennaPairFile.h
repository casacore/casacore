#ifndef CASACORE_ANTENNA_PAIR_FILE_H_
#define CASACORE_ANTENNA_PAIR_FILE_H_

#include "BufferedColumnarFile.h"

#include <cassert>
#include <limits>
#include <stdexcept>
#include <string>
#include <vector>

namespace casacore {

/**
 * This class is able to store the combination of ANTENNA1 and ANTENNA2
 * columns in a compressed manner. It does this by assuming that the
 * pair of antenna indices are repeated, i.e. every timestep in the measurement
 * set follows the same sequence of baselines. By only writing the repeated
 * sequence once, the storage required is reduce to that of only one timestep.
 *
 * The class checks to see if the writing satisfies the required constraints,
 * and if they do not an exception is thrown.
 */
class AntennaPairFile {
 public:
  AntennaPairFile() noexcept = default;

  AntennaPairFile(AntennaPairFile&& source) noexcept
      : file_(std::move(source.file_)),
        rows_in_pattern_(source.rows_in_pattern_),
        data_(source.data_) {
    source.rows_in_pattern_ = 0;
    source.data_.clear();
  }

  ~AntennaPairFile() noexcept { Close(); }

  AntennaPairFile& operator=(AntennaPairFile&& rhs) {
    Close();
    file_ = std::move(rhs.file_);
    rows_in_pattern_ = rhs.rows_in_pattern_;
    data_ = rhs.data_;
    return *this;
  }

  /**
   * Create a new UVW file on disk with the given filename.
   */
  static AntennaPairFile CreateNew(const std::string& filename) {
    return AntennaPairFile(filename);
  }

  /**
   * Open an already existing UVW file from disk with the given filename.
   */
  static AntennaPairFile OpenExisting(const std::string& filename) {
    return AntennaPairFile(filename, true);
  }

  void WriteAntenna1(uint64_t row, int32_t antenna1) {
    WriteAntenna<0>(row, antenna1);
  }

  void WriteAntenna2(uint64_t row, int32_t antenna2) {
    WriteAntenna<1>(row, antenna2);
  }

  void WritePair(uint64_t row, int32_t antenna1, int32_t antenna2) {
    WriteAntenna<0>(row, antenna1);
    WriteAntenna<1>(row, antenna2);
  }

  int32_t ReadAntenna1(uint64_t row) { return ReadAntenna<0>(row); }

  int32_t ReadAntenna2(uint64_t row) { return ReadAntenna<1>(row); }

  void Close() {
    if (file_.IsOpen()) {
      if (rows_in_pattern_ == 0) {
        WriteHeader();
        WriteData();
      }
      file_.Close();
      rows_in_pattern_ = 0;
      data_.clear();
    }
  }

  const std::string& Filename() const { return file_.Filename(); }

  /**
   * The number of rows that form one repeating pattern. Function is added
   * for testing purposes.
   */
  uint64_t NRowsInPattern() const { return rows_in_pattern_; }

 private:
  /**
   * Create a new file on disk.
   */
  AntennaPairFile(const std::string& filename)
      : file_(BufferedColumnarFile::CreateNew(filename, kHeaderSize,
                                              sizeof(int32_t) * 2)) {}

  /**
   * Open an existing file from disk. The last parameter is a dummy
   * parameter to distinguish it from the creating constructor.
   */
  AntennaPairFile(const std::string& filename, bool /*open existing*/)
      : file_(BufferedColumnarFile::OpenExisting(filename, kHeaderSize)) {
    ReadHeader();
    ReadData();
  }

  static bool HasUnsetAntenna(const std::array<int32_t, 2>& pair) {
    return pair[0] == kUnsetAntenna || pair[1] == kUnsetAntenna;
  }

  /**
   * @tparam AntennaNumber Number of the antenna inside the pair: zero or one.
   */
  template <size_t AntennaNumber>
  void WriteAntenna(uint64_t row, int32_t antenna) {
    static_assert(AntennaNumber == 0 || AntennaNumber == 1);
    if (rows_in_pattern_ == 0) {
      const bool has_unfinished_row =
          !data_.empty() && HasUnsetAntenna(data_.back());
      if (has_unfinished_row) {
        if (row >= data_.size())
          throw std::runtime_error(
              "Incorrect writing order for AntennaPairFile (in unfinished "
              "pair, row=" +
              std::to_string(row) + ")");
        const bool is_rewrite = row < data_.size() - 1 ||
                                data_.back()[AntennaNumber] != kUnsetAntenna;
        if (is_rewrite) {
          if (data_[row][AntennaNumber] != antenna)
            throw std::runtime_error(
                "Antenna " + std::to_string(AntennaNumber) + " value in row " +
                std::to_string(row) + " is rewritten with a different value");
        } else {
          data_.back()[AntennaNumber] = antenna;
          if (data_.back() == data_.front() && data_.size() > 1) {
            // This row is the same as the first row, so this row is the first
            // row of the next repeated pattern, and we have discovered the size
            // of the pattern.
            data_.pop_back();
            rows_in_pattern_ = data_.size();
            WriteHeader();
            WriteData();
          }
        }
      } else {
        if (row > data_.size())
          throw std::runtime_error(
              "Incorrect writing order for AntennaPairFile (in new pair, row=" +
              std::to_string(row) + ")");
        if (row == data_.size()) {
          if constexpr (AntennaNumber == 0)
            data_.emplace_back(std::array<int32_t, 2>{antenna, kUnsetAntenna});
          else
            data_.emplace_back(std::array<int32_t, 2>{kUnsetAntenna, antenna});
        } else {
          // This is a rewrite of an already written value
          if (data_[row][AntennaNumber] != antenna)
            throw std::runtime_error(
                "Antenna " + std::to_string(AntennaNumber) + " value in row " +
                std::to_string(row) + " is rewritten with a different value");
        }
      }
    } else {
      const std::array<int32_t, 2>& pair = data_[row % rows_in_pattern_];
      if (pair[AntennaNumber] != antenna)
        throw std::runtime_error(
            "Error writing to AntennaPairFile, row " + std::to_string(row) +
            ": the antenna pairs do not follow a consistent pattern");
    }
  }

  template <size_t AntennaNumber>
  int32_t ReadAntenna(uint64_t row) {
    static_assert(AntennaNumber == 0 || AntennaNumber == 1);
    int32_t antenna;
    if (rows_in_pattern_ == 0) {
      if (row >= data_.size())
        throw std::runtime_error(
            "Invalid read of antenna pair: requested row is beyond the number "
            "of written rows, and writing of antenna pattern not finished");
      antenna = data_[row][AntennaNumber];
      if (antenna == kUnsetAntenna)
        throw std::runtime_error(
            "Trying to read antenna value that has not been written yet");
      return antenna;
    } else {
      return data_[row % rows_in_pattern_][AntennaNumber];
    }
  }

  void ReadHeader() {
    unsigned char data[kHeaderSize];
    file_.ReadHeader(data);
    if (!std::equal(data, data + 8, kMagicHeaderTag)) {
      throw std::runtime_error(
          "The Antenna-pair columnar file header not have the expected tag for "
          "antenna columns: the measurement set may be damaged");
    }
    rows_in_pattern_ = reinterpret_cast<uint64_t&>(data[8]);
  }

  void WriteHeader() {
    unsigned char data[kHeaderSize];
    std::copy_n(kMagicHeaderTag, 8, data);
    reinterpret_cast<uint64_t&>(data[8]) = rows_in_pattern_;
    file_.WriteHeader(data);
  }

  void ReadData() {
    data_.resize(file_.NRows());
    for (uint64_t row = 0; row != data_.size(); ++row) {
      file_.Read(row, 0, data_[row].data(), 2);
    }
  }

  void WriteData() {
    // Note that rows_in_pattern_ might still be zero if the pattern has not
    // been finished. This would happen if MS is partially written, reopened and
    // written further. This is rather unlikely to happen, but it is easy to
    // support.
    for (uint64_t row = 0; row != data_.size(); ++row) {
      file_.Write(row, 0, data_[row].data(), 2);
    }
  }

  /**
   * The header:
   * char[8] "AntPair\0"
   * uint64_t rows_per_block
   */
  constexpr static size_t kHeaderSize = 16;
  constexpr static const char kMagicHeaderTag[8] = "AntPair";
  constexpr static int32_t kUnsetAntenna = std::numeric_limits<int32_t>::min();
  BufferedColumnarFile file_;
  /**
   * This value remains zero until the repeating pattern was found.
   */
  uint64_t rows_in_pattern_ = 0;
  std::vector<std::array<int32_t, 2>> data_;
};

}  // namespace casacore

#endif
