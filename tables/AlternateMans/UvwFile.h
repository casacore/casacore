#ifndef CASACORE_UVW_FILE_H_
#define CASACORE_UVW_FILE_H_

#include "BufferedColumnarFile.h"

#include <algorithm>
#include <cassert>
#include <limits>
#include <ostream>
#include <stdexcept>
#include <string>

namespace casacore {

/**
 * Stores values of a UVW column in a compressed way. Instead of storing a UVW
 * value per baseline, it stores one per antenna. It uses the fact that:
 *
 * baseline_uvw = antenna2_uvw - antenna1_uvw.
 *
 * By taking the first written antenna as reference antenna and storing the
 * relative UVW distance of the other antennas towards the reference antenna,
 * the baseline uvws can be reconstructed.
 *
 * A small downside is that this requires the measurement set to be "reasonably"
 * ordered: every timestep should have the same baselines in the same order.
 * Also, it requires the baselines to be ordered such that a relation to the
 * reference antenna can be made. A measurement set that is ordered either by
 * antenna1 or antenna2 for which no baselines have been removed satisfies this.
 * Removing an entire antenna is fine.
 *
 * Example of acceptable antenna lists:
 * - [0,0] ; [0,1] ; [0,2] ; [1,1] ; [1,2] ; [2,2]
 *   Ordered by antenna1, no missing baselines, no missing antennas.
 * - [0,1] ; [0,2] ; [1,2]
 *   Ordered by antenna1, only auto-correlations are missing, no missing
 * antennas.
 * - [0,0] ; [1,0] ; [2,0] ; [1,1] ; [2,1] ; [2,2]
 *   Ordered by antenna2, only auto-correlations are missing, no missing
 * antennas.
 * - [1,3] ; [1,4] ; [3,4]
 *   Ordered by antenna1, misses antenna 0 and 2.
 *
 * An example of a baseline ordering that is not accepted:
 * - [1, 2] ; [3, 4]
 *   The class will use 1 as reference antenna and calculate antenna 2 relative
 * to that. However, it can't store 3 or 4 relative to 1.
 */
class UvwFile {
 public:
  UvwFile() noexcept = default;

  UvwFile(UvwFile&& source) noexcept
      : file_(std::move(source.file_)),
        n_rows_(source.n_rows_),
        rows_per_block_(source.rows_per_block_),
        active_block_(source.active_block_),
        reference_antenna_(source.reference_antenna_),
        start_antenna_2_(source.start_antenna_2_),
        n_antennas_(source.n_antennas_),
        block_uvws_(std::move(source.block_uvws_)),
        block_is_changed_(source.block_is_changed_) {
    source.n_rows_ = 0;
    source.rows_per_block_ = 0;
    source.active_block_ = 0;
    source.reference_antenna_ = 0;
    source.start_antenna_2_ = 0;
    source.n_antennas_ = 0;
    source.block_uvws_.clear();
    source.block_is_changed_ = false;
  }

  ~UvwFile() noexcept { Close(); }

  UvwFile& operator=(UvwFile&& rhs) {
    Close();
    file_ = std::move(rhs.file_);
    n_rows_ = rhs.n_rows_;
    rows_per_block_ = rhs.rows_per_block_;
    active_block_ = rhs.active_block_;
    reference_antenna_ = rhs.reference_antenna_;
    start_antenna_2_ = rhs.start_antenna_2_;
    n_antennas_ = rhs.n_antennas_;
    block_uvws_ = std::move(rhs.block_uvws_);
    block_is_changed_ = rhs.block_is_changed_;
    rhs.n_rows_ = 0;
    rhs.rows_per_block_ = 0;
    rhs.active_block_ = 0;
    rhs.reference_antenna_ = 0;
    rhs.start_antenna_2_ = 0;
    rhs.n_antennas_ = 0;
    rhs.block_uvws_.clear();
    rhs.block_is_changed_ = false;
    return *this;
  }

  /**
   * Create a new UVW file on disk with the given filename.
   */
  static UvwFile CreateNew(const std::string& filename) {
    return UvwFile(filename);
  }

  /**
   * Open an already existing UVW file from disk with the given filename.
   */
  static UvwFile OpenExisting(const std::string& filename) {
    return UvwFile(filename, true);
  }

  /**
   * Write a single row to the column. This has to be done in a reasonable
   * order; see the class description.
   * @param row denotes the row in the Casacore measurement set to be read.
   */
  void WriteUvw(uint64_t row, size_t antenna1, size_t antenna2,
                const double* uvw) {
    assert(file_.IsOpen());
    if (row > n_rows_) {
      throw std::runtime_error(
          "Uvw data must be written in order (writing row " +
          std::to_string(row) + ", after writing " + std::to_string(n_rows_) +
          " rows)");
    }
    // The row/block is zero when there's not yet a full block written.
    if (rows_per_block_ == 0) {
      if (row == 0) {
        reference_antenna_ = antenna1;
        start_antenna_2_ = antenna2;
        n_antennas_ = std::max(antenna1, antenna2) + 1;
        block_uvws_.resize(n_antennas_, kUnsetPosition);
        block_uvws_[reference_antenna_] = {0.0, 0.0, 0.0};
      } else if (antenna1 == reference_antenna_ &&
                 antenna2 == start_antenna_2_) {
        // This baseline is the first baseline of a new block, so the block size
        // can be determined
        rows_per_block_ = n_rows_;
        n_antennas_ = block_uvws_.size();
        WriteHeader();
        ActivateBlock(1);
      } else {
        n_antennas_ = std::max({antenna1 + 1, antenna2 + 1, n_antennas_});
        block_uvws_.resize(n_antennas_, kUnsetPosition);
      }
    } else {
      const uint64_t block = row / rows_per_block_;
      ActivateBlock(block);
    }
    if (antenna1 != antenna2) {
      if (antenna1 == reference_antenna_) {
        // baseline = a2 - a1 with a1 = 0
        const std::array<double, 3> ant2_uvw{uvw[0], uvw[1], uvw[2]};
        StoreOrCheck(antenna2, ant2_uvw);
      } else if (antenna2 == reference_antenna_) {
        // baseline = a2 - a1 with a2 = 0
        const std::array<double, 3> ant1_uvw{-uvw[0], -uvw[1], -uvw[2]};
        StoreOrCheck(antenna1, ant1_uvw);
      } else if (IsSet(antenna1)) {
        // baseline = a2 - a1. Given a1:
        // a2 = baseline + a1
        const std::array<double, 3> ant1_uvw = block_uvws_[antenna1];
        const std::array<double, 3> ant2_uvw{
            uvw[0] + ant1_uvw[0], uvw[1] + ant1_uvw[1], uvw[2] + ant1_uvw[2]};
        StoreOrCheck(antenna2, ant2_uvw);
      } else if (IsSet(antenna2)) {
        // baseline = a2 - a1. Given a2:
        // a1 = a2 - baseline
        const std::array<double, 3> ant2_uvw = block_uvws_[antenna2];
        const std::array<double, 3> ant1_uvw{
            ant2_uvw[0] - uvw[0], ant2_uvw[1] - uvw[1], ant2_uvw[2] - uvw[2]};
        StoreOrCheck(antenna1, ant1_uvw);
      } else {
        throw std::runtime_error(
            "Baselines are written in a non-ordered way: they need to be "
            "ordered either by antenna 1 or by antenna 2");
      }
    } else {
      // In the special case of a file with only auto-correlations, the
      // positions of the antennas can not be established in this case, it's
      // also not necessary, because a zero uvw can be returned for
      // auto-correlations. However, it will cause StoreOrCheck() to be
      // never called, and therefore the block is never written to the file, and
      // upon read the nr. of rows can not be determined. So, if this is an
      // auto-correlation that has not yet been written, mark the block as
      // changed so it gets written.
      block_is_changed_ = block_is_changed_ || !IsSet(antenna1);
    }
    n_rows_ = std::max(n_rows_, row + 1);
  }

  /**
   * Read a single row. This may be done in random order, but is most efficient
   * when reading a file contiguously.
   * @param row denotes the row in the Casacore measurement set to be read.
   */
  void ReadUvw(uint64_t row, size_t antenna1, size_t antenna2, double* uvw) {
    assert(file_.IsOpen());
    if (row >= n_rows_ || antenna1 >= n_antennas_ || antenna2 >= n_antennas_) {
      throw std::runtime_error(
          "Invalid read for Uvw data: row " + std::to_string(row) +
          ", baseline (" + std::to_string(antenna1) + ", " +
          std::to_string(antenna2) + ") was requested. File has only " +
          std::to_string(n_rows_) + " rows with " +
          std::to_string(n_antennas_) + " antennas.");
    }
    if (rows_per_block_ != 0) {
      const uint64_t block = row / rows_per_block_;
      ActivateBlock(block);
    }
    uvw[0] = block_uvws_[antenna2][0] - block_uvws_[antenna1][0];
    uvw[1] = block_uvws_[antenna2][1] - block_uvws_[antenna1][1];
    uvw[2] = block_uvws_[antenna2][2] - block_uvws_[antenna1][2];
  }

  void Close() {
    if (file_.IsOpen()) {
      // This handles two special cases: i) if only one block of visibilities
      // is written, no repetition of baseline would have been identified yet.
      // In that case, we now know the block size. ii) in case only a single
      // auto-correlation (one antenna) is written without any
      // cross-correlations, the compressed file will remain empty. We then have
      // to identify how many rows are really in the MS, which is done by
      // setting rows_per_block_ to the nr of rows. When reading such an MS, the
      // fact that n_antennas_ == 1 is then a trigger to use rows_per_block_ as
      // nrows, instead of the size of the compressed file.
      if (rows_per_block_ == 0) {
        rows_per_block_ = n_rows_;
        n_antennas_ = block_uvws_.size();
        WriteHeader();
      } else if (n_antennas_ == 1) {
        rows_per_block_ = n_rows_;
        WriteHeader();
      }
      if (block_is_changed_) {
        WriteActiveBlock();
      }
      file_.Close();
    }
  }

  uint64_t NRows() const { return n_rows_; }
  std::string Filename() const { return file_.Filename(); }

 private:
  /**
   * Create a new file on disk.
   */
  UvwFile(const std::string& filename)
      : file_(BufferedColumnarFile::CreateNew(filename, kHeaderSize,
                                              sizeof(double) * 3)) {}

  /**
   * Open an existing file from disk. The last parameter is a dummy
   * parameter to distinguish it from the creating constructor.
   */
  UvwFile(const std::string& filename, bool /*open existing*/)
      : file_(BufferedColumnarFile::OpenExisting(filename, kHeaderSize)) {
    ReadHeader();
    active_block_ = std::numeric_limits<uint64_t>::max();
    if (n_antennas_ > 1) {
      if (file_.NRows() % (n_antennas_ - 1) != 0) {
        throw std::runtime_error(
            "Uvw file has an incorrect number of rows (" +
            std::to_string(file_.NRows()) + ", expecting multiple of " +
            std::to_string(n_antennas_ - 1) + "): file corrupted?");
      }
      const uint64_t n_blocks = file_.NRows() / (n_antennas_ - 1);
      n_rows_ = n_blocks * rows_per_block_;
    } else if (n_antennas_ == 1) {
      n_rows_ = rows_per_block_;
    }
    if (n_rows_ > 0 && n_antennas_ <= reference_antenna_)
      throw std::runtime_error(
          "Invalid combination of values for n_antenna and reference antenna "
          "in file: file damaged?");
    // Setting the size of block_uvws_ here, saves an extra size check in
    // ActivateBlock().
    block_uvws_.assign(n_antennas_, kUnsetPosition);
  }

  /**
   * If this block does not have a value for the specified antenna, store
   * the uvw value for it. If it does have a value, the value is checked. This
   * check has a relatively high tolerance (1e-5, fractionally) because it is
   * only there to catch "significant" errors caused by e.g. writing in the
   * wrong order.
   */
  void StoreOrCheck(size_t antenna, const std::array<double, 3>& antenna_uvw) {
    if (IsSet(antenna)) {
      if (!AreNear(block_uvws_[antenna], antenna_uvw)) {
        std::ostringstream msg;
        msg << "Inconsistent UVW value written for antenna " << antenna
            << ": old value is " << UvwAsString(block_uvws_[antenna])
            << ", new value is " << UvwAsString(antenna_uvw) << ".";
        throw std::runtime_error(msg.str());
      }
    } else {
      if (block_uvws_.size() <= antenna)
        block_uvws_.resize(antenna + 1, kUnsetPosition);
      block_uvws_[antenna] = antenna_uvw;
      block_is_changed_ = true;
    }
  }

  void ActivateBlock(size_t block) {
    if (block != active_block_) {
      if (block_is_changed_) {
        WriteActiveBlock();
      }

      active_block_ = block;
      ReadActiveBlock();
    }
  }

  void ReadActiveBlock() {
    const uint64_t block_start_row = (n_antennas_ - 1) * active_block_;
    if (block_start_row < file_.NRows()) {
      block_uvws_.clear();
      for (size_t antenna = 0; antenna != n_antennas_; ++antenna) {
        if (antenna != reference_antenna_) {
          const uint64_t row = antenna < reference_antenna_
                                   ? block_start_row + antenna
                                   : block_start_row + antenna - 1;
          std::array<double, 3>& uvw = block_uvws_.emplace_back();
          file_.Read(row, 0, uvw.data(), 3);
        } else {
          block_uvws_.emplace_back(std::array<double, 3>{0.0, 0.0, 0.0});
        }
      }
    } else {
      std::fill(block_uvws_.begin(), block_uvws_.end(), kUnsetPosition);
      block_uvws_[reference_antenna_] = {0.0, 0.0, 0.0};
    }
  }

  void WriteActiveBlock() {
    if (block_uvws_.size() != n_antennas_)
      throw std::runtime_error("Trying to write an incomplete UVW block");
    const uint64_t block_start_row = (n_antennas_ - 1) * active_block_;
    for (size_t antenna = 0; antenna != n_antennas_; ++antenna) {
      if (antenna != reference_antenna_) {
        const uint64_t row = antenna < reference_antenna_
                                 ? block_start_row + antenna
                                 : block_start_row + antenna - 1;
        file_.Write(row, 0, block_uvws_[antenna].data(), 3);
      }
    }
    block_is_changed_ = false;
  }

  void ReadHeader() {
    unsigned char data[kHeaderSize];
    file_.ReadHeader(data);
    if (!std::equal(data, data + 8, kMagicHeaderTag)) {
      throw std::runtime_error(
          "The UVW columnar file header not have the expected tag for UVW "
          "columns: the measurement set may be damaged");
    }
    rows_per_block_ = reinterpret_cast<uint64_t&>(data[8]);
    reference_antenna_ = reinterpret_cast<uint64_t&>(data[16]);
    n_antennas_ = reinterpret_cast<uint64_t&>(data[24]);
  }

  void WriteHeader() {
    unsigned char data[kHeaderSize];
    std::copy_n(kMagicHeaderTag, 8, data);
    reinterpret_cast<uint64_t&>(data[8]) = rows_per_block_;
    reinterpret_cast<uint64_t&>(data[16]) = reference_antenna_;
    reinterpret_cast<uint64_t&>(data[24]) = n_antennas_;
    file_.WriteHeader(data);
  }

  bool IsSet(size_t antenna) const {
    return block_uvws_.size() > antenna &&
           block_uvws_[antenna] != kUnsetPosition;
  }
  static bool AreNear(std::array<double, 3> a, std::array<double, 3> b) {
    return AreNear(a[0], b[0]) && AreNear(a[1], b[1]) && AreNear(a[2], b[2]);
  }
  static bool AreNear(double a, double b) {
    const double magnitude = std::max({1e-5, std::fabs(a), std::fabs(b)});
    return (std::fabs(a - b) / magnitude) < 1e-5;
  }
  static std::string UvwAsString(const std::array<double, 3>& uvw) {
    std::ostringstream str;
    str << "[" << uvw[0] << ", " << uvw[1] << ", " << uvw[2] << "]";
    return str.str();
  }

  /**
   * The header:
   * char[8] "Uvw-col\0" (=kMagicHeaderTag)
   * uint64_t rows_per_block
   * uint64_t reference_antenna
   * uint64_t n_antenna
   */
  constexpr static size_t kHeaderSize = 32;
  constexpr static const char kMagicHeaderTag[8] = "Uvw-col";
  constexpr static std::array<double, 3> kUnsetPosition = {
      std::numeric_limits<double>::max(), std::numeric_limits<double>::max(),
      std::numeric_limits<double>::max()};
  BufferedColumnarFile file_;
  /**
   * Number of rows in the Uvw column. It is increased when writing a new
   * Uvw row using @ref WriteUvw(). This concept of a "row" is different
   * from a row in the BufferedColumnarFile (i.e., @c file_ ), as for each
   * block (see below), only the uvw per antenna are stored, and these form
   * the rows in that file.
   */
  uint64_t n_rows_ = 0;
  /**
   * A "block" is a contiguous number of baselines that together form
   * one timestep. This is the same as saying they form one (triangular)
   * correlation matrix. It can have auto-correlation but this is not
   * required. A block may have missing antennas, but has some constraints
   * on the ordering; see the class description. Per block, the UVW values
   * are calculated per antenna. This value specifies the number of table
   * rows in one block, i.e. the number of set elements in the correlation
   * matrix.
   */
  uint64_t rows_per_block_ = 0;
  uint64_t active_block_ = 0;
  size_t reference_antenna_ = 0;
  // This value is used to determine the first baseline in the data, which is
  // the baseline (reference_antenna_, start_antenna_2_).
  size_t start_antenna_2_ = 0;
  size_t n_antennas_ = 0;
  // UVW for each antenna in the block
  std::vector<std::array<double, 3>> block_uvws_;
  bool block_is_changed_ = false;
};

}  // namespace casacore

#endif
