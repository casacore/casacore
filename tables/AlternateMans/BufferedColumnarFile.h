#ifndef CASACORE_BUFFERED_COLUMNAR_FILE_H_
#define CASACORE_BUFFERED_COLUMNAR_FILE_H_

#include <cassert>
#include <complex>
#include <cstdint>
#include <string>
#include <vector>

#include "BitPacking.h"
#include "RowBasedFile.h"

namespace casacore {

/**
 * Class that provides binary table I/O. It is rather low-level, and requires
 * the user to keep track of the columns and their datatype. It uses a simple
 * binary format where the columns are interleaved into the output file.
 * Booleans are written with bit-packing.
 *
 * This class limits calls to the read/write functions by caching a block of
 * data around the last accessed row. The size of the buffer is templated to
 * make testing of the caching easier. Production code can normally use the
 * alias BufferedColumnarFile, which uses a reasonable buffer size.
 *
 * This class aims to be as fast as possible for large data files that are read
 * from or written to consecutively. It does not try to optimize random access,
 * partial data access and access is not transactional.
 *
 * The class uses exceptions to handle any I/O errors.
 */
template <uint64_t BufferSize = 100 * 1024>
class VarBufferedColumnarFile : private RowBasedFile {
 public:
  using RowBasedFile::AddRows;
  using RowBasedFile::DeleteRow;
  using RowBasedFile::Filename;
  using RowBasedFile::IsOpen;
  using RowBasedFile::NRows;
  using RowBasedFile::ReadHeader;
  using RowBasedFile::Stride;
  using RowBasedFile::WriteHeader;

  VarBufferedColumnarFile() noexcept = default;

  VarBufferedColumnarFile(const VarBufferedColumnarFile& rhs) = delete;
  VarBufferedColumnarFile(VarBufferedColumnarFile&& rhs) noexcept
      : packed_buffer_(std::move(rhs.packed_buffer_)),
        block_changed_(rhs.block_changed_),
        active_block_(rhs.active_block_),
        rows_per_block_(rhs.rows_per_block_),
        block_buffer_(std::move(rhs.block_buffer_)) {
    rhs.block_changed_ = false;
    rhs.active_block_ = 0;
    rhs.rows_per_block_ = 0;
  }

  ~VarBufferedColumnarFile() noexcept {
    if (IsOpen()) {
      if (block_changed_) {
        const uint64_t start_row = active_block_ * rows_per_block_;
        const size_t n_rows_to_write =
            std::min(rows_per_block_, std::max(NRows(), start_row) - start_row);
        Seek(start_row * Stride() + DataLocation(), SEEK_SET);
        WriteData(block_buffer_.data(), n_rows_to_write * Stride());
        block_changed_ = false;
      }
    }
  }

  VarBufferedColumnarFile& operator=(VarBufferedColumnarFile&& rhs) {
    RowBasedFile::operator=(std::move(rhs));
    std::swap(packed_buffer_, rhs.packed_buffer_);
    std::swap(block_changed_, rhs.block_changed_);
    std::swap(active_block_, rhs.active_block_);
    std::swap(rows_per_block_, rhs.rows_per_block_);
    std::swap(block_buffer_, rhs.block_buffer_);
    return *this;
  }

  /**
   * Close the file. After closing, all calls to I/O functions
   * cause undefined behaviour, untill the class is assigned to
   * a new instance.
   */
  void Close() {
    if (IsOpen()) {
      if (block_changed_) {
        const uint64_t start_row = active_block_ * rows_per_block_;
        const size_t n_rows_to_write =
            std::min(rows_per_block_, std::max(NRows(), start_row) - start_row);
        Seek(start_row * Stride() + DataLocation(), SEEK_SET);
        WriteData(block_buffer_.data(), n_rows_to_write * Stride());
        block_changed_ = false;
      }
      RowBasedFile::Close();
    }
  }

  /**
   * Create a new file on disk. If the file exists, it is overwritten.
   * @param header Optional header.
   * @param stride The number of bytes in one row (total over all columns).
   */
  static VarBufferedColumnarFile CreateNew(const std::string& filename,
                                           uint64_t header_size,
                                           uint64_t stride) {
    return VarBufferedColumnarFile(filename, header_size, stride);
  }

  /**
   * Open an existing file from disk. If the file does not exists, an
   * exception is thrown.
   * @param header Optional header.
   * @param stride The number of bytes in one row (total over all columns).
   */
  static VarBufferedColumnarFile OpenExisting(const std::string& filename,
                                              size_t header_size) {
    return VarBufferedColumnarFile(filename, header_size);
  }

  /**
   * Read one cell containing an array of floats. If the row was not written
   * yet, zeros are returned.
   * @param row The cell's row index.
   * @param column_offset The position of this column counted from the start
   * of the row, in bytes.
   * @param data Buffer in which the data will be stored.
   * @param n Size of the column in number of elements (NOT in bytes!).
   */
  void Read(uint64_t row, uint64_t column_offset, float* data, uint64_t n) {
    ReadImplementation(row, column_offset, data, n);
  }
  /**
   * Read array of doubles. See float version for documentation.
   */
  void Read(uint64_t row, uint64_t column_offset, double* data, uint64_t n) {
    ReadImplementation(row, column_offset, data, n);
  }
  /**
   * Read array of complex floats. See float version for documentation.
   */
  void Read(uint64_t row, uint64_t column_offset, std::complex<float>* data,
            uint64_t n) {
    ReadImplementation(row, column_offset, data, n);
  }
  /**
   * Read an array of bools. See float version for documentation. Booleans are
   * stored with bit-packing.
   */
  void Read(uint64_t row, uint64_t column_offset, bool* data, uint64_t n) {
    const size_t byte_size = (n + 7) / 8;
    assert(column_offset + byte_size <= Stride());
    ActivateBlock(row);
    if (row >= NRows()) {
      std::fill_n(data, n, false);
    } else {
      Seek(row * Stride() + column_offset + DataLocation(), SEEK_SET);
      ReadData(packed_buffer_.data(), byte_size);
      UnpackBoolArray(data, packed_buffer_.data(), n);
    }
  }
  /**
   * Write one cell containing an array of floats. If the row is past the end of
   * the file, the file is enlarged (making NRows() = row + 1).
   */
  void Write(uint64_t row, uint64_t column_offset, float* data, uint64_t n) {
    WriteImplementation(row, column_offset, data, n);
  }

  /**
   * Write an array of doubles. See float version for documentation.
   */
  void Write(uint64_t row, uint64_t column_offset, double* data, uint64_t n) {
    WriteImplementation(row, column_offset, data, n);
  }
  /**
   * Write an array of complex floats. See float version for documentation.
   */
  void Write(uint64_t row, uint64_t column_offset,
             const std::complex<float>* data, uint64_t n) {
    WriteImplementation(row, column_offset, data, n);
  }
  /**
   * Write an array of complex doubles. See float version for documentation.
   */
  void Write(uint64_t row, uint64_t column_offset,
             const std::complex<double>* data, uint64_t n) {
    WriteImplementation(row, column_offset, data, n);
  }
  /**
   * Write an array of bools. Bools are stored with bit-packing. See float
   * version for documentation.
   */
  void Write(uint64_t row, uint64_t column_offset, const bool* data,
             uint64_t n) {
    assert(column_offset + n <= Stride());
    ActivateBlock(row);
    PackBoolArray(packed_buffer_.data(), data, n);
    Seek(row * Stride() + column_offset + DataLocation(), SEEK_SET);
    WriteData(packed_buffer_.data(), n);
    SetNRows(std::max(row + 1, NRows()));
  }

  /**
   * Set the number of bytes per row for this file. This changes the format
   * of the file, and because of this the file is emptied.
   */
  void SetStride(uint64_t new_stride) {
    RowBasedFile::SetStride(new_stride);
    packed_buffer_.resize((new_stride + 7) / 8);
    active_block_ = std::numeric_limits<uint64_t>::max();
    rows_per_block_ =
        new_stride == 0 ? 0 : std::max<size_t>(1, BufferSize / new_stride);
    block_buffer_.resize(rows_per_block_ * new_stride);
    block_changed_ = false;
  }

 private:
  // Create or overwrite a new columnar file on disk
  VarBufferedColumnarFile(const std::string& filename, uint64_t header_size,
                          uint64_t stride)
      : RowBasedFile(filename, header_size, stride),
        packed_buffer_((stride + 7) / 8),
        rows_per_block_(std::max<size_t>(1, BufferSize / stride)),
        block_buffer_(rows_per_block_ * stride) {}

  // Open an existing columnar file
  VarBufferedColumnarFile(const std::string& filename, size_t header_size)
      : RowBasedFile(filename, header_size) {
    if (Stride() != 0) {
      packed_buffer_.resize((Stride() + 7) / 8);
      rows_per_block_ = std::max<size_t>(1, BufferSize / Stride());
      block_buffer_.resize(rows_per_block_ * Stride());
    }
    active_block_ = std::numeric_limits<uint64_t>::max();
  }

  void ActivateBlock(uint64_t row) {
    const uint64_t block = row / rows_per_block_;
    if (active_block_ != block) {
      if (block_changed_) {
        const uint64_t start_row = active_block_ * rows_per_block_;
        const size_t n_rows_to_write =
            std::min(rows_per_block_, std::max(NRows(), start_row) - start_row);
        Seek(start_row * Stride() + DataLocation(), SEEK_SET);
        WriteData(block_buffer_.data(), n_rows_to_write * Stride());
        block_changed_ = false;
      }

      const uint64_t start_row = block * rows_per_block_;
      const size_t n_rows_to_read =
          std::min(rows_per_block_, std::max(NRows(), start_row) - start_row);
      if (n_rows_to_read > 0) {
        Seek(start_row * Stride() + DataLocation(), SEEK_SET);
        ReadData(block_buffer_.data(), n_rows_to_read * Stride());
      }
      std::fill(block_buffer_.begin() + n_rows_to_read * Stride(),
                block_buffer_.end(), 0);

      active_block_ = block;
    }
  }

  template <typename ValueType>
  void ReadImplementation(uint64_t row, uint64_t column_offset, ValueType* data,
                          uint64_t n) {
    assert(column_offset + n * sizeof(ValueType) <= Stride());
    if (row >= NRows()) {
      std::fill_n(data, n, ValueType());
    } else {
      ActivateBlock(row);
      const uint64_t block_row = active_block_ * rows_per_block_;
      const unsigned char* position =
          block_buffer_.data() + (row - block_row) * Stride() + column_offset;
      std::copy_n(position, n * sizeof(ValueType),
                  reinterpret_cast<unsigned char*>(data));
    }
  }

  template <typename ValueType>
  void WriteImplementation(uint64_t row, uint64_t column_offset,
                           const ValueType* data, uint64_t n) {
    assert(column_offset + n * sizeof(ValueType) <= Stride());
    ActivateBlock(row);
    const uint64_t block_row = active_block_ * rows_per_block_;
    unsigned char* position =
        block_buffer_.data() + (row - block_row) * Stride() + column_offset;
    std::copy_n(reinterpret_cast<const unsigned char*>(data),
                n * sizeof(ValueType), position);
    SetNRows(std::max(row + 1, NRows()));
    block_changed_ = true;
  }

  std::vector<unsigned char> packed_buffer_;

  bool block_changed_ = false;
  uint64_t active_block_ = 0;
  uint64_t rows_per_block_ = 0;
  std::vector<unsigned char> block_buffer_;
};

using BufferedColumnarFile = VarBufferedColumnarFile<100 * 1024>;

}  // namespace casacore

#endif
