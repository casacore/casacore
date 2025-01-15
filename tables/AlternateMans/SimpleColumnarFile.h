#ifndef CASACORE_COLUMNAR_FILE_H_
#define CASACORE_COLUMNAR_FILE_H_

#include <cassert>
#include <complex>
#include <cstdint>
#include <string>
#include <vector>

#include "BitPacking.h"
#include "RowBasedFile.h"

namespace casacore {

/**
 * Class that provides binary table I/O. It is similar to
 * @ref BufferedColumnarFile, but is a simple implementation to demonstrate the
 * interface and to test the base class @ref RowBasedFile. For documentation,
 * see @ref BufferedColumnarFile.
 *
 * This class writes the data in a cell directly to file when @ref Write() is
 * called, and always reads it back when @ref Read() is called.
 */
class SimpleColumnarFile : private RowBasedFile {
 public:
  using RowBasedFile::AddRows;
  using RowBasedFile::Close;
  using RowBasedFile::DeleteRow;
  using RowBasedFile::Filename;
  using RowBasedFile::IsOpen;
  using RowBasedFile::NRows;
  using RowBasedFile::ReadHeader;
  using RowBasedFile::Stride;
  using RowBasedFile::WriteHeader;

  SimpleColumnarFile() noexcept = default;

  SimpleColumnarFile(const SimpleColumnarFile& rhs) = delete;
  SimpleColumnarFile(SimpleColumnarFile&& rhs) noexcept
      : packed_buffer_(std::move(rhs.packed_buffer_)) {}

  ~SimpleColumnarFile() noexcept = default;

  SimpleColumnarFile& operator=(SimpleColumnarFile&& rhs) {
    RowBasedFile::operator=(std::move(rhs));
    std::swap(packed_buffer_, rhs.packed_buffer_);
    return *this;
  }

  static SimpleColumnarFile CreateNew(const std::string& filename,
                                      uint64_t header_size, uint64_t stride) {
    return SimpleColumnarFile(filename, header_size, stride);
  }

  static SimpleColumnarFile OpenExisting(const std::string& filename,
                                         size_t header_size) {
    return SimpleColumnarFile(filename, header_size);
  }

  void Read(uint64_t row, uint64_t column_offset, std::complex<float>* data,
            uint64_t n) {
    ReadImplementation(row, column_offset, data, n);
  }
  void Read(uint64_t row, uint64_t column_offset, float* data, uint64_t n) {
    ReadImplementation(row, column_offset, data, n);
  }
  void Read(uint64_t row, uint64_t column_offset, double* data, uint64_t n) {
    ReadImplementation(row, column_offset, data, n);
  }
  void Read(uint64_t row, uint64_t column_offset, bool* data, uint64_t n) {
    const size_t byte_size = (n + 7) / 8;
    assert(column_offset + byte_size <= Stride());
    if (row >= NRows()) {
      std::fill_n(data, n, false);
    } else {
      Seek(row * Stride() + column_offset + DataLocation(), SEEK_SET);
      ReadData(packed_buffer_.data(), byte_size);
      UnpackBoolArray(data, packed_buffer_.data(), n);
    }
  }
  void Write(uint64_t row, uint64_t column_offset,
             const std::complex<float>* data, uint64_t n) {
    WriteImplementation(row, column_offset, data, n);
  }
  void Write(uint64_t row, uint64_t column_offset,
             const std::complex<double>* data, uint64_t n) {
    WriteImplementation(row, column_offset, data, n);
  }
  void Write(uint64_t row, uint64_t column_offset, float* data, uint64_t n) {
    WriteImplementation(row, column_offset, data, n);
  }
  void Write(uint64_t row, uint64_t column_offset, double* data, uint64_t n) {
    WriteImplementation(row, column_offset, data, n);
  }
  void Write(uint64_t row, uint64_t column_offset, const bool* data,
             uint64_t n) {
    assert(column_offset + n <= Stride());
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
  }

 private:
  // Create or overwrite a new columnar file on disk
  SimpleColumnarFile(const std::string& filename, uint64_t header_size,
                     uint64_t stride)
      : RowBasedFile(filename, header_size, stride),
        packed_buffer_((stride + 7) / 8) {}

  // Open an existing columnar file
  SimpleColumnarFile(const std::string& filename, size_t header_size)
      : RowBasedFile(filename, header_size) {
    packed_buffer_.resize((Stride() + 7) / 8);
  }

  template <typename ValueType>
  void ReadImplementation(uint64_t row, uint64_t column_offset, ValueType* data,
                          uint64_t n) {
    assert(column_offset + n * sizeof(ValueType) <= Stride());
    if (row >= NRows()) {
      std::fill_n(data, n, ValueType());
    } else {
      Seek(row * Stride() + column_offset + DataLocation(), SEEK_SET);
      ReadData(reinterpret_cast<unsigned char*>(data), n * sizeof(ValueType));
    }
  }

  template <typename ValueType>
  void WriteImplementation(uint64_t row, uint64_t column_offset,
                           const ValueType* data, uint64_t n) {
    assert(column_offset + n * sizeof(ValueType) <= Stride());
    Seek(row * Stride() + column_offset + DataLocation(), SEEK_SET);
    WriteData(reinterpret_cast<const unsigned char*>(data),
              n * sizeof(ValueType));
    SetNRows(std::max(row + 1, NRows()));
  }

  std::vector<unsigned char> packed_buffer_;
};

}  // namespace casacore

#endif
