#ifndef CASACORE_ROW_BASED_FILE_H_
#define CASACORE_ROW_BASED_FILE_H_

#include <fcntl.h>
#include <unistd.h>

#include <cstdint>
#include <string>
#include <vector>

namespace casacore {

class RowBasedFile {
 public:
  RowBasedFile() = default;
  RowBasedFile(const RowBasedFile& rhs) = delete;
  RowBasedFile(RowBasedFile&& rhs) noexcept
      : file_(rhs.file_),
        n_rows_(rhs.n_rows_),
        stride_(rhs.stride_),
        data_location_(rhs.data_location_),
        filename_(rhs.filename_) {
    rhs.file_ = -1;
    rhs.n_rows_ = 0;
    rhs.stride_ = 0;
    rhs.data_location_ = kPrivateHeaderSize;
    rhs.filename_ = "";
  }

  // Create or overwrite a new columnar file on disk
  RowBasedFile(const std::string& filename, uint64_t header_size,
               uint64_t stride)
      : stride_(stride), filename_(filename) {
    file_ = open(filename.c_str(), O_CREAT | O_RDWR | O_TRUNC,
                 S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
    if (file_ < 0)
      throw std::runtime_error("I/O error: could not create new file '" +
                               filename + "'");
    WriteData(reinterpret_cast<unsigned char*>(&stride_), sizeof(stride_));
    data_location_ = header_size + kPrivateHeaderSize;
  }

  // Open an existing columnar file
  RowBasedFile(const std::string& filename, size_t header_size)
      : filename_(filename) {
    file_ = open(filename.c_str(), O_RDWR);
    if (file_ < 0)
      throw std::runtime_error("I/O error: could not open file '" + filename +
                               "'");
    ReadData(reinterpret_cast<unsigned char*>(&stride_), kPrivateHeaderSize);
    data_location_ = header_size + kPrivateHeaderSize;
    const uint64_t pos = lseek(file_, 0, SEEK_END);
    n_rows_ = stride_ == 0 ? 0 : (pos - data_location_) / stride_;
  }
  ~RowBasedFile() noexcept {
    if (IsOpen()) close(file_);
  }
  RowBasedFile& operator=(RowBasedFile&& rhs) {
    Close();
    std::swap(file_, rhs.file_);
    std::swap(n_rows_, rhs.n_rows_);
    std::swap(stride_, rhs.stride_);
    std::swap(data_location_, rhs.data_location_);
    std::swap(filename_, rhs.filename_);
    return *this;
  }

  /**
   * Close the file. After closing, all calls to I/O functions
   * cause undefined behaviour, untill the class is assigned to
   * a new instance.
   */
  void Close() {
    if (IsOpen()) {
      Truncate(NRows());
      int result = close(file_);
      if (result < 0)
        throw std::runtime_error("Could not close file " + filename_);
      file_ = -1;
      n_rows_ = 0;
      stride_ = 0;
      filename_ = "";
    }
  }

  void Truncate(uint64_t n_rows) {
    const int result = ftruncate(file_, n_rows_ * stride_ + data_location_);
    if (result < 0) {
      char errstr[128];
      char* msg = strerror_r(errno, errstr, 128);
      throw std::runtime_error("I/O error: could not truncate file '" +
                               filename_ + "' to have " +
                               std::to_string(n_rows) + " rows: " + msg);
    }
  }

  void Seek(off_t pos, int seek_direction) {
    const off_t result = lseek(file_, pos, seek_direction);
    if (result < 0)
      throw std::runtime_error("I/O error: could not seek through file '" +
                               filename_ + "'");
  }

  void ReadData(unsigned char* data, uint64_t size) {
    const int result = ::read(file_, data, size);
    if (result < 0)
      throw std::runtime_error("I/O error: could not read from file '" +
                               filename_ + "'");
  }

  void WriteData(const unsigned char* data, uint64_t size) {
    const int result = write(file_, data, size);
    if (result < 0)
      throw std::runtime_error("I/O error: could not write to file '" +
                               filename_ + "'");
  }
  bool IsOpen() const { return file_ >= 0; }
  /**
   * Offset of the first row in the file. When using Seek() to move to
   * a row, this number should be added to the offset.
   */
  uint64_t DataLocation() const { return data_location_; }
  const std::string& Filename() const { return filename_; }
  /**
   * Number of bytes reserved for an optional header.
   */
  uint64_t HeaderSize() const { return data_location_ - sizeof(stride_); }
  /**
   * Write an optional extra header to the file. When creating the file,
   * the requested space is saved to store this header.
   * @param data An array equal to the size of the header given
   * in the @ref CreateNew() and @ref OpenExisting() calls.
   */
  void WriteHeader(const unsigned char* data) {
    Seek(kPrivateHeaderSize, SEEK_SET);
    WriteData(data, data_location_ - kPrivateHeaderSize);
  }
  /**
   * Read an optional extra header to the file. @see WriteHeader().
   */
  void ReadHeader(unsigned char* data) {
    Seek(kPrivateHeaderSize, SEEK_SET);
    ReadData(data, data_location_ - kPrivateHeaderSize);
  }
  /**
   * Total number of rows stored in this file.
   */
  uint64_t NRows() const { return n_rows_; }
  void SetNRows(uint64_t new_n_rows) { n_rows_ = new_n_rows; }
  /**
   * Total number of bytes in one row. This value is also stored in the file,
   * and is read from the file in @ref OpenExisting().
   */
  uint64_t Stride() const { return stride_; }
  /**
   * Set the number of bytes per row for this file. This changes the format
   * of the file, and because of this the file is emptied.
   */
  void SetStride(uint64_t new_stride) {
    Truncate(0);
    n_rows_ = 0;
    stride_ = new_stride;
    Seek(0, SEEK_SET);
    WriteData(reinterpret_cast<unsigned char*>(&stride_), sizeof(stride_));
  }

  /**
   * Adds a given number of rows to the back of the file.
   */
  void AddRows(uint64_t n_rows) { SetNRows(NRows() + n_rows); }

  /**
   * Deletes the last row.
   */
  void DeleteRow() {
    if (NRows() > 0) {
      SetNRows(NRows() - 1);
    }
  }

 private:
  // The "C" file API is used because we need to use (f)truncate, which is not
  // available from the C++ fstream API.
  int file_ = -1;
  uint64_t n_rows_ = 0;
  uint64_t stride_ = 0;
  static constexpr uint64_t kPrivateHeaderSize = sizeof(stride_);
  uint64_t data_location_ = kPrivateHeaderSize;
  std::string filename_;
};

}  // namespace casacore

#endif
