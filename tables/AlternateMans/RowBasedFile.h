#ifndef CASACORE_ROW_BASED_FILE_H_
#define CASACORE_ROW_BASED_FILE_H_

#include <fcntl.h>
#include <unistd.h>

#include <array>
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
    rhs.data_location_ = kWriterPrivateHeaderSize;
    rhs.filename_ = "";
  }

  /**
   * Create or overwrite a new columnar file on disk
   */
  RowBasedFile(const std::string& filename, uint64_t header_size,
               uint64_t stride)
      : stride_(stride), filename_(filename) {
    file_ = open(filename.c_str(), O_CREAT | O_RDWR | O_TRUNC,
                 S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
    if (file_ < 0)
      throw std::runtime_error("I/O error: could not create new file '" +
                               filename + "'");
    data_location_ = header_size + kWriterPrivateHeaderSize;
    WritePrivateHeader();
  }

  /**
   * Open an existing columnar file
   */
  RowBasedFile(const std::string& filename, size_t header_size)
      : filename_(filename) {
    file_ = open(filename.c_str(), O_RDWR);
    if (file_ < 0)
      throw std::runtime_error("I/O error: could not open file '" + filename +
                               "'");
    uint32_t magic_tag;
    ReadData(reinterpret_cast<unsigned char*>(&magic_tag), sizeof(uint32_t));
    if (magic_tag != kMagicFileTag) {
      throw std::runtime_error(
          "Could not read file " + filename +
          ": file does not obey the Casacore row-based file format: either the "
          "file is damaged, or this is not a Casacore row-based file");
    }

    uint32_t file_version;
    ReadData(reinterpret_cast<unsigned char*>(&file_version), sizeof(uint32_t));
    const uint32_t major_version = (file_version & 0xFF00) >> 8;
    constexpr uint32_t kWriterMajorVersion = (kFileVersion & 0xFF00) >> 8;
    if (major_version > kWriterMajorVersion) {
      throw std::runtime_error("The file " + filename +
                               " requires a reader of at least major version " +
                               std::to_string(major_version) +
                               ". This reader is for major version " +
                               std::to_string(kWriterMajorVersion) + ".");
    }

    // Combine reading of private header size, stride and user header size in
    // one read call.
    std::array<unsigned char, sizeof(uint32_t) + 2 * sizeof(uint64_t)>
        rest_of_private_header;
    ReadData(rest_of_private_header.data(), rest_of_private_header.size());
    private_header_size_ =
        reinterpret_cast<uint32_t&>(rest_of_private_header.data()[0]);
    stride_ = reinterpret_cast<uint64_t&>(
        rest_of_private_header.data()[sizeof(uint32_t)]);
    size_t file_user_header_size = reinterpret_cast<uint64_t&>(
        rest_of_private_header.data()[sizeof(uint32_t) + sizeof(uint64_t)]);
    if (file_user_header_size != header_size) {
      throw std::runtime_error("Error reading file " + filename +
                               ": inconsistent size of private header");
    }

    data_location_ = header_size + private_header_size_;
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
   * cause undefined behaviour, until the class is assigned to
   * a new instance.
   */
  void Close() {
    if (IsOpen()) {
      Truncate(NRows());
      int result = close(file_);
      file_ = -1;
      if (result < 0)
        throw std::runtime_error("Could not close file " + filename_);
      n_rows_ = 0;
      stride_ = 0;
      filename_ = "";
    }
  }

  void Truncate(uint64_t n_rows) {
    const int result = ftruncate(file_, n_rows_ * stride_ + data_location_);
    if (result < 0) {
      throw std::runtime_error(
          "I/O error: could not truncate file '" + filename_ + "' to have " +
          std::to_string(n_rows) + " rows: " + ErrorString());
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
  uint64_t HeaderSize() const { return data_location_ - private_header_size_; }
  /**
   * Write an optional extra header to the file. When creating the file,
   * the requested space is saved to store this header.
   * @param data An array equal to the size of the header given
   * in the @ref CreateNew() and @ref OpenExisting() calls.
   */
  void WriteHeader(const unsigned char* data) {
    Seek(private_header_size_, SEEK_SET);
    WriteData(data, data_location_ - private_header_size_);
  }
  /**
   * Read an optional extra header to the file. @see WriteHeader().
   */
  void ReadHeader(unsigned char* data) {
    Seek(private_header_size_, SEEK_SET);
    ReadData(data, data_location_ - private_header_size_);
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
    const uint64_t header_size = HeaderSize();
    Truncate(0);
    n_rows_ = 0;
    stride_ = new_stride;
    Seek(0, SEEK_SET);
    data_location_ = header_size + kWriterPrivateHeaderSize;
    WritePrivateHeader();
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
  void WritePrivateHeader() {
    // Collect entire private header in one write call
    std::array<unsigned char, kWriterPrivateHeaderSize> private_header_buffer;
    reinterpret_cast<uint32_t&>(private_header_buffer[0]) = kMagicFileTag;
    reinterpret_cast<uint32_t&>(private_header_buffer[4]) = kFileVersion;
    reinterpret_cast<uint32_t&>(private_header_buffer[8]) =
        kWriterPrivateHeaderSize;
    reinterpret_cast<uint64_t&>(private_header_buffer[12]) = stride_;
    reinterpret_cast<uint64_t&>(private_header_buffer[20]) = HeaderSize();

    WriteData(private_header_buffer.data(), private_header_buffer.size());
  }
  
  /**
   * The size of the private header that the writer creates for the current file
   * format. This number is written into the file. Whenever a file is read, the
   * number written into the file should be used to skip over the header. That
   * way, members can be added to the header that can still be read by older
   * readers. The header consists of:
   * - u32: "Crbf" (magic file tag)
   * - u32: file version
   * - u32: private header size
   * - u64: stride
   * - u64: full header size (private header + user header)
   */
  inline constexpr static uint32_t kWriterPrivateHeaderSize =
      3 * sizeof(uint32_t) + 2 * sizeof(uint64_t);

  /**
   * First four bytes of a file. This spells out "Crbf" when stored as a little
   * endian number, which stands for "Casacore Row-based file". Files without
   * this magic number in the first four bytes are rejected.
   *
   * This also makes sure that a file written on a little endian machine is
   * rejected by a big endian machine, and vice versa. Because big endian
   * machines are extremely rare for astronomical processing, no effort is made
   * to make it interchangable at this point of time. The official format is
   * declared to use little endian numbers.
   */
  inline constexpr static uint32_t kMagicFileTag = 0x66627243;

  /**
   * Version of this file, in format 0xaabb, where aa is the major version and
   * bb is the minor version. The major version is checked against the version
   * of the reader: if the file has a higher major version than the reader, the
   * file is rejected. The minor version is not checked, thus an increase in
   * minor version indicates a change in file format that is still readable by
   * older readers.
   */
  inline constexpr static uint32_t kFileVersion = 0x0100;

  static std::string ErrorStringHelper(int result_value, char* buffer) {
    if (result_value == 0)
      return buffer;
    else
      return "Unknown error";
  }
  
  static std::string ErrorStringHelper(char* returned_buffer,
                                       char* /*supplied_buffer*/) {
    return std::string(returned_buffer);
  }
  
  static std::string ErrorString() {
    char errstr[128];
    // This is a small trick to allow both versions of strerror_r: by using
    // function overloading, the right behaviour is picked.
    return ErrorStringHelper(strerror_r(errno, errstr, 128), errstr);
  }

  // The "C" file API is used because we need to use (f)truncate, which is not
  // available from the C++ fstream API.
  int file_ = -1;
  uint32_t private_header_size_ = kWriterPrivateHeaderSize;
  uint64_t n_rows_ = 0;
  uint64_t stride_ = 0;
  /**
   * This variable is also used to set/calculate the header size, using the
   * relation: data_location_ = private_header_size_ + header_size.
   */
  uint64_t data_location_ = kWriterPrivateHeaderSize;
  std::string filename_;
};

}  // namespace casacore

#endif
