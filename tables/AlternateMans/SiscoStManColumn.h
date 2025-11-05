#ifndef CASACORE_SISCO_ST_MAN_COLUMN_H_
#define CASACORE_SISCO_ST_MAN_COLUMN_H_

#include <casacore/tables/DataMan/StManColumn.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/IPosition.h>

#include <casacore/tables/Tables/ScalarColumn.h>

#include "SiscoReader.h"
#include "SiscoWriter.h"
#include "ShapesFileReader.h"
#include "ShapesFileWriter.h"

#include <filesystem>
#include <optional>

namespace casacore {

class SiscoStMan;

/**
 * Base class for columns of the StokesIStMan.
 * @author André Offringa
 */
class SiscoStManColumn final : public StManColumn {
 public:
  /**
   * Constructor, to be overloaded by subclass.
   * @param parent The parent stman to which this column belongs.
   * @param dtype The column's type as defined by Casacore.
   */
  explicit SiscoStManColumn(SiscoStMan &parent, DataType dtype)
      : StManColumn(dtype), parent_(parent) {
    if (dtype != casacore::TpComplex) {
      throw std::runtime_error(
          "Sisco storage manager column can only be used for a data column "
          "with single precision complex values");
    }
  }

  /**
   * Whether this column is writable
   * @returns @c true
   */
  bool isWritable() const final { return true; }

  bool canChangeShape() const final { return true; }

  void setShape(rownr_t, const IPosition &) final {
    // Shape is implied from the array; explicit setting of the shape is not
    // required.
  }
  void setShape(unsigned, const IPosition &) final {}

  bool isShapeDefined(rownr_t row) final {
    if ((writer_ && row >= current_row_) || !file_exists_) {
      return false;
    } else {
      return true;
    }
  }
  bool isShapeDefined(unsigned) final { return false; }

  /** Set the dimensions of values in this column. */
  void setShapeColumn(const IPosition &shape) final {
    if (shape.size() != 2) {
      throw std::runtime_error(
          "Sisco storage manager is used for a column with " +
          std::to_string(shape.size()) +
          " dimensions, but it can only be used for "
          "columns with exactly 2 dimensions");
    }
    // This call is ignored; shape will always be determined from array size.
  }

  /** Get the dimensions of the values in a particular row.
   * @param rownr The row to get the shape for. */
  IPosition shape(rownr_t row) final {
    if ((writer_ && row >= current_row_) || !file_exists_) {
      return IPosition{0, 0};
    } else {
      if (!reader_ || row < current_row_) {
        OpenReader();
      }
      while (current_row_ < row) {
        SkipRow();
      }
      return shape_buffer_[shape_read_position_];
    }
  }
  IPosition shape(unsigned row) final {
    return shape(static_cast<rownr_t>(row));
  }

  /**
   * Read the values for a particular row.
   * @param row The row number to get the values for.
   * @param dataPtr The array of values.
   */
  void getArrayV(rownr_t row, ArrayBase &dataPtr) final {
    Array<std::complex<float>> &array =
        static_cast<Array<std::complex<float>> &>(dataPtr);
    if (!reader_ || row < current_row_) {
      OpenReader();
    }
    while (current_row_ < row) {
      SkipRow();
    }

    const IPosition &shape = shape_buffer_[shape_read_position_];
    shape_read_position_ = (shape_read_position_ + 1) % shape_buffer_.size();
    if (shape.size() >= 2) {
      const int n_polarizations = shape[0];
      const size_t n_channels = shape[1];

      if (n_channels) {
        bool ownership;
        Complex *storage = array.getStorage(ownership);
        buffer_.resize(n_channels);
        for (int polarization = 0; polarization != n_polarizations;
             ++polarization) {
          reader_->GetNextResult(buffer_);
          for (size_t channel = 0; channel != n_channels; ++channel) {
            storage[channel * n_polarizations + polarization] =
                buffer_[channel];
          }
        }
        array.putStorage(storage, ownership);
      }
    }

    RequestOneMoreRow();
    ++current_row_;
  }

  /**
   * Write values into a particular row.
   * @param row The row number to write the values to.
   * @param dataPtr The data pointer.
   */
  void putArrayV(rownr_t row, const ArrayBase &dataPtr) final {
    const Array<std::complex<float>> &array =
        static_cast<const Array<std::complex<float>> &>(dataPtr);
    if (!writer_ || row < current_row_) {
      OpenWriter();
    }
    while (current_row_ < row) {
      WriteEmptyRow();
    }

    const int field_id = field_id_column_(current_row_);
    const int data_desc_id = data_desc_id_column_(current_row_);
    const int antenna1 = antenna1_column_(current_row_);
    const int antenna2 = antenna2_column_(current_row_);
    if (array.shape().size() >= 2) {
      const int n_polarizations = array.shape()[0];
      const size_t n_channels = array.shape()[1];

      if (n_channels) {
        bool ownership;
        const std::complex<float> *storage = array.getStorage(ownership);
        buffer_.resize(n_channels);
        for (int polarization = 0; polarization != n_polarizations;
             ++polarization) {
          const size_t baseline_id = GetBaselineId(
              field_id, data_desc_id, antenna1, antenna2, polarization);
          for (size_t channel = 0; channel != n_channels; ++channel) {
            buffer_[channel] =
                storage[channel * n_polarizations + polarization];
          }
          writer_->Write(baseline_id, buffer_);
        }
        array.freeStorage(storage, ownership);
      }
    }

    ++current_row_;
    shapes_writer_->Write(array.shape());
  }

  void Prepare();

 private:
  SiscoStManColumn(const SiscoStManColumn &source) = delete;
  void operator=(const SiscoStManColumn &source) = delete;

  void Reset() {
    reader_.reset();
    shapes_reader_.reset();
    writer_.reset();
    shapes_writer_.reset();
  }

  void OpenWriter() {
    Reset();
    writer_.emplace(parent_.fileName(), parent_.PredictLevel(),
                    parent_.DeflateLevel());
    char header_buffer[kHeaderSize];
    std::fill_n(header_buffer, kHeaderSize, 0);
    std::copy_n(kMagic, kMagicSize, &header_buffer[0]);
    std::copy_n(reinterpret_cast<const char *>(&kVersionMajor), 2,
                &header_buffer[kMagicSize]);
    std::copy_n(reinterpret_cast<const char *>(&kVersionMinor), 2,
                &header_buffer[kMagicSize + 2]);
    std::span<const std::byte> header(
        reinterpret_cast<const std::byte *>(header_buffer), kHeaderSize);
    writer_->Open(header);

    shapes_writer_.emplace(ShapesFilename());

    current_row_ = 0;
    baseline_ids_.clear();
    baseline_count_ = 0;
  }

  void OpenReader() {
    Reset();
    reader_.emplace(parent_.fileName());
    char header_buffer[kHeaderSize];
    std::span<std::byte> header(reinterpret_cast<std::byte *>(header_buffer),
                                kHeaderSize);
    reader_->Open(header);
    char magic_tag[kMagicSize];
    short version_major;
    short version_minor;
    std::copy_n(&header_buffer[0], kMagicSize, magic_tag);
    std::copy_n(&header_buffer[kMagicSize], 2,
                reinterpret_cast<char *>(&version_major));
    std::copy_n(&header_buffer[kMagicSize + 2], 2,
                reinterpret_cast<char *>(&version_minor));
    if (version_major != kMajorVersion) {
      throw std::runtime_error(
          "The file on disk is written as a Sisco version " +
          std::to_string(version_major) +
          " file, whereas this Casacore version supports only version " +
          std::to_string(kMajorVersion));
    }
    shapes_reader_.emplace(ShapesFilename());

    current_row_ = 0;
    baseline_ids_.clear();
    baseline_count_ = 0;
    // Always request half of the requests that fit in the buffer of
    // SiscoReader, so that SiscoReader can preprocess requests using multiple
    // threads. Every time a row is read/skipped, another row is requested.
    shape_buffer_.resize(reader_->GetRequestBufferSize() / 2);
    shape_read_position_ = 0;
    shape_write_position_ = 0;
    current_shape_reading_row_ = 0;
    for (size_t i = 0; i != shape_buffer_.size(); ++i) {
      RequestOneMoreRow();
    }
  }

  void RequestOneMoreRow() {
    const casacore::IPosition shape = shapes_reader_->Read();
    if (!shapes_reader_->Eof() && shape.size() >= 2) {
      const int field_id = field_id_column_(current_shape_reading_row_);
      const int data_desc_id = data_desc_id_column_(current_shape_reading_row_);
      const int antenna1 = antenna1_column_(current_shape_reading_row_);
      const int antenna2 = antenna2_column_(current_shape_reading_row_);
      const int n_polarizations = shape[0];
      const int n_channels = shape[1];
      for (int polarization = 0; polarization != n_polarizations;
           ++polarization) {
        const size_t baseline_id = GetBaselineId(
            field_id, data_desc_id, antenna1, antenna2, polarization);
        reader_->Request(baseline_id, n_channels);
      }
    }
    shape_buffer_[shape_write_position_] = shape;
    shape_write_position_ = (shape_write_position_ + 1) % shape_buffer_.size();
    current_shape_reading_row_++;
  }

  size_t GetBaselineId(int field_id, int data_desc_id, int antenna1,
                       int antenna2, int polarization) {
    const std::array<int, 5> baseline{field_id, data_desc_id, antenna1,
                                      antenna2, polarization};
    std::map<std::array<int, 5>, size_t>::const_iterator iterator =
        baseline_ids_.find(baseline);
    if (iterator == baseline_ids_.end()) {
      iterator = baseline_ids_.emplace(baseline, baseline_count_).first;
      ++baseline_count_;
    }
    return iterator->second;
  }

  std::string ShapesFilename() const {
    return parent_.fileName() + kShapesExtension;
  }

  void WriteEmptyRow() {
    shapes_writer_->Write(IPosition{0, 0});
    ++current_row_;
  }

  void SkipRow() {
    const casacore::IPosition &shape = shape_buffer_[shape_read_position_];
    shape_read_position_ = (shape_read_position_ + 1) % shape_buffer_.size();
    if (shape.size() >= 2) {
      const int n_polarizations = shape[0];
      const int n_channels = shape[1];
      if (n_channels) {
        buffer_.resize(n_channels);
        for (int polarization = 0; polarization != n_polarizations;
             ++polarization) {
          reader_->GetNextResult(buffer_);
        }
      }
    }
    RequestOneMoreRow();
    ++current_row_;
  }

  static constexpr size_t kHeaderSize = 20;
  static constexpr char kMagic[] = "Sisco\0\0\0";
  static constexpr size_t kMagicSize = 8;
  static constexpr uint16_t kVersionMajor = 2;
  static constexpr uint16_t kVersionMinor = 0;
  static constexpr char kShapesExtension[] = "-shapes";

  ScalarColumn<int> field_id_column_;
  ScalarColumn<int> data_desc_id_column_;
  ScalarColumn<int> antenna1_column_;
  ScalarColumn<int> antenna2_column_;

  SiscoStMan &parent_;
  std::optional<sisco::SiscoWriter> writer_;
  std::optional<sisco::SiscoReader> reader_;
  std::optional<ShapesFileWriter> shapes_writer_;
  std::optional<ShapesFileReader> shapes_reader_;
  // A circular buffer to store the already read shapes
  std::vector<IPosition> shape_buffer_;
  size_t shape_read_position_ = 0;
  size_t shape_write_position_ = 0;
  rownr_t current_shape_reading_row_ = 0;
  rownr_t current_row_ = 0;
  std::vector<std::complex<float>> buffer_;
  std::map<std::array<int, 5>, size_t> baseline_ids_;
  size_t baseline_count_;
  bool file_exists_ = false;
};

}  // namespace casacore

#include "SiscoStMan.h"

namespace casacore {

void SiscoStManColumn::Prepare() {
  Table &table = parent_.table();
  field_id_column_ = ScalarColumn<int>(table, "FIELD_ID");
  data_desc_id_column_ = ScalarColumn<int>(table, "DATA_DESC_ID");
  antenna1_column_ = ScalarColumn<int>(table, "ANTENNA1");
  antenna2_column_ = ScalarColumn<int>(table, "ANTENNA2");
  file_exists_ = std::filesystem::exists(parent_.fileName()) &&
                 std::filesystem::exists(ShapesFilename());
}

}  // namespace casacore

#endif
