#ifndef CASACORE_UVW_STORAGE_MANAGER_H_
#define CASACORE_UVW_STORAGE_MANAGER_H_

#include <casacore/tables/DataMan/DataManager.h>

#include <casacore/casa/Containers/Record.h>

#include "AntennaPairFile.h"

#include <memory>
#include <stdexcept>

namespace casacore {

class AntennaPairStManColumn;

/**
 * A storage manager that saves the UVW with (lossless) compression.
 */
class AntennaPairStMan final : public DataManager {
 public:
  AntennaPairStMan(const String &, const Record &);

  /**
   * The columns are not copied: the new manager will be empty.
   */
  AntennaPairStMan(const AntennaPairStMan &source);

  ~AntennaPairStMan() noexcept;

  AntennaPairStMan &operator=(const AntennaPairStMan &source) = delete;

  DataManager *clone() const final { return new AntennaPairStMan(*this); }

  static DataManager *makeObject(const String &name, const Record &spec) {
    return new AntennaPairStMan(name, spec);
  }

  String dataManagerType() const final { return "AntennaPairStMan"; }

  Record dataManagerSpec() const final { return Record(); }

 private:
  Bool flush(AipsIO &, Bool) final { return false; }

  void create64(rownr_t nRow) final;

  rownr_t open64(rownr_t nRow, AipsIO &) final;

  DataManagerColumn *makeScalarColumn(const String &name, int dataType,
                                      const String &dataTypeID) final;

  DataManagerColumn *makeDirArrColumn(const String &, int,
                                      const String &) final {
    throw std::runtime_error(
        "makeDirArrColumn() called on a AntennaPairStMan. AntennaPairStMan can "
        "only make scalar "
        "columns");
  }

  DataManagerColumn *makeIndArrColumn(const String &, int,
                                      const String &) final {
    throw std::runtime_error(
        "makeIndArrColumn() called on a AntennaPairStMan. AntennaPairStMan can "
        "only make "
        "scalar columns");
  }

  rownr_t resync64(rownr_t nRow) final { return nRow; }

  void deleteManager() final;

  // Reopen the storage manager files for read/write.
  void reopenRW() final {}

  // Add rows to the storage manager.
  void addRow64(rownr_t nrrow) final;

  // Delete a row from all columns.
  void removeRow64(rownr_t row_nr) final;

  // Do the final addition of a column.
  void addColumn(DataManagerColumn *) final;

  // Remove a column from the data file.
  void removeColumn(DataManagerColumn *) final;

  std::string name_;
  std::array<std::unique_ptr<AntennaPairStManColumn>, 2> columns_;
  AntennaPairFile file_;
};

}  // namespace casacore

#endif
