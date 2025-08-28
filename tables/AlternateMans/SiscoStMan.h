#ifndef CASACORE_SISCO_ST_MAN_H_
#define CASACORE_SISCO_ST_MAN_H_

#include <casacore/tables/DataMan/DataManager.h>

#include <memory>

#ifndef DOXYGEN_SHOULD_SKIP_THIS
extern "C" {
#endif
void register_siscostman();
#ifndef DOXYGEN_SHOULD_SKIP_THIS
}
#endif

namespace casacore {

class SiscoStManColumn;

/**
 * The Stokes I storage manager behaves like a full set of (4) polarizations but
 * only stores the Stokes I value on disk.
 */
class SiscoStMan final : public casacore::DataManager {
 public:
  /**
   * This constructor is called by Casacore when it needs to create a
   * SiscoStMan. When Casacore loads an SiscoStMan for an existing MS, the
   * "spec" parameter will be empty, thus the class should initialize its
   * properties by reading them from the file. The @p spec is used to make a new
   * storage manager with specs similar to another one.
   * @param name Name of this storage manager.
   * @param spec Specs to initialize this class with.
   */
  SiscoStMan(const casacore::String &name, const casacore::Record &spec);

  /**
   * Copy constructor that initializes a storage manager with similar specs.
   * The columns are not copied: the new manager will be empty.
   */
  SiscoStMan(const SiscoStMan &source);

  ~SiscoStMan() noexcept;

  /**
   * This SiscoStMan takes the settings of the source (but not the
   * columns and/or data).
   */
  SiscoStMan &operator=(const SiscoStMan &source) = delete;

  casacore::DataManager *clone() const final { return new SiscoStMan(*this); }
  /**
   * Create an object with given name and spec.
   * This methods gets registered in the DataManager "constructor" map.
   * The caller has to delete the object. New class will be
   * initialized via @ref SiscoStMan(const casacore::String& name, const
   * casacore::Record& spec).
   * @returns A SiscoStMan with given specs.
   */
  static casacore::DataManager *makeObject(const casacore::String &name,
                                           const casacore::Record &spec) {
    return new SiscoStMan(name, spec);
  }

  casacore::String dataManagerType() const final { return "SiscoStMan"; }

  casacore::Record dataManagerSpec() const final;

  bool canAddRow() const final { return true; }

  bool canRemoveRow() const final { return false; }

  bool canAddColumn() const final { return !column_; }

  bool canRemoveColumn() const final { return static_cast<bool>(column_); }

  /**
   * This function makes the SiscoStMan known to Casacore.
   */
  static void registerClass();

  int DeflateLevel() const { return deflate_level_; }
  int PredictLevel() const { return predict_level_; }

 protected:
 private:
  friend class SiscoStManColumn;

  casacore::Bool flush(casacore::AipsIO &,
                       [[maybe_unused]] casacore::Bool doFsync) final {
    return false;
  }

  // Let the storage manager create files as needed for a new table.
  // This allows a column with an indirect array to create its file.
  void create64(casacore::rownr_t nRow) final;

  // Open the storage manager file for an existing table.
  // Return the number of rows in the data file.
  casacore::rownr_t open64(casacore::rownr_t nRow, casacore::AipsIO &) final;

  // Create a column in the storage manager on behalf of a table column.
  // The caller will NOT delete the newly created object.
  // Create a scalar column.
  casacore::DataManagerColumn *makeScalarColumn(
      const casacore::String &name, int dataType,
      const casacore::String &dataTypeID) final;

  // Create a direct array column.
  casacore::DataManagerColumn *makeDirArrColumn(
      const casacore::String &name, int dataType,
      const casacore::String &dataTypeID) final;

  // Create an indirect array column.
  casacore::DataManagerColumn *makeIndArrColumn(
      const casacore::String &name, int dataType,
      const casacore::String &dataTypeID) final;

  casacore::rownr_t resync64(casacore::rownr_t nRow) final;

  void deleteManager() final;

  // Prepare the columns, let the data manager initialize itself further.
  // Prepare is called after create/open has been called for all
  // columns. In this way one can be sure that referenced columns
  // are read back and partly initialized.
  void prepare() final;

  // Reopen the storage manager files for read/write.
  void reopenRW() final;

  // Add rows to the storage manager.
  void addRow64(casacore::rownr_t nrrow) final;

  // Delete a row from all columns.
  void removeRow64(casacore::rownr_t row_nr) final;

  // Do the final addition of a column.
  void addColumn(casacore::DataManagerColumn *) final;

  // Remove a column from the data file.
  void removeColumn(casacore::DataManagerColumn *) final;

  std::string name_;
  // The item-type needs to be a pointer, because casacore::StManColumn
  // does not have move construct/assignment.
  std::unique_ptr<SiscoStManColumn> column_;
  int deflate_level_ = 9;
  int predict_level_ = 2;
};

}  // namespace casacore

#endif
