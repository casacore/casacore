#ifndef CASACORE_STOKES_I_STORAGE_MANAGER_H_
#define CASACORE_STOKES_I_STORAGE_MANAGER_H_

#include <casacore/tables/DataMan/DataManager.h>

#include <casacore/casa/Containers/Record.h>

#include "BufferedColumnarFile.h"

#ifndef DOXYGEN_SHOULD_SKIP_THIS
extern "C" {
#endif
void register_stokesistman();
#ifndef DOXYGEN_SHOULD_SKIP_THIS
}
#endif

namespace casacore {

class StokesIStManColumn;

/**
 * The Stokes I storage manager behaves like a full set of (4) polarizations but
 * only stores the Stokes I value on disk.
 */
class StokesIStMan final : public casacore::DataManager {
 public:
  /**
   * This constructor is called by Casacore when it needs to create a
   * StokesIStMan. When Casacore loads an StokesIStMan for an existing MS, the
   * "spec" parameter will be empty, thus the class should initialize its
   * properties by reading them from the file. The @p spec is used to make a new
   * storage manager with specs similar to another one.
   * @param name Name of this storage manager.
   * @param spec Specs to initialize this class with.
   */
  StokesIStMan(const casacore::String &name, const casacore::Record &spec);

  /**
   * Copy constructor that initializes a storage manager with similar specs.
   * The columns are not copied: the new manager will be empty.
   */
  StokesIStMan(const StokesIStMan &source);

  ~StokesIStMan() noexcept;

  /**
   * This StokesIStMan takes the settings of the source (but not the
   * columns and/or data).
   */
  StokesIStMan &operator=(const StokesIStMan &source) = delete;

  casacore::DataManager *clone() const final { return new StokesIStMan(*this); }
  /**
   * Create an object with given name and spec.
   * This methods gets registered in the DataManager "constructor" map.
   * The caller has to delete the object. New class will be
   * initialized via @ref DyscoStMan(const casacore::String& name, const
   * casacore::Record& spec).
   * @returns A DyscoStMan with given specs.
   */
  static casacore::DataManager *makeObject(const casacore::String &name,
                                           const casacore::Record &spec) {
    return new StokesIStMan(name, spec);
  }

  casacore::String dataManagerType() const final { return "StokesIStMan"; }

  casacore::Record dataManagerSpec() const final;

  casacore::Bool canAddRow() const final { return true; }

  casacore::Bool canRemoveRow() const final { return true; }

  casacore::Bool canAddColumn() const final { return true; }

  casacore::Bool canRemoveColumn() const final { return true; }

  /**
   * This function makes the StokesIStMan known to Casacore.
   */
  static void registerClass();

 protected:
 private:
  friend class StokesIStManColumn;

  uint64_t CalculateAndUpdateStride();

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
  std::vector<std::unique_ptr<StokesIStManColumn>> columns_;
  BufferedColumnarFile file_;
};

}  // namespace casacore

#endif
