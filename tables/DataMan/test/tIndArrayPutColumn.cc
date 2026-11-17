#include <boost/test/unit_test.hpp>

#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/DataMan/IncrementalStMan.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/namespace.h>

#include <filesystem>

using namespace casacore;

namespace {

void checkPutColumnReusesArrays(const std::string& tableName, DataManager& dataManager) {
  std::filesystem::remove_all(tableName);
  {
    TableDesc tableDesc("", "1", TableDesc::Scratch);
    tableDesc.addColumn(ArrayColumnDesc<double>("array", 1));
    SetupNewTable setup(tableName, tableDesc, Table::New);
    setup.bindAll(dataManager);
    Table table(setup, 10);
    ArrayColumn<double> column(table, "array");
    for (unsigned int row = 0; row < 10; ++row) {
      column.put(row, Vector<double>(4, row));
    }
  }
  const std::string arrayFile = tableName + "/table.f0i";
  const size_t initialSize = std::filesystem::file_size(arrayFile);
  {
    Table table(tableName, Table::Update);
    ArrayColumn<double> column(table, "array");
    for (int i = 0; i < 20; ++i) {
      column.putColumn(Matrix<double>(4, 10, 200.0 + i));
    }
  }
  BOOST_CHECK_EQUAL(std::filesystem::file_size(arrayFile), initialSize);
  {
    Table table(tableName);
    BOOST_CHECK(allEQ(ArrayColumn<double>(table, "array").getColumn(), 219.0));
  }
  std::filesystem::remove_all(tableName);
}

}  // namespace

BOOST_AUTO_TEST_CASE(ism_put_column_reuses_indirect_arrays) {
  IncrementalStMan ism("ISM");
  checkPutColumnReusesArrays("tIndArrayPutColumn_tmp.ism", ism);
}

BOOST_AUTO_TEST_CASE(stmanaipsio_put_column_reuses_indirect_arrays) {
  StManAipsIO aipsio;
  checkPutColumnReusesArrays("tIndArrayPutColumn_tmp.aipsio", aipsio);
}
