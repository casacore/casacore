#include <boost/test/unit_test.hpp>

#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/DataMan/IncrementalStMan.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/namespace.h>

#include <filesystem>

using namespace casacore;

BOOST_AUTO_TEST_CASE(repeated_same_shape_put_reuses_indirect_array) {
  const std::string tableName = "tISMIndArrayRefCount_tmp.data";
  std::filesystem::remove_all(tableName);

  TableDesc tableDesc("", "1", TableDesc::Scratch);
  tableDesc.addColumn(ArrayColumnDesc<float>("array"));
  SetupNewTable setup(tableName, tableDesc, Table::New);
  IncrementalStMan ism("ISM");
  setup.bindAll(ism);
  Table table(setup, 3);

  ArrayColumn<float> column(table, "array");
  Vector<float> value0(4);
  value0 = 0.0f;
  Vector<float> value1(16);
  value1 = 1.0f;
  Vector<float> value2(8);
  value2 = 2.0f;
  column.put(0, value0);
  column.put(1, value1);
  column.put(2, value2);
  table.flush(true);

  const std::string arrayFile = tableName + "/table.f0i";
  const size_t initialSize = std::filesystem::file_size(arrayFile);

  // Changing the shape allocates a new record. Its initial reference count
  // must remain one so subsequent puts can reuse it.
  value1.resize(32);
  value1 = 3.0f;
  column.put(1, value1);
  table.flush(true);
  const size_t changedShapeSize = std::filesystem::file_size(arrayFile);
  BOOST_CHECK_GT(changedShapeSize, initialSize);

  for (int i = 0; i < 20; ++i) {
    value1 = static_cast<float>(i);
    column.put(1, value1);
  }
  table.flush(true);

  BOOST_CHECK_EQUAL(std::filesystem::file_size(arrayFile), changedShapeSize);
  Vector<float> result;
  column.get(1, result);
  BOOST_CHECK_EQUAL(result(0), 19.0f);
  BOOST_CHECK_EQUAL(result.nelements(), 32);

  table = Table();
  std::filesystem::remove_all(tableName);
}
