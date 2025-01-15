#include <boost/test/unit_test.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/directory.hpp>

#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScaColDesc.h>

#include "../dyscostman.h"

using namespace casacore;
using namespace dyscostman;

BOOST_AUTO_TEST_SUITE(dyscostman)

casacore::Record GetDyscoSpec() {
  casacore::Record dyscoSpec;
  dyscoSpec.define("distribution", "TruncatedGaussian");
  dyscoSpec.define("normalization", "AF");
  dyscoSpec.define("distributionTruncation", 2.0);
  dyscoSpec.define("dataBitCount", 10);
  dyscoSpec.define("weightBitCount", 12);
  return dyscoSpec;
}

struct TestTableFixture {
  explicit TestTableFixture(size_t nAnt) {
    casacore::TableDesc tableDesc;
    IPosition shape(2, 1, 1);
    casacore::ArrayColumnDesc<casacore::Complex> columnDesc(
        "DATA", "", "DyscoStMan", "", shape);
    columnDesc.setOptions(casacore::ColumnDesc::Direct |
                          casacore::ColumnDesc::FixedShape);
    casacore::ScalarColumnDesc<int> ant1Desc("ANTENNA1"), ant2Desc("ANTENNA2"),
        fieldDesc("FIELD_ID"), dataDescIdDesc("DATA_DESC_ID");
    casacore::ScalarColumnDesc<double> timeDesc("TIME");
    tableDesc.addColumn(columnDesc);
    tableDesc.addColumn(ant1Desc);
    tableDesc.addColumn(ant2Desc);
    tableDesc.addColumn(fieldDesc);
    tableDesc.addColumn(dataDescIdDesc);
    tableDesc.addColumn(timeDesc);
    casacore::SetupNewTable setupNewTable("TestTable", tableDesc,
                                          casacore::Table::New);

    register_dyscostman();
    DataManagerCtor dyscoConstructor = DataManager::getCtor("DyscoStMan");
    std::unique_ptr<DataManager> dysco(
        dyscoConstructor("DATA_dm", GetDyscoSpec()));
    setupNewTable.bindColumn("DATA", *dysco);
    casacore::Table newTable(setupNewTable);

    size_t a1 = 0, a2 = 1;
    double time = 10.0;
    const size_t nRow = 2 * nAnt * (nAnt - 1) / 2;
    newTable.addRow(nRow);
    casacore::ScalarColumn<int> a1Col(newTable, "ANTENNA1"),
        a2Col(newTable, "ANTENNA2"), fieldCol(newTable, "FIELD_ID"),
        dataDescIdCol(newTable, "DATA_DESC_ID");
    casacore::ScalarColumn<double> timeCol(newTable, "TIME");
    for (size_t i = 0; i != nRow; ++i) {
      a1Col.put(i, a1);
      a2Col.put(i, a2);
      fieldCol.put(i, 0);
      dataDescIdCol.put(i, 0);
      timeCol.put(i, time);
      a2++;
      if (a2 == nAnt) {
        ++a1;
        a2 = a1 + 1;
        if (a2 == nAnt) {
          a1 = 0;
          a2 = 1;
          ++time;
        }
      }
    }

    casacore::ArrayColumn<casacore::Complex> dataCol(newTable, "DATA");
    for (size_t i = 0; i != nRow; ++i) {
      casacore::Array<casacore::Complex> arr(shape);
      *arr.cbegin() = i;
      dataCol.put(i, arr);
    }
  }
  ~TestTableFixture() { boost::filesystem::remove_all("TestTable"); }
};

BOOST_AUTO_TEST_CASE(spec) {
  DyscoStMan dysco(8, 12);
  Record spec = dysco.dataManagerSpec();
  BOOST_CHECK_EQUAL(spec.asInt("dataBitCount"), 8);
  BOOST_CHECK_EQUAL(spec.asInt("weightBitCount"), 12);
}

BOOST_AUTO_TEST_CASE(name) {
  DyscoStMan dysco1(8, 12, "withparameters");
  BOOST_CHECK_EQUAL(dysco1.dataManagerType(), "DyscoStMan");
  BOOST_CHECK_EQUAL(dysco1.dataManagerName(), "withparameters");

  DyscoStMan dysco2("testname", GetDyscoSpec());
  BOOST_CHECK_EQUAL(dysco2.dataManagerType(), "DyscoStMan");
  BOOST_CHECK_EQUAL(dysco2.dataManagerName(), "testname");

  std::unique_ptr<DataManager> dysco3(dysco2.clone());
  BOOST_CHECK_EQUAL(dysco3->dataManagerType(), "DyscoStMan");
  BOOST_CHECK_EQUAL(dysco3->dataManagerName(), "testname");

  register_dyscostman();
  DataManagerCtor dyscoConstructor = DataManager::getCtor("DyscoStMan");
  std::unique_ptr<DataManager> dysco4(
      dyscoConstructor("Constructed", GetDyscoSpec()));
  BOOST_CHECK_EQUAL(dysco4->dataManagerName(), "Constructed");

  TestTableFixture fixture(3);
  casacore::Table table("TestTable");
  casacore::ArrayColumn<casacore::Complex> dataCol(table, "DATA");
  DataManager* dm = table.findDataManager("DATA", true);
  BOOST_CHECK_EQUAL(dm->dataManagerName(), "DATA_dm");
}

BOOST_AUTO_TEST_CASE(makecolumn) {
  DyscoStMan dysco(8, 12, "mydysco");
  dysco.createDirArrColumn("DATA", casacore::DataType::TpComplex, "");
  dysco.createDirArrColumn("WEIGHT_SPECTRUM", casacore::DataType::TpFloat, "");
  dysco.createDirArrColumn("CORRECTED_DATA", casacore::DataType::TpComplex, "");
  dysco.createDirArrColumn("ANYTHING", casacore::DataType::TpComplex, "");
  BOOST_CHECK(true);
}

BOOST_AUTO_TEST_CASE(maketable) {
  size_t nAnt = 3;
  TestTableFixture fixture(nAnt);

  casacore::Table table("TestTable");
  casacore::ArrayColumn<casacore::Complex> dataCol(table, "DATA");
  for (size_t i = 0; i != table.nrow(); ++i) {
    BOOST_CHECK_CLOSE_FRACTION((*dataCol(i).cbegin()).real(), float(i), 1e-4);
  }
}

BOOST_AUTO_TEST_CASE(read_past_end) {
  /**
   * While reading past the end of a file might seem wrong in any case, it can
   * happen that a user reads a line that was not stored yet in the particular
   * column, but which does exist in the table. This is valid (and DPPP does
   * this).
   */
  size_t nAnt = 3;
  TestTableFixture fixture(nAnt);

  casacore::Table table("TestTable");
  casacore::ArrayColumn<casacore::Complex> dataCol(table, "DATA");
  for (size_t i = table.nrow(); i != table.nrow() * 2; ++i) {
    BOOST_CHECK_CLOSE_FRACTION((*dataCol(i).cbegin()).real(), 0.0, 1e-4);
  }
}

BOOST_AUTO_TEST_CASE(readonly) {
  size_t nAnt = 3;
  TestTableFixture fixture(nAnt);

  boost::filesystem::directory_iterator end_itr;

  for (boost::filesystem::directory_iterator itr("TestTable/"); itr != end_itr;
       ++itr) {
    if (boost::filesystem::is_regular_file(itr->path())) {
      boost::filesystem::permissions(
          itr->path(),
          boost::filesystem::others_read | boost::filesystem::owner_read);
    }
  }
  casacore::Table table("TestTable");
  casacore::ArrayColumn<casacore::Complex> dataCol(table, "DATA");
  for (size_t i = 0; i != table.nrow(); ++i) {
    BOOST_CHECK_CLOSE_FRACTION((*dataCol(i).cbegin()).real(), float(i), 1e-4);
  }
}

BOOST_AUTO_TEST_SUITE_END()
