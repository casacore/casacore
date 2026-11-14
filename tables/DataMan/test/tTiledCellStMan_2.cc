#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/tables/DataMan/TiledCellStMan.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Exceptions/Error.h>

#include <boost/test/unit_test.hpp>

// <summary>
// Test row-range (getColumnRange, putColumnCells, ...) access on TiledCellStMan
// </summary>

// A TiledCellStMan hypercube holds a single cell and has no row axis. The
// optimized TSMDataColumn code for accessing multiple cells assumes that the
// rows are mapped to the last axis of the hypercube, so it must not be used
// for this storage manager. This used to fail with
// "TSMShape::offset - shapes do not conform".

using casacore::IPosition;
using casacore::Slicer;

namespace {
const unsigned int nrow = 6;

casacore::Cube<float> expectedValue() {
  // Row r holds the value r+1 in every element.
  casacore::Cube<float> exp(4, 2, nrow);
  for (unsigned int r = 0; r < nrow; r++) {
    exp.xyPlane(r) = float(r + 1);
  }
  return exp;
}
}  // namespace

BOOST_AUTO_TEST_SUITE(tiled_cell_stman_2)

BOOST_AUTO_TEST_CASE(row_range_access) {
  casacore::TableDesc td("", "1", casacore::TableDesc::Scratch);
  td.addColumn(
      casacore::ArrayColumnDesc<float>("A", IPosition(2, 4, 2), casacore::ColumnDesc::FixedShape));
  td.defineHypercolumn("TSMExample", 2, casacore::stringToVector("A"));
  casacore::SetupNewTable newtab("tTiledCellStMan_2_tmp.data", td, casacore::Table::New);
  casacore::TiledCellStMan sm("TSMExample", IPosition(2, 4, 2));
  newtab.bindAll(sm);
  casacore::Table table(newtab, nrow);
  casacore::ArrayColumn<float> col(table, "A");

  // Whole column, single cells.
  casacore::Cube<float> exp = expectedValue();
  col.putColumn(exp);
  BOOST_CHECK(allEQ(col.getColumn(), exp));
  for (unsigned int r = 0; r < nrow; r++) {
    BOOST_CHECK(allEQ(col(r), exp.xyPlane(r)));
  }

  // Row range get.
  casacore::Cube<float> sub =
      col.getColumnRange(Slicer(IPosition(1, 2), IPosition(1, 2), Slicer::endIsLength));
  BOOST_CHECK_EQUAL(sub.shape(), IPosition(3, 4, 2, 2));
  BOOST_CHECK(allEQ(sub, exp(IPosition(3, 0, 0, 2), IPosition(3, 3, 1, 3))));

  // Row range get with a stride.
  casacore::Slicer every2(IPosition(1, 0), IPosition(1, nrow - 1), IPosition(1, 2),
                          Slicer::endIsLast);
  sub.reference(col.getColumnRange(every2));
  BOOST_CHECK_EQUAL(sub.shape(), IPosition(3, 4, 2, 3));
  BOOST_CHECK(allEQ(sub.xyPlane(1), exp.xyPlane(2)));

  // Non-contiguous rows.
  casacore::Vector<casacore::rownr_t> rows(3);
  rows(0) = 5;
  rows(1) = 0;
  rows(2) = 3;
  sub.reference(col.getColumnCells(casacore::RefRows(rows)));
  BOOST_CHECK_EQUAL(sub.shape(), IPosition(3, 4, 2, 3));
  BOOST_CHECK(allEQ(sub.xyPlane(0), exp.xyPlane(5)));
  BOOST_CHECK(allEQ(sub.xyPlane(1), exp.xyPlane(0)));
  BOOST_CHECK(allEQ(sub.xyPlane(2), exp.xyPlane(3)));

  // Row range get with an array section.
  casacore::Slicer section(IPosition(2, 1, 0), IPosition(2, 2, 2), Slicer::endIsLength);
  casacore::Cube<float> secSub =
      col.getColumnRange(Slicer(IPosition(1, 2), IPosition(1, 2), Slicer::endIsLength), section);
  BOOST_CHECK_EQUAL(secSub.shape(), IPosition(3, 2, 2, 2));
  BOOST_CHECK(allEQ(secSub.xyPlane(0), 3.0f));
  BOOST_CHECK(allEQ(secSub.xyPlane(1), 4.0f));

  // Row range put.
  casacore::Cube<float> seven(4, 2, 2);
  seven = 7.0f;
  col.putColumnRange(Slicer(IPosition(1, 2), IPosition(1, 2), Slicer::endIsLength), seven);
  exp.xyPlane(2) = 7.0f;
  exp.xyPlane(3) = 7.0f;
  BOOST_CHECK(allEQ(col.getColumn(), exp));

  // Row range put with an array section.
  casacore::Cube<float> nine(2, 2, 2);
  nine = 9.0f;
  col.putColumnRange(Slicer(IPosition(1, 0), IPosition(1, 2), Slicer::endIsLength), section, nine);
  for (unsigned int r = 0; r < 2; r++) {
    for (unsigned int j = 0; j < 2; j++) {
      for (unsigned int i = 1; i < 3; i++) {
        exp(i, j, r) = 9.0f;
      }
    }
  }
  BOOST_CHECK(allEQ(col.getColumn(), exp));

  // putColumnCells with non-contiguous rows.
  casacore::Cube<float> three(4, 2, 3);
  three = 3.0f;
  col.putColumnCells(casacore::RefRows(rows), three);
  exp.xyPlane(5) = 3.0f;
  exp.xyPlane(0) = 3.0f;
  exp.xyPlane(3) = 3.0f;
  BOOST_CHECK(allEQ(col.getColumn(), exp));
}

BOOST_AUTO_TEST_SUITE_END()
