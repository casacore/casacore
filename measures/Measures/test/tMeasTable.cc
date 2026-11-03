#include <casacore/measures/Measures/MeasTable.h>

#define BOOST_TEST_MODULE measures
#define BOOST_TEST_DYN_LINK

#include <boost/test/unit_test.hpp>

using casacore::MPosition;
using casacore::MeasTable;

BOOST_AUTO_TEST_SUITE(meas_table)

BOOST_AUTO_TEST_CASE(wgs84_observatory_position) {
  // Alma uses a WGS84 position, so we use ALMA for testing. If ALMA's position were to
  // be updated in the observatory table, the next line should be updated as well.
  // These are longitude, lattitude, height:
  constexpr double kAlmaPosition[3] = {-67.7549, -23.0229, 5056.8};
  MPosition result_position;
  BOOST_CHECK(MeasTable::Observatory(result_position, "ALMA"));
  BOOST_CHECK_EQUAL(result_position.getRef().getType(), MPosition::WGS84);
  const casacore::Quantum<casacore::Vector<double>> angles = result_position.getValue().getAngle("deg");
  BOOST_CHECK_CLOSE_FRACTION(angles.getValue()[0], kAlmaPosition[0], 1e-5);
  BOOST_CHECK_CLOSE_FRACTION(angles.getValue()[1], kAlmaPosition[1], 1e-5);
  BOOST_CHECK_CLOSE_FRACTION(result_position.getValue().radius(), kAlmaPosition[2], 1e-5);
}

BOOST_AUTO_TEST_CASE(itrf_observatory_position) {
  // LOFAR uses an ITRF position. If LOFAR's position were to be updated in the observatory
  // table, the next line should be updated as well.
  // These are X, Y, Z:
  constexpr double kLofarPosition[3] = {3826577.022720000, 461022.995082000, 5064892.814};
  MPosition result_position;
  BOOST_CHECK(MeasTable::Observatory(result_position, "LOFAR"));
  BOOST_CHECK_EQUAL(result_position.getRef().getType(), MPosition::ITRF);
  BOOST_CHECK_CLOSE_FRACTION(result_position.getValue()(0), kLofarPosition[0], 1e-8);
  BOOST_CHECK_CLOSE_FRACTION(result_position.getValue()(1), kLofarPosition[1], 1e-8);
  BOOST_CHECK_CLOSE_FRACTION(result_position.getValue()(2), kLofarPosition[2], 1e-8);
}

BOOST_AUTO_TEST_SUITE_END()
