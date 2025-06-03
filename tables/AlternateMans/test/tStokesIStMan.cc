#include <boost/test/unit_test.hpp>

#include <casacore/tables/AlternateMans/StokesIStManColumn.h>

BOOST_AUTO_TEST_SUITE(stokes_i_st_man_column)

BOOST_AUTO_TEST_CASE(expand_from_stokes_i) {
  casacore::ExpandFromStokesI<int>(nullptr, 0);

  double test_data_a[4] = {1.0, -1.0, -1.0, -1.0};
  casacore::ExpandFromStokesI(test_data_a, 1);
  BOOST_CHECK_EQUAL(test_data_a[0], 1.0);
  BOOST_CHECK_EQUAL(test_data_a[1], 0.0);
  BOOST_CHECK_EQUAL(test_data_a[2], 0.0);
  BOOST_CHECK_EQUAL(test_data_a[3], 1.0);

  const std::complex<float> u = {8.8, 8.8};
  std::complex<float> test_data_b[12] = {{-3.7, 2.0}, {5.2, 0.0}, {std::numeric_limits<float>::quiet_NaN(), -2.1}, u,u,u,u,u,u,u,u,u};
  casacore::ExpandFromStokesI(test_data_b, 3);
  const std::complex<float> reference_b[8] = {
    {-3.7, 2.0}, {0.0}, {0.0}, {-3.7, 2.0}, // 1
    {5.2, 0.0}, {0.0}, {0.0}, {5.2, 0.0} // 2
  };
  BOOST_CHECK_EQUAL_COLLECTIONS(test_data_b, test_data_b+8, reference_b, reference_b+8);
  BOOST_CHECK(std::isnan(test_data_b[8].real()));
  BOOST_CHECK_EQUAL(test_data_b[8].imag(), -2.1f);
  BOOST_CHECK_EQUAL(test_data_b[9], 0.0f);
  BOOST_CHECK_EQUAL(test_data_b[10], 0.0f);
  BOOST_CHECK(std::isnan(test_data_b[11].real()));
  BOOST_CHECK_EQUAL(test_data_b[11].imag(), -2.1f);
}

BOOST_AUTO_TEST_CASE(transform_to_stokes_i) {
  casacore::TransformToStokesI<int>(nullptr, nullptr, 0);

  constexpr size_t data_a_size = 12;
  const bool test_data_a[data_a_size] = {false, false, false, false, true, true, true, true, false, false, false, false};
  char buffer_a[data_a_size * sizeof(bool)];
  const bool* result_a = casacore::TransformToStokesI(test_data_a, buffer_a, data_a_size/4);
  BOOST_CHECK(!result_a[0]);
  BOOST_CHECK(result_a[1]);
  BOOST_CHECK(!result_a[2]);

  constexpr size_t data_b_size = 16;
  const double test_data_b[data_b_size] = {3.0, 0.0, 0.0, 3.0, 20.0,  0.0,  0.0, 24.0, 0.0,  0.0,   0.0, 1000.0, std::numeric_limits<double>::quiet_NaN(), 0.0, 0.0, 0.0};
  char buffer_b[data_b_size * sizeof(double)];
  const double* result_b = casacore::TransformToStokesI(test_data_b, buffer_b, data_b_size/4);
  BOOST_CHECK_CLOSE_FRACTION(result_b[0], 3.0, 1e-11);
  BOOST_CHECK_CLOSE_FRACTION(result_b[1], 22.0, 1e-11);
  BOOST_CHECK_CLOSE_FRACTION(result_b[2], 500.0, 1e-11);
  BOOST_CHECK(std::isnan(result_b[3]));
}

BOOST_AUTO_TEST_SUITE_END()
