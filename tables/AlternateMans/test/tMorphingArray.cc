#include "../DataManagers/MorphingArray.h"

#include <complex>

#include <boost/test/unit_test.hpp>

namespace casacore {

BOOST_AUTO_TEST_SUITE(morphing_array)

BOOST_AUTO_TEST_CASE(construct) {
  BOOST_CHECK_EQUAL(MorphingArray().Size(), 0);
  BOOST_CHECK(MorphingArray().Data<float>() == nullptr);
}

BOOST_AUTO_TEST_CASE(resize) {
  MorphingArray a;
  // allocate
  a.Resize<std::complex<float>>(5);
  BOOST_CHECK_EQUAL(a.Size(), 5);
  BOOST_CHECK(a.Data<float>() != nullptr);
  const std::complex<float> kValue(2.0f, 3.0f);
  a.Data<std::complex<float>>()[4] = kValue;
  BOOST_CHECK_EQUAL(a.Data<std::complex<float>>()[4], kValue);
  
  // shrink
  a.Resize<std::complex<float>>(3);
  a.Data<std::complex<float>>()[2] = kValue;
  BOOST_CHECK_EQUAL(a.Data<std::complex<float>>()[2], kValue);
  
  // expand
  a.Resize<std::complex<float>>(8);
  BOOST_CHECK_EQUAL(a.Size(), 8);
  a.Data<std::complex<float>>()[7] = kValue;
  BOOST_CHECK_EQUAL(a.Data<std::complex<float>>()[7], kValue);
}

BOOST_AUTO_TEST_CASE(move) {
  MorphingArray empty;
  MorphingArray a;
  a.Resize<char>(5);
  a = std::move(empty);
  BOOST_CHECK_EQUAL(a.Size(), 0);
  BOOST_CHECK(a.Data<char>() == nullptr);
  BOOST_CHECK_EQUAL(empty.Size(), 0);
  BOOST_CHECK(empty.Data<char>() == nullptr);
  
  a.Resize<char>(5);
  a.Data<char>()[4] = 31;
  MorphingArray b(std::move(a));
  BOOST_CHECK_EQUAL(a.Size(), 0);
  BOOST_CHECK(a.Data<char>() == nullptr);
  BOOST_CHECK_EQUAL(b.Size(), 5);
  BOOST_CHECK_EQUAL(b.Data<char>()[4], 31);
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace casacore
