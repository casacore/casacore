#include <boost/test/unit_test.hpp>

#include "../BitPacking.h"

BOOST_AUTO_TEST_SUITE(bit_packing)

BOOST_AUTO_TEST_CASE(pack_and_unpack) {
  BOOST_CHECK_NO_THROW(PackBoolArray(nullptr, nullptr, 0));
  BOOST_CHECK_NO_THROW(UnpackBoolArray(nullptr, nullptr, 0));
  
  const std::array<bool, 12> input_a={true, true, false, true, false, false, true, false, true, false, true, false};
  std::array<unsigned char, 2> packed_a={0xFF, 0xFF};
  PackBoolArray(packed_a.data(), input_a.data(), input_a.size());
  BOOST_CHECK_EQUAL(packed_a[0], 0b01001011);
  BOOST_CHECK_EQUAL(packed_a[1], 0b00000101);
  
  std::array<bool, 12> unpacked_a;
  std::fill(unpacked_a.begin(), unpacked_a.end(), false);
  UnpackBoolArray(unpacked_a.data(), packed_a.data(), input_a.size());
  BOOST_CHECK_EQUAL_COLLECTIONS(unpacked_a.begin(), unpacked_a.end(), input_a.begin(), input_a.end());
}

BOOST_AUTO_TEST_SUITE_END()

