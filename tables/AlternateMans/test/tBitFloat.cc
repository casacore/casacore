#include <casacore/tables/AlternateMans/BitFloat.h>

#include <boost/test/unit_test.hpp>

#include <cmath>
#include <complex>

namespace casacore {

BOOST_AUTO_TEST_SUITE(bitfloat)

BOOST_AUTO_TEST_CASE(construct) {
  constexpr BitFloat f1(0.0f);
  BOOST_CHECK_EQUAL(f1.Mantissa(), 0);
  BOOST_CHECK_EQUAL(f1.Exponent(), -127);
  BOOST_CHECK(!f1.Sign());
  BOOST_CHECK(BitFloat::GetKind(f1.ToFloat()) == BitFloatKind::Zero);

  constexpr BitFloat f2(1.0f);
  BOOST_CHECK_EQUAL(f2.Mantissa(), 0x800000);
  BOOST_CHECK_EQUAL(f2.Exponent(), 0);
  BOOST_CHECK(!f2.Sign());
  BOOST_CHECK(BitFloat::GetKind(f2.ToFloat()) == BitFloatKind::Normal);

  constexpr BitFloat f2b(2.0f);
  BOOST_CHECK_EQUAL(f2b.Mantissa(), 0x800000);
  BOOST_CHECK_EQUAL(f2b.Exponent(), 1);
  BOOST_CHECK(!f2b.Sign());
  BOOST_CHECK(BitFloat::GetKind(f2b.ToFloat()) == BitFloatKind::Normal);

  constexpr BitFloat f3(-1.0f);
  BOOST_CHECK_EQUAL(f3.Mantissa(), 0x800000);
  BOOST_CHECK_EQUAL(f3.Exponent(), 0);
  BOOST_CHECK(f3.Sign());
  BOOST_CHECK(BitFloat::GetKind(f3.ToFloat()) == BitFloatKind::Normal);

  constexpr BitFloat f4(-0.0f);
  BOOST_CHECK_EQUAL(ToString(BitFloat::GetKind(f4.ToFloat())), "negative zero");

  constexpr BitFloat f5(std::numeric_limits<float>::quiet_NaN());
  BOOST_CHECK_EQUAL(ToString(BitFloat::GetKind(f5.ToFloat())), "nan");

  constexpr BitFloat f6(std::numeric_limits<float>::signaling_NaN());
  BOOST_CHECK_EQUAL(ToString(BitFloat::GetKind(f6.ToFloat())), "signalling nan");

  // Smallest subnormal
  constexpr BitFloat f7(std::bit_cast<float>(0b00000000000000000000000000000001));
  BOOST_CHECK_EQUAL(ToString(BitFloat::GetKind(f7.ToFloat())), "subnormal");

  // Largest subnormal
  constexpr BitFloat f8(std::bit_cast<float>(0b00000000011111111111111111111111));
  BOOST_CHECK_EQUAL(ToString(BitFloat::GetKind(f8.ToFloat())), "subnormal");

  // Smallest normal number
  constexpr BitFloat f9(std::bit_cast<float>(0b00000000100000000000000000000000));
  BOOST_CHECK_EQUAL(f9.Mantissa(), 0x800000);
  BOOST_CHECK_EQUAL(f9.Exponent(), -126);
  BOOST_CHECK(!f9.Sign());
  BOOST_CHECK(BitFloat::GetKind(f9.ToFloat()) == BitFloatKind::Normal);

  // Largest normal number
  constexpr BitFloat f10(std::bit_cast<float>(0b01111111011111111111111111111111));
  BOOST_CHECK_EQUAL(f10.Mantissa(), 0xFFFFFF);
  BOOST_CHECK_EQUAL(f10.Exponent(), 127);
  BOOST_CHECK(!f10.Sign());
  BOOST_CHECK(BitFloat::GetKind(f10.ToFloat()) == BitFloatKind::Normal);
}

BOOST_AUTO_TEST_CASE(to_float) {
  BOOST_CHECK_EQUAL(BitFloat(1.0f).ToFloat(), 1.0f);
  BOOST_CHECK_EQUAL(BitFloat(2.0f).ToFloat(), 2.0f);
  BOOST_CHECK_EQUAL(-BitFloat(1.0f).ToFloat(), -1.0f);
  BOOST_CHECK_EQUAL(-BitFloat(2.0f).ToFloat(), -2.0f);
  BOOST_CHECK_EQUAL(BitFloat(3.1415926f).ToFloat(), 3.1415926f);
  BOOST_CHECK_EQUAL(BitFloat(0.0f).ToFloat(), 0.0f);
  BOOST_CHECK_EQUAL(BitFloat(-0.0f).ToFloat(), -0.0f);

  BOOST_CHECK_EQUAL(BitFloat(0, -1, false).ToFloat(), 0.0f);
  BOOST_CHECK_EQUAL(BitFloat(0, -1, true).ToFloat(), -0.0f);

  BitFloat v(1.0f);
  v += v;
  BOOST_CHECK_EQUAL(v.ToFloat(), 2.0f);

  constexpr BitFloat subnormal(1e-40f);
  BOOST_CHECK(subnormal.ToFloat() == 1e-40f);
   BOOST_CHECK(BitFloat::GetKind(subnormal.ToFloat()) == BitFloatKind::Subnormal);
 
  constexpr BitFloat nan_value(std::numeric_limits<float>::quiet_NaN());
  BOOST_CHECK(BitFloat::GetKind(nan_value.ToFloat()) == BitFloatKind::NaN);
  BOOST_CHECK(std::isnan(nan_value.ToFloat()));
  
  constexpr float negative_nan_float = -std::numeric_limits<float>::quiet_NaN();
  constexpr BitFloat negative_nan_value(negative_nan_float);
  BOOST_CHECK(BitFloat::GetKind(negative_nan_value.ToFloat()) == BitFloatKind::NaN);
  BOOST_CHECK(std::isnan(negative_nan_value.ToFloat()));
  BOOST_CHECK(negative_nan_value.Sign());
  BOOST_CHECK_EQUAL(std::bit_cast<uint32_t>(negative_nan_value.ToFloat()), std::bit_cast<uint32_t>(negative_nan_float));
  
  constexpr BitFloat mantissa_copy(negative_nan_value.Mantissa(), negative_nan_value.Exponent(), negative_nan_value.Sign());
  BOOST_CHECK(std::isnan(mantissa_copy.ToFloat()));
  BOOST_CHECK(mantissa_copy.Sign());
  BOOST_CHECK_EQUAL(std::bit_cast<uint32_t>(mantissa_copy.ToFloat()), std::bit_cast<uint32_t>(negative_nan_float));
}

BOOST_AUTO_TEST_CASE(add_to_positive) {
  constexpr BitFloat one(1.0f);
  constexpr BitFloat two(2.0f);
  constexpr BitFloat matched_one = *Match(one, two.Exponent());
  constexpr BitFloat matched_two = *Match(two, one.Exponent());
  constexpr BitFloat result1(BitFloat(one) += matched_two);
  BOOST_CHECK_EQUAL(result1.Mantissa(), 0x1800000);
  BOOST_CHECK_EQUAL(result1.Exponent(), 0);
  BOOST_CHECK(!result1.Sign());
  constexpr BitFloat result2(BitFloat(two) += matched_one);
  BOOST_CHECK_EQUAL(result2.Mantissa(), 0x0C00000);
  BOOST_CHECK_EQUAL(result2.Exponent(), 1);
  BOOST_CHECK(!result2.Sign());
  constexpr BitFloat result3(BitFloat(one) += -matched_two);
  BOOST_CHECK_EQUAL(result3.Mantissa(), 0x0800000);
  BOOST_CHECK_EQUAL(result3.Exponent(), 0);
  BOOST_CHECK(result3.Sign());
  constexpr BitFloat result4(BitFloat(two) += -matched_one);
  BOOST_CHECK_EQUAL(result4.Mantissa(), 0x0400000);
  BOOST_CHECK_EQUAL(result4.Exponent(), 1);
  BOOST_CHECK(!result4.Sign());
  constexpr BitFloat result5(BitFloat(one) += one);
  BOOST_CHECK_EQUAL(result5.Mantissa(), 0x1000000);
  BOOST_CHECK_EQUAL(result5.Exponent(), 0);
  BOOST_CHECK(!result5.Sign());
  constexpr BitFloat result6(BitFloat(one) += -one);
  BOOST_CHECK_EQUAL(result6.Mantissa(), 0x0000000);
  BOOST_CHECK_EQUAL(result6.Exponent(), 0);
  BOOST_CHECK(!result6.Sign());
}

BOOST_AUTO_TEST_CASE(add_to_negative) {
  constexpr BitFloat one(1.0f);
  constexpr BitFloat two(2.0f);
  constexpr BitFloat matched_one = *Match(one, two.Exponent());
  constexpr BitFloat matched_two = *Match(two, one.Exponent());
  BitFloat result = -one;
  result += matched_two;
  BOOST_CHECK_EQUAL(result.ToFloat(), 1.0f);
  result = -two;
  result += matched_one;
  BOOST_CHECK_EQUAL(result.ToFloat(), -1.0f);
}

BOOST_AUTO_TEST_CASE(add_to_zero) {
  constexpr BitFloat two(2.0f);
  constexpr BitFloat matched_zero = *Match(BitFloat(0.0f), two.Exponent());
  constexpr BitFloat result = (BitFloat(two) += matched_zero);
  BOOST_CHECK_EQUAL(result.Mantissa(), 0x0800000);
  BOOST_CHECK_EQUAL(result.Exponent(), 1);
  BOOST_CHECK(!result.Sign());

  BitFloat residual = two;
  residual -= two;
  residual += two;
  BOOST_CHECK(residual == two);

  BitFloat shifted(0, -1, false);
  BOOST_CHECK_EQUAL(shifted.Mantissa(), 0);
  BOOST_CHECK_EQUAL(shifted.Exponent(), -1);
  BOOST_CHECK(!shifted.Sign());
  BitFloat shifted_two(*Match(two, shifted.Exponent()));
  BOOST_CHECK_EQUAL(shifted_two.Exponent(), -1);
  BOOST_CHECK(!shifted_two.Sign());
  shifted += shifted_two;
  BOOST_CHECK_EQUAL(shifted.ToFloat(), shifted_two.ToFloat());
  BOOST_CHECK_EQUAL(shifted.Mantissa(), shifted_two.Mantissa());
  BOOST_CHECK_EQUAL(static_cast<int>(shifted.Exponent()), shifted_two.Exponent());
  BOOST_CHECK(!shifted.Sign());
}

BOOST_AUTO_TEST_CASE(subtract_from_positive) {
  constexpr BitFloat one(1.0f);
  constexpr BitFloat two(2.0f);
  constexpr BitFloat matched_one = *Match(one, two.Exponent());
  constexpr BitFloat matched_two = *Match(two, one.Exponent());
  constexpr BitFloat result1(BitFloat(one) -= matched_two);
  BOOST_CHECK_EQUAL(result1.Mantissa(), 0x0800000);
  BOOST_CHECK_EQUAL(result1.Exponent(), 0);
  BOOST_CHECK(result1.Sign());
  constexpr BitFloat result2(BitFloat(two) -= matched_one);
  BOOST_CHECK_EQUAL(result2.Mantissa(), 0x0400000);
  BOOST_CHECK_EQUAL(result2.Exponent(), 1);
  BOOST_CHECK(!result2.Sign());
  constexpr BitFloat result3(BitFloat(one) -= -matched_two);
  BOOST_CHECK_EQUAL(result3.Mantissa(), 0x1800000);
  BOOST_CHECK_EQUAL(result3.Exponent(), 0);
  BOOST_CHECK(!result3.Sign());
  constexpr BitFloat result4(BitFloat(two) -= -matched_one);
  BOOST_CHECK_EQUAL(result4.Mantissa(), 0x0C00000);
  BOOST_CHECK_EQUAL(result4.Exponent(), 1);
  BOOST_CHECK(!result4.Sign());
  constexpr BitFloat result5(BitFloat(one) -= one);
  BOOST_CHECK_EQUAL(result5.Mantissa(), 0x0000000);
  BOOST_CHECK_EQUAL(result5.Exponent(), 0);
  BOOST_CHECK(!result5.Sign());
  constexpr BitFloat result6(BitFloat(one) -= -one);
  BOOST_CHECK_EQUAL(result6.Mantissa(), 0x1000000);
  BOOST_CHECK_EQUAL(result6.Exponent(), 0);
  BOOST_CHECK(!result6.Sign());

  constexpr BitFloat zero(*Match(BitFloat(0.0f), two.Exponent()));
  BitFloat result = two;
  result -= zero;
  BOOST_CHECK_EQUAL(result.Mantissa(), 0x0800000);
  BOOST_CHECK_EQUAL(result.Exponent(), 1);
  BOOST_CHECK(!result.Sign());
  BOOST_CHECK_CLOSE_FRACTION(result.ToFloat(), 2.0f, 1e-6);
}

BOOST_AUTO_TEST_CASE(subtract_from_negative) {
  constexpr BitFloat one(1.0f);
  constexpr BitFloat two(2.0f);
  constexpr BitFloat matched_one = *Match(one, two.Exponent());
  constexpr BitFloat matched_two = *Match(two, one.Exponent());
  constexpr BitFloat result1(BitFloat(-one) -= matched_two);
  BOOST_CHECK_EQUAL(result1.Mantissa(), 0x1800000);
  BOOST_CHECK_EQUAL(result1.Exponent(), 0);
  BOOST_CHECK(result1.Sign());
  constexpr BitFloat result2(BitFloat(-two) -= matched_one);
  BOOST_CHECK_EQUAL(result2.Mantissa(), 0x0C00000);
  BOOST_CHECK_EQUAL(result2.Exponent(), 1);
  BOOST_CHECK(result2.Sign());
  constexpr BitFloat result3(BitFloat(-one) -= -matched_two);
  BOOST_CHECK_EQUAL(result3.Mantissa(), 0x0800000);
  BOOST_CHECK_EQUAL(result3.Exponent(), 0);
  BOOST_CHECK(!result3.Sign());
  constexpr BitFloat result4(BitFloat(-two) -= -matched_one);
  BOOST_CHECK_EQUAL(result4.Mantissa(), 0x0400000);
  BOOST_CHECK_EQUAL(result4.Exponent(), 1);
  BOOST_CHECK(result4.Sign());
  constexpr BitFloat result5(BitFloat(-one) -= -one);
  BOOST_CHECK_EQUAL(result5.Mantissa(), 0x0000000);
  BOOST_CHECK_EQUAL(result5.Exponent(), 0);
  BOOST_CHECK(result5.Sign());
  constexpr BitFloat result6(BitFloat(-one) -= -one);
  BOOST_CHECK_EQUAL(result6.Mantissa(), 0x0000000);
  BOOST_CHECK_EQUAL(result6.Exponent(), 0);
  BOOST_CHECK(result6.Sign());
}

void CheckMantissaPacking(float f) {
  BitFloat original(f);
  const uint32_t to_float_ui32 = std::bit_cast<uint32_t>(original.ToFloat());
  const uint32_t expected_ui32 = std::bit_cast<uint32_t>(f);
  BOOST_CHECK_EQUAL(expected_ui32, to_float_ui32);
  
  BitFloat decompressed(BitFloat::FromCompressed(original.PackMantissa(), original.Exponent()));
  BOOST_CHECK_EQUAL(original.Mantissa(), decompressed.Mantissa());
  BOOST_CHECK_EQUAL(original.Sign(), decompressed.Sign());
  BOOST_CHECK_EQUAL(original.Exponent(), decompressed.Exponent());
  const float result(decompressed.ToFloat());
  if(std::isfinite(f)) {
    BOOST_CHECK_EQUAL(f, original.ToFloat());
  }
  const uint32_t result_ui32 = std::bit_cast<uint32_t>(result);
  BOOST_CHECK_EQUAL(expected_ui32, result_ui32);
}

BOOST_AUTO_TEST_CASE(allows_math) {
  BOOST_CHECK(!BitFloat().AllowsMath());
  BOOST_CHECK(!BitFloat(0.0f).AllowsMath());
  BOOST_CHECK(BitFloat(1.0f).AllowsMath());
  BOOST_CHECK(!BitFloat(std::numeric_limits<float>::quiet_NaN()).AllowsMath());
  BOOST_CHECK(!BitFloat(-std::numeric_limits<float>::quiet_NaN()).AllowsMath());
  BOOST_CHECK(!BitFloat(std::numeric_limits<float>::infinity()).AllowsMath());
  BOOST_CHECK(!BitFloat(-std::numeric_limits<float>::infinity()).AllowsMath());
}

BOOST_AUTO_TEST_CASE(mantissa_packing) {
  const uint32_t nan_mantissa = BitFloat(std::numeric_limits<float>::quiet_NaN()).Mantissa();
  BOOST_CHECK_EQUAL(BitFloat(std::numeric_limits<float>::quiet_NaN()).PackMantissa(), nan_mantissa);
  BOOST_CHECK_EQUAL(BitFloat(-std::numeric_limits<float>::quiet_NaN()).PackMantissa(), nan_mantissa + 0x80000000);
  
  BOOST_CHECK(!BitFloat::UnpackMantissa(BitFloat(0.0f).PackMantissa()).second);
  BOOST_CHECK(!BitFloat::UnpackMantissa(BitFloat(std::numeric_limits<float>::quiet_NaN()).PackMantissa()).second);
  BOOST_CHECK(!BitFloat::UnpackMantissa(BitFloat(std::numeric_limits<float>::infinity()).PackMantissa()).second);
  BOOST_CHECK(BitFloat::UnpackMantissa(BitFloat(-0.0f).PackMantissa()).second);
  BOOST_CHECK(BitFloat::UnpackMantissa(BitFloat(-std::numeric_limits<float>::quiet_NaN()).PackMantissa()).second);
  BOOST_CHECK(BitFloat::UnpackMantissa(BitFloat(-std::numeric_limits<float>::infinity()).PackMantissa()).second);
  
  CheckMantissaPacking(1.0);
  CheckMantissaPacking(-1.0);
  CheckMantissaPacking(0.0);
  CheckMantissaPacking(0.25);
  CheckMantissaPacking(4.0);
  CheckMantissaPacking(0);
  CheckMantissaPacking(-std::numeric_limits<float>::quiet_NaN());
  CheckMantissaPacking(std::numeric_limits<float>::quiet_NaN());
  CheckMantissaPacking(-std::numeric_limits<float>::infinity());
  CheckMantissaPacking(std::numeric_limits<float>::infinity());
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace casacore
