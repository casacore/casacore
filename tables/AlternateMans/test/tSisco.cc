#include <casacore/tables/AlternateMans/Sisco.h>

#include <boost/test/unit_test.hpp>

#include <cmath>

namespace casacore {

BOOST_AUTO_TEST_SUITE(sisco)

BOOST_AUTO_TEST_CASE(match) {
  constexpr BitFloat zero(0.0f);
  constexpr BitFloat one(1.0f);
  constexpr BitFloat ten(10.0f);
  BOOST_CHECK(!Match(one, zero.Exponent()));
  BOOST_CHECK(!Match(ten, zero.Exponent()));

  std::optional<BitFloat> result = Match(zero, one.Exponent());
  BOOST_CHECK(result);
  BOOST_CHECK_EQUAL(result->Mantissa(), 0);
  BOOST_CHECK_EQUAL(result->Exponent(), one.Exponent());

  result = Match(zero, ten.Exponent());
  BOOST_CHECK(result);
  BOOST_CHECK_EQUAL(result->Mantissa(), 0);
  BOOST_CHECK_EQUAL(result->Exponent(), ten.Exponent());

  result = Match(one, ten.Exponent());
  BOOST_CHECK(result);
  BOOST_CHECK_EQUAL(result->Exponent(), ten.Exponent());
  BOOST_CHECK_CLOSE_FRACTION(result->ToFloat(), 1.0f, 1e-6);

  result = Match(ten, one.Exponent());
  BOOST_CHECK(result);
  BOOST_CHECK_EQUAL(result->Exponent(), one.Exponent());
  BOOST_CHECK_CLOSE_FRACTION(result->ToFloat(), 10.0f, 1e-6);
}

BOOST_AUTO_TEST_CASE(predict_from_last) {
  constexpr BitFloat zero(0.0f);
  constexpr BitFloat one(1.0f);
  constexpr BitFloat two(2.0f);
  constexpr BitFloat predicted_12 = Predict(one, two.Exponent());
  BOOST_CHECK_EQUAL(predicted_12.Mantissa(), 0x0400000);
  BOOST_CHECK_EQUAL(predicted_12.Exponent(), 1);
  BOOST_CHECK(!predicted_12.Sign());
  constexpr BitFloat predicted_21 = Predict(two, one.Exponent());
  BOOST_CHECK_EQUAL(predicted_21.Mantissa(), 0x1000000);
  BOOST_CHECK_EQUAL(predicted_21.Exponent(), 0);
  BOOST_CHECK(!predicted_21.Sign());
  constexpr BitFloat predicted_11 = Predict(one, one.Exponent());
  BOOST_CHECK_EQUAL(predicted_11.Mantissa(), 0x0800000);
  BOOST_CHECK_EQUAL(predicted_11.Exponent(), 0);
  BOOST_CHECK(!predicted_11.Sign());
  constexpr BitFloat predicted_22 = Predict(two, two.Exponent());
  BOOST_CHECK_EQUAL(predicted_22.Mantissa(), 0x0800000);
  BOOST_CHECK_EQUAL(predicted_22.Exponent(), 1);
  BOOST_CHECK(!predicted_22.Sign());

  constexpr BitFloat predicted_01 = Predict(zero, one.Exponent());
  BOOST_CHECK_EQUAL(predicted_01.Mantissa(), 0);
  BOOST_CHECK(!predicted_01.Sign());
  BOOST_CHECK_EQUAL(predicted_01.ToFloat(), 0.0f);
  constexpr BitFloat predicted_10 = Predict(one, zero.Exponent());
  BOOST_CHECK_EQUAL(predicted_10.Mantissa(), 0);
  BOOST_CHECK(!predicted_10.Sign());
}

BOOST_AUTO_TEST_CASE(residual) {
  constexpr BitFloat one(1.0f);
  constexpr BitFloat two(2.0f);
  constexpr BitFloat residual_12 = Residual(one, two);
  BOOST_CHECK_EQUAL(residual_12.Mantissa(), 0x0400000);
  BOOST_CHECK_EQUAL(residual_12.Exponent(), 1);
  BOOST_CHECK(!residual_12.Sign());
  constexpr BitFloat residual_21 = Residual(two, one);
  BOOST_CHECK_EQUAL(residual_21.Mantissa(), 0x0800000);
  BOOST_CHECK_EQUAL(residual_21.Exponent(), 0);
  BOOST_CHECK(residual_21.Sign());
  constexpr BitFloat residual_11 = Residual(one, one);
  BOOST_CHECK_EQUAL(residual_11.Mantissa(), 0x0000000);
  BOOST_CHECK_EQUAL(residual_11.Exponent(), 0);
  BOOST_CHECK(!residual_12.Sign());
}

BOOST_AUTO_TEST_CASE(predict_from_two) {
  constexpr BitFloat zero(0, 0, false);
  constexpr BitFloat subnormal(1e-40f);
  constexpr BitFloat pointone(0.1f);
  constexpr BitFloat pointtwo(0.2f);
  constexpr BitFloat pointthree(0.3);

  BitFloat predicted = Predict(pointone, pointtwo, pointthree.Exponent());
  BOOST_CHECK_CLOSE_FRACTION(predicted.ToFloat(), 0.3, 1e-6);

  predicted = Predict(zero, pointone, pointthree.Exponent());
  BOOST_CHECK_CLOSE_FRACTION(predicted.ToFloat(), 0.2, 1e-6);

  predicted = Predict(pointone, pointtwo, -2);
  BOOST_CHECK_CLOSE_FRACTION(predicted.ToFloat(), 0.3, 1e-6);

  predicted = Predict(subnormal, pointtwo, -2);
  BOOST_CHECK_CLOSE_FRACTION(predicted.ToFloat(), 0.2, 1e-6);

  predicted = Predict(pointtwo, subnormal, -2);
  BOOST_CHECK_CLOSE_FRACTION(predicted.ToFloat(), 0.2, 1e-6);
}

BOOST_AUTO_TEST_CASE(predict_from_three) {
  constexpr BitFloat p3(1.0f);
  constexpr BitFloat p2(4.0f);
  constexpr BitFloat p1(9.0f);
  constexpr BitFloat predicted = Predict(p3, p2, p1, p1.Exponent());
  BOOST_CHECK_CLOSE_FRACTION(predicted.ToFloat(), 16.0, 1e-6);
}

BOOST_AUTO_TEST_CASE(predict_from_three_with_overflow_a) {
  constexpr BitFloat q3(1398181690, -8, true);
  BOOST_CHECK_CLOSE_FRACTION(q3.ToFloat(), -0.651079059f, 1e-6);
  constexpr BitFloat q2(1429783430, -8, true);
  BOOST_CHECK_CLOSE_FRACTION(q2.ToFloat(), -0.66579479, 1e-6);
  constexpr BitFloat q1(1460812991, -8, true);
  BOOST_CHECK_CLOSE_FRACTION(q1.ToFloat(), -0.680244029, 1e-6);
  BitFloat predicted = Predict(q3, q2, q1, -8);
  BOOST_CHECK_EQUAL(predicted.ToFloat(), 0.0f);
  BOOST_CHECK_EQUAL(predicted.Mantissa(), 0);
  BOOST_CHECK_EQUAL(predicted.Exponent(), -8);
}

BOOST_AUTO_TEST_CASE(predict_from_three_with_overflow_b) {
  constexpr BitFloat p3(-1.66f);
  constexpr BitFloat p2(0.256f);
  constexpr BitFloat p1(0.114f);
  constexpr BitFloat target(0.01f);
  constexpr BitFloat predicted = Predict(p3, p2, p1, target.Exponent());
  BOOST_CHECK_EQUAL(predicted.ToFloat(), 0.0f);
  BOOST_CHECK_EQUAL(predicted.Mantissa(), 0);
  BOOST_CHECK_EQUAL(predicted.Exponent(), target.Exponent());
}

BOOST_AUTO_TEST_CASE(predict_from_three_with_overflow_c) {
  constexpr BitFloat p3(0.041196f);
  constexpr BitFloat p2(-7.54668f);
  constexpr BitFloat p1(-6.61567f);
  constexpr int8_t target_exponent = -5;
  constexpr BitFloat predicted = Predict(p3, p2, p1, target_exponent);
  BOOST_CHECK(!predicted.MantissaOverflow());
  BOOST_CHECK_EQUAL(predicted.ToFloat(), 0.0f);
  BOOST_CHECK_EQUAL(predicted.Mantissa(), 0);
  BOOST_CHECK_EQUAL(predicted.Exponent(), target_exponent);
}

BOOST_AUTO_TEST_CASE(predict_from_four) {
  constexpr BitFloat p4(1.0f);
  constexpr BitFloat p3(8.0f);
  constexpr BitFloat p2(27.0f);
  constexpr BitFloat p1(64.0f);
  constexpr BitFloat predicted = Predict(p4, p3, p2, p1, p1.Exponent());
  BOOST_CHECK_CLOSE_FRACTION(predicted.ToFloat(), 125.0, 1e-6);
}

void Check(const BitFloat& input, const BitFloat& result, size_t number_index) {
  const float input_float = input.ToFloat();
  // this one is useful for debugging...
  BOOST_CHECK_EQUAL(std::to_string(number_index) + ": " + std::to_string(input_float), std::to_string(number_index) + ": " + std::to_string(result.ToFloat()));
  BOOST_CHECK_EQUAL(std::bit_cast<uint32_t>(input_float), std::bit_cast<uint32_t>(result.ToFloat()));
}

BOOST_AUTO_TEST_CASE(compress_1d) {
  const std::vector<BitFloat> input{
    BitFloat(0.0f),
    BitFloat(1.0f),
    BitFloat(0.5f),
    BitFloat(0.5f),
    BitFloat(std::numeric_limits<float>::quiet_NaN()),
    BitFloat(0.5f),
    BitFloat(std::numeric_limits<float>::infinity()),
    BitFloat(0.5f),
    BitFloat(-std::numeric_limits<float>::quiet_NaN()),
    BitFloat(0.5f),
    BitFloat(-std::numeric_limits<float>::infinity()),
    BitFloat(std::numeric_limits<float>::quiet_NaN()),
    BitFloat(std::numeric_limits<float>::quiet_NaN()),
    BitFloat(std::numeric_limits<float>::infinity()),
    BitFloat(std::numeric_limits<float>::infinity())
  };
  for(int level=0; level!=4; ++level) {
    std::vector<std::byte> mantissa_data(input.size() * sizeof(uint32_t));
    std::vector<std::byte> exponent_data(input.size() * sizeof(int8_t));
    Compress1D(level, input, mantissa_data, exponent_data);
    std::vector<BitFloat> result(input.size());
    Decompress1D(level, mantissa_data, exponent_data, result);
    for(size_t i=0; i!=input.size(); ++i) {
      Check(input[i], result[i], i);
    }
  }
}

BOOST_AUTO_TEST_CASE(average_predict) {
  constexpr BitFloat a(0.0f);
  constexpr BitFloat b(2.0f);
  constexpr BitFloat c(4.0f);
  constexpr BitFloat d(-4.0f);
  // Since a is zero, it will be discarded
  constexpr BitFloat result1 = AveragePredict(a, b, b.Exponent());
  BOOST_CHECK_CLOSE_FRACTION(result1.ToFloat(), 2.0f, 1e-5);
  constexpr BitFloat result2 = AveragePredict(c, b, b.Exponent());
  BOOST_CHECK_CLOSE_FRACTION(result2.ToFloat(), 3.0f, 1e-5);
  constexpr BitFloat result3 = AveragePredict(c, d, c.Exponent());
  BOOST_CHECK_CLOSE_FRACTION(result3.ToFloat(), 0.0f, 1e-5);
}

BOOST_AUTO_TEST_CASE(quadratic_compress_1d) {
  std::vector<std::byte> mantissa(4);
  std::vector<std::byte> exponent(1);
  std::vector<BitFloat> one(1, BitFloat(1.0f));
  QuadraticCompress1D(one, mantissa, exponent);
  std::vector<BitFloat> result(1);
  QuadraticDecompress1D(mantissa, exponent, result);
  BOOST_CHECK_EQUAL(result[0].ToFloat(), 1.0f);
}

BOOST_AUTO_TEST_CASE(linear_predict_from_3) {
  constexpr BitFloat y3(0.0f);
  constexpr BitFloat y2(2.0f);
  constexpr BitFloat y1(4.0f);
  constexpr BitFloat result1 = LinearPredict(y3, y2, y1, y1.Exponent());
  BOOST_CHECK_CLOSE_FRACTION(result1.ToFloat(), 6.0f, 1e-5);
  constexpr BitFloat result2 = LinearPredict(y1, y1, y1, y1.Exponent());
  BOOST_CHECK_CLOSE_FRACTION(result2.ToFloat(), 4.0f, 1e-5);
  constexpr BitFloat result3 = LinearPredict(y2, y2, y1, y1.Exponent());
  BOOST_CHECK_CLOSE_FRACTION(result3.ToFloat(), 4.0f + 2.0f/3.0f, 1e-5);

  constexpr BitFloat z3(-4.0f);
  constexpr BitFloat z2(-3.0f);
  constexpr BitFloat z1(-2.0f);
  constexpr BitFloat result4 = LinearPredict(z3, z2, z1, y1.Exponent());
  BOOST_CHECK_CLOSE_FRACTION(result4.ToFloat(), -1.0f, 1e-5);
  constexpr BitFloat result5 = LinearPredict(z1, z1, z1, y1.Exponent());
  BOOST_CHECK_CLOSE_FRACTION(result5.ToFloat(), -2.0f, 1e-5);
  constexpr BitFloat result6 = LinearPredict(z1, y2, z1, y1.Exponent());
  BOOST_CHECK_CLOSE_FRACTION(result6.ToFloat(), -2.0f/3.0f, 1e-5);
}

BOOST_AUTO_TEST_CASE(quadratic_predict_from_4) {
  constexpr BitFloat y4(1.0f);
  constexpr BitFloat y3(2.0f);
  constexpr BitFloat y2(3.0f);
  constexpr BitFloat y1(4.0f);
  constexpr BitFloat result1 = QuadraticPredict(y4, y3, y2, y1, y1.Exponent());
  BOOST_CHECK_CLOSE_FRACTION(result1.ToFloat(), 5.0f, 1e-5);

  constexpr BitFloat z4(-1.0f);
  constexpr BitFloat z3(-4.0f);
  constexpr BitFloat z2(-9.0f);
  constexpr BitFloat z1(-16.0f);
  constexpr BitFloat result2 = QuadraticPredict(z4, z3, z2, z1, z1.Exponent());
  BOOST_CHECK_CLOSE_FRACTION(result2.ToFloat(), -25.0f, 1e-5);
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace casacore
