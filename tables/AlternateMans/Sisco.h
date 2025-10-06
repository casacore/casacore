#ifndef SISCO_SISCO_H_
#define SISCO_SISCO_H_

#include <optional>
#include <span>
#include <vector>

#include "BitFloat.h"

namespace casacore::sisco {

struct CompressorState {
  std::vector<BitFloat> previous4;
  std::vector<BitFloat> previous3;
  std::vector<BitFloat> previous2;
  std::vector<BitFloat> previous1;
  std::vector<BitFloat> scratch;
};

constexpr size_t kCompressedMantissaSize = sizeof(uint32_t);
constexpr size_t kCompressedExponentSize = sizeof(int8_t);

void DifferenceCompress1D(std::span<const BitFloat> input,
                          std::span<std::byte> mantissa_data,
                          std::span<std::byte> exponent_data);

void Average2Compress1D(std::span<const BitFloat> input,
                        std::span<std::byte> mantissa_data,
                        std::span<std::byte> exponent_data);

void LinearCompress1D(std::span<const BitFloat> input,
                      std::span<std::byte> mantissa_data,
                      std::span<std::byte> exponent_data);

void Linear3Compress1D(std::span<const BitFloat> input,
                       std::span<std::byte> mantissa_data,
                       std::span<std::byte> exponent_data);

void QuadraticCompress1D(std::span<const BitFloat> input,
                         std::span<std::byte> mantissa_data,
                         std::span<std::byte> exponent_data);

void Quadratic4Compress1D(std::span<const BitFloat> input,
                          std::span<std::byte> mantissa_data,
                          std::span<std::byte> exponent_data);

void CubicCompress1D(std::span<const BitFloat> input,
                     std::span<std::byte> mantissa_data,
                     std::span<std::byte> exponent_data);

void DifferenceDecompress1D(std::span<const std::byte> mantissa_data,
                            std::span<const std::byte> exponent_data,
                            std::span<BitFloat> output);

void Average2Decompress1D(std::span<const std::byte> mantissa_data,
                          std::span<const std::byte> exponent_data,
                          std::span<BitFloat> output);

void LinearDecompress1D(std::span<const std::byte> mantissa_data,
                        std::span<const std::byte> exponent_data,
                        std::span<BitFloat> output);

void Linear3Decompress1D(std::span<const std::byte> mantissa_data,
                         std::span<const std::byte> exponent_data,
                         std::span<BitFloat> output);

void QuadraticDecompress1D(std::span<const std::byte> mantissa_data,
                           std::span<const std::byte> exponent_data,
                           std::span<BitFloat> output);

void Quadratic4Decompress1D(std::span<const std::byte> mantissa_data,
                            std::span<const std::byte> exponent_data,
                            std::span<BitFloat> output);

void CubicDecompress1D(std::span<const std::byte> mantissa_data,
                       std::span<const std::byte> exponent_data,
                       std::span<BitFloat> output);

void DirectCompress2D(std::span<const float> row,
                      std::span<std::byte> mantissa_data,
                      std::span<std::byte> exponent_data);

void DifferenceCompress2D(CompressorState& state, std::span<const float> row,
                          std::span<std::byte> mantissa_data,
                          std::span<std::byte> exponent_data);

void Average2Compress2D(CompressorState& state, std::span<const float> row,
                        std::span<std::byte> mantissa_data,
                        std::span<std::byte> exponent_data);

void LinearCompress2D(CompressorState& state, std::span<const float> row,
                      std::span<std::byte> mantissa_data,
                      std::span<std::byte> exponent_data);

void LinearQuadraticCompress2D(CompressorState& state,
                               std::span<const float> row,
                               std::span<std::byte> mantissa_data,
                               std::span<std::byte> exponent_data);

void Linear3Compress2D(CompressorState& state, std::span<const float> row,
                       std::span<std::byte> mantissa_data,
                       std::span<std::byte> exponent_data);

void QuadraticCompress2D(CompressorState& state, std::span<const float> row,
                         std::span<std::byte> mantissa_data,
                         std::span<std::byte> exponent_data);

void Quadratic4Compress2D(CompressorState& state, std::span<const float> row,
                          std::span<std::byte> mantissa_data,
                          std::span<std::byte> exponent_data);

void CubicCompress2D(CompressorState& state, std::span<const float> row,
                     std::span<std::byte> mantissa_data,
                     std::span<std::byte> exponent_data);

void DirectDecompress2D(std::span<std::byte> mantissa_data,
                        std::span<std::byte> exponent_data,
                        std::span<float> row);

void DifferenceDecompress2D(CompressorState& state,
                            std::span<std::byte> mantissa_data,
                            std::span<std::byte> exponent_data,
                            std::span<float> row);

void Average2Decompress2D(CompressorState& state,
                          std::span<std::byte> mantissa_data,
                          std::span<std::byte> exponent_data,
                          std::span<float> row);

void LinearDecompress2D(CompressorState& state,
                        std::span<std::byte> mantissa_data,
                        std::span<std::byte> exponent_data,
                        std::span<float> row);

void LinearQuadraticDecompress2D(CompressorState& state,
                                 std::span<std::byte> mantissa_data,
                                 std::span<std::byte> exponent_data,
                                 std::span<float> row);

void Linear3Decompress2D(CompressorState& state,
                         std::span<std::byte> mantissa_data,
                         std::span<std::byte> exponent_data,
                         std::span<float> row);

void QuadraticDecompress2D(CompressorState& state,
                           std::span<std::byte> mantissa_data,
                           std::span<std::byte> exponent_data,
                           std::span<float> row);

void Quadratic4Decompress2D(CompressorState& state,
                            std::span<std::byte> mantissa_data,
                            std::span<std::byte> exponent_data,
                            std::span<float> row);

void CubicDecompress2D(CompressorState& state,
                       std::span<std::byte> mantissa_data,
                       std::span<std::byte> exponent_data,
                       std::span<float> row);

size_t DefaultThreadCount();

inline void Compress1D(int level, std::span<const BitFloat> row,
                       std::span<std::byte> mantissa_data,
                       std::span<std::byte> exponent_data) {
  switch (level) {
    case 0:
      DifferenceCompress1D(row, mantissa_data, exponent_data);
      return;
    case 1:
      LinearCompress1D(row, mantissa_data, exponent_data);
      return;
    case 2:
      QuadraticCompress1D(row, mantissa_data, exponent_data);
      return;
    case 3:
      CubicCompress1D(row, mantissa_data, exponent_data);
      return;
    case 10:
      Average2Compress1D(row, mantissa_data, exponent_data);
      return;
    case 11:
      Linear3Compress1D(row, mantissa_data, exponent_data);
      return;
    case 12:
      Quadratic4Compress1D(row, mantissa_data, exponent_data);
      return;
  }
  throw std::runtime_error("Invalid compression level");
}

inline void Decompress1D(int level, std::span<std::byte> mantissa_data,
                         std::span<std::byte> exponent_data,
                         std::span<BitFloat> row) {
  switch (level) {
    case 0:
      DifferenceDecompress1D(mantissa_data, exponent_data, row);
      return;
    case 1:
      LinearDecompress1D(mantissa_data, exponent_data, row);
      return;
    case 2:
      QuadraticDecompress1D(mantissa_data, exponent_data, row);
      return;
    case 3:
      CubicDecompress1D(mantissa_data, exponent_data, row);
      return;
    case 10:
      Average2Decompress1D(mantissa_data, exponent_data, row);
      return;
    case 11:
      Linear3Decompress1D(mantissa_data, exponent_data, row);
      return;
    case 12:
      Quadratic4Decompress1D(mantissa_data, exponent_data, row);
      return;
  }
  throw std::runtime_error("Invalid compression level");
}

inline void Compress2D(int level, CompressorState& state,
                       std::span<const float> row,
                       std::span<std::byte> mantissa_data,
                       std::span<std::byte> exponent_data) {
  switch (level) {
    case -1:
      DirectCompress2D(row, mantissa_data, exponent_data);
      return;
    case 0:
      DifferenceCompress2D(state, row, mantissa_data, exponent_data);
      return;
    case 1:
      LinearCompress2D(state, row, mantissa_data, exponent_data);
      return;
    case 2:
      QuadraticCompress2D(state, row, mantissa_data, exponent_data);
      return;
    case 3:
      CubicCompress2D(state, row, mantissa_data, exponent_data);
      return;
    case 10:
      Average2Compress2D(state, row, mantissa_data, exponent_data);
      return;
    case 11:
      Linear3Compress2D(state, row, mantissa_data, exponent_data);
      return;
    case 12:
      Quadratic4Compress2D(state, row, mantissa_data, exponent_data);
      return;
    case 21:
      LinearQuadraticCompress2D(state, row, mantissa_data, exponent_data);
      return;
  }
  throw std::runtime_error("Invalid compression level");
}

inline void Decompress2D(int level, CompressorState& state,
                         std::span<std::byte> mantissa_data,
                         std::span<std::byte> exponent_data,
                         std::span<float> row) {
  switch (level) {
    case -1:
      DirectDecompress2D(mantissa_data, exponent_data, row);
      return;
    case 0:
      DifferenceDecompress2D(state, mantissa_data, exponent_data, row);
      return;
    case 1:
      LinearDecompress2D(state, mantissa_data, exponent_data, row);
      return;
    case 2:
      QuadraticDecompress2D(state, mantissa_data, exponent_data, row);
      return;
    case 3:
      CubicDecompress2D(state, mantissa_data, exponent_data, row);
      return;
    case 10:
      Average2Decompress2D(state, mantissa_data, exponent_data, row);
      return;
    case 11:
      Linear3Decompress2D(state, mantissa_data, exponent_data, row);
      return;
    case 12:
      Quadratic4Decompress2D(state, mantissa_data, exponent_data, row);
      return;
    case 21:
      LinearQuadraticDecompress2D(state, mantissa_data, exponent_data, row);
      return;
  }
  throw std::runtime_error("Invalid compression level");
}

/**
 * If the predicted value is approximately twice as large as the target value
 * (based on its exponent only), predict zero instead as we're too far off.
 * This function is tailored for the Predict functions, and expects value to be
 * matched.
 */
inline constexpr BitFloat PredictThreshold(const BitFloat& value) {
  if (value.Mantissa() & 0xFE000000)
    return BitFloat(0, value.Exponent(), false);
  else
    return value;
}

inline constexpr BitFloat Predict(const BitFloat& previous,
                                  int8_t value_exponent) {
  if (!previous.AllowsMath()) {
    return BitFloat(0, value_exponent, false);
  } else if (previous.Exponent() == value_exponent) {
    return previous;
  } else if (previous.Exponent() > value_exponent) {
    // Shift predicted mantissa left to have same exponent
    const uint8_t shift = previous.Exponent() - value_exponent;
    if (shift > 1)
      return BitFloat(0, value_exponent, false);
    else {
      const BitFloat result(previous.Mantissa() << shift, value_exponent,
                            previous.Sign());
      return PredictThreshold(result);
    }
  } else {
    // Shift predicted mantissa right to have same exponent
    const uint8_t shift = value_exponent - previous.Exponent();
    if (shift > 24)
      return BitFloat(0, value_exponent, false);
    else
      return BitFloat(previous.Mantissa() >> shift, value_exponent,
                      previous.Sign());
  }
}

inline constexpr BitFloat Residual(const BitFloat& previous,
                                   const BitFloat& value) {
  const BitFloat predict = Predict(previous, value.Exponent());
  BitFloat residual = value;
  residual -= predict;
  return residual;
}

inline constexpr BitFloat Predict(const BitFloat& previous2,
                                  const BitFloat& previous1,
                                  int8_t value_exponent) {
  if (!previous1.AllowsMath()) {
    return Predict(previous2, value_exponent);
  } else if (!previous2.AllowsMath()) {
    return Predict(previous1, value_exponent);
  } else if (!BitFloat::AllowsMath(value_exponent)) {
    return BitFloat(0, value_exponent, false);
  } else {
    const std::optional<BitFloat> matched_p2 = Match(previous2, value_exponent);
    if (!matched_p2) return BitFloat(0, value_exponent, false);
    std::optional<BitFloat> result = Match(previous1, value_exponent);
    if (!result) return BitFloat(0, value_exponent, false);
    (*result) *= 2;
    (*result) -= *matched_p2;
    return PredictThreshold(*result);
  }
}

inline constexpr BitFloat Residual(const BitFloat& previous2,
                                   const BitFloat& previous1,
                                   const BitFloat& value) {
  const BitFloat predict = Predict(previous2, previous1, value.Exponent());
  BitFloat residual = value;
  residual -= predict;
  return residual;
}

inline constexpr BitFloat AveragePredict(const BitFloat& previous2,
                                         const BitFloat& previous1,
                                         int8_t value_exponent) {
  if (!previous1.AllowsMath()) {
    return Predict(previous2, value_exponent);
  } else if (!previous2.AllowsMath()) {
    return Predict(previous1, value_exponent);
  } else if (!BitFloat::AllowsMath(value_exponent)) {
    return BitFloat(0, value_exponent, false);
  } else {
    const std::optional<BitFloat> matched_p2 = Match(previous2, value_exponent);
    if (!matched_p2) return BitFloat(0, value_exponent, false);
    std::optional<BitFloat> result = Match(previous1, value_exponent);
    if (!result) return BitFloat(0, value_exponent, false);
    (*result) += *matched_p2;
    (*result) /= 2;
    return PredictThreshold(*result);
  }
}

inline constexpr BitFloat Predict(const BitFloat& previous3,
                                  const BitFloat& previous2,
                                  const BitFloat& previous1,
                                  int8_t value_exponent) {
  if (!previous1.AllowsMath()) {
    return Predict(previous2, value_exponent);
  } else if (!previous2.AllowsMath()) {
    return Predict(previous1, value_exponent);
  } else if (!previous3.AllowsMath()) {
    return Predict(previous2, previous1, value_exponent);
  } else if (!BitFloat::AllowsMath(value_exponent)) {
    return BitFloat(0, value_exponent, false);
  } else {
    const std::optional<BitFloat> matched_p3 = Match(previous3, value_exponent);
    if (!matched_p3) return Predict(previous2, previous1, value_exponent);
    std::optional<BitFloat> matched_p2 = Match(previous2, value_exponent);
    if (!matched_p2) return Predict(previous1, value_exponent);
    std::optional<BitFloat> result = Match(previous1, value_exponent);
    if (!result) return Predict(previous2, value_exponent);
    // We have to calculate:
    // p1 + dp12 + (dp12 - dp23)
    // = p1 + (p1 - p2) + (p1 - p2) - (p2 - p3)
    // = 3p1 - 3p2 + p3
    (*result) *= 3;
    (*matched_p2) *= 3;
    (*result) -= *matched_p2;
    (*result) += *matched_p3;
    return PredictThreshold(*result);
  }
}

inline constexpr BitFloat LinearPredict(const BitFloat& previous3,
                                        const BitFloat& previous2,
                                        const BitFloat& previous1,
                                        int8_t value_exponent) {
  if (!previous1.AllowsMath()) {
    return Predict(previous2, value_exponent);
  } else if (!previous2.AllowsMath()) {
    return Predict(previous1, value_exponent);
  } else if (!previous3.AllowsMath()) {
    return Predict(previous2, previous1, value_exponent);
  } else if (!BitFloat::AllowsMath(value_exponent)) {
    return BitFloat(0, value_exponent, false);
  } else {
    std::optional<BitFloat> matched_p3 = Match(-previous3, value_exponent);
    if (!matched_p3) return Predict(previous2, previous1, value_exponent);
    const std::optional<BitFloat> matched_p2 = Match(previous2, value_exponent);
    if (!matched_p2) return Predict(previous1, value_exponent);
    std::optional<BitFloat> result = Match(previous1, value_exponent);
    if (!result) return Predict(previous2, value_exponent);
    // We have to calculate:
    // ( -2 p3 + p2 + 4 p1 ) / 3
    // Note that sign of p3 was already flipped
    (*result) *= 4;
    (*result) += *matched_p2;
    (*matched_p3) *= 2;
    (*result) += *matched_p3;
    (*result) /= 3;
    return PredictThreshold(*result);
  }
}

inline constexpr BitFloat Predict(const BitFloat& previous4,
                                  const BitFloat& previous3,
                                  const BitFloat& previous2,
                                  const BitFloat& previous1,
                                  int8_t value_exponent) {
  if (!previous1.AllowsMath()) {
    return Predict(previous2, value_exponent);
  } else if (!previous2.AllowsMath()) {
    return Predict(previous1, value_exponent);
  } else if (!previous3.AllowsMath()) {
    return Predict(previous2, previous1, value_exponent);
  } else if (!previous4.AllowsMath()) {
    return Predict(previous3, previous2, previous1, value_exponent);
  } else if (!BitFloat::AllowsMath(value_exponent)) {
    return BitFloat(0, value_exponent, false);
  } else {
    const std::optional<BitFloat> matched_p4 = Match(previous4, value_exponent);
    if (!matched_p4)
      return Predict(previous3, previous2, previous1, value_exponent);
    std::optional<BitFloat> matched_p3 = Match(previous3, value_exponent);
    if (!matched_p3) return Predict(previous2, previous1, value_exponent);
    std::optional<BitFloat> matched_p2 = Match(previous2, value_exponent);
    if (!matched_p2) return Predict(previous1, value_exponent);
    std::optional<BitFloat> result = Match(previous1, value_exponent);
    if (!result) return Predict(previous2, value_exponent);
    // We have to calculate:
    // p_next = -p4 + 4p3 - 6p2 + 4p1
    (*result) *= 4;
    (*matched_p2) *= 6;
    (*result) -= *matched_p2;
    (*matched_p3) *= 4;
    (*result) += *matched_p3;
    (*result) -= *matched_p4;
    return PredictThreshold(*result);
  }
}

inline constexpr BitFloat QuadraticPredict(const BitFloat& previous4,
                                           const BitFloat& previous3,
                                           const BitFloat& previous2,
                                           const BitFloat& previous1,
                                           int8_t value_exponent) {
  if (!previous1.AllowsMath()) {
    return Predict(previous2, value_exponent);
  } else if (!previous2.AllowsMath()) {
    return Predict(previous1, value_exponent);
  } else if (!previous3.AllowsMath()) {
    return Predict(previous2, previous1, value_exponent);
  } else if (!previous4.AllowsMath()) {
    return Predict(previous3, previous2, previous1, value_exponent);
  } else if (!BitFloat::AllowsMath(value_exponent)) {
    return BitFloat(0, value_exponent, false);
  } else {
    std::optional<BitFloat> matched_p4 = Match(previous4, value_exponent);
    if (!matched_p4)
      return Predict(previous3, previous2, previous1, value_exponent);
    std::optional<BitFloat> matched_p3 = Match(previous3, value_exponent);
    if (!matched_p3) return Predict(previous2, previous1, value_exponent);
    std::optional<BitFloat> matched_p2 = Match(previous2, value_exponent);
    if (!matched_p2) return Predict(previous1, value_exponent);
    std::optional<BitFloat> result = Match(previous1, value_exponent);
    if (!result) return Predict(previous2, value_exponent);
    // We have to calculate:
    // p_next = ( 3p4 - 5p3 - 3p2 + 9p1 ) / 4
    (*result) *= 9;
    (*matched_p2) *= 3;
    (*result) -= *matched_p2;
    (*matched_p3) *= 5;
    (*result) -= *matched_p3;
    (*matched_p4) *= 3;
    (*result) += *matched_p4;
    (*result) /= 4;
    return PredictThreshold(*result);
  }
}

}  // namespace casacore::sisco

#endif
