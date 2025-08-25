#include "Sisco.h"

#include <cassert>

namespace casacore {

void DifferenceCompress1D(std::span<const BitFloat> input, std::span<std::byte> mantissa_data, std::span<std::byte> exponent_data) {
  uint32_t* mantissas = reinterpret_cast<uint32_t*>(mantissa_data.data());
  uint8_t* exponents = reinterpret_cast<uint8_t*>(exponent_data.data());

  BitFloat previous(input[0]);
  mantissas[0] = previous.PackMantissa();
  exponents[0] = previous.Exponent();

  for(size_t i=1; i!=input.size(); ++i) {
    const BitFloat predicted = Predict(previous, input[i].Exponent());
    BitFloat value = input[i];
    if(value.AllowsMath())
      value -= predicted;
    previous = input[i];
    mantissas[i] = value.PackMantissa();
    exponents[i] = value.Exponent();
  }
}

void LinearCompress1D(std::span<const BitFloat> input, std::span<std::byte> mantissa_data, std::span<std::byte> exponent_data) {
  uint32_t* mantissas = reinterpret_cast<uint32_t*>(mantissa_data.data());
  uint8_t* exponents = reinterpret_cast<uint8_t*>(exponent_data.data());

  BitFloat previous2(input[0]);
  mantissas[0] = previous2.PackMantissa();
  exponents[0] = previous2.Exponent();

  BitFloat value(input[1]);
  BitFloat previous1(value);
  BitFloat predicted = Predict(previous2, value.Exponent());
  if(value.AllowsMath())
    value -= predicted;
  mantissas[1] = value.PackMantissa();
  exponents[1] = value.Exponent();

  for(size_t i=2; i!=input.size(); ++i) {
    value = input[i];
    predicted = Predict(previous2, previous1, value.Exponent());
    previous2 = previous1;
    previous1 = value;
    if(value.AllowsMath())
      value -= predicted;
    mantissas[i] = value.PackMantissa();
    exponents[i] = value.Exponent();
  }
}

void QuadraticCompress1D(std::span<const BitFloat> input, std::span<std::byte> mantissa_data, std::span<std::byte> exponent_data) {
  if(input.size() == 0)
    return;

  uint32_t* mantissas = reinterpret_cast<uint32_t*>(mantissa_data.data());
  uint8_t* exponents = reinterpret_cast<uint8_t*>(exponent_data.data());

  BitFloat previous3(input[0]);
  mantissas[0] = input[0].PackMantissa();
  exponents[0] = input[0].Exponent();
  if(input.size() == 1)
    return;

  BitFloat value(input[1]);
  BitFloat previous2(input[1]);
  if(value.AllowsMath())
    value -= Predict(previous3, value.Exponent());
  mantissas[1] = value.PackMantissa();
  exponents[1] = value.Exponent();
  if(input.size() == 2)
    return;

  value = input[2];
  BitFloat previous1(value);
  if(value.AllowsMath())
    value -= Predict(previous3, previous2, value.Exponent());
  mantissas[2] = value.PackMantissa();
  exponents[2] = value.Exponent();

  for(size_t i=3; i!=input.size(); ++i) {
    value = input[i];
    if(value.AllowsMath())
      value -= Predict(previous3, previous2, previous1, value.Exponent());
    mantissas[i] = value.PackMantissa();
    exponents[i] = value.Exponent();
    previous3 = previous2;
    previous2 = previous1;
    previous1 = input[i];
  }
}

void DifferenceDecompress1D(std::span<const std::byte> mantissa_data,
                        std::span<const std::byte> exponent_data,
                        std::span<BitFloat> output) {
  const uint32_t* mantissas = reinterpret_cast<const uint32_t*>(mantissa_data.data());
  const int8_t* exponents = reinterpret_cast<const int8_t*>(exponent_data.data());

  BitFloat p1 = BitFloat::FromCompressed(mantissas[0], exponents[0]);
  output[0] = p1;

  for(size_t i=1; i!=output.size(); ++i) {
    const std::pair<uint32_t, bool> mantissa_and_sign = BitFloat::UnpackMantissa(mantissas[i]);
    output[i] = BitFloat(mantissa_and_sign.first, exponents[i], mantissa_and_sign.second);
    if(output[i].AllowsMath())
      output[i] += Predict(p1, exponents[i]);
    p1 = output[i];
  }
}

void LinearDecompress1D(std::span<const std::byte> mantissa_data,
                        std::span<const std::byte> exponent_data,
                        std::span<BitFloat> output) {
  const uint32_t* dmantissas = reinterpret_cast<const uint32_t*>(mantissa_data.data());
  const int8_t* dexponents = reinterpret_cast<const int8_t*>(exponent_data.data());

  BitFloat p2 = BitFloat::FromCompressed(dmantissas[0], dexponents[0]);
  output[0] = p2;

  std::pair<uint32_t, bool> mantissa_and_sign = BitFloat::UnpackMantissa(dmantissas[1]);
  BitFloat p1(mantissa_and_sign.first, dexponents[1], mantissa_and_sign.second);
  if(p1.AllowsMath())
    p1 += Predict(p2, dexponents[1]);
  output[1] = p1;

  for(size_t i=2; i!=output.size(); ++i) {
    mantissa_and_sign = BitFloat::UnpackMantissa(dmantissas[i]);
    output[i] = BitFloat(mantissa_and_sign.first, dexponents[i], mantissa_and_sign.second);
    if(output[i].AllowsMath())
      output[i] += Predict(p2, p1, dexponents[i]);
    p2 = p1;
    p1 = output[i];
  }
}

void QuadraticDecompress1D(std::span<const std::byte> mantissa_data, std::span<const std::byte> exponent_data, std::span<BitFloat> output) {
  if(output.empty())
    return;
  const uint32_t* dmantissas = reinterpret_cast<const uint32_t*>(mantissa_data.data());
  const int8_t* dexponents = reinterpret_cast<const int8_t*>(exponent_data.data());

  BitFloat p3 = BitFloat::FromCompressed(dmantissas[0], dexponents[0]);
  output[0] = p3;
  if(output.size() == 1)
    return;

  std::pair<uint32_t, bool> mantissa_and_sign = BitFloat::UnpackMantissa(dmantissas[1]);
  BitFloat p2(mantissa_and_sign.first, dexponents[1], mantissa_and_sign.second);
  if(p2.AllowsMath())
    p2 += Predict(p3, dexponents[1]);
  output[1] = p2;
  if(output.size() == 2)
    return;

  mantissa_and_sign = BitFloat::UnpackMantissa(dmantissas[2]);
  BitFloat p1(mantissa_and_sign.first, dexponents[2], mantissa_and_sign.second);
  if(p1.AllowsMath())
    p1 += Predict(p3, p2, dexponents[2]);
  output[2] = p1;

  for(size_t i=3; i!=output.size(); ++i) {
    mantissa_and_sign = BitFloat::UnpackMantissa(dmantissas[i]);
    output[i] = BitFloat(mantissa_and_sign.first, dexponents[i], mantissa_and_sign.second);
    if(output[i].AllowsMath())
      output[i] += Predict(p3, p2, p1, dexponents[i]);
    p3 = p2;
    p2 = p1;
    p1 = output[i];
  }
}

void DirectCompress2D(std::span<const float> row, std::span<std::byte> mantissa_data, std::span<std::byte> exponent_data) {
  uint32_t* dmantissas = reinterpret_cast<uint32_t*>(mantissa_data.data());
  int8_t* dexponents = reinterpret_cast<int8_t*>(exponent_data.data());
  for(size_t i=0; i!=row.size(); ++i) {
    const BitFloat value(row[i]);
    dmantissas[i] = value.PackMantissa();
    dexponents[i] = value.Exponent();
  }
}

void DifferenceCompress2D(CompressorState& state, std::span<const float> row, std::span<std::byte> mantissa_data, std::span<std::byte> exponent_data) {
  if(state.previous1.empty()) {
    // No state: compress raw values
    state.previous1.reserve(row.size());
    state.scratch.resize(row.size());
    for(const float value : row) {
      state.previous1.emplace_back(value);
    }
    DifferenceCompress1D(state.previous1, mantissa_data, exponent_data);
  } else {
    assert(state.previous1.size() == row.size());
    assert(state.scratch.size() == row.size());
    for(size_t i=0; i!=row.size(); ++i) {
      state.scratch[i] = BitFloat(row[i]);
      const BitFloat predicted = Predict(state.previous1[i], state.scratch[i].Exponent());
      state.previous1[i] = state.scratch[i];
      if(state.scratch[i].AllowsMath())
        state.scratch[i] -= predicted;
    }
    DifferenceCompress1D(state.scratch, mantissa_data, exponent_data);
  }
}

void LinearCompress2D(CompressorState& state, std::span<const float> row, std::span<std::byte> mantissa_data, std::span<std::byte> exponent_data) {
  if(state.previous1.empty()) {
    // No state: compress raw values
    state.previous1.reserve(row.size());
    for(size_t i=0; i!=row.size(); ++i) {
      state.previous1.emplace_back(row[i]);
    }
    LinearCompress1D(state.previous1, mantissa_data, exponent_data);

  } else if(state.previous2.empty()) {
    assert(state.previous1.size() == row.size());
    assert(state.scratch.empty());
    // Single previous value available
    state.previous2 = std::move(state.previous1);
    state.previous1.reserve(row.size());
    state.scratch.reserve(row.size());
    for(size_t i=0; i!=row.size(); ++i) {
      const BitFloat& value = state.previous1.emplace_back(row[i]);
      BitFloat& scratch_value = state.scratch.emplace_back(value);
      if(scratch_value.AllowsMath())
        scratch_value -= Predict(state.previous2[i], scratch_value.Exponent());
    }
    LinearCompress1D(state.scratch, mantissa_data, exponent_data);

  } else {
    assert(state.previous1.size() == row.size());
    assert(state.previous2.size() == row.size());
    assert(state.scratch.size() == row.size());
    for(size_t i=0; i!=row.size(); ++i) {
      state.scratch[i] = BitFloat(row[i]);
      BitFloat predicted = Predict(state.previous2[i], state.previous1[i], state.scratch[i].Exponent());
      state.previous2[i] = state.previous1[i];
      state.previous1[i] = state.scratch[i];
      if(state.scratch[i].AllowsMath())
        state.scratch[i] -= predicted;
    }
    LinearCompress1D(state.scratch, mantissa_data, exponent_data);
  }
}

void QuadraticCompress2D(CompressorState& state, std::span<const float> row, std::span<std::byte> mantissa_data, std::span<std::byte> exponent_data) {
  if(state.previous1.empty()) {
    // No state: compress raw values
    state.previous1.reserve(row.size());
    for(size_t i=0; i!=row.size(); ++i) {
      state.previous1.emplace_back(row[i]);
    }
    QuadraticCompress1D(state.previous1, mantissa_data, exponent_data);

  } else if(state.previous2.empty()) {
    assert(state.previous1.size() == row.size());
    assert(state.scratch.empty());
    // Single previous value available
    state.previous2 = std::move(state.previous1);
    state.previous1.reserve(row.size());
    state.scratch.reserve(row.size());
    for(size_t i=0; i!=row.size(); ++i) {
      const BitFloat& value = state.previous1.emplace_back(row[i]);
      BitFloat& scratch_value = state.scratch.emplace_back(value);
      if(scratch_value.AllowsMath())
        scratch_value -= Predict(state.previous2[i], scratch_value.Exponent());
    }
    QuadraticCompress1D(state.scratch, mantissa_data, exponent_data);

  } else if(state.previous3.empty()) {
    assert(state.previous1.size() == row.size());
    assert(state.previous2.size() == row.size());
    assert(state.scratch.size() == row.size());
    // Two previous values available
    state.previous3 = std::move(state.previous2);
    state.previous2 = std::move(state.previous1);
    state.previous1.reserve(row.size());
    for(size_t i=0; i!=row.size(); ++i) {
      const BitFloat& value = state.previous1.emplace_back(row[i]);
      state.scratch[i] = value;
      if(state.scratch[i].AllowsMath())
        state.scratch[i] -= Predict(state.previous3[i], state.previous2[i], state.scratch[i].Exponent());
    }
    QuadraticCompress1D(state.scratch, mantissa_data, exponent_data);

  } else {
    assert(state.previous1.size() == row.size());
    assert(state.previous2.size() == row.size());
    assert(state.previous3.size() == row.size());
    assert(state.scratch.size() == row.size());
    // Three previous values available
    for(size_t i=0; i!=row.size(); ++i) {
      state.scratch[i] = BitFloat(row[i]);
      BitFloat predicted = Predict(state.previous3[i], state.previous2[i], state.previous1[i], state.scratch[i].Exponent());
      state.previous3[i] = state.previous2[i];
      state.previous2[i] = state.previous1[i];
      state.previous1[i] = state.scratch[i];
      if(state.scratch[i].AllowsMath())
        state.scratch[i] -= predicted;
    }
    QuadraticCompress1D(state.scratch, mantissa_data, exponent_data);
  }
}

void DirectDecompress2D(std::span<std::byte> mantissa_data, std::span<std::byte> exponent_data, std::span<float> row) {
  const uint32_t* dmantissas = reinterpret_cast<const uint32_t*>(mantissa_data.data());
  const int8_t* dexponents = reinterpret_cast<const int8_t*>(exponent_data.data());
  for(size_t i=0; i!=row.size(); ++i) {
    const BitFloat value = BitFloat::FromCompressed(dmantissas[i], dexponents[i]);
    row[i] = value.ToFloat();
  }
}

void DifferenceDecompress2D(CompressorState& state, std::span<std::byte> mantissa_data, std::span<std::byte> exponent_data, std::span<float> row) {
  if(state.previous1.empty()) {
    // No state: decompress raw values
    state.previous1.resize(row.size());
    state.scratch.resize(row.size());
    DifferenceDecompress1D(mantissa_data, exponent_data, state.previous1);
    for(size_t i=0; i!=row.size(); ++i) {
      row[i] = state.previous1[i].ToFloat();
    }
  } else {
    assert(state.previous1.size() == row.size());
    assert(state.scratch.size() == row.size());
    // Previous values available
    DifferenceDecompress1D(mantissa_data, exponent_data, state.scratch);
    for(size_t i=0; i!=row.size(); ++i) {
      BitFloat& value = state.scratch[i];
      if(value.AllowsMath())
        value += Predict(state.previous1[i], value.Exponent());
      row[i] = value.ToFloat();
    }
    std::swap(state.scratch, state.previous1);
  }
}

void LinearDecompress2D(CompressorState& state, std::span<std::byte> mantissa_data, std::span<std::byte> exponent_data, std::span<float> row) {
  if(state.previous1.empty()) {
    // No state: decompress raw values
    state.previous1.resize(row.size());
    LinearDecompress1D(mantissa_data, exponent_data, state.previous1);
    for(size_t i=0; i!=row.size(); ++i) {
      row[i] = state.previous1[i].ToFloat();
    }
  } else if(state.previous2.empty()) {
    assert(state.previous1.size() == row.size());
    // Single previous value available
    state.previous2 = std::move(state.previous1);
    state.previous1.resize(row.size());
    state.scratch.resize(row.size());
    LinearDecompress1D(mantissa_data, exponent_data, state.previous1);
    for(size_t i=0; i!=row.size(); ++i) {
      BitFloat& value = state.previous1[i];
      if(value.AllowsMath())
        value += Predict(state.previous2[i], value.Exponent());
      row[i] = value.ToFloat();
    }
  } else {
    assert(state.previous1.size() == row.size());
    assert(state.previous2.size() == row.size());
    assert(state.scratch.size() == row.size());
    // Two previous values available
    LinearDecompress1D(mantissa_data, exponent_data, state.scratch);
    for(size_t i=0; i!=row.size(); ++i) {
      BitFloat& value = state.scratch[i];
      if(value.AllowsMath())
        value += Predict(state.previous2[i], state.previous1[i], value.Exponent());
      row[i] = value.ToFloat();
    }
    std::swap(state.scratch, state.previous1);
    // Use previous2 storage for scratch in next call
    std::swap(state.scratch, state.previous2);
  }
}

void QuadraticDecompress2D(CompressorState& state, std::span<std::byte> mantissa_data, std::span<std::byte> exponent_data, std::span<float> row) {
  if(state.previous1.empty()) {
    // No state: decompress raw values
    state.previous1.resize(row.size());
    QuadraticDecompress1D(mantissa_data, exponent_data, state.previous1);
    for(size_t i=0; i!=row.size(); ++i) {
      row[i] = state.previous1[i].ToFloat();
    }
  } else if(state.previous2.empty()) {
    assert(state.previous1.size() == row.size());
    // Single previous value available
    state.previous2 = std::move(state.previous1);
    state.previous1.resize(row.size());
    QuadraticDecompress1D(mantissa_data, exponent_data, state.previous1);
    for(size_t i=0; i!=row.size(); ++i) {
      BitFloat& value = state.previous1[i];
      if(value.AllowsMath())
        value += Predict(state.previous2[i], value.Exponent());
      row[i] = value.ToFloat();
    }

  } else if(state.previous3.empty()) {
    assert(state.previous1.size() == row.size());
    assert(state.previous2.size() == row.size());
    // Two previous values available
    state.previous3 = std::move(state.previous2);
    state.previous2 = std::move(state.previous1);
    state.previous1.resize(row.size());
    QuadraticDecompress1D(mantissa_data, exponent_data, state.previous1);
    for(size_t i=0; i!=row.size(); ++i) {
      BitFloat& value = state.previous1[i];
      if(value.AllowsMath())
        value += Predict(state.previous3[i], state.previous2[i], value.Exponent());
      row[i] = value.ToFloat();
    }
    state.scratch.resize(row.size());

  } else {
    assert(state.previous1.size() == row.size());
    assert(state.previous2.size() == row.size());
    assert(state.previous3.size() == row.size());
    assert(state.scratch.size() == row.size());
    // Three previous values available
    QuadraticDecompress1D(mantissa_data, exponent_data, state.scratch);
    for(size_t i=0; i!=row.size(); ++i) {
      BitFloat& value = state.scratch[i];
      if(value.AllowsMath())
        value += Predict(state.previous3[i], state.previous2[i], state.previous1[i], value.Exponent());
      row[i] = value.ToFloat();
    }
    std::swap(state.scratch, state.previous1);
    std::swap(state.scratch, state.previous2);
    // Use previous3 storage for scratch in next call
    std::swap(state.scratch, state.previous3);
  }
}

} // namespace casacore
