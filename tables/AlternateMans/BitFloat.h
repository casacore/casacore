#ifndef SISCO_BIT_FLOAT_H_
#define SISCO_BIT_FLOAT_H_

#include <bit>
#include <cinttypes>
#include <limits>
#include <optional>
#include <stdexcept>
#include <string_view>

namespace casacore {

enum class BitFloatKind : uint8_t {
  Zero,
  NegativeZero,
  Normal,
  NaN,
  SignallingNaN,
  Infinity,
  NegativeInfinity,
  Subnormal
};

constexpr std::string_view ToString(BitFloatKind kind) {
  switch (kind) {
    case BitFloatKind::Zero:
      return "zero";
    case BitFloatKind::NegativeZero:
      return "negative zero";
    case BitFloatKind::Normal:
      return "normal";
    case BitFloatKind::NaN:
      return "nan";
    case BitFloatKind::SignallingNaN:
      return "signalling nan";
    case BitFloatKind::Infinity:
      return "infinity";
    case BitFloatKind::NegativeInfinity:
      return "negative infinity";
    case BitFloatKind::Subnormal:
      return "subnormal";
  }
  __builtin_unreachable();
}

class BitFloat {
 public:
  constexpr BitFloat() : mantissa_(0), exponent_(-127), sign_(false) {}

  constexpr explicit BitFloat(float f)
      : mantissa_((std::bit_cast<uint32_t>(f) & 0x007FFFFF) | 0x00800000),
        exponent_(static_cast<int8_t>(
                      (std::bit_cast<uint32_t>(f) & 0x7F800000) >> 23) -
                  127),
        sign_(std::bit_cast<uint32_t>(f) & 0x80000000) {
    if (f == 0.0f) mantissa_ = 0;
  }

  constexpr BitFloat(uint32_t mantissa, int8_t exponent, bool sign)
      : mantissa_(mantissa), exponent_(exponent), sign_(sign) {}

  constexpr uint32_t Mantissa() const { return mantissa_; }
  constexpr int8_t Exponent() const { return exponent_; }
  constexpr uint32_t PackMantissa() const {
    if (MantissaOverflow()) {
      throw std::runtime_error(
          "An overflow occured! Value = " + std::to_string(ToFloat()) +
          ", exponent = " + std::to_string(exponent_) + ", mantissa = " +
          std::to_string(mantissa_) + ", sign = " + std::to_string(sign_));
    }
    return (mantissa_ & 0x7FFFFFFFu) | (sign_ ? 0x80000000u : 0u);
  }
  constexpr static std::pair<uint32_t, bool> UnpackMantissa(
      uint32_t mantissa_with_sign) {
    const uint32_t mantissa = (mantissa_with_sign & 0x7FFFFFFFu);
    const bool sign = (mantissa_with_sign & 0x80000000u) != 0u;
    return {mantissa, sign};
  }
  constexpr static BitFloat FromCompressed(uint32_t mantissa_with_sign,
                                           int8_t exponent) {
    const std::pair<uint32_t, bool> unpacked =
        UnpackMantissa(mantissa_with_sign);
    return BitFloat(unpacked.first, exponent, unpacked.second);
  }
  constexpr bool Sign() const { return sign_; }
  /// lhs and rhs must be exponent-matched beforehand
  constexpr BitFloat& operator+=(const BitFloat& rhs) {
    if (sign_ == rhs.sign_) {
      mantissa_ = mantissa_ + rhs.mantissa_;
    } else if (mantissa_ >= rhs.mantissa_) {
      mantissa_ = mantissa_ - rhs.mantissa_;
    } else {
      mantissa_ = rhs.mantissa_ - mantissa_;
      sign_ = !sign_;
    }
    return *this;
  }
  /// lhs and rhs must be exponent-matched beforehand
  constexpr BitFloat& operator-=(const BitFloat& rhs) {
    if (sign_ != rhs.sign_) {
      mantissa_ = mantissa_ + rhs.mantissa_;
    } else if (mantissa_ >= rhs.mantissa_) {
      mantissa_ = mantissa_ - rhs.mantissa_;
    } else {
      mantissa_ = rhs.mantissa_ - mantissa_;
      sign_ = !sign_;
    }
    return *this;
  }
  constexpr BitFloat& operator*=(unsigned factor) {
    mantissa_ *= factor;
    return *this;
  }
  constexpr float ToFloat() const {
    int8_t exponent = exponent_;
    uint32_t result = mantissa_;
    // Exponent values of +128 and -127 have special meaning.
    // 128 will be wrapped.
    if (exponent_ != static_cast<int8_t>(128u) && exponent_ != -127) {
      if (mantissa_ == 0) return sign_ ? -0.0f : 0.0f;
      while (result & 0xFF000000) {
        ++exponent;
        result = (result >> 1);
      }
      while ((result & 0x00800000) == 0) {
        --exponent;
        result = (result << 1);
      }
    }
    // The double cast of the exponent is necessary to prevent sign extension.
    result =
        (result & 0x007FFFFF) |
        (static_cast<uint32_t>(static_cast<uint8_t>(exponent + 127)) << 23) |
        (sign_ ? 0x80000000 : 0x0);
    return std::bit_cast<float>(result);
  }
  constexpr operator float() const { return ToFloat(); }

  static constexpr bool AllowsMath(int8_t exponent) {
    return exponent != static_cast<int8_t>(128u) && exponent != -127;
  }

  constexpr bool AllowsMath() const { return AllowsMath(exponent_); }

  friend constexpr BitFloat operator-(const BitFloat& input) {
    return BitFloat(input.mantissa_, input.exponent_, !input.sign_);
  }

  friend constexpr bool operator==(const BitFloat& lhs, const BitFloat& rhs) {
    return lhs.mantissa_ == rhs.mantissa_ && lhs.exponent_ == rhs.exponent_ &&
           lhs.sign_ == rhs.sign_;
  }

  friend constexpr std::optional<BitFloat> Match(const BitFloat& input,
                                                 int8_t value_exponent) {
    if (input.Exponent() == value_exponent) {
      return input;
    } else if (input.Exponent() > value_exponent) {
      const uint8_t shift = input.Exponent() - value_exponent;
      if (shift > 7)
        return {};
      else
        return BitFloat(input.Mantissa() << shift, value_exponent,
                        input.Sign());
    } else {
      const uint8_t shift = value_exponent - input.Exponent();
      if (shift > 24)
        return BitFloat(0, value_exponent, input.Sign());
      else
        return BitFloat(input.Mantissa() >> shift, value_exponent,
                        input.Sign());
    }
  }

  constexpr bool MantissaOverflow() const { return mantissa_ & 0x80000000; }

  constexpr static BitFloatKind GetKind(float f) {
    const uint32_t value = std::bit_cast<uint32_t>(f);
    if (value == 0) {
      return BitFloatKind::Zero;
    } else if (value == 0x80000000) {
      return BitFloatKind::NegativeZero;
    } else if ((value & 0x7F800000) == 0x7F800000) {
      // exponent is 128
      if (value & 0x00400000)
        return BitFloatKind::NaN;
      else
        return BitFloatKind::SignallingNaN;
    } else if ((value & 0x7FFFFFFF) == 0b1111111100000000000000000000000) {
      if (value & 0x80000000)
        return BitFloatKind::NegativeInfinity;
      else
        return BitFloatKind::Infinity;
    } else if ((value & 0x7F800000) == 0) {
      return BitFloatKind::Subnormal;
    } else {
      return BitFloatKind::Normal;
    }
  }

 private:
  uint32_t mantissa_;
  int8_t exponent_;
  bool sign_;
};

}  // namespace casacore

#endif
