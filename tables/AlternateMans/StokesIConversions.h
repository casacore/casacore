#ifndef CASACORE_ALTERNATE_MANS_STOKES_I_CONVERSIONS_H_
#define CASACORE_ALTERNATE_MANS_STOKES_I_CONVERSIONS_H_

#include <stdexcept>

namespace casacore {

/**
 * Expands @p n values from single Stokes I values
 * to have 4 values, in place. This implies the array
 * should have place to store n*4 values.
 */
template <typename T>
inline void ExpandFromStokesI(T *data, size_t n) {
  for (size_t i = 0; i != n; ++i) {
    const size_t index = n - i - 1;
    const T value = data[index];
    data[index * 4] = value;
    data[index * 4 + 1] = T();
    data[index * 4 + 2] = T();
    data[index * 4 + 3] = value;
  }
}

template <>
inline void ExpandFromStokesI(bool *data, size_t n) {
  for (size_t i = 0; i != n; ++i) {
    const size_t index = n - i - 1;
    const bool value = data[index];
    data[index * 4] = value;
    data[index * 4 + 1] = value;
    data[index * 4 + 2] = value;
    data[index * 4 + 3] = value;
  }
}

/**
 * Performs in-place expansion of @p n pair of diagonal values such that each pair
 * becomes 4 values with zeros for the off-diagonal elements.
 * This implies the array should have place to store n*4 values.
 */
template <typename T>
inline void ExpandFromDiagonal(T *data, size_t n) {
  for (size_t i = n*2; i > 0; i -= 2) {
    const size_t index = i - 2;
    data[index * 2 + 3] = data[index + 1];
    data[index * 2 + 2] = T();
    data[index * 2 + 1] = T();
    data[index * 2] = data[index];
  }
}

/**
 * Calculates for every set of 4 input values the Stokes-I values by
 * doing out = 0.5 * (in_pp + in_qq), where pp/qq can be xx/yy or
 * ll/rr. If pq or qp is non-zero, an exception is thrown.
 *
 * If type T is bool, then out = in_pp || in_qq.
 *
 * @param input buffer of size @p n*4 values
 * @param buffer destination of size @p n*2 values
 * @param n is the number of groups of 4 values to convert.
 */
template <typename T>
inline T *TransformToStokesI(const T *input, T *buffer, size_t n) {
  for (size_t i = 0; i != n; ++i) {
    buffer[i] = T((input[i * 4] + input[i * 4 + 3]) * T(0.5));
    if (input[i * 4 + 1] != T(0) || input[i * 4 + 2] != T(0))
      throw std::runtime_error(
          "Stokes-I storage modes cannot store data for which the 2nd and 3rd "
          "correlation are non-zero");
    // While we could also check whether pp == qq, this is a bit more
    // complicated because of rounding inaccuracies. The above check should
    // catch the most crucial misuse of the stman, so a pp == qq check is not
    // performed.
  }
  return buffer;
}

template <>
inline bool *TransformToStokesI(const bool *input, bool *buffer, size_t n) {
  for (size_t i = 0; i != n; ++i) {
    const bool a = input[i * 4];
    const bool b = input[i * 4 + 1];
    const bool c = input[i * 4 + 2];
    const bool d = input[i * 4 + 3];
    if (a != b || a != c || a != d)
      throw std::runtime_error(
          "Stokes-I storage modes cannot store boolean data for which not all "
          "correlations are equal");
    buffer[i] = a;
  }
  return buffer;
}

/**
 * Check if every set of 4 input values contains only non-zeros on
 * the diagonal (pp or qq).
 * If pq or qp is non-zero, an exception is thrown.
 *
 * @param input buffer of size @p n*4 values
 * @param n is the number of groups of 4 values to convert.
 */
template <typename T>
inline void CheckIsDiagonal(const T *input, size_t n) {
  for (size_t i = 0; i != n; ++i) {
    const T b = input[i * 4 + 1];
    const T c = input[i * 4 + 2];
    if (b != T(0) || c != T(0))
      throw std::runtime_error(
          "Diagonal storage modes cannot store data for which the 2nd and 3rd "
          "correlation are non-zero");
  }
}

}  // namespace casacore

#endif
