#ifndef CASACORE_ALTERNATE_MANS_STOKES_I_CONVERSIONS_H_
#define CASACORE_ALTERNATE_MANS_STOKES_I_CONVERSIONS_H_

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
 * Calculates for every set of 4 input values the Stokes-I values by
 * doing out = 0.5 * (in_pp + in_qq), where pp/qq can be xx/yy or
 * ll/rr. If pq or qp is non-zero, an exception is thrown.
 *
 * If type T is bool, then out = in_pp || in_qq.
 */
template <typename T>
inline T *TransformToStokesI(const T *input, T *buffer, size_t n) {
  for (size_t i = 0; i != n; ++i) {
    // Placement new is used, because the lifetime of type T needs
    // to be started.
    buffer[i] = T((input[i * 4] + input[i * 4 + 3]) * T(0.5));
    if (input[i * 4 + 1] != T(0.0) || input[i * 4 + 2] != T(0.0))
      throw std::runtime_error(
          "Stokes-I storage modes cannot store data for which the 2nd and 3rd "
          "correlation are non-zero");
    // While we could also check whether pp == qq, this is a bit more
    // complicated because of rounding inaccuracies. The above check should
    // catch the most crucial misuse of the stman, so a pp == qq check is not
    // performed.
  }
  return reinterpret_cast<T *>(buffer);
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
  return reinterpret_cast<bool *>(buffer);
}

}  // namespace casacore

#endif
