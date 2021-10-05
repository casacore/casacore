#include "stochasticencoder.h"

#include <gsl/gsl_cdf.h>
#include <gsl/gsl_sf_erf.h>

#include <cmath>
#include <limits>

#ifndef M_SQRT2l
#define M_SQRT2l 1.4142135623730950488016887242096981L
#endif

namespace dyscostman {

template <typename ValueType>
inline typename StochasticEncoder<ValueType>::num_t
StochasticEncoder<ValueType>::cumulative(num_t x) {
  return num_t(0.5) + num_t(0.5) * gsl_sf_erf(x / num_t(M_SQRT2l));
}

template <typename ValueType>
typename StochasticEncoder<ValueType>::num_t
StochasticEncoder<ValueType>::invCumulative(num_t c, num_t err) {
  if (c < 0.5)
    return (-invCumulative(1.0 - c, err));
  else if (c == 0.5)
    return 0.0;
  else if (c == 1.0)
    return std::numeric_limits<num_t>::infinity();
  else if (c > 1.0)
    return std::numeric_limits<num_t>::quiet_NaN();

  num_t x = 1.0;
  num_t fx = cumulative(x);
  num_t xLow, xHi;
  if (fx < c) {
    do {
      x *= 2.0;
      fx = cumulative(x);
    } while (fx < c);
    xLow = x * 0.5;
    xHi = x;
  } else {
    xLow = 0.0;
    xHi = 1.0;
  }
  num_t error = xHi;
  int notConverging = 0;
  do {
    x = (xLow + xHi) * 0.5;
    fx = cumulative(x);
    if (fx > c)
      xHi = x;
    else
      xLow = x;
    num_t currErr = std::fabs(fx - c);
    if (currErr >= error) {
      ++notConverging;
      // not converging anymore; stop.
      if (notConverging > 10) return x;
    } else
      notConverging = 0;
    error = currErr;
  } while (error > err);
  return x;
}

template <typename ValueType>
StochasticEncoder<ValueType>::StochasticEncoder(size_t quantCount,
                                                ValueType stddev,
                                                bool gaussianMapping)
    : _encDictionary(quantCount - 1), _decDictionary(quantCount - 1) {
  // The minimum squared error is reached when each quantity gets an equal share
  // of error The error of a single quantity is \int _-dist ^dist P(x) x^2 dx
  // The integral of x^2 yields a third order term in the solutions.
  // The consequence is that it is optimal to quantize to value as given by
  // uniformly gridding the inverse cumulative normal distribution with a sigma
  // of sqrt(3).
  stddev = stddev * std::sqrt(3);

  _decDictionary.reserve(quantCount);
  typename Dictionary::iterator encItem = _encDictionary.begin();
  typename Dictionary::iterator decItem = _decDictionary.begin();
  if (gaussianMapping) {
    for (size_t i = 0; i != quantCount - 1; ++i) {
      if (i != 0) {
        num_t rightBoundary = ((num_t)i) / (num_t)(quantCount - 1);
        *encItem = stddev * invCumulative(rightBoundary);
        ++encItem;
      }

      num_t val = ((num_t)i + num_t(0.5)) / (num_t)(quantCount - 1);
      *decItem = stddev * invCumulative(val);
      ++decItem;
    }
  } else {
    for (size_t i = 0; i != quantCount - 1; ++i) {
      if (i != 0) {
        num_t rightBoundary = -1.0 + 2.0 * ((num_t)i) / (num_t)(quantCount - 1);
        *encItem = stddev * rightBoundary;
        ++encItem;
      }

      num_t val =
          -1.0 + 2.0 * ((num_t)i + num_t(0.5)) / (num_t)(quantCount - 1);
      *decItem = stddev * val;
      // item.symbol = i;
      ++decItem;
    }
  }

  // Bounding element so that lower_bound always returns a valid symbol
  // Note that if the input is max, it will be encoded as an inf.
  *encItem = std::numeric_limits<ValueType>::max();

  // The last encoding quantity will be used for representing non-finite
  // values. This value is reserved, but outside of the size of the
  // vector -- somewhat dirty but OK since a uvector is used, for which
  // we know it behaves OK in this case. This will make sure that
  // lower_bound() never sees the NaN.
  *decItem = std::numeric_limits<ValueType>::quiet_NaN();
}

template <typename ValueType>
void StochasticEncoder<ValueType>::initializeStudentT(double nu, double rms) {
  size_t quantCount = _encDictionary.size() + 1;
  _decDictionary.reserve(quantCount);
  typename Dictionary::iterator encItem = _encDictionary.begin();
  typename Dictionary::iterator decItem = _decDictionary.begin();
  for (size_t i = 0; i != quantCount - 1; ++i) {
    if (i != 0) {
      num_t rightBoundary = ((num_t)i) / (num_t)(quantCount - 1);
      *encItem = gsl_cdf_tdist_Pinv(rightBoundary, nu) * rms;
      ++encItem;
    }

    num_t val = ((num_t)i + num_t(0.5)) / (num_t)(quantCount - 1);
    *decItem = gsl_cdf_tdist_Pinv(val, nu) * rms;
    ++decItem;
  }
  *encItem = std::numeric_limits<ValueType>::max();
  *decItem = std::numeric_limits<ValueType>::quiet_NaN();
}

template <typename ValueType>
void StochasticEncoder<ValueType>::initializeTruncatedGaussian(
    double truncationValue, double rms) {
  size_t quantCount = _encDictionary.size() + 1;
  _decDictionary.reserve(quantCount);
  typename Dictionary::iterator encItem = _encDictionary.begin();
  typename Dictionary::iterator decItem = _decDictionary.begin();

  double cdfValueAtTrunc = gsl_cdf_gaussian_P(-truncationValue, 1.0);
  double factor = 1.0 - 2.0 * cdfValueAtTrunc;

  for (size_t i = 0; i != quantCount - 1; ++i) {
    if (i != 0) {
      double rightBoundary = ((num_t)i) / (num_t)(quantCount - 1);
      double cdfVal = rightBoundary * factor + cdfValueAtTrunc;
      *encItem = gsl_cdf_gaussian_Pinv(cdfVal, rms);
      ++encItem;
    }

    double val = ((num_t)i + num_t(0.5)) / (num_t)(quantCount - 1);
    double cdfVal = val * factor + cdfValueAtTrunc;
    *decItem = gsl_cdf_gaussian_Pinv(cdfVal, rms);
    ++decItem;
  }
  *encItem = std::numeric_limits<ValueType>::max();
  *decItem = std::numeric_limits<ValueType>::quiet_NaN();
}

template class StochasticEncoder<float>;

}  // namespace dyscostman
