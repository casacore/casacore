#ifndef DYSCO_STOCHASTIC_ENCODER_H
#define DYSCO_STOCHASTIC_ENCODER_H

#include "uvector.h"

#include <algorithm>
#include <cmath>
#include <cstring>

namespace dyscostman {

/**
 * Lossy encoder for stochastic values.
 *
 * This encoder can encode a numeric value represented by a floating point
 * number (float, double, ...) into an integer value with a given limit.
 * It can be made to be the least-square error quantization for some
 * distributions.
 *
 * Encoding and decoding have asymetric time complexity / speeds, as decoding
 * is easier than encoding. Decoding is a single indexing into an array, thus
 * extremely fast and with constant time complexity. Encoding is a binary
 * search through the quantizaton values, thus takes O(log quantcount).
 * Typical performance of encoding is 100 MB/s.
 *
 * If the values are encoded into a number of bits which are not divisible by
 * eight, the BytePacker class can be used to pack the values.
 *
 * @author André Offringa (offringa@gmail.com)
 */
template <typename ValueType = float>
class StochasticEncoder {
 public:
  /**
   * Construct encoder for given dictionary size and Gaussian stddev.
   * This constructor initializes the lookup table, and is therefore
   * quite slow.
   * @param quantCount The number of quantization levels, i.e., the dictionary
   * size. Normally this is 2^bitcount. One quantity will be saved for storing
   * non-finite values (NaN and infinites).
   * @param stddev The standard deviation of the data. The closer this value is
   * to the real stddev, the more accurate the encoder will be.
   * @param gaussianMapping Used for testing with non-gaussian distributions.
   */
  StochasticEncoder(size_t quantCount, ValueType stddev,
                    bool gaussianMapping = true);

  static StochasticEncoder StudentTEncoder(size_t quantCount, double nu,
                                           double rms) {
    StochasticEncoder<ValueType> encoder(quantCount);
    encoder.initializeStudentT(nu, rms);
    return encoder;
  }

  static StochasticEncoder TruncatedGausEncoder(size_t quantCount, double trunc,
                                                double rms) {
    StochasticEncoder<ValueType> encoder(quantCount);
    encoder.initializeTruncatedGaussian(trunc, rms);
    return encoder;
  }

  /**
   * Unsigned integer type used for representing the encoded symbols.
   */
  typedef unsigned symbol_t;

  /**
   * Template type used for representing floating point values that
   * are to be encoded.
   */
  typedef ValueType value_t;

  /**
   * Get the quantized symbol for the given floating point value.
   * This method is implemented with a binary search, so takes
   * O(log N), with N the dictionary size (2^bitcount).
   * Use Decode() on the returned symbol to get
   * the decoded value.
   * @param value Floating point value to be encoded.
   */
  symbol_t Encode(ValueType value) const {
    if (std::isfinite(value))
      return _encDictionary.symbol(_encDictionary.lower_bound(value));
    else
      return QuantizationCount() - 1;
  }

  static std::uniform_int_distribution<unsigned> GetDitherDistribution() {
    return std::uniform_int_distribution<unsigned>(0, ((1u << 31) - 1));
  }

  /**
   * Get the quantized symbol for the given floating point value.
   * Dithering is applied, which will cause the average error to
   * converge to zero, assuming the error is uniformly distributed.
   * This method is implemented with a binary search, so takes
   * O(log N), with N the dictionary size (2^bitcount).
   * Use Decode() on the returned symbol to get
   * the decoded value.
   * @param value Floating point value to be encoded.
   * @param ditherValue The dithering value to apply.
   */
  symbol_t EncodeWithDithering(ValueType value, unsigned ditherValue) const {
    if (std::isfinite(value)) {
      const typename Dictionary::const_iterator lowerBound =
          _decDictionary.lower_bound(value);
      if (lowerBound == _decDictionary.begin())
        return _decDictionary.symbol(lowerBound);
      if (lowerBound == _decDictionary.end())
        return _decDictionary.symbol(lowerBound - 1);
      const ValueType rightValue = _decDictionary.value(lowerBound);
      const ValueType leftValue = _decDictionary.value(lowerBound - 1);

      ValueType ditherMark =
          ValueType(1u << 31) * (value - leftValue) / (rightValue - leftValue);
      if (ditherMark > ditherValue)
        return _decDictionary.symbol(lowerBound);
      else
        return _decDictionary.symbol(lowerBound - 1);
    } else {
      return _encDictionary.size();
    }
  }

  /**
   * Will return the right boundary of the given symbol.
   * The right boundary is the smallest value that would not be
   * quantized to the given symbol anymore. If no such boundary
   * exists, 0.0 is returned.
   */
  value_t RightBoundary(symbol_t symbol) const {
    if (symbol != _encDictionary.size())
      return _encDictionary.value(symbol);
    else
      return 0.0;
  }

  /**
   * Get the centroid value that belongs to the given symbol.
   * @param symbol Symbol to be decoded
   * @returns The best estimate of the original value.
   */
  ValueType Decode(symbol_t symbol) const {
    return _decDictionary.value(symbol);
  }

  size_t QuantizationCount() const { return _decDictionary.size() + 1; }

  ValueType MaxQuantity() const { return _decDictionary.largest_value(); }

  ValueType MinQuantity() const { return _decDictionary.smallest_value(); }

 private:
  explicit StochasticEncoder(size_t quantCount)
      : _encDictionary(quantCount - 1), _decDictionary(quantCount - 1) {}

  void initializeStudentT(double nu, double rms);

  void initializeTruncatedGaussian(double truncationValue, double rms);

  class Dictionary {
   public:
    typedef value_t *iterator;
    typedef const value_t *const_iterator;

    Dictionary() : _values() {}

    explicit Dictionary(size_t size) : _values(size) {}

    void reserve(size_t size) { _values.reserve(size); }

    void resize(size_t size) { _values.resize(size); }

    /**
     * Returns an iterator pointing to the first element in the dictionary
     * that is not less than (i.e. greater or equal to) value.
     *
     * This implementation is like @ref lower_bound_fast(), but additionally
     * assumes the dictionary has at least two elements, avoiding one
     * comparison.
     */
    const_iterator lower_bound(value_t val) const {
      size_t p = 0, q = _values.size();
      size_t m = (p + q) / 2;
      if (_values[m] <= val)
        p = m;
      else
        q = m;
      while (p + 1 != q) {
        size_t m = (p + q) / 2;
        if (_values[m] <= val)
          p = m;
        else
          q = m;
      }
      return (_values[p] < val) ? (&_values[q]) : (&_values[p]);
    }

    /**
     * Returns an iterator pointing to the first element in the dictionary
     * that is not less than (i.e. greater or equal to) value.
     *
     * This implementation turns out to be slightly faster than the
     * STL implementation. It performs 10.7 MB/s, vs. 9.0 MB/s for the
     * STL. 18% faster. Using "unsigned" instead of "size_t" is 5% slower.
     * (It's not a fair STL comparison, because this implementation
     * does not check for empty vector).
     */
    const_iterator lower_bound_fast(value_t val) const {
      size_t p = 0, q = _values.size();
      while (p + 1 != q) {
        size_t m = (p + q) / 2;
        if (_values[m] <= val)
          p = m;
        else
          q = m;
      }
      return (_values[p] < val) ? (&_values[q]) : (&_values[p]);
    }

    /**
     * Below is the first failed result of an attempt to beat the STL in
     * performance. It turns out to be 13% slower for larger dictionaries,
     * compared to the STL implementation that is used in the class
     * above. It performs 7.9 MB/s. 26% compared to the 'fastest' lower_bound.
     */
    const_iterator lower_bound_slow(value_t val) const {
      const value_t *p = &*_values.begin(), *q = p + _values.size();
      while (p + 1 != q) {
        // This is a bit inefficient, but (p + q)/2 was not allowed, because
        // operator+(ptr,ptr) is not allowed.
        const value_t *m = p + (q - p) / 2;
        if (*m <= val)
          p = m;
        else
          q = m;
      }
      return p;
    }

    iterator begin() { return &*_values.begin(); }
    const_iterator begin() const { return &*_values.begin(); }
    const_iterator end() const { return &*_values.end(); }
    symbol_t symbol(const_iterator iter) const { return (iter - begin()); }
    symbol_t largest_symbol() const { return _values.size() - 1; }
    value_t value(const_iterator iter) const { return *iter; }
    value_t value(symbol_t sym) const { return _values[sym]; }
    value_t largest_value() const { return _values.back(); }
    value_t smallest_value() const { return _values.front(); }
    size_t size() const { return _values.size(); }
    size_t capacity(size_t) const { return _values.capacity(); }

   private:
    ao::uvector<value_t> _values;
  };

  typedef long double num_t;

  static num_t cumulative(num_t x);
  static num_t invCumulative(num_t c, num_t err = num_t(1e-13));

  Dictionary _encDictionary;
  Dictionary _decDictionary;
};

}  // namespace dyscostman

#endif
