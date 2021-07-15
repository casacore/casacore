#ifndef DYSCO_WEIGHT_ENCODER_H
#define DYSCO_WEIGHT_ENCODER_H

#include <cmath>
#include <vector>

namespace dyscostman {

/**
 * Encoder for visibility weights. It's a linear quantizer for
 * non-negative values, with a single scaling factor. The scaling
 * factor will be such that the max value will still fit.
 * @author André Offringa
 */
template <typename T> class WeightEncoder {
public:
  /** Value type of the original weights (a floating point value).*/
  typedef T value_t;
  /** Value type of the symbols to which the weights are encoded. */
  typedef unsigned symbol_t;

  /** Construct a new encoder with the given quantization count.
   * @param quantCount The number of quantization levels.
   */
  explicit WeightEncoder(unsigned quantCount) : _quantCount(quantCount) {}

  /**
   * Encodes a vector of values. Will return a vector of symbols and a
   * scaling value. Together, these can be used to get the original values
   * back (with some loss due to quantization), by using @ref Decode().
   * @param scaleVal Will receive the scale value for the vector of values
   * @param dest The destinal vector with symbols. It is assumed that it
   * is already of the right size, i.e., equal to input.size().
   * @param input The input array of values to be encoded.
   */
  void Encode(value_t &scaleVal, std::vector<symbol_t> &dest,
              const std::vector<value_t> &input) const {
    // Find max value (implicit assumption input is not empty)
    typename std::vector<value_t>::const_iterator i = input.begin();
    scaleVal = *i;
    ++i;
    while (i != input.end()) {
      if (*i > scaleVal)
        scaleVal = *i;
      ++i;
    }

    i = input.begin();
    typename std::vector<symbol_t>::iterator d = dest.begin();
    const value_t scaleFact = value_t(_quantCount - 1) / scaleVal;
    while (i != input.end()) {
      *d = roundf(scaleFact * (*i));

      ++i;
      ++d;
    }
  }

  /**
   * Decode a previously encoded value.
   * @param dest The destination for the decoded weight values.
   * @param scaleVal The scale value for the vector of values.
   * @param input The input symbols to be decoded.
   * @see Encode()
   */
  void Decode(std::vector<value_t> &dest, value_t scaleVal,
              const std::vector<symbol_t> &input) const {
    typename std::vector<symbol_t>::const_iterator i = input.begin();
    typename std::vector<value_t>::iterator d = dest.begin();
    const value_t scaleFact = scaleVal / value_t(_quantCount - 1);
    while (i != input.end()) {
      *d = (*i) * scaleFact;

      ++i;
      ++d;
    }
  }

private:
  unsigned _quantCount;
};

} // namespace dyscostman

#endif
