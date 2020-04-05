#ifndef ELEMENT_FUNCTIONS_H
#define ELEMENT_FUNCTIONS_H

#include <cmath>
#include <complex>
#include <limits>

namespace casacore {
namespace arrays_internal {

inline bool near(unsigned val1, unsigned val2, double tol) {
  if (tol <= 0) {
    return (val1 == val2);
  }
  if (val1 == val2) {
    return true;
  } else if (val1 > val2) {
    return (double(val1-val2) <= tol*std::max(val1,val2));
  } else {
    return (double(val2-val1) <= tol*std::max(val1,val2));
  }
}

inline bool near(int val1, int val2, double tol) {
  if (tol <=0) {
    return (val1 == val2);
  }
  if (val1 == val2) {
    return true;
  }
  if ((0<val1) != (0<val2)) {
    return false;
  }
  const int aval1 = std::abs(val1);
  const int aval2 = std::abs(val2);
  return (double(aval1-aval2) <= tol*double(std::max(aval1,aval2)));
}

inline bool near(float val1, float val2, double tol) {
  if (tol <=0) {
    return (val1 == val2);
  }
  if (val1 == val2) {
    return true;
  }
  if (val1 == 0) {
    return (std::fabs(val2) <= (1+tol)*std::numeric_limits<float>::min());
  }
  else if (val2 == 0) {
    return (std::fabs(val1) <= (1+tol)*std::numeric_limits<float>::min());
  }
  if ((0<val1) != (0<val2)) {
    return false;
  }
  return (std::fabs(val1-val2) <= tol*std::max(std::fabs(val1), std::fabs(val2)));
}

inline bool near(double val1, double val2, double tol) {
  if (tol <=0) {
    return (val1 == val2);
  }
  if (val1 == val2) {
    return true;
  }
  if (val1 == 0) {
    return (std::fabs(val2) <= (1+tol)*std::numeric_limits<double>::min());
  }
  else if (val2 == 0) {
    return (std::fabs(val1) <= (1+tol)*std::numeric_limits<double>::min());
  }
  if ((0<val1) != (0<val2)) {
    return false;
  }
  return (std::fabs(val1-val2) <= tol*std::max(std::fabs(val1),std::fabs(val2)));
}

inline bool near(float val1, double val2, double tol) {
   return near(double(val1), val2, tol);
}

inline bool near(double val1, float val2, double tol) {
   return near(val1, double(val2), tol);
}

inline bool near(const std::complex<float> &val1, const std::complex<float> &val2, double tol=1.0e-5)
{
  if (tol <= 0) return val1 == val2;
  if (val1 == val2) return true;
  if (near(val1.real(), val2.real(), tol) &&
      near(val1.imag(), val2.imag(), tol)) return true;
  float aval1(std::abs(val1)), aval2(std::abs(val2));
  if (aval1 == 0) return aval2 <= (1+tol)*std::numeric_limits<float>::min();
  else if (aval2 == 0) return aval1 <= (1+tol)*std::numeric_limits<float>::min();
  std::complex<double> dval(val1);
  dval -= std::complex<double>(val2);
  return std::abs(dval) <= tol * (aval1 < aval2 ? aval2 : aval1);
}

inline bool near(const std::complex<double> &val1, const std::complex<double> &val2, double tol=1.0e-13)
{
  if (tol <= 0) return val1 == val2;
  if (val1 == val2) return true;
  if (std::abs(val1) == 0) return std::abs(val2) <= (1+tol)*std::numeric_limits<double>::min();
  else if (std::abs(val2) == 0) return std::abs(val1) <= (1+tol)*std::numeric_limits<double>::min();
  double aval1(std::abs(val1)), aval2(std::abs(val2));
  return std::abs(val1-val2) <= tol * (aval1 < aval2 ? aval2 : aval1);
}

inline bool nearAbs(const std::complex<float> &val1, const std::complex<float> &val2, double tol=1.0e-5)
{
  return std::abs(val2 - val1) <= tol;
}

inline bool nearAbs(const std::complex<double> &val1, const std::complex<double> &val2, double tol=1.0e-13)
{
  return std::abs(val2 - val1) <= tol;
}

inline bool nearAbs(unsigned val1, unsigned val2, double tol) {
  if (val1 == val2) {
    return true;
  } else if (val1 > val2) {
    return (tol >= double(val1 - val2));
  } else {
    return (tol >= double(val2 - val1));
  }
}

inline bool nearAbs(int val1, int val2, double tol) {
  return (tol >= double(std::abs(val2 - val1)));
}

inline bool nearAbs(float val1, float val2, double tol) {
  return (tol >= double(std::fabs(val2 - val1)));
}

inline bool nearAbs(double val1, double val2, double tol) {
  return (tol >= std::fabs(val2 - val1));
}

// Define a function to compare all elements of two sequences.
// It returns true if all elements compare true.
// An example compare operator is <src>std::equal_to</src>.
// <group>
template<typename InputIterator1, typename InputIterator2, typename CompareOperator>
inline bool compareAll (InputIterator1 first1, InputIterator1 last1,
                        InputIterator2 first2, CompareOperator op)
{
  for (; first1!=last1; ++first1, ++first2) {
    if (!op(*first1, *first2)) return false;
  }
  return true;
}
// For use with a constant left value.
// This avoids use of bind1st or bind2nd which can fail for gcc-4.3.
// (see ArrayMath.h).
template<typename InputIterator1, typename T, typename CompareOperator>
inline bool compareAllLeft (InputIterator1 first1, InputIterator1 last1,
                            T left, CompareOperator op)
{
  for (; first1!=last1; ++first1) {
    if (!op(left, *first1)) return false;
  }
  return true;
}
// For use with a constant right value.
// This avoids use of bind1st or bind2nd which can fail for gcc-4.3.
// (see ArrayMath.h).
template<typename InputIterator1, typename T, typename CompareOperator>
inline bool compareAllRight (InputIterator1 first1, InputIterator1 last1,
                              T right, CompareOperator op)
{
  for (; first1!=last1; ++first1) {
    if (!op(*first1, right)) return false;
  }
  return true;
}
// </group>

// Define a function to compare all elements of two sequences.
// It returns true if any element compares true.
// An example compare operator is <src>std::equal_to</src>.
// <group>
template<typename InputIterator1, typename InputIterator2, typename CompareOperator>
inline bool compareAny (InputIterator1 first1, InputIterator1 last1,
                        InputIterator2 first2, CompareOperator op)
{
  for (; first1!=last1; ++first1, ++first2) {
    if (op(*first1, *first2)) return true;
  }
  return false;
}
// For use with a constant left value.
// This avoids use of bind1st or bind2nd which can fail for gcc-4.3.
// (see ArrayMath.h).
template<typename InputIterator1, typename T, typename CompareOperator>
inline bool compareAnyLeft (InputIterator1 first1, InputIterator1 last1,
                            T left, CompareOperator op)
{
  for (; first1!=last1; ++first1) {
    if (op(left, *first1)) return true;
  }
  return false;
}
// For use with a constant right value.
// This avoids use of bind1st or bind2nd which can fail for gcc-4.3.
// (see ArrayMath.h).
template<typename InputIterator1, typename T, typename CompareOperator>
inline bool compareAnyRight (InputIterator1 first1, InputIterator1 last1,
                              T right, CompareOperator op)
{
  for (; first1!=last1; ++first1) {
    if (op(*first1, right)) return true;
  }
  return false;
}
// </group>

// Functor to add squared diff of right and base value to left.
// It can be used to calculate the variance.
// Note: it is specialized for complex values to handle real and imag separately.
template<typename T, typename Accum=T>
struct SumSqrDiff
{
  explicit SumSqrDiff(T base) : itsBase(base) {}
  Accum operator() (Accum left, T right) const
    { return left + (right-itsBase)*(right-itsBase); }
private:
  Accum itsBase;    // store as Accum, so subtraction results in Accum
};
  
// Specialize for complex values.
// Variance has to be taken for the absolute value of a complex value. thus
//       sum(abs((a[i] - mean)**2
// where the sqrt used in abs and the **2 cancel each other, thus can be left out.
// See also https://en.wikipedia.org/wiki/Complex_random_variable#Variance
// Note that although the sum is real, a complex value is used to have equal template types.
template<typename T>
struct SumSqrDiff<std::complex<T>>
{
  explicit SumSqrDiff(std::complex<T> base) : itsBase(base) {}
  std::complex<T> operator() (std::complex<T> left, std::complex<T> right) const
  { return left + ((right.real() - itsBase.real()) * (right.real() - itsBase.real()) +
                    (right.imag() - itsBase.imag()) * (right.imag() - itsBase.imag())); }
private:
  std::complex<T> itsBase;
};

template<typename T>
bool isnan(const std::complex<T> &val)
{
  return std::isnan(val.real()) || std::isnan(val.imag());
}

template<typename T>
bool isinf(const std::complex<T> &val)
{
  return std::isinf(val.real()) || std::isinf(val.imag());
}

template<typename T>
bool isfinite(const std::complex<T> &val)
{
  return std::isfinite(val.real()) || std::isfinite(val.imag());
}

inline int floormod (int x, int y)
{
  int r = x%y;
  if (r != 0   &&  (x<0) != (y<0)) r+=y;
  return r;
}
inline long long floormod (long long x, long long y)
{
  long long r = x%y;
  if (r != 0  &&  (x<0) != (y<0)) r+=y;
  return r;
}
inline float floormod (float x, float y)
{
  float r = std::fmod(x,y);
  if (r != 0  &&  (x<0) != (y<0)) r+=y;
  return r;
}
inline double floormod (double x, double y)
{
  double r = std::fmod(x,y);
  if (r != 0  &&  (x<0) != (y<0)) r+=y;
  return r;
}

template<class T, class F> inline void convertScalar (T& out, F in)
{ out = static_cast<T>(in); }

inline void convertScalar (std::complex<float>& out, std::complex<double> in)
{ out = std::complex<float>(in.real(), in.imag()); }

} }

#endif
