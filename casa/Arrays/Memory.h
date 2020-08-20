#ifndef CASACORE_COPY_2_H
#define CASACORE_COPY_2_H

#include <cstring>

namespace casacore { //#Begin casa namespace

template<typename T>
void copy_n_with_stride(const T* from, std::size_t n, T* to,
    std::size_t toStride, std::size_t fromStride)
{
  while (n--)
  {
    *to = *from;
    to += toStride;
    from += fromStride;
  }
}

template<typename T>
void move_n_with_stride(T* from, std::size_t n, T* to,
    std::size_t toStride, std::size_t fromStride)
{
  while (n--)
  {
    *to = std::move(*from);
    to += toStride;
    from += fromStride;
  }
}

template<typename T>
void fill_n_with_stride(T* dest, size_t n, const T& value, size_t stride)
{
  while (n--)
  {
    *dest = value;
    dest += stride;
  };
}

} // namespaces

#endif
