#ifndef ARRAY2_ARRAY_FWD_H
#define ARRAY2_ARRAY_FWD_H

#include <memory>

namespace casacore { //#Begin casa namespace

template<typename T, typename Alloc = std::allocator<T>> class Array;
template<typename T, typename Alloc = std::allocator<T>> class Vector;
template<typename T, typename Alloc = std::allocator<T>> class Matrix;
template<typename T, typename Alloc = std::allocator<T>> class Cube;
typedef bool LogicalArrayElem;
typedef Array<LogicalArrayElem> LogicalArray;
template<typename T, typename ArrayAlloc, typename MaskAlloc> class MaskedArray;
typedef MaskedArray<LogicalArrayElem, std::allocator<LogicalArrayElem>, std::allocator<LogicalArrayElem>> MaskedLogicalArray;
class Slice;
class Slicer;
template<typename T, typename Alloc = std::allocator<T>> class ArrayIterator;

}

#endif
