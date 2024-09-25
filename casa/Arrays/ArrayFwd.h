#ifndef ARRAY2_ARRAY_FWD_H
#define ARRAY2_ARRAY_FWD_H

#include <memory>

namespace casacore { //#Begin casa namespace

template<typename T> class Array;
template<typename T> class Vector;
template<typename T> class Matrix;
template<typename T> class Cube;
typedef bool LogicalArrayElem;
typedef Array<LogicalArrayElem> LogicalArray;
template<typename T> class MaskedArray;
typedef MaskedArray<LogicalArrayElem> MaskedLogicalArray;
class Slice;
class Slicer;
template<typename T> class ArrayIterator;

}

#endif
