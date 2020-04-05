#ifndef CASACORE_ARRAYSTR_H
#define CASACORE_ARRAYSTR_H

#include "Array.h"

#include <istream>
#include <ostream>

namespace casacore {
  
// Write out an ascii representation of an array of any dimensionality.
// Arrays of dimensionality 3 or greater are written out vector by vector,
// preceeded by the position of the start of the vector. If the origin of
// the array isn't zero it is printed. The shape of the array is always
// printed.
template<typename T, typename Alloc>
std::ostream &operator << (std::ostream &, const Array<T, Alloc> &);

// Read an ascii representation of an array. All types with an <src><<</src>
// operator can be handled. The basic format of the input should be:
// <srcblock>
//	[element element element ....] 
// </srcblock>
// Elements are separated by whitespace, or a comma, optionally surrounded
// by white space. <br>
// <note role=warning> Some input routines read fields between blank spaces. This
// is (at the moment) especially true for Quantities and Strings.
// In those cases
// the separator should be blank (or a comma following a blank), and the
// end ']' should have a blank in front.
// A crude fix for String arrays having separators <src>,</src> and <src>]</src>
// without blanks preceding has been made; but slows routines down </note>
// The default input is a vector of unspecified length. The input shape
// can be changed by pre-pending the input with:
// <srcblock>
//	{[shape]}
// </srcblock>
// where shape is an unsigned integer vector. The shape will be used to check
// the input length; and, depending on the possibility, to resize/reshape the
// result. However, reshaping of e.g. a Vector to a Matrix cannot be done, and
// the result will stay in the form asked.<br>
// Input order is row major, however by preceding the input with:
// <srcblock>
//	{T[shape]}	
// </srcblock>
// the order will be reversed.<br>
// Reshaping of the Array provided will depend on the type of Array and its
// state. If a general Array, the shape will be
// as defined by user. If fixed Array (e.g. Matrix, Vector, Cube) the number
// of dimesnsions will be kept. If the user specified more dimensions
// then supported (e.g. 3 for Matrix), the last dimesions will be collapsed.
// If less dimensions are specified, the missing ones will be set to 1.
// will be kept.<br>
// The read() version can be used to force a shape (ip), or an input
// transpose (it) (which can be undone by the user specifying transpose).
//
// <group>
template<typename T, typename Alloc>
std::istream &operator>> (std::istream &s, Array<T, Alloc> &x);

template<typename T, typename Alloc>
bool read(std::istream &s, Array<T, Alloc> &x,
			    const IPosition *ip=0, bool it=false);
// </group>

// General read support function for matrices.
// In principle these functions will not be
// used by general user, but could be. They can be used by Array type
// classes (like Slice, Lattice) to do the work of comparable input
// functions as the one for Arrays.
// In these functions p is the shape
// of the returned Block x. This shape is either deduced from the user
// specification; made equal to (1, nelements) if no user shape is
// given; is set to ip if specified. The function will return false (and
// p = (0)) in the case of an invalid input element; a number of elements
// input not equal to ip (if specified); the shape given by user as input
// does not conform to ip (if given) or the number of elements input.<br>
// trans will be true if transpose asked by user; or if forced by it.
template<typename T, typename Alloc> bool readArrayBlock(std::istream &s, bool &trans,
  IPosition &p,
  std::vector<T, Alloc> &x,
  const IPosition *ip=0, bool it=false);

// <summary>
// Global functions for Matrix/Vector input/output using ASCII format.
// </summary>

// <use visibility=export>

// <prerequisite>
//   <li> <linkto class=Matrix>Matrix</linkto>
//   <li> <linkto class=Vector>Vector</linkto>
// </prerequisite>

// <synopsis>
// These global functions support file I/O between ASCII files and
// Matrices or Vectors.
// </synopsis>

// <example>
// <srcblock>
//    Matrix<float> picture(256, 256); picture = 0.0;
//    String fileName="picture.data";
//
//    // operations to populate picture
//    // ...
//
//    writeAsciiMatrix (picture, fileName);
// </srcblock>
// </example>

// <linkfrom anchor="Array Ascii IO" classes="Vector Matrix">
//    <here>Array Ascii IO</here> -- Simple Ascii input/output for Arrays.
// </linkfrom>

// <group name=Array Ascii IO>

// These routines read and write a Matrix of data.  The first line of
// input will be examined to determine the number of columns in the matrix.
// The maximum number of columns provided for is 100.  Each item may be up 
// to 50 characters long.
//
// Each item must be separated from others by one (or more) blank column.
// The "line" may be up to 1024 characters long.  Each subsequent line must
// contain the SAME number of items as the first line but may be any length
// (up to 1024 characters).
//
// The matrix need NOT be square.  
//
// The matrix should be declared but NOT dimensioned in the calling program.

// <group>
template <typename T, typename Alloc>
void readAsciiMatrix (Matrix<T, Alloc>& mat, const char* fileName);

template <typename T, typename Alloc>
void writeAsciiMatrix (const Matrix<T, Alloc>& mat, const char* fileName);
// </group>

template<typename T, typename Alloc>
std::string to_string(const Array<T, Alloc> array);

}

#include "ArrayStr.tcc"

#endif
