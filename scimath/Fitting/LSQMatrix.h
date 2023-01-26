//# LSQMatrix.h: Support class for the LSQ package
//# Copyright (C) 2004,2005,2006
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef SCIMATH_LSQMATRIX_H
#define SCIMATH_LSQMATRIX_H

//# Includes
#include <casacore/casa/aips.h>
#include <algorithm>
#include <casacore/casa/Utilities/RecordTransformable.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class AipsIO;

// <summary> Support class for the LSQ package </summary>
// <reviewed reviewer="Wim Brouw" date="2004/03/20" tests="tLSQFit"
//	 demos="">
// </reviewed>

// <prerequisite>
//   <li> Some knowledge of Matrix operations
// </prerequisite>
//
// <etymology>
// From Least SQuares and Matrix
// </etymology>
//
// <synopsis>
// The LSQMatrix class contains the handling of the basic container used
// in the <linkto class="LSQFit">LSQFit</linkto> class and its derivatives.
// This basic container is a triangular matrix.
//
// The basic operations provided are referencing and indexing of cells,
// rows, columns and diagonal of the triangular matrix.
// The class is a private structure, with explicit friends.
//
// The class contains a number of public methods (with _pub in name) that
// can be used anywhere, and which perform index range checking.
//
// The contents can be saved in a record (<src>toRecord</src>), 
// and an object can be created from a record (<src>fromRecord</src>).
// The record identifier is 'tmat'.
// </synopsis>
//
// <example>
// See the <linkto class="LSQFit">LSQFit</linkto> class for its use.
// </example>
//
// <motivation>
// The class was written to isolate the handling of the normal equations
// used in the <src>LSQ</src> classes.
// </motivation>
//
// <todo asof="2004/03/20">
//	<li> Look in possibility of an STL iterator along row, column and
//         diagonal
// </todo>

class LSQMatrix : public RecordTransformable {
  //# Friends
  friend class LSQFit;

 public:
  // A set of public interface functions. Checks for index ranges are made, 
  // and zero or null returned if error.
  // <group>
  // Get row pointer in normal equation (points to element <src>[i][0]</src>)
  double *row_pub(uint32_t i) const { return (i<n_p) ? row(i) : 0; };
  // Get next row or previous row pointer in normal equation if the pointer
  // <src>row</src> is at row <src>i</src>.
  // <group>
  void incRow_pub(double *&row, uint32_t i) const { if (i<n_p-1) incRow(row,i); };
  void decRow_pub(double *&row, uint32_t i) const { if (i>0) decRow(row,i); };
  // </group>
  // Get diagonal element pointer <src>[i][i]</src>
  double *diag_pub(uint32_t i) const { return ((i<n_p) ? diag(i) : 0); };
  // Get length of triangular array
  uint32_t nelements_pub() const { return (len_p); };
  // Get number of rows
  uint32_t nrows_pub() const { return n_p; };
  // Make diagonal element 1 if zero (Note that this is always called when
  // <src>invert()</src> is called). Only n-length sub-matrix is done.
  void doDiagonal_pub(uint32_t n) { if (n<n_p) doDiagonal(n); }
  // Multiply n-length of diagonal with <src>1+fac</src>
  void mulDiagonal_pub(uint32_t n, double fac) { if (n<n_p) mulDiagonal(n,fac); };
  // Add <src>fac</src> to n-length of diagonal
  void addDiagonal_pub(uint32_t n, double fac) { if (n<n_p) addDiagonal(n,fac); };
  // Determine max of abs values of n-length of diagonal
  double maxDiagonal_pub(uint32_t n){ return ((n<n_p) ? maxDiagonal(n) : 0); };
  // </group>

 private:
  //# Constructors
  // Default constructor (empty, only usable after a <src>set(n)</src>)
  LSQMatrix();
  // Construct an object with the number of rows and columns indicated.
  // If a <src>bool</src> argument is present, the number
  // will be taken as double the number given (assumes complex). 
  // <group>
  explicit LSQMatrix(uint32_t n);
  LSQMatrix(uint32_t n, bool);
  // </group>
  // Copy constructor (deep copy)
  LSQMatrix(const LSQMatrix &other);
  // Assignment (deep copy)
  LSQMatrix &operator=(const LSQMatrix &other);

  //# Destructor
  ~LSQMatrix();

  //# Operators
  // Index an element in the triangularised matrix
  // <group>
  double &operator[](uint32_t index) { return (trian_p[index]); };
  double operator[](uint32_t index) const { return (trian_p[index]); };
  // </group>

  //# General Member Functions
  // Reset all data to zero
  void reset() { clear(); };
  // Set new sizes (default is for Real, a bool argument will make it complex)
  // <group>
  void set(uint32_t n);
  void set(uint32_t n, bool);
  // </group>
  // Get row pointer in normal equation (points to element <src>[i][0]</src>)
  double *row(uint32_t i) const { return &trian_p[((n2m1_p-i)*i)/2]; };
  // Get next row or previous row pointer in normal equation if the pointer
  // <src>row</src> is at row <src>i</src>.
  // <group>
  void incRow(double *&row, uint32_t i) const { row += nm1_p-i; };
  void decRow(double *&row, uint32_t i) const { row -= n_p-i; };
  // </group>
  // Get diagonal element pointer <src>[i][i]</src>
  double *diag(uint32_t i) const { return &trian_p[((n2p1_p-i)*i)/2]; };
  // Get length of triangular array
  uint32_t nelements() const { return (len_p); };
  // Get number of rows
  uint32_t nrows() const { return n_p; };
  // Copy data.
  void copy(const LSQMatrix &other);
  // Initialise matrix
  void init();
  // Clear matrix
  void clear();
  // De-initialise matrix
  void deinit();
  // Make diagonal element 1 if zero (Note that this is always called when
  // <src>invert()</src> is called). Only n-length sub-matrix is done.
  void doDiagonal(uint32_t n);
  // Multiply n-length of diagonal with <src>1+fac</src>
  void mulDiagonal(uint32_t n, double fac);
  // Add <src>fac</src> to n-length of diagonal
  void addDiagonal(uint32_t n, double fac);
  // Determine max of abs values of n-length of diagonal
  double maxDiagonal(uint32_t n);
  // Create a Matrix from a record. An error message is generated, and false
  // returned if an invalid record is given. A valid record will return true.
  // Error messages are postfixed to error.
  // <group>
  bool fromRecord(String &error, const RecordInterface &in);
  // </group>
  // Create a record from an LSQMatrix. The return will be false and an error
  // message generated only if the object does not contain a valid Matrix.
  // Error messages are postfixed to error.
  bool toRecord(String &error, RecordInterface &out) const;
  // Get identification of record
  const String &ident() const;
  // Convert a <src>carray</src> to/from a record. Field only written if 
  // non-zero length. No carray created if field does not exist on input.
  // false returned if unexpectedly no data available for non-zero length
  // (put), or a field has zero length vector(get).
  // <group>
  static bool putCArray(String &error, RecordInterface &out,
			const String &fname,
			uint32_t len, const double * const in);
  static bool getCArray(String &error, const RecordInterface &in,
			const String &fname,
			uint32_t len, double *&out);
  static bool putCArray(String &error, RecordInterface &out,
			const String &fname,
			uint32_t len, const uint32_t * const in);
  static bool getCArray(String &error, const RecordInterface &in,
			const String &fname,
			uint32_t len, uint32_t *&out);
  // </group>

  // Save or restore using AipsIO.
  void fromAipsIO (AipsIO& in);
  void toAipsIO (AipsIO& out) const;
  static void putCArray (AipsIO& out, uint32_t len, const double* const in);
  static void getCArray (AipsIO& in, uint32_t len, double*& out);
  static void putCArray (AipsIO& out, uint32_t len, const uint32_t* const in);
  static void getCArray (AipsIO& in, uint32_t len, uint32_t*& out);

  //# Data
  // Matrix size (linear size)
  uint32_t n_p;
  // Derived sizes (all 0 if n_p equals 0)
  // <group>
  // Total size
  uint32_t len_p;
  // <src>n-1</src>
  uint32_t nm1_p;
  // <src>2n-1</src>
  int32_t n2m1_p;
  // <src>2n+1</src>
  int32_t n2p1_p;
  // </group>
  // Matrix (triangular n_p * n_p)
  double *trian_p;
  // Record field names
  static const String tmatsiz;
  static const String tmatdat;
  // <group>
  // </group>
  //
};


} //# NAMESPACE CASACORE - END

#endif
