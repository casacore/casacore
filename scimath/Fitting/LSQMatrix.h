// # LSQMatrix.h: Support class for the LSQ package
// # Copyright (C) 2004,2005,2006
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This library is free software; you can redistribute it and/or modify it
// # under the terms of the GNU Library General Public License as published by
// # the Free Software Foundation; either version 2 of the License, or (at your
// # option) any later version.
// #
// # This library is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
// # License for more details.
// #
// # You should have received a copy of the GNU Library General Public License
// # along with this library; if not, write to the Free Software Foundation,
// # Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

#ifndef SCIMATH_LSQMATRIX_H
#define SCIMATH_LSQMATRIX_H

// # Includes
#include <casacore/casa/aips.h>
#include <algorithm>
#include <casacore/casa/Utilities/RecordTransformable.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

// # Forward Declarations
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
  // # Friends
  friend class LSQFit;

 public:
  // A set of public interface functions. Checks for index ranges are made,
  // and zero or null returned if error.
  // <group>
  // Get row pointer in normal equation (points to element <src>[i][0]</src>)
  double *row_pub(unsigned int i) const { return (i < n_p) ? row(i) : 0; };
  // Get next row or previous row pointer in normal equation if the pointer
  // <src>row</src> is at row <src>i</src>.
  // <group>
  void incRow_pub(double *&row, unsigned int i) const {
    if (i < n_p - 1) incRow(row, i);
  };
  void decRow_pub(double *&row, unsigned int i) const {
    if (i > 0) decRow(row, i);
  };
  // </group>
  // Get diagonal element pointer <src>[i][i]</src>
  double *diag_pub(unsigned int i) const { return ((i < n_p) ? diag(i) : 0); };
  // Get length of triangular array
  unsigned int nelements_pub() const { return (len_p); };
  // Get number of rows
  unsigned int nrows_pub() const { return n_p; };
  // Make diagonal element 1 if zero (Note that this is always called when
  // <src>invert()</src> is called). Only n-length sub-matrix is done.
  void doDiagonal_pub(unsigned int n) {
    if (n < n_p) doDiagonal(n);
  }
  // Multiply n-length of diagonal with <src>1+fac</src>
  void mulDiagonal_pub(unsigned int n, double fac) {
    if (n < n_p) mulDiagonal(n, fac);
  };
  // Add <src>fac</src> to n-length of diagonal
  void addDiagonal_pub(unsigned int n, double fac) {
    if (n < n_p) addDiagonal(n, fac);
  };
  // Determine max of abs values of n-length of diagonal
  double maxDiagonal_pub(unsigned int n) { return ((n < n_p) ? maxDiagonal(n) : 0); };
  // </group>

 private:
  // # Constructors
  //  Default constructor (empty, only usable after a <src>set(n)</src>)
  LSQMatrix();
  // Construct an object with the number of rows and columns indicated.
  // If a <src>Bool</src> argument is present, the number
  // will be taken as double the number given (assumes complex).
  // <group>
  explicit LSQMatrix(unsigned int n);
  LSQMatrix(unsigned int n, bool);
  // </group>
  // Copy constructor (deep copy)
  LSQMatrix(const LSQMatrix &other);
  // Assignment (deep copy)
  LSQMatrix &operator=(const LSQMatrix &other);

  // # Destructor
  ~LSQMatrix();

  // # Operators
  //  Index an element in the triangularised matrix
  //  <group>
  double &operator[](unsigned int index) { return (trian_p[index]); };
  double operator[](unsigned int index) const { return (trian_p[index]); };
  // </group>

  // # General Member Functions
  //  Reset all data to zero
  void reset() { clear(); };
  // Set new sizes (default is for Real, a Bool argument will make it complex)
  // <group>
  void set(unsigned int n);
  void set(unsigned int n, bool);
  // </group>
  // Get row pointer in normal equation (points to element <src>[i][0]</src>)
  double *row(unsigned int i) const { return &trian_p[((n2m1_p - i) * i) / 2]; };
  // Get next row or previous row pointer in normal equation if the pointer
  // <src>row</src> is at row <src>i</src>.
  // <group>
  void incRow(double *&row, unsigned int i) const { row += nm1_p - i; };
  void decRow(double *&row, unsigned int i) const { row -= n_p - i; };
  // </group>
  // Get diagonal element pointer <src>[i][i]</src>
  double *diag(unsigned int i) const { return &trian_p[((n2p1_p - i) * i) / 2]; };
  // Get length of triangular array
  unsigned int nelements() const { return (len_p); };
  // Get number of rows
  unsigned int nrows() const { return n_p; };
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
  void doDiagonal(unsigned int n);
  // Multiply n-length of diagonal with <src>1+fac</src>
  void mulDiagonal(unsigned int n, double fac);
  // Add <src>fac</src> to n-length of diagonal
  void addDiagonal(unsigned int n, double fac);
  // Determine max of abs values of n-length of diagonal
  double maxDiagonal(unsigned int n);
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
  static bool putCArray(String &error, RecordInterface &out, const String &fname, unsigned int len,
                        const double *const in);
  static bool getCArray(String &error, const RecordInterface &in, const String &fname,
                        unsigned int len, double *&out);
  static bool putCArray(String &error, RecordInterface &out, const String &fname, unsigned int len,
                        const unsigned int *const in);
  static bool getCArray(String &error, const RecordInterface &in, const String &fname,
                        unsigned int len, unsigned int *&out);
  // </group>

  // Save or restore using AipsIO.
  void fromAipsIO(AipsIO &in);
  void toAipsIO(AipsIO &out) const;
  static void putCArray(AipsIO &out, unsigned int len, const double *const in);
  static void getCArray(AipsIO &in, unsigned int len, double *&out);
  static void putCArray(AipsIO &out, unsigned int len, const unsigned int *const in);
  static void getCArray(AipsIO &in, unsigned int len, unsigned int *&out);

  // # Data
  //  Matrix size (linear size)
  unsigned int n_p;
  // Derived sizes (all 0 if n_p equals 0)
  // <group>
  // Total size
  unsigned int len_p;
  // <src>n-1</src>
  unsigned int nm1_p;
  // <src>2n-1</src>
  int n2m1_p;
  // <src>2n+1</src>
  int n2p1_p;
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

}  // namespace casacore

#endif
