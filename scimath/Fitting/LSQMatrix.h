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
//#
//# $Id$

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
  Double *row_pub(uInt i) const { return (i<n_p) ? row(i) : 0; };
  // Get next row or previous row pointer in normal equation if the pointer
  // <src>row</src> is at row <src>i</src>.
  // <group>
  void incRow_pub(Double *&row, uInt i) const { if (i<n_p-1) incRow(row,i); };
  void decRow_pub(Double *&row, uInt i) const { if (i>0) decRow(row,i); };
  // </group>
  // Get diagonal element pointer <src>[i][i]</src>
  Double *diag_pub(uInt i) const { return ((i<n_p) ? diag(i) : 0); };
  // Get length of triangular array
  uInt nelements_pub() const { return (len_p); };
  // Get number of rows
  uInt nrows_pub() const { return n_p; };
  // Make diagonal element 1 if zero (Note that this is always called when
  // <src>invert()</src> is called). Only n-length sub-matrix is done.
  void doDiagonal_pub(uInt n) { if (n<n_p) doDiagonal(n); }
  // Multiply n-length of diagonal with <src>1+fac</src>
  void mulDiagonal_pub(uInt n, Double fac) { if (n<n_p) mulDiagonal(n,fac); };
  // Add <src>fac</src> to n-length of diagonal
  void addDiagonal_pub(uInt n, Double fac) { if (n<n_p) addDiagonal(n,fac); };
  // Determine max of abs values of n-length of diagonal
  Double maxDiagonal_pub(uInt n){ return ((n<n_p) ? maxDiagonal(n) : 0); };
  // </group>

 private:
  //# Constructors
  // Default constructor (empty, only usable after a <src>set(n)</src>)
  LSQMatrix();
  // Construct an object with the number of rows and columns indicated.
  // If a <src>Bool</src> argument is present, the number
  // will be taken as double the number given (assumes complex). 
  // <group>
  explicit LSQMatrix(uInt n);
  LSQMatrix(uInt n, Bool);
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
  Double &operator[](uInt index) { return (trian_p[index]); };
  Double operator[](uInt index) const { return (trian_p[index]); };
  // </group>

  //# General Member Functions
  // Reset all data to zero
  void reset() { clear(); };
  // Set new sizes (default is for Real, a Bool argument will make it complex)
  // <group>
  void set(uInt n);
  void set(uInt n, Bool);
  // </group>
  // Get row pointer in normal equation (points to element <src>[i][0]</src>)
  Double *row(uInt i) const { return &trian_p[((n2m1_p-i)*i)/2]; };
  // Get next row or previous row pointer in normal equation if the pointer
  // <src>row</src> is at row <src>i</src>.
  // <group>
  void incRow(Double *&row, uInt i) const { row += nm1_p-i; };
  void decRow(Double *&row, uInt i) const { row -= n_p-i; };
  // </group>
  // Get diagonal element pointer <src>[i][i]</src>
  Double *diag(uInt i) const { return &trian_p[((n2p1_p-i)*i)/2]; };
  // Get length of triangular array
  uInt nelements() const { return (len_p); };
  // Get number of rows
  uInt nrows() const { return n_p; };
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
  void doDiagonal(uInt n);
  // Multiply n-length of diagonal with <src>1+fac</src>
  void mulDiagonal(uInt n, Double fac);
  // Add <src>fac</src> to n-length of diagonal
  void addDiagonal(uInt n, Double fac);
  // Determine max of abs values of n-length of diagonal
  Double maxDiagonal(uInt n);
  // Create a Matrix from a record. An error message is generated, and False
  // returned if an invalid record is given. A valid record will return True.
  // Error messages are postfixed to error.
  // <group>
  Bool fromRecord(String &error, const RecordInterface &in);
  // </group>
  // Create a record from an LSQMatrix. The return will be False and an error
  // message generated only if the object does not contain a valid Matrix.
  // Error messages are postfixed to error.
  Bool toRecord(String &error, RecordInterface &out) const;
  // Get identification of record
  const String &ident() const;
  // Convert a <src>carray</src> to/from a record. Field only written if 
  // non-zero length. No carray created if field does not exist on input.
  // False returned if unexpectedly no data available for non-zero length
  // (put), or a field has zero length vector(get).
  // <group>
  static Bool putCArray(String &error, RecordInterface &out,
			const String &fname,
			uInt len, const Double * const in);
  static Bool getCArray(String &error, const RecordInterface &in,
			const String &fname,
			uInt len, Double *&out);
  static Bool putCArray(String &error, RecordInterface &out,
			const String &fname,
			uInt len, const uInt * const in);
  static Bool getCArray(String &error, const RecordInterface &in,
			const String &fname,
			uInt len, uInt *&out);
  // </group>

  // Save or restore using AipsIO.
  void fromAipsIO (AipsIO& in);
  void toAipsIO (AipsIO& out) const;
  static void putCArray (AipsIO& out, uInt len, const Double* const in);
  static void getCArray (AipsIO& in, uInt len, Double*& out);
  static void putCArray (AipsIO& out, uInt len, const uInt* const in);
  static void getCArray (AipsIO& in, uInt len, uInt*& out);

  //# Data
  // Matrix size (linear size)
  uInt n_p;
  // Derived sizes (all 0 if n_p equals 0)
  // <group>
  // Total size
  uInt len_p;
  // <src>n-1</src>
  uInt nm1_p;
  // <src>2n-1</src>
  Int n2m1_p;
  // <src>2n+1</src>
  Int n2p1_p;
  // </group>
  // Matrix (triangular n_p * n_p)
  Double *trian_p;
  // Record field names
  static const String tmatsiz;
  static const String tmatdat;
  // <group>
  // </group>
  //
};


} //# NAMESPACE CASACORE - END

#endif
