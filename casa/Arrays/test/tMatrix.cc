#include <boost/test/unit_test.hpp>

#include <cstdint>
#include <limits>

#include "../ArrayLogical.h"
#include "../Matrix.h"

using namespace casacore;

BOOST_AUTO_TEST_SUITE(matrix_class)

// Simple matrix tests

BOOST_AUTO_TEST_CASE( matrix_initialize )
{
  Matrix<int> a(5u, 5u);
  a = 3;
  BOOST_CHECK(a.nrow() == a.ncolumn() && a.nrow() == 5);
  BOOST_CHECK(allEQ(a, 3));
  BOOST_CHECK(allLE(a, 3)); 
  BOOST_CHECK(allEQ(a, a)); 
  BOOST_CHECK(allNE(a, 1));
  
  BOOST_CHECK (allEQ(Matrix<int>(5u,6u, 4u), 4));
}

BOOST_AUTO_TEST_CASE( matrix_assign )
{
  Matrix<int> a(5u, 5u, 3), b;
  b.assign_conforming( 2*a );
  BOOST_CHECK(allEQ (b, 6));
  a.row(3) = 6;
  BOOST_CHECK(allEQ (a.row(3), 6));
  a.column(3) = 1;
  BOOST_CHECK(allEQ (a.column(3), 1));
  a.diagonal(-1) = 7;
  BOOST_CHECK(allEQ (a.diagonal(-1), 7));
}

BOOST_AUTO_TEST_CASE( matrix_slice )
{
	Matrix<int> a(5u, 5u, 3);
	Matrix<int> c = a(Slice(2,1), Slice(3,1));
	BOOST_CHECK_EQUAL(c.size(), 1);
	BOOST_CHECK_EQUAL(c(0, 0), 3);
}

BOOST_AUTO_TEST_CASE( matrix_reform )
{
  Matrix<int> a(5u, 5u, 3);
  IPosition l(1);
  l(0) = a.nelements();
  Vector<int> d(a.reform(l));
  for (int i = 0; i < 5; i++)
    for (int j = 0; j < 5; j++)
      BOOST_CHECK(a(i,j) == d(i + j*5));
}

BOOST_AUTO_TEST_CASE( matrix_from_vector )
{
  Vector<int> v(10);
  indgen(v);
  Matrix<int> vm(v);
  BOOST_CHECK_EQUAL(vm.ndim(), 2);
  BOOST_CHECK_EQUAL(vm.nelements(), v.nelements());
  for (int i = 0; i < int(v.nelements()); i++)
  {
    BOOST_CHECK_EQUAL(vm(i,0), v(i));
    BOOST_CHECK_EQUAL(v(i), i);
  }
}

BOOST_AUTO_TEST_CASE( matrix_storage1 )
{
  Matrix<int> m(8u, 8u, -1);
  m = -1;
  bool deleteIt;
  int *storage = m.getStorage(deleteIt);
  BOOST_CHECK_EQUAL(deleteIt, false);
  BOOST_CHECK_EQUAL(storage[0], -1);
  BOOST_CHECK_EQUAL(storage[8*8-1], -1);
  
  for (size_t i = 0; i < m.nelements(); i++)
    storage[i] = 1;
  BOOST_CHECK(allEQ (m, 1));
	m.putStorage(storage, deleteIt);
}

BOOST_AUTO_TEST_CASE( matrix_storage2 )
{
  Matrix<int> m(8u, 8u, 1);
  bool deleteIt;
	int *storage = m(Slice(0,2,3), Slice(2,2,4)).getStorage(deleteIt);
	BOOST_CHECK_EQUAL(deleteIt, true);
	for (int i=0; i < 4; i++)
    storage[i] = 0;
	BOOST_CHECK_EQUAL(m(0,2), 1);
  BOOST_CHECK_EQUAL(m(0,6), 1);
  BOOST_CHECK_EQUAL(m(3,2), 1);
  BOOST_CHECK_EQUAL(m(3,6), 1);
	m(Slice(0,2,3), Slice(2,2,4)).putStorage(storage, deleteIt);
	BOOST_CHECK_EQUAL(m(0,2), 0);
  BOOST_CHECK_EQUAL(m(0,6), 0);
  BOOST_CHECK_EQUAL(m(3,2), 0);
  BOOST_CHECK_EQUAL(m(3,6), 0);
}

void checkRCDVec (const Vector<int>& v1, const Vector<int>& v2)
{
  BOOST_CHECK (allEQ(v1,v2));
  Array<int>::const_iterator iter1 = v1.begin();
  Array<int>::const_iterator iter2 = v2.begin();
  for (size_t i=0; i<v1.size(); ++i, ++iter1, ++iter2) {
    BOOST_CHECK (v1[i] == v2[i]);
    BOOST_CHECK (iter1 != v1.end());
    BOOST_CHECK (iter2 != v2.end());
    BOOST_CHECK (v1[i] == *iter1);
    BOOST_CHECK (v2[i] == *iter2);
  }
  BOOST_CHECK (iter1 == v1.end());
  BOOST_CHECK (iter2 == v2.end());
}

void checkRCD (const Vector<int>& vn, const Vector<int>& vc)
{
  Slice sl(1,3,2);  // start=1,n=3,inc=2
  checkRCDVec (vn, vc);
  checkRCDVec (vn(sl), vc(sl));
  checkRCDVec (vn(IPosition(1,1), IPosition(1,5), IPosition(1,2)), vn(sl));
  checkRCDVec (vc(IPosition(1,1), IPosition(1,5), IPosition(1,2)), vc(sl));
}

void doRowColDiag (const Matrix<int>& m)
{
  // Make contiguous copy of matrix.
  Matrix<int> cm(m.copy());
  BOOST_CHECK (cm.contiguousStorage());
  // Check row selection and subsetting.
  Vector<int> r0(m.row(1));
  Vector<int> cr0(cm.row(1));
  BOOST_CHECK (!r0.contiguousStorage() && !cr0.contiguousStorage());
  checkRCD (r0, cr0);
  // Check column selection and subsetting.
  Vector<int> c0(m.column(1));
  Vector<int> cc0(cm.column(1));
  BOOST_CHECK (cc0.contiguousStorage());
  checkRCD (c0, cc0);
  // Check diagonal selection and subsetting.
  Vector<int> d0(m.diagonal());
  Vector<int> cd0(cm.diagonal());
  BOOST_CHECK (!d0.contiguousStorage() && !cd0.contiguousStorage());
  checkRCD (d0, cd0);
}

BOOST_AUTO_TEST_CASE( row_col_diag )
{
  Matrix<int> m(18,18);
  indgen (m);
  doRowColDiag (m);
  doRowColDiag (m(IPosition(2,1,1), IPosition(2,12,12), IPosition(2,1,1)));
  doRowColDiag (m(IPosition(2,1,1), IPosition(2,12,12), IPosition(2,2,2)));
  doRowColDiag (m(IPosition(2,1,2), IPosition(2,17,12), IPosition(2,3,2)));
}

BOOST_AUTO_TEST_CASE( init_from_data )
{
  IPosition shape(2, 2, 2);
  std::unique_ptr<std::vector<int>> values(new std::vector<int>(shape.product()));
  Matrix<int> c(shape, values->data(), COPY);
  values.reset();
  c.resize(IPosition(2, 2, 3), false);
  c.resize(4, 4, false);
  BOOST_CHECK(true);
}

BOOST_AUTO_TEST_CASE( reference_1d_array )
{
  // Matrix.reference(1-d array)
  Array<int> ai(IPosition(1,10));
  Matrix<int> mi;
  mi.reference(ai);
  BOOST_CHECK(mi.shape() == IPosition(2,10,1));
  ai = 11;
  BOOST_CHECK(allEQ(mi, 11));
}

BOOST_AUTO_TEST_CASE( matrix_identity )
{
  for (size_t i=0; i<20; i++) {
    Matrix<double> x = Matrix<double>::identity(i);
    BOOST_CHECK(x.ncolumn() == i);
    BOOST_CHECK(x.nrow() == i);
    for (size_t j=0; j<i; j++) {
      for (size_t k=0; k<i; k++) {
        if (j == k) {
          BOOST_CHECK(x(j, k) == 1);
        } else {
          BOOST_CHECK(x(j, k) == 0);
        }
      }
    }
  }
}

BOOST_AUTO_TEST_CASE( multi_dimensional_copy )
{
  // Test the copy ctor for arrays with !1 dimension.
  Array<int> arr;
  Matrix<int> mat(arr);
  BOOST_CHECK (mat.ndim()==2  &&  mat.nelements()==0);
  BOOST_CHECK (mat.shape() == IPosition(2,0));
}

BOOST_AUTO_TEST_CASE( non_degenerate )
{
  // Test if a non-degerate Matrix throws an exception.
  Matrix<int> m1(IPosition(2,1,2));
  Matrix<int> mr;
  BOOST_CHECK_THROW(mr.nonDegenerate(m1), std::exception);
  mr.nonDegenerate(m1, 1);
  BOOST_CHECK (mr.shape() == IPosition(2,1,2));
}

BOOST_AUTO_TEST_CASE( array_assign )
{
  // Array assign
  Array<int> ai(IPosition(1,10));
  ai = 1;
  Matrix<int> mi(5,3);
  mi = 2;
  bool exc = false;
  try {
    mi.assign (ai);
  } catch (std::exception&) {
    exc = true;
  }
  BOOST_CHECK (exc);
  BOOST_CHECK(mi.shape() == IPosition(2,5,3));
  BOOST_CHECK(allEQ(mi, 2));
  ai.assign (mi);
  BOOST_CHECK(ai.shape() == IPosition(2,5,3));
  BOOST_CHECK(allEQ(ai, 2));
}

BOOST_AUTO_TEST_CASE( uninitialized_constructor_a )
{
	Matrix<int> y1(5, 4, Matrix<int>::uninitialized);
  BOOST_CHECK_EQUAL (y1.shape()[0], 5);
  BOOST_CHECK_EQUAL (y1.shape()[1], 4);
  y1 = 7;
	BOOST_CHECK (allEQ(y1, 7));
}

BOOST_AUTO_TEST_CASE( uninitialized_constructor_b )
{
	Matrix<int> y1(IPosition{5, 4}, Matrix<int>::uninitialized);
  BOOST_CHECK_EQUAL (y1.shape()[0], 5);
  BOOST_CHECK_EQUAL (y1.shape()[1], 4);
  y1 = 7;
	BOOST_CHECK (allEQ(y1, 7));
}

BOOST_AUTO_TEST_SUITE_END()
