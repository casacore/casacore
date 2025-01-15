//# tArrayIO2.cc: This program tests the tArrayIO2 functions
//# Copyright (C) 1993,1994,1995,1996,1999,2000,2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

//# Includes

#include "../ArrayStr.h"
#include "../Vector.h"
#include "../Matrix.h"

#include <boost/test/unit_test.hpp>
#include <boost/filesystem/operations.hpp>

// This test program tests the ArrayIO2 functions.
// It writes all kind of stuff, reads it back and writes it to stdout.
// A script compares this output with a reference output file.

using namespace casacore;

BOOST_AUTO_TEST_SUITE(array_IO_2)

BOOST_AUTO_TEST_CASE( bin )
{
  Vector<int> ip(1000);
  for (size_t i=0; i<1000; i++) {
    ip(i) = i*2;
  }
  write_array (ip, "tArrayIO2_tmp.data");
  Vector<int> ipi(1000);
  read_array (ipi, "tArrayIO2_tmp.data");
  for (size_t i=0; i<1000; i++)
  {
    BOOST_CHECK_EQUAL(ipi(i), i*2);
  }
  boost::filesystem::remove("tArrayIO2_tmp.data");
}

BOOST_AUTO_TEST_CASE( matrix_read )
{
  const char* str =
    "1 2 3 4\n"
    "2 3 4 5\n"
    "1 4 5 6\n"
    "4 5 6 7\n";
    
  std::ofstream ofile("tArrayIO2.in_mat");
  ofile << str << '\n';
  ofile.close();
  
  Matrix<int> mat;
  readAsciiMatrix (mat, "tArrayIO2.in_mat");
  std::ostringstream ostr;
  ostr << mat;
  BOOST_CHECK_EQUAL(ostr.str(),
    "Axis Lengths: [4, 4]  (NB: Matrix in Row/Column order)\n"
    "[1, 2, 3, 4\n"
    " 2, 3, 4, 5\n"
    " 1, 4, 5, 6\n"
    " 4, 5, 6, 7]\n");
  boost::filesystem::remove("tArrayIO2.in_mat");
}

BOOST_AUTO_TEST_CASE( matrix_write )
{
  Matrix<int> mat(4, 4);
  for(int y=0; y!=4; ++y) {
    for(int x=0; x!=4; ++x) {
      mat(IPosition{x, y}) = x + y + 1;
    }
  }
  writeAsciiMatrix (mat, "tArrayIO2_tmp.mat");
  
  std::ifstream ifile("tArrayIO2_tmp.mat");
  std::stringstream sstr;
  sstr << ifile.rdbuf();
  BOOST_CHECK_EQUAL(sstr.str(),
    "1  2  3  4  \n"
    "2  3  4  5  \n"
    "3  4  5  6  \n"
    "4  5  6  7  \n");
  boost::filesystem::remove("tArrayIO2_tmp.mat");
}

BOOST_AUTO_TEST_CASE( vector_read )
{
  std::ofstream ofile("tArrayIO2.in_vec");
  const char* str =
    "1.23\n"
    "2.34 3.45\n"
    " 4.56\n"
    "   5.67 6.78 9\n";
  ofile << str << '\n';
  ofile.close();
  
  Vector<double> vec;
  readAsciiVector (vec, "tArrayIO2.in_vec");
  Vector<double> ref{1.23, 2.34, 3.45, 4.56, 5.67, 6.78, 9};
  BOOST_CHECK_EQUAL(vec.shape(), ref.shape());
  for(size_t i=0; i!=ref.nelements(); ++i)
    BOOST_CHECK_EQUAL(vec[i], ref[i]);
  boost::filesystem::remove("tArrayIO2.in_vec");
}

BOOST_AUTO_TEST_CASE( vector_write )
{
  Vector<double> vec{1.23, 2.34, 3.45, 4.56, 5.67, 6.78, 9};
  writeAsciiVector (vec, "tArrayIO2_tmp.vec");
  
  std::ifstream ifile("tArrayIO2_tmp.vec");
  std::stringstream sstr;
  sstr << ifile.rdbuf();
  BOOST_CHECK_EQUAL(sstr.str(),
    "1.23  2.34  3.45  4.56  5.67  6.78  9  \n");
  boost::filesystem::remove("tArrayIO2_tmp.vec");
}

template<typename Arr>
void checkExtract(const std::string& str, const Arr& ref, const std::string& outputStr)
{
  std::stringstream istr(str);
  Arr a;
  istr >> a;
  BOOST_CHECK(istr);
  BOOST_CHECK_EQUAL(a.shape().size(), ref.shape().size());
  BOOST_CHECK_EQUAL(a.shape(), ref.shape());
  BOOST_CHECK_EQUAL(a.shape(), ref.shape());
  std::ostringstream astr, refstr;
  astr << a;
  refstr << ref;
  BOOST_CHECK_EQUAL(astr.str(), refstr.str());
  BOOST_CHECK_EQUAL(astr.str(), outputStr);
}

BOOST_AUTO_TEST_CASE( double_istream_extract_arr1 )
{
  checkExtract(
    "[1 2 3 4 5 6]",
    Array<double>{1,2,3,4,5,6},
    "[1, 2, 3, 4, 5, 6]"
  );
}

BOOST_AUTO_TEST_CASE( double_istream_extract_matrix1 )
{
  const double data[] = {1,2,3,4,5,6};
  checkExtract(
    "[1 2 3 4 5 6]\n",
    Matrix<double>(IPosition{6, 1}, data),
    "Axis Lengths: [6, 1]  (NB: Matrix in Row/Column order)\n"
      "[1\n"
      " 2\n"
      " 3\n"
      " 4\n"
      " 5\n"
      " 6]\n"
  );
}

BOOST_AUTO_TEST_CASE( double_istream_extract_arr2 )
{
  const double data[] = {1,4,2,5,3,6};
  checkExtract(
    "{[2 3]}[1 2 3 4 5 6]\n",
    Array<double>(IPosition{2, 3}, data),
    "Axis Lengths: [2, 3]  (NB: Matrix in Row/Column order)\n"
      "[1, 2, 3\n"
      " 4, 5, 6]\n"
  );
}

BOOST_AUTO_TEST_CASE( double_istream_extract_matrix2 )
{
  const double data[] = {1,4,2,5,3,6};
  checkExtract(
    "{[2 3]}[1 2 3 4 5 6]\n",
    Matrix<double>(IPosition{2, 3}, data),
    "Axis Lengths: [2, 3]  (NB: Matrix in Row/Column order)\n"
      "[1, 2, 3\n"
      " 4, 5, 6]\n"
  );
}

BOOST_AUTO_TEST_CASE( double_istream_extract_arr3 )
{
  const double data[] = {1,2,3,4,5,6};
  checkExtract(
    "{t[2 3]}[1 2 3 4 5 6]\n",
    Array<double>(IPosition{2, 3}, data),
    "Axis Lengths: [2, 3]  (NB: Matrix in Row/Column order)\n"
      "[1, 3, 5\n"
      " 2, 4, 6]\n"
  );
}

BOOST_AUTO_TEST_CASE( double_istream_extract_string1 )
{
  checkExtract(
    "[ x, y , x]\n",
    Array<std::string>{"x","y","x"},
    "[x, y, x]"
  );
}

BOOST_AUTO_TEST_CASE( double_istream_extract_string2 )
{
  checkExtract(
    "[x, y]\n",
    Array<std::string>{"x","y"},
    "[x, y]"
  );
}

BOOST_AUTO_TEST_SUITE_END()
