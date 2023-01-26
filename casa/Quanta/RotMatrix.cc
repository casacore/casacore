//# RotMatrix.cc: a 3x3 rotation matrix
//# Copyright (C) 1995,1996,1997,1998,1999
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

//# Includes
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Quanta/RotMatrix.h>
#include <casacore/casa/Quanta/Euler.h>
#include <casacore/casa/IO/ArrayIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// RotMatrix class

//# Constructors
RotMatrix::RotMatrix() {
  for (int32_t i=0; i<3; i++)
    for (int32_t j=0; j<3; j++) {
      rotat[i][j] = (i==j) ? double(1.0) : double(0.0);
    }
}

RotMatrix::RotMatrix(const RotMatrix &other) {
  for (int32_t i=0; i<3; i++)
    for (int32_t j=0; j<3; j++)
      rotat[i][j] = other.rotat[i][j];
}

RotMatrix &RotMatrix::operator=(const RotMatrix &other) {
  if (this != &other) {
    for (int32_t i=0; i<3; i++)
      for (int32_t j=0; j<3; j++)
	rotat[i][j] = other.rotat[i][j];
  }
  return *this;
}

RotMatrix::RotMatrix(const Euler &other) {
  for (int32_t k=0; k<3; k++)
    for (int32_t j=0; j<3; j++) {
      rotat[k][j] = (k==j) ? double(1.0) : double(0.0);
    }
  for (int32_t i=0; i<3; i++)
    applySingle(other(i),other.get(i));
}

RotMatrix::RotMatrix(const Euler &other, int32_t ax0, int32_t ax1, int32_t ax2) {
  for (int32_t i=0; i<3; i++)
    for (int32_t j=0; j<3; j++) {
      rotat[i][j] = (i==j) ? double(1.0) : double(0.0);
    }
  DebugAssert(abs(ax0) <= 3 && abs(ax1) <= 3 && abs(ax2) <= 3, AipsError);
  applySingle(other(0),ax0);
  applySingle(other(1),ax1);
  applySingle(other(2),ax2);
}

//# Destructor
RotMatrix::~RotMatrix() {}

//# Operators

RotMatrix &RotMatrix::operator*=(const RotMatrix &other) {
  double a[3];
  int32_t j,k;
  for (int32_t i=0; i<3; i++) {
    for (j=0; j<3; j++)
      a[j] = rotat[i][j];
    for (j=0; j<3; j++) {
      rotat[i][j] = a[0];
      rotat[i][j] *= other.rotat[0][j];
      for (k=1; k<3; k++) {
	rotat[i][j] += a[k] * other.rotat[k][j];
      }
    }
  }
  return *this;
}

RotMatrix RotMatrix::operator*(const RotMatrix &other) const {
    RotMatrix result = *this;
    result *= other;
    return result;
}

double &RotMatrix::operator()(uint32_t row, uint32_t column) {
    DebugAssert(row < 3 && column < 3, AipsError);
    return rotat[row][column];
}

const double &RotMatrix::operator()(uint32_t row, uint32_t column) const{
    DebugAssert(row < 3 && column < 3, AipsError);
    return rotat[row][column];
}

//# Methods

Matrix<double> RotMatrix::get() const {
  Matrix<double> tmp(3,3);
  for (int32_t row=0; row<3; row++)
    for (int32_t col=0; col<3; col++)
      tmp(row, col) = rotat[row][col];
  return tmp;
}

void RotMatrix::transpose() {
  double tmp;
  for (int32_t row=0; row<3; row++)
    for (int32_t col=row+1; col<3; col++) {
      tmp = rotat[row][col];
      rotat[row][col] = rotat[col][row];
      rotat[col][row] = tmp;
    }
}

void RotMatrix::set(const Matrix<double> &in) {
  for (int32_t row=0; row<3; row++)
    for (int32_t col=0; col<3; col++)
      rotat[row][col] = in(row, col);
}

void RotMatrix::set(const Vector<double> &in0, const Vector<double> &in1,
		    const Vector<double> &in2) {
  for (int32_t col=0; col<3; col++) {
    rotat[0][col] = in0(col);
    rotat[1][col] = in1(col);
    rotat[2][col] = in2(col);
  }
}

ostream &operator<< (ostream &os, const RotMatrix &rot) {
  os << rot.get();
  return os;
}

void RotMatrix::
applySingle(double angle, int32_t which) {
  if (angle*which != 0.0) {
    RotMatrix tmp;
    int32_t i = which%3;
    int32_t j = (i + 1)%3;
    tmp.rotat[i][i] = tmp.rotat[j][j] = cos(angle);
    tmp.rotat[i][j] = -(tmp.rotat[j][i] = sin(angle));
    this->operator*=(tmp);
  }
}

} //# NAMESPACE CASACORE - END

