//# MatrixSolver.cc: Abstract base class for solvers of AX=B
//# Copyright (C) 1994,1995
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
//# $ID: $

#include <aips/aips.h>
#include <aips/Exceptions.h>
#include <trial/Arrays/MatrixSolver.h>

#include <aips/Logging/LogSink.h>
#include <aips/Logging/LogMessage.h>

#include <strstream.h>

MatrixSolver::MatrixSolver():SolTolerance(0.0), MaxIterations(0), solved(False),
   gain(1.0){}

MatrixSolver::MatrixSolver(const MatrixSolver & other) {    
    AMatrix.reference((Matrix<FType> &)other.AMatrix);
    BVector.reference((Vector<FType> &)other.BVector);
    RVector.reference((Vector<FType> &)other.RVector);
    XVector.reference((Vector<FType> &)other.XVector);
    BNorm=other.BNorm;
    RNorm=other.RNorm;
    solved=other.solved;
    MaxIterations=other.MaxIterations;
    SolTolerance=other.SolTolerance;
    gain=other.gain;
}

MatrixSolver::MatrixSolver(const Matrix<FType> & amatrix,
		     const Vector<FType> & bvector) 
:  SolTolerance(0.0), MaxIterations(0), solved(False), gain(1.0) {
    AMatrix.reference((Matrix<FType> &)amatrix);
    BVector.reference((Vector<FType> &)bvector);
    XVector.resize(AMatrix.shape()(1));
    RVector.resize(bvector.shape());
    BNorm=norm(BVector);
    RNorm=BNorm;
}

void MatrixSolver::setAB(const Matrix<FType> & amatrix,
		     const Vector<FType> & bvector) 
{
    AMatrix.reference((Matrix<FType> &)amatrix);
    BVector.reference((Vector<FType> &)bvector);
    XVector.resize(AMatrix.shape()(1));
    RVector.resize(bvector.shape());
    BNorm=norm(BVector);
    RNorm=BNorm;
}

void MatrixSolver::setX(const Vector<FType> & xvector) 
{
    XVector.reference((Vector<FType> &)xvector);
}

MatrixSolver & MatrixSolver::operator=(const MatrixSolver & other) {
    if (this==&other) return *this;
    AMatrix.reference((Matrix<FType> &)other.AMatrix);
    BVector.reference((Vector<FType> &)other.BVector);
    RVector.reference((Vector<FType> &)other.RVector);
    XVector.reference((Vector<FType> &)other.XVector);
    BNorm=other.BNorm;
    RNorm=other.RNorm;
    solved=other.solved;
    MaxIterations=other.MaxIterations;
    SolTolerance=other.SolTolerance;
    gain=other.gain;
    return *this;
}    

// Virtual destructor
MatrixSolver::~MatrixSolver() {};

// Virtual solve method
Bool MatrixSolver::solve() {return False;}

// Returning the residual vector is a general operation.
const Vector<FType> & MatrixSolver::getResidual() {

  // Calculate residual vector
  RVector=BVector.ac()-product(AMatrix, XVector).ac();

  // Calculate norm of RVector
  RNorm = norm(RVector);

  return RVector;
}

const Vector<FType> & MatrixSolver::getSolution() {
  return XVector;
}

// Determine if the solution has small enough residual vector.
Bool MatrixSolver::accurateSolution() {

  LogMessage message(LogOrigin("MatrixSolver", "accurateSolution"));

  // Calculate norm of RVector assuming that RVector is current
  RNorm = norm(RVector);

  // Now determine if the residual vector norm is less than the
  // Solution tolerance times the original BVector norm.
  
  ostrstream o;o<<"MatrixSolver: Norms of initial and residual vectors "<<
		 BNorm<<", "<<RNorm;
  message.message(o);
  logSink().post(message);
  if (RNorm<(SolTolerance*BNorm)) {
    setSolved(True);
  }
  else {
    setSolved(False);
  }
  return Solved();
}

