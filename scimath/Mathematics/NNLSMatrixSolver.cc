//# NNLSMatrixSolver.cc: concrete class for NNLS solvers of AX=B
//# Copyright (C) 1994,1995,1999,2001,2002,2003
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

#include <casacore/casa/aips.h>
#include <casacore/scimath/Mathematics/NNLSMatrixSolver.h>
#include <casacore/casa/Arrays/MatrixMath.h>

#include <casacore/casa/Logging/LogSink.h>
#include <casacore/casa/Logging/LogMessage.h>

#include <casacore/casa/sstream.h>
namespace casacore { //# NAMESPACE CASACORE - BEGIN

#if !defined(NEED_FORTRAN_UNDERSCORES)
#define NEED_FORTRAN_UNDERSCORES 1
#endif

#if NEED_FORTRAN_UNDERSCORES
   #define nnls nnls_
#endif

extern "C" {
  void nnls(FType*,int*,int*,int*,FType*,FType*,float*,FType*,FType*,int*,int*,int*);
}

// Default Constructor
NNLSMatrixSolver::NNLSMatrixSolver(): MatrixSolver() {}
  
// Copy Constructor
NNLSMatrixSolver::NNLSMatrixSolver(const NNLSMatrixSolver & other)
: MatrixSolver(other) {}
  
// Create a NNLSMatrixSolver from a matrix A and a Vector B
// <note role=warning> A and B are accessed by reference, so don't 
// modify them during the lifetime of the NNLSMatrixSolver </note>
NNLSMatrixSolver::NNLSMatrixSolver(const Matrix<FType> & A,
				   const Vector<FType> & B)
: MatrixSolver(A,B) {}
  
// Destructor
NNLSMatrixSolver::~NNLSMatrixSolver() {}

Bool NNLSMatrixSolver::solve() // Solve AX=B for X
{
  
  LogMessage message(LogOrigin("NNLSMatrixSolver","solve"));

  Bool delete_it;
  FType *a_data = AMatrix.getStorage(delete_it);
  FType *x_data = XVector.getStorage(delete_it);
  FType *b_data = BVector.getStorage(delete_it);
  int nflux=XVector.nelements();
  int ndata=BVector.nelements();
  float rnorm=0.0;
  FType *w=new FType[nflux];
  FType *zz=new FType[ndata];
  int *index=new int[nflux];
  int itmax=MaxIters();
  if(itmax==0) itmax=3*nflux;
  int mode=0;

  // Call Fortran NNLS routine
  nnls(a_data,&ndata,&ndata,&nflux,b_data,x_data,&rnorm,w,zz,index,&itmax,
       &mode);

  RVector=BVector-product(AMatrix,XVector);	// Update residual vector
  if (mode==2) {
    ostringstream o;o<<"dimensions set up incorrectly";
    message.priority(LogMessage::SEVERE);
    message.message(o);logSink().post(message);
    setSolved(False);
    return Solved();
  }
  if (mode==3) {
    ostringstream o;o<<"Exceeded number of iterations";
    message.priority(LogMessage::SEVERE);
    message.message(o);logSink().post(message);
    setSolved(False);
    return Solved();
  }
  
  if(accurateSolution()) {
    ostringstream o;o<<"Solution acheived";
    message.message(o);logSink().post(message);
    setSolved(True);
  }
  else {
    ostringstream o;o<<"Solution not formally accurate enough";
    message.message(o);logSink().post(message);
    setSolved(False);
  }
  return Solved();
}


} //# NAMESPACE CASACORE - END

