//# dGaussianND.cc:  demo for the n-dim Gaussian class
//# Copyright (C) 1996,1999,2001,2002
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

#include <casacore/scimath/Functionals/GaussianND.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main(){
  cout << "The example from the Header File" << endl;
  uInt ndim = 2;
  Float height = 1;
  Vector<Float> mean(ndim); mean(0) = 0, mean(1) = 1;
  Vector<Float> variance(ndim); variance(0) = .1, variance(1) = 7;
  GaussianND<Float> g(ndim, height, mean, variance); 
  Vector<Float> x(ndim); x = 0;
  cout << "g("<< x <<") = " << g(x) <<endl; // g([0,0])=1*exp(-1/2*1/7);
  x(1)++;
  cout << "g("<< x <<") = " <<g(x) <<endl;  // g([0,1])= 1
  cout << "Height: " << g.height() <<endl;    // Height: 1
  cout << "Flux: " << g.flux() << endl;       // Flux: 2*Pi*Sqrt(.1*7)
  cout << "Mean: " << g.mean() << endl;  // Mean: [0, -1]
  cout << "Variance: " << g.variance() <<endl;  // Variance: [.1, 7]
  cout << "Covariance: "<< g.covariance()<<endl;// Covariance: [.1, 0]
                                                     //             [0,  7]
  g.setFlux(1);
  cout << "g("<< x <<") = " <<g(x) <<endl;  //g([0,1])=1/(2*Pi*Sqrt(.7))
  cout << "Height: " << g.height() <<endl;    // Height: 1/(2*Pi*Sqrt(.7))
  cout << "Flux: " << g.flux() << endl;       // Flux: 1
  cout << "Mean: " << g.mean() << endl;  // Mean: [0, -1]
  cout << "Variance: " << g.variance() <<endl;  // Variance: [.1, 7]
  cout << "Covariance: "<< g.covariance()<<endl;// Covariance: [.1, 0]
                                                     //             [0,  7]
}
