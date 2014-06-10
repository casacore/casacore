//# Copyright (C) 1995,1996,1997,1999,2001,2002,2005
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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#

#include <scimath/Functionals/PoissonFunction.h>

#include <scimath/Mathematics/AutoDiff.h>
#include <scimath/Mathematics/AutoDiffA.h>
#include <scimath/Mathematics/AutoDiffIO.h>
#include <scimath/Mathematics/AutoDiffMath.h>
#include <casa/BasicSL/Constants.h>
#include <casa/BasicMath/Math.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Utilities/Assert.h>

#include <casa/iostream.h>

#include <casa/namespace.h>
int main() {
  PoissonFunction<Double> null;
  AlwaysAssertExit(null.lambda() == 1.0 && null.height() == 1.0);

  const float ERROR_TOL = .00001;
  AlwaysAssertExit(near(null(0.0), .367879, ERROR_TOL) && near(null(1.0), .367879, ERROR_TOL));
  // name()
  cout << "Name of function: " << null.name() << endl; 
  AlwaysAssertExit(null.name() == "poisson");

  PoissonFunction<Double> poisson1(4.0, 2.0);
  AlwaysAssertExit(poisson1.lambda() == 4.0 && poisson1.height()==2.0);
  const PoissonFunction<Double> &cpoisson1 = poisson1;
  AlwaysAssertExit(cpoisson1.lambda() == 4.0 && cpoisson1.height()==2.0 );
  poisson1.setLambda(2.0);
  poisson1.setHeight(3.0);
  AlwaysAssertExit(poisson1[PoissonFunction<Double>::LAMBDA] == 2.0 );
  AlwaysAssertExit(poisson1[PoissonFunction<Double>::HEIGHT] == 3.0 );
  poisson1[PoissonFunction<Double>::LAMBDA] = 3.0;
  poisson1[PoissonFunction<Double>::HEIGHT] = 2.0;
  // <<
  cout << "Function Parameters: " << poisson1 << endl;

  AlwaysAssertExit(near(poisson1(3.0),0.4480836, ERROR_TOL ));
  Vector<Double> xvec(1);
  xvec = 5.0;
  cout << "Value at 5:      " << poisson1(xvec(0)) << endl;

  ///
  // Copy constructor
  PoissonFunction<Double> pc(poisson1);
  cout << "Copy: " << pc << "; "
		  <<"f(5) = " << pc(xvec(0)) << endl;

  PoissonFunction<Double> pcb(pc);
  cout << "Copy back: " << pcb << endl <<"f(5) = " << pcb(xvec(0)) << endl;
  AlwaysAssertExit(near(poisson1(xvec(0)), .2016376, ERROR_TOL));
  AlwaysAssertExit(near(pc(xvec(0)), .2016376, ERROR_TOL));
  AlwaysAssertExit(near(pcb(xvec(0)), .2016376, ERROR_TOL));
  xvec = 7.0;
  AlwaysAssertExit(near(poisson1(xvec(0)), .043208063, ERROR_TOL));
  

  //Available parameters
  PoissonFunction<Double> poisson2(poisson1);
  PoissonFunction<Double> poisson3;
  poisson3 = poisson2;
  cout << "NParameters="<<poisson1.nparameters()<<endl;
  AlwaysAssertExit(poisson1.nparameters() == 2);
  Vector<Double> parms = poisson1.parameters().getParameters();
  AlwaysAssertExit(parms(0) == 3.0);
  AlwaysAssertExit(parms(1) == 2.0);
  AlwaysAssertExit(allEQ(parms, poisson2.parameters().getParameters()) &&
		   allEQ(parms, poisson3.parameters().getParameters()));

  poisson1[0] = 1.0;
  poisson1[1] = 4.0;
  AlwaysAssertExit(poisson1.lambda() == 1.0 );
  AlwaysAssertExit(poisson1.height() == 4.0 );
  
  parms[0] = 11.0;
  parms[1] = 2.0;
  poisson1.parameters().setParameters(parms);
  AlwaysAssertExit(allEQ(poisson1.parameters().getParameters(), parms));
  

  // clone();
  cout << "Original value f(1):     " << poisson1(1.0) << endl;
  AlwaysAssertExit(nearAbs(poisson1(1.0), 0.0003674374, 1e-5))
  Function<Double> *poisson4ptr = poisson1.clone();
  cout << "f.clone(1):              " << (*poisson4ptr)(1.0) << endl;
  AlwaysAssertExit(near(poisson1(1.0), (*poisson4ptr)(1.0)))

  delete poisson4ptr;


  cout << "OK" << endl;
  return 0;
}
