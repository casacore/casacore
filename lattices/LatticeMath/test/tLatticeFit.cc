//# tLatticeFit.cc: test the baselineFit function
//# Copyright (C) 1995,1996,1998,1999,2000,2001,2002
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
//# $Id$

#include<casacore/lattices/LatticeMath/LatticeFit.h>

#include<casacore/casa/Arrays.h>
#include<casacore/scimath/Fitting/LinearFitSVD.h>
#include <casacore/scimath/Functionals/Polynomial.h>
#include<casacore/lattices/Lattices/ArrayLattice.h>
#include<casacore/lattices/Lattices/SubLattice.h>
#include<casacore/lattices/Lattices/MaskedLattice.h>
#include<casacore/casa/Utilities/Assert.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main() {

    uInt nx = 10, ny = 20, nz = 30;
    Cube<Float> cube(10, 20, 30);

    Vector<Float> fittedParameters;

    // x^2
    Polynomial<AutoDiff<Float> > square(2);

    LinearFitSVD<Float> fitter;
    fitter.setFunction(square);


    // x axis
    {
        Vector<Float> x(nx); indgen((Array<Float>&)x); // 0, 1, 2, ...
	Vector<Bool> mask(nx);
	mask = True;
        for (uInt k=0; k < nz; k++) {
            for (uInt j=0; j < ny; j++) {
	        cube.xyPlane(k).column(j) =
		  Float(j*k)*((Array<Float>&)x)*((Array<Float>&)x);
	    }
	}
	ArrayLattice<Float> inLattice(cube);
	Cube<Float> outCube(nx,ny,nz);
	ArrayLattice<Float> outLattice(outCube);
	LatticeFit::fitProfiles (outLattice, fittedParameters, fitter, inLattice, 0, mask,
		    True);
	AlwaysAssertExit(allNearAbs((Array<Float>&)outCube, 0.0f, 7.e-3));


	AlwaysAssertExit(near(fittedParameters(2),
			      Float((ny-1)*(nz-1)), 1.0e-3));
	LatticeFit::fitProfiles (outLattice, fittedParameters, fitter, inLattice, 0, mask,
		    False);
	AlwaysAssertExit(allNearAbs((Array<Float>&)outCube, (Array<Float>&)cube, 7.e-3));
	AlwaysAssertExit(near(fittedParameters(2),
			      Float((ny-1)*(nz-1)), 1.0e-3));
//crashes
/*
        {
           SubLattice<Float>* pOutResid = new SubLattice<Float>(outLattice);
           SubLattice<Float> inSubLattice(inLattice);
           LatticeFit::fitProfiles (pOutFit, pOutResid, inSubLattice, pSigma, 
                                    fitter, 0, False);
           delete pOutResid;
        }
*/
    }

    // y axis
    {
        Vector<Float> x(ny); indgen((Array<Float>&)x); // 0, 1, 2, ...
	Vector<Bool> mask(ny);
	mask = True;
        for (uInt k=0; k < nz; k++) {
            for (uInt i=0; i < nx; i++) {
	        cube.xyPlane(k).row(i) =
		  Float(i*k)*((Array<Float>&)x)*((Array<Float>&)x);
	    }
	}
	ArrayLattice<Float> inLattice(cube);
	Cube<Float> outCube(nx,ny,nz);
	ArrayLattice<Float> outLattice(outCube);
	LatticeFit::fitProfiles (outLattice, fittedParameters, fitter, inLattice, 1, mask,
		    True);
	AlwaysAssertExit(allNearAbs((Array<Float>&)outCube, 0.0f, 3.e-2));
	AlwaysAssertExit(near(fittedParameters(2),
			      Float((nx-1)*(nz-1)), 1.0e-3));
	LatticeFit::fitProfiles (outLattice, fittedParameters, fitter, inLattice, 1, mask,
		    False);
	AlwaysAssertExit(allNearAbs((Array<Float>&)outCube, (Array<Float>&)cube, 3.e-2));
	AlwaysAssertExit(near(fittedParameters(2),
			      Float((nx-1)*(nz-1)), 1.0e-3));
    }

    // z axis
    {
        Vector<Float> x(nz); indgen((Array<Float>&)x); // 0, 1, 2, ...
	Vector<Bool> mask(nz); mask = True;
	for (uInt k=0; k < nz; k++) {
	     for (uInt j=0; j < ny; j++) {
	       for (uInt i=0; i < nx; i++) {
	        cube(i,j,k) = Float(i*j)*x(k)*x(k);
	       }
	     }
	}
	ArrayLattice<Float> inLattice(cube);
	Cube<Float> outCube(nx,ny,nz);
	ArrayLattice<Float> outLattice(outCube);
	LatticeFit::fitProfiles (outLattice, fittedParameters, fitter, inLattice, 2, mask,
		    True);
	AlwaysAssertExit(allNearAbs((Array<Float>&)outCube, 0.0f, 2.0e-2));
	AlwaysAssertExit(near(fittedParameters(2),
			      Float((nx-1)*(ny-1)), 1.0e-3));
	LatticeFit::fitProfiles (outLattice, fittedParameters, fitter, inLattice, 2, mask,
		    False);
	AlwaysAssertExit(allNearAbs((Array<Float>&)outCube, (Array<Float>&)cube, 2.0e-2));
	AlwaysAssertExit(near(fittedParameters(2),
			      Float((nx-1)*(ny-1)), 1.0e-3));
    }


    cout << "OK" << endl;
    return 0;
}
