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

    uint32_t nx = 10, ny = 20, nz = 30;
    Cube<float> cube(10, 20, 30);

    Vector<float> fittedParameters;

    // x^2
    Polynomial<AutoDiff<float> > square(2);

    LinearFitSVD<float> fitter;
    fitter.setFunction(square);


    // x axis
    {
        Vector<float> x(nx); indgen((Array<float>&)x); // 0, 1, 2, ...
	Vector<bool> mask(nx);
	mask = true;
        for (uint32_t k=0; k < nz; k++) {
            for (uint32_t j=0; j < ny; j++) {
	        cube.xyPlane(k).column(j) =
		  float(j*k)*((Array<float>&)x)*((Array<float>&)x);
	    }
	}
	ArrayLattice<float> inLattice(cube);
	Cube<float> outCube(nx,ny,nz);
	ArrayLattice<float> outLattice(outCube);
	LatticeFit::fitProfiles (outLattice, fittedParameters, fitter, inLattice, 0, mask,
		    true);
	AlwaysAssertExit(allNearAbs((Array<float>&)outCube, 0.0f, 7.e-3));


	AlwaysAssertExit(near(fittedParameters(2),
			      float((ny-1)*(nz-1)), 1.0e-3));
	LatticeFit::fitProfiles (outLattice, fittedParameters, fitter, inLattice, 0, mask,
		    false);
	AlwaysAssertExit(allNearAbs((Array<float>&)outCube, (Array<float>&)cube, 7.e-3));
	AlwaysAssertExit(near(fittedParameters(2),
			      float((ny-1)*(nz-1)), 1.0e-3));
//crashes
/*
        {
           SubLattice<float>* pOutResid = new SubLattice<float>(outLattice);
           SubLattice<float> inSubLattice(inLattice);
           LatticeFit::fitProfiles (pOutFit, pOutResid, inSubLattice, pSigma, 
                                    fitter, 0, false);
           delete pOutResid;
        }
*/
    }

    // y axis
    {
        Vector<float> x(ny); indgen((Array<float>&)x); // 0, 1, 2, ...
	Vector<bool> mask(ny);
	mask = true;
        for (uint32_t k=0; k < nz; k++) {
            for (uint32_t i=0; i < nx; i++) {
	        cube.xyPlane(k).row(i) =
		  float(i*k)*((Array<float>&)x)*((Array<float>&)x);
	    }
	}
	ArrayLattice<float> inLattice(cube);
	Cube<float> outCube(nx,ny,nz);
	ArrayLattice<float> outLattice(outCube);
	LatticeFit::fitProfiles (outLattice, fittedParameters, fitter, inLattice, 1, mask,
		    true);
	AlwaysAssertExit(allNearAbs((Array<float>&)outCube, 0.0f, 3.e-2));
	AlwaysAssertExit(near(fittedParameters(2),
			      float((nx-1)*(nz-1)), 1.0e-3));
	LatticeFit::fitProfiles (outLattice, fittedParameters, fitter, inLattice, 1, mask,
		    false);
	AlwaysAssertExit(allNearAbs((Array<float>&)outCube, (Array<float>&)cube, 3.e-2));
	AlwaysAssertExit(near(fittedParameters(2),
			      float((nx-1)*(nz-1)), 1.0e-3));
    }

    // z axis
    {
        Vector<float> x(nz); indgen((Array<float>&)x); // 0, 1, 2, ...
	Vector<bool> mask(nz); mask = true;
	for (uint32_t k=0; k < nz; k++) {
	     for (uint32_t j=0; j < ny; j++) {
	       for (uint32_t i=0; i < nx; i++) {
	        cube(i,j,k) = float(i*j)*x(k)*x(k);
	       }
	     }
	}
	ArrayLattice<float> inLattice(cube);
	Cube<float> outCube(nx,ny,nz);
	ArrayLattice<float> outLattice(outCube);
	LatticeFit::fitProfiles (outLattice, fittedParameters, fitter, inLattice, 2, mask,
		    true);
	AlwaysAssertExit(allNearAbs((Array<float>&)outCube, 0.0f, 2.0e-2));
	AlwaysAssertExit(near(fittedParameters(2),
			      float((nx-1)*(ny-1)), 1.0e-3));
	LatticeFit::fitProfiles (outLattice, fittedParameters, fitter, inLattice, 2, mask,
		    false);
	AlwaysAssertExit(allNearAbs((Array<float>&)outCube, (Array<float>&)cube, 2.0e-2));
	AlwaysAssertExit(near(fittedParameters(2),
			      float((nx-1)*(ny-1)), 1.0e-3));
    }


    cout << "OK" << endl;
    return 0;
}
