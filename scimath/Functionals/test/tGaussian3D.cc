//# tGaussian3D.cc: Test program for class Gaussian3D
//# Copyright (C) 2000,2001,2003
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
//# License for more details.
//#
//# You should have received a copy of the GNU General Public License
//# along with this program; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/scimath/Functionals/Gaussian3D.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/scimath/Mathematics/AutoDiffA.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffIO.h>
#include <casacore/casa/Exceptions.h>

#include <casacore/casa/aips.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/stdlib.h>


#include <casacore/casa/namespace.h>

int main()
{
  double height = 9.0;
  double xCen = 0.0;
  double yCen = 0.0;
  double zCen = 0.0;
  double xWidth = 1.0;
  double yWidth = 1.0;
  double zWidth = 1.0;
  double theta = 0.0;
  double phi = 0.0;

// Specialized (hand-coded derivatives)

  AutoDiff<double> heightAD(height,9,0);
  AutoDiff<double> xCenAD(xCen,9,1);
  AutoDiff<double> yCenAD(yCen,9,2);
  AutoDiff<double> zCenAD(zCen,9,3);
  AutoDiff<double> xWidthAD(xWidth,9,4);
  AutoDiff<double> yWidthAD(yWidth,9,5);
  AutoDiff<double> zWidthAD(zWidth,9,6);
  AutoDiff<double> thetaAD(theta,9,7);
  AutoDiff<double> phiAD(phi,9,8);
  Gaussian3D<AutoDiff<double> > gauss3dAD(heightAD, xCenAD, yCenAD, zCenAD,
                               xWidthAD, yWidthAD, zWidthAD,
                               thetaAD, phiAD);

// Automatic

  AutoDiffA<double> heightADA(height,9,0);
  AutoDiffA<double> xCenADA(xCen,9,1);
  AutoDiffA<double> yCenADA(yCen,9,2);
  AutoDiffA<double> zCenADA(zCen,9,3);
  AutoDiffA<double> xWidthADA(xWidth,9,4);
  AutoDiffA<double> yWidthADA(yWidth,9,5);
  AutoDiffA<double> zWidthADA(zWidth,9,6);
  AutoDiffA<double> thetaADA(theta,9,7);
  AutoDiffA<double> phiADA(phi,9,8);
  Gaussian3D<AutoDiffA<double> > gauss3dADA(heightADA, xCenADA, yCenADA, zCenADA,
                                            xWidthADA, yWidthADA, zWidthADA,
                                            thetaADA, phiADA);
//
/*
  uint32_t npar = argc - 1;
  if (npar > 9) npar = 9;
  for (uint32_t i = 0; i < npar; i++) {
    gauss3dAD[i] = atof(argv[i+1]);  //set parameter
  }
*/

  for (double z = -0.25; z < 0.26; z+=0.25)  {
    for (double y = -1.0; y < 1.01; y+=0.25)    {
      for (double x = -1.0; x < 1.01; x+=0.25)       {
        cout << "[" << int32_t(gauss3dAD(x,y,z).value()) << int32_t(gauss3dADA(x,y,z).value()) << "] ";
        AlwaysAssertExit(near(gauss3dAD(x,y,z).value(),
                              gauss3dADA(x,y,z).value()));
      }
      cout << endl;
    }
    cout << endl;
  }
  cout << endl << endl;

// Check specialized and auto-derivatives

  for (double z = -0.25; z < 0.26; z+=0.25)  {
    for (double y = -1.0; y < 1.01; y+=0.25)    {
      for (double x = -1.0; x < 1.01; x+=0.25)       {
        AlwaysAssertExit(allNearAbs(gauss3dAD(x,y,z).derivatives(),
                                    gauss3dADA(x,y,z).derivatives(), 1.0e-13));
      }
    }
  }
  cout << endl;


  return 0;
}


