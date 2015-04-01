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
//#
//# $Id$

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
  Double height = 9.0;
  Double xCen = 0.0;
  Double yCen = 0.0;
  Double zCen = 0.0;
  Double xWidth = 1.0;
  Double yWidth = 1.0;
  Double zWidth = 1.0;
  Double theta = 0.0;
  Double phi = 0.0;

// Specialized (hand-coded derivatives)

  AutoDiff<Double> heightAD(height,9,0);
  AutoDiff<Double> xCenAD(xCen,9,1);
  AutoDiff<Double> yCenAD(yCen,9,2);
  AutoDiff<Double> zCenAD(zCen,9,3);
  AutoDiff<Double> xWidthAD(xWidth,9,4);
  AutoDiff<Double> yWidthAD(yWidth,9,5);
  AutoDiff<Double> zWidthAD(zWidth,9,6);
  AutoDiff<Double> thetaAD(theta,9,7);
  AutoDiff<Double> phiAD(phi,9,8);
  Gaussian3D<AutoDiff<Double> > gauss3dAD(heightAD, xCenAD, yCenAD, zCenAD,
                               xWidthAD, yWidthAD, zWidthAD,
                               thetaAD, phiAD);

// Automatic

  AutoDiffA<Double> heightADA(height,9,0);
  AutoDiffA<Double> xCenADA(xCen,9,1);
  AutoDiffA<Double> yCenADA(yCen,9,2);
  AutoDiffA<Double> zCenADA(zCen,9,3);
  AutoDiffA<Double> xWidthADA(xWidth,9,4);
  AutoDiffA<Double> yWidthADA(yWidth,9,5);
  AutoDiffA<Double> zWidthADA(zWidth,9,6);
  AutoDiffA<Double> thetaADA(theta,9,7);
  AutoDiffA<Double> phiADA(phi,9,8);
  Gaussian3D<AutoDiffA<Double> > gauss3dADA(heightADA, xCenADA, yCenADA, zCenADA,
                                            xWidthADA, yWidthADA, zWidthADA,
                                            thetaADA, phiADA);
//
/*
  uInt npar = argc - 1;
  if (npar > 9) npar = 9;
  for (uInt i = 0; i < npar; i++) {
    gauss3dAD[i] = atof(argv[i+1]);  //set parameter
  }
*/

  for (Double z = -0.25; z < 0.26; z+=0.25)  {
    for (Double y = -1.0; y < 1.01; y+=0.25)    {
      for (Double x = -1.0; x < 1.01; x+=0.25)       {
        cout << "[" << Int(gauss3dAD(x,y,z).value()) << Int(gauss3dADA(x,y,z).value()) << "] ";
        AlwaysAssertExit(near(gauss3dAD(x,y,z).value(),
                              gauss3dADA(x,y,z).value()));
      }
      cout << endl;
    }
    cout << endl;
  }
  cout << endl << endl;

// Check specialized and auto-derivatives

  for (Double z = -0.25; z < 0.26; z+=0.25)  {
    for (Double y = -1.0; y < 1.01; y+=0.25)    {
      for (Double x = -1.0; x < 1.01; x+=0.25)       {
        AlwaysAssertExit(allNearAbs(gauss3dAD(x,y,z).derivatives(),
                                    gauss3dADA(x,y,z).derivatives(), 1.0e-13));
      }
    }
  }
  cout << endl;


  return 0;
}


