#include <scimath/Functionals/Gaussian3D.h>
#include <casa/Arrays/ArrayLogical.h>
#include <scimath/Mathematics/AutoDiffA.h>
#include <scimath/Mathematics/AutoDiff.h>
#include <scimath/Mathematics/AutoDiffIO.h>
#include <casa/Exceptions.h>

#include <casa/aips.h>
#include <casa/iostream.h>
#include <casa/stdlib.h>


#include <casa/namespace.h>

int main(int argc, const char* argv[])
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


