//# tImageDecomposer.cc: 
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002
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

#include <casa/iostream.h>
#include <casa/stdlib.h>
#include <casa/math.h>
#include <casa/aips.h>

#include <images/Images/ImageInterface.h>

#include <lattices/Lattices/TiledShape.h>
#include <images/Images/TempImage.h>
#include <coordinates/Coordinates/LinearCoordinate.h>
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <coordinates/Coordinates/CoordinateUtil.h>
#include <scimath/Functionals/Gaussian2D.h> 
#include <scimath/Functionals/Gaussian3D.h>
#include <images/Images/ImageDecomposer.h>
#include <casa/OS/Timer.h>

#include <casa/namespace.h>
typedef Double imagetype;

TempImage<imagetype> createtestimage(const Matrix<imagetype>& components,
				     const Vector<imagetype>& originworldcoord,
				     imagetype inc, const IPosition& imageshape);
void printGaussianComponents(const Matrix<imagetype>& components);
Bool compareParameters(const Matrix<imagetype>& given, 
                       const Matrix<imagetype>& calculated);


//////////////////////////////////////////

int main(int argc, const char* argv[])
{  
    //Tests the ImageDecomposer class

  try
  {
    TempImage<imagetype> image;
    Matrix<imagetype> givenpars;
    IPosition imageshape;
    Vector<imagetype> origincoord;
    Timer timer; 


    //basic test
    cout << endl;
    cout << "Basic test (no contour decomposition)" << endl;
    cout << "-------------------------------------" << endl;

    givenpars.resize(1,6);
    givenpars(0,0) = 1; givenpars(0,1) = 0;   givenpars(0,2) = 0;
    givenpars(0,3) = 3; givenpars(0,4) = 0.9; givenpars(0,5) = 1;
    imageshape = IPosition(2,5,5);
    origincoord.resize(2);
    origincoord(0) = -2; origincoord(1) = -2;
    image = createtestimage(givenpars, origincoord, 1, imageshape);

    timer.mark();
    ImageDecomposer<imagetype> id(image);
    id.setDeblend(False);
    id.decomposeImage();
    cout << "(No component map generated without deblender.)" << endl;
    cout << "Given parameters: " << endl;
    printGaussianComponents(givenpars);
    cout << "Output parameters: " << endl;
    printGaussianComponents(id.componentList());
    if (compareParameters(givenpars,id.componentList())) return 1;
    cout << "Processing time: " << timer.real() << " s" << endl;
    

    // deblender test
    // -------------------------------

    cout << endl;
    cout << "Deblender test" << endl;
    cout << "--------------" << endl;

    givenpars.resize(4,6);
    givenpars(0,0) = 15;  givenpars(0,1) = -3;  givenpars(0,2) = -3;
    givenpars(0,3) = 1.6; givenpars(0,4) = 0.7; givenpars(0,5) = 0.9;
    givenpars(1,0) = 6;   givenpars(1,1) = -1;  givenpars(1,2) = 3;
    givenpars(1,3) = 2.0; givenpars(1,4) = 0.9; givenpars(1,5) = 0.6;
    givenpars(2,0) = 9;   givenpars(2,1) = 1;   givenpars(2,2) = 0.0;
    givenpars(2,3) = 2.2; givenpars(2,4) = 0.9; givenpars(2,5) = 1;
    givenpars(3,0) = 9;   givenpars(3,1) = 3;   givenpars(3,2) = 2.4;
    givenpars(3,3) = 1.9; givenpars(3,4) = 0.8; givenpars(3,5) = 0.3;

    imageshape = IPosition(2,9,9);
    origincoord.resize(2);
    origincoord(0) = -4; origincoord(1) = -4;
 
    TempImage<imagetype> image2;
    image2 = createtestimage(givenpars, origincoord, 1, imageshape);

    timer.mark();
    id.setImage(image2);
    id.setDeblend(True);
    id.setDeblendOptions(1.0, 7);
    id.decomposeImage();
    id.display();
    cout << "Given parameters: " << endl;
    printGaussianComponents(givenpars);
    cout << "Output parameters: " << endl;
    printGaussianComponents(id.componentList());
    if (compareParameters(givenpars,id.componentList())) return 1;
    cout << "Processing time: " << timer.real() << " s" << endl;



    //overlapping squares test
    cout << endl;
    cout << "Overlapping region squares test" << endl;
    cout << "-------------------------------" << endl;

    givenpars.resize(4,6);
    givenpars(0,0) = 10;  givenpars(0,1) = -2.7; givenpars(0,2) = 0.5;
    givenpars(0,3) = 3.5; givenpars(0,4) = 0.4;  givenpars(0,5) = 0.7;
    givenpars(1,0) = 9;   givenpars(1,1) = 1;    givenpars(1,2) = -2.6;
    givenpars(1,3) = 3.1; givenpars(1,4) = 0.35; givenpars(1,5) = 0.8;
    givenpars(2,0) = 11;  givenpars(2,1) = 0.2;  givenpars(2,2) = 3;
    givenpars(2,3) = 3.3; givenpars(2,4) = 0.4;  givenpars(2,5) = 0.6;
    givenpars(3,0) = 9;   givenpars(3,1) = 3;    givenpars(3,2) = 0.7;
    givenpars(3,3) = 3.2; givenpars(3,4) = 0.45; givenpars(3,5) = 0.75;

    imageshape = IPosition(2,9,9);
    origincoord.resize(2);
    origincoord(0) = -4; origincoord(1) = -4;
 
    TempImage<imagetype> image3;
    image3 = createtestimage(givenpars, origincoord, 1, imageshape);

    timer.mark();
    id.setImage(image3);
    id.setDeblendOptions(1.0, 10);
    id.decomposeImage();
    id.display();
    cout << "Given parameters: " << endl;
    printGaussianComponents(givenpars);
    cout << "Output parameters: " << endl;
    printGaussianComponents(id.componentList());
    if (compareParameters(givenpars,id.componentList())) return 1;
    cout << "Processing time: " << timer.real() << " s" << endl;

    //3D complex test
    cout << endl;
    cout << "3D highly blended test" << endl;
    cout << "----------------------" << endl;

    givenpars.resize(6,9);

    givenpars(0,0) = 8.5;  givenpars(0,7) = 0.2;  givenpars(0,8) = 0.25;
    givenpars(0,1) = 1;    givenpars(0,2) = -3;   givenpars(0,3) = 1;  
    givenpars(0,4) = 2;    givenpars(0,5) = 1.9;  givenpars(0,6) = 1.7;  

    givenpars(1,0) = 10;   givenpars(1,7) = 0.2;  givenpars(1,8) = 0.1;
    givenpars(1,1) = 1.0;  givenpars(1,2) = -0.5; givenpars(1,3) = 0.2;  
    givenpars(1,4) = 2.8;  givenpars(1,5) = 3.2;  givenpars(1,6) = 2.1;  

    givenpars(2,0) = 6;    givenpars(2,7) = -0.7; givenpars(2,8) = 0.3;
    givenpars(2,1) = -2.1; givenpars(2,2) = -0.7; givenpars(2,3) = 1.6;  
    givenpars(2,4) = 2.3;  givenpars(2,5) = 2.6;  givenpars(2,6) = 2.5;  
 
    givenpars(3,0) = 7.5;  givenpars(3,7) = -0.5; givenpars(3,8) = 0.05;
    givenpars(3,1) = -0.5; givenpars(3,2) = -2.9; givenpars(3,3) = -1.8;  
    givenpars(3,4) = 2.8;  givenpars(3,5) = 3.8;  givenpars(3,6) = 1.5; 

    givenpars(4,0) = 7;    givenpars(4,7) = 0.5;  givenpars(4,8) = -0.1;
    givenpars(4,1) = -3;   givenpars(4,2) = 2;    givenpars(4,3) = -1;  
    givenpars(4,4) = 1.6;  givenpars(4,5) = 2.4;  givenpars(4,6) = 1.8;  

    givenpars(5,0) = 8;    givenpars(5,7) = -0.6; givenpars(5,8) = 0.2;
    givenpars(5,1) = 1.5;  givenpars(5,2) = 3;    givenpars(5,3) = 2;  
    givenpars(5,4) = 2.3;  givenpars(5,5) = 1.7;  givenpars(5,6) = 1.2;  

    imageshape.resize(3);
    imageshape = IPosition(3,9,9,5);
    origincoord.resize(3);
    origincoord(0) = -4; origincoord(1) = -4; origincoord(2) = -2;

    TempImage<imagetype> image4;
    image4 = createtestimage(givenpars, origincoord, 1, imageshape);

    timer.mark();
    id.setImage(image4);
    id.setDeblendOptions(1.0, 16, 1);
    id.setFitOptions(0.05, -1, 128);
    id.decomposeImage();
    id.display();
    cout << "Given parameters: " << endl;
    printGaussianComponents(givenpars);
    cout << "Output parameters: " << endl;
    printGaussianComponents(id.componentList());
    if (compareParameters(givenpars,id.componentList())) return 1;
    cout << "Processing time: " << timer.real() << " s" << endl;

    //Noise test??

  }
  catch (AipsError err)
  {
    cout << err.getMesg() << endl;
    cout << "Unexpected failure!" << endl;
    return 1;
  }

  cout << "OK" << endl;

  return 0;
}

////////////////////////////

TempImage<Double> createtestimage(const Matrix<Double>& components,
				  const Vector<Double>& originworldcoord,
				  Double inc,
				  const IPosition& imageshape)
{
  //forms test image out of a composite gaussian function
  
  TempImage<Double> image;

  uInt dim = imageshape.nelements();

  if (dim == 2)
  {
    image = TempImage<Double>(TiledShape(imageshape),
			      CoordinateUtil::defaultCoords2D(), 1);
  }
  else if (dim == 3)
  {
    image = TempImage<Double>(TiledShape(imageshape),
			      CoordinateUtil::defaultCoords3D(), 1);
  }
  else 
  {
    return TempImage<Double>();      //return empty image
  }


  Block<Gaussian2D<Double> > datagauss2d((dim==2) * components.nrow());
  Block<Gaussian3D<Double> > datagauss3d((dim==3) * components.nrow());

  for (uInt g = 0; g < components.nrow(); g++)
    for (uInt p = 0; p < components.ncolumn(); p++)
    {
      //cout << g << ',' << p << '=' << components(g,p) << endl;
      if (dim==2) datagauss2d[g][p] = components(g,p);
      if (dim==3) datagauss3d[g][p] = components(g,p);
    }
  
  Vector<Double> cdelt(1);     cdelt = inc;
  Matrix<Double> pc(1,1);      pc = 1.0;
  Vector<Double> crpix(1);     crpix = 0.0;
  Vector<String> emptystrv(1); emptystrv = "";
  Vector<Double> xminv(1);     xminv = originworldcoord(0);
  Vector<Double> yminv(1);     yminv = originworldcoord(1);
  Vector<Double> zminv(1);     if (dim == 3) zminv = originworldcoord(2);

  IPosition pos(dim);
  Double x = originworldcoord(0);
  for (Int xi = 0; xi < imageshape(0); xi++)
  {
    Double y = originworldcoord(1);
    for (Int yi = 0; yi < imageshape(1); yi++)
    {  
      if (dim==2) 
      {
        Double f = 0;
        for (uInt g = 0; g < components.nrow(); g++)
          f += datagauss2d[g](x,y);
        image.putAt(f, IPosition(2,xi,yi));
      }
      else
      {
        Double z = originworldcoord(2);
        for (Int zi = 0; zi < imageshape(2); zi++)
        {
          Double f = 0;
          for (uInt g = 0; g < components.nrow(); g++)
            f += datagauss3d[g](x,y,z);
          image.putAt(f, IPosition(3,xi,yi,zi));
          z+= inc;
        }
      }      
      y += inc;
    }    
    x += inc;
  }

  LinearCoordinate xcoord(emptystrv, emptystrv, xminv, cdelt, pc, crpix);
  LinearCoordinate ycoord(emptystrv, emptystrv, yminv, cdelt, pc, crpix);
  LinearCoordinate zcoord(emptystrv, emptystrv, zminv, cdelt, pc, crpix);
  CoordinateSystem csystem;
  csystem.addCoordinate(xcoord);
  csystem.addCoordinate(ycoord);
  if (dim == 3) csystem.addCoordinate(zcoord);
  image.setCoordinateInfo(csystem);  

  return image;
}


void printGaussianComponents(const Matrix<imagetype>& components)
{
  cout.precision(2);  
  for (uInt g = 0; g < components.nrow(); g++)
  {
    cout << g+1 << ": ";
    if (components(g,0) == 0) {
       cout << "Failed"; 
    } else { 
      cout << "Peak: " << components(g,0) << "  ";

      if (components.ncolumn() == 6) {
        cout << "Mu: [" << components(g,1) 
             << ", " << components(g,2) << "]  ";
        cout << "Axes: [" << components(g,3)
             << ", " << components(g,3) * components(g,4) << "]  ";
        cout << "Rotation: " << components(g,5) /*  * 180 / C::pi  */;
      }
      if (components.ncolumn() == 9) {
        cout << "Mu: [" << components(g,1) 
             << ", " << components(g,2) 
             << ", " << components(g,3) << "]  ";
        cout << "Axes: [" << components(g,4)
             << ", " << components(g,5) 
             << ", " << components(g,6) << "]  ";
        cout << "Rotation: [" << components(g,7)/*  *180/C::pi */
             << ", " << components(g,8)         /*  *180/C::pi */ << "]";
      }
    }
    cout << endl;
  }
  cout.precision(4);
   
  return;
}

Bool compareParameters(const Matrix<imagetype>& given, 
                       const Matrix<imagetype>& calculated)
{
  imagetype error=0;
  uInt dim = given.ncolumn() / 3;

  if (given.nrow() != calculated.nrow())
  {
    cout << "Wrong number of components." << endl;
    return 1;
  }

  for (uInt g = 0; g < given.nrow(); g++)
  {
    error += 5 * abs(1 - calculated(g,0) / given(g,0));
    error += abs(calculated(g,1) - given(g,1));
    error += abs(calculated(g,2) - given(g,2));
    if (dim == 2)
    {
       error += 2 * abs(1 - calculated(g,3) / given(g,3));
       error += 5 * abs(1 - calculated(g,4) / given(g,4));
       error += 5 * abs(calculated(g,5) - given(g,5));
    }
    if (dim == 3)
    { 
       error += abs(calculated(g,3) - given(g,3));
       error += abs(1 - calculated(g,4) / given(g,4));
       error += abs(1 - calculated(g,5) / given(g,5));
       error += abs(1 - calculated(g,6) / given(g,6));
       error += 3 * abs(calculated(g,7) - given(g,7));
       error += 3 * abs(calculated(g,8) - given(g,8));
    }
  }
 
  cout << "Total error: " << error << endl;
  if (error > 0.15 * given.nrow())
  { 
    cout << "Error is excessive." << endl;  
    return 1;
  }
  return 0;

}
