//# tFitIntoImage.cc:  this tests DOdeconvolver::fitIntoImage
//# Copyright (C) 1996,1997,1999
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

#include <iostream.h>
#include <aips/aips.h>
#include <trial/Images/TempImage.h>
#include <aips/Arrays/Matrix.h>
#include <trial/Images/ImageRegrid.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Arrays/Slicer.h>
#include <trial/Coordinates.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <iostream.h>
#include <iomanip.h>

void fillupTestImage(ImageInterface<Float>& testim);
void showMe(const ImageInterface<Float>& testim);

int main()
{
  CoordinateSystem coordsys0;   // standard!  refPix = 3,3
  CoordinateSystem coordsys1;   // different refVal
  CoordinateSystem coordsys2;   // rpix = 6,6
  CoordinateSystem coordsys3;   // rpix = 1,1
  CoordinateSystem coordsys4;   // rpix = 0,0
  CoordinateSystem coordsys5;   // rpix = 4,3
  CoordinateSystem coordsys6;   // rpix = 100, 100
  CoordinateSystem coordsys7;   // rpix = 6, 8
  {
    Matrix<Double> xform(2,2);                                    
    xform = 0.0; xform.diagonal() = 1.0;                          
    Vector<String> units(2); units = "deg";               
        
    DirectionCoordinate dirCoords0(MDirection::J2000,                  
				  Projection(Projection::SIN),        
				  135*C::pi/180.0, 60*C::pi/180.0,    
				  -1*C::pi/180.0/3600.0, 1*C::pi/180.0/3600.0,				  
				  xform,                              
				  3, 3);  
    dirCoords0.setWorldAxisUnits(units);                               
    
    DirectionCoordinate dirCoords1(MDirection::J2000,                  
				  Projection(Projection::SIN),        
				  135*C::pi/180.0, 61*C::pi/180.0,    
				  -1*C::pi/180.0/3600.0, 1*C::pi/180.0/3600.0,				  
				  xform,                              
				  3, 3);  
    dirCoords1.setWorldAxisUnits(units);                               
    
     DirectionCoordinate dirCoords2(MDirection::J2000,                  
				  Projection(Projection::SIN),        
				  135*C::pi/180.0, 60*C::pi/180.0,    
				  -1*C::pi/180.0/3600.0, 1*C::pi/180.0/3600.0,				  
				  xform,                              
				  6, 6);  
    dirCoords2.setWorldAxisUnits(units);                               
    
    DirectionCoordinate dirCoords3(MDirection::J2000,                  
				  Projection(Projection::SIN),        
				  135*C::pi/180.0, 60*C::pi/180.0,    
				  -1*C::pi/180.0/3600.0, 1*C::pi/180.0/3600.0,				  
				  xform,                              
				  1, 1);  
    dirCoords3.setWorldAxisUnits(units);                               
    
    DirectionCoordinate dirCoords4(MDirection::J2000,                  
				  Projection(Projection::SIN),        
				  135*C::pi/180.0, 60*C::pi/180.0,    
				  -1*C::pi/180.0/3600.0, 1*C::pi/180.0/3600.0,				  
				  xform,                              
				  0,0);  
    dirCoords4.setWorldAxisUnits(units);                               
    
    DirectionCoordinate dirCoords5(MDirection::J2000,                  
				  Projection(Projection::SIN),        
				  135*C::pi/180.0, 60*C::pi/180.0,    
				  -1*C::pi/180.0/3600.0, 1*C::pi/180.0/3600.0,				  
				  xform,                              
				  4, 3);  
    dirCoords5.setWorldAxisUnits(units);                               
    
    DirectionCoordinate dirCoords6(MDirection::J2000,                  
				  Projection(Projection::SIN),        
				  135*C::pi/180.0, 60*C::pi/180.0,    
				  -1*C::pi/180.0/3600.0, 1*C::pi/180.0/3600.0,				  
				  xform,                              
				  100, 100);  
    dirCoords6.setWorldAxisUnits(units);                               
    
    DirectionCoordinate dirCoords7(MDirection::J2000,                  
				  Projection(Projection::SIN),        
				  135*C::pi/180.0, 60*C::pi/180.0,    
				  -1*C::pi/180.0/3600.0, 1*C::pi/180.0/3600.0,				  
				  xform,                              
				  6, 8);  
    dirCoords6.setWorldAxisUnits(units);                               
    
    // StokesCoordinate
    Vector<Int> iquv(1);                                         
    iquv(0) = Stokes::I;
    StokesCoordinate stokesCoords1(iquv);      
    
    // SpectralCoordinate
    SpectralCoordinate spectralCoords1(MFrequency::TOPO,           
				      2000 * 1.0E+6,                 
				      20 * 1.0E+3,                   
				      0,                             
				      2000.40575 * 1.0E+6);          
    units.resize(1);
    units = "Hz";
    spectralCoords1.setWorldAxisUnits(units);
    
    // CoordinateSystems
    coordsys0.addCoordinate(dirCoords0);
    coordsys0.addCoordinate(stokesCoords1);
    coordsys0.addCoordinate(spectralCoords1);      

    coordsys1.addCoordinate(dirCoords1);
    coordsys1.addCoordinate(stokesCoords1);
    coordsys1.addCoordinate(spectralCoords1);
    
    coordsys2.addCoordinate(dirCoords2);
    coordsys2.addCoordinate(stokesCoords1);
    coordsys2.addCoordinate(spectralCoords1);      

    coordsys3.addCoordinate(dirCoords3);
    coordsys3.addCoordinate(stokesCoords1);
    coordsys3.addCoordinate(spectralCoords1);      

    coordsys4.addCoordinate(dirCoords4);
    coordsys4.addCoordinate(stokesCoords1);
    coordsys4.addCoordinate(spectralCoords1);      

    coordsys5.addCoordinate(dirCoords5);
    coordsys5.addCoordinate(stokesCoords1);
    coordsys5.addCoordinate(spectralCoords1);      

    coordsys6.addCoordinate(dirCoords6);
    coordsys6.addCoordinate(stokesCoords1);
    coordsys6.addCoordinate(spectralCoords1);      

    coordsys7.addCoordinate(dirCoords7);
    coordsys7.addCoordinate(stokesCoords1);
    coordsys7.addCoordinate(spectralCoords1);      
  }
    
  TempImage<Float>  testImage(IPosition(4,6,6,1,1), coordsys0, 1);
  fillupTestImage(testImage);
  cout << "Initial Image: " << endl;
  showMe(testImage);

   ImageInterface<Float> *new1;
 /* 
  // This one should return 0: invalid coords
  new1 = deconvolver::fitIntoImage(testImage, IPosition(4,12,12,1,1), coordsys1);
  if (new1 == 0) {
    cout << "Correct behavior for mismatched tangent point" << endl;
  } else {
    cout << "FAILED behavior for mismatched tangent point" << endl;
  }    
  */


  ImageRegrid<Float> regridder(IPosition(4,12,12,1,1),coordsys2);

  // test simple padding
  regridder.setTemplate(IPosition(4,12,12,1,1),coordsys2);
  new1 = regridder.regrid(testImage, True);
  cout << "Padded Image: rpix = 6,6" << endl;
  showMe(*new1);
  delete new1;


  // test offset padding
   regridder.setTemplate(IPosition(4,12,12,1,1),coordsys7);
  new1 = regridder.regrid(testImage, True);
  cout << "Offset Padded Image: rpix=6,8" << endl;
  showMe(*new1);
  delete new1;



  // test subset
  regridder.setTemplate(IPosition(4,4,4,1,1),coordsys3);
  new1 = regridder.regrid(testImage, True);
  cout << "Subset Image: rpix=1,1" << endl;
  showMe(*new1);
  delete new1;



  // test subset
  regridder.setTemplate(IPosition(4,4,4,1,1),coordsys4);
  new1 = regridder.regrid(testImage, True);
  cout << "Mixed 1 Image: rpix = 1,1 " << endl;
  showMe(*new1);
  delete new1;




  // test subset
  regridder.setTemplate(IPosition(4,4,4,1,1),coordsys5);
  new1 = regridder.regrid(testImage, True);
  cout << "Mixed 2 Image: rpix = 4,3" << endl;
  showMe(*new1);
  delete new1;



  // test far off image
  regridder.setTemplate(IPosition(4,4,4,1,1),coordsys6);
  new1 = regridder.regrid(testImage, True);
  cout << "Test far Off Image (should be 0): " << endl;
  showMe(*new1);
  delete new1;


  
  // test simple padding again
  regridder.setTemplate(IPosition(4,12,12,1,1),coordsys2);
  new1 = regridder.regrid(testImage, True);
  cout << "Padded Image Again: rpix = 6,6" << endl;
  showMe(*new1);
  delete new1;


};


void fillupTestImage(ImageInterface<Float>& testim) {
  testim.putAt(1.0, IPosition(4,0,0,0,0));
  testim.putAt(1.0, IPosition(4,1,0,0,0));
  testim.putAt(1.0, IPosition(4,2,0,0,0));
  testim.putAt(1.0, IPosition(4,3,0,0,0));
  testim.putAt(1.0, IPosition(4,4,0,0,0));
  testim.putAt(1.0, IPosition(4,5,0,0,0));
  testim.putAt(1.0, IPosition(4,0,1,0,0));
  testim.putAt(1.0, IPosition(4,0,2,0,0));
  testim.putAt(1.0, IPosition(4,0,3,0,0));
  testim.putAt(1.0, IPosition(4,0,4,0,0));
  testim.putAt(1.0, IPosition(4,0,5,0,0));
  testim.putAt(1.0, IPosition(4,0,5,0,0));
  testim.putAt(1.0, IPosition(4,1,5,0,0));
  testim.putAt(1.0, IPosition(4,2,5,0,0));
  testim.putAt(1.0, IPosition(4,3,5,0,0));
  testim.putAt(1.0, IPosition(4,4,5,0,0));
  testim.putAt(1.0, IPosition(4,5,5,0,0));
  testim.putAt(1.0, IPosition(4,5,1,0,0));
  testim.putAt(1.0, IPosition(4,5,2,0,0));
  testim.putAt(1.0, IPosition(4,5,3,0,0));
  testim.putAt(1.0, IPosition(4,5,4,0,0));
  testim.putAt(1.0, IPosition(4,5,5,0,0));

  testim.putAt(2.0, IPosition(4,1,1,0,0));
  testim.putAt(2.0, IPosition(4,2,1,0,0));
  testim.putAt(2.0, IPosition(4,3,1,0,0));
  testim.putAt(2.0, IPosition(4,4,1,0,0));
  testim.putAt(2.0, IPosition(4,1,4,0,0));
  testim.putAt(2.0, IPosition(4,2,4,0,0));
  testim.putAt(2.0, IPosition(4,3,4,0,0));
  testim.putAt(2.0, IPosition(4,4,4,0,0));
  testim.putAt(2.0, IPosition(4,1,1,0,0));
  testim.putAt(2.0, IPosition(4,1,2,0,0));
  testim.putAt(2.0, IPosition(4,1,3,0,0));
  testim.putAt(2.0, IPosition(4,1,4,0,0));
  testim.putAt(2.0, IPosition(4,4,1,0,0));
  testim.putAt(2.0, IPosition(4,4,2,0,0));
  testim.putAt(2.0, IPosition(4,4,3,0,0));
  testim.putAt(2.0, IPosition(4,4,4,0,0));

  testim.putAt(3.0, IPosition(4,2,2,0,0));
  testim.putAt(3.0, IPosition(4,2,3,0,0));
  testim.putAt(3.0, IPosition(4,3,2,0,0));
  testim.putAt(3.0, IPosition(4,3,3,0,0));
}


void showMe(const ImageInterface<Float>  &testImage) {
  Int ni = testImage.shape()(0);
  Int nj = testImage.shape()(1);
  // OK, we are only looking at the first two dimensions of
  // a 2 or MORE (ie, 4) dimensional lattice, taking the
  // first plane
  IPosition where = testImage.shape() - testImage.shape();  

  Int i;
  Int j;    
  cout << setprecision(2);
  for(j=(nj-1);j>-1;j--) {
    for(i=0;i<ni;i++) {
      where(0) = i;
      where(1) = j;
      cout << " " << testImage.getAt(where);
    }
    cout << endl;
  }
};
