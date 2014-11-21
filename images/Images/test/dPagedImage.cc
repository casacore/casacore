//# tPagedImage.cc: This program tests the PagedImage class
//# Copyright (C) 1994,1995,1996,1999,2000,2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or(at your option)
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
//#--------------------------------------------------------------------------

#include<casacore/casa/aips.h>
#include <casacore/images/Images/PagedImage.h>
///#include <trial/ImgCrdSys/ImageCoordinate.h>

#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/tables/Tables.h>
#include <casacore/casa/BasicSL/String.h>

#include <casacore/casa/stdlib.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

int main()
{
    cout << "untested" << endl;
}

/*
//#predeclarations
void getArguments(int argc, char **argv, Bool &verbose);
Bool fudgedEquality(Float a, Float b);
void deleteFile(const String &filename);
ImageCoordinate build3Dcoords();
void createStandardImageOnDisk(const String &filename);
void describeImage(const String &message, const PagedImage<Float> &image);
void testConstructors();
void testSetMemberFunctions();
void testArrayofImagesAndAssignmentOperator();

//# define mega global variables
Bool verbose_;
const Float TEST_PIXEL_VALUE = 99.99;
const String STANDARD = "image-standard";
const String NAME0 = "image-test-file_0";
const String NAME1 = "image-test-file_1";
const String NAME2 = "image-test-file_2";
const String NAME3 = "image-test-file_3";
const String NAME4 = "image-test-file_4";
const String NAME5 = "image-test-file_5";
const String NAME6 = "image-test-file_6";
const String NAME7 = "image-test-file_7";
const String NAME8 = "image-test-file_8";
const String NAME9 = "image-test-file_9";

int main(int argc, const char* argv[])
{
  getArguments(argc, argv, verbose_);
  createStandardImageOnDisk(STANDARD);
  try {
    testConstructors();
    testSetMemberFunctions();
    testArrayofImagesAndAssignmentOperator();
    deleteFile(STANDARD);
    cout << "OK" << endl;
  } catch (AipsError x) {
    cerr << "Caught Exception: " << x.getMesg() << endl;
  } 
  return 0;
}

// fill debug variable
void getArguments(int argc, char **argv, Bool &verbose)
{
  verbose = False;
  for(uInt i=0; i< argc; i++)
    if(!strcmp(argv [i], "-v"))
      verbose = True;
}

// equality function for floats
Bool fudgedEquality(Float a, Float b)
{
  Float absA = fabs(a);
  Float absB = fabs(b);
  Float delta = fabs(absA - absB);
  Float margin;
  if(absA > absB)
     margin = absA / 1.0e5;
  else
     margin = absB / 1.0e5;
  if(delta > margin) {
    return False;
    }
  else {
    return True;
  }
}

// function which deletes files on disk
void deleteFile(const String &filename)
{
  String command = "rm -rf ";
  command += filename;
  system(command);
}

// function to print Image data
void describeImage(const String &message, const PagedImage<Float> &image)
{
  cout << message << " shape: "<< image.shape() << endl;
// *
  cout << "coordinates, axisNames: "
       << image.coordinates().axisNames() << endl;
  cout << "             referencePixels: "
       << image.coordinates().referencePixels() << endl;
  cout << "             referenceValues: "
       << image.coordinates().referenceValues()<< endl;
  cout << "             deltas: "
       << image.coordinates().deltas()<< endl;
  cout << "             shape: "
       << image.coordinates().imageShape() << endl;
// * /
  cout << "             ok: "<< image.ok()<< endl; 
}

// build a set of ImageCoordinates in three dimensions
ImageCoordinate build3Dcoords()
{
  // we need to know a projection method - here it is Global Sinusoid
  ProjectedPosition::Type myMethod(ProjectedPosition::GLS);
  // we need to know what form the coordinates are in - here it is RA & Dec
  SkyPosition::Type myVectorType(SkyPosition::EQUATORIAL);
  // we need to know the epoch of the coordinates
  SkyPosition::Epoch myEpoch(SkyPosition::J2000);
  // we need to know the vector itself.
  Vector<Double> myCoords(2);
  myCoords(0) = 122.35;
  myCoords(1) = -33.7764;
  // we need to know the position of the observer.
  // Let's create an EarthPosition with full description of all parameters.
  // We need a type - GEOCENTRIC seems good.
  EarthPosition::Type theType(EarthPosition::GEOCENTRIC);
  // We need the time and date of the observation... 
  Double julianDate = 2449376.0;
  // and we can add on the UT.
  julianDate += 16.52/24.0;
  // we need the coordinates of our position.
  Vector<Double> ourPosition(3);
  // geocentric longitude (in degrees) goes in the first field of the vector.
  ourPosition(0) = 107.2334666;
  // geocentric latitude (in degrees) goes in the second field.
  ourPosition(1) = 34.1562394;
  // geocentric radius (in meters) goes in the last field.
  ourPosition(2) = 6372139.592;
  // then use these to build our EarthPosition. 
  EarthPosition myObs(theType, julianDate, ourPosition);
  // we need to know a rotation - here it is zero.
  Double myRot = 0;
  // we need to know where the spherical position is to be on our 2-d 
  // projection i.e. what pixel is associated with my object's position?
  Vector<Double> thePixel(2);
  thePixel(0) = 55.0;
  thePixel(1) = 526.3;
  // finally, we need to know the number of spherical units per integer on
  // our 2-d projection (i.e. binning per pixel).
  Vector<Double> theBinning(2);
  theBinning(0) = 3.04e-03;
  theBinning(1) = 3.6255e-03;
  // Now we can make fruit of our labor - the ProjectedPosition itself.
  ProjectedPosition myMapping(myMethod, myVectorType, myEpoch, myCoords,
			      myObs, myRot, thePixel, theBinning);
  
  // we need to know what the units are of the measured value
  MeasuredValue::Type myValueUnit(MeasuredValue::RADIO_VELOCITY);
  // we need to know what the above units are in reference 
  // here it is the velocity of the earth.
  ReferenceValue myRefValue(ReferenceValue::VELOCITY, 9.56e+03);
  // we need to know the value of the measurement
  Double myValue(9.5688823e+03);
  // we need to know the binning per "pixel"
  Double myBin(5.63e-04);
  // we need to know the position on the "number line" of our value
  Double myValuePos(27.3);
  // Now we may construct the LinearAxis itself.
  LinearAxis myLinearAxis(myValueUnit,myValue,myRefValue,myBin,myValuePos);
  
  ImageCoordinate coords;
  coords.addAxis(myMapping);
  coords.addAxis(myLinearAxis);
  return coords;
}

// build an Image and store in a file on disk
void createStandardImageOnDisk(const String &filename)
{
  if(verbose_) cout<< "-- createStandardImageOnDisk --"<< endl;
  
  ImageCoordinate coords(build3Dcoords());
  IPosition imageShape(3, 50,47,31);
  
  PagedImage<Float> standard(imageShape, coords, filename);
  standard.set(TEST_PIXEL_VALUE);
  
  if(verbose_) describeImage("standard image", standard);
}

// these are the constructors to test:
//   PagedImage(const IPosition &shape, const MinimalCoords &coordinateInfo,
//              const String &nameOfNewFile, uInt rowNumber);
//   PagedImage(const IPosition &shape, const Array<T> &array, 
//              const MinimalCoords &coordinateInfo,
//              const String &nameOfNewFile, uInt rowNumber);
//   PagedImage(const String &filename, uInt rowNumber);
//   PagedImage(const PagedImage<T> &other);
//
void testConstructors()
{
  if (verbose_) cout << "-- testConstructors --" << endl;
  {
    IPosition shape(3,10,10,4);
    ImageCoordinate coords(build3Dcoords());

    PagedImage<Float> image1(shape, coords, NAME0);
    if (verbose_) 
      describeImage("shape, coords, filename ctor: ", image1);

    PagedImage<Float> image2(shape, coords, NAME1, True);
			     
    if (verbose_) 
      describeImage("array, array, coords, filename, masking? ctor: ", image2);

    PagedImage<Float> image3(STANDARD);
    if (verbose_) describeImage("old file ctor: ", image3);
    if (image3(IPosition(3,5,4,3)) != TEST_PIXEL_VALUE)
      throw(AipsError("image3, file-constructed image, wrong pixel value"));
    if (!image3.ok())
      throw(AipsError("file-constructed image not ok"));

    PagedImage<Float> *image4 = new PagedImage<Float>(image3);
    if(verbose_)
      describeImage("Copy ctor: ", *image4);
    if((*image4)(IPosition(3,5,4,3)) != TEST_PIXEL_VALUE)
      throw(AipsError("image4, file assigned image, wrong pixel value"));
    if(!image4->ok())
      throw(AipsError("file-constructed image not ok"));
    delete image4;
    }
  
  deleteFile(NAME0);
  deleteFile(NAME1);
} 

// test some of the data member manipulation functions
void testSetMemberFunctions()
{
  if(verbose_) cout<< "-- test set member functions --"<< endl;
  {
    ImageCoordinate coords;
    {
      PagedImage<Float> image6(IPosition(3,10,10,4), coords, NAME2);
      
      if(verbose_) describeImage("shape, coords, filename ctor: ", image6);
      if(image6.ok())
	throw(AipsError("shape-constructed image6 is ok, but shouldn't be!"));
    }

    coords = build3Dcoords();
    PagedImage<Float> image6(IPosition(3,10,10,4), coords, NAME2);
    if(!image6.ok())
      throw(AipsError("shape-constructed image6 - is not ok!"));			     
    image6.rename(NAME2);
    if(verbose_) describeImage("image6, after rename", image6);
    if(!image6.ok())
      throw(AipsError("image6 after rename - not ok"));
    
    image6.setCoordinateInfo(build3Dcoords());
    if(verbose_) describeImage("image6, after setCoordinates", image6);
    if(!image6.ok())
      throw(AipsError("image6 after setCoordinates - not ok"));
  }
  
  deleteFile(NAME2);
} 

//
void testArrayofImagesAndAssignmentOperator()
{
  if(verbose_) cout<< "-- test array of images --"<< endl;
// *
  const uInt max = 10;
  String filenames [max];
  filenames [0] = NAME0;
  filenames [1] = NAME1;
  filenames [2] = NAME2;
  filenames [3] = NAME3;
  filenames [4] = NAME4;
  filenames [5] = NAME5;
  filenames [6] = NAME6;
  filenames [7] = NAME7;
  filenames [8] = NAME8;
  filenames [9] = NAME9;
  { 
    Image<Float> images [max];
    for(uInt i=0; i< max; i++) {
      if(verbose_) cout<< "-- image array "<< i<< " --"<< endl;
      if(i%2) {
        images [i] = Image<Float>(IPosition(2,10,10));
        images [i].setCoordinateInfo(MinimalCoords());
        images [i].setName(filenames [i]);
        if(verbose_) 
          describeImage("shape constructed image, setCoords, setName in array",
                         images [i]);
        } // if i is odd
      else {
        images [i] =
          Image<Float>(IPosition(i+1,5), MinimalCoords(), filenames [i]);
        if(verbose_) 
          describeImage("shape,coords,name constructed image in array,",
                         images [i]);
        } // else
      if(!images[i].ok())
        throw(AipsError("one of array of images not ok"));
      } // for i
    } // scope
  
  if(verbose_) cout<< "-- about to delete image files --"<< endl;

  for(uInt i=0; i< max; i++)
    deleteFile(filenames [i]);
//  * /
}
*/
