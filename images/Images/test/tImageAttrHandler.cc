//# tImageAttrHandler.cc: Test program for tImageAttr classes
//# Copyright (C) 2012
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

#include <images/Images/PagedImage.h>
#include <images/Images/HDF5Image.h>
#include <images/Images/ImageOpener.h>
#include <images/Images/ImageUtilities.h>
#include <coordinates/Coordinates/CoordinateUtil.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/Arrays/ArrayMath.h>
#include <iostream>
#include <casa/Exceptions/Error.h>

using namespace casa;
using namespace std;

void testCreate (ImageInterface<Float>& image)
{
  cout << "testCreate ..." << endl;
  ImageAttrHandler& attrHand (image.attrHandler(True));
  cout << "GOT HANDLER"<<endl;
  cout << attrHand.groupNames()<<endl;
  cout << attrHand.hasGroup("testGroup1")<<endl;
  ImageAttrGroup& group = attrHand.createGroup ("testGroup1");
  cout << "GOT GROUP "<<group.nvalues()<<' '<<group.attrNames()<<endl;
  cout << attrHand.hasGroup("testGroup1")<<endl;
  cout << attrHand.groupNames()<<endl;
  group.putData ("attr1", ValueHolder(Vector<String>(1,"aa")));
  cout << "GOT GROUP "<<group.nvalues()<<' '<<group.attrNames()<<endl;
  cout<<group.attrNames() << endl;
}

void testCreateCasa (const String& imageName)
{
  PagedImage<Float> image(IPosition(2,128,128),
                          CoordinateUtil::defaultCoords2D(),
                          imageName);
  testCreate (image);
}

void testCreateHDF5 (const String& imageName)
{
  HDF5Image<Float> image(IPosition(2,128,128),
                         CoordinateUtil::defaultCoords2D(),
                          imageName);
  testCreate (image);
}

ImageInterface<Float>* doOpen (const String& imageName)
{
  LatticeBase* latt = ImageOpener::openImage (imageName);
  ImageInterface<Float>* image = dynamic_cast<ImageInterface<Float>*>(latt);
  AlwaysAssertExit (image);
  return image;
}

void testRead (const String& imageName)
{
  cout << "testRead ..." << endl;
  ImageInterface<Float>* image = doOpen(imageName);
  ImageAttrHandler& attrHand (image->attrHandler());
  cout << attrHand.groupNames()<<endl;
  ImageAttrGroup& group = attrHand.openGroup ("testGroup1");
  cout << "GOT GROUP "<<group.nvalues()<<' '<<group.attrNames()<<endl;
  delete image;
}

void testUpdate (const String& imageName)
{
  cout << endl << "testUpdate ..." << endl;
  ImageInterface<Float>* image = doOpen(imageName);
  ImageAttrHandler& attrHand (image->attrHandler());
  ImageAttrGroup& group1 = attrHand.openGroup ("testGroup1");
  Array<Int> arr1(IPosition(2,4,1));
  indgen (arr1);
  group1.putData ("attr2", ValueHolder(arr1));
  ImageAttrGroup& group2 = attrHand.createGroup ("testGroup2");
  Array<Int> arr2(IPosition(2,3,4));
  indgen (arr2);
  Vector<String> measInfo(2);
  measInfo[0] = "direction";
  measInfo[1] = "J2000";
  group2.putData ("attr2", ValueHolder(arr2),
                  Vector<String>(1,"rad"), measInfo);
  delete image;
}

void testCopy (const String& nameIn, const String& nameOut, bool hdf5)
{
  cout << endl << "testCopy " << nameIn << " to " << nameOut << endl;
  ImageInterface<Float>* image = doOpen(nameIn);
  ImageInterface<Float>* newImage = 0;
  if (hdf5) {
    cout << ">>> to HDF5<<<" << endl;
    newImage = new HDF5Image<Float>  (image->shape(), image->coordinates(),
                                      nameOut);
  } else {
    cout << ">>> to Casa<<<" << endl;
    newImage = new PagedImage<Float> (image->shape(), image->coordinates(),
                                      nameOut);
  }
  newImage->copyData (*image);
  ImageUtilities::copyMiscellaneous (*newImage, *image);
  delete image;
  delete newImage;
}

void showAll (const String& imageName)
{
  cout << endl << "image = " << imageName << endl;
  ImageInterface<Float>* image = doOpen(imageName);
  ImageAttrHandler& attrHand (image->attrHandler());
  Vector<String> groupNames = attrHand.groupNames();
  for (uInt i=0; i<groupNames.size(); ++i) {
    ImageAttrGroup& group = attrHand.openGroup (groupNames[i]);
    cout << "Attribute group " << groupNames[i] << "  nvalues="
         << group.nvalues() << endl;
    Vector<String> attrNames = group.attrNames();
    for (uInt j=0; j<attrNames.size(); ++j) {
      cout << attrNames[j] << ": "
           << group.getData(attrNames[j]) << "  "
           << group.getUnit(attrNames[j]) << "  "
           << group.getMeasInfo(attrNames[j]) << endl;
    }
  }
  delete image;
}

void testAll (const String& imageName, Bool hasHDF5)
{
  testRead   (imageName);
  showAll    (imageName);
  testUpdate (imageName);
  showAll    (imageName);
  testCopy   (imageName, imageName + "_cp1", false);
  showAll    (imageName + "_cp1");
  testCopy   (imageName, imageName + "_cp2", hasHDF5);
  showAll    (imageName + "_cp2");
}

int main (int argc, char* argv[])
{
  try {
    Bool hasHDF5 = HDF5Object::hasHDF5Support();
    // Test PagedImage.
    cout << endl << ">>> Test Casa image <<<" << endl;
    testCreateCasa ("tImageAttrHandler_tmp.img1");
    testAll ("tImageAttrHandler_tmp.img1", hasHDF5);
    // Test HDF5.
    // If not available, do Pagedmage again (to have correct output)..
    if (hasHDF5) {
      cout << endl << ">>> Test HDF5 image <<<" << endl;
      testCreateHDF5 ("tImageAttrHandler_tmp.img2");
    } else {
      cout << endl << ">>> Test Casa image <<<" << endl;
      testCreateCasa ("tImageAttrHandler_tmp.img2");
    }
    testAll ("tImageAttrHandler_tmp.img2", hasHDF5);
    // If an image is given, show its attributes.
    if (argc > 1) {
      showAll (argv[1]);
      testCopy (argv[1], argv[1] + String("_cp"), True);
      showAll (argv[1] + String("_cp"));
    }
  } catch (std::exception& x) {
    cout << "Uncaught exception: " << x.what() << endl;
    return 1;
  }
  return 0;
}
