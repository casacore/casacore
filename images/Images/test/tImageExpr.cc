//# tImageExpr.cc: This program tests the ImageExpr class
//# Copyright (C) 2014
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


#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/images/Images/ImageExpr.h>
#include <casacore/images/Images/ImageExprParse.h>
#include <casacore/images/Images/PagedImage.h>
#include <casacore/images/Images/ImageOpener.h>

#include <casacore/casa/iostream.h>
#include <casacore/casa/namespace.h>

void testExpr()
{
  IPosition shape(2,5,10);
  Array<float> arr(shape);
  indgen (arr);
  {
    // Create the images.
    PagedImage<float> im1(shape, CoordinateUtil::defaultCoords2D(),
                          "tImageExpr_tmp.img1");
    im1.put (arr);
    PagedImage<float> im2(shape, CoordinateUtil::defaultCoords2D(),
                          "tImageExpr_tmp.img2");
    im2.put (arr - float(5));
  }
  {
    // Form an expression and save it.
    String expr("'tImageExpr_tmp\\.img1' + tImageExpr_tmp.img2 + 5");
    LatticeExprNode node(ImageExprParse::command(expr));
    ImageExpr<float> img (node, expr);
    AlwaysAssertExit (allEQ(img.get(), arr+arr));
    AlwaysAssertExit (! img.isPersistent());
    TableRecord rec;
    rec.define ("key", "value");
    img.setMiscInfo (rec);
    img.save ("tImageExpr_tmp.imgexpr");
    AlwaysAssertExit (img.isPersistent());
    AlwaysAssertExit (ImageExprParse::getImageNames().size() == 2  &&
                      ImageExprParse::getImageNames()[0] == "tImageExpr_tmp.img1" &&
                      ImageExprParse::getImageNames()[1] == "tImageExpr_tmp.img2");
  }
  {
    // Reopen the expression from the file.
    LatticeBase* latt = ImageOpener::openImageExpr ("tImageExpr_tmp.imgexpr");
    ImageExpr<float>* img = dynamic_cast<ImageExpr<float>*>(latt);
    AlwaysAssertExit (img != 0);
    AlwaysAssertExit (allEQ(img->get(), arr+arr));
    AlwaysAssertExit (img->isPersistent());
    AlwaysAssertExit (ImageExprParse::getImageNames().size() == 2  &&
                      ImageExprParse::getImageNames()[0] == "tImageExpr_tmp.img1" &&
                      ImageExprParse::getImageNames()[1] == "tImageExpr_tmp.img2");
    AlwaysAssertExit (img->miscInfo().asString("key") == "value");
    delete img;
  }
  {
    // Do a recursive test.
    // Form an expression and save it.
    String expr("tImageExpr_tmp.imgexpr + tImageExpr_tmp.img1");
    LatticeExprNode node(ImageExprParse::command(expr));
    ImageExpr<float> img (node, expr);
    AlwaysAssertExit (allEQ(img.get(), arr+arr+arr));
    AlwaysAssertExit (! img.isPersistent());
    img.save ("tImageExpr_tmp:imgexpr2");
    AlwaysAssertExit (img.isPersistent());
  }
  {
    // Reopen the 2nd expression from the file.
    LatticeBase* latt = ImageOpener::openImageExpr ("tImageExpr_tmp:imgexpr2");
    ImageExpr<float>* img = dynamic_cast<ImageExpr<float>*>(latt);
    AlwaysAssertExit (img != 0);
    AlwaysAssertExit (allEQ(img->get(), arr+arr+arr));
    AlwaysAssertExit (img->isPersistent());
    delete img;
    AlwaysAssertExit (ImageExprParse::getImageNames().size() == 2  &&
                      ImageExprParse::getImageNames()[0] == "tImageExpr_tmp.imgexpr" &&
                      ImageExprParse::getImageNames()[1] == "tImageExpr_tmp.img1");
  }
  Block<LatticeExprNode> nodes;
  {
    cout<<"try as expr"<<endl;
    // Reopen as an expression from the file. Escape the colon.
    LatticeBase* latt = ImageOpener::openExpr ("float('tImageExpr_tmp\\:imgexpr2')", nodes);
    ImageExpr<float>* img = dynamic_cast<ImageExpr<float>*>(latt);
    AlwaysAssertExit (img != 0);
    AlwaysAssertExit (allEQ(img->get(), arr+arr+arr));
    AlwaysAssertExit (! img->isPersistent());
    delete img;
    cout<<"done try as expr"<<endl;
  }
  // Try to open it as an expression without escaping the colon (also as unquoted).
  // It should give an exception 'unknown image'.
  bool exc = false;
  try {
    cout<<"try as expr2"<<endl;
    ImageOpener::openExpr ("float('tImageExpr_tmp:imgexpr2' + 1)", nodes);
  } catch (const std::exception& x) {
    cout << "Expected exception: " << x.what() << endl;
    exc = true;
  }
  AlwaysAssertExit (exc);
  exc = false;
  try {
    cout<<"try as expr3"<<endl;
    ImageOpener::openExpr ("float(tImageExpr_tmp:imgexpr2)", nodes);
  } catch (const std::exception& x) {
    cout << "Expected exception: " << x.what() << endl;
    exc = true;
  }
  AlwaysAssertExit (exc);
}


int main() {
  try {
    testExpr();
  } catch (const std::exception& x) {
    cerr << x.what() << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;
}
