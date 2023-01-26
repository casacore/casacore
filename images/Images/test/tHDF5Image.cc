//# tHDF5Image.cc:  test the HDF5Image class
//# Copyright (C) 2008
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

#include <casacore/images/Images/HDF5Image.h>
#include <casacore/images/Images/ImageInfo.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/scimath/Functionals/Polynomial.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Quanta/QLogical.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Regex.h>

#include <casacore/casa/stdlib.h>
#include <casacore/casa/iostream.h>

using namespace casacore;


// Remove the dirname from the file name in an error message.
String removeDir (const String& msg)
{
  String s = msg;
  s.gsub (Regex("/.*/t"), "t");
  return s;
}

float const_arg_func(const float& val)
{
  return 3.0*val;
}

float func(float val)
{
  return 2.0*val*val;
}


int main()
{
  // Exit with untested if no HDF5 support.
  if (! HDF5Object::hasHDF5Support()) {
    return 3;
  }
  try {

    // Build things to make a HDF5Image
    CoordinateSystem cSys = CoordinateUtil::defaultCoords2D();
    IPosition shape(2,32,64);
    TiledShape tiledShape(shape);

    // Test constructors
    {
      HDF5Image<float> pIm(tiledShape, cSys, "tHDF5Image_tmp.img1");
    }
    AlwaysAssertExit (isHDF5Image("tHDF5Image_tmp.img1"));
    AlwaysAssertExit (hdf5imagePixelType("tHDF5Image_tmp.img1") == TpFloat);
    {
      HDF5Image<float> pIm2("tHDF5Image_tmp.img1");
    }

    // Test copy constructor.  This is by reference so test that
    // this is so
    {
      HDF5Image<float> pIm(String("tHDF5Image_tmp.img1"));
      HDF5Image<float> pIm2(pIm);
      pIm.putAt(float(1.0), IPosition(2,0,0));
      AlwaysAssert( (pIm(IPosition(2,0,0))==float(1.0)), AipsError);
      pIm2.putAt(float(10.0), IPosition(2,0,0));
      AlwaysAssert((pIm2(IPosition(2,0,0))==float(10.0)), AipsError);
      AlwaysAssert((pIm(IPosition(2,0,0))==float(10.0)), AipsError);
      AlwaysAssert(pIm(IPosition(2,0,0))==pIm.getAt(IPosition(2,0,0)),
		   AipsError);
    }

    // Test assignment.  This is by reference so test that
    // this is so
    {
      HDF5Image<float> pIm(String("tHDF5Image_tmp.img1"));
      HDF5Image<float> pIm2(pIm);
      pIm2 = pIm;
      pIm.putAt(float(1.0), IPosition(2,0,0));
      AlwaysAssert( (pIm(IPosition(2,0,0))==float(1.0)), AipsError);
      pIm2.putAt(float(10.0), IPosition(2,0,0));
      AlwaysAssert((pIm2.getAt(IPosition(2,0,0))==float(10.0)), AipsError);
      AlwaysAssert((pIm.getAt(IPosition(2,0,0))==float(10.0)), AipsError);
      AlwaysAssert(pIm(IPosition(2,0,0))==pIm.getAt(IPosition(2,0,0)),
		   AipsError);
    }

    // Test clones.  There is not much we can do to make sure they are ok !
    // They are by reference too.
    {
      HDF5Image<float> pIm(String("tHDF5Image_tmp.img1"));
      pIm.putAt(float(1.0), IPosition(2,0,0));
      AlwaysAssert( (pIm(IPosition(2,0,0))==float(1.0)), AipsError);
      Lattice<float>* lat = pIm.clone();
      AlwaysAssert(pIm.shape()==lat->shape(), AipsError);
      lat->putAt(float(10.0), IPosition(2,0,0));
      AlwaysAssert(((*lat).getAt(IPosition(2,0,0))==float(10.0)), AipsError);
      AlwaysAssert((pIm.getAt(IPosition(2,0,0))==float(10.0)), AipsError);
      delete lat;
    }
    {
      HDF5Image<float> pIm(String("tHDF5Image_tmp.img1"));
      pIm.putAt(float(1.0), IPosition(2,0,0));
      AlwaysAssert( (pIm.getAt(IPosition(2,0,0))==float(1.0)), AipsError);
      Lattice<float>* ii = pIm.cloneII();
      AlwaysAssert(pIm.shape()==ii->shape(), AipsError);
      ii->putAt(float(10.0), IPosition(2,0,0));
      AlwaysAssert(((*ii).getAt(IPosition(2,0,0))==float(10.0)), AipsError);
      AlwaysAssert((pIm.getAt(IPosition(2,0,0))==float(10.0)), AipsError);
      delete ii;
    }

    // Test some miscellaneous little things
    {
      HDF5Image<float> pIm(String("tHDF5Image_tmp.img1"));
      AlwaysAssert(hdf5imagePixelType(String("tHDF5Image_tmp.img1"))==TpFloat,
		   AipsError);
      AlwaysAssert(pIm.name(true)==String("tHDF5Image_tmp.img1"),
		   AipsError);
      cout << "Absolute name = " << pIm.name(false) << endl;
      AlwaysAssert(pIm.isPaged(), AipsError);
      AlwaysAssert(pIm.isWritable(), AipsError);
      AlwaysAssert(pIm.ok(), AipsError);
      AlwaysAssert(pIm.shape()==shape, AipsError);
      IPosition niceCursorShape = pIm.niceCursorShape();
      // Only true for small images
      AlwaysAssert(niceCursorShape==shape, AipsError);
      Unit units("Jy");
      pIm.setUnits(units);
      AlwaysAssert(pIm.units().getName()=="Jy", AipsError);

      Record rec;
      rec.define("x", double(1.0));
      rec.define("y", double(2.0));
      pIm.setMiscInfo(rec);
      TableRecord rec2 = pIm.miscInfo();
      AlwaysAssert(rec2.nfields()==2, AipsError);
      AlwaysAssert(rec2.isDefined("x"), AipsError);
      AlwaysAssert(rec2.isDefined("y"), AipsError);
      AlwaysAssert(rec2.dataType("x")==TpDouble, AipsError);
      AlwaysAssert(rec2.dataType("y")==TpDouble, AipsError);
      AlwaysAssert(rec2.asDouble("x")==double(1.0), AipsError);
      AlwaysAssert(rec2.asDouble("y")==double(2.0), AipsError);

      CoordinateSystem cSys2 = pIm.coordinates();
      Vector<String> axisUnits = cSys2.worldAxisUnits();
      axisUnits(0) = "deg"; axisUnits(1) = "deg";
      cSys2.setWorldAxisUnits(axisUnits);
      pIm.setCoordinateInfo(cSys2);
      CoordinateSystem cSys3 = pIm.coordinates();
      AlwaysAssert(cSys2.near(cSys3,1e-6), AipsError);

      ImageInfo info = pIm.imageInfo();
      AlwaysAssert(info.restoringBeam().isNull(), AipsError);
      Quantity a1(10.0,Unit("arcsec"));
      Quantity a2(8.0,Unit("arcsec"));
      Quantity a3(-45.0,Unit("deg"));
      info.setRestoringBeam(GaussianBeam(a1, a2, a3));
      pIm.setImageInfo(info);
      info = pIm.imageInfo();
      AlwaysAssert(info.restoringBeam().getMajor()==a1, AipsError);
      AlwaysAssert(info.restoringBeam().getMinor()==a2, AipsError);
      AlwaysAssert(info.restoringBeam().getPA()==a3, AipsError);
    }

    // do{Put,Get}Slice tests
    {
      IPosition shape2(2,5, 10);
      TiledShape tiledShape2(shape2);
      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords2D();
      HDF5Image<float> pIm(tiledShape2, cSys2, "tHDF5Image_tmp.img7");

      // Fill (in slow way, so use small image)
      IPosition pos(2);
      for (int32_t i=0; i<shape2(0); i++) {
	for (int32_t j=0; j<shape2(1); j++) {
	  pos(0) = i;
	  pos(1) = j;
	  pIm.putAt(float(i*j), pos);
	}  
      }

      Slicer slice(IPosition(2,0,0), shape2, IPosition(2,1,1));
      Array<float> data;
      pIm.doGetSlice(data, slice);
      AlwaysAssert(data.shape()==shape2, AipsError);

      for (int32_t i=0; i<shape2(0); i++) {
	for (int32_t j=0; j<shape2(1); j++) {
	  pos(0) = i;
	  pos(1) = j;
	  AlwaysAssert(data(pos)==pIm.getAt(pos), AipsError);
	}  
      }

      data*float(20.0);
      pIm.doPutSlice(data, IPosition(2,0,0), IPosition(2,1,1));
      for (int32_t i=0; i<shape2(0); i++) {
	for (int32_t j=0; j<shape2(1); j++) {
	  pos(0) = i;
	  pos(1) = j;
	  AlwaysAssert(data(pos)==pIm.getAt(pos), AipsError);
	}  
      }

      Array<float> data2(data.copy());
      data.resize(IPosition(2,2,2));
      data.set(0.0);
      pIm.doPutSlice(data, IPosition(2,0,0), IPosition(2,1,1));
      pos(0) = 0; pos(1) = 0;
      AlwaysAssert(pIm(IPosition(2,0,0))==float(0.0), AipsError);
      AlwaysAssert(pIm(IPosition(2,0,1))==float(0.0), AipsError);
      AlwaysAssert(pIm(IPosition(2,1,0))==float(0.0), AipsError);
      AlwaysAssert(pIm(IPosition(2,1,1))==float(0.0), AipsError);
      for (int32_t i=2; i<shape2(0); i++) {
	for (int32_t j=2; j<shape2(1); j++) {
	  pos(0) = i;
	  pos(1) = j;
	  AlwaysAssert(data2(pos)==pIm.getAt(pos), AipsError);
	}  
      }
    }

    // Silly Lattice addition operator
    {
      IPosition shape2(2,5, 10);
      TiledShape tiledShape2(shape2);
      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords2D();
      HDF5Image<float> pIm(tiledShape2, cSys2, "tHDF5Image_tmp.img8");

      IPosition pos(2);
      Array<float> data(shape2);
      for (int32_t i=0; i<shape2(0); i++) {
	for (int32_t j=0; j<shape2(1); j++) {
	  pos(0) = i;
	  pos(1) = j;
	  data(pos) = float(i*j);
	}  
      }
      pIm.doPutSlice(data, IPosition(2,0,0), IPosition(2,1,1));

      ArrayLattice<float> lat(shape2);
      lat.set(float(10.0));
      pIm += lat;
      Array<float> data2(data.copy());
      data2 += float(10.0);

      Slicer slice(IPosition(2,0,0), shape2, IPosition(2,1,1));
      Array<float> data3;
      pIm.doGetSlice(data3, slice);
      AlwaysAssert(allNear(data3, data2, 1e-6), AipsError);
    }

    // apply functions
    {
      IPosition shape2(2,5, 10);
      TiledShape tiledShape2(shape2);
      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords2D();
      HDF5Image<float> pIm(tiledShape2, cSys2, "tHDF5Image_tmp.img9");
      pIm.set(3.0);
      AlwaysAssert(allEQ(pIm.get(), float(3.0)), AipsError);
      // 2 * x * x
      pIm.apply(&func);
      AlwaysAssert(allNear(pIm.get(), float(18.0), double(1e-6)), AipsError);
      // 3 * x (const arg)
      pIm.set(3.0);
      AlwaysAssert(allEQ(pIm.get(), float(3.0)), AipsError);
      pIm.apply(&const_arg_func);
      AlwaysAssert(allNear(pIm.get(), float(9.0), double(1e-6)), AipsError);
      // Polynomial 1 + 2x + 3x**2
      pIm.set(3.0);
      AlwaysAssert(allEQ(pIm.get(), float(3.0)), AipsError);

      Polynomial<float> poly(3);
      poly.setCoefficient(1, 1.0);
      poly.setCoefficient(2, 2.0);
      poly.setCoefficient(3, 3.0);
      pIm.apply(poly);
      AlwaysAssert(allNear(pIm.get(), poly(3.0), double(1e-6)), AipsError);
    }

    // Do some iterating to test the makeIter function (indirectly)
    {
      IPosition shape2(2, 128, 256);
      TiledShape tiledShape2(shape2);
      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords2D();
      HDF5Image<float> pIm(tiledShape2, cSys2, "tHDF5Image_tmp.img10");
      pIm.set(1.0);
      LatticeIterator<float> it(pIm);
      while (!it.atEnd()) {
	AlwaysAssert(allEQ(it.cursor(), float(1.0)), AipsError);
	it++;
      }
    }

    cout<< "ok"<< endl;
  } catch (std::exception& x) {
    cerr << "Exception caught: " << x.what() << endl;
    return 1;
  } 

  return 0;
}
