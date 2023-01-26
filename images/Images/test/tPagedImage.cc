//# tPagedImage.cc:  test the PagedImage class
//# Copyright (C) 1994,1995,1998,1999,2000,2001,2002
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

#include <casacore/images/Images/PagedImage.h>
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
#include <casacore/tables/Tables/TableUtil.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Regex.h>

#include <casacore/casa/stdlib.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>


// Remove the dirname from the table name in an error message.
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

Table makeScrTable(const String& name)
{
   SetupNewTable setup(name, TableDesc(), Table::Scratch);
   Table table(setup);
   return table;
}

Table makeNewTable(const String& name)
{
   SetupNewTable setup(name, TableDesc(), Table::New);
   Table table(setup);
   return table;
}

void testTempCloseDelete()
{
  IPosition shape(2,32,64);
  CoordinateSystem cSys = CoordinateUtil::defaultCoords2D();
  // Check image gets deleted if marked for delete.
  {
    TiledShape tiledShape(shape);
    PagedImage<float> img (tiledShape, cSys, "tPagedImage_tmp.imgtc");
    img.putAt(float(1.0), IPosition(2,1,1));
    img.table().markForDelete();
  }
  // Check image gets deleted if marked for delete,
  // even after tempClose.
  AlwaysAssertExit (! File("tPagedImage_tmp.imgtc").exists());
  {
    IPosition shape(2,32,64);
    TiledShape tiledShape(shape);
    PagedImage<float> img (tiledShape, cSys, "tPagedImage_tmp.imgtc");
    img.putAt(float(1.0), IPosition(2,1,1));
    img.table().markForDelete();
    img.tempClose();
  }
  AlwaysAssertExit (! File("tPagedImage_tmp.imgtc").exists());
  // Check image gets deleted if marked for delete,
  // even after tempClose and reopen.
  {
    IPosition shape(2,32,64);
    TiledShape tiledShape(shape);
    PagedImage<float> img (tiledShape, cSys, "tPagedImage_tmp.imgtc");
    img.putAt(float(1.0), IPosition(2,1,1));
    img.table().markForDelete();
    img.tempClose();
    img.putAt(float(1.0), IPosition(2,0,0));
  }
  AlwaysAssertExit (! File("tPagedImage_tmp.imgtc").exists());
  // Check image does not get deleted if first marked for delete,
  // but after tempClose and reopen is unmarked for delete.
  {
    IPosition shape(2,32,64);
    TiledShape tiledShape(shape);
    PagedImage<float> img (tiledShape, cSys, "tPagedImage_tmp.imgtc");
    img.putAt(float(1.0), IPosition(2,1,1));
    img.table().markForDelete();
    img.tempClose();
    img.putAt(float(1.0), IPosition(2,0,0));
    img.table().unmarkForDelete();
  }
  AlwaysAssertExit (File("tPagedImage_tmp.imgtc").exists());
}

int main()
{
  try {

// Build things to make a PagedImage

    CoordinateSystem cSys = CoordinateUtil::defaultCoords2D();
    IPosition shape(2,32,64);
    TiledShape tiledShape(shape);

// Test constructors

    {
       Table table = makeScrTable(String("tPagedImage_tmp.img1"));
       PagedImage<float> pIm(tiledShape, cSys, table);
    }
    {
       PagedImage<float> pIm(tiledShape, cSys, 
                             String("tPagedImage_tmp.img2"));
    }
    {
       PagedImage<float> pIm(tiledShape, cSys, 
                             String("tPagedImage_tmp.img3"),
                             TableLock(TableLock::AutoLocking));
    }
    TableUtil::deleteTable(String("tPagedImage_tmp.img3"));
    {
       Table table = makeScrTable(String("tPagedImage_tmp.img4"));
       PagedImage<float> pIm(tiledShape, cSys, table);
       PagedImage<float> pIm2(table);
    }
    {
       Table table = makeNewTable(String("tPagedImage_tmp.img5"));
       PagedImage<float> pIm(tiledShape, cSys, table);
    }
    {
       PagedImage<float> pIm(String("tPagedImage_tmp.img5"));
    }
    {
       PagedImage<float> pIm(String("tPagedImage_tmp.img5"),
                             TableLock(TableLock::AutoLocking));
    }
//
// Test copy constructor.  This is by reference so test that
// this is so
//
    {
       PagedImage<float> pIm(String("tPagedImage_tmp.img5"));
       PagedImage<float> pIm2(pIm);
       pIm.tempClose();
       pIm.putAt(float(1.0), IPosition(2,0,0));
       AlwaysAssert( (pIm(IPosition(2,0,0))==float(1.0)), AipsError);
       pIm2.putAt(float(10.0), IPosition(2,0,0));
       AlwaysAssert((pIm2(IPosition(2,0,0))==float(10.0)), AipsError);
       AlwaysAssert((pIm(IPosition(2,0,0))==float(10.0)), AipsError);
//
       AlwaysAssert(pIm(IPosition(2,0,0))==pIm.getAt(IPosition(2,0,0)),
                    AipsError);
    }
//
// Test assignment.  This is by reference so test that
// this is so
//
    {
       PagedImage<float> pIm(String("tPagedImage_tmp.img5"));
       PagedImage<float> pIm2(pIm);
       pIm2 = pIm;
       pIm.putAt(float(1.0), IPosition(2,0,0));
       AlwaysAssert( (pIm(IPosition(2,0,0))==float(1.0)), AipsError);
       pIm2.putAt(float(10.0), IPosition(2,0,0));
       AlwaysAssert((pIm2.getAt(IPosition(2,0,0))==float(10.0)), AipsError);
       AlwaysAssert((pIm.getAt(IPosition(2,0,0))==float(10.0)), AipsError);
//
       AlwaysAssert(pIm(IPosition(2,0,0))==pIm.getAt(IPosition(2,0,0)),
                    AipsError);
    }

//
// Test clones.  There is not much we can do to make sure they are ok !
// They are by reference too.
//
    {
       PagedImage<float> pIm(String("tPagedImage_tmp.img5"));
       pIm.putAt(float(1.0), IPosition(2,0,0));
       AlwaysAssert( (pIm(IPosition(2,0,0))==float(1.0)), AipsError);
//
       Lattice<float>* lat = pIm.clone();
       AlwaysAssert(pIm.shape()==lat->shape(), AipsError);
//
       lat->putAt(float(10.0), IPosition(2,0,0));
       AlwaysAssert(((*lat).getAt(IPosition(2,0,0))==float(10.0)), AipsError);
       AlwaysAssert((pIm.getAt(IPosition(2,0,0))==float(10.0)), AipsError);
       delete lat;
    }
    {
       PagedImage<float> pIm(String("tPagedImage_tmp.img5"));
       pIm.putAt(float(1.0), IPosition(2,0,0));
       AlwaysAssert( (pIm.getAt(IPosition(2,0,0))==float(1.0)), AipsError);
//
       Lattice<float>* ii = pIm.cloneII();
       AlwaysAssert(pIm.shape()==ii->shape(), AipsError);

//
       ii->putAt(float(10.0), IPosition(2,0,0));
       AlwaysAssert(((*ii).getAt(IPosition(2,0,0))==float(10.0)), AipsError);
       AlwaysAssert((pIm.getAt(IPosition(2,0,0))==float(10.0)), AipsError);
       delete ii;
    }
//
// Test some miscellaneous little things
//
    {
       PagedImage<float> pIm(String("tPagedImage_tmp.img5"));
//
       AlwaysAssert(imagePixelType(String("tPagedImage_tmp.img5"))==TpFloat,   // Global function
                    AipsError);
       AlwaysAssert(pIm.name(true)==String("tPagedImage_tmp.img5"),
                    AipsError);
       cout << "Absolute name = " << pIm.name(false) << endl;
       AlwaysAssert(pIm.isPaged(), AipsError);
       AlwaysAssert(pIm.isWritable(), AipsError);
       AlwaysAssert(pIm.ok(), AipsError);
//
       pIm.rename(String("tPagedImage_tmp.img6"));
       AlwaysAssert(pIm.name(true)==String("tPagedImage_tmp.img6"),
                    AipsError);
       pIm.rename(String("tPagedImage_tmp.img5"));
       AlwaysAssert(pIm.name(true)==String("tPagedImage_tmp.img5"),
                    AipsError);
//
       AlwaysAssert(pIm.rowNumber()==0, AipsError);
       AlwaysAssert(pIm.shape()==shape, AipsError);
       IPosition niceCursorShape = pIm.niceCursorShape();
//       cout << "niceCursorShape=" << niceCursorShape << endl;

// Only true for small images

       AlwaysAssert(niceCursorShape==shape, AipsError);
//
       IPosition shape2(2,10,20);
       pIm.resize(shape2);
       IPosition shape0(3,5,10,20);
       bool ok = false;
       try {
          pIm.resize(shape0);
       } catch (std::exception& x) {
//          cout << "Caught error " << x.what() << endl;
          ok = true;
       } 
       if (!ok) {
          throw(AipsError("Resize did not fail. This was unexpected"));
       }
//
       pIm.tempClose();
       Unit units("Jy");
       pIm.setUnits(units);
       AlwaysAssert(pIm.units().getName()=="Jy", AipsError);
//
       TableRecord rec;
       rec.define("x", double(1.0));
       rec.define("y", double(2.0));
       pIm.tempClose();
       pIm.setMiscInfo(rec);
       pIm.tempClose();
       TableRecord rec2 = pIm.miscInfo();
       AlwaysAssert(rec2.nfields()==2, AipsError);
       AlwaysAssert(rec2.isDefined("x"), AipsError);
       AlwaysAssert(rec2.isDefined("y"), AipsError);
       AlwaysAssert(rec2.dataType("x")==TpDouble, AipsError);
       AlwaysAssert(rec2.dataType("y")==TpDouble, AipsError);
       AlwaysAssert(rec2.asDouble("x")==double(1.0), AipsError);
       AlwaysAssert(rec2.asDouble("y")==double(2.0), AipsError);
//
//       cout << "Cache size = " << pIm.maximumCacheSize() << endl;
       pIm.setCacheSizeFromPath(shape, IPosition(2,0,0),
                                shape, IPosition(2,0,1));
//       cout << "Cache size = " << pIm.maximumCacheSize() << endl;
       pIm.setMaximumCacheSize(100);
       AlwaysAssert(pIm.maximumCacheSize()==100, AipsError);
//       cout << "Cache size = " << pIm.maximumCacheSize() << endl;
//
       pIm.clearCache();
       pIm.showCacheStatistics(cout);
//
       CoordinateSystem cSys2 = pIm.coordinates();
       Vector<String> axisUnits = cSys2.worldAxisUnits();
       axisUnits(0) = "deg"; axisUnits(1) = "deg";
       cSys2.setWorldAxisUnits(axisUnits);
       pIm.setCoordinateInfo(cSys2);
       CoordinateSystem cSys3 = pIm.coordinates();
       AlwaysAssert(cSys2.near(cSys3,1e-6), AipsError);
//
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
     TableUtil::deleteTable(String("tPagedImage_tmp.img5"));
//
// do{Put,Get}Slice tests
//
    {
       Table table = makeScrTable(String("tPagedImage_tmp.img7"));
       IPosition shape2(2,5, 10);
       TiledShape tiledShape2(shape2);
       CoordinateSystem cSys2 = CoordinateUtil::defaultCoords2D();
       PagedImage<float> pIm(tiledShape2, cSys2, table);
//
// Fill (slowly so use small image)
//
       IPosition pos(2);
       for (int32_t i=0; i<shape2(0); i++) {
          for (int32_t j=0; j<shape2(1); j++) {
             pos(0) = i;
             pos(1) = j;
             pIm.putAt(float(i*j), pos);
          }  
       }
//
       Slicer slice(IPosition(2,0,0), shape2, IPosition(2,1,1));
       Array<float> data;
       pIm.doGetSlice(data, slice);
       AlwaysAssert(data.shape()==shape2, AipsError);
//
       for (int32_t i=0; i<shape2(0); i++) {
          for (int32_t j=0; j<shape2(1); j++) {
             pos(0) = i;
             pos(1) = j;
             AlwaysAssert(data(pos)==pIm.getAt(pos), AipsError);
          }  
       }
//
       data*float(20.0);
       pIm.doPutSlice(data, IPosition(2,0,0), IPosition(2,1,1));
       for (int32_t i=0; i<shape2(0); i++) {
          for (int32_t j=0; j<shape2(1); j++) {
             pos(0) = i;
             pos(1) = j;
             AlwaysAssert(data(pos)==pIm.getAt(pos), AipsError);
          }  
       }
//
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
//
// Silly Lattice addition operator
//
    {
       Table table = makeScrTable(String("tPagedImage_tmp.img8"));
       IPosition shape2(2,5, 10);
       TiledShape tiledShape2(shape2);
       CoordinateSystem cSys2 = CoordinateUtil::defaultCoords2D();
       PagedImage<float> pIm(tiledShape2, cSys2, table);
//
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
//
       ArrayLattice<float> lat(shape2);
       lat.set(float(10.0));
       pIm += lat;
       Array<float> data2(data.copy());
       data2 += float(10.0);
//
       Slicer slice(IPosition(2,0,0), shape2, IPosition(2,1,1));
       Array<float> data3;
       pIm.doGetSlice(data3, slice);
//
       AlwaysAssert(allNear(data3, data2, 1e-6), AipsError);
   }
//
// apply functions
//
   {
       Table table = makeScrTable(String("tPagedImage_tmp.img9"));
       IPosition shape2(2,5, 10);
       TiledShape tiledShape2(shape2);
       CoordinateSystem cSys2 = CoordinateUtil::defaultCoords2D();
       PagedImage<float> pIm(tiledShape2, cSys2, table);
       pIm.set(3.0);
       AlwaysAssert(allEQ(pIm.get(), float(3.0)), AipsError);
//
// 2 * x * x
//
       pIm.apply(&func);
       AlwaysAssert(allNear(pIm.get(), float(18.0), double(1e-6)), AipsError);
//
// 3 * x (const arg)
//
       pIm.set(3.0);
       AlwaysAssert(allEQ(pIm.get(), float(3.0)), AipsError);
       pIm.apply(&const_arg_func);
       AlwaysAssert(allNear(pIm.get(), float(9.0), double(1e-6)), AipsError);
//
// Polynomial 1 + 2x + 3x**2
//
       pIm.set(3.0);
       AlwaysAssert(allEQ(pIm.get(), float(3.0)), AipsError);
//
       Polynomial<float> poly(3);
       poly.setCoefficient(1, 1.0);
       poly.setCoefficient(2, 2.0);
       poly.setCoefficient(3, 3.0);
       pIm.apply(poly);
       AlwaysAssert(allNear(pIm.get(), poly(3.0), double(1e-6)), AipsError);
   }
//
// test table function.  I don't really know what else to do with it.
//
   {
       Table table = makeScrTable(String("tPagedImage_tmp.img11"));
       IPosition shape2(2, 5, 10);
       TiledShape tiledShape2(shape2);
       CoordinateSystem cSys2 = CoordinateUtil::defaultCoords2D();
       PagedImage<float> pIm(tiledShape2, cSys2, table);
       pIm.set(1.0);
//
       Table t = pIm.table();
       AlwaysAssert(removeDir(t.tableName()) == String("tPagedImage_tmp.img11"),
                    AipsError);

   }
//
// Do some iterating to test the makeIter function (indirectly)
//
   {
       Table table = makeScrTable(String("tPagedImage_tmp.img10"));
       IPosition shape2(2, 128, 256);
       TiledShape tiledShape2(shape2);
       CoordinateSystem cSys2 = CoordinateUtil::defaultCoords2D();
       PagedImage<float> pIm(tiledShape2, cSys2, table);
       pIm.set(1.0);
//
       LatticeIterator<float> it(pIm);
       while (!it.atEnd()) {
          AlwaysAssert(allEQ(it.cursor(), float(1.0)), AipsError);
          it++;
       }
   }
   {
	   // per plane beam support
	   String name = "tPagedImage_tmp_afsdf.im";
	   PagedImage<float> temp(
			   TiledShape(IPosition(4, 64 ,64, 4, 16)),
			   CoordinateUtil::defaultCoords4D(), name
	   );
	   ImageInfo info = temp.imageInfo();
	   Quantity maj(5, "arcsec");
	   Quantity min(3, "arcsec");
	   Quantity pa(30, "deg");
	   info.setAllBeams(16, 4, GaussianBeam());
	   bool ok = true;
	   try {
		   temp.setImageInfo(info);
		   ok = false;
	   }
	   catch (std::exception& x) {
		   cout << "Exception thrown as expected: " << x.what() << endl;
	   }
	   AlwaysAssert(ok, AipsError);
	   info.setBeam(0, 0, maj, min, pa);
	   try {
		   temp.setImageInfo(info);
		   ok = false;
	   }
	   catch (std::exception& x) {}
	   AlwaysAssert(ok, AipsError);
	   for (uint32_t i=0; i<4; i++) {
		   for (uint32_t j=0; j<16; j++) {
			   info.setBeam(j, i, maj, min, pa);
		   }
	   }
	   AlwaysAssert(temp.setImageInfo(info), AipsError);
           GaussianBeam beam2 = temp.imageInfo().restoringBeam(2,2);
	   ok = true;
	   try {
             GaussianBeam beam = temp.imageInfo().restoringBeam();
             ok = false;
	   }
	   catch (std::exception& x) {
             cout << "Exception thrown as expected: " << x.what() << endl;
	   }
	   AlwaysAssert(ok, AipsError);
	   // AlwaysAssert(beam.size() == 0, AipsError);
	   AlwaysAssert(temp.imageInfo().hasMultipleBeams(), AipsError);
	   min = Quantity(min.getValue() + 0.1, min.getUnit());
	   info.setBeam(2, 2, maj, min, pa);
	   AlwaysAssert(temp.setImageInfo(info), AipsError);
	   AlwaysAssert(temp.imageInfo().hasMultipleBeams(), AipsError);
	   GaussianBeam beam = temp.imageInfo().restoringBeam(2, 2);
	   AlwaysAssert(beam.getMajor() == maj, AipsError);
	   AlwaysAssert(beam.getMinor() == min, AipsError);
	   AlwaysAssert(beam.getPA() == pa, AipsError);
   }

   // Test the temporary close if marked for delete.
   testTempCloseDelete();

   cout<< "ok"<< endl;
  } catch (std::exception& x) {
    cerr << "Exception caught: " << x.what() << endl;
    return 1;
  } 

  return 0;



}
