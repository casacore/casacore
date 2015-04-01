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
//#
//# $Id$

#include <casacore/images/Images/PagedImage.h>
#include <casacore/images/Images/ImageInfo.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayIO.h>
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

#include <casacore/casa/namespace.h>


// Remove the dirname from the table name in an error message.
String removeDir (const String& msg)
{
  String s = msg;
  s.gsub (Regex("/.*/t"), "t");
  return s;
}

Float const_arg_func(const Float& val)
{
  return 3.0*val;
}

Float func(Float val)
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
    PagedImage<Float> img (tiledShape, cSys, "tPagedImage_tmp.imgtc");
    img.putAt(Float(1.0), IPosition(2,1,1));
    img.table().markForDelete();
  }
  // Check image gets deleted if marked for delete,
  // even after tempClose.
  AlwaysAssertExit (! File("tPagedImage_tmp.imgtc").exists());
  {
    IPosition shape(2,32,64);
    TiledShape tiledShape(shape);
    PagedImage<Float> img (tiledShape, cSys, "tPagedImage_tmp.imgtc");
    img.putAt(Float(1.0), IPosition(2,1,1));
    img.table().markForDelete();
    img.tempClose();
  }
  AlwaysAssertExit (! File("tPagedImage_tmp.imgtc").exists());
  // Check image gets deleted if marked for delete,
  // even after tempClose and reopen.
  {
    IPosition shape(2,32,64);
    TiledShape tiledShape(shape);
    PagedImage<Float> img (tiledShape, cSys, "tPagedImage_tmp.imgtc");
    img.putAt(Float(1.0), IPosition(2,1,1));
    img.table().markForDelete();
    img.tempClose();
    img.putAt(Float(1.0), IPosition(2,0,0));
  }
  AlwaysAssertExit (! File("tPagedImage_tmp.imgtc").exists());
  // Check image does not get deleted if first marked for delete,
  // but after tempClose and reopen is unmarked for delete.
  {
    IPosition shape(2,32,64);
    TiledShape tiledShape(shape);
    PagedImage<Float> img (tiledShape, cSys, "tPagedImage_tmp.imgtc");
    img.putAt(Float(1.0), IPosition(2,1,1));
    img.table().markForDelete();
    img.tempClose();
    img.putAt(Float(1.0), IPosition(2,0,0));
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
       PagedImage<Float> pIm(tiledShape, cSys, table);
    }
    {
       PagedImage<Float> pIm(tiledShape, cSys, 
                             String("tPagedImage_tmp.img2"));
    }
    {
       PagedImage<Float> pIm(tiledShape, cSys, 
                             String("tPagedImage_tmp.img3"),
                             TableLock(TableLock::AutoLocking));
    }
    Table::deleteTable(String("tPagedImage_tmp.img3"));
    {
       Table table = makeScrTable(String("tPagedImage_tmp.img4"));
       PagedImage<Float> pIm(tiledShape, cSys, table);
       PagedImage<Float> pIm2(table);
    }
    {
       Table table = makeNewTable(String("tPagedImage_tmp.img5"));
       PagedImage<Float> pIm(tiledShape, cSys, table);
    }
    {
       PagedImage<Float> pIm(String("tPagedImage_tmp.img5"));
    }
    {
       PagedImage<Float> pIm(String("tPagedImage_tmp.img5"),
                             TableLock(TableLock::AutoLocking));
    }
//
// Test copy constructor.  This is by reference so test that
// this is so
//
    {
       PagedImage<Float> pIm(String("tPagedImage_tmp.img5"));
       PagedImage<Float> pIm2(pIm);
       pIm.tempClose();
       pIm.putAt(Float(1.0), IPosition(2,0,0));
       AlwaysAssert( (pIm(IPosition(2,0,0))==Float(1.0)), AipsError);
       pIm2.putAt(Float(10.0), IPosition(2,0,0));
       AlwaysAssert((pIm2(IPosition(2,0,0))==Float(10.0)), AipsError);
       AlwaysAssert((pIm(IPosition(2,0,0))==Float(10.0)), AipsError);
//
       AlwaysAssert(pIm(IPosition(2,0,0))==pIm.getAt(IPosition(2,0,0)),
                    AipsError);
    }
//
// Test assignment.  This is by reference so test that
// this is so
//
    {
       PagedImage<Float> pIm(String("tPagedImage_tmp.img5"));
       PagedImage<Float> pIm2(pIm);
       pIm2 = pIm;
       pIm.putAt(Float(1.0), IPosition(2,0,0));
       AlwaysAssert( (pIm(IPosition(2,0,0))==Float(1.0)), AipsError);
       pIm2.putAt(Float(10.0), IPosition(2,0,0));
       AlwaysAssert((pIm2.getAt(IPosition(2,0,0))==Float(10.0)), AipsError);
       AlwaysAssert((pIm.getAt(IPosition(2,0,0))==Float(10.0)), AipsError);
//
       AlwaysAssert(pIm(IPosition(2,0,0))==pIm.getAt(IPosition(2,0,0)),
                    AipsError);
    }

//
// Test clones.  There is not much we can do to make sure they are ok !
// They are by reference too.
//
    {
       PagedImage<Float> pIm(String("tPagedImage_tmp.img5"));
       pIm.putAt(Float(1.0), IPosition(2,0,0));
       AlwaysAssert( (pIm(IPosition(2,0,0))==Float(1.0)), AipsError);
//
       Lattice<Float>* lat = pIm.clone();
       AlwaysAssert(pIm.shape()==lat->shape(), AipsError);
//
       lat->putAt(Float(10.0), IPosition(2,0,0));
       AlwaysAssert(((*lat).getAt(IPosition(2,0,0))==Float(10.0)), AipsError);
       AlwaysAssert((pIm.getAt(IPosition(2,0,0))==Float(10.0)), AipsError);
       delete lat;
    }
    {
       PagedImage<Float> pIm(String("tPagedImage_tmp.img5"));
       pIm.putAt(Float(1.0), IPosition(2,0,0));
       AlwaysAssert( (pIm.getAt(IPosition(2,0,0))==Float(1.0)), AipsError);
//
       Lattice<Float>* ii = pIm.cloneII();
       AlwaysAssert(pIm.shape()==ii->shape(), AipsError);

//
       ii->putAt(Float(10.0), IPosition(2,0,0));
       AlwaysAssert(((*ii).getAt(IPosition(2,0,0))==Float(10.0)), AipsError);
       AlwaysAssert((pIm.getAt(IPosition(2,0,0))==Float(10.0)), AipsError);
       delete ii;
    }
//
// Test some miscellaneous little things
//
    {
       PagedImage<Float> pIm(String("tPagedImage_tmp.img5"));
//
       AlwaysAssert(imagePixelType(String("tPagedImage_tmp.img5"))==TpFloat,   // Global function
                    AipsError);
       AlwaysAssert(pIm.name(True)==String("tPagedImage_tmp.img5"),
                    AipsError);
       cout << "Absolute name = " << pIm.name(False) << endl;
       AlwaysAssert(pIm.isPaged(), AipsError);
       AlwaysAssert(pIm.isWritable(), AipsError);
       AlwaysAssert(pIm.ok(), AipsError);
//
       pIm.rename(String("tPagedImage_tmp.img6"));
       AlwaysAssert(pIm.name(True)==String("tPagedImage_tmp.img6"),
                    AipsError);
       pIm.rename(String("tPagedImage_tmp.img5"));
       AlwaysAssert(pIm.name(True)==String("tPagedImage_tmp.img5"),
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
       Bool ok = False;
       try {
          pIm.resize(shape0);
       } catch (AipsError& x) {
//          cout << "Caught error " << x.getMesg() << endl;
          ok = True;
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
       rec.define("x", Double(1.0));
       rec.define("y", Double(2.0));
       pIm.tempClose();
       pIm.setMiscInfo(rec);
       pIm.tempClose();
       TableRecord rec2 = pIm.miscInfo();
       AlwaysAssert(rec2.nfields()==2, AipsError);
       AlwaysAssert(rec2.isDefined("x"), AipsError);
       AlwaysAssert(rec2.isDefined("y"), AipsError);
       AlwaysAssert(rec2.dataType("x")==TpDouble, AipsError);
       AlwaysAssert(rec2.dataType("y")==TpDouble, AipsError);
       AlwaysAssert(rec2.asDouble("x")==Double(1.0), AipsError);
       AlwaysAssert(rec2.asDouble("y")==Double(2.0), AipsError);
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
     Table::deleteTable(String("tPagedImage_tmp.img5"));
//
// do{Put,Get}Slice tests
//
    {
       Table table = makeScrTable(String("tPagedImage_tmp.img7"));
       IPosition shape2(2,5, 10);
       TiledShape tiledShape2(shape2);
       CoordinateSystem cSys2 = CoordinateUtil::defaultCoords2D();
       PagedImage<Float> pIm(tiledShape2, cSys2, table);
//
// Fill (slowly so use small image)
//
       IPosition pos(2);
       for (Int i=0; i<shape2(0); i++) {
          for (Int j=0; j<shape2(1); j++) {
             pos(0) = i;
             pos(1) = j;
             pIm.putAt(Float(i*j), pos);
          }  
       }
//
       Slicer slice(IPosition(2,0,0), shape2, IPosition(2,1,1));
       Array<Float> data;
       pIm.doGetSlice(data, slice);
       AlwaysAssert(data.shape()==shape2, AipsError);
//
       for (Int i=0; i<shape2(0); i++) {
          for (Int j=0; j<shape2(1); j++) {
             pos(0) = i;
             pos(1) = j;
             AlwaysAssert(data(pos)==pIm.getAt(pos), AipsError);
          }  
       }
//
       data*Float(20.0);
       pIm.doPutSlice(data, IPosition(2,0,0), IPosition(2,1,1));
       for (Int i=0; i<shape2(0); i++) {
          for (Int j=0; j<shape2(1); j++) {
             pos(0) = i;
             pos(1) = j;
             AlwaysAssert(data(pos)==pIm.getAt(pos), AipsError);
          }  
       }
//
       Array<Float> data2(data.copy());
       data.resize(IPosition(2,2,2));
       data.set(0.0);
       pIm.doPutSlice(data, IPosition(2,0,0), IPosition(2,1,1));
       pos(0) = 0; pos(1) = 0;
       AlwaysAssert(pIm(IPosition(2,0,0))==Float(0.0), AipsError);
       AlwaysAssert(pIm(IPosition(2,0,1))==Float(0.0), AipsError);
       AlwaysAssert(pIm(IPosition(2,1,0))==Float(0.0), AipsError);
       AlwaysAssert(pIm(IPosition(2,1,1))==Float(0.0), AipsError);
       for (Int i=2; i<shape2(0); i++) {
          for (Int j=2; j<shape2(1); j++) {
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
       PagedImage<Float> pIm(tiledShape2, cSys2, table);
//
       IPosition pos(2);
       Array<Float> data(shape2);
       for (Int i=0; i<shape2(0); i++) {
          for (Int j=0; j<shape2(1); j++) {
             pos(0) = i;
             pos(1) = j;
             data(pos) = Float(i*j);
          }  
       }
       pIm.doPutSlice(data, IPosition(2,0,0), IPosition(2,1,1));
//
       ArrayLattice<Float> lat(shape2);
       lat.set(Float(10.0));
       pIm += lat;
       Array<Float> data2(data.copy());
       data2 += Float(10.0);
//
       Slicer slice(IPosition(2,0,0), shape2, IPosition(2,1,1));
       Array<Float> data3;
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
       PagedImage<Float> pIm(tiledShape2, cSys2, table);
       pIm.set(3.0);
       AlwaysAssert(allEQ(pIm.get(), Float(3.0)), AipsError);
//
// 2 * x * x
//
       pIm.apply(&func);
       AlwaysAssert(allNear(pIm.get(), Float(18.0), Double(1e-6)), AipsError);
//
// 3 * x (const arg)
//
       pIm.set(3.0);
       AlwaysAssert(allEQ(pIm.get(), Float(3.0)), AipsError);
       pIm.apply(&const_arg_func);
       AlwaysAssert(allNear(pIm.get(), Float(9.0), Double(1e-6)), AipsError);
//
// Polynomial 1 + 2x + 3x**2
//
       pIm.set(3.0);
       AlwaysAssert(allEQ(pIm.get(), Float(3.0)), AipsError);
//
       Polynomial<Float> poly(3);
       poly.setCoefficient(1, 1.0);
       poly.setCoefficient(2, 2.0);
       poly.setCoefficient(3, 3.0);
       pIm.apply(poly);
       AlwaysAssert(allNear(pIm.get(), poly(3.0), Double(1e-6)), AipsError);
   }
//
// test table function.  I don't really know what else to do with it.
//
   {
       Table table = makeScrTable(String("tPagedImage_tmp.img11"));
       IPosition shape2(2, 5, 10);
       TiledShape tiledShape2(shape2);
       CoordinateSystem cSys2 = CoordinateUtil::defaultCoords2D();
       PagedImage<Float> pIm(tiledShape2, cSys2, table);
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
       PagedImage<Float> pIm(tiledShape2, cSys2, table);
       pIm.set(1.0);
//
       LatticeIterator<Float> it(pIm);
       while (!it.atEnd()) {
          AlwaysAssert(allEQ(it.cursor(), Float(1.0)), AipsError);
          it++;
       }
   }
   {
	   // per plane beam support
	   String name = "tPagedImage_tmp_afsdf.im";
	   PagedImage<Float> temp(
			   TiledShape(IPosition(4, 64 ,64, 4, 16)),
			   CoordinateUtil::defaultCoords4D(), name
	   );
	   ImageInfo info = temp.imageInfo();
	   Quantity maj(5, "arcsec");
	   Quantity min(3, "arcsec");
	   Quantity pa(30, "deg");
	   info.setAllBeams(16, 4, GaussianBeam());
	   Bool ok = True;
	   try {
		   temp.setImageInfo(info);
		   ok = False;
	   }
	   catch (AipsError& x) {
		   cout << "Exception thrown as expected: " << x.getMesg() << endl;
	   }
	   AlwaysAssert(ok, AipsError);
	   info.setBeam(0, 0, maj, min, pa);
	   try {
		   temp.setImageInfo(info);
		   ok = False;
	   }
	   catch (AipsError& x) {}
	   AlwaysAssert(ok, AipsError);
	   for (uInt i=0; i<4; i++) {
		   for (uInt j=0; j<16; j++) {
			   info.setBeam(j, i, maj, min, pa);
		   }
	   }
	   AlwaysAssert(temp.setImageInfo(info), AipsError);
           GaussianBeam beam2 = temp.imageInfo().restoringBeam(2,2);
	   ok = True;
	   try {
             GaussianBeam beam = temp.imageInfo().restoringBeam();
             ok = False;
	   }
	   catch (AipsError& x) {
             cout << "Exception thrown as expected: " << x.getMesg() << endl;
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
  } catch (AipsError& x) {
    cerr << "Exception caught: " << x.getMesg() << endl;
    return 1;
  } 

  return 0;



}
