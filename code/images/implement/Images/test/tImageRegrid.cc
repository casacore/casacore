#include <aips/Inputs/Input.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/Cube.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Containers/Block.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Images/TempImage.h>
#include <trial/Images/SubImage.h>
#include <trial/Images/PagedImage.h>
#include <trial/Images/ImageRegrid.h>
#include <trial/Images/ImageRegion.h>
#include <trial/Lattices/MaskedLattice.h> 
#include <trial/Lattices/LCPagedMask.h> 
#include <aips/Lattices/TempLattice.h>
#include <aips/Lattices/TiledShape.h>
#include <trial/Mathematics/Interpolate2D.h>
#include <aips/Logging/LogIO.h>
#include <aips/Utilities/Assert.h>
#include <aips/strstream.h>
#include <aips/iostream.h>



main (int argc, char **argv)
{
   Input inputs(1);
   inputs.version ("$Revision$");

// Get inputs

   inputs.create("in", "", "Input image name");
   inputs.create("core", "True", "Image in core");
   inputs.create("axes", "-10", "axes");
   inputs.create("method", "linear", "Method");
   inputs.create("save", "False", "Save output ?");
   inputs.create("shape", "-10", "Shape");
   inputs.create("replicate", "False", "Replicate ?");
   inputs.readArguments(argc, argv);
   const String in = inputs.getString("in");
   const Bool core = inputs.getBool("core");
   const Bool save = inputs.getBool("save");
   const String method = inputs.getString("method");
   const Block<Int> axesU(inputs.getIntArray("axes"));
   const Block<Int> shapeU(inputs.getIntArray("shape"));
   const Bool replicate = inputs.getBool("replicate");
//
   Int lim = 0;
   if (core) lim = 1000000;
//
   ImageInterface<Float>* pIm = 0;
   if (in.empty()) {
      IPosition shape(3, 64, 128, 32);
      TiledShape shape2(shape);
      CoordinateSystem cSys = CoordinateUtil::makeCoordinateSystem(shape, False);
//
      pIm = new TempImage<Float>(shape2, cSys, lim);
      pIm->set(1.0);
//
      TempLattice<Bool> inMask(shape2, lim);
      inMask.set(True);
      TempImage<Float>* pTemp = dynamic_cast<TempImage<Float>*>(pIm);
      pTemp->attachMask(inMask);
   } else {
      pIm = new PagedImage<Float>(in);
   }
//
   IPosition axes(3, 0, 1, 2);
   if (axesU.nelements()>0) {
      if (axesU.nelements()==1 && axesU[0]==-10) {
      } else {
         axes.resize(axesU.nelements());
         for (uInt i=0; i<axes.nelements(); i++) axes(i) = axesU[i];
      }
   }
//
   IPosition shapeIn = pIm->shape();
   IPosition shapeOut = shapeIn;
   if (shapeU.nelements()>0) {
      if (shapeU.nelements()==1 && shapeU[0]==-10) {
      } else {
         shapeOut.resize(shapeU.nelements());
         for (uInt i=0; i<shapeOut.nelements(); i++) shapeOut(i) = shapeU[i];
      }
   }
   cerr << "shape = " << shapeIn << shapeOut << endl;
   cerr << "axes = " << axes << endl;
//
   uInt n = pIm->ndim();
   CoordinateSystem cSysOut = pIm->coordinates();
   Vector<Double> incr = cSysOut.increment().copy();
   Vector<Double> refp  = cSysOut.referencePixel().copy();
   Vector<Double> refv  = cSysOut.referenceValue().copy();
   for (uInt i=0; i<n; i++) {
      incr(i) = incr(i) * shapeIn(i) / shapeOut(i);
      refp(i) = shapeOut(i) / 2.0;
   }
   cSysOut.setReferencePixel(refp);
   cSysOut.setIncrement(incr);
//

   ImageInterface<Float>* pImOut = 0;
   if (save) {
      pImOut = new PagedImage<Float>(shapeOut, cSysOut, String("outFile"));
   } else {
      pImOut = new TempImage<Float>(shapeOut, cSysOut, lim);
   }
   String maskName = pImOut->makeUniqueRegionName(String("mask"), 0);    
   pImOut->makeMask(maskName, True, True, True, True);
//
   Interpolate2D::Method emethod = Interpolate2D::stringToMethod(method);
   ImageRegrid<Float> regridder;
   regridder.regrid(*pImOut, emethod, axes, *pIm, replicate);
   delete pIm;
   delete pImOut;
}


