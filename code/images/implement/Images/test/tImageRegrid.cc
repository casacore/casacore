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
#include <strstream.h>
#include <iostream.h>



main (int argc, char **argv)
{
   Input inputs(1);
   inputs.version ("$Revision$");

// Get inputs

   inputs.create("in", "", "Input image name");
   inputs.create("core", "True", "Image in core");
   inputs.create("axes", "-10", "axes");
   inputs.readArguments(argc, argv);
   const String in = inputs.getString("in");
   const Bool core = inputs.getBool("core");
   const Block<Int> axesU(inputs.getIntArray("axes"));
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
/*
   cerr << "input name = " << pIm->name() << endl;
   cerr << "input.isPersistent=" << pIm->isPersistent() << endl;
   cerr << "Shape=" << pIm->shape() << endl;
   cerr << "nCoords = " << pIm->coordinates().nCoordinates() << endl;
*/

/*
   PagedImage<Float> out(pIm->shape(), pIm->coordinates(), String("outFile"));
   String maskName = out.makeUniqueRegionName(String("mask"), 0);    
   LCPagedMask mask(RegionHandler::makeMask(out, maskName));
   out.defineRegion (maskName, ImageRegion(mask), RegionHandler::Masks);
   out.setDefaultMask(maskName);
*/

   TiledShape shapeOut(pIm->shape());
   TempImage<Float> out(shapeOut, pIm->coordinates(), lim);
   TempLattice<Bool> outMask(shapeOut, lim);
   outMask.set(True);
   out.attachMask(outMask);
//
   Interpolate2D<Float>::Method method = Interpolate2D<Float>::LINEAR;
   IPosition axes(3, 0, 1, 2);
   if (axesU.nelements()>0) {
      if (axesU.nelements()==1 && axesU[0]==-10) {
      } else {
         axes.resize(axesU.nelements());
         for (uInt i=0; i<axes.nelements(); i++) axes = axesU[i];
      }
   }
//
   ImageRegrid<Float> regridder;
   regridder.regrid(out, method, axes, *pIm);
   delete pIm;
}


