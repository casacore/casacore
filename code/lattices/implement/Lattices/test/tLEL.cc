#include <aips/aips.h>
#include <aips/Arrays.h>
#include <aips/Inputs/Input.h>
#include <aips/Measures.h>
#include <aips/Utilities/String.h>
#include <aips/Exceptions/Error.h>
#include <trial/Images/PagedImage.h>
#include <trial/Images/SubImage.h>
#include <trial/Images/ImageRegion.h>
#include <trial/Lattices/LCSlicer.h>
#include <trial/Lattices/LatticeExprNode.h>
#include <iostream.h>
 
 
 
main (int argc, char **argv)
{
   Input inputs(1);
   inputs.version ("$Revision$");
 
   inputs.create("infile", "", "Infile");
   inputs.readArguments(argc, argv);
 
   const String infile = inputs.getString("infile");
 
 
try {
 
   PagedImage<Float> im(infile);
   const uInt ndim = im.ndim();
   IPosition shape = im.shape();
   IPosition blc(ndim,0);
   IPosition trc(shape-1);
   blc(3) = 1;               // Stokes == Q
   trc(3) = 1;
//
   LCSlicer slicer(blc, trc, RegionType::Abs);
   ImageRegion region(slicer);
   SubImage<Float> subIm(im, region);
cerr << "Shape = " << subIm.shape() << endl;
//
cerr << "make n4 scalar node" << endl;
   LatticeExprNode n1 (subIm);
   LatticeExprNode n4 (stddev(n1));
cerr << "n4.isScalar = " << n4.isScalar() << endl;
//
cerr << "evaluate scalar node " << endl;
      cerr << "n4.scalar = " << n4.getFloat() << endl;
 
}  catch (AipsError x) {
    cerr << "aipserror: error " << x.getMesg() << endl;
    exit(1);
  };
 
  exit(0);
 
}

