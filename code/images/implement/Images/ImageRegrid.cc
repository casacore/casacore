//# DOdeconvolver.cc: this implements the deconvolver DO
//# Copyright (C) 1996,1997,1998,1999
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
//#
//# $Id$

#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/ArrayMath.h>

#include <aips/Logging.h>
#include <aips/Logging/LogIO.h>

#include <aips/Utilities/Assert.h>


#include <aips/Mathematics/Constants.h>

#include <aips/Logging/LogSink.h>
#include <aips/Logging/LogMessage.h>

#include <aips/Arrays/ArrayMath.h>

#include <trial/MeasurementEquations/StokesImageUtil.h>
#include <trial/Lattices/CopyLattice.h> 
#include <trial/Lattices/LatticeExpr.h> 
#include <trial/Lattices/LatticeFFT.h> 
#include <trial/Lattices/LatticeCleaner.h> 
#include <trial/Lattices/LatticeCleanerProgress.h> 
#include <trial/Lattices/LatticeConvolver.h> 
#include <trial/Lattices/TiledLineStepper.h> 
#include <trial/Lattices/LatticeStepper.h> 
#include <trial/Lattices/LatticeNavigator.h> 
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/SubLattice.h>
#include <trial/Lattices/LCBox.h>
#include <trial/Lattices/LCSlicer.h>

#include <trial/Images/TempImage.h>
#include <trial/Images/SubImage.h>

#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Coordinates/SpectralCoordinate.h>
#include <trial/Coordinates/StokesCoordinate.h>
#include <trial/Coordinates/Projection.h>

#include <trial/Images/ImageRegrid.h>



// defaul constructor
template<class T>
ImageRegrid<T>::ImageRegrid() :
  templateShape(0),
  templateCoords_p(0),
  interpOrder(6),
  outShape(0)
{
};

// useful constructor 1
template<class T>
ImageRegrid<T>::ImageRegrid(const IPosition& tShape,
			    const CoordinateSystem& tCoords,
			    const uInt iOrder,
			    const IPosition& oShape) :
  templateShape(tShape),
  templateCoords_p(&tCoords),
  interpOrder(iOrder),
  outShape(oShape)
{
};


// useful constructor 2
template<class T>
ImageRegrid<T>::ImageRegrid(const IPosition& tShape,
			    const CoordinateSystem& tCoords) :
  templateShape(tShape),
  templateCoords_p(&tCoords)
{
  interpOrder = 6;
  outShape = templateShape;
};

// copy constructor
template<class T>
ImageRegrid<T>::ImageRegrid(const ImageRegrid& other)  :
  templateShape(0),
  templateCoords_p(0),
  interpOrder(6),
  outShape(0)
{
  templateShape = other.templateShape;
  templateCoords_p = other.templateCoords_p;
  outShape = other.outShape;
  interpOrder = other.interpOrder;
};


template<class T>
ImageRegrid<T>::~ImageRegrid(){
};




template<class T>
ImageRegrid<T>& ImageRegrid<T>::operator=(const ImageRegrid& other) 
{
  if (this != &other) {
    templateShape = other.templateShape;
    templateCoords_p = other.templateCoords_p;
    outShape = other.outShape;
    interpOrder = other.interpOrder;
  }
  return *this;
};

template<class T>
void ImageRegrid<T>::setTemplate(const IPosition& tShape,
				 const CoordinateSystem& tCoords){
  templateShape = tShape;
  templateCoords_p = &tCoords;
};

template<class T>
void ImageRegrid<T>::setInterpOrder (uInt order){
  interpOrder = order;
};

template<class T>
void ImageRegrid<T>::setOutShape (const IPosition& oShape){
  outShape = oShape;
};




template<class T>
ImageInterface<T>* ImageRegrid<T>::regrid(ImageInterface<T>& imageData,	
				   Bool stokesImageConventions) 
{

  Int directionIndex=templateCoords_p->findCoordinate(Coordinate::DIRECTION);
  AlwaysAssert(directionIndex>=0, AipsError);
  DirectionCoordinate
    templateDirCoord=templateCoords_p->directionCoordinate(directionIndex);
  directionIndex=imageData.coordinates().findCoordinate(Coordinate::DIRECTION);
  AlwaysAssert(directionIndex>=0, AipsError);
  DirectionCoordinate
    dataDirCoord=imageData.coordinates().directionCoordinate(directionIndex);
  Vector<String> units(2); units = "deg";                       
  dataDirCoord.setWorldAxisUnits(units);
  templateDirCoord.setWorldAxisUnits(units);

  if (stokesImageConventions) {

    if (!StokesImageUtil::standardImageCoordinates(*templateCoords_p) ||
	!StokesImageUtil::standardImageCoordinates(imageData.coordinates())) {
      throw(AipsError("In ImageRegrid::fitIntoStokesImage: image coordinates are non-standard"));
    }
    // can now assume that Coords are: Direction/Stokes/Spectral
    // For now, we mandate that the Stokes and Spectral axes are identical
    if (templateShape(2) != imageData.shape()(2)) {
      throw(AipsError("In ImageRegrid::fitIntoStokesImage: images STOKES shapes differ"));
    }    
    if (templateShape(3) != imageData.shape()(3)) {
      throw(AipsError("In ImageRegrid::fitIntoStokesImage: images SPECTRAL shapes differ"));
    } 


    if (convergentDirCoords(dataDirCoord, templateDirCoord)) {
      return ( fitIntoStokesImage(imageData) );
    } else {
      throw(AipsError("ImageRegrid::regrid() - hgeom not implemented yet"));
      // return ( hgeomStokesImage(imageData) );
    }

  } else {  // nonStokesImage conventions
    throw(AipsError("ImageRegrid::regrid() - currently requires StokesImage assumptions"));

    /*

    Bool convergentCoords = True;
    // Need to test for convergentCoords, which will be a different test
    // than above
    if (convergentCoords) {        
      return ( fitIntoGenericImage(imageData) );
    } else {
      return ( hgeomGenericImage(imageData) );
    }
    */

  }
  
// Shut compiler up

  ImageInterface<T>* p = 0;
  return p;

};


template<class T>
Bool
ImageRegrid<T>::convergentDirCoords(const DirectionCoordinate& aaa,
				    const DirectionCoordinate& bbb)
{
  // test for convergent coordinates
  Bool isConvergent = True;   

  Vector<Double> aRefVal = aaa.referenceValue();
  Vector<Double> aInc = aaa.increment();
  Vector<Double> aRefPix = aaa.referencePixel();
  Vector<String> aAxisNames = aaa.worldAxisNames();
  Matrix<Double> aLinearTransform = aaa.linearTransform();

  Vector<Double> bRefVal = bbb.referenceValue();
  Vector<Double> bInc = bbb.increment();
  Vector<Double> bRefPix = bbb.referencePixel();
  Vector<String> bAxisNames = bbb.worldAxisNames();
  Matrix<Double> bLinearTransform = bbb.linearTransform();
  
  if ( (aRefVal(0) != bRefVal(0)) ||  (aRefVal(1) != bRefVal(1)) ) {
    isConvergent = False;
  }
  if ( (aInc(0) != bInc(0)) || (aInc(1) != bInc(1)) ) {
    isConvergent = False;
  }
  if ( (aAxisNames(0) != bAxisNames(0)) || (aAxisNames(1) != bAxisNames(1)) ) {
    isConvergent = False;
  }
  
  Int iax, iay, ibx, iby;
  aLinearTransform.shape(iax, iay);
  bLinearTransform.shape(ibx, iby);
  if (iax != ibx || iay != iby) {
      isConvergent = False;
  }
  for (Int ix=0;ix<iax; ix++) {
    for (Int iy=0;iy<iay; iy++) {
      if (aLinearTransform(ix, iy) !=  bLinearTransform(ix, iy)) {
	isConvergent = False;
      }
    }
  }

  if ( (aAxisNames(0) != bAxisNames(0)) || (aAxisNames(1) != bAxisNames(1)) ) {
    isConvergent = False;
  }

  return  isConvergent;
};


// this is private: we can assume that the proper checks have occured:
// Image Coordinate Systems are in StokesImage Order, and
// coordinates are "congruent" (ie, rotation, reference value, and cell size
// are the same).

template<class T>
ImageInterface<T>* 
ImageRegrid<T>::fitIntoStokesImage(ImageInterface<T>& imageData) {

  Int directionIndex=templateCoords_p->findCoordinate(Coordinate::DIRECTION);
  AlwaysAssert(directionIndex>=0, AipsError);
  DirectionCoordinate
    templateDirCoord=templateCoords_p->directionCoordinate(directionIndex);
  directionIndex=imageData.coordinates().findCoordinate(Coordinate::DIRECTION);
  AlwaysAssert(directionIndex>=0, AipsError);
  DirectionCoordinate
    dataDirCoord=imageData.coordinates().directionCoordinate(directionIndex);
  Vector<String> units(2); units = "deg";                       
  dataDirCoord.setWorldAxisUnits(units);
  templateDirCoord.setWorldAxisUnits(units);
  
  if (templateDirCoord.near(&dataDirCoord) && 
      imageData.shape() == templateShape) {
    return imageData.cloneII();
  }  else {

    Vector<Double> tRefPix = templateDirCoord.referencePixel();
    Vector<Double> dRefPix = dataDirCoord.referencePixel();

    TempImage<T> *out_p;
    out_p = new TempImage<T> (templateShape, *templateCoords_p);
    
    IPosition zeroIP(4,0,0,0,0);
    // corners of data with respect to the template
    IPosition dshape(imageData.shape());
    IPosition dblc(4,0,0,0,0);
    IPosition dtrc = dshape - 1;
    dblc(0) = (Int)(tRefPix(0) - dRefPix(0));
    dblc(1) = (Int)(tRefPix(1) - dRefPix(1));
    dtrc(0) = dblc(0) + dshape(0) - 1;
    dtrc(1) = dblc(1) + dshape(1) - 1;
    // corners of template with respect to the data
    IPosition tshape(templateShape);
    IPosition tblc(4,0,0,0,0);
    IPosition ttrc = tshape - 1;
    tblc(0) = (Int)(dRefPix(0) - tRefPix(0));
    tblc(1) = (Int)(dRefPix(1) - tRefPix(1));
    ttrc(0) = tblc(0) + tshape(0) - 1;
    ttrc(1) = tblc(1) + tshape(1) - 1;
    
    IPosition _dshape(2); _dshape(0) = dshape(0); _dshape(1) = dshape(1); 
    IPosition _tshape(2); _tshape(0) = tshape(0); _tshape(1) = tshape(1); 
    IPosition _dblc(2); _dblc(0) = dblc(0); _dblc(1) = dblc(1);
    IPosition _dtrc(2); _dtrc(0) = dtrc(0); _dtrc(1) = dtrc(1);
    IPosition _tblc(2); _tblc(0) = tblc(0); _tblc(1) = tblc(1);
    IPosition _ttrc(2); _ttrc(0) = ttrc(0); _ttrc(1) = ttrc(1);
    
  /*   for debugging
       cout << "dblc = " << dblc << endl;
       cout << "dtrc = " << dtrc << endl;
       cout << "dshape = " << dshape << endl;
       cout << "tblc = " << tblc << endl;
       cout << "ttrc = " << ttrc << endl;
       cout << "tshape = " << tshape << endl;
       cout << "max(_dblc.asVector()) = " << (max(_dblc.asVector())) << endl;
       cout << "min((_dshape -1 - _ttrc).asVector()) = " <<
       (min((_dshape -1 - _ttrc).asVector())) << endl;
  */

    if (min(_dblc.asVector()) >= 0 
	&& max((_tshape -1 - _dtrc).asVector()) >= 0) {
      // data is a simple subregion of template: just padd
      out_p->set(0.0);
      LCBox box(dblc,dtrc,templateShape);
      SubLattice<T> sub( *out_p, box, True );
      LatticeExpr<T> expr =  (LatticeExprNode)  imageData;
      sub.copyData(expr);
      return out_p;
    } else if (max(_dblc.asVector()) < 0 
	       && min((_dshape -1 - _ttrc).asVector()) >= 0) {
      // data is a simple super-region of template: just subsection data image
      LCBox box(tblc,ttrc,imageData.shape());
      SubLattice<T> sub( imageData, box, False );
      LatticeExpr<T> expr = (LatticeExprNode) sub;
      out_p->copyData(expr);
      return out_p;
    } else if (max( (_dblc - (_tshape-1)).asVector()) > 0 
	       || min(_dtrc.asVector()) < 0) {
      // no overlap in images! return zero image
      out_p->set(0.0);
      return out_p;
    } else {
      // mixed case: images overlap, but neither image is a subset of the other
      // strategy: take a subset of the data image, copy it to the appropriate
      // subset of the template image
      
      // ntblc, nttrc are the corners of the TEMPLATE WRT the data image:
      // make sure tblc and ttrc are within 0, imageData.shape()
      
      out_p->set(0.0);
      IPosition ntblc = max(tblc, zeroIP);
      IPosition nttrc = min(ttrc, (imageData.shape()-1));
      //	cout << " mixed ntblc = " << ntblc << endl;
      //	cout << " mixed nttrcc = " << nttrc << endl;
      //	cout << "data shape = " << imageData.shape() << endl;
      LCBox dbox(ntblc,nttrc,imageData.shape());
      SubLattice<T> dsub( imageData, dbox, False );
      
      // now, glue that sublattice into the sublattice which is the overlap of the
      // Data image in the Tamplate
      IPosition ndblc = ntblc + dblc;
      IPosition ndtrc = nttrc + dblc;
      cout << " mixed ndblc = " << ndblc << endl;
      //	cout << " mixed ndtrc = " << ndtrc << endl;
      //	cout << " templateShape = " << templateShape << endl;
      LCBox tbox(ndblc,ndtrc,templateShape);
      SubLattice<T> tsub( *out_p, tbox, True );
      
      // check that they are the same size:
      if ( dsub.shape() != tsub.shape() ) {
	throw(AipsError("ImageRegrid::fitIntoStokesImage() -- Subs are not correct shapes"));
      }
      LatticeExpr<T> expr =  (LatticeExprNode)dsub;
      tsub.copyData(expr);
      return out_p;
    }
  }
};
