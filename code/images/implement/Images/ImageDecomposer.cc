//# ImageDecomposer.cc: defines the ImageDecomposer class
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2002
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

#include <trial/Images/ImageDecomposer.h>

#include <aips/Arrays/Slicer.h>
#include <aips/Lattices/TiledShape.h>
#include <trial/Fitting/FitGaussian.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Lattices/LatticeRegion.h>
#include <trial/Lattices/LCMask.h>
#include <trial/Lattices/LatticeExpr.h>
#include <trial/Lattices/LatticeExprNode.h>
#include <aips/Lattices/LatticeIterator.h>
#include <aips/Utilities/Assert.h>
#include <trial/Images/TempImage.h>
#include <trial/Images/SubImage.h>


template <class T>
ImageDecomposer<T>::ImageDecomposer()
 : itsImagePtr(0),
   itsMapPtr(0),
   itsShape(0), 
   itsDim(0),
   itsNRegions(0),
   itsNComponents(0)
{}

template <class T>
ImageDecomposer<T>::ImageDecomposer(ImageInterface<T>& image)
 : itsImagePtr(image.cloneII()),
   itsMapPtr(0),
   itsShape(itsImagePtr->shape()),
   itsDim(itsShape.nelements()),
   itsNRegions(0),
   itsNComponents(0)
{
  itsMapPtr = new TempLattice<Int>(TiledShape(itsShape), 1); 
  if (!itsMapPtr) {
     delete itsImagePtr;
     throw(AipsError("Failed to create internal TempLattice"));
  }
//
  itsMapPtr->set(0);
}


template <class T>
ImageDecomposer<T>::ImageDecomposer(const ImageDecomposer<T>& other)

 : itsImagePtr(other.itsImagePtr->cloneII()),
   itsMapPtr(0),
   itsShape(other.itsShape),
   itsDim(other.itsDim),
   itsNRegions(0),
   itsNComponents(0)
{
  itsMapPtr = new TempLattice<Int>(TiledShape(itsShape), 1);  
  if (!itsMapPtr) {
     delete itsImagePtr;
     throw(AipsError("Failed to create internal TempLattice"));
  }
//
  itsNRegions = other.itsNRegions;
  itsNComponents = other.itsNComponents;
  itsList = other.itsList.copy();

// Copy values from other.itsMapPtr

  itsMapPtr->copyData(*(other.itsMapPtr));
}

template <class T>
ImageDecomposer<T> &ImageDecomposer<T>::operator=(const ImageDecomposer<T> &other)
{
   if (this != &other) {
      delete itsImagePtr;
      delete itsMapPtr;
//
      itsImagePtr = other.itsImagePtr->cloneII();
      itsShape = other.itsShape;
      itsDim = other.itsDim;
      itsNRegions = 0;
      itsNComponents = 0;
//
      itsMapPtr = new TempLattice<Int>(TiledShape(itsShape), 1);  
      itsMapPtr->copyData(*(other.itsMapPtr));
      itsList = other.itsList.copy();
   }
   return *this;
}



template <class T>
ImageDecomposer<T>::~ImageDecomposer()
{
  if (itsImagePtr) {
     delete itsImagePtr;
     itsImagePtr = 0;
  }
  if (itsMapPtr) {
     delete itsMapPtr;
     itsMapPtr = 0;
  }
}

template <class T>
void ImageDecomposer<T>::setImage(ImageInterface<T>& image)
{
   if (itsImagePtr) {
      delete itsImagePtr;
      itsImagePtr = 0;
   }
   if (itsMapPtr) {
      delete itsMapPtr;
      itsMapPtr = 0;
   }
//
   itsImagePtr = image.cloneII();
   itsShape = itsImagePtr->shape();
   itsDim = itsShape.nelements();
   itsNRegions = 0;
   itsNComponents = 0;
//
   itsMapPtr = new TempLattice<Int>(TiledShape(itsShape), 1); 
   if (!itsMapPtr) {
     delete itsImagePtr;
     throw(AipsError("Failed to create internal TempLattice"));
   }
   itsMapPtr->set(0);
}


template <class T>
Int ImageDecomposer<T>::getCell(Int x, Int y) const
{
  return itsMapPtr->getAt(IPosition(2,x,y));
}

template <class T>
Int ImageDecomposer<T>::getCell(Int x, Int y, Int z) const
{
  return itsMapPtr->getAt(IPosition(3,x,y,z));
}
template <class T>
Int ImageDecomposer<T>::getCell(const IPosition& coord) const
{
  return itsMapPtr->getAt(coord);  //note: 3D IPosition works on 2D image
}


template <class T>
void ImageDecomposer<T>::setCell(Int x, Int y, Int sval)
{
  itsMapPtr->putAt(sval, IPosition(2,x,y));
  return;
}
template <class T>
void ImageDecomposer<T>::setCell(Int x, Int y, Int z, Int sval)
{
  itsMapPtr->putAt(sval, IPosition(3,x,y,z));
  return;
}
template <class T>
void ImageDecomposer<T>::setCell(const IPosition& coord, Int sval)
{
  itsMapPtr->putAt(sval, coord);
  return;
}

template <class T>
IPosition ImageDecomposer<T>::shape() const                    
{
  return itsShape;
}

template <class T>
Int ImageDecomposer<T>::shape(uInt axis) const                 
{
  if (itsDim > axis) return itsShape(axis);
  return 1;
}

template <class T>
Vector<T> ImageDecomposer<T>::autoContour(T mincon, T maxcon, T inc) const
{
  if (inc == T(0)) {
    throw(AipsError("Vector<T> ImageDecomposer<T>::autocontour"
                    "T mincon, T maxcon, T inc) - inc cannot be zero"));
  }
  if ((maxcon - mincon) * inc < 0) inc = -inc;

  Int c = 0;
  for (T cl = mincon; cl <= maxcon; cl += inc) c++;

  Vector<T> contours(c);
  c = 0;
  for (T cl = mincon; cl <= maxcon; cl += inc) {
    contours(c++) = cl;
  }
  return contours;
}

template <class T>
Vector<T> ImageDecomposer<T>::autoContour(Int nContours, T minValue) const
{
// IMPR: a noise estimate to determine default value of lowest contour
// would be useful.
 
  Vector<T> contours(nContours);
  T maxValue;
//
  maxValue = findAreaGlobalMax(IPosition(itsDim,0), shape());
  maxValue -= (maxValue-minValue)/((nContours-1)*3);
  //cerr << "Autocontour: minvalue, maxvalue = " << minValue << ", " << maxValue << endl;

// Make maximum contour ~1/3 contour increment less than max value of image

  for (Int i=0; i<nContours; i++) {
    contours(i) =  minValue + (maxValue-minValue)*i/(nContours-1);
  }
//
  return contours;
}

template <class T>
Vector<T> ImageDecomposer<T>::autoContour(const Function1D<T>& fn,
                                          Int ncontours, T minvalue) const
{
// NOTE: This function has not been recently tested.

  Vector<T> contours(ncontours); 
  T maxvalue;
  T calibzero, calibmax;
// 
  for (Int i=1; i<ncontours; i++) {
    if (fn(T(i-1))>fn(T(i))) {
       throw(AipsError("ImageDecomposer<T>::autoContour-"
                       " fn must be nondecreasing in domain"));
    }
  }
//  
  maxvalue = findAreaGlobalMax(IPosition(itsDim,0), shape());
  maxvalue -= (maxvalue-minvalue)/((ncontours-1)*10);  //much closer to top
  calibzero = minvalue - fn(T(0));
  calibmax = (maxvalue - calibzero) / fn(ncontours - 1);
//
  for (Int i=0; i<ncontours; i++) {
    contours(i) = calibzero + calibmax*fn(i);
  }
//
  return contours;
}

 

template <class T>
void ImageDecomposer<T>::zero()
{
// Set all elements in the itsMapPtr to zero and clear the component list.

  itsMapPtr->set(0);
  itsNRegions = 0;
  itsNComponents = 0;
  itsList.resize();
  return;
}

template <class T>
void ImageDecomposer<T>::clear()
{
  //Clear the component map
  LatticeIterator<Int> iter(*itsMapPtr);
  Bool deleteIt;
  Int* p = 0;
  for (iter.reset(); !iter.atEnd(); iter++) {
     Array<Int>& tmp = iter.rwCursor();
     p = tmp.getStorage(deleteIt);
     for (uInt i=0; i<tmp.nelements(); i++) if (p[i] != MASKED) p[i] = 0;
     tmp.putStorage(p, deleteIt);
  }
  itsNRegions = 0;

  //Clear the component list
  itsNComponents = 0;
  itsList.resize();
  return;
}

template <class T>
T ImageDecomposer<T>::getImageVal(Int x, Int y) const
{
  return getImageVal(IPosition(2,x,y));
}

template <class T>
T ImageDecomposer<T>::getImageVal(Int x, Int y, Int z) const
{
  return getImageVal(IPosition(3,x,y,z));
}

template <class T>
T ImageDecomposer<T>::getImageVal(IPosition coord) const
{
  return itsImagePtr->getAt(coord);
}
  
template <class T>
Int ImageDecomposer<T>::getContourVal(Int x, Int y, const Vector<T>& clevels) const
{
  return getContourVal(IPosition(2,x,y), clevels);
}

template <class T>
Int ImageDecomposer<T>::getContourVal(Int x, Int y, Int z, 
                                      const Vector<T>& clevels) const
{
  return getContourVal(IPosition(3,x,y,z), clevels); 
}

template <class T>
Int ImageDecomposer<T>::getContourVal(IPosition coord, 
                                      const Vector<T>& clevels) const
{
  T val = itsImagePtr->getAt(coord);
  for (uInt c = 0; c < clevels.nelements(); c++) {
    if (val < clevels(c)) return c - 1;
  }
//
  return clevels.nelements()-1;
}


template <class T>
void ImageDecomposer<T>::boundRegions(Block<IPosition>& blc,
                                      Block<IPosition>& trc)
{
// Boxes each region in the componentmap:
// blc is set to the lowest coordinate value in each region; 
// trc is set to one above the highest coordinate value in each region.
            
  DebugAssert(blc.nelements() == itsNRegions, AipsError);
  DebugAssert(trc.nelements() == itsNRegions, AipsError);
   
  for (uInt r=0; r<itsNRegions; r++) {
    blc[r] = itsShape;
    trc[r] = IPosition(itsDim,0);
  }
//
  {
    IPosition pos(itsDim,0); 
    decrement(pos);
    Int r;
    while (increment(pos,shape())) {
        r = getCell(pos);
        if (r > 0) {
          for (uInt i = 0; i < itsDim; i++) {
            if (blc[r-1](i) > pos(i)) blc[r-1](i) = pos(i);
            if (trc[r-1](i) <= pos(i)) trc[r-1](i) = pos(i)+1;
          }
        }
    }
  }
//
  return;
}     

template <class T>
T ImageDecomposer<T>::findAreaGlobalMax(IPosition blc, IPosition trc) const
{
  T val;  
  T maxval = 0.0;
  correctBlcTrc(blc,trc);
//
  {      
    IPosition pos(blc);  
    decrement(pos);
    while (increment(pos,trc))  {
      val = getImageVal(pos);
      if (val > maxval) maxval = val;
    }
  }  
//   
  return maxval;
}


template <class T>
void ImageDecomposer<T>::findAreaGlobalMax(T& maxval, IPosition& maxvalpos,
                                           IPosition blc, IPosition trc) const
{  
  T val; 

  maxvalpos = IPosition(itsDim,0);
  maxval = 0.0;
  correctBlcTrc (blc,trc);
//
  {
    IPosition pos(blc); decrement(pos);
    while (increment(pos,trc))  {     
      val = getImageVal(pos);
      if (val > maxval) {maxval = val; maxvalpos = pos;} 
    }
  }

}

template <class T>
Vector<T> ImageDecomposer<T>::findAreaLocalMax(IPosition blc, IPosition trc,
                                               Int naxis) const
{
  uInt const blocksize = 10;
  uInt maxn = 0;
  Vector<T> maxvals;
  correctBlcTrc (blc, trc);
//
  {
    IPosition pos(blc); 
    decrement(pos);
    while (increment(pos,trc))  {     
      if (isLocalMax(pos,naxis)) {
        if (maxn % blocksize == 0) {
          maxvals.resize(maxn+blocksize, True);
        }     
        maxvals(maxn) = getImageVal(pos);
        maxn++;
      }
    }
  }
  maxvals.resize(maxn, True);
  return maxvals;
}


template <class T>
void ImageDecomposer<T>::findAreaLocalMax(Vector<T>& maxvals, 
                                          Block<IPosition>& maxvalpos,
                                          IPosition blc, IPosition trc,
                                          Int naxis) const
{
  uInt const blocksize = 10;
  uInt maxn = 0;
  maxvals.resize();
  maxvalpos.resize(0);
  correctBlcTrc(blc, trc);
//
  {
    IPosition pos(blc); 
    decrement(pos);
    while (increment(pos,trc))  {     
      if (isLocalMax(pos,naxis))  {
        if (maxn % blocksize == 0) {
          maxvals.resize(maxn+blocksize, True);
          maxvalpos.resize(maxn+blocksize, False, True);
        }     
        maxvals(maxn) = getImageVal(pos);
        maxvalpos[maxn] = pos;
        maxn++;
      }
    }
  }
  maxvals.resize(maxn, True);
  maxvalpos.resize(maxn, True, True);
  return;
}



template <class T>
Vector<T> ImageDecomposer<T>::findRegionLocalMax(Int regionID, Int naxis) const
{
  uInt const blocksize = 10;
  uInt maxn = 0;
  Vector<T> maxvals;
  {
    IPosition pos(itsDim,0); 
    decrement(pos);
    while (increment(pos,shape())) {     
      if ((getCell(pos) == regionID) && isLocalMax(pos,naxis)) {
        if (maxn % blocksize == 0) {
          maxvals.resize(maxn+blocksize, True);
        }     
        maxvals(maxn) = getImageVal(pos);
        maxn++;
      }
    }
  }
  maxvals.resize(maxn, True);
  return maxvals;
}


template <class T>
void ImageDecomposer<T>::findRegionLocalMax(Vector<T>& maxvals, 
                                            Block<IPosition>& maxvalpos,
                                            Int regionID, Int naxis) const
{
  uInt const blocksize = 10;
  uInt maxn = 0;
  maxvals.resize();
  maxvalpos.resize(0);
//
  {
    IPosition pos(itsDim,0); 
    decrement(pos);
    while (increment(pos,shape())) {     
      if ((getCell(pos) == regionID) && isLocalMax(pos,naxis)) {
        cout << "Local max at " << pos << endl;
        if (maxn % blocksize == 0)  {
          maxvals.resize(maxn+blocksize, True);
          maxvalpos.resize(maxn+blocksize, False, True);
	}     
        maxvals(maxn) = getImageVal(pos);
        maxvalpos[maxn] = pos;
        maxn++;
      }
    }
  }
//	     
  maxvals.resize(maxn, True);
  maxvalpos.resize(maxn, True, True);
//
  return;
}

template <class T>
Vector<T> ImageDecomposer<T>::findAllRegionGlobalMax() const
{
  //NOTE: while the regions are identified in the itsMapPtr with #s starting at
  //one, the array returned by this function begin with zero, so there is
  //an offset of one between itsMapPtr IDs and those used by this function.

  Int r;
  T val; 
  Vector<T> maxval(itsNRegions);
  maxval = 0.0; 
//  
  {
    IPosition pos(itsDim,0); 
    decrement(pos);
    while (increment(pos,shape())) {     
      r = getCell(pos);
      if (r > 0) {
        val = getImageVal(pos);
        if (val > maxval(r-1)) maxval(r-1) = val;  
      }
    }
  }
  return maxval;
}

template <class T>
void ImageDecomposer<T>::findAllRegionGlobalMax(Vector<T>& maxvals, 
                                                Block<IPosition>& maxvalpos) const
{
  //NOTE: while the regions are identified in the itsMapPtr with #s starting at
  //one, the arrays returned by this function begin with zero, so there is
  //an offset of one between itsMapPtr IDs and those used by this function.

  Int r;
  T val; 

  maxvals.resize(itsNRegions);
  maxvalpos.resize(itsNRegions);
  maxvals = 0;  //note: wholly negative images still return 0

  {
    IPosition pos(itsDim,0); 
    decrement(pos);
    while (increment(pos,shape())) {     
      r = getCell(pos);
      if (r > 0) {
        val = getImageVal(pos);
        if (val > maxvals(r-1)) {
           maxvals(r-1) = val; 
           maxvalpos[r-1] = pos;
        } 
      }
    }
  }

  return;
}


template <class T>
Bool ImageDecomposer<T>::isLocalMax(const IPosition& pos, Int naxis) const
{
  if (pos.nelements()==2) {
     return isLocalMax(pos(0), pos(1), naxis);
  } else if (pos.nelements()==3) {
     return isLocalMax(pos(0), pos(1), pos(2),naxis);
  } else {
     throw(AipsError("ImageDecomposer<T>::localmax(IPosition pos, Int naxis)"
                       " - pos has wrong number of dimensions"));
  }
  return False;
}

template <class T>
Bool ImageDecomposer<T>::isLocalMax(Int x, Int y, Int naxis) const
{
  T val = getImageVal(x,y);
  Int ximin = (x>0)? -1:0;
  Int yimin = (y>0)? -1:0;
  Int ximax = (x+1<shape(0))? 1:0;
  Int yimax = (y+1<shape(1))? 1:0;
  for (Int xi=ximin; xi<=ximax; xi++) {
    for (Int yi=yimin; yi<=yimax; yi++) {
      if   ( ((naxis > 0) || !(xi || yi))
          && ((naxis > 1) || !(xi && yi))
	  && (getImageVal(x+xi,y+yi) > val))  {
        return False;
      }
    }
  }
//
  return True;
}

template <class T>
Bool ImageDecomposer<T>::isLocalMax(Int x, Int y, Int z, Int naxis) const
{
  T maxval = getImageVal(x,y,z);
  Int ximin = (x>0)? -1:0;
  Int yimin = (y>0)? -1:0;
  Int zimin = (z>0)? -1:0;
  Int ximax = (x+1<shape(0))? 1:0;
  Int yimax = (y+1<shape(1))? 1:0;
  Int zimax = (z+1<shape(2))? 1:0;
  for (Int xi=ximin; xi<=ximax; xi++) {
    for (Int yi=yimin; yi<=yimax; yi++) {
      for (Int zi=zimin; zi<=zimax; zi++) {
        if ( ((naxis > 0) || !(xi || yi || zi))
	  && ((naxis > 1) || !(xi && yi || xi && zi || yi && zi ))      
          && ((naxis > 2) || !(xi && yi && zi))
          && (getImageVal(x+xi,y+yi,z+zi) > maxval)) {
           return False;
        }
      }
    }
  }
//  
  return True;
}

template <class T>
void ImageDecomposer<T>::estimateComponentWidths(Matrix<T>& width,
                                          const Block<IPosition>& maxvalpos) 
                                          const
{
// Finds a rough estimate of the width of each component.  
// Requires the location of each component.

  width.resize(maxvalpos.nelements(), itsDim);
  Bool dblflag = 0; 
//
  for (uInt r = 0; r < maxvalpos.nelements(); r++) {
    IPosition lpos(itsDim);
    IPosition rpos(itsDim);
    IPosition maxpos(itsDim); 
    maxpos = maxvalpos[r];
    T maxvalr = getImageVal(maxpos);
    T thrval = maxvalr*0.25;
    T val, prevval;
    for (uInt a = 0; a < itsDim; a++) {
      lpos = maxpos;
      val = maxvalr;
      prevval = val;
      while ((lpos(a) > 0) && (val >= thrval) && (val <= prevval))  {
        prevval = val;
        lpos(a) --;      
        val = getImageVal(lpos);
      }
      if (val < thrval) {
        width(r,a) = T(maxpos(a)-lpos(a)) - (thrval-val) / (prevval-val);
      } else if (val > prevval) {
        width(r,a) = T(maxpos(a)-lpos(a));
      } else   { //lpos == 0
        {width(r,a) = 0; dblflag = 1;}  //can't find left limit; 
                                        //use 2xright limit instead
      }
//
      rpos = maxpos;    
      val = maxvalr;
      prevval = val;
      while ((rpos(a)<shape(a)-1) && (val >= thrval) && (val <= prevval))  {
        prevval = val;
        rpos(a) ++;      
        val = getImageVal(rpos);
      }
      if (val < thrval) {
        width(r,a) += T(rpos(a)-maxpos(a)) - (thrval-val) / (prevval-val);
      } else if (val > prevval) {
        width(r,a) += T(rpos(a)-maxpos(a));
      } else {
        if (!dblflag) { 
          dblflag = 1;  //use 2x left side instead
	} else {
          width(r,a) += T(rpos(a)-maxpos(a)) * (maxvalr-thrval)/(maxvalr-val);
	  dblflag = 1;
	}
      }
//
      if (width(r,a) <= 0.0) width(r,a) = shape(a);//gaussian bigger than image
      if (!dblflag) width(r,a) /= 2.0;
    }
  }  
//
  return;
}

template <class T>
uInt ImageDecomposer<T>::identifyRegions(T thrval, Int naxis)
{
// Performs a single threshold scan on the image.  In other words,
// identifies all contigous blocks of pixels in the target image above the
// threshold value thrval, assigning each unique block to an integer, 
// starting at one.  All pixels with target image values below thrval are set
// to zero.

// NOTE: Formerly a specialization existed for 2D which may have been slightly
// more efficient.  However, it complicated the code excessively (this
// program is already far too long.)  It could be resurrected if necessary.

  Int const blocksize = 1024;  //increment to grow size of anchor array
  Int const pageexpsize = 128;
  Int cnum = 0;  //region number
  if (naxis > Int(itsDim)) naxis = itsDim;  

// The program first scans through the image until it finds any pixel in
// any region.  Once there, it immediately scans all 6 adjacent pixels, 
// registering any that fall within the region and setting them as anchors. 
// It then moves to the first of these anchors and repeats the process, 
// until no more anchors can be found and every existing anchor has already
// been explored.  It then scans until it locates a new region, and repeats
// until every region has been similarly scanned.

// This has the equivalent effect of a 'surface' of active anchors
// sweeping across each region starting at a seed point until the
// region is fully explored.

// The naxis parameter must be 2 or greater to avoid spurious detections
// along horizontal ridges.  However, this slows down performance by a factor
// of roughly two, so in instances where objects are clearly defined and
// not closely blended and the image is very large naxis=1 may be better. 

  IPosition scanpos(itsDim,0); //decrement(scanpos);
  while (true) {    
    //First find any pixel in next region.
    //Stop scanning when an unassigned, unmasked pixel is found exceeding
    //the threshold value.

    while (getImageVal(scanpos) < thrval || getCell(scanpos))  {
      if (!increment(scanpos,shape())) {
         return itsNRegions = cnum;
      } 
        //scanned w/out finding new region
    }
//   
    IPosition pos(scanpos);
    cnum++;
 
// As many anchors will be required as pixels in the region (the volume) - 
// but only a small fraction of that at any one time (the active surface).
// So the program allocates 'pages' of anchors as they become necessary,
// but continually deletes pages of anchors that have already been used.

    Int seta = -1;   //index of highest established anchor
    Int geta = -1;   //index of highest analyzed anchor
                     //the active surface is all anchors geta < a < seta
    PtrBlock<Matrix<Int> *> aindex(pageexpsize);  //anchor structure
    Int setblock = -1;  //index of page containing highest established anchor
    Int getblock = -1;  //index of page containing highest analyzed anchor
    setCell(pos,cnum);

    do  { //(geta < seta)  
      //cout << geta << " / " << seta << ", " << pos << 
      //  " = " << getCell(pos) << endl;

      //Analyze the cell -
      //Scan the cells around the active cell as follows:
      //naxis = 1: scan 6 adjacent cells (axes only)
      //naxis = 2: scan 18 adjacent cells (axes and 2-axis diagonals)
      //naxis = 3: scan 26 adjacent cells (axes and 2/3-axis diagonals)

      Int ximin = (pos(0)>0)? -1:0;
      Int yimin = (pos(1)>0)? -1:0;
      Int zimin = ((itsDim>=3)&&(pos(2)>0))? -1:0;
      Int ximax = (pos(0)+1<shape(0))? 1:0;
      Int yimax = (pos(1)+1<shape(1))? 1:0;
      Int zimax = ((itsDim>=3)&&(pos(2)+1<shape(2)))? 1:0;  //safe for 2D
//
      for (Int xi=ximin; xi<=ximax; xi++) {
        for (Int yi=yimin; yi<=yimax; yi++) {
          for (Int zi=zimin; zi<=zimax; zi++) {
            if ( (xi || yi || zi) &&
	        ((naxis > 1) || !(xi && yi || xi && zi || yi && zi )) &&
	        ((naxis > 2) || !(xi && yi && zi))) {
              IPosition ipos(pos);
              ipos(0) += xi; ipos(1) += yi; if (itsDim == 3) ipos(2) += zi;

	                                         //if any contiguous pixel is
              if ((getImageVal(ipos) >= thrval)  // above threshold and
                 && getCell(ipos) != cnum)     { // not yet scanned...
	        //record its location as an anchor and come back later.
                seta++;

                if ((seta % blocksize) == 0) { 

                  //current block out of memory: allocate new memory block

                  setblock++;
                  if ((setblock % pageexpsize == 0) && setblock) {
                    aindex.resize(((setblock/pageexpsize)+1)*pageexpsize);
	          }
                  aindex[setblock] = new Matrix<Int>(blocksize,itsDim);
                }

                //set new anchor
                for (uInt axis = 0; axis < itsDim; axis ++) {
                  (*(aindex[setblock]))(seta%blocksize,axis) = ipos(axis);
		}

                //cout<<'A'<<seta<<'['<<x+xi<<','<<y+yi<<','<<z+zi<<']'<<endl;

	        setCell(ipos, cnum);
              }
            }  
          }
        }
      }     
      geta++;

      
      if (((geta % blocksize) == 0) || (geta>seta)) {
        if (getblock>=0) {
          //memory block data obsolete: delete old memory block
          delete aindex[getblock];
	}
        getblock++;
      }

      if (geta <= seta) {
        //go to lowest anchor
        for (uInt axis = 0; axis < itsDim; axis++) {
          pos(axis) = (*(aindex[getblock]))(geta%blocksize,axis);
	}
        //cout << ">>A" << geta << "  " << endl;
      }

    } while (geta <= seta);
  }
}

template <class T>
void ImageDecomposer<T>::decomposeImage(T thresholdVal, T maximumError)
{

// Find contiguous regions via thresholding

  uInt nRegions = identifyRegions(thresholdVal);
  cerr << "Found " << nRegions << " regions" << endl;

// Fit each region.  A further pass is done through each region
// to ascertain whether there are multiple components to be
// fit within it.

  fitRegions(maximumError);
}



template <class T>
void ImageDecomposer<T>::decomposeImage(T thresholdVal, 
                                        uInt nContour,
                                        T maximumError,
                                        Bool varyContours=True)
{
  const Bool showProcess = False;

// Make a local decomposer

  ImageDecomposer<T> thresholdMap(*itsImagePtr);


// Generate global contours

  Vector<T> mainContours(nContour);
  if (!varyContours) {
     mainContours = autoContour(nContour, thresholdVal);
     //cerr << "Main contours = " << mainContours << endl;
     if (showProcess) displayContourMap(mainContours);
  }

// Find contiguous regions via thresholding

  uInt nRegions = thresholdMap.identifyRegions(thresholdVal);
  if (showProcess) {
    cout << "Located " << nRegions << " regions above threshold "
         << thresholdVal << "." << endl;
    thresholdMap.display();
  }


// Find a blc and a trc for each region

  Block<IPosition> blc(nRegions);
  Block<IPosition> trc(nRegions);
  thresholdMap.boundRegions(blc, trc);
  if (isDerived()) zero();
//
  if (showProcess) {
    cout << "Bounded " << nRegions <<" regions for decomposition and fitting:" 
         << endl;
    for (uInt r = 0; r < nRegions; r++) {
      cout << r+1 <<": " << blc[r] << trc[r] << endl;;
    }
  }

// For each distinct region above the threshold, form a componentmap
// (subcomponentmap) and perform the fitting.  Regions are treated as
// independent entities by the fitter - even if one region contains a 
// very high-sigma component that may extend into another region, this
// is not taken into account by the other region

  for (uInt r=0; r<nRegions; r++) {

    // Make a decomposer for this region

    Slicer sl(blc[r], trc[r]-blc[r], Slicer::endIsLength);
    SubImage<T> subIm(*itsImagePtr, sl);
    ImageDecomposer<T> subpmap(subIm);

    // Flag pixels outside the target region (this makes sure that other
    // regions that happen to overlap part of the target region's bounding
    // rectangle are not counted twice, and that only the target region pixels
    // are used in fitting.)
    {
      IPosition pos(subpmap.itsDim,0); decrement(pos);
      while (increment(pos,subpmap.shape())) {     
        if (thresholdMap.getCell(blc[r] + pos) != Int(r+1)) {
          subpmap.setCell(pos, MASKED);
	}
      }
    }
    
    Vector<T> subContours(nContour);
    Vector<T> *contourPtr;
 
// Generate contours for this region or use global 
// ones for entire image

    if (varyContours)  {
      subContours = subpmap.autoContour(nContour, thresholdVal); 
      contourPtr = &subContours;
    } else {
      contourPtr = &mainContours;
    }
//
    if (showProcess)  {    
      cout << "-----------------------------------------" << endl;
      cout << "Subimage " << r << endl;
      cout << "Contour Map:" << endl;
      subpmap.displayContourMap(*contourPtr);
    }

// Deblend components in this region

    subpmap.deblendRegions(*contourPtr);   
    if (showProcess) {
      cout << "Component Map:" << endl;
      subpmap.display();
    }

// Fit gaussians to region

    subpmap.fitComponents(maximumError);  
    if (showProcess) {
      cout << "Object " << r+1 << " subcomponents: " << endl;
      subpmap.printComponents(); 
      cout << endl;
    }

// Add this region back into the main component map

    synthesize(subpmap, blc[r]);     
  }
//
  return;
}

template <class T>
void ImageDecomposer<T>::deblendRegions(const Vector<T>& contours)
{

// Performs the contour decomposition on a blended image to generate a 
// component map that can detect components blended above any threshold(s),
// by performing threshold scans at each contour level and recognizing
// as individual any components that are distinct above any such level.

  Int const printIntermediates = 0;
  Int const blocksize = 3;
  Int ncontours = contours.nelements(); 

// Holds components found in individual contours
// it is not a contour map, it is a component
// map thresholded at the current contour value.)
// This is the 'lower contour'.

  ImageDecomposer<T> contourMap(*this);

// Coordinates of the first pixel found in each component (a rough estimate of the center)

  Block<IPosition> regcenter; 
  if (isDerived()) zero();

// Component decomposition:
// This is the main deblending algorithm.  The program starts by performing
// a threshold scan at the top contour value, separating any regions
// that are visibly distinct at that threshold.  It continues downward though
// the contour vector, forming a component map from each contour and comparing
// that with the contour map that is gradually forming to distinguish new
// regions, correspondent regions, and blending regions, and assigns the
// pixels in the main itsMapPtr based on this information.

  for (Int c = ncontours-1; c >= 0; c--) {
    if (printIntermediates == 2) cout << endl << "CONTOUR " << c << endl;
//
    Int lowreg, highreg;   //number of regions in lower contour, current pmap
    contourMap.clear();    //only necessary if region grows between contours
//
    lowreg = contourMap.identifyRegions(contours(c));
    highreg = itsNRegions;          
//
    if (printIntermediates)  {
      cout << "Now comparing current pmap to contour " << c << '.' << endl;
      display();
      contourMap.display();
    }

// Indicates corresponding object in lower contour

    Vector<Int> root(highreg+1, 0);

// Total number of different objects above this region

    Vector<Int> nOffspring(lowreg+1, 0);

// Region IDs of objects above this region

    Block<Vector<Int>*> offspring(lowreg+1);

// Can't finish allocation until nOffspring is known.
    
// Scan through current pmap ("higher contour") and find the root of all
// regions as defined in the lower contour.  Simultaneously mark all
// regions in the lower contour that have "offspring" above.

    for (Int r = 1; r <= highreg; r++)    {
      IPosition pos(itsDim,0); decrement(pos);
      while (increment(pos,shape()) && !root(r)){
        if (getCell(pos) == r) {
          root(r) = contourMap.getCell(pos);
          nOffspring(contourMap.getCell(pos)) += 1;
        }
      }
    }

// Set up offspring array

    for (Int r = 1; r <= lowreg; r++) {
      offspring[r] = new Vector<Int>(nOffspring(r));
    }
// 
    for (Int lr = 1; lr <= lowreg; lr++) {
      Int f = 0;
      for (Int hr = 1; hr <= highreg; hr++) {
        if (root(hr) == lr) (*offspring[lr])(f++) = hr;
      }
    }
//
    if (printIntermediates == 2) {
      cout << "Contour Level " << c << endl;
      cout << highreg << ' ' << lowreg << endl;

      for (Int hr = 1; hr <= highreg; hr++) {
        cout << "root of " << hr << ':' << root(hr) << endl;
      }
//    
      for (Int lr = 1; lr <= lowreg; lr++) {
        for (Int f = 0; f < nOffspring(lr); f++) {
          cout << "offspring of " << lr << ':' << (*offspring[lr])(f) << endl;
        }
      }
    }

// The object/region/component labeling is done in three steps to make the 
// order most closely match the order in which they were found, and the 
// order of the peak intensities.

// First (and most complex) step is to deblend the large consolidated 
// regions (nOffspring >= 2).  The algorithm scans all pixels that 
// exist in the new contour but not in the current pmap and belong to 
// a region with multiple offspring, then scans the surrounding vicinity
// in the current pmap until it finds a pixel belonging to a region there,
// to which the new pixel is assigned. 

    Int cgindex = 1;  //component index base, global for this contour
    ImageDecomposer<T> copyMap(*this); //The original data must be temporarily
                                       //preserved while the componentmap is 
                                       //overwritten for comparisons between 
                                       //nearby pixels
//
    for (Int lr = 1; lr <= lowreg; lr++) {
      if (nOffspring(lr) >= 2) {
        IPosition pos(itsDim,0); decrement(pos);
        while (increment(pos,shape())) {

// Renumber pixels that already exist in the pmap

          if ((contourMap.getCell(pos)==lr)&&(copyMap.getCell(pos))) {  
            for (Int f = 0; f < nOffspring(lr); f++) {

// Translate old pmap id to new pmap id

              if ((*offspring[lr])(f) == copyMap.getCell(pos)) setCell(pos, cgindex+f);
	    }
	  }

// Number new pixels.

          if ((contourMap.getCell(pos)==lr)&&(!copyMap.getCell(pos))) {
            Int pinh = 0;  //region as defined in pmap pixel is set to
            Int srad = 1;  //search radius
            uInt naxis = 1;

// cout << "Searching for nearest cell to " << pos << endl;

            while(!pinh && srad < 250) {    //search increasing naxis, srad

// IMPR: an N-dimensional structure would be better here. 

	      Int xi, yi, zi;
	      Int ximin = (pos(0)-srad < 0)?                   -pos(0) : -srad;
              Int ximax = (pos(0)+srad >= shape(0))? shape(0)-pos(0)-1 :  srad;
              Int yimin = (pos(1)-srad < 0)?                   -pos(1) : -srad;
              Int yimax = (pos(1)+srad >= shape(1))? shape(1)-pos(1)-1 :  srad;
              Int zimin = 0, zimax = 0;
              if (itsDim == 2) {
                 zimin = 0; zimax = 0;
              }
              if (itsDim >= 3) {
                zimin = (pos(2)-srad < 0)?                   -pos(2) : -srad;  
                zimax = (pos(2)+srad >= shape(2))? shape(2)-pos(2)-1 :  srad;  
	      }
              while (!pinh && naxis <= itsDim) {
                for (xi = ximin; xi <= ximax; xi++) {
                  for (yi = yimin; yi <= yimax; yi++) {
                    for (zi = zimin; zi <= zimax; zi++) {
                      IPosition ipos(pos);
                      ipos(0) += xi; ipos(1) += yi; 
                      if (itsDim==3) ipos(2) += zi;

                      if (abs(xi)<srad && abs(yi)<srad && abs(zi)<srad) {
                        continue; //border of radius only
                      }

                      if   ( ((naxis==1) && (xi&&yi || xi&&zi || yi&&zi))
		          || ((naxis==2) && (xi && yi && zi))) {
		        continue;
                      }
//                            
                      Int inh = copyMap.getCell(ipos);
                      if (inh<=0) continue;
                      if (!pinh) {
                        pinh = inh;
                        for (Int f = 0; f < nOffspring(lr); f++) {

// Translate old pmap id to new pmap id

                          if ((*offspring[lr])(f) == inh) {
                            setCell(pos, cgindex+f);
			  }
                                //reassign pixel to new component
	      	        }
                      } else if (pinh!=inh)  {  //equidistant to different objects

// Temporarily flag as nonexistant object

                        setCell(pos, INDETERMINATE);
                      }
                    }
                  }
	        }
		naxis++;
	      }	         
              naxis = 1; srad++;    
            }       
          }
        }
        cgindex += nOffspring(lr);
      }
    }

// Now scan nonforked regions that exist in both contours.
// This is as simple as just renumbering the region.

    for (Int lr = 1; lr <= lowreg; lr++) {
      if (nOffspring(lr) == 1) {
        IPosition pos(itsDim,0); 
        decrement(pos);
        while (increment(pos,shape()))	{
          if (contourMap.getCell(pos) == lr) setCell(pos, cgindex);
	}
	cgindex++;
      }
    }  //IMPR: probably can make this work with single scan 
       //similar to above algorithm

// Finally, scan regions that only exist in lower contour. Same as above,
// but since these are new regions, add their initial positions to the seed
// arrays and increment the region count.

    for (Int lr = 1; lr <= lowreg; lr++)  { 
      if (nOffspring(lr) == 0) {
        IPosition newregioncenter(itsDim,0);
        uInt topcells = 0;
//
        {
          IPosition pos(itsDim,0); 
          decrement(pos);
          while (increment(pos,shape())) {
            //cout << pos << endl;
            if (contourMap.getCell(pos) == lr) {
              setCell(pos, cgindex);
              newregioncenter += pos;
              topcells++;
            }
	  }
	}
        newregioncenter /= topcells;  //note: integer division. may or may not
	                              //want to keep this

	cgindex++;

        if ((itsNRegions % blocksize) == 0) {
          regcenter.resize(itsNRegions+blocksize, True);
	}

// Add to region center array

        regcenter[itsNRegions] = newregioncenter;
        itsNRegions++;
      }
    }   
  } 
  

// At end of scan, assign all flagged pixels to region containing the 
// nearest "seed" - the first point identified in each region.

  if (printIntermediates == 2) {
    cout << "Located the following seeds:" << endl;
    for (uInt s = 0; s < itsNRegions; s++)
      cout << s << " at "  << regcenter[s]
           << " in component #" << getCell(regcenter[s]) << endl;
  }

  {
    IPosition pos(itsDim,0); 
    decrement(pos);
    while (increment(pos,shape())) {
      if (getCell(pos) == INDETERMINATE) {
	Int mindistsq = 1073741823;  //maximum Int value
        for (uInt s = 0; s < itsNRegions; s++) {
          Int distsq = 0;
          for (uInt a = 0; a < itsDim; a++) {
            distsq += (pos(a) - regcenter[s](a)) * (pos(a) - regcenter[s](a));
	  }
          if (distsq < mindistsq) {
            setCell(pos, getCell(regcenter[s]) );
	    mindistsq = distsq;
	  }
	}
      }   
    }
  }  
  return;
}



//The numRegions and numComponents functions are both getters for their
//respective variables and flags telling whether the image has been derived
//and/or decomposed.

template <class T>
uInt ImageDecomposer<T>::numRegions() const
{
  return itsNRegions;
}

template <class T>
uInt ImageDecomposer<T>::numComponents() const
{
  return itsNComponents;
}

template <class T>
Bool ImageDecomposer<T>::isDerived() const
{
  return itsNRegions>0;
}

template <class T>
Bool ImageDecomposer<T>::isDecomposed() const
{
  return itsNComponents>0;
}

template <class T>
Matrix<T> ImageDecomposer<T>::fitRegion(Int nregion, T maximumError)
{
cerr << "Fit Region " << nregion << endl;

// Fits multiple gaussians to a single region.  First performs  a local 
// maximum scan to estimate the number of components in the region.

  uInt nGaussians = 0;
  uInt npoints = 0;
  uInt ngpar = 0;
  if (itsDim == 2) ngpar = 6; 
  if (itsDim == 3) ngpar = 9;
  if (!isDerived()) nregion = 0;  //fit to all data.

// Determine number of data points in the region

  {
    IPosition pos(itsDim,0); 
    decrement(pos);
    while (increment(pos,shape())) {
      if (getCell(pos) == nregion) npoints++;
    }
  }

// Fill data and positions arrays

  Matrix<T> positions(npoints,itsDim);
  Vector<T> dataValues(npoints);
  Vector<T> sigma(npoints);  
  sigma = 1.0;
  uInt k = 0;
  {
    IPosition pos(itsDim,0); 
    decrement(pos);
    while (increment(pos,shape())) {
      if (getCell(pos) == nregion) {
        for (uInt i = 0; i<itsDim; i++) {
          positions(k,i) = T(pos(i));
	}
        dataValues(k) = getImageVal(pos);    
        k++;
      }
    }
  }

// Estimate the initial parameters.

  Matrix<T> width;
  Vector<T> maxval;
  Block<IPosition> maxvalpos;

// This estimates whether there are multiple components
// in the current region or not.

  findRegionLocalMax(maxval, maxvalpos, nregion, 2);
  estimateComponentWidths(width, maxvalpos);
  nGaussians = maxval.nelements();
  cerr << "Found " << nGaussians << " components" << endl;
//
  Matrix<T> initestimate(nGaussians, ngpar);
  Matrix<T> solution(nGaussians, ngpar);
//
  if (itsDim == 2) {
    for (uInt r = 0; r < nGaussians; r++) {
      initestimate(r,0) = maxval(r);
      initestimate(r,1) = maxvalpos[r](0);
      initestimate(r,2) = maxvalpos[r](1);
      initestimate(r,3) = width(r,1);
      initestimate(r,4) = width(r,0)/width(r,1);
      initestimate(r,5) = 0;
    }
  }
  if (itsDim == 3) {
    for (uInt r = 0; r < nGaussians; r++)  {
      initestimate(r,0) = maxval(r);
      initestimate(r,1) = maxvalpos[r](0);
      initestimate(r,2) = maxvalpos[r](1); 
      initestimate(r,3) = maxvalpos[r](2);        
      initestimate(r,4) = width(r,0); 
      initestimate(r,5) = width(r,1);
      initestimate(r,6) = width(r,2);
      initestimate(r,7) = (0.0);
      initestimate(r,8) = (0.0);
    }
  }

/*
cerr << "Data = " << dataValues << endl;
cerr << "pos = " << positions  << endl;
*/

// Fit for nGaussians simultaneously

  solution = fitGauss(positions, dataValues, nGaussians, initestimate, maximumError);
  return solution;  

}

template <class T>
void ImageDecomposer<T>::fitRegions(T maximumError)
{
// Fits gaussians to an image; multiple gaussians per region in the pmap.
// The regions are fit sequentially and independently, so this function 
// can be used on the main image.
// If the map is not yet thresholded, will fit to the entire image as if it
// were a single composite object, which will be very slow.

  uInt ngpar = 0;
  if (itsDim == 2) ngpar = 6; 
  if (itsDim == 3) ngpar = 9;

  if (itsNRegions == 0)  { //not deblended.
    itsList = fitRegion(0, maximumError);
    return;
  }
//
  for (uInt r = 1; r <= itsNRegions; r++) {
    Matrix<T> subitsList;
    Matrix<T> olditsList;
    subitsList = fitRegion(r, maximumError);
    olditsList = itsList;
    itsList.resize(itsNComponents + subitsList.nrow(), ngpar);
//
    for (uInt c = 0; c < itsNComponents; c++) {
      for (uInt p = 0; p < ngpar; p++) {
        itsList(c,p) = olditsList(c,p);
      }
    }

    for (uInt subc = 0; subc < subitsList.nrow(); subc++) {
      for (uInt p = 0; p < ngpar; p++) {
        itsList(itsNComponents+subc, p) = subitsList(subc, p);
      }
    }
    itsNComponents += subitsList.nrow();
  }

  return;
}


template <class T>
void ImageDecomposer<T>::fitComponents(T maximumError)  
{
// Fits gaussians to an image; one gaussian per region in the pmap.
// This function is intended to be used only by ImageDecomposer on its
// intermediary subimages; using it at higher level will execute a full
// gaussian fit on the main image and will be extremely slow. Every 
// nonflagged object pixel in the image is used in fitting.

// If the deblended flag is True, the function will treat each region as
// an individual component and will fit that many gaussians to the image

  uInt ngaussians; 
  uInt ngpar = 0;
  if (itsDim == 2) ngpar = 6; 
  if (itsDim == 3) ngpar = 9;
  uInt npoints = 0;
  if (!isDerived()) {
    throw(AipsError("Cannot fit until components are deblended"
                    " - use identifyRegions() or deblendRegions()"));
  }

// Determine number of data points in all the regions
// and get data and position vectors

  {
    IPosition pos(itsDim,0); 
    decrement(pos);
    while (increment(pos,shape())) {
      if (getCell(pos) > 0) npoints++;
    }
  }
  Matrix<T> positions(npoints,itsDim);
  Vector<T> dataValues(npoints);
  {
    IPosition pos(itsDim,0); 
    decrement(pos);
    uInt p = 0;
    while (increment(pos,shape())) {
      if (getCell(pos) > 0) {
        for (uInt i=0; i<itsDim; i++) {
          positions(p,i) = T(pos(i));
	}
        dataValues(p) = getImageVal(pos);
        p++;
      }
    }
  }

// Estimate the initial parameters.

  Matrix<T> width;
  Vector<T> maxval;
  Block<IPosition> maxvalpos;

  ngaussians = itsNRegions;
  findAllRegionGlobalMax(maxval, maxvalpos);
  estimateComponentWidths(width, maxvalpos);

  //for (uInt r = 0; r < ngaussians; r++)
  //  cout << width.row(r) << endl;

    //IMPR:
    //It would also be helpful for the purposes of estimating the rotation
    //parameters to determine the moments.  Jarvis and Tyson 1981 (AJ) has
    //2D formulae for how to do this, which could be used to estimate theta
    //(by collapsing the z axis; i.e. ignoring it in the calculation.) 
    //I think this could be extended to estimate phi by collapsing the xy axes
    //onto an axis determined by theta as well.  It may or not be worth it.


  Matrix<T> initestimate(ngaussians, ngpar);
  Matrix<T> solution(ngaussians, ngpar);
//
  if (itsDim == 2) {
    for (uInt r = 0; r < ngaussians; r++) {
      initestimate(r,0) = maxval[r];
      initestimate(r,1) = maxvalpos[r](0);
      initestimate(r,2) = maxvalpos[r](1);
      initestimate(r,3) = width(r,1);
      initestimate(r,4) = width(r,0)/width(r,1);
      initestimate(r,5) = 0.0;
    }
  } else if (itsDim == 3) {
    for (uInt r = 0; r < ngaussians; r++) {
      initestimate(r,0) = maxval[r];
      initestimate(r,1) = maxvalpos[r](0);
      initestimate(r,2) = maxvalpos[r](1); 
      initestimate(r,3) = maxvalpos[r](2);        
      initestimate(r,4) = width(r,0); 
      initestimate(r,5) = width(r,1);
      initestimate(r,6) = width(r,2);
      initestimate(r,7) = (0.0);
      initestimate(r,8) = (0.0);
    }
  }

  //cout << "Estimated parameters for component Gaussians:" << endl;
  //for (uInt r = 0; r < ngaussians; r++)
  //  cout << 'C' << r << ": " << initestimate.row(r) << endl;


  solution = fitGauss(positions, dataValues, ngaussians, initestimate, 
                      maximumError);
//
  itsNComponents = ngaussians;     
  itsList.resize(solution.shape());
  itsList = solution;

  return;
}

template <class T>
Matrix<T> ImageDecomposer<T>::fitGauss(const Matrix<T>& positions, 
                                       const Vector<T>& dataValues, 
                                       uInt ngaussians,
                                       const Matrix<T>& initestimate,
                                       T maximumError) const
{
// Fits the specified number of 3D gaussians to the data, and returns 
// solution in image (world) coordinates.
  
  uInt ngpar = 0;
  if (itsDim == 2) ngpar = 6; 
  if (itsDim == 3) ngpar = 9;

  //IMPR: ngaussians parameter probably redundant - use initestimate.nrows()
  //IMPR: The success rate of fitting with a given set of initial estimates
  // is often less than 50%.  Even a large retry matrix will occasionally fail
  // given a large enough data set.  The retry selection system needs to be 
  // automated,  preferably using a more sophisticated method than the near-
  // arbitrary choices for retry values uses here.  Such a change should be
  // implemented in FitGaussian, not here.

  // In case of circular gaussians, it might also be a good idea to fix
  // the angle parameters if the result diverges with reasonable chisq.

  Matrix<T> rt(7,ngpar);//matrix used to modify initial estimates on failure
  rt.column(0) = 1;  
  if (itsDim == 2) {
    rt.column(1) = 0;
    rt.column(2) = 0;
    rt(0,3) = 1;    rt(0,4) = 0.6;  rt(0,5) = 0;
    rt(1,3) = 0.5;  rt(1,4) = 1;    rt(1,5) = 0;
    rt(2,3) = 1;    rt(2,4) = 1;    rt(2,5) = 0.52;
    rt(3,3) = 1;    rt(3,4) = 1;    rt(3,5) = -0.52;
    rt(4,3) = 1.5;  rt(4,4) = 1;    rt(4,5) = 0;
    rt(5,3) = 1;    rt(5,4) = 0.6;  rt(5,5) = 0.52;
    rt(6,4) = 1;    rt(6,4) = 0.6;  rt(6,5) = -0.52;
  }
  if (itsDim == 3) {
    rt.column(1) = 0;
    rt.column(2) = 0;
    rt.column(3) = 0;
    rt(0,4) = 1;   rt(0,5) = 0.7; rt(0,6) = 0.4; rt(0,7) = 0;   rt(0,8) = 0;
    rt(1,4) = 0.3; rt(1,5) = 0.3; rt(1,6) = 0.3; rt(1,7) = 0;   rt(1,8) = 0;
    rt(2,4) = 1;   rt(2,5) = 1;   rt(2,6) = 1;   rt(2,7) = 0.5; rt(2,8) = 0;
    rt(3,4) = 1;   rt(3,5) = 1;   rt(3,6) = 1;   rt(3,7) = 0;   rt(3,8) = 0.5;
    rt(4,4) = 1;   rt(4,5) = 1;   rt(4,6) = 1;   rt(4,7) =-0.5; rt(4,8) = 0;
    rt(5,4) = 1;   rt(5,5) = 1;   rt(5,6) = 1;   rt(5,7) = 0.5; rt(5,8) = 0.5;
    rt(6,4) = 1;   rt(6,5) = 1;   rt(6,6) = 1;   rt(6,7) =-0.5; rt(6,8) =-0.5;
  }
//
  return fitGauss(positions, dataValues, ngaussians, initestimate, rt, maximumError);
}


template <class T>
Matrix<T> ImageDecomposer<T>::fitGauss(const Matrix<T>& positions, 
                                       const Vector<T>& dataValues, 
                                       uInt ngaussians,
                                       const Matrix<T>& initestimate,
				       const Matrix<T>& retrymatrix,
                                       T maximumError) const
                                         
{
// Fits the specified number of 3D gaussians to the data, and returns 
// solution in image (world) coordinates.
// IMPR: ngaussians parameter probably redundant - use initestimate.nrows()
  
  Matrix<T> solution;
  T chisquare;

// Might be useful to send to screen in AIPS++
// cout << "Primary estimation matrix:" << endl;
// for (uInt r = 0; r < ngaussians; r++)
//     cout << initestimate.row(r) << endl;
  
  
  FitGaussian<T> fitter(itsDim,ngaussians);
  fitter.setFirstEstimate(initestimate);
  fitter.setRetryFactors(retrymatrix);

  try{ 
    solution = fitter.fit(positions, dataValues, maximumError);
  } catch (AipsError fiterr) {
    string errormsg;
    errormsg = fiterr.getMesg(); 
    cout << errormsg << endl;
    cout << "Fitting failed." << endl;
    solution = 0;
    chisquare = -1.0;
    return solution;
  }

  if (fitter.converged()) {  
    chisquare = fitter.chisquared();
  } else {
    cout << "Fitting did not converge " << endl;  
    solution = 0;
    chisquare = -1.0;
    return solution;
  }
//
  return solution;
}



template <class T>
void ImageDecomposer<T>::display() const
{
// Displays the componentmap in a terminal environment as an array of 
// characters on screen.  

  Int windowwidth = 80;
  Int const spacing = 4;
  Int const cposinc = shape(0) * 2 + spacing;
  if (cposinc > windowwidth) windowwidth = cposinc;
  Int cpos = 0;
//
  Int z = 0;
  Int benchz = 0;
//
  //cerr << "shape = " << shape() << endl;
  while (z < shape(2)) {
    for (Int y = 0; y < shape(1); y++) {
      z = benchz;
      while ((cpos += cposinc) < windowwidth && z < shape(2)) {
        for (Int x = 0; x < itsShape(0); x++) {
          if (getCell(x,y,z) >= 0) {
             cout << getCell(x,y,z);
          } else if (getCell(x,y,z) == INDETERMINATE) {
             cout << '*';
          } else  if (getCell(x,y,z) == MASKED)  {
             cout << '-';
	  }
          if (getCell(x,y,z) < 10) {
             cout << ' ';
          }
        }
        z++;
        cout << "    ";
      }
      cpos = 0;
      cout << endl;
    }
    benchz = z;
    cout << endl;
  }
  cout << endl;
  return;
}

template <class T>
void ImageDecomposer<T>::displayContourMap(const Vector<T>& clevels) const
{
// Displays the target image as a contourmap in a terminal environment as 
// an array of characters on screen.  

  Int windowwidth = 80;
  Int const spacing = 4;
  Int const cposinc = shape(0) * 2 + spacing;
  if (cposinc > windowwidth) windowwidth = cposinc;
  Int cpos = 0;

  Int z = 0;
  Int benchz = 0;
  cout << "Contour levels:" << clevels << endl;

  if (itsDim == 2) {
    for (Int y = 0; y < shape(1); y++) {
      for (Int x = 0; x < shape(0); x++) {
        if (getContourVal(x,y,clevels) >= 0) {
          cout << getContourVal(x,y,clevels); 
        }
        else if (getContourVal(x,y,clevels) <= -1) {
          cout << '-';
        }
        if (getContourVal(x,y,clevels) < 10) {
          cout << ' ';
        }
      }
      cout << endl;
    }
    cout << endl;
  }

  if (itsDim == 3){
    //this actually works in 2 dimensions on a TempImage, but not on a 
    //SubImage, where there is a failure inside the getImageVal command
    //on a failed assertion involving a LatticeRegion object.  As a result
    //the above specialization was written, but it would be nice if 3-D
    //IPositions worked on 2-D images in SubImage as well as TempImage.
  while (z < shape(2)) {
    for (Int y = 0; y < shape(1); y++) {
      z = benchz;
      while ((cpos += cposinc) < windowwidth && (z < shape(2))) {
        for (Int x = 0; x < shape(0); x++) {
          if (getContourVal(x,y,z,clevels) >= 0) {
            cout << getContourVal(x,y,z,clevels); 
          }
          else if (getContourVal(x,y,z,clevels) <= -1) {
            cout << '-';
          }
          if (getContourVal(x,y,z,clevels) < 10) {
             cout << ' ';
          }
        }
        z++;
        cout << "    ";
      }
      cpos = 0;
      cout << endl;
    }
    benchz = z;
    cout << endl;
  }
  cout << endl;
  }

  return;
}


template <class T>
void ImageDecomposer<T>::printComponents() const
{
  //Prints the components as formatted output on screen.
  //IMPR: Probably could be modified as an ostream output function.

  for (uInt g = 0; g < itsList.nrow(); g++)
  {
    cout << g+1 << ": ";
    if (itsList(g,0) == 0) {
       cout << "Failed"; 
    } else { 
      cout << "Peak: " << itsList(g,0) << "  ";

      if (itsDim == 2) {
        cout << "Mu: [" << itsList(g,1) 
             << ", " << itsList(g,2) << "]  ";
        cout << "Axes: [" << itsList(g,3)
             << ", " << itsList(g,3) * itsList(g,4) << "]  ";
        cout << "Rotation: " << itsList(g,5) /*  * 180 / C::pi  */;
      }
      if (itsDim == 3) {
        cout << "Mu: [" << itsList(g,1) 
             << ", " << itsList(g,2) 
             << ", " << itsList(g,3) << "]  ";
        cout << "Axes: [" << itsList(g,4)
             << ", " << itsList(g,5) 
             << ", " << itsList(g,6) << "]  ";
        cout << "Rotation: [" << itsList(g,7)/*  *180/C::pi */
             << ", " << itsList(g,8)         /*  *180/C::pi */ << "]";
      }
    }
    cout << endl;
  }
   
  return;
}


template <class T>
void ImageDecomposer<T>::correctBlcTrc(IPosition& blc, IPosition& trc) const
{
  //Ensures blc and trc correctly describe the limits of a rectangular block.

  Int t;
  for (uInt i = 0; i<itsDim; i++)  {
    if (blc(i)<0) blc(i) = 0;     
    if (trc(i)>shape(i)) trc(i) = shape(i);
    if (trc(i)<0) trc(i) = 0;     
//
    if (blc(i)>shape(i)) blc(i) = shape(i);
    if (blc(i)>trc(0)) {                      // nebk - ERROR should be trc(a) not trc(0) ??
       t = blc(i); 
       blc(i) = trc(i); 
       trc(i) = t;
    }
  }
  return;
}

template <class T>
Bool ImageDecomposer<T>::increment(IPosition& pos,const IPosition& limit) const
{
// N-Dimensional looping function: use in place of nested for loops
// Returns False when pos reaches limit.
// Use as follows:    while(increment(pos,limit))

  pos(itsDim-1)++;
  for (uInt i = itsDim-1; i>0; i--) { 
    if (pos(i) == limit(i)) {
       pos(i) = 0; 
       pos(i-1)++;
    } else {
       return True;
    }
  }
//
  if (pos(0) == limit(0)) {
     return False;
  } else {
     return True;
  }
}

template <class T>
void ImageDecomposer<T>::decrement(IPosition& pos) const
{
  //To ensure while loop starts at 0,0,0..., decrement before first increment
  pos(itsDim-1)--;
}


template <class T>                                   
void ImageDecomposer<T>::synthesize(const ImageDecomposer<T>& subdecomposer,
                                    IPosition blc)
{
// Overlays a smaller map onto an empty region of a larger map,
// and adds submap component list to main component list.

// The user should exercise caution with this function and synthesize submaps
// only into regions of the main map that are truly empty (0), because the 
// program does not perform any blending between components.  
// Otherwise, false detections are likely.

  uInt ngpar = 0;
  if (itsDim == 2) ngpar = 6; 
  if (itsDim == 3) ngpar = 9;

// Scan to the edge of the boundary of the host map or the submap, whichever
// is closer.

  IPosition scanlimit(itsDim);  //submap-indexed
  for (uInt i=0; i<itsDim; i++)  {
    if (subdecomposer.shape(i) > shape(i) - blc(i)) {
      scanlimit(i) = shape(i) - blc(i);
    } else {
      scanlimit(i) = subdecomposer.shape(i);  
    }
  }

// Write pixels in sub- component map to main component map.

  {
    IPosition pos(itsDim,0);  //submap-indexed
    decrement(pos);
    while (increment(pos,scanlimit)) {     
        if (subdecomposer.getCell(pos) > 0) {
          setCell(pos + blc, itsNRegions + subdecomposer.getCell(pos));
        }
    }
  }
  itsNRegions += subdecomposer.numRegions();

// Add components in subdecomposer to components in main decomposer.

  if (subdecomposer.isDecomposed())  { 

    Matrix<T> oldList;   
    oldList = itsList;
    itsList.resize(itsNComponents+subdecomposer.numComponents(),ngpar);
    for (uInt c = 0; c < itsNComponents; c++) {
      for (uInt p = 0; p < ngpar; p++) {
        itsList(c,p) = oldList(c,p);  //copy after resize
      }
    }

    for (uInt subc = 0; subc < subdecomposer.numComponents(); subc++) {
      for (uInt p = 0; p < ngpar; p++) {
        itsList(itsNComponents+subc,p)=subdecomposer.itsList(subc,p);     
      }
      // make adjustments to center values due to offset
      if (itsDim == 2) {
        itsList(itsNComponents+subc,1) += blc(0);
        itsList(itsNComponents+subc,2) += blc(1);
      } else if (itsDim == 3) {
        itsList(itsNComponents+subc,1) += blc(0);
        itsList(itsNComponents+subc,2) += blc(1);
        itsList(itsNComponents+subc,3) += blc(2);   
      }
    }
    itsNComponents += subdecomposer.numComponents();

  }
//
  //renumberRegions();     //this should have no effect unless this function
                           //was used to overwrite an entire region, which
                           //should be avoided.
}

/*
     RO_LatticeIterator<Int> otherIter(*(other.itsMapPtr));
     Bool deleteOther, deleteThis;
     const Int* pOther = 0;
     Int* pThis = 0;
     for (otherIter.reset(); !otherIter.atEnd(); otherIter++) {
        const Array<Int>& otherArray = otherIter.cursor();
        Array<Int> thisArray = itsMapPtr->getSlice(otherIter.position(), otherIter.cursorShape());
//
        pOther = otherArray.getStorage(deleteOther);
        pThis = thisArray.getStorage(deleteThis);
//
        for (uInt i=0; i<otherIter.cursor().nelements(); i++) {
           if (pOther[i] != regionID) pThis[i] = MASKED;
        }
//
        otherArray.freeStorage(pOther, deleteOther);
        thisArray.putStorage(pThis, deleteThis);
     }
*/


// currently (6/20) working on changing the TempLattice to a Lattice*
// making it compatible with both the TempLattice (main pmap) and SubLattice
// (subpmaps).   It may not work, however.




