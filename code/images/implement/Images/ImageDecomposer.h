//# ImageDecomposer.h: decompose images into components
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

#if !defined(TRIAL_IMAGEDECOMPOSER_H)
#define TRIAL_IMAGEDECOMPOSER_H

#include <aips/iostream.h>
#include <aips/math.h>
#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Containers/Block.h>
#include <aips/Functionals/Function1D.h>
#include <aips/Lattices/TempLattice.h>
#include <trial/Lattices/SubLattice.h>
#include <trial/Images/ImageInterface.h>


// <summary>
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="tImageDecomposer.cc">
// </reviewed>
//
// <prerequisite>
// <list>
//   <item> <linkto class=CoordinateSystem>CoordinateSystem</linkto>   
//   <item> <linkto class=ImageInterface>ImageInterface</linkto>
// </list>
// </prerequisite>

// <etymology>
// </etymology>

// <synopsis>
// The ImageDecomposer has all the main necessary functionality of the
// project, and is the only new class the end-user will see.
// Only the fitting is done elsewhere.
//
// The data structure of the ImageDecomposer is the "componentmap",
// a 3D TempImage of integers, where the value  of each integer indicates 
// the component to which that pixel belongs.
// The componentmap's top-level function is decomposeimage(), which
// takes an image and breaks it into fragments surrounding each
// distinct object, each with their own sub-componentmap, then applies the
// deblendregions() function to each fragment's  componentmap to separate the 
// individual components in each fragment. 
// (This function is similar to clfind, Williams et al
// 1994.)  The sub-componentmaps are then synthesized into a main map.
// It is also possible for the user to directly use deblendregions() or
// identifyregions() directly to form the componentmap , and then fitgauss3d() to 
// find the components; they have the same overall functionality.
//
// Most of the code is compatible with an N-dimensional image.  However,
// the contour decomposition and fitting functions are 2/3-D only.
//
//
//
//  DEFINITIONS of terms used in this code, which hopefully are used 
//  consistently in the code function/variable names and remarks.
// 
//  Region - a contiguous block of pixels holding one or more objects
//  Componentmap - a lattice of pixels (see above)  where the the value
//     of each pixel is an integer representing the Region to which it belongs.
//  Object - a consolidated block of pixels found by a single threshold scan.
//     Simple Objects contain just one Component.
//  Area - a group of pixels located in a specified rectagular subsection
//     of a pmap / Image.
//  Component - an individual single source in the image.  May refer either
//     to the pmap pixels or the 9 parameters used to model them.  Components
//     have no substructure, and are the base unit for Gaussian fitting.
//
// </synopsis>  
//
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//   
// <motivation>
// </motivation>
//
// <note>
// </note>
//
// <todo asof="1996/09/04">
//   <li> Generalize dimensionality
// </todo>
  


template <class T> class ImageDecomposer {

public:

   enum componentValues {
       INDETERMINATE=-1,
       MASKED=-2 
   };

// Default constructor.   Object is not viable until setImage called
   ImageDecomposer();

// Construct from image
   ImageDecomposer(ImageInterface<T>& image);

// Copy constructor.
   ImageDecomposer(const ImageDecomposer<T>& other);

// Assignment
   ImageDecomposer<T> &operator=(const ImageDecomposer<T> &other);

// Destructor
   ~ImageDecomposer();

// Set a new image.  Resets internal componentmap.
   void setImage (ImageInterface<T>& image);
//
// The simplest method of image decomposition. Initially, the image
// is decomposed into contiguous regions via a thresholding process.
// Then for each region, an estimate of the number of components
// in that region is made (by looking for local maxima) and a simultaneous 
// fit for that many components is made.
//
// The fit will be repeated until it converges to a result with 
// chisquared > chisqcriterion.
//
// Does not generate an accurate componentmap. 
  void decomposeImage(T thresholdval, T chisqcriterion); 

// The more complex method of image decomposition.  Uses successive
// contouring to assign each pixel to a specific component, then fits
// Gaussian components.  Components located in contiguous blocks are
// fitted together.  The thresholdVal specifies both the noise cutoff
// and the threshold at which components are fitted separately.  Each fit
// will be repeated until it converges to a result with chisquare >
// chiSqCriterion.  If varyContours=True, each contiguous component
// block is contoured separately depending on its regional maximum.
// If varyContours=False, all components are contoured based on the global
// maximum.
//
// The primary, highest-level function of the class; used to form a component
// map out of a standard AIPS++ image and automatically fit to the components.
// It first breaks the image down into regions based on the specified
// threshold value, then forms a contour map of each subimage and uses
// deblendregions to form component maps of each subimage, which are synthesized
// together to make the final map.
  void decomposeImage(T thresholdVal, uInt nContour, T chiSqCriterion, 
                      Bool varyContours=True);

//Retrieves the target image's value at the given location.
// <group>
  T getImageVal(Int x, Int y) const;
  T getImageVal(Int x, Int y, Int z) const;
  T getImageVal(IPosition coord) const;
// </group>
//
  uInt numRegions() const;
  uInt numComponents() const;
//
  void display() const;
  void displayContourMap(const Vector<T>& clevels) const;
  void printComponents() const;
//
private:
  ImageInterface<T> *itsImagePtr;  
  TempLattice<Int> *itsMapPtr;        // The actual component map.  
  IPosition itsShape;                 // Image shape
  uInt itsDim;                        // Image dimensions
  uInt itsNRegions;                   // Number of distinct regions in component map
  uInt itsNComponents;                // Number of components that have been fitted
  Matrix<T> itsList;              //Matrix containing parameters of the component 
                                  //gaussians - should always have size 
                                  //ncomponents x 9
// 
  IPosition shape() const;                    
  Int shape(uInt axis) const;                 
  Bool isDerived() const;
  Bool isDecomposed() const;  
  void correctBlcTrc(IPosition& blc, IPosition& trc) const;
  Bool increment(IPosition& pos, const IPosition& shape) const;
  void decrement(IPosition& pos) const;

// Returns the component to which the specified cell belongs
// <group>
  Int getCell(Int x, Int y) const;             
  Int getCell(Int x, Int y, Int z) const;    
  Int getCell(const IPosition& coord) const;
// </endgroup>

// Assigns the specified cell to the specified component
// <group>
  void setCell(Int x, Int y, Int sval);       
  void setCell(Int x, Int y, Int z, Int sval); 
  void setCell(const IPosition& coord, Int sval);
// </group>

// Semi-automatic way to set contour levels: at the given increment counting
// between mincon and maxcon.
  Vector<T> autoContour(T minCon, T maxCon, T inc) const;

// Linearly spaces contours between minvalue and just below the
// maximum value in the target region of the target image, and returns
// the contour values as a Vector.
  Vector<T> autoContour(Int nContours=11, T minValue=0) const;

// Nonlinear spacing option for contouring; spaces contours according to the
// function given.  The domain of the function is 0 <-> ncontours-1; the
// range is automatically calibrated to be minvalue<-> maxvalue.  The function
// should be nondecreasing in the domain such that each contour is greater
// than the last.
  Vector<T> autoContour(const Function1D<T>& fn,
                        Int nContours = 11, T minValue = 0) const;


// Overlays a smaller map onto an empty region of a larger map,
// and adds submap component list to main component list.
// The user should exercise caution with this function and synthesize submaps
// only into regions of the main map that are truly empty (0), because the
// program does not perform any blending between itsMapPtr components.  Otherwise,
// false detections are likely.
  void synthesize(const ImageDecomposer<T>& subdecomposer);
// 
// Set all elements in the itsMapPtr to zero and clear the component list.
  void zero();

// Set all nonmasked elements in itsMapPtr to zero and clear the component list.
  void clear();

// Eliminates redundant regions (components with no representative cells in
// the component map) by renumbering higher-numbered regions to fill in
// the gaps.  For example..
// 011          011
// 113  becomes 112
// 113          112
  void renumberRegions();

// Boxes each region in the componentmap:
// blc is set to the lowest coordinate value in each region;
// trc is set to one above the highest coordinate value in each region.
  void boundRegions(Block<IPosition>& blc, Block<IPosition>& trc);

// Finds the greatest value inside the specified rectangular area of the
// target image.
// <group>
  T findAreaGlobalMax(IPosition blc, IPosition trc) const;
  void findAreaGobalMax(T& maxval, IPosition& maxvalpos, 
                         IPosition blc, IPosition trc) const;
  void findAreaGlobalMax(T& maxval, IPosition& maxvalpos,
                         IPosition blc, IPosition trc) const;
  Vector<T> findAreaGlobalMax(IPosition blc, IPosition trc, Int naxis) const;
  void findAreaGlobalMax(Vector<T>& maxvals,
                         Block<IPosition>& maxvalpos,
                         IPosition blc, IPosition trc,
                         Int naxis) const;
// </group>

// Finds all local maxima inside the specified rectangular area of the
// target image.
  Vector<T> findAreaLocalMax(IPosition blc, IPosition trc, Int naxis) const;
  void findAreaLocalMax(Vector<T>& maxvals,Block<IPosition>& maxvalpos, 
                        IPosition blc, IPosition trc, Int naxis) const;

// Finds the maximum value of the target image in each region of the
// componentmap.
  Vector<T> findAllRegionGlobalMax() const;
  void findAllRegionGlobalMax(Vector<T>& maxvals, 
                              Block<IPosition>& maxvalpos) const;

// Finds all local maxima of the target image inside the specifed region
// of the componentmap.
  Vector<T> findRegionLocalMax(Int nregion, Int naxis) const;
  void findRegionLocalMax(Vector<T>& maxvals, Block<IPosition>& maxvalpos, 
                          Int nregion, Int naxis) const;

  //Compares specified pixel to adjacent pixels to determine if it is
  //greatest in local pixel block.
  //2D:
  //naxis = 1: compare to 4 adjacent pixels (axes only)
  //naxis = 2: compare to 8 adjacent pixels (axes and diagonals)
  //3D:
  //naxis = 1: compare to 6 adjacent pixels (axes only)
  //naxis = 2: compare to 18 adjacent pixels (axes and 2-axis diagonals)
  //naxis = 3: compare to 26 adjacent pixels (axes and 2/3-axis diagonals)
  Bool isLocalMax(const IPosition& pos, Int naxis) const;
//
  Bool isLocalMax(Int x, Int y, Int naxis) const;
  Bool isLocalMax(Int x, Int y, Int z, Int naxis) const;
//
  void estimateComponentWidths(Matrix<T>& width,
                               const Block<IPosition>& maxvalpos) const;

// Performs a single threshold scan on the image.  In other words,
// identifies all contigous blocks of pixels in the target image above the
// threshold value thrval, assigning each unique block to an integer,
// starting at one.  All pixels with target image values below thrval are set
// to zero.
  uInt identifyRegions(T thrval, Int naxis = 2);
  void deblendRegions(const Vector<T>& contours);

// Retrieves the number of the highest contour with a value less then the
// target image's value at the given location.
  Int getContourVal(IPosition coord, const Vector<T>& clevels) const;
  Int getContourVal(Int x, Int y, Int z, const Vector<T>& clevels) const;
  Int getContourVal(Int x, Int y, const Vector<T>& clevels) const;
//
  Matrix<T> fitRegion(Int region, T chisqcriterion);
//
  void fitRegions(T chisqcriterion);
  void fitComponents(T chisqcriterion);
//
  Matrix<T> fitGauss(const Matrix<T>& positions, 
                     const Vector<T>& dataValues,
                     uInt ngaussians,
                     const Matrix<T>& initestimate, T chisqcriterion) const;

//
  Matrix<T> fitGauss(const Matrix<T>& positions,
                     const Vector<T>& dataValue, 
                     uInt ngaussians,
                     const Matrix<T>& initestimate, 
                     const Matrix<T>& retrymatrix, T chisqcriterion) const;

};
#endif
