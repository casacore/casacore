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

//#Includes
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
// A tool to separate a complex image into individual components.
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
// It takes an image, and separates it into components.
// </etymology>

// <synopsis>
// ImageDecomposer is an image decomposition tool that performs several tasks,
// with the end result being that a strongly blended image is separated into
// components - both in the sense that it determines the parameters for each
// parameter (assuming a Gaussian model) and that is physically assigns each
// pixel in the image to an individual object.  The products of these two
// operations are called the component list and the component map, 
// respectively.  The fitting process (which determines the component list) and
// the pixel-decomposition process (which determines the component map) are
// designed to work cooperatively to increase the efficiency and accuracy of
// both.
// 
// The algorithm between the decomposition is based on the function clfind
// described in Williams et al 1994, which uses a contouring procedure whereby
// a closed contour designates a separate component.  The program first 
// separates the image into clearly distint 'regions' of blended emission, then
// contours each region to determine the areas constituting each component and
// passes this information on to the fitter, which determines the component 
// list.  
//
// The software is compatible with 2 and 3 dimensional images, but is not
// yet structured for higher dimensions.
// </synopsis>

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
//

//
//
// <example>
// <srcblock>
//  TempImage<Double> image;
//  //(populate the image with data: see dImageDecomposer.cc)
//  ImageDecomposer<Double> id(image);
//  id.decomposeImage(0.1, 10, 0.1, 0);
//  id.display();
//  id.printComponents();
// </srcblock>
// </example>
//   
// <motivation>
// </motivation>
//
// <note>
// </note>
//
// <todo asof="2002/06/20">
//   <li> Generalize dimensionality 
//   <li> Numerous possible improvements to make are documented in the code.
// </todo>
  


template <class T> class ImageDecomposer {

public:

// 'Special' flag values for pixels in the component map.  An indeterminate
// pixel lies directly between two components and cannot be immediately 
// assigned.  A masked pixel is not inside the targeted region of the
// sub-componentmap and is not used in decomposition or fitting.
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

// Tell the decomposer what image to decompose ("target image")
// This resets internal component map.
   void setImage (ImageInterface<T>& image);

//
// The simplest method of image decomposition. Initially, the image
// is decomposed into contiguous regions via a thresholding process.
// Then for each region, an estimate of the number of components
// in that region is made (by looking for local maxima) and a simultaneous 
// fit for that many components is made.
//
// The fit will be repeated until it converges to a result with 
// RMS < maximumError, though if it cannot converge within 3 x 2^g iterations
// it will take the best result available.
//
// Does not generate an accurate componentmap. 
  void decomposeImage(T thresholdval, T chisqcriterion); 

// The more complex method of image decomposition.  Uses successive
// contouring to assign each pixel to a specific component, then fits
// Gaussians to the components.  Components located in contiguous blocks are
// fitted together.  The thresholdVal specifies both the noise cutoff
// and the threshold at which components are fitted separately.  Each fit
// will be repeated until it converges to a result with chisquare >
// chiSqCriterion.  If varyContours=True, each contiguous component
// block is contoured separately depending on its regional maximum.
// If varyContours=False, all components are contoured based on the global
// maximum.
  void decomposeImage(T thresholdVal, uInt nContour, T maximumError, 
                      Bool varyContours=True);

//Retrieves the target image's value at the given location.
// <group>
  T getImageVal(IPosition coord) const;
// </group>

// Returns the number of regions found in the image.
  uInt numRegions() const;

// Returns the number of components found in the image.
  uInt numComponents() const;
//
// Command-line text output functions; generally useful only for debugging.
// <group>
  void display() const;
  void displayContourMap(const Vector<T>& clevels) const;
  void printComponents() const;
// </group>
//
private:
  ImageInterface<T> *itsImagePtr;// Points to the target image.
  Lattice<Int> *itsMapPtr;       // The actual component map.  
  IPosition itsShape;            // Component map shape
  uInt itsDim;                   // Component map number of dimensions
  uInt itsNRegions;              // Number of distinct regions in component map
  uInt itsNComponents;           // Number of components that have been fitted
  Matrix<T> itsList;             // The component list (Gaussian parameters for
                                 // each component.)  

// Returns the shape of the component map.
  IPosition shape() const;                

// Returns the length of a specific axis.
  Int shape(uInt axis) const;            

// Returns True if the image has been thresholded (split up into regions.)
  Bool isDerived() const;

// Returns True if the image has been decomposed (split up into components.)
  Bool isDecomposed() const;  

// Makes sure a pair of IPositions is in the correct format for blc/trc, and
// corrects them if they are not.
  void correctBlcTrc(IPosition& blc, IPosition& trc) const;

// Used as an N-dimensional interator.  This should probably be replaced by
// LatticeIterators...?
// <group>
  Bool increment(IPosition& pos, const IPosition& shape) const;
  void decrement(IPosition& pos) const;
// </group>

//Retrieves the target image's value at the given location.
// <group>
  T getImageVal(Int x, Int y) const;
  T getImageVal(Int x, Int y, Int z) const;
// </group>

// Returns the component to which the specified cell belongs
// <group>
  Int getCell(Int x, Int y) const;             
  Int getCell(Int x, Int y, Int z) const;    
  Int getCell(const IPosition& coord) const;
// </group>

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
// range is automatically calibrated to be minvalue <-> maxvalue.  The function
// should be nondecreasing in the domain such that each contour is greater
// than the last.
  Vector<T> autoContour(const Function1D<T>& fn,
                        Int nContours = 11, T minValue = 0) const;


// Overlays a smaller map onto an empty region of a larger map,
// and adds submap component list to main component list.
// The user should exercise caution with this function and synthesize submaps
// only into regions of the main map that are truly empty (0), as no blending
// is assumed between different maps.
  void synthesize(const ImageDecomposer<T>& subdecomposer, IPosition blc);

// 
// Set all elements in the component map to zero and clear the component list.
  void zero();

// Set all nonmasked elements in the component map to zero and clear the 
// component list.
  void clear();

// Boxes each region in the componentmap:
// blc is set to the lowest coordinate value in each region;
// trc is set to one above the highest coordinate value in each region.
  void boundRegions(Block<IPosition>& blc, Block<IPosition>& trc);

// Finds the greatest value inside the specified rectangular area of the
// target image.
// <group>
  T findAreaGlobalMax(IPosition blc, IPosition trc) const;
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
// <group>
  Vector<T> findAreaLocalMax(IPosition blc, IPosition trc, Int naxis) const;
  void findAreaLocalMax(Vector<T>& maxvals,Block<IPosition>& maxvalpos, 
                        IPosition blc, IPosition trc, Int naxis) const;
// </group>

// Finds the maximum value of the target image in each region of the
// componentmap.
// <group>
  Vector<T> findAllRegionGlobalMax() const;
  void findAllRegionGlobalMax(Vector<T>& maxvals, 
                              Block<IPosition>& maxvalpos) const;
// </group>


// Finds all local maxima of the target image inside the specifed region
// of the componentmap.
// <group>
  Vector<T> findRegionLocalMax(Int nregion, Int naxis) const;
  void findRegionLocalMax(Vector<T>& maxvals, Block<IPosition>& maxvalpos, 
                          Int nregion, Int naxis) const;
// </group>


//Compares specified pixel to adjacent pixels to determine if it is
//greatest in local pixel block.
//2D:
//naxis = 1: compare to 4 adjacent pixels (axes only)
//naxis = 2: compare to 8 adjacent pixels (axes and diagonals)
//3D:
//naxis = 1: compare to 6 adjacent pixels (axes only)
//naxis = 2: compare to 18 adjacent pixels (axes and 2-axis diagonals)
//naxis = 3: compare to 26 adjacent pixels (axes and 2/3-axis diagonals)
// <group>
  Bool isLocalMax(const IPosition& pos, Int naxis) const;
  Bool isLocalMax(Int x, Int y, Int naxis) const;
  Bool isLocalMax(Int x, Int y, Int z, Int naxis) const;
// </group>

// Finds a rough estimate of the width of each component by scanning to find
// the full width at quarter maximum.
// Requires the location of each component.
  void estimateComponentWidths(Matrix<T>& width,
                               const Block<IPosition>& maxvalpos) const;

// Performs a single threshold scan on the image.  In other words,
// identifies all contigous blocks of pixels in the target image above the
// threshold value thrval, assigning each unique block to an integer,
// starting at one.  All pixels with target image values below thrval are set
// to zero.
  uInt identifyRegions(T thrval, Int naxis = 2);

// Performs the contour decomposition on a blended image to generate a 
// component map that can detect components blended above any threshold(s),
// by performing threshold scans at each contour level and recognizing
// as individual any components that are distinct above any such level.
  void deblendRegions(const Vector<T>& contours);

// Retrieves the number of the highest contour with a value less then the
// target image's value at the given location.
// <group>
  Int getContourVal(IPosition coord, const Vector<T>& clevels) const;
  Int getContourVal(Int x, Int y, Int z, const Vector<T>& clevels) const;
  Int getContourVal(Int x, Int y, const Vector<T>& clevels) const;
// <//group>

// Fits multiple gaussians to a single region.  First performs  a local 
// maximum scan to estimate the number of components in the region.
  Matrix<T> fitRegion(Int region, T maximumError);


// Fits gaussians to an image; multiple gaussians per region in the pmap.
// The regions are fit sequentially and independently, so this function 
// can be used on the main image.
// If the map is not yet thresholded, will fit to the entire image as if it
// were a single composite object, which will be very slow.
  void fitRegions(T maximumError);

// Fits gaussians to an image; one gaussian per region in the pmap.
// This function is intended to be used only by ImageDecomposer on its
// intermediary subimages; using it at higher level will execute a full
// gaussian fit on the main image and will be extremely slow. Every 
// nonflagged object pixel in the image is used in fitting.

// If the deblended flag is True, the function will treat each region as
// an individual component and will fit that many gaussians to the image
  void fitComponents(T maximumError);


// Fits the specified number of 3D gaussians to the data, and returns 
// solution in image (world) coordinates.  Essentially just an interface
// for FitGaussian.
// <group>
  Matrix<T> fitGauss(const Matrix<T>& positions, 
                     const Vector<T>& dataValues,
                     uInt ngaussians,
                     const Matrix<T>& initestimate, T maximumError) const;
  Matrix<T> fitGauss(const Matrix<T>& positions,
                     const Vector<T>& dataValue, 
                     uInt ngaussians,
                     const Matrix<T>& initestimate, 
                     const Matrix<T>& retrymatrix, T maximumError) const;
// </group>

};
#endif
