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

#ifndef IMAGES_IMAGEDECOMPOSER_H
#define IMAGES_IMAGEDECOMPOSER_H

#include <casa/iostream.h>
#include <casa/math.h>
#include <casa/aips.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Containers/Block.h>
#include <scimath/Functionals/Function1D.h>
#include <lattices/Lattices/TempLattice.h>
#include <lattices/Lattices/SubLattice.h>
#include <images/Images/ImageInterface.h>


namespace casa { //# NAMESPACE CASA - BEGIN

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
//   <li> <linkto class=CoordinateSystem>CoordinateSystem</linkto>   
//   <li> <linkto class=ImageInterface>ImageInterface</linkto>
// </prerequisite>

// <etymology>
// It takes an image, and separates it into components.
// </etymology>

// <synopsis>
// ImageDecomposer is an image decomposition tool that performs several tasks,
// with the end result being that a strongly blended image is separated into
// components - both in the sense that it determines the parameters for each
// component (assuming a Gaussian model) and that it physically assigns each
// pixel in the image to an individual object.  The products of these two
// operations are called the component list and the component map, 
// respectively.  The fitting process (which determines the component list) and
// the pixel-decomposition process (which determines the component map) are
// designed to work cooperatively to increase the efficiency and accuracy of
// both, though each can operate without the other if necessary.
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

// <example>
// <srcblock>
//  TempImage<Double> image;
//  //(populate the image with data: see dImageDecomposer.cc)
//  ImageDecomposer<Double> id(image);
//  id.setDeblendOptions(0.3, 8);
//  id.setFitOptions(0.4);
//  id.decomposeImage();
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
// <todo asof="2002/07/23">
//   <li> Generalize dimensionality 
//   <li> Use Lattice iterators in place of IPosition loops wherever possible
//   <li> Speed up fitting by not sending every region pixel to the fitter
//   <li> Send the completed componentmap to the user as an AIPS++ (Int?) Image
//   <li> Return a ComponentList instead of a Matrix
//   <li> Enable custom contouring at user level
//   <li> Add progress meter
//   <li> Numerous other improvements to make are documented in the code
// </todo>
  


template <class T> class ImageDecomposer {

public:

// 'Special' flag values for pixels in the component map.  An indeterminate
// pixel lies directly between two components and cannot be immediately 
// assigned.  A masked pixel is not inside the targeted region of the
// sub-componentmap and is not used in decomposition or fitting.
   enum componentValues {
     INDETERMINATE = -1,
     MASKED = -2 
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

// Tell the decomposer what image to decompose ("target image").
// Also resets the internal component map.
   void setImage (ImageInterface<T>& image);

// Tells the program whether or not to use the contour-based deblender. If not,
// the program will instead perform a single thresholding followed by a
// local maximum scan before fitting.
   void setDeblend(Bool deblendIt=True);

// Specifies deblending options:
// <ul>
// <li> thresholdVal: noise cutoff level, used to distinguish source pixels
//      from background pixels.  Also, regions which are not blended above this
//      value will be fit separately.
// <li> nContour: number of total contours to use in deblending regions.
// <li> minRange: the minimum number of contours necessary to distinguish
//      an object as a separate component.
// <li> nAxis: paramater used to define whether or not two adjacent blocks
//      of pixels are contiguous - see identifyRegions for details.
// </ul>
// See decomposeImage for more information on the deblending process.
   void setDeblendOptions(T thresholdVal=0.1, uInt nContour=11, 
                          Int minRange=2, Int nAxis=2);

// Tells the program whether or not to perform fitting.  If not, the component
// list will be dstermined by estimation from the values of the first and 
// second order moments of each component.
   void setFit(Bool fitIt=True);

// Specifies fitting options:
// <ul>
// <li> maximumRMS: The maximum RMS residual value (after fitting) allowed to
//      identify a fit as successful.
// <li> maxRetries: the maximum number of times the fit will be restarted in
//      order to try to reach a successful result (convergent with 
//      RMS < maximumRMS).  The default value of -1 tells the program
//      to calculate a reasonable value automatically based on the complexity
//      of the image.
// <li> maxIter: maximum number of iterations to spend on each fit.
// <li> convCriteria: criterion to establish convergence: see NonLinearFitLM.
// </ul>
// Additional information on these parameters can be found in FitGaussian.
   void setFitOptions(T maximumRMS=0.1, Int maxRetries=-1, uInt maxIter=256,
                      T convCriteria=0.0001);


// The primary method of this class - executes the instructions stated in the
// options above by deblending and/or fitting to the image to generate
// the component map and/or component list.
  void decomposeImage(); 


// Returns the number of regions found in the image.  A 'region' as defined
// in this code is a subset of the image of contiguous pixels whose values
// are greater than the threshold value specified in decomposeImage. A region
// may contain one or more components.
  uInt numRegions() const;

// Returns the number of components found in the image.  A 'component' as
// defined in this code is a source that can be described as a single Gaussian.
// This can only be determined after deblending.
  uInt numComponents() const;

// Returns the shape of the component map.
  IPosition shape() const;                

// Returns the length of a specific axis.
  Int shape(uInt axis) const;            

// Returns True if the image has been thresholded (split up into regions.)
  Bool isDerived() const;

// Returns True if the image has been decomposed (split up into components.)
  Bool isDecomposed() const;  

// Returns the component parameters as a Matrix.  (Ideally, this should be
// a ComponentList.)
  Matrix<T> componentList() const;

// Currently does nothing; in the future should return the component map
// in a way that it can be seen by the user in AIPS++, preferably as a
// colorized image.
  void componentMap() const; 

// Command-line text output functions.
// <group>
  void display() const;
  void displayContourMap(const Vector<T>& clevels) const;
  void printComponents() const;
// </group>
// Boxes each region in the componentmap:
// blc is set to the lowest coordinate value in each region;
// trc is set to one above the highest coordinate value in each region.
  void boundRegions(Block<IPosition>& blc, Block<IPosition>& trc);
private:
  ImageInterface<T> *itsImagePtr;// Points to the target image.
  Lattice<Int> *itsMapPtr;       // The actual component map.  
  IPosition itsShape;            // Component map shape
  uInt itsDim;                   // Component map number of dimensions
  uInt itsNRegions;              // Number of distinct regions in component map
  uInt itsNComponents;           // Number of components that have been fitted
  Matrix<T> itsList;             // The component list (Gaussian parameters for
                                 // each component.)  
  Bool itsDeblendIt;
  T itsThresholdVal;
  uInt itsNContour;              // Decomposition options
  Int itsMinRange;               // IMPR: maybe use a struct?
  Int itsNAxis;
  
  Bool itsFitIt;
  T itsMaximumRMS; 
  Int itsMaxRetries;             // Fitting options
  uInt itsMaxIter;
  T itsConvCriteria;



  void copyOptions(const ImageDecomposer<T>& other);

// Makes sure a pair of IPositions is in the correct format for blc/trc, and
// corrects them if they are not.
  void correctBlcTrc(IPosition& blc, IPosition& trc) const;

// Used as an N-dimensional interator.  This should probably be replaced by
// LatticeIterators...?
// <group>
  Bool increment(IPosition& pos, const IPosition& shape) const;
  void decrement(IPosition& pos) const;
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


//Eliminates any regions whose corresponding values in killRegion are True
// by setting all pixel values in the componentmap set to that region to
// zero.  Zero-oriented; there is an offset of one between the index in
// killRegion and the actual region in the componentmap.
  void destroyRegions(const Vector<Bool>& killRegion);

// Eliminates regions with no cells by replacing them with higher-numbered 
// regions.
  void renumberRegions();

// Overlays a smaller map onto an empty region of a larger map,
// and adds submap component list to main component list.
// The user should exercise caution with this function and synthesize submaps
// only into regions of the main map that are truly empty (0), as no blending
// is assumed between different maps.
  void synthesize(const ImageDecomposer<T>& subdecomposer, IPosition blc);

// Set all elements in the component map to zero and clear the component list.
  void zero();

// Set all nonmasked elements in the component map to zero and clear the 
// component list.
  void clear();



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
// This function is mostly obsolete, and is only used when the contour 
// deblender is off (since the component map is necessary to determine the
// moments).
  void estimateComponentWidths(Matrix<T>& width,
                               const Block<IPosition>& maxvalpos) const;

// Calculates the 0th-2nd order moments of a region.
  Array<T> calculateMoments(Int region) const;


// Performs a single threshold scan on the image.  In other words,
// identifies all contigous blocks of pixels in the target image above the
// threshold value thrval, assigning each unique block to an integer,
// starting at one.  All pixels with target image values below thrval are set
// to zero.
  uInt identifyRegions(T thrval, Int naxis=2);

// Performs the contour decomposition on a blended image to generate a 
// component map that can detect components blended above any threshold(s),
// by performing threshold scans at each contour level and recognizing
// as individual any components that are distinct above any such level.
  void deblendRegions(const Vector<T>& contours, Int minRange=1, Int naxis=2);

// Retrieves the target image's value at the given location.
// <group>
  T getImageVal(IPosition coord) const;
  T getImageVal(Int x, Int y) const;
  T getImageVal(Int x, Int y, Int z) const;
// </group>

// Retrieves the number of the highest contour with a value less then the
// target image's value at the given location.
// <group>
  Int getContourVal(IPosition coord, const Vector<T>& clevels) const;
  Int getContourVal(Int x, Int y, Int z, const Vector<T>& clevels) const;
  Int getContourVal(Int x, Int y, const Vector<T>& clevels) const;
  Int getContourVal(T val, const Vector<T>& clevels) const;
// </group>

// Fits multiple gaussians to a single region.  First performs a local 
// maximum scan to estimate the number of components in the region.
  Matrix<T> fitRegion(Int region);

// Fits gaussians to an image; multiple gaussians per region in the component
// map.  The regions are fit sequentially and independently, so this function 
// can be used on the main image.  If the map is not yet thresholded, will fit
// to the entire image as if it  were a single composite object, which will be
// very slow.
  void fitRegions();

// Fits gaussians to an image; one gaussian per region in the pmap.
// This function is intended to be used only by ImageDecomposer on its
// intermediary subimages; using it at higher level will execute a full
// gaussian fit on the main image and will be extremely slow. Every 
// nonflagged object pixel in the image is used in fitting.

// If the deblended flag is True, the function will treat each region as
// an individual component and will fit that many gaussians to the image
  void fitComponents();

// Estimate the component parameters based on moments calculated using 
// the component map.
  Matrix<T> estimateComponents();

// Fits the specified number of 3D gaussians to the data, and returns 
// solution in image (world) coordinates.  Essentially just an interface
// for FitGaussian.
// <group>
  Matrix<T> fitGauss(const Matrix<T>& positions, const Vector<T>& dataValues,
                     const Matrix<T>& initestimate) const;

// </group>

};

} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <images/Images/ImageDecomposer.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
