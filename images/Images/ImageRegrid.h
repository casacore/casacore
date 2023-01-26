//# ImageRegrid.h: Regrid Images
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
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

#ifndef IMAGES_IMAGEREGRID_H
#define IMAGES_IMAGEREGRID_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/scimath/Mathematics/Interpolate2D.h>
#include <casacore/scimath/Mathematics/NumericTraits.h>
#include <set>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> class MaskedLattice;
template<class T> class ImageInterface;
template<class T> class Lattice;
template<class T> class LatticeIterator;

class CoordinateSystem;
class DirectionCoordinate;
class Coordinate;
class ObsInfo;
class IPosition;
class Unit;
class ProgressMeter;

// <summary>This regrids one image to match the coordinate system of another</summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="ImageInterface">ImageInterface</linkto>
//   <li> <linkto class="CoordinateSystem">CoordinateSystem</linkto>
//   <li> <linkto class="Interpolate2D">Interpolate2D</linkto>
//   <li> <linkto class="InterpolateArray1D">InterpolateArray1D</linkto>
// </prerequisite>
//
// <etymology>
//  Regrids, or resamples, images.  
// </etymology>
//
// <synopsis>
//  This class enables you to regrid one image to the coordinate
//  system of another.    You can regrid any or all of the
//  axes in the image.  A range of interpolation schemes are available.
//
//  It will cope with coordinate systems being in different orders
//  (coordinate, world axes, pixel axes).  The basic approach is to
//  make a mapping from the input to the output coordinate systems,
//  but the output CoordinateSystem order is preserved in the output
//  image.
//
//  Any DirectionCoordinate or LinearCoordinate holding exactly two axes
//  is regridded in one pass with a 2-D interpolation scheme.
//  All other axes are regridded in separate passes with a 1D interpolation 
//  scheme.    This means that a LinearCoordinate holding say 3 axes
//  where some of them are coupled will not be correctly regridded.
//  StokesCoordinates cannot be  regridded.
//
//  Multiple passes are made through the data, and the output of 
//  each pass is the input of the next pass.  The intermediate 
//  images are stored as TempImages which may be in memory or 
//  on disk, depending on their size.
//
//  It can also simply insert this image into that one via
//  an integer shift.
// </synopsis>
//
// <example>
// 
// <srcblock>
// </srcblock>
// </example>
//
// <motivation> 
// A common image analysis need is to regrid images, e.g. to compare
// images from different telescopes.
// </motivation>
//
// <thrown>
// <li> AipsError 
// </thrown>
//
// <todo asof="1999/04/20">
// </todo>

template <class T> class ImageRegrid
{
public:

  // Default constructor
  ImageRegrid();

  // copy constructor (copy semantics)
  ImageRegrid(const ImageRegrid &other);

  // destructor
  ~ImageRegrid();

  // Assignment copy semantics)
  ImageRegrid<T>& operator=(const ImageRegrid& other);

  // Regrid inImage onto the grid specified by outImage.
  // If outImage has a writable mask, it will be updated in that 
  // output pixels at which the regridding failed will be masked bad (false)
  // and the pixel value set to zero. Otherwise the output mask is not changed.
  // Specify which pixel axes of outImage are to be
  // regridded.  The coordinate and axis order of outImage
  // is preserved, regardless of where the relevant coordinates
  // are in inImage.
  //
  // decimate only applies when replicate=false. it is
  // the coordinate grid computation decimation FACTOR
  // (e.g.  nCoordGrid ~ nIn / decimate). 0 means no decimation
  // (slowest and most accurate)
  void regrid(ImageInterface<T>& outImage, 
              typename Interpolate2D::Method method,
              const IPosition& whichOutPixelAxes,
	      const ImageInterface<T>& inImage,
              bool replicate=false, uint32_t decimate=0,
              bool showProgress=false, bool forceRegrid=false,
              bool verbose=false);

// Get and set the 2-D coordinate grid.  After a call to function <src>regrid</src>
// in which coupled 2D coordinate (presently only DirectionCoordinate) is
// regridded, this coordinate grid will be available.  It can be reused
// via the <src>set2DCoordinateGrid</src> function for another like plane
// (e.g. if you choose to regrid planes of a cube separately).   When you provide
// the coordinate grid, it will no longer (for that 2D coordinate only) be
// computed internally, which may save a lot of time.  Ordinarily, if you
// regridded many planes of a cube in one call to regrid, the coordinate grid
// is cached for you.   To trigger successive calls to regrid to go back to
// internal computation, set zero length Cube and Matrix.  <src>gridMask</src>
// is true for successfull coordinate conversions, and false otherwise.
// <group>
  void get2DCoordinateGrid (Cube<double>& grid, Matrix<bool>& gridMask) const;
  void set2DCoordinateGrid (const Cube<double>& grid, const Matrix<bool>& gridMask, bool notify=false);
// </group>
//
  // Inserts inImage into outImage.  The alignment is done by
  // placing the blc of inImage at the specified 
  // absolute pixel of the outImage (outPixelLocation).  If 
  // the outPixelLocation vector is of zero length, then the images 
  // are aligned by their reference pixels.  Only integral shifts are done
  // in the aligment process. If outImage has a mask,  it will be updated.
  // Returns false if no overlap of images, in which case the
  // output is not updated.
  bool insert(ImageInterface<T>& outImage,
              const Vector<double>& outPixelLocation,
              const ImageInterface<T>& inImage);

  // Print out useful debugging information (level 0 is none,
  // 1 is some, 2 is too much)
  void showDebugInfo(int32_t level=0) {itsShowLevel = level;};

  // Enable/disable Measures Reference conversions
  void disableReferenceConversions(bool disable=true) {itsDisableConversions = disable;};

  // Helper function.  We are regridding from cSysFrom to cSysTo for the
  // specified pixel axes of cSyFrom. This function returns a CoordinateSystem which,
  // for the pixel axes being regridded, copies the coordinates from cSysTo
  // (if coordinate type present in cSysTo) or cSysFrom (coordinate
  // type not present in cSysTo).
  // For the axes not being regridded, it copies the coordinates from
  // cSysFrom.  This helps you build the cSys for function regrid.
  // The ObsInfo from cSysFrom is copied to the output CoordinateSystem.
  // If inShape has one or more elements it represenents the size of the
  // image to be regridded. It this must have the same number of elements
  // as the number of pixel axes in <src>cSysFrom</src>. If any of the values
  // are unity (ie the axes are degenerate), and the corresponding axis in <src>csysFrom</src> is the only
  // axis in its corresponding coordinate, this coordinate will not be replaced
  // even if the axis is specified in <src>axes</src>.
  // Upon return, <src>coordsToBeRegridded</src> will contain a list of the coordinates that will
  // be regridded.
  static CoordinateSystem makeCoordinateSystem(
		  LogIO& os,
                  std::set<Coordinate::Type>& coordsToBeRegridded,
		  const CoordinateSystem& cSysTo,
		  const CoordinateSystem& cSysFrom,
		  const IPosition& axes,
		  const IPosition& inShape=IPosition(),
		  bool giveStokesWarning=true
  );

 private:

  int32_t itsShowLevel;
  bool itsDisableConversions;
//
  Cube<double> its2DCoordinateGrid;
  Matrix<bool> its2DCoordinateGridMask;
//
  Cube<double> itsUser2DCoordinateGrid;
  Matrix<bool> itsUser2DCoordinateGridMask;
  bool itsNotify;
//  
  // Check shape and axes.  Exception if no good.  If pixelAxes
  // of length 0, set to all axes according to shape
  void _checkAxes(IPosition& outPixelAxes,
                  const IPosition& inShape,
                  const IPosition& outShape,
                  const Vector<int32_t>& pixelAxisMap,
                  const CoordinateSystem& outCoords,
                  bool verbose);

  // Find maps between coordinate systems
  void findMaps (uint32_t nDim, 
                 Vector<int32_t>& pixelAxisMap1,
                 Vector<int32_t>& pixelAxisMap2,
                 const CoordinateSystem& inCoords,
                 const CoordinateSystem& outCoords) const;

  // Find scale factor to conserve flux 
   double findScaleFactor(const Unit& units, 
                          const CoordinateSystem& inCoords, 
                          const CoordinateSystem& outCoords, 
                          int32_t inCoordinate, int32_t outCoordinate,
                          LogIO& os) const;

  // Regrid one Coordinate
   void _regridOneCoordinate (LogIO& os, IPosition& outShape2,
                              Vector<bool>& doneOutPixelAxes,
                              MaskedLattice<T>* &finalOutPtr,  
                              MaskedLattice<T>* &inPtr,   
                              MaskedLattice<T>* &outPtr,  
                              CoordinateSystem& outCoords,
                              const CoordinateSystem& inCoords,
                              int32_t outPixelAxis,
                              const ImageInterface<T>& inImage,
                              const IPosition& outShape,
                              bool replicate, uint32_t decimate,
                              bool outIsMasked, bool showProgress,
                              bool forceRegrid, 
                              typename Interpolate2D::Method method,
                              bool verbose);

  // Regrid  DirectionCoordinate or 2-axis LinearCoordinate
   void regridTwoAxisCoordinate  (LogIO& os, MaskedLattice<T>& outLattice,
                         const MaskedLattice<T>& inLattice,
                         const Unit& imageUnit, 
                         const CoordinateSystem& inCoords,
                         const CoordinateSystem& outCoords,
                         int32_t inCoordinate, int32_t outCoordinate,
                         const Vector<int32_t> inPixelAxes,
                         const Vector<int32_t> outPixelAxes,
                         const Vector<int32_t> pixelAxisMap1,  
                         const Vector<int32_t> pixelAxisMap2,
                         typename Interpolate2D::Method method,
                         bool replicate, uint32_t decimate,
                         bool showProgress);

  // Make regridding coordinate grid for this cursor.
  void make2DCoordinateGrid (LogIO& os, bool& allFail, bool&missedIt,
                             double& minInX, double& minInY, 
                             double& maxInX, double& maxInY,
                             Cube<double>& in2DPos,
                             Matrix<bool>& succeed,
                             const CoordinateSystem& inCoords,
                             const CoordinateSystem& outCoords,
                             int32_t inCoordinate, int32_t outCoordinate,
                             uint32_t xInAxis, uint32_t yInAxis,
                             uint32_t xOutAxis, uint32_t yOutAxis,
                             const IPosition& inPixelAxes,
                             const IPosition& outPixelAxes,
                             const IPosition& inShape,
                             const IPosition& outPos,
                             const IPosition& cursorShape,
                             uint32_t decimate=0);

  // Make replication coordinate grid for this cursor
   void make2DCoordinateGrid (Cube<double>& in2DPos,
                              double& minInX, double& minInY, 
                              double& maxInX, double& maxInY,
                              const Vector<double>& pixelScale, 
                              uint32_t xInAxis, uint32_t yInAxis,
                              uint32_t xOutAxis, uint32_t yOutAxis,
                              uint32_t xInCorrAxis, uint32_t yInCorrAxis,
                              uint32_t xOutCorrAxis, uint32_t yOutCorrAxis,
                              const IPosition& outPos, const IPosition& cursorShape);

  // Make regridding coordinate grid for this axis
   void make1DCoordinateGrid (Block<typename NumericTraits<T>::BaseType>& xOut,
                              Vector<bool>& failed,
                              bool& allFailed,
                              bool& allGood,
                              const Coordinate& inCoord,
                              const Coordinate& outCoord,
                              int32_t inAxisInCoordinate,
                              int32_t outAxisInCoordinate,
                              MFrequency::Convert& machine,
                              bool useMachine);


  // Make replication coordinate grid for this axis
   void make1DCoordinateGrid (Block<typename NumericTraits<T>::BaseType>& xOut,
           typename NumericTraits<T>::BaseType pixelScale) const;

  // Regrid 1 axis
  void regrid1D (MaskedLattice<T>& outLattice,
                 const MaskedLattice<T>& inLattice,
                 const Coordinate& inCoord,
                 const Coordinate& outCoord,
                 const Vector<int32_t>& inPixelAxes,
                 const Vector<int32_t>& outPixelAxes,
                 int32_t inAxisInCoordinate,
                 int32_t outAxisInCoordinate,
                 const Vector<int32_t> pixelAxisMap,
                 typename Interpolate2D::Method method,
                 MFrequency::Convert& machine,
                 bool replicate,
                 bool useMachine, bool showProgress);

//
   void regrid2DMatrix(Lattice<T>& outCursor,
                       LatticeIterator<bool>*& outMaskIterPtr,
                       const Interpolate2D& interp,  
                                    ProgressMeter*& pProgress,
                                    double& iPix,
                                    uint32_t nDim,
                                    uint32_t xInAxis, uint32_t yInAxis,
                                    uint32_t xOutAxis, uint32_t yOutAxis,
                                    double scale,
                                    bool inIsMasked, bool outIsMasked,
                                    const IPosition& outPos,
                                    const IPosition& outCursorShape,
                                    const IPosition& inChunkShape,
                                    const IPosition& inChunkBlc,
                                    const IPosition& pixelAxisMap2,
                                    Array<T>& inDataChunk,
                                    Array<bool>*& inMaskChunkPtr,
                                    const Cube<double>& pix2DPos,
                                    const Matrix<bool>& succeed);

   void findXYExtent (bool& missedIt, bool& allFailed,
                      double& minInX, double& minInY,
                      double& maxInX, double& maxInY,
                      Cube<double>& in2DPos,
                      const Matrix<bool>& succeed,
                      uint32_t xInAxis, uint32_t yInAxis,
                      uint32_t xOutAxis, uint32_t yOutAxis,
                      const IPosition& outPos,
                      const IPosition& outCursorShape,
                      const IPosition& inShape);
//
   bool minmax(double &minX, double &maxX, double& minY, double& maxY,
               const Array<double> &xData,
               const Array<double> &yData,
               const Array<bool>& mask);
};

//# Declare extern templates for often used types.
  extern template class ImageRegrid<float>;

} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/images/Images/ImageRegrid.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif

