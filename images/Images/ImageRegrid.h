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
//#
//#
//# $Id$

#ifndef IMAGES_IMAGEREGRID_H
#define IMAGES_IMAGEREGRID_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/scimath/Mathematics/Interpolate2D.h>
#include <set>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> class MaskedLattice;
template<class T> class ImageInterface;
template<class T> class Lattice;
template<class T> class LatticeIterator;
template<class T> class Vector;

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
  // output pixels at which the regridding failed will be masked bad (False)
  // and the pixel value set to zero. Otherwise the output mask is not changed.
  // Specify which pixel axes of outImage are to be
  // regridded.  The coordinate and axis order of outImage
  // is preserved, regardless of where the relevant coordinates
  // are in inImage.
  //
  // decimate only applies when replicate=False. it is
  // the coordinate grid computation decimation FACTOR
  // (e.g.  nCoordGrid ~ nIn / decimate). 0 means no decimation
  // (slowest and most accurate)
  void regrid(ImageInterface<T>& outImage, 
              typename Interpolate2D::Method method,
              const IPosition& whichOutPixelAxes,
	      const ImageInterface<T>& inImage,
              Bool replicate=False, uInt decimate=0,
              Bool showProgress=False, Bool forceRegrid=False,
              Bool verbose=False);

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
// is True for successfull coordinate conversions, and False otherwise.
// <group>
  void get2DCoordinateGrid (Cube<Double>& grid, Matrix<Bool>& gridMask) const;
  void set2DCoordinateGrid (const Cube<Double>& grid, const Matrix<Bool>& gridMask, Bool notify=False);
// </group>
//
  // Inserts inImage into outImage.  The alignment is done by
  // placing the blc of inImage at the specified 
  // absolute pixel of the outImage (outPixelLocation).  If 
  // the outPixelLocation vector is of zero length, then the images 
  // are aligned by their reference pixels.  Only integral shifts are done
  // in the aligment process. If outImage has a mask,  it will be updated.
  // Returns False if no overlap of images, in which case the
  // output is not updated.
  Bool insert(ImageInterface<T>& outImage,
              const Vector<Double>& outPixelLocation,
              const ImageInterface<T>& inImage);

  // Print out useful debugging information (level 0 is none,
  // 1 is some, 2 is too much)
  void showDebugInfo(Int level=0) {itsShowLevel = level;};

  // Enable/disable Measures Reference conversions
  void disableReferenceConversions(Bool disable=True) {itsDisableConversions = disable;};

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
		  Bool giveStokesWarning=True
  );

 private:

  Int itsShowLevel;
  Bool itsDisableConversions;
//
  Cube<Double> its2DCoordinateGrid;
  Matrix<Bool> its2DCoordinateGridMask;
//
  Cube<Double> itsUser2DCoordinateGrid;
  Matrix<Bool> itsUser2DCoordinateGridMask;
  Bool itsNotify;
//  
  // Check shape and axes.  Exception if no good.  If pixelAxes
  // of length 0, set to all axes according to shape
  void _checkAxes(IPosition& outPixelAxes,
                  const IPosition& inShape,
                  const IPosition& outShape,
                  const Vector<Int>& pixelAxisMap,
                  const CoordinateSystem& outCoords,
                  Bool verbose);

  // Find maps between coordinate systems
  void findMaps (uInt nDim, 
                 Vector<Int>& pixelAxisMap1,
                 Vector<Int>& pixelAxisMap2,
                 const CoordinateSystem& inCoords,
                 const CoordinateSystem& outCoords) const;

  // Find scale factor to conserve flux 
   Double findScaleFactor(const Unit& units, 
                          const CoordinateSystem& inCoords, 
                          const CoordinateSystem& outCoords, 
                          Int inCoordinate, Int outCoordinate,
                          LogIO& os) const;

  // Regrid one Coordinate
   void _regridOneCoordinate (LogIO& os, IPosition& outShape2,
                              Vector<Bool>& doneOutPixelAxes,
                              MaskedLattice<T>* &finalOutPtr,  
                              MaskedLattice<T>* &inPtr,   
                              MaskedLattice<T>* &outPtr,  
                              CoordinateSystem& outCoords,
                              const CoordinateSystem& inCoords,
                              Int outPixelAxis,
                              const ImageInterface<T>& inImage,
                              const IPosition& outShape,
                              Bool replicate, uInt decimate,
                              Bool outIsMasked, Bool showProgress,
                              Bool forceRegrid, 
                              typename Interpolate2D::Method method,
                              Bool verbose);

  // Regrid  DirectionCoordinate or 2-axis LinearCoordinate
   void regridTwoAxisCoordinate  (LogIO& os, MaskedLattice<T>& outLattice,
                         const MaskedLattice<T>& inLattice,
                         const Unit& imageUnit, 
                         const CoordinateSystem& inCoords,
                         const CoordinateSystem& outCoords,
                         Int inCoordinate, Int outCoordinate,
                         const Vector<Int> inPixelAxes,
                         const Vector<Int> outPixelAxes,
                         const Vector<Int> pixelAxisMap1,  
                         const Vector<Int> pixelAxisMap2,
                         typename Interpolate2D::Method method,
                         Bool replicate, uInt decimate,
                         Bool showProgress);

  // Make regridding coordinate grid for this cursor.
  void make2DCoordinateGrid (LogIO& os, Bool& allFail, Bool&missedIt,
                             Double& minInX, Double& minInY, 
                             Double& maxInX, Double& maxInY,
                             Cube<Double>& in2DPos,
                             Matrix<Bool>& succeed,
                             const CoordinateSystem& inCoords,
                             const CoordinateSystem& outCoords,
                             Int inCoordinate, Int outCoordinate,
                             uInt xInAxis, uInt yInAxis,
                             uInt xOutAxis, uInt yOutAxis,
                             const IPosition& inPixelAxes,
                             const IPosition& outPixelAxes,
                             const IPosition& inShape,
                             const IPosition& outPos,
                             const IPosition& cursorShape,
                             uInt decimate=0);

  // Make replication coordinate grid for this cursor
   void make2DCoordinateGrid (Cube<Double>& in2DPos,
                              Double& minInX, Double& minInY, 
                              Double& maxInX, Double& maxInY,
                              const Vector<Double>& pixelScale, 
                              uInt xInAxis, uInt yInAxis,
                              uInt xOutAxis, uInt yOutAxis,
                              uInt xInCorrAxis, uInt yInCorrAxis,
                              uInt xOutCorrAxis, uInt yOutCorrAxis,
                              const IPosition& outPos, const IPosition& cursorShape);

  // Make regridding coordinate grid for this axis
   void make1DCoordinateGrid (Block<Float>& xOut,
                              Vector<Bool>& failed,
                              Bool& allFailed,
                              Bool& allGood,
                              const Coordinate& inCoord,
                              const Coordinate& outCoord,
                              Int inAxisInCoordinate,
                              Int outAxisInCoordinate,
                              MFrequency::Convert& machine,
                              Bool useMachine);


  // Make replication coordinate grid for this axis
   void make1DCoordinateGrid (Block<Float>& xOut, Float pixelScale) const;

  // Regrid 1 axis
  void regrid1D (MaskedLattice<T>& outLattice,
                 const MaskedLattice<T>& inLattice,
                 const Coordinate& inCoord,
                 const Coordinate& outCoord,
                 const Vector<Int>& inPixelAxes,
                 const Vector<Int>& outPixelAxes,
                 Int inAxisInCoordinate,
                 Int outAxisInCoordinate,
                 const Vector<Int> pixelAxisMap,
                 typename Interpolate2D::Method method,
                 MFrequency::Convert& machine,
                 Bool replicate,
                 Bool useMachine, Bool showProgress);

//
   void regrid2DMatrix(Lattice<T>& outCursor,
                       LatticeIterator<Bool>*& outMaskIterPtr,
                       const Interpolate2D& interp,  
                                    ProgressMeter*& pProgress,
                                    Double& iPix,
                                    uInt nDim,
                                    uInt xInAxis, uInt yInAxis,
                                    uInt xOutAxis, uInt yOutAxis,
                                    Double scale,
                                    Bool inIsMasked, Bool outIsMasked,
                                    const IPosition& outPos,
                                    const IPosition& outCursorShape,
                                    const IPosition& inChunkShape,
                                    const IPosition& inChunkBlc,
                                    const IPosition& pixelAxisMap2,
                                    Array<T>& inDataChunk,
                                    Array<Bool>*& inMaskChunkPtr,
                                    const Cube<Double>& pix2DPos,
                                    const Matrix<Bool>& succeed);

   void findXYExtent (Bool& missedIt, Bool& allFailed,
                      Double& minInX, Double& minInY,
                      Double& maxInX, Double& maxInY,
                      Cube<Double>& in2DPos,
                      const Matrix<Bool>& succeed,
                      uInt xInAxis, uInt yInAxis,
                      uInt xOutAxis, uInt yOutAxis,
                      const IPosition& outPos,
                      const IPosition& outCursorShape,
                      const IPosition& inShape);
//
   Bool minmax(Double &minX, Double &maxX, Double& minY, Double& maxY,
               const Array<Double> &xData,
               const Array<Double> &yData,
               const Array<Bool>& mask);
};

 

} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/images/Images/ImageRegrid.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif

