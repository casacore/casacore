//# MomentCalculator.h: 
//# Copyright (C) 1997
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

#if !defined(AIPS_MOMENTCALCULATOR_H)
#define AIPS_MOMENTCALCULATOR_H

//# Includes
#include <aips/aips.h>
#include <trial/Lattices/LineCollapser.h>
#include <aips/Functionals/Gaussian1D.h>
#include <aips/Mathematics/NumericTraits.h>
#include <trial/Tasking/PGPlotter.h>
#include <trial/Images/SubImage.h>

//# Forward Declarations
template <class T> class Vector;
template <class T> class ImageMoments;

// <summary>
// Abstract base class for moment calculator classes
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="ImageMoments">ImageMoments</linkto>
//   <li> <linkto class="LatticeApply">LatticeApply</linkto>
//   <li> <linkto class="LineCollapser">LineCollapser</linkto>
// </prerequisite>
//
// <synopsis>
//  This class, its concrete derived classes, and the classes LineCollapser,
//  ImageMoments and LatticeApply are connected as follows.   LatticeApply offers 
//  functions so that the application programmer does not need to worry about how 
//  to optimally iterate through a Lattice; it deals with tiling and to a 
//  lesser extent memory.    LatticeApply functions are used by offering a class 
//  object to them that has a member function with a name and signature 
//  specified by an abstract base class that LatticeApply uses and the 
//  offered class inherits from.   Specifically, in this case, MomentCalcBase
//  inherits from LineCollapser and LatticeApply uses objects and methods of this
//  class (but does not inherit from it).  This defines the functions
//  <src>collapse</src> and <src>multiProcess</src> which operate on a vector
//  extracted from a Lattice.  The former returns one number, the latter a vector
//  of numbers from that profile.  MomentCalcBase is a base class for
//  for moment calculation and the <src>multiProcess</src>
//  functions are used to compute moments  (e.g., mean, sum, sum squared, 
//  intensity weighted velocity etc).
//
//  It is actually the concrete classes derived from MomentCalcBase (call them,
//  as a group, the MomentCalculator classes) that implement the <src>multiProcess</src> 
//  functions.  These derived classes allow different 
//  algorithms to be written with which moments of the vector can be computed. 
//
//  Now, so far, we have a LatticeApply function which iterates through Lattices,
//  extracts vectors, and offers them up to functions implemented in the derived 
//  MomentCalculator classes to compute the moments.   As well as that, we need some
//  class to actually construct the MomentCalculator classes and to feed them to 
//  LatticeApply.   This is the role of the ImageMoments class.  It is a high level 
//  class which takes control information from users specifying which moments they 
//  would like to calculate and how.   It also provides the ancilliary masking lattice to 
//  the MomentCalculator constructors. The actual computational work is done by the 
//  MomentCalculator classes. So ImageMoments, MomentCalcBase and its derived 
//  MomentCalculator classes are really one unit; none of them are useful without 
//  the others.  The separation of functionality is caused by having the
//  LatticeApply class that knows all about optimally iterating through Lattices.
//
//  The coupling between these classes is done partly by the "friendship".   ImageMoments
//  grants friendship to MomentCalcBase so that the latter has access to the private data and 
//  private functions of ImageMoments.  MomentCalcBase then operates as an interface between 
//  its derived MomentCalculator classes and ImageMoments. It retrieves private data 
//  from ImageMoments, and also activates private functions in ImageMoments, on behalf 
//  of the MomentCalculator classes. The rest of the coupling is done via the constructors 
//  of the derived MomentCalculator classes.  
//
//  Finally, MomentCalcBase also has a number of protected functions that are common to its
//  derived classes (e.g. plotting, fitting, accumulating sums etc).  It also has protected 
//  data that is common to all the MomentCalculator classes.  This protected data is accessed 
//  directly by name rather than with interface functions as there is too much of it.  Of 
//  course, since MomentCalcBase is an abstract base class, it is up to the MomentCalculator 
//  classes to give the MomentCalcBase protected data objects values.
//
//  For discussion about different moments and algorithms to compute them see the 
//  discussion in <linkto class="ImageMoments">ImageMoments</linkto> and also in
//  the derived classes documentation.
// </synopsis>
//
// <example>
//  Since MomentCalcBase is an abstract class, we defer code examples to
//  the derived classes.
// </example>
//
// <motivation>
// We were desirous of writing functions to optimally iterate through Lattices
// so that the application programmer did not have to know anything about tiling
// or memory if possible.   These are the LatticeApply functions. To incorporate 
// ImageMoments into this scheme required some of it to be shifted into 
// MomentCalcBase and its derived classes.
// </motivation>
//
// <todo asof="yyyy/mm/dd">
//  Derive more classes !
// </todo>


template <class T> class MomentCalcBase : public LineCollapser<T>
{
public:
   virtual ~MomentCalcBase();

// Returns the number of failed fits if doing fitting
   virtual uInt nFailedFits() const {return nFailed_p;};

protected:


// A number of private data members are kept here in the base class
// as they are common to the derived classes.  Since this class
// is abstract, they have to be filled by the derived classes.

// This vector is a container for all the possible moments that
// can be calculated.  They are in the order given by the ImageMoments
// enum MomentTypes
   Vector<T> calcMoments_p;

// This vector tells us which elements of the calcMoments_p vector
// we wish to select
   Vector<Int> selectMoments_p;

// Although the general philosophy of these classes is to compute
// all the posisble moments and then select the ones we want,
// some of them are too expensive to calculate unless they are
// really wanted.  These are the median moments and those that
// require a second pass.  These control Bools tell us whether
// we really want to compute the expensive ones.
   Bool doMedianI_p, doMedianV_p, doAbsDev_p;

// These vectors are used to transform coordinates between pixel and world
   Vector<Double> pixelIn_p, worldOut_p;

// All computations involving Coordinate conversions are relatively expensive
// This Bool signifies whether we need coordinate calculations or not for
// any of the moments
   Bool doCoordCalc_p;

// This vector houses the world coordinate values for the profile if it
// was from a separable axis. This means this vector can be pre computed 
// just once, instead of working out the coordinates for each profile 
// (expensive).  It should only be filled if doCoordCalc_p is True
   Vector<Double> sepWorldCoord_p;

// This gives the plotter name.  If no plotting, it won't be attached
   PGPlotter plotter_p;

// This Bool tells us whether we want to see all profiles plotted with the 
// Y range or whether they are to be scaled individually
   Bool fixedYLimits_p;

// When we are plotting, if we have asked to all profiles with the same 
// Y min and max, these are the values to use
   T yMinAuto_p, yMaxAuto_p;

// This vector is used to hold the abcissa values when plotting profiles
   Vector<T> abcissa_p;

// This string tells us the name of the moment axis (VELO or FREQ etc)
   String momAxisType_p;

// This is the number of Gaussian fits that failed.
   uInt nFailed_p;


// Accumulate statistical sums from a vector
   void accumSums(NumericTraits<T>::PrecisionType& s0,
                  NumericTraits<T>::PrecisionType& s0Sq,
                  NumericTraits<T>::PrecisionType& s1,
                  NumericTraits<T>::PrecisionType& s2,
                  Int& iMin,
                  Int& iMax,
                  T& dMin,
                  T& dMax,   
                  const Int i,  
                  const T datum,
                  const Double coord) const
//
// Accumulate statistical sums from this datum
//
// Input:
//  i              Index
//  datum          Pixel value
//  coord          Coordinate value on moment axis
// Input/output:  
//  iMin,max       index of dMin and dMax
//  dMin,dMax      minimum and maximum value
// Output:
//  s0             sum (I)
//  s0Sq           sum (I*I)
//  s1             sum (I*v)
//  s2             sum (I*v*v)
{
   NumericTraits<T>::PrecisionType dDatum = datum;
   s0 += dDatum;
   s0Sq += dDatum*dDatum;
   s1 += dDatum*coord;
   s2 += dDatum*coord*coord;
   if (datum < dMin) {
     iMin = i;
     dMin = datum;
   }
   if (datum > dMax) {
     iMax = i;
     dMax = datum;
   }
};


// Determine if the spectrum is pure noise 
   uInt allNoise(T& dMean,
                 const Vector<T>& data,
                 const Vector<Bool>& mask,
                 const T peakSNR,
                 const T stdDeviation) const;

// Check validity of constructor inputs
   void constructorCheck(Vector<T>& calcMoments,
                         const Vector<Int>& selectMoments,
                         const uInt nLatticeOut) const;

// Find out from the selectMoments array whether we want
// to compute the more expensive moments
   void costlyMoments(ImageMoments<T>& iMom,
                      Bool& doMedianI,  
                      Bool& doMedianV,
                      Bool& doAbsDev) const;

// Return plotting device from ImageMoments object
   PGPlotter& device(ImageMoments<T>& iMom) const;

// Return automatic/interactive switch from the ImageMoments object
   Bool& doAuto(ImageMoments<T>& iMom) const;

// Return the Bool saying whether we need to compute coordinates
// or not for the requested moments
   Bool doCoordCalc(ImageMoments<T>& iMom) const;

// Return the Bool from the ImageMoments object saying whether we 
// are going to fit Gaussians to the profiles or not.
   Bool& doFit(ImageMoments<T>& iMom) const;

// Draw a horizontal line across the full x range of the plot
   void drawHorizontal(const T& y,
                       PGPlotter& plotter) const;

// Draw a spectrum on the current panel with the box already drawn on
   void drawLine (const Vector<T>& x,
                  const Vector<T>& y,
                  PGPlotter& plotter) const;

// Draw and label a spectrum on the current or next panel
   Bool drawSpectrum (const Vector<T>& x,
                      const Vector<T>& y,
                      const Vector<Bool>& mask,
                      const Bool fixedYLimits,
                      const T yMinAuto,
                      const T yMaxAuto,
                      const String xLabel, 
                      const String yLabel, 
                      const String title,
                      const Bool advancePanel,
                      PGPlotter& plotter) const;

// Draw on lines marking the mean and +/- sigma
   void drawMeanSigma  (const T dMean,
                        const T dSigma,
                        PGPlotter& plotter) const;


// Draw a vertical line of the given length at a given abcissa
   void drawVertical(const T x,
                     const T yMin,
                     const T yMax,
                     PGPlotter& plotter) const;

// Find the next masked or unmasked point in a vector
   Bool findNextDatum     (uInt& iFound,
                           const uInt& n,
                           const Vector<Bool>& mask,
                           const uInt& iStart,
                           const Bool& findGood) const;
   
// Fit a Gaussian to x and y arrays given guesses for the gaussian parameters
   Bool fitGaussian (uInt& nFailed,
                     T& peak,
                     T& pos,
                     T& width,
                     T& level,
                     const Vector<T>& x,
                     const Vector<T>& y,
                     const Vector<Bool>& mask,
                     const T peakGuess,
                     const T posGuess,
                     const T widthGuess,
                     const T levelGuess) const;
                        
// Return the fixed Y-plotting limits switch from the
// ImageMomemts object
   Bool& fixedYLimits(ImageMoments<T>& iMom) const;

// Automatically fit a Gaussian to a spectrum, including finding the
// starting guesses.
   Bool getAutoGaussianFit(uInt& nFailed,
                           Vector<T>& gaussPars,
                           const Vector<T>& x,
                           const Vector<T>& y,
                           const Vector<Bool>& mask,
                           const T peakSNR,
                           const T stdDeviation,
                           PGPlotter& plotter,
                           const Bool fixedYLimits,
                           const T yMinAuto,
                           const T yMaxAuto,
                           const String xLabel,
                           const String yLabel,
                           const String title) const;

// Automatically work out a guess for the Gaussian parameters
// Returns False if all pixels masked.
   Bool getAutoGaussianGuess(T& peakGuess,    
                             T& posGuess,
                             T& widthGuess,
                             T& levelGuess,
                             const Vector<T>& x,
                             const Vector<T>& y,
                             const Vector<Bool>& mask) const;

// Read the cursor button
   void getButton(Bool& reject,
                  Bool& redo,
                  PGPlotter& plotter) const;


// Interactively define a guess for a Gaussian fit, and then
// do the fit.  Do this repeatedly  until the user is content.
   Bool getInterGaussianFit(uInt& nFailed,
                            Vector<T>& gaussPars,
                            LogIO& os,
                            const Vector<T>& x,
                            const Vector<T>& y,  
                            const Vector<Bool>& mask,
                            const Bool fixedYLimits,
                            const T yMinAuto,
                            const T yMaxAuto,
                            const String xLabel,
                            const String yLabel,
                            const String title,
                            PGPlotter& plotter) const;

// Interactively define a guess for the Gaussian parameters
   void getInterGaussianGuess(T& peakGuess,
                              T& posGuess,
                              T& widthGuess,
                              Vector<Int>& window,
                              Bool& reject,
                              LogIO& os,
                              const Int nPts,
                              PGPlotter& plotter) const;

// Read the cursor and return its coordinates if not off the plot.
// Also interpret which button was pressed
   Bool getLoc(T& x,
               Bool& allSubsequent,
               Bool& ditch,
               Bool& redo,
               const Bool final,
               PGPlotter& plotter) const;
                        
// Compute the world coordinate for the given moment axis pixel   
   Double getMomentCoord(ImageMoments<T>& iMom,
                         Vector<Double>& pixelIn,
                         Vector<Double>& worldOut,
                         const Double momentPixel) const
// 
// Find the value of the world coordinate on the moment axis
// for the given moment axis pixel value. 
//
// Input
//   momentPixel   is the index in the profile extracted from the data
// Input/output
//   pixelIn       Pixels to convert.  Must all be filled in except for
//                 pixelIn(momentPixel).
//   worldOut      Vector to hold result
//
// Should really return a Fallible as I don't check and see
// if the coordinate transformation fails or not
//
{
   pixelIn(iMom.momentAxis_p) = momentPixel;
   iMom.pInImage_p->coordinates().toWorld(worldOut, pixelIn);
   return worldOut(iMom.momentAxis_p);
};


// Examine a mask and determine how many segments of unmasked points
// it consists of.    
   void lineSegments (uInt& nSeg,
                      Vector<uInt>& start,
                      Vector<uInt>& nPts,
                      const Vector<Bool>& mask) const;

// Resize an abcissa vector for plotting
   void makeAbcissa(Vector<T>& x,
                    const Int& n) const;

// Return the moment axis from the ImageMoments object
   Int& momentAxis(ImageMoments<T>& iMom) const;

// Return the name of the moment/profile axis
   String momentAxisName(ImageMoments<T>& iMom) const;

// Return the number of moments that the ImageMoments class can calculate
   uInt nMaxMoments() const;

// Return the peak SNR for determination of all noise spectra from
// the ImageMoments object
   T& peakSNR(ImageMoments<T>& iMom) const;

// Return the selected pixel intensity range from the ImageMoments 
// object and the Bools describing whether it is inclusion or exclusion
   void selectRange(Vector<T>& pixelRange,                
                    Bool& doInclude,
                    Bool& doExlude,
                    ImageMoments<T>& iMom) const;

// The MomentCalculators compute a vector of all possible moments.
// This function returns a vector which selects the desired moments from that
// "all moment" vector.
   Vector<Int> selectMoments(ImageMoments<T>& iMom) const;

// Fill the ouput moments array
   void setCalcMoments (ImageMoments<T>& iMom,
                        Vector<T>& calcMoments,
                        Vector<Double>& pixelIn,
                        Vector<Double>& worldOut,
                        Bool doCoordCalc,
                        T dMedian,
                        T vMedian,
                        Int nPts,
                        NumericTraits<T>::PrecisionType s0,
                        NumericTraits<T>::PrecisionType s1,
                        NumericTraits<T>::PrecisionType s2,
                        NumericTraits<T>::PrecisionType s0Sq,
                        NumericTraits<T>::PrecisionType sumAbsDev,
                        T dMin,
                        T dMax,
                        Int iMin,
                        Int iMax) const
//
// Fill the moments vector
//
// Outputs:
//   calcMoments The moments
//
{
	
// Short hand to fish ImageMoments enum values out   
// Despite being our friend, we cannot refer to the
// enum values as just, say, "AVERAGE"
     
   typedef ImageMoments<Float> IM;
           
             
// Normalize and fill moments
                
   calcMoments(IM::AVERAGE) = s0 / nPts;
   calcMoments(IM::INTEGRATED) = s0; 
   calcMoments(IM::WEIGHTED_MEAN_COORDINATE) = s1 / s0;
   calcMoments(IM::WEIGHTED_DISPERSION_COORDINATE) = 
     (s2 / s0) - calcMoments(IM::WEIGHTED_MEAN_COORDINATE) *
                 calcMoments(IM::WEIGHTED_MEAN_COORDINATE);
   calcMoments(IM::WEIGHTED_DISPERSION_COORDINATE) =
      abs(calcMoments(IM::WEIGHTED_DISPERSION_COORDINATE));
   if (calcMoments(IM::WEIGHTED_DISPERSION_COORDINATE) > 0.0) {
      calcMoments(IM::WEIGHTED_DISPERSION_COORDINATE) =
         sqrt(calcMoments(IM::WEIGHTED_DISPERSION_COORDINATE));
   } else {
      calcMoments(IM::WEIGHTED_DISPERSION_COORDINATE) = 0.0;
   }

// Standard deviation about mean of I
                 
   if (Float((s0Sq - s0*s0/nPts)/(nPts-1)) > 0) {
      calcMoments(IM::STANDARD_DEVIATION) = sqrt((s0Sq - s0*s0/nPts)/(nPts-1));
   } else {
      calcMoments(IM::STANDARD_DEVIATION) = 0;
   }

// Rms of I

   calcMoments(IM::RMS) = sqrt(s0Sq/nPts);
     
// Absolute mean deviation

   calcMoments(IM::ABS_MEAN_DEVIATION) = sumAbsDev / nPts;

// Maximum value

   calcMoments(IM::MAXIMUM) = dMax;
                                      
// Coordinate of maximum value

   if (doCoordCalc) calcMoments(IM::MAXIMUM_COORDINATE) =
         getMomentCoord(iMom, pixelIn, worldOut, Double(iMax));                                     

// Minimum value
   calcMoments(IM::MINIMUM) = dMin;

// Coordinate of minimum value

   if (doCoordCalc) calcMoments(IM::MINIMUM_COORDINATE) =
          getMomentCoord(iMom, pixelIn, worldOut, Double(iMin));

// Medians

     calcMoments(IM::MEDIAN) = dMedian;
     calcMoments(IM::MEDIAN_COORDINATE) = vMedian;
};



// Fill a string with the position of the cursor
   void setPosLabel(String& title,
                    const IPosition& pos) const;

// Set up separable moment axis coordinate vector and
// conversion vectors if not separable
   void setUpCoords (ImageMoments<T>& iMom,
                     Vector<Double>& pixelIn,
                     Vector<Double>& worldOut,
                     Vector<Double>& sepWorldCoord,
                     LogIO& os) const;

// Plot the Gaussian fit
   void showGaussFit(const T peak,
                     const T pos,    
                     const T width,
                     const T level,
                     const Vector<T>& x,
                     const Vector<T>& y,
                     const Vector<Bool>& mask,
                     PGPlotter& plotter) const;

// Find some statistics from teh masked vector.
// Returns False if no unmasked points.
   Bool stats(T& dMin, T& dMax, 
              uInt& minPos, uInt& maxPos,
              T& mean,
              const Vector<T>& profile,
              const Vector<Bool>& mask) const;
                 
// Return standard deviation of image from ImageMoments object
   T& stdDeviation(ImageMoments<T>& iMom) const;
 
// Return the auto y min and max from the ImageMoments object
   void yAutoMinMax(T& yMin, 
                    T& yMax, 
                    ImageMoments<T>& iMom) const;

protected:
// Check if #pixels is indeed 1.

   virtual void init (uInt nOutPixelsPerCollapse);
};

                   	        	         

// <summary> Computes simple clipped, and masked moments</summary>
// <use visibility=export>
// 
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
// 
// <prerequisite>
//   <li> <linkto class="ImageMoments">ImageMoments</linkto>
//   <li> <linkto class="LatticeApply">LatticeApply</linkto>
//   <li> <linkto class="MomentCalcBase">MomentCalcBase</linkto>
//   <li> <linkto class="LineCollapser">LineCollapser</linkto>
// </prerequisite>
//
// <synopsis>
//  This concrete class is derived from the abstract base class MomentCalcBase
//  which provides an interface layer to the ImageMoments driver class.
//  ImageMoments creates a MomentClip object and passes it to the LatticeApply
//  function, lineMultiApply. This function iterates through a given lattice, 
//  and invokes the <src>multiProcess</src> member function of MomentClip on each vector 
//  of pixels that it extracts from the input lattice.  The <src>multiProcess</src>
//  function returns a vector of moments which are inserted into the output 
//  lattices also supplied to the LatticeApply function.
//
//  MomentClip computes moments directly from a vector of pixel intensities 
//  extracted from the primary lattice.  An optional pixel intensity inclusion 
//  or exclusion range can be applied.   It can also compute a mask based on the 
//  inclusion or exclusion ranges applied to an ancilliary lattice (the ancilliary 
//  vector corresponding to the primary vector is extracted).  This mask is then 
//  applied to the primary vector for moment computation (ImageMoments offers
//  a smoothed version of the primary lattice as the ancilliary lattice)
//
//  The constructor takes an ImageMoments object; the one that is constructing
//  the MomentClip object of course.   There is much control information embodied  
//  in the state of the ImageMoments object.  This information is extracted by the 
//  MomentCalcBase class and passed on to MomentClip for consumption.
//
//  Note that the ancilliary lattice is only accessed if the ImageMoments 
//  object indicates that a pixel inclusion or exclusion range has been 
//  given as well as the pointer to the lattice having a non-zero value.
//
//  See the <linkto class="ImageMoments">ImageMoments</linkto>
//  for discussion about the moments that are available for computation.
//
// </synopsis>
//
// <example>
// This example comes from ImageMoments.   outPt is a pointer block holding
// pointers to the output lattices.  The ancilliary masking lattice is
// just a smoothed version of the input lattice.
//
// <srcBlock>
// 
//// Construct desired moment calculator object.  Use it polymorphically 
//// via a pointer to the base class.  os_P is a LogIO object.
//
//   MomentCalcBase<T>* pMomentCalculator = 0;
//   if (clipMethod || smoothClipMethod) {
//      pMomentCalculator = new MomentClip<T>(pSmoothedImage, *this, os_p, outPt.nelements());
//   } else if (windowMethod) {
//      pMomentCalculator = new MomentWindow<T>(pSmoothedImage, *this, os_p, outPt.nelements());
//   } else if (fitMethod) {
//      pMomentCalculator = new MomentFit<T>(*this, os_p, outPt.nelements());
//   }
//
//// Iterate optimally through the image, compute the moments, fill the output lattices
//
//   LatticeApply<T>::lineMultiApply(outPt, *pInImage_p, *pMomentCalculator,   
//                                   momentAxis_p, pProgressMeter);
//   delete pMomentCalculator;
//
// </srcBlock>
// </example>
//
//
// <todo asof="yyyy/mm/dd">
// </todo>



template <class T> class MomentClip : public MomentCalcBase<T>
{
public:

// Constructor.  The pointer is to an ancilliary  lattice used as a mask.
// If no masking lattice is desired, the pointer value must be zero.  We also 
// need the ImageMoments object which is calling us, the ImageMoments 
// logger, and the number of output lattices ImageMoments has created.
   MomentClip(Lattice<T>* pAncilliaryLattice,
              ImageMoments<T>& iMom,
              LogIO& os,
              const uInt nLatticeOut);

// Destructor (does nothing).
  ~MomentClip();

// This function is not implemented and throws an exception.
   virtual void process(T& out,
                        Bool& outMask,
                        const Vector<T>& in,
                        const Vector<Bool>& inMask,
                        const IPosition& pos);

// This function returns a vector of numbers from each input vector.
// the output vector contains the moments known to the ImageMoments
// object passed into the constructor.
   virtual void multiProcess(Vector<T>& out,
                             Vector<Bool>& outMask,
                             const Vector<T>& in,
                             const Vector<Bool>& inMask,
                             const IPosition& pos);

// Can handle null mask
   virtual Bool canHandleNullMask() const {return True;};

private:

   Lattice<T>* pAncilliaryLattice_p; 
   ImageMoments<T>& iMom_p;
   LogIO os_p;

   const Vector<T>* pProfileSelect_p;
   Vector<T> ancilliarySliceRef_p;
   Vector<T> selectedData_p;
   Vector<Int> selectedDataIndex_p;
   Bool doInclude_p, doExclude_p;
   Vector<T> range_p;
   IPosition sliceShape_p;

};



// <summary> Computes moments from a windowed profile </summary>
// <use visibility=export>
// 
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
// 
// <prerequisite>
//   <li> <linkto class="ImageMoments">ImageMoments</linkto>
//   <li> <linkto class="LatticeApply">LatticeApply</linkto>
//   <li> <linkto class="MomentCalcBase">MomentCalcBase</linkto>
//   <li> <linkto class="LineCollapser">LineCollapser</linkto>
// </prerequisite>
//
// <synopsis>
//  This concrete class is derived from the abstract base class MomentCalcBase
//  which provides an interface layer to the ImageMoments driver class.
//  ImageMoments creates a MomentWindow object and passes it to the LatticeApply
//  function lineMultiApply.  This function iterates through a given lattice, 
//  and invokes the <src>multiProcess</src> member function of MomentWindow on each profile
//  of pixels that it extracts from the input lattice.  The <src>multiProcess</src> function 
//  returns a vector of moments which are inserted into the output lattices also
//  supplied to the LatticeApply function.
//
//  MomentWindow computes moments from a subset of the pixels selected from  the
//  input profile.  This subset is a simple index range, or window.  The window is
//  selected, for each profile, that is thought to surround the spectral feature 
//  of interest.  This window can be found from the primary lattice, or from an 
//  ancilliary lattice (ImageMoments offers a smoothed version of the primary 
//  lattice as the ancilliary lattice).  The moments are always computed from 
//  primary lattice data.   
//
//  For each profile, the window can be found either interactively or automatically.
//  There are two interactive methods.  Either you just mark the window with the
//  cursor, or you interactively fit a Gaussian to the profile and the +/- 3-sigma
//  window is returned.  There are two automatic methods.  Either Bosma's converging
//  mean algorithm is used, or an automatically  fit Gaussian +/- 3-sigma window
//  is returned.
// 
//  The constructor takes an ImageMoments object; the one that is constructing
//  the MomentWindow object of course.   There is much control information embodied  
//  in the state  of the ImageMomemts object.  This information is extracted by the
//  MomentCalcBase class and passed on to MomentClip for consumption.
//
//  Note that the ancilliary lattice is only accessed if the pointer to it
//  is non zero.
//  
//  See the <linkto class="ImageMoments">ImageMoments</linkto>
//  for discussion about the moments that are available for computation.
//  
// </synopsis>
//
// <example>
// <srcBlock>
// This example comes from ImageMoments.   outPt is a pointer block holding
// pointers to the output lattices.  The ancilliary masking lattice is
// just a smoothed version of the input lattice.  os_P is a LogIO object.
//   
// <srcBlock>
// 
//// Construct desired moment calculator object.  Use it polymorphically via 
//// a pointer to the base class.
//  
//   MomentCalcBase<T>* pMomentCalculator = 0;
//   if (clipMethod || smoothClipMethod) {
//      pMomentCalculator = new MomentClip<T>(pSmoothedImage, *this, os_p, outPt.nelements());
//   } else if (windowMethod) {
//      pMomentCalculator = new MomentWindow<T>(pSmoothedImage, *this, os_p, outPt.nelements());
//   } else if (fitMethod) {
//      pMomentCalculator = new MomentFit<T>(*this, os_p, outPt.nelements());
//   }
//  
//// Iterate optimally through the image, compute the moments, fill the output lattices
//  
//   LatticeApply<T>::lineMultiApply(outPt, *pInImage_p, *pMomentCalculator,   
//                                   momentAxis_p, pProgressMeter);
//   delete pMomentCalculator;
//  
// </srcBlock>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="yyyy/mm/dd">
// </todo>


template <class T> class MomentWindow : public MomentCalcBase<T>
{
public:

// Constructor.  The pointer is to a lattice containing the masking
// lattice (created by ImageMoments).   We also need the 
// ImageMoments object which is calling us, the ImageMoments logger,
// and the number of output lattices ImageMoments has created.
   MomentWindow(Lattice<T>* pAncilliaryLattice,
                ImageMoments<T>& iMom,
                LogIO& os,
                const uInt nLatticeOut);

// Destructor (does nothing).
  ~MomentWindow();

// This function is not implemented and throws an exception.
   virtual void process(T& out,
                        Bool& outMask,
                        const Vector<T>& in,
                        const Vector<Bool>& inMask,
                        const IPosition& pos);

// This function returns a vector of numbers from each input vector.
// the output vector contains the moments known to the ImageMoments
// object passed into the constructor.
   virtual void multiProcess(Vector<T>& out,
                             Vector<Bool>& outMask,
                             const Vector<T>& in,
                             const Vector<Bool>& inMask,
                             const IPosition& pos);

                             
private:

   Lattice<T>* pAncilliaryLattice_p; 
   ImageMoments<T>& iMom_p;
   LogIO os_p;

   const Vector<T>* pProfileSelect_p;
   Vector<T> ancilliarySliceRef_p;
   Vector<T> selectedData_p;
   T stdDeviation_p, peakSNR_p;
   Bool doAuto_p, doFit_p;
   IPosition sliceShape_p;


// Draw two vertical lines marking a spectral window
   void drawWindow(const Vector<Int>& window,
                   PGPlotter& plotter) const;


// Automatically determine the spectral window
   Bool getAutoWindow(uInt& nFailed,
                      Vector<Int>& window,
                      const Vector<T>& x,
                      const Vector<T>& y,
                      const Vector<Bool>& mask,
                      const T peakSNR,
                      const T stdDeviation,
                      const Bool doFit,
                      PGPlotter& plotter,
                      const Bool fixedYLimits,                 
                      const T yMinAuto,                 
                      const T yMaxAuto,                 
                      const String xLabel,
                      const String yLabel,
                      const String title) const;

// Automatically determine the spectral window via Bosma's algorithm
   Bool getBosmaWindow (Vector<Int>& window,
                        const Vector<T>& x,
                        const Vector<T>& y,
                        const Vector<Bool>& mask,
                        const T peakSNR,
                        const T stdDeviation,
                        PGPlotter& plotter,
                        const Bool fixedYLimits,
                        const T yMinAuto,
                        const T yMaxAuto,
                        const String xLabel,
                        const String yLabel,
                        const String title) const;

// Interactively specify the spectral window with the cursor
   Bool getInterDirectWindow(Bool& allSubsequent,
                             LogIO& os,
                             Vector<Int>& window,
                             const Vector<T>& x,
                             const Vector<T>& y,
                             const Vector<Bool>& mask,
                             const Bool fixedYLimits,   
                             const T yMinAuto,   
                             const T yMaxAuto,
                             const String xLabel,
                             const String yLabel,
                             const String title,
                             PGPlotter& plotter) const;

// Interactively define the spectral window
// Returns false if can't define window.
   Bool getInterWindow (uInt& nFailed,
                        Bool& allSubsequent,
                        LogIO& os,
                        Vector<Int>& window,
                        const Bool doFit,
                        const Vector<T>& x,
                        const Vector<T>& y,
                        const Vector<Bool>& mask,
                        const Bool fixedYLimits,
                        const T yMinAuto,
                        const T yMaxAuto,
                        const String xLabel,
                        const String yLabel,
                        const String title,
                        PGPlotter& plotter) const;

// Take the fitted Gaussian parameters and set an N-sigma window.
// If the window is too small return a Fail condition.
   Bool setNSigmaWindow(Vector<Int>& window,  
                        const T pos,
                        const T width,
                        const Int nPts,
                        const Int N) const;

};



// <summary> Compute moments from a Gaussian fitted to a profile</summary>
// <use visibility=export>
// 
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
// 
// <prerequisite>
//   <li> <linkto class="ImageMoments">ImageMoments</linkto>
//   <li> <linkto class="LatticeApply">LatticeApply</linkto>
//   <li> <linkto class="MomentCalcBase">MomentCalcBase</linkto>
//   <li> <linkto class="LineCollapser">LineCollapser</linkto>
// </prerequisite>
//
// <synopsis>
//  This concrete class is derived from the abstract base class MomentCalcBase
//  which provides an interface layer to the ImageMoments driver class.  
//  ImageMoments creates a MomentFit object and passes it to the LatticeApply
//  function, lineMultiApply. This function iterates through a given lattice,
//  and invokes the <src>multiProcess</src> member function of MomentFit on each vector
//  of pixels that it extracts from the input lattice.  The <src>multiProcess</src>
//  function returns a vector of moments which are inserted into the output
//  lattices also supplied to the LatticeApply function.
// 
//  MomentFit computes moments by fitting a Gaussian to each profile.  The
//  moments are then computed from that fit.   The fitting can be done either
//  interactively or automatically.
// 
//  The constructor takes an ImageMoments object; the one that is constructing
//  the MomentFit object of course.   There is much control information embodied
//  in the state of the ImageMoments object.  This information is extracted by the
//  MomentCalcBase class and passed on to MomentFit for consumption.
//   
//  See the <linkto class="ImageMoments">ImageMoments</linkto>
//  for discussion about the moments that are available for computation.
//  
// </synopsis>
//
// <example>
// <srcBlock>
// This example comes from ImageMoments.   outPt is a pointer block holding
// pointers to the output lattices.   os_P is a LogIO object.
//                                     
// <srcBlock>
// 
//// Construct desired moment calculator object.  Use it polymorphically via
//// a pointer to the base class.
//
//   MomentCalcBase<T>* pMomentCalculator = 0;
//   if (clipMethod || smoothClipMethod) {
//      pMomentCalculator = new MomentClip<T>(pSmoothedImage, *this, os_p, outPt.nelements());
//   } else if (windowMethod) {
//      pMomentCalculator = new MomentWindow<T>(pSmoothedImage, *this, os_p, outPt.nelements());
//   } else if (fitMethod) {
//      pMomentCalculator = new MomentFit<T>(*this, os_p, outPt.nelements());
//   }
//
//// Iterate optimally through the image, compute the moments, fill the output lattices
//
//   LatticeApply<T>::lineMultiApply(outPt, *pInImage_p, *pMomentCalculator,   
//                                   momentAxis_p, pProgressMeter);
//   delete pMomentCalculator;
// </srcBlock>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="yyyy/mm/dd">
// </todo>


template <class T> class MomentFit : public MomentCalcBase<T>
{
public:

// Constructor.  We need the ImageMoments object which is calling us, 
// the ImageMoments logger, and the number of output lattices 
// ImageMoments has created.
   MomentFit(ImageMoments<T>& iMom,
             LogIO& os,
             const uInt nLatticeOut);

// Destructor (does nothing).
  ~MomentFit();

// This function is not implemented and throws an exception.
   virtual void process(T& out,
                        Bool& outMask,
                        const Vector<T>& in,
                        const Vector<Bool>& inMask,
                        const IPosition& pos);

// This function returns a vector of numbers from each input vector.
// the output vector contains the moments known to the ImageMoments
// object passed into the constructor.
   virtual void multiProcess(Vector<T>& out,
                             Vector<Bool>& outMask,
                             const Vector<T>& in,
                             const Vector<Bool>& inMask,
                             const IPosition& pos);

private:

   ImageMoments<T>& iMom_p;
   LogIO os_p;
   T stdDeviation_p, peakSNR_p;
   Bool doAuto_p, doFit_p;
   Gaussian1D<T> gauss_p;

};



#endif
