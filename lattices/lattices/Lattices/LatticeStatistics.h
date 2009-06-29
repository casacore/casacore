//# LatticeStatistics.h: generate statistics from a Lattice
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
//# $Id$

#ifndef LATTICES_LATTICESTATISTICS_H
#define LATTICES_LATTICESTATISTICS_H


//# Includes
#include <casa/aips.h>
#include <casa/Arrays/Array.h>
#include <casa/Containers/Block.h>
#include <casa/Arrays/Vector.h>
#include <lattices/Lattices/LatticeStatsBase.h>
#include <lattices/Lattices/TiledCollapser.h>
#include <lattices/Lattices/TiledCollapser.h>
#include <lattices/Lattices/LatticeExprNode.h>
#include <scimath/Mathematics/NumericTraits.h>
#include <casa/System/PGPlotter.h>
#include <casa/Utilities/DataType.h>
#include <casa/BasicSL/String.h>
#include <casa/Logging/LogIO.h>


namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations
template <class T> class MaskedLattice;
template <class T> class TempLattice;
class IPosition;
#include <casa/iosstrfwd.h>


// <summary>
// Compute and display various statistics from a lattice
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
// <prerequisite>
//   <li> <linkto class=LatticeStatsBase>LatticeStatsBase</linkto>
//   <li> <linkto class=MaskedLattice>MaskedLattice</linkto>
// </prerequisite>

// <etymology>
// This is a class designed to display and retrieve statistics from lattices
// </etymology>

// <synopsis>
// This class enable you to display and/or retrieve statistics evaluated over 
// specified regions of a lattice.  The dimension of the region is arbitrary, but 
// the size of each dimension is always the shape of the corresponding lattice axis.
// The statistics are displayed as a function of location of the axes not
// used to evaluate the statistics over.  The axes which you evaluate the statistics
// over are called the cursor axes, the others are called the display axes.
//
// For example, consider a lattice cube (call the axes xyz or [0,1,2]).  You could 
// display statistics from xy planes (cursor axes [0,1]) as a function of z (display
// axes [2]).   Or  you could retrieve statistics from the z axis (cursor axes [2])
// for each [x,y] location (display axes [0,1]).
//
// This class inherits from  <linkto class="LatticeStatsBase">LatticeStatsBase</linkto> 
// This base class provides an <src>enum</src> defining allowed statistics types and a 
// helper function to convert between a <src>String</src> and a 
// <src>Vector<Int></src> describing  the desired statistics to plot.  
// An example is shown below.
//
// This class can list, plot and retrieve statistics.  When it lists statistics,
// it always lists all the available statistics.  When you plot statistics,
// you must specify which ones you would like to see.
//
// This class generates a "storage lattice" into which it writes the accumulated
// statistical sums.  It is from this storage lattice that the plotting and retrieval
// arrays are drawn.  The storage lattice is either in core or on disk 
// depending upon its size (if > 10% of memory given by .aipsrc system.resources.memory
// then it goes into a disk-based PagedArray).  If on disk,  the
// storage lattice is deleted when the <src>LatticeStatistics</src> class 
// object destructs.    However, currently, if the process is terminated ungracefully,
// the storage lattice will be left over.  
// </synopsis>
//
// <note role=tip>
// This class has a few virtual functions; they are not part of a nice general
// polymorphic interface; rather they have specialized functionality.  The idea 
// of these is that you can derive a class from LatticeStatistics, such as 
// <linkto class="ImageStatistics">ImageStatistics</linkto> which provides
// you with a little more information when displaying/logging the 
// statistics (such as world coordinates)
// The virtual functions are
// <ul>
// <li> <src>getBeamArea</src> can be used to return the synthesized beam
//    area so that the FLUX statistic can be computed
// <li> <src>listStats</src> is used to list the statistics to the logger
// <li> <src>getLabelsM</src> find the X-axis label and the title label
//     for the plotting.   
// </ul>
// </note>
//
// <note role=tip>
// If you ignore return error statuses from the functions that set the
// state of the class, the internal status of the class is set to bad.
// This means it will just  keep on returning error conditions until you
// explicitly recover the situation.  A message describing the last
// error condition can be recovered with function errorMessage.
// </note>


// <example>
// <srcBlock>
//// Construct PagedImage (which isA MaskedLattice) from file name
//
//      PagedImage<Float> inImage(inName);
//   
//// Construct statistics object
//      
//      LogOrigin or("myClass", "myFunction(...)", WHERE);
//      LogIO os(or);
//      LatticeStatistics<Float> stats(SubImage<FLoat>(inImage), os);
//      
//// Set cursor axes to see statistics of yz planes (0 relative)
//
//      Vector<Int> cursorAxes(2)
//      cursorAxes(0) = 1;
//      cursorAxes(1) = 2;
//      if (!stats.setAxes(cursorAxes)) return 1;
//
//// Set to list and plot mean, sigma and rms
//
//      if (!stats.setList(True)) return 1;
//      String device = "/xs";
//      Vector<Int> nxy(2);
//      nxy(0) = 1;
//      nxy(1) = 1;
//      Vector<Int> statsToPlot = LatticeStatsBase::toStatisticTypes("mean,rms,sigma");
//      if (!stats.setPlotting(statsToPlot, device, nxy)) return 1;
// 
//// Now activate actual listing and plotting
// 
//      if (!stats.display ()) return 1;
//
//// Retrieve statistics into array
//
//      Array<Double> sum;
//      if (!stats.getStatistic(sum, LatticeStatsBase::SUM)) return 1;
//
// </srcBlock>
// In this example, a <src>PagedImage</src> is constructed (which isA
// MaskedLattice) with .  We set the cursor axes 
// to be the y and z axes, we specify to list the statistics if we plot them,
// and we ask to plot the mean, standard deviation, and root mean square of each 
// yz plane as a function of x location on the PGPLOT device "/xs" with
// 1 subplot per page (there will be only one in this case).  After the
// plotting and listing, we also retrieve the sum of the selected pixels
// as a function of x location into an array.
// </example>

// <motivation>
// The generation of statistical information from a lattice is a basic 
// and necessary capability.
// </motivation>

// <todo asof="1996/11/26">
//   <li> Implement plotting for complex lattices
//   <li> Retrieve statistics at specified location of display axes
// </todo>


template <class T> class LatticeStatistics : public LatticeStatsBase
{
// TypeDef

typedef typename NumericTraits<T>::PrecisionType AccumType;

public:

// Constructor takes the lattice and a <src>LogIO</src> object for logging.
// You can specify whether you want to see progress meters or not.
// You can force the storage lattice to be disk based, otherwise
// the decision for core or disk is taken for you.
   LatticeStatistics (const MaskedLattice<T>& lattice, 
                      LogIO& os,
                      Bool showProgress=True,
                      Bool forceDisk=False);

// Constructor takes the lattice only. In the absence of a logger you get no messages.
// This includes error messages and potential listing of the statistics.
// You can specify whether you want to see progress meters or not.
// You can force the storage lattice to be disk based, otherwise
// the decision for core or disk is taken for you.
   LatticeStatistics (const MaskedLattice<T>& lattice,
                      Bool showProgress=True,
                      Bool forceDisk=False);

// Copy constructor.  Copy semantics are followed.  Therefore any storage lattice 
// that has already been created for <src>other</src> is copied to <src>*this</src>
   LatticeStatistics(const LatticeStatistics<T> &other);

// Destructor
   virtual ~LatticeStatistics ();

// Assignment operator.  Deletes any storage lattice associated with
// the object being assigned to and copies any storage lattice that has
// already been created for "other".
   LatticeStatistics<T> &operator=(const LatticeStatistics<T> &other);

// Set the cursor axes (0 relative).  A return value of <src>False</src>
// indicates you have asked for an invalid axis.  The default state of the class
// is to set the cursor axes to all axes in the lattice.
   Bool setAxes (const Vector<Int>& cursorAxes);

// You may specify a pixel intensity range as either one for which 
// all pixels in that range are included or one for which all pixels 
// in that range are excluded.   One or the other of <src>include</src> 
// and <src>exclude</src> must therefore be a zero length vector if you 
// call this function.    If you are setting an <src>include</src>
// range, then if you set <src>setMinMaxToInclude=True</src>, the
// minimum and maximum values that this class returns will always be 
// the minimum and maximum of the <src>include</src> range, respectively.
// A return value of <src>False</src> indicates that 
// you have given both an <src>include</src> and an <src>exclude</src> 
// range.  A vector of length 1 for <src>include</src> and/or <src>exclude</src>
// means that the range will be set to (say for <src>include</src>)
// <src>-abs(include(0))</src> to <src>abs(include(0))</src>.  A return value
// of <src>False</src> indicates that both an inclusion and exclusion 
// range were given or that the internal state of the class is bad.   If you don't
// call this function, the default state of the class  is to include all pixels.
   Bool setInExCludeRange(const Vector<T>& include,
                          const Vector<T>& exclude,
                          Bool setMinMaxToInclude=False);

// This function allows you to control whether the statistics are written to
// the output stream if you are also making a plot.  A return value of 
// <src>False</src> indicates that the internal state of the class is bad.
// If you have created the <src>LatticeStatistics</src> object without
// a <src>LogIO</src> object, you won't see any listings, but no error
// conditions will be generated.  The default state of the class is to 
// not list the output when making a plot. 
   Bool setList(const Bool& doList);

// This functions enable you to specify which statistics you would like to
// plot, sets the name of the PGPLOT plotting device and the number of
// subplots in x and y per page.   If you set <src>device</src> 
// but offer a zero length array for <src>nxy</src> then <src>nxy</src> is 
// set to [1,1].  Similarly, the default for <src>statsToPlot</src> is
// to plot the mean and standard deviation. Use the helper function
// <src>LatticeStatsBase::toStatisticTypes(String& stats)</src> to convert 
// a <src>String</src> to the desired <src>Vector<Int> statsToPlot</src>.  
// A return value of <src>False</src> indicates invalid plotting arguments
// or that the internal state of the class is bad.  If you don't call this function,
// the default state of the class is to not make plots.
   Bool setPlotting(PGPlotter& plotter,
                    const Vector<Int>& statsToPlot,
                    const Vector<Int>& nxy);

// Display the statistics by listing and/or plotting them.  If you don't call
// this function then you won't see anything !  A return value of <src>False</src>
// indicates an invalid plotting device, or that the internal state of the class is bad.
   Bool display ();

// CLose plotter
   void closePlotting();


// Return the display axes.  The returned vector will be valid only if <src>setAxes</src>
// has been called, or if one of the active "display" or "get*" methods has been called. 
   Vector<Int> displayAxes() const {return displayAxes_p;} 

// Recover the desired Statistic into an array.  If you choose to use
// the T version, be aware that the values in the AccumType version of the
// Array may not be representable in the T version (e.g. large values for
// SumSq).  The shape of the
// array is the shape of the display axes (e.g. if the shape of the lattice is
// [nx,ny,nz] and you ask for the mean of the y axis the shape of the returned
// array would be [nx,nz].    A returned array of zero shape indicates that there 
// were no good values.   A return   value of <src>False</src> 
// indicates that the internal state of  the class is bad.
// <group>
   Bool getStatistic (Array<AccumType>& stat, LatticeStatsBase::StatisticsTypes type, Bool dropDeg=True);
   Bool getConvertedStatistic (Array<T>& stat, LatticeStatsBase::StatisticsTypes type, Bool dropDeg=True);
// </group>

// Recover position of min and max. Only works if there are no
// display axes (i.e. statistics found over entire image), otherwise,
// the returned values are resized to 0 shape.  A return  
// value of <src>False</src> indicates that the internal state of 
// the class is bad.
   Bool getMinMaxPos(IPosition& minPos, IPosition& maxPos);

// This function gets a vector containing all the statistics
// for a given location.  If <src>posInLattice=True</src> then
// the location is a location in the input lattice.  Any
// positions on the display axes are ignored.  Otherwise, you
// should just give locations for the display axes only.
// Use can use the enum in class LatticeStatsBase to find out
// which locations in the vector contain which statistics.
// A returned vector of zero shape indicates that there 
// were no good values. A return  value of <src>False</src> 
// indicates that the  internal state of the class is bad.
   Bool getStats (Vector<AccumType>&,
                  const IPosition& pos,
                  const Bool posInLattice=False);

// Reset argument error condition.  If you specify invalid arguments to
// one of the above <src>set</src> functions, an internal flag will be set which will
// prevent the work functions from doing anything (should you have chosen 
// to ignore the Boolean return values of the <src>set</src> functions).
// This function allows you to reset that internal state to good.
   void resetError () {goodParameterStatus_p = True;};

// Get full lattice min and max only.  Returns False if no unmasked data, else returns True.
// Honours any include or exclude range if set.
   Bool getFullMinMax (T& dataMin, T& dataMax);

// Recover last error message
   String errorMessage() const {return error_p;};

// Set a new MaskedLattice object.  A return value of <src>False</src> indicates the 
// lattice had an invalid type or that the internal state of the class is bad.
   Bool setNewLattice(const MaskedLattice<T>& lattice);

// Did we construct with a logger ?
   Bool hasLogger () const {return haveLogger_p;};

protected:

   LogIO os_p;
   Vector<Int> cursorAxes_p, displayAxes_p;
   Bool goodParameterStatus_p;
   Bool haveLogger_p, fixedMinMax_p;

// doRobust means that when the storage lattice is generated, the
// robust statistics are generated as well

   Bool doRobust_p;
   Bool doList_p;
   IPosition minPos_p, maxPos_p, blcParent_p;
   String error_p;
//
// Virtual Functions.  See implementation to figure it all out !
//
// Get beam volume if possible.  Your lattice needs to be
// an ImageInterface for you to be able to do this.
// See for example, class ImageStatistics.  When you provide
// the beam, then the Flux statistic, if requested, can be
// computed.  Returns False if beam not available, else True.
// The implementation here returns False.
   virtual Bool getBeamArea (Double& beamArea) const;

   virtual void listMinMax (ostringstream& osMin,
                            ostringstream& osMax,
                            Int oWidth, DataType type);

// List the statistics to the logger.   The implementation here
// is adequate for all lattices.  See ImageStatistics for an
// example of where extra information is provided. hasBeam is
// the return value of getBeamArea. If it is true, that means
// that the FLUX statistics will be available in the storage
// lattice.  dPos is the location of the start of the cursor in the
// storage image for this row.  stats(j,i) is the statistics matrix.
// for the jth point and the ith statistic.
// The return value is False if something goes wrong !
// Have a look at the implementation to see what you really
// have to do.
   virtual Bool listStats (Bool hasBeam, const IPosition& dPos,
                           const Matrix<AccumType>& ord);

// Gets labels for higher order axes and x axis.
// dPos is the location of the start of the cursor in the
// storage image for this row. 
   virtual void getLabels(String& higherOrderLabel, String& xAxisLabel,
                          const IPosition& dPos) const;

// Given a location in the storage lattice, convert those locations on the   
// non-statistics axis (the last one) and optionally account for the 
// lattice subsectioning
   IPosition locInLattice (const IPosition& storagePosition,
                           Bool relativeToParent=True) const;
 
// Non-virtual functions
//
// set stream manipulators
   void setStream (ostream& os, Int oPrec);

private:
   const MaskedLattice<T>* pInLattice_p;
   TempLattice<AccumType>* pStoreLattice_p;
   Vector<Int> nxy_p, statsToPlot_p;
   Vector<T> range_p;
   PGPlotter plotter_p;
   Bool noInclude_p, noExclude_p;
       
   Bool needStorageLattice_p, doneSomeGoodPoints_p, someGoodPointsValue_p;
   Bool showProgress_p, forceDisk_p;
//
   T minFull_p, maxFull_p;
   Bool doneFullMinMax_p;

// Summarize the statistics found over the entire lattice
   virtual void summStats();
   virtual void displayStats( AccumType nPts, AccumType sum, AccumType median,
	   AccumType medAbsDevMed, AccumType quartile, AccumType sumSq, AccumType mean,
	   AccumType var, AccumType rms, AccumType sigma, AccumType dMin, AccumType dMax );

// Calculate statistic from storage lattice and return in an array
   Bool calculateStatistic (Array<AccumType>& slice, 
                            LatticeStatsBase::StatisticsTypes type,
                            Bool dropDeg);

// Convert a <AccumType> to a <Float> for plotting
   static Float convertATtoF (AccumType value) {return Float(std::real(value));};

// Find the next good or bad point in an array
   Bool findNextDatum     (uInt& iFound,
                           const uInt& n,
                           const Vector<AccumType>& mask,
                           const uInt& iStart,
                           const Bool& findGood) const;

// Find the next label in a list of comma delimitered labels
   Bool findNextLabel     (String& subLabel,
                           Int& iLab,
                           String& label) const;

// Find the median per cursorAxes chunk
   void generateRobust (); 

// Create a new storage lattice
   Bool generateStorageLattice (); 

// Examine an array and determine how many segments of good points it consists 
// of.    A good point occurs if the array value is greater than zero.
   void lineSegments (uInt& nSeg,
                      Vector<uInt>& start,
                      Vector<uInt>& nPts,
                      const Vector<AccumType>& mask) const;

// Given a location in the lattice and a statistic type, work
// out where to put it in the storage lattice
   IPosition locInStorageLattice(const IPosition& latticePosition,
                                 LatticeStatsBase::StatisticsTypes type) const;

// Draw each Y-axis sublabel in a string with a different colour
   void multiColourYLabel (String& label,
                           PGPlotter& plotter,
                           const String& LRLoc,
                           const Vector<uInt>& colours,
                           const Int& nLabs) const;

// Plot an array which may have some blanked points.
// Thus we plot it in segments         
   void multiPlot        (PGPlotter& plotter,
                          const Vector<AccumType>& x,
                          const Vector<AccumType>& y,
                          const Vector<AccumType>& n) const;

// Find min and max of good data in arrays specified by pointers
   void minMax            (Bool& none, AccumType& dMin, AccumType& dMax,
                           const Vector<AccumType>& d,
                           const Vector<AccumType>& n) const;

// Find the next nice PGPLOT colour index 
   Int niceColour         (Bool& initColours) const; 

// Plot the statistics
   Bool plotStats         (Bool hasBeam, const IPosition& dPos, 
                           const Matrix<AccumType>& ord,
                           PGPlotter& plotter);

// Retrieve a statistic from the storage lattice and return in an array
   Bool retrieveStorageStatistic (Array<AccumType>& slice, 
                                  LatticeStatsBase::StatisticsTypes type, 
                                  Bool dropDeg);

// Retrieve a statistic from the storage lattice at the specified
// location and return in an array
   Bool retrieveStorageStatistic (Vector<AccumType>& slice, 
                                  const IPosition& pos,
                                  const Bool posInLattice);

// Find the shape of slice from the statistics lattice at one
// spatial pixel
   IPosition statsSliceShape () const; 

// See if there were some valid points found in the storage lattice
   Bool someGoodPoints ();  


// Stretch min and max by 5%
   void stretchMinMax (AccumType& dMin, AccumType& dMax) const;
};




// <summary> Generate statistics, tile by tile, from a masked lattice </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=LatticeApply>LatticeApply</linkto>
//   <li> <linkto class=TiledCollapser>TiledCollapser</linkto>
// </prerequisite>
//
// <etymology>
// This class is used by <src>LatticeStatistics</src> to generate
// statistical sum from an input <src>MaskedLattice</src>.
// The input lattice is iterated through in tile-sized chunks
// and fed to an object of this class.
// </etymology>
//
// <synopsis>
// <src>StatsTiledCollapser</src> is derived from <src>TiledCollapser</src> which
// is a base class used to define methods.  Objects of this base class are
// used by <src>LatticeApply</src> functions.  In this particular case,
// we are interested in <src>LatticeApply::tiledApply</src>.  This  function iterates
// through a <src>MaskedLattice</src> and allows you to collapse one or more
// axes, computing some values from it, and placing those values into
// an output <src>MaskedLattice</src>.  It iterates through the input
// lattice in optimal tile-sized chunks.    <src>LatticeStatistics</src> 
// uses a <src>StatsTiledCollapser</src> object which it gives to 
// <src>LatticeApply::tiledApply</src> for digestion.  After it has
// done its work, <src>LatticeStatistics</src> then accesses the output
// <src>Lattice</src> that it made.
// </synopsis>
//
// <example>
// <srcblock>
//// Create collapser. Control information is passed in via the constructor
//
//   StatsTiledCollapser<T> collapser(range_p, noInclude_p, noExclude_p,   
//                                    fixedMinMax_p, blcParent_p);
// 
//// This is the first output axis getting  collapsed values. In LatticeStatistics
//// this is the last axis of the output lattice
// 
//   Int newOutAxis = outLattice.ndim()-1;
//
//// tiledApply does the work by passing the collapser data in chunks
//// and by writing the results into the output lattice 
//
//   LatticeApply<T>::tiledApply(outLattice, inLattice,
//                               collapser, collapseAxes,
//                               newOutAxis);
//
// </srcblock>
// In this example, a collapser is made and passed to LatticeApply.
// Afterwards, the output Lattice is available for use.
// The Lattices must all be the correct shapes on input to tiledApply
// </example>
//
// <motivation>
// The LatticeApply classes enable the ugly details of optimal
// Lattice iteration to be hidden from the user.
// </motivation>
//
// <todo asof="1998/05/10">   
//   <li> 
// </todo>

template <class T, class U=T>
class StatsTiledCollapser : public TiledCollapser<T,U>
{
public:
// Constructor provides pixel selection range and whether that
// range is an inclusion or exclusion range.  If <src>fixedMinMax=True</src>
// and an inclusion range is given, the min and max is set to
// that inclusion range.  
    StatsTiledCollapser(const Vector<T>& pixelRange, Bool noInclude, 
                        Bool noExclude, Bool fixedMinMax);

// Initialize process, making some checks
    virtual void init (uInt nOutPixelsPerCollapse);

// Initialiaze the accumulator
    virtual void initAccumulator (uInt n1, uInt n3);

// Process the data in the current chunk. 
    virtual void process (uInt accumIndex1, 
                          uInt accumIndex3, 
                          const T* inData, 
                          const Bool* inMask,
                          uInt inIncr, 
                          uInt nrval,
                          const IPosition& startPos, 
                          const IPosition& shape);

// End the accumulation process and return the result arrays
    virtual void endAccumulator(Array<U>& result,
                                Array<Bool>& resultMask,
                                const IPosition& shape);

// Can handle null mask
   virtual Bool canHandleNullMask() const {return True;};

// Find the location of the minimum and maximum data values
// in the input lattice.
   void minMaxPos(IPosition& minPos, IPosition& maxPos);


private:
    Vector<T> range_p;
    Bool noInclude_p, noExclude_p, fixedMinMax_p;
    IPosition minPos_p, maxPos_p;

// Accumulators for sum, sum squared, number of points
// minimum, and maximum

    Block<U> *pSum_p;
    Block<U> *pSumSq_p;
    Block<U>* pNPts_p;
    Block<T>* pMin_p;
    Block<T>* pMax_p;
    Block<Bool>* pInitMinMax_p;
//
    uInt n1_p;
    uInt n3_p;
};

} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <lattices/Lattices/LatticeStatistics.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
