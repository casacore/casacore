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

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/lattices/LatticeMath/LatticeStatsBase.h>
#include <casacore/lattices/LatticeMath/TiledCollapser.h>
#include <casacore/lattices/LatticeMath/TiledCollapser.h>
#include <casacore/lattices/LEL/LatticeExprNode.h>
#include <casacore/lattices/LatticeMath/LatticeStatsDataProvider.h>
#include <casacore/lattices/LatticeMath/MaskedLatticeStatsDataProvider.h>
#include <casacore/scimath/Mathematics/NumericTraits.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/scimath/Mathematics/FitToHalfStatisticsData.h>
#include <casacore/scimath/Mathematics/StatisticsData.h>
#include <vector>
#include <list>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class MaskedLattice;
template <class T> class TempLattice;
class IPosition;

template <class AccumType, class T, class U> class StatisticsAlgorithm;
template <class AccumType, class T, class U> class ClassicalStatistics;

#include <casacore/casa/iosstrfwd.h>


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

public:


	typedef typename NumericTraits<T>::PrecisionType AccumType;

	struct AlgConf {
		StatisticsData::ALGORITHM algorithm;
		// hinges-fences f factor
		Double hf;
		// fit to have center type
		FitToHalfStatisticsData::CENTER ct;
		// fit to half data portion to use
		FitToHalfStatisticsData::USE_DATA ud;
		// fit to half center value
		AccumType cv;
		// Chauvenet zscore
		Double zs;
		// Chauvenet max iterations
		Int mi;
	};

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

// Display the statistics by listing and/or plotting them.  If you don't call
// this function then you won't see anything !  A return value of <src>False</src>
// indicates an invalid plotting device, or that the internal state of the class is bad.

   Bool display();

   Bool getLayerStats(String& stats, Double area, 
                      Int zAxis=-1, Int zLayer=-1, 
                      Int hAxis=-1, Int hLayer=-1); 

   typedef std::pair<String,String> stat_element;
   typedef std::list<stat_element> stat_list;
   Bool getLayerStats( stat_list &stats, Double area, 
                      Int zAxis=-1, Int zLayer=-1, 
                      Int hAxis=-1, Int hLayer=-1); 

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

   // configure object to use Classical Statistics
   // The time, t_x, it takes to compute classical statistics using algorithm x, can
   // be modeled by
   // t_x = n_sets*(a_x + b_x*n_el)
   // where n_sets is the number of independent sets of data to compute stats on,
   // each containing n_el number of elements. a_x is the time it takes to compute
   // stats a a single set of data, and b_x is the time it takes to accumulate
   // a single point.
   // The old algorithm was developed in the early history of the project, I'm guessing
   // by Neil Kileen, while the new algorithm was developed in 2015 by Dave Mehringer
   // as part of the stats framework project. The old algorithm is faster in the regime
   // of large n_sets and small n_el, while the new algorithm is faster in the
   // regime of small n_sets and large n_el.
   // If one always wants to use one of these algorithms, that algorithm's coefficients
   // should be set to 0, while setting the other algorithm's coefficients to positive
   // values. Note that it's the relative, not the absolute, values of these
   // coeffecients that is important
   // The version that takes no parameters uses the default values of the coefficients;
   // <group>
   void configureClassical();

   void configureClassical(Double aOld, Double bOld, Double aNew, Double bNew);
   // </group>

   // configure to use fit to half algorithm.
   void configureFitToHalf(
		   FitToHalfStatisticsData::CENTER centerType=FitToHalfStatisticsData::CMEAN,
		   FitToHalfStatisticsData::USE_DATA useData=FitToHalfStatisticsData::LE_CENTER,
		   AccumType centerValue=0
   );

   // configure to use hinges-fences algorithm
   void configureHingesFences(Double f);

   // configure to use Chauvenet's criterion
   void configureChauvenet(
		   Double zscore=-1, Int maxIterations=-1
   );

   // get number of iterations associated with Chauvenet criterion algorithm
   std::map<String, uInt> getChauvenetNiter() const { return _chauvIters; }

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

   // FIXME The indirect dependence of this class on ImageInterface related
   // issues (eg flux density) breaks encapsulation. All the ImageInterface related code should be
   // encapsulated in ImageStatistics. Unfortunately, that requires significantly
   // more time than I have atm. A return value of False means that the object in
   // question cannot compute flux density values. The default implementation returns False.
   virtual Bool _canDoFlux() const { return False; }

   virtual Quantum<AccumType> _flux(Bool&, AccumType, Double) const {
	   ThrowCc("Logic Error: This object cannot compute flux density");
   }

   virtual void listMinMax (ostringstream& osMin,
                            ostringstream& osMax,
                            Int oWidth, DataType type);

   //

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
   virtual Bool listLayerStats (
             const Matrix<AccumType>& ord,
             ostringstream& rslt, Int zLayer); 
/*
// Gets labels for higher order axes and x axis.
// dPos is the location of the start of the cursor in the
// storage image for this row. 
   virtual void getLabels(String& higherOrderLabel, String& xAxisLabel,
                          const IPosition& dPos) const;
*/
// Given a location in the storage lattice, convert those locations on the   
// non-statistics axis (the last one) and optionally account for the 
// lattice subsectioning
   IPosition locInLattice (const IPosition& storagePosition,
                           Bool relativeToParent=True) const;
 
// Non-virtual functions
//
// set stream manipulators
   void setStream (ostream& os, Int oPrec);

   // get the storage lattice shape
   inline IPosition _storageLatticeShape() const { return pStoreLattice_p->shape(); }

   virtual Bool _computeFlux(
		Array<AccumType>& flux, const Array<AccumType>& npts, const Array<AccumType>& sum
   );

   virtual Bool _computeFlux(
		   Quantum<AccumType>& flux, AccumType sum, const IPosition& pos,
		   Bool posInLattice
   );

   // convert a position in the input lattice to the corresponding
   // position in the stats storage lattice. The number of elements
   // in storagePos will not be changed and only the first N elements
   // will be modified where N = the number of elements in latticePos.
   // <src>storagePos</src> must therefore have at least as many elements
   // as <src>latticePos</src>. Returns False if
   //<src>latticePos</src> is inconsistent with the input lattice.
   void _latticePosToStoragePos(
		   IPosition& storagePos, const IPosition& latticePos
   );

private:

   const MaskedLattice<T>* pInLattice_p;
   CountedPtr<TempLattice<AccumType> > pStoreLattice_p;
   Vector<Int> nxy_p, statsToPlot_p;
   Vector<T> range_p;
   Bool noInclude_p, noExclude_p;
       
   Bool needStorageLattice_p, doneSomeGoodPoints_p, someGoodPointsValue_p;
   Bool showProgress_p, forceDisk_p;

   T minFull_p, maxFull_p;
   Bool doneFullMinMax_p;

   AlgConf _algConf;
   std::map<String, uInt> _chauvIters;

   Double _aOld, _bOld, _aNew, _bNew;

   void _setDefaultCoeffs() {
	   // coefficients from timings run on PagedImages on
	   // etacarinae.cv.nrao.edu (dmehring's development
	   // machine)
       _aOld = 4.7e-7;
       _bOld = 2.3e-8;
       _aNew = 1.6e-5;
       _bNew = 1.5e-8;
   }

// Summarize the statistics found over the entire lattice
   virtual void summStats();

	   virtual void displayStats(
		   AccumType nPts, AccumType sum, AccumType median,
		   AccumType medAbsDevMed, AccumType quartile, AccumType sumSq, AccumType mean,
		   AccumType var, AccumType rms, AccumType sigma, AccumType dMin, AccumType dMax,
		   AccumType q1, AccumType q3
	   );

// Calculate statistic from storage lattice and return in an array
   Bool calculateStatistic (Array<AccumType>& slice, 
                            LatticeStatsBase::StatisticsTypes type,
                            Bool dropDeg);

// Find the median per cursorAxes chunk
   void generateRobust (); 

// Create a new storage lattice
   Bool generateStorageLattice (); 

// Given a location in the lattice and a statistic type, work
// out where to put it in the storage lattice
   IPosition locInStorageLattice(const IPosition& latticePosition,
                                 LatticeStatsBase::StatisticsTypes type) const;

// Find min and max of good data in arrays specified by pointers
   void minMax            (Bool& none, AccumType& dMin, AccumType& dMax,
                           const Vector<AccumType>& d,
                           const Vector<AccumType>& n) const;

// Retrieve a statistic from the storage lattice and return in an array
   Bool retrieveStorageStatistic (Array<AccumType>& slice,
                                  const LatticeStatsBase::StatisticsTypes type,
                                  const Bool dropDeg);

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

   CountedPtr<StatisticsAlgorithm<AccumType, const T*, const Bool*> > _createStatsAlgorithm() const;

   void _configureDataProviders(
		   LatticeStatsDataProvider<T>& lattDP,
		   MaskedLatticeStatsDataProvider<T>& maskedLattDP
	) const;

   void _doStatsLoop(uInt nsets, CountedPtr<LattStatsProgress> progressMeter);
};

} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LatticeMath/LatticeStatistics.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
