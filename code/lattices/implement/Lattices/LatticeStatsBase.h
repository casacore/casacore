//# LatticeStatsBase.h: base class for LatticeStatistics class
//# Copyright (C) 1996,1999,2000
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

#if !defined(AIPS_LATTICESTATSBASE_H)
#define AIPS_LATTICESTATSBASE_H

#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
class IPosition;
class Regex;

// <summary> Base class for LatticeStatistics class</summary>
// <use visibility=export>
// 
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
// 
// <prerequisite>
//   <li> Vector
//   <li> String
// </prerequisite>
//
// <etymology>
// A simple base class for the <linkto class="LatticeStatistics">LatticeStatistics</linkto> class
// </etymology>
//
// <synopsis>
// This base class provides an <src>enum</src> defining allowed statistics types 
// and a helper function to convert between a <src>String</src> and a
// <src>Vector<Int></src> describing the desired statistics to plot.  The reason for
// having it as a base class rather than just part of LatticeStatistics is that
// the latter is templated, and it doesn't make much sense to invoke the static function
// <src>setStatisticTypes</src> function with a templated type.
// </synopsis>
//
// <example>
// <srcBlock>
//    Vector<Int> statsToPlot = LatticeStatsBase::toStatisticTypes("mean,rms,sigma");
// </srcBlock>
// </example>
//
// <motivation>
// My sensibilities were offended at having to say
//
//    <src>Vector<Int> statsToPlot = LatticeStatistics<Float>::toStatisticTypes("mean,rms,sigma");</src>
//
// when the <src><Float></src> was meaningless.
// </motivation>
//
// <todo asof="yyyy/mm/dd">
// </todo>


class LatticeStatsBase
{
public:

// This <src>enum StatisticTypes</src> is provided for use with the
// <src>LatticeStatistics<T>::setPlotting</src> function.  It gives the allowed 
// statistics types that you can ask for.
   
enum StatisticsTypes {
 
// The number of points
   NPTS,

// The sum
   SUM,

// The sum squared
   SUMSQ,

// The median - does not fit well into storage lattice approach
   MEDIAN,

// The minimum
   MIN,

// The maximum
   MAX,

// The mean
   MEAN,

// The variance about the mean
   VARIANCE,

// The standard deviation about the mean
   SIGMA,

// The rms
   RMS,

// The flux density (can't always compute this - needs the beam)
   FLUX,

// The total number of available statistics to plot
   NSTATS,

// The total number of accumulation image items (not for general use:
// note that the accumulation items MUST come first in this enum)
   NACCUM = MAX+1
};

// Helper function to convert a String containing a list of desired statistics to
// the correct Vector<Int> required for the LatticeStatistics<T>::setPlotting
// function.  This may be usful if your user interface involves strings rather than integers.
// A new value is added to the output vector (which is resized appropriately) if any of the
// substrings "npts", "min", "max", "sum", "sumsq", "mean", "sigma", "rms", 
// and "flux" is present.  An empty vector results if there are no matches
   static Vector<Int> toStatisticTypes (const String& statistics, 
                                        const Regex& delimiter);
   static Vector<Int> toStatisticTypes (const Vector<String>& statistics);

// Returns -1 if the statistic string is not valid
   static Int toStatisticType (const String& statistic);

// Check and fill in defaults for a <src>Vector<Int></src> containing the 
// number of subplots in x and y to be put on a plot.  The <src>Vector<Int></src> 
// is resized to 2 before assignment.  A return value of <src>False</src> indicates 
// invalid arguments.
   static Bool setNxy (Vector<Int>& nxy,
                       ostream& os);

// A storage image is used to accumulate information as a function of the display
// axes as an image is iterated through.  This function sets the storage image shape 
// to that appropriate to the shape of the display axes and the desired size of the first
// or last dimension.  
   static void setStorageImageShape (IPosition& storeImageShape,
                                     const Bool& last,
                                     const Int& axisSize,
                                     const Vector<Int>& displayAxes,
                                     const IPosition& shape);

// Stretch a range by 10%
   static void stretchMinMax (Float& min, 
                              Float& max);
};

#endif

