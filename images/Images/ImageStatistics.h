//# ImageStatistics.h: generate statistics from an image
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2003
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

#ifndef IMAGES_IMAGESTATISTICS_H
#define IMAGES_IMAGESTATISTICS_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/lattices/LatticeMath/LatticeStatistics.h>
#include <casacore/scimath/Mathematics/NumericTraits.h>
#include <casacore/casa/iosstrfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class ImageInterface;
class IPosition;

// <summary>
// Displays various statistics from an image.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LatticeStatistics>LatticeStatistics</linkto> (base class)
//   <li> <linkto class=ImageInterface>ImageInterface</linkto>
// </prerequisite>

// <etymology>
// This is a class designed to display and retrieve statistics from images
// </etymology>

// <synopsis>
// This class enable you to display and/or retrieve statistics evaluated over 
// specified regions from an image.  The dimension of the region is arbitrary, but 
// the size of each dimension is always the size of the corresponding image axis.
// The statistics are displayed as a function of location of the axes not
// used to evaluate the statistics over.  The axes which you evaluate the statistics
// over are called the cursor axes, the others are called the display axes.
//
// This class is derived from the class LatticeStatistics which does all
// the work.  This class only adds some extra capability in terms of
// logging world (rather than pixel) coordinates and computing the
// synthesized beam area, if there is one.  There are just a few virtual
// functions for you to over-ride.  These are rather specialized, they
// are not part of a general polymorphic interface, just a way to
// separate the Lattice and Image functionality out.
//
// See LatticeStatistics for details and examples.
// </synopsis>
//
// <motivation>
// The generation of statistical information from an image is a basic 
// and necessary capability.
// </motivation>

// <todo asof="1996/11/26">
//   <li> Deal with complex images at least for statistics retrieval if not
//        plotting.
//   <li> Retrieve statistics at specified location of display axes
//   <li> Standard errors on statistical quantities
//   <li> Median, other more exotic statistics. Life made difficult by
//        accumulation image approach
// </todo>


template <class T> class ImageStatistics : public LatticeStatistics<T>
{
public:

// Constructor takes the image and a <src>LogIO</src> object for logging.
// You can specify whether you want to see progress meters or not.
// You can force the storage image to be disk based, otherwise
// the decision for core or disk is taken for you.
   ImageStatistics (const ImageInterface<T>& image, 
                    LogIO& os,
                    Bool showProgress=True,
                    Bool forceDisk=False);

// Constructor takes the image only. In the absence of a logger you get no messages.
// This includes error messages and potential listing of the statistics.
// You can specify whether you want to see progress meters or not.
// You can force the storage image to be disk based, otherwise
// the decision for core or disk is taken for you.
   ImageStatistics (const ImageInterface<T>& image,
                    Bool showProgress=True,
                    Bool forceDisk=False);

// Copy constructor.  Copy semantics are followed.  Therefore any storage image 
// that has already been created for <src>other</src> is copied to <src>*this</src>
   ImageStatistics(const ImageStatistics<T> &other);

// Destructor
   virtual ~ImageStatistics ();

// Assignment operator.  Deletes any storage image associated with
// the object being assigned to and copies any storage image that has
// already been created for "other".
   ImageStatistics<T> &operator=(const ImageStatistics<T> &other);

// Set a new ImageInterface object.  A return value of <src>False</src> indicates the 
// image had an invalid type or that the internal state of the class is bad.
   Bool setNewImage (const ImageInterface<T>& image);

   void setPrecision(Int precision);

   void setBlc(const IPosition& blc);

   IPosition getBlc() const;

   Int getPrecision() const;

   // list robust statistics? Should be called before display()
   void showRobust(const Bool show);

   inline void recordMessages(const Bool rm) { _recordMessages = rm; }

   inline vector<String> getMessages() { return _messages; }

   inline void clearMessages() { _messages.resize(0); }

    void setListStats(Bool b) { _listStats = b; }
protected:

   typedef typename NumericTraits<T>::PrecisionType AccumType;

   virtual Bool _canDoFlux() const;



private:
// Data

   LogIO os_p;
   const ImageInterface<T>* pInImage_p;
   IPosition blc_;
   Int precision_;
   Bool _showRobust, _recordMessages, _listStats;
   mutable vector<String> _messages;

// Virtual functions.  See LatticeStatistics for more information
// about these, or see the implementation.

// Get label for higher order axes
   virtual void getLabels(String& higherOrder, String& xAxis, const IPosition& dPos) const;

    // Get beam area in pixels if possible. Return False if the beam area could not be
    // calculated.
    virtual Bool _getBeamArea(
    	Array<Double>& beamArea, String& msg
    ) const;

// List min and max with world coordinates
   virtual void listMinMax (ostringstream& osMin,
                            ostringstream& osMax,
                            Int oWidth, DataType type);

// List the statistics
   virtual Bool listStats (Bool hasBeam, const IPosition& dPos,
                           const Matrix<AccumType>& ord);

   virtual void displayStats(
		   AccumType nPts, AccumType sum, AccumType median,
           AccumType medAbsDevMed, AccumType quartile,
           AccumType sumSq, AccumType mean, AccumType var,
           AccumType rms, AccumType sigma, AccumType dMin,
           AccumType dMax, AccumType q1, AccumType q3
   );


   // If <src>isFluxDensity</src> is False, then the computed value is
   // a flux (ie flux density integrated over a spectral extent)
   Quantum<AccumType> _flux(
		   Bool& isFluxDensity, AccumType sum, Double beamAreaInPixels
   ) const;

   Bool _computeFlux(
		   Array<AccumType>& flux, const Array<AccumType>& npts,
		   const Array<AccumType>& sum
   );

   Bool _computeFlux(
		   Quantum<AccumType>& flux, AccumType sum, const IPosition& pos,
   		   Bool posInLattice
   );
  //# Make members of parent class known.
protected:
  using LatticeStatistics<T>::locInLattice;
  using LatticeStatistics<T>::setStream;
  using LatticeStatistics<T>::error_p;
  using LatticeStatistics<T>::goodParameterStatus_p;
  using LatticeStatistics<T>::haveLogger_p;
  using LatticeStatistics<T>::displayAxes_p;
  using LatticeStatistics<T>::cursorAxes_p;
  using LatticeStatistics<T>::doRobust_p;
  using LatticeStatistics<T>::doList_p;
  using LatticeStatistics<T>::fixedMinMax_p;
  using LatticeStatistics<T>::minPos_p;
  using LatticeStatistics<T>::maxPos_p;
  using LatticeStatistics<T>::blcParent_p;
public:
  using LatticeStatistics<T>::NPTS;
  using LatticeStatistics<T>::SUM;
  using LatticeStatistics<T>::FLUX;
  using LatticeStatistics<T>::MEAN;
  using LatticeStatistics<T>::MEDIAN;
  using LatticeStatistics<T>::RMS;
  using LatticeStatistics<T>::SIGMA;
  using LatticeStatistics<T>::MIN;
  using LatticeStatistics<T>::MAX;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/images/Images/ImageStatistics.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif

