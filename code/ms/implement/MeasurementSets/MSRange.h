//# MSRange.h: this defines MSRange, which determines ranges of ms values 
//# Copyright (C) 1997,1998,1999,2000
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

#if !defined(TRIAL_MSRANGE_H)
#define TRIAL_MSRANGE_H

#include <aips/aips.h>
#include <aips/MeasurementSets/MeasurementSet.h>
#include <trial/MeasurementSets/MSSelectionKeywords.h>
template <class T> class ROArrayColumn;
template <class T> class ROScalarColumn;
class GlishRecord;

// <summary>
// MSRange determines ranges of values in a MeasurementSet
// </summary>

// <visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MeasurementSet
//   <li> GlishRecord
// </prerequisite>
//
// <etymology>
// MSRange is a class that determines ranges of values in an MS
// </etymology>
//
// <synopsis>
// This class is used to determine the range of values present for
// the various columns in a MeasurementSet.
// This class is initialized from a MeasurementSet. If the MS contains more
// than one SPECTRAL_WINDOW_ID, it should be preselected on this to allow
// a consistent set of frequencies to be returned.
// The ms DO provides access to this class from glish and GUIs.
//
// <example> <srcblock>
// MSRange myRange(myMS);
// Vector<String> items(3); 
// // fill in some fields
// items(0)="field_id";
// items(1)="time";
// items(2)="spectral_window_id";
// // get the range of values for the items specified
// cout << myRange.range(items)<<endl;
// // sample output: range=[field_id=[0,1,2],time=[4.5e9, 4.51e9],
// //   spectral_window_id=[0,1]];
// // Now preselect on spectral window
// MSSelector mss(myMS);
// mss.selectinit(0,1); // select spectral window 1
// myRange.setMS(mss.selectedTable());
// // Following line will avoid check by MSRange, but can be omitted for
// // a single spectral window.
// // Setting it to -1 signals multiple windows with same shape data.
// myRange.selectedSpectralWindow(1); 
// items(2)="amplitude";
// cout<< myRange.range(items)<<endl;
// // sampe output: [field_id=[0,1,2],time=[4.5e9, 4.51e9],
// //   amplitude=[0.00132,1.543]]
// </srcblock></example>
// </synopsis>
//
// <motivation>
// Finding out the range of values in a column is often needed before a 
// sensible selection of data can be made. This class, formerly part of 
// MSSelector, separates out this functionality.
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="1998/11/25">
//   <li> maybe add channel selection and polarization conversion
// </todo>

class MSRange
{
public:
  enum {
    // spectral window selection and shapes have not been checked
    UNCHECKED = -3,
    // multiple spectral windows with varying shapes
    UNSELECTED = -2,
    // multiple spectral windows with same shape
    ALL = -1
  };

  MSRange();
  
  // construct from an MS. You can use the second argument to
  // signal to MSRange that the MS provided has already been pre-selected
  // on SPECTRAL_WINDOW_ID, allowing it to skip the check on
  // this. Set spectralWindowId to MSRange::ALL if the MS contains multiple
  // spectral windows, all with the same data shape.
  // The 2nd argument is mainly for internal use by the MS DO, 
  // providing incorrect values could cause runtime failures.
  explicit MSRange(const MeasurementSet& ms, Int spectralWindowId=UNCHECKED);
  
  // Copy constructor
  MSRange(const MSRange& other);
  
  // Assignment
  MSRange& operator=(const MSRange& other);
  
  // Change or Set the MS this MSRange refers to.
  // You can use the second argument to
  // signal to MSRange that the MS provided has already been pre-selected
  // on SPECTRAL_WINDOW_ID, allowing it to skip the check on
  // this. Set spectralWindowId to MSRange::ALL if the MS contains multiple
  // spectral windows, all with the same data shape.
  // The 2nd argument is mainly for internal use by the MS DO, 
  // providing incorrect values could cause runtime failures.
  void setMS(const MeasurementSet& ms, Int spectralWindowId=UNCHECKED);
  
  // Return the range of values for each of the items specified in 
  // the record. For index-like items a list of values is returned,
  // for non-index items the minimum and maximum are returned.
  // See the enum description in MSSelector for the list of supported items.
  // Correct for one-based indexing if oneBased is True.
  GlishRecord range(const Vector<String>& items, Bool OneBased=False);

  // Same as previous function, with Vector of MSS::Field keys instead
  // of Strings
  GlishRecord range(const Vector<Int>& items, Bool OneBased=False);

  // Similar to above, with a single enum, for convenience
  GlishRecord range(MSS::Field item);

  // Set the block size (in Mbytes) to use when reading the data column.
  // The default is 10 MB. Actual memory used is higher due to 
  // temporaries and caching.
  void setBlockSize(Int blockSize=10);

protected:

  // check the spectral window selection (one, or more with same shape, or
  // unselected)
  Bool checkSelection();

  // get the range of a ScalarColumn<Int>, correct for 1-based 
  // indexing if oneBased is True, and add to out record.
  void scalarRange(GlishRecord& out, const String& item, 
		   const ROScalarColumn<Int>& id, Bool oneBased);

  // get the range of a ScalarColumn<Int>
  Vector<Int> MSRange::scalarRange(const ROScalarColumn<Int>& id);

  // get the minimum and maximum of a Complex data column, after
  // application of some function to convert to Float (e.g., real,
  // amplitude,...). This function reads the data in blocks of
  // size blockSize, as set by the setBlockSize function.
  void minMax(Float& mini, Float& maxi, 
	      Array<Float> (*func)(const Array<Complex>&),
	      const ROArrayColumn<Complex>& data);

  // Get the range of interferometer numbers given the antenna1 and antenna2
  // columns.
  Vector<Int> ifrNumbers(const ROScalarColumn<Int>& ant1,
			 const ROScalarColumn<Int>& ant2);

private:

  MeasurementSet ms_p; // the original ms
  Int blockSize_p;
  Int ddId_p;
  Bool checked_p;
};

#endif


