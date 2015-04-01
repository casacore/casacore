//# MSRange.h: this defines MSRange, which determines ranges of ms values 
//# Copyright (C) 1997,1998,1999,2000,2001
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

#ifndef MS_MSRANGE_H
#define MS_MSRANGE_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MSSel/MSSelectionKeywords.h>
namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T> class ArrayColumn;
template <class T> class ScalarColumn;
class Record;
class MSSelector;
// <summary>
// MSRange determines ranges of values in a MeasurementSet
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MeasurementSet
//   <li> Record
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
// than one DATA_DESC_ID, it can be preselected on this to allow
// a consistent set of frequencies to be returned.
// The ms DO provides access to this class from glish and GUIs.
//
// <example> <srcblock>
// MSRange myRange(myMS);
// Vector<String> items(3); 
// // fill in some fields
// items(0)="field_id";
// items(1)="time";
// items(2)="data_desc_id";
// // get the range of values for the items specified
// cout << myRange.range(items)<<endl;
// // sample output: range=[field_id=[0,1,2],time=[4.5e9, 4.51e9],
// //   data_desc_id=[0,1,2]];
// // Now preselect on data_desc_id
// MSSelector mss(myMS);
// Vector<Int> dd(2); dd(0)=1; dd(1)=2;
// mss.selectinit(0,dd); // select data desc ids 1 and 2
// MSRange r2(mss);
// items(2)="amplitude";
// cout<< r2.range(items)<<endl;
// // sample output: [field_id=[0,1,2],time=[4.5e9, 4.51e9],
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

  // Default constructor, only useful to assign to.
  MSRange();
  
  // Construct from an MS.
  explicit MSRange(const MeasurementSet& ms);
	  
  // construct from an MSSelector, if this constructor is used, the data
  // will be channel selected and polarization converted as specified in
  // the MSSelector object, and the current selection is used in the range.
  explicit MSRange(const MSSelector& msSel);

  // Copy constructor
  MSRange(const MSRange& other);
  
  // Assignment
  MSRange& operator=(const MSRange& other);
   
  // Return the range of values for each of the items specified in 
  // the record. For index-like items a list of values is returned,
  // for non-index items the minimum and maximum are returned.
  // Items with varying array shape will not be returned by this function (i.e.
  //  you may need to preselect the MS passed to MSRange).
  // See the enum description in MSSelector for the list of supported items.
  // Use the data flags if useFlags is True.
  // Correct for one-based indexing if oneBased is True.
  Record range(const Vector<String>& items, 
		    Bool useFlags=True,
		    Bool OneBased=False);

  // Same as previous function, with Vector of MSS::Field keys instead
  // of Strings
  Record range(const Vector<Int>& items, 
		    Bool useFlags=True,
		    Bool OneBased=False);

  // Similar to above, with a single enum, for convenience
  Record range(MSS::Field item,
		    Bool useFlags=True);

  // Set the block size (in Mbytes) to use when reading the data column.
  // The default is 10 MB. Actual memory used is higher due to 
  // temporaries and caching.
  void setBlockSize(Int blockSize=10);

protected:

  // check the data description selection (one or more with same shape, or
  // varying shape)
  Bool checkShapes();

  // get the range of a ScalarColumn<Int>, correct for 1-based 
  // indexing if oneBased is True, and add to out record.
  void scalarRange(Record& out, const String& item, 
		   const ScalarColumn<Int>& id, Bool oneBased);

  // get the range of a ScalarColumn<Int>
  Vector<Int> scalarRange(const ScalarColumn<Int>& id);

  // get the minimum and maximum of a Complex data column, after
  // application of some function to convert to Float (e.g., real,
  // amplitude,...). This function reads the data in blocks of
  // size blockSize, as set by the setBlockSize function.
  void minMax(Matrix<Float>& minmax, 
	      const Vector<Bool>& funcSel,
	      const ArrayColumn<Complex>& data1,
	      const ArrayColumn<Complex>& data2,
	      const ArrayColumn<Bool>& flag,
	      Int dataType,
	      Bool useFlags);

  // get the minimum and maximum of a Float data column
  // This function reads the data in blocks of
  // size blockSize, as set by the setBlockSize function.
  void minMax(Float& mini, Float& maxi, 
	      const ArrayColumn<Float>& data,
	      const ArrayColumn<Bool>& flag,
	      Bool useFlags);

  // Get the range of interferometer numbers given the antenna1 and antenna2
  // columns.
  Vector<Int> ifrNumbers(const ScalarColumn<Int>& ant1,
			 const ScalarColumn<Int>& ant2);

private:
  // The function types
  enum {Amp,Phase,Real,Imag,Data,nFuncType};

  // The data types
  enum {Observed,Corrected,Model,Ratio,Residual,ObsResidual,ObsFloat,nDataType};

  MeasurementSet ms_p; // the original ms
  Int blockSize_p;
  Vector<Int> ddId_p;
  Vector<uInt> spwId_p;
  Vector<uInt> polId_p;
  Bool constantShape_p;
  const MSSelector* sel_p;
};


} //# NAMESPACE CASACORE - END

#endif


