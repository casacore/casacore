//# MSSelector.h: this defines MSSelector, which specifies MS selections
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

#ifndef MS_MSSELECTOR_H
#define MS_MSSELECTOR_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/StokesConverter.h>
#include <casacore/ms/MSOper/MSDerivedValues.h>
#include <casacore/ms/MSSel/MSSelectionKeywords.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T> class ArrayColumn;
class Record;
class MSIter;

// <summary>
// MSSelector specifies selections on a MeasurementSet
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
// MSSelector's main function is selection of data from a MeasurementSet
// </etymology>
//
// <synopsis>
// This class is used to select and retrieve data from a MeasurementSet. 
// It allows selections on e.g., time, field, spectral window (all row based),
// but also on channel and polarization (within a row). It can optionally
// do polarization conversion, spectral averaging and time averaging on the
// data retrieved and allows modified data to be written back to the Table.
// This class also provides the DO interface to the MS Iterator.
// The ms DO uses this class to allow these operations to be done from glish.
//
// <example> <srcblock>
// MSSelector msSelector(myMS);
// // select data desc Id 1
// msSelector.initSelection(1);
// Vector<String> items(3); 
// // fill in some fields
// items(0)="field_id";
// items(1)="time";
// items(2)="num_chan";
// // get the range of values for the items specified
// MSRange msRange(msSelector.selectedTable(),msSelector.spectralWindow());
// Record range=msRange.range(items);
// //.. change the ranges as needed
// // now select with the new range
// msSelector.select(range);
// int32_t nchan=10, start=3, width=1, incr=2;
// msSelector.selectChannel(nchan,start,width,incr)
// // get out some data
// Vector<String> dataItems(3);
// dataItems(0)="data";
// dataItems(1)="antenna1";
// dataItems(2)="antenna2";
// Record dataRec=msSelector.getData(items);
// </srcblock></example>
// </synopsis>
//
// <motivation>
// Selection from an MS is needed in various places. It makes sense to
// provide a uniform interface for MS selection.
// </motivation>
//
// <thrown>
//    <li>
// </thrown>
//
// <todo asof="1998/12/11">
//   <li> provide access to all other columns in the MS?
// </todo>

class MSSelector
{
  friend class MSRange;

public:

  MSSelector();
  
  // construct from an MS, the MS will supply the range of the various
  // parameters that can be selected on.
  explicit MSSelector(MeasurementSet& ms);
  
  // Copy constructor, this will initialize the MS with other's MS
  MSSelector(const MSSelector& other);
  
  // Assignment, this will initialize the MS with other's MS
  MSSelector& operator=(const MSSelector& other);

  ~MSSelector();
  
  // Change or Set the MS this MSSelector refers to.
  void setMS(MeasurementSet& ms);
  
  // initialize the selection by specifying, optionally, 
  // the DATA_DESC_IDs. 
  // If you don't specify the dataDescIds and the data shape is constant
  // all data is selected, if the shape does change, only the first 
  // dataDescId is selected. If you specify a number of dataDescIds
  // and they all have the same shape, they are all selected, otherwise
  // only the first is selected. The function returns false if
  // the selection was limited due to changing data shape.
  // Use the reset argument to return to the completely unselected ms.
  bool initSelection(const Vector<int32_t>& dataDescIds, bool reset=false);

  // As above without the data desc id argument
  bool initSelection(bool reset=false);

  // Return the data desc IDs selected 
  Vector<int32_t> dataDescId() const;

  // Set the mapping from input channels in the DATA column to
  // output channels. nChan is the number of output channels,
  // start is the first channel to use, width specifies how wide a 
  // block of channels to average, increment specifies the start of
  // the next block relative to the start of the current block.
  // Note: averaging uncalibrated data should be avoided (no bandpass applied)
  bool selectChannel(int32_t nChan, int32_t start, int32_t width, int32_t incr);

  // Specify the output polarization. 
  // Missing input polarizations are assumed to be zero.
  // This selection/conversion assumes that parallactic angle rotation
  // is taken care of elsewhere (i.e., results may only be correct for
  // CORRECTED_DATA and MODEL_DATA conversions, not for the observed DATA)
  bool selectPolarization(const Vector<String>& wantedPol);

  // Select the MS based on the selections present in the input record.
  // The format of this record is the same as that returned by range.
  // Not all possible items can be selected on, some are quietly ignored.
  // Correct for one-based indexing if oneBased is true.
  bool select(const Record& items, bool oneBased=false);

  // Select the MS based on the TaQL selection string
  bool select(const String& msSelect);

  // Return the data for the items requested, all returned values
  // will be arrays, the last dimension of these is the table row number.
  // The data arrays are normally 3D with axes: polarization, frequency, row.
  // If ifrAxis is set to true, the data arrays returned will be 4D, with
  // the data being split out along an extra interferometer axis, the
  // axes will be: polarization, frequency, interferometer and time.
  // Missing interferometers will be marked flagged.
  // The order of the interferometers is that specified by the last
  // select call.
  // Add a (flagged) gap in the data at every antenna1 change if ifrAxisGap>0.
  // Use inc > 1 to return data from every inc'th row.
  // Use average=true to vector average the data along the row or time axis 
  // taking the weights column into account (use selectChannel to average
  // channels together as well). Note that different interferometers will be
  // averaged together if ifrAxis is false.
  // Correct for one-based indexing if oneBased is true.
  Record getData(const Vector<String>& items, bool ifrAxis, 
		      int32_t ifrAxisGap=0, int32_t inc=1,
		      bool average=false, bool oneBased=false);

  // Put the data for the items provided. Note that only fields corresponding
  // to actual table columns can be put (i.e., no AMPLITUDEs, IFR_NUMBERs etc)
  // The data will need to have the correct shape for the column and a last
  // dimension matching the number of selected rows (or last two dimensions
  // matching times and interferometers, for data retrieved with ifraxis=T)
  // Channel selection is supported, but the width parameter has to be 1.
  bool putData(const Record& items);

  // Set up an iterator, iterating over the specified columns, with
  // optional time interval and maximum number of rows to return at once
  // (the default of zero returns all rows). To keep MSIter from adding  
  // the default sort columns, specify addDefaultSortColumns=false
  bool iterInit(const Vector<String>& columns,
		double interval, rownr_t maxRows=0,
		bool addDefaultSortColumns=true);
  
  // Step the iterator, sets the selection to the current table iteration.
  // Returns false if there is no more data
  // and sets the selection back to the state before iteration started.
  bool iterNext();

  // (Re)Set the iterator to the first iteration, call this after iterInit.
  bool iterOrigin();

  // End the iteration (before reaching the last iteration)
  // and set the selection back to the state before iteration started.
  bool iterEnd();

  // Number of rows in selected table
  rownr_t nrow() const;

  // Return the selected table
  Table selectedTable() const;

  // Return the selection status of the table
  bool selected() const;

protected:
  // average and convert data
  void getAveragedData(Array<Complex>& avData, const Array<bool>& flag,
		       const ArrayColumn<Complex>& col) const;

  // average and convert float data
  void getAveragedData(Array<float>& avData, const Array<bool>& flag,
		       const ArrayColumn<float>& col) const;

  // average and convert data, with row Slicer
  void getAveragedData(Array<Complex>& avData, const Array<bool>& flag,
		       const ArrayColumn<Complex>& col,
		       const Slicer & rowSlicer) const;

  // average and convert float data, with row Slicer
  void getAveragedData(Array<float>& avData, const Array<bool>& flag,
		       const ArrayColumn<float>& col,
		       const Slicer & rowSlicer) const;

  // "average" flag, at present all output which has a flagged input is flagged
  Array<bool> getAveragedFlag(Array<bool>& avFlag, 
		       const ArrayColumn<bool>& col) const;

  // "average" flag, at present all output which has a flagged input is flagged,
  // with row Slicer
  Array<bool> getAveragedFlag(Array<bool>& avFlag, 
		       const ArrayColumn<bool>& col,
		       const Slicer& rowSlicer) const;

  // "unaverage" flag, distribute the flags back to the channels that went
  // into the average
  void putAveragedFlag(const Array<bool>& avFlag,
		       ArrayColumn<bool>& col);

  // get the weight, set sigma=true when retrieving sigma's
  Array<float> getWeight(const ArrayColumn<float>& wtCol,
			 bool sigma=false) const;

  // make the data slicer, pass in the first and the number of correlations
  // to select
  void makeSlicer(int32_t start, int32_t nCorr) const;

  // reorder from 2d to 1d (removing ifr axis)
  void reorderFlagRow(Array<bool>& flagRow);

  // reorder from 2d to 1d (removing ifr axis)
  void reorderWeight(Array<float>& weight);

  // time average the input data, return new flags
  void timeAverage(Array<bool>& dataFlags, Array<Complex>& data,
		   const Array<bool>& flags, const Array<float>& weights);

  // check if the data description selection has been done & do default
  // selection if not. Return false if the selection fails.
  bool checkSelection();

private:
  // The function types
  enum {Amp,Phase,Real,Imag,Data,nFuncType};

  // The data types
  enum {Observed,Corrected,Model,Ratio,Residual,ObsResidual,ObsFloat,nDataType};

  MeasurementSet ms_p; // the original ms
  MeasurementSet selms_p; // the selected ms
  MeasurementSet savems_p; // the saved preselection
  MSIter* msIter_p;
  bool initSel_p;
  Vector<int32_t> dataDescId_p, lastDataDescId_p;
  Vector<uint32_t> spwId_p, polId_p;
  Vector<int32_t> chanSel_p;
  bool useSlicer_p;
  mutable bool haveSlicer_p;
  mutable Slicer slicer_p;
  Slice chanSlice_p,polSlice_p;
  Vector<int32_t> polIndex_p;
  int32_t wantedOne_p;
  bool convert_p, subSet_p;
  StokesConverter stokesConverter_p;
  Vector<String> polSelection_p;
  Vector<int32_t> ifrSelection_p,ifrAxis_p;
  Matrix<double> chanFreq_p,bandwidth_p;
  MSDerivedValues msd_p;
  Matrix<int64_t> rowIndex_p; // mapping of rows to time and ifr slots
  RowNumbers selRows_p; // range of rows from selms_p returned by getData
  rownr_t startRow_p, maxRow_p; // start and length of range of rows
  bool useIfrDefault_p;

};
inline rownr_t MSSelector::nrow() const { return selms_p.nrow();}
inline Vector<int32_t> MSSelector::dataDescId() const { return dataDescId_p;}
inline Table MSSelector::selectedTable() const {return selms_p;}
inline bool MSSelector::selected() const {return initSel_p;}


} //# NAMESPACE CASACORE - END

#endif

