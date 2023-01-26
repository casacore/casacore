//# MSFlagger.h: this defines MSFlagger, which implement flagging/editing
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

#ifndef MS_MSFLAGGER_H
#define MS_MSFLAGGER_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/casa/Containers/Record.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MSSelector;
class Table;
class String;

// <summary>
// MSFlagger specifies selections on a MeasurementSet
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MeasurementSet
//   <li> SomeOtherClass
//   <li> some concept
// </prerequisite>
//
// <etymology>
// MSFlagger is a class that sets flags in an MS
// </etymology>
//
// <synopsis>
// This class is used to change the flag and flag_history columns in
// a MeasurementSet. It provides functions for automated flagging based on
// clipping the data that is too far from the median value.
// The ms DO  uses this class to allow flagging from glish or a GUI.
//
// <example> <srcblock>
// MSFlagger msFlagger(myMS);
// </srcblock></example>
// </synopsis>
//
// <motivation>
// Flagging/editing of data is a central requirement in data processing, this
// class provides some simple flagging algorithms and the code
// that modifies & creates flag columns in the MS.
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> add this feature
// </todo>

class MSFlagger
{
public:
  MSFlagger();
  
  // construct from an MSSelector object
  MSFlagger(MSSelector& msSel);
  
  // Copy constructor
  MSFlagger(const MSFlagger& other);
  
  // Assignment
  MSFlagger& operator=(const MSFlagger& other);
  
  ~MSFlagger();

  // Change or Set the MS this MSFlagger refers to.
  void setMSSelector(MSSelector& msSel);
  
  // Fill an internal buffer with the data item requested, similar to getData
  // except that the data is not returned, but kept around for further
  // processing. Only a single DATA related quantity can be requested, the
  // corresponding FLAG and FLAG_ROW columns are read automatically.
  // Reorder the data to 4d with ifr and time axis if ifrAxis is true.
  bool fillDataBuffer(const String& item, bool ifrAxis);

  // Difference the data, subtracting the average over a window of
  // specified width and taking the absolute value. Complex quantities are
  // turned into the corresponding amplitude after differencing.
  // If doMedian==true the median difference is returned for window>2.
  // For a window width of one, the previous sample is
  // subtracted, giving a derivative like quantity.
  // Note that the subtraction is done on row-by-row basis for TIME
  // differencing, it is up to you to select a single baseline (if
  // you didn't use ifrAxis=true in fillDataBuffer).
  // Available directions are: TIME, CHANNEL
  // Returns statistics over the buffer: median for times and channels,
  // average absolute deviation over times, channels and all pixels.
  Record diffDataBuffer(const String& direction, int32_t window=1,
			     bool doMedian = false);

  // Return the contents of the internal data buffer, including the flags
  // as a Record
  Record getDataBuffer()
  { return buffer_p;}

  // Clip the data buffer at a specified level by setting the corresponding
  // flags in the buffer. The cliplevel is specified as a multiple of
  // the average absolute deviations returned by diffDataBuffer.
  // A value of zero or less will skip the corresponding clip operation.
  // Clipping will be done repeatedly, recalculating the deviations, until
  // no more points are clipped.
  bool clipDataBuffer(float pixelLevel, float timeLevel, float channelLevel);

  // Replace the flags in the buffer with those in the supplied record.
  // This allows interactive flagging from glish to be written back to the
  // buffer for subsequent operations. The record should contain a
  // flag and flag_row field.
  bool setDataBufferFlags(const Record& flags);

  // Write the flags in the buffer back to the table
  bool writeDataBufferFlags();

  // Clear the internal data buffer, reclaiming memory
  bool clearDataBuffer()
  { buffer_p=Record(); return true;}

  // Create the FLAG_HISTORY column and initialize it from the
  // FLAG_ROW and FLAG columns. Returns false if FLAG_HISTORY already exists.
  // The first flagging bit is filled with the flags as found in the MS,
  // subsequent bits can be used for user generated flags.
  bool createFlagHistory(int32_t nHis = 2);

  // Apply the flags in the FLAG_HISTORY column to the FLAG and FLAG_ROW
  // columns. Returns false if FLAG_HISTORY doesn't exist.
  // The default argument will apply the currently active flag level
  // (as specified by the FLAG_LEVEL column keyword).
  // Sets the current level to the flag level restored.
  bool restoreFlags(int32_t level=-1);
  
  // Save the current flags to the FLAG_HISTORY. Save to the currently
  // active level or (newLevel=true) the next highest level (if available).
  // Will reset the current level to the level saved to.
  bool saveFlags(bool newLevel);

  // Return the current flaglevel (value of FLAG_LEVEL keyword)
  int32_t flagLevel();

protected:
  // fill the FLAG_HISTORY column from the FLAG and FLAG_ROW column
  void fillFlagHist(int32_t nHis, int32_t numCorr, int32_t numChan, Table& tab);

  // find the HypercubeId column for a tiled column (if any)
  bool findHypercubeId(String& hyperCubeId, const String& column, 
		       const Table& tab);

  // copy the flags to the flag history
  void saveToFlagHist(int32_t level, Table& tab);

  // copy the flag history back to the flags
 void applyFlagHist(int32_t level, Table& tab);

  // get buffer statistics - med=median, ad=average absolute deviation,
  // T=Time, F=Frequency.
  void getStats(Array<float>& medTF, Array<float>& adTF, 
		Array<float>& medT, Array<float>& medFmedT, 
		Array<float>& adT, Array<float>& medF, 
		Array<float>& medTmedF, Array<float>& adF,
		const Array<float>& diff, const Array<bool>& flag,
		const Array<bool>& flagRow);

  // add the statistics to a buffer
  void addStats(Record& buf, const Array<bool>& flag,
		const Array<bool> flagRow, const Array<float>& data);

  // reorder from 2d to 1d (removing ifr axis)
  void reorderFlagRow(Array<bool>& flagRow);

  // collapse array "in" (with absolute differences) 
  // along specified axis by taking medians by profile taking into account
  // the flags.
  void diffMedian(Array<float>& out, const Array<float>& in, 
		  int32_t axis, const Array<bool>& flag);

  // apply the row flags to the data flags and v.v.
  void applyRowFlags(Array<bool>& flag, Array<bool>& flagRow);

  // check if we are attached to an MSSelector
  bool check();

private:
  MSSelector* msSel_p; 
  Record buffer_p;
};


} //# NAMESPACE CASACORE - END

#endif







