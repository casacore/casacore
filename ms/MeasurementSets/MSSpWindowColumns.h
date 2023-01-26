//# MSSpWindowColumns.h: provides easy access to MSSpectralWindow columns
//# Copyright (C) 1996,1999,2000,2002
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

#ifndef MS_MSSPWINDOWCOLUMNS_H
#define MS_MSSPWINDOWCOLUMNS_H

#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/measures/Measures/MCFrequency.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/measures/TableMeasures/ScalarQuantColumn.h>
#include <casacore/measures/TableMeasures/ArrayMeasColumn.h>
#include <casacore/measures/TableMeasures/ArrayQuantColumn.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/measures/Measures/MeasFrame.h>
#include <casacore/ms/MeasurementSets/MSDopplerColumns.h>
#include <casacore/ms/MeasurementSets/MSSourceColumns.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MSSpectralWindow;

// <summary>
// A class to provide easy access to MSSpectralWindow columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSSpectralWindow
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// MSSpectralWindowColumns stands for MeasurementSet SpectralWindow Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the MSSpectralWindow Table,
// it does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to
// worry about getting those right. There is an access function
// for every predefined column. Access to non-predefined columns will still
// have to be done with explicit declarations.
// See <linkto class=MSColumns> MSColumns</linkto> for an example.
// </synopsis>
//
// <motivation>
// See <linkto class=MSColumns> MSColumns</linkto> for the motivation.
// </motivation>

class MSSpWindowColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  MSSpWindowColumns(const MSSpectralWindow& msSpWindow);

  // The destructor does nothing special
  ~MSSpWindowColumns();

  // Access to required columns
  // <group>
  ArrayColumn<double>& chanFreq() {return chanFreq_p;}
  ArrayMeasColumn<MFrequency>& chanFreqMeas() {return chanFreqMeas_p;}
  ArrayQuantColumn<double>& chanFreqQuant() {return chanFreqQuant_p;}
  ArrayColumn<double>& chanWidth() {return chanWidth_p;}
  ArrayQuantColumn<double>& chanWidthQuant() { return chanWidthQuant_p;}
  ArrayColumn<double>& effectiveBW() {return effectiveBW_p;}
  ArrayQuantColumn<double>& effectiveBWQuant() { return effectiveBWQuant_p;}
  ScalarColumn<bool>& flagRow() {return flagRow_p;}
  ScalarColumn<int32_t>& freqGroup() {return freqGroup_p;}
  ScalarColumn<String>& freqGroupName() {return freqGroupName_p;}
  ScalarColumn<int32_t>& ifConvChain() {return ifConvChain_p;}
  ScalarColumn<int32_t>& measFreqRef() {return measFreqRef_p;}
  ScalarColumn<String>& name() {return name_p;}
  ScalarColumn<int32_t>& netSideband() {return netSideband_p;}
  ScalarColumn<int32_t>& numChan() {return numChan_p;}
  ScalarColumn<double>& refFrequency() {return refFrequency_p;}
  ScalarQuantColumn<double>& refFrequencyQuant() {return refFrequencyQuant_p;}
  ScalarMeasColumn<MFrequency>& refFrequencyMeas() {return refFrequencyMeas_p;}
  ArrayColumn<double>& resolution() {return resolution_p;}
  ArrayQuantColumn<double>& resolutionQuant() { return resolutionQuant_p;}
  ScalarColumn<double>& totalBandwidth() {return totalBandwidth_p;}
  ScalarQuantColumn<double>& totalBandwidthQuant() {
    return totalBandwidthQuant_p;}
  // </group>

  // Access to optional columns
  // <group>
  ArrayColumn<String>& assocNature() {return assocNature_p;}
  ArrayColumn<int32_t>& assocSpwId() {return assocSpwId_p;}
  ScalarColumn<int32_t>& bbcNo() {return bbcNo_p;}
  ScalarColumn<int32_t>& bbcSideband() {return bbcSideband_p;}
  ScalarColumn<int32_t>& dopplerId() {return dopplerId_p;}
  ScalarColumn<int32_t>& receiverId() {return receiverId_p;}
  // </group>

  // Const access to columns
  // <group>
  const ArrayColumn<double>& chanFreq() const {return chanFreq_p;}
  const ArrayQuantColumn<double>& chanFreqQuant() const {
    return chanFreqQuant_p;}
  const ArrayMeasColumn<MFrequency>& chanFreqMeas() const {
    return chanFreqMeas_p;}
  const ArrayColumn<double>& chanWidth() const {return chanWidth_p;}
  const ArrayQuantColumn<double>& chanWidthQuant() const {
    return chanWidthQuant_p;}
  const ArrayColumn<double>& effectiveBW() const {return effectiveBW_p;}
  const ArrayQuantColumn<double>& effectiveBWQuant() const {
    return effectiveBWQuant_p;}
  const ScalarColumn<int32_t>& freqGroup() const {return freqGroup_p;}
  const ScalarColumn<String>& freqGroupName() const {return freqGroupName_p;}
  const ScalarColumn<int32_t>& ifConvChain() const {return ifConvChain_p;}
  const ScalarColumn<bool>& flagRow() const {return flagRow_p;}
  const ScalarColumn<int32_t>& measFreqRef() const {return measFreqRef_p;}
  const ScalarColumn<String>& name() const {return name_p;}
  const ScalarColumn<int32_t>& netSideband() const {return netSideband_p;}
  const ScalarColumn<int32_t>& numChan() const {return numChan_p;}
  const ScalarColumn<double>& refFrequency() const {return refFrequency_p;}
  const ScalarQuantColumn<double>& refFrequencyQuant() const {
    return refFrequencyQuant_p;}
  const ScalarMeasColumn<MFrequency>& refFrequencyMeas() const {
    return refFrequencyMeas_p;}
  const ArrayColumn<double>& resolution() const {return resolution_p;}
  const ArrayQuantColumn<double>& resolutionQuant() const {
    return resolutionQuant_p;}
  const ScalarColumn<double>& totalBandwidth() const {
    return totalBandwidth_p;}
  const ScalarQuantColumn<double>& totalBandwidthQuant() const {
    return totalBandwidthQuant_p;}
  // </group>

  // Const access to optional columns
  // <group>
  const ArrayColumn<String>& assocNature() const {return assocNature_p;}
  const ArrayColumn<int32_t>& assocSpwId() const {return assocSpwId_p;}
  const ScalarColumn<int32_t>& bbcNo() const {return bbcNo_p;}
  const ScalarColumn<int32_t>& bbcSideband() const {return bbcSideband_p;}
  const ScalarColumn<int32_t>& dopplerId() const {return dopplerId_p;}
  const ScalarColumn<int32_t>& receiverId() const {return receiverId_p;}
  // </group>

  // Convenience function that returns the number of rows in any of the columns
  rownr_t nrow() const {return chanFreq_p.nrow();}

  // returns the last row that contains a spectral window that has the
  // specified reference frequency, number of channels, total-bandwidth and IF
  // conversion chain. All frequencies need to match within the specified
  // tolerance. Both the totalBandwidth & the tolerance arguments must have the
  // same dimensions as the Hz and an AipsError exception is thrown, in debug
  // mode, if the dimensions are wrong. In addition to the numerical values the
  // frequency reference frame is checked and needs to match the value in the
  // MEAS_FREQ_REF column. No conversions to other reference frames are
  // done. Will only try to match on rows where FLAG_ROW is false. If tryRow is
  // set to a non-negative value then that row is checked first to see if it
  // matches. An AIpsError exception is thrown if tryRow is bigger than the
  // number of rows in the Table. Returns -1 if no match could be found.
  int64_t matchSpw(const MFrequency& refFreq, uint32_t nChan, 
                 const Quantum<double>& bandwidth, int32_t ifChain,
                 const Quantum<double>& tolerance, int64_t tryRow=-1) const;
  // Similar to above, but also pass in the frame info.			 
  int64_t matchSpw(const MFrequency& refFreq, const MFrequency& chanFreq1,
                 const MeasFrame& measFrm,
                 const MSDopplerColumns& msdopc, const MSSourceColumns& mssrcc, uint32_t nChan, 
                 const Quantum<double>& bandwidth, int32_t ifChain,
                 const Quantum<double>& tolerance, int64_t tryRow=-1) const; 
  // This is to check that the channels are matched individually
  // and also if the spw is matched in reverse; 

  //Same as the above but returns all the possible match that it could find
  // in the spectral window table. 
  RowNumbers allMatchedSpw(const MFrequency& refFreq, uint32_t nChan, 
                           const Quantum<double>& bandwidth, int32_t ifChain,
                           const Quantum<double>& tolerance) const;

  //This version does a channel to channel match too and also return
  // the reversed if it matches but the channels are in inverse order
  // like an upper or lower side band having same characteristics
  int64_t matchSpw(const MFrequency& refFreq, uint32_t nChan, 
                 const Quantum<double>& bandwidth, int32_t ifChain,
                 const Quantum<double>& tolerance, Vector<double>& otherFreqs, 
                 bool& reversed) const;

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  MSSpWindowColumns();

  //# attach this object to the supplied table.
  void attach(const MSSpectralWindow& msSpWindow);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  MSSpWindowColumns(const MSSpWindowColumns&);
  MSSpWindowColumns& operator=(const MSSpWindowColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const MSSpectralWindow& msSpWindow);
  
  //# functions to match the supplied arguments against the values in the
  //# specified row.
  //<group>
  bool matchRefFrequency(rownr_t row, MFrequency::Types refType, 
			 double refFreqInHz, double tolInHz) const;
  bool matchRefFreqCnvtrd(rownr_t row, MFrequency refOrChanFreq, const bool isRefFreq,
                          const MeasFrame& measFrm,
                          const MSDopplerColumns& msdopc, const MSSourceColumns& mssrcc,
                          double tolInHz) const;
  bool matchChanFreq(rownr_t row, const Vector<double>& chanFreqInHz,
		     double tolInHz) const;
  bool matchIfConvChain(rownr_t row, int32_t ifChain) const;
  bool matchTotalBandwidth(rownr_t row, double bandwidthInHz,
			   double tolInHz) const;
  bool matchNumChan(rownr_t row, int32_t nChan) const;
  //</group>

  //# required columns
  ArrayColumn<double> chanFreq_p;
  ArrayColumn<double> chanWidth_p;
  ArrayColumn<double> effectiveBW_p;
  ScalarColumn<bool> flagRow_p;
  ScalarColumn<int32_t> freqGroup_p;
  ScalarColumn<String> freqGroupName_p;
  ScalarColumn<int32_t> ifConvChain_p;
  ScalarColumn<int32_t> measFreqRef_p;
  ScalarColumn<String> name_p;
  ScalarColumn<int32_t> netSideband_p;
  ScalarColumn<int32_t> numChan_p;
  ScalarColumn<double> refFrequency_p;
  ArrayColumn<double> resolution_p;
  ScalarColumn<double> totalBandwidth_p;
  //# optional columns
  ArrayColumn<String> assocNature_p;
  ArrayColumn<int32_t> assocSpwId_p;
  ScalarColumn<int32_t> bbcNo_p;
  ScalarColumn<int32_t> bbcSideband_p;
  ScalarColumn<int32_t> dopplerId_p;
  ScalarColumn<int32_t> receiverId_p;

  //# Access to Measure columns
  ArrayMeasColumn<MFrequency> chanFreqMeas_p;
  ScalarMeasColumn<MFrequency> refFrequencyMeas_p;

  //# Access to Quantum columns
  ArrayQuantColumn<double> chanFreqQuant_p;
  ArrayQuantColumn<double> chanWidthQuant_p;
  ArrayQuantColumn<double> effectiveBWQuant_p;
  ScalarQuantColumn<double> refFrequencyQuant_p;
  ArrayQuantColumn<double> resolutionQuant_p;
  ScalarQuantColumn<double> totalBandwidthQuant_p;
};

//# Define the RO version for backward compatibility.
typedef MSSpWindowColumns ROMSSpWindowColumns;

} //# NAMESPACE CASACORE - END

#endif
