//# NewMSSpWindowColumns.h: provides easy access to NewMSSpectralWindow columns
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

#if !defined(AIPS_NEWMSSPWINDOWCOLUMNS_H)
#define AIPS_NEWMSSPWINDOWCOLUMNS_H

#include <aips/aips.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/TableMeasures/ScalarMeasColumn.h>
#include <aips/TableMeasures/ScalarQuantColumn.h>
#include <aips/TableMeasures/ArrayMeasColumn.h>
#include <aips/TableMeasures/ArrayQuantColumn.h>
#include <aips/Utilities/String.h>

class NewMSSpectralWindow;

// <summary>
// A class to provide easy read-only access to NewMSASpectralWindow columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSSpectralWindow
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// RONewMSSpectralWindowColumns stands for Read-Only NewMeasurementSet SpectralWindow Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the NewMSSpectralWindow Table.
// It does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to
// worry about getting those right. There is an access function
// for every predefined column. Access to non-predefined columns will still
// have to be done with explicit declarations.
// See <linkto class=RONewMSColumns> RONewMSColumns</linkto> for an example.
// </synopsis>
//
// <motivation>
// See <linkto class=NewMSColumns> NewMSColumns</linkto> for the motivation.
// </motivation>

class RONewMSSpWindowColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  RONewMSSpWindowColumns(const NewMSSpectralWindow& msSpWindow);

  // The destructor does nothing special
  ~RONewMSSpWindowColumns();

  // Access to columns
  // <group>
  const ROArrayColumn<Double>& chanFreq() const {return chanFreq_p;}
  const ROArrayQuantColumn<Double>& chanFreqQuant() const {
    return chanFreqQuant_p;}
  const ROArrayMeasColumn<MFrequency>& chanFreqMeas() const {
    return chanFreqMeas_p;}
  const ROArrayColumn<Double>& chanWidth() const {return chanWidth_p;}
  const ROArrayQuantColumn<Double>& chanWidthQuant() const {
    return chanWidthQuant_p;}
  const ROArrayColumn<Double>& effectiveBW() const {return effectiveBW_p;}
  const ROArrayQuantColumn<Double>& effectiveBWQuant() const {
    return effectiveBWQuant_p;}
  const ROScalarColumn<Int>& freqGroup() const {return freqGroup_p;}
  const ROScalarColumn<String>& freqGroupName() const {return freqGroupName_p;}
  const ROScalarColumn<Int>& ifConvChain() const {return ifConvChain_p;}
  const ROScalarColumn<Bool>& flagRow() const {return flagRow_p;}
  const ROScalarColumn<Int>& measFreqRef() const {return measFreqRef_p;}
  const ROScalarColumn<String>& name() const {return name_p;}
  const ROScalarColumn<Int>& netSideband() const {return netSideband_p;}
  const ROScalarColumn<Int>& numChan() const {return numChan_p;}
  const ROScalarColumn<Double>& refFrequency() const {return refFrequency_p;}
  const ROScalarQuantColumn<Double>& refFrequencyQuant() const {
    return refFrequencyQuant_p;}
  const ROScalarMeasColumn<MFrequency>& refFrequencyMeas() const {
    return refFrequencyMeas_p;}
  const ROArrayColumn<Double>& resolution() const {return resolution_p;}
  const ROArrayQuantColumn<Double>& resolutionQuant() const {
    return resolutionQuant_p;}
  const ROScalarColumn<Double>& totalBandwidth() const {
    return totalBandwidth_p;}
  const ROScalarQuantColumn<Double>& totalBandwidthQuant() const {
    return totalBandwidthQuant_p;}
  // </group>

  // Access to optional columns
  // <group>
  const ROArrayColumn<String>& assocNature() const {return assocNature_p;}
  const ROArrayColumn<Int>& assocSpwId() const {return assocSpwId_p;}
  const ROScalarColumn<Int>& bbcNo() const {return bbcNo_p;}
  const ROScalarColumn<Int>& bbcSideband() const {return bbcSideband_p;}
  const ROScalarColumn<Int>& dopplerId() const {return dopplerId_p;}
  const ROScalarColumn<Int>& receiverId() const {return receiverId_p;}
  // </group>

  // Convenience function that returns the number of rows in any of the columns
  uInt nrow() const {return chanFreq_p.nrow();}

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
  Int matchSpw(const MFrequency& refFreq, uInt nChan, 
	       const Quantum<Double>& bandwidth, Int ifChain,
	       const Quantum<Double>& tolerance, Int tryRow=-1) const;

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  RONewMSSpWindowColumns();

  //# attach this object to the supplied table.
  void attach(const NewMSSpectralWindow& msSpWindow);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  RONewMSSpWindowColumns(const RONewMSSpWindowColumns&);
  RONewMSSpWindowColumns& operator=(const RONewMSSpWindowColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const NewMSSpectralWindow& msSpWindow);

  //# functions to match the supplied arguments against the values in the
  //# specified row.
  //<group>
  Bool matchRefFrequency(uInt row, MFrequency::Types refType, 
			 Double refFreqInHz, Double tolInHz) const;
  Bool matchChanFreq(uInt row, const Vector<Double>& chanFreqInHz,
		     Double tolInHz) const;
  Bool matchIfConvChain(uInt row, Int ifChain) const;
  Bool matchTotalBandwidth(uInt row, Double bandwidthInHz,
			   Double tolInHz) const;
  Bool matchNumChan(uInt row, Int nChan) const;
  //</group>


  //# required columns
  ROArrayColumn<Double> chanFreq_p;
  ROArrayColumn<Double> chanWidth_p;
  ROArrayColumn<Double> effectiveBW_p;
  ROScalarColumn<Bool> flagRow_p;
  ROScalarColumn<Int> freqGroup_p;
  ROScalarColumn<String> freqGroupName_p;
  ROScalarColumn<Int> ifConvChain_p;
  ROScalarColumn<Int> measFreqRef_p;
  ROScalarColumn<String> name_p;
  ROScalarColumn<Int> netSideband_p;
  ROScalarColumn<Int> numChan_p;
  ROScalarColumn<Double> refFrequency_p;
  ROArrayColumn<Double> resolution_p;
  ROScalarColumn<Double> totalBandwidth_p;
  //# optional columns
  ROArrayColumn<String> assocNature_p;
  ROArrayColumn<Int> assocSpwId_p;
  ROScalarColumn<Int> bbcNo_p;
  ROScalarColumn<Int> bbcSideband_p;
  ROScalarColumn<Int> dopplerId_p;
  ROScalarColumn<Int> receiverId_p;

  //# Access to Measure columns
  ROArrayMeasColumn<MFrequency> chanFreqMeas_p;
  ROScalarMeasColumn<MFrequency> refFrequencyMeas_p;

  //# Access to Quantum columns
  ROArrayQuantColumn<Double> chanFreqQuant_p;
  ROArrayQuantColumn<Double> chanWidthQuant_p;
  ROArrayQuantColumn<Double> effectiveBWQuant_p;
  ROScalarQuantColumn<Double> refFrequencyQuant_p;
  ROArrayQuantColumn<Double> resolutionQuant_p;
  ROScalarQuantColumn<Double> totalBandwidthQuant_p;
};

// <summary>
// A class to provide easy read-write access to NewMSSpectralWindow columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSSpectralWindow
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// NewMSSpectralWindowColumns stands for NewMeasurementSet SpectralWindow Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the NewMSSpectralWindow Table,
// it does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to
// worry about getting those right. There is an access function
// for every predefined column. Access to non-predefined columns will still
// have to be done with explicit declarations.
// See <linkto class=NewMSColumns> NewMSColumns</linkto> for an example.
// </synopsis>
//
// <motivation>
// See <linkto class=NewMSColumns> NewMSColumns</linkto> for the motivation.
// </motivation>

class NewMSSpWindowColumns: public RONewMSSpWindowColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  NewMSSpWindowColumns(NewMSSpectralWindow& msSpWindow);

  // The destructor does nothing special
  ~NewMSSpWindowColumns();

  // Read-write access to required columns
  // <group>
  ArrayColumn<Double>& chanFreq() {return chanFreq_p;}
  ArrayMeasColumn<MFrequency>& chanFreqMeas() {return chanFreqMeas_p;}
  ArrayQuantColumn<Double>& chanFreqQuant() {return chanFreqQuant_p;}
  ArrayColumn<Double>& chanWidth() {return chanWidth_p;}
  ArrayQuantColumn<Double>& chanWidthQuant() { return chanWidthQuant_p;}
  ArrayColumn<Double>& effectiveBW() {return effectiveBW_p;}
  ArrayQuantColumn<Double>& effectiveBWQuant() { return effectiveBWQuant_p;}
  ScalarColumn<Bool>& flagRow() {return flagRow_p;}
  ScalarColumn<Int>& freqGroup() {return freqGroup_p;}
  ScalarColumn<String>& freqGroupName() {return freqGroupName_p;}
  ScalarColumn<Int>& ifConvChain() {return ifConvChain_p;}
  ScalarColumn<Int>& measFreqRef() {return measFreqRef_p;}
  ScalarColumn<String>& name() {return name_p;}
  ScalarColumn<Int>& netSideband() {return netSideband_p;}
  ScalarColumn<Int>& numChan() {return numChan_p;}
  ScalarColumn<Double>& refFrequency() {return refFrequency_p;}
  ScalarQuantColumn<Double>& refFrequencyQuant() {return refFrequencyQuant_p;}
  ScalarMeasColumn<MFrequency>& refFrequencyMeas() {return refFrequencyMeas_p;}
  ArrayColumn<Double>& resolution() {return resolution_p;}
  ArrayQuantColumn<Double>& resolutionQuant() { return resolutionQuant_p;}
  ScalarColumn<Double>& totalBandwidth() {return totalBandwidth_p;}
  ScalarQuantColumn<Double>& totalBandwidthQuant() {
    return totalBandwidthQuant_p;}
  // </group>

  // Read-write access to optional columns
  // <group>
  ArrayColumn<String>& assocNature() {return assocNature_p;}
  ArrayColumn<Int>& assocSpwId() {return assocSpwId_p;}
  ScalarColumn<Int>& bbcNo() {return bbcNo_p;}
  ScalarColumn<Int>& bbcSideband() {return bbcSideband_p;}
  ScalarColumn<Int>& dopplerId() {return dopplerId_p;}
  ScalarColumn<Int>& receiverId() {return receiverId_p;}
  // </group>

  // Read-only access to required columns
  // <group>
  const ROArrayColumn<Double>& chanFreq() const {
    return RONewMSSpWindowColumns::chanFreq();}
  const ROArrayQuantColumn<Double>& chanFreqQuant() const {
    return RONewMSSpWindowColumns::chanFreqQuant();}
  const ROArrayMeasColumn<MFrequency>& chanFreqMeas() const {
    return RONewMSSpWindowColumns::chanFreqMeas();}
  const ROArrayColumn<Double>& chanWidth() const {
    return RONewMSSpWindowColumns::chanWidth();}
  const ROArrayQuantColumn<Double>& chanWidthQuant() const {
    return RONewMSSpWindowColumns::chanWidthQuant();}
  const ROArrayColumn<Double>& effectiveBW() const {
    return RONewMSSpWindowColumns::effectiveBW();}
  const ROArrayQuantColumn<Double>& effectiveBWQuant() const {
    return RONewMSSpWindowColumns::effectiveBWQuant();}
  const ROScalarColumn<Int>& freqGroup() const {
    return RONewMSSpWindowColumns::freqGroup();}
  const ROScalarColumn<String>& freqGroupName() const {
    return RONewMSSpWindowColumns::freqGroupName();}
  const ROScalarColumn<Int>& ifConvChain() const {
    return RONewMSSpWindowColumns::ifConvChain();}
  const ROScalarColumn<Bool>& flagRow() const {
    return RONewMSSpWindowColumns::flagRow();}
  const ROScalarColumn<Int>& measFreqRef() const {
    return RONewMSSpWindowColumns::measFreqRef();}
  const ROScalarColumn<String>& name() const {
    return RONewMSSpWindowColumns::name();}
  const ROScalarColumn<Int>& netSideband() const {
    return RONewMSSpWindowColumns::netSideband();}
  const ROScalarColumn<Int>& numChan() const {
    return RONewMSSpWindowColumns::numChan();}
  const ROScalarColumn<Double>& refFrequency() const {
    return RONewMSSpWindowColumns::refFrequency();}
  const ROScalarQuantColumn<Double>& refFrequencyQuant() const {
    return RONewMSSpWindowColumns::refFrequencyQuant();}
  const ROScalarMeasColumn<MFrequency>& refFrequencyMeas() const {
    return RONewMSSpWindowColumns::refFrequencyMeas();}
  const ROArrayColumn<Double>& resolution() const {
    return RONewMSSpWindowColumns::resolution();}
  const ROArrayQuantColumn<Double>& resolutionQuant() const {
    return RONewMSSpWindowColumns::resolutionQuant();}
  const ROScalarColumn<Double>& totalBandwidth() const {
    return RONewMSSpWindowColumns::totalBandwidth();}
  const ROScalarQuantColumn<Double>& totalBandwidthQuant() const {
    return RONewMSSpWindowColumns::totalBandwidthQuant();}
  // </group>

  // Read-only access to optional columns
  // <group>
  const ROArrayColumn<String>& assocNature() const {
    return RONewMSSpWindowColumns::assocNature();}
  const ROArrayColumn<Int>& assocSpwId() const {
    return RONewMSSpWindowColumns::assocSpwId();}
  const ROScalarColumn<Int>& bbcNo() const {
    return RONewMSSpWindowColumns::bbcNo();}
  const ROScalarColumn<Int>& bbcSideband() const {
    return RONewMSSpWindowColumns::bbcSideband();}
  const ROScalarColumn<Int>& dopplerId() const {
    return RONewMSSpWindowColumns::dopplerId();}
  const ROScalarColumn<Int>& receiverId() const {
    return RONewMSSpWindowColumns::receiverId();}
  // </group>

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  NewMSSpWindowColumns();

  //# attach this object to the supplied table.
  void attach(NewMSSpectralWindow& msSpWindow);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  NewMSSpWindowColumns(const NewMSSpWindowColumns&);
  NewMSSpWindowColumns& operator=(const NewMSSpWindowColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(NewMSSpectralWindow& msSpWindow);
  
  //# required columns
  ArrayColumn<Double> chanFreq_p;
  ArrayColumn<Double> chanWidth_p;
  ArrayColumn<Double> effectiveBW_p;
  ScalarColumn<Bool> flagRow_p;
  ScalarColumn<Int> freqGroup_p;
  ScalarColumn<String> freqGroupName_p;
  ScalarColumn<Int> ifConvChain_p;
  ScalarColumn<Int> measFreqRef_p;
  ScalarColumn<String> name_p;
  ScalarColumn<Int> netSideband_p;
  ScalarColumn<Int> numChan_p;
  ScalarColumn<Double> refFrequency_p;
  ArrayColumn<Double> resolution_p;
  ScalarColumn<Double> totalBandwidth_p;
  //# optional columns
  ArrayColumn<String> assocNature_p;
  ArrayColumn<Int> assocSpwId_p;
  ScalarColumn<Int> bbcNo_p;
  ScalarColumn<Int> bbcSideband_p;
  ScalarColumn<Int> dopplerId_p;
  ScalarColumn<Int> receiverId_p;

  //# Access to Measure columns
  ArrayMeasColumn<MFrequency> chanFreqMeas_p;
  ScalarMeasColumn<MFrequency> refFrequencyMeas_p;

  //# Access to Quantum columns
  ArrayQuantColumn<Double> chanFreqQuant_p;
  ArrayQuantColumn<Double> chanWidthQuant_p;
  ArrayQuantColumn<Double> effectiveBWQuant_p;
  ScalarQuantColumn<Double> refFrequencyQuant_p;
  ArrayQuantColumn<Double> resolutionQuant_p;
  ScalarQuantColumn<Double> totalBandwidthQuant_p;
};
#endif
