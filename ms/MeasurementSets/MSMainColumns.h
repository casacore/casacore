//# MSmainColumns.h: provides easy access to MeasurementSet main table columns
//# Copyright (C) 2000
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

#ifndef MS_MSMAINCOLUMNS_H
#define MS_MSMAINCOLUMNS_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/Muvw.h>
#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/measures/Measures/MCuvw.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/measures/TableMeasures/ScalarQuantColumn.h>
#include <casacore/measures/TableMeasures/ArrayQuantColumn.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MeasurementSet;
class String;

// <summary>
// A class for easy access to MeasurementSet main table columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MeasurementSet
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// MSMainColumns stands for MeasurementSet main Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the MeasurementSet.
// It does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to
// worry about getting those right. There is an access function
// for every predefined column. Access to non-predefined columns will still
// have to be done with explicit declarations.
// <note role=warning> The Table that is used to construct this class must not
// be destroyed (or go out of scope) before this class does. Otherwise the
// scalar and array columns use by this class will be left dangling.</note>
// </synopsis>
// </synopsis>
//
// <example>
// <srcblock>
// // use as follows
// MeasurementSet ms("myMS",Table::Update); 
// MSColumns msc(ms);
// // show data from row 5
// cout << msc.data()(5);
// // change name of antenna on row 3 in antenna table
// msc.antenna().name().put(3,"NewAnt-3");
// </srcblock>
// </example>
//
// <motivation>
// Having to type long lists of Scalar and Array column declarations gets
// very tedious. This class attempts to relieve some of that tedium, while
// at the same time concentrating all the declarations in one place,
// making Type errors in the column declaration (only caught at run-time) less
// probable. Type errors in the use of the columns is caught at compile
// time.
// </motivation>
//
// <todo asof="1997/02/01">
//   <li> We might decide to merge this class with the MeasurementSet
// </todo>

class MSMainColumns
{
public:

  // Create a columns object that accesses the data in the specified Table
  MSMainColumns(const MeasurementSet& ms);

  // The desctructor does nothing special
  ~MSMainColumns();

  // Access to required columns
  // <group>
  ScalarColumn<Int>& antenna1() {return antenna1_p;}
  ScalarColumn<Int>& antenna2() {return antenna2_p;}
  ScalarColumn<Int>& arrayId() {return arrayId_p;}
  ScalarColumn<Int>& dataDescId() {return dataDescId_p;}
  ScalarColumn<Double>& exposure() {return exposure_p;}
  ScalarQuantColumn<Double>& exposureQuant() { 
    return exposureQuant_p;}
  ScalarColumn<Int>& feed1() {return feed1_p;}
  ScalarColumn<Int>& feed2() {return feed2_p;}
  ScalarColumn<Int>& fieldId() {return fieldId_p;}
  ArrayColumn<Bool>& flag() {return flag_p;}
  ArrayColumn<Bool>& flagCategory() {return flagCategory_p;}
  ScalarColumn<Bool>& flagRow() {return flagRow_p;}
  ScalarColumn<Double>& interval() {return interval_p;}
  ScalarQuantColumn<Double>& intervalQuant() {
    return intervalQuant_p;}
  ScalarColumn<Int>& observationId() {return observationId_p;}
  ScalarColumn<Int>& processorId() {return processorId_p;}
  ScalarColumn<Int>& scanNumber() {return scanNumber_p;}
  ArrayColumn<Float>& sigma() {return sigma_p;}
  ScalarColumn<Int>& stateId() {return stateId_p;}
  ScalarColumn<Double>& time() {return time_p;}
  ScalarQuantColumn<Double>& timeQuant() {
    return timeQuant_p;}
  ScalarMeasColumn<MEpoch>& timeMeas() {
    return timeMeas_p;}
  ScalarColumn<Double>& timeCentroid() {return timeCentroid_p;}
  ScalarQuantColumn<Double>& timeCentroidQuant() {
    return timeCentroidQuant_p;}
  ScalarMeasColumn<MEpoch>& timeCentroidMeas() {
    return timeCentroidMeas_p;}
  ArrayColumn<Double>& uvw() {return uvw_p;}
  ArrayQuantColumn<Double>& uvwQuant() {
    return uvwQuant_p;}
  ScalarMeasColumn<Muvw>& uvwMeas() {
    return uvwMeas_p;}
  ArrayColumn<Float>& weight() {return weight_p;}
  // </group>

  // Access to optional columns
  // <group>
  ScalarColumn<Int>& antenna3() {return antenna3_p;}
  ScalarColumn<Bool>& baselineRef() {return baselineRef_p;}
  ArrayColumn<Complex>& correctedData() {return correctedData_p;}
  ArrayColumn<Complex>& data() {return data_p;}
  ScalarColumn<Int>& feed3() {return feed3_p;}
  ArrayColumn<Float>& floatData() {return floatData_p;}
  ArrayColumn<Float>& imagingWeight() {return imagingWeight_p;}
  ArrayColumn<Complex>& lagData() {return lagData_p;}
  ArrayColumn<Complex>& modelData() {return modelData_p;}
  ScalarColumn<Int>& phaseId() {return phaseId_p;}
  ScalarColumn<Int>& pulsarBin() {return pulsarBin_p;}
  ScalarColumn<Int>& pulsarGateId() {return pulsarGateId_p;}
  ArrayColumn<Float>& sigmaSpectrum() {return sigmaSpectrum_p;}
  ScalarColumn<Double>& timeExtraPrec() {return timeExtraPrec_p;}
  ScalarQuantColumn<Double>& timeExtraPrecQuant() {
    return timeExtraPrecQuant_p;}
  ArrayColumn<Double>& uvw2() {return uvw2_p;}
  ScalarMeasColumn<Muvw>& uvw2Meas() {
    return uvw2Meas_p;}
  ArrayQuantColumn<Double>& uvw2Quant() {
    return uvw2Quant_p;}
  ArrayColumn<Complex>& videoPoint() {return videoPoint_p;}
  ArrayColumn<Float>& weightSpectrum() {return weightSpectrum_p;}
  ArrayColumn<Float>& weightSpectrumCorrected() {return weightSpectrumCorrected_p;}
  // </group>

  // Const access to required columns
  // <group>
  const ScalarColumn<Int>& antenna1() const {return antenna1_p;}
  const ScalarColumn<Int>& antenna2() const {return antenna2_p;}
  const ScalarColumn<Int>& arrayId() const {return arrayId_p;}
  const ScalarColumn<Int>& dataDescId() const {return dataDescId_p;}
  const ScalarColumn<Double>& exposure() const {return exposure_p;}
  const ScalarQuantColumn<Double>& exposureQuant() const { 
    return exposureQuant_p;}
  const ScalarColumn<Int>& feed1() const {return feed1_p;}
  const ScalarColumn<Int>& feed2() const {return feed2_p;}
  const ScalarColumn<Int>& fieldId() const {return fieldId_p;}
  const ArrayColumn<Bool>& flag() const {return flag_p;}
  const ArrayColumn<Bool>& flagCategory() const {return flagCategory_p;}
  const ScalarColumn<Bool>& flagRow() const {return flagRow_p;}
  const ScalarColumn<Double>& interval() const {return interval_p;}
  const ScalarQuantColumn<Double>& intervalQuant() const {
    return intervalQuant_p;}
  const ScalarColumn<Int>& observationId() const {return observationId_p;}
  const ScalarColumn<Int>& processorId() const {return processorId_p;}
  const ScalarColumn<Int>& scanNumber() const {return scanNumber_p;}
  const ArrayColumn<Float>& sigma() const {return sigma_p;}
  const ScalarColumn<Int>& stateId() const {return stateId_p;}
  const ScalarColumn<Double>& time() const {return time_p;}
  const ScalarQuantColumn<Double>& timeQuant() const {
    return timeQuant_p;}
  const ScalarMeasColumn<MEpoch>& timeMeas() const {
    return timeMeas_p;}
  const ScalarColumn<Double>& timeCentroid() const {return timeCentroid_p;}
  const ScalarQuantColumn<Double>& timeCentroidQuant() const {
    return timeCentroidQuant_p;}
  const ScalarMeasColumn<MEpoch>& timeCentroidMeas() const {
    return timeCentroidMeas_p;}
  const ArrayColumn<Double>& uvw() const {return uvw_p;}
  const ArrayQuantColumn<Double>& uvwQuant() const {
    return uvwQuant_p;}
  const ScalarMeasColumn<Muvw>& uvwMeas() const {
    return uvwMeas_p;}
  const ArrayColumn<Float>& weight() const {return weight_p;}
  // </group>

  // Access to optional columns
  // <group>
  const ScalarColumn<Int>& antenna3() const {return antenna3_p;}
  const ScalarColumn<Bool>& baselineRef() const {return baselineRef_p;}
  const ArrayColumn<Complex>& correctedData() const {return correctedData_p;}
  const ArrayColumn<Complex>& data() const {return data_p;}
  const ScalarColumn<Int>& feed3() const {return feed3_p;}
  const ArrayColumn<Float>& floatData() const {return floatData_p;}
  const ArrayColumn<Float>& imagingWeight() const {return imagingWeight_p;}
  const ArrayColumn<Complex>& lagData() const {return lagData_p;}
  const ArrayColumn<Complex>& modelData() const {return modelData_p;}
  const ScalarColumn<Int>& phaseId() const {return phaseId_p;}
  const ScalarColumn<Int>& pulsarBin() const {return pulsarBin_p;}
  const ScalarColumn<Int>& pulsarGateId() const {return pulsarGateId_p;}
  const ArrayColumn<Float>& sigmaSpectrum() const {return sigmaSpectrum_p;}
  const ScalarColumn<Double>& timeExtraPrec() const {return timeExtraPrec_p;}
  const ScalarQuantColumn<Double>& timeExtraPrecQuant() const {
    return timeExtraPrecQuant_p;}
  const ArrayColumn<Double>& uvw2() const {return uvw2_p;}
  const ScalarMeasColumn<Muvw>& uvw2Meas() const {
    return uvw2Meas_p;}
  const ArrayQuantColumn<Double>& uvw2Quant() const {
    return uvw2Quant_p;}
  const ArrayColumn<Complex>& videoPoint() const {return videoPoint_p;}
  const ArrayColumn<Float>& weightSpectrum() const {return weightSpectrum_p;}
  const ArrayColumn<Float>& weightSpectrumCorrected() const {return weightSpectrumCorrected_p;}
  // </group>

  // Convenience function that returns the number of rows in any of the columns
  uInt nrow() const {return antenna1_p.nrow();}
  
  // Returns the category labels for the FLAG_CATEGORY column.
  Vector<String> flagCategories() const;

  // set the epoch type for the TIME and TIME_CENTROID columns. 
  // <note role=tip>
  // In principle this function can only be used if the table is empty,
  // otherwise already written values may thereafter have an incorrect
  // reference, offset, or unit.  However, it is possible that part of the
  // table gets written before these values are known.  In that case the
  // reference, offset, or units can be set by using a False
  // <src>tableMustBeEmpty</src> argument.
  // </note>
  void setEpochRef(MEpoch::Types ref, Bool tableMustBeEmpty=True);

  // set the UVW reference type for the UVW and UVW2 (if defined) columns. This
  // can only be done when the table has no rows. Trying to do so at other
  // times will throw an exception.
  void setUVWRef(Muvw::Types ref);

  // Set the flag category labels to the supplied values (in the CATEGORY
  // keyword of the FLAG_CATEGORY column). Throws an exception, when compiled
  // in Debug mode, if the length of the supplied Vector is not the same as the
  // length of the third dimension of the FLAG_CATEGORY column.
  void setFlagCategories(const Vector<String>& categories);

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  MSMainColumns();

  //# attach this object to the supplied table.
  void attach(const MeasurementSet& ms);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  MSMainColumns(const MSMainColumns&);
  MSMainColumns& operator=(const MSMainColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const MeasurementSet& ms);
  
  //# required columns
  ScalarColumn<Int> antenna1_p;
  ScalarColumn<Int> antenna2_p;
  ScalarColumn<Int> arrayId_p;
  ScalarColumn<Int> dataDescId_p;
  ScalarColumn<Double> exposure_p;
  ScalarColumn<Int> feed1_p;
  ScalarColumn<Int> feed2_p;
  ScalarColumn<Int> fieldId_p;
  ArrayColumn<Bool> flag_p;
  ArrayColumn<Bool> flagCategory_p;
  ScalarColumn<Bool> flagRow_p;
  ScalarColumn<Double> interval_p;
  ScalarColumn<Int> observationId_p;
  ScalarColumn<Int> processorId_p;
  ScalarColumn<Int> scanNumber_p;
  ArrayColumn<Float> sigma_p;
  ScalarColumn<Int> stateId_p;
  ScalarColumn<Double> time_p;
  ScalarColumn<Double> timeCentroid_p;
  ArrayColumn<Double> uvw_p;
  ArrayColumn<Float> weight_p;
  //# optional columns
  ScalarColumn<Int> antenna3_p;
  ScalarColumn<Bool> baselineRef_p;
  ArrayColumn<Complex> data_p;
  ScalarColumn<Int> feed3_p;
  ArrayColumn<Float> floatData_p;
  ArrayColumn<Complex> lagData_p;
  ScalarColumn<Int> phaseId_p;
  ScalarColumn<Int> pulsarBin_p;
  ScalarColumn<Int> pulsarGateId_p;
  ArrayColumn<Float> sigmaSpectrum_p;
  ScalarColumn<Double> timeExtraPrec_p;
  ArrayColumn<Double> uvw2_p;
  ArrayColumn<Complex> videoPoint_p;
  ArrayColumn<Float> weightSpectrum_p;
  ArrayColumn<Float> weightSpectrumCorrected_p;

  //# columns required for synthesis applications - all optional
  ArrayColumn<Complex> correctedData_p;
  ArrayColumn<Float> imagingWeight_p;
  ArrayColumn<Complex> modelData_p;

  //# Access to Measure columns
  ScalarMeasColumn<MEpoch> timeMeas_p;
  ScalarMeasColumn<MEpoch> timeCentroidMeas_p;
  ScalarMeasColumn<Muvw> uvwMeas_p;
  //# optional Measure columns
  ScalarMeasColumn<Muvw> uvw2Meas_p;

  //# Access to Quantum columns
  ScalarQuantColumn<Double> exposureQuant_p;
  ScalarQuantColumn<Double> intervalQuant_p;
  ScalarQuantColumn<Double> timeQuant_p;
  ScalarQuantColumn<Double> timeCentroidQuant_p;
  ArrayQuantColumn<Double> uvwQuant_p;
  //# optional Quantum columns
  ScalarQuantColumn<Double> timeExtraPrecQuant_p;
  ArrayQuantColumn<Double> uvw2Quant_p;

};

//# Define the RO version for backward compatibility.
typedef MSMainColumns ROMSMainColumns;

} //# NAMESPACE CASACORE - END

#endif


