//# MSSysCalColumns.h: provides easy access to MSSysCal columns
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

#ifndef MS_MSSYSCALCOLUMNS_H
#define MS_MSSYSCALCOLUMNS_H

#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/measures/TableMeasures/ArrayQuantColumn.h>
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/measures/TableMeasures/ScalarQuantColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MSSysCal;

// <summary>
// A class to provide easy access to MSSysCal columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSSysCal
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// MSSysCalColumns stands for MeasurementSet SysCal Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the MSSysCal Table,
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

class MSSysCalColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  MSSysCalColumns(const MSSysCal& msSysCal);

  // The destructor does nothing special
  ~MSSysCalColumns();

  // Is this object defined? (MSSysCal table is optional)
  bool isNull() const {return isNull_p;}

  // Access to required columns
  // <group>
  ScalarColumn<int32_t>& antennaId() {return antennaId_p;}
  ScalarColumn<int32_t>& feedId() {return feedId_p;}
  ScalarColumn<double>& interval() {return interval_p;}
  ScalarQuantColumn<double>& intervalQuant() {return intervalQuant_p;}
  ScalarColumn<int32_t>& spectralWindowId() {return spectralWindowId_p;}
  ScalarColumn<double>& time() {return time_p;}
  ScalarQuantColumn<double>& timeQuant() {return timeQuant_p;}
  ScalarMeasColumn<MEpoch>& timeMeas() {return timeMeas_p;}
  // </group>

  // Access to optional columns
  // <group>
  ScalarColumn<float>& phaseDiff() {return phaseDiff_p;}
  ScalarQuantColumn<float>& phaseDiffQuant() {return phaseDiffQuant_p;}
  ScalarColumn<bool>& phaseDiffFlag() {return phaseDiffFlag_p;}
  ArrayColumn<float>& tant() {return tant_p;}
  ArrayQuantColumn<float>& tantQuant() {return tantQuant_p;}
  ScalarColumn<bool>& tantFlag() {return tantFlag_p;}
  ArrayColumn<float>& tantSpectrum() {return tantSpectrum_p;}
  ArrayQuantColumn<float>& tantSpectrumQuant() {return tantSpectrumQuant_p;}
  ArrayColumn<float>& tantTsys() {return tantTsys_p;}
  ScalarColumn<bool>& tantTsysFlag() {return tantTsysFlag_p;}
  ArrayColumn<float>& tantTsysSpectrum() {return tantTsysSpectrum_p;}
  ArrayColumn<float>& tcal() {return tcal_p;}
  ArrayQuantColumn<float>& tcalQuant() {return tcalQuant_p;}
  ScalarColumn<bool>& tcalFlag() {return tcalFlag_p;}
  ArrayColumn<float>& tcalSpectrum() {return tcalSpectrum_p;}
  ArrayQuantColumn<float>& tcalSpectrumQuant() {return tcalSpectrumQuant_p;}
  ArrayColumn<float>& trx() {return trx_p;}
  ArrayQuantColumn<float>& trxQuant() {return trxQuant_p;}
  ScalarColumn<bool>& trxFlag() {return trxFlag_p;}
  ArrayColumn<float>& trxSpectrum() {return trxSpectrum_p;}
  ArrayQuantColumn<float>& trxSpectrumQuant() {return trxSpectrumQuant_p;}
  ArrayColumn<float>& tsky() {return tsky_p;}
  ArrayQuantColumn<float>& tskyQuant() {return tskyQuant_p;}
  ScalarColumn<bool>& tskyFlag() {return tskyFlag_p;}
  ArrayColumn<float>& tskySpectrum() {return tskySpectrum_p;}
  ArrayQuantColumn<float>& tskySpectrumQuant() {return tskySpectrumQuant_p;}
  ArrayColumn<float>& tsys() {return tsys_p;}
  ArrayQuantColumn<float>& tsysQuant() {return tsysQuant_p;}
  ScalarColumn<bool>& tsysFlag() {return tsysFlag_p;}
  ArrayColumn<float>& tsysSpectrum() {return tsysSpectrum_p;}
  ArrayQuantColumn<float>& tsysSpectrumQuant() {return tsysSpectrumQuant_p;}
  // </group>

  // Const access to columns
  // <group>
  const ScalarColumn<int32_t>& antennaId() const {return antennaId_p;}
  const ScalarColumn<int32_t>& feedId() const {return feedId_p;}
  const ScalarColumn<double>& interval() const {return interval_p;}
  const ScalarQuantColumn<double>& intervalQuant() const {
    return intervalQuant_p;}
  const ScalarColumn<int32_t>& spectralWindowId() const {
    return spectralWindowId_p;}
  const ScalarColumn<double>& time() const {return time_p;}
  const ScalarQuantColumn<double>& timeQuant() const {return timeQuant_p;}
  const ScalarMeasColumn<MEpoch>& timeMeas() const {return timeMeas_p;}
  // </group>

  // Const access to optional columns
  // <group>
  const ScalarColumn<float>& phaseDiff() const {return phaseDiff_p;}
  const ScalarQuantColumn<float>& phaseDiffQuant() const {
    return phaseDiffQuant_p;}
  const ScalarColumn<bool>& phaseDiffFlag() const {return phaseDiffFlag_p;}
  const ArrayColumn<float>& tant() const {return tant_p;}
  const ArrayQuantColumn<float>& tantQuant() const {return tantQuant_p;}
  const ScalarColumn<bool>& tantFlag() const {return tantFlag_p;}
  const ArrayColumn<float>& tantSpectrum() const {return tantSpectrum_p;}
  const ArrayQuantColumn<float>& tantSpectrumQuant() const {
    return tantSpectrumQuant_p;}
  const ArrayColumn<float>& tantTsys() const {return tantTsys_p;}
  const ScalarColumn<bool>& tantTsysFlag() const {return tantTsysFlag_p;}
  const ArrayColumn<float>& tantTsysSpectrum() const {
    return tantTsysSpectrum_p;}
  const ArrayColumn<float>& tcal() const {return tcal_p;}
  const ArrayQuantColumn<float>& tcalQuant() const {return tcalQuant_p;}
  const ScalarColumn<bool>& tcalFlag() const {return tcalFlag_p;}
  const ArrayColumn<float>& tcalSpectrum() const {return tcalSpectrum_p;}
  const ArrayQuantColumn<float>& tcalSpectrumQuant() const {
    return tcalSpectrumQuant_p;}
  const ArrayColumn<float>& trx() const {return trx_p;}
  const ArrayQuantColumn<float>& trxQuant() const {return trxQuant_p;}
  const ScalarColumn<bool>& trxFlag() const {return trxFlag_p;}
  const ArrayColumn<float>& trxSpectrum() const {return trxSpectrum_p;}
  const ArrayQuantColumn<float>& trxSpectrumQuant() const {
    return trxSpectrumQuant_p;}
  const ArrayColumn<float>& tsky() const {return tsky_p;}
  const ArrayQuantColumn<float>& tskyQuant() const {return tskyQuant_p;}
  const ScalarColumn<bool>& tskyFlag() const {return tskyFlag_p;}
  const ArrayColumn<float>& tskySpectrum() const {return tskySpectrum_p;}
  const ArrayQuantColumn<float>& tskySpectrumQuant() const {
    return tskySpectrumQuant_p;}
  const ArrayColumn<float>& tsys() const {return tsys_p;}
  const ArrayQuantColumn<float>& tsysQuant() const {return tsysQuant_p;}
  const ScalarColumn<bool>& tsysFlag() const {return tsysFlag_p;}
  const ArrayColumn<float>& tsysSpectrum() const {return tsysSpectrum_p;}
  const ArrayQuantColumn<float>& tsysSpectrumQuant() const {
    return tsysSpectrumQuant_p;}
  // </group>

  // Convenience function that returns the number of rows in any of the
  // columns. Returns zero if the object is null.
  rownr_t nrow() const {return isNull() ? 0 : antennaId_p.nrow();}

  // set the epoch type for the TIME column.
  // <note role=tip>
  // In principle this function can only be used if the table is empty,
  // otherwise already written values may thereafter have an incorrect
  // reference, offset, or unit.  However, it is possible that part of the
  // table gets written before these values are known.  In that case the
  // reference, offset, or units can be set by using a false
  // <src>tableMustBeEmpty</src> argument.
  // </note>
  void setEpochRef(MEpoch::Types ref, bool tableMustBeEmpty=true);

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  MSSysCalColumns();

  //# attach this object to the supplied table.
  void attach(const MSSysCal& msSysCal);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  MSSysCalColumns(const MSSysCalColumns&);
  MSSysCalColumns& operator=(const MSSysCalColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const MSSysCal& msSysCal);
  
  //# Is the object not attached to a Table.
  bool isNull_p;

  //# required columns
  ScalarColumn<int32_t> antennaId_p;
  ScalarColumn<int32_t> feedId_p;
  ScalarColumn<double> interval_p;
  ScalarColumn<int32_t> spectralWindowId_p;
  ScalarColumn<double> time_p;
  //# optional columns
  ScalarColumn<float> phaseDiff_p;
  ScalarColumn<bool> phaseDiffFlag_p;
  ArrayColumn<float> tant_p;
  ScalarColumn<bool> tantFlag_p;
  ArrayColumn<float> tantSpectrum_p;
  ArrayColumn<float> tantTsys_p;
  ScalarColumn<bool> tantTsysFlag_p;
  ArrayColumn<float> tantTsysSpectrum_p;
  ArrayColumn<float> tcal_p;
  ScalarColumn<bool> tcalFlag_p;
  ArrayColumn<float> tcalSpectrum_p;
  ArrayColumn<float> trx_p;
  ScalarColumn<bool> trxFlag_p;
  ArrayColumn<float> trxSpectrum_p;
  ArrayColumn<float> tsky_p;
  ScalarColumn<bool> tskyFlag_p;
  ArrayColumn<float> tskySpectrum_p;
  ArrayColumn<float> tsys_p;
  ScalarColumn<bool> tsysFlag_p;
  ArrayColumn<float> tsysSpectrum_p;

  //# Access to Measure columns
  ScalarMeasColumn<MEpoch> timeMeas_p;

  //# Access to Quantum columns
  ScalarQuantColumn<double> intervalQuant_p;
  ScalarQuantColumn<double> timeQuant_p;
  //# optional Quantum columns
  ScalarQuantColumn<float> phaseDiffQuant_p;
  ArrayQuantColumn<float> tantQuant_p;
  ArrayQuantColumn<float> tantSpectrumQuant_p;
  ArrayQuantColumn<float> tcalQuant_p;
  ArrayQuantColumn<float> tcalSpectrumQuant_p;
  ArrayQuantColumn<float> trxQuant_p;
  ArrayQuantColumn<float> trxSpectrumQuant_p;
  ArrayQuantColumn<float> tskyQuant_p;
  ArrayQuantColumn<float> tskySpectrumQuant_p;
  ArrayQuantColumn<float> tsysQuant_p;
  ArrayQuantColumn<float> tsysSpectrumQuant_p;
};

//# Define the RO version for backward compatibility.
typedef MSSysCalColumns ROMSSysCalColumns;

} //# NAMESPACE CASACORE - END

#endif
