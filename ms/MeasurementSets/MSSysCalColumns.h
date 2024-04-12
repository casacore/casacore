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
//#        Internet email: casa-feedback@nrao.edu.
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
  Bool isNull() const {return isNull_p;}

  // Access to required columns
  // <group>
  ScalarColumn<Int>& antennaId() {return antennaId_p;}
  ScalarColumn<Int>& feedId() {return feedId_p;}
  ScalarColumn<Double>& interval() {return interval_p;}
  ScalarQuantColumn<Double>& intervalQuant() {return intervalQuant_p;}
  ScalarColumn<Int>& spectralWindowId() {return spectralWindowId_p;}
  ScalarColumn<Double>& time() {return time_p;}
  ScalarQuantColumn<Double>& timeQuant() {return timeQuant_p;}
  ScalarMeasColumn<MEpoch>& timeMeas() {return timeMeas_p;}
  // </group>

  // Access to optional columns
  // <group>
  ScalarColumn<Float>& phaseDiff() {return phaseDiff_p;}
  ScalarQuantColumn<Float>& phaseDiffQuant() {return phaseDiffQuant_p;}
  ScalarColumn<Bool>& phaseDiffFlag() {return phaseDiffFlag_p;}
  ArrayColumn<Float>& tant() {return tant_p;}
  ArrayQuantColumn<Float>& tantQuant() {return tantQuant_p;}
  ScalarColumn<Bool>& tantFlag() {return tantFlag_p;}
  ArrayColumn<Float>& tantSpectrum() {return tantSpectrum_p;}
  ArrayQuantColumn<Float>& tantSpectrumQuant() {return tantSpectrumQuant_p;}
  ArrayColumn<Float>& tantTsys() {return tantTsys_p;}
  ScalarColumn<Bool>& tantTsysFlag() {return tantTsysFlag_p;}
  ArrayColumn<Float>& tantTsysSpectrum() {return tantTsysSpectrum_p;}
  ArrayColumn<Float>& tcal() {return tcal_p;}
  ArrayQuantColumn<Float>& tcalQuant() {return tcalQuant_p;}
  ScalarColumn<Bool>& tcalFlag() {return tcalFlag_p;}
  ArrayColumn<Float>& tcalSpectrum() {return tcalSpectrum_p;}
  ArrayQuantColumn<Float>& tcalSpectrumQuant() {return tcalSpectrumQuant_p;}
  ArrayColumn<Float>& trx() {return trx_p;}
  ArrayQuantColumn<Float>& trxQuant() {return trxQuant_p;}
  ScalarColumn<Bool>& trxFlag() {return trxFlag_p;}
  ArrayColumn<Float>& trxSpectrum() {return trxSpectrum_p;}
  ArrayQuantColumn<Float>& trxSpectrumQuant() {return trxSpectrumQuant_p;}
  ArrayColumn<Float>& tsky() {return tsky_p;}
  ArrayQuantColumn<Float>& tskyQuant() {return tskyQuant_p;}
  ScalarColumn<Bool>& tskyFlag() {return tskyFlag_p;}
  ArrayColumn<Float>& tskySpectrum() {return tskySpectrum_p;}
  ArrayQuantColumn<Float>& tskySpectrumQuant() {return tskySpectrumQuant_p;}
  ArrayColumn<Float>& tsys() {return tsys_p;}
  ArrayQuantColumn<Float>& tsysQuant() {return tsysQuant_p;}
  ScalarColumn<Bool>& tsysFlag() {return tsysFlag_p;}
  ArrayColumn<Float>& tsysSpectrum() {return tsysSpectrum_p;}
  ArrayQuantColumn<Float>& tsysSpectrumQuant() {return tsysSpectrumQuant_p;}
  // </group>

  // Const access to columns
  // <group>
  const ScalarColumn<Int>& antennaId() const {return antennaId_p;}
  const ScalarColumn<Int>& feedId() const {return feedId_p;}
  const ScalarColumn<Double>& interval() const {return interval_p;}
  const ScalarQuantColumn<Double>& intervalQuant() const {
    return intervalQuant_p;}
  const ScalarColumn<Int>& spectralWindowId() const {
    return spectralWindowId_p;}
  const ScalarColumn<Double>& time() const {return time_p;}
  const ScalarQuantColumn<Double>& timeQuant() const {return timeQuant_p;}
  const ScalarMeasColumn<MEpoch>& timeMeas() const {return timeMeas_p;}
  // </group>

  // Const access to optional columns
  // <group>
  const ScalarColumn<Float>& phaseDiff() const {return phaseDiff_p;}
  const ScalarQuantColumn<Float>& phaseDiffQuant() const {
    return phaseDiffQuant_p;}
  const ScalarColumn<Bool>& phaseDiffFlag() const {return phaseDiffFlag_p;}
  const ArrayColumn<Float>& tant() const {return tant_p;}
  const ArrayQuantColumn<Float>& tantQuant() const {return tantQuant_p;}
  const ScalarColumn<Bool>& tantFlag() const {return tantFlag_p;}
  const ArrayColumn<Float>& tantSpectrum() const {return tantSpectrum_p;}
  const ArrayQuantColumn<Float>& tantSpectrumQuant() const {
    return tantSpectrumQuant_p;}
  const ArrayColumn<Float>& tantTsys() const {return tantTsys_p;}
  const ScalarColumn<Bool>& tantTsysFlag() const {return tantTsysFlag_p;}
  const ArrayColumn<Float>& tantTsysSpectrum() const {
    return tantTsysSpectrum_p;}
  const ArrayColumn<Float>& tcal() const {return tcal_p;}
  const ArrayQuantColumn<Float>& tcalQuant() const {return tcalQuant_p;}
  const ScalarColumn<Bool>& tcalFlag() const {return tcalFlag_p;}
  const ArrayColumn<Float>& tcalSpectrum() const {return tcalSpectrum_p;}
  const ArrayQuantColumn<Float>& tcalSpectrumQuant() const {
    return tcalSpectrumQuant_p;}
  const ArrayColumn<Float>& trx() const {return trx_p;}
  const ArrayQuantColumn<Float>& trxQuant() const {return trxQuant_p;}
  const ScalarColumn<Bool>& trxFlag() const {return trxFlag_p;}
  const ArrayColumn<Float>& trxSpectrum() const {return trxSpectrum_p;}
  const ArrayQuantColumn<Float>& trxSpectrumQuant() const {
    return trxSpectrumQuant_p;}
  const ArrayColumn<Float>& tsky() const {return tsky_p;}
  const ArrayQuantColumn<Float>& tskyQuant() const {return tskyQuant_p;}
  const ScalarColumn<Bool>& tskyFlag() const {return tskyFlag_p;}
  const ArrayColumn<Float>& tskySpectrum() const {return tskySpectrum_p;}
  const ArrayQuantColumn<Float>& tskySpectrumQuant() const {
    return tskySpectrumQuant_p;}
  const ArrayColumn<Float>& tsys() const {return tsys_p;}
  const ArrayQuantColumn<Float>& tsysQuant() const {return tsysQuant_p;}
  const ScalarColumn<Bool>& tsysFlag() const {return tsysFlag_p;}
  const ArrayColumn<Float>& tsysSpectrum() const {return tsysSpectrum_p;}
  const ArrayQuantColumn<Float>& tsysSpectrumQuant() const {
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
  // reference, offset, or units can be set by using a False
  // <src>tableMustBeEmpty</src> argument.
  // </note>
  void setEpochRef(MEpoch::Types ref, Bool tableMustBeEmpty=True);

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
  Bool isNull_p;

  //# required columns
  ScalarColumn<Int> antennaId_p;
  ScalarColumn<Int> feedId_p;
  ScalarColumn<Double> interval_p;
  ScalarColumn<Int> spectralWindowId_p;
  ScalarColumn<Double> time_p;
  //# optional columns
  ScalarColumn<Float> phaseDiff_p;
  ScalarColumn<Bool> phaseDiffFlag_p;
  ArrayColumn<Float> tant_p;
  ScalarColumn<Bool> tantFlag_p;
  ArrayColumn<Float> tantSpectrum_p;
  ArrayColumn<Float> tantTsys_p;
  ScalarColumn<Bool> tantTsysFlag_p;
  ArrayColumn<Float> tantTsysSpectrum_p;
  ArrayColumn<Float> tcal_p;
  ScalarColumn<Bool> tcalFlag_p;
  ArrayColumn<Float> tcalSpectrum_p;
  ArrayColumn<Float> trx_p;
  ScalarColumn<Bool> trxFlag_p;
  ArrayColumn<Float> trxSpectrum_p;
  ArrayColumn<Float> tsky_p;
  ScalarColumn<Bool> tskyFlag_p;
  ArrayColumn<Float> tskySpectrum_p;
  ArrayColumn<Float> tsys_p;
  ScalarColumn<Bool> tsysFlag_p;
  ArrayColumn<Float> tsysSpectrum_p;

  //# Access to Measure columns
  ScalarMeasColumn<MEpoch> timeMeas_p;

  //# Access to Quantum columns
  ScalarQuantColumn<Double> intervalQuant_p;
  ScalarQuantColumn<Double> timeQuant_p;
  //# optional Quantum columns
  ScalarQuantColumn<Float> phaseDiffQuant_p;
  ArrayQuantColumn<Float> tantQuant_p;
  ArrayQuantColumn<Float> tantSpectrumQuant_p;
  ArrayQuantColumn<Float> tcalQuant_p;
  ArrayQuantColumn<Float> tcalSpectrumQuant_p;
  ArrayQuantColumn<Float> trxQuant_p;
  ArrayQuantColumn<Float> trxSpectrumQuant_p;
  ArrayQuantColumn<Float> tskyQuant_p;
  ArrayQuantColumn<Float> tskySpectrumQuant_p;
  ArrayQuantColumn<Float> tsysQuant_p;
  ArrayQuantColumn<Float> tsysSpectrumQuant_p;
};

//# Define the RO version for backward compatibility.
typedef MSSysCalColumns ROMSSysCalColumns;

} //# NAMESPACE CASACORE - END

#endif
