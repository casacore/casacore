//# NewMSSysCalColumns.h: provides easy access to NewMSSysCal columns
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

#if !defined(AIPS_NewMSSYSCALCOLUMNS_H)
#define AIPS_NewMSSYSCALCOLUMNS_H

#include <aips/MeasurementSets/NewMSSysCal.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Quanta/Unit.h>
#include <aips/Quanta/Quantum.h>
#include <aips/TableMeasures/ScalarMeasColumn.h>
#include <aips/TableMeasures/ScalarQuantColumn.h>
#include <aips/TableMeasures/ArrayQuantColumn.h>
class MEpoch;

// <summary>
// A convenience class to provide easy access to NewMSSysCal columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSSysCal
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// NewMSSysCalColumns stands for NewMeasurementSet SysCal Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the NewMSSysCal Table,
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

class NewMSSysCalColumns
{
public:

  NewMSSysCalColumns(NewMSSysCal& msSysCal);

  ~NewMSSysCalColumns();

  // Is this object defined? (NewMSSysCal table is optional)
  Bool isNull() {return isNull_p;}

  // Access to columns
  ScalarColumn<Int>& antennaId() {return antennaId_p;}
  ScalarColumn<Int>& feedId() {return feedId_p;}
  ScalarColumn<Double>& interval() {return interval_p;}
  ScalarColumn<Int>& spectralWindowId() {return spectralWindowId_p;}
  ScalarColumn<Double>& time() {return time_p;}
  ScalarColumn<Float>& phaseDiff() {return phaseDiff_p;}
  ScalarColumn<Bool>& phaseDiffFlag() {return phaseDiffFlag_p;}
  ArrayColumn<Float>& tant() {return tant_p;}
  ScalarColumn<Bool>& tantFlag() {return tantFlag_p;}
  ArrayColumn<Float>& tantSpectrum() {return tantSpectrum_p;}
  ArrayColumn<Float>& tantTsys() {return tantTsys_p;}
  ScalarColumn<Bool>& tantTsysFlag() {return tantTsysFlag_p;}
  ArrayColumn<Float>& tantTsysSpectrum() {return tantTsysSpectrum_p;}
  ArrayColumn<Float>& tcal() {return tcal_p;}
  ScalarColumn<Bool>& tcalFlag() {return tcalFlag_p;}
  ArrayColumn<Float>& tcalSpectrum() {return tcalSpectrum_p;}
  ArrayColumn<Float>& trx() {return trx_p;}
  ScalarColumn<Bool>& trxFlag() {return trxFlag_p;}
  ArrayColumn<Float>& trxSpectrum() {return trxSpectrum_p;}
  ArrayColumn<Float>& tsky() {return tsky_p;}
  ScalarColumn<Bool>& tskyFlag() {return tskyFlag_p;}
  ArrayColumn<Float>& tskySpectrum() {return tskySpectrum_p;}
  ArrayColumn<Float>& tsys() {return tsys_p;}
  ScalarColumn<Bool>& tsysFlag() {return tsysFlag_p;}
  ArrayColumn<Float>& tsysSpectrum() {return tsysSpectrum_p;}

  // Access to Measure columns
  ScalarMeasColumn<MEpoch>& timeMeas() { return timeMeas_p;}

  // Access to Quantum columns
  ScalarQuantColumn<Double>& intervalQuant() { return intervalQuant_p;}
  ScalarQuantColumn<Double>& timeQuant() { return timeQuant_p;}
  ScalarQuantColumn<Float>& phaseDiffQuant() { return phaseDiffQuant_p;}
  ArrayQuantColumn<Float>& tantQuant() { return tantQuant_p;}
  ArrayQuantColumn<Float>& tantSpectrumQuant() { return tantSpectrumQuant_p;}
  ArrayQuantColumn<Float>& tcalQuant() { return tcalQuant_p;}
  ArrayQuantColumn<Float>& tcalSpectrumQuant() { return tcalSpectrumQuant_p;}
  ArrayQuantColumn<Float>& trxQuant() { return trxQuant_p;}
  ArrayQuantColumn<Float>& trxSpectrumQuant() { return trxSpectrumQuant_p;}
  ArrayQuantColumn<Float>& tskyQuant() { return tskyQuant_p;}
  ArrayQuantColumn<Float>& tskySpectrumQuant() { return tskySpectrumQuant_p;}
  ArrayQuantColumn<Float>& tsysQuant() { return tsysQuant_p;}
  ArrayQuantColumn<Float>& tsysSpectrumQuant() { return tsysSpectrumQuant_p;}

private:

  Bool isNull_p;
  ScalarColumn<Int> antennaId_p;
  ScalarColumn<Int> feedId_p;
  ScalarColumn<Double> interval_p;
  ScalarColumn<Int> spectralWindowId_p;
  ScalarColumn<Double> time_p;
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

  // Access to Measure columns
  ScalarMeasColumn<MEpoch> timeMeas_p;

  // Access to Quantum columns
  ScalarQuantColumn<Double> intervalQuant_p;
  ScalarQuantColumn<Double> timeQuant_p;
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

// <summary>
// A convenience class to provide easy access to NewMSSysCal columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSSysCal
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// RONewMSSysCalColumns stands for Read-Only NewMeasurementSet SysCal Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the NewMSSysCal Table.
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

class RONewMSSysCalColumns
{
public:

  RONewMSSysCalColumns(const NewMSSysCal& msSysCal);

  ~RONewMSSysCalColumns();

  // Is this object defined? (NewMSSysCal table is optional)
  Bool isNull() {return isNull_p;}

  // Access to columns
  const ROScalarColumn<Int>& antennaId() const {return antennaId_p;}
  const ROScalarColumn<Int>& feedId() const {return feedId_p;}
  const ROScalarColumn<Double>& interval() const {return interval_p;}
  const ROScalarColumn<Int>& spectralWindowId() const {return spectralWindowId_p;}
  const ROScalarColumn<Double>& time() const {return time_p;}
  const ROScalarColumn<Float>& phaseDiff() const {return phaseDiff_p;}
  const ROScalarColumn<Bool>& phaseDiffFlag() const {return phaseDiffFlag_p;}
  const ROArrayColumn<Float>& tant() const {return tant_p;}
  const ROScalarColumn<Bool>& tantFlag() const {return tantFlag_p;}
  const ROArrayColumn<Float>& tantSpectrum() const {return tantSpectrum_p;}
  const ROArrayColumn<Float>& tantTsys() const {return tantTsys_p;}
  const ROScalarColumn<Bool>& tantTsysFlag() const {return tantTsysFlag_p;}
  const ROArrayColumn<Float>& tantTsysSpectrum() const {return tantTsysSpectrum_p;}
  const ROArrayColumn<Float>& tcal() const {return tcal_p;}
  const ROScalarColumn<Bool>& tcalFlag() const {return tcalFlag_p;}
  const ROArrayColumn<Float>& tcalSpectrum() const {return tcalSpectrum_p;}
  const ROArrayColumn<Float>& trx() const {return trx_p;}
  const ROScalarColumn<Bool>& trxFlag() const {return trxFlag_p;}
  const ROArrayColumn<Float>& trxSpectrum() const {return trxSpectrum_p;}
  const ROArrayColumn<Float>& tsky() const {return tsky_p;}
  const ROScalarColumn<Bool>& tskyFlag() const {return tskyFlag_p;}
  const ROArrayColumn<Float>& tskySpectrum() const {return tskySpectrum_p;}
  const ROArrayColumn<Float>& tsys() const {return tsys_p;}
  const ROScalarColumn<Bool>& tsysFlag() const {return tsysFlag_p;}
  const ROArrayColumn<Float>& tsysSpectrum() const {return tsysSpectrum_p;}

  // Access to Measure columns
  const ROScalarMeasColumn<MEpoch>& timeMeas() const { return timeMeas_p;}

  // Access to Quantum columns
  const ROScalarQuantColumn<Double>& intervalQuant() const { return intervalQuant_p;}
  const ROScalarQuantColumn<Double>& timeQuant() const { return timeQuant_p;}
  const ROScalarQuantColumn<Float>& phaseDiffQuant() const { return phaseDiffQuant_p;}
  const ROArrayQuantColumn<Float>& tantQuant() const { return tantQuant_p;}
  const ROArrayQuantColumn<Float>& tantSpectrumQuant() const { return tantSpectrumQuant_p;}
  const ROArrayQuantColumn<Float>& tcalQuant() const { return tcalQuant_p;}
  const ROArrayQuantColumn<Float>& tcalSpectrumQuant() const { return tcalSpectrumQuant_p;}
  const ROArrayQuantColumn<Float>& trxQuant() const { return trxQuant_p;}
  const ROArrayQuantColumn<Float>& trxSpectrumQuant() const { return trxSpectrumQuant_p;}
  const ROArrayQuantColumn<Float>& tskyQuant() const { return tskyQuant_p;}
  const ROArrayQuantColumn<Float>& tskySpectrumQuant() const { return tskySpectrumQuant_p;}
  const ROArrayQuantColumn<Float>& tsysQuant() const { return tsysQuant_p;}
  const ROArrayQuantColumn<Float>& tsysSpectrumQuant() const { return tsysSpectrumQuant_p;}

private:

  Bool isNull_p;
  ROScalarColumn<Int> antennaId_p;
  ROScalarColumn<Int> feedId_p;
  ROScalarColumn<Double> interval_p;
  ROScalarColumn<Int> spectralWindowId_p;
  ROScalarColumn<Double> time_p;
  ROScalarColumn<Float> phaseDiff_p;
  ROScalarColumn<Bool> phaseDiffFlag_p;
  ROArrayColumn<Float> tant_p;
  ROScalarColumn<Bool> tantFlag_p;
  ROArrayColumn<Float> tantSpectrum_p;
  ROArrayColumn<Float> tantTsys_p;
  ROScalarColumn<Bool> tantTsysFlag_p;
  ROArrayColumn<Float> tantTsysSpectrum_p;
  ROArrayColumn<Float> tcal_p;
  ROScalarColumn<Bool> tcalFlag_p;
  ROArrayColumn<Float> tcalSpectrum_p;
  ROArrayColumn<Float> trx_p;
  ROScalarColumn<Bool> trxFlag_p;
  ROArrayColumn<Float> trxSpectrum_p;
  ROArrayColumn<Float> tsky_p;
  ROScalarColumn<Bool> tskyFlag_p;
  ROArrayColumn<Float> tskySpectrum_p;
  ROArrayColumn<Float> tsys_p;
  ROScalarColumn<Bool> tsysFlag_p;
  ROArrayColumn<Float> tsysSpectrum_p;

  // Access to Measure columns
  ROScalarMeasColumn<MEpoch> timeMeas_p;

  // Access to Quantum columns
  ROScalarQuantColumn<Double> intervalQuant_p;
  ROScalarQuantColumn<Double> timeQuant_p;
  ROScalarQuantColumn<Float> phaseDiffQuant_p;
  ROArrayQuantColumn<Float> tantQuant_p;
  ROArrayQuantColumn<Float> tantSpectrumQuant_p;
  ROArrayQuantColumn<Float> tcalQuant_p;
  ROArrayQuantColumn<Float> tcalSpectrumQuant_p;
  ROArrayQuantColumn<Float> trxQuant_p;
  ROArrayQuantColumn<Float> trxSpectrumQuant_p;
  ROArrayQuantColumn<Float> tskyQuant_p;
  ROArrayQuantColumn<Float> tskySpectrumQuant_p;
  ROArrayQuantColumn<Float> tsysQuant_p;
  ROArrayQuantColumn<Float> tsysSpectrumQuant_p;
};

#endif
