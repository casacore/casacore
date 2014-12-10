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
//#
//# $Id$

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
// A class to provide easy read-only access to MSSysCal columns
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
// ROMSSysCalColumns stands for Read-Only MeasurementSet SysCal Table
// columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the MSSysCal Table.
// It does the declaration of all the Scalar and ArrayColumns with the
// correct types, so the application programmer doesn't have to
// worry about getting those right. There is an access function
// for every predefined column. Access to non-predefined columns will still
// have to be done with explicit declarations.
// See <linkto class=ROMSColumns> ROMSColumns</linkto> for an example.
// </synopsis>
//
// <motivation>
// See <linkto class=MSColumns> MSColumns</linkto> for the motivation.
// </motivation>

class ROMSSysCalColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  ROMSSysCalColumns(const MSSysCal& msSysCal);

  // The destructor does nothing special
  ~ROMSSysCalColumns();

  // Is this object defined? (MSSysCal table is optional)
  Bool isNull() const {return isNull_p;}

  // Access to columns
  // <group>
  const ROScalarColumn<Int>& antennaId() const {return antennaId_p;}
  const ROScalarColumn<Int>& feedId() const {return feedId_p;}
  const ROScalarColumn<Double>& interval() const {return interval_p;}
  const ROScalarQuantColumn<Double>& intervalQuant() const {
    return intervalQuant_p;}
  const ROScalarColumn<Int>& spectralWindowId() const {
    return spectralWindowId_p;}
  const ROScalarColumn<Double>& time() const {return time_p;}
  const ROScalarQuantColumn<Double>& timeQuant() const {return timeQuant_p;}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const {return timeMeas_p;}
  // </group>

  // Access to optional columns
  // <group>
  const ROScalarColumn<Float>& phaseDiff() const {return phaseDiff_p;}
  const ROScalarQuantColumn<Float>& phaseDiffQuant() const {
    return phaseDiffQuant_p;}
  const ROScalarColumn<Bool>& phaseDiffFlag() const {return phaseDiffFlag_p;}
  const ROArrayColumn<Float>& tant() const {return tant_p;}
  const ROArrayQuantColumn<Float>& tantQuant() const {return tantQuant_p;}
  const ROScalarColumn<Bool>& tantFlag() const {return tantFlag_p;}
  const ROArrayColumn<Float>& tantSpectrum() const {return tantSpectrum_p;}
  const ROArrayQuantColumn<Float>& tantSpectrumQuant() const {
    return tantSpectrumQuant_p;}
  const ROArrayColumn<Float>& tantTsys() const {return tantTsys_p;}
  const ROScalarColumn<Bool>& tantTsysFlag() const {return tantTsysFlag_p;}
  const ROArrayColumn<Float>& tantTsysSpectrum() const {
    return tantTsysSpectrum_p;}
  const ROArrayColumn<Float>& tcal() const {return tcal_p;}
  const ROArrayQuantColumn<Float>& tcalQuant() const {return tcalQuant_p;}
  const ROScalarColumn<Bool>& tcalFlag() const {return tcalFlag_p;}
  const ROArrayColumn<Float>& tcalSpectrum() const {return tcalSpectrum_p;}
  const ROArrayQuantColumn<Float>& tcalSpectrumQuant() const {
    return tcalSpectrumQuant_p;}
  const ROArrayColumn<Float>& trx() const {return trx_p;}
  const ROArrayQuantColumn<Float>& trxQuant() const {return trxQuant_p;}
  const ROScalarColumn<Bool>& trxFlag() const {return trxFlag_p;}
  const ROArrayColumn<Float>& trxSpectrum() const {return trxSpectrum_p;}
  const ROArrayQuantColumn<Float>& trxSpectrumQuant() const {
    return trxSpectrumQuant_p;}
  const ROArrayColumn<Float>& tsky() const {return tsky_p;}
  const ROArrayQuantColumn<Float>& tskyQuant() const {return tskyQuant_p;}
  const ROScalarColumn<Bool>& tskyFlag() const {return tskyFlag_p;}
  const ROArrayColumn<Float>& tskySpectrum() const {return tskySpectrum_p;}
  const ROArrayQuantColumn<Float>& tskySpectrumQuant() const {
    return tskySpectrumQuant_p;}
  const ROArrayColumn<Float>& tsys() const {return tsys_p;}
  const ROArrayQuantColumn<Float>& tsysQuant() const {return tsysQuant_p;}
  const ROScalarColumn<Bool>& tsysFlag() const {return tsysFlag_p;}
  const ROArrayColumn<Float>& tsysSpectrum() const {return tsysSpectrum_p;}
  const ROArrayQuantColumn<Float>& tsysSpectrumQuant() const {
    return tsysSpectrumQuant_p;}
  // </group>

  // Convenience function that returns the number of rows in any of the
  // columns. Returns zero if the object is null.
  uInt nrow() const {return isNull() ? 0 : antennaId_p.nrow();}

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  ROMSSysCalColumns();

  //# attach this object to the supplied table.
  void attach(const MSSysCal& msSysCal);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  ROMSSysCalColumns(const ROMSSysCalColumns&);
  ROMSSysCalColumns& operator=(const ROMSSysCalColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const MSSysCal& msSysCal);

  //# Is the object not attached to a Table.
  Bool isNull_p;

  //# required columns
  ROScalarColumn<Int> antennaId_p;
  ROScalarColumn<Int> feedId_p;
  ROScalarColumn<Double> interval_p;
  ROScalarColumn<Int> spectralWindowId_p;
  ROScalarColumn<Double> time_p;
  //# optional columns
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

  //# Access to Measure columns
  ROScalarMeasColumn<MEpoch> timeMeas_p;

  //# Access to Quantum columns
  ROScalarQuantColumn<Double> intervalQuant_p;
  ROScalarQuantColumn<Double> timeQuant_p;
  //# Optional Quantum columns
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

// <summary>
// A class to provide easy read-write access to MSSysCal columns
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

class MSSysCalColumns: public ROMSSysCalColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  MSSysCalColumns(MSSysCal& msSysCal);

  // The destructor does nothing special
  ~MSSysCalColumns();

  // Read-write access to required columns
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

  // Read-write access to optional columns
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

  // Read-only access to required columns
  // <group>
  const ROScalarColumn<Int>& antennaId() const {
    return ROMSSysCalColumns::antennaId();}
  const ROScalarColumn<Int>& feedId() const {
    return ROMSSysCalColumns::feedId();}
  const ROScalarColumn<Double>& interval() const {
    return ROMSSysCalColumns::interval();}
  const ROScalarQuantColumn<Double>& intervalQuant() const {
    return ROMSSysCalColumns::intervalQuant();}
  const ROScalarColumn<Int>& spectralWindowId() const {
    return ROMSSysCalColumns::spectralWindowId();}
  const ROScalarColumn<Double>& time() const {
    return ROMSSysCalColumns::time();}
  const ROScalarQuantColumn<Double>& timeQuant() const {
    return ROMSSysCalColumns::timeQuant();}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const {
    return ROMSSysCalColumns::timeMeas();}
  // </group>

  // Read-only access to optional columns
  // <group>
  const ROScalarColumn<Float>& phaseDiff() const {
    return ROMSSysCalColumns::phaseDiff();}
  const ROScalarQuantColumn<Float>& phaseDiffQuant() const {
    return ROMSSysCalColumns::phaseDiffQuant();}
  const ROScalarColumn<Bool>& phaseDiffFlag() const {
    return ROMSSysCalColumns::phaseDiffFlag();}
  const ROArrayColumn<Float>& tant() const {
    return ROMSSysCalColumns::tant();}
  const ROArrayQuantColumn<Float>& tantQuant() const {
    return ROMSSysCalColumns::tantQuant();}
  const ROScalarColumn<Bool>& tantFlag() const {
    return ROMSSysCalColumns::tantFlag();}
  const ROArrayColumn<Float>& tantSpectrum() const {
    return ROMSSysCalColumns::tantSpectrum();}
  const ROArrayQuantColumn<Float>& tantSpectrumQuant() const {
    return ROMSSysCalColumns::tantSpectrumQuant();}
  const ROArrayColumn<Float>& tantTsys() const {
    return ROMSSysCalColumns::tantTsys();}
  const ROScalarColumn<Bool>& tantTsysFlag() const {
    return ROMSSysCalColumns::tantTsysFlag();}
  const ROArrayColumn<Float>& tantTsysSpectrum() const {
    return ROMSSysCalColumns::tantTsysSpectrum();}
  const ROArrayColumn<Float>& tcal() const {
    return ROMSSysCalColumns::tcal();}
  const ROArrayQuantColumn<Float>& tcalQuant() const {
    return ROMSSysCalColumns::tcalQuant();}
  const ROScalarColumn<Bool>& tcalFlag() const {
    return ROMSSysCalColumns::tcalFlag();}
  const ROArrayColumn<Float>& tcalSpectrum() const {
    return ROMSSysCalColumns::tcalSpectrum();}
  const ROArrayQuantColumn<Float>& tcalSpectrumQuant() const {
    return ROMSSysCalColumns::tcalSpectrumQuant();}
  const ROArrayColumn<Float>& trx() const {return ROMSSysCalColumns::trx();}
  const ROArrayQuantColumn<Float>& trxQuant() const {
    return ROMSSysCalColumns::trxQuant();}
  const ROScalarColumn<Bool>& trxFlag() const {
    return ROMSSysCalColumns::trxFlag();}
  const ROArrayColumn<Float>& trxSpectrum() const {
    return ROMSSysCalColumns::trxSpectrum();}
  const ROArrayQuantColumn<Float>& trxSpectrumQuant() const {
    return ROMSSysCalColumns::trxSpectrumQuant();}
  const ROArrayColumn<Float>& tsky() const {
    return ROMSSysCalColumns::tsky();}
  const ROArrayQuantColumn<Float>& tskyQuant() const {
    return ROMSSysCalColumns::tskyQuant();}
  const ROScalarColumn<Bool>& tskyFlag() const {
    return ROMSSysCalColumns::tskyFlag();}
  const ROArrayColumn<Float>& tskySpectrum() const {
    return ROMSSysCalColumns::tskySpectrum();}
  const ROArrayQuantColumn<Float>& tskySpectrumQuant() const {
    return ROMSSysCalColumns::tskySpectrumQuant();}
  const ROArrayColumn<Float>& tsys() const {
    return ROMSSysCalColumns::tsys();}
  const ROArrayQuantColumn<Float>& tsysQuant() const {
    return ROMSSysCalColumns::tsysQuant();}
  const ROScalarColumn<Bool>& tsysFlag() const {
    return ROMSSysCalColumns::tsysFlag();}
  const ROArrayColumn<Float>& tsysSpectrum() const {
    return ROMSSysCalColumns::tsysSpectrum();}
  const ROArrayQuantColumn<Float>& tsysSpectrumQuant() const {
    return ROMSSysCalColumns::tsysSpectrumQuant();}
  // </group>

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
  void attach(MSSysCal& msSysCal);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  MSSysCalColumns(const MSSysCalColumns&);
  MSSysCalColumns& operator=(const MSSysCalColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(MSSysCal& msSysCal);
  
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

} //# NAMESPACE CASACORE - END

#endif
