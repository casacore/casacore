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

#if !defined(AIPS_NEWMSSYSCALCOLUMNS_H)
#define AIPS_NEWMSSYSCALCOLUMNS_H

#include <aips/aips.h>
#include <aips/Measures/MEpoch.h>
#include <aips/TableMeasures/ArrayQuantColumn.h>
#include <aips/TableMeasures/ScalarMeasColumn.h>
#include <aips/TableMeasures/ScalarQuantColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/ScalarColumn.h>

class NewMSSysCal;

// <summary>
// A class to provide easy read-only access to NewMSSysCal columns
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
// RONewMSSysCalColumns stands for Read-Only NewMeasurementSet SysCal Table
// columns.
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
  // Create a columns object that accesses the data in the specified Table
  RONewMSSysCalColumns(const NewMSSysCal& msSysCal);

  // The destructor does nothing special
  ~RONewMSSysCalColumns();

  // Is this object defined? (NewMSSysCal table is optional)
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
  RONewMSSysCalColumns();

  //# attach this object to the supplied table.
  void attach(const NewMSSysCal& msSysCal);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  RONewMSSysCalColumns(const RONewMSSysCalColumns&);
  RONewMSSysCalColumns& operator=(const RONewMSSysCalColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const NewMSSysCal& msSysCal);

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
// A class to provide easy read-write access to NewMSSysCal columns
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

class NewMSSysCalColumns: public RONewMSSysCalColumns
{
public:
  // Create a columns object that accesses the data in the specified Table
  NewMSSysCalColumns(NewMSSysCal& msSysCal);

  // The destructor does nothing special
  ~NewMSSysCalColumns();

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
    return RONewMSSysCalColumns::antennaId();}
  const ROScalarColumn<Int>& feedId() const {
    return RONewMSSysCalColumns::feedId();}
  const ROScalarColumn<Double>& interval() const {
    return RONewMSSysCalColumns::interval();}
  const ROScalarQuantColumn<Double>& intervalQuant() const {
    return RONewMSSysCalColumns::intervalQuant();}
  const ROScalarColumn<Int>& spectralWindowId() const {
    return RONewMSSysCalColumns::spectralWindowId();}
  const ROScalarColumn<Double>& time() const {
    return RONewMSSysCalColumns::time();}
  const ROScalarQuantColumn<Double>& timeQuant() const {
    return RONewMSSysCalColumns::timeQuant();}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const {
    return RONewMSSysCalColumns::timeMeas();}
  // </group>

  // Read-only access to optional columns
  // <group>
  const ROScalarColumn<Float>& phaseDiff() const {
    return RONewMSSysCalColumns::phaseDiff();}
  const ROScalarQuantColumn<Float>& phaseDiffQuant() const {
    return RONewMSSysCalColumns::phaseDiffQuant();}
  const ROScalarColumn<Bool>& phaseDiffFlag() const {
    return RONewMSSysCalColumns::phaseDiffFlag();}
  const ROArrayColumn<Float>& tant() const {
    return RONewMSSysCalColumns::tant();}
  const ROArrayQuantColumn<Float>& tantQuant() const {
    return RONewMSSysCalColumns::tantQuant();}
  const ROScalarColumn<Bool>& tantFlag() const {
    return RONewMSSysCalColumns::tantFlag();}
  const ROArrayColumn<Float>& tantSpectrum() const {
    return RONewMSSysCalColumns::tantSpectrum();}
  const ROArrayQuantColumn<Float>& tantSpectrumQuant() const {
    return RONewMSSysCalColumns::tantSpectrumQuant();}
  const ROArrayColumn<Float>& tantTsys() const {
    return RONewMSSysCalColumns::tantTsys();}
  const ROScalarColumn<Bool>& tantTsysFlag() const {
    return RONewMSSysCalColumns::tantTsysFlag();}
  const ROArrayColumn<Float>& tantTsysSpectrum() const {
    return RONewMSSysCalColumns::tantTsysSpectrum();}
  const ROArrayColumn<Float>& tcal() const {
    return RONewMSSysCalColumns::tcal();}
  const ROArrayQuantColumn<Float>& tcalQuant() const {
    return RONewMSSysCalColumns::tcalQuant();}
  const ROScalarColumn<Bool>& tcalFlag() const {
    return RONewMSSysCalColumns::tcalFlag();}
  const ROArrayColumn<Float>& tcalSpectrum() const {
    return RONewMSSysCalColumns::tcalSpectrum();}
  const ROArrayQuantColumn<Float>& tcalSpectrumQuant() const {
    return RONewMSSysCalColumns::tcalSpectrumQuant();}
  const ROArrayColumn<Float>& trx() const {return RONewMSSysCalColumns::trx();}
  const ROArrayQuantColumn<Float>& trxQuant() const {
    return RONewMSSysCalColumns::trxQuant();}
  const ROScalarColumn<Bool>& trxFlag() const {
    return RONewMSSysCalColumns::trxFlag();}
  const ROArrayColumn<Float>& trxSpectrum() const {
    return RONewMSSysCalColumns::trxSpectrum();}
  const ROArrayQuantColumn<Float>& trxSpectrumQuant() const {
    return RONewMSSysCalColumns::trxSpectrumQuant();}
  const ROArrayColumn<Float>& tsky() const {
    return RONewMSSysCalColumns::tsky();}
  const ROArrayQuantColumn<Float>& tskyQuant() const {
    return RONewMSSysCalColumns::tskyQuant();}
  const ROScalarColumn<Bool>& tskyFlag() const {
    return RONewMSSysCalColumns::tskyFlag();}
  const ROArrayColumn<Float>& tskySpectrum() const {
    return RONewMSSysCalColumns::tskySpectrum();}
  const ROArrayQuantColumn<Float>& tskySpectrumQuant() const {
    return RONewMSSysCalColumns::tskySpectrumQuant();}
  const ROArrayColumn<Float>& tsys() const {
    return RONewMSSysCalColumns::tsys();}
  const ROArrayQuantColumn<Float>& tsysQuant() const {
    return RONewMSSysCalColumns::tsysQuant();}
  const ROScalarColumn<Bool>& tsysFlag() const {
    return RONewMSSysCalColumns::tsysFlag();}
  const ROArrayColumn<Float>& tsysSpectrum() const {
    return RONewMSSysCalColumns::tsysSpectrum();}
  const ROArrayQuantColumn<Float>& tsysSpectrumQuant() const {
    return RONewMSSysCalColumns::tsysSpectrumQuant();}
  // </group>

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  NewMSSysCalColumns();

  //# attach this object to the supplied table.
  void attach(NewMSSysCal& msSysCal);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  NewMSSysCalColumns(const NewMSSysCalColumns&);
  NewMSSysCalColumns& operator=(const NewMSSysCalColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(NewMSSysCal& msSysCal);
  
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
#endif
