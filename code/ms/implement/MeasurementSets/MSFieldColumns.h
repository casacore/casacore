//# NewMSFieldColumns.h: provides easy access to NewMSField columns
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

#if !defined(AIPS_NewMSFIELDCOLUMNS_H)
#define AIPS_NewMSFIELDCOLUMNS_H

#include <aips/MeasurementSets/NewMSField.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Quanta/Unit.h>
#include <aips/Quanta/Quantum.h>
#include <aips/TableMeasures/ScalarMeasColumn.h>
#include <aips/TableMeasures/ArrayMeasColumn.h>
#include <aips/TableMeasures/ScalarQuantColumn.h>
#include <aips/TableMeasures/ArrayQuantColumn.h>

#include <aips/Measures/MDirection.h>
class MEpoch;

// <summary>
// A convenience class to provide easy access to NewMSField columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSField
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// NewMSFieldColumns stands for NewMeasurementSet Field Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the NewMSField Table,
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

class NewMSFieldColumns
{
public:

  NewMSFieldColumns(NewMSField& msField);

  ~NewMSFieldColumns();

  // Access to columns
  ScalarColumn<String>& code() {return code_p;}
  ArrayColumn<Double>& delayDir() {return delayDir_p;}
  ScalarColumn<Bool>& flagRow() {return flagRow_p;}
  ScalarColumn<String>& name() {return name_p;}
  ScalarColumn<Int>& numPoly() {return numPoly_p;}
  ArrayColumn<Double>& phaseDir() {return phaseDir_p;}
  ArrayColumn<Double>& referenceDir() {return referenceDir_p;}
  ScalarColumn<Int>& sourceId() {return sourceId_p;}
  ScalarColumn<Double>& time() {return time_p;}
  ScalarColumn<Int>& ephemerisId() {return ephemerisId_p;}

  // Access to Measure columns. 
  // Note that the Directions with stored polynomial have Col() added to their
  // name, they are better accessed via the interpolated functions below.
  ArrayMeasColumn<MDirection>& delayDirMeasCol() 
    {return delayDirMeas_p;}
  ArrayMeasColumn<MDirection>& phaseDirMeasCol() 
    {return phaseDirMeas_p;}
  ArrayMeasColumn<MDirection>& referenceDirMeasCol() 
    {return referenceDirMeas_p;}
  ScalarMeasColumn<MEpoch>& timeMeas() { return timeMeas_p;}

  // Access to Quantum columns
  ScalarQuantColumn<Double>& timeQuant() { return timeQuant_p;}
  //# ArrayQuantColumn<Double>& delayDirQuant() { return delayDirQuant_p;}
  //# ArrayQuantColumn<Double>& phaseDirQuant() { return phaseDirQuant_p;}
  //# ArrayQuantColumn<Double>& referenceDirQuant() { return referenceDirQuant_p;}

  // Access to interpolated directions, the default time of zero will
  // return the 0th order element of the polynomial.
  MDirection delayDirMeas(Int row, Double time = 0);
  MDirection phaseDirMeas(Int row, Double time = 0);
  MDirection referenceDirMeas(Int row, Double time = 0);

  // Interpolate the direction Measure polynomial
  static MDirection interpolateDirMeas(const Array<MDirection>& arrDir, Int numPoly, 
				Double interTime, Double timeOrigin);
private:


  ScalarColumn<String> code_p;
  ArrayColumn<Double> delayDir_p;
  ScalarColumn<Bool> flagRow_p;
  ScalarColumn<String> name_p;
  ScalarColumn<Int> numPoly_p;
  ArrayColumn<Double> phaseDir_p;
  ArrayColumn<Double> referenceDir_p;
  ScalarColumn<Int> sourceId_p;
  ScalarColumn<Double> time_p;
  ScalarColumn<Int> ephemerisId_p;
  
  // Access to Measure columns
  ArrayMeasColumn<MDirection> delayDirMeas_p;
  ArrayMeasColumn<MDirection> phaseDirMeas_p;
  ArrayMeasColumn<MDirection> referenceDirMeas_p;
  ScalarMeasColumn<MEpoch> timeMeas_p;

  // Access to Quantum columns
//#ArrayQuantColumn<Double> delayDirQuant_p;
//#ArrayQuantColumn<Double> phaseDirQuant_p;
//#ArrayQuantColumn<Double> referenceDirQuant_p;
  ScalarQuantColumn<Double> timeQuant_p;

};

// <summary>
// A convenience class to provide easy access to NewMSField columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> NewMSField
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// RONewMSFieldColumns stands for Read-Only NewMeasurementSet Field Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the NewMSField Table.
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

class RONewMSFieldColumns
{
public:

  RONewMSFieldColumns(const NewMSField& msField);

  ~RONewMSFieldColumns();

  // Access to columns
  const ROScalarColumn<String>& code() const {return code_p;}
  const ROArrayColumn<Double>& delayDir() const {return delayDir_p;}
  const ROScalarColumn<Bool>& flagRow() const {return flagRow_p;}
  const ROScalarColumn<String>& name() const {return name_p;}
  const ROScalarColumn<Int>& numPoly() const {return numPoly_p;}
  const ROArrayColumn<Double>& phaseDir() const {return phaseDir_p;}
  const ROArrayColumn<Double>& referenceDir() const {return referenceDir_p;}
  const ROScalarColumn<Int>& sourceId() const {return sourceId_p;}
  const ROScalarColumn<Double>& time() const {return time_p;}
  const ROScalarColumn<Int>& ephemerisId() const {return ephemerisId_p;}

  // Access to Measure columns
  const ROArrayMeasColumn<MDirection>& delayDirMeasCol() const 
    {return delayDirMeas_p;}
  const ROArrayMeasColumn<MDirection>& phaseDirMeasCol() const 
    {return phaseDirMeas_p;}
  const ROArrayMeasColumn<MDirection>& referenceDirMeasCol() const 
    {return referenceDirMeas_p;}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const { return timeMeas_p;}

  // Access to Quantum columns
  const ROScalarQuantColumn<Double>& timeQuant() const { return timeQuant_p;}

  //#  const ROArrayQuantColumn<Double>& delayDirQuant() const { return delayDirQuant_p;}
  //#  const ROArrayQuantColumn<Double>& phaseDirQuant() const { return phaseDirQuant_p;}
  //#  const ROArrayQuantColumn<Double>& referenceDirQuant() const { return referenceDirQuant_p;}

  // Access to interpolated directions, the default time of zero will
  // return the 0th order element of the polynomial.
  MDirection delayDirMeas(Int row, Double time = 0) const;
  MDirection phaseDirMeas(Int row, Double time = 0) const;
  MDirection referenceDirMeas(Int row, Double time = 0) const;


private:

  ROScalarColumn<String> code_p;
  ROArrayColumn<Double> delayDir_p;
  ROScalarColumn<Bool> flagRow_p;
  ROScalarColumn<String> name_p;
  ROScalarColumn<Int> numPoly_p;
  ROArrayColumn<Double> phaseDir_p;
  ROArrayColumn<Double> referenceDir_p;
  ROScalarColumn<Int> sourceId_p;
  ROScalarColumn<Double> time_p;
  ROScalarColumn<Int> ephemerisId_p;

  // Access to Measure columns
  ROArrayMeasColumn<MDirection> delayDirMeas_p;
  ROArrayMeasColumn<MDirection> phaseDirMeas_p;
  ROArrayMeasColumn<MDirection> referenceDirMeas_p;
  ROScalarMeasColumn<MEpoch> timeMeas_p;

  // Access to Quantum columns
  ROScalarQuantColumn<Double> timeQuant_p;

//# ROArrayQuantColumn<Double> delayDirQuant_p;
//# ROArrayQuantColumn<Double> phaseDirQuant_p;
//# ROArrayQuantColumn<Double> referenceDirQuant_p;
};

#endif
