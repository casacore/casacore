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

#if !defined(AIPS_NEWMSFIELDCOLUMNS_H)
#define AIPS_NEWMSFIELDCOLUMNS_H

#include <aips/aips.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MEpoch.h>
#include <aips/TableMeasures/ArrayMeasColumn.h>
#include <aips/TableMeasures/ScalarMeasColumn.h>
#include <aips/TableMeasures/ScalarQuantColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Utilities/String.h>

class MVDirection;
class NewMSField;
template <class Qtype> class Quantum;
template <class T> class Matrix;

// <summary>
// A class to provide easy access to NewMSField columns
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
  // Construct from the supplied Table
  RONewMSFieldColumns(const NewMSField& msField);

  // The desctructor does nothing special
  ~RONewMSFieldColumns();

  // Read-only access to required columns
  // <group>
  const ROScalarColumn<String>& code() const {return code_p;}
  const ROArrayColumn<Double>& delayDir() const {return delayDir_p;}
  const ROArrayMeasColumn<MDirection>& delayDirMeasCol() const 
    {return delayDirMeas_p;}
  const ROScalarColumn<Bool>& flagRow() const {return flagRow_p;}
  const ROScalarColumn<String>& name() const {return name_p;}
  const ROScalarColumn<Int>& numPoly() const {return numPoly_p;}
  const ROArrayColumn<Double>& phaseDir() const {return phaseDir_p;}
  const ROArrayMeasColumn<MDirection>& phaseDirMeasCol() const 
    {return phaseDirMeas_p;}
  const ROArrayColumn<Double>& referenceDir() const {return referenceDir_p;}
  const ROArrayMeasColumn<MDirection>& referenceDirMeasCol() const 
    {return referenceDirMeas_p;}
  const ROScalarColumn<Int>& sourceId() const {return sourceId_p;}
  const ROScalarColumn<Double>& time() const {return time_p;}
  const ROScalarQuantColumn<Double>& timeQuant() const { return timeQuant_p;}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const { return timeMeas_p;}
  // </group>

  // Read-only access to optional columns
  // <group>
  const ROScalarColumn<Int>& ephemerisId() const {return ephemerisId_p;}
  // </group>

  // Access to interpolated directions, the default time of zero will
  // return the 0th order element of the polynomial.
  // <group>
  MDirection delayDirMeas(Int row, Double time = 0) const;
  MDirection phaseDirMeas(Int row, Double time = 0) const;
  MDirection referenceDirMeas(Int row, Double time = 0) const;
  // </group>

  // Convenience function that returns the number of rows in any of the columns
  uInt nrow() const {return name_p.nrow();}

  // returns the last row that has a reference direction, phase direction and
  // delay direction that match, to within the specified angular separation,
  // the supplied values. Only matches on rows where the direction is constant
  // ie., NUM_POLY is 0 and where FLAG_ROW is False. Throws an exception
  // (AipsError) if the reference frames do not match or if the separation does
  // not have angular units (when compiled in debug mode). Returns -1 if no
  // match could be found. If tryRow is positive, then that row is tested to
  // see if it matches before any others are tested. Setting tryRow to a
  // positive value greater than the table length will throw an exception
  // (AipsError), when compiled in debug mode.
  Int matchDirection(const MDirection& referenceDirection, 
		     const MDirection& delayDirection,
		     const MDirection& phaseDirection,
		     const Quantum<Double>& maxSeparation, Int tryRow=-1);

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  RONewMSFieldColumns();

  //# attach this object to the supplied table.
  void attach(const NewMSField& msField);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  RONewMSFieldColumns(const RONewMSFieldColumns&);
  RONewMSFieldColumns& operator=(const RONewMSFieldColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(const NewMSField& msField);
  
  //# Functions which check the supplied values against the relevant column and
  //# the specified row. The row must have a numpoly value of zero and the
  //# specified mdir arguments must have a shape of [1,2]. It and the mvdir
  //# argument are temporaries that are passed in to prevent them being
  //# created inside these small functions.
  // <group>
  Bool matchReferenceDir(uInt row, const MVDirection& dirVal,
			 const Double& sepInRad, 
			 Matrix<Double>& mdir, MVDirection& mvdir) const;
  Bool matchDelayDir(uInt row, const MVDirection& dirVal, 
		     const Double& sepInRad,
		     Matrix<Double>& mdir, MVDirection& mvdir) const;
  Bool matchPhaseDir(uInt row, const MVDirection& dirVal,
		     const Double& sepInRad,
		     Matrix<Double>& mdir, MVDirection& mvdir) const;
  // </group>

  //# required columns
  ROScalarColumn<String> name_p;
  ROScalarColumn<String> code_p;
  ROScalarColumn<Double> time_p;
  ROScalarColumn<Int> numPoly_p;
  ROArrayColumn<Double> delayDir_p;
  ROArrayColumn<Double> phaseDir_p;
  ROArrayColumn<Double> referenceDir_p;
  ROScalarColumn<Int> sourceId_p;
  ROScalarColumn<Bool> flagRow_p;
  //# optional columns
  ROScalarColumn<Int> ephemerisId_p;

  //# Access to Measure columns
  ROScalarMeasColumn<MEpoch> timeMeas_p;
  ROArrayMeasColumn<MDirection> delayDirMeas_p;
  ROArrayMeasColumn<MDirection> phaseDirMeas_p;
  ROArrayMeasColumn<MDirection> referenceDirMeas_p;

  //# Access to Quantum columns
  ROScalarQuantColumn<Double> timeQuant_p;
};

// <summary>
// A class to provide easy read-write access to NewMSField columns
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

class NewMSFieldColumns: public RONewMSFieldColumns
{
public:
  // Construct from the supplied Table
  NewMSFieldColumns(NewMSField& msField);

  // The desctructor does nothing special
  ~NewMSFieldColumns();

  // Read-write access to required columns
  //
  // Note that the direction measures with a stored polynomial have Col() added
  // to their name. They are better accessed via the functions that have the
  // same name, without the Col suffix, that will do the interpolation for
  // you. These functions are in the RONewMSFieldColumns class.
  // <group>
  ScalarColumn<String>& code() {return code_p;}
  ArrayColumn<Double>& delayDir() {return delayDir_p;}
  ArrayMeasColumn<MDirection>& delayDirMeasCol() 
    {return delayDirMeas_p;}
  ScalarColumn<Bool>& flagRow() {return flagRow_p;}
  ScalarColumn<String>& name() {return name_p;}
  ScalarColumn<Int>& numPoly() {return numPoly_p;}
  ArrayColumn<Double>& phaseDir() {return phaseDir_p;}
  ArrayMeasColumn<MDirection>& phaseDirMeasCol() 
    {return phaseDirMeas_p;}
  ArrayColumn<Double>& referenceDir() {return referenceDir_p;}
  ArrayMeasColumn<MDirection>& referenceDirMeasCol() 
    {return referenceDirMeas_p;}
  ScalarColumn<Int>& sourceId() {return sourceId_p;}
  ScalarColumn<Double>& time() {return time_p;}
  ScalarQuantColumn<Double>& timeQuant() { return timeQuant_p;}
  ScalarMeasColumn<MEpoch>& timeMeas() { return timeMeas_p;}
  // </group>

  // Read-write access to optional columns
  // <group>
  ScalarColumn<Int>& ephemerisId() {return ephemerisId_p;}
  // </group>

  // Read-only access to required columns
  // <group>
  const ROScalarColumn<String>& code() const {
    return RONewMSFieldColumns::code();}
  const ROArrayColumn<Double>& delayDir() const {
    return RONewMSFieldColumns::delayDir();}
  const ROArrayMeasColumn<MDirection>& delayDirMeasCol() const {
    return RONewMSFieldColumns::delayDirMeasCol();}
  const ROScalarColumn<Bool>& flagRow() const {
    return RONewMSFieldColumns::flagRow();}
  const ROScalarColumn<String>& name() const {
    return RONewMSFieldColumns::name();}
  const ROScalarColumn<Int>& numPoly() const {
    return RONewMSFieldColumns::numPoly();}
  const ROArrayColumn<Double>& phaseDir() const {
    return RONewMSFieldColumns::phaseDir();}
  const ROArrayMeasColumn<MDirection>& phaseDirMeasCol() const {
    return RONewMSFieldColumns::phaseDirMeasCol();}
  const ROArrayColumn<Double>& referenceDir() const {
    return RONewMSFieldColumns::referenceDir();}
  const ROArrayMeasColumn<MDirection>& referenceDirMeasCol() const {
    return RONewMSFieldColumns::referenceDirMeasCol();}
  const ROScalarColumn<Int>& sourceId() const {
    return RONewMSFieldColumns::sourceId();}
  const ROScalarColumn<Double>& time() const {
    return RONewMSFieldColumns::time();}
  const ROScalarQuantColumn<Double>& timeQuant() const {
    return RONewMSFieldColumns::timeQuant();}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const { 
    return RONewMSFieldColumns::timeMeas();}
  // </group>

  // Read-only access to optional columns
  // <group>
  const ROScalarColumn<Int>& ephemerisId() const {
    return RONewMSFieldColumns::ephemerisId();}
  // </group>

  // Interpolate the direction Measure polynomial
  static MDirection interpolateDirMeas(const Array<MDirection>& arrDir, 
				       Int numPoly, Double interTime, 
				       Double timeOrigin);

  // set the epoch reference type for the TIME column. This can only be done
  // when the table has no rows. Trying to do so at other times will throw an
  // exception.
  void setEpochRef(MEpoch::Types ref);

  // set the direction reference type for the REFERENCE_DIR, DELAY_DIR &
  // PHASE_DIR columns. This can only be done when the table has no
  // rows. Trying to do so at other times will throw an exception.
  void setDirectionRef(MDirection::Types ref);

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  NewMSFieldColumns();

  //# attach this object to the supplied table.
  void attach(NewMSField& msField);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  NewMSFieldColumns(const NewMSFieldColumns&);
  NewMSFieldColumns& operator=(const NewMSFieldColumns&);

  //# Check if any optional columns exist and if so attach them.
  void attachOptionalCols(NewMSField& msField);
  
  //# required columns
  ScalarColumn<String> name_p;
  ScalarColumn<String> code_p;
  ScalarColumn<Double> time_p;
  ScalarColumn<Int> numPoly_p;
  ArrayColumn<Double> delayDir_p;
  ArrayColumn<Double> phaseDir_p;
  ArrayColumn<Double> referenceDir_p;
  ScalarColumn<Int> sourceId_p;
  ScalarColumn<Bool> flagRow_p;
  //# optional columns
  ScalarColumn<Int> ephemerisId_p;
  
  //# Access to Measure columns
  ScalarMeasColumn<MEpoch> timeMeas_p;
  ArrayMeasColumn<MDirection> delayDirMeas_p;
  ArrayMeasColumn<MDirection> phaseDirMeas_p;
  ArrayMeasColumn<MDirection> referenceDirMeas_p;

  //# Access to Quantum columns
  ScalarQuantColumn<Double> timeQuant_p;

};

#endif
