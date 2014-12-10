//# MSFieldColumns.h: provides easy access to MSField columns
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

#ifndef MS_MSFIELDCOLUMNS_H
#define MS_MSFIELDCOLUMNS_H

#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MCDirection.h>
#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/measures/Measures/MRadialVelocity.h>
#include <casacore/measures/Measures/MeasComet.h>
#include <casacore/measures/TableMeasures/ArrayMeasColumn.h>
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/measures/TableMeasures/ScalarQuantColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Containers/SimOrdMap.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MVDirection;
class MSField;
template <class Qtype> class Quantum;
template <class T> class Matrix;

// <summary>
// A class to provide easy access to MSField columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSField
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// ROMSFieldColumns stands for Read-Only MeasurementSet Field Table columns.
// </etymology>
//
// <synopsis>
// This class provides read-only access to the columns in the MSField Table.
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

class ROMSFieldColumns
{
public:
  // Construct from the supplied Table
  ROMSFieldColumns(const MSField& msField);

  // The desctructor does nothing special
  ~ROMSFieldColumns();

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

  // Access to interpolated directions from polynomials or ephemerides, 
  // the default time of zero will return the 0th order element of the polynomial.
  // or, if there is an ephemeris, the position at the time origin of the ephemeris.
  // 
  // In addtion to the directions, if there is an ephemeris available,
  // also the radial velocity and the distance rho can be accessed.
  //
  // The method needInterTime returns True if there is a polynomial or ephemeris
  // connected to this field table row, and an interpolation time value should
  // be provided.
  // The method ephemPath returns the absolute path to the ephemeris table connected to 
  // the field table row, an empty string if there is none.  
  // <group>
  MDirection delayDirMeas(Int row, Double time = 0) const;
  MDirection phaseDirMeas(Int row, Double time = 0) const;
  MDirection referenceDirMeas(Int row, Double time = 0) const;
  MRadialVelocity radVelMeas(Int row, Double time = 0) const;
  Quantity rho(Int row, Double time = 0) const;
  Bool needInterTime(Int row) const;
  String ephemPath(Int row) const;

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
		     const Quantum<Double>& maxSeparation, 
		     Int tryRow=-1,
		     Double time=0);

  // Update the MeasComets objects belonging to this FIELD table.
  // Needed when the entries in the EPHEMERIS_ID column have changed.
  void updateMeasComets();


protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  ROMSFieldColumns();

  //# attach this object to the supplied table.
  void attach(const MSField& msField);

  Int measCometIndex(int row) const;
  String measCometsPath_p;
  Vector<MeasComet*> measCometsV_p;
  SimpleOrderedMap <Int, Int> ephIdToMeasComet_p;

  // Extract the direction Measure from the corresponding ephemeris
  // using the nominal position as an offset.
  // Note that interTime is assumed to use the same time reference frame
  // as originEpoch.
  MDirection extractDirMeas(const MDirection& offsetDir, 
			    Int index, Double& interTime, 
			    MEpoch originEpoch) const;

  void getMJDs(Double& originMJD, Double& interMJD, 
	       const Double interTime, const MEpoch originEpoch) const;

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  ROMSFieldColumns(const ROMSFieldColumns&);
  ROMSFieldColumns& operator=(const ROMSFieldColumns&);

  //# Check if any optional columns exist and if so attach them.
  //# Initialise the necessary MeasComet objects if the EPHEMERIS_ID column is present.
  void attachOptionalCols(const MSField& msField);
  
  //# Functions which check the supplied values against the relevant column and
  //# the specified row. The row must have a numpoly value of zero. The mvdir
  //# argument is a temporary that is passed in to prevent it from being
  //# created inside these small functions.
  // <group>
  Bool matchReferenceDir(uInt row, const MVDirection& dirVal,
			 const Double& sepInRad, 
			 MVDirection& mvdir, Double time=0) const;
  Bool matchDelayDir(uInt row, const MVDirection& dirVal, 
		     const Double& sepInRad,
		     MVDirection& mvdir, Double time=0) const;
  Bool matchPhaseDir(uInt row, const MVDirection& dirVal,
		     const Double& sepInRad,
		     MVDirection& mvdir, Double time=0) const;
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
// A class to provide easy read-write access to MSField columns
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="1997/02/01" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MSField
//   <li> ArrayColumn
//   <li> ScalarColumn
// </prerequisite>
//
// <etymology>
// MSFieldColumns stands for MeasurementSet Field Table columns.
// </etymology>
//
// <synopsis>
// This class provides access to the columns in the MSField Table,
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

class MSFieldColumns: public ROMSFieldColumns
{
public:
  // Construct from the supplied Table
  MSFieldColumns(MSField& msField);

  // The desctructor does nothing special
  ~MSFieldColumns();

  // Read-write access to required columns
  //
  // Note that the direction measures with a stored polynomial have Col() added
  // to their name. They are better accessed via the functions that have the
  // same name, without the Col suffix, that will do the interpolation for
  // you. These functions are in the ROMSFieldColumns class.
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
    return ROMSFieldColumns::code();}
  const ROArrayColumn<Double>& delayDir() const {
    return ROMSFieldColumns::delayDir();}
  const ROArrayMeasColumn<MDirection>& delayDirMeasCol() const {
    return ROMSFieldColumns::delayDirMeasCol();}
  const ROScalarColumn<Bool>& flagRow() const {
    return ROMSFieldColumns::flagRow();}
  const ROScalarColumn<String>& name() const {
    return ROMSFieldColumns::name();}
  const ROScalarColumn<Int>& numPoly() const {
    return ROMSFieldColumns::numPoly();}
  const ROArrayColumn<Double>& phaseDir() const {
    return ROMSFieldColumns::phaseDir();}
  const ROArrayMeasColumn<MDirection>& phaseDirMeasCol() const {
    return ROMSFieldColumns::phaseDirMeasCol();}
  const ROArrayColumn<Double>& referenceDir() const {
    return ROMSFieldColumns::referenceDir();}
  const ROArrayMeasColumn<MDirection>& referenceDirMeasCol() const {
    return ROMSFieldColumns::referenceDirMeasCol();}
  const ROScalarColumn<Int>& sourceId() const {
    return ROMSFieldColumns::sourceId();}
  const ROScalarColumn<Double>& time() const {
    return ROMSFieldColumns::time();}
  const ROScalarQuantColumn<Double>& timeQuant() const {
    return ROMSFieldColumns::timeQuant();}
  const ROScalarMeasColumn<MEpoch>& timeMeas() const { 
    return ROMSFieldColumns::timeMeas();}
  // </group>

  // Read-only access to optional columns
  // <group>
  const ROScalarColumn<Int>& ephemerisId() const {
    return ROMSFieldColumns::ephemerisId();}
  // </group>

  // Interpolate the direction Measure polynomial
  static MDirection interpolateDirMeas(const Array<MDirection>& arrDir, 
				       Int numPoly, Double interTime, 
				       Double timeOrigin);


  // set the epoch reference type for the TIME column. 
  // <note role=tip>
  // In principle this function can only be used if the table is empty,
  // otherwise already written values may thereafter have an incorrect
  // reference, offset, or unit.  However, it is possible that part of the
  // table gets written before these values are known.  In that case the
  // reference, offset, or units can be set by using a False
  // <src>tableMustBeEmpty</src> argument.
  // </note>
  void setEpochRef(MEpoch::Types ref, Bool tableMustBeEmpty=True);

  // set the direction reference type for the REFERENCE_DIR, DELAY_DIR &
  // PHASE_DIR columns. This can only be done when the table has no
  // rows. Trying to do so at other times will throw an exception.
  void setDirectionRef(MDirection::Types ref);

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  MSFieldColumns();

  //# attach this object to the supplied table.
  void attach(MSField& msField);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  MSFieldColumns(const MSFieldColumns&);
  MSFieldColumns& operator=(const MSFieldColumns&);

  //# Check if any optional columns exist and if so attach them.
  //# Initialise the necessary MeasComet objects if the EPHEMERIS_ID column is present.
  void attachOptionalCols(MSField& msField);
  
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


} //# NAMESPACE CASACORE - END

#endif
