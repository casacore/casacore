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
#include <casacore/casa/Arrays/ArrayFwd.h>
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
#include <map>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MVDirection;
class MSField;
template <class Qtype> class Quantum;


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

class MSFieldColumns
{
public:
  // Construct from the supplied Table
  MSFieldColumns(const MSField& msField);

  // The desctructor does nothing special
  ~MSFieldColumns();

  // Access to required columns
  //
  // Note that the direction measures with a stored polynomial have Col() added
  // to their name. They are better accessed via the functions that have the
  // same name, without the Col suffix, that will do the interpolation for
  // you.
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

  // Const access to required columns
  // <group>
  const ScalarColumn<String>& code() const {return code_p;}
  const ArrayColumn<Double>& delayDir() const {return delayDir_p;}
  const ArrayMeasColumn<MDirection>& delayDirMeasCol() const
    {return delayDirMeas_p;}
  const ScalarColumn<Bool>& flagRow() const {return flagRow_p;}
  const ScalarColumn<String>& name() const {return name_p;}
  const ScalarColumn<Int>& numPoly() const {return numPoly_p;}
  const ArrayColumn<Double>& phaseDir() const {return phaseDir_p;}
  const ArrayMeasColumn<MDirection>& phaseDirMeasCol() const
    {return phaseDirMeas_p;}
  const ArrayColumn<Double>& referenceDir() const {return referenceDir_p;}
  const ArrayMeasColumn<MDirection>& referenceDirMeasCol() const
    {return referenceDirMeas_p;}
  const ScalarColumn<Int>& sourceId() const {return sourceId_p;}
  const ScalarColumn<Double>& time() const {return time_p;}
  const ScalarQuantColumn<Double>& timeQuant() const { return timeQuant_p;}
  const ScalarMeasColumn<MEpoch>& timeMeas() const { return timeMeas_p;}
  // </group>

  // Access to optional columns
  // <group>
  ScalarColumn<Int>& ephemerisId() {return ephemerisId_p;}
  // </group>

  // Const access to optional columns
  // <group>
  const ScalarColumn<Int>& ephemerisId() const {return ephemerisId_p;}
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

  // Access to interpolated directions from polynomials or ephemerides, 
  // the default time of zero will return the 0th order element of the polynomial.
  // or, if there is an ephemeris, the position at the time origin of the ephemeris.
  // 
  // If there is an ephemeris attached to a field table row, the nominal values of the
  // direction columns are interpreted as an offset to the ephemeris. So if there is
  // an ephemeris attached (EPHEMERIS_ID column contains value > -1), then the direction
  // returned by  delayDirMeas, phaseDirMeas, and referenceDirMeas is the ephemeris
  // direction plus the offset taken from the corresponding direction column.
  // This permits the convinient implementation of mosaics where several field table
  // rows share one ephemeris and use different offsets in each row to create the
  // mosaic pattern.
  // 
  // The unaltered ephemeris direction can be queried with the method ephemerisDirMeas(). 
  // If there is no ephemeris attached, ephemerisDirMeas() will return the same as 
  // referenceDirMeas().
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
  MDirection delayDirMeas(rownr_t row, Double time = 0) const;
  MDirection phaseDirMeas(rownr_t row, Double time = 0) const;
  MDirection referenceDirMeas(rownr_t row, Double time = 0) const;
  MDirection ephemerisDirMeas(rownr_t row, Double time = 0) const;
  MRadialVelocity radVelMeas(rownr_t row, Double time = 0) const;
  Quantity rho(rownr_t row, Double time = 0) const;
  Bool needInterTime(rownr_t row) const;
  String ephemPath(rownr_t row) const;

  // </group>

  // Convenience function that returns the number of rows in any of the columns
  rownr_t nrow() const {return name_p.nrow();}

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
  Int64 matchDirection(const MDirection& referenceDirection, 
                       const MDirection& delayDirection,
                       const MDirection& phaseDirection,
                       const Quantum<Double>& maxSeparation, 
                       Int64 tryRow=-1,
                       Double time=0);

  // Update the MeasComets objects belonging to this FIELD table.
  // Needed when the entries in the EPHEMERIS_ID column have changed.
  void updateMeasComets();

protected:
  //# default constructor creates a object that is not usable. Use the attach
  //# function correct this.
  MSFieldColumns();

  //# attach this object to the supplied table.
  void attach(const MSField& msField);

private:
  //# Make the assignment operator and the copy constructor private to prevent
  //# any compiler generated one from being used.
  MSFieldColumns(const MSFieldColumns&);
  MSFieldColumns& operator=(const MSFieldColumns&);

  //# Check if any optional columns exist and if so attach them.
  //# Initialise the necessary MeasComet objects if the EPHEMERIS_ID column is present.
  void attachOptionalCols(const MSField& msField);
  
  //# Functions which check the supplied values against the relevant column and
  //# the specified row. The row must have a numpoly value of zero. The mvdir
  //# argument is a temporary that is passed in to prevent it from being
  //# created inside these small functions.
  // <group>
  Bool matchReferenceDir(rownr_t row, const MVDirection& dirVal,
			 const Double& sepInRad, 
			 MVDirection& mvdir, Double time=0) const;
  Bool matchDelayDir(rownr_t row, const MVDirection& dirVal, 
		     const Double& sepInRad,
		     MVDirection& mvdir, Double time=0) const;
  Bool matchPhaseDir(rownr_t row, const MVDirection& dirVal,
		     const Double& sepInRad,
		     MVDirection& mvdir, Double time=0) const;
  // </group>

  Int measCometIndex(rownr_t row) const;

  // Extract the direction Measure from the corresponding ephemeris
  // using the nominal position as an offset.
  // Note that interTime is assumed to use the same time reference frame
  // as originEpoch.
  MDirection extractDirMeas(const MDirection& offsetDir, 
			    Int index, Double& interTime, 
			    MEpoch originEpoch) const;

  void getMJDs(Double& originMJD, Double& interMJD, 
	       const Double interTime, const MEpoch originEpoch) const;

  //# MeasComet data
  String measCometsPath_p;
  Vector<MeasComet*> measCometsV_p;
  std::map<Int, Int> ephIdToMeasComet_p;

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

//# Define the RO version for backward compatibility.
typedef MSFieldColumns ROMSFieldColumns;

} //# NAMESPACE CASACORE - END

#endif
