//# FluxCalcVQS.cc: Implementation of FluxCalcVQS.h
//# Copyright (C) 2013
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
#include <components/ComponentModels/FluxCalcVQS.h>
#include <casa/Arrays/Vector.h>
#include <casa/BasicSL/String.h>
#include <casa/Logging/LogIO.h>
#include <casa/Quanta/MVTime.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MFrequency.h>
#include <measures/Measures/MEpoch.h>
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/ArrayColumn.h>

// Handy for passing anonymous arrays to functions.
#include <scimath/Mathematics/RigidVector.h>
#include <scimath/Functionals/ScalarSampledFunctional.h>

#include <components/ComponentModels/FluxCalcLogFreqPolynomial.h>

#include <map>


namespace casa { //# NAMESPACE CASA - BEGIN
FluxCalcVQS::FluxCalcVQS() :
  srcEnum_p(FluxStdSrcs::UNKNOWN_SOURCE),
  istimevar_p(false)
{ }

// Defined even though it's pure virtual; see http://www.gotw.ca/gotw/031.htm
FluxCalcVQS::~FluxCalcVQS() { }
Bool FluxCalcVQS::operator()(Vector<Flux<Double> >& values,
                            Vector<Flux<Double> >& errors,
                            const Vector<MFrequency>& mfreqs)
{
  uInt nfreqs = mfreqs.nelements();
  
  values.resize(nfreqs);
  errors.resize(nfreqs);
  
  if (coeffsmat_p.nelements()) {
      // coeffs have been read from a table.
      uInt nep=0; // should be a single epoch (single row)
      setSourceCoeffsfromVec(nep);    
  }

  Bool success = true;
  for(uInt f = 0; f < nfreqs; ++f)
    success &= (*this)(values[f], errors[f], mfreqs[f], False);

  return success;
}

//with interpolation over epochs
Bool FluxCalcVQS::operator()(Vector<Flux<Double> >& values,
                            Vector<Flux<Double> >& errors,
                            const Vector<MFrequency>& mfreqs,
                            const MEpoch& mtime,
                            const String& interpmethod)

{
  uInt nfreqs = mfreqs.nelements();

  values.resize(nfreqs);
  errors.resize(nfreqs);

  // for those considered to be variable, for each set of coefficients
  // at each epoch, call the follwoing to get values (fluxes)
  // and accumate in vector of vector to do interpolation.
  // istimevar_p to determine if the source is variable. If not
  // no interpolation is done, use first row of the coeffs table. 
  
  Bool success = true;

  Int nep = 1;
  if (istimevar_p) nep=epochvec_p.nelements();

  Flux<Double> tmpfluxes;
  Flux<Double> tmperrors;
  Vector<Double> tmpfluxvec;
  Vector<Double> tmperrvec;
  
  fluxes_p.resize(nep);
  for(uInt f = 0; f < nfreqs; ++f){
    for(uInt iep=0; iep < (uInt)nep; ++iep) {
      setSourceCoeffsfromVec(iep);    
      success &= (*this)(tmpfluxes,tmperrors,mfreqs[f],True);
      tmpfluxes.value(tmpfluxvec);
      tmperrors.value(tmperrvec);
      // currently only I flux is returned...
      //cerr<<"tmpfluxvec[0]="<<tmpfluxvec[0]<<endl;
      //cerr<<"epochvec_p[iep]="<<epochvec_p[iep]<<endl;
      fluxes_p[iep]=tmpfluxvec[0];
    }   
    if (istimevar_p) {
      //setup interpolator 
      ScalarSampledFunctional<Double> dts(epochvec_p);
      ScalarSampledFunctional<Float> flxs(fluxes_p);
      Interpolate1D<Double, Float> interpolateFlux(dts,flxs);
      interpolateFlux.setMethod(getInterpMethod_p(interpmethod));

      values[f].setValue(interpolateFlux(mtime.get("d").getValue()));
    }
    else { // no interpolation for non-var source, use first row data
      values[f].setValue(fluxes_p[0]);  
    }
  }

//      success &= (*this)(values[f], errors[f], mfreqs[f]);
    
  return success;
}

Bool FluxCalcVQS::setSource(const String& sourceName, const MDirection& sourceDir)
{
  srcEnum_p = srcNameToEnum(sourceName,sourceDir);
  //return srcEnum_p != FCQS::UNKNOWN_SOURCE;
  return srcEnum_p != FSS::UNKNOWN_SOURCE;
}

FluxCalcVQS::Source FluxCalcVQS::getSrcEnum()
{
  return srcEnum_p;
}

void FluxCalcVQS::readQSCoeffsTable(const Path& fileName)
{
  //table containing the coefficents has
  //epoch, vector of coefficients
  const String& fullName = fileName.absoluteName();
  LogIO os(LogOrigin("FluxCalcVQS", "readQSCoeffsTable", WHERE));
   os << LogIO::NORMAL1
      << "Reading the coefficient data from a table, "<<  fullName
      << LogIO::POST;
  
  AlwaysAssert(Table::isReadable(fullName), AipsError);
  Table_p = Table(fullName, Table::Old);
  //String srcName(names_p[srcEnum_p](0));
  String srcName(EnumToSrcName(srcEnum_p));
  String srcCoeffColName=srcName+"_coeffs";
  String srcCoeffErrorColName=srcName+"_coefferrs";

  //check the source data exist in the table
  const ColumnDescSet& cds=Table_p.tableDesc().columnDescSet();
  if (!cds.isDefined(srcCoeffColName)) 
    throw(AipsError(srcName+" does not appears to be in "+fullName));
  const ROScalarColumn<Double> epochCol(Table_p, "Epoch");
  const ROArrayColumn<Float> CoeffCol(Table_p, srcCoeffColName);
  const ROArrayColumn<Float> CoeffErrorCol(Table_p, srcCoeffErrorColName);
  Vector<Double> tempEpochs;
  epochCol.getColumn(tempEpochs,True);
  CoeffCol.getColumn(coeffsmat_p,True);
  CoeffErrorCol.getColumn(coefferrsmat_p,True);
  //convert the epoch (year + fraction) to mjd
  convertYearFracToMjd(tempEpochs,epochvec_p);
  os << LogIO::DEBUG1
     << "nepoch="<<epochvec_p.nelements()
     << "coeff_0 for the first epoch (coeffsmat_p(0,0))="<<coeffsmat_p(0,0) 
     << LogIO::POST;
}

void FluxCalcVQS::interpolate(const String& interpmethod)
{
  ScalarSampledFunctional<Double> dts(epochvec_p);
  ScalarSampledFunctional<Float> flxs(fluxes_p);
  Interpolate1D<Double, Float> interpolateFlux(dts,flxs);
  interpolateFlux.setMethod(getInterpMethod_p(interpmethod));
}

Interpolate1D<Double,Float>::Method FluxCalcVQS::getInterpMethod_p(const String& interpmethod)
{
  if (interpmethod.contains("nearest"))
    return Interpolate1D<Double,Float>::nearestNeighbour;
  if (interpmethod.contains("linear"))
    return Interpolate1D<Double,Float>::linear;
  if (interpmethod.contains("cubic")) 
    return Interpolate1D<Double,Float>::cubic;
  if (interpmethod.contains("spline"))
    return Interpolate1D<Double,Float>::spline;
  throw(AipsError("Unknown interpolation method: "+interpmethod));
}

void FluxCalcVQS::convertYearFracToMjd(const Vector<Double>& yearfrac, Vector<Double>& mjds)
{
  uInt daysintheyr=365;
  uInt n = yearfrac.nelements();
  mjds.resize(n);
  for (uInt i=0; i < n; i++) {
    if (Time::isLeapYear(uInt(yearfrac(i)))) daysintheyr=366;
    Double fintyr, fyrfrac;
    fyrfrac = modf((Double)yearfrac(i),&fintyr);
    Float days=fyrfrac*daysintheyr;
    Time time0(Int(yearfrac(i)),1,1);
    Double mjdtime0 = time0.modifiedJulianDay();

    mjds[i] = mjdtime0 + days;
  }
}
    
void FluxCalcVQS::setSourceCoeffsfromVec(uInt& i) 
{
  //cerr<<"i="<<i<<endl;
  //Vector<Float> err(4,0.0);
  tvcoeffs_p(0)=coeffsmat_p.column(i); 
  //cerr<<"coeffsmat_p.column.nelements()="<<coeffsmat_p.column(i).nelements()<<endl;
  //cerr<<"err.elements()="<<err.nelements()<<endl;
  tvcoeffs_p(1)=coefferrsmat_p.column(i); 
}

void FluxCalcVQS::isTimeVar(Bool istimevar) 
{
  istimevar_p=istimevar;
}

} //# NAMESPACE CASA - END
