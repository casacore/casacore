//# MSContinuumSubtractor.cc:  Subtract continuum from spectral line data
//# Copyright (C) 2004
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
//#
//#include <casa/Quanta/MVTime.h>
#include <casa/Containers/RecordFieldId.h>
#include <measures/Measures/Stokes.h>
#include <ms/MeasurementSets/MSColumns.h>
#include <ms/MeasurementSets/MeasurementSet.h>
#include <ms/MeasurementSets/MSSelector.h>
#include <ms/MeasurementSets/MSSelection.h>
//#include <ms/MeasurementSets/MSRange.h>
#include <msvis/MSVis/MSContinuumSubtractor.h>
#include <msvis/MSVis/VisSet.h>
#include <scimath/Fitting/LinearFit.h>
#include <scimath/Functionals/Polynomial.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayUtil.h>
//#include <casa/Arrays/MaskedArray.h>
//#include <casa/Arrays/MaskArrMath.h>
#include <casa/Containers/Record.h>
#include <casa/Exceptions/Error.h>
#include <casa/Logging/LogIO.h>
#include <tables/Tables/TableParse.h>
//#include <casa/iomanip.h>
#include <casa/iostream.h>


namespace casa { //# NAMESPACE CASA - BEGIN

//
// Constructor assigns pointer (if MS goes out of scope you will get rubbish)
MSContinuumSubtractor::MSContinuumSubtractor (MeasurementSet& ms)
  : ms_p(&ms),itsSolInt(0.0),itsOrder(0),itsMode("subtract")
{

  // Make a VisSet so scratch columns are created
  Block<Int> nosort(0);
  Matrix<Int> noselection;
  Double timeInterval=0.0;
  Bool compress(False);
  VisSet vs(ms,nosort,noselection,timeInterval,compress);

  nSpw_= vs.numberSpw();

}


//
// Assignment operator
//
MSContinuumSubtractor& MSContinuumSubtractor::operator=(MSContinuumSubtractor& other)
{
  if (this==&other) return *this;
  ms_p = other.ms_p;
  return *this;
}


//
// Destructor does nothing
//
MSContinuumSubtractor::~MSContinuumSubtractor()
{}

// 
// Set the spws to process
//
void MSContinuumSubtractor::setSpw(const String& spw)
{

  // NB: this method assumes spwids == ddids!

  //  cout << "spw = " << spw << endl;

  MSSelection mssel;
  mssel.setSpwExpr(spw);
  Vector<Int> spwlist;
  spwlist=mssel.getSpwList(ms_p);

  if (spwlist.nelements()==0) {
    spwlist.resize(nSpw_);
    indgen(spwlist);
  }
   
  //  cout << "spwids = " << spwlist << endl;

  setDataDescriptionIds(spwlist);

}

void MSContinuumSubtractor::setDataDescriptionIds(const Vector<Int>& ddIds)
{
  itsDDIds = ddIds;
}

// Set the required field Ids
void MSContinuumSubtractor::setField(const String& field)
{

  //  cout << "field = " << field << endl;

  MSSelection mssel;
  mssel.setFieldExpr(field);
  Vector<Int> fldlist;
  fldlist=mssel.getFieldList(ms_p);

  //  cout << "fldlist = " << fldlist << endl;

  setFields(fldlist);

}

void MSContinuumSubtractor::setFields(const Vector<Int>& fieldIds)
{
  itsFieldIds = fieldIds;
}

// Set the channels to use in the fit
void MSContinuumSubtractor::setChannels(const Vector<Int>& channels)
{
  itsChannels = channels;
}

// Set the solution interval in seconds, the value zero implies scan averaging
void MSContinuumSubtractor::setSolutionInterval(Float solInt)
{
  itsSolInt = solInt;
}

// Set the order of the fit (1=linear)
void MSContinuumSubtractor::setOrder(Int order)
{
  itsOrder = order;
}

// Set the processing mode: subtract, model or replace
void MSContinuumSubtractor::setMode(const String& mode)
{
  itsMode = mode;
  cerr<<"mode now set to "<< mode<<endl;
}

// Do the subtraction (or save the model)
void MSContinuumSubtractor::subtract()
{
  
  LogIO os(LogOrigin("MSContinuumSubtractor","subtract"));
  os<< LogIO::NORMAL<< "MSContinuumSubtractor::subtract() - parameters:"<<
      "ddIds="<<itsDDIds<<", fieldIds="<<itsFieldIds<<", channels="<<
      itsChannels<<", solInt="<<itsSolInt<<", order="<<itsOrder<<
      ", mode="<<itsMode<<LogIO::POST;
  

  ostringstream select;
  select <<"select from $1 where ANTENNA1!=ANTENNA2";
  if (itsFieldIds.nelements()>0) {
    select<<" && FIELD_ID IN ["<<itsFieldIds(0);
    for (uInt j=1; j<itsFieldIds.nelements(); j++) select<<", "<<itsFieldIds(j);
    select<<"]";
  }
  if (itsDDIds.nelements()>0) {
    select<<" && DATA_DESC_ID IN ["<<itsDDIds(0);
    for (uInt j=1; j<itsDDIds.nelements(); j++) select<<", "<<itsDDIds(j);
    select<<"]";
  }
  //os <<"Selection string: "<<select.str()<<LogIO::POST;
  //os <<" nrow="<<ms_p->nrow()<<LogIO::POST;
  MeasurementSet selectedMS(tableCommand(select,*ms_p));
  MSSelector msSel(selectedMS);
  //os <<" nrow="<<msSel.nrow()<<LogIO::POST;
  MSColumns msc(selectedMS);
  if (itsDDIds.nelements()>1) {
    cout<<"Processing "<<itsDDIds.nelements()<<" spectral windows"<<endl;
  }
  for (uInt iDD=0; iDD<itsDDIds.nelements(); iDD++) {
    Vector<Int> ddIDs(1,itsDDIds(iDD));
    msSel.initSelection(ddIDs);
    //os <<" nrow="<<msSel.nrow()<<LogIO::POST;
    if (msSel.nrow()==0) continue;
    Int nChan=msc.spectralWindow()
                  .numChan()(msc.dataDescription().spectralWindowId()(ddIDs(0)));
    Vector<Int> corrTypes=msc.polarization().
          corrType()(msc.dataDescription().polarizationId()(ddIDs(0)));
    // default to all channels
    Vector<Bool> chanMask(nChan,True);

    // Handle non-trivial channel selection:
    if (itsChannels.nelements()>0) {
      if (allLT(itsChannels,nChan)) {
	chanMask=False;
	for (uInt j=0; j<itsChannels.nelements(); j++) {
	  chanMask(itsChannels(j))=True;
	}
      }   
      else {
	throw(AipsError("Bad channel selection."));
      }
    }
    // select parallel hand polarizations
    Vector<String> polSel(corrTypes.nelements()); 
    Int nPol = 0;
    for (uInt j=0; j<corrTypes.nelements(); j++) {
      if (corrTypes(j)==Stokes::XX||corrTypes(j)==Stokes::YY||
          corrTypes(j)==Stokes::RR||corrTypes(j)==Stokes::LL) {
        polSel(nPol++)=Stokes::name(Stokes::type(corrTypes(j)));
      }
    }
    polSel.resize(nPol,True);
    msSel.selectPolarization(polSel);
     
    msSel.iterInit(
        stringToVector("ARRAY_ID,DATA_DESC_ID,SCAN_NUMBER,FIELD_ID,TIME"),
        itsSolInt,0,False);
    msSel.iterOrigin();
    Int nIter=1;
    while (msSel.iterNext()) nIter++;
    cout<<"Processing "<<nIter<<" slots."<<endl<<" Slot ";
    msSel.iterOrigin();
    Int iter=1;
    do {
      cout<<" "<<iter++;cout.flush();
      Record avRec = msSel.getData(stringToVector("corrected_data"),True,0,1,True);
      Record dataRec = msSel.getData(stringToVector("model_data,corrected_data"),
                                     True,0,1);
      Array<Complex> avCorData(avRec.asArrayComplex("corrected_data"));
      Array<Complex> modelData(dataRec.asArrayComplex("model_data"));
      Array<Complex> correctedData(dataRec.asArrayComplex("corrected_data"));
      Int nTime=modelData.shape()[3];
      Int nIfr=modelData.shape()[2];

      // fit
      Vector<Float> x(nChan);
      for (Int i=0; i<nChan; i++) x(i)=i;
      LinearFit<Float> fitter;
      Polynomial<AutoDiff<Float> > apoly(itsOrder);
      fitter.setFunction(apoly);
      Polynomial<Float> poly(itsOrder);
      
      Vector<Float> y1(nChan),y2(nChan);
      Vector<Float> sol(itsOrder+1);
      Vector<Complex> tmp(nChan);
      IPosition start(3,0,0,0),end(3,0,nChan-1,0);
      for (; start[2]<nIfr; start[2]++,end[2]++) {
        for (start[0]=end[0]=0; start[0]<nPol; start[0]++,end[0]++) {
          Vector<Complex> c(avCorData(start,end).nonDegenerate());
          tmp=c; // copy into contiguous storage
          real(y1,tmp);
          imag(y2,tmp);
          sol = fitter.fit(x,y1,&chanMask);
          poly.setCoefficients(sol);
          y1=x;
          y1.apply(poly);
          sol = fitter.fit(x,y2,&chanMask);
          poly.setCoefficients(sol);
          y2=x;
          y2.apply(poly);
          for (Int chn=0; chn<nChan; chn++) c(chn)=Complex(y1(chn),y2(chn));
        }
      }
        
      IPosition start4(4,0,0,0,0),end4(4,nPol-1,nChan-1,nIfr-1,0);
      for (Int iTime=0; iTime<nTime; iTime++,start4[3]++,end4[3]++) {
        if  (itsMode!="replace") {
          Array<Complex> model(modelData(start4,end4).nonDegenerate(3));
          model=avCorData;
        }
        if (itsMode!="model") {
          Array<Complex> corr(correctedData(start4,end4).nonDegenerate(3));
          if (itsMode=="replace") corr=avCorData;
          if (itsMode=="subtract") corr-=avCorData;
        }
      }  
                    
      Record newDataRec;
      if (itsMode=="model"||itsMode=="subtract") {
        newDataRec.define("model_data",modelData);
      }
      if (itsMode=="replace"||itsMode=="subtract") {
        newDataRec.define("corrected_data",correctedData);
      }
      msSel.putData(newDataRec);
      
    } while (msSel.iterNext());
    cout<<endl;
    
  }
}

} //# NAMESPACE CASA - END

