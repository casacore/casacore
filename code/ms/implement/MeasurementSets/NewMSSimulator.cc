//# NewMSSimulator.cc:  this defines MSSimulator, which simulates a MeasurementSet
//# Copyright (C) 1995,1996,1998,1999,2000,2001,2002,2003
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

//# Includes
#include <trial/MeasurementSets/NewMSSimulator.h>
#include <trial/MeasurementSets/MSDerivedValues.h>
#include <aips/MeasurementSets/MeasurementSet.h>
#include <aips/MeasurementSets/MSColumns.h>
#include <aips/Logging/LogIO.h>
#include <aips/Containers/Record.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/StManAipsIO.h>
#include <aips/Tables/StandardStMan.h>
#include <aips/Tables/IncrementalStMan.h>
#include <aips/Tables/TiledColumnStMan.h>
#include <aips/Tables/TiledShapeStMan.h>
#include <aips/Tables/TiledDataStMan.h>
#include <aips/Tables/TiledDataStManAccessor.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/Random.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/MatrixMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/Slice.h>
#include <aips/Measures/Stokes.h>
#include <aips/Measures/MeasFrame.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MCPosition.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Measures/MeasData.h>
#include <aips/Measures.h>
#include <aips/Utilities/CountedPtr.h>
#include <aips/Utilities/Assert.h>
#include <aips/Arrays/ArrayUtil.h>
#include <aips/iostream.h>
#include <aips/fstream.h>
#include <aips/sstream.h>

uInt MSSimFeedRec::write(MSFeed& msf, MSFeedColumns &msfc, 
			 uInt row, uInt nant) const 
{
    Int a = (antId_p < 0) ? 0 : antId_p;
    Int na = (antId_p < 0) ? nant : a+1;

    Matrix<Double> defboff(2, nrecp_p, 0);
    Matrix<Complex> defpolresp(nrecp_p, nrecp_p, Complex(0.0, 0.0));
    defpolresp.diagonal() = Complex(1.0, 0.0);
    MPosition defpos;
    Vector<Double> defang(nrecp_p, 0.0);

    Vector<String> ptype(pols_p.length());
    uInt p;
    String::const_iterator c;
    for(p=0, c=pols_p.begin(); 
	p < ptype.nelements(); 
	p++, ++c) 
    {
	ptype(p) = *c;
    }

    msf.addRow(na-a);

    for (; a < na; a++, row++) {
	msfc.antennaId().put(row, a);
	msfc.feedId().put(row, feedId_p);
	msfc.spectralWindowId().put(row, spwId_p);
	msfc.time().put(row, time_p);
	msfc.interval().put(row, intv_p);
	msfc.numReceptors().put(row, nrecp_p);
	msfc.polarizationType().put(row, ptype);
	msfc.beamId().put(row, beamId_p);
	msfc.beamOffset().put(row, (beamOff_p) ? *beamOff_p : defboff);
	msfc.polResponse().put(row, (polResp_p) ? *polResp_p : defpolresp);
	msfc.positionMeas().put(row, (position_p) ? *position_p : defpos);
	msfc.receptorAngle().put(row, (angle_p) ? *angle_p : defang);
    }

    return na;
}

//#########################################

uInt MSSimFeedBuf::write(MSFeed &msf, uInt nants) const {
    MSFeedColumns msfc(msf);
    uInt row = msf.nrow();

    uInt newrows = countRows(nants);
    for(uInt i=0; i < recs_p.nelements(); i++) 
	row += recs_p[i]->write(msf, msfc, row, nants);
    return newrows;
}


//#########################################

NewMSSimulator::NewMSSimulator() :
  fractionBlockageLimit_p(1.0e-6),
  elevationLimit_p(Quantity(10.0, "deg")),
  autoCorrelationWt_p(0.0)
{}

NewMSSimulator::NewMSSimulator(const NewMSSimulator & mss)
{
  operator=(mss);
}


void NewMSSimulator::initAnt(const String& telescope,
			  const Vector<Double>& x, 
			  const Vector<Double>& y, 
			  const Vector<Double>& z,
			  const Vector<Float>& dishDiameter,
			  const Vector<String>& mount,
			  const Vector<String>& name,
			  const String& coordsystem,
			  const MPosition& mRefLocation) 
{
  LogIO os(LogOrigin("MSSimulator", "initAnt()", WHERE));

  telescope_p = telescope;
  nAnt_p = x.nelements();
  refPosition_p = mRefLocation;
  os << "Configuration Reference position = " 
     << mRefLocation.getAngle("deg").getValue("deg")
     <<  LogIO::POST;

  antId_p.resize(nAnt_p); antXYZ_p.resize(3,nAnt_p); antDiam_p.resize(nAnt_p); 
  mountType_p.resize(nAnt_p); antName_p.resize(nAnt_p); 

  Vector<Double> xx( x.nelements() );
  Vector<Double> yy( x.nelements() );
  Vector<Double> zz( x.nelements() );
  if (coordsystem == "global") {
    xx = x;  yy = y;  zz = z;    
  } else if (coordsystem == "local") {
    local2global( xx, yy, zz, mRefLocation, x, y, z);
  } else if (coordsystem == "longlat") {
    longlat2global( xx, yy, zz, mRefLocation, x, y, z);
  } else {
    os <<  LogIO::SEVERE << "Unknown coordinate system type: " << coordsystem << LogIO::POST;
  }

  for (Int i=0; i<nAnt_p; i++) {
    antXYZ_p(0,i)=xx(i);
    antXYZ_p(1,i)=yy(i);
    antXYZ_p(2,i)=zz(i);
    antId_p(i)=i;
    mountType_p(i)=mount(i);
    antName_p(i)=name(i);
    antDiam_p(i)=dishDiameter(i);
    
    //    os << " Antenna:"<<i<<" x:"<<antXYZ_p(0,i)<<" y:"<<antXYZ_p(1,i)
    // <<" z:"<<antXYZ_p(2,i)<<LogIO::POST;
  }
}


void NewMSSimulator::local2global(Vector<Double>& xGeo,
			       Vector<Double>& yGeo,
			       Vector<Double>& zGeo,
			       const MPosition& mRefLocation,
			       const Vector<Double>& xLocal,
			       const Vector<Double>& yLocal,
			       const Vector<Double>& zLocal) 
{
  uInt nn = xLocal.nelements();
  xGeo.resize(nn);
  yGeo.resize(nn);
  zGeo.resize(nn);

  MPosition::Convert loc2(mRefLocation, MPosition::ITRF);
  MPosition locitrf(loc2());
  Vector<Double> xyz = locitrf.get("m").getValue();

  Vector<Double> ang = locitrf.getAngle("rad").getValue();
  Double d1, d2;
  d1 = ang(0);
  d2 = ang(1);
  Double cosLong = cos(d1);
  Double sinLong = sin(d1);
  Double cosLat = cos(d2);
  Double sinLat = sin(d2);

  for (uInt i=0; i< nn; i++) {

    Double xG1 = -sinLat * yLocal(i) + cosLat * zLocal(i);
    Double yG1 = xLocal(i);

    xGeo(i) =   cosLong * xG1 - sinLong * yG1  + xyz(0);
    yGeo(i) =   sinLong * xG1 + cosLong * yG1  + xyz(1);

    zGeo(i) = cosLat * yLocal(i)  + sinLat * zLocal(i)  + xyz(2);
  }

};

void NewMSSimulator::longlat2global(Vector<Double>& xReturned,
				 Vector<Double>& yReturned,
				 Vector<Double>& zReturned,
				 const MPosition& mRefLocation,
				 const Vector<Double>& xIn,
				 const Vector<Double>& yIn,
				 const Vector<Double>& zIn)
{
  LogIO os(LogOrigin("MSSimulator", "longlat2global()", WHERE));
  os <<  LogIO::SEVERE << "MSSimulator::longlat2global not yet implemented" << LogIO::POST;
};


void NewMSSimulator::initFields(const uInt nSources,
			     const Vector<String>& sourceName, 
			     const Vector<MDirection>& sourceDirection,
			     const Vector<Int>& intsPerPointing,
			     const Vector<Int>& mosPointingsX,
			     const Vector<Int>& mosPointingsY,
			     const Vector<Float>& mosSpacing)           
{
  // NOTE:  this does not yet correctly deal with different EPOCHs in the direction
  LogIO os(LogOrigin("MSsimulator", "initFields()", WHERE));

  nSources_p = nSources;

  radec_p.resize(2,nSources_p); 
  mosSpacing_p.resize(nSources_p);  
  srcName_p.resize(nSources_p); 
  nIntFld_p.resize(nSources_p);  
  nMos_p.resize(2,nSources_p);

  radecRefFrame_p = MDirection::showType(sourceDirection(0).
					 getRefPtr()->getType());

  String refFrame_check;
  for (Int i=0; i< nSources_p; i++) {
    refFrame_check =  MDirection::showType(sourceDirection(i).myType()) ;
    if ( refFrame_check != radecRefFrame_p ) {
      os << "Inconsistent source position reference frames, assuming all are " 
	 << radecRefFrame_p << LogIO::POST;
    }

    srcName_p(i) = sourceName(i);
    radec_p(0,i) = sourceDirection(i).getAngle().getValue("rad")(0);
    radec_p(1,i) = sourceDirection(i).getAngle().getValue("rad")(1);
    nMos_p(0,i)  = mosPointingsX(i);
    nMos_p(1,i)  = mosPointingsY(i);
    nIntFld_p(i) = intsPerPointing(i);
    mosSpacing_p(i) = mosSpacing(i);
    os << "Source "<<i<<": name:"<<srcName_p(i)<<", ra:"<<radec_p(0,i)<<
    ", dec:"<<radec_p(1,i)<<", #integrations: "<<nIntFld_p(i)<<LogIO::POST;
  }
};

void NewMSSimulator::setTimes(const MEpoch& mStartTime, 
			   const Quantity& qDuration, 
			   const Quantity& qIntegrationTime, 
			   const Quantity& qGapTime) 
{
//    qIntegrationTime_p = qIntegrationTime;
//    qGapTime_p = qGapTime;
//    qStartTime_p = qStartTime;
//    qStopTime_p = qStopTime;
//    mRefTime_p =  mRefTime; 
//    useHourAngles_p = useHourAngles;

//    Tstart_p = qStartTime.getValue("s");
    Tdur_p = qDuration.getValue("s");
    Tgap_p = qGapTime.getValue("s");
    Tint_p = qIntegrationTime.getValue("s");

    // convert reference time to UTC
    MEpoch::Ref tref(MEpoch::UTC);
    MEpoch::Convert tconvert(mStartTime, tref);
    mStartTime_p = tconvert();
//    Tstart_p = tconvert().get("s").getValue("s");
}


void NewMSSimulator::initSpWindows(const uInt nSpWindows,
				const Vector<String>& spWindowName,
				const Vector<Int>& nChan,
				const Vector<Quantity>& startFreq,
				const Vector<Quantity>& freqInc,
				const Vector<Quantity>& freqRes,
				const Vector<String>& stokesString)
{

  LogIO os(LogOrigin("MSsimulator", "initSpWindows()", WHERE)); 

  nSpWindows_p = nSpWindows;

  spWindowName_p.resize(nSpWindows_p);
  nChan_p.resize(nSpWindows_p); 
  startFreq_p.resize(nSpWindows_p); 
  freqInc_p.resize(nSpWindows_p); 
  freqRes_p.resize(nSpWindows_p); 
  nIntSpW_p.resize(nSpWindows_p); 
  nCorr_p.resize(nSpWindows_p);
  stokesTypes_p.resize(4,nSpWindows_p);
  stokesTypes_p.set( Stokes::Undefined );

  for (Int i=0; i<nSpWindows_p; i++) {
        spWindowName_p(i) = spWindowName(i);
	nChan_p(i) = nChan(i); 
	//  os << "#chan="<< nChan_p(i);
	startFreq_p(i) = startFreq(i).getValue("Hz"); 
	//  os << ", startFreq="<< startFreq_p(i);
	freqInc_p(i) = freqInc(i).getValue("Hz"); 
	//  os << ", freqInc="<< freqInc_p(i);
	freqRes_p(i) = freqRes(i).getValue("Hz"); 
	//  os << ", freqRes="<< freqRes_p(i);
	nIntSpW_p(i) = 1;  // temp: integrations per spectral window

	String myStokesString = stokesString(i);
	for (Int j=0; j<4; j++) {	  
	  while (myStokesString.at(0,1) == " ") {
	    myStokesString.del(0,1);
	  }

	  if (myStokesString.length() == 0) 
	    break;

	  stokesTypes_p(j,i) = Stokes::type( myStokesString.at(0, 2) );
	  myStokesString.del(0,2);
	  nCorr_p(i) = j+1; 
	  if (stokesTypes_p(j,i)==Stokes::Undefined) {
	    os<< " Undefined polarization type in input"<<LogIO::POST;
	  }
	}
	// os << LogIO::POST;
    }
}


// NOTE:  initAnt and initSpWindows must be called before this one!
void NewMSSimulator::setFeed(uInt feedId, const String& pols, 
			  const MPosition *position,
			  const Vector<Double> *receptorAngle,
			  const Matrix<Double> *beamOffset, 
			  const Matrix<Complex> *polResponse,
			  Int antId, Double time, Double interval,
			  Int spwId)
{
    LogIO os(LogOrigin("MSSimulator", "setFeed()", WHERE));

    if (nAnt_p <= 0) {
	os <<  LogIO::SEVERE 
	   << "MSSimulator::initFeeds: must call initAnt() first" 
	   << LogIO::POST;
    }
    if (nSpWindows_p <= 0) {
	os <<  LogIO::SEVERE
	   << "MSSimulator::initFeeds: must call initSpWindows() first" 
	   << LogIO::POST;
    }

    if (antId >= nAnt_p) 
	throw AipsError(String("Antenna ID out of range of 0:") + (nAnt_p-1) + 
			": " + antId);

    checkpols(pols);
    MSSimFeedRec &rec = feeds_p.addRec(pols.length(), pols);
    rec.setKey(feedId, antId, spwId, time, interval);
    if (position) rec.setPosition(*position);
    if (receptorAngle) rec.receptorAngle() = *receptorAngle;
    if (beamOffset) rec.beamOffset() = *beamOffset;
    if (polResponse) rec.polResponse() = *polResponse;
}

NewMSSimulator::~NewMSSimulator() 
{
}


NewMSSimulator & NewMSSimulator::operator=(const NewMSSimulator & other) 
{
    if (this==&other) return *this;
    // copy state...
    return *this;
}

void NewMSSimulator::writeMS(const String& name)
{
    // make MS with standard columns
    TableDesc td(MS::requiredTableDesc());
    // add data column
    MS::addColumnToDesc(td,MS::DATA,2);
    if (nSpWindows_p==1) {
      // all data has same shape, make columns direct
      td.removeColumn(MS::columnName(MS::DATA));
      MS::addColumnToDesc(td, MS::DATA, IPosition(2,nCorr_p(0),nChan_p(0)), 
			  ColumnDesc::Direct);
      td.removeColumn(MS::columnName(MS::FLAG));
      MS::addColumnToDesc(td, MS::FLAG, IPosition(2,nCorr_p(0),nChan_p(0)), 
			  ColumnDesc::Direct);
      td.removeColumn(MS::columnName(MS::SIGMA));
      MS::addColumnToDesc(td, MS::SIGMA, IPosition(1,nCorr_p(0)), 
			  ColumnDesc::Direct);
    } 
    // define tiled hypercube for the data (may add flag etc later)
    Vector<String> coordColNames(0); //# don't use coord columns
    td.addColumn(ScalarColumnDesc<Int>("DATA_HYPERCUBE_ID",
				       "Index for Data Tiling"));
    td.defineHypercolumn("TiledData",3,
			 stringToVector(MS::columnName(MS::DATA)+","+
					MS::columnName(MS::FLAG)),
			 coordColNames,
			 stringToVector("DATA_HYPERCUBE_ID")
			 );
    td.defineHypercolumn("TiledUVW",2,
			 stringToVector(MS::columnName(MS::UVW)));

    
    SetupNewTable newtab(name, td, Table::New);

    // Set the default Storage Manager to be the Incr one
    IncrementalStMan incrStMan ("ISMData");
    newtab.bindAll(incrStMan, True);
    StManAipsIO aipsStMan;
    TiledDataStMan tiledStMan("TiledData");
    // Bind the DATA column to the tiled stman
    newtab.bindColumn(MS::columnName(MS::DATA),tiledStMan);
    newtab.bindColumn(MS::columnName(MS::FLAG),tiledStMan);
    newtab.bindColumn("DATA_HYPERCUBE_ID",tiledStMan);
    // Tile UVW access to avoid loading it all into memory at once
    TiledColumnStMan tiledStManUVW("TiledUVW",IPosition(2,3,1024));
    newtab.bindColumn(MS::columnName(MS::UVW),tiledStManUVW);
    
    // Change some to be aips
    newtab.bindColumn(MS::columnName(MS::ANTENNA2),aipsStMan);

    MeasurementSet simulMS(newtab,0);

//      // create a non uniform FEED table, if necessary
//      if (!feeds_p.uniform()) {
//  	LogIO os(LogOrigin("MSSimulator"));
//  	os << LogIO::WARN << "Multiple, non-uniform feeds detected"
//  	   << LogIO::POST;

//  	TableDesc td(MSFeed::requiredTableDesc());
//  	td.defineHypercolumn("TiledBeamOffset", 2, 
//  		stringToVector(MSFeed::columnName(MSFeed::BEAM_OFFSET)));
//  	td.defineHypercolumn("TiledPolType", 2, 
//  		stringToVector(MSFeed::columnName(MSFeed::POLARIZATION_TYPE)));
//  	td.defineHypercolumn("TiledPolResp", 3, 
//  		stringToVector(MSFeed::columnName(MSFeed::POL_RESPONSE)));
//  	td.defineHypercolumn("TiledRecpAngle", 2, 
//  		stringToVector(MSFeed::columnName(MSFeed::RECEPTOR_ANGLE)));

//  	SetupNewTable feedSetup(simulMS.feedTableName(),
//  				MSFeed::requiredTableDesc(), Table::New);
//  	TiledShapeStMan tsmbo("TiledBeamOffset", 
//  			      IPosition(3,2,2,nAnt_p));
//  	TiledShapeStMan tsmpt("TiledPolType", IPosition(2,2,nAnt_p));
//  	TiledShapeStMan tsmpr("TiledPolResp", IPosition(3,2,2,nAnt_p));
//  	TiledShapeStMan tsmra("TiledRecpAngle", IPosition(2,2,nAnt_p));

//  	feedSetup.bindColumn(MSFeed::columnName(MSFeed::BEAM_OFFSET), tsmbo);
//  	feedSetup.bindColumn(MSFeed::columnName(MSFeed::POLARIZATION_TYPE), 
//  			     tsmpt);
//  	feedSetup.bindColumn(MSFeed::columnName(MSFeed::POL_RESPONSE), tsmpr);
//  	feedSetup.bindColumn(MSFeed::columnName(MSFeed::RECEPTOR_ANGLE), 
//  			     tsmra);
			     
//  	simulMS.rwKeywordSet().defineTable(MS::keywordName(MS::FEED), 
//  					   Table(feedSetup));
//      }

//    createSubtables(simulMS);
    simulMS.createDefaultSubtables(Table::New);

    // fill all 'coordinate' columns
    fillCoords(simulMS);
}

/* commented out till new TiledStMan is available which doesn't need
   the extendHypercube call (in fillcoords)
void MSSimulator::writeMS(MeasurementSet& ms)
{
    if (ms.nrow()>0) {
	throw(AipsError("MSSimulator::writeMS(ms) - ms must be empty"));
    }
    if (!ms.keywordSet().isDefined("ANTENNA")) {
	// assume there are no subtables yet
	ms.createDummySubtables(Table::New);
    }
    fillCoords(ms);
}
*/

void NewMSSimulator::fillCoords(MeasurementSet & ms)
{
    LogIO os(LogOrigin("MSSimulator", "fillCoords()", WHERE));
    MSDerivedValues msd;
    Vector<MPosition> vpos(1);
    vpos(0) =  refPosition_p;
    msd.setAntennaPositions(vpos);


    MSColumns msc(ms);
    msc.setEpochRef(MEpoch::UTC, False);
    Double Tstart = mStartTime_p.get("s").getValue("s");
    Double Tend = Tstart + Tdur_p;

    //    os<< " calculating Coordinates ..."<<LogIO::POST;
    
    // fill Observation Table
    MSObservation& obs=ms.observation();

    //    // add the position column because the fits writer expects it.
    //    TableDesc td;
    //    MSArray::addColumnToDesc(td,MSArray::POSITION,IPosition(1,3),
    //			     ColumnDesc::Direct);
    //    StManAipsIO stman;
    //    arr.addColumn(td,stman);

    MSObservationColumns& obsc=msc.observation();
    obs.addRow();
    obsc.telescopeName().put(0,telescope_p);
    //    Vector<Double> arrpos(3); arrpos=0.0;
    //    arrc.position().put(0,arrpos);

    // fill Antenna table
    MSAntenna& ant=ms.antenna();
    MSAntennaColumns& antc=msc.antenna();
    ant.addRow(nAnt_p); // make nAnt_p rows
    antc.dishDiameter().putColumn(antDiam_p);
    antc.mount().putColumn(mountType_p);
    antc.type().fillColumn("GROUND-BASED");
    antc.name().putColumn(antName_p);
    Vector<Double> offsets(3); offsets=0.; 
    antc.offset().fillColumn(offsets);
    antc.position().putColumn(antXYZ_p);
    antc.station().fillColumn("");
    antc.flagRow().fillColumn(False);
    
    // fill Feed table
    feeds_p.write(ms.feed(), nAnt_p);

    // fill spectralWindow table
    ms.spectralWindow().addRow(nSpWindows_p);
    ms.polarization().addRow(nSpWindows_p);
    ms.dataDescription().addRow(nSpWindows_p);
    MSSpWindowColumns& spwc=msc.spectralWindow();
    MSDataDescColumns& ddc=msc.dataDescription();
    MSPolarizationColumns& polc=msc.polarization();
    spwc.numChan().putColumn(nChan_p);
    spwc.name().fillColumn("");
    spwc.netSideband().fillColumn(1);
    spwc.ifConvChain().fillColumn(0);
    spwc.freqGroup().fillColumn(0);
    spwc.freqGroupName().fillColumn("Group 1");
    spwc.flagRow().fillColumn(False);
    spwc.measFreqRef().fillColumn(MFrequency::TOPO);
    polc.flagRow().fillColumn(False);
    ddc.flagRow().fillColumn(False);
    polc.numCorr().putColumn(nCorr_p);
    Vector <Double> freqs, freqRes;
    Vector<Int> StokesTypes; 
    for (Int i=0; i< nSpWindows_p; i++) {
        ddc.spectralWindowId().put(i,i);
        ddc.polarizationId().put(i,i);
	freqs.resize(nChan_p(i));
	freqRes.resize(nChan_p(i));
	freqRes=freqRes_p(i);
	for (Int chan=0; chan<nChan_p(i); chan++) 
	    freqs(chan)=startFreq_p(i)+chan*freqInc_p(i);
	StokesTypes.resize(nCorr_p(i));
	StokesTypes=stokesTypes_p.column(i)(Slice(0,nCorr_p(i)));
	// translate stokesTypes into receptor products, catch invalId
	// fallibles.
	Matrix<Int> corrProduct(uInt(2),uInt(nCorr_p(i)));
	Fallible<Int> fi;
	for (Int j=0; j< nCorr_p(i); j++) {
	    fi=Stokes::receptor1(Stokes::type(StokesTypes(j)));
	    corrProduct(0,j)=(fi.isValid() ? fi.value() : 0);
	    fi=Stokes::receptor2(Stokes::type(StokesTypes(j)));
	    corrProduct(1,j)=(fi.isValid() ? fi.value() : 0);
	}
	spwc.refFrequency().put(i,startFreq_p(i));
	spwc.chanFreq().put(i,freqs);
	spwc.chanWidth().put(i,freqRes);
	spwc.effectiveBW().put(i,freqRes);
	spwc.resolution().put(i,freqRes);
	spwc.totalBandwidth().put(i,nChan_p(i)*freqInc_p(i));
	polc.corrType().put(i,StokesTypes);
	polc.corrProduct().put(i,corrProduct);
    }

    // Now that we know the spectral windows, we can add the appropriate
    // hypercubes to the table.
    TiledDataStManAccessor accessor(ms,"TiledData");
    for (Int i=0; i<nSpWindows_p; i++) {
	Record values;
	values.define("DATA_HYPERCUBE_ID",i);
	// choose a tile size in the channel direction that is <=10
	// and doesn't waste too much storage.
	Int tileSize=(nChan_p(i)+nChan_p(i)/10)/(nChan_p(i)/10+1);
	// make the tile about 32k big
	accessor.addHypercube(IPosition(3,nCorr_p(i),nChan_p(i),0),
			      IPosition(3,nCorr_p(i),tileSize,
					4000/nCorr_p(i)/tileSize),
			      values);
    }


    MSFieldColumns& fieldc=msc.field();
    Int nField=0;
    for (Int i=0; i<nSources_p; i++) nField+=nMos_p(0,i)*nMos_p(1,i);
    ms.field().addRow(nField);
    fieldc.code().fillColumn("");
    fieldc.time().fillColumn(Tstart);
    fieldc.numPoly().fillColumn(0);
    Vector<MDirection> direction(1);

    Int row=0;
    for (Int i=0; i<nSources_p; i++) {
	Double lambda = C::c / startFreq_p(0);
	Double spacing = lambda / (2.0 * antDiam_p(0));
	if (mosSpacing_p(i) > 0.0) {
	  spacing *= mosSpacing_p(i);
	}
	for (Int j=0; j<nMos_p(0,i); j++) {
	    for (Int k=0; k<nMos_p(1,i); k++) {
	      if (radecRefFrame_p == "J2000") {
		direction(0)=MDirection
		  (MVDirection(radec_p(0,i)+(j-nMos_p(0,i)/2)*
			       spacing / cos(radec_p(1,i)),
			       radec_p(1,i)+(k-nMos_p(1,i)/2)*spacing),
		   MDirection::J2000);
	      } else {
		direction(0)=MDirection
		  (MVDirection(radec_p(0,i)+(j-nMos_p(0,i)/2)*
			       spacing / cos(radec_p(1,i)),
			       radec_p(1,i)+(k-nMos_p(1,i)/2)*spacing),
		   MDirection::B1950);
	      }
	      fieldc.sourceId().put(row,i);
	      fieldc.delayDirMeasCol().put(row,direction);
	      fieldc.phaseDirMeasCol().put(row,direction);
	      fieldc.referenceDirMeasCol().put(row,direction);
	      ostringstream name;
	      if (nMos_p(0,i)*nMos_p(1,i)>1) {
		name << flush <<srcName_p(i) <<"_"<<j<<"_"<<k<<ends;
	      } else {
		name << flush <<srcName_p(i) << ends;
	      }
	      fieldc.name().put(row,String(name));
	      // os << pName << LogIO::POST;
	      row++;
	    }
	}
    }
    
    // init counters past end
    Int FldId = nField-1;
    Int nSrc = ms.field().nrow();
    Int SrcId = nSrc-1;
    Int FldCount = nIntFld_p(SrcId);
    Int SpWId = nIntSpW_p.nelements()-1;
    Int SpWCount = nIntSpW_p(SpWId);
    Int loopCount= max(sum(nIntSpW_p),sum(nIntFld_p));
    Int counter=0;
    Int scan=0;
    // os <<" Tstart= "<<Tstart<<", Tend="<<Tend<<LogIO::POST;
    row=-1;
    Double Time=Tstart;
    Bool firstTime = True;

    // set the flag catagories
    Vector<String> flgCat(3);
    flgCat(0)="FLAG_CMD";
    flgCat(1)="SHADOWING";
    flgCat(2)="ELEVATION_LIMIT";
    msc.flagCategory().rwKeywordSet().define("CATEGORY",flgCat);

    uInt nShadowed = 0;
    uInt nSubElevation = 0;

    for (Int itime=0; Time<Tend; itime++) {

        MEpoch epUT1 (Quantity(Time/C::day, "d"), MEpoch::UT1);
	MEpoch::Ref refGMST1(MEpoch::GMST1);
	MEpoch::Convert epGMST1(epUT1, refGMST1);
	Double gmst = epGMST1().get("d").getValue("d");
	gmst = (gmst - Int(gmst)) * C::_2pi;  // Into Radians

	Double ra, dec; // current phase center

	// update counters and field/freq info
	if (++FldCount >= nIntFld_p(SrcId)) {
	    FldCount = 0;
	    if (++FldId >= nField) FldId = 0;
	    SrcId = msc.field().sourceId()(FldId);
	}
	if (++SpWCount >= nIntSpW_p(SpWId)) {
	    SpWCount = 0;
	    if (uInt(++SpWId) >= nIntSpW_p.nelements()) SpWId = 0;
	}
	if (++counter > loopCount) {
	  // insert gap
	  Time+=Tgap_p;
	  counter=0;
	}

	MDirection fc = msc.field().phaseDirMeas(FldId);
	ra = fc.getAngle().getValue()(0);
	dec = fc.getAngle().getValue()(1);
	Record values;
	values.define("DATA_HYPERCUBE_ID",SpWId);
        Bool firstBaseline = True;
	Vector<Double> uvwvec(3);
	Matrix<Complex> data(nCorr_p(SpWId),nChan_p(SpWId)); 
	
//	Matrix<Bool> flag(nCorr_p(SpWId),nChan_p(SpWId)); 
	Cube<Bool> flag(nCorr_p(SpWId),nChan_p(SpWId),3);
	flag=False;
	// random number generator
	//	MLCG rndGen(1234567);
	//	Normal normal(0.0, 1.0, &rndGen);


	Vector<Bool> isShadowed(nAnt_p);  isShadowed.set(False);
	Vector<Bool> isTooLow(nAnt_p);    isTooLow.set(False);
	Double fractionBlocked1=0.0, fractionBlocked2=0.0;
	Int startingRow = row;
	Double diamMax2 = square( max(antDiam_p) );

        Matrix<Double> firstRot(Rot3D(2,(gmst-ra +0.25*C::_2pi)));
	Matrix<Double> secondRot(Rot3D(0,C::pi_2-dec));

        // We can extend the ms and the hypercube by all baselines
	Int nBase;
        if(autoCorrelationWt_p > 0.0) {
	  nBase =nAnt_p*(nAnt_p+1)/2;
	}
	else {
	  nBase =nAnt_p*(nAnt_p-1)/2;
	}
	ms.addRow(nBase);
	accessor.extendHypercube(nBase,values);
	for (Int ant1=0; ant1<nAnt_p; ant1++) {
	    Double x1=antXYZ_p(0,ant1), y1=antXYZ_p(1,ant1), z1=antXYZ_p(2,ant1);
	    for (Int ant2=ant1; ant2<nAnt_p; ant2++) {
	        if ( (ant1 != ant2) ||  autoCorrelationWt_p > 0.0) {
		  row++; 
		  //		  ms.addRow();
		  //		  accessor.extendHypercube(1,values);
		  if (firstBaseline) {
                    msc.scanNumber().put(row,scan++);
		    msc.fieldId().put(row,FldId);
		    msc.dataDescId().put(row,SpWId);
		    msc.time().put(row,Time+Tint_p/2);
		    msc.timeCentroid().put(row,Time+Tint_p/2);
                    firstBaseline=False;
		  }
		  msc.antenna1().put(row,ant1);
		  msc.antenna2().put(row,ant2);
		  // this is probably wrong...
		  Double x2=antXYZ_p(0,ant2), y2=antXYZ_p(1,ant2), z2=antXYZ_p(2,ant2);
		  uvwvec(0) = x2-x1;
		  uvwvec(1) = y2-y1;
		  uvwvec(2) = z2-z1;
		  uvwvec=product(firstRot,uvwvec);
		  uvwvec=product(secondRot,uvwvec);

		  if (ant1 != ant2) {
		    blockage(fractionBlocked1, fractionBlocked2,
			     uvwvec, antDiam_p(ant1), antDiam_p(ant2) );
		    if (fractionBlocked1 > fractionBlockageLimit_p) {
		      isShadowed(ant1) = True;
		    }
		    if (fractionBlocked2 > fractionBlockageLimit_p) {
		      isShadowed(ant2) = True;
		    }
		  }
		  
		  msc.uvw().put(row,uvwvec);
		  
		  data=Complex(0.,0.);
		  
		  msc.data().put(row,data);
		  msc.flagCategory().put(row,flag);
		  msc.flag().put(row,flag.xyPlane(0));
		  msc.flagRow().put(row,False);
		  
		  // Deal with differing diameter case
		  Float sigma1 = diamMax2/(antDiam_p(ant1) * antDiam_p(ant2));
		  Float wt = 1/square(sigma1);
		  if  (ant1 == ant2 ) {
		    wt *= autoCorrelationWt_p;
		  }
		  Vector<Float> tmp(nCorr_p(SpWId)); tmp=wt;
		  msc.weight().put(row, tmp);
		  tmp=sigma1;
		  msc.sigma().put(row,tmp);
		  if (row==0) {
		    // we're using the incr stMan so we only need to 
		    // put these once
		    msc.arrayId().put(0,0);
		    msc.processorId().put(0,0);
		    msc.exposure().put(0,Tint_p);
		    msc.feed1().put(row,0);
		    msc.feed2().put(row,0);
		    msc.interval().put(0,Tint_p);
		    msc.observationId().put(0,0);
		    msc.stateId().put(0,-1);
		  }
		}
	    }
	}

	// go back and flag weights based on shadowing
	// Future option: we could increase sigma based on
	// fraction shadowed.
	Cube<Bool> trueFlag(nCorr_p(SpWId),nChan_p(SpWId),3); 
//	Matrix<Bool> trueFlag(nCorr_p(SpWId),nChan_p(SpWId)); 
	trueFlag=False;
	trueFlag.xyPlane(1) = True;
	    
	Int reRow = startingRow;
	for (Int ant1=0; ant1<nAnt_p; ant1++) {
	  for (Int ant2=ant1; ant2<nAnt_p; ant2++) {
	    if ( (ant1 != ant2) ||  autoCorrelationWt_p > 0.0) {
	      reRow++; 
	      if ( isShadowed(ant1) || isShadowed(ant2) ) {
		msc.flagCategory().put(reRow,trueFlag);
		msc.flag().put(reRow,trueFlag.xyPlane(1));
		msc.flagRow().put(reRow, True);
		nShadowed++;
	      }
	    }
	  }
	}

	// This will not work for VLBI! 
	msd.setAntennaPositions(vpos);
	  
	MEpoch ep(Quantity((Time + Tint_p/2), "s"));
	msd.setEpoch(ep);
	      
	msd.setFieldCenter( fc );
	      
	Double elevation = msd.azel().getAngle("rad").getValue("rad")(1);
	
	// go back and flag weights based on elevationLimit_p
	trueFlag=False;
	trueFlag.xyPlane(2)=True;
	for (Int ant1=0; ant1<nAnt_p; ant1++) {
	  
	  if (elevation < elevationLimit_p.getValue("rad")) {
	    isTooLow(ant1) = True;
	  }
	  if (firstTime) {
	    firstTime = False;
	    Double az1 = msd.azel().getAngle("deg").getValue("deg")(0);
	    Double el1 = msd.azel().getAngle("deg").getValue("deg")(1);
	    Double ha1 = msd.hourAngle() *  180.0/C::pi / 15.0;
	    Double timeDays = Time / C::day;
	    os << "Starting conditions: " << LogIO::POST;
	    os << "     az = " << az1 << LogIO::POST;
	    os << "     el = " << el1 << LogIO::POST;
	    os << "     ha = " << ha1 << LogIO::POST;
	    os << "     time = " << timeDays << LogIO::POST;
	  }
	}
	reRow = startingRow;
	for (Int ant1=0; ant1<nAnt_p; ant1++) {
	  for (Int ant2=ant1; ant2<nAnt_p; ant2++) {
	    if ( (ant1 != ant2) ||  autoCorrelationWt_p > 0.0) {
	      reRow++; 
	      if ( isTooLow(ant1) || isTooLow(ant2) ) {
		msc.flagCategory().put(reRow,trueFlag);
		msc.flag().put(reRow,trueFlag.xyPlane(2));
		msc.flagRow().put(reRow, True);
		nSubElevation++;
	      }
	    }
	  }
	}

	Time+=Tint_p; 
    }

    Double az1 = msd.azel().getAngle("deg").getValue("deg")(0);
    Double el1 = msd.azel().getAngle("deg").getValue("deg")(1);
    Double ha1 = msd.hourAngle()  *  180.0/C::pi / 15.0 ;
    Double timeDays = Time / C::day;
    os << "Stopping conditions: " << LogIO::POST;
    os << "     az = " << az1 << LogIO::POST;
    os << "     el = " << el1 << LogIO::POST;
    os << "     ha = " << ha1 << LogIO::POST;
    os << "     time = " << timeDays << LogIO::POST;


    os << (row+1) << " visibilities simulated " << LogIO::POST;
    os << nShadowed << " visibilities flagged due to shadowing " << LogIO::POST;
    os << nSubElevation << " visibilities flagged due to elevation limit of " << 
      elevationLimit_p.getValue("deg") << " degrees " << LogIO::POST;

};



// Calculates the fractional blockage of one antenna by another
// We will want to put this somewhere else eventually, but I don't yet know where!
// Till then.
// Stolen from Fred Schwab
void NewMSSimulator::blockage(Double &fraction1, Double &fraction2,
			   const Vector<Double>& uvw, 
			   const Double diam1, 
			   const Double diam2)
{
  Double separation = sqrt( square(uvw(0)) + square(uvw(1)) );
  Double rmin = 0.5 * min(abs(diam1),abs(diam2));
  Double rmax = 0.5 * max(abs(diam1),abs(diam2));
  if (separation >= (rmin+rmax)) {
    fraction1 = 0.0;
    fraction2 = 0.0;
  } else if ( (separation+rmin) <= rmax) {
    fraction1 = min(1.0, square(abs(diam2)/abs(diam1)));
    fraction2 = min(1.0, square(abs(diam1)/abs(diam2)));
  } else {
    Double c = separation/(0.5 * abs(diam1));
    Double s=abs(diam2)/abs(diam1);        
    Double sinb=sqrt(2.0 * (square(c*s)+square(c)+square(s))-pow(c,4.0)-pow(s,4.0)-1.0)
      /(2.0 * c);
    Double sina=sinb/s;
    //  Due to roundoff, sina or sinb might be ever so slightly larger than 1
    //  in the case of unequal radii, with the center of one antenna pattern
    //  inside the other:
    sinb=min(1.0, sinb);
    sina=min(1.0, sina);
    
    Double b=asin(sinb);
    Double a=asin(sina);
    Double area=(square(s)*a+b)-(square(s)*sina*cos(a)+sinb*cos(b));
    fraction1 = area/C::pi;
    fraction2 = fraction1/square(s);
  }
  // if antenna1 is in behind, w is > 0, 2 is NOT shadowed
  if (uvw(2) > 0.0) fraction2 = 0.0;
  // if antenna1 is in front, w is < 0, 1 is NOT shadowed
  if (uvw(2) < 0.0) fraction1 = 0.0;

  return;
};

/*
void MSSimulator::createSubtables(MeasurementSet &ms) {
    TableRecord &kws = ms.rwKeywordSet();
    Table::TableOption option = Table::New;

    SetupNewTable antennaSetup(ms.antennaTableName(),
			       MSAntenna::requiredTableDesc(),option);
    kws.defineTable(MS::keywordName(MS::ANTENNA), Table(antennaSetup));
		    
    SetupNewTable dataDescSetup(ms.dataDescriptionTableName(),
				MSDataDescription::requiredTableDesc(),option);
    kws.defineTable(MS::keywordName(MS::DATA_DESCRIPTION), 
		    Table(dataDescSetup));

    if (!kws.isDefined(MS::keywordName(MS::FEED))) {
	SetupNewTable feedSetup(ms.feedTableName(),
				MSFeed::requiredTableDesc(),option);
	kws.defineTable(MS::keywordName(MS::FEED), 
				   Table(feedSetup));
    }

    SetupNewTable flagCmdSetup(ms.flagCmdTableName(),
			       MSFlagCmd::requiredTableDesc(),option);
    kws.defineTable(MS::keywordName(MS::FLAG_CMD), Table(flagCmdSetup));
		    
    SetupNewTable fieldSetup(ms.fieldTableName(),
			     MSField::requiredTableDesc(),option);
    kws.defineTable(MS::keywordName(MS::FIELD), Table(fieldSetup));

    SetupNewTable historySetup(ms.historyTableName(),
			       MSHistory::requiredTableDesc(),option);
    kws.defineTable(MS::keywordName(MS::HISTORY), Table(historySetup));
		    
    SetupNewTable observationSetup(ms.observationTableName(),
				   MSObservation::requiredTableDesc(),option);
    kws.defineTable(MS::keywordName(MS::OBSERVATION), 
		    Table(observationSetup));

    // Pointing table can be large, set some sensible defaults for storageMgrs
    SetupNewTable pointingSetup(ms.pointingTableName(),
			       MSPointing::requiredTableDesc(),option);
    IncrementalStMan ismPointing ("ISMPointing");
    StandardStMan ssmPointing("SSMPointing",32768);
    pointingSetup.bindAll(ismPointing,True);
    pointingSetup.bindColumn(MSPointing::columnName(MSPointing::ANTENNA_ID),
			     ssmPointing);
    kws.defineTable(MS::keywordName(MS::POINTING), Table(pointingSetup));
		    
    SetupNewTable polarizationSetup(ms.polarizationTableName(),
			       MSPolarization::requiredTableDesc(),option);
    kws.defineTable(MS::keywordName(MS::POLARIZATION),
		    Table(polarizationSetup));

    SetupNewTable processorSetup(ms.processorTableName(),
			       MSProcessor::requiredTableDesc(),option);
    kws.defineTable(MS::keywordName(MS::PROCESSOR), Table(processorSetup));
		    
    SetupNewTable spectralWindowSetup(ms.spectralWindowTableName(),
			       MSSpectralWindow::requiredTableDesc(),option);
    kws.defineTable(MS::keywordName(MS::SPECTRAL_WINDOW),  
		    Table(spectralWindowSetup));

    SetupNewTable stateSetup(ms.stateTableName(),
			       MSState::requiredTableDesc(),option);
    kws.defineTable(MS::keywordName(MS::STATE), Table(stateSetup));
		    
    ms.initRefs();
}
*/
