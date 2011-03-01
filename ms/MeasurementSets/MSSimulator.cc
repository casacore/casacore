//# MSSimulator.cc:  this defines MSSimulator, which simulates a MeasurementSet
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
#include <ms/MeasurementSets/MSSimulator.h>
#include <ms/MeasurementSets/MSDerivedValues.h>
#include <ms/MeasurementSets/MeasurementSet.h>
#include <ms/MeasurementSets/MSColumns.h>
#include <casa/Logging/LogIO.h>
#include <casa/Containers/Record.h>
#include <tables/Tables/SetupNewTab.h>
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/ArrayColumn.h>
#include <tables/Tables/TableDesc.h>
#include <tables/Tables/ScaColDesc.h>
#include <tables/Tables/TableRecord.h>
#include <tables/Tables/StManAipsIO.h>
#include <tables/Tables/IncrementalStMan.h>
#include <tables/Tables/StandardStMan.h>
#include <tables/Tables/TiledColumnStMan.h>
#include <tables/Tables/TiledShapeStMan.h>
#include <tables/Tables/TiledDataStMan.h>
#include <tables/Tables/TiledStManAccessor.h>
#include <tables/Tables/TiledDataStManAccessor.h>
#include <casa/BasicSL/Constants.h>
#include <casa/BasicMath/Random.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/MatrixMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/Arrays/Slice.h>
#include <measures/Measures/Stokes.h>
#include <measures/Measures/MeasFrame.h>
#include <measures/Measures/MEpoch.h>
#include <measures/Measures/MCPosition.h>
#include <measures/Measures/MPosition.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MeasConvert.h>
#include <measures/Measures/MeasData.h>
#include <measures/Measures.h>
#include <casa/Utilities/CountedPtr.h>
#include <casa/Utilities/Assert.h>
#include <casa/Arrays/ArrayUtil.h>
#include <casa/Arrays/Slicer.h>
#include <casa/iostream.h>
#include <casa/fstream.h>
#include <casa/sstream.h>
#include <ms/MeasurementSets/MSTileLayout.h>


namespace casa { //# NAMESPACE CASA - BEGIN

MSSimulator::MSSimulator() :
  fractionBlockageLimit_p(1.0e-6),
  elevationLimit_p(Quantity(10.0, "deg")),
  autoCorrelationWt_p(0.0)
{}

MSSimulator::MSSimulator(const MSSimulator & mss)
{
  operator=(mss);
}


void MSSimulator::initAnt(const String& telescope,
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


void MSSimulator::local2global(Vector<Double>& xGeo,
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

}

void MSSimulator::longlat2global(Vector<Double>&,
				 Vector<Double>&,
				 Vector<Double>&,
				 const MPosition&,
				 const Vector<Double>&,
				 const Vector<Double>&,
				 const Vector<Double>&)
{
  LogIO os(LogOrigin("MSSimulator", "longlat2global()", WHERE));
  os <<  LogIO::SEVERE << "MSSimulator::longlat2global not yet implemented" << LogIO::POST;
}


void MSSimulator::initFields(const uInt nSources,
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
  radecRefFrame_p = sourceDirection(0).getRefString();
 
  String refFrame_check;
  for (Int i=0; i< nSources_p; i++) {
    refFrame_check =  sourceDirection(i).getRefString();
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
}

void MSSimulator::setTimes(const Quantity& qIntegrationTime, 
			   const Quantity& qGapTime, 
			   const Bool      useHourAngles,
			   const Quantity& qStartTime, 
			   const Quantity& qStopTime, 
			   const MEpoch&   mRefTime) 
{
  qIntegrationTime_p = qIntegrationTime;
  qGapTime_p = qGapTime;
  qStartTime_p = qStartTime;
  qStopTime_p = qStopTime;
  mRefTime_p =  mRefTime; 
  useHourAngles_p = useHourAngles;

}


void MSSimulator::initSpWindows(const uInt nSpWindows,
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
// This method is currently brain dead, we will have to revive it
// at a later date -- we can ONLY make perfect R L or X Y feeds
void MSSimulator::initFeeds(const String& mode)
{
  LogIO os(LogOrigin("MSsimulator", "initFeeds()", WHERE));

  if (nAnt_p <= 0) {
    os <<  LogIO::SEVERE << "MSSimulator::initFeeds: must call initAnt() first" << LogIO::POST;
  }
  if (nSpWindows_p <= 0) {
    os <<  LogIO::SEVERE << "MSSimulator::initFeeds: must call initSpWindows() first" << LogIO::POST;
  }

  Int antId = -1;  // ie, apply this info to ALL antennas
  // use same feed parameters for all antennas
  nFeed_p=nSpWindows_p*nAnt_p;

  // mode == "perfect R L" OR "perfect X Y"
  String feedPol0="R", feedPol1="L";
  if (mode.contains("X", 0)) {
    feedPol0 = "X";
    feedPol1 = "Y";
  }    

  feedAntId_p.resize(nFeed_p); feedId_p.resize(nFeed_p); 
  feedSpWId_p.resize(nFeed_p); feedBeamId_p.resize(nFeed_p); 
  feedNumRec_p.resize(nFeed_p); beamOffset_p.resize(2,2,nFeed_p); 
  feedPol_p.resize(2,nFeed_p); feedXYZ_p.resize(3,nFeed_p); 
  feedAngle_p.resize(2,nFeed_p); polResp_p.resize(2,2,nFeed_p);
  feedAntId_p(0)=antId;

  for(Int i=0; i < nSpWindows_p ; i++){
    feedAntId_p(i)=0;
    feedId_p(i) = 0;
    feedSpWId_p(i) = -1;
    feedBeamId_p(i) = 0;
    feedNumRec_p(i) = 2;
    for (Int j=0; j<feedNumRec_p(i); j++) {
      beamOffset_p(0,j,i) = 0.0;
      beamOffset_p(1,j,i) = 0.0;
    }
  
    feedPol_p(0,i) = feedPol0;
    feedPol_p(1,i) = feedPol1;
    
    feedXYZ_p(0,i) = 0.0;
    feedXYZ_p(1,i) = 0.0;
    feedXYZ_p(2,i) = 0.0;
    
    feedAngle_p(0,i) = 0.0;
    feedAngle_p(1,i) = 0.0;

    //# polResp_p(0,0,i)=Complex(axr,axi);
    //# polResp_p(1,0,i)=Complex(ayr,ayi);
    //# polResp_p(0,1,i)=Complex(bxr,bxi);
    //# polResp_p(1,1,i)=Complex(byr,byi);
    polResp_p.xyPlane(i)=Complex(0.0,0.0); //# skip this for now
    polResp_p(0,0,i)=polResp_p(1,1,i)=Complex(1.0,0.0);
  }
  
  // copy entries for first antenna to all others (except for spWindow)
  for (Int i=1; i<nAnt_p; i++) {
    for (Int j=0; j<nSpWindows_p; j++) {
      Int n=i*nSpWindows_p+j;
      feedAntId_p(n)=i;
      feedId_p(n)=feedId_p(j);
      feedSpWId_p(n)= -1;              //  HEY!  Watch out for this when we get real
      feedBeamId_p(n)=feedBeamId_p(j);
      feedNumRec_p(n)=feedNumRec_p(j);
      beamOffset_p.xyPlane(n)=beamOffset_p.xyPlane(j);
      feedPol_p.column(n)=feedPol_p.column(j);
      feedXYZ_p.column(n)=feedXYZ_p.column(j);
      feedAngle_p.column(n)=feedAngle_p.column(j);
      polResp_p.xyPlane(n)=polResp_p.xyPlane(j);
    }
  }
}


MSSimulator::~MSSimulator() 
{
}


MSSimulator & MSSimulator::operator=(const MSSimulator & other) 
{
    if (this==&other) return *this;
    // copy state...
    return *this;
}

void MSSimulator::writeMS(const String& name)
{
    // make MS with standard columns
    TableDesc td(MS::requiredTableDesc());
    // add data column
    MS::addColumnToDesc(td,MS::DATA,2);

    td.defineHypercolumn("TiledData",3,
			 stringToVector(MS::columnName(MS::DATA)));
    td.defineHypercolumn("TiledFlag",3,
 			 stringToVector(MS::columnName(MS::FLAG)));
    td.defineHypercolumn("TiledFlagCategory",4,
 			 stringToVector(MS::columnName(MS::FLAG_CATEGORY)));
    td.defineHypercolumn("TiledUVW",2,
 			 stringToVector(MS::columnName(MS::UVW)));
    td.defineHypercolumn("TiledWgt",2,
			 stringToVector(MS::columnName(MS::WEIGHT)));
    td.defineHypercolumn("TiledSigma", 2,
			 stringToVector(MS::columnName(MS::SIGMA)));
    
    SetupNewTable newtab(name, td, Table::New);

    // Set the default Storage Manager to be the Incr one
    IncrementalStMan incrStMan ("ISMData");
    newtab.bindAll(incrStMan, True);

    // Bind ANTENNA1, ANTENNA2 and DATA_DESC_ID to the standardStMan 
    // as they may change sufficiently frequently to make the
    // incremental storage manager inefficient for these columns.
    
    StandardStMan aipsStMan(32768);
    newtab.bindColumn(MS::columnName(MS::ANTENNA1), aipsStMan);
    newtab.bindColumn(MS::columnName(MS::ANTENNA2), aipsStMan);
    newtab.bindColumn(MS::columnName(MS::DATA_DESC_ID), aipsStMan);
    
    IPosition dataShape(2,nCorr_p(0),nChan_p(0));
    Int obsType(MSTileLayout::Standard);
    // Use FastMosaic for big mosaics
    if((nMos_p(0,0)*nMos_p(1,0))>10) obsType = MSTileLayout::FastMosaic;
    Int nBase;
    if(autoCorrelationWt_p > 0.0) {
      nBase =nAnt_p*(nAnt_p+1)/2;
    }
    else {
      nBase =nAnt_p*(nAnt_p-1)/2;
    }
    Int nInt(nIntFld_p(0));
    IPosition tileShape = MSTileLayout::tileShape(dataShape,obsType,
						  nBase, nInt);

    TiledShapeStMan tiledStMan1("TiledData",tileShape);
    TiledShapeStMan tiledStMan1f("TiledFlag",tileShape);
    TiledShapeStMan tiledStMan1fc("TiledFlagCategory",
				  IPosition(4,tileShape(0),tileShape(1),1,
 					   tileShape(2)));
    TiledColumnStMan tiledStMan3("TiledUVW",IPosition(2,3,1024));
    TiledShapeStMan tiledStMan4("TiledWgt", 
				IPosition(2,tileShape(0),tileShape(2)));
    TiledShapeStMan tiledStMan5("TiledSigma", 
				IPosition(2,tileShape(0),tileShape(2)));

    // Bind the DATA, FLAG & WEIGHT_SPECTRUM columns to the tiled stman
    newtab.bindColumn(MS::columnName(MS::DATA),tiledStMan1);
    newtab.bindColumn(MS::columnName(MS::FLAG),tiledStMan1f);
    newtab.bindColumn(MS::columnName(MS::FLAG_CATEGORY),tiledStMan1fc);
    newtab.bindColumn(MS::columnName(MS::UVW),tiledStMan3);
    newtab.bindColumn(MS::columnName(MS::WEIGHT),tiledStMan4);
    newtab.bindColumn(MS::columnName(MS::SIGMA),tiledStMan5);

    MeasurementSet simulMS(newtab,0);
    simulMS.createDefaultSubtables(Table::New);

    { // Set the TableInfo
      TableInfo& info(simulMS.tableInfo());
      info.setType(TableInfo::type(TableInfo::MEASUREMENTSET));
      info.setSubType(String("simulator"));
      info.readmeAddLine
	("This is a measurement set Table holding simulated astronomical observations");
    }
    
    // fill all 'coordinate' columns
    extendMS(simulMS);
}


void MSSimulator::extendMS(MeasurementSet & ms)
{
    LogIO os(LogOrigin("MSSimulator", "extendMS()", WHERE));
    const double forever=1.e30;
    MSDerivedValues msd;
    Vector<MPosition> vpos(1);
    vpos(0) =  refPosition_p;
    msd.setAntennaPositions(vpos);

    
    {
      Tint_p = qIntegrationTime_p.getValue("s");
      Tgap_p = qGapTime_p.getValue("s");
      Double t_offset = 0.0;   // This shifts the time forward by less than a day
                               // until the Tstart_p represents the staring Hour Angle
      if (useHourAngles_p) {
	msd.setEpoch( mRefTime_p );
	Quantity d0(radec_p(0,0), "rad");
	Quantity d1(radec_p(1,0), "rad");
	MDirection fc(d0, d1, MDirection::Ref(MDirection::B1950) );
	msd.setFieldCenter( fc );
	t_offset = - msd.hourAngle() * 3600.0 * 180.0/C::pi / 15.0; // in seconds
      }

      MEpoch::Ref tref(MEpoch::TAI);
      MEpoch::Convert tconvert(mRefTime_p, tref);
      MEpoch taiRefTime = tconvert();      

      Tstart_p = qStartTime_p.getValue("s") + 
	taiRefTime.get("s").getValue("s") + t_offset;
      Tend_p = qStopTime_p.getValue("s") + 
	taiRefTime.get("s").getValue("s") + t_offset;
    }


    //    os<< " calculating Coordinates ..."<<LogIO::POST;
    
    // fill Observation Table
    MSObservation& obs=ms.observation();

    //    // add the position column because the fits writer expects it.
    //    TableDesc td;
    //    MSArray::addColumnToDesc(td,MSArray::POSITION,IPosition(1,3),
    //			     ColumnDesc::Direct);
    //    StManAipsIO stman;
    //    arr.addColumn(td,stman);

    MSColumns msc(ms);
    MSObservationColumns& obsc=msc.observation();
    Int nobsrow= obsc.nrow();
    obs.addRow();
    obsc.telescopeName().put(nobsrow,telescope_p);
    //    Vector<Double> arrpos(3); arrpos=0.0;
    //    arrc.position().put(0,arrpos);

    // fill Antenna table
    MSAntenna& ant=ms.antenna();
    MSAntennaColumns& antc=msc.antenna();
    Int numOfAnt=antc.nrow();

    ant.addRow(nAnt_p); // make nAnt_p rows
    Slicer antSlice(IPosition(1,numOfAnt),IPosition(1, numOfAnt+nAnt_p-1),
		    IPosition(1,1), Slicer::endIsLast );
    antc.dishDiameter().putColumnRange(antSlice,antDiam_p);
    antc.mount().putColumnRange(antSlice, mountType_p);
    antc.type().fillColumn("GROUND-BASED");
    antc.name().putColumnRange(antSlice,antName_p);
    Vector<Double> offsets(3); offsets=0.; 
    antc.offset().fillColumn(offsets);
    antc.position().putColumnRange(antSlice, antXYZ_p);
    antc.station().fillColumn("");
    antc.flagRow().fillColumn(False);
    
    

    // fill Feed table
    MSFeedColumns& feedc=msc.feed();
    Int numFeeds=feedc.nrow();
    Slicer feedSlice(IPosition(1,numFeeds),IPosition(1, nFeed_p+numFeeds-1),
		     IPosition(1,1), Slicer::endIsLast);
    ms.feed().addRow(nFeed_p);
    feedc.antennaId().putColumnRange(feedSlice,feedAntId_p);
    feedc.feedId().putColumnRange(feedSlice,feedId_p);
    feedc.spectralWindowId().putColumnRange(feedSlice,feedSpWId_p);
    //    feedc.time().fillColumn(Tstart_p);
    //    feedc.interval().fillColumn(forever); //no time dependence
    feedc.beamId().putColumnRange(feedSlice,feedBeamId_p);
    feedc.numReceptors().putColumnRange(feedSlice, feedNumRec_p);
    feedc.position().putColumnRange(feedSlice, feedXYZ_p);
    for (Int i=numFeeds; i<(nFeed_p+numFeeds); i++) {
	feedc.beamOffset().put(i,beamOffset_p.xyPlane(i-numFeeds));
	feedc.polarizationType().put(i,feedPol_p.column(i-numFeeds));
	feedc.polResponse().put(i,polResp_p.xyPlane(i-numFeeds));
	feedc.receptorAngle().put(i,feedAngle_p.column(i-numFeeds));
	feedc.time().put(i, Tstart_p);
	feedc.interval().put(i, forever);
    }

    // fill spectralWindow table
    MSSpWindowColumns& spwc=msc.spectralWindow();
    Int nSpwid=spwc.nrow();
    Slicer spwSlice(IPosition(1,nSpwid),IPosition(1, nSpwid+nSpWindows_p-1),
		    IPosition(1,1), Slicer::endIsLast);
    
    MSDataDescColumns& ddc=msc.dataDescription();
    MSPolarizationColumns& polc=msc.polarization();
    ms.spectralWindow().addRow(nSpWindows_p);
    ms.polarization().addRow(nSpWindows_p);
    ms.dataDescription().addRow(nSpWindows_p);
    spwc.numChan().putColumnRange(spwSlice,nChan_p);
    spwc.name().fillColumn("");
    spwc.netSideband().fillColumn(1);
    spwc.ifConvChain().fillColumn(0);
    spwc.freqGroup().fillColumn(0);
    spwc.freqGroupName().fillColumn("Group 1");
    spwc.flagRow().fillColumn(False);
    spwc.measFreqRef().fillColumn(MFrequency::TOPO);
    polc.flagRow().fillColumn(False);
    ddc.flagRow().fillColumn(False);
    polc.numCorr().putColumnRange(spwSlice, nCorr_p);
    Vector <Double> freqs, freqRes;
    Vector<Int> StokesTypes; 
    for (Int i=nSpwid; i< (nSpwid+nSpWindows_p); i++) {
        ddc.spectralWindowId().put(i,i-nSpwid);
        ddc.polarizationId().put(i,i-nSpwid);
	freqs.resize(nChan_p(i-nSpwid));
	freqRes.resize(nChan_p(i-nSpwid));
	freqRes=freqRes_p(i-nSpwid);
	for (Int chan=0; chan<nChan_p(i-nSpwid); chan++) 
	    freqs(chan)=startFreq_p(i-nSpwid)+chan*freqInc_p(i-nSpwid);
	StokesTypes.resize(nCorr_p(i-nSpwid));
	StokesTypes=stokesTypes_p.column(i-nSpwid)(Slice(0,nCorr_p(i-nSpwid)));
	// translate stokesTypes into receptor products, catch invalId
	// fallibles.
	Matrix<Int> corrProduct(uInt(2),uInt(nCorr_p(i-nSpwid)));
	Fallible<Int> fi;
	for (Int j=0; j< nCorr_p(i-nSpwid); j++) {
	    fi=Stokes::receptor1(Stokes::type(StokesTypes(j)));
	    corrProduct(0,j)=(fi.isValid() ? fi.value() : 0);
	    fi=Stokes::receptor2(Stokes::type(StokesTypes(j)));
	    corrProduct(1,j)=(fi.isValid() ? fi.value() : 0);
	}
	spwc.refFrequency().put(i,startFreq_p(i-nSpwid));
	spwc.chanFreq().put(i,freqs);
	spwc.chanWidth().put(i,freqRes);
	spwc.effectiveBW().put(i,freqRes);
	spwc.resolution().put(i,freqRes);
	spwc.totalBandwidth().put(i,nChan_p(i-nSpwid)*freqInc_p(i-nSpwid));
	polc.corrType().put(i,StokesTypes);
	polc.corrProduct().put(i,corrProduct);
    }


    MSFieldColumns& fieldc=msc.field();
    Int numField=fieldc.nrow();
    MSPointingColumns& pointingc=msc.pointing();
    Int numPointing=pointingc.nrow();
    Int nField=0; Int newFieldRows;
    for (Int i=0; i<nSources_p; i++) nField+=nMos_p(0,i)*nMos_p(1,i);
    if(nAnt_p == 1){
      ms.field().addRow(nSources_p); //SINGLE DISH CASE
      newFieldRows=nSources_p;
    }
    else{
      ms.field().addRow(nField); //INTERFEROMETER CASE
      newFieldRows=nField;
    }
    Double numofpointingcycle=(Tend_p-Tstart_p)/((Tgap_p+Tint_p)*nField);
    Int numpointrows=Int(numofpointingcycle*nField);
    Double remainderpointings=(Tend_p-Tstart_p-
			       numpointrows*(Tgap_p+Tint_p))/(Tgap_p+Tint_p);
    numpointrows=numpointrows+Int(remainderpointings);
    if((remainderpointings-Int(remainderpointings))*(Tgap_p+Tint_p)/Tint_p >=1.0)
      numpointrows += 1;
    numpointrows=numpointrows*nAnt_p;
    ms.pointing().addRow(numpointrows);
    numpointrows += numPointing;
    fieldc.code().fillColumn("");
    for (Int m=numField; m < numField+newFieldRows; m++){
      fieldc.time().put(m, Tstart_p);
      fieldc.numPoly().put(m, 0);
    }
    for (Int m=numPointing; m < (numPointing+nField*nAnt_p); m++){
      pointingc.time().put(m,Tstart_p);
      pointingc.timeOrigin().put(m,Tstart_p);
      pointingc.numPoly().put(m,0);
      pointingc.interval().put(m,-1);
      pointingc.tracking().put(m,True);
    }
    Vector<MDirection> direction(1);
    Int row=numField;
    Int pointrow=numPointing;
    Double pointtime=Tstart_p;
    while(pointtime < Tend_p){
      row=numField;
      for (Int i=0; i<nSources_p; i++) {
	if(nAnt_p==1) row=numField+i;
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
	    if ((nMos_p(0,i)*nMos_p(1,i)>1) && (nAnt_p > 1)) {
	      name << flush <<srcName_p(i) <<"_"<<j<<"_"<<k;
	    } else {
	      name << flush <<srcName_p(i);
	    }
	    fieldc.name().put(row,String(name));
	    Double pinterval=nIntFld_p(i)*qIntegrationTime_p.getValue("s");
	    if(pointrow < numpointrows){
	      for (Int m=0; m < nAnt_p ; m++){
		pointingc.time().put(pointrow, (pointtime+  pinterval/2.0));
		pointingc.interval().put(pointrow, pinterval);
		pointingc.antennaId().put(pointrow, m+numOfAnt);
		pointingc.name().put(pointrow, String(name));
		pointingc.directionMeasCol().put(pointrow,direction);
		pointingc.targetMeasCol().put(pointrow,direction);             
		pointrow++;
	      }
	    }
	    pointtime=pointtime+
	      nIntFld_p(i)*qIntegrationTime_p.getValue("s")
	      +qGapTime_p.getValue("s");
	    // os << pName << LogIO::POST;
       	    if(nAnt_p > 1) row++;
	  }
	}
      }
    }
    
    // init counters past end
    if(nAnt_p ==1)nField=nSources_p;
    Int FldId=nField-1;
    Int nSrc = ms.field().nrow();
    Int SrcId = nSrc-1;
    Int FldCount = nIntFld_p(SrcId);
    Int SpWId = nIntSpW_p.nelements()-1;
    Int oldSpWId=SpWId;
    Int SpWCount = nIntSpW_p(SpWId);
    Int loopCount= max(sum(nIntSpW_p),sum(nIntFld_p));
    Int counter=0;
    Int scan=-1;
    // os <<" Tstart= "<<Tstart_p<<", Tend="<<Tend_p<<LogIO::POST;
    
    row=ms.nrow()-1;
    Vector<Int> tmpids(row+1);
    tmpids=msc.observationId().getColumn();
    Int maxObsId=-1;
    if (tmpids.nelements()>0) maxObsId=max(tmpids);
    tmpids=msc.arrayId().getColumn();
    Int maxArrayId=-1;
    if (tmpids.nelements()>0) maxArrayId=max(tmpids);
    tmpids.resize(0);

    Double Time=Tstart_p;
    Bool firstTime = True;

    uInt nShadowed = 0;
    uInt nSubElevation = 0;

    for (Int itime=0; Time<Tend_p; itime++) {

        MEpoch epUT1 (Quantity(Time/C::day, "d"), MEpoch::UT1);
	MEpoch::Ref refGMST1(MEpoch::GMST1);
	MEpoch::Convert epGMST1(epUT1, refGMST1);
	Double gmst = epGMST1().get("d").getValue("d");
	gmst = (gmst - Int(gmst)) * C::_2pi;  // Into Radians

	Double ra, dec; // current phase center

	// update counters and field/freq info
	if (++FldCount >= nIntFld_p(SrcId)) {
	    FldCount = 0;
	    ++scan;
	    if (++FldId >= nField) FldId = 0;
	    SrcId = msc.field().sourceId()(FldId);
	}
	if (++SpWCount >= nIntSpW_p(SpWId)) {
	    SpWCount = 0;
	    if(oldSpWId != SpWId){
	      ++scan;
	      oldSpWId=SpWId;
	    }
	    if (uInt(++SpWId) >= nIntSpW_p.nelements()) SpWId = 0;
	}
	
	MDirection fc = msc.field().phaseDirMeas(FldId);
	ra = fc.getAngle().getValue()(0);
	dec = fc.getAngle().getValue()(1);


       	Record values;
	values.define("DATA_HYPERCUBE_ID",SpWId+nSpwid);
       
        Bool firstBaseline = True;
	Vector<Double> uvwvec(3);
	Matrix<Complex> data(nCorr_p(SpWId),nChan_p(SpWId)); 
	
	Matrix<Bool> flag(nCorr_p(SpWId),nChan_p(SpWId)); 
	flag=False;
	// random number generator
	//	MLCG rndGen(1234567);
	//	Normal normal(0.0, 1.0, &rndGen);


	Vector<Bool> isShadowed(nAnt_p);  isShadowed.set(False);
	Vector<Bool> isTooLow(nAnt_p);    isTooLow.set(False);
	Double fractionBlocked1=0.0, fractionBlocked2=0.0;
	Int startingRow = row;
	Double diamMax2 = square( max(antDiam_p) );


        // rough transformation from antenna position difference (ant2-ant1) to uvw
        Double H0 = gmst-ra, sH0=sin(H0), cH0=cos(H0), sd=sin(dec), cd=cos(dec);
        Matrix<Double> trans(3,3,0);
        trans(0,0) = -sH0;    trans(0,1) = -cH0;
        trans(1,0) =  sd*cH0; trans(1,1) = -sd*sH0; trans(1,2) = -cd;
        trans(2,0) = -cd*cH0; trans(2,1) = cd*sH0;  trans(2,2) = -sd; 

        // We can extend the ms and the hypercube by all baselines
	Int nBase;
        if(autoCorrelationWt_p > 0.0) {
	  nBase =nAnt_p*(nAnt_p+1)/2;
	}
	else {
	  nBase =nAnt_p*(nAnt_p-1)/2;
	}
	ms.addRow(nBase);
	
	//	accessor.extendHypercube(nBase,values);


	for (Int ant1=0; ant1<nAnt_p; ant1++) {
	    Double x1=antXYZ_p(0,ant1), y1=antXYZ_p(1,ant1), z1=antXYZ_p(2,ant1);
	    for (Int ant2=ant1; ant2<nAnt_p; ant2++) {
	        if ( (ant1 != ant2) ||  autoCorrelationWt_p > 0.0) {
		  row++; 
		  //		  ms.addRow();
		  //		  accessor.extendHypercube(1,values);
		  //		  if (firstBaseline) {
                    msc.scanNumber().put(row,scan);
		    msc.fieldId().put(row,FldId+numField);
		    msc.dataDescId().put(row,SpWId+nSpwid);
		    msc.time().put(row,Time+Tint_p/2);
                    firstBaseline=False;
		    //	  }
		  msc.antenna1().put(row,ant1+numOfAnt);
		  msc.antenna2().put(row,ant2+numOfAnt);
		  // this is probably wrong...
		  Double x2=antXYZ_p(0,ant2), y2=antXYZ_p(1,ant2), z2=antXYZ_p(2,ant2);
		  uvwvec(0) = x2-x1;
		  uvwvec(1) = y2-y1;
		  uvwvec(2) = z2-z1;
		  uvwvec=product(trans,uvwvec);

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
		  
		  data.set(Complex(0.,0.));
		  msc.data().put(row,data);		  
		  msc.flag().put(row,flag);
		  msc.flagRow().put(row,False);

		  if(ms.tableDesc().isColumn("CORRECTED_DATA")){
		    msc.correctedData().setShape(row, data.shape());
		    msc.correctedData().put(row,data);
		    msc.modelData().setShape(row,data.shape());
		    msc.modelData().put(row, data);
		    Vector<Float> dummywgt(nChan_p(SpWId));
		    dummywgt.set(1.0);
		    msc.imagingWeight().setShape(row, data.shape().getLast(1));
		    msc.imagingWeight().put(row, dummywgt);
		  }
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
		  
		  if (row==(startingRow+1)) {
		    // we're using the incr stMan so we only need to 
		    // put these once
		    msc.arrayId().put(row,maxArrayId+1);
		    msc.processorId().put(row,0);
		    msc.exposure().put(row,Tint_p);
		    msc.feed1().put(row,0);
		    msc.feed2().put(row,0);
		    msc.interval().put(row,Tint_p);
		    msc.observationId().put(row,maxObsId+1);
		    msc.stateId().put(row,-1);
	       	  }
		  
		}
	    }
	}

	// go back and flag weights based on shadowing
	// Future option: we could increase sigma based on
	// fraction shadowed.
	Matrix<Bool> trueFlag(nCorr_p(SpWId),nChan_p(SpWId)); 
	trueFlag=True;
	    
	Int reRow = startingRow;
	for (Int ant1=0; ant1<nAnt_p; ant1++) {
	  for (Int ant2=ant1; ant2<nAnt_p; ant2++) {
	    if ( (ant1 != ant2) ||  autoCorrelationWt_p > 0.0) {
	      reRow++; 
	      if ( isShadowed(ant1) || isShadowed(ant2) ) {
		msc.flag().put(reRow,trueFlag);
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
		msc.flag().put(reRow,trueFlag);
		msc.flagRow().put(reRow, True);
		nSubElevation++;
	      }
	    }
	  }
	}

	if (++counter >= loopCount) {
	  // insert gap
	  Time+=Tgap_p;
	  counter=0;
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

}

// Calculates the fractional blockage of one antenna by another
// We will want to put this somewhere else eventually, but I don't yet know where!
// Till then.
// Stolen from Fred Schwab
void MSSimulator::blockage(Double &fraction1, Double &fraction2,
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
}



} //# NAMESPACE CASA - END

