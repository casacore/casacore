//# NewMSSimulator.cc:  this defines NewMSSimulator, which simulates a MeasurementSet
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
#include <aips/Tables/IncrementalStMan.h>
#include <aips/Tables/StandardStMan.h>
#include <aips/Tables/TiledColumnStMan.h>
#include <aips/Tables/TiledShapeStMan.h>
#include <aips/Tables/TiledDataStMan.h>
#include <aips/Tables/TiledStManAccessor.h>
#include <aips/Tables/TiledDataStManAccessor.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/Random.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/MatrixMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/Slice.h>
#include <aips/Measures/Stokes.h>
#include <aips/Measures/MeasFrame.h>
#include <aips/Quanta/MVDirection.h>
#include <aips/Quanta/MVAngle.h>
#include <aips/Quanta/MVTime.h>
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
#include <aips/Arrays/Slicer.h>
#include <aips/iostream.h>
#include <aips/fstream.h>
#include <aips/sstream.h>
#include <trial/MeasurementSets/MSTileLayout.h>

const uInt nCat = 6; // Number of Flag categories

const String sigmaCol = "sigmaHyperColumn";
const String dataCol = "dataHyperColumn";
const String scratchDataCol = "scratchDataHyperColumn";
const String imweightCol = "imWeightHyperColumn";
const String flagCol = "flagHyperColumn";

const String sigmaTileId = "SIGMA_HYPERCUBE_ID";
const String dataTileId = "DATA_HYPERCUBE_ID";
const String scratchDataTileId = "SCRATCH_DATA_HYPERCUBE_ID";
const String flagTileId = "FLAG_CATEGORY_HYPERCUBE_ID";
const String imweightTileId = "IMAGING_WEIGHT_HYPERCUBE_ID";
  
NewMSSimulator::NewMSSimulator(const String& MSName) :
  ms_p(0), dataAcc_p(), scratchDataAcc_p(), sigmaAcc_p(), flagAcc_p(), imweightAcc_p(),
  hyperCubeID_p(-1)
{
  LogIO os(LogOrigin("NewMSSimulator", "NewMSSimulator<const String& MSName)", WHERE));

  // make MS with standard columns
  TableDesc msDesc(MS::requiredTableDesc());

  // Add other columns, including the scratch columns
  MS::addColumnToDesc(msDesc,MS::DATA,2);
  MS::addColumnToDesc(msDesc,MS::MODEL_DATA,2);
  MS::addColumnToDesc(msDesc,MS::CORRECTED_DATA,2);
  MS::addColumnToDesc(msDesc,MS::IMAGING_WEIGHT,1);
  
  // Add index columns for tiling. We use three tiles: data, sigma, and flag.
  // Some of these contain more than one column
  msDesc.addColumn(ScalarColumnDesc<Int>(dataTileId,
					 "Index for Data tiling"));
  msDesc.addColumn(ScalarColumnDesc<Int>(scratchDataTileId,
					 "Index for Scratch Data tiling"));
  msDesc.addColumn(ScalarColumnDesc<Int>(sigmaTileId,
					 "Index for Sigma tiling"));
  msDesc.addColumn(ScalarColumnDesc<Int>(imweightTileId,
  					 "Index for Imaging Weight tiling"));
  msDesc.addColumn(ScalarColumnDesc<Int>(flagTileId,
					 "Index for Flag Category tiling"));

  // setup hypercolumns for the data/flag/flag_catagory/sigma & weight columns.
  {
    Vector<String> dataCols(2);
    dataCols(0) = MeasurementSet::columnName(MeasurementSet::DATA);
    dataCols(1) = MeasurementSet::columnName(MeasurementSet::FLAG);
    const Vector<String> coordCols(0);
    const Vector<String> idCols(1, dataTileId);
    msDesc.defineHypercolumn(dataCol, 3, dataCols, coordCols, idCols);
  }
  {
    Vector<String> dataCols(2);
    dataCols(0) = MeasurementSet::columnName(MeasurementSet::MODEL_DATA);
    dataCols(1) = MeasurementSet::columnName(MeasurementSet::CORRECTED_DATA);
    const Vector<String> coordCols(0);
    const Vector<String> idCols(1, scratchDataTileId);
    msDesc.defineHypercolumn(scratchDataCol, 3, dataCols, coordCols, idCols);
  }
  {
    Vector<String> dataCols(2);
    dataCols(0) = MeasurementSet::columnName(MeasurementSet::SIGMA);
    dataCols(1) = MeasurementSet::columnName(MeasurementSet::WEIGHT);
    const Vector<String> coordCols(0);
    const Vector<String> idCols(1, sigmaTileId);
    msDesc.defineHypercolumn(sigmaCol, 2, dataCols, coordCols, idCols);
  }
  {
    Vector<String> dataCols(1);
    dataCols(0) = MeasurementSet::columnName(MeasurementSet::IMAGING_WEIGHT);
    const Vector<String> coordCols(0);
    const Vector<String> idCols(1, imweightTileId);
    msDesc.defineHypercolumn(imweightCol, 2, dataCols, coordCols, idCols);
  }
  {
    Vector<String> dataCols(1);
    dataCols(0) = MeasurementSet::columnName(MeasurementSet::FLAG_CATEGORY);
    const Vector<String> coordCols(0);
    const Vector<String> idCols(1, flagTileId);
    msDesc.defineHypercolumn(flagCol, 4, dataCols, coordCols, idCols);
  }

  SetupNewTable newMS(MSName, msDesc, Table::New);
  
  // Set the default Storage Manager to be the Incr one
  {
    IncrementalStMan incrStMan ("ismdata");
    newMS.bindAll(incrStMan, True);
  }
  
  // Bind ANTENNA1, ANTENNA2 and DATA_DESC_ID to the standardStMan 
  // as they may change sufficiently frequently to make the
  // incremental storage manager inefficient for these columns.
  
  {
    StandardStMan aipsStMan(32768);
    newMS.bindColumn(MS::columnName(MS::ANTENNA1), aipsStMan);
    newMS.bindColumn(MS::columnName(MS::ANTENNA2), aipsStMan);
    newMS.bindColumn(MS::columnName(MS::DATA_DESC_ID), aipsStMan);
  }
  
  // These columns contain the bulk of the data so save them in a tiled way
  {
    TiledDataStMan dataMan(dataCol);
    newMS.bindColumn(MeasurementSet::
		     columnName(MeasurementSet::DATA), dataMan);
    newMS.bindColumn(MeasurementSet::
		     columnName(MeasurementSet::FLAG), dataMan);
    newMS.bindColumn(dataTileId, dataMan);
  }
  {
    TiledDataStMan dataMan(scratchDataCol);
    newMS.bindColumn(MeasurementSet::
		     columnName(MeasurementSet::MODEL_DATA), dataMan);
    newMS.bindColumn(MeasurementSet::
		     columnName(MeasurementSet::CORRECTED_DATA), dataMan);
    newMS.bindColumn(scratchDataTileId, dataMan);
  }
  {
    TiledDataStMan dataMan(sigmaCol);
    newMS.bindColumn(MeasurementSet::
 		     columnName(MeasurementSet::SIGMA), dataMan);
    newMS.bindColumn(MeasurementSet::
 		     columnName(MeasurementSet::WEIGHT), dataMan);
    newMS.bindColumn(sigmaTileId, dataMan);
  }

  {
    TiledDataStMan dataMan(imweightCol);
    newMS.bindColumn(MeasurementSet::
 		     columnName(MeasurementSet::IMAGING_WEIGHT), dataMan);
    newMS.bindColumn(imweightTileId, dataMan);
  }

  {
    TiledDataStMan dataMan(flagCol);
    newMS.bindColumn(MeasurementSet::
   		     columnName(MeasurementSet::FLAG_CATEGORY), dataMan);
    newMS.bindColumn(flagTileId, dataMan);
  }

  // Now we can create the MeasurementSet and add the (empty) subtables
  ms_p=new MeasurementSet(newMS,0);
  ms_p->createDefaultSubtables(Table::New);
  ms_p->flush();
  
  // Set the TableInfo
  { 
    TableInfo& info(ms_p->tableInfo());
    info.setType(TableInfo::type(TableInfo::MEASUREMENTSET));
    info.setSubType(String("simulator"));
    info.readmeAddLine
      ("This is a MeasurementSet Table holding simulated astronomical observations");
  }

  // Now we can make the accessors to be used when adding hypercolumns
  dataAcc_p = TiledDataStManAccessor(*ms_p, dataCol);
  scratchDataAcc_p = TiledDataStManAccessor(*ms_p, scratchDataCol);
  sigmaAcc_p = TiledDataStManAccessor(*ms_p, sigmaCol);
  flagAcc_p = TiledDataStManAccessor(*ms_p, flagCol);
  imweightAcc_p = TiledDataStManAccessor(*ms_p, imweightCol);

  // We're done - wasn't that easy?
}

// Add new hypercubes as the shape changes
void NewMSSimulator::addHyperCubes(const Int nAnt, const Int nChan, const Int nCorr,
				   const Int obsType)
{
  IPosition dataShape(2,nCorr,nChan);

  uInt nBase;
  if(autoCorrelationWt_p > 0.0) {
    nBase =nAnt*(nAnt+1)/2;
  }
  else {
    nBase =nAnt*(nAnt-1)/2;
  }
  // Tile holds all correlations and channels for all baselines for one time
  IPosition tileShape = MSTileLayout::tileShape(dataShape, obsType, nBase, 1);

  // For the moment, we add a new hypercolumn each time we are called
  Record tileId;
  {
    tileId.define(sigmaTileId, static_cast<Int>(10*hyperCubeID_p));
    uInt rowTiles = nBase*nChan/nCorr;
    if (rowTiles < nBase) rowTiles = nBase;
    sigmaAcc_p.addHypercube(IPosition(2, nCorr, 0), 
			    IPosition(2, nCorr, rowTiles),
			    tileId);
  }
  
  {
    tileId.define(dataTileId, static_cast<Int>(10*hyperCubeID_p+1));
    const uInt chanTiles = (nChan+7)/8;
    uInt rowTiles = nBase*nChan/nCorr/chanTiles;
    if (rowTiles < nBase) rowTiles = nBase;
    dataAcc_p.addHypercube(IPosition(3, nCorr, nChan, 0), 
			   IPosition(3, nCorr, chanTiles, rowTiles),
			   tileId);

    tileId.define(scratchDataTileId, static_cast<Int>(10*hyperCubeID_p+2));
    scratchDataAcc_p.addHypercube(IPosition(3, nCorr, nChan, 0), 
				  IPosition(3, nCorr, chanTiles, rowTiles),
				  tileId);

    tileId.define(flagTileId, static_cast<Int>(10*hyperCubeID_p + 3));
    flagAcc_p.addHypercube(IPosition(4, nCorr, nChan, nCat, 0), 
			   IPosition(4, nCorr, chanTiles, nCat, rowTiles),
			   tileId);

    tileId.define(imweightTileId, static_cast<Int>(10*hyperCubeID_p + 4));
    imweightAcc_p.addHypercube(IPosition(2, nChan, 0), 
			       IPosition(2, chanTiles, rowTiles),
			       tileId);
  }
}

NewMSSimulator::NewMSSimulator(const NewMSSimulator & mss)
{
  operator=(mss);
}


void NewMSSimulator::initAnt(const String& telescope,
			     const Vector<Double>& x, 
			     const Vector<Double>& y, 
			     const Vector<Double>& z,
			     const Vector<Double>& dishDiameter,
			     const Vector<Double>& offset,
			     const Vector<String>& mount,
			     const Vector<String>& name,
			     const String& coordsystem,
			     const MPosition& mRefLocation) 
{
  LogIO os(LogOrigin("NewMSSimulator", "initAnt()", WHERE));
  
  telescope_p=telescope;

  Int nAnt = x.nelements();
  
  Vector<Double> xx( x.nelements() );
  Vector<Double> yy( x.nelements() );
  Vector<Double> zz( x.nelements() );
  if (coordsystem == "global") {
    xx = x;  yy = y;  zz = z;    
    os << "Using global coordinates for the antennas" << LogIO::POST;
  } else if (coordsystem == "local") {

    MVAngle mvLong= mRefLocation.getAngle().getValue()(0);
    MVAngle mvLat= mRefLocation.getAngle().getValue()(1);
    
    os << "Using local coordinates for the antennas" << endl
       << "Reference position = ";
    os.output().width(13); os << mvLong.string(MVAngle::ANGLE,7);
    os.output().width(14);  os << mvLat.string(MVAngle::DIG2,7);
    os <<  LogIO::POST;
    local2global( xx, yy, zz, mRefLocation, x, y, z);
  } else if (coordsystem == "longlat") {
    os << "Using longitude-latitude coordinates for the antennas" << LogIO::POST;
    longlat2global( xx, yy, zz, mRefLocation, x, y, z);
  } else {
    os <<  LogIO::SEVERE << "Unknown coordinate system type: "
       << coordsystem << LogIO::POST;
  }
  
  Vector<Int> antId(nAnt);
  Matrix<Double> antXYZ(3,nAnt);
  
  for (Int i=0; i<nAnt; i++) {
    antXYZ(0,i)=xx(i);
    antXYZ(1,i)=yy(i);
    antXYZ(2,i)=zz(i);
    antId(i)=i;
  }

  MSColumns msc(*ms_p);
  MSAntennaColumns& antc=msc.antenna();
  Int numOfAnt=antc.nrow();
  MSAntenna& ant=ms_p->antenna();
  
  ant.addRow(nAnt); // make nAnt rows
  Slicer antSlice(IPosition(1,numOfAnt),
		  IPosition(1, numOfAnt+nAnt-1),
		  IPosition(1,1), Slicer::endIsLast );
  antc.dishDiameter().putColumnRange(antSlice,dishDiameter);
  antc.mount().putColumnRange(antSlice, mount);
  antc.name().putColumnRange(antSlice,name);
  //  antc.offset().putColumnRange(antSlice,offset);
  antc.position().putColumnRange(antSlice, antXYZ);
  antc.station().fillColumn("");
  antc.flagRow().fillColumn(False);
  antc.type().fillColumn("GROUND-BASED");
  os << "Added rows to ANTENNA table" << LogIO::POST;
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
  LogIO os(LogOrigin("NewMSSimulator", "longlat2global()", WHERE));
  os <<  LogIO::SEVERE << "NewMSSimulator::longlat2global not yet implemented" << LogIO::POST;
};


void NewMSSimulator::initFields(const String& sourceName, 
				const MDirection& sourceDirection,
				const String& calCode)
{
  LogIO os(LogOrigin("MSsimulator", "initFields()", WHERE));
  
  MSColumns msc(*ms_p);
  MSFieldColumns& fieldc=msc.field();
  Int baseFieldID=fieldc.nrow();

  os << "Creating new field " << sourceName << ", ID "
     << baseFieldID+1 << LogIO::POST;

  ms_p->field().addRow(1); //SINGLE DISH CASE
  fieldc.name().put(baseFieldID, sourceName);
  fieldc.code().put(baseFieldID, calCode);
  fieldc.time().put(baseFieldID, 0.0);
  fieldc.numPoly().put(baseFieldID, 0);
  fieldc.sourceId().put(baseFieldID,0);
  Vector<MDirection> direction(1);
  direction(0)=sourceDirection;
  fieldc.delayDirMeasCol().put(baseFieldID,direction);
  fieldc.phaseDirMeasCol().put(baseFieldID,direction);
  fieldc.referenceDirMeasCol().put(baseFieldID,direction);

};

void NewMSSimulator::initSpWindows(const String& spWindowName,
				   const Int& nChan,
				   const Quantity& startFreq,
				   const Quantity& freqInc,
				   const Quantity& freqRes,
				   const String& stokesString)
{
  
  LogIO os(LogOrigin("MSsimulator", "initSpWindows()", WHERE)); 
  
  Vector<Int> stokesTypes(4);
  stokesTypes=Stokes::Undefined;
  String myStokesString = stokesString;
  Int nCorr=0;
  for (Int j=0; j<4; j++) {	  
    while (myStokesString.at(0,1) == " ") {
      myStokesString.del(0,1);
    }
    
    if (myStokesString.length() == 0) 
      break;
    
    stokesTypes(j) = Stokes::type( myStokesString.at(0, 2) );
    myStokesString.del(0,2);
    nCorr = j+1; 
    if (stokesTypes(j)==Stokes::Undefined) {
      os<< " Undefined polarization type in input"<<LogIO::POST;
    }
  }
  MSColumns msc(*ms_p);
  MSSpWindowColumns& spwc=msc.spectralWindow();
  MSDataDescColumns& ddc=msc.dataDescription();
  MSPolarizationColumns& polc=msc.polarization();
  Int baseSpWID=spwc.nrow();
  os << "Creating new spectral window " << spWindowName << ", ID "
     << baseSpWID+1 << LogIO::POST;
  // fill spectralWindow table
  ms_p->spectralWindow().addRow(1);
  ms_p->polarization().addRow(1);
  ms_p->dataDescription().addRow(1);
  spwc.numChan().put(baseSpWID,nChan);
  spwc.name().put(baseSpWID,spWindowName);
  spwc.netSideband().fillColumn(1);
  spwc.ifConvChain().fillColumn(0);
  spwc.freqGroup().fillColumn(0);
  spwc.freqGroupName().fillColumn("Group 1");
  spwc.flagRow().fillColumn(False);
  spwc.measFreqRef().fillColumn(MFrequency::TOPO);
  polc.flagRow().fillColumn(False);
  ddc.flagRow().fillColumn(False);
  polc.numCorr().put(baseSpWID, nCorr);
  Vector <Double> freqs(nChan), bandwidth(nChan);
  bandwidth=freqInc.getValue("Hz");
  ddc.spectralWindowId().put(baseSpWID,baseSpWID);
  ddc.polarizationId().put(baseSpWID,baseSpWID);
  Double vStartFreq(startFreq.getValue("Hz"));
  Double vFreqInc(freqInc.getValue("Hz"));
  for (Int chan=0; chan<nChan; chan++) {
    freqs(chan)=vStartFreq+chan*vFreqInc;
  }

  // translate stokesTypes into receptor products, catch invalid
  // fallibles.
  Matrix<Int> corrProduct(uInt(2),uInt(nCorr));
  Fallible<Int> fi;
  for (Int j=0; j< nCorr; j++) {
    fi=Stokes::receptor1(Stokes::type(stokesTypes(j)));
    corrProduct(0,j)=(fi.isValid() ? fi.value() : 0);
    fi=Stokes::receptor2(Stokes::type(stokesTypes(j)));
    corrProduct(1,j)=(fi.isValid() ? fi.value() : 0);
  }
  spwc.refFrequency().put(baseSpWID,vStartFreq);
  spwc.chanFreq().put(baseSpWID,freqs);
  spwc.chanWidth().put(baseSpWID,bandwidth);
  spwc.effectiveBW().put(baseSpWID,bandwidth);
  spwc.resolution().put(baseSpWID,bandwidth);
  spwc.totalBandwidth().put(baseSpWID,nChan*vFreqInc);
  polc.corrType().put(baseSpWID,stokesTypes);
  polc.corrProduct().put(baseSpWID,corrProduct);

  {
    MSSpWindowColumns msSpW(ms_p->spectralWindow());
    Int nSpw=ms_p->spectralWindow().nrow();
    if(nSpw==0) nSpw=1;
    Matrix<Int> selection(2,nSpw);
    selection.row(0)=0; //start
    selection.row(1)=msSpW.numChan().getColumn(); 
    ArrayColumn<Complex> mcd(*ms_p,"MODEL_DATA");
    mcd.rwKeywordSet().define("CHANNEL_SELECTION",selection);
  }

}


// NOTE:  initAnt and initSpWindows must be called before this one!
// This method is currently brain dead, we will have to revive it
// at a later date -- we can ONLY make perfect R L or X Y feeds
void NewMSSimulator::initFeeds(const String& mode)
{
  LogIO os(LogOrigin("MSsimulator", "initFeeds()", WHERE));
  
  MSColumns msc(*ms_p);
  MSAntennaColumns& antc=msc.antenna();
  Int nAnt=antc.nrow();
  
  if (nAnt <= 0) {
    os <<  LogIO::SEVERE << "NewMSSimulator::initFeeds: must call initAnt() first" << LogIO::POST;
  }
  
  Int antId = -1;  // ie, apply this info to ALL antennas
  // use same feed parameters for all antennas
  Int nFeed=nAnt;
  
  // mode == "perfect R L" OR "perfect X Y"
  String feedPol0="R", feedPol1="L";
  if (mode.contains("X", 0)) {
    feedPol0 = "X";
    feedPol1 = "Y";
  }    
  
  Vector<Int> feedAntId(nFeed);
  Vector<Int> feedId(nFeed); 
  Vector<Int> feedSpWId(nFeed);
  Vector<Int> feedBeamId(nFeed);
  
  Vector<Int> feedNumRec(nFeed);
  Cube<Double> beamOffset(2,2,nFeed);
  
  Matrix<String> feedPol(2,nFeed);
  Matrix<Double> feedXYZ(3,nFeed);
  
  Matrix<Double> feedAngle(2,nFeed);
  Cube<Complex> polResp(2,2,nFeed);
  feedAntId(0)=antId;
  
  feedAntId(0)=0;
  feedId(0) = 0;
  feedSpWId(0) = -1;
  feedBeamId(0) = 0;
  feedNumRec(0) = 2;
  for (Int j=0; j<feedNumRec(0); j++) {
    beamOffset(0,j,0) = 0.0;
    beamOffset(1,j,0) = 0.0;
  }
  
  feedPol(0, 0) = feedPol0;
  feedPol(1, 0) = feedPol1;
  
  feedXYZ(0,0) = 0.0;
  feedXYZ(1,0) = 0.0;
  feedXYZ(2,0) = 0.0;
  
  feedAngle(0,0) = 0.0;
  feedAngle(1,0) = 0.0;
  
  //# polResp(0,0)=Complex(axr,axi);
  //# polResp(1,0)=Complex(ayr,ayi);
  //# polResp(0,1)=Complex(bxr,bxi);
  //# polResp(1,1)=Complex(byr,byi);
  polResp=Complex(0.0,0.0); //# skip this for now
  polResp(0,0,0)=polResp(1,1,0)=Complex(1.0,0.0);

  // copy entries for first antenna to all others (except for spWindow)
  for (Int i=1; i<nAnt; i++) {
    feedAntId(i)=i;
    feedId(i)=feedId(0);
    feedSpWId(i)= -1;              //  HEY!  Watch out for this when we get real
    feedBeamId(i)=feedBeamId(0);
    feedNumRec(i)=feedNumRec(0);
    beamOffset.xyPlane(i)=beamOffset.xyPlane(0);
    feedPol.column(i)=feedPol.column(0);
    feedXYZ.column(i)=feedXYZ.column(0);
    feedAngle.column(i)=feedAngle.column(0);
    polResp.xyPlane(i)=polResp.xyPlane(0);
  }

  // fill Feed table - don't check to see if any of the positions match
  MSFeedColumns& feedc=msc.feed();
  Int numFeeds=feedc.nrow();
  Slicer feedSlice(IPosition(1,numFeeds),IPosition(1, nFeed+numFeeds-1),
		   IPosition(1,1), Slicer::endIsLast);
  ms_p->feed().addRow(nFeed);
  feedc.antennaId().putColumnRange(feedSlice,feedAntId);
  feedc.feedId().putColumnRange(feedSlice,feedId);
  feedc.spectralWindowId().putColumnRange(feedSlice,feedSpWId);
  feedc.beamId().putColumnRange(feedSlice,feedBeamId);
  feedc.numReceptors().putColumnRange(feedSlice, feedNumRec);
  feedc.position().putColumnRange(feedSlice, feedXYZ);
  const double forever=1.e30;
  for (Int i=numFeeds; i<(nFeed+numFeeds); i++) {
    feedc.beamOffset().put(i,beamOffset.xyPlane(i-numFeeds));
    feedc.polarizationType().put(i,feedPol.column(i-numFeeds));
    feedc.polResponse().put(i,polResp.xyPlane(i-numFeeds));
    feedc.receptorAngle().put(i,feedAngle.column(i-numFeeds));
    feedc.time().put(i, 0.0);
    feedc.interval().put(i, forever);
  }
  os << "Added rows to FEED table" << LogIO::POST;
};


NewMSSimulator::~NewMSSimulator() 
{
}


NewMSSimulator & NewMSSimulator::operator=(const NewMSSimulator & other) 
{
  if (this==&other) return *this;
  // copy state...
  return *this;
}

void NewMSSimulator::observe(const String& sourceName,
			     const String& spWindowName,
			     const Quantity& qIntegrationTime, 
			     const Bool      useHourAngles,
			     const Quantity& qStartTime, 
			     const Quantity& qStopTime, 
			     const MEpoch&   mRefTime) 
{
  LogIO os(LogOrigin("NewMSSimulator", "observe()", WHERE));

  MSColumns msc(*ms_p);
  
  // Do we have antenna information?
  MSAntennaColumns& antc=msc.antenna();
  if(antc.nrow()==0) {
    os << "Antenna information not yet defined" << LogIO::EXCEPTION;
  }
  Int nAnt=antc.nrow();
  Vector<Double> antDiam;
  antc.dishDiameter().getColumn(antDiam);
  Matrix<Double> antXYZ(3,nAnt);
  antc.position().getColumn(antXYZ);
  
  MSDerivedValues msd;
  msd.setAntennas(msc.antenna());
  
  // Spectral window
  MSSpWindowColumns& spwc=msc.spectralWindow();
  if(spwc.nrow()==0) {
    os << "Spectral window information not yet defined" << LogIO::EXCEPTION;
  }
  Int baseSpWID=spwc.nrow();
  Int existingSpWID=-1;
  // Check for existing spectral window with correct name
  if(baseSpWID>0) {
    Vector<String> spWindowNames;
    spwc.name().getColumn(spWindowNames);
    for(uInt i=0;i<spWindowNames.nelements();i++) {
      if (spWindowNames(i)==spWindowName) {
	existingSpWID=i;
	break;
      }
    }
  }
  if(existingSpWID<0) {
    os << "Spectral window named " << spWindowName << " not yet defined" << LogIO::EXCEPTION;
  }
  MSPolarizationColumns& polc=msc.polarization();
  baseSpWID=existingSpWID;
  Double startFreq, freqInc;
  Vector<Double> resolution;
  spwc.refFrequency().get(baseSpWID,startFreq);
  spwc.resolution().get(baseSpWID,resolution);
  freqInc=resolution(0);
  Int nChan=resolution.nelements();
  Matrix<Int> corrProduct;
  polc.corrProduct().get(baseSpWID,corrProduct);
  Int nCorr=corrProduct.ncolumn();
  {
    ostringstream oss;
    oss << "Spectral window : "<<spWindowName<<endl
	<< "     reference frequency : " << startFreq/1.0e9 << "GHz" << endl
	<< "     number of channels : " << nChan << endl
	<< "     total bandwidth : " << nChan*freqInc/1.0e9 << "GHz" << endl
	<< "     number of correlations : " << nCorr << endl;
    os<<String(oss)<<LogIO::POST;
  }
  
  // Field
  MSFieldColumns& fieldc=msc.field();
  if(fieldc.nrow()==0) {
    os << "Field information not yet defined" << LogIO::EXCEPTION;
  }
  Int baseFieldID=fieldc.nrow();
  Int existingFieldID=-1;

  // Check for existing field with correct name
  if(baseFieldID>0) {
    Vector<String> fieldNames;
    fieldc.name().getColumn(fieldNames);
    for(uInt i=0;i<fieldNames.nelements();i++) {
      if (fieldNames(i)==sourceName) {
	existingFieldID=i;
	break;
      }
    }
  }
  
  if(existingFieldID<0) {
    os << "Field named " << sourceName << " not yet defined" << LogIO::EXCEPTION;
  }
  baseFieldID=existingFieldID;
  Vector<MDirection> fcs(1);
  fieldc.phaseDirMeasCol().get(baseFieldID,fcs);
  msd.setFieldCenter(fcs(0));
  MDirection fieldCenter=fcs(0);
  {
    os << "Observing source : "<<sourceName<<endl
       << "     direction : " << formatDirection(fieldCenter)<<LogIO::POST;
  }
  
  // Now we know where we are and where we are pointing, we can do the time calculations
  Double Tstart, Tend, Tint;
  {
    Tint = qIntegrationTime.getValue("s");
    Double t_offset = 0.0;   // This shifts the time forward by less than a day
    // until the qStartTime represents the starting Hour Angle
    if (useHourAngles) {
      os << "Times specified are interpreted as (source-dependent) hour angles" << LogIO::POST;
      msd.setEpoch( mRefTime );
      msd.setFieldCenter(fieldCenter);
      t_offset = - msd.hourAngle() * 3600.0 * 180.0/C::pi / 15.0; // in seconds
    }
    
    MEpoch::Ref tref(MEpoch::TAI);
    MEpoch::Convert tconvert(mRefTime, tref);
    MEpoch taiRefTime = tconvert();      
    
    Tstart = qStartTime.getValue("s") + 
      taiRefTime.get("s").getValue("s") + t_offset;
    Tend = qStopTime.getValue("s") + 
      taiRefTime.get("s").getValue("s") + t_offset;
    os << "Time range : " << endl
       << "     start : " << formatTime(Tstart) << endl
       << "     stop  : " << formatTime(Tend) << LogIO::POST;
  }

  // fill Observation Table for every call. Eventually we should fill
  // in the schedule information
  if(True) {
    MSObservation& obs=ms_p->observation();
    MSObservationColumns& obsc=msc.observation();
    Int nobsrow= obsc.nrow();
    obs.addRow();
    obsc.telescopeName().put(nobsrow,telescope_p);
    Vector<Double> timeRange(2);
    timeRange(0)=Tstart;
    timeRange(1)=Tend;
    obsc.timeRange().put(nobsrow,timeRange);
    obsc.observer().put(nobsrow,"AIPS++ simulator");
  }
  
  // init counters past end
  Int scan=-1;
  
  Int row=ms_p->nrow()-1;
  Vector<Int> tmpids(row+1);
  tmpids=msc.observationId().getColumn();
  Int maxObsId=-1;
  if (tmpids.nelements()>0) maxObsId=max(tmpids);
  tmpids=msc.arrayId().getColumn();
  Int maxArrayId=-1;
  if (tmpids.nelements()>0) maxArrayId=max(tmpids);
  tmpids.resize(0);
  
  Double Time=Tstart;
  Bool firstTime = True;
  
  uInt nShadowed = 0;
  uInt nSubElevation = 0;
  
  // Start scan number from last one (if there was one)
  Int nMSRows=ms_p->nrow();
  if(nMSRows>0) {
    msc.scanNumber().get(nMSRows-1,scan);
  }

  // One call to observe corresponds to one scan
  scan++;

  // Add new hypercubes
  hyperCubeID_p=scan;
  addHyperCubes(nAnt, nChan, nCorr, 1);

  // We can extend the ms and the hypercubes just once
  Int nNewRows;
  if(autoCorrelationWt_p > 0.0) {
    nNewRows =nAnt*(nAnt+1)/2;
  }
  else {
    nNewRows =nAnt*(nAnt-1)/2;
  }
  nNewRows*=max(1, Int(0.5+(Tend-Tstart)/Tint));
  ms_p->addRow(nNewRows);
  os << "Adding " << nNewRows << " rows" << LogIO::POST;
  {
    Record tileId;
    tileId.define(sigmaTileId, static_cast<Int>(10*hyperCubeID_p));
    sigmaAcc_p.extendHypercube(nNewRows, tileId);
    tileId.define(dataTileId, static_cast<Int>(10*hyperCubeID_p + 1));
    dataAcc_p.extendHypercube(nNewRows, tileId);
    tileId.define(scratchDataTileId, static_cast<Int>(10*hyperCubeID_p + 2));
    scratchDataAcc_p.extendHypercube(nNewRows, tileId);
    tileId.define(flagTileId, static_cast<Int>(10*hyperCubeID_p + 3));
    flagAcc_p.extendHypercube(nNewRows, tileId);
    tileId.define(imweightTileId, static_cast<Int>(10*hyperCubeID_p + 4));
    imweightAcc_p.extendHypercube(nNewRows, tileId);
  }
    
  os << "Looping over integrations" << LogIO::POST;

  for (Int itime=0; Time<Tend; itime++) {
    
    //    os << "     simulating scan " << scan+1
    //       << " from " << formatTime(Time)
    //       << " to " << formatTime(Time+Tint) << LogIO::POST;

    MEpoch epUT1 (Quantity(Time/C::day, "d"), MEpoch::UT1);
    MEpoch::Ref refGMST1(MEpoch::GMST1);
    MEpoch::Convert epGMST1(epUT1, refGMST1);
    Double gmst = epGMST1().get("d").getValue("d");
    gmst = (gmst - Int(gmst)) * C::_2pi;  // Into Radians
    
    Double ra, dec; // current phase center
    
    MDirection fc = msc.field().phaseDirMeas(baseFieldID);
    ra = fc.getAngle().getValue()(0);
    dec = fc.getAngle().getValue()(1);
    
    Record values;
    values.define("DATA_HYPERCUBE_ID",baseSpWID);
    
    Bool firstBaseline = True;
    Vector<Double> uvwvec(3);
    Matrix<Complex> data(nCorr,nChan); 
    
    Matrix<Bool> flag(nCorr,nChan); 
    flag=False;
    // random number generator
    //	MLCG rndGen(1234567);
    //	Normal normal(0.0, 1.0, &rndGen);
    
    Vector<Bool> isShadowed(nAnt);  isShadowed.set(False);
    Vector<Bool> isTooLow(nAnt);    isTooLow.set(False);
    Double fractionBlocked1=0.0, fractionBlocked2=0.0;
    Int startingRow = row;
    Double diamMax2 = square( max(antDiam) );
    
    
    // rough transformation from antenna position difference (ant2-ant1) to uvw
    Double H0 = gmst-ra, sH0=sin(H0), cH0=cos(H0), sd=sin(dec), cd=cos(dec);
    Matrix<Double> trans(3,3,0);
    trans(0,0) = -sH0;    trans(0,1) = -cH0;
    trans(1,0) =  sd*cH0; trans(1,1) = -sd*sH0; trans(1,2) = -cd;
    trans(2,0) = -cd*cH0; trans(2,1) = cd*sH0;  trans(2,2) = -sd; 
    
    for (Int ant1=0; ant1<nAnt; ant1++) {
      Double x1=antXYZ(0,ant1), y1=antXYZ(1,ant1), z1=antXYZ(2,ant1);
      for (Int ant2=ant1; ant2<nAnt; ant2++) {
	if ( (ant1 != ant2) ||  autoCorrelationWt_p > 0.0) {
	  row++; 
	  //		  if (firstBaseline) {
	  msc.scanNumber().put(row,scan);
	  msc.fieldId().put(row,baseFieldID);
	  msc.dataDescId().put(row,baseSpWID);
	  msc.time().put(row,Time+Tint/2);
	  firstBaseline=False;
	  //	  }
	  msc.antenna1().put(row,ant1);
	  msc.antenna2().put(row,ant2);
	  // this is probably wrong...
	  Double x2=antXYZ(0,ant2), y2=antXYZ(1,ant2), z2=antXYZ(2,ant2);
	  uvwvec(0) = x2-x1;
	  uvwvec(1) = y2-y1;
	  uvwvec(2) = z2-z1;
	  uvwvec=product(trans,uvwvec);
	  
	  if (ant1 != ant2) {
	    blockage(fractionBlocked1, fractionBlocked2,
		     uvwvec, antDiam(ant1), antDiam(ant2) );
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
	  msc.data().put(row,data);		  
	  msc.flag().put(row,flag);
	  msc.flagRow().put(row,False);
	  
	  msc.correctedData().setShape(row, data.shape());
	  msc.correctedData().put(row,data);
	  msc.modelData().setShape(row,data.shape());
	  msc.modelData().put(row, data);
	  Vector<Float> dummywgt(nChan);
	  dummywgt.set(1.0);
	  msc.imagingWeight().setShape(row, data.shape().getLast(1));
	  msc.imagingWeight().put(row, dummywgt);

	  // Deal with differing diameter case
	  Float sigma1 = diamMax2/(antDiam(ant1) * antDiam(ant2));
	  Float wt = 1/square(sigma1);
	  if  (ant1 == ant2 ) {
	    wt *= autoCorrelationWt_p;
	  }		  
	  Vector<Float> tmp(nCorr); tmp=wt;
	  msc.weight().put(row, tmp);
	  tmp=sigma1;
	  msc.sigma().put(row,tmp);
	  
	  if (row==(startingRow+1)) {
	    // we're using the incr stMan so we only need to 
	    // put these once
	    msc.arrayId().put(row,maxArrayId+1);
	    msc.processorId().put(row,0);
	    msc.exposure().put(row,Tint);
	    msc.feed1().put(row,0);
	    msc.feed2().put(row,0);
	    msc.interval().put(row,Tint);
	    msc.observationId().put(row,maxObsId+1);
	    msc.stateId().put(row,-1);
	  }
	  
	}
      }
    }
    
    // go back and flag weights based on shadowing
    // Future option: we could increase sigma based on
    // fraction shadowed.
    Matrix<Bool> trueFlag(nCorr,nChan); 
    trueFlag=True;
    
    Int reRow = startingRow;
    for (Int ant1=0; ant1<nAnt; ant1++) {
      for (Int ant2=ant1; ant2<nAnt; ant2++) {
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
    
    MEpoch ep(Quantity((Time + Tint/2), "s"));
    msd.setEpoch(ep);
    
    msd.setFieldCenter(fc);
    
    // go back and flag weights based on elevationLimit_p
    for (Int ant1=0; ant1<nAnt; ant1++) {
      
      // We want to find elevation for each antenna separately (for VLBI)
      msd.setAntenna(ant1);
      Vector<Double> azel=msd.azel().getAngle("rad").getValue("rad");
      
      if (azel(1) < elevationLimit_p.getValue("rad")) {
	isTooLow(ant1) = True;
      }
      if (firstTime) {
	firstTime = False;
	Double ha1 = msd.hourAngle() *  180.0/C::pi / 15.0;
	os << "Starting conditions for antenna 1: " << LogIO::POST;
	os << "     time = " << formatTime(Time) << LogIO::POST;
	os << "     scan = " << scan+1 << LogIO::POST;
	os << "     az   = " << azel(0) *  180.0/C::pi << " deg" << LogIO::POST;
	os << "     el   = " << azel(1) *  180.0/C::pi<< " deg" << LogIO::POST;
	os << "     ha   = " << ha1 << " hours" << LogIO::POST;
      }
    }
    reRow = startingRow;
    for (Int ant1=0; ant1<nAnt; ant1++) {
      for (Int ant2=ant1; ant2<nAnt; ant2++) {
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
    
    Int numpointrows=nAnt;
    MSPointingColumns& pointingc=msc.pointing();
    Int numPointing=pointingc.nrow();
    ms_p->pointing().addRow(numpointrows);
    numpointrows += numPointing;
    Double Tint=qIntegrationTime.getValue("s");
    Vector<MDirection> direction(1);
    direction(0)=fieldCenter;
    for (Int m=numPointing; m < (numPointing+nAnt); m++){
      pointingc.numPoly().put(m,0);
      pointingc.interval().put(m,-1);
      pointingc.tracking().put(m,True);
      pointingc.time().put(m,Time);
      pointingc.timeOrigin().put(m,Tstart);
      pointingc.interval().put(m,Tint);
      pointingc.antennaId().put(m, m);
      pointingc.directionMeasCol().put(m,direction);
      pointingc.targetMeasCol().put(m,direction);             
    }
    Time+=Tint; 
  }
  
  {
    msd.setAntenna(0);
    Vector<Double> azel=msd.azel().getAngle("rad").getValue("rad");

    Double ha1 = msd.hourAngle() *  180.0/C::pi / 15.0;
    os << "Stopping conditions for antenna 1: " << LogIO::POST;
    os << "     time = " << formatTime(Time) << LogIO::POST;
    os << "     scan = " << scan+1 << LogIO::POST;
    os << "     az   = " << azel(0) *  180.0/C::pi << " deg" << LogIO::POST;
    os << "     el   = " << azel(1) *  180.0/C::pi << " deg" << LogIO::POST;
    os << "     ha   = " << ha1 << " hours" << LogIO::POST;
  }
  
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


String NewMSSimulator::formatDirection(const MDirection& direction) {
  MVAngle mvRa=direction.getAngle().getValue()(0);
  MVAngle mvDec=direction.getAngle().getValue()(1);
  ostringstream oss;
  oss.setf(ios::left, ios::adjustfield);
  oss.width(14);
  oss << mvRa(0.0).string(MVAngle::TIME,8);
  oss.width(14);
  oss << mvDec.string(MVAngle::DIG2,8);
  oss << "     " << MDirection::showType(direction.getRefPtr()->getType());
  return String(oss);
}

String NewMSSimulator::formatTime(const Double time) {
  MVTime mvtime(Quantity(time, "s"));
  return mvtime.string(MVTime::DMY,7);
}
