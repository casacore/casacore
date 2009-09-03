//# NewMSSimulator.cc:  this defines NewMSSimulator, which simulates a MeasurementSet
//# Copyright (C) 1995-2009
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
#include <ms/MeasurementSets/NewMSSimulator.h>
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
#include <casa/Arrays/Cube.h>
#include <casa/Arrays/MatrixMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/Slice.h>
#include <measures/Measures/Stokes.h>
#include <measures/Measures/MeasFrame.h>
#include <casa/Quanta/MVuvw.h>
#include <casa/Quanta/MVDirection.h>
#include <casa/Quanta/MVAngle.h>
#include <casa/Quanta/MVTime.h>
#include <measures/Measures/MeasTable.h>
#include <measures/Measures/MBaseline.h>
#include <measures/Measures/MCBaseline.h>
#include <measures/Measures/Muvw.h>
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
#include <scimath/Mathematics/RigidVector.h>
#include <scimath/Mathematics/SquareMatrix.h>

// temporary to get access to beam_offsets
#include <ms/MeasurementSets/MSIter.h>
//

namespace casa { //# NAMESPACE CASA - BEGIN

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

// a but ugly solution to use the feed table parser of MSIter
// to extract antennaMounts and BeamOffsets.
struct MSFeedParameterExtractor : protected MSIter {
  
  MSFeedParameterExtractor(const MeasurementSet &ms) {
      msc_p=new ROMSColumns(ms);
      msc_p->antenna().mount().getColumn(antennaMounts_p,True);
      checkFeed_p=True;
      setFeedInfo();
  }
  // Return a string mount identifier for each antenna
  using MSIter::antennaMounts;

  // Return a cube containing pairs of coordinate offset for each receptor
  // of each feed (values are in radians, coordinate system is fixed with
  // antenna and is the same as used to define the BEAM_OFFSET parameter
  // in the feed table). The cube axes are receptor, antenna, feed.
  using MSIter::getBeamOffsets;

  // True if all elements of the cube returned by getBeamOffsets are zero
  using MSIter::allBeamOffsetsZero;

};
//



void NewMSSimulator::defaults() {
  fractionBlockageLimit_p=1e-6;
  elevationLimit_p=Quantity(8.0, "deg");
  autoCorrelationWt_p=1.0;
  telescope_p="Unknown";
  qIntegrationTime_p=Quantity(10.0, "s");
  useHourAngle_p=True;
  Quantity today;
  MVTime::read(today, "today");
  mRefTime_p=MEpoch(today, MEpoch::UTC);
}
  
NewMSSimulator::NewMSSimulator(const String& MSName) :
  ms_p(0), dataAcc_p(), scratchDataAcc_p(), sigmaAcc_p(), flagAcc_p(), imweightAcc_p(),
  maxData_p(2e9)
{
  LogIO os(LogOrigin("NewMSSimulator", "NewMSSimulator<const String& MSName)", WHERE));

  defaults();

  // make MS with standard columns
  TableDesc msDesc(MS::requiredTableDesc());

  // Add other columns, including the scratch columns
  MS::addColumnToDesc(msDesc,MS::DATA,2);
  MS::addColumnToDesc(msDesc,MS::MODEL_DATA,2);
  MS::addColumnToDesc(msDesc,MS::CORRECTED_DATA,2);
  MS::addColumnToDesc(msDesc,MS::IMAGING_WEIGHT,1);
  
  // Add index columns for tiling. We use three tiles: data, sigma, and flag.
  // Some of these contain more than one column
  /*
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
  */
  // setup hypercolumns for the data/flag/flag_catagory/sigma & weight columns.
  {
    Vector<String> dataCols(1);
    dataCols(0) = MeasurementSet::columnName(MeasurementSet::DATA);
    const Vector<String> coordCols(0);
    const Vector<String> idCols(1, dataTileId);
    //msDesc.defineHypercolumn(dataCol, 3, dataCols, coordCols, idCols);
    msDesc.defineHypercolumn(dataCol, 3, dataCols);
  }
{
    Vector<String> dataCols(1);
    dataCols(0) = MeasurementSet::columnName(MeasurementSet::FLAG);
    const Vector<String> coordCols(0);
    const Vector<String> idCols(1, dataTileId);
    //msDesc.defineHypercolumn(dataCol, 3, dataCols, coordCols, idCols);
    msDesc.defineHypercolumn("FlagColumn", 3, dataCols);
  }
  {
    Vector<String> dataCols(1);
    dataCols(0) = MeasurementSet::columnName(MeasurementSet::MODEL_DATA);
    const Vector<String> coordCols(0);
    const Vector<String> idCols(1, scratchDataTileId);
    //    msDesc.defineHypercolumn(scratchDataCol, 3, dataCols, coordCols, idCols);
    msDesc.defineHypercolumn("ModelDataColumn", 3, dataCols);
  }
 {
    Vector<String> dataCols(1);
    dataCols(0) = MeasurementSet::columnName(MeasurementSet::CORRECTED_DATA);
    const Vector<String> coordCols(0);
    const Vector<String> idCols(1, scratchDataTileId);
    //msDesc.defineHypercolumn(scratchDataCol, 3, dataCols, coordCols, idCols);
    msDesc.defineHypercolumn("CorrectedDataColumn", 3, dataCols);
  }

  {
    Vector<String> dataCols(1);
    dataCols(0) = MeasurementSet::columnName(MeasurementSet::SIGMA);
    const Vector<String> coordCols(0);
    const Vector<String> idCols(1, sigmaTileId);
    //msDesc.defineHypercolumn(sigmaCol, 2, dataCols, coordCols, idCols);
    msDesc.defineHypercolumn("SigmaColumn", 2, dataCols);
  }
  {
    Vector<String> dataCols(1);
    dataCols(0) = MeasurementSet::columnName(MeasurementSet::WEIGHT);
    const Vector<String> coordCols(0);
    const Vector<String> idCols(1, sigmaTileId);
    // msDesc.defineHypercolumn(sigmaCol, 2, dataCols, coordCols, idCols);
    msDesc.defineHypercolumn("WeightColumn", 2, dataCols);
  }

  {
    Vector<String> dataCols(1);
    dataCols(0) = MeasurementSet::columnName(MeasurementSet::IMAGING_WEIGHT);
    const Vector<String> coordCols(0);
    const Vector<String> idCols(1, imweightTileId);
    //msDesc.defineHypercolumn(imweightCol, 2, dataCols, coordCols, idCols);
    msDesc.defineHypercolumn(imweightCol, 2, dataCols);
  }
  {
    Vector<String> dataCols(1);
    dataCols(0) = MeasurementSet::columnName(MeasurementSet::FLAG_CATEGORY);
    const Vector<String> coordCols(0);
    const Vector<String> idCols(1, flagTileId);
    //    msDesc.defineHypercolumn(flagCol, 4, dataCols, coordCols, idCols);
    msDesc.defineHypercolumn(flagCol, 4, dataCols);
  }

  SetupNewTable newMS(MSName, msDesc, Table::New);
  
  // Set the default Storage Manager to be the Incr one
  {
    IncrementalStMan incrStMan ("ismdata");
    newMS.bindAll(incrStMan, True);
  }
  
  // Bind ANTENNA1, and ANTENNA2 to the standardStMan 
  // as they may change sufficiently frequently to make the
  // incremental storage manager inefficient for these columns.
  
  {
    StandardStMan ssm(32768);
    newMS.bindColumn(MS::columnName(MS::ANTENNA1), ssm);
    newMS.bindColumn(MS::columnName(MS::ANTENNA2), ssm);
  }
  
  IPosition tileShape(3, 4, 100, 100);
  // These columns contain the bulk of the data so save them in a tiled way
  {
    TiledShapeStMan dataMan(dataCol, tileShape);
    newMS.bindColumn(MeasurementSet::
		     columnName(MeasurementSet::DATA), dataMan);
  }
  {
    TiledShapeStMan dataMan("FlagColumn", tileShape);
    newMS.bindColumn(MeasurementSet::
		     columnName(MeasurementSet::FLAG), dataMan);
  }
  {
    TiledShapeStMan dataMan("ModelDataColumn", tileShape);
    newMS.bindColumn(MeasurementSet::
		     columnName(MeasurementSet::MODEL_DATA), dataMan);
  }
  {
    TiledShapeStMan dataMan("CorrectedDataColumn", tileShape);
    newMS.bindColumn(MeasurementSet::
		     columnName(MeasurementSet::CORRECTED_DATA), dataMan);
  }
  {
    TiledShapeStMan dataMan("SigmaColumn", IPosition(2,tileShape(0), tileShape(2)));
    newMS.bindColumn(MeasurementSet::
 		     columnName(MeasurementSet::SIGMA), dataMan);
  }
  {
   TiledShapeStMan dataMan("WeightColumn", IPosition(2,tileShape(0), tileShape(2))); 
    newMS.bindColumn(MeasurementSet::
 		     columnName(MeasurementSet::WEIGHT), dataMan);
  }

  {
    TiledShapeStMan dataMan(imweightCol, IPosition(2,tileShape(1), 
						   tileShape(2)));
    newMS.bindColumn(MeasurementSet::
 		     columnName(MeasurementSet::IMAGING_WEIGHT), dataMan);
    // newMS.bindColumn(imweightTileId, dataMan);
  }

  {
    TiledShapeStMan dataMan(flagCol, 
			    IPosition(4,tileShape(0),tileShape(1), 1,
				      tileShape(2)));
    newMS.bindColumn(MeasurementSet::
   		     columnName(MeasurementSet::FLAG_CATEGORY), dataMan);
    // newMS.bindColumn(flagTileId, dataMan);
  }

  // Now we can create the MeasurementSet and add the (empty) subtables
  ms_p=new MeasurementSet(newMS,0);
  ms_p->createDefaultSubtables(Table::New);
  // Its better to have an empty SOURCE subtable than none 
  // (ms.tofits for example requires one)
  // We really should fill it in ::setfield() but that can wait
  // This is from SimpleSimulator - not sure why we're not using that.
  // add the SOURCE table
  TableDesc tdesc = MSSource::requiredTableDesc();
  MSSource::addColumnToDesc(tdesc, MSSourceEnums::REST_FREQUENCY, 1);
  SetupNewTable sourceSetup(ms_p->sourceTableName(),tdesc,Table::New);
  ms_p->rwKeywordSet().defineTable(MS::keywordName(MS::SOURCE),
				   Table(sourceSetup));
  //
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
  /*
  dataAcc_p = TiledDataStManAccessor(*ms_p, dataCol);
  scratchDataAcc_p = TiledDataStManAccessor(*ms_p, scratchDataCol);
  sigmaAcc_p = TiledDataStManAccessor(*ms_p, sigmaCol);
  flagAcc_p = TiledDataStManAccessor(*ms_p, flagCol);
  imweightAcc_p = TiledDataStManAccessor(*ms_p, imweightCol);
  */
  // We're done - wasn't that easy?

  dataWritten_p=0.0;
  hyperCubeID_p=-1;
  lastSpWID_p=-1;
  lastNchan_p=-1;
  hasHyperCubes_p=False;
}

NewMSSimulator::NewMSSimulator(MeasurementSet& theMS) :
  ms_p(0), dataAcc_p(), scratchDataAcc_p(), sigmaAcc_p(), flagAcc_p(), imweightAcc_p(),
  maxData_p(2e9)
{
  LogIO os(LogOrigin("NewMSSimulator", "NewMSSimulator(MeasurementSet& theMS)", WHERE));

  defaults();

  ms_p = new MeasurementSet(theMS);

  os << "Opening MeasurementSet " << ms_p->tableName() << " with " << ms_p->nrow() << " rows" << LogIO::POST;
  dataWritten_p=ms_p->nrow();
  
  TableDesc td(ms_p->tableDesc());
  if(td.isColumn(dataTileId)) {
    hasHyperCubes_p=True;
    // Now we can make the accessors to be used when adding hypercolumns
    dataAcc_p = TiledDataStManAccessor(*ms_p, dataCol);
    scratchDataAcc_p = TiledDataStManAccessor(*ms_p, scratchDataCol);
    sigmaAcc_p = TiledDataStManAccessor(*ms_p, sigmaCol);
    flagAcc_p = TiledDataStManAccessor(*ms_p, flagCol);
    imweightAcc_p = TiledDataStManAccessor(*ms_p, imweightCol);
    
    ScalarColumn<Int> hyperCubeIDColumn(*ms_p, dataTileId);
    hyperCubeID_p=max(hyperCubeIDColumn.getColumn());
    os << "   last hyper cube ID = " << hyperCubeID_p << LogIO::POST;
  }
  else {
    hasHyperCubes_p=False;
  }
  {
    MSColumns msc(*ms_p);
    MSSpWindowColumns& spwc=msc.spectralWindow();
    lastSpWID_p=spwc.nrow();
    lastNchan_p=spwc.chanFreq()(lastSpWID_p-1).nelements();

    os << "   last spectral window ID = " << lastSpWID_p << LogIO::POST;
  }
}

// Add new hypercubes as the shape changes
void NewMSSimulator::addHyperCubes(const Int id, const Int nBase, const Int nChan,
				   const Int nCorr)
{
  Record tileId;
  const uInt chanTiles=(nChan+7)/8;
  
  tileId.define(sigmaTileId, static_cast<Int>(10*id));
  sigmaAcc_p.addHypercube(IPosition(2, nCorr, 0), 
			  IPosition(2, nCorr, nBase),
			  tileId);
  
  tileId.define(dataTileId, static_cast<Int>(10*id+1));
  dataAcc_p.addHypercube(IPosition(3, nCorr, nChan, 0), 
			 IPosition(3, nCorr, chanTiles, nBase),
			 tileId);
  
  tileId.define(scratchDataTileId, static_cast<Int>(10*id+2));
  scratchDataAcc_p.addHypercube(IPosition(3, nCorr, nChan, 0), 
				IPosition(3, nCorr, chanTiles, nBase),
				tileId);
  
  tileId.define(flagTileId, static_cast<Int>(10*id + 3));
  flagAcc_p.addHypercube(IPosition(4, nCorr, nChan, nCat, 0), 
			 IPosition(4, nCorr, chanTiles, nCat, nBase),
			 tileId);
  
  tileId.define(imweightTileId, static_cast<Int>(10*id + 4));
  imweightAcc_p.addHypercube(IPosition(2, nChan, 0), 
			     IPosition(2, chanTiles, nBase),
			     tileId);
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
			     const Vector<Double>&,
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

void NewMSSimulator::longlat2global(Vector<Double>&,
				    Vector<Double>&,
				    Vector<Double>&,
				    const MPosition&,
				    const Vector<Double>&,
				    const Vector<Double>&,
				    const Vector<Double>&)
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
				   const Quantity&,
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
  stokesTypes.resize(nCorr, True);
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

void NewMSSimulator::initFeeds(const String& mode) {
  LogIO os(LogOrigin("MSsimulator", "initFeeds()", WHERE));
  if(mode=="list") {
    os << "Mode list not supported without x,y,pol set" << LogIO::EXCEPTION;
  }

  Vector<Double> x;
  Vector<Double> y;
  Vector<String> pol;
  initFeeds(mode, x, y, pol);
}

// NOTE:  initAnt and initSpWindows must be called before this one!
void NewMSSimulator::initFeeds(const String& mode,
			       const Vector<Double>& x,
			       const Vector<Double>& y,
			       const Vector<String>& pol)
{
  LogIO os(LogOrigin("MSsimulator", "initFeeds()", WHERE));
  
  MSColumns msc(*ms_p);
  MSAntennaColumns& antc=msc.antenna();
  Int nAnt=antc.nrow();
  
  if (nAnt <= 0) {
    os <<  LogIO::SEVERE << "NewMSSimulator::initFeeds: must call initAnt() first" << LogIO::POST;
  }
  
  Int nFeed=x.nelements();
  
  String feedPol0="R", feedPol1="L";
  Bool isList=False;
  if(nFeed>0) {
    isList=True;
    if(x.nelements()!=y.nelements()) {
      os << "Feed x and y must be the same length" << LogIO::EXCEPTION;
    }
    if(pol.nelements()!=x.nelements()) {
      os << "Feed polarization list must be same length as the number of positions" << LogIO::EXCEPTION;
    }
    os <<  "Constructing FEED table from list" << LogIO::POST;
  }
  else {
    nFeed=1;
    // mode == "perfect R L" OR "perfect X Y"
    if (mode.contains("X", 0)) {
      feedPol0 = "X";
      feedPol1 = "Y";
    }
  }
  
  Int nRow=nFeed*nAnt;
  Vector<Int> feedAntId(nRow);
  Vector<Int> feedId(nRow); 
  Vector<Int> feedSpWId(nRow);
  Vector<Int> feedBeamId(nRow);
  
  Vector<Int> feedNumRec(nRow);
  Cube<Double> beamOffset(2,2,nRow);
  
  Matrix<String> feedPol(2,nRow);
  Matrix<Double> feedXYZ(3,nRow);
  Matrix<Double> feedAngle(2,nRow);
  Cube<Complex> polResp(2,2,nRow);
  
  Int iRow=0;
  if(isList) {
    polResp=Complex(0.0,0.0);
    for (Int i=0; i<nAnt; i++) {
      for (Int j=0; j<nFeed; j++) {
	feedAntId(iRow)=i;
	feedId(iRow) = j;
	feedSpWId(iRow) = -1;
	feedBeamId(iRow) = 0;
	feedNumRec(iRow) = 2;
	beamOffset(0,0,iRow) = x(j);
	beamOffset(1,0,iRow) = y(j);
	beamOffset(0,1,iRow) = x(j);
	beamOffset(1,1,iRow) = y(j);
	feedXYZ(0,iRow) = 0.0;
	feedXYZ(1,iRow) = 0.0;
	feedXYZ(2,iRow) = 0.0;
	feedAngle(0,iRow) = 0.0;
	feedAngle(1,iRow) = 0.0;
	if (pol(j).contains("X", 0)) {
	  feedPol(0, iRow) = "X";
	  feedPol(1, iRow) = "Y";
	}
	else {
	  feedPol(0, iRow) = "L";
	  feedPol(1, iRow) = "R";
	}
	polResp(0,0,iRow)=polResp(1,1,iRow)=Complex(1.0,0.0);
	os << "Row " << iRow+1 << " : Feed " << j+1 << " on antenna "
	   << i+1 << " "
	   << x(j) << " " << y(j) << " " << pol(j) << LogIO::POST;
	iRow++;
      }
    }
  }
  else {
    polResp=Complex(0.0,0.0);
    for (Int i=0; i<nAnt; i++) {
      feedAntId(iRow)=i;
      feedId(iRow) = 0;
      feedSpWId(iRow) = -1;
      feedBeamId(iRow) = 0;
      feedNumRec(iRow) = 2;
      beamOffset(0,0,iRow) = 0.0;
      beamOffset(1,0,iRow) = 0.0;
      beamOffset(0,1,iRow) = 0.0;
      beamOffset(1,1,iRow) = 0.0;
      feedXYZ(0,iRow) = 0.0;
      feedXYZ(1,iRow) = 0.0;
      feedXYZ(2,iRow) = 0.0;
      feedAngle(0,iRow) = 0.0;
      feedAngle(1,iRow) = 0.0;
      feedPol(0, iRow) = feedPol0;
      feedPol(1, iRow) = feedPol1;
      polResp(0,0,iRow)=polResp(1,1,iRow)=Complex(1.0,0.0);
      iRow++;
    }
  }
  
  // fill Feed table - don't check to see if any of the positions match
  MSFeedColumns& feedc=msc.feed();
  Int numFeeds=feedc.nrow();
  Slicer feedSlice(IPosition(1,numFeeds),IPosition(1, nRow+numFeeds-1),
		   IPosition(1,1), Slicer::endIsLast);
  ms_p->feed().addRow(nRow);
  feedc.antennaId().putColumnRange(feedSlice,feedAntId);
  feedc.feedId().putColumnRange(feedSlice,feedId);
  feedc.spectralWindowId().putColumnRange(feedSlice,feedSpWId);
  feedc.beamId().putColumnRange(feedSlice,feedBeamId);
  feedc.numReceptors().putColumnRange(feedSlice, feedNumRec);
  feedc.position().putColumnRange(feedSlice, feedXYZ);
  const double forever=1.e30;
  for (Int i=numFeeds; i<(nRow+numFeeds); i++) {
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

  if(ms_p)
    delete ms_p;
  ms_p=0;

}


NewMSSimulator & NewMSSimulator::operator=(const NewMSSimulator & other) 
{
  if (this==&other) return *this;
  // copy state...
  return *this;
}

void NewMSSimulator::settimes(const Quantity& qIntegrationTime,
			      const Bool useHourAngle,
			      const MEpoch&   mRefTime)

{
  LogIO os(LogOrigin("NewMSSimulator", "settimes()", WHERE));

  qIntegrationTime_p=qIntegrationTime;
  useHourAngle_p=useHourAngle;
  mRefTime_p=mRefTime;
  if(useHourAngle_p) {
    hourAngleDefined_p=False;
  }
  t_offset_p=0.0;
}

void NewMSSimulator::observe(const String& sourceName,
			     const String& spWindowName,
			     const Quantity& qStartTime, 
			     const Quantity& qStopTime)
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
  
  // Do we have feed information?
  MSFeedColumns& feedc=msc.feed();
  if(feedc.nrow()==0) {
    os << "Feed information not yet defined" << LogIO::EXCEPTION;
  }
  Int nFeed=feedc.nrow()/nAnt;
  
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

  // A bit ugly solution to extract the information about beam offsets
  Cube<RigidVector<Double, 2> > beam_offsets;
  Vector<String> antenna_mounts;
  { // to close MSIter, when the job is done
    MSFeedParameterExtractor msfpe_tmp(*ms_p);
    beam_offsets=msfpe_tmp.getBeamOffsets();
    antenna_mounts=msfpe_tmp.antennaMounts();
  }
  if (beam_offsets.nplane()!=(uInt)nFeed || beam_offsets.ncolumn()!=(uInt)nAnt)
      os<< "Feed table format is incompatible with existing code of NewMSSimulator::observe"<<LogIO::EXCEPTION;
  //
  
  // Now we know where we are and where we are pointing, we can do the time calculations
  Double Tstart, Tend, Tint;
  {
    Tint = qIntegrationTime_p.getValue("s");

    MEpoch::Ref tref(MEpoch::TAI);
    MEpoch::Convert tconvert(mRefTime_p, tref);
    MEpoch taiRefTime = tconvert();      
    
    // until the qStartTime represents the starting Hour Angle
    if (useHourAngle_p&&!hourAngleDefined_p) {
      msd.setEpoch( mRefTime_p );
      msd.setFieldCenter(fieldCenter);
      t_offset_p = - msd.hourAngle() * 3600.0 * 180.0/C::pi / 15.0; // in seconds
      hourAngleDefined_p=True;
      os << "Times specified are interpreted as hour angles for first source observed" << endl
	 << "     offset in time = " << t_offset_p / 3600.0 << " hours from "
	 << formatTime(taiRefTime.get("s").getValue("s")) << LogIO::POST;
    }
    
    Tstart = qStartTime.getValue("s") + 
      taiRefTime.get("s").getValue("s") + t_offset_p;
    Tend = qStopTime.getValue("s") + 
      taiRefTime.get("s").getValue("s") + t_offset_p;
    os << "Time range : " << endl
       << "     start : " << formatTime(Tstart) << endl
       << "     stop  : " << formatTime(Tend) << LogIO::POST;
  }

  // fill Observation Table for every call. Eventually we should fill
  // in the schedule information
  MSObservation& obs=ms_p->observation();
  MSObservationColumns& obsc=msc.observation();
  Int nobsrow= obsc.nrow();
  obs.addRow();
  obsc.telescopeName().put(nobsrow,telescope_p);
  Vector<Double> timeRange(2);
  timeRange(0)=Tstart;
  timeRange(1)=Tend;
  obsc.timeRange().put(nobsrow,timeRange);
  obsc.observer().put(nobsrow,"CASA simulator");
  
  Int row=ms_p->nrow()-1;
  Int maxObsId=-1;
  Int maxArrayId=0;
  {
    Vector<Int> tmpids(row+1);
    tmpids=msc.observationId().getColumn();
    if (tmpids.nelements()>0) maxObsId=max(tmpids);
    tmpids=msc.arrayId().getColumn();
    if (tmpids.nelements()>0) maxArrayId=max(tmpids);
  }
  
  Double Time=Tstart;
  Bool firstTime = True;
  
  uInt nShadowed = 0;
  uInt nSubElevation = 0;
  
  // Start scan number from last one (if there was one)
  Int nMSRows=ms_p->nrow();

  // init counters past end
  Int scan=-1;
  
  if(nMSRows>0) {
    msc.scanNumber().get(nMSRows-1,scan);
  }

  // One call to observe corresponds to one scan
  scan++;

  // We can extend the ms and the hypercubes just once
  Int nBaselines;
  if(autoCorrelationWt_p > 0.0) {
    nBaselines =nAnt*(nAnt+1)/2;
  }
  else {
    nBaselines =nAnt*(nAnt-1)/2;
  }
  Int nNewRows=nBaselines*nFeed;
  Int nIntegrations=max(1, Int(0.5+(Tend-Tstart)/Tint));
  nNewRows*=nIntegrations;

  // We need to do addition in this order to get a new TSM file.

  // Various conditions for new hypercube
  Bool needNewHyperCube=False;
  if(hasHyperCubes_p) {
    if(hyperCubeID_p<0) needNewHyperCube=True;
    if(lastSpWID_p<0) {
      needNewHyperCube=True;
    }
    else if((baseSpWID!=lastSpWID_p) &&(lastNchan_p!=nChan)) {
      needNewHyperCube=True;
    }
    if((maxData_p>0)&&(dataWritten_p>maxData_p)) {
      needNewHyperCube=True;
    }
  }
  if(needNewHyperCube) {
    hyperCubeID_p++;
    os << "Creating new hypercube " << hyperCubeID_p+1 << LogIO::POST;
    addHyperCubes(hyperCubeID_p, nBaselines, nChan, nCorr);
    dataWritten_p=0;
    lastSpWID_p=baseSpWID;
    lastNchan_p=nChan;
  }
  // ... Next extend the table
  os << "Adding " << nNewRows << " rows" << LogIO::POST;
  ms_p->addRow(nNewRows);

  // ... Finally extend the hypercubes
  if(hasHyperCubes_p) {
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
    // Size of scratch columns
    Double thisChunk=16.0*Double(nChan)*Double(nCorr)*Double(nNewRows);
    dataWritten_p+=thisChunk;
    os << "Written " << thisChunk/(1024.0*1024.0) << " Mbytes to scratch columns" << LogIO::POST;
  }

  Matrix<Complex> data(nCorr,nChan); 
  data.set(Complex(0.0));

  Matrix<Bool> flag(nCorr,nChan); 
  flag=False;
    
  Vector<Float> imagingWeight(nChan);
  imagingWeight.set(1.0);


 
  os << "Calculating uvw coordinates for " << nIntegrations << " integrations" << LogIO::POST;

  for(Int feed=0; feed<nFeed; feed++) {
    if (nFeed) 
       os << "Processing feed "<<feed<< LogIO::POST;
    // for now assume that all feeds have the same offsets w.r.t.
    // antenna frame for all antennas
    RigidVector<Double, 2> beamOffset=beam_offsets(0,0,feed);

    for(Int integration=0; integration<nIntegrations; integration++) {
    
      MEpoch epUT1 (Quantity(Time/C::day, "d"), MEpoch::UT1);
      MEpoch::Ref refGMST1(MEpoch::GMST1);
      MEpoch::Convert epGMST1(epUT1, refGMST1);
      Double gmst = epGMST1().get("d").getValue("d");
      gmst = (gmst - Int(gmst)) * C::_2pi;  // Into Radians
    
      MEpoch ep(Quantity((Time + Tint/2), "s"), MEpoch::UT1);
      msd.setEpoch(ep);
      
      // current phase center for a beam without offset
      // For each individual beam pointing center always coincides
      // with the phase center
      
      // ???? May be we can use fcs defined earlier instead of fc ????
      MDirection fc = msc.field().phaseDirMeas(baseFieldID);
      msd.setFieldCenter(fc);
      msd.setAntenna(0); // assume for now that all par. angles are the same 
            
      Vector<Bool> isShadowed(nAnt);  isShadowed.set(False);
      Vector<Bool> isTooLow(nAnt);    isTooLow.set(False);
      Double fractionBlocked1=0.0, fractionBlocked2=0.0;
      Int startingRow = row;
      Double diamMax2 = square( max(antDiam) );

      // fringe stopping center could be different for different feeds
      MDirection feed_phc=fc;
    
      // Do the first row outside the loop
      msc.scanNumber().put(row+1,scan);
      msc.fieldId().put(row+1,baseFieldID);
      msc.dataDescId().put(row+1,baseSpWID);
      msc.time().put(row+1,Time+Tint/2);
      msc.timeCentroid().put(row+1,Time+Tint/2);
      msc.arrayId().put(row+1,maxArrayId);
      msc.processorId().put(row+1,0);
      msc.exposure().put(row+1,Tint);
      msc.interval().put(row+1,Tint);
      msc.observationId().put(row+1,maxObsId+1);
      msc.stateId().put(row+1,-1);

      // assume also that all mounts are the same and posit. angle is the same
      if (antenna_mounts[0]=="ALT-AZ" || antenna_mounts[0]=="alt-az") {
          // parallactic angle rotation is necessary
          SquareMatrix<Double, 2> xform(SquareMatrix<Double, 2>::General);
          // SquareMatrix' default constructor is a bit strange, we probably
          // need to change it in the future

      
          const Double pa=msd.parAngle();  
          const Double cpa=cos(pa);
          const Double spa=sin(pa);
          xform(0,0)=cpa;
          xform(1,1)=cpa;
          xform(0,1)=-spa;
          xform(1,0)=spa;
          beamOffset*=xform;
      }
      // x direction is flipped to convert az-el type frame to ra-dec
      feed_phc.shift(-beamOffset(0),beamOffset(1),True);
      ///Below code is replaced with calcUVW that does a baseline conversion
      ///to J2000 too
    
      //      Double ra, dec; // current phase center
      //      ra = feed_phc.getAngle().getValue()(0);
      //      dec = feed_phc.getAngle().getValue()(1);

      // Transformation from antenna position difference (ant2-ant1) to uvw
      //      Double H0 = gmst-ra, sH0=sin(H0), cH0=cos(H0), sd=sin(dec), cd=cos(dec);
      //      Matrix<Double> trans(3,3,0);
      //      trans(0,0) = -sH0;    trans(0,1) = -cH0;
      //      trans(1,0) =  sd*cH0; trans(1,1) = -sd*sH0; trans(1,2) = -cd;
      //      trans(2,0) = -cd*cH0; trans(2,1) = cd*sH0;  trans(2,2) = -sd; 
      //      Matrix<Double> antUVW(3,nAnt);	 

      // for (Int ant1=0; ant1<nAnt; ant1++)
      //           antUVW.column(ant1)=product(trans,antXYZ.column(ant1)); 
      // Rotate antennas to correct frame
      Matrix<Double> antUVW(3,nAnt);	      
      calcAntUVW(ep, feed_phc, antUVW);

   
      for(Int ant1=0; ant1<nAnt; ant1++) {
	Double x1=antUVW(0,ant1), y1=antUVW(1,ant1), z1=antUVW(2,ant1);
	Int startAnt2=ant1+1;
	if(autoCorrelationWt_p>0.0) startAnt2=ant1;
	for (Int ant2=startAnt2; ant2<nAnt; ant2++) {
	  row++; 
	  
	  msc.antenna1().put(row,ant1);
	  msc.antenna2().put(row,ant2);
	  msc.feed1().put(row,feed);
	  msc.feed2().put(row,feed);
	  
	  Double x2=antUVW(0,ant2), y2=antUVW(1,ant2), z2=antUVW(2,ant2);
	  Vector<Double> uvwvec(3);
	  uvwvec(0) = x2-x1;
	  uvwvec(1) = y2-y1;
	  uvwvec(2) = z2-z1;
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
	  msc.imagingWeight().setShape(row, data.shape().getLast(1));
	  msc.imagingWeight().put(row, imagingWeight);
	  
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
	}
      }
    
    
    // go back and flag weights based on shadowing
    // Future option: we could increase sigma based on
    // fraction shadowed.
    Matrix<Bool> trueFlag(nCorr,nChan); 
    trueFlag=True;

    Int reRow = startingRow;
    for (Int ant1=0; ant1<nAnt; ant1++) {
	Int startAnt2=ant1+1;
	if(autoCorrelationWt_p>0.0) startAnt2=ant1;
	for (Int ant2=startAnt2; ant2<nAnt; ant2++) {
	  reRow++; 
	  if ( isShadowed(ant1) || isShadowed(ant2) ) {
	    msc.flag().put(reRow,trueFlag);
	    msc.flagRow().put(reRow, True);
	    nShadowed++;
	  }
	}
    }
    
        
    
    // Find antennas pointing below the elevation limit
    Vector<Double> azel(2);
    for (Int ant1=0; ant1<nAnt; ant1++) {
	
	// We want to find elevation for each antenna separately (for VLBI)
	msd.setAntenna(ant1);
	azel=msd.azel().getAngle("rad").getValue("rad");      
	
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

    // Now flag all antennas pointing below the elevation limit
    reRow = startingRow;    
    for (Int ant1=0; ant1<nAnt; ant1++) {
	Int startAnt2=ant1+1;
	if(autoCorrelationWt_p>0.0) startAnt2=ant1;
	for (Int ant2=startAnt2; ant2<nAnt; ant2++) {
	  reRow++; 
	  if ( isTooLow(ant1) || isTooLow(ant2) ) {
	    msc.flag().put(reRow,trueFlag);
	    msc.flagRow().put(reRow, True);
	    nSubElevation++;
	  }
	}
    }    
    
    Int numpointrows=nAnt;
    MSPointingColumns& pointingc=msc.pointing();
    Int numPointing=pointingc.nrow();
    ms_p->pointing().addRow(numpointrows);
    numpointrows += numPointing;
    Double Tint=qIntegrationTime_p.getValue("s");
    Vector<MDirection> direction(1);
    direction(0)=fieldCenter;
    for (Int m=numPointing; m < (numPointing+nAnt); m++){
      pointingc.numPoly().put(m,0);
      pointingc.interval().put(m,-1);
      pointingc.tracking().put(m,True);
      pointingc.time().put(m,Time);
      pointingc.timeOrigin().put(m,Tstart);
      pointingc.interval().put(m,Tint);
      pointingc.antennaId().put(m, m-numPointing);
      pointingc.directionMeasCol().put(m,direction);
      pointingc.targetMeasCol().put(m,direction);             
    }
    Time+=Tint; 
  }  // time ranges
} // feeds
  
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

Bool NewMSSimulator::calcAntUVW(MEpoch& epoch, MDirection& refdir, 
				      Matrix<Double>& uvwAnt){

  MSColumns msc(*ms_p);
 // Lets define a Measframe with the telescope nominal position
  MPosition obsPos;
  if(!MeasTable::Observatory(obsPos, telescope_p)){
    //not a known observatory then lets use antenna(0) position...as ref pos
    //does not matter really as the difference will make the baseline
    obsPos=msc.antenna().positionMeas()(0);    
  }

  MVPosition basePos=obsPos.getValue();
  MeasFrame measFrame(obsPos);
  measFrame.set(epoch);
  measFrame.set(refdir);
  MVBaseline mvbl;
  MBaseline basMeas;
  MBaseline::Ref basref(MBaseline::ITRF, measFrame);
  basMeas.set(mvbl, basref);
  basMeas.getRefPtr()->set(measFrame);
  // going to convert from ITRF vector to J2000 baseline vector I guess !
  if(refdir.getRef().getType() != MDirection::J2000)
    throw(AipsError("Ref direction is not in  J2000 "));

  Int nAnt=msc.antenna().nrow();
  uvwAnt.resize(3,nAnt);
  MBaseline::Convert elconv(basMeas, MBaseline::Ref(MBaseline::J2000));
  Muvw::Convert uvwconv(Muvw(), Muvw::Ref(Muvw::J2000, measFrame));
  for(Int k=0; k< nAnt; ++k){
    MPosition antpos=msc.antenna().positionMeas()(k);
 
    MVBaseline mvblA(obsPos.getValue(), antpos.getValue());
    basMeas.set(mvblA, basref);
    MBaseline bas2000 =  elconv(basMeas);
    MVuvw uvw2000 (bas2000.getValue(), refdir.getValue());
    const Vector<double>& xyz = uvw2000.getValue();
    uvwAnt.column(k)=xyz;
  }

  return True;

}



} //# NAMESPACE CASA - END

