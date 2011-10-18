//# AntennaResponses.h: AntennaResponses provides access to antenna response data
//# Copyright (C) 1995-1999,2000-2004
//# Associated Universities, Inc. Washington DC, USA
//# Copyright by ESO (in the framework of the ALMA collaboration)
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
//# Correspondence concerning CASA should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: CASA Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#
//# $Id$

//# Includes
#include <casa/aips.h>
#include <images/Images/AntennaResponses.h>
#include <casa/Arrays/Vector.h>
#include <casa/Exceptions/Error.h>
#include <casa/Quanta/MVTime.h>
#include <casa/Quanta/QLogical.h>
#include <casa/Quanta/Quantum.h>
#include <casa/OS/Time.h>
#include <casa/Logging/LogIO.h>
#include <casa/Utilities/GenSort.h>
#include <casa/System/AipsrcValue.h>
#include <casa/BasicSL/String.h>
#include <casa/iostream.h>
#include <measures/TableMeasures/ScalarMeasColumn.h>
#include <measures/TableMeasures/ArrayMeasColumn.h>
#include <measures/TableMeasures/TableMeasValueDesc.h>
#include <measures/TableMeasures/TableMeasOffsetDesc.h>
#include <measures/TableMeasures/TableMeasRefDesc.h>
#include <measures/TableMeasures/TableMeasDesc.h>
#include <measures/TableMeasures/ArrayQuantColumn.h>
#include <measures/Measures/MEpoch.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MFrequency.h>
#include <measures/Measures/MCFrequency.h>
#include <measures/Measures/MCEpoch.h>
#include <measures/Measures/MCDirection.h>
#include <measures/Measures/MCPosition.h>
#include <measures/Measures/MeasTable.h>
#include <measures/Measures/MeasData.h>
#include <measures/Measures/MeasRef.h>
#include <measures/Measures/MeasFrame.h>
#include <measures/Measures/MeasConvert.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/TableDesc.h>
#include <tables/Tables/ArrayColumn.h>
#include <tables/Tables/ColumnDesc.h>
#include <tables/Tables/ArrColDesc.h>
#include <tables/Tables/SetupNewTab.h>
#include <tables/Tables/ScaColDesc.h>
#include <tables/Tables/ScalarColumn.h>

namespace casa { //# NAMESPACE CASA - BEGIN

  AntennaResponses::AntennaResponses(const String& path){
    init(path);
  }

  Bool AntennaResponses::init(const String& path){
    // reset members to empty

    paths_p.resize(); 

    numRows_p = 0;
    ObsName_p.resize();
    StartTime_p.resize();
    AntennaType_p.resize();
    ReceiverType_p.resize();
    BeamId_p.resize();
    BeamNumber_p.resize();
    ValidCenter_p.resize();
    ValidCenterMin_p.resize();
    ValidCenterMax_p.resize();
    NumSubbands_p.resize();
    BandName_p.resize();
    SubbandMinFreq_p.resize();
    SubbandMaxFreq_p.resize();
    FuncType_p.resize();
    FuncName_p.resize();
    FuncChannel_p.resize();
    NomFreq_p.resize();
    RotAngOffset_p.resize();

    pathIndex_p.resize();

    if(path==""){
      paths_p.resize(1, "");
      return True;
    }
    else{
      // fill members from table on disk
      return append(path);
    }
  }


  Bool AntennaResponses::append(const String& path){

    if(isInit(path)){
      // Returns False if the path was already read before.
      //cout << "Path has been read before." << endl;
      return False;
    }
    
    // open table
    Table tab(path);    

    // get number of rows
    uInt numRows = tab.nrow();

    uInt nPaths = paths_p.nelements();

    if(numRows>0){
      // read columns and append to vectors;

      ROScalarColumn<Int> beamIdCol(tab, "BEAM_ID");
      ROScalarColumn<String> nameCol(tab, "NAME");
      ROScalarColumn<Int> beamNumberCol(tab, "BEAM_NUMBER");
      ROScalarMeasColumn<MEpoch> startTimeCol(tab, "START_TIME");
      ROScalarColumn<String> antennaTypeCol(tab, "ANTENNA_TYPE");
      ROScalarColumn<String> receiverTypeCol(tab, "RECEIVER_TYPE");
      ROScalarColumn<Int> numSubbandsCol(tab, "NUM_SUBBANDS");
      ROArrayColumn<String> bandNameCol(tab, "BAND_NAME");
      ROArrayQuantColumn<Double> subbandMinFreqCol(tab, "SUBBAND_MIN_FREQ");
      ROArrayQuantColumn<Double> subbandMaxFreqCol(tab, "SUBBAND_MAX_FREQ");
      ROScalarMeasColumn<MDirection> centerCol(tab, "CENTER");
      ROScalarMeasColumn<MDirection> validCenterMinCol(tab, "VALID_CENTER_MIN");
      ROScalarMeasColumn<MDirection> validCenterMaxCol(tab, "VALID_CENTER_MAX");
      ROArrayColumn<Int> functionTypeCol(tab, "FUNCTION_TYPE");   
      ROArrayColumn<String> functionNameCol(tab, "FUNCTION_NAME");
      ROArrayColumn<uInt> functionChannelCol(tab, "FUNCTION_CHANNEL");
      ROArrayQuantColumn<Double> nomFreqCol(tab, "NOMINAL_FREQ");
      ROArrayQuantColumn<Double> rotAngOffsetCol(tab, "RESPONSE_ROTATION_OFFSET");

      numRows_p += numRows;

      ObsName_p.resize(numRows_p,True);
      StartTime_p.resize(numRows_p,True);
      AntennaType_p.resize(numRows_p,True);
      ReceiverType_p.resize(numRows_p,True);
      BeamId_p.resize(numRows_p,True);
      BeamNumber_p.resize(numRows_p,True);
      ValidCenter_p.resize(numRows_p,True);
      ValidCenterMin_p.resize(numRows_p,True);
      ValidCenterMax_p.resize(numRows_p,True);
      NumSubbands_p.resize(numRows_p,True);
      BandName_p.resize(numRows_p,True);
      SubbandMinFreq_p.resize(numRows_p,True);
      SubbandMaxFreq_p.resize(numRows_p,True);
      FuncType_p.resize(numRows_p,True);
      FuncName_p.resize(numRows_p,True);
      FuncChannel_p.resize(numRows_p,True);
      NomFreq_p.resize(numRows_p,True);
      RotAngOffset_p.resize(numRows_p,True);

      pathIndex_p.resize(numRows_p,True);

      for(uInt i=0; i<numRows; i++){
	uInt j = i + numRows_p - numRows;
	BeamId_p(j) = beamIdCol(i);
	ObsName_p(j) = nameCol(i);
	BeamNumber_p(j) = beamNumberCol(i);
	StartTime_p(j) = startTimeCol(i);
	AntennaType_p(j) = antennaTypeCol(i);
	ReceiverType_p(j) = receiverTypeCol(i);
	NumSubbands_p(j) = numSubbandsCol(i);
	BandName_p(j).assign(bandNameCol(i));

	Vector<Quantity> tQ;
	tQ = subbandMinFreqCol(i);
	uInt nSubB = tQ.nelements();
	SubbandMinFreq_p(j).resize(nSubB);
	for(uInt k=0; k<nSubB; k++){
	  SubbandMinFreq_p(j)(k) = MVFrequency(tQ(k));
	}
	tQ = subbandMaxFreqCol(i);
	SubbandMaxFreq_p(j).resize(nSubB);
	for(uInt k=0; k<nSubB; k++){
	  SubbandMaxFreq_p(j)(k) = MVFrequency(tQ(k));
	}
	tQ = nomFreqCol(i);
	NomFreq_p(j).resize(nSubB);
	for(uInt k=0; k<nSubB; k++){
	  NomFreq_p(j)(k) = MVFrequency(tQ(k));
	}
	tQ = rotAngOffsetCol(i);
	RotAngOffset_p(j).resize(nSubB);
	for(uInt k=0; k<nSubB; k++){
	  RotAngOffset_p(j)(k) = MVAngle(tQ(k));
	}

	ValidCenter_p(j) = centerCol(i);
	ValidCenterMin_p(j) = validCenterMinCol(i);
	ValidCenterMax_p(j) = validCenterMaxCol(i);

	Vector<Int> tFType;
	tFType = functionTypeCol(i);
	FuncType_p(j).resize(nSubB);
	for(uInt k=0; k<nSubB; k++){
	  FuncType_p(j)(k) = FuncType(tFType(k));
	}

	FuncName_p(j).assign(functionNameCol(i));
	FuncChannel_p(j).assign(functionChannelCol(i));

	pathIndex_p(j) = nPaths;
	
      }

    } // end if

    paths_p.resize(nPaths+1, True);
    String tempS = path;
    while(tempS.lastchar()=='/' && tempS.size()>1){ // don't want trailing "/"
      tempS.erase(tempS.size()-1,1);
    }
    paths_p(nPaths) = path;

    return True;

  }

  Bool AntennaResponses::isInit(){

    return (paths_p.nelements()!=0);
  }

  Bool AntennaResponses::isInit(const String& path){
    Bool found = False;
    for (uInt i=0; i<paths_p.nelements(); i++){
      if(paths_p(i) == path){
	found = True;
      }
    }
    return found;
  }

  Bool AntennaResponses::getRowAndIndex(uInt& row, uInt& subBand,
					const String& obsName,
					const MEpoch& obsTime,
					const MFrequency& freq,
					const FuncTypes& requFType,
					const String& antennaType,
					const MDirection& center,
					const String& receiverType,
					const Int& beamNumber){
    Bool rval = False;
    Unit uS("s");
    Unit uHz("Hz");

    // calculate azimuth, elevation, and topo frequency
    // first, get the reference frame 
    MPosition mp;
    if (!MeasTable::Observatory(mp,obsName)) {
      // unknown observatory
      LogIO os(LogOrigin("AntennaResponses",
			 String("getRowAndIndex"),
			 WHERE));
      os << LogIO::NORMAL << String("Unknown observatory ") << obsName 
	 << LogIO::POST;
      return False;
    }
    mp=MPosition::Convert(mp, MPosition::ITRF)();
    MeasFrame frame(mp, obsTime);
    Vector<Double> azel = MDirection::Convert(center, 
					      MDirection::Ref(MDirection::AZEL,
							      frame)		
			      )().getAngle("deg").getValue();

    Quantity f = MFrequency::Convert(freq, MFrequency::TOPO)().get(uHz);

    

    // loop over rows
    uInt i,j;
    vector<uInt> rowV, subBandV;
    vector<Quantity> timeV;
    for(i=0; i<numRows_p; i++){
      if(ObsName_p(i) == obsName
	 && StartTime_p(i).get(uS) <= obsTime.get(uS)
	 && AntennaType_p(i) == antennaType
	 && ReceiverType_p(i) == receiverType
	 && BeamNumber_p(i) == beamNumber
	 ){
	// remains to test center and freq and functype
	// first freq and functype
	Bool found = False;
	for(j=0; j<NumSubbands_p(i); j++){
	  //cout << "f " << f << " min " << SubbandMinFreq_p(i)(j).get() 
	  //     << " max " << SubbandMaxFreq_p(i)(j).get() << endl;
	  if( (FuncType_p(i)(j) == requFType
	       || requFType == AntennaResponses::ANY)
	      && SubbandMinFreq_p(i)(j).get() <= f
	      && f <= SubbandMaxFreq_p(i)(j).get()
	      ){
	    found = True;
	    break;
	  }
	}
	if(found){ // now test center
	  Vector<Double> azelMin = MDirection::Convert(ValidCenterMin_p(i), 
						       MDirection::Ref(MDirection::AZEL,
								       frame)
						       )().getAngle("deg").getValue();
	  Vector<Double> azelMax = MDirection::Convert(ValidCenterMax_p(i), 
						       MDirection::Ref(MDirection::AZEL,
								       frame)
						       )().getAngle("deg").getValue();

	  // need to accomodate the ambiguity of the AZ
	  Double modAz = fmod(azel(0) - azelMin(0),360.);
	  Double modAzMax = fmod(azelMax(0) - azelMin(0), 360.);

// 	  cout << " i, j, " << i << ", " << j << " az min actual max" << azelMin(0) << " " 
// 	       << azel(0) << " " << azelMax(0) << endl;
// 	  cout << " i, j, " << i << ", " << j << " cannonised az  actual max" << modAz << " " << modAzMax << endl;
// 	  cout << " i, j, " << i << ", " << j << " el min actual max" << azelMin(1) << " " << azel(1) << " " << azelMax(1) << endl;

	  if((fabs(azelMin(0)-azelMax(0))<1e-5 // all AZ are valid (accomodate numerical problems at 360 deg)
	      || ( 0. <= modAz  && modAz <= modAzMax))
	     && azelMin(1) <= azel(1) && azel(1) <= azelMax(1)){
	    // memorize the applicable row, sub band, and time
	    rval = True;
	    rowV.push_back(i);
	    subBandV.push_back(j);
	    timeV.push_back(StartTime_p(i).get(uS));
	  }
	}
      }
    } // end for

    if(rval){
      Vector<uInt> sortIndex;
      GenSortIndirect<Quantity>::sort(sortIndex, Vector<Quantity>(timeV));
      // take the latest row
      row = rowV[sortIndex(sortIndex.nelements()-1)];
      subBand = subBandV[sortIndex(sortIndex.nelements()-1)];
    }
    return rval;

  }

  Bool AntennaResponses::getRowAndIndex(uInt& row, uInt& subBand,
					const String& obsName,
					const Int& beamId,
					const MFrequency& freq){

    Bool rval = False;
    Unit uHz("Hz");

    // calculate topo frequency
    Quantity f = MFrequency::Convert(freq, MFrequency::TOPO)().get(uHz);

    // loop over rows
    uInt i,j;
    for(i=0; i<numRows_p; i++){
      if(ObsName_p(i) == obsName
	 && BeamNumber_p(i) == beamId
	){
	for(j=0; j<NumSubbands_p(i); j++){
	  if(SubbandMinFreq_p(i)(j).get() <= f
	     && f <= SubbandMaxFreq_p(i)(j).get()
	     ){
	    rval = True;
	    break;
	  }
	}
	if(rval){
	  break;
	}
      }
    } // end for

    if(rval){
      row = i;
      subBand = j;
    }
    return rval;

  }


  Bool AntennaResponses::getImageName(String& functionImageName, // the path to the image
				      uInt& functionChannel, // the channel to use
				      MFrequency& nomFreq, // nominal frequency of the image (at the given channel)
				      FuncTypes& fType, // the function type of the image
				      MVAngle& rotAngOffset, // response rotation angle offset
				      const String& obsName, // (the observatory name, e.g. "ALMA" or "ACA")
				      const MEpoch& obsTime,
				      const MFrequency& freq,
				      const FuncTypes& requFType, // the requested function type
				      const String& antennaType,
				      const MDirection& center,
				      const String& receiverType,
				      const Int& beamNumber){
    
    uInt row, subBand;

    if(!getRowAndIndex(row, subBand,
		       obsName, obsTime, freq,
		       requFType, antennaType,
		       center, receiverType,
		       beamNumber)){
      return False;
    }
    else{
      functionImageName = FuncName_p(row)(subBand);
      if(functionImageName.firstchar()!='/'){ // need to prepend the path
	String tempS = paths_p(pathIndex_p(row));
	string::size_type p = tempS.find_last_of('/',tempS.size());
	functionImageName = tempS.substr(0,p) + "/" + functionImageName;
      }
      functionChannel = FuncChannel_p(row)(subBand);
      nomFreq = MFrequency(NomFreq_p(row)(subBand),MFrequency::TOPO);
      fType = FuncType_p(row)(subBand);
      rotAngOffset = RotAngOffset_p(row)(subBand);
      return True;
    }

  }
		
  // overloaded method: as previous method but using beamId
  // (instead of obs. time, ant. type,  rec. type, and center)
  Bool AntennaResponses::getImageName(String& functionImageName,
				      uInt& functionChannel,
				      MFrequency& nomFreq, // nominal frequency of the image
				      FuncTypes& fType, // the function type of the image
				      MVAngle& rotAngOffset, // response rotation angle offset
				      const String& obsName, // (the observatory name, e.g. "ALMA" or "ACA")
				      const Int& beamId,
				      const MFrequency& freq){
    uInt row, subBand;

    if(!getRowAndIndex(row, subBand,
		       obsName, beamId, freq)){
      return False;
    }
    else{
//       cout << "row " << row << " subband " << subBand << endl;
//       cout << "numrows " << numRows_p << endl;
//       for(uInt i=0; i<FuncName_p.nelements(); i++){
// 	for(uInt j=0; j< FuncName_p(i).nelements(); j++){
// 	  cout << "i, j " << i << ", " << j << " " << FuncName_p(i)(j) << endl;
// 	}
//       }
      functionImageName = FuncName_p(row)(subBand);
      if(functionImageName.firstchar()!='/'){ // need to prepend the path
	String tempS = paths_p(pathIndex_p(row));
	string::size_type p = tempS.find_last_of('/',tempS.size());
	functionImageName = tempS.substr(0,p) + "/" + functionImageName;
      }
      functionChannel = FuncChannel_p(row)(subBand);
      nomFreq = MFrequency(NomFreq_p(row)(subBand),MFrequency::TOPO);
      fType = FuncType_p(row)(subBand);
      rotAngOffset = RotAngOffset_p(row)(subBand);
      return True;
    }
  }


  Bool AntennaResponses::putRow(uInt& row,
				const String& obsName,
				const Int& beamId,
				const Vector<String>& bandName,
				const Vector<MVFrequency>& subbandMinFreq,
				const Vector<MVFrequency>& subbandMaxFreq,
				const Vector<FuncTypes>& funcType,
				const Vector<String>& funcName,
				const Vector<uInt>& funcChannel,
				const Vector<MVFrequency>& nomFreq,
				const Vector<MVAngle>& rotAngOffset,
				const String& antennaType,
				const MEpoch& startTime,
				const MDirection& center,
				const MDirection& validCenterMin,
				const MDirection& validCenterMax,
				const String& receiverType,
				const Int& beamNumber){
    // Put the given row into the present antenna reponses table (in memory).

    // Returns false, if the table was not initialised or the given data was
    // not consistent.
    if(paths_p.nelements()==0){
      LogIO os(LogOrigin("AntennaResponses",
			 String("putRow"),
			 WHERE));
      os << LogIO::NORMAL << String("Table not initialized.") << obsName 
	 << LogIO::POST;
      return False;
    }
    // Consistency checks: 
    //   - all vectors have same dimension which is then used to set numSubbands
    uInt tNumSubbands = bandName.nelements();
    if(!(
	 tNumSubbands == subbandMinFreq.nelements() &&
	 tNumSubbands == subbandMaxFreq.nelements() &&
	 tNumSubbands == funcType.nelements() &&
	 tNumSubbands == funcName.nelements() &&
	 tNumSubbands == funcChannel.nelements() &&
	 tNumSubbands == nomFreq.nelements() &&
	 tNumSubbands == rotAngOffset.nelements())
       ){
      LogIO os(LogOrigin("AntennaResponses", String("putRow"), WHERE));
      os << LogIO::NORMAL << String("Inconsistent vector dimensions.") << obsName 
	 << LogIO::POST;
      return False;
    }
    //   - beamId is unique for the given observatory
    Bool isUnique = True;
    for(uInt i=0; i<numRows_p; i++){
      if(ObsName_p(i)==obsName && BeamId_p(i)==beamId && row!=i){
	isUnique = False;
	break;
      }
    }
    if(!isUnique){
      LogIO os(LogOrigin("AntennaResponses", String("putRow"), WHERE));
      os << LogIO::WARN << "Beam id " <<  beamId << " not unique." 
	 << LogIO::POST;
      return False;
    }
    //   - center, validCenterMin, and validCenterMax have the same MDirection type
    String dirRef = center.getRefString();
    if(!(dirRef == validCenterMin.getRefString() &&
	 dirRef == validCenterMax.getRefString())
       ){
      LogIO os(LogOrigin("AntennaResponses",
			 String("putRow"),
			 WHERE));
      os << LogIO::WARN << "Inconsistent direction type." 
	 << LogIO::POST;
      return False;
    }
    
    uInt theRow = 0;

    // If the row exists at the position given by uInt row, it is overwritten.
    if(row<numRows_p){
      theRow = row;
    }
    else{ // If it doesn't exist, the table is resized by one in memory and the new
      // row is added at the last position. The variable "row" then contains the
      // actual row that was filled.
      theRow = row = numRows_p;
      numRows_p++;
      ObsName_p.resize(numRows_p,True);
      StartTime_p.resize(numRows_p,True);
      AntennaType_p.resize(numRows_p,True);
      ReceiverType_p.resize(numRows_p,True);
      BeamId_p.resize(numRows_p,True);
      BeamNumber_p.resize(numRows_p,True);
      ValidCenter_p.resize(numRows_p,True);
      ValidCenterMin_p.resize(numRows_p,True);
      ValidCenterMax_p.resize(numRows_p,True);
      NumSubbands_p.resize(numRows_p,True);
      BandName_p.resize(numRows_p,True);
      SubbandMinFreq_p.resize(numRows_p,True);
      SubbandMaxFreq_p.resize(numRows_p,True);
      FuncType_p.resize(numRows_p,True);
      FuncName_p.resize(numRows_p,True);
      FuncChannel_p.resize(numRows_p,True);
      NomFreq_p.resize(numRows_p,True);
      RotAngOffset_p.resize(numRows_p,True);
    }  

    ObsName_p(theRow) = obsName;
    StartTime_p(theRow) = startTime;
    AntennaType_p(theRow) = antennaType;
    ReceiverType_p(theRow) = receiverType;
    BeamId_p(theRow) = beamId;
    BeamNumber_p(theRow) = beamNumber;
    ValidCenter_p(theRow) = center;
    ValidCenterMin_p(theRow) = validCenterMin;
    ValidCenterMax_p(theRow) = validCenterMax;
    NumSubbands_p(theRow) = tNumSubbands;
    BandName_p(theRow).assign(bandName);
    SubbandMinFreq_p(theRow).assign(subbandMinFreq);
    SubbandMaxFreq_p(theRow).assign(subbandMaxFreq);
    FuncType_p(theRow).assign(funcType);
    FuncName_p(theRow).assign(funcName);
    FuncChannel_p(theRow).assign(funcChannel);
    NomFreq_p(theRow).assign(nomFreq);
    RotAngOffset_p(theRow).assign(rotAngOffset);

    return True;

  }


  void AntennaResponses::create(const String& path){

    // set up table description

    TableDesc tD("AntennaResponsesDesc", TableDesc::New);
    tD.comment() = "antenna responses table";

    tD.addColumn(ScalarColumnDesc<Int> ("BEAM_ID", "unique for the given observatory name"));
    tD.addColumn(ScalarColumnDesc<String> ("NAME", "name of the observatory as in the Observatories table"));
    tD.addColumn(ScalarColumnDesc<Int> ("BEAM_NUMBER", "for observataories which support several simultaneous beams, zero-based"));
    tD.addColumn(ScalarColumnDesc<Double> ("START_TIME", "the time from which onwards this table row is valid, measure fixed to UTC"));
    tD.addColumn(ScalarColumnDesc<String> ("ANTENNA_TYPE", "for heterogeneous arrays: indication of the antenna type"));
    tD.addColumn(ScalarColumnDesc<String> ("RECEIVER_TYPE", "permits multiple receivers per band"));
    tD.addColumn(ScalarColumnDesc<Int> ("NUM_SUBBANDS", "number of elements in the array columns in this table"));
    tD.addColumn(ArrayColumnDesc<String> ("BAND_NAME", "name of the frequency band"));
    tD.addColumn(ArrayColumnDesc<Double> ("SUBBAND_MIN_FREQ", "minimum frequency of the subband in the observatory frame"));
    tD.addColumn(ArrayColumnDesc<Double> ("SUBBAND_MAX_FREQ", "maximum frequency of the subband in the observatory frame"));
    tD.addColumn(ArrayColumnDesc<Double> ("CENTER", "the nominal center sky position where this row is valid"));
    tD.addColumn(ScalarColumnDesc<Int> ("CENTER_REF", ColumnDesc::Direct));
    tD.addColumn(ArrayColumnDesc<Double> ("VALID_CENTER_MIN", "sky position validity range min values"));
    tD.addColumn(ArrayColumnDesc<Double> ("VALID_CENTER_MAX", "sky position validity range max values"));
    tD.addColumn(ArrayColumnDesc<Int> ("FUNCTION_TYPE"));
    tD.addColumn(ArrayColumnDesc<String> ("FUNCTION_NAME"));
    tD.addColumn(ArrayColumnDesc<uInt> ("FUNCTION_CHANNEL"));
    tD.addColumn(ArrayColumnDesc<Double> ("NOMINAL_FREQ"));
    tD.addColumn(ArrayColumnDesc<Double> ("RESPONSE_ROTATION_OFFSET"));

    // Add TableMeasures information for designated Measures/Quanta columns

    TableMeasValueDesc timeMeasVal(tD, "START_TIME");
    TableMeasRefDesc timeMeasRef(MEpoch::DEFAULT);
    TableMeasDesc<MEpoch> timeMeasCol(timeMeasVal, timeMeasRef);
    timeMeasCol.write(tD);

    TableQuantumDesc timeQuantDesc(tD, "START_TIME", Unit ("s"));
    timeQuantDesc.write(tD);

    TableQuantumDesc freqQuantDesc(tD, "SUBBAND_MIN_FREQ", Unit ("Hz"));
    freqQuantDesc.write (tD);
    TableQuantumDesc freqQuantDesc2(tD, "SUBBAND_MAX_FREQ", Unit ("Hz"));
    freqQuantDesc2.write (tD);
    TableQuantumDesc freqQuantDesc3(tD, "NOMINAL_FREQ", Unit ("Hz"));
    freqQuantDesc3.write (tD);
    TableQuantumDesc angQuantDesc(tD, "RESPONSE_ROTATION_OFFSET", Unit ("deg"));
    angQuantDesc.write (tD);

    TableMeasValueDesc refDirMeasVal (tD, "CENTER");
    TableMeasRefDesc refDirMeasRef (tD, "CENTER_REF");
    TableMeasDesc<MDirection> refDirMeasCol (refDirMeasVal, refDirMeasRef);
    refDirMeasCol.write(tD);
    TableMeasValueDesc refDirMeasValMin (tD, "VALID_CENTER_MIN");
    TableMeasDesc<MDirection> refDirMeasColMin (refDirMeasValMin, refDirMeasRef);
    refDirMeasColMin.write(tD);
    TableMeasValueDesc refDirMeasValMax (tD, "VALID_CENTER_MAX");
    TableMeasDesc<MDirection> refDirMeasColMax (refDirMeasValMax, refDirMeasRef);
    refDirMeasColMax.write(tD);

    // create the table
    SetupNewTable newtab (path, tD, Table::New);
    Table tab(newtab, numRows_p);

    if(numRows_p>0){
      // fill the table

      ScalarColumn<Int> beamIdCol(tab, "BEAM_ID");
      ScalarColumn<String> nameCol(tab, "NAME");
      ScalarColumn<Int> beamNumberCol(tab, "BEAM_NUMBER");
      ScalarMeasColumn<MEpoch> startTimeCol(tab, "START_TIME");
      ScalarColumn<String> antennaTypeCol(tab, "ANTENNA_TYPE");
      ScalarColumn<String> receiverTypeCol(tab, "RECEIVER_TYPE");
      ScalarColumn<Int> numSubbandsCol(tab, "NUM_SUBBANDS");
      ArrayColumn<String> bandNameCol(tab, "BAND_NAME");
      ArrayQuantColumn<Double> subbandMinFreqCol(tab, "SUBBAND_MIN_FREQ");
      ArrayQuantColumn<Double> subbandMaxFreqCol(tab, "SUBBAND_MAX_FREQ");
      ScalarMeasColumn<MDirection> centerCol(tab, "CENTER");
      ScalarMeasColumn<MDirection> validCenterMinCol(tab, "VALID_CENTER_MIN");
      ScalarMeasColumn<MDirection> validCenterMaxCol(tab, "VALID_CENTER_MAX");
      ArrayColumn<Int> functionTypeCol(tab, "FUNCTION_TYPE");   
      ArrayColumn<String> functionNameCol(tab, "FUNCTION_NAME");
      ArrayColumn<uInt> functionChannelCol(tab, "FUNCTION_CHANNEL");
      ArrayQuantColumn<Double> nomFreqCol(tab, "NOMINAL_FREQ");
      ArrayQuantColumn<Double> rotAngOffsetCol(tab, "RESPONSE_ROTATION_OFFSET");

      for(uInt i=0; i<numRows_p; i++){
	beamIdCol.put(i, BeamId_p(i));
	nameCol.put(i, ObsName_p(i));
	beamNumberCol.put(i, BeamNumber_p(i));
	startTimeCol.put(i, StartTime_p(i));
	antennaTypeCol.put(i, AntennaType_p(i));
	receiverTypeCol.put(i, ReceiverType_p(i));
	numSubbandsCol.put(i, NumSubbands_p(i));
	bandNameCol.put(i, BandName_p(i));

	Vector<Quantity> bMF(SubbandMinFreq_p(i).nelements());
	for(uInt k=0; k<bMF.nelements(); k++){
	  bMF(k) = (SubbandMinFreq_p(i)(k)).get(); // convert MVFrequency to Quantity in Hz
	}
	subbandMinFreqCol.put(i, bMF); 

	for(uInt k=0; k<bMF.nelements(); k++){
	  bMF(k) = (SubbandMaxFreq_p(i)(k)).get(); // convert MVFrequency to Quantity in Hz
	}
	subbandMaxFreqCol.put(i, bMF);

	for(uInt k=0; k<bMF.nelements(); k++){
	  bMF(k) = (NomFreq_p(i)(k)).get(); // convert MVFrequency to Quantity in Hz
	}
	nomFreqCol.put(i, bMF);

	for(uInt k=0; k<bMF.nelements(); k++){
	  bMF(k) = (RotAngOffset_p(i)(k)).get(Unit("deg")); // convert MVAngle to Quantity in deg
	}
	rotAngOffsetCol.put(i, bMF);

	centerCol.put(i, ValidCenter_p(i));
	validCenterMinCol.put(i, ValidCenterMin_p(i));
	validCenterMaxCol.put(i, ValidCenterMax_p(i));

	Vector<Int> iFT(FuncType_p(i).nelements());
	for(uInt k=0; k<iFT.nelements(); k++){
	  iFT(k) = static_cast<Int>(FuncType_p(i)(k));
	}
	functionTypeCol.put(i, iFT);

	functionNameCol.put(i, FuncName_p(i));
	functionChannelCol.put(i, FuncChannel_p(i));
      }

    }

    return;

  }

  AntennaResponses::FuncTypes AntennaResponses::FuncType(Int i){
    if(-1 < i && i < static_cast<Int>(AntennaResponses::N_FuncTypes) ){
      return static_cast<AntennaResponses::FuncTypes>(i);
    }
    else{
      return AntennaResponses::INVALID;
    }
  }

  AntennaResponses::FuncTypes AntennaResponses::FuncType(const String& sftyp){

    if(sftyp=="NA") return AntennaResponses::NA;
    if(sftyp=="AIF") return AntennaResponses::AIF;
    if(sftyp=="EFP") return AntennaResponses::EFP;
    if(sftyp=="VP") return AntennaResponses::VP;
    if(sftyp=="VPMAN") return AntennaResponses::VPMAN;
    if(sftyp=="INTERNAL") return AntennaResponses::INTERNAL;
    return AntennaResponses::INVALID;

  }

  Bool AntennaResponses::getBandName(String& bandName, 
				     const String& obsName,
				     const MVFrequency& freq){    
    // brute force search
    Quantity f = freq.get();
    bandName = "";
    Bool rval = False;

    uInt i, j;

    for(i=0; i<numRows_p; i++){
      if(obsName == ObsName_p(i)){
	for(j=0; j<NumSubbands_p(i); j++){
	  if(SubbandMinFreq_p(i)(j).get() <= f
	     && f <= SubbandMaxFreq_p(i)(j).get()){
	    rval = True;
	    break;
	  }
	}
	if(rval){
	  break;
	}
      }
    }
    if(rval){
      bandName = BandName_p(i)(j);
    }
    return rval;

  }

} //# NAMESPACE CASA - END

