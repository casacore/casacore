//# MSConcat.h: A class for concatenating MeasurementSets.
//# Copyright (C) 2000,2002,2003
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

#ifndef MS_MSCONCAT_H
#define MS_MSCONCAT_H

#include <casacore/casa/aips.h>
#include <casacore/ms/MeasurementSets/MSColumns.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/casa/Arrays/IPosition.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class TableDesc;
class MSMainColumns;
class MSDataDescColumns;
class MSSpWindowColumns;
class MSPolarizationColumns;
class MSAntenna;
class MSDataDescription;
class MSFeed;
class MSField;
class MSPolarization;
class MSSpectralWindow;
template <class T> class Block;

// <summary>A class with functions for concatenating MeasurementSets</summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="yyyy/mm/dd">
// </todo>

class MSConcat: public MSColumns
{
public:
  MSConcat(MeasurementSet& ms);

  void virtualconcat(MeasurementSet& otherMS, 
		     const bool checkShapeAndCateg=true,
		     const String& obsidAndProcAndScanTableName="");

  void concatenate(const MeasurementSet& otherMS,
		   const uint32_t handling=0,   //# 0 (default): complete concat of all tables
                                            //# 1 : don't concatenate the MAIN table
                                            //# 2 : don't concatenate the POINTING table
                                            //# 3 : neither concat MAIN nor POINTING table
                   const String& destMSName=""); //# support for virtual concat

  void setTolerance(Quantum<double>& freqTol, Quantum<double>& dirTol); 
  void setWeightScale(const float weightScale); 
  void setRespectForFieldName(const bool respectFieldName); //# If true, fields of same direction are not merged
                                                            //# if their name is different

private:
  MSConcat();
  static IPosition isFixedShape(const TableDesc& td);
  static IPosition getShape(const MSDataDescColumns& ddCols, 
			    const MSSpWindowColumns& spwCols, 
			    const MSPolarizationColumns& polCols, 
			    uint32_t whichShape);
  void checkShape(const IPosition& otherShape) const;
  void checkCategories(const MSMainColumns& otherCols) const;
  bool checkEphIdInField(const MSFieldColumns& otherFldCol) const;
  bool copyPointing(const MSPointing& otherPoint, const Block<uint32_t>& newAntIndices);
  bool copyPointingB(MSPointing& otherPoint, const Block<uint32_t>& newAntIndices);
  bool copySysCal(const MSSysCal& otherSysCal, const Block<uint32_t>& newAndIndices);
  bool copyWeather(const MSWeather& otherWeather, const Block<uint32_t>& newAndIndices);
  bool copyGainCurve(const MeasurementSet& otherMS, const Block<uint32_t>& newAndIndices);
  bool copyPhaseCal(const MeasurementSet& otherMS, const Block<uint32_t>& newAndIndices);
  int32_t copyObservation(const MSObservation& otherObs, const bool remRedunObsId=true);
                             //# by default remove redundant observation table rows
  int32_t copyProcessor(const MSProcessor& otherObs, const bool remRedunProcId=true);
                             //# by default remove redundant processor table rows
  Block<uint32_t> copyAntennaAndFeed(const MSAntenna& otherAnt,
				 const MSFeed& otherFeed);
  Block<uint32_t> copyState(const MSState& otherState);
  Block<uint32_t> copyField(const MeasurementSet& otherms);
  Block<uint32_t> copySpwAndPol(const MSSpectralWindow& otherSpw,
			    const MSPolarization& otherPol,
			    const MSDataDescription& otherDD);
  bool copySource(const MeasurementSet& otherms);
  bool updateSource();
  bool updateSource2();
  bool sourceRowsEquivalent(const MSSourceColumns& sourceCol, 
			    const rownr_t& rowi, const rownr_t& rowj,
			    const bool dontTestDirection=false,
			    const bool dontTestTransAndRest=false);

  bool obsRowsEquivalent(const MSObservationColumns& obsCol, 
			 const rownr_t& rowi, const rownr_t& rowj);

  bool procRowsEquivalent(const MSProcessorColumns& procCol, 
			 const uint32_t& rowi, const uint32_t& rowj);


  void updateModelDataKeywords(MeasurementSet& ms);

  MeasurementSet itsMS;
  IPosition itsFixedShape;
  Quantum<double> itsFreqTol;
  Quantum<double> itsDirTol;
  float itsWeightScale;
  bool itsRespectForFieldName;
  Vector<bool> itsChanReversed;
  std::map <int32_t, int32_t> newSourceIndex_p;
  std::map <int32_t, int32_t> newSourceIndex2_p;
  std::map <int32_t, int32_t> newSPWIndex_p;
  std::map <int32_t, int32_t> newObsIndexA_p;
  std::map <int32_t, int32_t> newObsIndexB_p;
  std::map <int32_t, int32_t> otherObsIdsWithCounterpart_p;
  std::map <int32_t, int32_t> newProcIndexA_p;
  std::map <int32_t, int32_t> newProcIndexB_p;
  std::map <int32_t, int32_t> solSystObjects_p;

  bool doSource_p;
  bool doSource2_p;
  bool doSPW_p;
  bool doObsA_p;
  bool doObsB_p;
  bool doProcA_p;
  bool doProcB_p;

};

template<class T>
bool areEQ(const ScalarColumn<T>& col, rownr_t row_i, rownr_t row_j) 
{
  T value_i, value_j;
  col.get(row_i, value_i);
  col.get(row_j, value_j);
  return (value_i == value_j);
}

template<class T>
bool areEQ(const ArrayColumn<T>& col, rownr_t row_i, rownr_t row_j) 
{
  bool rval(false);
  Array<T> arr_i;
  Array<T> arr_j;
  
  col.get(row_i, arr_i, true);
  col.get(row_j, arr_j, true);
  size_t ni = arr_i.nelements();
  size_t nj = arr_j.nelements();
  if( (ni==0 && nj==0) ||    // no data is regarded as equal
      allEQ(arr_i, arr_j)){
    rval = true;
  }
  return rval;
}

inline int32_t getMapValue (const std::map<int32_t,int32_t>& m, int32_t k)
{
  auto iter = m.find(k);
  return (iter == m.end()  ?  -1 : iter->second);
}
  

} //# NAMESPACE CASACORE - END

#endif



