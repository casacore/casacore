//# MSTable.cc:  the class that hold measurements from telescopes
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


#if defined(__GNUG__) && (__GNUG__ == 2) && (__GNUC_MINOR__ < 91)
#include <casacore/ms/MeasurementSets/MSTable.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
 
namespace casacore { //# NAMESPACE CASACORE - BEGIN

#define MSTableStatics(ColEnum,KeyEnum) \
SimpleOrderedMap<Int, String> MSTable<ColEnum,KeyEnum>::columnMap_p(""); \
SimpleOrderedMap<Int, Int> MSTable<ColEnum,KeyEnum>::colDTypeMap_p(TpOther); \
SimpleOrderedMap<Int, String> MSTable<ColEnum,KeyEnum>::colCommentMap_p(""); \
SimpleOrderedMap<Int, String> MSTable<ColEnum,KeyEnum>::colUnitMap_p(""); \
SimpleOrderedMap<Int, String> \
  MSTable<ColEnum,KeyEnum>::colMeasureTypeMap_p(""); \
SimpleOrderedMap<Int, String> MSTable<ColEnum,KeyEnum>::keywordMap_p(""); \
SimpleOrderedMap<Int, Int> MSTable<ColEnum,KeyEnum>::keyDTypeMap_p(TpOther); \
SimpleOrderedMap<Int, String> MSTable<ColEnum,KeyEnum>::keyCommentMap_p(""); \
SimpleCountedConstPtr<TableDesc> MSTable<ColEnum,KeyEnum>::requiredTD_p;
 
MSTableStatics(MS::PredefinedColumns,MS::PredefinedKeywords)
MSTableStatics(MSAntenna::PredefinedColumns,MSAntenna::PredefinedKeywords)
MSTableStatics(MSDataDescription::PredefinedColumns,MSDataDescription::PredefinedKeywords)
MSTableStatics(MSFeed::PredefinedColumns,MSFeed::PredefinedKeywords)
MSTableStatics(MSField::PredefinedColumns,MSField::PredefinedKeywords)
MSTableStatics(MSFlagCmd::PredefinedColumns,MSFlagCmd::PredefinedKeywords)
MSTableStatics(MSFreqOffset::PredefinedColumns,MSFreqOffset::PredefinedKeywords)
MSTableStatics(MSHistory::PredefinedColumns,MSHistory::PredefinedKeywords)
MSTableStatics(MSObservation::PredefinedColumns,MSObservation::PredefinedKeywords)
MSTableStatics(MSPointing::PredefinedColumns,MSPointing::PredefinedKeywords)
MSTableStatics(MSPolarization::PredefinedColumns,MSPolarization::PredefinedKeywords)
MSTableStatics(MSProcessor::PredefinedColumns,MSProcessor::PredefinedKeywords)
MSTableStatics(MSSource::PredefinedColumns,MSSource::PredefinedKeywords)
MSTableStatics(MSSpectralWindow::PredefinedColumns,MSSpectralWindow::PredefinedKeywords)
MSTableStatics(MSState::PredefinedColumns,MSState::PredefinedKeywords)
MSTableStatics(MSSysCal::PredefinedColumns,MSSysCal::PredefinedKeywords)
MSTableStatics(MSWeather::PredefinedColumns,MSWeather::PredefinedKeywords)
MSTableStatics(MSDoppler::PredefinedColumns,MSDoppler::PredefinedKeywords)


} //# NAMESPACE CASACORE - END

#endif
