//# NewMSTable.cc:  the class that hold measurements from telescopes
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
#include <aips/MeasurementSets/NewMSTable.h>
#include <aips/MeasurementSets/NewMeasurementSet.h>
 
#define NewMSTableStatics(ColEnum,KeyEnum) \
SimpleOrderedMap<Int, String> NewMSTable<ColEnum,KeyEnum>::columnMap_p(""); \
SimpleOrderedMap<Int, Int> NewMSTable<ColEnum,KeyEnum>::colDTypeMap_p(TpOther); \
SimpleOrderedMap<Int, String> NewMSTable<ColEnum,KeyEnum>::colCommentMap_p(""); \
SimpleOrderedMap<Int, String> NewMSTable<ColEnum,KeyEnum>::colUnitMap_p(""); \
SimpleOrderedMap<Int, String> \
  NewMSTable<ColEnum,KeyEnum>::colMeasureTypeMap_p(""); \
SimpleOrderedMap<Int, String> NewMSTable<ColEnum,KeyEnum>::keywordMap_p(""); \
SimpleOrderedMap<Int, Int> NewMSTable<ColEnum,KeyEnum>::keyDTypeMap_p(TpOther); \
SimpleOrderedMap<Int, String> NewMSTable<ColEnum,KeyEnum>::keyCommentMap_p(""); \
SimpleCountedConstPtr<TableDesc> NewMSTable<ColEnum,KeyEnum>::requiredTD_p;
 
NewMSTableStatics(NewMS::PredefinedColumns,NewMS::PredefinedKeywords)
NewMSTableStatics(NewMSAntenna::PredefinedColumns,NewMSAntenna::PredefinedKeywords)
NewMSTableStatics(NewMSDataDescription::PredefinedColumns,NewMSDataDescription::PredefinedKeywords)
NewMSTableStatics(NewMSFeed::PredefinedColumns,NewMSFeed::PredefinedKeywords)
NewMSTableStatics(NewMSField::PredefinedColumns,NewMSField::PredefinedKeywords)
NewMSTableStatics(NewMSFlagCmd::PredefinedColumns,NewMSFlagCmd::PredefinedKeywords)
NewMSTableStatics(NewMSFreqOffset::PredefinedColumns,NewMSFreqOffset::PredefinedKeywords)
NewMSTableStatics(NewMSHistory::PredefinedColumns,NewMSHistory::PredefinedKeywords)
NewMSTableStatics(NewMSObservation::PredefinedColumns,NewMSObservation::PredefinedKeywords)
NewMSTableStatics(NewMSPointing::PredefinedColumns,NewMSPointing::PredefinedKeywords)
NewMSTableStatics(NewMSPolarization::PredefinedColumns,NewMSPolarization::PredefinedKeywords)
NewMSTableStatics(NewMSProcessor::PredefinedColumns,NewMSProcessor::PredefinedKeywords)
NewMSTableStatics(NewMSSource::PredefinedColumns,NewMSSource::PredefinedKeywords)
NewMSTableStatics(NewMSSpectralWindow::PredefinedColumns,NewMSSpectralWindow::PredefinedKeywords)
NewMSTableStatics(NewMSState::PredefinedColumns,NewMSState::PredefinedKeywords)
NewMSTableStatics(NewMSSysCal::PredefinedColumns,NewMSSysCal::PredefinedKeywords)
NewMSTableStatics(NewMSWeather::PredefinedColumns,NewMSWeather::PredefinedKeywords)
 
#endif
