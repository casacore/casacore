//# MSTable.cc:  the class that hold measurements from telescopes
//# Copyright (C) 1996,1997,2000,2001,2002
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

#include <ms/MeasurementSets/MSTable.h>
#include <ms/MeasurementSets/MSTable.tcc>
#include <ms/MeasurementSets/MSAntennaEnums.h>
#include <ms/MeasurementSets/MSDataDescEnums.h>
#include <ms/MeasurementSets/MSDopplerEnums.h>
#include <ms/MeasurementSets/MSFeedEnums.h>
#include <ms/MeasurementSets/MSFieldEnums.h>
#include <ms/MeasurementSets/MSFlagCmdEnums.h>
#include <ms/MeasurementSets/MSFreqOffEnums.h>
#include <ms/MeasurementSets/MSHistoryEnums.h>
#include <ms/MeasurementSets/MSMainEnums.h>
#include <ms/MeasurementSets/MSObsEnums.h>
#include <ms/MeasurementSets/MSPointingEnums.h>
#include <ms/MeasurementSets/MSPolEnums.h>
#include <ms/MeasurementSets/MSProcessorEnums.h>
#include <ms/MeasurementSets/MSSourceEnums.h>
#include <ms/MeasurementSets/MSSpWindowEnums.h>
#include <ms/MeasurementSets/MSStateEnums.h>
#include <ms/MeasurementSets/MSSysCalEnums.h>
#include <ms/MeasurementSets/MSWeatherEnums.h>

namespace casa { //# NAMESPACE CASA - BEGIN

  template class MSTable<MSAntennaEnums::PredefinedColumns,
                         MSAntennaEnums::PredefinedKeywords> ;
  template class MSTable<MSDataDescriptionEnums::PredefinedColumns,
                         MSDataDescriptionEnums::PredefinedKeywords> ;
  template class MSTable<MSDopplerEnums::PredefinedColumns,
                         MSDopplerEnums::PredefinedKeywords> ;
  template class MSTable<MSFeedEnums::PredefinedColumns,
                         MSFeedEnums::PredefinedKeywords> ;
  template class MSTable<MSFieldEnums::PredefinedColumns,
                         MSFieldEnums::PredefinedKeywords> ;
  template class MSTable<MSFlagCmdEnums::PredefinedColumns,
                         MSFlagCmdEnums::PredefinedKeywords> ;
  template class MSTable<MSFreqOffsetEnums::PredefinedColumns,
                         MSFreqOffsetEnums::PredefinedKeywords> ;
  template class MSTable<MSHistoryEnums::PredefinedColumns,
                         MSHistoryEnums::PredefinedKeywords> ;
  template class MSTable<MSMainEnums::PredefinedColumns,
                         MSMainEnums::PredefinedKeywords> ;
  template class MSTable<MSObservationEnums::PredefinedColumns,
                         MSObservationEnums::PredefinedKeywords> ;
  template class MSTable<MSPointingEnums::PredefinedColumns,
                         MSPointingEnums::PredefinedKeywords> ;
  template class MSTable<MSPolarizationEnums::PredefinedColumns,
                         MSPolarizationEnums::PredefinedKeywords> ;
  template class MSTable<MSProcessorEnums::PredefinedColumns,
                         MSProcessorEnums::PredefinedKeywords> ;
  template class MSTable<MSSourceEnums::PredefinedColumns,
                         MSSourceEnums::PredefinedKeywords> ;
  template class MSTable<MSSpectralWindowEnums::PredefinedColumns,
                         MSSpectralWindowEnums::PredefinedKeywords> ;
  template class MSTable<MSStateEnums::PredefinedColumns,
                         MSStateEnums::PredefinedKeywords> ;
  template class MSTable<MSSysCalEnums::PredefinedColumns,
                         MSSysCalEnums::PredefinedKeywords> ;
  template class MSTable<MSWeatherEnums::PredefinedColumns,
                         MSWeatherEnums::PredefinedKeywords> ;

} //# NAMESPACE CASA - END

