//# MSSysCalColumns.cc:  provides easy access to MeasurementSet columns
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

#include <casacore/ms/MeasurementSets/MSSysCalColumns.h>
#include <casacore/ms/MeasurementSets/MSSysCal.h>
#include <casacore/tables/Tables/ColDescSet.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSSysCalColumns::MSSysCalColumns():
  isNull_p(True)
{
}

MSSysCalColumns::MSSysCalColumns(const MSSysCal& msSysCal):
  isNull_p(True)
{
  attach(msSysCal);
}

MSSysCalColumns::~MSSysCalColumns() {}

void MSSysCalColumns::attach(const MSSysCal& msSysCal)
{
  isNull_p = msSysCal.isNull();
  if (!isNull()) {
    antennaId_p.attach(msSysCal, MSSysCal::
		       columnName(MSSysCal::ANTENNA_ID));
    feedId_p.attach(msSysCal, MSSysCal::columnName(MSSysCal::FEED_ID));
    interval_p.attach(msSysCal, MSSysCal::
		      columnName(MSSysCal::INTERVAL));
    spectralWindowId_p.attach(msSysCal, MSSysCal::
			      columnName(MSSysCal::SPECTRAL_WINDOW_ID));
    time_p.attach(msSysCal, MSSysCal::columnName(MSSysCal::TIME));
    timeMeas_p.attach(msSysCal, MSSysCal::columnName(MSSysCal::TIME));
    intervalQuant_p.attach(msSysCal, MSSysCal::
			   columnName(MSSysCal::INTERVAL));
    timeQuant_p.attach(msSysCal, MSSysCal::columnName(MSSysCal::TIME));
    const ColumnDescSet& cds = msSysCal.tableDesc().columnDescSet();
    const String& phaseDiff = MSSysCal::columnName(MSSysCal::PHASE_DIFF);
    if (cds.isDefined(phaseDiff)) {
      phaseDiff_p.attach(msSysCal, phaseDiff);
      phaseDiffQuant_p.attach(msSysCal, phaseDiff);
    }
    const String& phaseDiffFlag =
      MSSysCal::columnName(MSSysCal::PHASE_DIFF_FLAG);
    if (cds.isDefined(phaseDiffFlag)) {
      phaseDiffFlag_p.attach(msSysCal, phaseDiffFlag);
    }
    const String& tant = MSSysCal::columnName(MSSysCal::TANT);
    if (cds.isDefined(tant)) {
      tant_p.attach(msSysCal, tant);
      tantQuant_p.attach(msSysCal, tant);
    }
    const String& tantFlag = MSSysCal::columnName(MSSysCal::TANT_FLAG);
    if (cds.isDefined(tantFlag)) tantFlag_p.attach(msSysCal, tantFlag);
    const String& tantSpectrum =
      MSSysCal::columnName(MSSysCal::TANT_SPECTRUM);
    if (cds.isDefined(tantSpectrum)) {
      tantSpectrum_p.attach(msSysCal, tantSpectrum);
      tantSpectrumQuant_p.attach(msSysCal, tantSpectrum);
    }
    const String& tantTsys = MSSysCal::columnName(MSSysCal::TANT_TSYS);
    if (cds.isDefined(tantTsys)) tantTsys_p.attach(msSysCal, tantTsys);
    const String& tantTsysFlag =
      MSSysCal::columnName(MSSysCal::TANT_TSYS_FLAG);
    if (cds.isDefined(tantTsysFlag)) {
      tantTsysFlag_p.attach(msSysCal, tantTsysFlag);
    }
    const String& tantTsysSpectrum =
      MSSysCal::columnName(MSSysCal::TANT_TSYS_SPECTRUM);
    if (cds.isDefined(tantTsysSpectrum)) {
      tantTsysSpectrum_p.attach(msSysCal, tantTsysSpectrum);
    }
    const String& tcal = MSSysCal::columnName(MSSysCal::TCAL);
    if (cds.isDefined(tcal)) {
      tcal_p.attach(msSysCal, tcal);
      tcalQuant_p.attach(msSysCal, tcal);
    }
    const String& tcalFlag = MSSysCal::columnName(MSSysCal::TCAL_FLAG);
    if (cds.isDefined(tcalFlag)) tcalFlag_p.attach(msSysCal, tcalFlag);
    const String& tcalSpectrum =
      MSSysCal::columnName(MSSysCal::TCAL_SPECTRUM);
    if (cds.isDefined(tcalSpectrum)) {
      tcalSpectrum_p.attach(msSysCal, tcalSpectrum);
      tcalSpectrumQuant_p.attach(msSysCal, tcalSpectrum);
    }
    const String& trx = MSSysCal::columnName(MSSysCal::TRX);
    if (cds.isDefined(trx)) {
      trx_p.attach(msSysCal, trx);
      trxQuant_p.attach(msSysCal, trx);
    }
    const String& trxFlag = MSSysCal::columnName(MSSysCal::TRX_FLAG);
    if (cds.isDefined(trxFlag)) trxFlag_p.attach(msSysCal, trxFlag);
    const String& trxSpectrum =
      MSSysCal::columnName(MSSysCal::TRX_SPECTRUM);
    if (cds.isDefined(trxSpectrum)) {
      trxSpectrum_p.attach(msSysCal, trxSpectrum);
      trxSpectrumQuant_p.attach(msSysCal, trxSpectrum);
    }
    const String& tsky = MSSysCal::columnName(MSSysCal::TSKY);
    if (cds.isDefined(tsky)) {
      tsky_p.attach(msSysCal, tsky);
      tskyQuant_p.attach(msSysCal, tsky);
    }
    const String& tskyFlag = MSSysCal::columnName(MSSysCal::TSKY_FLAG);
    if (cds.isDefined(tskyFlag)) tskyFlag_p.attach(msSysCal, tskyFlag);
    const String& tskySpectrum =
      MSSysCal::columnName(MSSysCal::TSKY_SPECTRUM);
    if (cds.isDefined(tskySpectrum)) {
      tskySpectrum_p.attach(msSysCal, tskySpectrum);
      tskySpectrumQuant_p.attach(msSysCal, tskySpectrum);
    }
    const String& tsys = MSSysCal::columnName(MSSysCal::TSYS);
    if (cds.isDefined(tsys)) {
      tsys_p.attach(msSysCal, tsys);
      tsysQuant_p.attach(msSysCal, tsys);
    }
    const String& tsysFlag = MSSysCal::columnName(MSSysCal::TSYS_FLAG);
    if (cds.isDefined(tsysFlag)) tsysFlag_p.attach(msSysCal, tsysFlag);
    const String& tsysSpectrum =
      MSSysCal::columnName(MSSysCal::TSYS_SPECTRUM);
    if (cds.isDefined(tsysSpectrum)) {
      tsysSpectrum_p.attach(msSysCal, tsysSpectrum);
      tsysSpectrumQuant_p.attach(msSysCal, tsysSpectrum);
    }
  }
}

void MSSysCalColumns::setEpochRef(MEpoch::Types ref, Bool tableMustBeEmpty) {
  timeMeas_p.setDescRefCode(ref, tableMustBeEmpty);
}

} //# NAMESPACE CASACORE - END
