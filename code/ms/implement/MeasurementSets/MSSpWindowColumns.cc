//# NewMSSpWindowColumns.cc:  provides easy access to NewMeasurementSet columns
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

#include <aips/MeasurementSets/NewMSSpWindowColumns.h>
#include <aips/MeasurementSets/NewMSSpectralWindow.h>
#include <aips/Tables/ColDescSet.h>
#include <aips/Tables/TableDesc.h>

RONewMSSpWindowColumns::
RONewMSSpWindowColumns(const NewMSSpectralWindow& msSpWindow):
  chanFreq_p(msSpWindow, NewMSSpectralWindow::
	     columnName(NewMSSpectralWindow::CHAN_FREQ)),
  chanWidth_p(msSpWindow, NewMSSpectralWindow::
	      columnName(NewMSSpectralWindow::CHAN_WIDTH)),
  effectiveBW_p(msSpWindow, NewMSSpectralWindow::
		columnName(NewMSSpectralWindow::EFFECTIVE_BW)),
  flagRow_p(msSpWindow, NewMSSpectralWindow::
	    columnName(NewMSSpectralWindow::FLAG_ROW)),
  freqGroup_p(msSpWindow, NewMSSpectralWindow::
	      columnName(NewMSSpectralWindow::FREQ_GROUP)),
  freqGroupName_p(msSpWindow, NewMSSpectralWindow::
		  columnName(NewMSSpectralWindow::FREQ_GROUP_NAME)),
  ifConvChain_p(msSpWindow, NewMSSpectralWindow::
		columnName(NewMSSpectralWindow::IF_CONV_CHAIN)),
  measFreqRef_p(msSpWindow, NewMSSpectralWindow::
		columnName(NewMSSpectralWindow::MEAS_FREQ_REF)),
  name_p(msSpWindow, NewMSSpectralWindow::
	 columnName(NewMSSpectralWindow::NAME)),
  netSideband_p(msSpWindow, NewMSSpectralWindow::
		columnName(NewMSSpectralWindow::NET_SIDEBAND)),
  numChan_p(msSpWindow, NewMSSpectralWindow::
	    columnName(NewMSSpectralWindow::NUM_CHAN)),
  refFrequency_p(msSpWindow, NewMSSpectralWindow::
		 columnName(NewMSSpectralWindow::REF_FREQUENCY)),
  resolution_p(msSpWindow, NewMSSpectralWindow::
	       columnName(NewMSSpectralWindow::RESOLUTION)),
  totalBandwidth_p(msSpWindow, NewMSSpectralWindow::
		   columnName(NewMSSpectralWindow::TOTAL_BANDWIDTH)),
  assocNature_p(),
  assocSpwId_p(),
  bbcNo_p(),
  bbcSideband_p(),
  dopplerId_p(),
  receiverId_p(),
  chanFreqMeas_p(msSpWindow, NewMSSpectralWindow::
		 columnName(NewMSSpectralWindow::CHAN_FREQ)),
  refFrequencyMeas_p(msSpWindow, NewMSSpectralWindow::
		     columnName(NewMSSpectralWindow::REF_FREQUENCY)),
  chanFreqQuant_p(msSpWindow, NewMSSpectralWindow::
		  columnName(NewMSSpectralWindow::CHAN_FREQ)),
  chanWidthQuant_p(msSpWindow, NewMSSpectralWindow::
		   columnName(NewMSSpectralWindow::CHAN_WIDTH)),
  effectiveBWQuant_p(msSpWindow, NewMSSpectralWindow::
		     columnName(NewMSSpectralWindow::EFFECTIVE_BW)),
  refFrequencyQuant_p(msSpWindow, NewMSSpectralWindow::
		      columnName(NewMSSpectralWindow::REF_FREQUENCY)),
  resolutionQuant_p(msSpWindow, NewMSSpectralWindow::
		    columnName(NewMSSpectralWindow::RESOLUTION)),
  totalBandwidthQuant_p(msSpWindow, NewMSSpectralWindow::
			columnName(NewMSSpectralWindow::TOTAL_BANDWIDTH))
{
  attachOptionalCols(msSpWindow);
}

RONewMSSpWindowColumns::~RONewMSSpWindowColumns() {}

RONewMSSpWindowColumns::RONewMSSpWindowColumns():
  chanFreq_p(),
  chanWidth_p(),
  effectiveBW_p(),
  flagRow_p(),
  freqGroup_p(),
  freqGroupName_p(),
  ifConvChain_p(),
  measFreqRef_p(),
  name_p(),
  netSideband_p(),
  numChan_p(),
  refFrequency_p(),
  resolution_p(),
  totalBandwidth_p(),
  assocNature_p(),
  assocSpwId_p(),
  bbcNo_p(),
  bbcSideband_p(),
  dopplerId_p(),
  receiverId_p(),
  chanFreqMeas_p(),
  refFrequencyMeas_p(),
  chanFreqQuant_p(),
  chanWidthQuant_p(),
  effectiveBWQuant_p(),
  refFrequencyQuant_p(),
  resolutionQuant_p(),
  totalBandwidthQuant_p()
{
}

void RONewMSSpWindowColumns::attach(const NewMSSpectralWindow& msSpWindow)
{
  chanFreq_p.attach(msSpWindow, NewMSSpectralWindow::
		    columnName(NewMSSpectralWindow::CHAN_FREQ));
  chanWidth_p.attach(msSpWindow, NewMSSpectralWindow::
		     columnName(NewMSSpectralWindow::CHAN_WIDTH));
  effectiveBW_p.attach(msSpWindow, NewMSSpectralWindow::
		       columnName(NewMSSpectralWindow::EFFECTIVE_BW));
  flagRow_p.attach(msSpWindow, NewMSSpectralWindow::
		   columnName(NewMSSpectralWindow::FLAG_ROW));
  freqGroup_p.attach(msSpWindow, NewMSSpectralWindow::
		     columnName(NewMSSpectralWindow::FREQ_GROUP));
  freqGroupName_p.attach(msSpWindow, NewMSSpectralWindow::
			 columnName(NewMSSpectralWindow::FREQ_GROUP_NAME));
  ifConvChain_p.attach(msSpWindow, NewMSSpectralWindow::
		       columnName(NewMSSpectralWindow::IF_CONV_CHAIN));
  measFreqRef_p.attach(msSpWindow, NewMSSpectralWindow::
		       columnName(NewMSSpectralWindow::MEAS_FREQ_REF));
  name_p.attach(msSpWindow, NewMSSpectralWindow::
		columnName(NewMSSpectralWindow::NAME));
  netSideband_p.attach(msSpWindow, NewMSSpectralWindow::
		       columnName(NewMSSpectralWindow::NET_SIDEBAND));
  numChan_p.attach(msSpWindow, NewMSSpectralWindow::
		   columnName(NewMSSpectralWindow::NUM_CHAN));
  refFrequency_p.attach(msSpWindow, NewMSSpectralWindow::
			columnName(NewMSSpectralWindow::REF_FREQUENCY));
  resolution_p.attach(msSpWindow, NewMSSpectralWindow::
		      columnName(NewMSSpectralWindow::RESOLUTION));
  totalBandwidth_p.attach(msSpWindow, NewMSSpectralWindow::
			  columnName(NewMSSpectralWindow::TOTAL_BANDWIDTH));
  chanFreqMeas_p.attach(msSpWindow, NewMSSpectralWindow::
			columnName(NewMSSpectralWindow::CHAN_FREQ));
  refFrequencyMeas_p.attach(msSpWindow, NewMSSpectralWindow::
			    columnName(NewMSSpectralWindow::REF_FREQUENCY));
  chanFreqQuant_p.attach(msSpWindow, NewMSSpectralWindow::
			 columnName(NewMSSpectralWindow::CHAN_FREQ));
  chanWidthQuant_p.attach(msSpWindow, NewMSSpectralWindow::
			  columnName(NewMSSpectralWindow::CHAN_WIDTH));
  effectiveBWQuant_p.attach(msSpWindow, NewMSSpectralWindow::
			    columnName(NewMSSpectralWindow::EFFECTIVE_BW));
  refFrequencyQuant_p.attach(msSpWindow, NewMSSpectralWindow::
			     columnName(NewMSSpectralWindow::REF_FREQUENCY));
  resolutionQuant_p.attach(msSpWindow, NewMSSpectralWindow::
			   columnName(NewMSSpectralWindow::RESOLUTION));
  totalBandwidthQuant_p.attach(msSpWindow, NewMSSpectralWindow::
			       columnName(NewMSSpectralWindow::TOTAL_BANDWIDTH));
  attachOptionalCols(msSpWindow);
}

void RONewMSSpWindowColumns::
attachOptionalCols(const NewMSSpectralWindow& msSpWindow)
{
  const ColumnDescSet& cds=msSpWindow.tableDesc().columnDescSet();
  const String& assocNature=
    NewMSSpectralWindow::columnName(NewMSSpectralWindow::ASSOC_NATURE);
  if (cds.isDefined(assocNature)) assocNature_p.attach(msSpWindow,assocNature);
  const String& assocSpwId=
    NewMSSpectralWindow::columnName(NewMSSpectralWindow::ASSOC_SPW_ID);
  if (cds.isDefined(assocSpwId)) assocSpwId_p.attach(msSpWindow,assocSpwId);
  const String& bbcNo=
    NewMSSpectralWindow::columnName(NewMSSpectralWindow::BBC_NO);
  if (cds.isDefined(bbcNo)) bbcNo_p.attach(msSpWindow,bbcNo);
  const String& bbcSideband=
    NewMSSpectralWindow::columnName(NewMSSpectralWindow::BBC_SIDEBAND);
  if (cds.isDefined(bbcSideband)) bbcSideband_p.attach(msSpWindow,bbcSideband);
  const String& dopplerId=
    NewMSSpectralWindow::columnName(NewMSSpectralWindow::DOPPLER_ID);
  if (cds.isDefined(dopplerId)) dopplerId_p.attach(msSpWindow,dopplerId);
  const String& receiverId=
    NewMSSpectralWindow::columnName(NewMSSpectralWindow::RECEIVER_ID);
  if (cds.isDefined(receiverId)) receiverId_p.attach(msSpWindow,receiverId);
}

NewMSSpWindowColumns::NewMSSpWindowColumns(NewMSSpectralWindow& msSpWindow):
  RONewMSSpWindowColumns(msSpWindow),
  chanFreq_p(msSpWindow, NewMSSpectralWindow::
	     columnName(NewMSSpectralWindow::CHAN_FREQ)),
  chanWidth_p(msSpWindow, NewMSSpectralWindow::
	      columnName(NewMSSpectralWindow::CHAN_WIDTH)),
  effectiveBW_p(msSpWindow, NewMSSpectralWindow::
		columnName(NewMSSpectralWindow::EFFECTIVE_BW)),
  flagRow_p(msSpWindow, NewMSSpectralWindow::
	    columnName(NewMSSpectralWindow::FLAG_ROW)),
  freqGroup_p(msSpWindow, NewMSSpectralWindow::
	      columnName(NewMSSpectralWindow::FREQ_GROUP)),
  freqGroupName_p(msSpWindow, NewMSSpectralWindow::
		  columnName(NewMSSpectralWindow::FREQ_GROUP_NAME)),
  ifConvChain_p(msSpWindow, NewMSSpectralWindow::
		columnName(NewMSSpectralWindow::IF_CONV_CHAIN)),
  measFreqRef_p(msSpWindow, NewMSSpectralWindow::
		columnName(NewMSSpectralWindow::MEAS_FREQ_REF)),
  name_p(msSpWindow, NewMSSpectralWindow::
	 columnName(NewMSSpectralWindow::NAME)),
  netSideband_p(msSpWindow, NewMSSpectralWindow::
		columnName(NewMSSpectralWindow::NET_SIDEBAND)),
  numChan_p(msSpWindow, NewMSSpectralWindow::
	    columnName(NewMSSpectralWindow::NUM_CHAN)),
  refFrequency_p(msSpWindow, NewMSSpectralWindow::
		 columnName(NewMSSpectralWindow::REF_FREQUENCY)),
  resolution_p(msSpWindow, NewMSSpectralWindow::
	       columnName(NewMSSpectralWindow::RESOLUTION)),
  totalBandwidth_p(msSpWindow, NewMSSpectralWindow::
		   columnName(NewMSSpectralWindow::TOTAL_BANDWIDTH)),
  assocNature_p(),
  assocSpwId_p(),
  bbcNo_p(),
  bbcSideband_p(),
  dopplerId_p(),
  receiverId_p(),
  chanFreqMeas_p(msSpWindow, NewMSSpectralWindow::
		 columnName(NewMSSpectralWindow::CHAN_FREQ)),
  refFrequencyMeas_p(msSpWindow, NewMSSpectralWindow::
		     columnName(NewMSSpectralWindow::REF_FREQUENCY)),
  chanFreqQuant_p(msSpWindow, NewMSSpectralWindow::
		  columnName(NewMSSpectralWindow::CHAN_FREQ)),
  chanWidthQuant_p(msSpWindow, NewMSSpectralWindow::
		   columnName(NewMSSpectralWindow::CHAN_WIDTH)),
  effectiveBWQuant_p(msSpWindow, NewMSSpectralWindow::
		     columnName(NewMSSpectralWindow::EFFECTIVE_BW)),
  refFrequencyQuant_p(msSpWindow, NewMSSpectralWindow::
		      columnName(NewMSSpectralWindow::REF_FREQUENCY)),
  resolutionQuant_p(msSpWindow, NewMSSpectralWindow::
		    columnName(NewMSSpectralWindow::RESOLUTION)),
  totalBandwidthQuant_p(msSpWindow, NewMSSpectralWindow::
			columnName(NewMSSpectralWindow::TOTAL_BANDWIDTH))
{
  attachOptionalCols(msSpWindow);
}

NewMSSpWindowColumns::~NewMSSpWindowColumns() {}

NewMSSpWindowColumns::NewMSSpWindowColumns():
  RONewMSSpWindowColumns(),
  chanFreq_p(),
  chanWidth_p(),
  effectiveBW_p(),
  flagRow_p(),
  freqGroup_p(),
  freqGroupName_p(),
  ifConvChain_p(),
  measFreqRef_p(),
  name_p(),
  netSideband_p(),
  numChan_p(),
  refFrequency_p(),
  resolution_p(),
  totalBandwidth_p(),
  assocNature_p(),
  assocSpwId_p(),
  bbcNo_p(),
  bbcSideband_p(),
  dopplerId_p(),
  receiverId_p(),
  chanFreqMeas_p(),
  refFrequencyMeas_p(),
  chanFreqQuant_p(),
  chanWidthQuant_p(),
  effectiveBWQuant_p(),
  refFrequencyQuant_p(),
  resolutionQuant_p(),
  totalBandwidthQuant_p()
{
}

void NewMSSpWindowColumns::attach(NewMSSpectralWindow& msSpWindow)
{
  RONewMSSpWindowColumns::attach(msSpWindow);
  chanFreq_p.attach(msSpWindow, NewMSSpectralWindow::
		    columnName(NewMSSpectralWindow::CHAN_FREQ));
  chanWidth_p.attach(msSpWindow, NewMSSpectralWindow::
		     columnName(NewMSSpectralWindow::CHAN_WIDTH));
  effectiveBW_p.attach(msSpWindow, NewMSSpectralWindow::
		       columnName(NewMSSpectralWindow::EFFECTIVE_BW));
  flagRow_p.attach(msSpWindow, NewMSSpectralWindow::
		   columnName(NewMSSpectralWindow::FLAG_ROW));
  freqGroup_p.attach(msSpWindow, NewMSSpectralWindow::
		     columnName(NewMSSpectralWindow::FREQ_GROUP));
  freqGroupName_p.attach(msSpWindow, NewMSSpectralWindow::
			 columnName(NewMSSpectralWindow::FREQ_GROUP_NAME));
  ifConvChain_p.attach(msSpWindow, NewMSSpectralWindow::
		       columnName(NewMSSpectralWindow::IF_CONV_CHAIN));
  measFreqRef_p.attach(msSpWindow, NewMSSpectralWindow::
		       columnName(NewMSSpectralWindow::MEAS_FREQ_REF));
  name_p.attach(msSpWindow, NewMSSpectralWindow::
		columnName(NewMSSpectralWindow::NAME));
  netSideband_p.attach(msSpWindow, NewMSSpectralWindow::
		       columnName(NewMSSpectralWindow::NET_SIDEBAND));
  numChan_p.attach(msSpWindow, NewMSSpectralWindow::
		   columnName(NewMSSpectralWindow::NUM_CHAN));
  refFrequency_p.attach(msSpWindow, NewMSSpectralWindow::
			columnName(NewMSSpectralWindow::REF_FREQUENCY));
  resolution_p.attach(msSpWindow, NewMSSpectralWindow::
		      columnName(NewMSSpectralWindow::RESOLUTION));
  totalBandwidth_p.attach(msSpWindow, NewMSSpectralWindow::
			  columnName(NewMSSpectralWindow::TOTAL_BANDWIDTH));
  chanFreqMeas_p.attach(msSpWindow, NewMSSpectralWindow::
			columnName(NewMSSpectralWindow::CHAN_FREQ));
  refFrequencyMeas_p.attach(msSpWindow, NewMSSpectralWindow::
			    columnName(NewMSSpectralWindow::REF_FREQUENCY));
  chanFreqQuant_p.attach(msSpWindow, NewMSSpectralWindow::
			 columnName(NewMSSpectralWindow::CHAN_FREQ));
  chanWidthQuant_p.attach(msSpWindow, NewMSSpectralWindow::
			  columnName(NewMSSpectralWindow::CHAN_WIDTH));
  effectiveBWQuant_p.attach(msSpWindow, NewMSSpectralWindow::
			    columnName(NewMSSpectralWindow::EFFECTIVE_BW));
  refFrequencyQuant_p.attach(msSpWindow, NewMSSpectralWindow::
			     columnName(NewMSSpectralWindow::REF_FREQUENCY));
  resolutionQuant_p.attach(msSpWindow, NewMSSpectralWindow::
			   columnName(NewMSSpectralWindow::RESOLUTION));
  totalBandwidthQuant_p.attach(msSpWindow, NewMSSpectralWindow::
			       columnName(NewMSSpectralWindow::TOTAL_BANDWIDTH));
  attachOptionalCols(msSpWindow);
}

void NewMSSpWindowColumns::
attachOptionalCols(NewMSSpectralWindow& msSpWindow)
{
  const ColumnDescSet& cds=msSpWindow.tableDesc().columnDescSet();
  const String& assocNature=
    NewMSSpectralWindow::columnName(NewMSSpectralWindow::ASSOC_NATURE);
  if (cds.isDefined(assocNature)) assocNature_p.attach(msSpWindow,assocNature);
  const String& assocSpwId=
    NewMSSpectralWindow::columnName(NewMSSpectralWindow::ASSOC_SPW_ID);
  if (cds.isDefined(assocSpwId)) assocSpwId_p.attach(msSpWindow,assocSpwId);
  const String& bbcNo=
    NewMSSpectralWindow::columnName(NewMSSpectralWindow::BBC_NO);
  if (cds.isDefined(bbcNo)) bbcNo_p.attach(msSpWindow,bbcNo);
  const String& bbcSideband=
    NewMSSpectralWindow::columnName(NewMSSpectralWindow::BBC_SIDEBAND);
  if (cds.isDefined(bbcSideband)) bbcSideband_p.attach(msSpWindow,bbcSideband);
  const String& dopplerId=
    NewMSSpectralWindow::columnName(NewMSSpectralWindow::DOPPLER_ID);
  if (cds.isDefined(dopplerId)) dopplerId_p.attach(msSpWindow,dopplerId);
  const String& receiverId=
    NewMSSpectralWindow::columnName(NewMSSpectralWindow::RECEIVER_ID);
  if (cds.isDefined(receiverId)) receiverId_p.attach(msSpWindow,receiverId);
}
// Local Variables: 
// compile-command: "gmake NewMSSpWindowColumns"
// End: 

