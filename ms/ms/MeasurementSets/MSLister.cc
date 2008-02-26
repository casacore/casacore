//# MSLister.cc:  Class for listing records from a MeasurementSet
//# Copyright (C) 1998,1999,2000,2001
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
//#
#include <casa/Quanta/MVTime.h>
#include <casa/Containers/RecordFieldId.h>
#include <measures/Measures/Stokes.h>
#include <ms/MeasurementSets/MSColumns.h>
#include <ms/MeasurementSets/MeasurementSet.h>
#include <ms/MeasurementSets/MSLister.h>
#include <ms/MeasurementSets/MSSummary.h>
#include <ms/MeasurementSets/MSRange.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/MaskedArray.h>
#include <casa/Arrays/MaskArrMath.h>
#include <casa/iomanip.h>
#include <casa/iostream.h>
#include <casa/OS/File.h>

#include <fstream>

#include <ms/MeasurementSets/MSSelectionTools.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//
// Null constructor merely sets private formatting string
//
MSLister::MSLister ()
  : dashline_p(replicate('-',80))
{ 
  pMSSel_p = 0;
}


//
// Constructor assigns pointer (if MS goes out of scope you will get rubbish),
// initialises the output, sets string to format output, and initialises the
// listing.
//
MSLister::MSLister (const MeasurementSet& ms, LogIO& os)
  //  : pMS_p(&ms),
  : pMS_p(const_cast<MeasurementSet*>(&ms)),
    logStream_p(os),
    dashline_p(replicate('-',80))
{
  // Move these into initList()?
  // default precision (in case setPrecision is not called)
  precTime_p = 7;    // hh:mm:ss.s  (0.1 s)
  precUVDist_p = 0;  // 1 wavelength
  precAmpl_p = 3;    // mJy
  precPhase_p = 1;   // 0.1 deg
  precWeight_p = 0;  // unit weight
  // initializations
  wFld_p = wSpW_p = wChn_p = 0;

  // initialize list params
  initList();

  pMSSel_p = 0;
}


//
// Assignment operator
//
MSLister& MSLister::operator=(MSLister& other)
{
  if (this==&other) return *this;
  pMS_p = other.pMS_p;
  return *this;
}


//
// Destructor does nothing
//
MSLister::~MSLister()
{}

//
// Reinitialise output stream.  Do this before setMS() if doing both.
//
Bool MSLister::setNewOS (LogIO& os)
{
  logStream_p = os;
  return True;
}


//
// Reassign MS pointer and reinitialise MSLister object.  Do this after
// setNewOS() if doing both.
//
Bool MSLister::setMS (MeasurementSet& ms)
{
  pMS_p = &ms;
  initList();
  return True;
}


//
// initList() does things that need to be done once per MS: initialises
// the pagination/formatting, lists some header information, initialises
// the MSSelector object, and gets all the attribute ranges up front.
//
void MSLister::initList()
{
  // Establish the formatting
  // setPage();            // page size
  // setFormat();          // the # of decimal places for data

  // CLEANUP: Do we want to keep the header info or not?
  // // List some header information so the user knows what to select on
  // listHeader();

  // // Initialise the MSSelector object.  By default, initSelection() takes all
  // // polarisations and the first spectral channel.
  // mss_p.setMS(*pMS_p);
  // 
  // mss_p.initSelection();

  // Get the ranges (into ranges_p) of the following usefully
  // selectable attributes.  range_p can be changed later to
  // refine selection.
  // **SPW**
  items_p.resize(6,False);
  items_p(0)="time";                // the range of times
  items_p(1)="antenna1";            // the list of antenna1 id values
  items_p(2)="antenna2";            // the list of antenna2 id values
  items_p(3)="uvdist";              // the range of the UV-distance (m)
  //  items_p(4)="spectral_window_id";    // the list of spwin id values
  items_p(4)="data_desc_id";            // the list of data desc id values
  items_p(5)="field_id";            // the list of field id values
  getRanges(*pMS_p);

  // Set up for selection on channel or polarisation
  ROMSSpWindowColumns msSpWinC(pMS_p->spectralWindow());
  ROMSPolarizationColumns msPolC(pMS_p->polarization());
  nchan_p = msSpWinC.numChan()(0);
  npols_p = msPolC.corrType()(0).nelements();
  pols_p.resize(npols_p,False);
  for (uInt i=0; i<npols_p; i++) {
    pols_p(i) = Stokes::name(Stokes::type
                       (msPolC.corrType()(0)(IPosition(1,i))));
  }


  // Store spwin ref freqs for later use:
  //   (should get channel freqs for multi-channel MS's)
  //  freqs_p.resize(4,False);
  freqs_p=msSpWinC.refFrequency().getColumn();

  // Create map from data_desc_id to spwid:
  ROMSDataDescColumns msDDI(pMS_p->dataDescription());
  spwins_p=msDDI.spectralWindowId().getColumn();

  // Signal completion of initList
  logStream_p << LogIO::NORMAL1 << "Listing initialised for this MS" << LogIO::POST;
}

// CLEANUP: Removing member function (2008/02/08).  If class still works,
//   remove this code later.
// void MSLister::setPage (const uInt width, const uInt height)
// {
//   // Set up pagination for output.  Default is for landscape printing (w=120)
//   // with font size 10 (h=60).
//   pageWidth_p = width;
//   pageHeight_p = height;
// }

// This function is not currently used.  However, if an output precision
// option is added to this class, this will be useful.
void MSLister::setFormat (const uInt ndec)
{
  // Set up data display precision
  nDecimal_p = ndec;
}

void MSLister::setPrecision ( const Int precTime, const Int precUVDist,
                        const Int precAmpl, const Int precPhase, 
                        const Int precWeight )
{
  // Set private precision vars on basis of user input:
  precTime_p   = precTime+6;  // internally, time precision includes hhmmss.
  precUVDist_p = precUVDist;
  precAmpl_p   = precAmpl;
  precPhase_p  = precPhase;
  precWeight_p = precWeight;
}

// CLEANUP: This function is currently not used.  Remove it?
void MSLister::listHeader()
{
  // Construct the MSSummary object and output the header info
  // ALL OF THESE SHOULD BE GIVEN PRIORITY NORMAL1.
  MSSummary header(*pMS_p);
  header.listTitle (logStream_p);
  header.listWhat (logStream_p,False);
  header.listSpectralWindow (logStream_p,True);
  header.listPolarization (logStream_p,True);
  header.listAntenna (logStream_p,True);
  logStream_p.post();
}


// Get the ranges of a fixed set of MS key attributes
void MSLister::getRanges(const MeasurementSet &ms)
{
  // Get the full range of columns for an MS.
  MSRange msr(ms);
  ranges_p = msr.range(items_p);          
}

void MSLister::list (const String options,
                     const String datacolumn,
                     const String field,
                     const String spw,
                     const String antenna,
                     const String timerange,
                     const String correlation,
                     const String scan,
                     const String feed,
                     const String array,
                     const String uvrange,
                     const String average,
                     const bool   showflags,
                     const String msSelect,
                     const long   pagerows,
                     const String listfile)
{
  try{

    String chanmode;
    Int nchan;
    Int start;
    Int step;
    MRadialVelocity mStart;
    MRadialVelocity mStep;

    // Choose MSSelector keywords according to the value of datacolumn
    dataColSel.resize(2);
    if( datacolumn.empty() || datacolumn == "data") {
        dataColSel(0) = "amplitude";
        dataColSel(1) = "phase";
    } else if (datacolumn == "corrected") {
        dataColSel(0) = "corrected_amplitude";
        dataColSel(1) = "corrected_phase";
    } else if (datacolumn == "model") {
        dataColSel(0) = "model_amplitude";
        dataColSel(1) = "model_phase";
    } else if (datacolumn == "residual") {
        dataColSel(0) = "residual_amplitude";
        dataColSel(1) = "residual_phase";
    } else {
        logStream_p << LogIO::SEVERE << "datacolumn = " << datacolumn << LogIO::POST;
        throw(AipsError("Unrecognized value in parameter datacolumn"));
    }
    // logStream_p << "dataColSel = " << dataColSel << LogIO::POST;
    // cout << "dataColSel = " << dataColSel << endl;

    selectvis(timerange, spw, scan, field, antenna, uvrange, chanmode,
              nchan, start, step, mStart, mStep, correlation,
              array, msSelect);

    // List the data
    listData(pagerows, listfile);
  }
  catch (AipsError x) {
    logStream_p << LogOrigin("MSLister","list",WHERE) 
            << LogIO::SEVERE << "Caught exception: " << x.getMesg()
            << LogIO::POST;
    throw(AipsError("Error in MSLister::list"));
  } 
}

// // Select data (using MSSelection syntax)
// 
// Questions: Is it necessary to sort the MS?  Leaving sorting code for now.

// CLEANUP: Remove parameters that are not inputs to mssSetData; they are no
//   longer used anywhere.
void MSLister::selectvis(const String& timerange,
                     const String& spw,
                     const String& scan,
                     const String& field,
                     const String& antenna,
                     const String& uvrange,
                     const String& chanmode,        // Not inputs to mssSetData
                     const Int& nchan,              //  
                     const Int& start,              //
                     const Int& step,               //
                     const MRadialVelocity& mStart, //
                     const MRadialVelocity& mStep,  //
                     const String& correlation,
                     const String& array,
                     const String& msSelect)
{
  
  try {
    
    // List input parameter values.
    logStream_p << LogIO::DEBUG1 << "timerange   = " << timerange   << " , strlen = " << timerange.length()   << endl;
    logStream_p << LogIO::DEBUG1 << "spw         = " << spw         << " , strlen = " << spw.length()         << endl;
    logStream_p << LogIO::DEBUG1 << "scan        = " << scan        << " , strlen = " << scan.length()        << endl;
    logStream_p << LogIO::DEBUG1 << "field       = " << field       << " , strlen = " << field.length()       << endl;
    logStream_p << LogIO::DEBUG1 << "antenna     = " << antenna     << " , strlen = " << antenna.length()     << endl;
    logStream_p << LogIO::DEBUG1 << "uvrange     = " << uvrange     << " , strlen = " << uvrange.length()     << endl;
    logStream_p << LogIO::DEBUG1 << "correlation = " << correlation << " , strlen = " << correlation.length() << endl;
    logStream_p << LogIO::DEBUG1 << "array       = " << array       << " , strlen = " << array.length()       << endl;
    logStream_p << LogIO::DEBUG1 << "msSelect    = " << uvrange     << " , strlen = " << msSelect.length()    << LogIO::POST;
    // logStream_p << "feed        = " << feed        << " , strlen = " << feed.length()        << endl;
    // logStream_p << "average     = " << uvrange     << " , strlen = " << average.length()     << endl;
    // logStream_p << "showflags   = " << uvrange     << " , strlen = " << showflags.length()   << endl;

    // Apply selection to the original MeasurementSet
    if (!(timerange.empty() && spw.empty() && scan.empty() && field.empty() &&
          antenna.empty() && uvrange.empty() && correlation.empty() &&
          msSelect.empty()) ) {
      logStream_p << LogIO::NORMAL1 << "Performing selection on MeasurementSet" << LogIO::POST;
    } else { 
      logStream_p << LogIO::NORMAL2 << "No selection requested." << LogIO::POST;
    }

    if (pMSSel_p) {
      delete pMSSel_p;
      pMSSel_p=0;
    };
    
    // Assume no selection, for starters
    pMSSel_p = new MeasurementSet(*pMS_p);

    //                                           // mssSetData Param Names
    MSSelection *pMSSelection = new MSSelection(*pMS_p,
                           MSSelection::PARSE_NOW,
                           timerange,            // timeExpr
                           antenna,              // antennaExpr
                           field,                // fieldExpr
                           spw,                  // spwExpr
                           uvrange,              // uvDistExpr
                           msSelect,             // taQLExpr
                           correlation,          // corrExpr
                           scan,                 // scanExpr
                           array);               // arrayExpr

    // Check to see if selection returned any rows.
    Bool nonTrivial = pMSSelection->getSelectedMS(*pMSSel_p, "");

    // What channels are contained in the selected data?
    Matrix<Int> chanList=pMSSelection->getChanList();
    logStream_p << LogIO::DEBUG1 << "pMSSelection->getChanList() = " << endl 
                << pMSSelection->getChanList() << LogIO::POST;
              
    // Gather channels to be listed into Vector channels_p
    for(uInt i=0; i<chanList.nrow(); i++) {
      Int nChanAdd = chanList(i,2) - chanList(i,1) + 1;
      Int lenChannels;
      channels_p.shape(lenChannels);
      channels_p.resize(lenChannels + nChanAdd, True);
      if(chanList(i,3) > 0) {
        for(Int j=chanList(i,1); j<=chanList(i,2); j+=chanList(i,3)) {
          // push j onto end of Array channels_p
          channels_p(lenChannels) = j;
          lenChannels++;
        }
      } else { // if chanList(i,3) == 0
        // push chanList(i,1) onto end of Array channels_p
        channels_p(lenChannels) = chanList(i,2);
      }
    }
    if (chanList.nrow() == 0) { 
      channels_p.resize(0,False);
    }
    channels_p.shape(nchan_p);

    // If non-trivial MSSelection invoked and nrow reduced:
    if(nonTrivial && pMSSel_p->nrow()<pMS_p->nrow()) {

      // Escape if no rows selected
      if (pMSSel_p->nrow()==0) 
      throw(AipsError("Specified selection contains zero rows (no data)!"));

      // ...otherwise report how many rows are selected
      logStream_p << LogIO::NORMAL1 << "Selection reduces " << pMS_p->nrow() 
                << " rows to " << pMSSel_p->nrow() << " rows."
                << LogIO::POST;
    }
    else {
      // Selection did nothing:
      logStream_p << LogIO::NORMAL2 << "Selection did not drop any rows." << LogIO::POST;
    }

    // Set up for selection on channel or polarisation
    ROMSSpWindowColumns msSpWinC(pMSSel_p->spectralWindow());
    // If no channels were specified for selection, select all by default.
    if (nchan_p== 0) {
      nchan_p = msSpWinC.numChan()(0);
      channels_p.resize(nchan_p);
      for(Int i=0; i<nchan_p; i++) { channels_p(i)=i; }
    }
    // output the channels to be listed
    logStream_p << LogIO::NORMAL1 << "Listing channels: " << channels_p << LogIO::POST;
    logStream_p << LogIO::NORMAL2 << "Number of selected channels = " 
              << nchan_p << LogIO::POST;
    logStream_p << LogIO::DEBUG1 << "nchan_p = " << nchan_p << LogIO::POST;
    logStream_p << LogIO::DEBUG2 << "msSpwinC.numChan() = " << endl 
              << msSpWinC.numChan().getColumn() 
              << LogIO::POST;
    
    ROMSPolarizationColumns msPolC(pMSSel_p->polarization());
    npols_p = msPolC.corrType()(0).nelements();
    pols_p.resize(npols_p,False);
    for (uInt i=0; i<npols_p; i++) {
      pols_p(i) = Stokes::name(Stokes::type
                         (msPolC.corrType()(0)(IPosition(1,i))));
    }
  } // end try block
  catch (MSSelectionError& x) {
    // Re-initialize with the existing MS
    logStream_p << LogOrigin("MSLister","selectvis",WHERE) 
            << LogIO::SEVERE << "Caught exception: " << x.getMesg()
            << LogIO::POST;
    //initialize(*pMS_p,False);
    throw(AipsError("Error in data selection specification."));
  } 
  catch (AipsError x) {
    // Re-initialize with the existing MS
    logStream_p << LogOrigin("MSLister","selectvis",WHERE) 
            << LogIO::SEVERE << "Caught exception: " << x.getMesg()
            << LogIO::POST;
    // initialize(*pMS_p,False);
    throw(AipsError("Error in MSLister::selectvis()"));
  }
}; // end selectvis

void MSLister::listData(const int maxPageRows, 
                        const String listfile)
{
  // Now get the data for the listing.  
  // Currently we are extracting the data through a Record 
  // to a Record to Arrays using MSSelector::getData, which requires
  // a list of items, a reasonable default set of which is defined
  // here as a Vector<String> passable to MSSelector::getData 
  // (eventually, user will have some choice here, e.g., to get
  // real/imag instead of amp/ph, etc.).
  // Eventually, it would be easier if there were public RO access to the
  // MSSelector::selms_p member...
  // **SPW**

  try{ 
  
    items_p.resize(10,False);
    items_p(0)="time";
    items_p(1)="antenna1";            
    items_p(2)="antenna2";            
    items_p(3)="uvdist";              
    // items_p(4)="spectral_window_id";    
    items_p(4)="data_desc_id";  
    items_p(5)="field_id";            
    items_p(6)=dataColSel(0);
    items_p(7)=dataColSel(1);         
    items_p(8)="weight";        
    items_p(9)="flag";
  
    // Get ranges of selected data to ranges_p for use in field-width/precision setting
    getRanges(*pMSSel_p);
  
    // From here on, all MS access should go through mss_p.
  
    // Initialise the MSSelector object.  By default, initSelection() takes all
    // polarizations and the first spectral channel.
    mss_p.setMS(*pMSSel_p);
    mss_p.initSelection();
  
    // Now extract the selected data Record.  Note that mss_p is the *selected*
    // data, and mss_p.getData() is an implicit Record object
    dataRecords_p = mss_p.getData(items_p,False);
  
    // Construct arrays for the Record items.  
    //  The V-float declaration
    //  appears to be necessary (instead of V-double) despite the get()
    //  function's claim to do type promotion.
    Vector <Double>       rowTime,uvdist;
    Vector <Int>          datadescid;
    Vector <Int>          ant1,ant2,spwinid,fieldid;
    Array <Bool>          flag;
    Array <Float>         ampl,phase;
    Array <Float>         weight;
  
    // Fill the arrays.
    rowTime = dataRecords_p.asArrayDouble(RecordFieldId("time"));
    //  antenna 1
    if (dataRecords_p.isDefined("antenna1")) {
      ant1 = dataRecords_p.asArrayInt(RecordFieldId("antenna1"));
    } else if (dataRecords_p.isDefined("ant1")) {
      ant1 = dataRecords_p.asArrayInt(RecordFieldId("ant1"));
    } else {
      logStream_p << LogIO::SEVERE << "antenna1 isn't defined" << LogIO::POST;
      return;
    }
    //  antenna 2
    if (dataRecords_p.isDefined("antenna2")) {
      ant2 = dataRecords_p.asArrayInt(RecordFieldId("antenna2"));
    } else if (dataRecords_p.isDefined("ant2")) {
      ant2 = dataRecords_p.asArrayInt(RecordFieldId("ant2"));
    } else {
      logStream_p << LogIO::SEVERE << "antenna2 isn't defined" << LogIO::POST;
      return;
    }
    //  flag, uvdist, datadescid, fieldid
    flag = dataRecords_p.asArrayBool(RecordFieldId("flag"));
    uvdist = dataRecords_p.asArrayDouble(RecordFieldId("uvdist"));
    datadescid = dataRecords_p.asArrayInt(RecordFieldId("data_desc_id"));
    fieldid = dataRecords_p.asArrayInt(RecordFieldId("field_id"));
    //  dataColSel(0) (the data identified by this variable)
    if (dataRecords_p.isDefined(dataColSel(0))) {
      ampl = dataRecords_p.asArrayFloat(RecordFieldId(dataColSel(0)));
    } else {
      logStream_p << LogIO::SEVERE << "Column " << dataColSel(0) 
                << " (for amplitude) isn't defined." << LogIO::POST;
      return;
    }
    //  dataColSel(1) (the data identified by this variable)
    if (dataRecords_p.isDefined(dataColSel(1))) {
      phase = dataRecords_p.asArrayFloat(RecordFieldId(dataColSel(1)));
    } else {
      logStream_p << LogIO::SEVERE << "Column " << dataColSel(1) 
                << " (for phase) isn't defined." << LogIO::POST;
      return;
    }
    //  weight
    weight = dataRecords_p.asArrayFloat(RecordFieldId("weight"));
  
    // Number of rows that will be listed
    Int nTableRows = rowTime.nelements();
  
    spwinid.resize(nTableRows);
    
    // Convert units of some params:
    rowTime = rowTime/C::day;        // time now in days
    phase = phase/C::degree;   // phase now in degrees
    // For each row: translate Data Description IDs to Spectral Window IDs
    // This must be done before column widths can be calculated, before
    // data can be written.
    for (Int tableRow=0;tableRow<nTableRows;tableRow++) {
      // Translate data_desc_id to spwid:  
      spwinid(tableRow)=spwins_p(datadescid(tableRow));
      // Change uvdist to wavelengths as function of spwinid:
      //  Note that uv-distance data selection uses meters at the default unit.
      uvdist(tableRow) = uvdist(tableRow)/(C::c/freqs_p(spwinid(tableRow)));  
    }    
  
    // Add or adjust ranges_p to non-zero absolutes for non-index and/or 
    // converted values (so we can use ranges_p for field width and 
    // precision setting):
  
    Vector<Double> uvminmax(2);
    uvminmax(0)=min(uvdist);
    uvminmax(1)=max(uvdist);
    ranges_p.define("uvdist",uvminmax);
    
    Vector<Float> amplminmax(2);
    amplminmax(0)=min(ampl(ampl>0.0f));
    amplminmax(1)=max(ampl);
    ranges_p.define(dataColSel(0),amplminmax);
  
    // Find the range of phase.  Take care to avoid creating a 0-element
    //  MaskedArray, if all elements of phase are 0.0f.  A 0-element
    //  array will crash function min.
    Vector<Float> phminmax(2);
    MaskedArray<float> maPhase(phase, (phase!=0.0f));
    if (maPhase.nelementsValid() != 0) {
      phminmax(0)=min(abs(phase(phase!=0.0f))); 
      phminmax(1)=max(phase);
    } else { 
      phminmax(0) = 0.0f; 
      phminmax(1) = 0.0f; 
      logStream_p << LogIO::NORMAL1 << "All selected data has phase = 0.0" << LogIO::POST;
    }
    ranges_p.define(dataColSel(1),phminmax);
  
    Vector<Float> wtminmax(2);
    wtminmax(0)=min(weight(weight >0.0f));
    wtminmax(1)=max(weight);
    ranges_p.define("weight",wtminmax);
  
    // logStream_p << LogIO::DEBUG2 << "Printing out the Record ranges_p:" << endl
    //           << ranges_p << LogIO::POST;
  
    // Make flags for showing index columns.
    //  Test to see if more than one value is present
    //  in the data column.
    //  If the array consists of only 1 value, ranges_p will have only 1
    //  element, and the boolean value will be set to false!
    if (ranges_p.asArrayInt(RecordFieldId("field_id")).nelements() > 1) {
      doFld_p = True;
    } else {
      doFld_p = False;
      logStream_p << LogIO::NORMAL << "All selected data has FIELD = "
                << fieldid(0) // ranges_p.asArrayInt(RecordFieldId("field_id"))(0)
                << LogIO::POST;
    }
    // doSpW_p = (ranges_p.asArrayInt(RecordFieldId("data_desc_id")).nelements() > 1);
    if (ranges_p.asArrayInt(RecordFieldId("data_desc_id")).nelements() > 1) {
      doSpW_p = True;
    } else {
      doSpW_p = False;
      logStream_p << LogIO::NORMAL << "All selected data has SPW = "
                << datadescid(0) // ranges_p.asArrayInt(RecordFieldId("data_desc_id"))(0)
                << LogIO::POST;
    }
    if (nchan_p > 1) {
      doChn_p = True;
      // logStream_p << LogIO::NORMAL2 << "Selected data has CHANNEL(s) = " << endl;
      // for (Int i=0; i<nchan_p; i++) 
      //   logStream_p << LogIO::NORMAL2 << "LIST CHAN #, ";
      // logStream_p << LogIO::POST;
    } else {
      doChn_p = False;
      logStream_p << LogIO::NORMAL << "All selected data has CHANNEL = "
                << spwinid(0)
                << LogIO::POST;
    }
  
    // logStream_p << LogIO::DEBUG2 << "Fld id : " << ranges_p.get("field_id") << endl
    //           << "SpW id : " << ranges_p.get("spectral_window_id") << LogIO::POST;
  
    // From this point on, don't change scaling in list arrays, since the
    // field sizes are determined directly from the data.
      logStream_p << LogIO::DEBUG1 
                << "Minumum uvdist   = " << min(uvdist) << endl
                << "Maximum uvdist   = " << max(uvdist) << endl
                << "Minumum amp      = " << min(ampl) << endl
                << "Maximum amp      = " << max(ampl) << endl
                << "Minumum phase    = " << min(abs(phase)) << endl
                << "Maximum phase    = " << max(abs(phase)) << endl
                << "Minumum weight   = " << min(weight) << endl
                << "Maximum weight   = " << max(weight) << LogIO::POST;
  
    // Set order of magnitude and precision (for field width setting):
    // If prec*_p < 0, then enforce >=0 (detect minimum decimal places to show is NYI)
    // If prec*_p > 0, then increment o*_p to provide space for decimal
  
    oTime_p = 2; // this is space for 2 :'s in time  
    if ( precTime_p < 0 ) precTime_p = 7; // hh:mm:ss.s
    if ( precTime_p > 0 ) oTime_p++; // add space for decimal
  
    oUVDist_p = (uInt)max(1,(Int)rint(log10(max(uvdist))+0.5)); // order
    if ( precUVDist_p < 0 ) precUVDist_p = 0;
    if ( precUVDist_p > 0 ) oUVDist_p++;  // add space for decimal
  
    oAmpl_p = (uInt)max(1,(Int)rint(log10(max(ampl))+0.5)); 
    if ( precAmpl_p < 0 ) precAmpl_p = 3;  // mJy
    if ( precAmpl_p > 0 ) oAmpl_p++;  // add space for decimal
  
    oPhase_p = 3;  // 100s of degs 
    if ( precPhase_p < 0 ) precPhase_p = 0; 
    if ( precPhase_p > 0 ) oPhase_p++; // add space for decimal
    oPhase_p++; // add space for sign
  
    oWeight_p = (uInt)max(1,(Int)rint(log10(max(weight))+0.5));  // order
    if ( precWeight_p < 0 ) precWeight_p = 0;
    if ( precWeight_p > 0 ) oWeight_p++;  // add space for decimal
  
    // Set field widths.
    //   For index columns just use the size of the largest value:
    wAnt_p    = (uInt)rint(log10((float)max(max(ant1),max(ant2)))+0.5);
    wFlag_p = 2; // the flag is always 1 character
    if (doFld_p) wFld_p    = (uInt)rint(log10((float)max(fieldid))+0.5);
    if (doSpW_p) wSpW_p    = (uInt)rint(log10((float)max(spwinid))+0.5);
    if (doChn_p) wChn_p    = 3;
  
  
    //   The field width for non-index columns is given by the  
    //    sum of the order and precision:
    wTime_p   = oTime_p + precTime_p;
    wUVDist_p = oUVDist_p + precUVDist_p;  
    wAmpl_p   = oAmpl_p + precAmpl_p;  
    wPhase_p  = oPhase_p + precPhase_p;  
    wWeight_p = oWeight_p + precWeight_p;
  
    // Enforce minimum field widths,
    // add leading space so columns nicely separated,
    // and accumulate wTotal_p:
    wTotal_p = 0;  // initialize 
    wAnt_p    = max(wAnt_p,   (uInt)2);
    wIntrf_p  = 2*wAnt_p+1;                       wIntrf_p++;  wTotal_p+=wIntrf_p;
    if (doFld_p) { wFld_p = max(wFld_p,(uInt)3);  wFld_p++;    wTotal_p+=wFld_p; }
    if (doSpW_p) { wSpW_p = max(wSpW_p,(uInt)3);  wSpW_p++;    wTotal_p+=wSpW_p;}
    if (doChn_p) { wChn_p = max(wChn_p,(uInt)3);  wChn_p++;    wTotal_p+=wChn_p;}
  
    wTime_p   = max(wTime_p,  (uInt)12);
    wUVDist_p = max(wUVDist_p,(uInt)6);           wUVDist_p++; wTotal_p+=wUVDist_p;
    wAmpl_p   = max(wAmpl_p,  (uInt)4);           wAmpl_p++; 
    wPhase_p  = max(wPhase_p, (uInt)4); 
    wWeight_p = max(wWeight_p,(uInt)3);           wWeight_p++;
  
    wVis_p = wAmpl_p+wPhase_p+wWeight_p+wFlag_p;
    wTotal_p+=wTime_p+npols_p*wVis_p+1;
  
    // Make column-ated header rule according to total and field widths
  
    // replicate does not work if the first parameter is "-", but it does for '-'.
    // Bug report here: https://bugs.aoc.nrao.edu/browse/CAS-511
    String hSeparator=replicate('-',wTotal_p);
    uInt colPos=0;
    colPos+=wTime_p;   hSeparator[colPos]='|';
    colPos+=wIntrf_p;  hSeparator[colPos]='|';
    colPos+=wUVDist_p; hSeparator[colPos]='|';
    if (doFld_p) {colPos+=wFld_p;hSeparator[colPos]='|';}
    if (doSpW_p) {colPos+=wSpW_p;hSeparator[colPos]='|';}
    if (doChn_p) {colPos+=wChn_p;hSeparator[colPos]='|';}
    colPos++;
    for (uInt ipol=0; ipol<npols_p-1; ipol++) {
      colPos+=wVis_p;
      hSeparator[colPos]='|';
    }
  
    Vector<String> flagSym(2);
    flagSym(0) = " ";
    flagSym(1) = "F";
  
    // Finally print the records out, one per line.
  
    logStream_p << LogIO::NORMAL << "Listing " << rowTime.nelements()
              << " data records satisfying selection criteria, " << endl
              << "for each of " << npols_p << " polarisation(s) and " << nchan_p
              << " spectral channel(s)." << LogIO::POST;
  
    Bool prompt=True;
    Int pageRow=0;
  
    // REDIRECT OUTPUT TO FILE
    ofstream file;
    streambuf* sbuf = cout.rdbuf();
    if(listfile!="") {
      // non-interactive
      prompt = False;
  
      // Guard against trampling existing file
      File diskfile(listfile);
      if (diskfile.exists()) {
        String errmsg = "File: " + listfile +
        " already exists; delete it or choose a different name.";
        throw(AipsError(errmsg));
      }
      else
        cout << "Writing output to file: " << listfile << endl;
  
      file.open(listfile.data());
      cout.rdbuf(file.rdbuf());
    }
  
    // WRITE THE DATA
    for (Int tableRow=0; tableRow<nTableRows; tableRow++) { 
      date_p = MVTime(rowTime(tableRow)).string(MVTime::YMD_ONLY);
      Bool endOutput=False;
  
      for (Int i=0; i<nchan_p; i++) {
        uInt ichan = channels_p(i);
        // If page length reached, or new day, then paginate
        if ((pageRow/maxPageRows)*maxPageRows == pageRow ||
            date_p != lastdate_p) {
          // query the user, if we are interactive
          if (prompt && pageRow != 0) {
            string contStr;
            cout << "Type Q to quit, A to list all, or RETURN to continue [continue]: ";
            getline(cin,contStr);
            if ( (contStr.compare(0,1,"q") == 0) or
                 (contStr.compare(0,1,"Q") == 0) ) { endOutput=True; }
            if ( (contStr.compare(0,1,"a") == 0) or
                 (contStr.compare(0,1,"A") == 0) ) { prompt = False; }
          }
          if (endOutput) {break;} // break out of if block
          listColumnHeader();
          cout << hSeparator << endl;
        }
        // if (row != 0) cout << hSeparator << endl; // Print hSeparator at the end of each page.
        lastdate_p = date_p;
        if (endOutput) {break;} // break out of chan loop
  
        pageRow++;
  
        cout.setf(ios::fixed, ios::floatfield);
        cout.setf(ios::right, ios::adjustfield);
        
        cout.width(wTime_p);    cout << MVTime(rowTime(tableRow)).string(MVTime::TIME,precTime_p);
        cout.width(wAnt_p+1);   cout << ant1(tableRow);
        cout.width(1);            cout << "-";
        cout.width(wAnt_p);     cout << ant2(tableRow);
        cout.precision(precUVDist_p);
        cout.width(wUVDist_p);cout << uvdist(tableRow);
        // For the output to agree with listobs, do not add 1 to fieldid and spwinid.
        if (doFld_p) {cout.width(wFld_p);   cout << fieldid(tableRow);}
        if (doSpW_p) {cout.width(wSpW_p);   cout << spwinid(tableRow);}
        if (doChn_p) {cout.width(wChn_p);   cout << ichan;}
        cout << ":";
        for (uInt ipol=0; ipol<npols_p; ipol++) {
        cout.precision(precAmpl_p);
        cout.width(wAmpl_p);     cout << ampl(IPosition(3,ipol,ichan,tableRow));
        cout.precision(precPhase_p);
        cout.width(wPhase_p); cout << phase(IPosition(3,ipol,ichan,tableRow));
        cout.precision(precWeight_p);
        cout.width(wWeight_p); cout << weight(IPosition(2,ipol,tableRow));
        cout.setf(ios::right); cout.width(2);
        cout << flagSym(flag(IPosition(3,ipol,ichan,tableRow)));
        }
        cout << endl;
      }
      if (endOutput) {break;} // break out of row loop
    }
    cout << hSeparator << endl;
  
    // Post it
    logStream_p.post();
  
    // RESTORE COUT (if redirected to file)
    if(listfile!="")
      cout.rdbuf(sbuf);
      
  } // end try
  catch(AipsError x){
    logStream_p << LogIO::SEVERE << "Caught exception: " << x.getMesg()
              << LogIO::POST;
    throw(AipsError("Error in MSLister::listData"));
  }
} // end listData

void MSLister::listColumnHeader() {
  // Write the column headers

  // First line of column header
  cout.setf(ios::left, ios::adjustfield);
  cout.width(wTime_p);             cout << "Date/Time:";
  cout.setf(ios::right, ios::adjustfield);
  cout.width(wIntrf_p);            cout << " ";
  cout.width(wUVDist_p);           cout << " ";
  if (wFld_p) {cout.width(wFld_p); cout << " ";}
  if (wSpW_p) {cout.width(wSpW_p); cout << " ";}
  if (wChn_p) {cout.width(wChn_p); cout << " ";}
  cout << " ";
  cout.setf(ios::left, ios::adjustfield);
  for (uInt ipol=0; ipol<npols_p; ipol++) {
    cout.width(wVis_p); cout << " "+pols_p(ipol)+":";
  }
  cout << endl;
  
  // Second line of column header
  cout.setf(ios::left, ios::adjustfield);
  cout.width(wTime_p);             cout << date_p+"/";
  cout.setf(ios::right, ios::adjustfield);
  cout.width(wIntrf_p);            cout << "Intrf";
  cout.width(wUVDist_p);           cout << "UVDist";
  if (wFld_p) {cout.width(wFld_p); cout << "Fld";}
  if (wSpW_p) {cout.width(wSpW_p); cout << "SpW";}
  if (wChn_p) {cout.width(wChn_p); cout << "Chn";}
  cout << " ";
  for (uInt ipol=0; ipol<npols_p; ipol++) {
    cout.width(wAmpl_p);      cout << "Amp";
    cout.width(wPhase_p);     cout << "Phs";
    cout.width(wWeight_p);    cout << "Wt";
                              cout << " F"; // flag column
  }
  cout << endl;
  // << dashline_p << endl;
}

//
// Clear all the formatting flags
//
void MSLister::clearFlags()
{
  cout.unsetf(ios::left);
  cout.unsetf(ios::right);
  cout.unsetf(ios::internal);

  cout.unsetf(ios::dec);
  cout.unsetf(ios::oct);
  cout.unsetf(ios::hex);

  cout.unsetf(ios::showbase | ios::showpos | ios::uppercase | ios::showpoint);

  cout.unsetf(ios::scientific);
  cout.unsetf(ios::fixed);

}

} //# NAMESPACE CASA - END

