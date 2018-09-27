//# MSMetaData.h
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
//# $Id: MSMetaData.h 21586 2015-03-25 13:46:25Z gervandiepen $

#ifndef MS_MSMETADATA_H
#define MS_MSMETADATA_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Quanta/QVector.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/MSPointingColumns.h>
#include <casacore/casa/Utilities/CountedPtr.h>
#include <casacore/tables/Tables/TableProxy.h>
#include <map>

namespace casacore {

template <class T> class ArrayColumn;
struct ArrayKey;
struct ScanKey;
struct SourceKey;
struct SubScanKey;

// <summary>
// Class to interrogate  an MS for metadata. Interrogation happens on demand
// and resulting metadata are stored for use by subsequent queries if the
// cache has not exceeded the specified limit. Caching of MS main table columns
// has been removed because the cache can be swamped by columns for large
// MSes, meaning that smaller data structures, which are more computationally
// expensive to create, aren't cached. Also, the column data is usually only
// needed temporarily to compute smaller data structures, and the column data
// is not particularly expensive to recreate if necessary.
// Parallel processing is enabled using openmp.
// </summary>

class MSMetaData {

public:

    // for retrieving stats
    enum CorrelationType {
        AUTO,
        CROSS,
        BOTH
    };

    enum SQLDSwitch {
        SQLD_INCLUDE,
        SQLD_EXCLUDE,
        SQLD_ONLY
    };

    struct TimeStampProperties {
        std::set<Int> ddIDs;
        uInt nrows;
    };

    struct ColumnStats {
        Double max;
        Double median;
        Double min;
    };

    typedef std::map<Int, std::pair<Double, Quantity> > FirstExposureTimeMap;

    struct SubScanProperties {
        // number of auto-correlation rows
        uInt acRows;
        // number of cross-correlation rows.
        uInt xcRows;
        std::set<Int> antennas;
        Double beginTime;
        std::set<uInt> ddIDs;
        Double endTime;
        // the key is the spwID, the value is the meanInterval for
        // the subscan and that spwID
        std::map<uInt, Quantity> meanInterval;
        // The Int represents the data description ID,
        // The Double represents the time of the first time stamp,
        // The Quantity represents the exposure time for the corresponding
        // data description ID and time stamp
        FirstExposureTimeMap firstExposureTime;
        Quantity meanExposureTime;
        std::set<uInt> spws;
        // number of rows for each spectral window
        std::map<uInt, uInt> spwNRows;
        std::set<Int> stateIDs;
        std::map<Double, TimeStampProperties> timeProps;
    };

    // construct an object which stores a pointer to the MS and queries the MS
    // only as necessary. The MeasurementSet pointer passed in should not go out
    // of scope in the calling code until the caller has finished with this object,
    // or else subsequent method calls on this object will result in a segmentation
    // fault; the pointer is not copied.
    // <src>maxCacheSizeMB</src> is the maximum cache size in megabytes. <=0 means
    // do not use a cache, in which case, each method call will have to (re)query
    // the MS. It is highly recommended to use a cache of reasonable size for the
    // specified MS if multiple methods are going to be called.
    MSMetaData(const MeasurementSet *const &ms, const Float maxCacheSizeMB);

    virtual ~MSMetaData();

    // get the antenna diameters
    QVD getAntennaDiameters() const;

    // if the antenna name appears multiple times in the antenna table, the *last* ID
    // that it is associated with is returned.
    uInt getAntennaID(const String& antennaName) const;

    // get all the antenna IDs for the antenna with the specified name. 
    std::set<uInt> getAntennaIDs(const String& antennaName) const;

    // The returned IDs are ordered in the way they appear in the atenna table
    vector<std::set<uInt> > getAntennaIDs(const vector<String>& antennaNames) const;

    // In the first instance of getAntennaNames, namesToID map will have the *last* ID
    // of the antenna name, if it appears multiple times in the antenna table. In the second
    // occurrence, namesToIDsMap will have the full set of IDs for antenna names that appear
    // multiple times.

    vector<String> getAntennaNames(
        std::map<String, uInt>& namesToIDsMap,
        const vector<uInt>& antennaIDs=vector<uInt>(0)
    ) const;

    vector<String> getAntennaNames(
        std::map<String, std::set<uInt> >& namesToIDsMap,
        const vector<uInt>& antennaIDs=vector<uInt>(0)
    ) const;

    // get the antenna stations for the specified antenna IDs
    vector<String> getAntennaStations(const vector<uInt>& antennaIDs=vector<uInt>());

    // get the antenna stations for the specified antenna names. The outer vector is ordered
    // respective to antennaNames. Because an antenna name can appear more than once in
    // the antenna table, the inner vector is ordered by row number in which that antenna name
    // appears.
    vector<std::vector<String> > getAntennaStations(const vector<String>& antennaNames);

    // get the set of antenna IDs for the specified scan.
    std::set<Int> getAntennasForScan(const ScanKey& scan) const;

    // POLARIZATION.CORR_PRODUCT
    vector<Array<Int> > getCorrProducts() const;

    // POLARIZATION.CORR_TYPE
    vector<vector<Int> > getCorrTypes() const;

    vector<uInt> getDataDescIDToSpwMap() const;

    vector<uInt> getDataDescIDToPolIDMap() const;

    // Get the FIELD.SOURCE_ID column.
    vector<Int> getFieldTableSourceIDs() const;

    // get the mapping of field ID to scans
    vector<std::set<ScanKey> > getFieldToScansMap() const;

    std::map<String, std::set<Int> > getIntentToFieldsMap();

    std::map<String, std::set<ScanKey> > getIntentToScansMap();

    std::map<String, std::set<uInt> > getIntentToSpwsMap();

    std::set<String> getIntentsForScan(const ScanKey& scan) const;

    std::set<String> getIntentsForSubScan(const SubScanKey& subScan) const;

    std::shared_ptr<const std::map<SubScanKey, std::set<String> > > getSubScanToIntentsMap() const;

    // get all intents, in no particular (nor guaranteed) order.
    std::set<String> getIntents() const;

    // get a set of intents corresponding to a specified field
    std::set<String> getIntentsForField(Int fieldID);

    // get a set of intents corresponding to the specified spectral window
    std::set<String> getIntentsForSpw(const uInt spw);

    // number of correlations from the polarization table.
    vector<Int> getNumCorrs() const;

    //SOURCE.PROPER_MOTION, first value in pair is longitudinal proper motion,
    // second is latiduninal
    vector<std::pair<Quantity, Quantity> > getProperMotions() const;

    // get unique scan numbers
    std::set<Int> getScanNumbers(Int obsID, Int arrayID) const;

    // get a set of scan numbers for the specified stateID, obsID, and arrayID.
    // If obsID and/or arrayID is negative, all observation IDs and/or array IDs
    // will be used.
    std::set<Int> getScansForState(
        Int stateID, Int obsID, Int arrayID
    ) const;

    // get the mapping of scans to states
    std::map<ScanKey, std::set<Int> > getScanToStatesMap() const;

    // SOURCE.DIRECTION
    vector<MDirection> getSourceDirections() const;

    // SOURCE.NAME
    vector<String> getSourceNames() const;

    // Get the SOURCE.SOURCE_ID column. This is a very unfortunate column name,
    // because generally an "ID" column of the table with the same name refers to
    // the row number in that table. But not in this case.
    vector<Int> getSourceTableSourceIDs() const;

    // SOURCE.TIME
    std::shared_ptr<const Quantum<Vector<Double> > > getSourceTimes() const;

    // get a set of spectral windows for which the specified <src>intent</src>
    // applies.
    virtual std::set<uInt> getSpwsForIntent(const String& intent);

    // get the number of visibilities
    uInt nRows() const;

    uInt nRows(CorrelationType cType);

    std::shared_ptr<const std::map<SubScanKey, uInt> > getNRowMap(CorrelationType type) const;

    uInt nRows(
        CorrelationType cType, Int arrayID, Int observationID,
        Int scanNumber, Int fieldID
    ) const;

    uInt nRows(CorrelationType cType, uInt fieldID) const;

    // get number of spectral windows
    uInt nSpw(Bool includewvr) const;

    // number of unique states (number of rows from the STATE table)
    uInt nStates() const;

    // get the number of fields.
    uInt nFields() const;

    // get a mapping of spectral window ID to data descrption IDs
    std::vector<std::set<uInt> > getSpwToDataDescriptionIDMap() const;

    // get a set of spectral windows corresponding to the specified fieldID
    std::set<uInt> getSpwsForField(const Int fieldID) const;

    // get a set of spectral windows corresponding to the specified field name
    std::set<uInt> getSpwsForField(const String& fieldName);

    // get the values of the CODE column from the field table
    vector<String> getFieldCodes() const;

    // get the set of field IDs corresponding to the specified spectral window.
    std::set<Int> getFieldIDsForSpw(const uInt spw);

    // get the set of field names corresponding to the specified spectral window.
    std::set<String> getFieldNamesForSpw(const uInt spw);

    // get the mapping of fields to spws
    std::map<Int, std::set<uInt> > getFieldsToSpwsMap() const;

    // get rest frequencies from the SOURCE table
    std::map<SourceKey, std::shared_ptr<vector<MFrequency> > > getRestFrequencies() const;

    // get the set of spectral windows for the specified scan.
    std::set<uInt> getSpwsForScan(const ScanKey& scan) const;

    // get the set of spectral windows for the specified subscan.
    std::set<uInt> getSpwsForSubScan(const SubScanKey& subScan) const;

    // get the set of scan numbers for the specified spectral window.
    std::set<Int> getScansForSpw(uInt spw, Int obsID, Int arrayID) const;

    // get the complete mapping of scans to spws
    std::map<ScanKey, std::set<uInt> > getScanToSpwsMap() const;

    // get the complete mapping of spws to scans
    std::vector<std::set<ScanKey> > getSpwToScansMap() const;

    // get the transitions from the SOURCE table. If there are no transitions
    // for a particular key, the shared ptr contains the null ptr.
    std::map<SourceKey, std::shared_ptr<vector<String> > > getTransitions() const;

    // get the number of antennas in the ANTENNA table
    uInt nAntennas() const;

    // ALMA-specific. get set of spectral windows used for TDM. These are windows that have
    // 64, 128, or 256 channels
    std::set<uInt> getTDMSpw();

    // ALMA-specific. get set of spectral windows used for FDM. These are windows that do not
    // have 1, 4, 64, 128, or 256 channels.
    std::set<uInt> getFDMSpw();

    // ALMA-specific. get spectral windows that have been averaged. These are windows with 1 channel.
    std::set<uInt> getChannelAvgSpw();

    // ALMA-specific. Get the spectral window set used for WVR measurements. These have 4 channels each.
    std::set<uInt> getWVRSpw() const;

    // ALMA-specific. Get the square law detector (total power) spectral windows.
    std::set<uInt> getSQLDSpw();

    // Get the scan numbers which fail into the specified time range (center-tol to center+tol),
    // inclusive. A negative value of obsID and/or arrayID indicates that all observation IDs
    // and/or all arrayIDs should be used.
    std::set<Int> getScansForTimes(
        Double center, Double tol, Int obsID, Int arrayID
    ) const;

    // Get the times for the specified scans
    std::set<Double> getTimesForScans(std::set<ScanKey> scans) const;

    // get the times for the specified scan.
    // The return values come from the TIME column.
    std::set<Double> getTimesForScan(const ScanKey& scan) const;

    std::map<uInt, std::set<Double> > getSpwToTimesForScan(const ScanKey& scan) const;

    // get the time range for the specified scan. The pair will contain
    // the start and stop time of the scan, determined from min(TIME(x)-0.5*INTERVAL(x)) and
    // max(TIME(x)-0.5*INTERVAL(x))
    std::pair<Double, Double> getTimeRangeForScan(const ScanKey& scanKey) const;

    // get the map of scans to time ranges.
    std::shared_ptr<const std::map<ScanKey, std::pair<Double,Double> > > getScanToTimeRangeMap() const;

    // get the stateIDs associated with the specified scan. If obsID and/or arrayID
    // is negative, all observation IDs and/or array IDs will be used.
    std::set<Int> getStatesForScan(Int obsID, Int arrayID, Int scan) const;

    // get a map of spectral windows to unique timestamps.
    std::vector<std::set<Double> > getTimesForSpws(Bool showProgress=True) const;

    // get the position of the specified antenna relative to the observatory position.
    // the three vector returned represents the longitudinal, latitudinal, and elevation
    // offsets (elements 0, 1, and 2 respectively). The longitude and latitude offsets are
    // measured along the surface of a sphere centered at the earth's center and whose surface
    // intersects the position of the observatory.
    QVD getAntennaOffset(uInt which) const;

    // If the antenna name appears mulitple times, this will return the offset for the first
    // occurrence of it in the antenna table
    QVD getAntennaOffset(const String& name) const;

    // If the antenna name appears mulitple times, this will return all the offsets for it,
    // in the order they appear in the antenna table
    std::vector<QVD> getAntennaOffsets(const String& name) const;

    vector<QVD > getAntennaOffsets() const;

    // get the positions of the specified antennas. If <src>which</src> is empty, return
    // all antenna positions.
    vector<MPosition> getAntennaPositions(
        const vector<uInt>& which=std::vector<uInt>(0)
    ) const;

    // <src>names</src> cannot be empty.
    vector<vector<MPosition> > getAntennaPositions(const vector<String>& names);

    // the first key in the returned map is the spectral window ID, the second is
    // the average interval for the specified scan for that spw.
    std::map<uInt, Double> getAverageIntervalsForScan(const ScanKey& scan) const;

    // the first key in the returned map is the spectral window ID, the second is
    // the average interval for the specified sub scan for that spw.
    std::map<uInt, Quantity> getAverageIntervalsForSubScan(const SubScanKey& subScan) const;

    vector<uInt> getBBCNos() const;

    std::map<uInt, std::set<uInt> > getBBCNosToSpwMap(SQLDSwitch sqldSwitch);

    vector<vector<Double> > getEdgeChans();
    //Get the phase direction for a given field id and epoch
    //interpolate polynomial if it is the field id  is such or use ephemerides table 
    //if that is attached to that field id
    MDirection phaseDirFromFieldIDAndTime(const uInt fieldID,  
                          const MEpoch& ep=MEpoch(Quantity(0.0, Unit("s")))) const ;

    // Get the reference direction for a given field ID and epoch interpolate
    // polynomial if it is the field ID is such or use ephemerides table
    // if that is attached to that field ID
    MDirection getReferenceDirection(
        const uInt fieldID,
        const MEpoch& ep=MEpoch(Quantity(0.0, Unit("s")))
    ) const;
    
    // get the field IDs for the specified field name. Case insensitive.
    std::set<Int> getFieldIDsForField(const String& field) const;
    
    // get a list of the field names in the order in which they appear in the FIELD table.
    vector<String> getFieldNames() const;

    // get field IDs associated with the specified scan number.
    std::set<Int> getFieldsForScan(const ScanKey& scan) const;

    // get the field IDs associated with the specified scans
    std::set<Int> getFieldsForScans(
        const std::set<Int>& scans, Int obsID, Int arrayID
    ) const;

    // get the field IDs associated with the specified scans
    std::set<Int> getFieldsForScans(const std::set<ScanKey>& scans) const;

    // get the field IDs associated with the specified intent.
    std::set<Int> getFieldsForIntent(const String& intent);

    // get the field IDs associated with the specified source.
    std::set<Int> getFieldsForIntent(uInt sourceID) const;

    std::map<Int, std::set<Int> > getFieldsForSourceMap() const;

    std::map<Int, std::set<String> > getFieldNamesForSourceMap() const;

    // get the field names associated with the specified field IDs. If <src>fieldIDs</src>
    // is empty, a vector of all the field names is returned.
    vector<String> getFieldNamesForFieldIDs(const vector<uInt>& fieldIDs);

    // Get the fields which fail into the specified time range (center-tol to center+tol)
    std::set<Int> getFieldsForTimes(Double center, Double tol);

    // max cache size in MB
    Float getMaxCacheSizeMB() const { return _maxCacheMB; }

    // get telescope names in the order they are listed in the OBSERVATION table. These are
    // the telescopes (observatories), not the antenna names.
    vector<String> getObservatoryNames();

    // get the position of the specified telescope (observatory).
    MPosition getObservatoryPosition(uInt which) const;

    // get the phase directions from the FIELD subtable. The <src>ep</src> parameter
    // specifies for which epoch to return the directions of any ephemeris objects
    // in the data set. It is ignored for non-ephemeris objects.
    vector<MDirection> getPhaseDirs(const MEpoch& ep=MEpoch(Quantity(0.0, Unit("s")))) const;

    // get all ScanKeys in the dataset
    std::set<ScanKey> getScanKeys() const;

    // get all ScanKeys in the dataset that have the specified <src>arrayKey</src>.
    // If negative values for either the obsID and/or arrayID portions of the ArrayKey
    // indicate that all obsIDs and/or arrayIDs should be used.
    std::set<ScanKey> getScanKeys(const ArrayKey& arrayKey) const;

    // get the scans associated with the specified intent
    std::set<Int> getScansForIntent(
        const String& intent, Int obsID, Int arrayID
    ) const;

    // get the scan numbers associated with the specified field ID.
    std::set<Int> getScansForFieldID(Int fieldID, Int obsID, Int arrayID) const;

    // get the scan numbers associated with the specified field. Subclasses should not implement or override.
    std::set<Int> getScansForField(const String& field, Int obsID, Int arrayID) const;

    // The first value of the pair is spw, the second is polarization ID.
    std::map<std::pair<uInt, uInt>, uInt> getSpwIDPolIDToDataDescIDMap() const;

    // get a map of the spwIDs to spw names from the spw table
    vector<String> getSpwNames() const;

    // get all the spws associated with the data description IDs listed in the main table.
    // This will not correspond to a list of the row numbers in the SPECTRAL_WINDOW table
    // if there are data description IDs that are not in the main table.
    std::set<uInt> getSpwIDs() const;

    // get all sub scan keys for the specified array key.
    std::set<SubScanKey> getSubScanKeys(const ArrayKey& arrayKey) const;

    // get the sub scan properties for the specified sub scan.

    SubScanProperties getSubScanProperties(
        const SubScanKey& subScan, Bool showProgress=False
    ) const;

    std::shared_ptr<const std::map<SubScanKey, SubScanProperties> > getSubScanProperties(
        Bool showProgress=False
    ) const;

    // If True, force the subscan properties structure to be
    // cached regardless of the stipulations on the maximum cache. Normally,
    // the subscan properties structure is small compared to the size of any
    // one column that is necessary to create it, and since creating this
    // structure can be very expensive, especially for large datasets, it
    // is often a good idea to cache it if it will be accessed many times.
    void setForceSubScanPropsToCache(Bool b) { _forceSubScanPropsToCache = b; }

    // get a data structure, consumable by users, representing a summary of the dataset
    Record getSummary() const;

    // get the times for which the specified field was observed
    std::set<Double> getTimesForField(Int fieldID);

    // get the time stamps associated with the specified intent
    std::set<Double> getTimesForIntent(const String& intent) const;
    Bool hasBBCNo() const;

    //std::map<Double, Double> getExposuresForTimes() const;

    // get the unique baselines in the MS. These are not necessarily every combination of the
    // n(n-1)/2 possible antenna pairs, but rather the number of unique baselines represented in
    // the main MS table, which in theory can be less than n(n-1)/2 (for example if samples for
    // certain antenna pairs are not recorded. The returned Matrix is nAnts x nAnts in size. Pairs
    // that are true represent baselines represented in the main MS table.
    Matrix<Bool> getUniqueBaselines();

    // get the number of unique baselines represented in the main MS table which in theory can be
    // less than n*(n-1)/2. If <src>includeAutoCorrelation</src> is True, include autocorrelation
    // "baselines" in the enumeration.
    virtual uInt nBaselines(Bool includeAutoCorrelation=False);

    // get the effective total exposure time. This is the effective time spent collecting unflagged data.
    Quantity getEffectiveTotalExposureTime();

    // get the number of scans in the dataset
    uInt nScans();

    // get the number of observations (from the OBSERVATIONS table) in the dataset
    uInt nObservations() const;

    // get the contents of the OBSERVER column from the OBSERVATIONS table
    vector<String> getObservers() const;

    // get the contents of the PROJECT column from the OBSERVATIONS table
    vector<String> getProjects() const;

    // get the contents of the SCHEDULE column from the OBSERVATIONS table
    // Note that the embedded vectors may have different lengths
    vector<vector<String> > getSchedules() const;

    // get the time ranges from the OBSERVATION table
    vector<std::pair<MEpoch, MEpoch> > getTimeRangesOfObservations() const;

    // get the number of arrays (from the ARRAY table) in the dataset
    uInt nArrays();

    // get the number of data description IDs (from the DATA_DESCRIPTION table)
    uInt nDataDescriptions() const;

    // get the number of unflagged rows
    Double nUnflaggedRows() const;

    Double nUnflaggedRows(CorrelationType cType) const;

    Double nUnflaggedRows(
        CorrelationType cType, Int arrayID, uInt observationID,
        Int scanNumber, uInt fieldID
    ) const;

    Double nUnflaggedRows(CorrelationType cType, Int fieldID) const;

    inline Float getCache() const { return _cacheMB;}

    vector<Double> getBandWidths() const;

    vector<Quantity> getCenterFreqs() const;

    // get the effective bandwidth for each channel. Each element in
    // the returned vector represents a separate spectral window, with
    // ID given by its location in the vector. If asVelWidths is True,
    // convert the values to velocity widths.
    vector<QVD> getChanEffectiveBWs(Bool asVelWidths) const;

    vector<QVD > getChanFreqs() const;

    // get the resolution for each channel. Each element in
    // the returned vector represents a separate spectral window, with
    // ID given by its location in the vector. If asVelWidths is True,
    // convert the values to velocity widths.
    vector<QVD> getChanResolutions(Bool asVelWidths) const;

    vector<QVD > getChanWidths() const;

    vector<Quantity> getMeanFreqs() const;

    vector<Int> getNetSidebands() const;

    vector<MFrequency> getRefFreqs() const;

    vector<uInt> nChans() const;

    uInt nPol();

    // DEPRECATED
    // get a map of data desc ID, scan number pair to exposure time for the first time
    // for that data desc ID, scan number pair
    std::vector<std::map<Int, Quantity> > getFirstExposureTimeMap();

    // get map of scans to first exposure times
    std::map<ScanKey, FirstExposureTimeMap> getScanToFirstExposureTimeMap(Bool showProgress) const;

    // get polarization IDs for the specified scan and spwid
    std::set<uInt> getPolarizationIDs(uInt obsID, Int arrayID, Int scan, uInt spwid) const;

    // get the unique antennas (the union of the ANTENNA_1 and ANTENNA_2 columns) from
    // the main table
    const std::set<Int>& getUniqueAntennaIDs() const;

    // get unique data description IDs that exist in the main table
    std::set<uInt> getUniqueDataDescIDs() const;

    // DEPRECATED because of spelling error. Use getUniqueFieldIDs()
    // instead.
    inline std::set<Int> getUniqueFiedIDs() const {
        return getUniqueFieldIDs();
    }

    // get unique field IDs that exist in the main table.
    std::set<Int> getUniqueFieldIDs() const;

    // get the pointing directions associated with antenna1 and antenna2 for
    // the specified row of the main MS table
    std::pair<MDirection, MDirection> getPointingDirection(
        Int& ant1, Int& ant2, Double& time, uInt row,
        Bool interpolate=false, Int initialguess=0
    ) const;

    // get the time range for the entire dataset. min(TIME(x) - 0.5*INTERVAL(x)) to
    // max(TIME(x) + 0.5*INTERVAL(x))
    std::pair<Double, Double> getTimeRange(Bool showProgress=False) const;

    // Number of unique values from SOURCE.SOURCE_ID
    uInt nUniqueSourceIDsFromSourceTable() const;

    // get the unique spectral window IDs represented by the data description
    // IDs that appear in the main table
    std::set<uInt> getUniqueSpwIDs() const;

    const MeasurementSet* getMS() const { return _ms; }

    void setShowProgress(Bool b) { _showProgress = b; }

    // get statistics related to the values of the INTERVAL column. Returned
    // values are in seconds. All values in this column are used in the computation,
    // including those which associated row flags may be set. 
    ColumnStats getIntervalStatistics() const;

private:

    struct ScanProperties {
        // The Int represents the data description ID,
        // The Double represents the time of the first time stamp,
        // The Quantity represents the exposure time for the corresponding
        // data description ID and time stamp
        FirstExposureTimeMap firstExposureTime;
        // the key is the spwID, the value is the meanInterval for
        // the subscan and that spwID
        std::map<uInt, Quantity> meanInterval;
        // number of rows for each spectral window
        std::map<uInt, uInt> spwNRows;
        // time range (which takes into account helf of the corresponding
        // interval, which is not accounted for in the SubScanProperties times
        std::pair<Double, Double> timeRange;
        // times for each spectral window
        std::map<uInt, std::set<Double> > times;
    };

    struct SpwProperties {
        Double bandwidth;
        QVD chanfreqs;
        QVD chanwidths;
        Int netsideband;
        // The sum of all channel frequencies divided by the number of channels
        Quantity meanfreq;
        // The mean of the low frequency extent of the lowest frequency channel and
        // the high frequency extend of the highest frequency channel. Often, but not
        // necessarily, the same as meanfreq
        Quantity centerfreq;
        uInt nchans;
        // The center frequencies of the two channels at the edges of the window
        vector<Double> edgechans;
        uInt bbcno;
        // from the REF_FREQUENCY column
        MFrequency reffreq;
        String name;
        // EFFECTIVE_BANDWIDTH
        QVD effbw;
        // RESOLUTION
        QVD resolution;
    };

    // represents non-primary key data for a SOURCE table row
    struct SourceProperties {
        String name;
        std::shared_ptr<vector<MFrequency> > restfreq;
        std::shared_ptr<vector<String> > transition;
    };

    // The general pattern is that a mutable gets set only once, on demand, when its
    // setter is called for the first time. If this pattern is broken, defective behavior
    // will occur.

    const MeasurementSet* _ms;
    Bool _showProgress;
    mutable Float _cacheMB;
    const Float _maxCacheMB;
    mutable uInt _nStates, _nACRows, _nXCRows, _nSpw, _nFields, _nAntennas,
        _nObservations, _nScans, _nArrays, _nrows, _nPol, _nDataDescIDs;
    mutable std::map<ScanKey, std::set<uInt> > _scanToSpwsMap, _scanToDDIDsMap;
    mutable vector<uInt> _dataDescIDToSpwMap, _dataDescIDToPolIDMap;
    mutable std::map<Int, std::set<uInt> > _fieldToSpwMap;
    mutable std::map<ScanKey, std::set<Int> > _scanToStatesMap, _scanToFieldsMap, _scanToAntennasMap;
    mutable std::map<Int, std::set<Int> >    _fieldToStatesMap, _stateToFieldsMap, _sourceToFieldsMap;
    mutable std::map<std::pair<uInt, uInt>, uInt> _spwPolIDToDataDescIDMap;
    mutable std::map<String, std::set<uInt> > _antennaNameToIDMap;
    mutable std::shared_ptr<const std::map<ScanKey, ScanProperties> > _scanProperties;
    mutable std::shared_ptr<const std::map<SubScanKey, SubScanProperties> > _subScanProperties;

    mutable std::map<String, std::set<Int> > _intentToFieldIDMap;
    mutable std::map<String, std::set<ScanKey> > _intentToScansMap;
    mutable std::map<String, std::set<SubScanKey> > _intentToSubScansMap;
    mutable std::map<std::pair<ScanKey, uInt>, std::set<uInt> > _scanSpwToPolIDMap;
    mutable std::set<String> _uniqueIntents;
    mutable std::set<Int>  _uniqueFieldIDs, _uniqueStateIDs, _uniqueAntennaIDs;
    mutable std::set<uInt> _avgSpw, _tdmSpw, _fdmSpw, _wvrSpw, _sqldSpw, _uniqueDataDescIDs;
    mutable std::shared_ptr<std::map<SubScanKey, uInt> > _subScanToNACRowsMap, _subScanToNXCRowsMap;
    mutable std::shared_ptr<std::map<Int, uInt> > _fieldToNACRowsMap, _fieldToNXCRowsMap;
    mutable std::map<ScanKey, std::set<String> > _scanToIntentsMap;
    mutable std::shared_ptr<const std::map<SubScanKey, std::set<String> > > _subScanToIntentsMap;
    mutable vector<std::set<String> > _stateToIntentsMap, _spwToIntentsMap, _fieldToIntentsMap;
    mutable vector<SpwProperties> _spwInfo;
    mutable vector<std::set<Int> > _spwToFieldIDsMap, _obsToArraysMap;
    mutable vector<std::set<ScanKey> > _spwToScansMap, _ddidToScansMap, _fieldToScansMap;

    mutable vector<String> _fieldNames, _antennaNames, _observatoryNames,
        _stationNames, _observers, _projects, _sourceNames, _fieldCodes;
    mutable vector<vector<String> > _schedules;
    mutable vector<vector<Int> > _corrTypes;
    mutable vector<Array<Int> >_corrProds;

    mutable std::shared_ptr<std::map<ScanKey, std::set<Double> > > _scanToTimesMap;
    std::map<String, std::set<uInt> > _intentToSpwsMap;
    mutable std::map<String, std::set<Double> > _intentToTimesMap;

    std::shared_ptr<std::map<Int, std::set<Double> > > _fieldToTimesMap;
    std::shared_ptr<std::map<Double, std::set<Int> > > _timeToFieldsMap;

    mutable vector<MPosition> _observatoryPositions, _antennaPositions;
    mutable vector<QVD > _antennaOffsets;
    mutable QVD _antennaDiameters;
    Matrix<Bool> _uniqueBaselines;
    Quantity _exposureTime;
    mutable Double _nUnflaggedACRows, _nUnflaggedXCRows;
    mutable std::shared_ptr<vector<Double> > _unflaggedFieldNACRows, _unflaggedFieldNXCRows;
    mutable std::shared_ptr<std::map<SubScanKey, Double> > _unflaggedSubScanNACRows, _unflaggedSubScanNXCRows;
    const String _taqlTableName;
    const vector<const Table*> _taqlTempTable;

    mutable Bool _spwInfoStored, _forceSubScanPropsToCache;
    vector<std::map<Int, Quantity> > _firstExposureTimeMap;
    mutable vector<Int> _numCorrs, _source_sourceIDs, _field_sourceIDs;

    mutable std::set<ArrayKey> _arrayKeys;
    mutable std::set<ScanKey> _scanKeys;
    mutable std::set<SubScanKey> _subscans;
    mutable std::map<ScanKey, std::set<SubScanKey> > _scanToSubScans;
    mutable std::map<ArrayKey, std::set<SubScanKey> > _arrayToSubScans;

    mutable vector<std::pair<MEpoch, MEpoch> > _timeRangesForObs;

    mutable vector<MDirection> _phaseDirs, _sourceDirs;

    mutable vector<std::pair<Quantity, Quantity> > _properMotions;

    mutable std::map<SourceKey, SourceProperties> _sourceInfo;
    mutable std::shared_ptr<std::set<Int> > _ephemFields;
    mutable std::shared_ptr<const Quantum<Vector<Double> > > _sourceTimes;

    // disallow copy constructor and = operator
    MSMetaData(const MSMetaData&);
    MSMetaData operator =(const MSMetaData&);

    // This comment from thunter in the original ValueMapping python class
    // # Determine the number of polarizations for the first OBSERVE_TARGET intent.
    // # Used by plotbandpass for BPOLY plots since the number of pols cannot be inferred
    // # correctly from the caltable alone.  You cannot not simply use the first row, because
    // # it may be a pointing scan which may have different number of polarizations than what
    // # the TARGET and BANDPASS calibrator will have.
    // # -- T. Hunter
    // uInt _getNumberOfPolarizations();

    void _setSpwInfo(const MeasurementSet& ms);

    // set metadata from OBSERVATION table
    void _setObservation(const MeasurementSet& ms);

    Bool _cacheUpdated(const Float incrementInBytes) const;

    void _checkField(uInt fieldID) const;

    void _checkScan(const ScanKey& key) const;

    void _checkScans(const std::set<ScanKey>& scanKeys) const;

    void _checkSubScan(const SubScanKey& key) const;

    static void _checkTolerance(const Double tol);

    void _computeScanAndSubScanProperties(
        std::shared_ptr<std::map<ScanKey, MSMetaData::ScanProperties> >& scanProps,
        std::shared_ptr<std::map<SubScanKey, MSMetaData::SubScanProperties> >& subScanProps,
        Bool showProgress
    ) const;

    static void _getScalarIntColumn(
        Vector<Int>& v, TableProxy& table, const String& colname,
        Int beginRow, Int nrows
    );

    static void _getScalarDoubleColumn(
        Vector<Double>& v, TableProxy& table, const String& colname,
        Int beginRow, Int nrows
    );

    static void _getScalarQuantDoubleColumn(
        Quantum<Vector<Double> >& v, TableProxy& table, const String& colname,
        Int beginRow, Int nrows
    );

    void _mergeScanProps(
        std::shared_ptr<std::map<ScanKey, MSMetaData::ScanProperties> >& scanProps,
        std::shared_ptr<std::map<SubScanKey, MSMetaData::SubScanProperties> >& subScanProps,
        const std::vector<
            std::pair<std::map<ScanKey, ScanProperties>, std::map<SubScanKey, SubScanProperties> >
        >&  props
    ) const;

    void _createScanRecords(
        Record& parent, const ArrayKey& arrayKey,
        const std::map<SubScanKey, SubScanProperties>& subScanProps
    ) const;

    void _createSubScanRecords(
        Record& parent, uInt& scanNRows, std::set<Int>& antennasForScan,
        const ScanKey& scanKey, const std::map<SubScanKey, SubScanProperties>& subScanProps
    ) const;

    static void _createTimeStampRecords(
        Record& parent,
        const SubScanProperties& subScanProps
    );

    // convert a QVD in frequency units to velocity units using
    // the give reference frequency. No explicit checking is done
    // for unit correctness of the inputs.
    static QVD _freqWidthToVelWidth(const QVD& v, const Quantity& refFreq);

    // if _scanProps has been generated, just return it. If the caller has
    // configured the object to generate _scanProps at some point, this call will
    // generate it. Otherwise, the returned object contains a null pointer.
    std::shared_ptr<const std::map<ScanKey, ScanProperties> > _generateScanPropsIfWanted() const;

    // if _subScanProperties has been generated, just return it. If
    // the caller has configured the object to generate _subScanPropertiess
    // at some point, this call will generate it. Otherwise, the returned object
    // contains a null pointer.
    std::shared_ptr<const std::map<SubScanKey, SubScanProperties> >
    _generateSubScanPropsIfWanted() const;

    vector<String> _getAntennaNames(
        std::map<String, std::set<uInt> >& namesToIDsMap
    ) const;

    vector<MPosition> _getAntennaPositions() const;

    void _getAntennas(
        std::shared_ptr<Vector<Int> >& ant1,
        std::shared_ptr<Vector<Int> >& ant2
    ) const;

    std::shared_ptr<Vector<Int> > _getArrayIDs() const;

    std::map<ArrayKey, std::set<SubScanKey> > _getArrayKeysToSubScanKeys() const;

    // Uses openmp for parallel processing
    std::pair<std::map<ScanKey, ScanProperties>, std::map<SubScanKey, SubScanProperties> >
    _getChunkSubScanProperties(
        const Vector<Int>& scans, const Vector<Int>& fields,
        const Vector<Int>& ddIDs, const Vector<Int>& states,
        const Vector<Double>& times, const Vector<Int>& arrays,
        const Vector<Int>& observations, const Vector<Int>& ant1,
        const Vector<Int>& ant2, const Quantum<Vector<Double> >& exposureTimes,
        const Quantum<Vector<Double> >& intervalTimes, const vector<uInt>& ddIDToSpw,
        uInt beginRow, uInt endRow
    ) const;

    std::shared_ptr<Vector<Int> > _getDataDescIDs() const;

    // get the field IDs of ephemeris objects
    std::shared_ptr<std::set<Int> > _getEphemFieldIDs() const;

    std::shared_ptr<Quantum<Vector<Double> > > _getExposureTimes() const;

    std::shared_ptr<Vector<Int> > _getFieldIDs() const;

    // If there are no intents, then fieldToIntentsMap will be of length
    // nFields() and all of its entries will be the empty set, and
    // intentToFieldsMap will be empty
    void _getFieldsAndIntentsMaps(
        vector<std::set<String> >& fieldToIntentsMap,
        std::map<String, std::set<Int> >& intentToFieldsMap
    );

    void _getFieldsAndScansMaps(
        vector<std::set<ScanKey> >& fieldToScansMap,
        std::map<ScanKey, std::set<Int> >& scanToFieldsMap
    ) const;

    void _getFieldsAndSpwMaps(
        std::map<Int, std::set<uInt> >& fieldToSpwMap,
        vector<std::set<Int> >& spwToFieldMap
    ) const;

    void _getFieldsAndStatesMaps(
        std::map<Int, std::set<Int> >& fieldToStatesMap,
        std::map<Int, std::set<Int> >& stateToFieldsMap
    );

    void _getFieldsAndTimesMaps(
        std::shared_ptr<std::map<Int, std::set<Double> > >& fieldToTimesMap,
        std::shared_ptr<std::map<Double, std::set<Int> > >& timesToFieldMap
    );

    std::shared_ptr<ArrayColumn<Bool> > _getFlags() const;

    std::map<String, std::set<Double> > _getIntentsToTimesMap() const;

    std::shared_ptr<Quantum<Vector<Double> > > _getIntervals() const;

    std::shared_ptr<Vector<Int> > _getObservationIDs() const;

    std::shared_ptr<Vector<Int> > _getScans() const;

    vector<std::set<String> > _getSpwToIntentsMap();

    std::shared_ptr<Vector<Int> > _getStateIDs() const;

    std::shared_ptr<Vector<Double> > _getTimes() const;

    //std::shared_ptr<std::map<Double, TimeStampProperties> > _getTimeStampProperties() const;

    Bool _hasIntent(const String& intent) const;

    Bool _hasFieldID(Int fieldID) const;

    Bool _hasStateID(Int stateID) const;

    void _hasAntennaID(Int antennaID);

    std::map<Double, Double> _getTimeToTotalBWMap(
        const Vector<Double>& times, const Vector<Int>& ddIDs
    );

    MDirection _getInterpolatedDirection(
        const ROMSPointingColumns& pCols, const Int& index,
        const Double& time
    ) const;

    //map<SubScanKey, Quantity> _getMeanExposureTimes() const;

    vector<std::set<Int> > _getObservationIDToArrayIDsMap() const;

    vector<MPosition> _getObservatoryPositions();

    void _getRowStats(
        uInt& nACRows, uInt& nXCRows,
        std::map<SubScanKey, uInt>*& subScanToNACRowsMap,
        std::map<SubScanKey, uInt>*& subScanToNXCRowsMap,
        std::map<Int, uInt>*& fieldToNACRowsMap,
        std::map<Int, uInt>*& fieldToNXCRowsMap
    ) const;

    void _getRowStats(
        uInt& nACRows, uInt& nXCRows,
        std::shared_ptr<std::map<SubScanKey, uInt> >& scanToNACRowsMap,
        std::shared_ptr<std::map<SubScanKey, uInt> >& scanToNXCRowsMap,
        std::shared_ptr<std::map<Int, uInt> >& fieldToNACRowsMap,
        std::shared_ptr<std::map<Int, uInt> >& fieldToNXCRowsMap
    ) const;

    // get scan properties
    std::shared_ptr<const std::map<ScanKey, MSMetaData::ScanProperties> > _getScanProperties(
        Bool showProgress
    ) const;

    // get the scan keys in the specified set that have the associated arrayKey
    std::set<ScanKey> _getScanKeys(
        const std::set<ScanKey>& scanKeys, const ArrayKey& arrayKey
    ) const;

    // get all valid scan numbers associated with the specified arrayKey
    std::set<Int> _getScanNumbers(const ArrayKey& arrayKey) const;

    void _getScansAndDDIDMaps(
        std::map<ScanKey, std::set<uInt> >& scanToDDIDMap,
        vector<std::set<ScanKey> >& ddIDToScanMap
    ) const;

    void _getScansAndIntentsMaps(
        std::map<ScanKey, std::set<String> >& scanToIntentsMap,
        std::map<String, std::set<ScanKey> >& intentToScansMap
    ) const;

    void _getScansAndSpwMaps(
        std::map<ScanKey, std::set<uInt> >& scanToSpwMap,
        vector<std::set<ScanKey> >& spwToScanMap
    ) const;

    std::map<ScanKey, std::set<Int> > _getScanToAntennasMap() const;

    std::map<ScanKey, std::set<SubScanKey> > _getScanToSubScansMap() const;

    std::shared_ptr<std::map<ScanKey, std::set<Double> > > _getScanToTimesMap() const;

    std::map<SourceKey, SourceProperties> _getSourceInfo() const;

    vector<SpwProperties> _getSpwInfo(
        std::set<uInt>& avgSpw, std::set<uInt>& tdmSpw,
        std::set<uInt>& fdmSpw, std::set<uInt>& wvrSpw,
        std::set<uInt>& sqldSpw
    ) const;

    void _getSpwsAndIntentsMaps(
        vector<std::set<String> >& spwToIntentsMap,
        std::map<String, std::set<uInt> >& intentToSpwsMap
    );

    vector<SpwProperties>  _getSpwInfo2(
        std::set<uInt>& avgSpw, std::set<uInt>& tdmSpw, std::set<uInt>& fdmSpw,
        std::set<uInt>& wvrSpw, std::set<uInt>& sqldSpw
    ) const;

    void _getStateToIntentsMap(
        vector<std::set<String> >& statesToIntentsMap,
        std::set<String>& uniqueIntents
    ) const;

    vector<String> _getStationNames();

    void _getSubScansAndIntentsMaps(
        std::shared_ptr<const std::map<SubScanKey, std::set<String> > >& subScanToIntentsMap,
        std::map<String, std::set<SubScanKey> >& intentToSubScansMap
    ) const;

    void _getScanAndSubScanProperties(
        std::shared_ptr<const std::map<ScanKey, ScanProperties> >& scanProps,
        std::shared_ptr<const std::map<SubScanKey, SubScanProperties> >& subScanProps,
        Bool showProgress
    ) const;

    std::set<SubScanKey> _getSubScanKeys() const;

    // get subscans related to the given scan
    std::set<SubScanKey> _getSubScanKeys(const ScanKey& scanKey) const;

    void _getUnflaggedRowStats(
        Double& nACRows, Double& nXCRows,
        std::shared_ptr<std::map<SubScanKey, Double> >& subScanToNACRowsMap,
        std::shared_ptr<std::map<SubScanKey, Double> >& subScanToNXCRowsMap,
        std::shared_ptr<vector<Double> >& fieldToNACRowsMap,
        std::shared_ptr<vector<Double> >& fieldToNXCRowsMap
    ) const;

    void _getUnflaggedRowStats(
        Double& nACRows, Double& nXCRows,
        vector<Double>*& fieldNACRows, vector<Double>*& fieldNXCRows,
        std::map<SubScanKey, Double>*& scanNACRows,
        std::map<SubScanKey, Double>*& scanNXCRows
    ) const;

    static void _modifyFirstExposureTimeIfNecessary(
        FirstExposureTimeMap& current, const FirstExposureTimeMap& test
    );

    static void _modifyFirstExposureTimeIfNecessary(
        FirstExposureTimeMap& current, Int dataDescID,
        Double time, Double exposure, const Unit& eunit
    );

    static uInt _sizeof(const std::map<Double, MSMetaData::TimeStampProperties> & m);

    template <class T>
    static uInt _sizeof(const std::map<T, std::set<String> >& m);

    template <class T, class U>
    static uInt _sizeof(const std::map<T, std::set<U> >& m);

    template <class T, class U>
    static uInt _sizeof(const std::map<T, U>& m);

    static uInt _sizeof(const vector<std::set<String> >& m);

    static uInt _sizeof(const vector<String>& m);

    static uInt _sizeof(const vector<vector<String> >& m);

    template <class T>
    static uInt _sizeof(const vector<T>& v);

    static uInt _sizeof(const Quantum<Vector<Double> >& m);

    template <class T>
    static uInt _sizeof(const vector<std::set<T> >& v);

    template <class T> static uInt _sizeof(const std::map<String, std::set<T> >& map);

    static uInt _sizeof(const vector<std::map<Int, Quantity> >& map);

    static uInt _sizeof(const std::map<std::pair<Int, uInt>, std::set<uInt> >& map);

    static std::map<Int, uInt> _toUIntMap(const Vector<Int>& v);

    template <class T> std::shared_ptr<Vector<T> > _getMainScalarColumn(
        MSMainEnums::PredefinedColumns col
    ) const;

};



}

#endif
