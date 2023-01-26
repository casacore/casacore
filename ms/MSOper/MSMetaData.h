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
        std::set<int32_t> ddIDs;
        rownr_t nrows;
    };

    struct ColumnStats {
        double max;
        double median;
        double min;
    };

    typedef std::map<int32_t, std::pair<double, Quantity> > FirstExposureTimeMap;

    struct SubScanProperties {
        // number of auto-correlation rows
        rownr_t acRows;
        // number of cross-correlation rows.
        rownr_t xcRows;
        std::set<int32_t> antennas;
        double beginTime;
        std::set<uint32_t> ddIDs;
        double endTime;
        // the key is the spwID, the value is the meanInterval for
        // the subscan and that spwID
        std::map<uint32_t, Quantity> meanInterval;
        // The int32_t represents the data description ID,
        // The double represents the time of the first time stamp,
        // The Quantity represents the exposure time for the corresponding
        // data description ID and time stamp
        FirstExposureTimeMap firstExposureTime;
        Quantity meanExposureTime;
        std::set<uint32_t> spws;
        // number of rows for each spectral window
        std::map<uint32_t, rownr_t> spwNRows;
        std::set<int32_t> stateIDs;
        std::map<double, TimeStampProperties> timeProps;
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
    MSMetaData(const MeasurementSet *const &ms, const float maxCacheSizeMB);

    virtual ~MSMetaData();

    // get the antenna diameters
    QVD getAntennaDiameters() const;

    // if the antenna name appears multiple times in the antenna table, the *last* ID
    // that it is associated with is returned.
    uint32_t getAntennaID(const String& antennaName) const;

    // get all the antenna IDs for the antenna with the specified name. 
    std::set<uint32_t> getAntennaIDs(const String& antennaName) const;

    // The returned IDs are ordered in the way they appear in the atenna table
    vector<std::set<uint32_t> > getAntennaIDs(const vector<String>& antennaNames) const;

    // In the first instance of getAntennaNames, namesToID map will have the *last* ID
    // of the antenna name, if it appears multiple times in the antenna table. In the second
    // occurrence, namesToIDsMap will have the full set of IDs for antenna names that appear
    // multiple times.

    vector<String> getAntennaNames(
        std::map<String, uint32_t>& namesToIDsMap,
        const vector<uint32_t>& antennaIDs=vector<uint32_t>(0)
    ) const;

    vector<String> getAntennaNames(
        std::map<String, std::set<uint32_t> >& namesToIDsMap,
        const vector<uint32_t>& antennaIDs=vector<uint32_t>(0)
    ) const;

    // get the antenna stations for the specified antenna IDs
    vector<String> getAntennaStations(const vector<uint32_t>& antennaIDs=vector<uint32_t>());

    // get the antenna stations for the specified antenna names. The outer vector is ordered
    // respective to antennaNames. Because an antenna name can appear more than once in
    // the antenna table, the inner vector is ordered by row number in which that antenna name
    // appears.
    vector<std::vector<String> > getAntennaStations(const vector<String>& antennaNames);

    // get the set of antenna IDs for the specified scan.
    std::set<int32_t> getAntennasForScan(const ScanKey& scan) const;

    // POLARIZATION.CORR_PRODUCT
    vector<Array<int32_t> > getCorrProducts() const;

    // POLARIZATION.CORR_TYPE
    vector<vector<int32_t> > getCorrTypes() const;

    vector<uint32_t> getDataDescIDToSpwMap() const;

    vector<uint32_t> getDataDescIDToPolIDMap() const;

    // Get the FIELD.SOURCE_ID column.
    vector<int32_t> getFieldTableSourceIDs() const;

    // get the mapping of field ID to scans
    vector<std::set<ScanKey> > getFieldToScansMap() const;

    std::map<String, std::set<int32_t> > getIntentToFieldsMap();

    std::map<String, std::set<ScanKey> > getIntentToScansMap();

    std::map<String, std::set<uint32_t> > getIntentToSpwsMap();

    std::set<String> getIntentsForScan(const ScanKey& scan) const;

    std::set<String> getIntentsForSubScan(const SubScanKey& subScan) const;

    std::shared_ptr<const std::map<SubScanKey, std::set<String> > > getSubScanToIntentsMap() const;

    // get all intents, in no particular (nor guaranteed) order.
    std::set<String> getIntents() const;

    // get a set of intents corresponding to a specified field
    std::set<String> getIntentsForField(int32_t fieldID);

    // get a set of intents corresponding to a specified field name
    std::set<String> getIntentsForField(String field);

    // get a set of intents corresponding to the specified spectral window
    std::set<String> getIntentsForSpw(const uint32_t spw);

    // number of correlations from the polarization table.
    vector<int32_t> getNumCorrs() const;

    //SOURCE.PROPER_MOTION, first value in pair is longitudinal proper motion,
    // second is latiduninal
    vector<std::pair<Quantity, Quantity> > getProperMotions() const;

    // get unique scan numbers
    std::set<int32_t> getScanNumbers(int32_t obsID, int32_t arrayID) const;

    // get a set of scan numbers for the specified stateID, obsID, and arrayID.
    // If obsID and/or arrayID is negative, all observation IDs and/or array IDs
    // will be used.
    std::set<int32_t> getScansForState(
        int32_t stateID, int32_t obsID, int32_t arrayID
    ) const;

    // get the mapping of scans to states
    std::map<ScanKey, std::set<int32_t> > getScanToStatesMap() const;

    // SOURCE.DIRECTION
    vector<MDirection> getSourceDirections() const;

    // SOURCE.NAME
    vector<String> getSourceNames() const;

    // Get the SOURCE.SOURCE_ID column. This is a very unfortunate column name,
    // because generally an "ID" column of the table with the same name refers to
    // the row number in that table. But not in this case.
    vector<int32_t> getSourceTableSourceIDs() const;

    // SOURCE.TIME
    std::shared_ptr<const Quantum<Vector<double> > > getSourceTimes() const;

    // get a set of spectral windows for which the specified <src>intent</src>
    // applies.
    virtual std::set<uint32_t> getSpwsForIntent(const String& intent);

    // get the number of visibilities
    rownr_t nRows() const;

    rownr_t nRows(CorrelationType cType);

    std::shared_ptr<const std::map<SubScanKey, rownr_t> > getNRowMap(CorrelationType type) const;

    rownr_t nRows(
        CorrelationType cType, int32_t arrayID, int32_t observationID,
        int32_t scanNumber, int32_t fieldID
    ) const;

    rownr_t nRows(CorrelationType cType, uint32_t fieldID) const;

    // get number of spectral windows
    uint32_t nSpw(bool includewvr) const;

    // number of unique states (number of rows from the STATE table)
    uint32_t nStates() const;

    // get the number of fields.
    uint32_t nFields() const;

    // get a mapping of spectral window ID to data descrption IDs
    std::vector<std::set<uint32_t> > getSpwToDataDescriptionIDMap() const;

    // get a set of spectral windows corresponding to the specified fieldID
    std::set<uint32_t> getSpwsForField(const int32_t fieldID) const;

    // get a set of spectral windows corresponding to the specified field name
    std::set<uint32_t> getSpwsForField(const String& fieldName);

    // get the values of the CODE column from the field table
    vector<String> getFieldCodes() const;

    // get the set of field IDs corresponding to the specified spectral window.
    std::set<int32_t> getFieldIDsForSpw(const uint32_t spw);

    // get the set of field names corresponding to the specified spectral window.
    std::set<String> getFieldNamesForSpw(const uint32_t spw);

    // get the mapping of fields to spws
    std::map<int32_t, std::set<uint32_t> > getFieldsToSpwsMap() const;

    // get rest frequencies from the SOURCE table
    std::map<SourceKey, std::shared_ptr<vector<MFrequency> > > getRestFrequencies() const;

    // get the set of spectral windows for the specified scan.
    std::set<uint32_t> getSpwsForScan(const ScanKey& scan) const;

    // get the set of spectral windows for the specified subscan.
    std::set<uint32_t> getSpwsForSubScan(const SubScanKey& subScan) const;

    // get the set of scan numbers for the specified spectral window.
    std::set<int32_t> getScansForSpw(uint32_t spw, int32_t obsID, int32_t arrayID) const;

    // get the complete mapping of scans to spws
    std::map<ScanKey, std::set<uint32_t> > getScanToSpwsMap() const;

    // get the complete mapping of spws to scans
    std::vector<std::set<ScanKey> > getSpwToScansMap() const;

    // get the transitions from the SOURCE table. If there are no transitions
    // for a particular key, the shared ptr contains the null ptr.
    std::map<SourceKey, std::shared_ptr<vector<String> > > getTransitions() const;

    // get the number of antennas in the ANTENNA table
    uint32_t nAntennas() const;

    // ALMA-specific. get set of spectral windows used for TDM. These are windows that have
    // 64, 128, or 256 channels
    std::set<uint32_t> getTDMSpw();

    // ALMA-specific. get set of spectral windows used for FDM. These are windows that do not
    // have 1, 4, 64, 128, or 256 channels.
    std::set<uint32_t> getFDMSpw();

    // ALMA-specific. get spectral windows that have been averaged. These are windows with 1 channel.
    std::set<uint32_t> getChannelAvgSpw();

    // ALMA-specific. Get the spectral window set used for WVR measurements. These have 4 channels each.
    std::set<uint32_t> getWVRSpw() const;

    // ALMA-specific. Get the square law detector (total power) spectral windows.
    std::set<uint32_t> getSQLDSpw();

    // Get the scan numbers which fail into the specified time range (center-tol to center+tol),
    // inclusive. A negative value of obsID and/or arrayID indicates that all observation IDs
    // and/or all arrayIDs should be used.
    std::set<int32_t> getScansForTimes(
        double center, double tol, int32_t obsID, int32_t arrayID
    ) const;

    // Get the times for the specified scans
    std::set<double> getTimesForScans(std::set<ScanKey> scans) const;

    // get the times for the specified scan.
    // The return values come from the TIME column.
    std::set<double> getTimesForScan(const ScanKey& scan) const;

    std::map<uint32_t, std::set<double> > getSpwToTimesForScan(const ScanKey& scan) const;

    // get the time range for the specified scan. The pair will contain
    // the start and stop time of the scan, determined from min(TIME(x)-0.5*INTERVAL(x)) and
    // max(TIME(x)-0.5*INTERVAL(x))
    std::pair<double, double> getTimeRangeForScan(const ScanKey& scanKey) const;

    // get the map of scans to time ranges.
    std::shared_ptr<const std::map<ScanKey, std::pair<double,double> > > getScanToTimeRangeMap() const;

    // get the stateIDs associated with the specified scan. If obsID and/or arrayID
    // is negative, all observation IDs and/or array IDs will be used.
    std::set<int32_t> getStatesForScan(int32_t obsID, int32_t arrayID, int32_t scan) const;

    // get a map of spectral windows to unique timestamps.
    std::vector<std::set<double> > getTimesForSpws(bool showProgress=true) const;

    // get the position of the specified antenna relative to the observatory position.
    // the three vector returned represents the longitudinal, latitudinal, and elevation
    // offsets (elements 0, 1, and 2 respectively). The longitude and latitude offsets are
    // measured along the surface of a sphere centered at the earth's center and whose surface
    // intersects the position of the observatory.
    QVD getAntennaOffset(uint32_t which) const;

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
        const vector<uint32_t>& which=std::vector<uint32_t>(0)
    ) const;

    // <src>names</src> cannot be empty.
    vector<vector<MPosition> > getAntennaPositions(const vector<String>& names);

    // the first key in the returned map is the spectral window ID, the second is
    // the average interval for the specified scan for that spw.
    std::map<uint32_t, double> getAverageIntervalsForScan(const ScanKey& scan) const;

    // the first key in the returned map is the spectral window ID, the second is
    // the average interval for the specified sub scan for that spw.
    std::map<uint32_t, Quantity> getAverageIntervalsForSubScan(const SubScanKey& subScan) const;

    vector<uint32_t> getBBCNos() const;

    vector<String> getCorrBits() const;

    std::map<uint32_t, std::set<uint32_t> > getBBCNosToSpwMap(SQLDSwitch sqldSwitch);

    vector<vector<double> > getEdgeChans();
    //Get the phase direction for a given field id and epoch
    //interpolate polynomial if it is the field id  is such or use ephemerides table 
    //if that is attached to that field id
    MDirection phaseDirFromFieldIDAndTime(const uint32_t fieldID,  
                          const MEpoch& ep=MEpoch(Quantity(0.0, Unit("s")))) const ;

    // Get the reference direction for a given field ID and epoch interpolate
    // polynomial if it is the field ID is such or use ephemerides table
    // if that is attached to that field ID
    MDirection getReferenceDirection(
        const uint32_t fieldID,
        const MEpoch& ep=MEpoch(Quantity(0.0, Unit("s")))
    ) const;
    
    // get the field IDs for the specified field name. Case insensitive.
    std::set<int32_t> getFieldIDsForField(const String& field) const;
    
    // get a list of the field names in the order in which they appear in the
    // FIELD table.
    vector<String> getFieldNames() const;

    // get field IDs associated with the specified scan number.
    std::set<int32_t> getFieldsForScan(const ScanKey& scan) const;

    // get the field IDs associated with the specified scans
    std::set<int32_t> getFieldsForScans(
        const std::set<int32_t>& scans, int32_t obsID, int32_t arrayID
    ) const;

    // get the field IDs associated with the specified scans
    std::set<int32_t> getFieldsForScans(const std::set<ScanKey>& scans) const;

    // get the field IDs associated with the specified intent.
    std::set<int32_t> getFieldsForIntent(const String& intent);

    // get the field IDs associated with the specified source.
    std::set<int32_t> getFieldsForIntent(uint32_t sourceID) const;

    std::map<int32_t, std::set<int32_t> > getFieldsForSourceMap() const;

    std::map<int32_t, std::set<String> > getFieldNamesForSourceMap() const;

    // get the field names associated with the specified field IDs. If <src>fieldIDs</src>
    // is empty, a vector of all the field names is returned.
    vector<String> getFieldNamesForFieldIDs(const vector<uint32_t>& fieldIDs);

    // Get the fields which fail into the specified time range (center-tol to center+tol)
    std::set<int32_t> getFieldsForTimes(double center, double tol);

    // max cache size in MB
    float getMaxCacheSizeMB() const { return _maxCacheMB; }

    // get telescope names in the order they are listed in the OBSERVATION table. These are
    // the telescopes (observatories), not the antenna names.
    vector<String> getObservatoryNames();

    // get the position of the specified telescope (observatory).
    MPosition getObservatoryPosition(uint32_t which) const;

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
    std::set<int32_t> getScansForIntent(
        const String& intent, int32_t obsID, int32_t arrayID
    ) const;

    // get the scan numbers associated with the specified field ID.
    std::set<int32_t> getScansForFieldID(int32_t fieldID, int32_t obsID, int32_t arrayID) const;

    // get the scan numbers associated with the specified field. Subclasses should not implement or override.
    std::set<int32_t> getScansForField(const String& field, int32_t obsID, int32_t arrayID) const;

    // The first value of the pair is spw, the second is polarization ID.
    std::map<std::pair<uint32_t, uint32_t>, uint32_t> getSpwIDPolIDToDataDescIDMap() const;

    // get a map of the spwIDs to spw names from the spw table
    vector<String> getSpwNames() const;

    // get all the spws associated with the data description IDs listed in the main table.
    // This will not correspond to a list of the row numbers in the SPECTRAL_WINDOW table
    // if there are data description IDs that are not in the main table.
    std::set<uint32_t> getSpwIDs() const;

    // get all sub scan keys for the specified array key.
    std::set<SubScanKey> getSubScanKeys(const ArrayKey& arrayKey) const;

    // get the sub scan properties for the specified sub scan.

    SubScanProperties getSubScanProperties(
        const SubScanKey& subScan, bool showProgress=false
    ) const;

    std::shared_ptr<const std::map<SubScanKey, SubScanProperties> > getSubScanProperties(
        bool showProgress=false
    ) const;

    // If true, force the subscan properties structure to be
    // cached regardless of the stipulations on the maximum cache. Normally,
    // the subscan properties structure is small compared to the size of any
    // one column that is necessary to create it, and since creating this
    // structure can be very expensive, especially for large datasets, it
    // is often a good idea to cache it if it will be accessed many times.
    void setForceSubScanPropsToCache(bool b) { _forceSubScanPropsToCache = b; }

    // get a data structure, consumable by users, representing a summary of the dataset
    Record getSummary() const;

    // get the times for which the specified field was observed
    std::set<double> getTimesForField(int32_t fieldID);

    // get the time stamps associated with the specified intent
    std::set<double> getTimesForIntent(const String& intent) const;
    bool hasBBCNo() const;

    //std::map<double, double> getExposuresForTimes() const;

    // get the unique baselines in the MS. These are not necessarily every combination of the
    // n(n-1)/2 possible antenna pairs, but rather the number of unique baselines represented in
    // the main MS table, which in theory can be less than n(n-1)/2 (for example if samples for
    // certain antenna pairs are not recorded. The returned Matrix is nAnts x nAnts in size. Pairs
    // that are true represent baselines represented in the main MS table.
    Matrix<bool> getUniqueBaselines();

    // get the number of unique baselines represented in the main MS table which in theory can be
    // less than n*(n-1)/2. If <src>includeAutoCorrelation</src> is true, include autocorrelation
    // "baselines" in the enumeration.
    virtual uint32_t nBaselines(bool includeAutoCorrelation=false);

    // get the effective total exposure time. This is the effective time spent collecting unflagged data.
    Quantity getEffectiveTotalExposureTime();

    // get the number of scans in the dataset
    uint32_t nScans();

    // get the number of observations (from the OBSERVATIONS table) in the dataset
    uint32_t nObservations() const;

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
    uint32_t nArrays();

    // get the number of data description IDs (from the DATA_DESCRIPTION table)
    uint32_t nDataDescriptions() const;

    // get the number of unflagged rows
    double nUnflaggedRows() const;

    double nUnflaggedRows(CorrelationType cType) const;

    double nUnflaggedRows(
        CorrelationType cType, int32_t arrayID, uint32_t observationID,
        int32_t scanNumber, uint32_t fieldID
    ) const;

    double nUnflaggedRows(CorrelationType cType, int32_t fieldID) const;

    inline float getCache() const { return _cacheMB;}

    vector<double> getBandWidths() const;

    vector<Quantity> getCenterFreqs() const;

    // get the effective bandwidth for each channel. Each element in
    // the returned vector represents a separate spectral window, with
    // ID given by its location in the vector. If asVelWidths is true,
    // convert the values to velocity widths.
    vector<QVD> getChanEffectiveBWs(bool asVelWidths) const;

    vector<QVD > getChanFreqs() const;

    // get the resolution for each channel. Each element in
    // the returned vector represents a separate spectral window, with
    // ID given by its location in the vector. If asVelWidths is true,
    // convert the values to velocity widths.
    vector<QVD> getChanResolutions(bool asVelWidths) const;

    vector<QVD > getChanWidths() const;

    vector<Quantity> getMeanFreqs() const;

    vector<int32_t> getNetSidebands() const;

    vector<MFrequency> getRefFreqs() const;

    vector<uint32_t> nChans() const;

    uint32_t nPol();

    // DEPRECATED
    // get a map of data desc ID, scan number pair to exposure time for the first time
    // for that data desc ID, scan number pair
    std::vector<std::map<int32_t, Quantity> > getFirstExposureTimeMap();

    // get map of scans to first exposure times
    std::map<ScanKey, FirstExposureTimeMap> getScanToFirstExposureTimeMap(bool showProgress) const;

    // get polarization IDs for the specified scan and spwid
    std::set<uint32_t> getPolarizationIDs(uint32_t obsID, int32_t arrayID, int32_t scan, uint32_t spwid) const;

    // get the unique antennas (the union of the ANTENNA_1 and ANTENNA_2 columns) from
    // the main table
    const std::set<int32_t>& getUniqueAntennaIDs() const;

    // get unique data description IDs that exist in the main table
    std::set<uint32_t> getUniqueDataDescIDs() const;

    // DEPRECATED because of spelling error. Use getUniqueFieldIDs()
    // instead.
    inline std::set<int32_t> getUniqueFiedIDs() const {
        return getUniqueFieldIDs();
    }

    // get unique field IDs that exist in the main table.
    std::set<int32_t> getUniqueFieldIDs() const;

    // get the pointing directions associated with antenna1 and antenna2 for
    // the specified row of the main MS table
    std::pair<MDirection, MDirection> getPointingDirection(
        int32_t& ant1, int32_t& ant2, double& time, rownr_t row,
        bool interpolate=false, int32_t initialguess=0
    ) const;

    // get the time range for the entire dataset. min(TIME(x) - 0.5*INTERVAL(x)) to
    // max(TIME(x) + 0.5*INTERVAL(x))
    std::pair<double, double> getTimeRange(bool showProgress=false) const;

    // Number of unique values from SOURCE.SOURCE_ID
    uint32_t nUniqueSourceIDsFromSourceTable() const;

    // get the unique spectral window IDs represented by the data description
    // IDs that appear in the main table
    std::set<uint32_t> getUniqueSpwIDs() const;

    const MeasurementSet* getMS() const { return _ms; }

    void setShowProgress(bool b) { _showProgress = b; }

    // get statistics related to the values of the INTERVAL column. Returned
    // values are in seconds. All values in this column are used in the computation,
    // including those which associated row flags may be set. 
    ColumnStats getIntervalStatistics() const;

private:

    struct ScanProperties {
        // The int32_t represents the data description ID,
        // The double represents the time of the first time stamp,
        // The Quantity represents the exposure time for the corresponding
        // data description ID and time stamp
        FirstExposureTimeMap firstExposureTime;
        // the key is the spwID, the value is the meanInterval for
        // the subscan and that spwID
        std::map<uint32_t, Quantity> meanInterval;
        // number of rows for each spectral window
        std::map<uint32_t, rownr_t> spwNRows;
        // time range (which takes into account helf of the corresponding
        // interval, which is not accounted for in the SubScanProperties times
        std::pair<double, double> timeRange;
        // times for each spectral window
        std::map<uint32_t, std::set<double> > times;
    };

    struct SpwProperties {
        double bandwidth;
        QVD chanfreqs;
        QVD chanwidths;
        int32_t netsideband;
        // The sum of all channel frequencies divided by the number of channels
        Quantity meanfreq;
        // The mean of the low frequency extent of the lowest frequency channel and
        // the high frequency extend of the highest frequency channel. Often, but not
        // necessarily, the same as meanfreq
        Quantity centerfreq;
        uint32_t nchans;
        // The center frequencies of the two channels at the edges of the window
        vector<double> edgechans;
        uint32_t bbcno;
        // from the REF_FREQUENCY column
        MFrequency reffreq;
        String name;
        // EFFECTIVE_BANDWIDTH
        QVD effbw;
        // RESOLUTION
        QVD resolution;
        // CAS-13749 value for adhoc ALMA-specific SPECTRAL_WINDOW column
        String corrbit;
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
    bool _showProgress;
    mutable float _cacheMB;
    const float _maxCacheMB;
    mutable rownr_t _nACRows, _nXCRows;
    mutable uint32_t _nStates, _nSpw, _nFields, _nAntennas,
        _nObservations, _nScans, _nArrays, _nrows, _nPol, _nDataDescIDs;
    mutable std::map<ScanKey, std::set<uint32_t> > _scanToSpwsMap, _scanToDDIDsMap;
    mutable vector<uint32_t> _dataDescIDToSpwMap, _dataDescIDToPolIDMap;
    mutable std::map<int32_t, std::set<uint32_t> > _fieldToSpwMap;
    mutable std::map<ScanKey, std::set<int32_t> > _scanToStatesMap, _scanToFieldsMap, _scanToAntennasMap;
    mutable std::map<int32_t, std::set<int32_t> >    _fieldToStatesMap, _stateToFieldsMap, _sourceToFieldsMap;
    mutable std::map<std::pair<uint32_t, uint32_t>, uint32_t> _spwPolIDToDataDescIDMap;
    mutable std::map<String, std::set<uint32_t> > _antennaNameToIDMap;
    mutable std::shared_ptr<const std::map<ScanKey, ScanProperties> > _scanProperties;
    mutable std::shared_ptr<const std::map<SubScanKey, SubScanProperties> > _subScanProperties;

    mutable std::map<String, std::set<int32_t> > _intentToFieldIDMap;
    mutable std::map<String, std::set<ScanKey> > _intentToScansMap;
    mutable std::map<String, std::set<SubScanKey> > _intentToSubScansMap;
    mutable std::map<std::pair<ScanKey, uint32_t>, std::set<uint32_t> > _scanSpwToPolIDMap;
    mutable std::set<String> _uniqueIntents;
    mutable std::set<int32_t>  _uniqueFieldIDs, _uniqueStateIDs, _uniqueAntennaIDs;
    mutable std::set<uint32_t> _avgSpw, _tdmSpw, _fdmSpw, _wvrSpw, _sqldSpw, _uniqueDataDescIDs;
    mutable std::shared_ptr<std::map<SubScanKey, rownr_t> > _subScanToNACRowsMap, _subScanToNXCRowsMap;
    mutable std::shared_ptr<std::map<int32_t, rownr_t> > _fieldToNACRowsMap, _fieldToNXCRowsMap;
    mutable std::map<ScanKey, std::set<String> > _scanToIntentsMap;
    mutable std::shared_ptr<const std::map<SubScanKey, std::set<String> > > _subScanToIntentsMap;
    mutable vector<std::set<String> > _stateToIntentsMap, _spwToIntentsMap, _fieldToIntentsMap;
    mutable vector<SpwProperties> _spwInfo;
    mutable vector<std::set<int32_t> > _spwToFieldIDsMap, _obsToArraysMap;
    mutable vector<std::set<ScanKey> > _spwToScansMap, _ddidToScansMap, _fieldToScansMap;

    mutable vector<String> _fieldNames, _antennaNames, _observatoryNames,
        _stationNames, _observers, _projects, _sourceNames, _fieldCodes;
    mutable vector<vector<String> > _schedules;
    mutable vector<vector<int32_t> > _corrTypes;
    mutable vector<Array<int32_t> >_corrProds;

    mutable std::shared_ptr<std::map<ScanKey, std::set<double> > > _scanToTimesMap;
    std::map<String, std::set<uint32_t> > _intentToSpwsMap;
    mutable std::map<String, std::set<double> > _intentToTimesMap;

    std::shared_ptr<std::map<int32_t, std::set<double> > > _fieldToTimesMap;
    std::shared_ptr<std::map<double, std::set<int32_t> > > _timeToFieldsMap;

    mutable vector<MPosition> _observatoryPositions, _antennaPositions;
    mutable vector<QVD > _antennaOffsets;
    mutable QVD _antennaDiameters;
    Matrix<bool> _uniqueBaselines;
    Quantity _exposureTime;
    mutable double _nUnflaggedACRows, _nUnflaggedXCRows;
    mutable std::shared_ptr<vector<double> > _unflaggedFieldNACRows, _unflaggedFieldNXCRows;
    mutable std::shared_ptr<std::map<SubScanKey, double> > _unflaggedSubScanNACRows, _unflaggedSubScanNXCRows;
    const String _taqlTableName;
    const vector<const Table*> _taqlTempTable;

    mutable bool _spwInfoStored, _forceSubScanPropsToCache;
    vector<std::map<int32_t, Quantity> > _firstExposureTimeMap;
    mutable vector<int32_t> _numCorrs, _source_sourceIDs, _field_sourceIDs;

    mutable std::set<ArrayKey> _arrayKeys;
    mutable std::set<ScanKey> _scanKeys;
    mutable std::set<SubScanKey> _subscans;
    mutable std::map<ScanKey, std::set<SubScanKey> > _scanToSubScans;
    mutable std::map<ArrayKey, std::set<SubScanKey> > _arrayToSubScans;

    mutable vector<std::pair<MEpoch, MEpoch> > _timeRangesForObs;

    mutable vector<MDirection> _phaseDirs, _sourceDirs;

    mutable vector<std::pair<Quantity, Quantity> > _properMotions;

    mutable std::map<SourceKey, SourceProperties> _sourceInfo;
    mutable std::shared_ptr<std::set<int32_t> > _ephemFields;
    mutable std::shared_ptr<const Quantum<Vector<double> > > _sourceTimes;

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
    // uint32_t _getNumberOfPolarizations();

    void _setSpwInfo(const MeasurementSet& ms);

    // set metadata from OBSERVATION table
    void _setObservation(const MeasurementSet& ms);

    bool _cacheUpdated(const float incrementInBytes) const;

    void _checkField(uint32_t fieldID) const;

    void _checkScan(const ScanKey& key) const;

    void _checkScans(const std::set<ScanKey>& scanKeys) const;

    void _checkSubScan(const SubScanKey& key) const;

    static void _checkTolerance(const double tol);

    void _computeScanAndSubScanProperties(
        std::shared_ptr<std::map<ScanKey, MSMetaData::ScanProperties> >& scanProps,
        std::shared_ptr<std::map<SubScanKey, MSMetaData::SubScanProperties> >& subScanProps,
        bool showProgress
    ) const;

    static void _getScalarIntColumn(
        Vector<int32_t>& v, TableProxy& table, const String& colname,
        rownr_t beginRow, rownr_t nrows
    );

    static void _getScalarDoubleColumn(
        Vector<double>& v, TableProxy& table, const String& colname,
        rownr_t beginRow, rownr_t nrows
    );

    static void _getScalarQuantDoubleColumn(
        Quantum<Vector<double> >& v, TableProxy& table, const String& colname,
        rownr_t beginRow, rownr_t nrows
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
        Record& parent, rownr_t& scanNRows, std::set<int32_t>& antennasForScan,
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
        std::map<String, std::set<uint32_t> >& namesToIDsMap
    ) const;

    vector<MPosition> _getAntennaPositions() const;

    void _getAntennas(
        std::shared_ptr<Vector<int32_t> >& ant1,
        std::shared_ptr<Vector<int32_t> >& ant2
    ) const;

    std::shared_ptr<Vector<int32_t> > _getArrayIDs() const;

    std::map<ArrayKey, std::set<SubScanKey> > _getArrayKeysToSubScanKeys() const;

    // Uses openmp for parallel processing
    std::pair<std::map<ScanKey, ScanProperties>, std::map<SubScanKey, SubScanProperties> >
    _getChunkSubScanProperties(
        const Vector<int32_t>& scans, const Vector<int32_t>& fields,
        const Vector<int32_t>& ddIDs, const Vector<int32_t>& states,
        const Vector<double>& times, const Vector<int32_t>& arrays,
        const Vector<int32_t>& observations, const Vector<int32_t>& ant1,
        const Vector<int32_t>& ant2, const Quantum<Vector<double> >& exposureTimes,
        const Quantum<Vector<double> >& intervalTimes, const vector<uint32_t>& ddIDToSpw,
        rownr_t beginRow, rownr_t endRow
    ) const;

    std::shared_ptr<Vector<int32_t> > _getDataDescIDs() const;

    // get the field IDs of ephemeris objects
    std::shared_ptr<std::set<int32_t> > _getEphemFieldIDs() const;

    std::shared_ptr<Quantum<Vector<double> > > _getExposureTimes() const;

    std::shared_ptr<Vector<int32_t> > _getFieldIDs() const;

    // If there are no intents, then fieldToIntentsMap will be of length
    // nFields() and all of its entries will be the empty set, and
    // intentToFieldsMap will be empty
    void _getFieldsAndIntentsMaps(
        vector<std::set<String> >& fieldToIntentsMap,
        std::map<String, std::set<int32_t> >& intentToFieldsMap
    );

    void _getFieldsAndScansMaps(
        vector<std::set<ScanKey> >& fieldToScansMap,
        std::map<ScanKey, std::set<int32_t> >& scanToFieldsMap
    ) const;

    void _getFieldsAndSpwMaps(
        std::map<int32_t, std::set<uint32_t> >& fieldToSpwMap,
        vector<std::set<int32_t> >& spwToFieldMap
    ) const;

    void _getFieldsAndStatesMaps(
        std::map<int32_t, std::set<int32_t> >& fieldToStatesMap,
        std::map<int32_t, std::set<int32_t> >& stateToFieldsMap
    );

    void _getFieldsAndTimesMaps(
        std::shared_ptr<std::map<int32_t, std::set<double> > >& fieldToTimesMap,
        std::shared_ptr<std::map<double, std::set<int32_t> > >& timesToFieldMap
    );

    std::shared_ptr<ArrayColumn<bool> > _getFlags() const;

    std::map<String, std::set<double> > _getIntentsToTimesMap() const;

    std::shared_ptr<Quantum<Vector<double> > > _getIntervals() const;

    std::shared_ptr<Vector<int32_t> > _getObservationIDs() const;

    std::shared_ptr<Vector<int32_t> > _getScans() const;

    vector<std::set<String> > _getSpwToIntentsMap();

    std::shared_ptr<Vector<int32_t> > _getStateIDs() const;

    std::shared_ptr<Vector<double> > _getTimes() const;

    //std::shared_ptr<std::map<double, TimeStampProperties> > _getTimeStampProperties() const;

    bool _hasIntent(const String& intent) const;

    bool _hasFieldID(int32_t fieldID) const;

    bool _hasStateID(int32_t stateID) const;

    void _hasAntennaID(int32_t antennaID);

    std::map<double, double> _getTimeToTotalBWMap(
        const Vector<double>& times, const Vector<int32_t>& ddIDs
    );

    MDirection _getInterpolatedDirection(
        const MSPointingColumns& pCols, const int32_t& index,
        const double& time
    ) const;

    //map<SubScanKey, Quantity> _getMeanExposureTimes() const;

    vector<std::set<int32_t> > _getObservationIDToArrayIDsMap() const;

    vector<MPosition> _getObservatoryPositions();

    void _getRowStats(
        rownr_t& nACRows, rownr_t& nXCRows,
        std::map<SubScanKey, rownr_t>*& subScanToNACRowsMap,
        std::map<SubScanKey, rownr_t>*& subScanToNXCRowsMap,
        std::map<int32_t, rownr_t>*& fieldToNACRowsMap,
        std::map<int32_t, rownr_t>*& fieldToNXCRowsMap
    ) const;

    void _getRowStats(
        rownr_t& nACRows, rownr_t& nXCRows,
        std::shared_ptr<std::map<SubScanKey, rownr_t> >& scanToNACRowsMap,
        std::shared_ptr<std::map<SubScanKey, rownr_t> >& scanToNXCRowsMap,
        std::shared_ptr<std::map<int32_t, rownr_t> >& fieldToNACRowsMap,
        std::shared_ptr<std::map<int32_t, rownr_t> >& fieldToNXCRowsMap
    ) const;

    // get scan properties
    std::shared_ptr<const std::map<ScanKey, MSMetaData::ScanProperties> > _getScanProperties(
        bool showProgress
    ) const;

    // get the scan keys in the specified set that have the associated arrayKey
    std::set<ScanKey> _getScanKeys(
        const std::set<ScanKey>& scanKeys, const ArrayKey& arrayKey
    ) const;

    // get all valid scan numbers associated with the specified arrayKey
    std::set<int32_t> _getScanNumbers(const ArrayKey& arrayKey) const;

    void _getScansAndDDIDMaps(
        std::map<ScanKey, std::set<uint32_t> >& scanToDDIDMap,
        vector<std::set<ScanKey> >& ddIDToScanMap
    ) const;

    void _getScansAndIntentsMaps(
        std::map<ScanKey, std::set<String> >& scanToIntentsMap,
        std::map<String, std::set<ScanKey> >& intentToScansMap
    ) const;

    void _getScansAndSpwMaps(
        std::map<ScanKey, std::set<uint32_t> >& scanToSpwMap,
        vector<std::set<ScanKey> >& spwToScanMap
    ) const;

    std::map<ScanKey, std::set<int32_t> > _getScanToAntennasMap() const;

    std::map<ScanKey, std::set<SubScanKey> > _getScanToSubScansMap() const;

    std::shared_ptr<std::map<ScanKey, std::set<double> > > _getScanToTimesMap() const;

    std::map<SourceKey, SourceProperties> _getSourceInfo() const;

    vector<SpwProperties> _getSpwInfo(
        std::set<uint32_t>& avgSpw, std::set<uint32_t>& tdmSpw,
        std::set<uint32_t>& fdmSpw, std::set<uint32_t>& wvrSpw,
        std::set<uint32_t>& sqldSpw
    ) const;

    void _getSpwsAndIntentsMaps(
        vector<std::set<String> >& spwToIntentsMap,
        std::map<String, std::set<uint32_t> >& intentToSpwsMap
    );

    vector<SpwProperties>  _getSpwInfo2(
        std::set<uint32_t>& avgSpw, std::set<uint32_t>& tdmSpw, std::set<uint32_t>& fdmSpw,
        std::set<uint32_t>& wvrSpw, std::set<uint32_t>& sqldSpw
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
        bool showProgress
    ) const;

    std::set<SubScanKey> _getSubScanKeys() const;

    // get subscans related to the given scan
    std::set<SubScanKey> _getSubScanKeys(const ScanKey& scanKey) const;

    void _getUnflaggedRowStats(
        double& nACRows, double& nXCRows,
        std::shared_ptr<std::map<SubScanKey, double> >& subScanToNACRowsMap,
        std::shared_ptr<std::map<SubScanKey, double> >& subScanToNXCRowsMap,
        std::shared_ptr<vector<double> >& fieldToNACRowsMap,
        std::shared_ptr<vector<double> >& fieldToNXCRowsMap
    ) const;

    void _getUnflaggedRowStats(
        double& nACRows, double& nXCRows,
        vector<double>*& fieldNACRows, vector<double>*& fieldNXCRows,
        std::map<SubScanKey, double>*& scanNACRows,
        std::map<SubScanKey, double>*& scanNXCRows
    ) const;

    static void _modifyFirstExposureTimeIfNecessary(
        FirstExposureTimeMap& current, const FirstExposureTimeMap& test
    );

    static void _modifyFirstExposureTimeIfNecessary(
        FirstExposureTimeMap& current, int32_t dataDescID,
        double time, double exposure, const Unit& eunit
    );

    static uint32_t _sizeof(const std::map<double, MSMetaData::TimeStampProperties> & m);

    template <class T>
    static uint32_t _sizeof(const std::map<T, std::set<String> >& m);

    template <class T, class U>
    static uint32_t _sizeof(const std::map<T, std::set<U> >& m);

    template <class T, class U>
    static uint32_t _sizeof(const std::map<T, U>& m);

    static uint32_t _sizeof(const vector<std::set<String> >& m);

    static uint32_t _sizeof(const vector<String>& m);

    static uint32_t _sizeof(const vector<vector<String> >& m);

    template <class T>
    static uint32_t _sizeof(const vector<T>& v);

    static uint32_t _sizeof(const Quantum<Vector<double> >& m);

    template <class T>
    static uint32_t _sizeof(const vector<std::set<T> >& v);

    template <class T> static uint32_t _sizeof(const std::map<String, std::set<T> >& map);

    static uint32_t _sizeof(const vector<std::map<int32_t, Quantity> >& map);

    static uint32_t _sizeof(const std::map<std::pair<int32_t, uint32_t>, std::set<uint32_t> >& map);

    static std::map<int32_t, uint32_t> _toUIntMap(const Vector<int32_t>& v);

    template <class T> std::shared_ptr<Vector<T> > _getMainScalarColumn(
        MSMainEnums::PredefinedColumns col
    ) const;

};

}

#endif
