//# MSSimulator.h: this defines the MeasurementSet Simulator
//# Copyright (C) 1995,1996,1998,1999,2000,2002
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

#if !defined(TRIAL_NEWMSSIMULATOR_H)
#define TRIAL_NEWMSSIMULATOR_H


//# Includes
#include <aips/Utilities/String.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Cube.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MEpoch.h>
//# Forward Declarations
class MeasurementSet;
class MDirection;
class MSFeed;
class MSFeedColumns;

// <summary> a container for data destined for an MS FEED record </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="" demos="">

// <prerequisite>
//   <li> <linkto class="MSSimulator">MSSimulator</linkto> class
// </prerequisite>
//
// <etymology>
// This is a container for use by MSSimulator to load an MS FEED record
// </etymology>
//
// <synopsis> 
//
// </synopsis> 
//
// <motivation>
// MSSimulator buffers FEED table data as rows (records, as opposed to columns) 
// because, in principle, the number of receptors could change row-to-row; 
// thus, the shape of columns could change row-to-row.
// </motivation>
//
// <todo asof="97/10/01">
// </todo>
class MSSimFeedRec {
public:

    // create the record container.  
    //   nrec is the number of receptors associated with this feed.
    //   types is a String containing the polarization codes for each 
    //      receptor in their proper order (with no intervening spaces); 
    //      the length of the string must either be 0 (to be set later 
    //      with setPolTypes()) or equal to number of receptors.
    //   ant is the ID for the antenna associated with this feed; a value of -1 
    //      means this applies to all antennae.
    //   spw is the ID for the spectral window associated with this feed; a 
    //      value of -1 means that this applies to all windows.
    MSSimFeedRec(uInt nrec=2, String types="", Int ant=-1, Int spw=-1) : 
	nrecp_p(nrec), antId_p(ant), spwId_p(spw), beamId_p(-1), 
	pols_p(), position_p(0), beamOff_p(0), polResp_p(0), angle_p(0) 
    { 
	if (types.length() > 0) setPolTypes(types);
    }

    virtual ~MSSimFeedRec() {
	if (beamOff_p) { delete beamOff_p; beamOff_p = 0; }
	if (polResp_p) { delete polResp_p; polResp_p = 0; }
	if (position_p) { delete position_p; position_p = 0; }
	if (angle_p) { delete angle_p; angle_p = 0; }
    }

    // set the values that make up the lookup key for this feed.  Negative 
    // values for ant and spw mean the record applies to all antennae and 
    // windows, respectively.  A value of 0 for time defaults to the start
    // time.
    void setKey(uInt feed, Int ant=-1, Int spw=-1, 
		Double time=0, Double interval=0) 
    {
	feedId_p = feed;
	antId_p = ant;
	spwId_p = spw;
	time_p = time;
	intv_p = interval;
//	beamId_p = beam;
    }

    uInt numReceptors() const { return nrecp_p; }
    uInt feedId() const { return feedId_p; }
    Int antId() const { return antId_p; }
    Int spwId() const { return spwId_p; }
    Int beamId() const { return beamId_p; }
    Double time() const { return time_p; }
    Double interval() const { return intv_p; }

    uInt nrows(uInt nants) const { return ((antId_p < 0) ? nants : 1); }

    void setPolTypes(const String &types) {
	if (types.length() < nrecp_p) 
	    throw AipsError("Insufficient no. of pols passed in '" + types +
			    "'; expected " + nrecp_p + ", got " + 
			    types.length());
	pols_p = types;
    }

    // set the Beam ID
    void setBeamID(Int id) { beamId_p = id; }

    Matrix<Double>& beamOffset() {
	if (! beamOff_p) beamOff_p = new Matrix<Double>(2, nrecp_p, 0.0);
	return *beamOff_p;
    }

    Matrix<Complex>& polResponse() {
	if (! polResp_p) {
	    polResp_p = new Matrix<Complex>(nrecp_p, nrecp_p);
	    polResp_p->diagonal() = Complex(1.0, 0.0);
	}
	return *polResp_p;
    }

    void setPosition(const MPosition &pos) { position_p = new MPosition(pos); }

    Vector<Double>& receptorAngle() {
	if (! angle_p) angle_p = new Vector<Double>(nrecp_p, 0.0);
	return *angle_p;
    }
    
    uInt write(MSFeed& msf, MSFeedColumns& msfc, uInt row, uInt nant=0) const;

protected:
    
private:
    uInt feedId_p, nrecp_p;
    Int antId_p, spwId_p, beamId_p;
    Double time_p, intv_p;

    String pols_p;

    MPosition *position_p;
    Matrix<Double> *beamOff_p;
    Matrix<Complex> *polResp_p;
    Vector<Double> *angle_p;
};

// <summary> a container for data destined for an MS FEED table </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="" demos="">

// <prerequisite>
//   <li> <linkto class="MSSimulator">MSSimulator</linkto> class
// </prerequisite>
//
// <etymology>
// This class is used by MSSimulator to buffer records to be written to 
// an MS FEED table.
// </etymology>
//
// <synopsis> 
//
// </synopsis> 
//
// <motivation>
// MSSimulator buffers FEED table data as rows (records, as opposed to columns) 
// because, in principle, the number of receptors could change row-to-row; 
// thus, the shape of columns could change row-to-row.
// </motivation>
//
// <todo asof="97/10/01">
// </todo>
class MSSimFeedBuf {
public:
    MSSimFeedBuf() : recs_p() { }
    virtual ~MSSimFeedBuf() { 
	for(uInt i=0; i < recs_p.nelements(); i++) {
	    if (recs_p[i]) delete recs_p[i];
	}
    }

    // count the number of rows assuming a given number of antennae
    uInt countRows(uInt nants) const {
	uInt nrows = 0;
	for(uInt i=0; i < recs_p.nelements(); i++) 
	    nrows += recs_p[i]->nrows(nants);
	return nrows;
    }

    Bool uniform() const {
	if (recs_p.nelements() == 0) return True;
	Bool nr = recs_p[0]->numReceptors();
	for(uInt i=1; i < recs_p.nelements(); i++) {
	    if (nr != recs_p[i]->numReceptors()) return False;
	}
	return True;
    }

    MSSimFeedRec& addRec(uInt nrecp, String types) {
	uInt next = recs_p.nelements();
	recs_p.resize(next + 1);
	recs_p[next] = new MSSimFeedRec(nrecp);
	recs_p[next]->setPolTypes(types);
	return *(recs_p[next]);
    }

    // write out the FEED table assuming a given number of antennae
    uInt write(MSFeed &msf, uInt nants) const;

private:
    Block<MSSimFeedRec*> recs_p;
};



// <category lib=aips module="ModuleName">
// <summary> Create an empty MeasurementSet from observation and telescope descriptions. </summary>
// <reviewed reviewer="" date="" tests="">
//
// <prerequisite>
//# Classes you should understand before using this one.
// <list>
//   <item> MeasurementSet
// </list>
// </prerequisite>
//
// <etymology>
// MS is from MeasurementSet, Simulator refers to the generation of
// 'fake' data from a set of parameters for instrument and sources.
// </etymology>
//
// <synopsis> 
// This class creates a MeasurementSet from a set of parameters for instrument
// and sources. It does not simulate the data, only the coordinates of a 
// measurement. The application "simulator" uses this class to create a true
// simulated MS with perfect or corrupted data.
// </synopsis> 
//
// <motivation>
// To test calibration and imaging programs it is necessary to have flawless
// data and data with errors that are known exactly. This class generates
// empty MeasurementSets (only coordinates filled in) that can be filled 
// with predicted data.
// </motivation>
//
// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// <li>     The amount of information to be specified by the user
//          could be much larger. For the moment it has been restricted to
//          what is needed for testing the synthesis imaging code. Already
//          it is possible to create MeasurementSets that cannot be processed
//          yet.
// </todo>

class NewMSSimulator
{
public: 
  
  // Default constructor - use this, then invoke initAnt, initFeed, initSpWindow,
  // initConfig.
  NewMSSimulator();
  
  // Copy constructor - for completeness only
  NewMSSimulator(const NewMSSimulator & mss);
  
  //# Destructor
  ~NewMSSimulator();
  
//# Operators
  // Assignment
  NewMSSimulator & operator=(const NewMSSimulator &);
  
  // set the antenna and array data.
  // This is held in MSSimulator vectors, and is then written into
  // the appropriate MS Tables when writeMS() and its helper function
  // fillCoords() are called.   The same model is used for the Fields,
  // SpWindows, Feeds, and Times information below.
  void initAnt(const String& telname,
	       const Vector<Double>& x, 
	       const Vector<Double>& y, 
	       const Vector<Double>& z,
	       const Vector<Float>& dishDiameter,
	       const Vector<String>& mount,
	       const Vector<String>& name,
	       const String& coordsystem,
	       const MPosition& mRefLocation);

  // set the observed fields
  void initFields(const uInt nSources,
		  const Vector<String>& sourceName, 
		  const Vector<MDirection>& sourceDirection,
		  const Vector<Int>& intsPerPointing,
		  const Vector<Int>& mosPointingsX,
		  const Vector<Int>& mosPointingsY,
		  const Vector<Float>& mosSpacing);


    // set feeds
    void setFeed(uInt feedid, const String& pols, 
		 const MPosition *position=0,
		 const Vector<Double> *receptorAngle=0,
		 const Matrix<Double> *beamOffset=0, 
		 const Matrix<Complex> *polResponse=0,
		 Int antId=-1, Double time=0, Double interval=0,
		 Int spwId=-1);

  // set the spectral windows information
  void initSpWindows(const uInt nSpWindows,
		     const Vector<String>& spWindowName,
		     const Vector<Int>& nChan,
		     const Vector<Quantity>& startFreq,
		     const Vector<Quantity>& freqInc,
		     const Vector<Quantity>& freqRes,
		     const Vector<String>& stokesString);

  void setFractionBlockageLimit(const Double fraclimit) 
    { fractionBlockageLimit_p = fraclimit; }

  void setElevationLimit(const Quantity& ellimit) 
    { elevationLimit_p = ellimit; }

  void setAutoCorrelationWt(const Float autocorrwt) 
    { autoCorrelationWt_p = autocorrwt; }

  void setTimes(const MEpoch& mStartTime, 
		const Quantity& qDuration, 
		const Quantity& qIntegrationTime, 
		const Quantity& qGapTime);

    // set the given epoch measure, mStartTime, to the currently set
    // starting time in the reference frame used by this MS.  A 
    // reference to the modified mStartTime is returned.
    MEpoch& getStartTime(MEpoch& mStartTime) {
	mStartTime = mStartTime_p;
	return mStartTime;
    }

  // Write out a simulated MeasurementSet with the given name
  void writeMS(const String& msname); 


  
private:

//# Data Members
  Int nSources_p, nSpWindows_p, nAnt_p, nTimes_p;
//  Double Tref_p, Tstart_p, Tend_p, Tint_p, Tgap_p;
  Double Tdur_p, Tint_p, Tgap_p;
//  Quantity  qIntegrationTime_p, qGapTime_p, qStartTime_p, qStopTime_p;
//  MEpoch mRefTime_p;
  MEpoch mStartTime_p;
//  Bool useHourAngles_p;

    MSSimFeedBuf feeds_p;

  Vector<Double> mosSpacing_p, startFreq_p, freqInc_p, freqRes_p,
      arrayXYZ_p, antDiam_p;
  Vector<Int> nChan_p, nIntFld_p, nIntSpW_p, nCorr_p, antId_p;
  Matrix<Int> nMos_p,stokesTypes_p;
  Vector<String> srcName_p, antName_p, mountType_p, spWindowName_p;
  Matrix<Double> radec_p, antXYZ_p;
  MPosition refPosition_p;
  String  radecRefFrame_p, telescope_p;
  Double  fractionBlockageLimit_p;
  Quantity elevationLimit_p;

  Float autoCorrelationWt_p;

//# Constructors

//# Secure Member Functions
  
  void fillCoords(MeasurementSet & ms);

  void local2global(Vector<Double>& xReturned,
		    Vector<Double>& yReturned,
		    Vector<Double>& zReturned,
		    const MPosition& mRefLocation,
		    const Vector<Double>& xIn,
		    const Vector<Double>& yIn,
		    const Vector<Double>& zIn);

  void longlat2global(Vector<Double>& xReturned,
		      Vector<Double>& yReturned,
		      Vector<Double>& zReturned,
		      const MPosition& mRefLocation,
		      const Vector<Double>& xIn,
		      const Vector<Double>& yIn,
		      const Vector<Double>& zIn);

  // Returns the fractional blockage of one antenna by another
  // We will want to put this somewhere else eventually, but I don't yet know where!
  // Till then.
  // fraction1: fraction of antenna 1 that is blocked by 2
  // fraction2: fraction of antenna 2 that is blocked by 1
  // hint: at least one of the two will be 0.0
  void  blockage(Double &fraction1, Double &fraction2,
		 const Vector<Double>& uvw,             // uvw in same units as diam!
		 const Double diam1, const Double diam2);

//    void createSubtables(MeasurementSet &ms);
    void checkpols(const String& pols);
};

inline void NewMSSimulator::checkpols(const String& pols) {
    String allowed("XYRL");
    for(String::const_iterator c=pols.begin(); c != pols.end(); c++) {
	if (! allowed.index(*c))
	    throw AipsError(String("Unrecognized polarization type: ") + *c);
    }
}

#endif




