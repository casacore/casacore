//# VisSet.h: VisSet definitions
//# Copyright (C) 1996
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
//# Correspondence concerning AIPS++ should be adressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#
//# $Id$

#if !defined(TRIAL_VISSET_H)
#define TRIAL_VISSET_H

#include <aips/aips.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Arrays/Matrix.h>
#include <trial/MeasurementEquations/StokesVector.h>
#include <trial/MeasurementEquations/VisibilityIterator.h>
// <summary> 
// The interface to the MeasurementSet for synthesis processing
// </summary>

// <reviewed reviewer="" date="" tests="t" demos="">

// <prerequisite>
//   <li> Arrays module
//   <li> MeasurementSet
// </prerequisite>
//
// <etymology>
// VisSet is the Set of Visibilities
// </etymology>
//
// <synopsis> 
// The VisSet is a class that simplifies access to the visibility data
// for the synthesis processing code. It holds a reference to an original
// MeasurementSet with observed data, but also keeps two 'reference' copies
// with their own DATA column for the storage of model visibilities and
// corrected visibilities. All access to these MeasurementSets is done via
// three VisibilityIterators for observed, corrected and model data.
//
// The VisSet allows selection and sorting of the MeasurementSet to be applied.
// A number of columns can be specified to define the iteration order, a
// a time interval can be given to iterate in chunks of time and a channel
// selection can be made for each spectral window present in the data.
// </synopsis> 
//
// <example>
// <srcblock>
//    MeasurementSet ms("Example.MS",Table::Update);
//    cout << "Constructing VisSet"<<endl;
//    Block<Int> bi(2); 
//    bi[0]=MS::SPECTRAL_WINDOW_ID;
//    bi[1]=MS::TIME; 
//    Matrix<Int> chanSelection; // no channel selection
//    // iterate in 600s chunks within each SpectralWindow
//    Double interval=600.; 
//    VisSet vs(ms,bi,chanSelection,interval);
// </srcblock>
// </example>
//
// <motivation>
// The MeasurementSet interface is a bit cumbersome to use directly if lots
// of iteration through an observed MS and 2 reference MSs is needed.
// This class provides an easy interface. It also keeps the iterators around
// for reuse, thus avoiding repeated sorting of the data.
// </motivation>
//
// <todo asof="">
// </todo>

typedef uInt Antenna;
typedef Double Frequency;
typedef RigidVector<Float,3> Position;
typedef RigidVector<Double,3> Direction;

typedef Vector<CStokesVector> vvCoh;

typedef ROVisibilityIterator ROVisIter;
typedef VisibilityIterator VisIter;


class VisSet {
public:
  // default constructor, only useful to assign to later.
  VisSet() {}
  // Construct from a MeasurementSet, with iteration order specified in
  // columns (giving the MS enum for the column) 
  // Specify channel selection as a Matrix(3,nSpw) where for each
  // spectral window the three values are start,nChannel and
  // spectral window number. Spectral windows without an entry will have 
  // all channels selected.
  // Specify a time interval for iterating in 'chunks' of time.
  // The default time interval of 0 groups all times together.
  // This constructor creates two referencing MeasurementSets,
  // -model and -corrected, these have their own DATA column to store
  // model and corrected visibilities. If they already exist and have the
  // same channel selection applied, they are reused.
  VisSet(const MeasurementSet & ms, const Block<Int>& columns, 
	 const Matrix<Int>& chanSelection, Double timeInterval=0);
  // Construct from an existing VisSet, this references the underlying
  // MeasurementSet(s) but allows a new iteration order and time interval
  // to be specified.
  VisSet(const VisSet & vs, const Block<Int>& columns, Double timeInterval=0);
  // Destructor, flushes the data to disk
  ~VisSet();
  // referencing assignment operator
  VisSet& operator=(const VisSet& other);
  // Flushes the data to disk
  void flush();
  // Iterator access to the observed data
  VisIter& observedCoherence();
  // Iterator access to the
  // observed coherence corrected for non-position dependent effects
  VisIter& correctedCoherence();
  // Iterator access to the 
  // predicted coherence, without applying the non-position dependent
  // effects (G, D, C, ..). I.e., the model for the calibratedCoherence(). 
  VisIter& modelCoherence(); 
  // Reset the channel selection. Only subsets of the original selection
  // (set in constructor) can be specified.
  // Note: this calls origin on the iterators.
  void selectChannel(Int nGroup,Int start, Int width, Int increment, 
		     Int spectralWindow);
  // number of antennas
  Int numberAnt() const;
  // number of spectral windows
  Int numberSpw() const;
  // number of channels in each spectral window
  Vector<Int> numberChan() const;
  // start channel of VisSet selection in each spectral window
  Vector<Int> startChan() const;
  // number of coherences
  Int numberCoh() const;
private:
  // make a reference copy of the MS table, with the exception of
  // the DATA column and the 'extraWritableColumns'. 
  // We want a new writable data column with the same tile index column 
  // (if any).
  // Zero fills the new data column if fill=="zero", copies the data from
  // the input (applying selection) if fill=="copy".
  Table referenceCopy(const Table& tab, const String& extension, 
		      const Vector<String>& extraWritableColumns,
		      const String& fill);
  
  MeasurementSet obsCoh_p, corrCoh_p, modelCoh_p;
  VisIter obsIter_p, corrIter_p, modelIter_p;
  Matrix<Int> selection_p;
};

#endif

