//# ComponentUpdate.h: This class updates components in UV plane
//# Copyright (C) 2000
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#if !defined(TRIAL_COMPONENTUPDATE_H)
#define TRIAL_COMPONENTUPDATE_H

//# Includes

#include <aips/aips.h>
#include <aips/Fitting/FitLSQ.h>
#include <aips/Containers/Block.h>
#include <trial/ComponentModels/ComponentList.h>
#include <aips/Mathematics/Complexfwd.h>

//# Forward declarations
template <class T> class Array;
template <class T> class Vector;
template <class T> class Matrix;

// <summary> This class updates components in UV plane </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto module=ComponentModels>ComponentModels</linkto> module
// </prerequisite>

// <etymology>
// Component and Update
// </etymology>

// <synopsis>
// This class is an update machine for model components. The updating is
// done using UV-plane information (and, of course, already available 
// calibration information). In principle all the elements of a
// component can be updated (like polarisation flux, position, etc),
// but certain combinations are more useful (and orthogonal enough) to
// be of practical use.
//
// The update process works by comparing the theoretical UV-plane
// expectations of a model component, with the actual UV-plane data,
// and solve, in the least-squares sense) for the component data
// elements. In practice the full nodel sky, as known, is subtracted
// from the observed data, after which the solution for for differnces
// is done. This solution can proceed in a variety of ways:
// <ul>
// <li> for all model components to be solved for in one solution. In
// this case the dependences between the different source components
// are taken properly into account, but the number of simultaneously
// to be solved unknowns can be very large.
// <li> solve for all model components separately, as if they were the
// only one producing UV-plane output. In practice this would cause
// dependencies between sources (e.g. if two components have (almost)
// the same position, they will both be adjusted e.g, in flux as if
// they were the only one, resulting in a twice too high
// adjustment. By using an (estimate of the) synthesised beam, this
// can be adjusted
// <li> a combination of the above two. If components are close
// together, they will be combined as one source to be solved for,
// otherwise they will be treated separately.
// </ul>
// Non-component sky models (like images) could be adjusted for their
// overall parameters (integrated flux, position) as well. This could
// be added at a later stage.
//
// Not all of the different possibilities and adjustable parameters
// will be implemented initially.
// </synopsis>


// <motivation>
// To provide an accurate way to adjust model component parameters
// from the observed data, using all the available data. 
// </motivation>

// <todo asof="2000/07/13">
//  <li>
// </todo>

class ComponentUpdate {
  // The different solution types
  enum Type {
    // Sove each source individually
    SEPARATE=0,
    // Slove in one solution
    COMBINED,
    // Solve in clusters of sources
    CLUSTER,
    // Number of options
    N_Type
  };
  // The different solvable parameters
  enum Solve {
    // Solve for I and l,m
    ILM=0,
    // Number of solvable parameters
    N_Solve,
  };
    
public:
  //# Standard constructors/destructors
  // Default constructor --  update not possible without one or more
  // set() methods.
  ComponentUpdate();
  // Construct an update machine for the sources in the given
  // ComponentList, using the 'separate' type of solution, and ILM
  // solve. Note that the component list must be a writable one to
  // receive the updates.
  explicit ComponentUpdate(ComponentList &model);
  // Construct an update machine for the sources in the given
  // ComponentList, using the specified sovables and the separate type.
  ComponentUpdate(ComponentList &model,
		  const ComponentUpdate::Solve solve);
  // Construct an update machine for the sources in the given
  // ComponentList, using the specified type of solution and solvables
  ComponentUpdate(ComponentList &model,
		  const ComponentUpdate::Solve solve,
		  const ComponentUpdate::Type tp);
  // Destructor
  ~ComponentUpdate();
  // Create the solution equations. For each series of UV points,
  // provide the following information to the engine (we can speed it
  // up later by having raw rather than Array type interface). Needed:
  // <ul> <li> data: a vector(=data) of length equal to the number of
  // UV points provided in one go (nuv) representing the observed
  // residual data
  // <li> A Cube with dimensions (3, nsource, nuv) (the 3 for the ILM
  // case, but different for other solutions). nsource is the number
  // of sources, and the 3 are the coefficients of the derivatives (in
  // the default case the values provided should be X, 2pi.U.i.X and
  // 2pi.V.i.X (i == sqrt(-1); X == the calculated UV plane value for
  // the source model component). In this we solve dI/I, dl and dm. 
  // Order of indices is determined by VectorIterator...
  // </ul>
  // Extra arguments possible: data weight.<br>
  // Extra calls: Just provide list of data (and weight) and UVW
  // coordinates. Calls to some Skymodel.visibility() and/or
  // .derivative() would then suffice. <br>
  // Improvements, by providing telescope rather than baseline based
  // data; proper care taken analytically of integration over time and
  // frequency.
  // <group>
  void makeEquations(const Array<DComplex> &deriv,
		     const Vector<DComplex> &data);
  // </group>
  // Provide the solution (i.e. dI/I and dl, dm) with errors (standard
  // deviation of solution). The Matrices filled (and resized) are of
  // sizes 3,nsource.
  Bool solve(Matrix<Double> &sol, Matrix<Double> &err);
private:
  //# Data
  // List of number of unknowns for solvables
  static const Int N_unknown[N_Solve];
  // Type of solution list
  static const LSQ::normType solveType[N_Solve];
  // Type of solution
  ComponentUpdate::Type soltp_p;
  // Solvable parameters
  ComponentUpdate::Solve solve_p;
  // Number of sources to fit
  Int nmodel_p;
  // Work area for expression data
  Double *dt_p;
  // Component list to solve (note: a pointer to it)
  ComponentList complist_p;
  // List of solution fitting areas
  PtrBlock<FitLSQ *> fit_p;
  //# Standard constructors/destructors
  // Copy constructor (not implemented)
  ComponentUpdate(const ComponentUpdate &other);
  // Assignment (not implemented)
  ComponentUpdate &operator=(const ComponentUpdate &other);
  //# Member functions
  // Initialise the list of fitting areas
  void init();
  // Empty the list of fitting areas
  void clean();
};

#endif
