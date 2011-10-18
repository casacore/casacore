//# MultiTermLatticeCleaner.h: Minor Cycle for MSMFS deconvolution
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
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
//# Urvashi Rau <rurvashi@aoc.nrao.edu>
//# $Id$

#ifndef LATTICES_MULTITERMLATTICECLEANER_H
#define LATTICES_MULTITERMLATTICECLEANER_H

#include <lattices/Lattices/LatticeCleaner.h>
#include <lattices/Lattices/LatticeIterator.h>
#include <lattices/Lattices/LatticeExpr.h>
#include <lattices/Lattices/LatticeExprNode.h>

namespace casa { //# NAMESPACE CASA - BEGIN

template<class T> class MultiTermLatticeCleaner : public LatticeCleaner<T>
{
public:
  // Create a cleaner for a specific dirty image and PSF
  MultiTermLatticeCleaner();

  // The copy constructor uses reference semantics
  MultiTermLatticeCleaner(const MultiTermLatticeCleaner<T> & other);

  // The assignment operator also uses reference semantics
  MultiTermLatticeCleaner<T> & operator=(const MultiTermLatticeCleaner<T> & other); 

  // The destructor does nothing special.
  ~MultiTermLatticeCleaner();

  // Input : number of Taylor terms
  //         Reshapes PtrBlocks to hold the correct number of PSFs and Residual images
  Bool setntaylorterms(const int & nterms);
  
  // Input : scales
  Bool setscales(const Vector<Float> & scales);

  // Initialize all the memory being used.
  Bool initialise(Int nx,Int ny);

  // Set control parameters.
  Bool setcontrol(CleanEnums::CleanType cleanType,const Int niter,const Float gain,const Quantity& aThreshold,const Bool choose);

  // Input : psfs and dirty images
  Bool setpsf(int order, Lattice<T> & psf);
  
  // Input : psfs and dirty images
  Bool setresidual(int order, Lattice<T> & dirty);
 
  // Input : model images
  Bool setmodel(int order, Lattice<T> & model);
 
  // Input : mask
  Bool setmask(Lattice<T> & mask);
 
  // Run the minor cycle
  Int mtclean(LatticeCleanProgress* progress=0);

  // Output : Model images
  Bool getmodel(int order, Lattice<T> & model);
  
  // Ouput : psfs and dirty images
  Bool getresidual(int order, Lattice<T> & residual);
 
  // Output : Hessian matrix
  Bool getinvhessian(Matrix<Double> & invhessian);

private:
  LogIO os;

  using LatticeCleaner<T>::itsCleanType;
  using LatticeCleaner<T>::itsMaxNiter;
  using LatticeCleaner<T>::itsGain;
  using LatticeCleaner<T>::itsThreshold;
  using LatticeCleaner<T>::itsMask;
  using LatticeCleaner<T>::itsPositionPeakPsf;

  using LatticeCleaner<T>::findMaxAbsLattice;
  using LatticeCleaner<T>::findMaxAbsMaskLattice;
  using LatticeCleaner<T>::makeScale;
  using LatticeCleaner<T>::addTo;
  using LatticeCleaner<T>::makeBoxesSameSize;
  using LatticeCleaner<T>::validatePsf;

  Int ntaylor_p; // Number of terms in the Taylor expansion to use.
  Int psfntaylor_p; // Number of terms in the Taylor expansion for PSF.
  Int nscales_p; // Number of scales to use for the multiscale part.
  Int nx_p;
  Int ny_p;
  Int totalIters_p;
  
  // Image mask
  TempLattice<Float>* dirty_p;
  TempLattice<Complex>* dirtyFT_p;
  TempLattice<Float>* mask_p;
  TempLattice<Float>* fftmask_p;
  
  Vector<Float> scaleSizes_p; // Vector of scale sizes in pixels.
  Vector<Float> scaleBias_p; // Vector of scale biases !!
  Vector<Float> totalScaleFlux_p; // Vector of total scale fluxes.
  Vector<Float> totalTaylorFlux_p; // Vector of total flux in each taylor term.
  Float weightScaleFactor_p;
  Float maxPsf_p;

  IPosition gip,imshape;
  Int nx,ny,npol_p,nchan;
  Bool donePSF_p,donePSP_p,doneCONV_p;
 
  // h(s) [nx,ny,nscales]
  PtrBlock<TempLattice<Float>* > vecScales_p; 
  PtrBlock<TempLattice<Complex>* > vecScalesFT_p; 
  
  // B_k  [nx,ny,ntaylor]
  PtrBlock<TempLattice<Float>* > vecPsf_p; 
  PtrBlock<TempLattice<Complex>* > vecPsfFT_p; 
  
  // I_D : Residual/Dirty Images [nx,ny,ntaylor]
  PtrBlock<TempLattice<Float>* > vecDirty_p; 
 
  // I_M : Model Images [nx,ny,ntaylor]
  PtrBlock<TempLattice<Float>* > vecModel_p; 
 
  // A_{smn} = B_{sm} * B{sn} [nx,ny,ntaylor,ntaylor,nscales,nscales]
  // A_{s1s2mn} = B_{s1m} * B{s2n} [nx,ny,ntaylor,ntaylor,nscales,nscales]
  PtrBlock<TempLattice<Float>* > cubeA_p; 
  PtrBlock<LatticeIterator<Float>* > itercubeA_p;
  
  // R_{sk} = I_D * B_{sk} [nx,ny,ntaylor,nscales]
  PtrBlock<TempLattice<Float>* > matR_p; 
  PtrBlock<LatticeIterator<Float>* > itermatR_p;
  
  // a_{sk} = Solution vectors. [nx,ny,ntaylor,nscales]
  PtrBlock<TempLattice<Float>* > matCoeffs_p; 
  PtrBlock<LatticeIterator<Float>* > itermatCoeffs_p;

  // Memory to be allocated per TempLattice
  Double memoryMB_p;
  
  // Solve [A][Coeffs] = [I_D * B]
  // Shape of A : [ntaylor,ntaylor]
  PtrBlock<Matrix<Double>*> matA_p;    // 2D matrix to be inverted.
  PtrBlock<Matrix<Double>*> invMatA_p; // Inverse of matA_p;

  // Scratch Lattices and iterators.
  TempLattice<Complex>* cWork_p;
  TempLattice<Float>* tWork_p;
  LatticeIterator<Float>* itertWork_p;
  
  LatticeExprNode len_p;

  Float lambda_p;
  
  Int numberOfTempLattices(Int nscales,Int ntaylor);
  Int manageMemory(Bool allocate);
  
  Bool findMaxAbsLattice(const TempLattice<Float>& masklat,const Lattice<Float>& lattice,Float& maxAbs,IPosition& posMaxAbs, Bool flip=False);
  Int addTo(Lattice<Float>& to, const Lattice<Float>& add, Float multiplier);

  Int setupFFTMask();
  Int setupUserMask();
  Int setupBlobs();
  Int computeFluxLimit(Float &fluxlimit, Float threshold);
  Int computeMatrixA();
  Int computeRHS();
  Int solveMatrixEqn(Int scale);
  Int computePenaltyFunction(Int scale, Float &loopgain, Bool choosespec);
  Int updateSolution(IPosition globalmaxpos, Int maxscaleindex, Float loopgain);
  Int checkConvergence(Bool choosespec, Float thresh, Float fluxlimit); 
  
  Int IND2(Int taylor,Int scale);
  Int IND4(Int taylor1, Int taylor2, Int scale1, Int scale2);
  
  Bool adbg;
};

} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <lattices/Lattices/MultiTermLatticeCleaner.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif

