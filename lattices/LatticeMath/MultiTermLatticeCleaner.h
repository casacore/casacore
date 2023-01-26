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

#ifndef LATTICES_MULTITERMLATTICECLEANER_H
#define LATTICES_MULTITERMLATTICECLEANER_H

#include <casacore/casa/aips.h>
#include <casacore/lattices/LatticeMath/LatticeCleaner.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/lattices/LEL/LatticeExprNode.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

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
  bool setntaylorterms(const int & nterms);
  
  // Input : scales
  bool setscales(const Vector<float> & scales);

  // Initialize all the memory being used.
  bool initialise(int32_t nx,int32_t ny);

  // Set control parameters.
  bool setcontrol(CleanEnums::CleanType cleanType,const int32_t niter,const float gain,const Quantity& aThreshold,const bool choose);
  //# This function is defined in the base class LatticeCleaner, but was not
  //# defined in the new MultiTermLatticeCleaner.
  //# I (GvD) have added it for the time being.
  bool setcontrol(CleanEnums::CleanType cleanType, const int32_t niter,
		  const float gain, const Quantity& aThreshold,
		  const Quantity& /*fThreshold*/,
		  const bool choose=true)
    { return setcontrol (cleanType, niter, gain, aThreshold, choose); }

  // Input : psfs and dirty images
  bool setpsf(int order, Lattice<T> & psf);
  
  // Input : psfs and dirty images
  bool setresidual(int order, Lattice<T> & dirty);
 
  // Input : model images
  bool setmodel(int order, Lattice<T> & model);
 
  // Input : mask
  bool setmask(Lattice<T> & mask);
 
  // Run the minor cycle
  int32_t mtclean(LatticeCleanProgress* progress=0);

  // Output : Model images
  bool getmodel(int order, Lattice<T> & model);
  
  // Ouput : psfs and dirty images
  bool getresidual(int order, Lattice<T> & residual);
 
  // Output : Hessian matrix
  bool getinvhessian(Matrix<double> & invhessian);

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

  int32_t ntaylor_p; // Number of terms in the Taylor expansion to use.
  int32_t psfntaylor_p; // Number of terms in the Taylor expansion for PSF.
  int32_t nscales_p; // Number of scales to use for the multiscale part.
  int32_t nx_p;
  int32_t ny_p;
  int32_t totalIters_p;
  
  // Image mask
  TempLattice<float>* dirty_p;
  TempLattice<Complex>* dirtyFT_p;
  TempLattice<float>* mask_p;
  TempLattice<float>* fftmask_p;
  
  Vector<float> scaleSizes_p; // Vector of scale sizes in pixels.
  Vector<float> scaleBias_p; // Vector of scale biases !!
  Vector<float> totalScaleFlux_p; // Vector of total scale fluxes.
  Vector<float> totalTaylorFlux_p; // Vector of total flux in each taylor term.
  float weightScaleFactor_p;
  float maxPsf_p;

  IPosition gip,imshape;
  int32_t nx,ny,npol_p,nchan;
  bool donePSF_p,donePSP_p,doneCONV_p;
 
  // h(s) [nx,ny,nscales]
  PtrBlock<TempLattice<float>* > vecScales_p; 
  PtrBlock<TempLattice<Complex>* > vecScalesFT_p; 
  
  // B_k  [nx,ny,ntaylor]
  PtrBlock<TempLattice<float>* > vecPsf_p; 
  PtrBlock<TempLattice<Complex>* > vecPsfFT_p; 
  
  // I_D : Residual/Dirty Images [nx,ny,ntaylor]
  PtrBlock<TempLattice<float>* > vecDirty_p; 
 
  // I_M : Model Images [nx,ny,ntaylor]
  PtrBlock<TempLattice<float>* > vecModel_p; 
 
  // A_{smn} = B_{sm} * B{sn} [nx,ny,ntaylor,ntaylor,nscales,nscales]
  // A_{s1s2mn} = B_{s1m} * B{s2n} [nx,ny,ntaylor,ntaylor,nscales,nscales]
  PtrBlock<TempLattice<float>* > cubeA_p; 
  PtrBlock<LatticeIterator<float>* > itercubeA_p;
  
  // R_{sk} = I_D * B_{sk} [nx,ny,ntaylor,nscales]
  PtrBlock<TempLattice<float>* > matR_p; 
  PtrBlock<LatticeIterator<float>* > itermatR_p;
  
  // a_{sk} = Solution vectors. [nx,ny,ntaylor,nscales]
  PtrBlock<TempLattice<float>* > matCoeffs_p; 
  PtrBlock<LatticeIterator<float>* > itermatCoeffs_p;

  // Memory to be allocated per TempLattice
  double memoryMB_p;
  
  // Solve [A][Coeffs] = [I_D * B]
  // Shape of A : [ntaylor,ntaylor]
  PtrBlock<Matrix<double>*> matA_p;    // 2D matrix to be inverted.
  PtrBlock<Matrix<double>*> invMatA_p; // Inverse of matA_p;

  // Scratch Lattices and iterators.
  TempLattice<Complex>* cWork_p;
  TempLattice<float>* tWork_p;
  LatticeIterator<float>* itertWork_p;
  
  LatticeExprNode len_p;

  float lambda_p;
  
  int32_t numberOfTempLattices(int32_t nscales,int32_t ntaylor);
  int32_t manageMemory(bool allocate);
  
  bool findMaxAbsLattice(const TempLattice<float>& masklat,const Lattice<float>& lattice,float& maxAbs,IPosition& posMaxAbs, bool flip=false);
  int32_t addTo(Lattice<float>& to, const Lattice<float>& add, float multiplier);

  int32_t setupFFTMask();
  int32_t setupUserMask();
  int32_t setupBlobs();
  int32_t computeFluxLimit(float &fluxlimit, float threshold);
  int32_t computeMatrixA();
  int32_t computeRHS();
  int32_t solveMatrixEqn(int32_t scale);
  int32_t computePenaltyFunction(int32_t scale, float &loopgain, bool choosespec);
  int32_t updateSolution(IPosition globalmaxpos, int32_t maxscaleindex, float loopgain);
  int32_t checkConvergence(bool choosespec, float thresh, float fluxlimit); 
  
  int32_t IND2(int32_t taylor,int32_t scale);
  int32_t IND4(int32_t taylor1, int32_t taylor2, int32_t scale1, int32_t scale2);
  
  bool adbg;
};

} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LatticeMath/MultiTermLatticeCleaner.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif

