//# Copyright (C) 1997,1998,1999,2000,2001,2002,2003
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
//# $Id: MultiTermLatticeCleaner.cc 19909 2008-04-23 02:08:02Z UrvashiRau $


#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/ArrayMath.h>

#include <casa/Logging/LogIO.h>
#include <casa/OS/File.h>
#include <casa/Containers/Record.h>

#include <lattices/Lattices/LatticeCleaner.h>
#include <lattices/Lattices/MultiTermLatticeCleaner.h>
#include <lattices/Lattices/LatticeCleanProgress.h>
#include <lattices/Lattices/TiledLineStepper.h> 
#include <lattices/Lattices/LatticeStepper.h> 
#include <lattices/Lattices/LatticeNavigator.h> 
#include <lattices/Lattices/LatticeIterator.h>
#include <lattices/Lattices/TempLattice.h>
#include <lattices/Lattices/LatticeFFT.h>
#include <lattices/Lattices/LatticeExpr.h>
#include <lattices/Lattices/SubLattice.h>
#include <lattices/Lattices/LCBox.h>
#include <casa/Arrays/Slicer.h>
#include <lattices/Lattices/LatticeExpr.h>
#include <lattices/Lattices/LatticeExprNode.h>

#include <casa/OS/HostInfo.h>
#include <casa/System/PGPlotter.h>
#include <casa/Arrays/ArrayError.h>
#include <casa/Arrays/ArrayIter.h>
#include <casa/Arrays/VectorIter.h>

#include <casa/Utilities/GenSort.h>
#include <casa/BasicSL/String.h>
#include <casa/Utilities/Assert.h>
#include <casa/Utilities/Fallible.h>

#include <casa/BasicSL/Constants.h>

#include <casa/Logging/LogSink.h>
#include <casa/Logging/LogMessage.h>

#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/Matrix.h>
#include <scimath/Mathematics/MatrixMathLA.h>

namespace casa { //# NAMESPACE CASA - BEGIN

#define MIN(a,b) ((a)<=(b) ? (a) : (b))
#define MAX(a,b) ((a)>=(b) ? (a) : (b))
	
template<class T> MultiTermLatticeCleaner<T>::MultiTermLatticeCleaner():
  ntaylor_p(2),donePSF_p(False),donePSP_p(False),doneCONV_p(False)
  {
	  adbg=True;
  }

template <class T> MultiTermLatticeCleaner<T>::
    MultiTermLatticeCleaner(const MultiTermLatticeCleaner<T> & other):
   ntaylor_p(other.ntaylor_p)  //And others... minus some...
    {
    }

template<class T> MultiTermLatticeCleaner<T> & MultiTermLatticeCleaner<T>::
operator=(const MultiTermLatticeCleaner<T> & other) {
  if (this != &other) {
    ntaylor_p = other.ntaylor_p; // and others..... minus some
  }
  return *this;
}

template<class T> MultiTermLatticeCleaner<T>::
~MultiTermLatticeCleaner()
{
  manageMemory(False);
}

template <class T>
Bool MultiTermLatticeCleaner<T>::setscales(const Vector<Float> & scales)
{
   nscales_p = scales.nelements();
   scaleSizes_p.resize();
   scaleSizes_p = scales;
   totalScaleFlux_p.resize(nscales_p);
   totalScaleFlux_p.set(0.0);
   return True;
}

template <class T>
Bool MultiTermLatticeCleaner<T>::setntaylorterms(const int & nterms)
{
   ntaylor_p = nterms;
   psfntaylor_p = 2*nterms-1;
   totalTaylorFlux_p.resize(ntaylor_p);
   totalTaylorFlux_p.set(0.0);
   return True;
}

// Allocate memory, based on nscales and ntaylor
template <class T>
Bool MultiTermLatticeCleaner<T>::initialise(Int nx, Int ny)
{
  LogIO os(LogOrigin("MultiTermLatticeCleaner", "initialise()", WHERE));
  
  /* Verify Image Shapes */
  //  nx_p = model.shape(0);
  nx_p = nx;
  ny_p = ny;
  
  if(adbg) os << "Checking shapes" << LogIO::POST;
  
  /* Verify nscales_p and ntaylor_p */
  AlwaysAssert(nscales_p>0, AipsError);
  AlwaysAssert(ntaylor_p>0, AipsError);
 
  
  if(adbg) os << "Start allocating mem" << LogIO::POST;
  /* Allocate memory for many many TempLattices. */
  manageMemory(True);

  /* Set up the default Mask image */
  setupFFTMask();
   
  /* Create the scaled blobs and their FTs */
  setupBlobs();
  
  if(adbg) os << "Finished initializing MultiTermLatticeCleaner" << LogIO::POST;
  return True;
}

/* Input : PSFs */
template <class T>
Bool MultiTermLatticeCleaner<T>::setpsf(int order, Lattice<T> & psf)
{
	AlwaysAssert((order>=(int)0 && order<(int)vecPsf_p.nelements()), AipsError);
	if(order==0) AlwaysAssert(validatePsf(psf), AipsError);
	//AlwaysAssert(psf, AipsError);
	vecPsf_p[order]->copyData(LatticeExpr<Float>(psf));
	vecPsfFT_p[order]->copyData(LatticeExpr<Complex>(toComplex((*fftmask_p)*(*vecPsf_p[order]))));
	LatticeFFT::cfft2d(*vecPsfFT_p[order], True);

  return True;
}

/* Input : Dirty Images */
template <class T>
Bool MultiTermLatticeCleaner<T>::setresidual(int order, Lattice<T> & dirty)
{
	AlwaysAssert((order>=(int)0 && order<(int)vecDirty_p.nelements()), AipsError);
	//AlwaysAssert(dirty, AipsError);
	vecDirty_p[order]->copyData(LatticeExpr<Float>(dirty));
  return True;
}

/* Input : Model Component Image */
template <class T>
Bool MultiTermLatticeCleaner<T>::setmodel(int order, Lattice<T> & model)
{
	AlwaysAssert((order>=(int)0 && order<(int)vecModel_p.nelements()), AipsError);
	//AlwaysAssert(model, AipsError);
	vecModel_p[order]->copyData(LatticeExpr<Float>(model));
  return True;
}

/* Input : Mask */
template <class T>
Bool MultiTermLatticeCleaner<T>::setmask(Lattice<T> & mask)
{
	//AlwaysAssert(mask, AipsError);
	if(!itsMask) itsMask = new TempLattice<T>(mask.shape(), memoryMB_p);
	itsMask->copyData(LatticeExpr<Float>(mask));
  return True;
}

/* Output : Model Component Image */
template <class T>
Bool MultiTermLatticeCleaner<T>::getmodel(int order, Lattice<T> & model)
{
	AlwaysAssert((order>=(int)0 && order<(int)vecModel_p.nelements()), AipsError);
	//AlwaysAssert(model, AipsError);
	model.copyData(LatticeExpr<Float>(*vecModel_p[order]));
  return True;
}

/* Output Residual Image */
template <class T>
Bool MultiTermLatticeCleaner<T>::getresidual(int order, Lattice<T> & residual)
{
	AlwaysAssert((order>=(int)0 && order<(int)vecDirty_p.nelements()), AipsError);
	//AlwaysAssert(residual, AipsError);
	residual.copyData(LatticeExpr<Float>(*vecDirty_p[order]));
  return True;
}

/* Do the deconvolution */
template <class T>
Int MultiTermLatticeCleaner<T>::mtclean(LatticeCleanProgress* progress)
{
  LogIO os(LogOrigin("MultiTermLatticeCleaner", "mtclean()", WHERE));
  if(adbg)os << "SOLVER for Multi-Frequency Synthesis deconvolution" << LogIO::POST;
  
  Int convergedflag = 0;
  static Bool choosespec = True;
  static Int totalIters=0;
  
  /* Set up the Mask image */
  setupUserMask();
   
  /* Compute the current peak residual */
  Float zmaxval=0.0;
  IPosition zmaxpos;
  findMaxAbsLattice((*mask_p),(*vecDirty_p[0]),zmaxval,zmaxpos);
  os << "Initial Max Residual at iteration " << totalIters << " : " << zmaxval << "  at " << zmaxpos << LogIO::POST;
  if(totalIters==0)
  {
    for(Int i=0;i<2*ntaylor_p-1;i++)
    {
      findMaxAbsLattice((*mask_p),(*vecPsf_p[i]),zmaxval,zmaxpos);
      os << "Psf " << i << " : " << zmaxval << "  at " << zmaxpos << LogIO::POST;
    }
  }
 
  /* Compute all convolutions and the matrix A */
  computeMatrixA();
  
  /* Compute the convolutions of the current residual with all PSFs and scales */
  computeRHS();
  
  /* Compute the flux limits that determine the depth of the minor cycles. */
  Float fluxlimit =0.0;
  Float loopgain = itsGain;
  Float thresh = itsThreshold.getValue("Jy");
  computeFluxLimit(fluxlimit,thresh);
  
  /* Initialize persistent variables */
  gip = IPosition(4,nx_p,ny_p,1,1);  
  Float maxval,globalmaxval=-1e+10;
  IPosition maxpos(4,0),globalmaxpos(4,0);
  Int maxscaleindex=0;
  Int niters = itsMaxNiter;
  
  /********************** START MINOR CYCLE ITERATIONS ***********************/
  Int numiters = MIN(20,niters-totalIters);
  for(Int itercount=0;itercount<numiters;itercount++)
  {
    globalmaxval=-1e+10;
    
    /* Find the best component over all scales */
    for(Int scale=0;scale<nscales_p;scale++)
    {
       /* Solve the matrix eqn for all points in the lattice */
       solveMatrixEqn(scale);
       
       /* Now use the solved-for sets of coefficients and compute a penalty function */
       computePenaltyFunction(scale,loopgain,choosespec);
       
       /* Find the peak of the penalty function to choose the update direction */
       // Find the location and TAYLOR term of Max Eps sq  from twork.
       findMaxAbsLattice((*mask_p),*tWork_p,maxval,maxpos);
       
       /* Record the maximum penalty-function value and chosen scale */
       //if(maxval > globalmaxval)
       if((maxval*scaleBias_p[scale]) > globalmaxval)
       {
	       globalmaxval = maxval;
	       globalmaxpos = maxpos;
	       maxscaleindex = scale;
       }
       
    }// end of for scale
    
    /* Update the current solution by this chosen step */
    updateSolution(globalmaxpos,maxscaleindex,loopgain);
    
    /* Compute peak residuals */
    Float maxres=0.0;
    IPosition maxrespos;
    findMaxAbsLattice((*mask_p),(*matR_p[IND2(0,0)]),maxres,maxrespos);
    Float norma = (1.0/(*matA_p[0])(0,0));
    Float rmaxval = maxres*norma;
    
    /* Print out coefficients at each iteration */
    if(adbg)
    {
      //os << "[" << totalIters << "] Res: " << rmaxval << " Max: " << globalmaxval;
      os << "[" << totalIters << "] Res: " << rmaxval;
      os << " Pos: " <<  globalmaxpos << " Scale: " << scaleSizes_p[maxscaleindex];
      os << " Coeffs: ";
      for(Int taylor=0;taylor<ntaylor_p;taylor++)
	      os << (*matCoeffs_p[IND2(taylor,maxscaleindex)]).getAt(globalmaxpos) << "  ";
      os << LogIO::POST;
    }
    
    /* Increment iteration count */
    totalIters++;
    
    /* Check for convergence */
    convergedflag = checkConvergence(choosespec,thresh,fluxlimit);
    if(convergedflag == 2)
    {
      os << "Reached Stopping Threshold" << LogIO::POST;
      break;
    }
    if(convergedflag == 1)
    {
      os << "Reached Flux Limit for this Major cycle" << LogIO::POST;
      convergedflag = 0;
      break;
    }
    if(totalIters==itsMaxNiter) 
    {
      os << "Failed to reach stopping threshold" << LogIO::POST;
      convergedflag=-1;
      break;
    }
    
  }
  /********************** END MINOR CYCLE ITERATIONS ***********************/		
  
  /* Print out flux counts so far */
  if(adbg)
  {
     for(Int scale=0;scale<nscales_p;scale++) os << "Scale " << scale+1 << " with " << scaleSizes_p[scale] << " pixels has total flux = " << totalScaleFlux_p[scale] << LogIO::POST;
     for(Int taylor=0;taylor<ntaylor_p;taylor++) os << "Taylor " << taylor << " has total flux = " << totalTaylorFlux_p[taylor] << LogIO::POST;
  }
  
  return(convergedflag);
}

/* Indexing Wonders... */
template <class T>
Int MultiTermLatticeCleaner<T>::IND2(Int taylor, Int scale)
{
	return  taylor * nscales_p + scale;
}
template <class T>
Int MultiTermLatticeCleaner<T>::IND4(Int taylor1, Int taylor2, Int scale1, Int scale2)
{
	Int tt1=taylor1;
	Int tt2=taylor2;
	Int ts1=scale1;
	Int ts2=scale2;
	scale1 = MAX(ts1,ts2);
	scale2 = MIN(ts1,ts2);
	taylor1 = MAX(tt1,tt2);
	taylor2 = MIN(tt1,tt2);
	Int totscale = nscales_p*(nscales_p+1)/2;
	return ((taylor1*(taylor1+1)/2)+taylor2)*totscale + ((scale1*(scale1+1)/2)+scale2);
}

/*************************************
 *          Number of TempLattices 
 *************************************/
template <class T>
Int MultiTermLatticeCleaner<T>::numberOfTempLattices(Int nscales, Int ntaylor)
{
  Int ntotal4d = (nscales*(nscales+1)/2) * (ntaylor*(ntaylor+1)/2);
  return ntotal4d + 6 + 2 + (2+1)*nscales + (1+1)*ntaylor + 2*nscales*ntaylor;
}

/*************************************
 *          Allocate Memory
 *************************************/
template <class T>
Int MultiTermLatticeCleaner<T>::manageMemory(Bool direction)
{
  LogIO os(LogOrigin("MultiTermLatticeCleaner", "manageMemory()", WHERE));
	if(direction)
	{
		// Define max memory usage for all TempLattices. (half of available);
		memoryMB_p = Double(HostInfo::memoryTotal()/1024)/(2.0); // ? /(16.0) ?
		Int ntemp = numberOfTempLattices(nscales_p,ntaylor_p);
		Int numMB = nx_p*ny_p*4*ntemp/(1024*1024);
		os << "This algorithm needs " << numMB << " MBytes for " << ntemp << " TempLattices " << LogIO::POST;
		memoryMB_p = MIN(memoryMB_p, numMB);
		os << "Allocating " << memoryMB_p << " MBytes." << LogIO::POST;
		
	}
	if(adbg && direction)os << "Allocating mem ... " ;
	if(adbg && !direction)os << "Freeing mem ... " ;
	
	Int ntotal4d = (nscales_p*(nscales_p+1)/2) * (ntaylor_p*(ntaylor_p+1)/2);
	
	//gip = IPosition(2,ntaylor_p,ntaylor_p);  
	IPosition tgip(2,ntaylor_p,ntaylor_p);
	
	// Small A matrix to be inverted for each point..
	matA_p.resize(nscales_p); invMatA_p.resize(nscales_p);
	for(Int i=0;i<nscales_p;i++)
	{
		if(direction)
		{ 
			matA_p[i] = new Matrix<Double>(tgip);
			invMatA_p[i] = new Matrix<Double>(tgip);
		}
		else
		{
			delete matA_p[i] ;
			delete invMatA_p[i] ;
		}
	}
	
	/// Make this read from model.shape() or image.shape()
	gip = IPosition(4,nx_p,ny_p,1,1);  
	
	// I_D and mask
	if(direction)
	{
		dirty_p = new TempLattice<Float>(gip, memoryMB_p);
		dirtyFT_p = new TempLattice<Complex>(gip, memoryMB_p);
		mask_p = new TempLattice<Float>(gip, memoryMB_p);
		fftmask_p = new TempLattice<Float>(gip, memoryMB_p);
		// Temporary work-holder
		cWork_p = new TempLattice<Complex>(gip,memoryMB_p);
		tWork_p = new TempLattice<Float>(gip,memoryMB_p);
	}
	else
	{
		delete dirty_p;
		delete dirtyFT_p;
		delete fftmask_p;
		delete mask_p;
		delete cWork_p;
		delete tWork_p;
	}

	// Mask
	if(direction) itsMask=0;
	else
	{
		if(itsMask){ delete itsMask; itsMask=0;}
	}
	
	// Scales
	vecScales_p.resize(nscales_p);
	vecScalesFT_p.resize(nscales_p);
	for(Int i=0;i<nscales_p;i++) 
	{
		if(direction)
		{
			vecScales_p[i] = new TempLattice<Float>(gip,memoryMB_p);
			vecScalesFT_p[i] = new TempLattice<Complex>(gip,memoryMB_p);
		}
		else
		{
			delete vecScales_p[i];
			delete vecScalesFT_p[i];
		}
	}
	
	// Psfs and Models
	vecPsf_p.resize(psfntaylor_p);
	vecPsfFT_p.resize(psfntaylor_p);
	for(Int i=0;i<psfntaylor_p;i++) 
	{
		if(direction)
		{
			vecPsf_p[i] = new TempLattice<Float>(gip,memoryMB_p);
			vecPsfFT_p[i] = new TempLattice<Complex>(gip,memoryMB_p);
		}
		else
		{
			delete vecPsf_p[i];
			delete vecPsfFT_p[i];
		}
	}
	
	
	// Dirty/Residual Images
	vecDirty_p.resize(ntaylor_p);
	vecModel_p.resize(ntaylor_p);
	for(Int i=0;i<ntaylor_p;i++) 
	{
		if(direction)
		{
			vecDirty_p[i] = new TempLattice<Float>(gip,memoryMB_p);
			vecModel_p[i] = new TempLattice<Float>(gip,memoryMB_p);
		}
		else
		{
			delete vecDirty_p[i];
			delete vecModel_p[i];
		}
	}
	
	// Psf * Scales
	//  matPsfConvScales_p.resize(ntaylor_p*nscales_p);
	//  for(Int i=0;i<nscales_p*ntaylor_p;i++) matPsfConvScales_p = new TempLattice<Float>(gip,memoryMB_p);
	
	// Set up the latticeiterators also
	IPosition shapeOut;
	IPosition cursorShape;
	
	if(direction)
	{
		AlwaysAssert (tWork_p->isWritable(), AipsError);
		shapeOut = IPosition(tWork_p->shape());
		cursorShape = IPosition(tWork_p->niceCursorShape());
	}
	else
	{
		shapeOut = gip;
		cursorShape = gip;
	}
	
	LatticeStepper stepper(shapeOut, cursorShape, LatticeStepper::RESIZE);
	
	if(direction)itertWork_p = new LatticeIterator<Float>((*tWork_p), stepper);
	else delete itertWork_p;
	
	// (Psf * Scales) * (Psf * Scales)
	cubeA_p.resize(ntotal4d);
	itercubeA_p.resize(ntotal4d);
	for(Int i=0;i<ntotal4d;i++) 
	{
		if(direction) 
		{
			cubeA_p[i] = new TempLattice<Float>(gip,memoryMB_p);
			itercubeA_p[i] = new LatticeIterator<Float>((*cubeA_p[i]),stepper);
		}
		else 
		{
			delete cubeA_p[i];
			delete itercubeA_p[i];
		}
	}
	
	// I_D * (Psf * Scales)
	matR_p.resize(ntaylor_p*nscales_p);
	itermatR_p.resize(ntaylor_p*nscales_p);
	// Coefficients to be solved for.
	matCoeffs_p.resize(ntaylor_p*nscales_p);
	itermatCoeffs_p.resize(ntaylor_p*nscales_p);
	
	for(Int i=0;i<ntaylor_p*nscales_p;i++) 
	{
		if(direction)
		{	
			matR_p[i] = new TempLattice<Float>(gip,memoryMB_p);
			itermatR_p[i] = new LatticeIterator<Float>((*matR_p[i]),stepper);
			matCoeffs_p[i] = new TempLattice<Float>(gip,memoryMB_p);
			itermatCoeffs_p[i] = new LatticeIterator<Float>((*matCoeffs_p[i]),stepper);
		}
		else
		{
			delete matR_p[i];
			delete itermatR_p[i];
			delete matCoeffs_p[i];
			delete itermatCoeffs_p[i];
		}
	}
  
	if(adbg) os << "done" << LogIO::POST;
	
	return 0;
}

/*************************************
 *    Add two subLattices..      -- same code as in copyData.   
 *************************************/
template <class T>
Int MultiTermLatticeCleaner<T>::addTo(Lattice<Float>& to, const Lattice<Float>& add, Float multiplier)
{
	// Check the lattice is writable.
	// Check the shape conformance.
	AlwaysAssert (to.isWritable(), AipsError);
	const IPosition shapeIn  = add.shape();
	const IPosition shapeOut = to.shape();
	AlwaysAssert (shapeIn.isEqual (shapeOut), AipsError);
	IPosition cursorShape = to.niceCursorShape();
	LatticeStepper stepper (shapeOut, cursorShape, LatticeStepper::RESIZE);
	LatticeIterator<Float> toIter(to, stepper);
	RO_LatticeIterator<Float> addIter(add, stepper);
	for (addIter.reset(), toIter.reset(); !addIter.atEnd();addIter++, toIter++) 
	{
		toIter.rwCursor()+=addIter.cursor()*multiplier;
	}
	return 0;
}

/***************************************
 *  Set up the Masks.
 ****************************************/
template <class T>
Int MultiTermLatticeCleaner<T>::setupFFTMask()
{
   /* Set up fftmask - inner quarter */
   (*fftmask_p).set(0.0);
   IPosition mblc(4,nx_p/4,ny_p/4,0,0);
   IPosition mtrc(4,3*nx_p/4,3*ny_p/4,0,0);
   IPosition minc(4, 1);
   LCBox::verify(mblc,mtrc,minc,(*fftmask_p).shape());
   LCBox regmask(mblc,mtrc,(*fftmask_p).shape());
   SubLattice<Float> smask((*fftmask_p),regmask,True);
   smask.set(1.0);
   
   return 0;
}/* end of setupFFTMask() */

template <class T>
Int MultiTermLatticeCleaner<T>::setupUserMask()
{
   /* Copy the input mask */
   if(itsMask)
   {
      Int pol=0;
      IPosition blc1(4,0,0,pol,0);
      IPosition trc1(4,nx_p,ny_p,pol,0);
      IPosition inc1(4, 1);
      LCBox::verify(blc1,trc1,inc1,itsMask->shape());
      LCBox singlepolmask(blc1,trc1,itsMask->shape());
      (mask_p)->copyData(SubLattice<Float>(*itsMask,singlepolmask,True));
      /* Reconcile the two masks */
      (*mask_p).copyData(LatticeExpr<Float>((*mask_p)*(*fftmask_p)));
   }
   else
   {
      (*mask_p).copyData(LatticeExpr<Float>((*fftmask_p)));
   }
   
   return 0;
}/* end of setupUserMask() */


/***************************************
 *  Set up the Blobs of various scales.
 ****************************************/
template <class T>
Int MultiTermLatticeCleaner<T>::setupBlobs()
{
  LogIO os(LogOrigin("MultiTermLatticeCleaner", "setupBlobs", WHERE));
	// Set the scale sizes
	if(scaleSizes_p.nelements()==0)
	{
		scaleSizes_p.resize(nscales_p);
		Float scaleInc = 2.0;
		scaleSizes_p[0] = 0.0;
		//os << "scale 1 = " << scaleSizes_p(0) << " pixels" << LogIO::POST;
		for (Int scale=1; scale<nscales_p;scale++) 
		{
			scaleSizes_p[scale] = scaleInc * pow(10.0, (Float(scale)-2.0)/2.0) ;
			//os << "scale " << scale+1 << " = " << scaleSizes_p(scale) << " pixels" << LogIO::POST;
			
		}  
	}
	
	scaleBias_p.resize(nscales_p);
	totalScaleFlux_p.resize(nscales_p);
	//Float prefScale=2.0;
	//Float fac=6.0;
	if(nscales_p>1)
	{
		for(Int scale=0;scale<nscales_p;scale++) 
		{
			//scaleBias_p[scale] = 1 - 0.4 * scaleSizes_p[scale]/scaleSizes_p(nscales_p-1);
			scaleBias_p[scale] = 1.0;
			//////scaleBias_p[scale] = pow((Float)scale/fac,prefScale)*exp(-1.0*scale/fac)/(pow(prefScale/fac,prefScale)*exp(-1.0*prefScale/fac));
			//scaleBias_p[scale] = pow((Float)(scale+1)/fac,prefScale)*exp(-1.0*(scale+1)/fac);
			os << "scale " << scale+1 << " = " << scaleSizes_p(scale) << " pixels with bias = " << scaleBias_p[scale] << LogIO::POST;
			totalScaleFlux_p[scale]=0.0;
		}
	}
	else scaleBias_p[0]=1.0;
	
	// Compute the scaled blobs - prolate spheroids with tapering and truncation
	// vecScales_p, scaleSizes_p, vecScalesFT_p
	if(!donePSP_p) 
	{
		// Compute h(s1), h(s2),... depending on the number of scales chosen.
		// NSCALES = 1;
		os << "Calculating scales and their FTs " << LogIO::POST;
			
		for (Int scale=0; scale<nscales_p;scale++) 
		{
			AlwaysAssert(vecScales_p[scale], AipsError);
			AlwaysAssert(vecScalesFT_p[scale], AipsError);
			
			// First make the scale
			makeScale(*vecScales_p[scale], scaleSizes_p(scale));
			// Now store the XFR
			vecScalesFT_p[scale]->copyData(LatticeExpr<Complex>(toComplex((*fftmask_p)*(*vecScales_p[scale]))));
			// Now FFT
			LatticeFFT::cfft2d(*vecScalesFT_p[scale], True);
			if(0)//(adbg)
			{
				String llab("blob_"+String::toString((Int)scaleSizes_p(scale))+".im");
				gip = IPosition(4,nx_p,ny_p,1,1);  
				TempLattice<Float> store(gip,memoryMB_p);
				store.copyData(LatticeExpr<Float>(real(*vecScalesFT_p[scale])));
				String fllab("blobft_"+String::toString((Int)scaleSizes_p(scale))+".im");
			}
		}
		donePSP_p=True;
	}
	
	return 0;
}/* end of setupBlobs() */



/***************************************
 *  Compute convolutions and the A matrix.
 ****************************************/
template <class T>
Int MultiTermLatticeCleaner<T>::computeMatrixA()
{
  LogIO os(LogOrigin("MultiTermLatticeCleaner", "computeMatrixA", WHERE));
   gip = IPosition(4,nx_p,ny_p,1,1);  
   
   if(!doneCONV_p)
   {  
      // Compute the convolutions of the smoothed psfs with each other.
      // Compute Assxx
      // Compute A100, A101, A102
      //         A110, A111, A112
      //         A120, A121, A122   for h(s1)
      // Compute A200, A201, A202
      //         A210, A211, A212
      //         A220, A221, A222   for h(s2)
      //... depending on the number of scales chosen
	   
      // (PSF * scale) * (PSF * scale) -> cubeA_p [nx_p,ny_p,ntaylor,ntaylor,nscales]
      os << "Calculating PSF and Scale convolutions " << LogIO::POST;
      for (Int taylor1=0; taylor1<ntaylor_p;taylor1++) 
      for (Int taylor2=0; taylor2<=taylor1;taylor2++) 
      for (Int scale1=0; scale1<nscales_p;scale1++) 
      for (Int scale2=0; scale2<=scale1;scale2++) 
      {
	Int ttay1 = taylor1+taylor2;
        if(adbg)
	   os << "Calculating (PSF_"<< taylor1 << " * Scale_"<<scale1+1 << ") * (PSF_"<< taylor2<<" * Scale_"<<scale2+1<<")   using taylor "<< ttay1 << LogIO::POST;
	
	LatticeExpr<Complex> dpsExpr(((*vecPsfFT_p[ttay1]) *(*vecPsfFT_p[0]))*(*vecScalesFT_p[scale1])*(*vecScalesFT_p[scale2]));
	cWork_p->copyData(dpsExpr);
	LatticeFFT::cfft2d(*cWork_p, False);
	AlwaysAssert(cubeA_p[IND4(taylor1,taylor2,scale1,scale2)], AipsError);
	LatticeExpr<Float> realWork2(real(*cWork_p));
	cubeA_p[IND4(taylor1,taylor2,scale1,scale2)]->copyData(realWork2);
  
	Float zmaxval=0.0; IPosition zmaxpos;
	findMaxAbsLattice((*mask_p),(*cubeA_p[IND4(taylor1,taylor2,scale1,scale2)]),zmaxval,zmaxpos);
	//if(adbg) os << "Max (result) : " << zmaxval << "  at " << zmaxpos << LogIO::POST;
      }	  

      // Construct A, invA for each scale.
      
      IPosition wip(4,0,0,0,0);
      wip[0]=(nx_p/2); wip[1]=(ny_p/2);
      for (Int scale=0; scale<nscales_p;scale++) 
      {
	      // Fill up A
	      for (Int taylor1=0; taylor1<ntaylor_p;taylor1++) 
	      for (Int taylor2=0; taylor2<ntaylor_p;taylor2++) 
	      {
                (*matA_p[scale])(taylor1,taylor2) = (*cubeA_p[IND4(taylor1,taylor2,scale,scale)])(wip);
	      }
	      
	      if(adbg)os << "The Matrix A is : " << (*matA_p[scale]) << LogIO::POST;
	      
	      // Compute inv(A) 
	      // Use MatrixMathLA::invert
	      // of Use invertSymPosDef...
	      //
	      Double deter=0.0;
	      ////MatrixMathLA::invert((*invMatA_p[scale]),deter,(*matA_p[scale]));
	      invertSymPosDef((*invMatA_p[scale]),deter,(*matA_p[scale]));
	      if(adbg)os << "A matrix determinant : " << deter << LogIO::POST;
	      //if(fabs(deter) < 1.0e-08) os << "SINGULAR MATRIX !! STOP!! " << LogIO::EXCEPTION;
	      if(adbg)os << "Lapack Cholesky Decomp Matrix inv(A) is : " << (*invMatA_p[scale]) << LogIO::POST;
	      
      }
      
      doneCONV_p=True;
   } 
   
   return 0;
}/* end of computeMatrixA() */


/***************************************
 *  Compute convolutions of the residual images ( all specs ) with scales.
 *  --> the Right-Hand-Side of the matrix equation.
 ****************************************/
template <class T>
Int MultiTermLatticeCleaner<T>::computeRHS()
{
  LogIO os(LogOrigin("MultiTermLatticeCleaner", "computeRHS()", WHERE));
	IPosition blc1(4,0,0,0,0);
	IPosition trc1(4,nx_p,ny_p,0,0);
	IPosition inc1(4, 1);
	
	/* Compute R10 = I_D*B10, R11 = I_D*B11, R12 = I_D*B12
	 * Compute R20 = I_D*B20, R21 = I_D*B21, R22 = I_D*B22
	 * ... depending on the number of scales chosen.
	 */

	//cout << "Writing residual images to disk..." << endl;
	//storeAsImg("temp_residual_0",residual(0));
	//storeAsImg("temp_residual_1",residualspec(0,1));
	
	/* I_D * (PSF * scale) -> matR_p [nx_p,ny_p,ntaylor,nscales] */
	//os << "Calculating I_D * PSF_i * Scale_j " << LogIO::POST;
	for (Int taylor=0; taylor<ntaylor_p;taylor++) 
	{
	   /* Compute FT of dirty image */
	   dirtyFT_p->copyData(LatticeExpr<Complex>(toComplex((*fftmask_p)*(*vecDirty_p[taylor]))));
	   LatticeFFT::cfft2d(*dirtyFT_p, True);
	   
	   for (Int scale=0; scale<nscales_p;scale++) 
	   {
		if(adbg)os << "Calculating I_D * (PSF_"<< taylor << " * Scale_" << scale+1 << ")"<< LogIO::POST;
		LatticeExpr<Complex> dpsExpr( (*dirtyFT_p)*(*vecPsfFT_p[0])*(*vecScalesFT_p[scale]));
		cWork_p->copyData(dpsExpr);
		LatticeFFT::cfft2d(*cWork_p, False);
		AlwaysAssert(matR_p[IND2(taylor,scale)], AipsError);
		LatticeExpr<Float> realWork2(real(*cWork_p));
		matR_p[IND2(taylor,scale)]->copyData(realWork2);
		//String lab("_"+String::toString(taylor)+"_"+String::toString(scale));
	   }
	}
	
	return 0;
}/* end of computeRHS() */

/***************************************
 *  Compute  flux limit for minor cycles
 ****************************************/
template <class T>
Int MultiTermLatticeCleaner<T>::computeFluxLimit(Float &fluxlimit, Float threshold)
{
  LogIO os(LogOrigin("MultiTermLatticeCleaner", "computeFluxLimit", WHERE));

	// Find max residual ( from all scale and taylor convos of the residual image )
	// Find max ext PSF value ( from all scale convos of all the PSFs )
	// factor = 0.5;
	// fluxlimit = maxRes * maxExtPsf * factor;
	
	Float maxRes=0.0;
	Float maxExtPsf=0.0;
	Float tmax=0.0;
	IPosition tmaxpos;
	Float ffactor=0.01;
	Int maxscale=0;
	
	for(Int taylor=0;taylor<ntaylor_p;taylor++)
	for(Int scale=0; scale<nscales_p;scale++)
	{
		findMaxAbsLattice((*mask_p),(*matR_p[IND2(taylor,scale)]),tmax,tmaxpos);
		if(tmax > maxRes) maxscale = scale;
		maxRes = MAX(maxRes,tmax);
	}
	for (Int taylor1=0; taylor1<ntaylor_p;taylor1++) 
	for (Int taylor2=0; taylor2<=taylor1;taylor2++) 
	for (Int scale1=0; scale1<nscales_p;scale1++) 
	for (Int scale2=0; scale2<=scale1;scale2++) 
	{
		findMaxAbsLattice((*mask_p),(*cubeA_p[IND4(taylor1,taylor2,scale1,scale2)]),tmax,tmaxpos, True);
		maxExtPsf = MAX(maxExtPsf,tmax);
	}
	
	Float norma = sqrt((1.0/(*matA_p[maxscale])(0,0)));
	
	fluxlimit = max(threshold, (maxRes*norma) * ffactor);
	
	if(adbg)os << "Max Res : " << maxRes*norma << " FluxLimit : " << fluxlimit << LogIO::POST;
	
	return 0;
}/* end of computeFluxLimit() */



/***************************************
 *  Solve the matrix eqn for each point in the lattice.
 ****************************************/
template <class T>
Int MultiTermLatticeCleaner<T>::solveMatrixEqn(Int scale)
{
	/* Solve for the coefficients */
	for(Int taylor1=0;taylor1<ntaylor_p;taylor1++)
	{
		len_p = LatticeExprNode(0.0);
		for(Int taylor2=0;taylor2<ntaylor_p;taylor2++)
		{
			len_p = len_p + LatticeExprNode((Float)(*invMatA_p[scale])(taylor1,taylor2)*(*matR_p[IND2(taylor2,scale)]));
		}
		(*matCoeffs_p[IND2(taylor1,scale)]).copyData(LatticeExpr<Float>(len_p));
	}
	
	return 0;
}/* end of solveMatrixEqn() */
	
/***************************************
 *  Compute the penalty function
 ****************************************/
template <class T>
Int MultiTermLatticeCleaner<T>::computePenaltyFunction(Int scale, Float &loopgain, Bool choosespec)
{
	tWork_p->set(0.0);
	
	for(Int i=0;i<(Int)itermatCoeffs_p.nelements();i++) itermatCoeffs_p[i]->reset();
	for(Int i=0;i<(Int)itercubeA_p.nelements();i++) itercubeA_p[i]->reset();
	for(Int i=0;i<(Int)itermatR_p.nelements();i++) itermatR_p[i]->reset();
	
	for(itertWork_p->reset(); !(itertWork_p->atEnd()); (*itertWork_p)++)
	{
		if(choosespec)
		{
			for(Int taylor1=0;taylor1<ntaylor_p;taylor1++)
			{
				itertWork_p->rwCursor() += (Float)2.0*((itermatCoeffs_p[IND2(taylor1,scale)])->rwCursor())*((itermatR_p[IND2(taylor1,scale)])->rwCursor());
				
				for(Int taylor2=0;taylor2<ntaylor_p;taylor2++)
					itertWork_p->rwCursor() -= ((itermatCoeffs_p[IND2(taylor1,scale)])->rwCursor())*((itermatCoeffs_p[IND2(taylor2,scale)])->rwCursor())*((itercubeA_p[IND4(taylor1,taylor2,scale,scale)])->rwCursor());
			}
			// Constrain location too, based on the I0 flux being > thresh*5 or something..
		}
		else 
		{
			if(loopgain > 0.5) loopgain*=0.5;
			Float norm = sqrt((1.0/(*matA_p[scale])(0,0)));
			itertWork_p->rwCursor() += norm*((itermatR_p[IND2(0,scale)])->rwCursor());
		}
		for(Int i=0;i<(Int)itermatCoeffs_p.nelements();i++) (*itermatCoeffs_p[i])++;
		for(Int i=0;i<(Int)itercubeA_p.nelements();i++) (*itercubeA_p[i])++;
		for(Int i=0;i<(Int)itermatR_p.nelements();i++) (*itermatR_p[i])++;
	}
	
	return 0;
}/* end of computePenaltyFunction() */

/***************************************
 *  Update the model images and the convolved residuals
 ****************************************/
template <class T>
Int MultiTermLatticeCleaner<T>::updateSolution(IPosition globalmaxpos, Int maxscaleindex, Float loopgain)
{
   gip = IPosition(4,nx_p,ny_p,1,1);  
   IPosition support(4,nx_p/2,ny_p/2,0,0);
   
   //IPosition psfpeak(support);
   IPosition psfpeak(itsPositionPeakPsf);
   globalmaxpos[2]=0;
   globalmaxpos[3]=0;
   
   /* Region for the inner quarter..... the update region. */
   IPosition inc(4,1,1,0,0);
   IPosition blc(psfpeak-support/2);
   IPosition trc(psfpeak+support/2-IPosition(4,1,1,0,0));
   LCBox::verify(blc, trc, inc, gip);
   
   /* Shifted region, with the psf at the globalmaxpos. */
   IPosition blcPsf(2*psfpeak-support/2-globalmaxpos);
   IPosition trcPsf(2*psfpeak+support/2-globalmaxpos-IPosition(4,1,1,0,0));
   LCBox::verify(blcPsf, trcPsf, inc, gip);

   makeBoxesSameSize(blc,trc,blcPsf,trcPsf);
   
   LCBox subRegion(blc,trc,gip);
   LCBox subRegionPsf(blcPsf,trcPsf,gip);
   
   /* Update the model image */
   for(Int taylor=0;taylor<ntaylor_p;taylor++)
   {
	   SubLattice<Float> modelSub(*vecModel_p[taylor],subRegion,True);
	   SubLattice<Float> scaleSub((*vecScales_p[maxscaleindex]),subRegionPsf,True);
	   addTo(modelSub,scaleSub,loopgain*(*matCoeffs_p[IND2(taylor,maxscaleindex)]).getAt(globalmaxpos));
   }
   
   /* Update the convolved residuals */
   for(Int scale=0;scale<nscales_p;scale++)
   for(Int taylor1=0;taylor1<ntaylor_p;taylor1++)
   {
	   SubLattice<Float> residSub((*matR_p[IND2(taylor1,scale)]),subRegion,True);
	   for(Int taylor2=0;taylor2<ntaylor_p;taylor2++)
	   {
		   SubLattice<Float> smoothSub((*cubeA_p[IND4(taylor1,taylor2,scale,maxscaleindex)]),subRegionPsf,True);
		   addTo(residSub,smoothSub,-1*loopgain*(*matCoeffs_p[IND2(taylor2,maxscaleindex)]).getAt(globalmaxpos));
	   }
   }
   
   /* Update flux counters */
   for(Int taylor=0;taylor<ntaylor_p;taylor++)
   {
	   totalTaylorFlux_p[taylor] += loopgain*(*matCoeffs_p[IND2(taylor,maxscaleindex)]).getAt(globalmaxpos);
   }
   totalScaleFlux_p[maxscaleindex] += loopgain*(*matCoeffs_p[IND2(0,maxscaleindex)]).getAt(globalmaxpos);
   
   return 0;
}/* end of updateSolution() */

/* ................ */
template <class T>
Int MultiTermLatticeCleaner<T>::checkConvergence(Bool choosespec, Float thresh, Float fluxlimit)
{
    /* Calculate convergence thresholds..... */
    Float rmaxval=0.0;
    
#if 0
    /* Use the strongest I0 component, to compare against the convergence threshold */
    Float compval = fabs((*matCoeffs_p[IND2(0,maxscaleindex)]).getAt(globalmaxpos));
    //Float compval = fabs((*matCoeffs_p[IND2(0,maxscaleindex)]).getAt(globalmaxpos)) * (scaleSizes_p[maxscaleindex]+1);
    rmaxval = MAX( rmaxval , compval );
#endif
    
#if 1	
    /* Use the maximum residual (current), to compare against the convergence threshold */
    Float maxres=0.0;
    IPosition maxrespos;
    findMaxAbsLattice((*mask_p),(*matR_p[IND2(0,0)]),maxres,maxrespos);
    Float norma = (1.0/(*matA_p[0])(0,0));
    
    //rmaxval = MAX(rmaxval, maxres*norma/5.0);
    rmaxval = maxres*norma;
#endif
    
    /* Check for convergence */
    /* Switch between penalty functions, after a I0 component lower than the threshold
       is picked. Until then, pick components that minimize chi-sq. After switching, 
       pick components that correspond to the peak I0 residual */

    Int convergedflag = 0;
    // 0 : continue
    // 1 : converged because of fluxlimit for this cycle
    // 2 : converged because of threshold
    // -1 : stopped because of iteration limit.

    if( (fabs(rmaxval) < thresh) ){ convergedflag = 2;}
    else 
    {
	if( fabs(rmaxval) < fluxlimit ) { convergedflag=1; }
    }
    
    //if((fabs(rmaxval) < fluxlimit) || (fabs(rmaxval) < thresh*1.5 && !choosespec))
    //{convergedflag=1;}
    //else
    //{
    //  if(fabs(rmaxval) < thresh*5.0 && choosespec)
    //  {convergedflag=0; choosespec=False; if(adbg)os << "Switching stopping criterion" << LogIO::POST;}
    //}

    /* Stop, if there are negatives on the largest scale in the Io image */
    //if(nscales_p>1 && maxscaleindex == nscales_p-2)
    //	if((*matCoeffs_p[IND2(0,maxscaleindex)]).getAt(globalmaxpos) < 0.0)
    //	{converged = False;break;}
 
    return convergedflag;

}/* end of checkConvergence */


/*************************************
 *         Find the max and position 
 *         - restrict this to within the inner quarter.
 *************************************/
template <class T>
Bool MultiTermLatticeCleaner<T>::findMaxAbsLattice(const TempLattice<Float>& masklat,const Lattice<Float>& lattice,Float& maxAbs,IPosition& posMaxAbs, Bool flip)
{

  AlwaysAssert(masklat.shape()==lattice.shape(), AipsError);

  Array<Float> msk;
  
  posMaxAbs = IPosition(lattice.shape().nelements(), 0);
  maxAbs=0.0;
  //maxAbs=-1.0e+10;
  const IPosition tileShape = lattice.niceCursorShape();
  TiledLineStepper ls(lattice.shape(), tileShape, 0);
  TiledLineStepper lsm(masklat.shape(), tileShape, 0);
  {
    RO_LatticeIterator<Float> li(lattice, ls);
    RO_LatticeIterator<Float> lim(masklat, lsm);
    for(li.reset(),lim.reset();!li.atEnd();li++,lim++) 
    {
      IPosition posMax=li.position();
      IPosition posMin=li.position();
      Float maxVal=0.0;
      Float minVal=0.0;
      
      msk = lim.cursor();
      if(flip) msk = (Float)1.0 - msk;
      
      //minMaxMasked(minVal, maxVal, posMin, posMax, li.cursor(),lim.cursor());
      minMaxMasked(minVal, maxVal, posMin, posMax, li.cursor(),msk);
      
      
      if((maxVal)>(maxAbs)) 
      {
        maxAbs=maxVal;
	posMaxAbs=li.position();
	posMaxAbs(0)=posMax(0);
      }
    }
  }

  return True;
}



} //# NAMESPACE CASA - END
