//# LatticeCleanProgress.cc: Abstract base class to monitor progress in lattice operations
//# Copyright (C) 1997,1998,1999,2000,2001,2003
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


//# Includes
#include <casacore/lattices/LatticeMath/LatticeCleanProgress.h>
#include <casacore/casa/System/PGPlotter.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayMath.h>

#include <casacore/casa/sstream.h>



namespace casacore { //# NAMESPACE CASACORE - BEGIN

LatticeCleanProgress::LatticeCleanProgress(PGPlotter* pgplotter)
  : itsPgplotter(pgplotter),
    currentIndex(0),
    currentTotalIterations(0),
    currentFluxScale(1.0),
    currentMinFluxScale(0.0),
    currentMaxResidual(1.0),
    currentMinResidual(0.1),
    logMinRes(-1.0),
    logMaxRes(0.0),
    deltaY(0.1),
    xMin(0),
    xMax(1),
    fluxScaleJump(1.8),
    residScaleJump(3.0),
    forbidden(99999999.0)
{
}



LatticeCleanProgress::~LatticeCleanProgress()
{
  //  if(itsPgplotter) delete itsPgplotter; itsPgplotter=0;
}

// Call back function
bool LatticeCleanProgress::info(const bool lastcall,
				     const int32_t iteration,
				     const int32_t numberIterations,
				     const Vector<float>& maxima,
				     const Block<IPosition>& posMaximum,
				     const float strengthOptimum,
				     const int32_t optimumScale,
				     const IPosition&,
				     const float&,
				     const Vector<float>& totalFluxScale,
				     const bool resetBase) 
{
  uint32_t nScales = maxima.nelements();

  // "And this little piggy built his house out of straw..."
  //  When you remove the myDebug and cout statements, this core dumps.
  //  A veggie burger to the wolf who fixes this -- Mark H.
  bool myDebug = false;
  if (myDebug) cout << "A" << endl;

  // initialize some things here!
  if (iterationNumber.nelements() == 0) {
    initialize( maxima.nelements(),  abs(max(maxima)), numberIterations );
  } 

  // check to see if we need to increment baseFluxes
  if (resetBase && currentIndex > 0) {
    for (uint32_t i=0; i< nScales; i++) {
      baseFluxes(i) =  totalFluxesPer(i, currentIndex - 1);
    }
    baseFluxes(nScales) = totalFluxes(currentIndex-1);
  }

  // Do we need to resize the data storage?
  if (currentIndex >= totalFluxes.nelements() ) {
    resizeDataStorage();
  }

  // Fill in data storage

  if (myDebug) cout << "B" << endl;
  Vector<float> myTotalFluxScale(totalFluxScale.nelements());
  float myTotalFlux = 0;
  float myMinFlux = 0.0;

  iterationNumber(currentIndex) = iteration+1;
  for (uint32_t i=0;i<nScales;i++) {
    myTotalFluxScale(i) =  totalFluxScale(i) + baseFluxes(i);
    myTotalFlux += myTotalFluxScale(i);
    maxResiduals(i, currentIndex) = maxima(i);
    totalFluxesPer(i, currentIndex) = myTotalFluxScale(i);
    myMinFlux = min (myMinFlux, myTotalFluxScale(i));
  }
  totalFluxes(currentIndex) = myTotalFlux;
  myMinFlux = min (myMinFlux, myTotalFlux);

  for (uint32_t k=0;k<  nScales; k++) {
    if ( maxima(k) > 0.0) {
      posResiduals(k, currentIndex) = log10( maxima(k) );
    } else if ( maxima(k) < 0.0) {
      negResiduals(k, currentIndex) = log10( abs(maxima(k)) );
    }
  }
  currentIndex++;
  if (myDebug) cout << "C" << endl;

  if(itsPgplotter) {

    // Check for reploting conditions
    bool rePlot = false;
    if ( myTotalFlux > currentFluxScale) {
      rePlot = true;
      currentFluxScale *= fluxScaleJump;
    }
    if (min(abs(maxima)) < currentMinResidual) {
      rePlot = true;
      currentMinResidual /= residScaleJump;
    }
    if ( numberIterations > (int32_t)currentTotalIterations) {
      currentTotalIterations = numberIterations;
      rePlot = true;
    }
    if (myMinFlux < currentMinFluxScale) {
      currentMinFluxScale = -abs( fluxScaleJump * myMinFlux);
      rePlot = true;
    }

   if (rePlot) {
      basicSetUp(true);      
    } else {
      plotOne(iteration+1, maxima, myTotalFluxScale);
    }
  }  

  if (myDebug) cout << "D" << endl;


  //////////////////////////////////////
  // From here down: just Log Messages
  //////////////////////////////////////

  LogIO os(LogOrigin("ClarkCleanProgress", "info()", WHERE));
  // Always output this information
  if(!lastcall) {
    if(maxima.nelements()==1) {
      os << "Maximum abs = " << maxima(0) << " at "
	 << posMaximum[0]+1 << endl;
      os << "Iteration " << iteration+1 << " most significant residual = "
	 << strengthOptimum << " Jy, flux = " << myTotalFlux << endl;
    }
    else {
      for(uint32_t scale=0;scale<maxima.nelements();scale++) {
	os << "scale " << scale+1 << " maximum abs = " << maxima(scale) << " at "
	   << posMaximum[scale]+1 << ", flux = " << myTotalFluxScale(scale)
	   << endl;
      }
      os << "Iteration " << iteration+1 << " most significant residual = "
	 << strengthOptimum << " Jy, optimum scale " << optimumScale+1
	 << endl;
    }
  }
  else {
    for(uint32_t scale=0;scale<maxima.nelements();scale++) {
      os << "Total flux on scale " << scale+1 << " = "
	 << myTotalFluxScale(scale) << " Jy" << LogIO::POST;
    }
  }
  os << "Total flux = " << myTotalFlux << " Jy" << LogIO::POST;
  return false;
}


void  LatticeCleanProgress::basicSetUp(bool doPlot)
{
  // Set these global plotter scale variables

  logMinRes = log10(abs(currentMinResidual));
  logMaxRes = log10(abs(currentMaxResidual));
  deltaY = abs(logMaxRes - logMinRes);
  //  logMaxRes += 0.05*deltaY;
  //  logMinRes -= 0.05*deltaY;
  xMax = float(currentTotalIterations)*1.15;
  xMin = -0.05*float(currentTotalIterations);

  itsPgplotter->sch(0.6);
  itsPgplotter->sci(1);
  itsPgplotter->page();
  itsPgplotter->svp(0.06, 0.94, 0.64, 0.92);
  itsPgplotter->swin(xMin, xMax, logMinRes, logMaxRes);
  itsPgplotter->box("BCST", 0, 0, "BCNLST", 0, 0);
  itsPgplotter->lab(" ", "+ Peak Resid (Jy)", "Components subtracted");

  uint32_t scale;
  uint32_t nScales =  posResiduals.nrow();
  itsPgplotter->iden();

  for (scale=0;scale<nScales;scale++) {
    itsPgplotter->sci(scale+2);
    ostringstream oos;
    oos << "Scale " << scale+1;
    itsPgplotter->text(0.85*xMax,
		       (logMaxRes - 0.1*(1+scale)*deltaY),
		       oos);
  }

  if (doPlot) {
    for (scale=0;scale<nScales;scale++) {
      itsPgplotter->sci(scale+2);
      itsPgplotter->pt(iterationNumber, posResiduals.row(scale), 2);
    }
  }

  // middle graph
  itsPgplotter->sci(1);
  itsPgplotter->svp(0.06, 0.94, 0.36, 0.64);
  itsPgplotter->swin(xMin, xMax, logMaxRes, logMinRes);
  itsPgplotter->box("BCST", 0, 0, "BCNLST", 0, 0);
  itsPgplotter->lab(" ", "- Peak Resid (Jy)", " ");

  if (doPlot) {
    for (scale=0;scale<nScales;scale++) {
      itsPgplotter->sci(scale+2);
      itsPgplotter->pt(iterationNumber, negResiduals.row(scale), 2);
    }
  }

  // lower graph
  itsPgplotter->sci(1);
  itsPgplotter->svp(0.06, 0.94, 0.09, 0.36);
  itsPgplotter->swin(xMin, xMax, currentMinFluxScale, currentFluxScale);
  itsPgplotter->box("BCNST", 0, 0, "BCNST", 0, 0);
  itsPgplotter->lab("Number of iterations", "Total Flux", " ");

  {
    itsPgplotter->sci(1);
    ostringstream oos;
    oos << "Total Flux ";
    itsPgplotter->text(0.85*xMax, 
		       (0.5*(currentFluxScale - currentMinFluxScale)), 
		       oos);
  }

   if (doPlot) {
    for (scale=0;scale<nScales;scale++) {
      itsPgplotter->sci(scale+2);
      itsPgplotter->pt(iterationNumber, totalFluxesPer.row(scale), 2);
    }
    itsPgplotter->sci(1);
    itsPgplotter->pt(iterationNumber, totalFluxes, 2);
  }

  
}

void  LatticeCleanProgress::plotOne(const int32_t iteration, 
					 const Vector<float>& resid, 
					 const Vector<float>& flux)
{

  // assuming we've already called  basicSetUp, the scaling variables
  // are all setup already;  else, we'd better call them

  Vector<float> x(1);
  Vector<float> y(1);
  x(0) = iteration;
  itsPgplotter->sch(0.6);

  for (uint32_t i=0; i<resid.nelements(); i++) {
    itsPgplotter->sci(i+2);
    if (resid(i) > 0) {
      // top graph
      itsPgplotter->svp(0.06, 0.94, 0.64, 0.92);
      itsPgplotter->swin(xMin, xMax, logMinRes, logMaxRes);
      y(0) = log10(resid(i));
      itsPgplotter->pt(x,y,2);
    } else if (resid(i) < 0) {
      // middle graph
      itsPgplotter->svp(0.06, 0.94, 0.36, 0.64);
      itsPgplotter->swin(xMin, xMax, logMaxRes, logMinRes);
      y(0) = log10(abs(resid(i)));
      itsPgplotter->pt(x,y,2);
    }
  }

  // lower graph
  itsPgplotter->sci(1);
  itsPgplotter->svp(0.06, 0.94, 0.09, 0.36);
  itsPgplotter->swin(xMin, xMax, currentMinFluxScale, currentFluxScale);
  float sumf = sum(flux);
  for (uint32_t i=0; i<flux.nelements(); i++) {
    itsPgplotter->sci(i+2);
    y(0) = flux(i);
    itsPgplotter->pt(x,y,2);
  }
  itsPgplotter->sci(1);
  y(0) = sumf;
  itsPgplotter->pt(x,y,2);
}


void LatticeCleanProgress::resizeDataStorage()
{
  uint32_t nn = totalFluxesPer.ncolumn();
  uint32_t nScales = totalFluxesPer.nrow();

  Vector<float> tfr(totalFluxes);
  Vector<float> inr(iterationNumber);

  Matrix<float> tfpr(totalFluxesPer);
  Matrix<float> mrr(maxResiduals);
  Matrix<float> nrr(negResiduals);
  Matrix<float> prr(posResiduals);
  
  totalFluxes.resize(2*nn+1);
  iterationNumber.resize(2*nn+1);

  totalFluxesPer.resize(nScales, 2*nn+1);
  maxResiduals.resize(nScales, 2*nn+1);
  negResiduals.resize(nScales, 2*nn+1);
  posResiduals.resize(nScales, 2*nn+1);
  
  // to prevent trailing (or invalid) vector elements from being plotted
  iterationNumber = forbidden;
  posResiduals = forbidden;
  negResiduals = forbidden;
  
  // this is not precisely correct, as posRes and negRes have different
  // number of valid elements than totalFluxes; but should be safe
  uint32_t i, j;
  for (i=0;i<nn;i++) {
    totalFluxes(i) = tfr(i);
    iterationNumber(i) = inr(i);
    for (j=0;j<nScales;j++) {
      maxResiduals(j,i) = mrr(j,i);
      posResiduals(j,i) = prr(j,i);
      negResiduals(j,i) = nrr(j,i);
      totalFluxesPer(j,i) = tfpr(j,i);
    }
  }
}



void LatticeCleanProgress::initialize(const uint32_t nScales, 
					   const float& absMaxResid, 
					   const uint32_t numberIterations ) 
{
  iterationNumber.resize(100);
  totalFluxes.resize(100);  
  maxResiduals.resize(nScales, 100);  
  posResiduals.resize(nScales, 100);
  negResiduals.resize(nScales, 100);
  totalFluxesPer.resize(nScales, 100);  
  baseFluxes.resize(nScales+1); // extra one is the total flux;
  baseFluxes.set(0.0);

  // to prevent trailing vector elements from being plotted
  // (PGPLOT call doesn't let us say "plot just the first N elements",
  // so we'll put the X values of all points off the graph)
  iterationNumber = forbidden;
  posResiduals = forbidden;
  negResiduals = forbidden;

  if(itsPgplotter) {
  
    currentFluxScale = 1.5* absMaxResid ;
    currentMinFluxScale = 0.0;
    currentMaxResidual = currentFluxScale;
    currentMinResidual = currentMaxResidual/(1.5*residScaleJump);
    currentTotalIterations = numberIterations;
    basicSetUp(numberIterations);
    
  }
}




} //# NAMESPACE CASACORE - END

