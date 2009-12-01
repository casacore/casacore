*=======================================================================
*     Copyright (C) 1999,2001
*     Associated Universities, Inc. Washington DC, USA.
*
*     This library is free software; you can redistribute it and/or
*     modify it under the terms of the GNU Library General Public
*     License as published by the Free Software Foundation; either
*     version 2 of the License, or (at your option) any later version.
*
*     This library is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Library General Public License for more details.
*
*     You should have received a copy of the GNU Library General Public
*     License along with this library; if not, write to the Free
*     Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
*     MA 02139, USA.
*
*     Correspondence concerning AIPS++ should be addressed as follows:
*            Internet email: aips2-request@nrao.edu.
*            Postal address: AIPS++ Project Office
*                            National Radio Astronomy Observatory
*                            520 Edgemont Road
*                            Charlottesville, VA 22903-2475 USA
*
*     $Id$
*-----------------------------------------------------------------------
c
c  This code has been extracted from MIRIAD's selfcal.for routine,
c  originally written by Bob Sault. Also note that the BIMA and ATNF
c  versions of MIRIAD have slightly different versions of selfcal.for
c  (but not these two subroutines) and derivate programs, such as
c  BIMA's mselfcal.for and gmake.for
c
	subroutine phasol(Nblines,NAnts,Sum,SumVM,b1,b2,Gain,Convrg)
c
	implicit none
	logical Convrg
	integer Nblines,Nants
	integer b1(NBlines),b2(NBlines)
	complex SumVM(Nblines),Gain(NAnts),Sum(NAnts)
c
c  Solve for the phase corrections which minimise the error. This uses
c  a nonlinear Jacobi iteration, as suggested by Fred Schwab in "Adaptive
c  calibration of radio interferomter data", SPIE Vol. 231, 1980
c  International Optical Computing Conference (pp 18-24). The damping
c  heuristics are copied from AIPS ASCAL.
c
c  Input:
c    NBlines	Number of baselines.
c    Nants	Number of antennae.
c    b1,b2	This gives the antennae pair for each baseline.
c    SumVM	Sum of Model*conjg(Vis)
c  Scratch:
c    Sum
c  Output:
c    Convrg	If .true., then the algorithm converged.
c    Gain	The antenna gain solution.
c------------------------------------------------------------------------
	integer MaxIter
	real Epsi,Epsi2
	parameter(MaxIter=100,Epsi=1.e-8,Epsi2=1.e-4)
c
	real Factor,Change, absSum
	complex Temp
	integer Niter,i, count
c
c  Initialise.
c
	do i=1,NAnts
	  Gain(i) = (1.,0.)
	  Sum(i) = (0.,0.)
	enddo
c
	Factor = 0.8
	if(Nants.le.6)Factor = 0.5
c
c  Iterate.
c
	Convrg = .false.
	Niter = 0
	do while(.not.Convrg.and.Niter.lt.MaxIter)
	  Niter = Niter + 1
c
c  Sum the contributions over the baselines. Note that the following
c  loop has a dependency.
c
	  do i=1,nblines
	    Sum(b1(i)) = Sum(b1(i)) + Gain(b2(i)) *       SumVM(i)
	    Sum(b2(i)) = Sum(b2(i)) + Gain(b1(i)) * conjg(SumVM(i))
	  enddo
c
c  Update the gains. The following will be flagged as a short loop
c  on the Cray, if we assume we have fewer than 32 antennae. Hopefully
c  this will vectorise. For "typical" cases, the absolute value function
c  in the next loop takes up about 30-40% of the run time of this routine
c  on a VAX.
c
	  Change = 0
          count = 0
c#maxloop 32
	  do i=1,nants
	    absSum = abs(Sum(i))
	    if (absSum.gt.0.0) then
	       Temp = ( Sum(i)/absSum )
	       Temp = Gain(i) + Factor * ( Temp - Gain(i) )
	       Temp = Temp/abs(Temp)
	       Change = Change + real(Gain(i)-Temp)**2
     *			    + aimag(Gain(i)-Temp)**2
	       Gain(i) = Temp
               count = count + 1
	     endif
	    Sum(i) = (0.,0.)
	  enddo
	  Convrg = Change/count .lt. Epsi
	enddo
	Convrg = Change/count .lt. Epsi2
	end
c************************************************************************
	subroutine amphasol(NBlines,NAnts,Sum,Sum2,SumVM,SumVV,
     *						b1,b2,gain,convrg)
c
	implicit none
	logical Convrg
	integer NBlines,NAnts
	integer B1(NBlines),B2(NBlines)
	complex SumVM(NBlines),Gain(NAnts),Sum(NAnts)
	real SumVV(NBlines),Sum2(NAnts)
c
c  Solve for the amplitudes and phase corrections which minimise
c  error. Algorithm is again Schwab's Jacobi iteration. The damping
c  heuristics are copied from AIPS ASCAL or dreamed up by me (as was the
c  initial gain estimate).
c
c  Input:
c    NBlines	Number of baselines.
c    Nants	Number of antennae.
c    b1,b2	This gives the antennae pair for each baseline.
c    SumVM	Sum of Model*conjg(Vis), for each baseline.
c    SumVV	Sum of Vis*conjg(Vis), for each baseline.
c  Scratch:
c    Sum
c  Output:
c    Convrg	If .true., then the algorithm converged.
c    Gain	The antenna gain solution.
c
c------------------------------------------------------------------------
	integer maxiter
	real Epsi,Epsi2
	parameter(maxiter=100,Epsi=1.e-8,Epsi2=1e-4)
	integer i,Niter
	real Factor,Change,SumRVV,SumWt,SumRVM
	complex Temp
	real Factors(11)
	data Factors/0.5,0.75,8*0.9,0.5/
c
c  Calculate initial phase gain estimates.
c
	call phasol(NBlines,Nants,Sum,SumVM,b1,b2,gain,convrg)
	if(.not.convrg)return
c
c  Get an initial approximation of the gain solution. This finds a single
c  real gain which minimises the error. This helps stablise the algorithm
c  when the gain solution is very different from 1 (e.g. when we are
c  calculating a priori calibration factors).
c
	SumRVM = 0
	SumRVV = 0
	do i=1,NBlines
	  SumRVM = SumRVM + conjg(gain(b1(i)))*gain(b2(i))*SumVM(i)
	  SumRVV = SumRVV + SumVV(i)
	enddo
	Factor = sqrt(abs(SumRVM / SumRVV))
c
c  Ready for the amplitude/phase solution.
c
	do i=1,NAnts
	  Gain(i) = Factor * Gain(i)
	  Sum(i) = 0
	  Sum2(i) = 0.
	enddo
c
c  Iterate.
c
	Convrg = .false.
	Niter = 0
	do while(.not.Convrg.and.Niter.lt.MaxIter)
	  Niter = Niter + 1
c
c  Set the damping constant. I do not think this is really necessary.
c  AIPS does it.
c
	  if(Nants.le.6)then
	    Factor = 0.5
	  else
	    Factor = Factors(min(11,Niter))
	  endif
c
c  Sum the contributions over the baselines. Note that the following
c  loop has a dependency.
c
	  do i=1,nblines
	    Sum(b1(i)) = Sum(b1(i)) + Gain(b2(i)) *       SumVM(i)
	    Sum(b2(i)) = Sum(b2(i)) + Gain(b1(i)) * conjg(SumVM(i))
	    Sum2(b1(i)) = Sum2(b1(i)) +
     *	     (real(Gain(b2(i)))**2 + aimag(Gain(b2(i)))**2) * SumVV(i)
	    Sum2(b2(i)) = Sum2(b2(i)) +
     *	     (real(Gain(b1(i)))**2 + aimag(Gain(b1(i)))**2) * SumVV(i)
	  enddo
c
c  Update the gains. The following should be flagged as a short loop
c  on the Cray, if we assume we have fewer than 32 antennae. Hopefully
c  this will vectorise.
c
	  Change = 0
	  SumWt = 0
c#maxloop 32
	  do i=1,nants
	    if (Sum2(i).gt.0) then 
	       Temp = Sum(i)/Sum2(i) - Gain(i)
	       Gain(i) = Gain(i) + Factor * Temp
	       Change = Change + (real(Temp)**2 + aimag(Temp)**2)
	       SumWt  = SumWt  + (real(Gain(i))**2 + aimag(Gain(i))**2)
	    endif
	    Sum(i) = (0.,0.)
	    Sum2(i) = 0.
	  enddo
	  Convrg = Change/SumWt .lt. Epsi
	enddo
	Convrg = Change/SumWt .lt. Epsi2
	end
