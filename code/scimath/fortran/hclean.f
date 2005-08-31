*=======================================================================
*     Copyright (C) 1999,2000,2003
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
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   limagestep	real(nx,ny,npol) input dirty image; output residual image
c   domask	integer		 domask == 0 ==> no mask is present
c                                (formerly a logical, but that was trouble)
c   lmask	real(nx,ny) 	 input mask image
c   nx,ny,npol	integer		 shape of images
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine maximg(limagestep, domask, lmask, nx, ny, npol, fmin,
     $     fmax)
      
      implicit none
      integer nx, ny, npol, domask
      integer xbeg, xend, ybeg, yend
      real limagestep(nx, ny, npol)
      real lmask(nx, ny)
      real fmin, fmax

      real wpeak
      integer pol, ix, iy
      integer x1, x2, y1, y2
      
c     Now find peak in residual image 
      fmin=1e20
      fmax=-1e20
      do pol=1,npol
         do iy=1,ny
            do ix=1,nx
               if((domask.eq.0).or.(lmask(ix,iy).GT.0.5)) then
                  wpeak = limageStep(ix,iy,pol)
                  if(wpeak.GT.fmax) then
                     fmax=wpeak
                  end if
                  if(wpeak.LT.fmin) then
                     fmin=wpeak
                  end if
               end if
            end do
         end do
      end do
      
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   limage	real(nx,ny,npol) output model image (clean components image)
c   limagestep	real(nx,ny,npol) input dirty image; output residual image
c   lpsf	real(nx,ny) 	 input point spread function
c   domask	integer		 domask == 0 ==> no mask is present
c                                (formerly a logical, but that was trouble)
c   lmask	real(nx,ny) 	 input mask image
c   nx,ny,npol	integer		 shape of images
c   xbeg,xend	integer		 shape of box to clean  -  this could be larger
c   ybeg,yend                        than the inner quarter, but the subtraction
c                                    will be incorrect then for points outside the
c				     inner quarter   
c   niter	integer		 maximum allowed iterations
c   siter	integer		 starting iteration number
c   iter	integer		 last iteration number we get to in this algorithm
c   gain	real		 clean loop gain
c   thres	real		 flux cleaning threshold
c   cspeedup	real		 if > 0, thres = thres_o * exp( iter/cspeedup )
c   msgput	pointer to function which outputs the status message
c   stopnow	pointer to function which determines if it is stopping time
c      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine hclean(limage, limagestep, lpsf, domask, lmask, nx, ny, 
     $     npol, xbeg, xend, ybeg, yend, niter, siter, iter, gain, 
     $     thres, cspeedup, msgput, stopnow)
      
      implicit none
      integer nx, ny, npol, xbeg, xend, ybeg, yend
      integer niter, siter, iter, yes
      real limage(nx, ny, npol)
      real limagestep(nx, ny, npol)
      integer domask
      real lpsf(nx, ny), lmask(nx, ny)
      real gain, thres, cspeedup
      external msgput
      external stopnow

      real cthres
      real maxval, pv, val
      integer px, py, pol, ix, iy, cycle, i, maxiter
      integer x1, x2, y1, y2
      
      maxiter = siter
      yes=0
      do pol=1,npol
         do iter=siter, niter
            maxval=0.0
            px=1
            py=1
            do iy=ybeg,yend
               do ix=xbeg,xend
                  if((domask.eq.0).or.(lmask(ix,iy).GT.0.5)) then
                     val = abs(limagestep(ix,iy,pol))
                     if(val.GT.maxval) then
                        px=ix
                        py=iy
                        maxval=val
                     end if
                  end if
               end do
            end do
c     
            maxval=limageStep(px,py,pol)

            if (cspeedup .gt. 0.0) then
               cthres = thres * 2.0**(real(iter-siter)/cspeedup )
            else
               cthres = thres
            endif
            if((yes.EQ.1).OR.(abs(maxval).LT.cthres)) then
               goto 200
            endif
            
c     // Output ten lines of information if run to the end
            cycle=max(1,(niter-siter)/10)

            if((iter.EQ.siter).OR.(mod(iter,cycle)).EQ.1) then
               call msgput(npol, pol, iter, px, py, maxval)
               call stopnow(yes)
            endif
            if(yes.EQ.1) then
               goto 200
            endif

            x1 = max( 1, px - nx/2 )
            y1 = max( 1, py - ny/2 )
            x2 = min( nx, px + nx/2 -1 )
            y2 = min( ny, py + ny/2 -1 )
            pv=gain*maxval
            limage(px,py,pol)=limage(px,py,pol)+pv
            do iy=y1,y2
               do ix=x1,x2
                  limageStep(ix,iy,pol)=limageStep(ix,iy,pol)
     $                 -pv * lpsf(nx/2+ix-px+1,ny/2+iy-py+1)
               end do
            end do

         end do
 200     continue
         if(iter.gt.siter) then
            call msgput(npol, pol, -iter, px, py, maxval)
         end if
         maxiter=max(iter, maxiter)
      end do

 100  continue
      iter=maxiter
      if (iter .gt. niter) then
         iter = niter
      endif
      return
      end


