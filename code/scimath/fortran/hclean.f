      real function maxeig(i,q,u,v)
      real i, q, u, v
      maxeig=i+sqrt(q**2+u**2+v**2)
      return 
      end
      real function mineig(i,q,u,v)
      real i, q, u, v
      mineig=i-sqrt(q**2+u**2+v**2)
      return 
      end
      real function eigprod(i,q,u,v)
      real i, q, u, v
      eigprod=i**2-(q**2+u**2+v**2)
      return 
      end
      real function l2norm(i,q,u,v)
      real i, q, u, v
      l2norm=i**2+q**2+u**2+v**2
      return 
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
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
c   msgput	pointer to function which outputs the status message
c   stopnow	pointer to function which determines if it is stopping time
c      
      subroutine hclean(limage, limagestep, lpsf, domask, lmask, nx, ny, 
     $     npol, xbeg, xend, ybeg, yend, niter, siter, iter, gain, 
     $     thres, msgput, stopnow)
      
      implicit none
      integer nx, ny, npol, xbeg, xend, ybeg, yend
      integer niter, siter, iter, yes
      real limage(nx, ny, npol)
      real limagestep(nx, ny, npol)
      integer domask
      real lpsf(nx, ny), lmask(nx, ny)
      real gain, thres
      external msgput
      external stopnow

      real peak, mev, maxeig
      real maxVal(4), pv(4)
      integer px, py, pol, ix, iy, cycle
      integer x1, x2, y1, y2
      
      do iter=siter, niter
         
c     Now find peak in residual image 
         peak=0.0
         px=xbeg
         py=ybeg
         do iy=ybeg,yend
            do ix=xbeg,xend
               if(domask.EQ.0  .OR. (lmask(ix,iy).GT.0.000001) ) then
                  if(npol.EQ.4) then
                     mev=maxeig(limageStep(ix,iy,1),
     $                    limageStep(ix,iy,2), limageStep(ix,iy,3),
     $                    limageStep(ix,iy,4))
                  else if(npol.EQ.3) then
                     mev=maxeig(limageStep(ix,iy,1),
     $                    limageStep(ix,iy,2), limageStep(ix,iy,3),
     $                    0.0)
                  else if(npol.EQ.2) then
                     mev=limageStep(ix,iy,1)+ abs(limageStep(ix,iy,2))
                  else 
                     mev=limageStep(ix,iy,1)
                  end if
                  if(abs(mev).GT.peak) then
                     px=ix
                     py=iy
                     peak=abs(mev)
                  end if
               end if
            end do
         end do
      
         if(npol.EQ.4) then
            maxVal(1)=limageStep(px,py,1)
            maxVal(2)=limageStep(px,py,2)
            maxVal(3)=limageStep(px,py,3)
            maxVal(4)=limageStep(px,py,4)
         else if(npol.EQ.3) then
            maxVal(1)=limageStep(px,py,1)
            maxVal(2)=limageStep(px,py,2)
            maxVal(3)=limageStep(px,py,3)
            maxVal(4)=0.0
         else if(npol.EQ.2) then
            maxVal(1)=limageStep(px,py,1)
            maxVal(2)=0.0
            maxVal(3)=0.0
            maxVal(4)=limageStep(px,py,2)
         else 
            maxVal(1)=limageStep(px,py,1)
            maxVal(2)=0.0
            maxVal(3)=0.0
            maxVal(4)=0.0
         end if


         
c     // Output ten lines of information if run to the end
         cycle=max(1,(niter-siter)/10         )
         if((iter.EQ.siter).OR.(mod(iter,cycle)).EQ.1) then
            call msgput(iter, px, py, maxVal)
            call stopnow(yes)
         endif
         if((yes.EQ.1).OR.(peak.LT.thres)) then
            call msgput(-iter, px, py, maxVal)
            return
         endif
         
c  Add the scaled peak to the current image
c
         x1 = max( xbeg, px - nx/2 )
         y1 = max( ybeg, py - ny/2 )
         x2 = min( xend, px + nx/2 -1 )
         y2 = min( yend, py + ny/2 -1 )
         if((npol.EQ.4).OR.(npol.EQ.3)) then
            do pol=1,npol
               pv(pol)=gain*maxVal(pol)
            end do
            do pol=1,npol
               limage(px,py,pol)=limage(px,py,pol)+pv(pol)
            end do
            do pol=1,npol
               do iy=y1,y2
                  do ix=x1,x2
                     limageStep(ix,iy,pol)=limageStep(ix,iy,pol)
     $                    -pv(pol) * lpsf(nx/2+ix-px+1,ny/2+iy-py+1)
                  end do
               end do
            end do
         else if(npol.EQ.2) then
            pv(1)=gain*maxVal(1)
            pv(4)=gain*maxVal(4)
            limage(px,py,1)=limage(px,py,1)+pv(1)
            limage(px,py,2)=limage(px,py,2)+pv(4)
            do iy=y1,y2
               do ix=x1,x2
                  limageStep(ix,iy,1)=limageStep(ix,iy,1)
     $              - pv(1)*lpsf(nx/2+ix-px+1,ny/2+iy-py+1)
                  limageStep(ix,iy,2)=limageStep(ix,iy,2)
     $              - pv(4)*lpsf(nx/2+ix-px+1,ny/2+iy-py+1)
               end do
            end do
         else 
            pv(1)=gain*maxVal(1)
            limage(px,py,1)=limage(px,py,1)+pv(1)
            do iy=y1,y2
               do ix=x1,x2
                  limageStep(ix,iy,1)=limageStep(ix,iy,1)
     $                 -pv(1)*lpsf(nx/2+ix-px+1,ny/2+iy-py+1)
               end do
            end do
         end if
      end do
 100  continue
      call msgput(-niter, px, py, maxVal)
      return
      end


