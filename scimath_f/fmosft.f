*=======================================================================
*     Copyright (C) 1999,2001,2002
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
C
C Grid a number of visibility records
C
      subroutine gmosft (uvw, dphase, values, nvispol, nvischan,
     $     dopsf, flag, rflag, weight, nrow, rownum,
     $     scale, offset, grid, nx, ny, npol, nchan, freq, c,
     $     support, convsize, sampling, convfunc, 
     $     chanmap, polmap,
     $     sumwt, weightgrid, convweight, doweightgrid, convplanemap, 
     $     nconvplane)

      implicit none
      integer nx, ny, npol, nchan, nvispol, nvischan, nrow
      complex values(nvispol, nvischan, nrow)
      complex grid(nx, ny, npol, nchan)
     
      double precision uvw(3, nrow), freq(nvischan), c, scale(3),
     $     offset(3)
      double precision dphase(nrow), uvdist
      double precision xlast, ylast
      complex phasor
      integer flag(nvispol, nvischan, nrow)
      integer rflag(nrow)
      real weight(nvischan, nrow), phase
      double precision sumwt(npol, nchan)
      integer rownum
      integer support
      integer chanmap(nchan), polmap(npol)
      integer dopsf
      complex weightgrid(nx, ny, npol, nchan)
      integer doweightgrid

      complex nvalue
      complex nweight
      integer convsize, sampling
      integer nconvplane
      integer convplanemap(nrow)
      complex convfunc(convsize, convsize, nconvplane), cwt, crot
      complex convweight(convsize, convsize, nconvplane)

      complex shiftx(-support:support), shifty(-support:support)
      complex sconv(-support:support, -support:support, nconvplane)
      complex sconv2(-support:support, -support:support, nconvplane)
      real sumsconv
      real sumsconv2
      real ratioofbeam

      real norm
      real wt

      logical omosft, doshift

      real pos(3)
      integer loc(3), off(3), iloc(3)
      integer rbeg, rend
      integer ix, iy, iz, ipol, ichan
      integer apol, achan, aconvplane, irow
      double precision pi
      data pi/3.14159265358979323846/
      
      irow=rownum

      sumsconv=0
      sumsconv2=0
      ratioofbeam=1.0
      do ix=-support,support
         shiftx(ix)=1.0
         shifty(ix)=1.0
      end do
      do iz=1, nconvplane
         do iy=-support,support
            iloc(2)=iy+convsize/2+1
            do ix=-support,support
               iloc(1)=ix+convsize/2+1
               sconv(ix,iy,iz)=(convfunc(iloc(1), iloc(2),iz))
               sconv2(ix,iy,iz)=convweight(iloc(1), iloc(2),iz)
            end do
         end do
      end do
      doshift=.FALSE.

      if(irow.ge.0) then
         rbeg=irow+1
         rend=irow+1
      else 
         rbeg=1
         rend=nrow
      end if




      xlast=0.0
      ylast=0.0
      do irow=rbeg, rend
         aconvplane=convplanemap(irow)+1
         if(rflag(irow).eq.0) then 
            do ichan=1, nvischan
               achan=chanmap(ichan)+1
               if((achan.ge.1).and.(achan.le.nchan).and.
     $              (weight(ichan,irow).gt.0.0)) then
                  call smosft(uvw(1,irow), dphase(irow), freq(ichan), c, 
     $                 scale, offset, sampling, pos, loc, off, phasor)
                  if (omosft(nx, ny, loc, support)) then
                     do ipol=1, nvispol
                        apol=polmap(ipol)+1
                        if((flag(ipol,ichan,irow).ne.1).and.
     $                       (apol.ge.1).and.(apol.le.npol)) then
C     If we are making a PSF then we don't want to phase
C     rotate but we do want to reproject uvw
                           if(dopsf.eq.1) then
                              nvalue=cmplx(weight(ichan,irow))
                           else
                              nvalue=weight(ichan,irow)*
     $                             (values(ipol,ichan,irow)*phasor)
                           end if
                           if(doweightgrid .gt. 0) then
                              nweight=cmplx(weight(ichan,irow))
                           end if
                           
C     norm will be the value we would get for the peak
C     at the phase center. We will want to normalize 
C     the final image by this term.
                           norm=0.0
                           if(sampling.eq.1) then
                              do iy=-support,support
                                 do ix=-support,support
                                    grid(loc(1)+ix,
     $                                   loc(2)+iy,apol,achan)=
     $                                   grid(loc(1)+ix,
     $                                   loc(2)+iy,apol,achan)+
     $                                   nvalue*sconv(ix,iy, aconvplane)

                                    if(doweightgrid .gt. 0) then
                                       iloc(1)=nx/2+1+ix
                                       iloc(2)=ny/2+1+iy
                                       weightgrid(iloc(1),iloc(2),
     $                                  apol,achan)= weightgrid(
     $                                  iloc(1),iloc(2),apol,achan)
     $                               + nweight*sconv2(ix,iy,aconvplane)

                                    end if
                                 end do
                              end do
                           else 
                              do ix=-support,support
                                 iloc(1)=convsize/2+1+ix*sampling
     $                                +off(1)
                                 if(doshift) then
                                    cwt=conjg(convfunc(iloc(1),
     $                                   iloc(2),aconvplane))*
     $                                   shiftx(ix)*shifty(iy)
                                    sumsconv=sumsconv+real(cwt)
                                 else
                                    cwt=conjg(convfunc(iloc(1),
     $                                   iloc(2),aconvplane))
                                    sumsconv=sumsconv+real(cwt)
                                 end if
                                 grid(loc(1)+ix,
     $                                loc(2)+iy,apol,achan)=
     $                                grid(loc(1)+ix,
     $                                loc(2)+iy,apol,achan)+
     $                                nvalue*cwt
                                 norm=norm+real(cwt)
                              end do
                           end if  
                           sumwt(apol, achan)= sumwt(apol,achan)+
     $                             weight(ichan,irow)
                        end if
                     end do
                  end if
               end if
            end do
         end if
      end do
      return
      end
C
C Degrid a number of visibility records
C
      subroutine dmosft (uvw, dphase, values, nvispol, nvischan,
     $     flag, rflag,
     $     nrow, rownum, scale, offset, grid, nx, ny, npol, nchan, freq,
     $     c, support, convsize, sampling, convfunc,
     $     chanmap, polmap, convplanemap, nconvplane)

      implicit none
      integer nx, ny, npol, nchan, nvispol, nvischan, nrow
      integer nconvplane
      complex values(nvispol, nvischan, nrow)
      complex grid(nx, ny, npol, nchan)
      double precision uvw(3, nrow), freq(nvischan), c, scale(2),
     $     offset(2)
      double precision dphase(nrow), uvdist
      double precision xlast, ylast
      complex phasor
      integer flag(nvispol, nvischan, nrow)
      integer rflag(nrow)
      integer rownum
      integer support
      integer chanmap(nchan), polmap(npol)
      integer convplanemap(nrow)
      complex nvalue

      integer convsize, sampling
      complex convfunc(convsize, convsize, nconvplane), cwt, crot

      complex shiftx(-support:support), shifty(-support:support)
      complex sconv(-support:support, -support:support, nconvplane)
      real sconv2(-support:support, -support:support, nconvplane) 
      real sumsconv2

      real norm, phase

      logical omosft, doshift

      real pos(2)
      integer loc(2), off(2), iloc(2)
      integer rbeg, rend
      integer ix, iy, iz, ipol, ichan
      integer apol, achan, aconvplane, irow
      real wt, wtx, wty
      double precision pi
      data pi/3.14159265358979323846/

      irow=rownum

      do ix=-support,support
         shiftx(ix)=1.0
         shifty(ix)=1.0
      end do
      sumsconv2=0.0
      do iz=1, nconvplane
         do iy=-support,support
            iloc(2)=iy+convsize/2+1
            do ix=-support,support
               iloc(1)=ix+convsize/2+1
               sconv(ix,iy,iz)=conjg(convfunc(iloc(1), iloc(2),iz))
C     sconv2(ix,iy)=abs(sconv(ix,iy))
C     sumsconv2=sumsconv2+sconv2(ix,iy)
            end do
         end do
      end do
      doshift=.FALSE.

      if(irow.ge.0) then
         rbeg=irow+1
         rend=irow+1
      else 
         rbeg=1
         rend=nrow
      end if
C
      xlast=0.0
      ylast=0.0
      do irow=rbeg, rend
         aconvplane=convplanemap(irow)+1
         if(rflag(irow).eq.0) then
            do ichan=1, nvischan
               achan=chanmap(ichan)+1
               if((achan.ge.1).and.(achan.le.nchan)) then
                  call smosft(uvw(1,irow), dphase(irow), freq(ichan), c,
     $                 scale, offset, sampling, pos, loc, off, phasor)
                  if (omosft(nx, ny, loc, support)) then
                     do ipol=1, nvispol
                        apol=polmap(ipol)+1
                        if((flag(ipol,ichan,irow).ne.1).and.
     $                       (apol.ge.1).and.(apol.le.npol)) then
                           nvalue=0.0
                           norm=0.0
                           if(sampling.eq.1) then
                              do iy=-support,support
                                 do ix=-support,support
                                    nvalue=nvalue+(sconv(ix,iy,
     $                               aconvplane))*
     $                                   grid(loc(1)+ix,loc(2)+iy,
     $                                   apol,achan)
                                 end do
                              end do
                           else
                              do iy=-support,support
                                 iloc(2)=convsize/2+1+sampling*iy+off(2)
                                 do ix=-support,support
                                    iloc(1)=convsize/2+1+ix*sampling
     $                                   +off(1)
                                    if(doshift) then
                                       cwt=conjg(convfunc(iloc(1),
     $                                 iloc(2), aconvplane))*shiftx(ix)*
     $                                      shifty(iy)
                                    else
                                       cwt=conjg(convfunc(iloc(1),
     $                                      iloc(2),aconvplane))
                                    end if
                                    nvalue=nvalue+conjg(cwt)*
     $                                   grid(loc(1)+ix,loc(2)+iy,
     $                                   apol,achan)
                                 end do
                              end do
                           end if 
                           values(ipol,ichan,irow)=nvalue*conjg(
     $                         phasor)
                       end if
                     end do
                  end if
               end if
            end do
         end if
      end do
      return
      end
C
C Calculate gridded coordinates and the phasor needed for
C phase rotation. 
C
      subroutine smosft (uvw, dphase, freq, c, scale, offset, 
     $     sampling, pos, loc, off, phasor)
      implicit none
      integer loc(2), off(2), sampling
      double precision uvw(3), freq, c, scale(2), offset(2)
      real pos(2)
      double precision dphase, phase
      complex phasor
      integer idim
      double precision pi
      data pi/3.14159265358979323846/

      if(sampling.gt.1) then
         do idim=1,2
            pos(idim)=scale(idim)*uvw(idim)*freq/c+
     $           (offset(idim)+1.0)
            loc(idim)=nint(pos(idim))
            off(idim)=nint((loc(idim)-pos(idim))*sampling)
         end do
      else
         do idim=1,2
            pos(idim)=scale(idim)*uvw(idim)*freq/c+
     $           (offset(idim)+1.0)
            loc(idim)=nint(pos(idim))
            off(idim)=0
         end do
      end if

      phase=-2.0D0*pi*dphase*freq/c
      phasor=cmplx(cos(phase), sin(phase))
      return 
      end
      logical function omosft (nx, ny, loc, support)
      implicit none
      integer nx, ny, nw, loc(2), support
      omosft=(loc(1)-support.ge.1).and.(loc(1)+support.le.nx).and.
     $        (loc(2)-support.ge.1).and.(loc(2)+support.le.ny)
      return
      end
