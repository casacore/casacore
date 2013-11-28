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
C Grid a number of visibility records: single dish gridding
C but with complex images
C
      subroutine ggridsd (xy, values, nvispol, nvischan,
     $   dowt, flag, rflag, weight, nrow, irow,
     $   grid, wgrid, nx, ny, npol, nchan, 
     $   support, sampling, convFunc, chanmap, polmap, sumwt)

      implicit none
      integer nx, ny, npol, nchan, nvispol, nvischan, nrow
      complex values(nvispol, nvischan, nrow)
      complex grid(nx, ny, npol, nchan)
      real wgrid(nx, ny, npol, nchan)
      double precision xy(2,nrow)
      integer flag(nvispol, nvischan, nrow)
      integer rflag(nrow)
      real weight(nvischan, nrow)
      double precision sumwt(npol, nchan)
      integer irow
      integer support, sampling
      integer chanmap(nvischan), polmap(nvispol)
      integer dowt

      complex nvalue

      real convFunc(*)
      real norm
      real wt, wtx, wty

      logical ogridsd

      real pos(2), rloc(2)
      integer loc(2), off(2)
      integer rbeg, rend
      integer irad((2*support+1)**2)
      integer ix, iy, ipol, ichan
      integer apol, achan
      integer ir
      integer xloc(2*support+1), yloc(2*support+1)
      integer ax, ay

      if(irow.ge.0) then
         rbeg=irow+1
         rend=irow+1
      else 
         rbeg=1
         rend=nrow
      end if

      do irow=rbeg, rend
         if(rflag(irow).eq.0) then
         call sgridsd(xy(1,irow), sampling, pos, loc, off)
C          if (ogridsd(nx, ny, loc, support)) then
         if (ogridsd(nx, ny, loc, 0)) then
            ir=1
            norm=-(support+1)*sampling+off(1)
            rloc(2)=-(support+1)*sampling+off(2)
            do iy=1,2*support+1
               rloc(2)=rloc(2)+sampling
               rloc(1)=norm
               do ix=1,2*support+1
                  rloc(1)=rloc(1)+sampling
                  irad(ir)=sqrt(rloc(1)**2+rloc(2)**2)+1
                  ir=ir+1
               end do
            end do
            xloc(1)=loc(1)-support
            do ix=2,2*support+1
               xloc(ix)=xloc(ix-1)+1
            end do
            yloc(1)=loc(2)-support
            do iy=2,2*support+1
               yloc(iy)=yloc(iy-1)+1
            end do
            do ichan=1, nvischan
               achan=chanmap(ichan)+1
               if((achan.ge.1).and.(achan.le.nchan).and.
     $              (weight(ichan,irow).gt.0.0)) then
                  do ipol=1, nvispol
                     apol=polmap(ipol)+1
                     if((flag(ipol,ichan,irow).ne.1).and.
     $                    (apol.ge.1).and.(apol.le.npol)) then
                        if(dowt.eq.1) then
                           nvalue=cmplx(weight(ichan,irow))
                        else
                           nvalue=weight(ichan,irow)*
     $                          conjg(values(ipol,ichan,irow))
                        end if
                        norm=0.0
                        ir=1
C                        do iy=-support,support
                        do iy=1,2*support+1
                           ay=yloc(iy)
                           if ((ay.ge.1).and.(ay.le.ny)) then
C                           do ix=-support,support
                              do ix=1,2*support+1
                                 ax=xloc(ix)
                                 if ((ax.ge.1).and.(ax.le.nx)) then
                                    ir = (iy-1)*(2*support+1) + ix
                                    wt=convFunc(irad(ir))
                                    grid(ax,ay,apol,achan)=
     $                                   grid(ax,ay,apol,achan)+
     $                                   nvalue*wt
                                    wgrid(ax,ay,apol,achan)=
     $                                   wgrid(ax,ay,apol,achan)+
     $                                   weight(ichan,irow)*wt
                                    norm=norm+wt
                                 end if
C                                  ir=ir+1
                              end do
                           end if
                        end do
                        sumwt(apol,achan)=sumwt(apol,achan)+
     $                       weight(ichan,irow)*norm
                     end if
                  end do
               end if
            end do
         end if
         end if
      end do
      return
      end
C
C Degrid a number of visibility records: single dish gridding
C
      subroutine dgridsd (xy, values, nvispol, nvischan, flag, 
     $     rflag, nrow, irow, grid, nx, ny, npol, nchan, 
     $     support, sampling, convFunc, chanmap, polmap)

      implicit none
      integer nx, ny, npol, nchan, nvispol, nvischan, nrow
      complex values(nvispol, nvischan, nrow)
      complex grid(nx, ny, npol, nchan)
      double precision xy(2, nrow)
      integer flag(nvispol, nvischan, nrow)
      integer rflag(nrow)
      integer irow
      integer support, sampling
      integer chanmap(*), polmap(*)

      complex nvalue

      real convFunc(*)
      real norm

      logical ogridsd

      real pos(2), rloc(2)
      integer loc(2), off(2)
      integer rbeg, rend, irad
      integer ix, iy, ipol, ichan
      integer apol, achan
      real wt, wtx, wty

      integer ax, ay

      if(irow.ge.0) then
         rbeg=irow+1
         rend=irow+1
      else 
         rbeg=1
         rend=nrow
      end if

      do irow=rbeg, rend
         if(rflag(irow).eq.0) then
         call sgridsd(xy(1, irow), sampling, pos, loc, off)
C          if (ogridsd(nx, ny, loc, support)) then
         if (ogridsd(nx, ny, loc, 0)) then
            do ichan=1, nvischan
               achan=chanmap(ichan)+1
               if((achan.ge.1).and.(achan.le.nchan)) then
                  do ipol=1, nvispol
                     apol=polmap(ipol)+1
                     if((flag(ipol,ichan,irow).ne.1).and.
     $                    (apol.ge.1).and.(apol.le.npol)) then
                        nvalue=0.0
                        do iy=-support,support
                           rloc(2)=sampling*iy+off(2)
                           ay=loc(2)+iy
                           if ((ay.ge.1).and.(ay.le.ny)) then
                              do ix=-support,support
                                 ax=loc(1)+ix
                                 if ((ax.ge.1).and.(ax.le.nx)) then
                                    rloc(1)=sampling*ix+off(1)
                                    irad=sqrt(rloc(1)**2+rloc(2)**2)+1
                                    wt=convFunc(irad)
                                    nvalue=nvalue+wt*
     $                                   grid(ax,ay,apol,achan)
                                 end if
                              end do
                           end if
                        end do
                        values(ipol,ichan,irow)=conjg(nvalue)
                     end if
                  end do
               end if
            end do
         end if
         endif
      end do
      return
      end
C
C Calculate gridded coordinates
C
      subroutine sgridsd (xy, sampling, pos, loc, off)
      implicit none
      integer sampling
      integer loc(2), off(2)
      double precision xy(2)
      real pos(2)
      integer idim

      do idim=1,2
         pos(idim)=xy(idim)+1.0
         loc(idim)=nint(pos(idim))
         off(idim)=nint((loc(idim)-pos(idim))*sampling)
      end do
      return 
      end
C
C Is this on the grid?
C
      logical function ogridsd (nx, ny, loc, support)
      implicit none
      integer nx, ny, loc(2), support
      ogridsd=(loc(1)-support.ge.1).and.(loc(1)+support.le.nx).and.
     $     (loc(2)-support.ge.1).and.(loc(2)+support.le.ny)
      return
      end
C
C Grid a number of visibility records: single dish gridding
C but with complex images including additional process for
C min/max clipping 
C
      subroutine ggridsd2 (xy, values, nvispol, nvischan,
     $   dowt, flag, rflag, weight, nrow, irow,
     $   grid, wgrid, 
     $   npoints, gmin, wmin, cmin, gmax, wmax, cmax,
     $   nx, ny, npol, nchan, 
     $   support, sampling, convFunc, chanmap, polmap, sumwt)

      implicit none
      integer nx, ny, npol, nchan, nvispol, nvischan, nrow
      complex values(nvispol, nvischan, nrow)
      complex grid(nx, ny, npol, nchan)
      real wgrid(nx, ny, npol, nchan)
      integer npoints(nx, ny, npol)
      complex gmin(nx, ny, npol, nchan)
      complex gmax(nx, ny, npol, nchan)
      real wmin(nx, ny, npol, nchan)
      real wmax(nx, ny, npol, nchan)
      real cmax(nx, ny, npol, nchan)
      real cmin(nx, ny, npol, nchan)
      double precision xy(2,nrow)
      integer flag(nvispol, nvischan, nrow)
      integer rflag(nrow)
      real weight(nvischan, nrow)
      double precision sumwt(npol, nchan)
      integer irow
      integer support, sampling
      integer chanmap(nvischan), polmap(nvispol)
      integer dowt

      real convFunc(*)
      real norm
      real wt, wtx, wty

      logical ogridsd

      real pos(2), rloc(2)
      integer loc(2), off(2)
      integer rbeg, rend
      integer irad((2*support+1)**2)
      integer ix, iy, ipol, ichan
      integer apol, achan
      integer ir
      integer xloc(2*support+1), yloc(2*support+1)
      integer ax, ay

      if(irow.ge.0) then
         rbeg=irow+1
         rend=irow+1
      else 
         rbeg=1
         rend=nrow
      end if

      do irow=rbeg, rend
         if(rflag(irow).eq.0) then
         call sgridsd(xy(1,irow), sampling, pos, loc, off)
C          if (ogridsd(nx, ny, loc, support)) then
         if (ogridsd(nx, ny, loc, 0)) then
            ir=1
            norm=-(support+1)*sampling+off(1)
            rloc(2)=-(support+1)*sampling+off(2)
            do iy=1,2*support+1
               rloc(2)=rloc(2)+sampling
               rloc(1)=norm
               do ix=1,2*support+1
                  rloc(1)=rloc(1)+sampling
                  irad(ir)=sqrt(rloc(1)**2+rloc(2)**2)+1
                  ir=ir+1
               end do
            end do
            xloc(1)=loc(1)-support
            do ix=2,2*support+1
               xloc(ix)=xloc(ix-1)+1
            end do
            yloc(1)=loc(2)-support
            do iy=2,2*support+1
               yloc(iy)=yloc(iy-1)+1
            end do
            do ichan=1, nvischan
               achan=chanmap(ichan)+1
               if((achan.ge.1).and.(achan.le.nchan).and.
     $              (weight(ichan,irow).gt.0.0)) then
                  do ipol=1, nvispol
                     apol=polmap(ipol)+1
                     if((flag(ipol,ichan,irow).ne.1).and.
     $                    (apol.ge.1).and.(apol.le.npol)) then
                        norm=0.0
                        ir=1
C                        do iy=-support,support
                        do iy=1,2*support+1
                           ay=yloc(iy)
                           if ((ay.ge.1).and.(ay.le.ny)) then
C                            do ix=-support,support
                           do ix=1,2*support+1
                              ax=xloc(ix)
                              if ((ax.ge.1).and.(ax.le.nx)) then
                              ir = (iy-1)*(2*support+1) + ix
                              wt=convFunc(irad(ir))
                              grid(ax,ay,apol,achan)=
     $                             grid(ax,ay,apol,achan)+
     $                             weight(ichan,irow)*wt*
     $                             conjg(values(ipol,ichan,irow))
                              wgrid(ax,ay,apol,achan)=
     $                             wgrid(ax,ay,apol,achan)+
     $                             weight(ichan,irow)*wt
                              norm=norm+wt
C                              ir=ir+1
C-------------------------------------------------------------------
C update variables for clipping
C-------------------------------------------------------------------
                              if (wt .gt. 0.0) then
                                 if (ichan .eq. 1) then
                                    npoints(ax,ay,apol)=
     $                                   npoints(ax,ay,apol)+1
                                 end if
                                 if (real(values(ipol,ichan,irow)) .lt. 
     $                                real(gmin(ax,ay,apol,achan))) then
                                    gmin(ax,ay,apol,achan)=
     $                                   conjg(values(ipol,ichan,irow))
                                    wmin(ax,ay,apol,achan)=
     $                                   weight(ichan,irow)
                                    cmin(ax,ay,apol,achan)=wt
                                 end if
                                 if (real(values(ipol,ichan,irow)) .gt. 
     $                                real(gmax(ax,ay,apol,achan))) then
                                    gmax(ax,ay,apol,achan)=
     $                                   conjg(values(ipol,ichan,irow))
                                    wmax(ax,ay,apol,achan)=
     $                                   weight(ichan,irow)
                                    cmax(ax,ay,apol,achan)=wt
                                 end if
                              end if
C-------------------------------------------------------------------
                              end if
                           end do
                           end if
                        end do
                        sumwt(apol,achan)=sumwt(apol,achan)+
     $                       weight(ichan,irow)*norm
                     end if
                  end do
               end if
            end do
         end if
         end if
      end do
      return
      end
