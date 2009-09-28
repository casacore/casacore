*-----------------------------------------------------------------------
* vvroutines.f :  Collection of fortran routines used by VanVleck class.
*-----------------------------------------------------------------------
*
*     Copyright (C) 2002
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
*
*-----------------------------------------------------------------------
*
*     vvroutines contains a collection of subroutines used by the VanVleck
*     class for the special-case of equi-spaced quantizer input voltage
*     thresholds and equi-spaced output levels for the 3-level x 3-level
*     and 9-level x 9-level sampling.  These are taken from Appendix B.
*     of Fred Schwab's memo on the subject (eventually this will be a
*     GBT memo - when it is, the correct citation should be inserted here).
*
*     The following subroutines are found here:
*     vvr3 : for the 3x3 level Van Vleck curve
*     vv3auto : for the 3x3 auto-correlation case (x==y)
*     vv3zmean: for the 3x3 zero-mean case (mux=muy=0.0)
*     vv3zauto: for the 3x3 zero-mean, auto correlation case
*     vvr9 : for the 9x9 level Van Vleck curve
*     vv9auto : for the 9x9 auto-correlation case (x==y)
*     vv9zmean: for the 9x9 zero-mean case (mux=muy=0.0)
*     vv9zauto: for the 9x9 zero-mean, auto correlation case
*     BVND : bivariate normal integrals
*     PHID : used by BVND
*-----------------------------------------------------------------------
*     vvr3 : the Van Vleck curve for the 3x3 level case given 
*     mux, muy : the mean input levels
*     v1x, v1y : the first positive input thresholds
*     rho : the observed correlation
*     mux, muy, v1x, v1y are in units of the respective r.m.s.
*     input levels.
*-----------------------------------------------------------------------
      double precision function vvr3(mux,muy,v1x,v1y,rho)
      double precision mux,muy,v1x,v1y,rho,L,h,k,r,bvnd,rt2
      L(h,k,r)=bvnd(h,k,r)
      rt2=sqrt(2d0)
      vvr3=.5d0*(erf((-mux+v1x)/rt2)-erf((mux+v1x)/rt2)+
     & erf((-muy+v1y)/rt2)-erf((muy+v1y)/rt2))+
     & L(-mux-v1x,-muy-v1y,rho)+L(-mux-v1x,-muy+v1y,rho)+
     & L(-mux+v1x,-muy-v1y,rho)+L(-mux+v1x,-muy+v1y,rho)-1d0
      return
      end
*-----------------------------------------------------------------------
*     vvr3auto : the Van Vleck curve for the 3x3 level case given 
*     mux : the mean input level (x and y are the same here)
*     v1x : the first positive input threshold
*     rho : the observed correlation
*     mux, v1x, are in units of the r.m.s.input levels.
*-----------------------------------------------------------------------
      double precision function vvr3auto(mux,v1x,rho)
      double precision mux,v1x,rho,L,h,k,r,bvnd,rt2
      L(h,k,r)=bvnd(h,k,r)
      rt2=sqrt(2d0)
      vvr3auto=erf((-mux+v1x)/rt2)-erf((mux+v1x)/rt2)+
     &           L(-mux-v1x,-mux-v1x,rho)+
     &           2d0 * L(-mux-v1x,-mux+v1x,rho)+
     &           L(-mux+v1x,-mux+v1x,rho)-
     &      1d0
      return
      end
*-----------------------------------------------------------------------
*     vvr3zmean : the Van Vleck curve for the 3x3 level case given 
*     v1x, v1y : the first positive input thresholds (zero mean)
*     rho : the observed correlation
*     v1x, v1y are in units of the respective r.m.s. input levels.
*-----------------------------------------------------------------------
      double precision function vvr3zmean(v1x,v1y,rho)
      double precision v1x,v1y,rho,L,h,k,r,bvnd
      L(h,k,r)=bvnd(h,k,r)
      vvr3zmean=L(-v1x,-v1y,rho)+L(-v1x,v1y,rho)+
     & L(v1x,-v1y,rho)+L(v1x,v1y,rho)-1d0
      return
      end
*-----------------------------------------------------------------------
*     vvr3zauto : the Van Vleck curve for the 3x3 level case given 
*     v : the first positive input threshold (zero mean, x==y)
*     rho : the observed correlation
*     v is in units of the r.m.s. input levels.
*-----------------------------------------------------------------------
      double precision function vvr3zauto(v,rho)
      double precision v,rho,L,h,k,r,bvnd
      L(h,k,r)=bvnd(h,k,r)
      vvr3zauto=L(-v,-v,rho)+L(-v,v,rho)+
     & L(v,-v,rho)+L(v,v,rho)-1d0
      return
      end
*-----------------------------------------------------------------------
*     vvr9 : the Van Vleck curve for the 9x9 level case given 
*     mux, muy : the mean input levels
*     v1x, v1y : the first positive input thresholds
*     rho : the observed correlation
*     mux, muy, v1x, v1y are in units of the respective r.m.s.
*     input levels.
*-----------------------------------------------------------------------
      double precision function vvr9(mux,muy,v1x,v1y,rho)
      double precision mux,muy,v1x,v1y,rho,L,h,k,r,bvnd,rt2
      L(h,k,r)=bvnd(h,k,r)
      rt2=sqrt(2d0)
      vvr9=-16d0+2d0*(-erf((mux-7*v1x)/rt2)-erf((mux-5*v1x)/rt2)-
     & erf((mux-3*v1x)/rt2)+erf((-mux+v1x)/rt2)-
     & erf((mux+v1x)/rt2)-erf((mux+3*v1x)/rt2)-
     & erf((mux+5*v1x)/rt2)-erf((mux+7*v1x)/rt2)-
     & erf((muy-7*v1y)/rt2)-erf((muy-5*v1y)/rt2)-
     & erf((muy-3*v1y)/rt2)+erf((-muy+v1y)/rt2)-
     & erf((muy+v1y)/rt2)-erf((muy+3*v1y)/rt2)-
     & erf((muy+5*v1y)/rt2)-erf((muy+7*v1y)/rt2))+
     & L(-mux-7*v1x,-muy-7*v1y,rho)+L(-mux-7*v1x,-muy-5*v1y,rho)+
     & L(-mux-7*v1x,-muy-3*v1y,rho)+L(-mux-7*v1x,-muy-v1y,rho)+
     & L(-mux-7*v1x,-muy+v1y,rho)+L(-mux-7*v1x,-muy+3*v1y,rho)+
     & L(-mux-7*v1x,-muy+5*v1y,rho)+L(-mux-7*v1x,-muy+7*v1y,rho)+
     & L(-mux-5*v1x,-muy-7*v1y,rho)+L(-mux-5*v1x,-muy-5*v1y,rho)+
     & L(-mux-5*v1x,-muy-3*v1y,rho)+L(-mux-5*v1x,-muy-v1y,rho)+
     & L(-mux-5*v1x,-muy+v1y,rho)+L(-mux-5*v1x,-muy+3*v1y,rho)+
     & L(-mux-5*v1x,-muy+5*v1y,rho)+L(-mux-5*v1x,-muy+7*v1y,rho)+
     & L(-mux-3*v1x,-muy-7*v1y,rho)+L(-mux-3*v1x,-muy-5*v1y,rho)+
     & L(-mux-3*v1x,-muy-3*v1y,rho)+L(-mux-3*v1x,-muy-v1y,rho)+
     & L(-mux-3*v1x,-muy+v1y,rho)+L(-mux-3*v1x,-muy+3*v1y,rho)+
     & L(-mux-3*v1x,-muy+5*v1y,rho)+L(-mux-3*v1x,-muy+7*v1y,rho)+
     & L(-mux-v1x,-muy-7*v1y,rho)+L(-mux-v1x,-muy-5*v1y,rho)+
     & L(-mux-v1x,-muy-3*v1y,rho)+L(-mux-v1x,-muy-v1y,rho)+
     & L(-mux-v1x,-muy+v1y,rho)+L(-mux-v1x,-muy+3*v1y,rho)+
     & L(-mux-v1x,-muy+5*v1y,rho)+L(-mux-v1x,-muy+7*v1y,rho)+
     & L(-mux+v1x,-muy-7*v1y,rho)+L(-mux+v1x,-muy-5*v1y,rho)+
     & L(-mux+v1x,-muy-3*v1y,rho)+L(-mux+v1x,-muy-v1y,rho)+
     & L(-mux+v1x,-muy+v1y,rho)+L(-mux+v1x,-muy+3*v1y,rho)+
     & L(-mux+v1x,-muy+5*v1y,rho)+L(-mux+v1x,-muy+7*v1y,rho)+
     & L(-mux+3*v1x,-muy-7*v1y,rho)+L(-mux+3*v1x,-muy-5*v1y,rho)+
     & L(-mux+3*v1x,-muy-3*v1y,rho)+L(-mux+3*v1x,-muy-v1y,rho)+
     & L(-mux+3*v1x,-muy+v1y,rho)+L(-mux+3*v1x,-muy+3*v1y,rho)+
     & L(-mux+3*v1x,-muy+5*v1y,rho)+L(-mux+3*v1x,-muy+7*v1y,rho)+
     & L(-mux+5*v1x,-muy-7*v1y,rho)+L(-mux+5*v1x,-muy-5*v1y,rho)+
     & L(-mux+5*v1x,-muy-3*v1y,rho)+L(-mux+5*v1x,-muy-v1y,rho)+
     & L(-mux+5*v1x,-muy+v1y,rho)+L(-mux+5*v1x,-muy+3*v1y,rho)+
     & L(-mux+5*v1x,-muy+5*v1y,rho)+L(-mux+5*v1x,-muy+7*v1y,rho)+
     & L(-mux+7*v1x,-muy-7*v1y,rho)+L(-mux+7*v1x,-muy-5*v1y,rho)+
     & L(-mux+7*v1x,-muy-3*v1y,rho)+L(-mux+7*v1x,-muy-v1y,rho)+
     & L(-mux+7*v1x,-muy+v1y,rho)+L(-mux+7*v1x,-muy+3*v1y,rho)+
     & L(-mux+7*v1x,-muy+5*v1y,rho)+L(-mux+7*v1x,-muy+7*v1y,rho)
      return
      end
*-----------------------------------------------------------------------
*     vvr9auto : the Van Vleck curve for the 9x9 level case given 
*     mux : the mean input levels (x and y are the same here)
*     v1x : the first positive input threshold
*     rho : the observed correlation
*     mux, v1x, are in units of the respective r.m.s.
*     input levels.
*-----------------------------------------------------------------------
      double precision function vvr9auto(mux,v1x,rho)
      double precision mux,v1x,rho,L,h,k,r,bvnd,rt2
      L(h,k,r)=bvnd(h,k,r)
      rt2=sqrt(2d0)
      vvr9auto=-16d0+4d0*(-erf((mux-7*v1x)/rt2)-erf((mux-5*v1x)/rt2)
     &  -erf((mux-3*v1x)/rt2)+erf((-mux+v1x)/rt2)-erf((mux+v1x)/rt2)
     &  -erf((mux+3*v1x)/rt2)-erf((mux+5*v1x)/rt2)
     &  -erf((mux+7*v1x)/rt2))+
     & L(-mux-7*v1x,-mux-7*v1x,rho)+L(-mux-5*v1x,-mux-5*v1x,rho)+
     & L(-mux-3*v1x,-mux-3*v1x,rho)+L(-mux-v1x,-mux-v1x,rho)+
     & L(-mux+v1x,-mux+v1x,rho)+L(-mux+3*v1x,-mux+3*v1x,rho)+
     & L(-mux+5*v1x,-mux+5*v1x,rho)+L(-mux+7*v1x,-mux+7*v1x,rho)+
     & 2d0*(L(-mux-7*v1x,-mux-5*v1x,rho)+L(-mux-7*v1x,-mux-3*v1x,rho)+
     & L(-mux-7*v1x,-mux-v1x,rho)+L(-mux-7*v1x,-mux+v1x,rho)+
     & L(-mux-7*v1x,-mux+3*v1x,rho)+L(-mux-7*v1x,-mux+5*v1x,rho)+
     & L(-mux-7*v1x,-mux+7*v1x,rho)+L(-mux-5*v1x,-mux-3*v1x,rho)+
     & L(-mux-5*v1x,-mux-v1x,rho)+L(-mux-5*v1x,-mux+v1x,rho)+
     & L(-mux-5*v1x,-mux+3*v1x,rho)+L(-mux-5*v1x,-mux+5*v1x,rho)+
     & L(-mux-5*v1x,-mux+7*v1x,rho)+L(-mux-3*v1x,-mux-v1x,rho)+
     & L(-mux-3*v1x,-mux+v1x,rho)+L(-mux-3*v1x,-mux+3*v1x,rho)+
     & L(-mux-3*v1x,-mux+5*v1x,rho)+L(-mux-3*v1x,-mux+7*v1x,rho)+
     & L(-mux-v1x,-mux+v1x,rho)+L(-mux-v1x,-mux+3*v1x,rho)+
     & L(-mux-v1x,-mux+5*v1x,rho)+L(-mux-v1x,-mux+7*v1x,rho)+
     & L(-mux+v1x,-mux+3*v1x,rho)+L(-mux+v1x,-mux+5*v1x,rho)+
     & L(-mux+v1x,-mux+7*v1x,rho)+L(-mux+3*v1x,-mux+5*v1x,rho)+
     & L(-mux+3*v1x,-mux+7*v1x,rho)+L(-mux+7*v1x,-mux+5*v1x,rho))

      return
      end
*-----------------------------------------------------------------------
*     vvr9zmean : the Van Vleck curve for the 9x9 level case given 
*     v1x, v1y : the first positive input thresholds (zero mean)
*     rho : the observed correlation
*     v1x, v1y are in units of the respective r.m.s.
*     input levels.
*-----------------------------------------------------------------------
      double precision function vvr9zmean(v1x,v1y,rho)
      double precision v1x,v1y,rho,L,h,k,r,bvnd,rt2
      L(h,k,r)=bvnd(h,k,r)
      rt2=sqrt(2d0)
      vvr9zmean=-12d0 - erfc(v1x/rt2)/2.0 +
     & L(-7*v1x,-7*v1y,rho)+L(-7*v1x,-5*v1y,rho)+
     & L(-7*v1x,-3*v1y,rho)+L(-7*v1x,-v1y,rho)+
     & L(-7*v1x,v1y,rho)+L(-7*v1x,3*v1y,rho)+
     & L(-7*v1x,5*v1y,rho)+L(-7*v1x,7*v1y,rho)+
     & L(-5*v1x,-7*v1y,rho)+L(-5*v1x,-5*v1y,rho)+
     & L(-5*v1x,-3*v1y,rho)+L(-5*v1x,-v1y,rho)+
     & L(-5*v1x,v1y,rho)+L(-5*v1x,3*v1y,rho)+
     & L(-5*v1x,5*v1y,rho)+L(-5*v1x,7*v1y,rho)+
     & L(-3*v1x,-7*v1y,rho)+L(-3*v1x,-5*v1y,rho)+
     & L(-3*v1x,-3*v1y,rho)+L(-3*v1x,-v1y,rho)+
     & L(-3*v1x,v1y,rho)+L(-3*v1x,3*v1y,rho)+
     & L(-3*v1x,5*v1y,rho)+L(-3*v1x,7*v1y,rho)-
     & L(v1x,-7*v1y,-rho)+L(v1x,-7*v1y,rho)-
     & L(v1x,-5*v1y,-rho)+L(v1x,-5*v1y,rho)-
     & L(v1x,-3*v1y,-rho)+L(v1x,-3*v1y,rho)+
     & L(v1x,-v1y,rho)-L(v1x,v1y,-rho)+
     & 2*L(v1x,v1y,rho)-L(v1x,3*v1y,-rho)+
     & L(v1x,3*v1y,rho)-L(v1x,5*v1y,-rho)+
     & L(v1x,5*v1y,rho)-L(v1x,7*v1y,-rho)+
     & L(v1x,7*v1y,rho)+L(3*v1x,-7*v1y,rho)+
     & L(3*v1x,-5*v1y,rho)+L(3*v1x,-3*v1y,rho)+
     & L(3*v1x,-v1y,rho)+L(3*v1x,v1y,rho)+
     & L(3*v1x,3*v1y,rho)+L(3*v1x,5*v1y,rho)+
     & L(3*v1x,7*v1y,rho)+L(5*v1x,-7*v1y,rho)+
     & L(5*v1x,-5*v1y,rho)+L(5*v1x,-3*v1y,rho)+
     & L(5*v1x,-v1y,rho)+L(5*v1x,v1y,rho)+
     & L(5*v1x,3*v1y,rho)+L(5*v1x,5*v1y,rho)+
     & L(5*v1x,7*v1y,rho)+L(7*v1x,-7*v1y,rho)+
     & L(7*v1x,-5*v1y,rho)+L(7*v1x,-3*v1y,rho)+
     & L(7*v1x,-v1y,rho)+L(7*v1x,v1y,rho)+
     & L(7*v1x,3*v1y,rho)+L(7*v1x,5*v1y,rho)+
     & L(7*v1x,7*v1y,rho)

      return
      end
*-----------------------------------------------------------------------
*     vvr9zauto : the Van Vleck curve for the 9x9 level case given 
*     v : the first positive input threshold (x==y, zero mean)
*     rho : the observed correlation
*     v is in units of the r.m.s.
*     input levels.
*-----------------------------------------------------------------------
      double precision function vvr9zauto(v,rho)
      double precision v,rho,L,h,k,r,bvnd,rt2
      L(h,k,r)=bvnd(h,k,r)
      rt2=sqrt(2d0)
      vvr9zauto=-15d0+erfc((3*v)/rt2)+erfc((5*v)/rt2)+erfc((7*v)/rt2) +
     &     L(-7*v,-7*v,rho)+L(-5*v,-5*v,rho)+L(-3*v,-3*v,rho)+
     &     L(3*v,3*v,rho)+L(5*v,5*v,rho)+L(7*v,7*v,rho)+
     &     2*(L(-7*v,-5*v,rho)+L(-7*v,-3*v,rho)+
     &     L(-7*v,-v,rho)+L(-7*v,v,rho)+L(-7*v,3*v,rho)+
     &     L(-7*v,5*v,rho)+L(-7*v,7*v,rho)+
     &     L(-5*v,-3*v,rho)+L(-5*v,-v,rho)+L(-5*v,v,rho)+
     &     L(-5*v,3*v,rho)+L(-5*v,5*v,rho)+L(-5*v,7*v,rho)+
     &     L(-3*v,-1*v,rho)+L(-3*v,v,rho)+
     &     L(-3*v,3*v,rho)+L(-3*v,5*v,rho)+L(-3*v,7*v,rho)-
     &     L(v,v,-rho)+L(v,v,rho)-L(v,3*v,-rho)+
     &     L(v,3*v,rho)-L(v,5*v,-rho)+L(v,5*v,rho)-
     &     L(v,7*v,-rho)+L(v,7*v,rho)+
     &     L(3*v,5*v,rho)+L(3*v,7*v,rho)+
     &     L(5*v,7*v,rho))

      return
      end
*-----------------------------------------------------------------------
*   BVND was written by Alan Genz of the Department of Mathematics
*   at Washington State University.  See the Schwab memo for details.
*-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION BVND( DH, DK, R )      
*
*     A function for computing bivariate normal probabilities.
*
*       Alan Genz
*       Department of Mathematics
*       Washington State University
*       Pullman, WA 99164-3113
*       Email : alangenz<place an "at" symbol here>wsu.edu
*
*    This function is based on the method described by 
*        Drezner, Z and G.O. Wesolowsky, (1989),
*        On the computation of the bivariate normal inegral,
*        Journal of Statist. Comput. Simul. 35, pp. 101-107,
*    with major modifications for double precision, and for |R| close to 1.
*
* BVND - calculate the probability that X is larger than DH and Y is
*       larger than DK.
*
* Parameters
*
*   DH  DOUBLE PRECISION, integration limit
*   DK  DOUBLE PRECISION, integration limit
*   R   DOUBLE PRECISION, correlation coefficient
*
      DOUBLE PRECISION DH, DK, R, ZERO, TWOPI 
      INTEGER I, IS, LG, NG
      PARAMETER ( ZERO = 0, TWOPI = 6.283185307179586D0 ) 
      DOUBLE PRECISION X(10,3), W(10,3), AS, A, B, C, D, RS, XS, BVN 
      DOUBLE PRECISION PHID, SN, ASR, H, K, BS, HS, HK
*     Gauss Legendre Points and Weights, N =  6
      DATA ( W(I,1), X(I,1), I = 1,3) /
     &  0.1713244923791705D+00,-0.9324695142031522D+00,
     &  0.3607615730481384D+00,-0.6612093864662647D+00,
     &  0.4679139345726904D+00,-0.2386191860831970D+00/
*     Gauss Legendre Points and Weights, N = 12
      DATA ( W(I,2), X(I,2), I = 1,6) /
     &  0.4717533638651177D-01,-0.9815606342467191D+00,
     &  0.1069393259953183D+00,-0.9041172563704750D+00,
     &  0.1600783285433464D+00,-0.7699026741943050D+00,
     &  0.2031674267230659D+00,-0.5873179542866171D+00,
     &  0.2334925365383547D+00,-0.3678314989981802D+00,
     &  0.2491470458134029D+00,-0.1252334085114692D+00/
*     Gauss Legendre Points and Weights, N = 20
      DATA ( W(I,3), X(I,3), I = 1, 10 ) /
     &  0.1761400713915212D-01,-0.9931285991850949D+00,
     &  0.4060142980038694D-01,-0.9639719272779138D+00,
     &  0.6267204833410906D-01,-0.9122344282513259D+00,
     &  0.8327674157670475D-01,-0.8391169718222188D+00,
     &  0.1019301198172404D+00,-0.7463319064601508D+00,
     &  0.1181945319615184D+00,-0.6360536807265150D+00,
     &  0.1316886384491766D+00,-0.5108670019508271D+00,
     &  0.1420961093183821D+00,-0.3737060887154196D+00,
     &  0.1491729864726037D+00,-0.2277858511416451D+00,
     &  0.1527533871307259D+00,-0.7652652113349733D-01/
      SAVE X, W
      IF ( ABS(R) .LT. 0.3D0 ) THEN
         NG = 1
         LG = 3
      ELSE IF ( ABS(R) .LT. 0.75D0 ) THEN
         NG = 2
         LG = 6
      ELSE 
         NG = 3
         LG = 10
      ENDIF
      H = DH
      K = DK 
      HK = H*K
      BVN = 0
      IF ( ABS(R) .LT. 0.925D0 ) THEN
         HS = ( H*H + K*K )/2
         ASR = ASIN(R)
         DO I = 1, LG
            DO IS = -1, 1, 2
               SN = SIN( ASR*(  IS*X(I,NG) + 1 )/2 )
               BVN = BVN + W(I,NG)*EXP( ( SN*HK - HS )/( 1 - SN*SN ) )
            END DO
         END DO
         BVN = BVN*ASR/( 2*TWOPI ) + PHID(-H)*PHID(-K) 
      ELSE
         IF ( R .LT. 0 ) THEN
            K = -K
            HK = -HK
         ENDIF
         IF ( ABS(R) .LT. 1 ) THEN
            AS = ( 1 - R )*( 1 + R )
            A = SQRT(AS)
            BS = ( H - K )**2
            C = ( 4 - HK )/8 
            D = ( 12 - HK )/16
            ASR = -( BS/AS + HK )/2
            IF ( ASR .GT. -100 ) BVN = A*EXP(ASR)
     &             *( 1 - C*( BS - AS )*( 1 - D*BS/5 )/3 + C*D*AS*AS/5 )
            IF ( -HK .LT. 100 ) THEN
               B = SQRT(BS)
               BVN = BVN - EXP( -HK/2 )*SQRT(TWOPI)*PHID(-B/A)*B
     &                    *( 1 - C*BS*( 1 - D*BS/5 )/3 ) 
            ENDIF
            A = A/2
            DO I = 1, LG
               DO IS = -1, 1, 2
                  XS = ( A*(  IS*X(I,NG) + 1 ) )**2
                  RS = SQRT( 1 - XS )
                  ASR = -( BS/XS + HK )/2
                  IF ( ASR .GT. -100 ) THEN
                     BVN = BVN + A*W(I,NG)*EXP( ASR )
     &                    *( EXP( -HK*( 1 - RS )/( 2*( 1 + RS ) ) )/RS        
     &                    - ( 1 + C*XS*( 1 + D*XS ) ) )
                  END IF
               END DO
            END DO
            BVN = -BVN/TWOPI
         ENDIF
         IF ( R .GT. 0 ) BVN =  BVN + PHID( -MAX( H, K ) )
         IF ( R .LT. 0 ) BVN = -BVN + MAX( ZERO, PHID(-H) - PHID(-K) )
      ENDIF
      BVND = BVN
      END
*-----------------------------------------------------------------------
*   PHID is used by BVND
*-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION PHID(Z)
*     
*     Normal distribution probabilities accurate to 1.e-15.
*     Z = no. of standard deviations from the mean.
*     
*     Based upon algorithm 5666 for the error function, from:
*     Hart, J.F. et al, 'Computer Approximations', Wiley 1968
*     
*     Programmer: Alan Miller
*     
*     Latest revision - 30 March 1986
*     
      DOUBLE PRECISION P0, P1, P2, P3, P4, P5, P6, 
     &     Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7,
     &     Z, P, EXPNTL, CUTOFF, ROOTPI, ZABS
      PARAMETER(
     &     P0 = 220.20 68679 12376 1D0,
     &     P1 = 221.21 35961 69931 1D0, 
     &     P2 = 112.07 92914 97870 9D0,
     &     P3 = 33.912 86607 83830 0D0,
     &     P4 = 6.3739 62203 53165 0D0,
     &     P5 = .70038 30644 43688 1D0, 
     &     P6 = .035262 49659 98910 9D0 )
      PARAMETER(
     &     Q0 = 440.41 37358 24752 2D0,
     &     Q1 = 793.82 65125 19948 4D0, 
     &     Q2 = 637.33 36333 78831 1D0,
     &     Q3 = 296.56 42487 79673 7D0, 
     &     Q4 = 86.780 73220 29460 8D0,
     &     Q5 = 16.064 17757 92069 5D0, 
     &     Q6 = 1.7556 67163 18264 2D0,
     &     Q7 = .088388 34764 83184 4D0 )
      PARAMETER( ROOTPI = 2.5066 28274 63100 1D0 )
      PARAMETER( CUTOFF = 7.0710 67811 86547 5D0 )
*     
      ZABS = ABS(Z)
*     
*     |Z| &gt; 37
*     
      IF ( ZABS .GT. 37 ) THEN
         P = 0
      ELSE
*     
*     |Z| &lt;= 37
*     
         EXPNTL = EXP( -ZABS**2/2 )
*     
*     |Z| &lt; CUTOFF = 10/SQRT(2)
*     
         IF ( ZABS .LT. CUTOFF ) THEN
            P = EXPNTL*( ( ( ( ( ( P6*ZABS + P5 )*ZABS + P4 )*ZABS
     &            + P3 )*ZABS + P2 )*ZABS + P1 )*ZABS + P0 )
     &           /( ( ( ( ( ( ( Q7*ZABS + Q6 )*ZABS + Q5 )*ZABS 
     &            + Q4 )*ZABS + Q3 )*ZABS + Q2 )*ZABS + Q1 )*ZABS + Q0 )
*     
*     |Z| &gt;= CUTOFF.
*     
         ELSE
            P = EXPNTL/( ZABS + 1/( ZABS + 2/( ZABS + 3/( ZABS 
     &           + 4/( ZABS + 0.65D0 ) ) ) ) )/ROOTPI
         END IF
      END IF
      IF ( Z .GT. 0 ) P = 1 - P
      PHID = P
      END
