//# SpectralEstimate.cc: Get an initial estimate for spectral lines
//# Copyright (C) 2001
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
//# $Id$

//# Includes
#include <trial/Wnbt/SpectralEstimate.h>
#include <aips/Arrays/Vector.h>
#include <aips/Mathematics/Constants.h>
#include <trial/Wnbt/SpectralElement.h>

//# Constructors
SpectralEstimate::SpectralEstimate() {}

SpectralEstimate::SpectralEstimate(const Double ampl,
				   const Double center, const Double sigma) {}

SpectralEstimate::SpectralEstimate(const SpectralEstimate &other) {}

SpectralEstimate::~SpectralEstimate() {};

SpectralEstimate &SpectralEstimate::operator=(const SpectralEstimate &other) {
  if (this != &other) {
  };
  return *this;
}

#define MAXPAR          200                     /* max. number of parameters */
#define NINT(a)         (Int(a+0.5))                 /* 'returns' nearest integer */
#define MAX(a1,a2)      (a1 > a2 ? a1 : a2 )    /* 'returns' maximum */
#define MIN(a1,a2)      (a1 < a2 ? a1 : a2 )    /* 'returns' minimum */
SpectralElement pars[MAXPAR]; ///

///uInt SpectralEstimate::estimate(const Vector<Float> &prof) {
uInt SpectralEstimate::estimate(Double * prof) {
  Double   *work ;              /* work array */
  Int     *n ;                 /* number of points */
  Double   *p ;                 /* parameter estimates */
  Int     *np ;                /* number of parameters */
  Double   *rms ;               /* rms noise level */
  Double   *cutoff ;            /* cutoff level */
  Double   *minsig ;            /* minimum width of gaussians */
  Int     *q ;                 /* smoothing parameter */
  Int         iwhi, iwlo;                     /* window limits */
  Int         l, m;                           /* loop counters */
  Int         r = 0;                          /* return value */
  Double        BLANK = 0.0;            /* BLANK value */
  SpectralElement pars[MAXPAR]; ///

  ///   if (BLANK == 0.0) setdblank_c( &BLANK );     /* get blank */

  ///#ifdef  WITHWINDOW
  if (!window( prof, *n, *cutoff, *rms, &iwlo, &iwhi )) {
    return( r );                              /* no signal in profile */
  };
  iwhi = MIN( iwhi + (*q) , (*n) - 1 );        /* extend window with ... */
  iwlo = MAX( iwlo - (*q) , 0 );               /* smoothing parameter */
  ///#endif
  iwhi = *n - 1;
  iwlo = 0;   
  findc2( prof, work, *n, iwlo, iwhi, *q );       /* get second derivative */
  r = findga( prof, work, *n, iwlo, iwhi, *cutoff, *minsig );
  if (r > MAXPAR) r = MAXPAR;                  /* this  should neven happen */
  ///  if (r > 1) qsort( pars, r, sizeof( par_struct ), compar );
  for (l = 0, m = 0; m < (*np); l++) {
    if (l < r) {                              /* still something in list */
      p[m++] = pars[l].getAmpl();                    /* the amplitude */
      p[m++] = pars[l].getCenter();                    /* the centre */
      p[m++] = pars[l].getSigma();                    /* the width */
    } else {                                  /* list is exhausted */
      p[m++] = BLANK;                        /* set to blank */
      p[m++] = BLANK;                        /* set to blank */
      p[m++] = BLANK;                        /* set to blank */
    };
  };
  return( r );                                 /* return number of gaussians */
}

Int SpectralEstimate::compar(const SpectralElement &p1,
			     const SpectralElement &p2 ) {
   if (p1.getAmpl() > p2.getAmpl()) {           /* one > two */
      return( -1 );
   } else if (p1.getAmpl() < p2.getAmpl()) {                  /* one < two */
      return( 1 );
   } else {                                     /* one == two */
      return( 0 );
   };
}

///const SpectralElement &SpectralEstimate::element(uInt which) const {
const SpectralElement SpectralEstimate::element(uInt which) const {
  return SpectralElement(); ///
}

Int    SpectralEstimate::window( Double  y[] ,           /* the profile */
                        Int    n ,             /* length of profile */
                        Double  cutoff ,        /* the critical level */
                        Double  rms ,           /* nois level in profile */
                        Int    *wstart ,       /* begin of signal region */
                        Int    *wend )         /* end of signal region */
{
   Int         i;                              /* loop counter */
   Int         imax = 0;                       /* position of maximum */
   Int         nw = 0;                         /* width of window */
   Double       flux = 0.0;                     /* total flux in profile */
   Double       ymax;                           /* maximum in profile */

   for (ymax = y[0], i = 1; i < n; i++) {       /* loop to get max. and flux */
      Double    yi = y[i];

      if (yi > ymax) { ymax = yi; imax = i; }   /* new maximum */
      flux += yi;                               /* add to total flux */
   }
   if (ymax > cutoff) {                         /* above critical level */
      Int      iwhi, iwlo;                     /* window borders */
      Double    bnew, bold;                     /* new and old base level */
      Double     cnew, cold;                     /* new and old centre */
      Double    width = -1.0;                   /* window width */

      cnew = imax;                              /* start centre */
      bnew = flux;                              /* start base level */
      do {                                      /* loop */
         Double c, s;

         width += 1.0;                          /* increase width of window */
         cold = cnew;                           /* save current centre */
         bold = bnew;                           /* save current base */
         iwlo = NINT( cold - width );           /* start of window */
         iwhi = NINT( cold + width );           /* end of window */
         iwlo = MAX( 0, iwlo );                 /* start of window */
         iwhi = MIN( n - 1, iwhi );             /* end of window */
         s = c = 0.0;                           /* reset */
         for (i = iwlo; i <= iwhi; i++) {       /* loop over new window */
            Double      yi = y[i];

            s += yi;                            /* flux in window */
            c += yi * i;                        /* first moment in window */
         }
         bnew = flux - s;                       /* new base */
         nw = n - iwhi + iwlo - 1;              /* window width */
         if (s != 0.0) {                        /* we can devide */
            cnew = c / s;                       /* normalized first moment */
            if (cnew < 0 || cnew > (n - 1)) cnew = cold;
         }
      } while (fabs( bnew - bold ) > rms && nw );
      (*wstart) = iwlo;                         /* start of window */
      (*wend) = iwhi;                           /* end of window */
   }
   return( nw );                                /* return width of window */
}

void SpectralEstimate::findc2( Double  y[] ,           /* the profile */
			       Double  work[] ,        /* the derivative */
			       Int    n ,             /* length of profile */
			       Int    iwlo ,          /* start of window */
			       Int    iwhi ,          /* end of window */
			       Int    q )             /* smoothing parameter */
{
  Int          i, j;                           /* loop counters */
  static Int  oldq = 1;                       /* save this q */
  static Double        a = 3.0, b = 0.6666667;         /* save a and b */
  
  if (oldq != q) {                             /* re-calculate a and b */
    a = 90.0 / (Double) ( q * ( q + 1 ) * ( 4 * q * q - 1 ) * ( 2 * q + 3 ) );
    b = (Double) ( q * ( q + 1 ) ) / 3.0;
    oldq = q;
  };
  for (i = iwlo; i <= iwhi; i++) {             /* loop over window */
    Double    m0 = 0.0;                       /* reset zeroeth moment */
    Double     m2 = 0.0;                      /* reset second moment */
    
    for (j = -q; j <= q; j++) {               /* width is 2 * q + 1 */
      Int   k = i + j;
      
      if (k >= 0 && k < n) {                 /* inside range */
	Double      yi = y[k];              /* value from profile */
	
	m0 += yi;                           /* add to zeroeth moment */
	m2 += ( yi * (Double) ( j * j ) );  /* add to second moment */
      };
    };
    work[i] = a * ( m2 - b * m0 );            /* this is the derivative */
  };
}

Int SpectralEstimate::findga( Double  y[] ,           /* the profile */
	    Double  work[] ,        /* the second derivative */
	    Int    n ,             /* length of profile */
	    Int    iwlo ,          /* start of window */
	    Int    iwhi ,          /* end of window */
	    Double  cutoff ,        /* critical level */
	    Double   sigmin )       /* minimum value for sigma */
{
  Int i;                                      /* loop counter */
  Int iclo, ichi;                             /* window on gaussian */
  Int nmax = 0;                               /* peak counter */
  Int r = 0;                                  /* return value */

  iclo = iwlo;                                 /* reset */
  i = iwlo - 1;                                /* reset */
  while (i++ < iwhi) {                         /* loop over window */
    if (work[i] > 0.0) {                      /* we should check this */
      if (i > iwlo && i < iwhi) {            /* not at edge of  window */
	if (work[i-1] < work[i] && work[i+1] < work[i]) {
	  nmax += 1;                       /* peak in 2nd derivative */
	};
      } else if (i == iwlo && work[i+1] < work[i]) {
	nmax += 1;                          /* peak at start of window */
      } else if (i == iwhi && work[i-1] < work[i]) {
	nmax += 1;                          /* peak at end of window */
      };
    };
    switch( nmax ) {                          /* how many peaks ? */
    case 1: {                              /* search for next peak */
      break;
    }
    case 2: {                              /* we have a gaussian */
      Int        ic;                     /* loop counter */
      Double      a, b;
      Double       det;                   /* determinant */
      Double      m0m, m0, m1, m2;        /* some moments */
      
      ichi = i;                           /* end of gauss window */
      b = work[iclo];                     /* now  we use the Schwarz */
      a = (work[ichi] - b) / (Double) (ichi - iclo);
      m0m = m0 = m1 = m2 = 0.0;
      for (ic = iclo; ic <= ichi; ic++) { /* loop over gaussian */
	Double   wi;
	
	m0m += MIN( work[ic], 0.0 );
	wi = work[ic] - a * (Double) (ic - iclo) - b;
	m0 += wi;
	m1 += wi * (Double) ic;
	m2 += wi * (Double) (ic*ic);
      };
      det = m2 * m0 - m1 * m1;            /* determinant */
      if (det > 0.0 && fabs( m0m ) >  FLT_EPSILON) {
	Int     im, is;
	Double   pg, sg;
	Double   xm = m1 / m0;
	Double   yh, yl, ym;
	
	sg = 1.69 * sqrt( det ) / fabs( m0 );
	if (sg > sigmin) {               /* above critical width */
	  is = NINT( 1.73 * sg );
	  im = NINT( xm );
	  if ((im - is) < 0) yl = 0.0; else yl = y[im-is];
	  ym = y[im];
	  if ((im + is) > (n-1)) yh = 0.0; else yh = y[im+is];
	  pg = (ym-0.5*(yh+yl))/(1.0-exp(-0.5*((Double)(is*is))/sg/sg));
	  pg = MIN( pg, ym );
	  if (pg > cutoff) {            /* above critical level */
	    if (r < MAXPAR) {          /* add to list */
	      pars[r].setAmpl(pg);         /* amplitude */
	      pars[r].setCenter(sg);         /* width */
	      pars[r].setSigma(xm);         /* centre */
	    };
	    r += 1;                    /* increase # gaussians */
	  };
	};
      };
      iclo = ichi;                        /* next gauss starts here */
      nmax -= 1;                          /* one peak less */
      break;
    }
    default: {                             /* no gauss */
      iclo = i + 1;                       /* next gauss could start here */
      break;
    }
    };
  };
  return( r );
}
