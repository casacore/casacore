/*  pack								*/
/* & pjt								*/
/* : low-level-i/o							*/
/* +									*/
/*									*/
/*		This converts data between disk and internal		*/
/*		format. Disk format is IEEE reals and 16 or 32		*/
/*		bit integers (most significant byte first).		*/
/*									*/
/*		This assumes that these are the local machine format	*/
/*		(float == IEEE real, int == 32 bit integer,		*/
/*		short int == 16 bit integer).				*/
/*									*/
/*		packx_c, unpackx_c, pack32_c and unpack32_c are		*/
/*		implemented as macros (calling bcopy) in the		*/
/*		system dependent include file.				*/
/*--									*/
/*									*/
/*  History:								*/
/*    rjs  Dark-ages Original version.					*/
/*    bs   ?????89   Improved efficiency using "register" declarations.	*/
/*    rjs   1nov89   Incoporated Brian's changes.			*/
/*    mjs  28feb91   Merge Sun and Cray versions.			*/
/*    mjs  18mar91   Added convex definition.                           */
/*    mjs  19feb93   Added mips definition.                             */
/*    pjt  25jan95   linux kludge to include packALPHA.c                */
/*    pjt  14jun01   packALPHA.c now included in this source code       */
/*                   and using the standard WORDS_BIGENDIAN macro       */
/*    pjt  21jun02   MIR4 prototyping                                   */
/************************************************************************/

#if defined(HAVE_CONFIG_H) && HAVE_CONFIG_H
#include "config.h"
#endif

#include "sysdep.h"
#include "miriad.h"

#if defined(WORDS_BIGENDIAN)

static int words_bigendian = 1; /* never used actually, but handy symbol to find via nm(1) */

void	pack16_c(register int *from,char *to,int n)
{
	register short int	*tto;
	register int		i;

	tto = (short int *)to;
	for (i=0; i < n; i++)	*tto++ = *from++;
}
void	unpack16_c(char *from,register int *to,int n)
{
	register short int	*ffrom;
	register int		i;

	ffrom = (short int *)from;
	for (i=0; i < n; i++)	*to++ = *ffrom++;
}

void	pack64_c(register int *from,char *to,int n)
{
	register short int	*tto;
	register int		i;

	tto = (short int *)to;
	for (i=0; i < n; i++)	*tto++ = *from++;
}
void	unpack64_c(char *from,register int *to,int n)
{
	register short int	*ffrom;
	register int		i;

	ffrom = (short int *)from;
	for (i=0; i < n; i++)	*to++ = *ffrom++;
}

#endif


#ifndef WORDS_BIGENDIAN 
#ifndef unicos
static int words_littleendian = 1; /* never used actually, but handy symbol to find via nm(1) */
/************************************************************************/
/*									*/
/*  The pack routines -- these convert between the host format and	*/
/*  the disk format. Disk format is IEEE 32 and 64 bit reals, and 2's	*/
/*  complement integers. Byte order is the FITS byte order (most	*/
/*  significant bytes first).						*/
/*									*/
/*  This version is for a machine which uses IEEE internally, but which	*/
/*  uses least significant bytes first (little endian), e.g. PCs and	*/
/*  Alphas.								*/
/*									*/
/*  History:								*/
/*    rjs  21nov94 Original version.					*/
/************************************************************************/
void pack16_c(int *in,char *out,int n)
/*
  Pack an integer array into 16 bit integers.
------------------------------------------------------------------------*/
{
  int i;
  char *s;

  s = (char *)in;
  for(i=0; i < n; i++){
    *out++ = *(s+1);
    *out++ = *s;
    s += sizeof(int);
  }
}
/************************************************************************/
void unpack16_c(char *in,int *out,int n)
/*
  Unpack an array of 16 bit integers into integers.
------------------------------------------------------------------------*/
{
  int i;
  char *s;

  s = (char *)out;
  for(i=0; i < n; i++){
    *s++ = *(in+1);
    *s++ = *in;
    if(0x80 & *in){
      *s++ = 0xFF;
      *s++ = 0xFF;
    } else {
      *s++ = 0;
      *s++ = 0;
    }
    in += 2;
  }
}
/************************************************************************/
void pack32_c(int *in,char *out,int n)
/*
  Pack an array of integers into 32 bit integers.
------------------------------------------------------------------------*/
{
  int i;
  char *s;

  s = (char *)in;
  for(i = 0; i < n; i++){
    *out++ = *(s+3);
    *out++ = *(s+2);
    *out++ = *(s+1);
    *out++ = *s;
    s += 4;
  }
}
/************************************************************************/
void unpack32_c(char *in,int *out,int n)
/*
  Unpack an array of 32 bit integers into integers.
------------------------------------------------------------------------*/
{
  int i;
  char *s;

  s = (char *)out;
  for(i = 0; i < n; i++){
    *s++ = *(in+3);
    *s++ = *(in+2);
    *s++ = *(in+1);
    *s++ = *in;
    in += 4;
  }
}
/************************************************************************/
void pack64_c(int8 *in,char *out,int n)
/*
  Pack an integer array into 64 bit integers.
------------------------------------------------------------------------*/
{
  int i;
  char *s;

  s = (char *)in;
  for(i=0; i < n; i++){
    *out++ = *(s+7);
    *out++ = *(s+6);
    *out++ = *(s+5);
    *out++ = *(s+4);
    *out++ = *(s+3);
    *out++ = *(s+2);
    *out++ = *(s+1);
    *out++ = *s;
    s += 8;
  }
}
/************************************************************************/
void unpack64_c(char *in,int8 *out,int n)
/*
  Unpack an array of 64 bit integers into integers.
------------------------------------------------------------------------*/
{
  int i;
  char *s;

  s = (char *)out;
  for(i=0; i < n; i++){
    *s++ = *(in+7);
    *s++ = *(in+6);
    *s++ = *(in+5);
    *s++ = *(in+4);
    *s++ = *(in+3);
    *s++ = *(in+2);
    *s++ = *(in+1);
    *s++ = *in;
    in += 8;
  }
}
/************************************************************************/
void packr_c(float *in,char *out,int n)
/*
  Pack an array of reals into IEEE reals -- just do byte reversal.
------------------------------------------------------------------------*/
{
  int i;
  char *s;

  s = (char *)in;
  for(i = 0; i < n; i++){
    *out++ = *(s+3);
    *out++ = *(s+2);
    *out++ = *(s+1);
    *out++ = *s;
    s += 4;
  }
}
/************************************************************************/
void unpackr_c(char *in,float *out,int n)
/*
  Unpack an array of IEEE reals into reals -- just do byte reversal.
------------------------------------------------------------------------*/
{
  int i;
  char *s;

  s = (char *)out;
  for(i = 0; i < n; i++){
    *s++ = *(in+3);
    *s++ = *(in+2);
    *s++ = *(in+1);
    *s++ = *in;
    in += 4;
  }
}
/************************************************************************/
void packd_c(double *in,char *out,int n)
/*
  Pack an array of doubles -- this involves simply performing byte
  reversal.
------------------------------------------------------------------------*/
{
  int i;
  char *s;

  s = (char *)in;
  for(i = 0; i < n; i++){
    *out++ = *(s+7);
    *out++ = *(s+6);
    *out++ = *(s+5);
    *out++ = *(s+4);
    *out++ = *(s+3);
    *out++ = *(s+2);
    *out++ = *(s+1);
    *out++ = *s;
    s += 8;
  }
}
/************************************************************************/
void unpackd_c(char *in,double *out,int n)
/*
  Unpack an array of doubles -- this involves simply performing byte
  reversal.
------------------------------------------------------------------------*/
{
  int i;
  char *s;

  s = (char *)out;
  for(i = 0; i < n; i++){
    *s++ = *(in+7);
    *s++ = *(in+6);
    *s++ = *(in+5);
    *s++ = *(in+4);
    *s++ = *(in+3);
    *s++ = *(in+2);
    *s++ = *(in+1);
    *s++ = *in;
    in += 8;
  }
}
#endif
#endif


#if defined(unicos)
static int words_unicos = 1;
#define TWO15  0x8000
#define TWO16  0x10000
#define TWO31  0x80000000
#define TWO32  0x100000000
#define HILONG 0xFFFFFFFF00000000
#define LOLONG 0x00000000FFFFFFFF
#define WORD0  0x000000000000FFFF
#define WORD1  0x00000000FFFF0000
#define WORD2  0x0000FFFF00000000
#define WORD3  0xFFFF000000000000

/* Masks for IEEE floating format (both hi and lo types). */

#define IEEE_HISIGN     0x8000000000000000
#define IEEE_HIEXPO     0x7F80000000000000
#define IEEE_HIMANT     0x007FFFFF00000000
#define IEEE_LOSIGN     0x0000000080000000
#define IEEE_LOEXPO     0x000000007F800000
#define IEEE_LOMANT     0x00000000007FFFFF
#define IEEE_DMANT      0x000FFFFFFFFFFFF0
#define IEEE_DEXPO      0x7FF0000000000000

/* Masks for Cray floating format. */

#define CRAY_MANT       0x0000FFFFFF000000      /* Including unhidden bit. */
#define CRAY_MANT1      0x00007FFFFF000000      /* No unhidden bit. */
#define CRAY_DMANT      0x0000FFFFFFFFFFFF
#define CRAY_DMANT1     0x00007FFFFFFFFFFF
#define CRAY_EXPO       0x7FFF000000000000
#define SIGN            0x8000000000000000

/* Mask of a pointer to char giving the character offset in a Cray word. */

#define CHAR_OFFSET 0xE000000000000000

/************************************************************************/
void pack16_c(int *in,char *out,int n)
/*
  Pack an integer array into 16 bit integers for unicos
------------------------------------------------------------------------*/
{
  int temp,offset,*outd,in1,in2,in3,in4,i;

  if(n <= 0)return;                             /* Return if nothing to do. */
  temp = (int)out;
  offset = ( temp & CHAR_OFFSET ) >> 62;        /* Get offset of first word. */
  outd = (int *)(temp & ~CHAR_OFFSET);  /* Get address of words. */

/* Handle the first few which are not aligned on a Cray word. */

  switch(offset){
    case 1: *outd = (*outd & ~WORD2) | ((*in++ << 32) & WORD2);
            if(--n == 0)break;
    case 2: *outd = (*outd & ~WORD1) | ((*in++ << 16) & WORD1);
            if(--n == 0)break;
    case 3: *outd = (*outd & ~WORD0) | ((*in++    ) & WORD0);
            outd++;
  }

/* Handle the ones which are aligned on a Cray word. */

  for(i=0; i < n-3; i=i+4){
    in1 = *in++ << 48;
    in2 = *in++ << 32;
    in3 = *in++ << 16;
    in4 = *in++;
    *outd++ = (in1 & WORD3) | (in2 & WORD2) | (in3 & WORD1) | (in4 & WORD0);
  }
  n -= i;

/* Handle the last ones which are not aligned on a Cray word. */

  if(n-- > 0){
    *outd = (*outd & ~WORD3) | ((*in++ << 48) & WORD3);
    if(n-- > 0){
      *outd = (*outd & ~WORD2) | ((*in++ << 32) & WORD2);
      if(n-- > 0){
        *outd = (*outd & ~WORD1) | ((*in++ << 16) & WORD1);
      }
    }
  }
}
/************************************************************************/
void unpack16_c(char *in,int *out,int n)
/*
  Unpack an array of 16 bit integers into integers for unicos
------------------------------------------------------------------------*/
{
  int temp,offset,*ind,i;

  if(n <= 0)return;                             /* Return if nothing to do. */
  temp = (int)in;
  offset = ( temp & CHAR_OFFSET ) >> 62;        /* Get offset of first word. */
  ind = (int *)(temp & ~CHAR_OFFSET);   /* Get address of words. */

/* Handle the first few which are not word aligned. */

  switch(offset){
    case 1:  temp = (*ind >> 32) & WORD0;
             *out++ = (temp < TWO15 ? temp : temp - TWO16);
             if(--n == 0) break;
    case 2:  temp = (*ind >> 16) & WORD0;
             *out++ = (temp < TWO15 ? temp : temp - TWO16);
             if(--n == 0) break;
    case 3:  temp = (*ind++    ) & WORD0;
             *out++ = (temp < TWO15 ? temp : temp - TWO16);
             if(--n == 0) break;
  }

/* Handle those that are Cray-word-aligned. */

  for(i=0; i < n-3; i=i+4){
    temp = (*ind >> 48) & WORD0;
    *out++ = (temp < TWO15 ? temp : temp - TWO16);
    temp = (*ind >> 32) & WORD0;
    *out++ = (temp < TWO15 ? temp : temp - TWO16);
    temp = (*ind >> 16) & WORD0;
    *out++ = (temp < TWO15 ? temp : temp - TWO16);
    temp = (*ind++    ) & WORD0;
    *out++ = (temp < TWO15 ? temp : temp - TWO16);
  }
  n -= i;

/* Handle the last few which are not Cray-word-aligned. */

  if(n-- > 0){
    temp = (*ind >> 48) & WORD0;
    *out++ = (temp < TWO15 ? temp : temp - TWO16);
    if(n-- > 0){
      temp = (*ind >> 32) & WORD0;
      *out++ = (temp < TWO15 ? temp : temp - TWO16);
      if(n-- > 0){
        temp = (*ind >> 16) & WORD0;
        *out++ = (temp < TWO15 ? temp : temp - TWO16);
      }
    }
  }
}
/************************************************************************/
void pack32_c(int *in,char *out,int n)
/*
  Pack an array of integers into 32 bit integers for unicos
------------------------------------------------------------------------*/
{
  int temp,offset,*outd,i,in1,in2;

  if(n <= 0)return;                             /* Return if nothing to do. */
  temp = (int)out;
  offset = ( temp & CHAR_OFFSET ) >> 63;        /* Get offset of first long. */
  outd = (int *)(temp & ~CHAR_OFFSET);  /* Get address of words. */

/* Do the first one, if it is not aligned on a Cray word. */

  if(offset==1){
    *outd = (*outd & ~LOLONG) | (*in++ & LOLONG);
    outd++;
  }
  n -= offset;

/* Do those which are Cray word aligned. */

  for(i=0; i < n-1; i=i+2){
    in1 = *in++ << 32;
    in2 = *in++;
    *outd++ = (in1 & HILONG) | (in2 & LOLONG);
  }
  n -= i;

/* Handle the last one, if there is one. */

  if(n==1)*outd =  (*outd & ~HILONG) | ((*in++ << 32) & HILONG);
}
/************************************************************************/
void unpack32_c(char *in,int *out,int n)
/*
  Unpack an array of 32 bit integers into integers for unicos
------------------------------------------------------------------------*/
{
  int temp,offset,*ind,i;

  if(n <= 0)return;                             /* Return if nothing to do. */
  temp = (int)in;
  offset = ( temp & CHAR_OFFSET ) >> 63;        /* Get offset of first word. */
  ind = (int *)(temp & ~CHAR_OFFSET);   /* Get address of words. */

/* Handle one which is not Cray word aligned. */

  if(offset==1){
    temp = (*ind++ & LOLONG);
    *out++ = (temp < TWO31 ? temp : temp - TWO32);
  }
  n -= offset;

/* Handle those which are Cray word aligned. */

  for(i=0; i < n-1; i=i+2){
    temp = (*ind >> 32) & LOLONG;
    *out++ = (temp < TWO31 ? temp : temp - TWO32);
    temp = (*ind++    ) & LOLONG;
    *out++ = (temp < TWO31 ? temp : temp - TWO32);
  }
  n -= i;

/* Possibly handle a last one which is not Cray word aligned. */

  if(n==1){
    temp = (*ind >> 32) & LOLONG;
    *out++ = (temp < TWO31 ? temp : temp - TWO32);
  }
}
/************************************************************************/
void packr_c(float *in,char *out,int n)
/*
  Pack an array of Cray reals into IEEE reals.
------------------------------------------------------------------------*/
{
  int temp,offset,*outd,bias,*ind,tin,tout,i;

  if(n <= 0)return;                             /* Return if nothing to do. */
  temp = (int)out;
  offset = ( temp & CHAR_OFFSET ) >> 63;        /* Get offset of first long. */
  outd = (int *)(temp & ~CHAR_OFFSET);  /* Get address of words. */
  bias = (16384 - 126) << 48;
  ind = (int *)in;

/* Do the first one, if it is not aligned on a Cray word. */

  if(offset==1){
    tin     = *ind++;
    *outd    =   (*outd & ~LOLONG) |
                 (tin & CRAY_MANT ? (((tin & CRAY_EXPO)-bias) >> 25) |
                ((tin & CRAY_MANT1) >> 24) | ((tin & SIGN) >> 32) : 0);
    outd++;
  }
  n -= offset;

/* Do those which are Cray word aligned. */

  for(i=0; i < n-1; i=i+2){
    tin = *ind++;
    tout =      (tin & CRAY_MANT ? (((tin & CRAY_EXPO)-bias) << 7) |
                ((tin & CRAY_MANT1) << 8) |   (tin & SIGN)        : 0);
    tin = *ind++;
    *outd++ =   tout |
                (tin & CRAY_MANT ? (((tin & CRAY_EXPO)-bias) >> 25) |
                ((tin & CRAY_MANT1) >> 24) | ((tin & SIGN) >> 32) : 0);
  }
  n -= i;

/* Handle the last one, if there is one. */

  if(n==1){
    tin = *ind;
    *outd = (*outd & ~HILONG) |
            (tin & CRAY_MANT ? (((tin & CRAY_EXPO)-bias) << 7) |
            ((tin & CRAY_MANT1) << 8) |   (tin & SIGN)        : 0);
  }
}
/************************************************************************/
void unpackr_c(char *in,float *out,int n)
/*
  Unpack an array of IEEE reals into Cray reals.
------------------------------------------------------------------------*/
{
  int temp,tin,*ind,*outd,offset,i,bias;

  if(n <= 0)return;                             /* Return if nothing to do. */
  temp = (int)in;
  offset = ( temp & CHAR_OFFSET ) >> 63;        /* Get offset of first word. */
  ind = (int *)(temp & ~CHAR_OFFSET);   /* Get address of words. */
  outd = (int *)out;
  bias = ((16384-126) <<48) + (1 << 47);

/* Handle the first one if it is not aligned on a Cray word. */

  if(offset==1){
    tin = *ind++;
    *outd++ = (tin & IEEE_LOEXPO ? (((tin & IEEE_LOEXPO) << 25)+bias) |
        ((tin & IEEE_LOMANT) << 24) | ((tin & IEEE_LOSIGN) << 32) : 0);
  }
  n -= offset;

/* Handle the bulk of them that are aligned on Cray words. */

  for(i=0; i < n-1; i=i+2){
    tin = *ind++;
    *outd++ = (tin & IEEE_HIEXPO ? (((tin & IEEE_HIEXPO) >> 7)+bias) |
        ((tin & IEEE_HIMANT) >> 8 ) |  (tin & IEEE_HISIGN)        : 0);
    *outd++ = (tin & IEEE_LOEXPO ? (((tin & IEEE_LOEXPO) << 25)+bias) |
        ((tin & IEEE_LOMANT) << 24) | ((tin & IEEE_LOSIGN) << 32) : 0);
  }
  n -= i;

/* Handle the last one, if needed, which is not aligned on a Cray word. */

  if(n==1){
    tin = *ind;
    *outd++ = (tin & IEEE_HIEXPO ? (((tin & IEEE_HIEXPO) >> 7)+bias) |
        ((tin & IEEE_HIMANT) >> 8 ) |  (tin & IEEE_HISIGN)        : 0);
  }
}
/************************************************************************/
void packd_c(double *in,char *out,int n)
/*
  Pack an array of Cray reals into IEEE double precision. This assumes
  that a "double" and a "float" are identical.
------------------------------------------------------------------------*/
{
  int *ind,*outd,bias,i,tin;

  ind = (int *)in;
  outd = (int *)out;
  bias = (16384 - 1022) << 48;

  for(i=0; i < n; i++){
    tin = *ind++;
    *outd++ = (tin & CRAY_DMANT ? (tin & SIGN) |
      (((tin & CRAY_EXPO)-bias) << 4) | ((tin & CRAY_DMANT1) << 5) : 0 );
  }
}
/************************************************************************/
void unpackd_c(char *in,double *out,int n)
/*
  Unpack an array of IEEE double precision numbers into Cray reals. This
  assumes that a "double" and a "float" are identical.
------------------------------------------------------------------------*/
{
  int *ind,*outd,bias,i,tin;

  ind = (int *)in;
  outd = (int *)out;
  bias = ((16384 - 1022) << 48) | (1 << 47);

  for(i=0; i < n; i++){
    tin = *ind++;
    *outd++ = (tin & IEEE_DEXPO ? (tin & SIGN) |
      (((tin & IEEE_DEXPO) >> 4) + bias) | ((tin & IEEE_DMANT) >> 5) : 0 );
  }
}

#endif
