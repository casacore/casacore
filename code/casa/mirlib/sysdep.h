/*
 *  History:
 *    pjt 31oct89 _trace_ added as defined() option, BUFALIGN 8.
 *    rjs 21feb90 Added alternate way of defining FORT_TRUE and FALSE
 *		  to improve XMP and Cray-2 compatibility. This change
 *		  care of Brian Glendenning.
 *    mjs  ??     Increased BUFSIZE to avoid apparent OS problem on "ral".
 *    rjs  8feb91 Convex definitions.
 *    mjs 18mar91 More Convex definitions.
 *    rjs 16apr91 Removed macros redefining memcpy as bcopy -- no longer
 *		  needed, and bcopy is slower on the Suns anyway.
 *    rjs 24jun91 Added memcmp define for the convex.
 *    rjs 18dec92 Added hpux. Various tidying.
 *    mjs 19feb93 Added mips.
 *     jm 07nov94 Added definition of Null and typedef of Void.  The
 *                Void typedef permits proper casting in both ANSI
 *                and non-ANSI archs.  Also added definition to permit
 *                the use of const in non-ANSI declarations.
 *     jm 17nov94 Changed the conditional definition around the typedef
 *                of Void because Sun defines __STDC__ even when it is
 *                zero!  Defined PROTOTYPE as 1 if __STDC__ is set to 1;
 *                otherwise it is undefined.  Also added ARGS definition
 *                to aide forward declartion prototyping.
 *    rjs 20nov94 Added "alpha" ifdef.
 */

#ifndef Null
#define Null '\0'
#endif

/*
 *  Void is typedef'd to the proper word depending on the level of
 *  ANSI conformance.  Also, if ANSI conforming, Const is defined
 *  to const; otherwise, Const is defined as a NULL statement.
 *
 *  PROTOTYPE is defined only if function prototypes are correctly
 *  understood.
 *
 *  ARGS defines a macro that aides in presenting prototypes.
 *  Use it as (double parentheses required):
 *    extern void keyput_c ARGS((const char *task, char *arg));
 */

#ifdef __STDC__
#if (__STDC__ == 1)
typedef void Void;
#define Const const
#define PROTOTYPE 1
#define ARGS(alist) alist
#else
typedef char Void;
#define Const /* NULL */
#define ARGS(alist) ()
#endif /* (__STDC__ == 1) */
#else
typedef char Void;
#define Const /* NULL */
#define ARGS(alist) ()
#endif /* __STDC__ */

typedef int int2;

/************************************************************************/
/*									*/
/*			VMS definitions.				*/
/*									*/
/************************************************************************/

#ifdef vaxc
#define FORT_TRUE -1
#define FORT_FALSE 0
#define BUFDBUFF 1
#define BUFALIGN 512
#define BUFSIZE 16384
#define defined_params
#endif

/************************************************************************/
/*									*/
/*			UNICOS definitions				*/
/*									*/
/************************************************************************/

#ifdef unicos
#include <fortran.h>
#define FORT_TRUE  _btol(1)
#define FORT_FALSE _btol(0)
#define BUFDBUFF 0
#define BUFALIGN 8
#define BUFSIZE 16384
#define defined_params
#endif

/************************************************************************/
/*									*/
/*			UNIX definitions.				*/
/*									*/
/************************************************************************/

#ifndef defined_params
#if defined(convex) || defined(alpha)
#  define FORT_TRUE  -1
#else
#  define FORT_TRUE 1
#endif
#define FORT_FALSE 0

#define BUFDBUFF 0

#if defined(trace)
#  define BUFALIGN 8
#else
#  define BUFALIGN 2
#endif

#define BUFSIZE 16384

/* The Multiflow machine does not have the memcpy routine. Use bcopy
   instead.								*/

#if defined(trace)
#  define memcpy(a,b,c) bcopy((b),(a),(c))
#  define memcmp(a,b,c) bcmp((a),(b),(c))
#endif

/*  Short cut routines when no conversion is necessary. These are
    used for any IEEE floating point machine with FITS ordered bytes.	*/

#if defined(sun) || defined(alliant) || defined(trace) || defined(convex) || defined(hpux) || defined(mips)
#  define packr_c(a,b,c)    memcpy((b),(char *)(a),sizeof(float)*(c))
#  define unpackr_c(a,b,c)  memcpy((char *)(b),(a),sizeof(float)*(c))
#  define packd_c(a,b,c)    memcpy((b),(char *)(a),sizeof(double)*(c))
#  define unpackd_c(a,b,c)  memcpy((char *)(b),(a),sizeof(double)*(c))
#  define pack32_c(a,b,c)   memcpy((b),(char *)(a),sizeof(int)*(c))
#  define unpack32_c(a,b,c) memcpy((char *)(b),(a),sizeof(int)*(c))
#endif
#endif
