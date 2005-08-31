/*============================================================================
*
*   WCSLIB 4.1 - an implementation of the FITS WCS standard.
*   Copyright (C) 1995-2005, Mark Calabretta
*
*   WCSLIB is free software; you can redistribute it and/or modify it under
*   the terms of the GNU General Public License as published by the Free
*   Software Foundation; either version 2 of the License, or (at your option)
*   any later version.
*
*   WCSLIB is distributed in the hope that it will be useful, but WITHOUT ANY
*   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
*   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
*   details.
*
*   You should have received a copy of the GNU General Public License along
*   with WCSLIB; if not, write to the Free Software Foundation, Inc.,
*   59 Temple Place, Suite 330, Boston, MA  02111-1307, USA
*
*   Correspondence concerning WCSLIB may be directed to:
*      Internet email: mcalabre@atnf.csiro.au
*      Postal address: Dr. Mark Calabretta
*                      Australia Telescope National Facility, CSIRO
*                      PO Box 76
*                      Epping NSW 1710
*                      AUSTRALIA
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*   http://www.atnf.csiro.au/~mcalabre/index.html
*   $Id$
*=============================================================================
*
*   WCSLIB 4.1 - C routines that implement the FITS World Coordinate System
*   (WCS) standard.  Refer to
*
*      "Representations of world coordinates in FITS",
*      Greisen, E.W., & Calabretta, M.R. 2002, A&A, 395, 1061 (paper I)
*
*
*   Summary of routines
*   -------------------
*   These routines apply the linear transformation defined by the FITS WCS
*   standard.  They are based on the linprm struct, described in detail
*   below, which contains all information needed for the computations.  The
*   struct contains some members that must be set by the caller, and others
*   that are maintained by these routines, somewhat like a C++ class but with
*   no encapsulation.
*
*   Three routines, linini(), lincpy(), and linfree() are provided to manage
*   the linprm struct, and another, linprt(), prints its contents.
*
*   A setup routine, linset(), computes intermediate values in the linprm
*   struct from parameters in it that were supplied by the caller.  The struct
*   always needs to be set up by linset() but need not be called explicitly -
*   see the explanation of lin.flag below.
*
*   linp2s() and lins2p() implement the WCS linear transformations.
*
*   An auxiliary matrix inversion routine, matinv(), is included.  It uses
*   LU-triangular factorization with scaled partial pivoting.
*
*
*   Default constructor for the linprm struct; linini()
*   ---------------------------------------------------
*   linini() allocates memory for arrays in a linprm struct and sets all
*   members of the struct to default values.
*
*   N.B. every linprm struct should be initialized by linini(), possibly
*   repeatedly.  On the first invokation, and only the first invokation, the
*   flag member of the linprm struct must be set to -1 to initialize memory
*   management, regardless of whether linini() will actually be used to
*   allocate memory.
*
*   Given:
*      alloc    int      If true, allocate memory unconditionally for arrays
*                        in the linprm struct (see "Memory allocation and
*                        deallocation below").
*
*                        If false, it is assumed that pointers to these arrays
*                        have been set by the caller except if they are null
*                        pointers in which case memory will be allocated for
*                        them regardless.  (In other words, setting alloc true
*                        saves having to initalize these pointers to zero.)
*
*      naxis    int      The number of world coordinate axes, used to
*                        determine array sizes.
*
*   Given and returned:
*      lin      struct linprm*
*                        Linear transformation parameters (see below).
*                        Note that, in order to initialize memory management
*                        lin->flag should be set to -1 when lin is initialized
*                        for the first time (memory leaks may result if it had
*                        already been initialized).
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null linprm pointer passed.
*                           2: Memory allocation failed.
*
*
*   Copy routine for the linprm struct; lincpy()
*   --------------------------------------------
*   lincpy() does a deep copy of one linprm struct to another, using linini()
*   to allocate memory for its arrays if required.  Only the "information to
*   be provided" part of the struct is copied; a call to linset() is required
*   to initialize the remainder.
*
*   Given:
*      alloc    int      If true, allocate memory for the crpix, pc, and cdelt
*                        arrays in the destination.  Otherwise, it is assumed
*                        that pointers to these arrays have been set by the
*                        caller except if they are null pointers in which case
*                        memory will be allocated for them regardless.
*      linsrc   const struct linprm*
*                        Struct to copy from.
*
*   Given and returned:
*      lindst   struct linprm*
*                        Struct to copy to.  lindst->flag should be set to -1
*                        if lindst was not previously initialized (memory
*                        leaks may result if it was previously initialized).
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null linprm pointer passed.
*                           2: Memory allocation failed.
*
*
*   Destructor for the linprm struct; linfree()
*   -------------------------------------------
*   linfree() frees memory allocated for the linprm arrays by linini() and/or
*   linset().  linini() keeps a record of the memory it allocates and
*   linfree() will only attempt to free this.
*
*   N.B. linfree() must not be invoked on a linprm struct that was not
*   initialized by linini().
*
*   Given:
*      lin      struct linprm*
*                        Linear transformation parameters (see below).
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null linprm pointer passed.
*
*
*   Print routine for the linprm struct; linprt()
*   ---------------------------------------------
*   linprt() prints the contents of a linprm struct.
*
*   Given:
*      lin      const struct linprm*
*                        Linear transformation parameters (see below).
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null linprm pointer passed.
*
*
*   Setup routine; linset()
*   -----------------------
*   linset(), if necessary, allocates memory for the piximg and imgpix arrays
*   in the linprm struct and sets up the struct according to information
*   supplied within it (see "Linear transformation parameters" below).
*
*   Note that this routine need not be called directly; it will be invoked by
*   linp2x() and linx2p() if the "flag" struct member is anything other than a
*   predefined magic value.
*
*   Given and/or returned:
*      lin      struct linprm*
*                        Linear transformation parameters (see below).
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null linprm pointer passed.
*                           2: Memory allocation failed.
*                           3: PCi_ja matrix is singular.
*
*
*   Pixel-to-world transformation; linp2x()
*   ---------------------------------------
*   linp2x() transforms pixel coordinates to intermediate world coordinates.
*
*   Given and/or returned:
*      lin      struct linprm*
*                        Linear transformation parameters (see below).
*
*   Given:
*      ncoord   int      The number of coordinates, each of vector length
*      nelem    int      nelem but containing lin.naxis coordinate elements.
*      pixcrd   const double[ncoord][nelem]
*                        Array of pixel coordinates.
*
*   Returned:
*      imgcrd   double[ncoord][nelem]
*                        Array of intermediate world coordinates.
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null linprm pointer passed.
*                           2: Memory allocation failed.
*                           3: PCi_ja matrix is singular.
*
*
*   World-to-pixel transformation; linx2p()
*   ---------------------------------------
*   linx2p() transforms intermediate world coordinates to pixel coordinates.
*
*   Given and returned:
*      lin      struct linprm*
*                        Linear transformation parameters (see below).
*
*   Given:
*      ncoord   int      The number of coordinates, each of vector length
*      nelem    int      nelem but containing lin.naxis coordinate elements.
*      imgcrd   const double[ncoord][nelem]
*                        Array of intermediate world coordinates.
*
*   Returned:
*      pixcrd   double[ncoord][nelem]
*                        Array of pixel coordinates.
*
*   Function return value:
*               int      Status return value:
*                           0: Success.
*                           1: Null linprm pointer passed.
*                           2: Memory allocation failed.
*                           3: PCi_ja matrix is singular.
*
*
*   Linear transformation parameters
*   --------------------------------
*   The linprm struct consists of the following elements that must be
*   supplied:
*
*      int flag
*         This flag must be set to zero whenever any of the following members
*         of the linprm struct are set or modified.  This signals the
*         initialization routine, linset(), to recompute intermediaries.
*
*         flag should be set to -1 when linini() is called for the first time
*         for a linprm struct in order to initialize memory management.  It
*         must ONLY be used on the first initialization otherwise memory leaks
*         may result.
*
*      int naxis
*         Number of pixel and world coordinate elements.
*
*      double *crpix
*         Pointer to the first element of an array of double containing the
*         coordinate reference pixel, CRPIXja.
*
*      double *pc
*         Pointer to the first element of the PCi_ja (pixel coordinate)
*         transformation matrix.  The expected order is
*
*            lin.pc = {PC1_1, PC1_2, PC2_1, PC2_2};
*
*         This may be constructed conveniently from a two-dimensional array
*         via
*
*            double m[2][2] = {{PC1_1, PC1_2},
*                              {PC2_1, PC2_2}};
*
*         which is equivalent to,
*
*            double m[2][2];
*            m[0][0] = PC1_1;
*            m[0][1] = PC1_2;
*            m[1][0] = PC2_1;
*            m[1][1] = PC2_2;
*
*         for which the storage order is
*
*            PC1_1, PC1_2, PC2_1, PC2_2
*
*         so it would be legitimate to set lin.pc = *m.
*
*      double *cdelt
*         Pointer to the first element of an array of double containing the
*         coordinate increments, CDELTia.
*
*   The remaining members of the linprm struct are maintained by linset() and
*   must not be modified elsewhere:
*
*      int unity
*         True if the linear transformation matrix is unity.
*
*      double *piximg
*         Pointer to the first element of the matrix containing the product
*         of the CDELTia diagonal matrix and the PCi_ja matrix.
*
*      double *imgpix
*         Pointer to the first element of the inverse of the piximg matrix.
*
*      The remaining members of the linprm struct are used for memory
*      management by linini() and linfree().
*
*
*   Vector arguments
*   ----------------
*   Arrays of pixel and world coordinates are two dimensional, i.e.
*   pixcrd[ncoord][nelem], where nelem must equal or exceed the number of
*   coordinate elements, naxis, stored in the linprm struct.  Exception: when
*   ncoord == 1, nelem is not used.
*
*   Note that the function prototypes must declare two-dimensional arrays as
*   one-dimensional to avoid warnings about declaration of "incomplete types".
*   This was considered preferable to declaring them as simple
*   pointers-to-double which gives no indication that storage is associated
*   with them.
*
*
*   Memory allocation and deallocation
*   ----------------------------------
*   linini() optionally allocates memory for the crpix, pc, and cdelt arrays
*   in the linprm struct as described in the usage notes above.
*
*   If the pc matrix is not unity, linset() also allocates memory for the
*   piximg and imgpix arrays.  The caller must not modify these.
*
*   linini() maintains a record of memory it has allocated and this is used
*   by linfree() which linini() uses to free any memory that it may have
*   allocated on a previous invokation.  Thus it is not necessary for the
*   caller to invoke linfree() separately.  Likewise, linset() deallocates
*   memory that it may have allocated on a previous invokation.
*
*   A memory leak will result if a linprm struct goes out of scope before the
*   memory has been free'd, either by linfree() or otherwise.  Likewise, if
*   the linprm struct itself has been malloc'd and the allocated memory is not
*   free'd when the memory for the struct is free'd.  A leak may also arise if
*   the caller interferes with the array pointers in the "private" part of the
*   linprm struct.
*
*   Beware of making a shallow copy of a linprm struct by assignment; any
*   changes made to allocated memory in one would be reflected in the other,
*   and if the memory allocated for one was free'd the other would reference
*   unallocated memory.  Use lincpy() instead to make a deep copy.
*
*
*   Status return values
*   --------------------
*   Error messages to match the status value returned from each function are
*   encoded in the lin_errmsg character array.
*
*===========================================================================*/

#ifndef WCSLIB_LIN
#define WCSLIB_LIN

#ifdef __cplusplus
extern "C" {
#endif


extern const char *lin_errmsg[];
#define linini_errmsg lin_errmsg
#define lincpy_errmsg lin_errmsg
#define linfree_errmsg lin_errmsg
#define linprt_errmsg lin_errmsg
#define linset_errmsg lin_errmsg
#define linp2x_errmsg lin_errmsg
#define linx2p_errmsg lin_errmsg


struct linprm {
   /* Initialization flag (see the prologue above).                         */
   /*-----------------------------------------------------------------------*/
   int flag;			/* Set to zero to force initialization.     */

   /* Parameters to be provided (see the prologue above).                   */
   /*-----------------------------------------------------------------------*/
   int naxis;			/* The number of axes, given by NAXIS.      */
   double *crpix;		/* CRPIXja cards for each pixel axis.       */
   double *pc;			/* PCi_ja  linear transformation matrix.    */
   double *cdelt;		/* CDELTia cards for each coordinate axis.  */

   /* Information derived from the parameters supplied.                     */
   /*-----------------------------------------------------------------------*/
   int unity;			/* True if the PCi_ja matrix is unity.      */
   double *piximg;		/* Product of CDELTia and PCi_ja matrices.  */
   double *imgpix;		/* Inverse of the piximg matrix.            */

   int m_flag, m_naxis;		/* The remainder are for memory management. */
   double *m_crpix, *m_pc, *m_cdelt;
   int i_naxis;
};

#define LINLEN (sizeof(struct linprm)/sizeof(int))

int linini(int, int, struct linprm *);

int lincpy(int, const struct linprm *, struct linprm *);

int linfree(struct linprm *);

int linprt(const struct linprm *);

int linset(struct linprm *);

int linp2x(struct linprm *, int, int, const double[], double[]);

int linx2p(struct linprm *, int, int, const double[], double[]);

int matinv(int, const double [], double []);


/* Define macros for scalar invokation for compatibility with WCSLIB 2.x. */
#define linrev_errmsg lin_errmsg
#define linfwd_errmsg lin_errmsg
#define linrev(pixcrd, lin, imgcrd) linp2x(lin, 1, 1, pixcrd, imgcrd)
#define linfwd(imgcrd, lin, pixcrd) linx2p(lin, 1, 1, imgcrd, pixcrd)


#ifdef __cplusplus
};
#endif

#endif /* WCSLIB_LIN */
