*=======================================================================
*                              PGSBOX 3.2
*-----------------------------------------------------------------------
*
*   ATTENTION!
*   ----------
*
*   PGCRVL is defunct.
*
*   Do not use this routine - use PGSBOX instead.
*   ---------------------------------------------
*
*   What remains here is a driver for the more general PGSBOX routine
*   which is not based on pixel coordinates.
*
*   This residue of PGCRVL exists mainly for backwards compatibility.
*   PGCRVL's old AXEN argument is here translated into the BLC and TRC
*   arguments required by PGSBOX.
*
*   New applications should use PGSBOX directly.
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*   $Id$
*=======================================================================
      SUBROUTINE PGCRVL (AXEN, IDENTS, OPT, LABCTL, LABDEN, CI, GCODE,
     :   TIKLEN, NG1, GRID1, NG2, GRID2, DOEQ, NLFUNC, NLC, NLI, NLD,
     :   NLCPRM, NLIPRM, NLDPRM, NC, IC, CACHE, IERR)
*-----------------------------------------------------------------------
      LOGICAL   DOEQ
      INTEGER   AXEN(2), CI(7), GCODE(2), IC, IERR, LABCTL, LABDEN, NC,
     :          NG1, NG2, NLC, NLD, NLI, NLIPRM(NLI)
      REAL      BLC(2), TRC(2)
      DOUBLE PRECISION CACHE(4,0:NC), GRID1(0:NG1), GRID2(0:NG2),
     :          NLDPRM(NLD), TIKLEN
      CHARACTER IDENTS(3)*(*), NLCPRM(NLC)*1, OPT(2)*(*)

      EXTERNAL NLFUNC
*-----------------------------------------------------------------------
      BLC(1) = 0.5
      BLC(2) = 0.5
      TRC(1) = AXEN(1) + 0.5
      TRC(2) = AXEN(2) + 0.5

      CALL PGSBOX (BLC, TRC, IDENTS, OPT, LABCTL, LABDEN, CI, GCODE,
     :   TIKLEN, NG1, GRID1, NG2, GRID2, DOEQ, NLFUNC, NLC, NLI, NLD,
     :   NLCPRM, NLIPRM, NLDPRM, NC, IC, CACHE, IERR)

      RETURN
      END
