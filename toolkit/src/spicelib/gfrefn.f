C$Procedure GFREFN ( GF, default refinement estimator )

      SUBROUTINE GFREFN ( T1, T2, S1, S2, T )

C$ Abstract
C
C     Estimate, using a bisection method, the next abscissa value at
C     which a state change occurs. This is the default GF refinement
C     method.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     SEARCH
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      T1
      DOUBLE PRECISION      T2
      LOGICAL               S1
      LOGICAL               S2
      DOUBLE PRECISION      T

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     T1         I   One of two values bracketing a state change.
C     T2         I   The other value that brackets a state change.
C     S1         I   State at T1.
C     S2         I   State at T2.
C     T          O   New value at which to check for transition.
C
C$ Detailed_Input
C
C     T1       is one of two abscissa values (usually times)
C              bracketing a state change.
C
C     T2       is the other abscissa value that brackets a state change.
C
C     S1       is the system state at T1. This argument is provided
C              for forward compatibility; it's not currently used.
C
C     S2       is the system state at T2. This argument is provided
C              for forward compatibility; it's not currently used.
C
C$ Detailed_Output
C
C     T        is the midpoint of T1 and T2.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     "Refinement" means reducing the size of a bracketing interval on
C     the real line in which a solution is known to lie. In the GF
C     setting, the solution is the time of a state transition of a
C     binary function.
C
C     This routine supports solving for locations of bracketed state
C     transitions by the bisection method. This is the default
C     refinement method used by the GF system.
C
C     The argument list of this routine is compatible with the GF
C     system's general root finding routine. Refinement routines created
C     by users must have the same argument list in order to be used by
C     the GF mid-level APIs such as GFOCCE and GFFOVE.
C
C$ Examples
C
C     The following code fragment from an example program in the header
C     of GFOCCE shows the routine passed as the 12th argument.
C
C        C
C        C     Define as EXTERNAL the routines to pass to GFOCCE.
C        C
C              EXTERNAL              GFSTEP
C              EXTERNAL              GFREFN
C              EXTERNAL              GFREPI
C              EXTERNAL              GFREPU
C              EXTERNAL              GFREPF
C              EXTERNAL              GFBAIL
C
C                 ... initialize for the search ...
C
C              CALL GFOCCE ( 'ANY',
C             .              'MOON',   'ellipsoid',  'IAU_MOON',
C             .              'SUN',    'ellipsoid',  'IAU_SUN',
C             .              'LT',     'EARTH',       CNVTOL,
C             .               GFSTEP,   GFREFN,       RPT,
C             .               GFREPI,   GFREPU,       GFREPF,
C             .               BAIL,     GFBAIL,       CNFINE,  RESULT )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 26-OCT-2021 (JDR)
C
C        Edited the header to comply with NAIF standard.
C
C-    SPICELIB Version 1.0.0, 03-MAR-2009 (NJB) (EDW)
C
C-&


C$ Index_Entries
C
C     GF standard step refinement
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      BRCKTD

C
C     Local variables.
C
      DOUBLE PRECISION      X


      X = ( T1 * 0.5D0 ) + ( T2 * 0.5D0 )

      T = BRCKTD ( X, T1, T2 )

      RETURN
      END
