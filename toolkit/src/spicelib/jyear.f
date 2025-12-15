C$Procedure JYEAR ( Seconds per julian year )

      DOUBLE PRECISION FUNCTION JYEAR ()

C$ Abstract
C
C     Return the number of seconds in a julian year.
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
C     CONSTANTS
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     The function returns the number of seconds per julian year.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     The function returns the number of seconds per julian year.
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
C     The julian year is often used as a fundamental unit of time when
C     dealing with ephemeris data. For this reason its value in terms of
C     ephemeris seconds is recorded in this function.
C
C$ Examples
C
C     Suppose you wish to compute the number of julian centuries
C     that have elapsed since the ephemeris epoch J1950 (beginning
C     of the julian year 1950) at a particular ET epoch. The
C     following line of code will do the trick.
C
C
C        CENTRY = ( ET - UNITIM ( J1950(), 'JED', 'ET' ) )
C       .       / ( 100.0D0 * JYEAR() )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     [1]  P. Kenneth Seidelmann (Ed.), "Explanatory Supplement to the
C          Astronomical Almanac," Page 8, University Science Books,
C          1992.
C
C$ Author_and_Institution
C
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 08-APR-2021 (JDR)
C
C        Edited the header to comply with NAIF standard.
C
C-    SPICELIB Version 1.0.0, 13-JUL-1993 (WLT)
C
C-&


C$ Index_Entries
C
C     Number of seconds per julian year
C
C-&


      JYEAR = 31557600.0D0

      RETURN
      END
