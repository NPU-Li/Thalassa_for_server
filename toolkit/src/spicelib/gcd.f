C$Procedure GCD ( Greatest Common Divisor )

      INTEGER FUNCTION GCD ( A, B )

C$ Abstract
C
C     Return the greatest common divisor of two integers.
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
C     MATH
C     NUMBERS
C
C$ Declarations

      IMPLICIT NONE

      INTEGER          A
      INTEGER          B

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     A          I   Any integer
C     B          I   Any integer
C
C     The function returns the greatest common divisor of A and B.
C
C$ Detailed_Input
C
C     A        is any integer.
C
C     B        is any integer.
C
C$ Detailed_Output
C
C     The function returns the greatest common divisor of A and B.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If both A and B are zero, then GCD returns zero.
C
C     2)  If exactly one of A and B is zero, then GCD is by definition
C         the maximum of the absolute values of A and B.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine uses Euclid's Algorithm to find the greatest common
C     divisor (GCD) of the integers A and B. In other words the
C     largest integer, G, such that A = k*G for some k and B = j*G for
C     some G. Note if either A or B is zero, then we return the
C     maximum of the two integers ABS(A) and ABS(B).  If one is
C     non-zero we have just what the definition says. If both are zero
C     the definition above does not give us a GCD, so we take the GCD
C     of 0 and 0 to be 0.
C
C$ Examples
C
C     A      B            GCD
C     -----  -----         -----
C       8      4             4
C      120    44             4
C      15    135            15
C     101     97             1
C     119    221            17
C     144     81             9
C       0    111           111
C       0      0             0
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     [1]  D. Knuth, "The Art of Computer Programming Vol 1. --
C          Fundamental Algorithms," 2nd Ed., Addison-Wesley, 1973
C
C$ Author_and_Institution
C
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&


C$ Index_Entries
C
C     greatest common divisor
C
C-&


C
C     Local variables
C
      INTEGER          REMNDR
      INTEGER          ABSA
      INTEGER          ABSB
      INTEGER          P
      INTEGER          Q

      ABSA = ABS(A)
      ABSB = ABS(B)

      IF ( ABSA .GT. ABSB ) THEN
         P = ABSA
         Q = ABSB
      ELSE
         P = ABSB
         Q = ABSA
      END IF

      REMNDR = 1


      IF ( Q .NE. 0 ) THEN

         DO WHILE (REMNDR .NE. 0 )
            GCD    = Q
            REMNDR = P - (P/Q)*Q
            P      = Q
            Q      = REMNDR
         END DO

      ELSE

         GCD = P

      END IF

      RETURN

      END
