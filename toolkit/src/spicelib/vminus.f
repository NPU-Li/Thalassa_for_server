C$Procedure VMINUS ( Negate vector, "-V", 3 dimensions )

      SUBROUTINE VMINUS ( V1, VOUT )

C$ Abstract
C
C     Negate a double precision 3-dimensional vector.
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
C     VECTOR
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION  V1   ( 3 )
      DOUBLE PRECISION  VOUT ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     V1         I   Vector to be negated.
C     VOUT       O   Negated vector -V1.
C
C$ Detailed_Input
C
C     V1       is any double precision 3-dimensional vector.
C
C$ Detailed_Output
C
C     VOUT     is the negation (additive inverse) of V1. It is a
C              double precision 3-dimensional vector.
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
C     For each value of the index I from 1 to 3, VMINUS negates V1
C     by the expression:
C
C        VOUT(I) = - V1(I)
C
C     No error checking is performed since overflow can occur ONLY if
C     the dynamic range of positive floating point numbers is not the
C     same size as the dynamic range of negative floating point numbers
C     AND at least one component of V1 falls outside the common range.
C     The likelihood of this occurring is so small as to be of no
C     concern.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Define a set of 3-dimensional vectors and negate each of them.
C
C
C        Example code begins here.
C
C
C              PROGRAM VMINUS_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local parameters.
C        C
C              INTEGER               SETSIZ
C              PARAMETER           ( SETSIZ = 2 )
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      V1   ( 3, SETSIZ )
C              DOUBLE PRECISION      VOUT ( 3         )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define a set of 3-dimensional vectors.
C        C
C              DATA                  V1  /  1.D0, -2.D0, 0.D0,
C             .                             0.D0,  0.D0, 0.D0  /
C
C        C
C        C     Negate each vector
C        C
C              DO I=1, SETSIZ
C
C                 CALL VMINUS ( V1(1,I), VOUT )
C
C                 WRITE(*,'(A,3F6.1)') 'Input vector  : ',
C             .                        ( V1(J,I), J=1,3 )
C                 WRITE(*,'(A,3F6.1)') 'Negated vector: ', VOUT
C                 WRITE(*,*)
C
C              END DO
C
C              END
C
C
C        When this program was executed on a Mac/Intel/gfortran/64-bit
C        platform, the output was:
C
C
C        Input vector  :    1.0  -2.0   0.0
C        Negated vector:   -1.0   2.0  -0.0
C
C        Input vector  :    0.0   0.0   0.0
C        Negated vector:   -0.0  -0.0  -0.0
C
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
C     W.M. Owen          (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.3, 02-OCT-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard. Added complete
C        code example based on existing example.
C
C-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
C
C-&


C$ Index_Entries
C
C     negate a 3-dimensional vector
C
C-&


      VOUT(1) = -V1(1)
      VOUT(2) = -V1(2)
      VOUT(3) = -V1(3)
C
      RETURN
      END
