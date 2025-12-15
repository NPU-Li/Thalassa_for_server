C$Procedure MTXM  ( Matrix transpose times matrix, 3x3 )

      SUBROUTINE MTXM ( M1, M2, MOUT )

C$ Abstract
C
C     Multiply the transpose of a 3x3 matrix and a 3x3 matrix.
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
C     MATRIX
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION   M1   ( 3,3 )
      DOUBLE PRECISION   M2   ( 3,3 )
      DOUBLE PRECISION   MOUT ( 3,3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     M1         I   3x3 double precision matrix.
C     M2         I   3x3 double precision matrix.
C     MOUT       O   3x3 double precision matrix which is the product
C                    (M1**T) * M2.
C
C$ Detailed_Input
C
C     M1       is any 3x3 double precision matrix. Typically,
C              M1 will be a rotation matrix since then its
C              transpose is its inverse (but this is NOT a
C              requirement).
C
C     M2       is any 3x3 double precision matrix.
C
C$ Detailed_Output
C
C     MOUT     is s 3x3 double precision matrix. MOUT is the
C              product MOUT = (M1**T) x M2.
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
C     The code reflects precisely the following mathematical expression
C
C        For each value of the subscripts I and J from 1 to 3:
C
C        MOUT(I,J) = Summation from K=1 to 3 of  ( M1(K,I) * M2(K,J) )
C
C     Note that the reversal of the K and I subscripts in the left-hand
C     matrix M1 is what makes MOUT the product of the TRANSPOSE of M1
C     and not simply of M1 itself.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Given two 3x3 matrices, multiply the transpose of the first
C        matrix by the second one.
C
C
C        Example code begins here.
C
C
C              PROGRAM MTXM_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      M1   ( 3, 3 )
C              DOUBLE PRECISION      M2   ( 3, 3 )
C              DOUBLE PRECISION      MOUT ( 3, 3 )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define M1 and M2.
C        C
C              DATA                  M1 /  1.0D0,  4.0D0,  7.0D0,
C             .                            2.0D0,  5.0D0,  8.0D0,
C             .                            3.0D0,  6.0D0,  9.0D0  /
C
C              DATA                  M2 /  1.0D0, -1.0D0,  0.0D0,
C             .                            1.0D0,  1.0D0,  0.0D0,
C             .                            0.0D0,  0.0D0,  1.0D0  /
C
C        C
C        C     Multiply the transpose of M1 by M2.
C        C
C              CALL MTXM ( M1, M2, MOUT )
C
C              WRITE(*,'(A)') 'Transpose of M1 times M2:'
C              DO I = 1, 3
C
C                 WRITE(*,'(3F10.3)') ( MOUT(I,J), J=1,3)
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
C        Transpose of M1 times M2:
C            -3.000     5.000     7.000
C            -3.000     7.000     8.000
C            -3.000     9.000     9.000
C
C
C$ Restrictions
C
C     1)  The user is responsible for checking the magnitudes of the
C         elements of M1 and M2 so that a floating point overflow does
C         not occur. (In the typical use where M1 and M2 are rotation
C         matrices, this not a risk at all.)
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
C-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
C
C        Added IMPLICIT NONE statement.
C
C        Edited the header to comply with NAIF standard.
C        Added complete code example based on the existing example.
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
C     matrix_transpose times matrix 3x3_case
C
C-&


C
C     Local variables
C
      DOUBLE PRECISION      PRODM(3,3)

      INTEGER               I
      INTEGER               J

C
C  Perform the matrix multiplication
C
      DO   I=1,3
         DO   J=1,3
            PRODM(I,J) = M1(1,I)*M2(1,J)
     .                 + M1(2,I)*M2(2,J)
     .                 + M1(3,I)*M2(3,J)
         END DO
      END DO

C
C  Move the result into MOUT
C
      CALL MOVED ( PRODM, 9, MOUT )

      RETURN
      END
