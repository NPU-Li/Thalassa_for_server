C$Procedure MXMTG  ( Matrix times matrix transpose, general dimension )

      SUBROUTINE MXMTG ( M1, M2, NR1, NC1C2, NR2, MOUT )

C$ Abstract
C
C     Multiply a matrix and the transpose of a matrix, both of
C     arbitrary size.
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

      INTEGER            NR1
      INTEGER            NC1C2
      INTEGER            NR2
      DOUBLE PRECISION   M1   ( NR1,NC1C2 )
      DOUBLE PRECISION   M2   ( NR2,NC1C2 )
      DOUBLE PRECISION   MOUT ( NR1,NR2   )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     M1         I   Left-hand matrix to be multiplied.
C     M2         I   Right-hand matrix whose transpose is to be
C                    multiplied.
C     NR1        I   Row dimension of M1 and row dimension of MOUT.
C     NC1C2      I   Column dimension of M1 and column dimension of M2.
C     NR2        I   Row dimension of M2 and column dimension of MOUT.
C     MOUT       O   Product matrix M1 * M2**T.
C
C$ Detailed_Input
C
C     M1       is any double precision matrix of arbitrary size.
C
C     M2       is any double precision matrix of arbitrary size.
C
C              The number of columns in M2 must match the number of
C              columns in M1.
C
C     NR1      is the number of rows in both M1 and MOUT.
C
C     NC1C2    is the number of columns in M1 and (by necessity)
C              the number of columns of M2.
C
C     NR2      is the number of rows in both M2 and the number of
C              columns in MOUT.
C
C$ Detailed_Output
C
C     MOUT     is a double precision matrix of dimension NR1 x NR2.
C
C              MOUT is the product matrix given by
C
C                               T
C                 MOUT = M1 x M2
C
C              where the superscript "T" denotes the transpose
C              matrix.
C
C              MOUT must not overwrite M1 or M2.
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
C     For each value of the subscript I from 1 to NR1, and J from 1
C     to NR2:
C
C     MOUT(I,J) = Summation from K=1 to NC1C2 of  ( M1(I,K) * M2(J,K) )
C
C     Notice that the order of the subscripts of M2 are reversed from
C     what they would be if this routine merely multiplied M1 and M2.
C     It is this transposition of subscripts that makes this routine
C     multiply M1 and the TRANPOSE of M2.
C
C     Since this subroutine operates on matrices of arbitrary size, it
C     is not feasible to buffer intermediate results. Thus, MOUT
C     should NOT overwrite either M1 or M2.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C     1) Given a 2x3 and a 3x4 matrices, multiply the first matrix by
C        the transpose of the second one.
C
C
C        Example code begins here.
C
C
C              PROGRAM MXMTG_EX1
C              IMPLICIT NONE
C
C        C
C        C     Local variables.
C        C
C              DOUBLE PRECISION      M1   ( 2, 3 )
C              DOUBLE PRECISION      M2   ( 4, 3 )
C              DOUBLE PRECISION      MOUT ( 2, 4 )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Define M1 and M2.
C        C
C              DATA                  M1 /  1.0D0, 3.0D0,
C             .                            2.0D0, 2.0D0,
C             .                            3.0D0, 1.0D0  /
C
C              DATA                  M2 /  1.0D0, 2.0D0, 1.0D0, 2.0D0,
C             .                            2.0D0, 1.0D0, 2.0D0, 1.0D0,
C             .                            0.0D0, 2.0D0, 0.0D0, 2.0D0 /
C
C        C
C        C     Multiply M1 by the transpose of M2.
C        C
C              CALL MXMTG ( M1, M2, 2, 3, 4, MOUT )
C
C              WRITE(*,'(A)') 'M1 times transpose of M2:'
C              DO I = 1, 2
C
C                 WRITE(*,'(4F10.3)') ( MOUT(I,J), J=1,4)
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
C        M1 times transpose of M2:
C             5.000    10.000     5.000    10.000
C             7.000    10.000     7.000    10.000
C
C
C$ Restrictions
C
C     1)  No error checking is performed to prevent numeric overflow or
C         underflow.
C
C         The user is responsible for checking the magnitudes of the
C         elements of M1 and M2 so that a floating point overflow does
C         not occur.
C
C     2)  No error checking is performed to determine if the input and
C         output matrices have, in fact, been correctly dimensioned.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
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
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
C
C-&


C$ Index_Entries
C
C     matrix times matrix_transpose n-dimensional_case
C
C-&


C
C     Local variables
C
      DOUBLE PRECISION   SUM
      INTEGER            I
      INTEGER            J
      INTEGER            K

C
C  Perform the matrix multiplication
C
      DO I = 1, NR1

         DO J = 1, NR2
            SUM = 0.D0

            DO K = 1, NC1C2
               SUM = SUM + M1(I,K)*M2(J,K)
            END DO

            MOUT(I,J) = SUM

         END DO

      END DO

      RETURN
      END
